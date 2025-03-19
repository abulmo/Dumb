/**
 * File: search.d
 * Best move search.
 *
 * Authors: Richard Delorme
 * Date: 2025
 */

module search;

@safe:

import board, eval, move, util;
import std.stdio, std.string, std.format, std.algorithm, std.math;
import core.bitop;

/** Hash table score bound */
enum Bound { none, upper, lower, exact }

/**
 * struct Entry
 *
 * Transposition Table Entry
 *
 * Here we store:
 *  - a Zobrist Key representing a position
 *  - some search info: type of bound, depth & age of this entry
 *  - the best move found for this position
 *  - the best score found for this position
 */
struct Entry {
	ulong code;  /// zobrist key to distinguish positions
	ushort info; /// info stored: bound, depth & age
	Move move;   /// best move
	short value; /// best score

	/**
	 * Get the depth of the search results stored in the entry.
	 * returns: the depth.
	 */
	int depth() pure nothrow const { return (info >> 2) & 127; }

	/**
	 * Return the score bound stored in the Entry
	 * returns: a bound
	 */
	Bound bound() pure nothrow const { return cast (Bound) (info & 3);	}

	/**
	 * Return the score, if the score is a mate in 'x' plies, it is corrected to be in 'y' plies from the root position.
	 * params: ply = the ply of the position (to compute mate score).
	 * returns: a score
	 */
	int score(const int ply) pure nothrow const { return value < Score.low ? value + ply : (value > Score.high ? value - ply : value); }

	/**
	 * Store an entry.
	 * params: key = the key representing the position
	 *         depth = the depth
	 *         ply = the ply of the searched position in the search tree (to compute mate score)
	 *         date = the age of the position
	 *         bound = the bound of the score
	 *         score = the value of the bestscore found. If the score is a mate in 'x' plies from the root,
	 * its value is corrected to be in 'y' plies relative to the current position.
	 *         m = the best move found
	 */
	void set(const Key key, const int depth, const int ply, const int date, const Bound bound, const int score, const Move m) pure nothrow {
		code = key.code;
		info = cast (ushort) (bound | (depth << 2) | (date << 9));
		value = cast (short) (score < Score.low ? score - ply : (score > Score.high ? score + ply : score));
		move = m;
	}
}

/**
 * class TranspositionTable
 * An hash table to store already seen positions : ~390 Elo.
 */
struct TranspositionTable {
	enum size_t bucketSize = 4; /// Bucket size
	ubyte date;                 /// date: age of the hash table
	Entry [] entry;             /// array of entries
	size_t mask;                /// mask to index an entry from a zobrist key

	/**
	 * Constructor with an initial size
	 * params: size = initial size of the hash table.
	 */
	this(size_t size) pure nothrow {
		resize(size);
	}

	/**
	 * Resise the hash table
	 * params: size = new size of the hash table.
	 */
	void resize(size_t size) pure nothrow {
		mask = (1 << bsf(size / Entry.sizeof)) - 1;
		entry.length = mask + bucketSize;
		clear();
	}

	/**
	 * Clear the table
	 *
	 * Set all the entries to their initial values and the date to 0.
	 *
	 */
	void clear() pure nothrow {
		date = 0;
		foreach (ref h; entry) h = Entry.init;
	}

	/**
	 * Make the hash table older
	 * Increase the date value modulo 128. When the date reach 127, it is set to 0 and the age in all entries
	 * are resetted to 0 too.
	 */
	void age() pure nothrow {
		if (date == 127) {
			date = 0;
			foreach (ref h; entry) h.info &= 511;
		}
		++date;
	}

	/**
	 * Look for a position in the hash table, return true if one is found and fill the /found/ entry.
	 *
	 * params: key = Zobrist's key representing the board.
	 *         found = a copy of the entry
	 * returns: true if an entry matching the zobrist's key is found.
	 */
	bool probe(const Key key, ref Entry found) pure nothrow const {
		const size_t i = cast (size_t) (key.code & mask);
		foreach (ref h; entry[i .. i + bucketSize]) if (h.code == key.code) {
			found = h;
			return true;
		}
		return false;
	}

	/**
	 * Store a position : update an existing entry or replace the entry with the worst draft of the bucket
	 *
	 * params: key = Zobrist's key representing the board.
	 *         depth = search depth.
	 *         ply = search ply, ie distance drom the root position of the current position.
	 *         bound = score bound.
	 *         score = search bestscore of the current position.
	 *         move = search bestmove of the current position.
	 */
	void store(const Key key, const int depth, const int ply, const Bound bound, const int score, const Move move) pure nothrow {
		const size_t i = cast (size_t) (key.code & mask);
		size_t w = i;
		foreach (j; i .. i + bucketSize) {
			if (entry[j].code == key.code) return entry[j].set(key, depth, ply, date, bound, score, move);
			else if (entry[w].info > entry[j].info) w = j;
		}
		entry[w].set(key, depth, ply, date, bound, score, move);
	}

	/**
	 * Return a lower or exact bound depending on the relationship between the score and the β limit
	 */
	Bound bound(const int v, const int β) pure nothrow const { return v >= β ? Bound.lower : Bound.exact; }
}

/**
 * struct Option
 * Define the kind of search: mate/node limited/time limited or depth limited or if the engine is pondering
 */
struct Option {
	struct Max(T) { T max; }
	Max!(double) time; /// max time to search
	Max!(ulong) nodes; /// max nodes to search
	Max!(int) depth;   /// max depth to search
	Max!(int) mate;    /// search for a mate
	bool isPondering;  /// flag when pondering
	bool quiet;        /// search silently
}

/**
 * class Search
 * search the best move of a given postion
 */
final class Search {
	Board board;                         /// The position to search
	Eval eval;                           /// Evaluation function
	TranspositionTable tt;               /// Transposition table
	History history;                     /// History
	Moves rootMoves;                     /// List of moves to search at root
	Event event;                         /// Event (interface to send/receive text)
	Option option;                       /// Search options
	Chrono timer;                        /// Time spent by the search
	Line [Limits.Ply.max + 1] pv;        /// Principal variation (line of best moves)
	Line previousPV;                     /// Previous principal variation
	Move [2][Limits.Ply.max + 2] killer; /// Killer moves
	Move [Limits.Move.size] refutation;  /// refutation move
	ubyte [32][32] reduction;            /// reduction table
	int [Limits.Ply.max + 1] sv;         /// Static eValuation
	ulong pvsNodes;                      /// Node counter for principal variation search
	ulong qsNodes;                       /// Node counter for quiescence search
	int ply;                             /// Game ply
	int highestPly;                      /// Highest ply reached during quiescent search
	bool stop;                           /// A flag to stop the search

	/**
	 * Constructor: allocate the tranqposition table (64 MB by default)
	 * and initialize the reduction table.
	 *
	 * params: size size of the transposition table
	 */
	this(size_t size = 64 * 1024 * 1024) pure nothrow {
		tt = TranspositionTable(size);
		initReduction();
	}

	/**
	 *  Initialize the reduction table
	 */
	void initReduction() pure nothrow {
		foreach (d; 1 .. 32)
		foreach (m; 1 .. 32) reduction[d][m] = cast (ubyte) (1.0 * std.math.log(1.0 * d) + 0.95 * std.math.log(1.0 * m));
	}

	/**
	 * Return the reduction according to the depth d and the move index m, limiting them to 31.
	 * params: d = depth
	 *         m = move number
	 * returns: the depth reduction for LMR (late move reduction) & LMP (late move pruning)
	 */
	int reduce(const int d, const int m) const { return reduction[min(d, 31)][min(m, 31)]; }

	/**
	 *  Store a move into various heuristics: killers, refutation move, history.
	 *
	 * params: move = the move to store
	 *         previousMove = the opponent move made before the current move
	 *         depth = depth of the search that found that move.
	 */
	void store(Move move, const Move previousMove, const int depth, const ref Moves moves) pure nothrow  {
		if (!move.isTactical(board)) { // only store quiet moves
			if (move != killer[ply][0]) { killer[ply][1] = killer[ply][0]; killer[ply][0] = move; }
			if (ply > 0) refutation[previousMove & Limits.Move.mask] = move;
			history.update(board, move, depth * depth, history.good);
			for (int k = 0; move != moves[k]; ++k) history.update(board, moves[k], depth * depth, history.bad);
		}
	}

	/**
	 * Return true if enough time is available to search
	 *
	 * params: timeMax = upper bound of the time alloted to the search
	 * returns: true if the search can continue, false otherwise.
	 */
	bool checkTime(const double timeMax) nothrow const { return option.isPondering || timer.time < timeMax; }

	/**
	 *  Abort the search when a limit is reached or stop is received
	 *
	 *  returns: true if the search need to be stopped.
	 */
	bool abort() {
		if ((pvsNodes & 0x3ff) == 0) {
			if (event) {
				if (option.isPondering && event.has("ponderhit")) {
					option.isPondering = false;
					option.time.max += timer.time;
				} else if (event.has("stop")) {
					stop = true;
				} else if (event.has("isready")) {
					event.send("readyok");
					event.peek();
				}
			}
			if (!checkTime(option.time.max)) stop = true;
		}
		if (pvsNodes + qsNodes >= option.nodes.max) stop = true;
		return stop;
	}

	/**
	 * Return the distante to mate of a mate score
	 *
	 * params: s = score to compute the distance from.
	 * returns: the distance to mate.
	 */
	int mateIn(const int s) pure nothrow const { return s > Score.high ? (Score.mate + 1 - s) / 2 : (s < -Score.high ? -(Score.mate + s) / 2 : int.max); }

	/**
	 *  Write uci info about the search
	 *
	 * params: depth = the depth of the search
	 *         score = the score of the search

	 */
	void writeUCI(const int depth, const int score) {
		string s = format("info depth %d seldepth %d", depth, highestPly);
		if (abs(score) > Score.high) s ~= format(" score mate %d", mateIn(score)); else s ~= format(" score cp %d", score);
		s ~= format(" nodes %s time %.0f nps %.0f", pvsNodes + qsNodes, 1000.0 * timer.time, (pvsNodes + qsNodes)  / timer.time);
		if (pv[0].n > 0) s ~= format(" pv %s", pv[0].toString(board)); else s ~= format(" pv %s", bestMove.toPan(board));
		event.send(s);
	}

	/**
	 * Update the state of the search after a move is made. The chess position and the evaluation function are updated.
	 * params: quiet = template parameter to update (when true) or not the zobrist keys (unused in the quiescence search).
	 *         move = the move to make
	 */
	bool update(bool quiet = true)(const Move move) pure nothrow {
		if (move) eval.update(board, move);
		++ply;
		return board.update!quiet(move);
	}

	/**
	 * Restore the state of the search after undoing a move.
	 * params: move = the move to unmake
	 */
	void restore(const Move move) pure nothrow {
		board.restore(move);
		if (move) eval.restore();
		--ply;
	}

	/**
	 * Quiescence search: ~164 Elo.
	 *
	 * The quiescence search aims at removing all moves that can affect too much the evaluation of the position.
	 * So only the captures, promotions, pawn moves to the 7th rank (threatening a promotion) and check evasions
	 * are generated and evaluated in a simple alpha beta search.
	 *
	 * params: α = lower bound of the alphabeta window.
	 * params: β = upper bound of the alphabeta window.
	 * returns: the best score found
	 */
	int qs(int α, int β) pure nothrow {
		int score, bestscore;
		Moves moves = void;
		MoveItem item = void;
		Move move;

		// increase node count
		++qsNodes;

		// selective depth for UCI display
		highestPly = max(highestPly, ply);

		// stand pat: if not in check, compute the eval as best "quiet" score, and return it if it is good enough
		if (!board.inCheck) {
			bestscore = eval(board);
			if ((bestscore > α) && (α = bestscore) >= β) return bestscore;
		// else compute mate score limits based on the search ply as best score
		} else bestscore = ply - Score.mate;

		// compute the eval when the ply limit is reached
		if (ply == Limits.Ply.max) return eval(board);

		// Generate all turbulent moves: capture, promotion, pawn push to 7th rank, check evasion, ...
		moves.generate!(Generator.capture)(board, history);

		// loop over all good capture or check evasion move
		while ((move = (item = moves.next).move) != 0) if (board.inCheck || item.isTactical) {
			if (update!false(move)) score = -qs(-β, -α); else score = bestscore;
			restore(move);
			if (score > bestscore && (bestscore = score) > α && (α = bestscore) >= β) return bestscore;
		}

		// return the bestscore
		return bestscore;
	}

	/**
	 * Principal variation Search; an alphabeta variant using null window search on non PV nodes.
	 *
	 * params: α = lower bound of the alphabeta window.
	 * params: β = upper bound of the alphabeta window.
	 * params: d = depth to search up to.
	 * params: previousMove = the last move that produce the current position. Used to possibly retrieve
	 * a refutation move.
	 * returns: the best score found
	 */
	int pvs(int α, int β, const int d, const Move previousMove = 0) {
		const bool isPv = (α + 1 < β);
		int e, r, s, bs, v, quiet = 0, nLegal = 0;
		Moves moves = void;
		MoveItem i = void;
		Move m;
		Entry h;

		// Clear the PV
		pv[ply].clear();

		// Check if the search need to be stopped
		if (abort()) return α;

		// Once the search horizon is reached, call the quiescence search
		if (d <= 0) return qs(α, β);

		// increase node count
		++pvsNodes;

		// If the position is drawn return 0 */
		if (board.isDraw) return 0;

		// compute mate score limits based on the search ply and return a mate score if the search is useless
		bs = ply - Score.mate;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;

		// Evaluate the position and return if the search ply limit is reached
		v = sv[ply] = eval(board);
		if (ply == Limits.Ply.max) return v;

		// Look at the transposition table and return a score if it is available
		if (tt.probe(board.key, h)) {
			s = h.score(ply);
			if (!isPv && (h.depth >= d || s <= ply - Score.mate || s >= Score.mate - ply - 1)) {
				if (h.bound == Bound.exact) return s;
				else if (h.bound == Bound.lower && s >= β) return s;
				else if (h.bound == Bound.upper && s <= α) return s;
			}
			if ((h.bound != Bound.upper && s > v) || (h.bound != Bound.lower && s < v)) v = sv[ply] = s;
		}

		// Is the position tactical: in check or looking for a mate?
		const bool tactical = (board.inCheck || α >= Score.high || β <= Score.low);
		// Is the position suspicious: in the principal variation or the static evaluation increase at this ply?
		const bool suspicious = (isPv || (ply >= 2 && sv[ply] > sv[ply - 2]));

		// Some pruning on safe position
		if (!tactical && !isPv) {
			// razoring if the eval is very bad: ~50 Elo
			const razor = α - (70 * d - 30);
			if (v <= razor) {
				if (d <= 2) return qs(α, β);
				else if (d <= 5 && qs(razor, razor + 1) <= razor) return α;
			}

			// Pruning if the position is very good & player still has a figure at least
			if (v >= β && (board.color[board.player] & ~(board.piece[Piece.pawn] | board.piece[Piece.king]))) {
				// Futility pruning : ~18 Elo.
				if (v >= β + 180 * d - 110) return β;

				// Null move: ~92 Elo
				r = 3 + d / 4 + min((v - β) / 128, 3);
				update(0);
					s = -pvs(-β, -β + 1, d - r);
				restore(0);
				if (stop) return α;
				if (s >= β) {
					if (s >= Score.high) s = β;
					tt.store(board.key, d, ply, Bound.lower, s, h.move);
					return s;
				}

				// probcut: ~8 Elo
				if (d >= 8) {
					moves.generate!(Generator.capture)(board, history);
					r = 2 + d / 4; r = d & 1 ? r | 1 : r & ~1;
					const int λ = β + 50 + 2 * d - 4 * r;
					while ((m = (i = moves.next).move) != 0) if (i.isTactical) {
						if (update(m) && (s = -qs(-λ, -λ + 1)) >= λ) s = -pvs(-λ, -λ + 1, d - r, m);
						// else s < λ
						restore(m);
						if (stop) return α;
						if (s >= λ) {
							tt.store(board.key, d, ply, Bound.lower, s, m);
							return s;
						}
					}
				}
			}
		}

		// internal iterative reduction
		const bool IIR = (h.move == 0);
		// Store the old alpha value
		const int αOld = α;

		// Generate the moves (already done at ply 0)
		if (ply == 0) moves = rootMoves;
		else moves.generate(board, history, h.move, killer[ply], refutation[previousMove & Limits.Move.mask]);

		// loop over all moves
		while ((m = (i = moves.next).move) != 0) {
			if (update(m)) {
				++nLegal; // count legal moves
				e = board.inCheck;
				if (nLegal == 1) s = -pvs(-β, -α, d + e - 1, m); // first move full width
				else {
					// late move reduction on quiet moves & quiet position: ~259 Elo
					r = (i.isTactical || e || tactical) ? 0 : reduce(d, ++quiet) + IIR;
					// reduce less on suspicious position or good move
					if (r && suspicious) --r;
					if (r && history.isGood(board[m.to], m.to)) --r;
					// late move pruning: ~27 Elo
					if (r && quiet > (4 + d * d) / (2 - suspicious)) s = bs;
					// get the score from the next ply
					else {
						// use a null window
						s = -pvs(-α - 1, -α, d + e - r - 1, m);
						// re-search using full window on a bestmove or an exact score
						if ((r && s > bs) || (α < s && s < β)) s = -pvs(-β, -α, d + e - 1, m);
					}
				}
			} else s = bs; // default score on illegal pseudo-legal move
			restore(m);
			// return immedialety on aborted search
			if (stop) return α;
			// bestscore found ?
			if (s > bs && (bs = s) > α) {
				// save it as best move
				if (ply == 0) rootMoves.setBest(m, 0);
				tt.store(board.key, d + e, ply, tt.bound(bs, β), bs, m);
				store(m, previousMove, d + e, moves);
				if (isPv) pv[ply].set(m, pv[ply + 1]);
				// return on β cutoff
				if ((α = bs) >= β) break;
			}
		}

		// no legal move => mate or stalemate
		if (nLegal == 0) {
			if (board.inCheck) return bs;
			else return 0;
		}

		// save the search result for bad position without a better move
		if (!stop && bs <= αOld) tt.store(board.key, d, ply, Bound.upper, bs, moves[0]);

		// return the best score
		return bs;
	}

	/**
	 * Iterative deepening search. Use an aspiration window to diminish the searched tree size: ~99 Elo
	 *
	 */
	void iterativeDeepening() {
		int λ, υ, δ = 5;
		int score = 0;

		for (int depth = 1; persist(depth, score); ++depth) {
			int d = depth;
			previousPV = pv[0];
			if (depth <= 4) {
				score = pvs(-Score.mate, Score.mate, depth);
			} else for (λ = score - δ, υ = score + δ; !stop; δ *= 2) {
				λ = max(-Score.mate, λ); υ = min(Score.mate, υ);
				score = pvs(λ, υ, d);
				if      (score <= λ && λ > -Score.mate) { υ = (λ + υ) / 2; λ = score - δ; d = depth; }
				else if (score >= υ && υ < Score.mate) { λ = (λ + υ) / 2; υ = score + δ; d = depth - 1; }
			    else break;
			}
			if (!option.quiet) writeUCI(depth, score);
		}
	}

	/**
	 * Condition to keep searching the next ply
	 */
	bool persist(const int depth, const int score) const nothrow {
		return !stop
		    && checkTime(0.53 * option.time.max)
		    && depth <= option.depth.max
		    && pvsNodes + qsNodes < option.nodes.max
		    && mateIn(score) > option.mate.max;
	}

	/**
	 * Clear the search state
	 */
	void clear() pure nothrow {
		tt.clear();
		foreach (k; killer) k[] = 0;
		refutation[] = 0;
		history = History.init;

	}

	/**
	 * Resize the transposition table
	 */
	void resize(const size_t size) pure nothrow { tt.resize(size); }

	/* Get the best move found so far */
	Move bestMove() pure nothrow const{ return rootMoves[0]; }

	/**
	 * Get the opponent best move
	 */
	Move hint() pure nothrow const {
		if (pv[0][1]) return pv[0][1]; // from PV
		else return previousPV[1]; // from an older PV
	}

	/**
	 * Do some initialisation for a new position
	 */
	void set() pure nothrow {
		Entry h;

		tt.probe(board.key, h);
		rootMoves.generate(board, history, h.move);
		eval.set(board);
		history.scale(8);
		tt.age();
	}

	/**
	 * Search the position according to the given search options and list of moves to search
	 */
	void go(const ref Option o, const ref Moves moves) {
		timer.start();
		option = o;
		ply = highestPly = 0;
		pvsNodes = qsNodes = 0;
		stop = false;
		pv[0].clear;
		if (moves.length > 0) rootMoves = moves;
		if (rootMoves.length == 0) rootMoves.push(0);
		else iterativeDeepening();
	}
}
