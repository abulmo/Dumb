/*
 * File search.d
 * Best move search.
 * © 2017-2024 Richard Delorme
 */

module search;

import board, eval, move, util;
import std.stdio, std.string, std.format, std.algorithm, std.math;
import core.bitop;

/* Hash table score bound */
enum Bound {none, upper, lower, exact}

/*
 * struct Entry
 * Transposition Table Entry
 */
struct Entry {
	ulong code;
	ushort info;
	Move move;
	short value;

	/* Return the depth of the search stored in the entry */
	int depth() const { return (info >> 2) & 127; }

	/* Return the score bound of the Entry */
	Bound bound() const { return cast (Bound) (info & 3);	}

	/* Return the score, corrected for mate score */
	int score(const int ply) const { return value < Score.low ? value + ply : (value > Score.high ? value - ply : value); }

	/* Set an entry */
	void set(const Key k, const int d, const int ply, const int date, const Bound b, const int v, const Move m) {
		code = k.code;
		info = cast (ushort) (b | (d << 2) | (date << 9));
		value = cast (short) (v < Score.low ? v - ply : (v > Score.high ? v + ply : v));
		move = m;
	}
}

/*
 * class TranspositionTable
 * An hash table to store already seen positions
 */
final class TranspositionTable {
	enum size_t bucketSize = 4;
	ubyte date;
	Entry [] entry;
	size_t mask;

	/* Constructor with an initial size */
	this(size_t size) { resize(size); }

	/* Resise the hash table */
	void resize(size_t size) {
		mask = (1 << bsf(size / Entry.sizeof)) - 1;
		entry.length = mask + bucketSize;
		clear();
	}

	/* clear the hash table */
	void clear() { date = 0; foreach (ref h; entry) h = Entry.init; }

	/* make the hash table older */
	void age() {
		if (date == 127) { date = 0; foreach (ref h; entry) h.info &= 511; }
		++date;
	}

	/* Look for a position in the hash table, return true if one is found and fill the /found/ entry */
	bool probe(const Key k, ref Entry found) const {
		const size_t i = cast (size_t) (k.code & mask);
		foreach (ref h; entry[i .. i + bucketSize]) if (h.code == k.code) {
			found = h;
			return true;
		}
		return false;
	}

	/* store a position : update an existing entry or replace the entry with the worst draft of the bucket */
	void store(const Key k, const int depth, const int ply, const Bound b, const int v, const Move m) @trusted {
		const size_t i = cast (size_t) (k.code & mask);
		Entry *w = &entry[i];
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) return h.set(k, depth, ply, date, b, v, m);
			else if (w.info > h.info) w = &h;
		}
		w.set(k, depth, ply, date, b, v, m);
	}

	/* return a lower or exact bound depending on the relationship between the score and the β limit */
	Bound bound(const int v, const int β) const { return v >= β ? Bound.lower : Bound.exact; }
}

/*
 * struct Option
 * Define the kind of search: mate/node limited/time limited or depth limited or if the engine is pondering
 */
struct Option {
	struct Max(T) { T max; }
	Max!(double) time;
	Max!(ulong) nodes;
	Max!(int) depth, mate;
	bool isPondering;
}

/*
 * class Search
 * search the best move of a given postion
 */
final class Search {
	Board board;
	Eval eval;
	TranspositionTable tt;
	History history;
	Moves rootMoves;
	shared Event event;
	Option option;
	Chrono timer;
	Line [Limits.ply.max + 1] pv;
	Line line;
	Move [2][Limits.ply.max + 2] killer;
	Move [Limits.move.size] refutation;
	ubyte [32][32] reduction;
	int [Limits.ply.max + 1] sv;
	ulong pvsNodes, qsNodes;
	int ply, score;
	bool stop;

	/* constructor with initial size of 64 MB */
	this(size_t size = 64 * 1024 * 1024) {
		tt = new TranspositionTable(size);
		initReduction();
	}

	/* initialize the reduction table */
	void initReduction() {
		foreach (d; 1 .. 32)
		foreach (m; 1 .. 32) reduction[d][m] = cast (ubyte) (0.81 * std.math.log(1.0 * d) + 1.08 * std.math.log(1.0 * m));
	}

	/* return the reduction according to the depth d and the move index m */
	int reduce(const int d, const int m) const { return reduction[min(d, 31)][min(m, 31)]; }

	/* Store the move m into various heuristics: killers, refutation move, history */
	void store(Move m, const int d, const ref Moves moves) {
		if (!m.isTactical(board)) {
			if (m != killer[ply][0]) { killer[ply][1] = killer[ply][0]; killer[ply][0] = m; }
			if (ply > 0) refutation[line[ply - 1] & Limits.move.mask] = m;
			history.update(board, m, d * d, history.good);
			for (int k = 0; m != moves[k]; ++k) history.update(board, moves[k], d * d, history.bad);
		}
	}

	/* return true if enough time is available to search */
	bool checkTime(const double timeMax) const { return option.isPondering || timer.time < timeMax; }

	/* abort the search when a limit is reached or stop is received */
	bool abort() {
		if ((pvsNodes & 0x3ff) == 0) {
			if (event) {
				if (option.isPondering && event.has("ponderhit")) {
					option.isPondering = false;
					option.time.max += timer.time;
				} else if (event.has("stop")) {
					stop = true;
				} else if (event.has("isready")) {
					writeln("readyok");
					event.peek();
				}
			}
			if (!checkTime(option.time.max)) stop = true;
		}
		if (pvsNodes + qsNodes >= option.nodes.max) stop = true;
		return stop;
	}

	/* Return the distante to mate of a mate score */
	int mateIn(const int s) const { return s > Score.high ? (Score.mate + 1 - s) / 2 : (s < -Score.high ? -(Score.mate + s) / 2 : int.max); }

	/* Write uci info about the search */
	void writeUCI(const int d) {
		write("info depth ", d);
		if (abs(score) < Score.mate) { if (abs(score) > Score.high) write(" score mate ", mateIn(score)); else write(" score cp ", score); }
		writef(" nodes %s time %.0f nps %.0f", pvsNodes + qsNodes, 1000.0 * timer.time, (pvsNodes + qsNodes)  / timer.time);
		if (pv[0].n > 0) writeln(" pv ", pv[0].toString(board)); else writeln(" pv ", bestMove.toPan(board));
	}

	/* update the state of the search after a move is made */
	bool update(bool quiet = true)(const Move m) {
		line.push(m);
		if (m) eval.update(board, m);
		++ply;
		return board.update!quiet(m);
	}

	/* restore the state of the search after undoing a move */
	void restore(const Move m) {
		line.pop();
		board.restore(m);
		if (m) eval.restore();
		--ply;
	}

	/* Quiescence search */
	int qs(int α, int β) {
		int s, bs;
		Moves moves = void;
		MoveItem i = void;
		Move m;

		++qsNodes;

		// stand pat: if not in check, compute the eval as best "quiet" score, and return it if it is good enough
		if (!board.inCheck) {
			bs = eval(board);
			if ((bs > α) && (α = bs) >= β) return bs;
		// else compute mate score limits based on the search ply as best score
		} else bs = ply - Score.mate; 

		// compute the eval when the ply limit is reached
		if (ply == Limits.ply.max) return eval(board);

		// Generate all turbulent moves: capture, promotion, pawn push to 7th rank, check evasion, ...
		moves.generate!false(board, history);

		// loop over all good capture or check evasion move
		while ((m = (i = moves.next).move) != 0) if (board.inCheck || i.isTactical) {
			if (update!false(m)) s = -qs(-β, -α); else s = bs;
			restore(m);
			if (s > bs && (bs = s) > α && (α = bs) >= β) return bs;
		}

		// return the bestscore
		return bs;
	}

	/* Principal variation Search; an alphabeta variant using null window search on non PV node */
	int pvs(int α, int β, const int d) {
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

		++pvsNodes;

		// If the position is drawn return 0 */
		if (board.isDraw) return 0;

		// compute mate score limits based on the search ply and return a mate score if the search is useless
		bs = ply - Score.mate;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;

		// Evaluate the position and return if the search ply limit is reached
		v = sv[ply] = eval(board);
		if (ply == Limits.ply.max) return v;

		// Look at the transposition table and return a score if it is available
		if (tt.probe(board.key, h) && !isPv) {
			s = h.score(ply);
			if (h.depth >= d || s <= ply - Score.mate || s >= Score.mate - ply - 1) {
				if (h.bound == Bound.exact) return s;
				else if (h.bound == Bound.lower && s >= β) return s;
				else if (h.bound == Bound.upper && s <= α) return s;
			}
			if ((h.bound != Bound.upper && s > v) || (h.bound != Bound.lower && s < v)) v = sv[ply] = s;
		}

		// Is the position tactical: in check or looking for a mate?
		const bool tactical = (board.inCheck || abs(v) >= Score.big || α >= Score.big || β <= -Score.big);
		// Is the position suspicious: in the principal variation or the static evaluation increase at this ply?
		const bool suspicious = (isPv || (ply >= 2 && sv[ply] > sv[ply - 2]));

		// Some pruning on safe position
		if (!tactical && !isPv) {
			// razoring if the eval is very bad
			const int razor = α - 80 * d + 30;
			if (v <= razor) {
				if (d <= 2) return qs(α, β);
				else if (qs(razor, razor + 1) <= razor) return α;
			}

			// Pruning if the position is very good & player still has a figure at least
			if (v >= β && (board.color[board.player] & ~(board.piece[Piece.pawn] | board.piece[Piece.king]))) {
				// Futility pruning
				if (v >= β + 180 * d - 110) return β;
				// Null move
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
			}
		}

		// internal iterative reduction
		const bool IIR = (h.move == 0);
		// Store the old alpha value
		const int αOld = α;

		// Generate the moves (already done at ply 0)
		if (ply == 0) moves = rootMoves;
		else moves.generate(board, history, h.move, killer[ply], refutation[line[ply - 1] & Limits.move.mask]);

		// loop over all moves
		while ((m = (i = moves.next).move) != 0) {
			if (update(m)) {
				++nLegal; // count legal moves
				e = board.inCheck;
				if (nLegal == 1) s = -pvs(-β, -α, d + e - 1); // first move full width
				else {
					// reduce on quiet move & position.
					r = (i.isTactical || e || tactical) ? 0 : reduce(d, ++quiet) + IIR;
					// reduce less on suspicious position or good move
					if (r && suspicious) --r;
					if (r && history.isGood(board[m.to], m.to)) --r;
					// prune ?
					if (r && quiet > (4 + d * d) / (2 - suspicious)) s = bs;
					// get the score from the next ply
					else {
						// use a null window
						s = -pvs(-α - 1, -α, d + e - r - 1);
						// re-search using full window on a bestmove or an exact score
						if ((r && s > bs) || (α < s && s < β)) s = -pvs(-β, -α, d + e - 1);
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
				store(m, d + e, moves);
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

		// save the search result
		if (!stop && bs <= αOld) tt.store(board.key, d, ply, Bound.upper, bs, moves[0]);

		// return the best score
		return bs;
	}

	/* use an aspiration window to diminish the searched tree size */
	void aspiration(const int α, const int β, const int d) {
		int λ, υ, δ = 5;

		if (d <= 4) {
			score = pvs(α, β, d);
		} else for (λ = score - δ, υ = score + δ; !stop; δ *= 2) {
			λ = max(α, λ); υ = min(β, υ);
			score = pvs(λ, υ, d);
			if      (score <= λ && λ > α) { υ = (λ + υ) / 2; λ = score - δ; }
			else if (score >= υ && υ < β) { λ = (λ + υ) / 2; υ = score + δ; }
			else break;
		}
		writeUCI(d);
	}

	/* condition to keep searching the next ply */
	bool persist(const int d, const int s) const { return !stop && checkTime(0.6 * option.time.max) && d <= option.depth.max && pvsNodes + qsNodes < option.nodes.max && mateIn(s) > option.mate.max; }

	/* clear the search state */
	void clear() {
		tt.clear();
		foreach (k; killer) k[] = 0;
		history = History.init;
	}

	/* resize the transposition table */
	void resize(const size_t size) { tt.resize(size); }

	/* Get the best move found so far */
	Move bestMove() const { return rootMoves[0]; }

	/* Get the opponent best move */
	Move hint() const { return pv[0][1]; }

	/* do some initialisation for a new position */
	void set() {
		Entry h;
		tt.probe(board.key, h);
		rootMoves.generate(board, history, h.move);
		eval.set(board);
		history.scale(8);
		tt.age();
	}

	/* Search the position according to the given search options and list of moves to search */
	void go(const ref Option o, const ref Moves moves) {
		timer.start();
		option = o;
		ply = 0;
		pvsNodes = qsNodes = 0;
		stop = false;
		score = 0;
		if (moves.length > 0) rootMoves = moves;
		if (rootMoves.length == 0) rootMoves.push(0);
		else for (int d = 1; persist(d, score); ++d) aspiration(-Score.mate, Score.mate, d);
	}
}

