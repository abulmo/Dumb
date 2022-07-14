/*
 * File search.d
 * Best move search.
 * © 2017-2022 Richard Delorme
 */

module search;

import board, eval, move, util;
import std.stdio, std.string, std.format, std.algorithm, std.math;
import core.bitop;

/* Hash table score bound */
enum Bound {none, upper, lower, exact}

/* Entry Table Entry */
struct Entry {
	ulong code;
	ushort info;
	Move move;
	short value;

	int depth() const { return (info >> 2) & 127; }

	int date() const { return info >> 9; }

	Bound bound() const { return cast (Bound) (info & 3);	}

	int score(const int ply) const { return value < Score.low ? value + ply : (value > Score.high ? value - ply : value); }

	void set(const Key k, const int d, const int ply, const int date, const Bound b, const int v, const Move m) {
		code = k.code;
		info = cast (ushort) (b | (d << 2) | (date << 9));
		value = cast (short) (v < Score.low ? v - ply : (v > Score.high ? v + ply : v));
		move = m;
	}
}

/* Transposition table */
final class TranspositionTable {
	enum size_t bucketSize = 4;
	ubyte date;
	Entry [] entry;
	size_t mask;

	this(size_t size) { resize(size); }

	void resize(size_t size) {
		mask = (1 << bsf(size / Entry.sizeof)) - 1;
		entry.length = mask + bucketSize;
		clear();
	}

	void clear() { date = 0; foreach (ref h; entry) h = Entry.init; }

	void age() {
		if (date == 127) { date = 0; foreach (ref h; entry) h.info &= 511; }
		++date;
	}

	bool probe(const Key k, ref Entry found) {
		const size_t i = cast (size_t) (k.code & mask);
		foreach (ref h; entry[i .. i + bucketSize]) if (h.code == k.code) {
			found = h;
			return true;
		}
		return false;
	}

	void store(const Key k, const int depth, const int ply, const Bound b, const int v, const Move m) @trusted {
		const size_t i = cast (size_t) (k.code & mask);
		Entry *w = &entry[i];
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) return h.set(k, depth, ply, date, b, v, m);
			else if (w.info > h.info) w = &h;
		}
		w.set(k, depth, ply, date, b, v, m);
	}

	Bound bound(const int v, const int β) const { return v >= β ? Bound.lower : Bound.exact; }
}

/* Search Option */
struct Option {
	struct Max(T) { T max; } 
	Max!(double) time;
	Max!(ulong) nodes;
	Max!(int) depth, mate;
	bool isPondering;
}

/* Search */
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
	Move [2][Limits.ply.max + 2] killer;
	ubyte [32][32] reduction;
	int [Limits.ply.max + 1] sv;
	ulong pvsNodes, qsNodes;
	int ply, score;
	bool stop;

	this(size_t size = 64 * 1024 * 1024) {
		tt = new TranspositionTable(size);
		initReduction();
	}

	void initReduction() {
		foreach (d; 1 .. 32)
		foreach (m; 1 .. 32) reduction[d][m] = cast (ubyte) (0.81 * std.math.log(d) + 1.08 * std.math.log(m));
	}

	int reduce(const int d, const int m) const { return reduction[min(d, 31)][min(m, 31)]; }

	void store(Move m, const int d, const ref Moves moves) {
		if (m != killer[ply][0]) { killer[ply][1] = killer[ply][0]; killer[ply][0] = m; }
		history.update(board, m, d * d, history.good);
		for (int k = 0; m != moves[k]; ++k) history.update(board, moves[k], d * d, history.bad);
	}

	bool checkTime(const double timeMax) const { return option.isPondering || timer.time < timeMax; }

	bool abort() {
		if ((pvsNodes & 0x3ff) == 0) {
			if (event) {
				if (option.isPondering && event.has("ponderhit")) {
					option.isPondering = false;
					option.time.max += timer.time;
				} else if (event.has("stop")) {
					stop = true;
				} else if (event.has("isready")) {
					writeln("readyok"); event.peek();
				}
			}
			if (!checkTime(option.time.max)) stop = true;
		}
		if (pvsNodes + qsNodes >= option.nodes.max) stop = true;
		return stop;
	}

	int mateIn(const int s) const { return s > Score.high ? (Score.mate + 1 - s) / 2 : (s < -Score.high ? -(Score.mate + s) / 2 : int.max); }

	void writeUCI(const int d) {
		write("info depth ", d);
		if (abs(score) < Score.mate) { if (abs(score) > Score.high) write(" score mate ", mateIn(score)); else write(" score cp ", score); }
		writef(" nodes %s time %.0f nps %.0f", pvsNodes + qsNodes, 1000.0 * timer.time, (pvsNodes + qsNodes)  / timer.time);
		if (pv[0].n > 0) writeln(" pv ", pv[0].toString(board)); else writeln(" pv ", bestMove.toPan(board));
	}

	void update(bool quiet = true)(const Move m) {
		if (m) eval.update(board, m);
		board.update!quiet(m);
		++ply;
	}

	void restore(const Move m) {
		board.restore(m);
		if (m) eval.restore();
		--ply;
	}

	int qs(int α, int β) {
		int s, bs; 
		Moves moves = void;
		MoveItem i = void;
		Move m;

		++qsNodes;

		if (board.isDraw) return 0;

		bs = ply - Score.mate;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;

		if (!board.inCheck) {
			bs = eval(board);
			if ((bs > α) && (α = bs) >= β) return bs;
		}

		if (ply == Limits.ply.max) return eval(board);

		moves.generate!false(board, history);

		while ((m = (i = moves.next).move) != 0) if (board.inCheck || i.isTactical) {
			update!false(m);
				s = -qs(-β, -α);
			restore(m);
			if (s > bs && (bs = s) > α && (α = bs) >= β) return bs;
		}

		return bs;
	}

	int pvs(int α, int β, const int d) {
		const bool isPv = (α + 1 < β);
		int e, r, s, bs, v, quiet = 0;
		Moves moves = void;
		MoveItem i = void;
		Move m;
		Entry h;

		pv[ply].clear();

		if (abort()) return α;

		if (d <= 0) return qs(α, β);

		++pvsNodes;

		if (board.isDraw) return 0;

		bs = ply - Score.mate;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return α;

		if (tt.probe(board.key, h) && !isPv) {
			s = h.score(ply);
			if (h.depth >= d || s <= ply - Score.mate || s >= Score.mate - ply - 1) {
				if (h.bound == Bound.exact) return s;
				else if (h.bound == Bound.lower && s >= β) return s;
				else if (h.bound == Bound.upper && s <= α) return s;
			}
		}

		v = sv[ply] = eval(board);
		if (ply == Limits.ply.max) return v;

		const bool tactical = (board.inCheck || abs(v) >= Score.big || α >= Score.big || β <= -Score.big);
		const bool suspicious = (isPv || (ply >= 2 && sv[ply] > sv[ply - 2]));

		if (!tactical && !isPv) {
			if (v >= β + 243 * d - 124) return β;
			const int razor = α - 96 * d + 30;
			if (v <= razor) {
				if (d <= 2) return qs(α, β);
				else if (qs(razor, razor + 1) <= razor) return α;
			}

			if (v >= β && (board.color[board.player] & ~(board.piece[Piece.pawn] | board.piece[Piece.king]))) {
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

		const bool IIR = (h.move == 0);
		const int αOld = α;

		if (ply == 0) moves = rootMoves;
		else moves.generate(board, history, h.move, killer[ply]);

		while ((m = (i = moves.next).move) != 0) {
			update(m);
				e = board.inCheck;
				if (moves.isFirst(m)) s = -pvs(-β, -α, d + e - 1);
				else {
					r = (i.isTactical || e || tactical) ? 0 : reduce(d, ++quiet) + IIR;
					if (r && suspicious) --r;
					if (r && quiet > (4 + d * d) / (2 - suspicious)) s = bs;
					else {
						s = -pvs(-α - 1, -α, d + e - r - 1);
						if ((r && s > bs) || (α < s && s < β)) s = -pvs(-β, -α, d + e - 1);
					}
				}
			restore(m);
			if (stop) break;
			if (s > bs && (bs = s) > α) {
				if (ply == 0) rootMoves.setBest(m, 0);
				else tt.store(board.key, d, ply, tt.bound(bs, β), bs, m);
				if (!i.isTactical) store(m, d, moves);
				if (isPv) pv[ply].set(m, pv[ply + 1]);
				if ((α = bs) >= β) break;
			}
		}

		if (moves.length == 0) {
			if (board.inCheck) return bs;
			else return 0;
		}

		if (!stop && bs <= αOld) tt.store(board.key, d, ply, Bound.upper, bs, moves[0]);

		return bs;
	}

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

	bool persist(const int d, const int s) const { return !stop && checkTime(0.7 * option.time.max) && d <= option.depth.max && pvsNodes + qsNodes < option.nodes.max && mateIn(s) > option.mate.max; }

	void clear() {
		tt.clear();
		foreach (k; killer) k[] = 0;
		history = History.init;
	}

	void resize(const size_t size) { tt.resize(size); }

	Move bestMove() const { return rootMoves[0]; }

	Move hint() const { return pv[0][1]; }

	void set() {
		rootMoves.generate(board, history);
		eval.set(board);
		history.scale(8);
		tt.age();
	}

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

