/*
 * File search.d
 * Best move search.
 * © 2017-2019 Richard Delorme
 */

module search;

import board, eval, move, util;
import std.stdio, std.string, std.format, std.algorithm, std.math;

/* Hash table score bound */
enum Bound {none, upper, lower, exact}

/* Entry Table Entry */
struct Entry {
	ulong code;
	ushort info;
	Move move;
	short value;

	int depth() const @property { return info >> 2;	}

	Bound bound() const @property { return cast (Bound) (info & 3);	}

	int score(const int ply) const { return value < Score.low ? value + ply : (value > Score.high ? value - ply : value); }

	void set(const Key k, const int d, const int ply, const Bound b, const int v, const Move m) {
		code = k.code;
		info = cast (ushort) (b | (d << 2));
		value = cast (short) (v < Score.low ? v - ply : (v > Score.high ? v + ply : v));
		move = m;
	}
}

/* Transposition table */
final class TranspositionTable {
	enum size_t bucketSize = 4;
	Entry [] entry;
	size_t mask;

	this(size_t size) {
		resize(size);
		clear();
	}

	void resize(size_t size) {
		const size_t l = 1 << firstBit(size / Entry.sizeof);
		mask = l - 1;
		entry.length = mask + bucketSize;
	}

	void clear() { foreach (ref h; entry) h = Entry.init; }

	bool probe(const Key k, ref Entry found) {
		const size_t i = cast (size_t) (k.code & mask);
		foreach (ref h; entry[i .. i + bucketSize]) if (h.code == k.code) {
			found = h;
			return true;
		}
		return false;
	}

	void store(const Key k, const int depth, const int ply, const Bound b, const int v, const Move m) {
		const size_t i = cast (size_t) (k.code & mask);
		Entry *w = &entry[i];
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) {
				h.set(k, depth, ply, b, v, m);
				return;
			} else if (w.info > h.info) w = &h;
		}
		w.set(k, depth, ply, b, v, m);
	}

	Bound bound(const int v, const int β) const { return v >= β ? Bound.lower : Bound.exact; }
}

/* Search Option */
struct Option {
	struct Time { double max; } 
	struct Nodes { ulong max; }
	struct Depth { int max; }
	Time time;
	Nodes nodes;
	Depth depth;
	bool isPondering;
}

/* Search */
final class Search {
	Eval eval;
	TranspositionTable tt;
	Moves rootMoves;
	Line [Limits.ply.max + 1] pv;
	ulong nNodes;
	int ply, score;
	Chrono timer;
	bool stop;
	shared Event event;
	Option option;
	Board board;

	this(size_t size = 64 * 1024 * 1024) {
		eval = new Eval;
		tt = new TranspositionTable(size);
	}

	bool checkTime(const double timeMax) const { return option.isPondering || timer.time < timeMax; }

	bool abort() {
		if ((nNodes & 0x3ff) == 0) {
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
		if (nNodes >= option.nodes.max) stop = true;
		return stop;
	}

	void writeUCI(const int d, std.stdio.File f=stdout) const {
		f.write("info depth ", d, " score ");
		if (score > Score.high) f.write("mate ", (Score.mate + 1 - score) / 2);
		else if (score < -Score.high) f.write("mate ", -(Score.mate + score) / 2);
		else f.write("cp ", score);
		f.writefln(" nps %.0f time %.0f nodes %s pv %s", nNodes / timer.time, 1000 * timer.time, nNodes, pv[0]);
		f.flush();
	}

	void update(const Move m) {
		board.update(m);
		if (m) eval.update(board, m);
		++ply;
		++nNodes;
	}

	void restore(const Move m) {
		board.restore(m);
		if (m) eval.restore();
		--ply;
	}

	int qs(int α, int β) {
		int s, bs; 
		Moves moves = void;
		Move m;

		if (abort()) return α;
		if (board.isDraw) return 0;

		bs = ply - Score.mate;
		if (bs > α && (α = bs) >= β) return bs;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;			

		const αOld = α;
		if (!board.inCheck) {
			bs = eval(board);
			if ((bs > α) && (α = bs) >= β) return bs;
		}

		if (ply == Limits.ply.max) return eval(board);

		moves.generate!false(board);

		while ((m = moves.next.move) != 0) {
			update(m);
				s = -qs(-β, -α);
			restore(m);
			if (stop) break;
			if (s > bs && (bs = s) > α && (α = bs) >= β) break;
		}

		return bs;
	}

	int αβ(int α, int β, const int d) {
		const bool isPv = (α + 1 < β);
		int e, r, s, bs;
		Moves moves = void;
		Move m;
		Entry h;

		if (isPv) pv[ply].clear();

		if (d <= 0) return qs(α, β);
		if (abort()) return α;
		if (board.isDraw) return 0;

		bs = ply - Score.mate;
		if (bs > α && (α = bs) >= β) return bs;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;		

		if (tt.probe(board.key, h) && !isPv) {
			s = h.score(ply);
			if (h.depth >= d || s <= ply - Score.mate || s >= Score.mate - ply - 1) {
				if (h.bound == Bound.exact) return s;
				else if (h.bound == Bound.lower && s >= β) return s;
				else if (h.bound == Bound.upper && s <= α) return s;
			}
		}
		if (ply == Limits.ply.max) return eval(board);

		if (!isPv && !board.inCheck && eval(board) > β && α < Score.big && β > -Score.big && (board.color[board.player] & ~(board.piece[Piece.pawn] | board.piece[Piece.king]))) {
			r = 3;
			update(0);
				s = -αβ(-β, -β + 1, d - r);
			restore(0);
			if (!stop && s >= β) {
				if (s >= Score.high) s = β;
				tt.store(board.key, d, ply, Bound.lower, s, h.move);
				return s;
			}
		}

		if (h.move == 0) {
			r = isPv ? 2 : max(4, 2 + d / 4);
			if (d > r) {
				αβ(α, β, d - r);
				tt.probe(board.key, h);
			}
		}

		moves.generate(board, h.move);

		const αOld = α;

		while ((m = moves.next.move) != 0) {
			update(m);
				e = board.inCheck;
				if (moves.isFirst(m))  s = -αβ(-β, -α, d + e - 1);
				else {
					s = -αβ(-α - 1, -α, d + e - 1);
					if (α < s && s < β) s = -αβ(-β, -α, d + e - 1);
				}
			restore(m);
			if (stop) break;
			if (s > bs && (bs = s) > α) {
				tt.store(board.key, d, ply, tt.bound(bs, β), bs, m);
				if (isPv) pv[ply].set(m, pv[ply + 1]);
				if ((α = bs) >= β) return bs;
			}
		}

		if (moves.length == 0) {
			if (board.inCheck) return bs;
			else return 0;
		}

		if (!stop && bs <= αOld) tt.store(board.key, d, ply, Bound.upper, bs, moves[0]);

		return bs;
	}

	void αβRoot(int α, const int β, const int d) {
		const αOld = α;
		int e, s, bs = -Score.mate;

		pv[0].clear();

		foreach (i; 0 .. rootMoves.length) {
			Move m = rootMoves[i];
			update(m);
				e = board.inCheck;
				if (i == 0)  s = -αβ(-β, -α, d + e - 1);
				else {
					s = -αβ(-α - 1, -α, d + e - 1);
					if (α < s && s < β) s = -αβ(-β, -α, d + e - 1);
				}
			restore(m);
			if (stop) break;
			if (s > bs && (bs = s) > α) {
				rootMoves.setBest(m, 0);
				pv[0].set(m, pv[1]);
				tt.store(board.key, d, 0, tt.bound(bs, β), bs, m); 
				if ((α = bs) >= β) break;
			}
		}

		if (!stop && bs <= αOld) tt.store(board.key, d, ply, Bound.upper, bs, rootMoves[0]);
		score = bs;
	}

	void aspiration(const int α, const int β, const int d) {
		int λ, υ, δ = +10;

		if (d <= 4) {
			αβRoot(α, β, d);
		} else for (λ = score - δ, υ = score + δ; !stop; δ *= 2) {
			λ = max(α, λ); υ = min(β, υ);
			αβRoot(λ, υ, d);
			if      (score <= λ && λ > α) { υ = (λ + υ) / 2; λ = score - δ; }
			else if (score >= υ && υ < β) {	λ = (λ + υ) / 2; υ = score + δ; }
			else break;
		}
		writeUCI(d);
	}

	bool persist(const int d) const { return !stop && checkTime(0.7 * option.time.max) && d <= option.depth.max && nNodes < option.nodes.max; }

	void clear() { tt.clear(); }

	void resize(const size_t size) { tt.resize(size); }

	Move bestMove() const @property { return rootMoves[0]; }

	Move hint() const @property { return pv[0].n > 1 ? pv[0].move[1] : 0; }

	void set() {
		rootMoves.generate(board);
		eval.set(board);
	}

	void go(const ref Option o, const ref Moves moves) {
		timer.start();
		option = o;
		ply = 0;
		nNodes = 0;
		stop = false;
		if (moves.length > 0) rootMoves = moves;
		if (rootMoves.length == 0) rootMoves.push(0);
		else for (int d = 1; persist(d); ++d) aspiration(-Score.mate, Score.mate, d);
	}
}

