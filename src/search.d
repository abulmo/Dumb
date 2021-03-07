/*
 * File search.d
 * Best move search.
 * © 2017-2020 Richard Delorme
 */

module search;

import board, eval, move, util;
import std.stdio, std.string, std.format, std.algorithm, std.math;
import core.bitop;

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
	Board board;
	Eval eval;
	Moves rootMoves;
	shared Event event;
	Option option;
	Chrono timer;
	Line [Limits.ply.max + 1] pv;
	ulong nodes;
	int ply, score;
	bool stop;

	bool checkTime(const double timeMax) const { return option.isPondering || timer.time < timeMax; }

	bool abort() {
		if ((nodes & 0xfff) == 0) {
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
		if (nodes >= option.nodes.max) stop = true;
		return stop;
	}

	void writeUCI(const int d) {
		write("info depth ", d, " score ");
		if (score > Score.high) write("mate ", (Score.mate + 1 - score) / 2);
		else if (score < -Score.high) write("mate ", -(Score.mate + score) / 2);
		else write("cp ", score);
		writefln(" nodes %s time %.0f nps %.0f pv %s", nodes, 1000 * timer.time, (nodes)  / timer.time, pv[0].toString(board));
	}

	void update(bool quiet = true)(const Move m) {
		board.update!quiet(m);
		if (m) eval.update(board, m);
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

		++nodes;

		if (abort()) return α;

		if (board.isDraw) return 0;
		
		if (!board.inCheck) {
			bs = eval(board);
			if ((bs > α) && (α = bs) >= β) return bs;
		} else bs = ply - Score.mate;

		if (ply == Limits.ply.max) return eval(board);

		moves.generate!false(board);

		while ((m = (i = moves.next).move) != 0) {
			update!false(m);
				s = -qs(-β, -α);
			restore(m);
			if (stop) break;
			if (s > bs && (bs = s) > α && (α = bs) >= β) break;
		}

		return bs;
	}

	int αβ(int α, int β, const int d) {
		const bool isPv = (β > α + 1);
		int s, bs = ply - Score.mate;
		Moves moves = void;
		MoveItem i = void;
		Move m;

		pv[ply].clear();

		if (d <= 0) return qs(α, β);

		++nodes;

		if (abort()) return α;

		if (board.isDraw) return 0;

		if (ply == Limits.ply.max) return eval(board);

		if (ply == 0) moves = rootMoves;
		else moves.generate(board);

		while ((m = (i = moves.next).move) != 0) {
			update(m);
				s = -αβ(-β, -α, d - 1);
			restore(m);
			if (stop) break;
			if (s > bs && (bs = s) > α) {
				if (ply == 0) rootMoves.setBest(m, 0);
				if (isPv) pv[ply].set(m, pv[ply + 1]);
				if ((α = bs) >= β) break;
			}
		}

		if (moves.length == 0) {
			if (board.inCheck) return bs;
			else return 0;
		}

		return bs;
	}

	bool persist(const int d) const { return !stop && checkTime(0.7 * option.time.max) && d <= option.depth.max && nodes < option.nodes.max; }

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
		nodes = 0;
		stop = false;
		if (moves.length > 0) rootMoves = moves;
		if (rootMoves.length == 0) rootMoves.push(0);
		else for (int d = 1; persist(d); ++d) { 
			int s = αβ(-Score.mate, Score.mate, d);
			if (!stop)  score = s;
			writeUCI(d);
		}
	}
}

