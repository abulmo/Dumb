/*
 * File eval.d
 * Evaluation function
 * © 2017-2020 Richard Delorme
 */

module eval;

import board, move, util;
import std.algorithm, std.range;
import core.bitop;

enum Score {mate = 30_000, low = -29_000, high = 29_000, big = 3_000}

/* Value: a pair of opening / engame score */
struct Value {
	int opening;
	int endgame;

	Value opBinary(string op)(const Value s) const {
		Value r = { mixin("opening " ~ op ~ " s.opening"), mixin("endgame " ~ op ~ " s.endgame") };
		return r;
	}

	Value opBinary(string op)(const int v) const {
		Value r = {mixin("opening " ~ op ~ " v"), mixin("endgame " ~ op ~ " v")};
		return r;
	}

	void opOpAssign(string op)(const Value s) {
		mixin("opening " ~ op ~ "= s.opening;");
		mixin("endgame " ~ op ~ "= s.endgame;");
	}

	void opOpAssign(string op)(const int v) {
		mixin("opening " ~ op ~ "= v;");
		mixin("endgame " ~ op ~ "= v;");
	}
}

/* Evaluation function */
final class Eval {
	struct Stack {
		Value [Color.size] value;
		int stage;
	}
	static immutable int [Piece.size] stageValue = [0, 0, 3, 3, 5, 10, 0];
	Stack [Limits.ply.max + 1] stack;
	Value [Square.size][Piece.size] weight;
	Value tempo;
	enum Adjustment {none, mean, min};
	int ply;

	static double attraction(const Square x, const Square y) {
		const int r = rank(x) - rank(y);
		const int f = file(x) - file(y);
		const int d = (r * r + f * f);
		return d == 0 ? 2.0 : 1.0 / d;
	}

	static void buildPositional(string phase)(ref Value [Square.size] w, const Square [] y, const double a, const Adjustment r) {
		double m = 0.0, k;	
		double [Square.size] p;
 	
		foreach (s; allSquares) {
			Square x = cast (Square) s;
			k = attraction(x, y[0]); foreach (i; 1 .. y.length) k = max(attraction(x, y[i]), k); p[x] = a * k;
		}

		if (r == Adjustment.mean) {
			foreach (x; allSquares) m += p[x]; m /= Square.size;
		}

		foreach(x; allSquares) mixin("w[x]." ~ phase) += cast (int) (p[x] - m);

		if (r == Adjustment.min) {
			int d = int.max; foreach (x; Square.a2 .. Square.a8) d = min(d, mixin("w[x]." ~ phase));
			foreach (x; Square.a2 .. Square.a8) mixin("w[x]." ~ phase) -= d;
		}
	}

	static void addBonus(string phase)(ref Value [Square.size] w, const Square [] squares, const int bonus) { 
		foreach (x; squares) mixin("w[x]."~phase) += bonus;
	}

	void remove(const Piece p, const Color c, const Square x) {
		stack[ply].value[c] -= weight[p][forward(x, c)];
		stack[ply].stage -= stageValue[p];
	}

	void set(const Piece p, const Color c, const Square x) {
		stack[ply].value[c] += weight[p][forward(x, c)];
		stack[ply].stage += stageValue[p];
	}

	void deplace(const Piece p, const Color c, const Square from, const Square to) {
		stack[ply].value[c] += weight[p][forward(to, c)] - weight[p][forward(from, c)];
	}

	this() {
		immutable int [] w = [
			+2, +347, +96, +15, +108, +5, +47, -42, -41, -46, -9, -31, +25, +100, +382, +404, +548, +1238, +17,
			+128, +47, +19, +46, +11, +146, +356, +381, +685, +1163, -7,
		];
		size_t i;

		with (Square) {
			buildPositional!"opening"(weight[Piece.pawn],   [d4, e4],                 w[i++], Adjustment.none);
			buildPositional!"opening"(weight[Piece.pawn],   [b8, c8, d8, e8, f8, g8], w[i++], Adjustment.min);
			buildPositional!"opening"(weight[Piece.knight], [c6, d6, e6, f6],         w[i++], Adjustment.mean);
			buildPositional!"opening"(weight[Piece.bishop], [c3, f3, c6, f6],         w[i++], Adjustment.mean);
			buildPositional!"opening"(weight[Piece.rook],   [b7, c7, d7, e7, f7, g7], w[i++], Adjustment.mean);
			buildPositional!"opening"(weight[Piece.queen],  [d4, e4, d5, e5],         w[i++], Adjustment.mean);
			buildPositional!"opening"(weight[Piece.king],   [b1, g1],                 w[i++], Adjustment.mean);
			addBonus!"opening"(weight[Piece.pawn],   [d2, e2], w[i++]) ;
			addBonus!"opening"(weight[Piece.knight], [b1, g1], w[i++]) ;
			addBonus!"opening"(weight[Piece.bishop], [c1, f1], w[i++]) ;
			addBonus!"opening"(weight[Piece.rook],   [a1, h1], w[i++]) ;
			weight[Piece.queen][d1].opening += w[i++];
			weight[ Piece.king][e1].opening += w[i++];
			foreach (p;  Piece.pawn .. Piece.king) addBonus!"opening"(weight[p], allSquares, w[i++]);
			tempo.opening = w[i++];

			buildPositional!"endgame"(weight[Piece.pawn],   [b8, c8, d8, e8, f8, g8], w[i++], Adjustment.min);
			buildPositional!"endgame"(weight[Piece.knight], [d4, e4, d5, e5],         w[i++], Adjustment.mean);
			buildPositional!"endgame"(weight[Piece.bishop], [c3, f3, c6, f6],         w[i++], Adjustment.mean);
			buildPositional!"endgame"(weight[Piece.queen],  [d4, e4, d5, e5],         w[i++], Adjustment.mean);
			buildPositional!"endgame"(weight[Piece.king],   [d4, e4, d5, e5],         w[i++], Adjustment.mean);
			foreach (p;  Piece.pawn .. Piece.king) addBonus!"endgame"(weight[p], allSquares, w[i++]);
			tempo.endgame = w[i++];
		}
	}

	void set(const Board board) {
		Stack *s = &stack[0];

		ply = 0;
		s.value[Color.white] = s.value[Color.black] = Value.init;
		s.stage = 0;

		foreach(c; Color.white .. Color.size) {
			foreach(p; Piece.pawn .. Piece.size) {
				ulong b = board.color[c] & board.piece[p];
				const n = popcnt(b);
				s.stage += stageValue[p] * n;
				while (b) {
					const Square x = popSquare(b);
					s.value[c] += weight[p][forward(x, c)];
				}
			}
		}
	}

	void update(const Board b, const Move m) {
		const Color enemy = b.player;
		const Color player = opponent(enemy);
		const Piece p = m.promotion ? Piece.pawn : toPiece(b[m.to]);
		const Piece v = b.stack[b.ply].victim;

		stack[ply + 1] = stack[ply];
		++ply;

		deplace(p, player, m.from, m.to);
		if (v) remove(v, enemy, m.to);
		if (p == Piece.pawn) {
			if (m.promotion) {
				remove(p, player, m.to);
				set(m.promotion, player, m.to);
			} else if (b.stack[b.ply - 1].enpassant == m.to) {
				remove(Piece.pawn, enemy, m.to.shift);
			}
		}
		if (p == Piece.king) {
			if (m.to == m.from + 2) deplace(Piece.rook, player, m.from.shift(+3), m.from.shift(+1));
			if (m.to == m.from - 2) deplace(Piece.rook, player, m.from.shift(-4), m.from.shift(-1));
		}
	}

	void restore() { --ply;	}

	int opCall(const Board b) const {
		const Stack *s = &stack[ply];
		const Value value = s.value[b.player] - s.value[opponent(b.player)] + tempo;

		return (value.opening * s.stage + value.endgame * (64 - s.stage)) / 64;
	}
}

