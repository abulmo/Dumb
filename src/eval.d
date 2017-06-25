/*
 * File eval.d
 * Evaluation function
 * © 2017 Richard Delorme
 */

module eval;

import board, move, util;
import std.algorithm;

enum Score {mate = 30000, low = -29000, high = 29000, big = 3000}

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
		mixin("opening "~op~"= s.opening;");
		mixin("endgame "~op~"= s.endgame;");
	}

	void opOpAssign(string op)(const int v) {
		mixin("opening "~op~"= v;");
		mixin("endgame "~op~"= v;");
	}
}

/* Evaluation function */
final class Eval {
	struct Weight {
		Value [Piece.size] material;
		Value [Square.size][Piece.size] positional;
		Value tempo;
	}
	struct Stack {
		Value [Color.size] value;
		int stage;
	}
	static immutable int [Piece.size] stageValue = [0, 0, 3, 3, 5, 10, 0];
	Weight coeff;
	Stack [Limits.ply.max + 1] stack;
	int ply;

	static double attraction(const Square x, const Square y) {
		const int r = rank(x) - rank(y);
		const int f = file(x) - file(y);
		const int d = (r * r + f * f);
		return d == 0 ? 2.0 : 1.0 / d;
	}

	static int scale(const double w, const double f = 1600) { return cast (int) (f * w + (w > 0 ? 0.5 : w < 0 ? -0.5 : 0.0)); }

	static void adjustPawn(string phase) (ref Value [Square.size] p) {
		foreach(x; Square.a1 .. Square.a2) mixin("p[x]."~phase) = 0;
		foreach(x; Square.a8 .. Square.size) mixin("p[x]."~phase) = 0;
		int m = int.max;
		foreach(x; Square.a2 .. Square.a8) m = min(m, mixin("p[x]."~phase));
		foreach(x; Square.a2 .. Square.a8) mixin("p[x]."~phase) -= m;
	}

	static void buildPositional(string phase)(ref Value [Square.size] positional, const Square [] y, const double a, const bool isPawn) {
		double w;	
		double [Square.size] p;
	
		foreach (Square x; Square.a1 .. Square.size) {
			w = attraction(x, y[0]);
			foreach (i; 1 .. y.length) w = max(attraction(x, y[i]), w);
			p[x] = a * w;
		}
		if (!isPawn) {
			double m = 0.0; foreach (x; Square.a1 .. Square.size) m += p[x]; m /= Square.size;
			foreach (x; Square.a1 .. Square.size) p[x] -= m;
		}
		foreach(x; Square.a1 .. Square.size) mixin("positional[x]."~phase) += scale(p[x]);
	}

	void remove(const Piece p, const Color c, const Square x) {
		Stack *s = &stack[ply];
		s.value[c] -= coeff.positional[p][forward(x, c)] + coeff.material[p];
		s.stage -= stageValue[p];
	}

	void set(const Piece p, const Color c, const Square x) {
		Stack *s = &stack[ply];
		s.value[c] += coeff.positional[p][forward(x, c)] + coeff.material[p];
		s.stage += stageValue[p];
	}

	void deplace(const Piece p, const Color c, const Square from, const Square to) {
		Stack *s = &stack[ply];
		s.value[c] -= coeff.positional[p][forward(from, c)];
		s.value[c] += coeff.positional[p][forward(to, c)];
	}

	int toCentipawns(const Value value) const {
		const Stack *s = &stack[ply];
		return (value.opening * s.stage + value.endgame * (64 - s.stage)) / 1024;
	}

	this() {
		immutable double [] w = [
			+1.00, +3.82, +4.04, +5.48, +12.38,
			+0.02, +3.47, +0.96, +0.15, +1.08, +0.05, +0.47, -0.42, -0.41, -0.46, -0.09, -0.31, +0.25,
			+0.17,
			+1.46, +3.56, +3.81, +6.85, +11.63,
			+1.28, +0.47, +0.19, +0.46, +0.11,
			-0.07
		];
		
		size_t i;
		static immutable Square [] pawnCenter = [Square.d4, Square.e4];
		static immutable Square [] pawnAdvance = [Square.b8, Square.c8, Square.d8, Square.e8, Square.f8, Square.g8];
		static immutable Square [] knightOutpost = [Square.c6, Square.d6, Square.e6, Square.f6];
		static immutable Square [] knightCenter = [Square.d4, Square.e4, Square.d5, Square.e5];
		static immutable Square [] bishopCenter = [Square.c3, Square.f3, Square.c6, Square.f6];
		static immutable Square [] rook7thRank = [Square.b7, Square.c7, Square.d7, Square.e7, Square.f7, Square.g7];
		static immutable Square [] queenCenter = [Square.d4, Square.e4, Square.d5, Square.e5];
		static immutable Square [] kingCastle = [Square.b1, Square.g1];
		static immutable Square [] kingCenter = [Square.d4, Square.e4, Square.d5, Square.e5];

		foreach(p; Piece.pawn .. Piece.king) coeff.material[p].opening = scale(w[i++]);
		coeff.material[Piece.king].opening = 0;

		buildPositional!"opening"(coeff.positional[Piece.pawn],   pawnCenter,    w[i++], true);
		buildPositional!"opening"(coeff.positional[Piece.pawn],   pawnAdvance,   w[i++], true);
		buildPositional!"opening"(coeff.positional[Piece.knight], knightOutpost, w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.bishop], bishopCenter,  w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.rook],   rook7thRank,   w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.queen],  queenCenter,   w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.king],   kingCastle,    w[i++], false);
		adjustPawn!"opening"(coeff.positional[Piece.pawn]);
		coeff.positional[Piece.pawn][Square.d2].opening   += scale(w[i]) ;
		coeff.positional[Piece.pawn][Square.e2].opening   += scale(w[i++]);
		coeff.positional[Piece.knight][Square.b1].opening += scale(w[i]);
		coeff.positional[Piece.knight][Square.g1].opening += scale(w[i++]);
		coeff.positional[Piece.bishop][Square.c1].opening += scale(w[i]);
		coeff.positional[Piece.bishop][Square.f1].opening += scale(w[i++]);
		coeff.positional[Piece.rook][Square.a1].opening   += scale(w[i]);
		coeff.positional[Piece.rook][Square.h1].opening   += scale(w[i++]);
		coeff.positional[Piece.queen][Square.d1].opening  += scale(w[i++]);
		coeff.positional[Piece.king][Square.e1].opening   += scale(w[i++]);

		coeff.tempo.opening = scale(w[i++]);

		foreach(p; Piece.pawn .. Piece.king) coeff.material[p].endgame = scale(w[i++]);
		coeff.material[Piece.king].endgame = 0;

		buildPositional!"endgame"(coeff.positional[Piece.pawn],   pawnAdvance,  w[i++], true);
		buildPositional!"endgame"(coeff.positional[Piece.knight], knightCenter, w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.bishop], bishopCenter, w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.queen],  queenCenter,  w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.king],   kingCenter,   w[i++], false);
		adjustPawn!"endgame"(coeff.positional[Piece.pawn]);
	
		coeff.tempo.endgame = scale(w[i++]);
	}

	void set(const Board board) {
		Stack *s = &stack[0];

		ply = 0;
		s.value[Color.white] = s.value[Color.black] = Value.init;
		s.stage = 0;

		foreach(Color c; Color.white .. Color.size) {
			foreach(Piece p; Piece.pawn .. Piece.size) {
				ulong b = board.color[c] & board.piece[p];
				const n = countBits(b);
				s.stage += stageValue[p] * n;
				s.value[c] += coeff.material[p] * n;
				while (b) {
					const Square x = popSquare(b);
					s.value[c] += coeff.positional[p][forward(x, c)];
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
				remove(Piece.pawn, enemy, toSquare(file(m.to), rank(m.from)));
			}
		}
		if (p == Piece.king) {
			if (m.to == m.from + 2) deplace(Piece.rook, player, cast (Square) (m.from + 3), cast (Square) (m.from + 1));
			if (m.to == m.from - 2) deplace(Piece.rook, player, cast (Square) (m.from - 4), cast (Square) (m.from - 1));
		}
	}

	void restore() { --ply;	}

	int opCall(const Board b) const {
		const Color player = b.player;
		const Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		const Value value = s.value[player] - s.value[enemy] + coeff.tempo;

		return toCentipawns(value);
	}
}

