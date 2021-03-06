/*
 * File eval.d
 * Evaluation function
 * © 2017-2020 Richard Delorme
 */

module eval;

import board, move, util;
import std.algorithm, std.range, std.stdio;

enum Score {mate = 30_000, low = -29_000, high = 29_000, big = 3_000}

/* Value: a pair of opening / endgame score */
struct Value {
	int opening;
	int endgame;

	Value opBinary(string op)(const Value s) const {
		Value r = { mixin("opening " ~ op ~ " s.opening"), mixin("endgame " ~ op ~ " s.endgame") };
		return r;
	}

	void opOpAssign(string op)(const Value s) {
		mixin("opening " ~ op ~ "= s.opening;");
		mixin("endgame " ~ op ~ "= s.endgame;");
	}
}

/* Evaluation function */
struct Eval {
	struct Stack {
		Value [Color.size] value;
		int stage;
	}
	static immutable int [Piece.size] stageValue = [0, 0, 3, 3, 5, 10, 0];
	static immutable Value [Piece.size] material = [{  +0,   +0}, { +31, +100}, {+193, +228}, {+211, +254}, {+262, +456}, {+667, +778}, {  +0,   +0}, ];
	static immutable Value [Square.size][Piece.size] positional = [
		// none
		[],
		// pawn
		[{  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, 
		 {  +0,   -5}, {  +7,   +6}, {  +1,   +1}, {  -8,   -2}, {  -8,   -2}, {  +1,   +1}, {  +7,   +6}, {  +0,   -5}, 
		 {  +2,   -8}, {  +2,   -2}, {  -5,  -10}, {  -3,   -6}, {  -3,   -6}, {  -5,  -10}, {  +2,   -2}, {  +2,   -8}, 
		 {  -1,   -2}, {  -2,   +0}, {  +0,   -7}, {  +7,  -13}, {  +7,  -13}, {  +0,   -7}, {  -2,   +0}, {  -1,   -2}, 
		 {  +4,  +16}, {  +6,  +12}, {  +7,   +2}, { +21,   -2}, { +21,   -2}, {  +7,   +2}, {  +6,  +12}, {  +4,  +16}, 
		 { +22,  +44}, { +39,  +52}, { +50,  +42}, { +63,  +33}, { +63,  +33}, { +50,  +42}, { +39,  +52}, { +22,  +44}, 
		 { +76,  +87}, { +79,  +87}, { +81,  +63}, { +72,  +56}, { +72,  +56}, { +81,  +63}, { +79,  +87}, { +76,  +87}, 
		 {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, ],
		// knight
		[{-114,  -50}, { -15,  -33}, { -35,  -21}, { -35,   -1}, { -35,   -1}, { -35,  -21}, { -15,  -33}, {-114,  -50}, 
		 { -34,  -33}, { -32,   -2}, { -17,   +9}, {  -3,  +10}, {  -3,  +10}, { -17,   +9}, { -32,   -2}, { -34,  -33}, 
		 { -21,  -21}, {  -1,   +9}, {  +3,  +15}, { +14,  +37}, { +14,  +37}, {  +3,  +15}, {  -1,   +9}, { -21,  -21}, 
		 {  +0,   -1}, { +28,  +10}, { +30,  +37}, { +12,  +41}, { +12,  +41}, { +30,  +37}, { +28,  +10}, {  +0,   -1}, 
		 { +19,   -1}, { +13,  +10}, { +44,  +37}, { +34,  +41}, { +34,  +41}, { +44,  +37}, { +13,  +10}, { +19,   -1}, 
		 {  +5,  -21}, { +30,   +9}, { +40,  +15}, { +62,  +37}, { +62,  +37}, { +40,  +15}, { +30,   +9}, {  +5,  -21}, 
		 {  -6,  -33}, {  -2,   -2}, { +26,   +9}, { +31,  +10}, { +31,  +10}, { +26,   +9}, {  -2,   -2}, {  -6,  -33}, 
		 { -73,  -50}, { -34,  -33}, { +17,  -21}, { +14,   -1}, { +14,   -1}, { +17,  -21}, { -34,  -33}, { -73,  -50}, ],
		// bishop
		[{ -28,  -23}, {  +0,  -17}, { -13,  -18}, {  -4,   -4}, {  -4,   -4}, { -13,  -18}, {  +0,  -17}, { -28,  -23}, 
		 {  +0,  -17}, {  +0,   -4}, { +13,   +5}, {  -2,   +9}, {  -2,   +9}, { +13,   +5}, {  +0,   -4}, {  +0,  -17}, 
		 { -13,  -18}, { +13,   +5}, {  +5,  +14}, { +10,  +20}, { +10,  +20}, {  +5,  +14}, { +13,   +5}, { -13,  -18}, 
		 {  -4,   -4}, {  -2,   +9}, { +10,  +20}, { +28,  +23}, { +28,  +23}, { +10,  +20}, {  -2,   +9}, {  -4,   -4}, 
		 {  -4,   -4}, {  -2,   +9}, { +10,  +20}, { +28,  +23}, { +28,  +23}, { +10,  +20}, {  -2,   +9}, {  -4,   -4}, 
		 { -13,  -18}, { +13,   +5}, {  +5,  +14}, { +10,  +20}, { +10,  +20}, {  +5,  +14}, { +13,   +5}, { -13,  -18}, 
		 {  +0,  -17}, {  +0,   -4}, { +13,   +5}, {  -2,   +9}, {  -2,   +9}, { +13,   +5}, {  +0,   -4}, {  +0,  -17}, 
		 { -28,  -23}, {  +0,  -17}, { -13,  -18}, {  -4,   -4}, {  -4,   -4}, { -13,  -18}, {  +0,  -17}, { -28,  -23}, ],
		// rook
		[{ -30,   -7}, { -25,   -1}, { -14,   +0}, {  -8,   -1}, {  -8,   -1}, { -14,   +0}, { -25,   -1}, { -30,   -7}, 
		 { -64,   -1}, { -46,   +0}, { -32,   +3}, { -36,   +0}, { -36,   +0}, { -32,   +3}, { -46,   +0}, { -64,   -1}, 
		 { -50,   +0}, { -34,   +3}, { -32,   +0}, { -33,   +5}, { -33,   +5}, { -32,   +0}, { -34,   +3}, { -50,   +0}, 
		 { -19,   -1}, { -11,   +0}, { -11,   +5}, { -10,   +5}, { -10,   +5}, { -11,   +5}, { -11,   +0}, { -19,   -1}, 
		 {  +4,   -1}, {  +9,   +0}, { +25,   +5}, { +23,   +5}, { +23,   +5}, { +25,   +5}, {  +9,   +0}, {  +4,   -1}, 
		 { +20,   +0}, { +25,   +3}, { +45,   +0}, { +51,   +5}, { +51,   +5}, { +45,   +0}, { +25,   +3}, { +20,   +0}, 
		 { +36,   -1}, { +23,   +0}, { +45,   +3}, { +51,   +0}, { +51,   +0}, { +45,   +3}, { +23,   +0}, { +36,   -1}, 
		 { +36,   -7}, { +31,   -1}, { +26,   +0}, { +31,   -1}, { +31,   -1}, { +26,   +0}, { +31,   -1}, { +36,   -7}, ],
		// queen
		[{ -31,   +0}, { -32,   -3}, { -35,   -4}, { -14,   -4}, { -14,   -4}, { -35,   -4}, { -32,   -3}, { -31,   +0}, 
		 { -22,   -3}, { -20,   -8}, {  -8,   -9}, { -13,   -1}, { -13,   -1}, {  -8,   -9}, { -20,   -8}, { -22,   -3}, 
		 { -14,   -4}, { -11,   -9}, {  -5,   +8}, { -13,  +10}, { -13,  +10}, {  -5,   +8}, { -11,   -9}, { -14,   -4}, 
		 { -10,   -4}, {  +7,   -1}, {  -2,  +10}, {  -5,  +23}, {  -5,  +23}, {  -2,  +10}, {  +7,   -1}, { -10,   -4}, 
		 {  +1,   -4}, {  +4,   -1}, {  +6,  +10}, {  +5,  +23}, {  +5,  +23}, {  +6,  +10}, {  +4,   -1}, {  +1,   -4}, 
		 { +15,   -4}, { +17,   -9}, { +24,   +8}, { +36,  +10}, { +36,  +10}, { +24,   +8}, { +17,   -9}, { +15,   -4}, 
		 {  +8,   -3}, { -12,   -8}, { +23,   -9}, { +24,   -1}, { +24,   -1}, { +23,   -9}, { -12,   -8}, {  +8,   -3}, 
		 { +18,   +0}, { +15,   -3}, { +21,   -4}, { +23,   -4}, { +23,   -4}, { +21,   -4}, { +15,   -3}, { +18,   +0}, ],
		// king
		[{ -23,  -23}, {  -4,   -7}, { -19,   -3}, { -81,  -15}, { -27,  -15}, { -74,   -3}, {  -7,   -7}, { -16,  -23}, 
		 { -16,   -7}, { -26,   -2}, { -40,   +6}, { -59,   +6}, { -66,   +6}, { -44,   +6}, {  -9,   -2}, {  -4,   -7}, 
		 {  -2,   -3}, { -30,   +6}, { -32,  +12}, { -68,  +15}, { -36,  +15}, { -43,  +12}, { -40,   +6}, { -43,   -3}, 
		 { -32,  -15}, { +21,   +6}, {  +1,  +15}, {  -1,  +21}, {  -6,  +21}, {  -1,  +15}, {  -6,   +6}, { -50,  -15}, 
		 { +24,  -15}, {  -1,   +6}, { +26,  +15}, {  -9,  +21}, { +19,  +21}, { +47,  +15}, { +19,   +6}, {  +6,  -15}, 
		 {  -2,   -3}, {+186,   +6}, { +25,  +12}, { -19,  +15}, { +16,  +15}, { +35,  +12}, { +36,   +6}, { +10,   -3}, 
		 {  +4,   -7}, { +61,   -2}, { +25,   +6}, { -15,   +6}, { +11,   +6}, { +63,   +6}, { +39,   -2}, {+149,   -7}, 
		 { +20,  -23}, { +16,   -7}, { +19,   -3}, { +18,  -15}, { +17,  -15}, { +22,   -3}, { +25,   -7}, { +12,  -23}, ],
	];
	static immutable Value tempo = {  -2,   +2};		
	Stack [Limits.ply.max + 1] stack;
	int ply;

	void remove(const Piece p, const Color c, const Square x) {
		stack[ply].value[c] -= material[p] + positional[p][forward(x, c)];
		stack[ply].stage -= stageValue[p];
	}

	void set(const Piece p, const Color c, const Square x) {
		stack[ply].value[c] += material[p] + positional[p][forward(x, c)];
		stack[ply].stage += stageValue[p];
	}

	void deplace(const Piece p, const Color c, const Square from, const Square to) {
		stack[ply].value[c] += positional[p][forward(to, c)] - positional[p][forward(from, c)];
	}

	void set(const Board b) {
		Stack *s = &stack[0];

		ply = 0;
		s.value[Color.white] = s.value[Color.black] = Value.init;
		s.stage = 0;

		foreach (x; allSquares) {
			const Piece p = toPiece(b[x]);
			const Color c = cast (Color) (toColor(b[x]) & 1);
			s.value[c] += material[p] + positional[p][forward(x, c)];
			s.stage += stageValue[p];
		}
	}

	void update(const Board b, const Move m) {
		const Color enemy = b.player;
		const Color player = opponent(enemy);
		const Piece p = m.promotion ? Piece.pawn : toPiece(b[m.to]);
		const Piece v = b.stack[b.ply].victim;

		stack[ply + 1] = stack[ply];
		++ply;

		if (b.stack[b.ply].castled) {
			deplace(Piece.king, player, m.from, b.kingCastleTo[m.side][player]);
			deplace(Piece.rook, player, m.to, b.rookCastleTo[m.side][player]);
		} else {
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
		}
	}

	void restore() { --ply;	}

	int opCall(const Board b) const {
		const Stack *s = &stack[ply];
		const Value value = s.value[b.player] - s.value[opponent(b.player)] + tempo;

		return (value.opening * s.stage + value.endgame * (64 - s.stage)) / 64;
	}
}

