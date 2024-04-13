/*
 * File eval.d
 * Evaluation function
 * © 2017-2024 Richard Delorme
 */

module eval;

import board, move, util;
import std.algorithm, std.range, std.stdio;

enum Score {mate = 30_000, low = -29_000, high = 29_000, big = 3_000}

/*
 * struct Value
 * A pair of opening / endgame evaluation score
 */
struct Value {
	int opening;
	int endgame;

	/* binary operator (a+b, a-b,n etc.) */
	Value opBinary(string op)(const Value s) const {
		Value r = { mixin("opening " ~ op ~ " s.opening"), mixin("endgame " ~ op ~ " s.endgame") };
		return r;
	}

	/* operator assign operator (a += b, a-=b,n etc.) */
	void opOpAssign(string op)(const Value s) {
		mixin("opening " ~ op ~ "= s.opening;");
		mixin("endgame " ~ op ~ "= s.endgame;");
	}
}

/*
 * stuct Eval
 * A simple but fast evaluation function based on a Material table and a Positional Square Table (PST)
 */
struct Eval {
	struct Stack {
		Value [Color.size] value;
		int stage;
	}
	static immutable int [Piece.size] stageValue = [0, 0, 3, 3, 5, 10, 0];
	static immutable Value [Piece.size] material = [{  +0,   +0}, { +40, +100}, {+285, +241}, {+302, +269}, {+338, +510}, {+897, +840}, {  +0,   +0}, ];
	static immutable Value [Square.size][Piece.size] positional = [
		// none
		[],
		// pawn
		[{  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0},
		 {  +2,   -2}, {  -2,   +4}, { -12,   +4}, { -13,   +7}, { -10,   -2}, { +15,   -1}, { +23,   +0}, {  -8,   -4},
		 {  +1,   -2}, {  +2,   -2}, { -11,   -8}, {  +0,   +0}, {  -1,   -5}, {  +2,   -5}, { +23,   -3}, {  +0,   -6},
		 {  +2,   +6}, {  +1,   +3}, {  +2,   -9}, {  +5,  -11}, { +13,  -10}, { +12,   -5}, { +27,   +5}, {  -3,   -1},
		 { +13,  +21}, { +17,  +15}, {  +6,  +14}, { +17,   +0}, { +30,   +0}, { +34,   -5}, { +31,  +12}, {  +1,   +8},
		 { +42,  +45}, { +44,  +59}, { +57,  +46}, { +70,  +44}, { +81,  +30}, { +79,  +26}, { +89,  +36}, { +35,  +33},
		 {+188,  +73}, {+190,  +76}, {+220,  +70}, {+211,  +50}, {+204,  +50}, {+155,  +60}, { +64,  +84}, { +60,  +69},
		 {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, ],
		// knight
		[{ -73,  -86}, { -46,  -37}, { -70,  -25}, { -69,  -16}, { -42,  -26}, { -36,  -48}, { -40,  -46}, {-110,  -61},
		 { -76,  -46}, { -73,   +2}, { -51,   -7}, { -29,   +2}, { -32,   -3}, { -26,  -14}, { -50,   -5}, { -19,  -48},
		 { -60,  -36}, { -36,   -1}, { -28,  +17}, {  -2,  +28}, {  -6,  +28}, { -19,  +11}, { -19,   +0}, { -34,  -39},
		 { -36,   +3}, { -12,  +22}, {  -5,  +44}, {  -7,  +44}, {  +6,  +46}, {  +8,  +39}, { +28,  +20}, {  +0,  -14},
		 { -30,   +9}, { -23,  +29}, { +15,  +44}, { +30,  +45}, { +17,  +59}, { +58,  +36}, {  -2,  +39}, {  +9,  +11},
		 { -29,   +7}, { -19,  +30}, { +16,  +42}, { +58,  +33}, {+107,   +5}, { +83,  +11}, { +34,  +24}, { +10,   +5},
		 { -60,   +3}, { -26,  +25}, { +10,  +19}, { +69,   +9}, { +47,  +12}, { +72,   +1}, { +12,   +2}, { -20,  -15},
		 {-194,  -14}, {+117,  -50}, { +53,   -6}, {+101,  -22}, {+171,  -43}, {+184,  -51}, {+247,  -56}, { -94,  -46}, ],
		// bishop
		[{ -21,  -35}, {  -9,  -27}, { -28,  -36}, { -43,  -10}, { -35,  -15}, { -33,  -29}, { -29,  -27}, { -34,  -42},
		 {  +1,  -28}, { -11,  -22}, {  +5,  -15}, { -21,   +1}, { -17,   -3}, { -14,  -16}, {  +0,  -18}, {  +5,  -47},
		 { -17,  -15}, {  -5,   +0}, {  +1,   +9}, {  +2,   +9}, { -10,  +16}, {  +0,   +5}, {  +2,  -13}, {  -8,  -19},
		 { -33,   -1}, {  -8,  +20}, {  -7,  +15}, { +18,  +22}, { +20,  +19}, {  -6,  +19}, {  +4,   -2}, { -10,  -16},
		 { -22,   +4}, { -20,  +25}, { +14,  +15}, { +34,  +20}, { +31,  +22}, { +39,   -1}, { -16,  +21}, { -14,   +1},
		 { -22,  +13}, {  -8,  +27}, {  +9,  +22}, { +46,   +6}, { +52,   -1}, { +67,  +12}, { +53,   +5}, { +36,  -10},
		 { -72,  +27}, { -15,  +27}, {  -3,  +15}, {  -1,  +16}, {  -6,  +18}, { +11,   +2}, { -24,  +29}, { -52,   +4},
		 { -27,  +12}, { +26,   +5}, { +23,   +8}, { +31,  +20}, {  -8,  +13}, { +20,   -6}, {+126,  -36}, { +33,  -13}, ],
		// rook
		[{ -36,  -21}, { -32,  -14}, { -30,   -7}, { -26,   -3}, { -20,  -11}, { -23,  -12}, { -17,  -16}, { -34,  -24},
		 { -56,  -20}, { -35,  -20}, { -34,  -20}, { -37,  -17}, { -28,  -23}, { -31,  -20}, { -29,  -18}, { -21,  -31},
		 { -58,  -14}, { -48,   -6}, { -40,  -11}, { -54,   -5}, { -43,  -10}, { -44,  -11}, { -21,  -15}, { -33,  -32},
		 { -53,   +3}, { -53,  +11}, { -29,   +6}, { -22,   +3}, { -25,   +5}, { -32,   +2}, { -15,   -5}, { -36,  -15},
		 { -34,  +21}, { -24,  +19}, {  +4,  +15}, { +10,  +18}, { +20,  +12}, {  -3,  +13}, { +23,   +1}, { +12,   -9},
		 {  -7,  +25}, {  +7,  +18}, { +28,  +16}, { +59,   +8}, { +87,   -1}, { +75,   -3}, { +51,   -4}, { +43,   -1},
		 {  -3,  +28}, {  -5,  +30}, { +26,  +25}, { +58,  +21}, { +46,  +21}, { +63,   +9}, { +25,  +18}, { +60,   +1},
		 { +46,   +6}, { +52,   +8}, { +57,   +9}, { +58,  +13}, { +39,  +22}, { +45,  +11}, {+121,  -18}, { +63,   +0}, ],
		// queen
		[{ -19,  -37}, { -11,  -58}, { -20,  -57}, {  -4,  -72}, {  -5,  -84}, { -13, -112}, { -16,  -92}, {  -6,  -66},
		 { -16,  -46}, { -14,  -40}, {  -1,  -52}, {  -4,  -43}, {  -2,  -55}, {  +4,  -70}, {  -6,  -87}, { +18,  -76},
		 { -26,  -23}, { -13,  -23}, { -12,   -9}, { -19,  -14}, { -16,   -6}, {  -2,  -15}, {  +1,  -17}, {  -5,  -19},
		 { -19,  -25}, { -37,  +31}, { -14,  +15}, { -13,  +34}, { -11,  +25}, { -11,  +29}, {  +3,  +10}, { -12,  +24},
		 { -31,  +17}, { -41,  +41}, { -16,  +44}, { -21,  +58}, {  +3,  +54}, { +12,  +36}, {  +8,  +41}, {  -3,  +31},
		 { -39,  +20}, { -31,  +45}, { -26,  +58}, { +13,  +38}, { +41,  +31}, { +60,  +35}, { +58,   +4}, { +25,  +26},
		 { -55,  +44}, { -52,  +65}, { -14,  +52}, { -11,  +54}, {  -9,  +63}, { +19,  +39}, {  -2,  +46}, { +29,  +14},
		 {  -5,   +6}, { +21,  +14}, { +27,  +22}, { +50,  +13}, { +42,  +16}, { +47,  +14}, {+104,  -21}, { +88,  -18}, ],
		// king
		[{ -11,  -30}, {  +1,  -29}, { -11,  -21}, { -89,   -1}, { -14,  -45}, { -75,  -20}, { +24,  -39}, { +13,  -64},
		 { +23,  -43}, { -19,  -11}, { -32,   -6}, { -58,   +7}, { -59,   +6}, { -50,   +2}, {  +2,  -12}, {  +2,  -27},
		 {  +5,  -37}, { -18,   -4}, { -36,   +6}, { -42,  +19}, { -42,  +15}, { -45,  +10}, { -34,   +0}, { -53,   -6},
		 { -20,   +2}, { -32,  +11}, {  +1,  +10}, { +17,   +8}, { +14,  +10}, { -20,  +16}, { -29,  +14}, { -35,   +8},
		 { -23,   +3}, { -43,  +28}, { +38,   -6}, { +45,   +6}, { +65,   +0}, { +59,   +1}, {  +9,  +24}, { -27,  +21},
		 { -59,  +24}, { -28,  -25}, { -30,  +19}, { +70,   +4}, { +72,   -1}, {+104,  +14}, { +30,  +34}, { +23,  +16},
		 { -11,  -11}, { -53,  +18}, { -35,  +13}, { +40,  +26}, { +25,  +31}, { +34,  +21}, { -17,  +56}, { -73,   -8},
		 { +91,  -96}, { +20,   +5}, { -31,  +24}, { +32,  +19}, {+127,  -12}, { +43,  +42}, { -39,  +25}, {+238, -103}, ],
	];
	static immutable Value tempo = {  +4,   +2};
	Stack [Limits.ply.max + 1] stack;
	int ply;

	/* Change the evaluation when a piece is removed from the board (after a capture or a promotion */
	void remove(const Piece p, const Color c, const Square x) {
		stack[ply].value[c] -= material[p] + positional[p][forward(x, c)];
		stack[ply].stage -= stageValue[p];
	}

	/* Change the evaluation when a piece is put on the board */
	void set(const Piece p, const Color c, const Square x) {
		stack[ply].value[c] += material[p] + positional[p][forward(x, c)];
		stack[ply].stage += stageValue[p];
	}

	/* Change the evaluation when a piece move on the board */
	void deplace(const Piece p, const Color c, const Square from, const Square to) {
		stack[ply].value[c] += positional[p][forward(to, c)] - positional[p][forward(from, c)];
	}

	/* Set the evaluation state from a new position */
	void set(const Board b) {
		Stack *s = &stack[0];

		ply = 0;
		*s = Stack.init;

		foreach (x; Square.a1 .. Square.size) if (toColor(b[x]) != Color.none) set(toPiece(b[x]), toColor(b[x]), x);
	}

	/* Update the evaluation state when a move is done */
	void update(const Board b, const Move m) {
		const Color player = b.player;
		const Color enemy = opponent(player);
		const Piece p = toPiece(b[m.from]);
		const Piece v = toPiece(b[m.to]);

		stack[ply + 1] = stack[ply];
		++ply;

		if (b.isCastling(m)) {
			deplace(Piece.king, player, m.from, b.kingCastleTo[m.side][player]);
			deplace(Piece.rook, player, m.to, b.rookCastleTo[m.side][player]);
		} else {
			deplace(p, player, m.from, m.to);
			if (v) remove(v, enemy, m.to);
			if (p == Piece.pawn) {
				if (m.promotion) {
					remove(p, player, m.to);
					set(m.promotion, player, m.to);
				} else if (b.stack[b.ply].enpassant == m.to) {
					remove(Piece.pawn, enemy, m.to.enpassant);
				}
			}
		}
	}

	/* Restore the evaluation when a ove is undone */
	void restore() { --ply;	}

	/* Functor: return the evaluation function */
	int opCall(const Board b) const {
		const Stack *s = &stack[ply];
		const Value value = s.value[b.player] - s.value[opponent(b.player)] + tempo;

		return (value.opening * s.stage + value.endgame * (64 - s.stage)) / 64;
	}
}

