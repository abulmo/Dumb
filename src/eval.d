/**
 * File eval.d
 *
 * The evaluation function.
 *
 * The evaluation function statically evaluate the value of a position.
 * In Dumb, the evaluation function is intentionally very simple, hence the name of the engine.
 * It is based on material value and a positional square table, often abbreviated as PSQT.
 * However, these values have been optimized and already work pretty well. Also, to smooth the
 * transition between the opening and the endgame, two sets of values are used and the current
 * evaluation is interpolated according to the phase of the game (tapered evaluation function).
 * To Increase the speed of the static evaluation, the evaluation is updated after each move and
 * stored into a stack of FIFO type (First In, First Out). After a move is undone, the previous
 * value is retrieved by decreasing the index to the stack. This index is just the ply number of
 * the game.
 *
 * Authors: Richard Delorme
 * Date: 2025
 */

module eval;

import board, move, util;
import std.algorithm, std.format, std.range, std.stdio;

@safe:

/**
 * enum Score: Constant values for mate, low/high score.
 */
enum Score {mate = 30_000, low = -29_000, high = 29_000}

/**
 * struct Value: A pair of opening/endgame value.
 * TODO: use a SWAR technique to speed up the evaluation?
 */
struct Value {
	int opening; /// opening value
	int endgame; /// endgame value

	/**
	 * Operator overloading to add/sub/multiply/... the opening and the endgame
	 * part of the value struct.
	 *
	 * params: value input value
	 * returns: (this) 'op' v
	 */
	Value opBinary(string op)(const Value value) pure nothrow const {
		Value result = { mixin("opening " ~ op ~ " value.opening"), mixin("endgame " ~ op ~ " value.endgame") };
		return result;
	}

	/**
	 * Assign operator overloading to add/sub/multiply/... the opening and the endgame
	 * part of the value struct.
	 *
	 * (this) 'op'= v
	 *
	 * params: value input value
	 */
	void opOpAssign(string op)(const Value value) pure nothrow {
		mixin("opening " ~ op ~ "= value.opening;");
		mixin("endgame " ~ op ~ "= value.endgame;");
	}
}

/**
 * struct Eval: A simple but fast evaluation function based on a Material table and a Positional Square Table (PSQT).
 */
struct Eval {
	/// Stack: to keep the evaluation of each ply of the game
	struct Stack {
		Value [Color.size] value; /// evaluation for each player
		int stage; /// Game stage: 64 = opening -> 0 = endgame (no pieces except pawns on board).
	}
	static immutable int [Piece.size] stageValue = [0, 0, 3, 3, 5, 10, 0]; /// Stage value for each piece. Note: (3×2 + 3×2 + 5×2 + 10)×2 = 64
	
	/// Pieces value. By convention, a pawn in endgame = 100.
	static immutable Value [Piece.size] material = [ {  +0,   +0},
		{ +42, +100}, // pawn value: endgame pawn = 100 by convention
		{+236, +218}, // knight
		{+267, +241}, // bishop
		{+331, +441}, // rook
		{+778, +746}, // queen
		{  +0,   +0}, // king
	];
	/// Positional Square Table (PST or PSQT)
	static immutable Value [Square.size][Piece.size] positional = [
		// none
		[],
		// pawn
		[{  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0},
		 {  +0,   -6}, {  -1,   +7}, {  -6,   -3}, { -14,   +5}, { -10,  +11}, { +18,   -9}, { +21,   +1}, {  -6,  -12},
		 {  -3,   -6}, {  +3,   +0}, {  -5,   -7}, {  -1,  -12}, {  +0,   -7}, { +13,  -17}, { +22,   -5}, {  +5,  -15},
		 {  +0,   -6}, {  +7,   +3}, {  +3,  -12}, { +10,  -16}, { +12,  -17}, { +14,  -17}, { +13,   -6}, {  +0,   -9},
		 {  +9,   +7}, { +18,  +13}, { +10,   +3}, { +18,   -3}, { +30,  -11}, { +26,  -11}, { +17,   +3}, {  -3,   +5},
		 { +40,  +17}, { +48,  +27}, { +50,  +18}, { +58,  +19}, { +65,  +11}, { +69,   +4}, { +41,  +28}, { +21,  +17},
		 {+155,  +70}, {+162,  +71}, {+143,  +69}, {+161,  +51}, {+147,  +51}, {+122,  +73}, { +97,  +82}, { +70,  +77},
		 {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, {  +0,   +0}, ],
		// knight
		[{ -45,  -75}, { -17,  -39}, { -34,  -18}, { -34,   -4}, { -25,  -13}, { -22,  -25}, { -21,  -36}, { -79,  -52},
		 { -58,  -30}, { -36,   -8}, { -19,   +1}, {  -6,   +0}, {  -7,   +3}, {  -3,   -6}, { -31,   -8}, { -20,  -38},
		 { -37,  -22}, {  -9,   +8}, {  -4,  +19}, {  +9,  +22}, {  +8,  +20}, {  +1,  +13}, {  +5,   +3}, { -23,  -22},
		 { -12,   +1}, {  +7,  +11}, { +19,  +30}, { +12,  +41}, { +24,  +32}, { +18,  +35}, {  +9,  +12}, {  +1,   -7},
		 {  +0,   +5}, {  -1,  +23}, { +34,  +29}, { +39,  +34}, { +25,  +37}, { +39,  +35}, { +16,  +27}, { +13,   +6},
		 {  -5,   +0}, { +19,  +10}, { +39,  +18}, { +68,  +14}, { +92,   +6}, { +56,  +19}, { +45,   +5}, {  +0,   +2},
		 { -34,   -6}, {  +0,   +6}, { +26,  +12}, { +51,   +4}, { +43,   +1}, { +55,   -3}, {  -1,   +4}, { +11,  -11},
		 {-157,  -26}, { +39,  -23}, { +49,  -24}, { +80,  -27}, { +43,  -15}, { -17,   -1}, {-141,   -6}, { -97,  -24}, ],
		// bishop
		[{ -24,  -36}, { -13,  -21}, { -24,  -27}, { -36,   -2}, { -30,  -10}, { -33,  -19}, { -19,  -10}, { -46,  -33},
		 {  -9,  -25}, { -13,   -8}, {  -6,   -6}, { -19,   +8}, { -14,   +7}, {  -5,   -5}, {  +1,  -14}, { -17,  -36},
		 { -16,   -5}, {  -3,   -2}, {  -4,   +9}, {  -1,  +11}, {  -9,  +19}, {  -3,   +7}, {  -4,   -2}, {  -8,  -10},
		 { -14,   -2}, {  -4,   +6}, {  -1,  +20}, { +13,  +17}, { +19,  +13}, {  -7,  +23}, {  +4,   +7}, {  -7,   -5},
		 {  -3,   -1}, {  -9,  +22}, { +18,  +11}, { +37,  +12}, { +26,  +21}, { +31,  +10}, { -11,  +26}, {  -5,   +6},
		 {  +3,   +2}, { +15,   +1}, { +18,   +8}, { +41,   +9}, { +52,   +6}, { +55,   +4}, { +21,   +3}, { +21,   +4},
		 { -39,   -8}, {  -5,   +3}, {  +8,   +6}, { +19,   +7}, { +16,   +6}, {  +4,   +7}, {  -7,   +9}, { -36,   +2},
		 { -29,   +5}, {  +0,   +0}, {  +7,   -2}, { +33,  -11}, { +31,   -7}, { +28,   -9}, { -35,   +5}, { +12,   -8}, ],
		// rook
		[{ -28,  -11}, { -22,   -8}, { -23,   -1}, { -24,   -1}, { -19,   -4}, { -20,   -7}, { -16,  -12}, { -34,  -15},
		 { -44,  -15}, { -44,   -5}, { -40,   -1}, { -38,   -1}, { -37,   -4}, { -34,   -2}, { -27,   -2}, { -39,   -1},
		 { -48,   -4}, { -38,   -3}, { -42,   +4}, { -41,   +5}, { -39,   +3}, { -40,   +5}, { -25,   -3}, { -34,   -5},
		 { -27,   -6}, { -35,   +3}, { -29,   +9}, { -20,   +8}, { -14,   +2}, { -25,   +8}, {  -7,   -2}, { -15,   -9},
		 {  -1,   +0}, {  -3,   +8}, {  +7,  +11}, { +15,  +10}, { +11,  +11}, { +16,   +6}, {  +8,   +1}, {  +3,   -2},
		 { +18,   +4}, { +23,   +6}, { +31,  +10}, { +48,   +6}, { +60,   +1}, { +49,   +2}, { +39,   +0}, { +21,   +2},
		 { +17,   +8}, { +10,  +13}, { +41,   +9}, { +54,   +5}, { +48,   +6}, { +41,   +6}, { +10,  +12}, { +41,   -3},
		 { +49,  -11}, { +47,   -6}, { +49,   -1}, { +52,   -2}, { +37,   +3}, { +51,   -4}, { +23,   +0}, { +58,  -13}, ],
		// queen
		[{  -9,  -21}, { -14,  -38}, { -15,  -41}, {  +2,  -47}, {  -1,  -64}, { -19,  -72}, { -10,  -69}, { -15,  -42},
		 {  -8,  -38}, { -12,  -21}, {  +0,  -38}, {  -3,  -37}, {  +1,  -40}, {  +1,  -57}, {  -1,  -71}, {  -1,  -56},
		 { -19,  -15}, { -10,  -12}, {  -6,   -5}, { -11,   -5}, { -13,   -7}, {  -1,  -14}, {  +0,  -22}, {  -3,  -23},
		 { -12,   -8}, { -18,  +15}, {  -3,   +9}, {  -7,  +26}, {  -4,  +18}, {  -6,  +13}, {  +3,   +9}, {  -6,   +6},
		 {  -8,   +4}, { -15,  +24}, {  -8,  +27}, { -13,  +47}, {  +4,  +36}, {  +7,  +32}, {  +1,  +35}, {  +2,  +18},
		 { -15,  +14}, {  -5,  +21}, { -12,  +36}, { +10,  +35}, { +27,  +32}, { +43,  +28}, { +30,  +15}, { +19,  +23},
		 { -42,  +32}, { -47,  +47}, {  -9,  +35}, {  -4,  +47}, {  -6,  +55}, { +12,  +38}, { -21,  +45}, { +11,  +15},
		 {  +1,   +3}, { +26,  -11}, { +33,   +4}, { +33,   +8}, { +38,   +8}, { +46,   +4}, { +39,   +0}, { +20,   +3}, ],
		// king
		[{ +19,   +9}, { +39,  -29}, {  +8,  -23}, { -53,   -8}, { -17,  -31}, { -55,  -13}, { +21,  -32}, {  +0,  -39},
		 { +49,  -31}, { +19,  -13}, {  -6,   +0}, { -40,   +9}, { -42,   +6}, { -38,   +2}, {  +0,  -17}, { +11,  -32},
		 { +32,  -20}, { +20,   -6}, { -11,  +12}, { -42,  +22}, { -53,  +24}, { -54,  +15}, { -40,   +1}, { -29,  -21},
		 { +60,  -19}, { +22,   +8}, {  +5,  +21}, { -16,  +29}, { -26,  +28}, { -32,  +22}, { -46,  +14}, { -52,   -8},
		 { +48,   -1}, { +33,  +12}, { +17,  +29}, { +23,  +28}, {  -1,  +31}, {  +9,  +18}, {  +5,   +2}, { -17,  -16},
		 { +79,   -9}, { +11,   +4}, { +50,  +17}, { +52,  +18}, { +53,  +14}, { +46,   +0}, { +43,  -18}, { +44,  -25},
		 { +35,   +9}, { -22,  +32}, { -25,  +41}, { -29,  +18}, { +52,  -12}, { +33,   -7}, { -11,  -35}, { +59,  -33},
		 {-176,  +89}, { -71,  +67}, {  +3,   -2}, { -32,   +2}, { -61,   -2}, { +30,  -65}, { +24,  -22}, { +60,  -55}, ],
	];
	static immutable Value tempo = {  +3,   -1}; /// Tempo table: a constant offset for the side to move
	Stack [Limits.Ply.max + 1] stack; /// Stack: evaluation for each ply of the game
	int ply; /// Current ply to index the stack

	/**
	 * Change the evaluation when a piece is removed from the board after a capture or a promotion.
	 *
	 * params: piece = the piece to remove.
	 * params: color = the color of the piece to remove.
	 * params: x = the square's coordinate where the piece was.
	 */
	void remove(const Piece piece, const Color color, const Square x) pure nothrow {
		stack[ply].value[color] -= material[piece] + positional[piece][forward(x, color)];
		stack[ply].stage -= stageValue[piece];
	}

	/**
	 * Change the evaluation when a piece is put on the board.
	 *
	 * params: piece = the piece to put.
	 * params: color = the color of the piece to put.
	 * params: x = the square where the piece will be.
	 */
	void set(const Piece piece, const Color color, const Square x) pure nothrow {
		stack[ply].value[color] += material[piece] + positional[piece][forward(x, color)];
		stack[ply].stage += stageValue[piece];
	}

	/**
	 * Change the evaluation when a piece moves on the board.
	 *
	 * params: piece = the piece to move.
	 *         color = the color of the piece to move.
	 *         from = the origin square where the piece was.
	 *         to = the destination square.
	 */
	void relocate(const Piece piece, const Color color, const Square from, const Square to) pure nothrow {
		stack[ply].value[color] += positional[piece][forward(to, color)] - positional[piece][forward(from, color)];
	}

	/**
	 * Set the evaluation state from a new position.
	 *
	 * params: board = The position to evaluate.
	 */
	void set(const Board board) pure nothrow {
		Stack *s = &stack[0];

		ply = 0;
		*s = Stack.init;

		foreach (x; Square.a1 .. Square.size) if (toColor(board[x]) != Color.none) set(toPiece(board[x]), toColor(board[x]), x);
	}

	/**
	 * Update the evaluation state when a move is done.
	 *
	 * params: board = the chess position.
	 * params: move = the move to be made.
	 */
	void update(const Board board, const Move move) pure nothrow {
		const Color player = board.player;
		const Color enemy = opponent(player);
		const Piece piece = toPiece(board[move.from]);
		const Piece victim = toPiece(board[move.to]);

		stack[ply + 1] = stack[ply];
		++ply;

		if (board.isCastling(move)) {
			relocate(Piece.king, player, move.from, board.kingCastleTo[move.side][player]);
			relocate(Piece.rook, player, move.to, board.rookCastleTo[move.side][player]);
		} else {
			relocate(piece, player, move.from, move.to);
			if (victim) remove(victim, enemy, move.to);
			if (piece == Piece.pawn) {
				if (move.promotion) {
					remove(Piece.pawn, player, move.to);
					set(move.promotion, player, move.to);
				} else if (board.stack[board.ply].enpassant == move.to) {
					remove(Piece.pawn, enemy, move.to.enpassant);
				}
			}
		}
	}

	/**
	 * Restore the evaluation when a move is undone.
	 */
	void restore() pure nothrow { --ply; }

	/**
	 * Convert a value to an int based on the stage (opening to endgame) of the game.
	 *
	 * params: value = a Value to convert.
	 *         stage = the stage of the game.
	 * returns: an integer evaluating a chessboard position or part of it.
	 */
	static int toInt(const Value value, const int stage) pure nothrow {
		return (value.opening * stage + value.endgame * (64 - stage)) / 64;
	}

	/**
	 *  Functor: return the evaluation of the position.
	 *
	 * params: board = the board position.
	 * returns: a static evaluation of the position.
	 */
	int opCall(const Board board) pure nothrow const {
		const Stack *s = &stack[ply];
		const Value value = s.value[board.player] - s.value[opponent(board.player)] + tempo;

		return (toInt(value, s.stage) / 2) * 2; // rounded value to the nearest even number.
	}

	/**
	 * toString: return a string representation of the evaluation details.
	 *
	 * params: b = the board position
	 * returns: a table evaluating each square of the board as a string.
	 */
	string toString(const Board board) pure const {
		Square x;
		int file, rank;
		Value value;
		Piece piece;
		Color color;
		const int stage = stack[ply].stage;
		Value [Color.size][Piece.size] mat, pst;
		Value [Color.size] cmat, cpst;
		string text;

		text = "    a    b    c    d    e    f    g    h\n";
		for (rank = 7; rank >= 0; --rank)
		for (file = 0; file <= 7; ++file) {
			x = toSquare(file, rank);
			piece = toPiece(board[x]);
			color = toColor(board[x]);
			if (file == 0) text ~= format("%1d ", rank + 1);
			if (color != Color.none) {
				value = material[piece] + positional[piece][forward(x, color)];
				mat[piece][color] += material[piece];
				pst[piece][color] += positional[piece][forward(x, color)];
				cmat[color] += material[piece];
				cpst[color] += positional[piece][forward(x, color)];
				text ~= format("%+4d ",  toInt(value, stage) * (color == Color.white ? 1 : -1));
			} else text ~= format("  .  ");
			if (file == 7) text ~= format("%1d\n", rank + 1);
		}
		text ~= "    a    b    c    d    e    file    g    h\n\n";
		text ~= "                 White                      Black             \n";
		text ~= "   Piece  Material  Positional       Material  Positional\n";
		for (piece = Piece.pawn; piece < Piece.size; ++piece) 
			text ~= format("%8s  %+8d    %+8d       %+8d    %+8d\n", piece, toInt(mat[piece][Color.white], stage), toInt(pst[piece][Color.white], stage), toInt(mat[piece][Color.black], stage), toInt(pst[piece][Color.black],  stage));
		text ~= format("   total: %+8d    %+8d       %+8d    %+8d\n", toInt(cmat[Color.white], stage), toInt(cpst[Color.white], stage), toInt(cmat[Color.black], stage), toInt(cpst[Color.black], stage));
		text ~= format("   tempo: %8d\n\n", toInt(tempo, stage));

		return text;
	}
}