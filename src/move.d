/*
 * File move.d
 * move, list of moves & sequence of moves.
 * © 2017-2020 Richard Delorme
 */

module move;

import board, util;
import std.stdio, std.algorithm, std.ascii, std.format, std.math, std.range, std.string;

/* Move */
alias Move = ushort;

Square from(const Move m) @property { return cast (Square) (m & 63); }

Square to(const Move m) @property { return cast (Square) ((m >> 6) & 63); }

Piece promotion(const Move m) @property { return cast (Piece) (m >> 12); }

string toPan(const Move m, const Board b) {
	if (m.promotion) return format("%s%s%c", m.from, m.to, toChar(m.promotion));
	else if (b.isCastling(m) && !b.chess960) return format("%s%s", m.from, m.from > m.to ? m.from.shift(-2) : m.from.shift(2));
	else if (m) return format("%s%s", m.from, m.to);
	else return "0000";
}

Move fromPan(string s, const Board b) {
	if (s.length < 4) return 0;
	Piece promotion;
	Square from = toSquare(s[0..2]);
	Square to = toSquare(s[2..4]);
	if (s.length > 4) promotion = toPiece(s[4]);
	if (!b.chess960 && b[from].toPiece == Piece.king) to = cast (Square) (to == from + 2 ? from + 3 : to == from - 2 ? from - 4 : to);
	return cast (Move) (from | to << 6 | promotion << 12);
}

enum Bonus : short { tt = 10_000, killer = 10, history = 16_384, badCapture = -32_768 }


/* MoveItem : a move / sorting value */
struct MoveItem {
	Move move;
	short value;

	bool isTactical() const { return value > Bonus.killer; }
}

void insertionSort(MoveItem [] items) {
	foreach (i; 1 .. items.length) {
		size_t j;
		const tmp = items[i];
	    	for (j = i ; j > 0 && tmp.value > items[j - 1].value; j--) items[j] = items[j - 1];
		items[j] = tmp;
	}
}

/* Moves : an array of legal moves */
struct Moves {
	enum size = 256;
	MoveItem [size] item;	
	size_t index, n;
	
	static immutable short [Piece.size] vPiece = [0, 1, 2, 3, 4, 5, 6];
	static immutable short [Piece.size] vPromotion = [0, 0, 48, 16, 32, 64, 0];
	static immutable short [Piece.size] vCapture = [0, 256, 512, 768, 1024, 1280, 1536];

	void clear() { index = n = 0; }

	size_t length() const @property { return n; }

	void generate(bool doQuiet = true)(Board board) {
		index = n = 0;
		if (board.inCheck) board.generateMoves!true(this); else board.generateMoves!doQuiet(this);
		foreach(ref i; item[0 .. n]) {
			const p = toPiece(board[i.move.from]);
			const victim = board.isEnpassantCapture(i.move) ? Piece.pawn : toPiece(board[i.move.to]);
			if (victim || i.move.promotion) { 
				i.value = cast (short) (vCapture[victim] + vPromotion[i.move.promotion] - vPiece[p]);
			}
		}	
		insertionSort(item[0 .. n]);
		item[n] = MoveItem.init;
	}
	
	void generateAll(Board board) {
		index = n = 0;
		board.generateMoves!true(this);
		item[n] = MoveItem.init;
	}

	ref MoveItem next() return { return item[index++]; }

	void setBest(const Move m, const size_t i = 0) {
		foreach (j; 0 .. n) if (m == item[j].move) {
			const MoveItem tmp = item[j];
			foreach_reverse (k; i .. j) item[k + 1] = item[k];
			item[i] = tmp;
		}
	}

	void push(const Move m) { item[n++].move = m; }

	void push(const Square from, const Square to) { push(from | to << 6); }

	void pushPromotions(bool doQuiet = true)(const Square from, const Square to) {
		push(from | to << 6 | Piece.queen << 12);
		static if (doQuiet) {
			push(from | to << 6 | Piece.knight << 12);
			push(from | to << 6 | Piece.rook << 12);
			push(from | to << 6 | Piece.bishop << 12);
		}
	}

	Move opIndex(const size_t i) const { return item[i].move; }
}

/* struct Line: a sequence of moves */
struct Line {
	Move [Limits.ply.max] move;
	int n;

	void clear() { n = 0; }

	void push(const Move m) { move[n++] = m; }

	void push(const ref Line l) { foreach (m; l.move[0 .. l.n]) push(m); }

	void set(const Move m, const ref Line l) { clear(); push(m); push(l); }

	string toString(Board b) const {
		string s;
		foreach (m; move[0 .. n]) {
			s ~= m.toPan(b) ~ " ";
			b.update(m);
		}
		foreach_reverse (m; move[0 .. n]) b.restore(m);

		return s;
	}
}

