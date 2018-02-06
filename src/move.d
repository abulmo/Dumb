/*
 * File move.d
 * move, list of moves & sequence of moves.
 * © 2017-2018 Richard Delorme
 */

module move;

import board, util;
import std.stdio, std.ascii, std.format, std.string, std.algorithm;

/* Move */
alias Move = ushort;

Square from(const Move move) @property { return cast (Square) (move & 63); }

Square to(const Move move) @property { return cast (Square) ((move >> 6) & 63); }

Piece promotion(const Move move) @property { return cast (Piece) (move >> 12); }

string toPan(const Move move) {
	if (move.promotion) return format("%s%s%c", move.from, move.to, toChar(move.promotion));
	else if (move) return format("%s%s", move.from, move.to);
	else return "0000";
}

Move fromPan(string s) {
	if (s.length < 4) return 0;
	Piece promotion;
	Square from = toSquare(s[0..2]);
	Square to = toSquare(s[2..4]);
	if (s.length > 4) promotion = toPiece(s[4]);
	return cast (Move) (from | to << 6 | promotion << 12);
}

/* MoveItem : a move / sorting value*/
struct MoveItem {
	Move move;
	short value;
}

void insertionSort(MoveItem [] items) {
	const size_t n = items.length;

	if (n > 1) {
		foreach (i; 1 .. n) {
			size_t j;
			const tmp = items[i];
		    for (j = i ; j > 0 && tmp.value > items[j - 1].value; j--) {
				items[j] = items[j - 1];
			}
			items[j] = tmp;
		}
	}
}

/* Moves : an array of legal moves */
struct Moves {
	enum size = 256;
	MoveItem [size] item;
	size_t index;
	size_t n;
	Move ttMove;
	
	static immutable short [Piece.size] vPiece = [0, 1, 2, 3, 4, 5, 6];
	static immutable short [Piece.size] vPromotion = [0, 0, 48, 16, 32, 64, 0];
	static immutable short [Piece.size] vCapture = [0, 256, 512, 768, 1024, 1280, 1536];
	static immutable short [Square.size] center = [
		0, 1, 2, 3, 3, 2, 1, 0,
		1, 2, 3, 4, 4, 3, 2, 1, 
		2, 3, 4, 5, 5, 4, 3, 2, 
		3, 4, 5, 6, 6, 5, 4, 3,
		3, 4, 5, 6, 6, 5, 4, 3,
		2, 3, 4, 5, 5, 4, 3, 2, 
		1, 2, 3, 4, 4, 3, 2, 1, 
		0, 1, 2, 3, 3, 2, 1, 0
	];
	enum ttBonus = 10_000;

	void clear() { index = n = 0; }

	size_t length() const @property { return n; }

	void generate(bool doQuiet = true)(Board board, const Move ttMove = 0) {
		index = n = 0;
		if (board.inCheck) board.generateMoves(this); else board.generateMoves!doQuiet(this);
		foreach(ref i; item[0 .. n]) {
			if (i.move == ttMove) i.value = ttBonus;
			else {
				const p = toPiece(board[i.move.from]);
				const victim = toPiece(board[i.move.to]);
				if (victim || i.move.promotion) i.value = cast (short) (vCapture[victim] + vPromotion[i.move.promotion] - vPiece[p]);
				else if (p == Piece.king) i.value = 1;
				else i.value = center[i.move.to];
			}
		}
		insertionSort(item[0 .. n]);
		item[n] = MoveItem.init;
	}
	
	ref MoveItem next() { return item[index++]; }

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

	ref const(Move) opIndex(const size_t i) const { return item[i].move; }
	
	bool isFirst(const Move m) const { return m == item[0].move; }
}

/* struct Line: a sequence of moves */
struct Line {
	Move [Limits.ply.max] move;
	int n;

	void clear() { n = 0; }

	void push(const Move m) { move[n++] = m; }

	void push(const ref Line l) { foreach (m; l.move[0 .. l.n]) push(m); }

	void set(const Move m, const ref Line l) { clear(); push(m); push(l); }

	string toString() const {
		string s;
		foreach (m; move[0 .. n]) s ~= m.toPan() ~ " ";
		return s;
	}
}

