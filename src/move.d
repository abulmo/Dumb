/*
 * File move.d
 * Move, list of moves & sequence of moves.
 * © 2017-2023 Richard Delorme
 */

module move;

import board, util;
import std.stdio, std.algorithm, std.ascii, std.format, std.math, std.range, std.string;

/*
 * Move type
 * A move concatenate a source square, a destination square and eventually a promotion piece
 * into an ushort:
 * move =  from |  to  |promotion
 *        6 bits|6 bits|4 bits
 */
alias Move = ushort;

/* Return the source square of a move */
Square from(const Move m) { return cast (Square) (m & 63); }

/* Return the destination square of a  move */
Square to(const Move m) { return cast (Square) ((m >> 6) & 63); }

/* Return the promotion piece of a move */
Piece promotion(const Move m) { return cast (Piece) (m >> 12); }

/* Convert a move into a string using the Pure Algebraic Notation */
string toPan(const Move m, const Board b) {
	if (m.promotion) return format("%s%s%c", m.from, m.to, std.ascii.toLower(toChar(m.promotion)));
	else if (b.isCastling(m) && !b.chess960) return format("%s%s", m.from, m.from > m.to ? m.from.shift(-2) : m.from.shift(2));
	else if (m) return format("%s%s", m.from, m.to);
	else return "0000";
}

/* Convert a string using PAN into a move */
Move fromPan(string s, const Board b) {
	if (s.length < 4) return 0;
	Piece promotion;
	Square from = toSquare(s[0..2]);
	Square to = toSquare(s[2..4]);
	if (s.length > 4) promotion = toPiece(s[4]);
	if (!b.chess960 && b[from].toPiece == Piece.king) to = (to == from.shift(2) ? from.shift(3) : to == from.shift(-2) ? from.shift(-4) : to);
	return cast (Move) (from | to << 6 | promotion << 12);
}

/* Various bonus for move sorting */
enum Bonus : short { tt = 10_000, killer = 10, pushOn7thRank = 12, history = 16_384, badCapture = -32_768 }

/*
 * struct History
 * Tell if a move is more or less good according to its previous effect on search
 */
struct History {
	ushort [Square.size][CPiece.size] good, bad;

	/* rescale the good & bad arrays, usually by dividing all the value by 2 */
	void scale(const int r = 2) {
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) {
			good[p][x] /= r;
			bad[p][x] /= r;
		}
	}

	/* update an array */
	void update(const Board board, const Move m, const uint δ, ref ushort [Square.size][CPiece.size] a) {
		if ((a[board[m.from]][m.to] += δ) > Bonus.history) scale();
	}

	/* return a bonus value from its history */
	short bonus(const CPiece p, const Square to) const {
		if (good[p][to] + bad[p][to] == 0) return -Bonus.history / 2;
		else return cast (short) ((good[p][to] * Bonus.history) / (good[p][to] + bad[p][to]) - Bonus.history);
	}
}

/*
 * struct MoveItem
 * Contain a move & sorting bonus
 */
struct MoveItem {
	Move move;
	short bonus;

	/* return true if a move is tactical, ie if its bonus is higher than the killer bonus */
	bool isTactical() const { return bonus > Bonus.killer; }
}

/* A simple insertion sort algorith to sort an array of MoveItem according to their bonuses */
void insertionSort(MoveItem [] items) {
	foreach (i; 1 .. items.length) {
		size_t j;
		const tmp = items[i];
	    	for (j = i ; j > 0 && tmp.bonus > items[j - 1].bonus; j--) items[j] = items[j - 1];
		items[j] = tmp;
	}
}

/*
 * struct Moves
 * An array of moves
 */
struct Moves {
	enum size = 256;
	MoveItem [size] item;	
	size_t index, n;
	
	static immutable short [Piece.size] vPiece = [0, 1, 2, 3, 4, 5, 6];
	static immutable short [Piece.size] vPromotion = [0, 0, 48, 16, 32, 64, 0];
	static immutable short [Piece.size] vCapture = [0, 256, 512, 768, 1024, 1280, 1536];

	/* clear the array */
	void clear() { index = n = 0; }

	/* Rethurn the length or size of the array */
	size_t length() const { return n; }

	/* Generate & sort the moves */
	void generate(bool doQuiet = true)(Board board, const ref History h, const Move ttMove = 0, const Move [2] killer = [0, 0], const Move refutation = 0) {
		index = n = 0;
		board.generateMoves!doQuiet(this);
		foreach (ref i; item[0 .. n]) {
			if (i.move == ttMove) i.bonus = Bonus.tt;
			else {
				const p = toPiece(board[i.move.from]);
				const victim = board.isEnpassantCapture(i.move) ? Piece.pawn : toPiece(board[i.move.to]);
				if (victim || i.move.promotion) {
					i.bonus = cast (short) (vCapture[victim] + vPromotion[i.move.promotion] - vPiece[p]);
					if (board.see(i.move) < 0) i.bonus += Bonus.badCapture;
				} else if (p == Piece.pawn && forward(i.move.to, board.player).rank == 6) i.bonus = Bonus.pushOn7thRank;
				else if (i.move == killer[0]) i.bonus = Bonus.killer;
				else if (i.move == refutation) i.bonus = Bonus.killer - 1;
				else if (i.move == killer[1]) i.bonus = Bonus.killer - 2;
				else i.bonus = h.bonus(board[i.move.from], i.move.to);
			}
		}
		insertionSort(item[0 .. n]);
		item[n] = MoveItem.init;
	}
	
	/* generate all the moves */
	void generateAll(Board board) {
		index = n = 0;
		board.generateMoves!true(this);
		item[n] = MoveItem.init;
	}

	/* return the next move */
	ref MoveItem next() return { return item[index++]; }

	/* set the move as best, inserting it at the top of the move array */
	void setBest(const Move m, const size_t i = 0) {
		foreach (j; 0 .. n) if (m == item[j].move) {
			const MoveItem tmp = item[j];
			foreach_reverse (k; i .. j) item[k + 1] = item[k];
			item[i] = tmp;
		}
	}

	/* Add a move to the move array */
	void push(const Move m) { item[n++].move = m; }

	/* Add a move as from & to squares */
	void push(const Square from, const Square to) { push(from | to << 6); }

	/* add all promotions */
	void pushPromotions(bool doQuiet = true)(const Square from, const Square to) {
		push(from | to << 6 | Piece.queen << 12);
		static if (doQuiet) {
			push(from | to << 6 | Piece.knight << 12);
			push(from | to << 6 | Piece.rook << 12);
			push(from | to << 6 | Piece.bishop << 12);
		}
	}

	/* return a move from its index using the moves[i] notation */
	Move opIndex(const size_t i) const { return item[i].move; }
	
	/* Check if a move is the first (ie the best) one */
	bool isFirst(const Move m) const { return m == item[0].move; }
}

/*
 * struct Line
 * A sequence of moves aka a variation
 */
struct Line {
	Move [Limits.ply.max] move;
	int n;

	/* Reset its size to 0 */
	void clear() { n = 0; }

	/* Push a move at the end of the sequence */
	void push(const Move m) { move[n++] = m; }

	/* Push a sequence of moves */
	void push(const ref Line l) { foreach (m; l.move[0 .. l.n]) push(m); }

	/* create a new Line from a move and its following sequence of moves */
	void set(const Move m, const ref Line l) { clear(); push(m); push(l); }

	/* Return a move its index in the sequence */
	Move opIndex(const int i) const { return (0 <= i && i < n) ? move[i] : 0; }
	
	/* Remove the last move */
	void pop() { --n; }
	
	/* Convert the sequence to a string */
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

