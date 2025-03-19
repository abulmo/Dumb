/**
 * File: move.d
 *
 * Move, list of moves & sequence of moves.
 * Author: Richard Delorme
 * Copyright: MIT License
 * Date: 2025
 */

module move;

@safe:

import board, util;
import std.stdio, std.algorithm, std.ascii, std.format, std.math, std.range, std.string;

/**
 * Move type
 * A move concatenate a source square, a destination square and eventually a promotion piece
 * into an ushort (16-bit unsigned integer):
 * move =  from |  to  |promotion
 *        6 bits|6 bits|4 bits
 */
alias Move = ushort;

/**
 * Get the source square of a move by masking its 6 last bits (bits 0 .. 5).
 * params: move
 * returns: the source square of move
 */
Square from(const Move move) pure nothrow { return cast (Square) (move & 63); }

/**
 * Get the destination square of a move by shifting & masking the 6 bits 6 .. 11.
 * params: move
 * returns: the destination square of move
 */
Square to(const Move move) pure nothrow { return cast (Square) ((move >> 6) & 63); }

/**
 * Get the promotion piece of a move by shifting the bits 12 .. 15
 * params: move
 * returns: the promoted piece of move
 */
Piece promotion(const Move move) pure nothrow { return cast (Piece) (move >> 12); }

/**
 * Convert a move into a string using the Pure Algebraic Notation (PAN)
 * params: move
 * params: board
 * returns: a string containing the move using PAN.
 */
string toPan(const Move move, const Board board) pure {
	if (move.promotion) return format("%s%s%c", move.from, move.to, std.ascii.toLower(toChar(move.promotion)));
	else if (board.isCastling(move) && !board.chess960) return format("%s%s", move.from, move.from > move.to ? move.from.shift(-2) : move.from.shift(2));
	else if (move) return format("%s%s", move.from, move.to);
	else return "0000";
}

/**
 *  Convert a string using PAN into a move
 * params: s  the string to convert
 * params: board the current board
 * returns: a move
 */
Move fromPan(string s, const Board board) pure nothrow {
	if (s.length < 4) return 0;
	Piece promotion;
	Square from = toSquare(s[0..2]);
	Square to = toSquare(s[2..4]);
	if (s.length > 4) promotion = toPiece(s[4]);
	if (!board.chess960 && board[from].toPiece == Piece.king) to = (to == from.shift(2) ? from.shift(3) : to == from.shift(-2) ? from.shift(-4) : to);
	return cast (Move) (from | to << 6 | promotion << 12);
}

/**
 * Check if a move is /tactical/, ie a capture or a promotion or a pawn move to the 7th rank.
 * params: move = the move to test
 * params: board = the current board where the move would be made on.
 * returns: true if the move is /tactical/
 */
bool isTactical(Move move, const Board board) pure nothrow {
	return board[move.to] > CPiece.none // capture
	|| (toPiece(board[move.from]) == Piece.pawn && (forward(move.to, board.player).rank >= 6) // move to 7th rank or promote
	|| board.isEnpassantCapture(move)); // en passant capture
}

/**
 * Various bonuses for move sorting.
 */
enum Bonus : short { tt = 10_000, killer = 10, pushOn7thRank = 12, history = 16_384, badCapture = -32_768 }

/**
 * struct History
 * Tell if a move is more or less good according to its previous effect on search
 */
struct History {
	ushort [Square.size][CPiece.size] good, bad;

	/**
	 * Rescale the good & bad arrays by dividing all the value by a factor.
	 * params: r = the factor to reduce the scale of the arrays with (2 by default).
	 */
	void scale(const int r = 2) pure nothrow {
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) {
			good[p][x] /= r;
			bad[p][x] /= r;
		}
	}

	/**
	 * Update an history array after a move is made.
	 * params: board = the current board
	 *         move = the move made
	 *         δ = the term to increase the array with
	 *         a = the array to update.
	 */
	void update(const Board board, const Move move, const uint δ, ref ushort [Square.size][CPiece.size] a) pure nothrow {
		if ((a[board[move.from]][move.to] += δ) > Bonus.history) scale();
	}

	/**
	 * Get a bonus value from its history.
	 * params: p = the piece value
	 *         to = the destination square
	 * returns: a bonus value telling if similar moves were more or less good in the past.
	 */
	short bonus(const CPiece p, const Square to) pure nothrow const {
		if (good[p][to] + bad[p][to] == 0) return -Bonus.history / 2;
		else return cast (short) ((good[p][to] * Bonus.history) / (good[p][to] + bad[p][to]) - Bonus.history);
	}

	/**
	 * Tell if the move is good
	 * params: p = the piece value
	 *         to = the destination square
	 * returns: true if the move was more often good than bad.
	 */
	bool isGood(const CPiece p, const Square to) pure nothrow const { return good[p][to] > bad[p][to]; }
}


/**
 * struct MoveItem
 * Contain a move & sorting bonus
 * MoveItem are the element of the MoveArray structure
 */
struct MoveItem {
	Move move;
	short bonus;

	/**
	 * Return true if a move is tactical, ie if its bonus is higher than the killer bonus
	 * The bonus exclude bad tactical moves, according to their SEE.
	 * returns: true for good tactical move.
	 */
	bool isTactical() pure nothrow const { return bonus > Bonus.killer; }
}

/**
 * A simple insertion sort algorithm to sort an array of MoveItem according to their bonuses.
 * params: items = move items to sort.
 */
void insertionSort(MoveItem [] items) pure nothrow {
	foreach (i; 1 .. items.length) {
		size_t j;
		const tmp = items[i];
	    	for (j = i ; j > 0 && tmp.bonus > items[j - 1].bonus; j--) items[j] = items[j - 1];
		items[j] = tmp;
	}
}

/**
 * struct Moves
 * An array of moves.
 */
struct Moves {
	MoveItem [256] item;
	size_t index, length;

	static immutable short [Piece.size] vPiece = [0, 1, 2, 3, 4, 5, 6];
	static immutable short [Piece.size] vPromotion = [0, 0, 48, 16, 32, 64, 0];
	static immutable short [Piece.size] vCapture = [0, 256, 512, 768, 1024, 1280, 1536];

	/**
	 * Constructor: generate all the moves.
	 * params: board = the current board
	 */
	this(Board board) pure nothrow {
		board.generateMoves!(Generator.all)(this);
		item[length] = MoveItem.init;
	}

	/**
	 * Generate & sort the moves.
	 *
	 * Based on the generator, all the moves or only the captures, promotions & pawn move to the 7th rank are generated.
	 * Bonuses are assigned to each move:
	 	- the transposition table move is assigned a bonus of 10_000
		- captures are assigned a bonus according to the value of the captured piece, the promotion piece & the piece moving.
		- pawn moves to the 7th rank are assigned a bonus of 12
		- the killer moves are assigned a bonus of 10 & 8
		- the refutation move is assigned a bonus of 9
		- the history bonus is assigned according to the history of the move.
		- bad captures are assigned a bonus of -32_768
	 * Then, the moves are sorted according to their bonuses.
	 *
	 * params: generator = the type of moves to generate
	 *         board = the current board
	 *         history = the history of moves
	 *         ttMove = the transposition table move
	 *         killer = the killer moves
	 *         refutation = the refutation move
	 */
	void generate(Generator generator = Generator.all)(const Board board, const ref History history, const Move ttMove = 0, const Move [2] killer = [0, 0], const Move refutation = 0) pure nothrow {
		index = length = 0;
		board.generateMoves!generator(this);
		foreach (ref i; item[0 .. length]) {
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
				else i.bonus = history.bonus(board[i.move.from], i.move.to);
			}
		}
		insertionSort(item[0 .. length]);
		item[length] = MoveItem.init; // sentinel
	}

	/**
	 * Return the next move
	 * returns: the next move in the array
	 */
	ref MoveItem next() return pure nothrow {
		return item[index++];
	}

	/**
	 * Set the move as best, inserting it at the top of the move array.
	 * params: move = the move to set as best
	 *         i = the index where to insert the move
	 */
	void setBest(const Move move, const size_t i = 0) {
		foreach (j; 0 .. length) if (move == item[j].move) {
			const MoveItem tmp = item[j];
			foreach_reverse (k; i .. j) item[k + 1] = item[k];
			item[i] = tmp;
		}
	}

	/**
	 * Append a move to the move array
	 * params: move = the move to add
	 */
	void push(const Move move) pure nothrow {
		item[length++].move = move;
	}

	/**
	 * Append a move as from & to squares
	 * params: from = the source square
	 *         to = the destination square
	 */
	void push(const Square from, const Square to) pure nothrow {
		push(from | to << 6);
	}

	/**
	 * Append promotions.
	 * params: from = the source square
	 *         to = the destination square
	 *         doQuiet = if true, add quiet promotions (knight, rook, bishop)
	 */
	void pushPromotions(bool doQuiet = true)(const Square from, const Square to) pure nothrow {
		push(from | to << 6 | Piece.queen << 12);
		static if (doQuiet) {
			push(from | to << 6 | Piece.knight << 12);
			push(from | to << 6 | Piece.rook   << 12);
			push(from | to << 6 | Piece.bishop << 12);
		}
	}

	/**
	 * Return a move from its index using the moves[i] notation.
	 * params: i = the index of the move
	 * returns: the move at index i
	 */
	Move opIndex(const size_t i) pure nothrow const { 
		return item[i].move;
	}
}

/*
 * struct Line
 * A sequence of moves aka a variation.
 */
struct Line {
	Move [Limits.Ply.max] move;
	int n;

	/**
	 * Reset its size to 0.
	 */
	void clear() pure nothrow {
		n = 0;
	}

	/**
	 * Push a move at the end of the sequence.
	 * params: m = the move to add
	 */
	void push(const Move m) pure nothrow {
		move[n++] = m;
	}

	/**
	 * Push a sequence of moves.
	 * params: line = the sequence of moves to add
	 */
	void push(const ref Line line) pure nothrow {
		foreach (m; line.move[0 .. line.n]) push(m);
	}

	/**
	 * Create a new Line from a move and its following sequence of moves.
	 * params: m = the move to add
	 *         line = the sequence of moves to add
	 */
	void set(const Move m, const ref Line line) pure nothrow {
		clear();
		push(m);
		push(line);
	}

	/**
	 * Return a move its index in the sequence.
	 * params: i = the index of the move
	 */
	Move opIndex(const int i) const pure nothrow {
		return (0 <= i && i < n) ? move[i] : 0;
	}

	/**
	 * Remove the last move 
	 */
	void pop() {
		--n;
	}

	/**
	 * Convert the sequence to a string.
	 * params: b = the board where the moves are made
	 * returns: a string containing the sequence of moves in PAN
	 */
	string toString(Board b) const pure {
		string s;
		foreach (m; move[0 .. n]) {
			s ~= m.toPan(b) ~ " ";
			b.update(m);
		}
		foreach_reverse (m; move[0 .. n]) b.restore(m);

		return s;
	}
}
