/**
 * File: board.d
 *
 * This module defines all basic data types for chess: color, pieces, squares. Moreover, it provides the Board class
 * to represent a chess position. It also defines all the basic operations on the chessboard. Functions to get the
 * opponent's color, the legality of a castle move, the conversion of a piece to a readable character, etc. It also
 * defines more sophisticated functions like move generation, SEE, etc.
 *
 * Chess move generation uses magic bitboard, a technique to generate move tables for sliding pieces (bishop, rook, queen).
 *
 * Authors: Richard Delorme
 * Copyright: MIT
 * Date: 2025
 */

module board;

@safe:

import move, util;
import std.algorithm, std.ascii, std.conv, std.format, std.math, std.random, std.range, std.stdio, std.string;
import core.bitop;

/**
 * struct Limits
 * Some limits for ply, game & move.
 */
struct Limits {
	/// Ply limit: maximum depth of the search tree
	enum Ply { max = 127 }
	/// Game size limit: maximum number of moves in a game
	enum Game { size = 4_096 }
	/// Move limit: biggest number to handle a move without promotion
	enum Move { size = 4_096, mask = 4_095 }
}

/**
 * enum Color
 * White & Black player's color.
 */
enum Color : ubyte { white, black, size, none }

/**
 * Get the opponent's color.
 *
 * params: c = color
 * returns: Opponent's color.
 */
Color opponent(const Color c) pure nothrow {
	return cast(Color) !c;
}

/**
 *  Translate a char to a color
 *
 * params: character = A character representing the color
 * returns: a Color
 */
Color toColor(const char character) pure nothrow {
	return cast(Color) indexOf("wb", character);
}

/**
 * enum Piece
 * The enumeration of pieces.
 */
enum Piece : ubyte { none, pawn, knight, bishop, rook, queen, king, size }

/**
 *  Convert a char to a piece
 *
 * params: character = a character representing a piece
 * returns: a Piece
 */
Piece toPiece(const char character) pure nothrow {
	return cast(Piece) indexOf(".pnbrqk", character, CaseSensitive.no);
}

/**
 *  Convert a piece to a char
 *
 * params: piece = a piece
 * returns: a character repre>senting a piece
 */
char toChar(const Piece piece) pure nothrow {
	return ".PNBRQK?"[piece];
}

/**
 * enum CPiece
 * The enumeration of colored Pieces.
 */
enum CPiece : ubyte { none, _, wpawn, bpawn, wknight, bknight, wbishop, bbishop, wrook, brook, wqueen, bqueen, wking, bking, size }

/**
 * Convert a Piece & Color to a CPiece
 *
 * params: piece = a piece
 *         color = a color
 * returns: a colored piece
 */
CPiece toCPiece(const Piece piece, const Color color) pure nothrow {
	return cast(CPiece)(2 * piece + color);
}

/**
 * Convert a char to a CPiece.
 * params: character = a character representing a colored piece.
 * returns: a colored piece.
 */
CPiece toCPiece(const char character) pure nothrow {
	return cast(CPiece) indexOf("._PpNnBbRrQqKk", character);
}

/**
 * Get the Color of the CPiece.
 * params cpiece = a colored piece.
 * returns the color of the piece
 */
Color toColor(const CPiece cpiece) pure nothrow {
	static immutable Color[CPiece.size] c = iota(CPiece.none, CPiece.size).map!(x => x < CPiece.wpawn ? Color.none : cast(Color)(x & 1)).array;
	return c[cpiece];
}

/**
 * Get the Piece of the CPiece
 *
 * params: cpiece = a colored piece
 * returns: a piece
 */
Piece toPiece(const CPiece cpiece) pure nothrow {
	return cast(Piece)(cpiece / 2);
}

/**
 * enum Square
 * The enumeration of square coordinates
 */
enum Square : ubyte {
	a1, b1, c1, d1, e1, f1, g1, h1,
	a2, b2, c2, d2, e2, f2, g2, h2,
	a3, b3, c3, d3, e3, f3, g3, h3,
	a4, b4, c4, d4, e4, f4, g4, h4,
	a5, b5, c5, d5, e5, f5, g5, h5,
	a6, b6, c6, d6, e6, f6, g6, h6,
	a7, b7, c7, d7, e7, f7, g7, h7,
	a8, b8, c8, d8, e8, f8, g8, h8,
	size, none,
}

/**
 *  Shift a coordinate by some amount.
 *
 * params: x = square
 *         δ = shift amount
 * returns: the shifted square's coordinate.
 */
Square shift(const Square x, const int δ) pure nothrow {
	return cast(Square)(x + δ);
}

/**
 * Compute the enpassant coordinate, ie the coordinate of the captured pawn from the destination of the capturing pawn.
 *
 * params: x = square
 * returns: the enpassant square
 */
Square enpassant(const Square x) pure nothrow {
	return cast(Square)(x ^ 8);
}

/**
 * Mirror a square for black or white pawn advance.
 *
 * params: x = square
 *         color = color
 * returns: the mirrored square
 */
Square forward(const Square x, const Color color) pure nothrow {
	return cast(Square)(x ^ (56 * color));
}

/**
 * Mirror a square.
 *
 * params: x = square
 * returns: the mirrored square
 */
Square mirror(const Square x) pure nothrow {
	return cast(Square)(x ^ 56);
}

/**
 * Get the rank of a square.
 *
 * params: x = square
 * returns: the rank of the square
 */
int rank(const Square x) pure nothrow {
	return x >> 3;
}

/**
 * Get the file of a square.
 *
 * params: x = square
 * returns: the file of the square
 */
int file(const Square x) pure nothrow {
	return x & 7;
}

/**
 * Convert a square coordinate to a bitboard with 1 bit set.
 *
 * params: x = square
 * returns: the bitboard
 */
ulong toBit(const Square x) pure nothrow {
	return 1UL << x;
}

/**
 * Build a square from its file and rank.
 *
 * params: f = file
 *         r = rank
 * returns: the square
 */
Square toSquare(const int f, const int r) pure nothrow {
	return cast(Square)((r << 3) + f);
}

/**
 * Build a square from a string like a1, a2, ..., h8.
 *
 * params: s = string
 * returns: the square
 */
Square toSquare(string s) pure nothrow {
	return toSquare(s[0] - 'a', s[1] - '1');
}

/**
 * Get a square coordinate from a bitboard & remove it from the bitboard.
 *
 * params: b = bitboard
 * returns: the square
 */
Square popSquare(ref ulong b) pure nothrow {
	return cast(Square) popBit(b);
}

/**
 *  Get a square from a bitboard.
 *
 * params: b = bitboard
 * returns: the square
 */
Square firstSquare(const ulong b) pure nothrow {
	return cast(Square) bsf(b);
}

/** Rank masks */
immutable ulong [] rankMask = [ 0x00000000000000ff, 0x000000000000ff00, 0x0000000000ff0000, 0x00000000ff000000, 0x000000ff00000000, 0x0000ff0000000000, 0x00ff000000000000, 0xff00000000000000 ];

/** File masks */
immutable ulong [] fileMask = [ 0x0101010101010101, 0x0202020202020202, 0x0404040404040404, 0x0808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080 ];

/**
 * enum Castling
 * Castling side enumeration.
 */
enum Castling {
	kingSide,
	queenSide
}

/**
 * Get the side of a castling move
 * params: move = move
 * returns: the side of the castling move
 */
Castling side(const Move move) pure nothrow {
	return move.to > move.from ? Castling.kingSide : Castling.queenSide;
}

/**
 * Convert a char to a castle bitboard.
 *
 * params: character = a character representing a castling side
 * returns: the castling bitboard
 */
ulong toCastling(const char character) pure nothrow {
	with (Square) {
		static immutable Square  [] rooks = [h1, a1, h8, a8, a1, b1, c1, d1, e1, f1, g1, h1, a8, b8, c8, d8, e8, f8, g8, h8];
		size_t i = indexOf("KQkqABCDEFGHabcdefgh", character);
		return i == -1 ? 0 : rooks[i].toBit;
	}
}

/**
 * struct Key
 * Compute the Zobrist's key of a chess position.
 */
struct Key {
	ulong code; /// The Zobrist key

	static immutable ulong[Square.size][CPiece.size] square; /// Keys for each pieces for each squares
	static immutable ulong[Square.size] castling; /// Keys for castling state
	static immutable ulong[Square.none + 1] enpassant; /// Keys for enpassant state
	static immutable ulong[Color.size] color; /// Keys for each player colors
	static immutable ulong play; /// Key for turn change

	/**
	 *  Constructor of static keys
	 *
	 *  Fill the keys'tables with random value.
	 */
	shared static this() pure {
		Mt19937 r;
		r.seed(19_937);
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) square[p][x] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a1 .. Square.a2) castling[x] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a8 .. Square.size) castling[x] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a3 .. Square.a4) enpassant[x] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a6 .. Square.a7) enpassant[x] = uniform(ulong.min, ulong.max, r);
		foreach (c; Color.white .. Color.size) color[c] = uniform(ulong.min, ulong.max, r);
		play = color[Color.white] ^ color[Color.black];
	}

	/**
	 * Compute a key of a chess position from scratch
	 *
	 * params: board = board
	 */
	void set(const Board board) pure nothrow {
		const Board.Stack* s = &board.stack[board.ply];
		ulong rooks = s.castling;

		code = color[board.player];
		foreach (Square x; Square.a1 .. Square.size) code ^= square[board[x]][x];
		code ^= enpassant[s.enpassant];
		while (rooks) code ^= castling[popSquare(rooks)];
	}

	/**
	 * Update the Key after a move
	 *
	 * params: board = board
	 *         move = move
	 */
	void update(const Board board, const Move move) pure nothrow {
		Square x = Square.none;
		const Color player = board.player;
		const Color enemy = opponent(player);
		const CPiece p = board[move.from];
		const Board.Stack* s = &board.stack[board.ply];
		ulong rooks = 0;

		code = s.key.code;
		code ^= play;
		if (move != 0) {
			if (board.isCastling(move)) {
				CPiece r = toCPiece(Piece.rook, player);
				code ^= square[p][move.from] ^ square[p][board.kingCastleTo[move.side][player]];
				code ^= square[r][move.to] ^ square[r][board.rookCastleTo[move.side][player]];
				rooks = board.piece[Piece.rook] & board.color[player];
			}
			else {
				code ^= square[p][move.from] ^ square[p][move.to];
				code ^= square[board[move.to]][move.to];
				rooks = (move.from.toBit | move.to.toBit) & board.piece[Piece.rook];
				if (toPiece(p) == Piece.pawn) {
					if (move.promotion) code ^= square[p][move.to] ^ square[toCPiece(move.promotion, player)][move.to];
					else if (s.enpassant == move.to) code ^= square[toCPiece(Piece.pawn, enemy)][move.to.enpassant];
					else if (abs(move.to - move.from) == 16 && (board.mask[move.to].enpassant & (board.color[enemy] & board.piece[Piece.pawn]))) x = move.to.enpassant;
				}
				else if (toPiece(p) == Piece.king)
					rooks |= board.piece[Piece.rook] & board.color[player];
			}
			rooks &= s.castling;
			while (rooks) code ^= castling[popSquare(rooks)];
		}
		code ^= enpassant[s.enpassant] ^ enpassant[x];
	}
}

/**
 * struct Attack
 * Build magic attack tables;
 */
struct Attack {
	ulong mask; /// Mask for the occupancy bitboard
	ulong magic; /// Magic value
	ulong shift; /// Shift value
	ulong[] attack; /// Attack table

	version (USE_BMI) {
		/**
		 * Magic index using the pext instruction (for x86-64 with fast BMI2 only)
		 *
		 * params: o = occupancy bitboard
		 * returns: the index in the attack table
		 */
		ulong index(const ulong o) pure nothrow const {
			return pext(o, mask);
		}
	} else {
		/**
		 * Magic index using magic multiplication and bit shift.
		 *
		 * params: o = occupancy bitboard
		 * returns: the index in the attack table
		 */
		ulong index(const ulong o) pure nothrow const {
			return ((o & mask) * magic) >> shift;
		}
	}

	/**
	 * Compute an attack entry
	 *
	 * params: x = square
	 *         o = occupancy bitboard
	 *         dir = directions
	 * returns: the attack bitboard
	 */
	static ulong computeAttack(const Square x, const ulong o, const int[2][4] dir) pure nothrow {
		ulong a, b;

		foreach (d; dir) {
			for (int r = rank(x) + d[0], f = file(x) + d[1]; 0 <= r && r < 8 && 0 <= f && f < 8; r += d[0], f += d[1]) {
				a |= (b = 1UL << toSquare(f, r));
				if ((o & b) != 0) break;
			}
		}
		return a;
	}

	/**
	 * Constructor: Fill the attack table
	 *
	 * params: x = square
	 *         mk = mask
	 *         mg = magic
	 *         dir = directions
	 *
	 */
	this(const Square x, const ulong mk, const ulong mg, const int[2][4] dir) pure nothrow {
		ulong o;

		mask = mk;
		magic = mg;
		shift = 64 - popcnt(mk);
		attack.length = 1UL << popcnt(mk);
		do {
			attack[index(o)] = computeAttack(x, o, dir);
		} while ((o = ((o - mk) & mk)) != 0);
	}
}

/**
 * struct Mask
 * a set of bitmasks
 */
struct Mask {
	ulong diagonal; /// Diagonal masks
	ulong antidiagonal; /// Antidiagonal masks
	ulong file; /// File masks
	ulong rank; /// Rank masks
	ulong[Color.size] pawnAttack; /// Pawn attack masks
	ulong[Color.size] push; /// Pawn push masks
	ulong enpassant; /// Enpassant mask
	ulong knight; /// Knight mask
	ulong king; /// King masks
	ulong[Square.size] between; /// A mask between two square location
	Attack bishop; /// Attack table for bishops./// Pieces value. By convention, a pawn in endgame = 100.
	Attack rook; /// Attack table for rooks.
	ubyte castling; /// Castling mask.
}

/**
 * enum Generator: capture only, quiet only, all moves
 * Move Generator type
 */
enum Generator { capture = 1, quiet = 2, all = 3 }

/**
 * Verify if a move generator is a subset of another one.
 * This is used to check if a only captures or all moves should be generated.
 *
 * params: generator = generator.
 *         type = type.
 * returns: true if generator is a subset of type.
 */
bool as(Generator generator, Generator type) pure nothrow {
	return (generator & type) == type;
}

/**
 * class Board
 * The chessboard representation.
 */
final class Board {
	static immutable Mask[Square.size] mask; /// Mask tables
	static immutable int[Piece.size] seeValue = [0, 1, 3, 3, 5, 9, 300]; /// SEE values in pawns
	static immutable ulong[Color.size] rank8 = [rankMask[7], rankMask[0]]; /// Promotion rank
	static immutable ulong[Color.size] rank7 = [rankMask[6], rankMask[1]]; /// 7th rank
	static immutable int[Color.size] pushTable = [8, -8]; /// Move push
	static immutable Square[2][Color.size] kingCastleTo = [[Square.g1, Square.g8], [Square.c1, Square.c8]]; /// Castling squares: king destination
	static immutable Square[2][Color.size] rookCastleTo = [[Square.f1, Square.f8], [Square.d1, Square.d8]]; /// Castling squares: rook destination
	/// Bishop magic values
	static immutable ulong[Square.size] bishopMagic = [
		0x88b030028800d040, 0x018242044c008010, 0x0010008200440000, 0x4311040888800a00, 0x001910400000410a, 0x2444240440000000, 0x0cd2080108090008, 0x2048242410041004,
		0x8884441064080180, 0x00042131420a0240, 0x0028882800408400, 0x204384040b820200, 0x0402040420800020, 0x0000020910282304, 0x0096004b10082200, 0x4000a44218410802,
		0x0808034002081241, 0x00101805210e1408, 0x9020400208010220, 0x000820050c010044, 0x0024005480a00000, 0x0000200200900890, 0x808040049c100808, 0x9020202200820802,
		0x0410282124200400, 0x0090106008010110, 0x8001100501004201, 0x0104080004030c10, 0x0080840040802008, 0x2008008102406000, 0x2000888004040460, 0x00d0421242410410,
		0x8410100401280800, 0x0801012000108428, 0x0000402080300b04, 0x0c20020080480080, 0x40100e0201502008, 0x4014208200448800, 0x4050020607084501, 0x1002820180020288,
		0x800610040540a0c0, 0x0301009014081004, 0x2200610040502800, 0x0300442011002800, 0x0001022009002208, 0x0110011000202100, 0x1464082204080240, 0x0021310205800200,
		0x0814020210040109, 0xc102008208c200a0, 0xc100702128080000, 0x0001044205040000, 0x0001041002020000, 0x4200040408021000, 0x004004040c494000, 0x2010108900408080,
		0x0000820801040284, 0x0800004118111000, 0x0203040201108800, 0x2504040804208803, 0x0228000908030400, 0x0010402082020200, 0x00a0402208010100, 0x30c0214202044104
	];
	/// Rook magic values
	static immutable ulong[Square.size] rookMagic = [
		0x808000645080c000, 0x208020001480c000, 0x4180100160008048, 0x8180100018001680, 0x4200082010040201, 0x8300220400010008, 0x3100120000890004, 0x4080004500012180,
		0x01548000a1804008, 0x4881004005208900, 0x0480802000801008, 0x02e8808010008800, 0x08cd804800240080, 0x8a058002008c0080, 0x0514000c480a1001, 0x0101000282004d00,
		0x2048848000204000, 0x3020088020804000, 0x4806020020841240, 0x6080420008102202, 0x0010050011000800, 0xac00808004000200, 0x0000010100020004, 0x1500020004004581,
		0x0004c00180052080, 0x0220028480254000, 0x2101200580100080, 0x0407201200084200, 0x0018004900100500, 0x100200020008e410, 0x0081020400100811, 0x0000012200024494,
		0x8006c002808006a5, 0x0004201000404000, 0x0005402202001180, 0x0000081001002100, 0x0000100801000500, 0x4000020080800400, 0x4005050214001008, 0x810100118b000042,
		0x0d01020040820020, 0x000140a010014000, 0x0420001500210040, 0x0054210010030009, 0x0004000408008080, 0x0002000400090100, 0x0000840200010100, 0x0000233442820004,
		0x800a42002b008200, 0x0240200040009080, 0x0242001020408200, 0x4000801000480480, 0x2288008044000880, 0x000a800400020180, 0x0030011002880c00, 0x0041110880440200,
		0x0002001100442082, 0x01a0104002208101, 0x080882014010200a, 0x0000100100600409, 0x0002011048204402, 0x0012000168041002, 0x080100008a000421, 0x0240022044031182
	];

	/**
	 * struct Stack to do/undo moves.
	 * With uses this struct to store elements of the board that will be modified by a move.
	 */
	struct Stack {
		ulong checkers; /// Checking pieces
		ulong castling; /// Castling bitboard
		Key key; /// Zobrist key
		Square enpassant = Square.none; /// Enpassant square
		Move move; /// Last move
		Piece victim; /// Captured piece
		byte fifty; /// Fifty moves rule
		bool castled; /// Castling flag
	}

	ulong[Piece.size] piece; /// Piece bitboards
	ulong[Color.size] color; /// Color bitboards
	CPiece[Square.size] cpiece; /// Colored pieces mailbox
	Stack[Limits.Game.size] stack; /// Stack of move effects to undo a move
	Square[Color.size] xKing; /// King's square
	Color player; /// Current player
	int ply; /// Ply of the current position relative to the root
	int plyOffset; /// Ply of the root position
	bool chess960; /// Chess960 flag

	/**
	 * Constructor of static data: initialize the mask tables.
	 */
	shared static this() pure nothrow {
		int y, z;
		int[Square.size] d;
		static immutable int[2][8] knightDir = [
			[-2, -1], [-2, 1], [-1, -2], [-1, 2], [1, -2], [1, 2], [2, -1], [2, 1]
		];
		static immutable int[2][8] kingDir = [
			[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]
		];
		static immutable int[2][4] bishopDir = [[-1, -1], [-1, 1], [1, -1], [1, 1]];
		static immutable int[2][4] rookDir = [[-1, 0], [0, -1], [0, 1], [1, 0]];

		Square square(int f, int r) {
			return (0 <= r && r <= 7 && 0 <= f && f <= 7) ? toSquare(f, r) : Square.none;
		}

		ulong bit(int f, int r) {
			return (0 <= r && r <= 7 && 0 <= f && f <= 7) ? 1UL << toSquare(f, r) : 0;
		}

		foreach (x; Square.a1 .. Square.size) {
			const int f = file(x), r = rank(x);

			foreach (dir; kingDir)
				foreach (k; 1 .. 8)
					if ((y = square(f + k * dir[0], r + k * dir[1])) != Square.none) {
						d[y] = dir[0] + 8 * dir[1];
						for (z = x + d[y]; z != y; z += d[y])
							mask[x].between[y] |= 1UL << z;
					}

			for (y = x - 9; y >= 0 && d[y] == -9; y -= 9) mask[x].diagonal |= 1UL << y;
			for (y = x + 9; y < Square.size && d[y] == 9; y += 9) mask[x].diagonal |= 1UL << y;
			for (y = x - 7; y >= 0 && d[y] == -7; y -= 7) mask[x].antidiagonal |= 1UL << y;
			for (y = x + 7; y < Square.size && d[y] == 7; y += 7) mask[x].antidiagonal |= 1UL << y;
			mask[x].file = fileMask[file(x)] ^ x.toBit;
			mask[x].rank = rankMask[rank(x)] ^ x.toBit;

			mask[x].pawnAttack[Color.white] = bit(f - 1, r + 1) | bit(f + 1, r + 1);
			mask[x].pawnAttack[Color.black] = bit(f - 1, r - 1) | bit(f + 1, r - 1);
			if (r == 3 || r == 4) mask[x].enpassant = bit(f - 1, r) | bit(f + 1, r);

			foreach (dir; knightDir) mask[x].knight |= bit(f + dir[0], r + dir[1]);
			foreach (dir; kingDir) mask[x].king |= bit(f + dir[0], r + dir[1]);

			const ulong inside = ~(((rankMask[0] | rankMask[7]) & ~rankMask[r]) | ((fileMask[0] | fileMask[7]) & ~fileMask[f]));
			mask[x].bishop = immutable Attack(x, (mask[x].diagonal | mask[x].antidiagonal) & inside, bishopMagic[x], bishopDir);
			mask[x].rook = immutable Attack(x, (mask[x].rank | mask[x].file) & inside, rookMagic[x], rookDir);
		}
	}

	/**
	 * Verify if castling is possible:
	 * - The king and the rook have not moved, ie castling flag is set.
	 * - The squares between the king and the rook are empty
	 * - The king is not in check and does not pass through a square attacked by an enemy piece
	 *
	 * params: kingFrom = source King's square
	 *         rookFrom = source Rook's square
	 *         occupancy = occupancy bitboard
	 * returns: true if castling is possible
	 */
	bool canCastle(const Square kingFrom, const Square rookFrom, const ulong occupancy) pure nothrow const {
		const Color enemy = opponent(player);
		const Square kingTo = kingFrom < rookFrom ? kingCastleTo[Castling.kingSide][player] : kingCastleTo[Castling.queenSide][player];
		const Square rookTo = kingFrom < rookFrom ? rookCastleTo[Castling.kingSide][player] : rookCastleTo[Castling.queenSide][player];
		ulong kingPath = mask[kingFrom].between[kingTo] | kingTo.toBit, rookPath = mask[rookFrom].between[rookTo] | rookTo.toBit;

		if ((stack[ply].castling & rookFrom.toBit) == 0) return false;
		if (((kingPath | rookPath) & occupancy & ~rookFrom.toBit & ~kingFrom.toBit) != 0) return false;
		while (kingPath) {
			if (isSquareAttacked(popSquare(kingPath), enemy, occupancy ^ rookFrom.toBit)) return false;
		}

		return true;
	}

	/**
	 * Verify if a move is a castling move.
	 * Internally, a castling move is a king move that capture its own rook.
	 * params: move = move
	 * returns: true if the move is a castling move
	 */
	bool isCastling(const Move move) pure nothrow const {
		const CPiece from = cpiece[move.from], to = cpiece[move.to];
		return from.toColor == to.toColor && from.toPiece == Piece.king && to.toPiece == Piece.rook;
	}

	/**
	 * Get the checking piece squares as a bitboard.
	 * returns: bitboard of checking pieces
	 */
	ulong setCheckers() pure nothrow {
		const Square k = xKing[player];
		const ulong occupancy = ~piece[Piece.none];
		ulong checkers;

		checkers = bishopAttack(k, piece[Piece.bishop] + piece[Piece.queen], occupancy);
		checkers |= rookAttack(k, piece[Piece.rook] + piece[Piece.queen], occupancy);
		checkers |= knightAttack(k, piece[Piece.knight]);
		checkers |= pawnAttack(k, piece[Piece.pawn], player);
		checkers &= color[opponent(player)];

		return checkers;
	}

	/**
	 * Update the chessboard when a piece moves from a square to another one.
	 *
	 * params: from = source square
	 *         to = destination square
	 *         p = piece
	 */
	void relocate(const Square from, const Square to, const Piece p) pure nothrow {
		const ulong M = from.toBit ^ to.toBit;
		piece[Piece.none] ^= M;
		piece[p] ^= M;
		color[player] ^= M;
		cpiece[from] = CPiece.none;
		cpiece[to] = toCPiece(p, player);
	}

	/**
	 * Update the chessboard when a Piece is captured.
	 *
	 * params: victim = captured piece
	 *         x = square where the piece is captured
	 *         enemy = enemy color
	 */
	void capture(const Piece victim, const Square x, const Color enemy) pure nothrow {
		const ulong M = x.toBit;
		piece[Piece.none] ^= M;
		piece[victim] ^= M;
		color[enemy] ^= M;
	}

	/**
	 * Castle.
	 *
	 * params: kingFrom = source King's square
	 *         kingTo = destination King's square
	 *         rookFrom = source Rook's square
	 *         rookTo = destination Rook's square
	 */
	void castle(const Square kingFrom, const Square kingTo, const Square rookFrom, const Square rookTo) pure nothrow {
		const ulong kingMask = kingFrom.toBit ^ kingTo.toBit, rookMask = rookFrom.toBit ^ rookTo.toBit;
		piece[Piece.none] ^= kingMask ^ rookMask;
		piece[Piece.king] ^= kingMask;
		piece[Piece.rook] ^= rookMask;
		color[player] ^= kingMask ^ rookMask;
		cpiece[kingFrom] = cpiece[rookFrom] = CPiece.none;
		cpiece[kingTo] = toCPiece(Piece.king, player);
		cpiece[rookTo] = toCPiece(Piece.rook, player);
	}

	/**
	 * Verify if a square x is attacked by a piece of Color c
	 *
	 * params: x = square
	 *         c = color
	 *         occupancy = occupancy bitboard
	 * returns: true if the square is attacked
	 */
	bool isSquareAttacked(const Square x, const Color c, const ulong occupancy) pure nothrow const {
		return bishopAttack(x, color[c] & (piece[Piece.bishop] | piece[Piece.queen]), occupancy)
			|| rookAttack(x, color[c] & (piece[Piece.rook] | piece[Piece.queen]), occupancy)
			|| knightAttack(x, color[c] & piece[Piece.knight])
			|| pawnAttack(x, color[c] & piece[Piece.pawn], opponent(c))
			|| kingAttack(x, color[c] & piece[Piece.king]);
	}

	/**
	 * Generate the moves from an attack map and a source square.
	 * params: moves = moves list
	 *         attack = attack map
	 *         from = source square
	 */
	static void generateMoves(ref Moves moves, ulong attack, const Square from) pure nothrow {
		while (attack) {
			Square to = popSquare(attack);
			moves.push(from, to);
		}
	}

	/**
	 * Generate all promotions (or only promotion to queen if doQuiet is false).
	 *
	 * params: moves = moves list
	 *         attack = attack map
	 *         dir = direction
	 *         doQuiet = false if only promotion to queen is allowed
	 */
	static void generatePromotions(bool doQuiet = true)(ref Moves moves, ulong attack, const int dir) pure nothrow {
		while (attack) {
			Square to = popSquare(attack);
			Square from = to.shift(-dir);
			moves.pushPromotions!doQuiet(from, to);
		}
	}

	/**
	 * Generate pawn moves.
	 *
	 * params: moves = moves list
	 *         attack = attack map
	 *         dir = direction
	 */
	static void generatePawns(ref Moves moves, ulong attack, const int dir) pure nothrow {
		while (attack) {
			Square to = popSquare(attack);
			Square from = to.shift(-dir);
			moves.push(from, to);
		}
	}

	/**
	 * Generate piece moves.
	 *
	 * params: p = piece
	 *         moves = moves list
	 *         attacker = attacker bitboard
	 *         target = target bitboard
	 */
	void generatePieceMoves(Piece p, ref Moves moves, ulong attacker, const ulong target) pure nothrow const {
		const ulong occupancy = ~piece[Piece.none];

		while (attacker) {
			Square from = popSquare(attacker);
			generateMoves(moves, attack(p, from, target, occupancy), from);
		}
	}

	/**
	 * Compute an attack map for pawns.
	 *
	 * params: x = square
	 *         target = empty and/or opponent's squares
	 *         c = color
	 * returns: attack map
	 */
	static ulong pawnAttack(const Square x, const ulong target, const Color c) pure nothrow {
		return mask[x].pawnAttack[c] & target;
	}

	/**
	 * Compute an attack map for knights.
	 *
	 * params: x = square
	 *         target = empty and/or opponents' squares
	 * returns: attack map
	 */
	static ulong knightAttack(const Square x, const ulong target) pure nothrow {
		return mask[x].knight & target;
	}

	/**
	 * Compute an attack map for bishops.
	 *
	 * params: x = square
	 *         target = empty and/or opponents' squares
	 *         occupancy = occupancy bitboard
	 * returns: attack map
	 */
	static ulong bishopAttack(const Square x, const ulong target, const ulong occupancy) pure nothrow {
		return mask[x].bishop.attack[mask[x].bishop.index(occupancy)] & target;
	}

	/**
	 * Compute an attack map for rooks.
	 *
	 * params: x = square
	 *         occupancy = occupancy bitboard
	 *         c = color
	 *         target = empty and/or opponents' squares
	 * returns: attack map
	 */
	static ulong rookAttack(const Square x, const ulong target, const ulong occupancy) pure nothrow {
		return mask[x].rook.attack[mask[x].rook.index(occupancy)] & target;
	}

	/**
	 * Compute an attack map for queens.
	 *
	 * params: x = square
	 *         occupancy = occupancy bitboard
	 *         c = color
	 *         target = empty and/or opponents' squares
	 * returns: attack map
	 */
	static ulong queenAttack(const Square x, const ulong target, const ulong occupancy) pure nothrow {
		return (mask[x].bishop.attack[mask[x].bishop.index(occupancy)] + mask[x].rook.attack[mask[x].rook.index(occupancy)]) & target;
	}

	/**
	 * Compute an attack map for kings.
	 *
	 * params: x = square
	 *         occupancy = occupancy bitboard
	 *         c = color
	 *         target = empty and/or opponents' squares.
	 * returns: attack map
	 */
	static ulong kingAttack(const Square x, const ulong target) pure nothrow {
		return mask[x].king & target;
	}

	/**
	 * Compute an attack map, targeting empty squares and enemy pieces
	 *
	 * params: p = piece
	 *         x = square
	 *         target = target bitboard
	 *         occupancy = occupancy bitboard
	 *         c = color
	 * returns: attack map
	 */
	static ulong attack(Piece p, const Square x, const ulong target, const ulong occupancy = 0, const Color c = Color.white) pure nothrow {
		final switch(p) {
		case Piece.pawn: return pawnAttack(x, target, c);
		case Piece.knight: return knightAttack(x, target);
		case Piece.bishop: return bishopAttack(x, target, occupancy);
		case Piece.rook: return rookAttack(x, target, occupancy);
		case Piece.queen: return queenAttack(x, target, occupancy);
		case Piece.king: return kingAttack(x, target);
		case Piece.none, Piece.size: return 0;
		}
	}

	/**
	 * Clear the chess board. Make an empty chess board & init every things
	 */
	void clear() pure nothrow {
		foreach (p; Piece.none .. Piece.size) piece[p] = 0;
		foreach (c; Color.white .. Color.size) color[c] = 0;
		foreach (x; Square.a1 .. Square.size) cpiece[x] = CPiece.none;
		stack[0] = Stack.init;
		xKing[0] = xKing[1] = Square.none;
		player = Color.white;
		ply = 0;
		chess960 = false;
	}

	/**
	 *  Set the chess board according to a FEN string, by default the starting position
	 *
	 *  params: fen = FEN string
	 */
	void set(string fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") {
		Square x;
		CPiece p;
		int r = 7, f;
		string[] s = fen.split();

		void error(string msg) {
			writeln("string info Error 'Bad FEN: ", msg, " in ", fen, "'");
			set();
		}

		clear();

		if (s.length < 4) return error("missing fields");

		foreach (c; s[0]) {
			if (c == '/') {
				if (r <= 0) return error("rank overflow");
				if (f != 8) return error("missing square");
				f = 0;
				--r;
			}
			else if (isDigit(c)) {
				f += c - '0';
				if (f > 8) return error("file overflow");
			}
			else {
				if (f > 8) return error("file overflow");
				x = toSquare(f, r);
				cpiece[x] = p = toCPiece(c);
				if (cpiece[x] == CPiece.size) return error("bad piece");
				piece[toPiece(p)] |= x.toBit;
				color[toColor(p)] |= x.toBit;
				if (toPiece(p) == Piece.king)
					xKing[toColor(p)] = x;
				++f;
			}
		}
		if (r > 0 || f != 8) return error("missing squares");

		player = toColor(s[1][0]);
		if (player == Color.size) return error("bad player's turn");

		if (s.length > 5 && isNumeric(s[4])) {
			stack[ply].fifty = std.conv.to!ubyte(s[4]);
			plyOffset = 2 * (std.conv.to!int(s[5]) - 1) + player;
		}

		if (s[2] != "-") {
			foreach (c; s[2]) stack[ply].castling |= toCastling(c);
			stack[ply].castling &= piece[Piece.rook];
		}

		if (s[3] != "-") {
			stack[ply].enpassant = toSquare(s[3]);
			if (stack[ply].enpassant == Square.none) return error("bad enpassant");
		}

		piece[Piece.none] = ~(color[Color.white] | color[Color.black]);
		stack[ply].checkers = setCheckers();
		stack[ply].key.set(this);
	}

	/**
	 * Constructor: set the chess board to the initial position
	 */
	this() {
		set();
	}

	/**
	 * Get the Colored piece occupying a square using array indexing.
	 *
	 * params: x = square
	 * returns: colored piece occupying the square
	 */
	CPiece opIndex(const Square x) pure nothrow const {
		return cpiece[x];
	}

	/**
	 * Get a zobrist key
	 */
	Key key() pure nothrow const {
		return stack[ply].key;
	}

	/**
	 * Verify if the player's king is in check.
	 *
	 * returns: true if the player's king is in check
	 */
	bool inCheck() pure nothrow const {
		return stack[ply].checkers > 0;
	}

	/**
	 * Verify if the position is a draw according to the chess rules.
	 * Check the draws by 3-fold repetition, fifty move rule and lack of mating pieces.
	 * It does not check the other eventual draws (blocked positions, stalemate, etc.)
	 * returns: true if the position is a draw
	 */
	bool isDraw() pure nothrow const {
		// draw by 3 fold repetition
		int nRepetition = 0;
		const end = max(0, ply - stack[ply].fifty);
		for (int i = ply - 4; i >= end; i -= 2) {
			if (stack[i].key.code == stack[ply].key.code && ++nRepetition >= 2) return true;
		}

		// draw by fifty move rule (might be inaccurate ?)
		if (stack[ply].fifty > 100)	return true;

		// draw by lack of mating pieces
		if (piece[Piece.pawn] + piece[Piece.rook] + piece[Piece.queen] == 0) {
			const nMinor = popcnt(piece[Piece.knight] + piece[Piece.bishop]);
			if (nMinor <= 1) return true;
			const diff = abs(popcnt(color[Color.white]) - popcnt(color[Color.black]));
			const blackSquares = 0x55aa55aa55aa55aa;
			const whiteSquares = ~blackSquares;
			if (diff == nMinor && piece[Piece.knight] == 0 && ((piece[Piece.bishop] & blackSquares) == piece[Piece.bishop] || (piece[Piece.bishop] & whiteSquares) == piece[Piece.bishop])) return true;
		}

		return false;
	}

	/**
	 * Verify if a move is an enpassant capture.
	 *
	 * params: move = move
	 * returns: true if the move is an enpassant capture
	 */
	bool isEnpassantCapture(const Move move) pure nothrow const {
		return stack[ply].enpassant == move.to && cpiece[move.from] == toCPiece(Piece.pawn, player);
	}

	/**
	 * Update the chess board when a move is made, with some optimizations for quiescence search.
	 *
	 * params: move = move
	 *         quiet = true if the move is done during the quiet search
	 * returns: true if the move is legal
	 */
	bool update(bool quiet = true)(const Move move) {
		const to = move.to.toBit;
		const enemy = opponent(player);
		const p = toPiece(cpiece[move.from]);
		const Stack* u = &stack[ply];
		Stack* n = &stack[ply + 1];

		static if (quiet) n.key.update(this, move);
		n.castling = u.castling;
		n.enpassant = Square.none;
		n.fifty = cast(byte)(u.fifty + 1);
		n.castled = isCastling(move);

		if (move != 0) {
			if (n.castled) {
				n.castling &= ~(piece[Piece.rook] & color[player]);
				xKing[player] = kingCastleTo[move.side][player];
				castle(move.from, kingCastleTo[move.side][player], move.to, rookCastleTo[move.side][player]);
			} else {
				n.castling &= ~(move.from.toBit) & ~(move.to.toBit);
				n.victim = toPiece(cpiece[move.to]);
				relocate(move.from, move.to, p);
				if (n.victim) {
					n.fifty = 0;
					capture(n.victim, move.to, enemy);
				}
				if (p == Piece.pawn) {
					n.fifty = 0;
					if (move.promotion) {
						piece[Piece.pawn] ^= to;
						piece[move.promotion] ^= to;
						cpiece[move.to] = toCPiece(move.promotion, player);
					} else if (u.enpassant == move.to) {
						const x = move.to.enpassant;
						capture(Piece.pawn, x, enemy);
						cpiece[x] = CPiece.none;
					} else if (abs(move.to - move.from) == 16 && (mask[move.to].enpassant & (color[enemy] & piece[Piece.pawn]))) {
						n.enpassant = move.to.enpassant;
					}
				} else if (p == Piece.king) {
					n.castling &= ~(piece[Piece.rook] & color[player]);
					xKing[player] = move.to;
				}
			}
		}
		player = enemy;
		n.checkers = setCheckers();
		++ply;

		return !isSquareAttacked(xKing[opponent(enemy)], enemy, ~piece[Piece.none]);
	}

	/**
	 * Restore the chess board when a move is undone
	 *
	 * params: move = move
	 */
	void restore(const Move move) pure nothrow {
		const ulong to = move.to.toBit;
		const Color enemy = player;
		const p = move.promotion ? Piece.pawn : toPiece(cpiece[move.to]);
		const Stack* n = &stack[ply];
		const Stack* u = &stack[--ply];

		player = opponent(enemy);
		if (move != 0) {
			if (n.castled) {
				castle(kingCastleTo[move.side][player], move.from, rookCastleTo[move.side][player], move.to);
				xKing[player] = move.from;
			} else {
				relocate(move.to, move.from, p);
				if (n.victim) {
					capture(n.victim, move.to, enemy);
					cpiece[move.to] = toCPiece(n.victim, enemy);
				}
				if (p == Piece.pawn) {
					if (move.promotion) {
						piece[Piece.pawn] ^= to;
						piece[move.promotion] ^= to;
						cpiece[move.from] = toCPiece(Piece.pawn, player);
					} else if (u.enpassant == move.to) {
						const Square x = move.to.enpassant;
						capture(Piece.pawn, x, enemy);
						cpiece[x] = toCPiece(Piece.pawn, enemy);
					}
				}
				if (p == Piece.king)
					xKing[player] = move.from;
			}
		}
	}

	/**
	 * Generate pseudo-legal moves.
	 *
	 * params: moves = moves list
	 *         generator = generator type.
	 */
	void generateMoves(Generator generator = Generator.all)(ref Moves moves) pure nothrow const {
		const Color enemy = opponent(player);
		const ulong occupancy = ~piece[Piece.none];
		const ulong bq = piece[Piece.bishop] | piece[Piece.queen];
		const ulong rq = piece[Piece.rook] | piece[Piece.queen];
		const ulong checkers = stack[ply].checkers;
		const Square k = xKing[player];
		const int push = pushTable[player];
		const int left = push - 1;
		const int right = push + 1;
		ulong empties = piece[Piece.none], enemies = color[enemy];
		ulong target, attacker, o, attacked;
		Square from, to, x;

		// check evasion
		if (checkers) {
			// single check: king moves + checker capture or blocking check
			if (hasSingleBit(checkers)) {
				x = firstSquare(checkers);
				empties = mask[k].between[x];
				enemies = checkers;
				target = enemies | empties;
			} else {
				// double check: king moves only
				target = enemies = empties = 0;
			}
		}
		else {
			// normal moves
			target = enemies;
			static if (generator.as(Generator.quiet)) target |= empties;

			// special case: castling
			static if (generator.as(Generator.quiet)) {
				ulong rooks = stack[ply].castling & color[player];
				while (rooks) {
					x = popSquare(rooks);
					if (canCastle(k, x, occupancy))	moves.push(k, x);
				}
			}
		}

		// special case: enpassant square
		if (stack[ply].enpassant != Square.none) {
			to = stack[ply].enpassant;
			x = to.enpassant;
			from = x.shift(-1);
			if (file(to) > 0 && cpiece[from] == toCPiece(Piece.pawn, player)) moves.push(from, to);
			from = x.shift(+1);
			if (file(to) < 7 && cpiece[from] == toCPiece(Piece.pawn, player)) moves.push(from, to);
		}

		// Pawn moves
		attacker = piece[Piece.pawn] & color[player];
		// captures
		attacked = (player ? (attacker & ~fileMask[0]) >> 9 : (attacker & ~fileMask[0]) << 7) & enemies;
		generatePromotions!(generator.as(Generator.quiet))(moves, attacked & rank8[player], left);
		generatePawns(moves, attacked & ~rank8[player], left);
		attacked = (player ? (attacker & ~fileMask[7]) >> 7 : (attacker & ~fileMask[7]) << 9) & enemies;
		generatePromotions!(generator.as(Generator.quiet))(moves, attacked & rank8[player], right);
		generatePawns(moves, attacked & ~rank8[player], right);
		// push moves on 7th rank and promotions
		attacked = (player ? attacker >> 8 : attacker << 8) & piece[Piece.none];
		generatePromotions!(generator.as(Generator.quiet))(moves, attacked & rank8[player] & empties, push);
		generatePawns(moves, attacked & rank7[player] & empties, push);
		// quiet moves: other push moves
		static if ((generator.as(Generator.quiet))) {
			generatePawns(moves, attacked & ~(rank8[player] | rank7[player]) & empties, push);
			attacked = (player ? (attacked & rankMask[5]) >> 8 : (attacked & rankMask[2]) << 8) & empties;
			generatePawns(moves, attacked, 2 * push);
		}

		// piece moves
		generatePieceMoves(Piece.knight, moves, piece[Piece.knight] & color[player], target);
		generatePieceMoves(Piece.bishop, moves, bq & color[player], target);
		generatePieceMoves(Piece.rook, moves, rq & color[player], target);

		// king moves
		target = color[enemy];
		static if (generator.as(Generator.quiet)) {
			target |= piece[Piece.none];
		} else {
			if (checkers) target |= piece[Piece.none];
		}
		attacked = kingAttack(k, target);
		o = occupancy ^ k.toBit;
		while (attacked) {
			to = popSquare(attacked);
			if (!isSquareAttacked(to, enemy, o)) moves.push(k, to);
		}
	}

	/**
	 *  Return the next attacker to compute the SEE
	 *
	 *  params: board = board bitboards
	 *          to = target square
	 *          player = player color
	 *          enemy = enemy color
	 *          last = last attacker
	 *  returns: next attacker
	 */
	Piece nextAttacker(ref ulong[Color.size] board, const Square to, const Color player, const Color enemy, const Piece last) pure nothrow const {
		static immutable Piece[Piece.size] next = [ Piece.none, Piece.pawn, Piece.knight, Piece.bishop, Piece.rook, Piece.bishop, Piece.size ];
		const ulong occupancy = board[player] | board[enemy];
		ulong attacker;

		for (Piece p = next[last]; p <= Piece.king; ++p) {
			if ((attacker = attack(p, to, piece[p] & board[player], occupancy, enemy)) != 0) {
				board[player] ^= (attacker & -attacker);
				return p;
			}
		}
		return Piece.none;
	}

	/**
	 * (S)tatic (E)xchange (E)valuation (SEE): tha alphabeta version of the algorithm, both efficient & compact.
	 *
	 * params: move = move
	 * returns: SEE value
	 */
	int see(const Move move) pure nothrow const {
		const Color enemy = opponent(player);
		ulong[Color.size] board = color;
		Piece attacker = toPiece(cpiece[move.from]);
		Piece defender = (attacker == Piece.pawn && stack[ply].enpassant == move.to) ? Piece.pawn : toPiece(cpiece[move.to]);
		int β = seeValue[defender], α = β - seeValue[attacker], score;

		if (α <= 0) {
			board[player] ^= move.from.toBit;
			if ((defender = nextAttacker(board, move.to, enemy, player, Piece.pawn)) == Piece.none) return β;
			score = α;
			attacker = Piece.pawn;
			while (true) {
				score += seeValue[defender];
				if (score <= α || (attacker = nextAttacker(board, move.to, player, enemy, attacker)) == Piece.none) return α;
				if (score < β) β = score;

				score -= seeValue[attacker];
				if (score >= β || (defender = nextAttacker(board, move.to, enemy, player, defender)) == Piece.none) return β;
				if (score > α) α = score;
			}
		}

		return β;
	}

	/**
	 * Castling to string
	 *
	 * params: castling = castling mask
	 *         player = player color
	 * returns: castling string
	 */
	static string castlingToString(ulong castling, const Color player) pure nothrow {
		string s;
		char[Color.size] base = ['A', 'a'];
		while (castling) {
			Square x = popSquare(castling);
			s ~= cast(char)(base[player] + file(x));
		}
		return s;
	}

	/**
	 * toString function
	 * Convert a position into readable text (with Unicode chars)
	 * returns: a string
	 */
	override string toString() pure const {
		Square x;
		int f, r;
		wchar[] p = ['.', '?', '♙', '♟', '♘', '♞', '♗', '♝', '♖', '♜', '♕', '♛', '♔', '♚', '#'];
		string c = "wb", s;

		s ~= "  a b c d e f g h\n";
		for (r = 7; r >= 0; --r)
			for (f = 0; f <= 7; ++f) {
				x = toSquare(f, r);
				if (f == 0) s ~= format("%1d ", r + 1);
				s ~= p[cpiece[x]];
				s ~= " ";
				if (f == 7) s ~= format("%1d\n", r + 1);
			}
		s ~= "  a b c d e f g h\n";
		s ~= c[player] ~ " ";
		if (stack[ply].enpassant != Square.none) s ~= format(" ep: %s", stack[ply].enpassant);
		if (stack[ply].castling) {
			s ~= " " ~ castlingToString(stack[ply].castling & color[Color.white], Color.white);
			s ~= castlingToString(stack[ply].castling & color[Color.black], Color.black);
		}
		s ~= format(" move %d, fifty %d [K:%s, k:%s]\n", (ply + plyOffset) / 2 + 1, stack[ply].fifty, xKing[Color.white], xKing[Color.black]);
		if (chess960) s ~= "chess960 mode\n";
		s ~= format(" key %016x", stack[ply].key.code);

		return s;
	}
}
