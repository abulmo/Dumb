/*
 * File board.d
 * Chess board representation, move generation, etc.
 * © 2017-2022 Richard Delorme
 */

module board;
import move, util;
import std.ascii, std.conv, std.format, std.stdio, std.string;
import std.algorithm, std.math, std.random, std.range;
import core.bitop;

/* limits */
struct Limits {
	enum ply { max = 100 }
	enum game { size = 4096 }
}

/* Color */
enum Color : ubyte {white, black, size, none}

Color opponent(const Color c) { return cast (Color) !c; }

Color toColor(const char c) {return cast (Color) indexOf("wb", c); }

/* Piece */
enum Piece : ubyte {none, pawn, knight, bishop, rook, queen, king, size}

Piece toPiece(const char c) { return cast (Piece) indexOf(".pnbrqk", c, CaseSensitive.no); }

char toChar(const Piece p) { return ".PNBRQK?"[p]; }

/* Colored Piece */
enum CPiece : ubyte {none, _, wpawn, bpawn, wknight, bknight, wbishop, bbishop, wrook, brook, wqueen, bqueen, wking, bking, size}

CPiece toCPiece(const Piece p, const Color c) {	return cast (CPiece) (2 * p + c); }

CPiece toCPiece(const char c) { return cast (CPiece) indexOf("._PpNnBbRrQqKk", c); }

Color toColor(const CPiece p) {
	static immutable Color[CPiece.size] c = iota(CPiece.none, CPiece.size).map!(x => x < CPiece.wpawn ? Color.none : cast (Color) (x & 1)).array;
	return c[p];
}

Piece toPiece(const CPiece p) { return cast (Piece) (p / 2); }

/* Square */
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

auto allSquares() { return iota(Square.a1, Square.size).array; }

Square shift(const Square x, const int δ) { return cast (Square) (x + δ); }

Square shift(const Square x) { return cast (Square) (x ^ 8); }

Square forward(const Square x, const Color c) { return cast (Square) (x ^ (56 * c)); }

Square mirror(const Square x) { return cast (Square) (x ^ 56); }

int rank(const Square x) { return x >> 3; }

int file(const Square x) { return x & 7; }

ulong toBit(const Square x) { return 1UL << x; }

Square toSquare(const int f, const int r) {	return cast (Square) ((r << 3) + f); }

Square toSquare(string s) { return toSquare(s[0] - 'a', s[1] - '1'); }

Square popSquare(ref ulong b) { return cast (Square) popBit(b); }

Square firstSquare(const ulong b) { return cast (Square) bsf(b); }

/* Rank/file mask */
immutable ulong [] rankMask = [ 0x00000000000000ff, 0x000000000000ff00, 0x0000000000ff0000, 0x00000000ff000000, 0x000000ff00000000, 0x0000ff0000000000, 0x00ff000000000000, 0xff00000000000000 ];
immutable ulong [] fileMask = [ 0x0101010101010101, 0x0202020202020202, 0x0404040404040404, 0x0808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080 ];

/* Castling */
enum Castling { kingSide, queenSide };

Castling side(const Move m) { return m.to > m.from ? Castling.kingSide : Castling.queenSide; }

ulong toCastling(const char c) {
	with (Square) {
		static immutable Square  [] rooks = [h1, a1, h8, a8, a1, b1, c1, d1, e1, f1, g1, h1, a8, b8, c8, d8, e8, f8, g8, h8];
		size_t i = indexOf("KQkqABCDEFGHabcdefgh", c);
		return i == -1 ? 0 : rooks[i].toBit;
	}
}

/* Zobrist key */
struct Key {
	ulong code;

	static immutable ulong [Square.size][CPiece.size] square;
	static immutable ulong [Square.size] castling;
	static immutable ulong [Square.none + 1] enpassant;
	static immutable ulong [Color.size] color;
	static immutable ulong play;

	shared static this() {
		Mt19937 r;
		r.seed(19_937);
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) square[p][x] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a1 .. Square.a2)   castling[x] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a8 .. Square.size) castling[x] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a3 .. Square.a4) enpassant[x] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a6 .. Square.a7) enpassant[x] = uniform(ulong.min, ulong.max, r);
		foreach (c; Color.white .. Color.size) color[c] = uniform(ulong.min, ulong.max, r);
		play = color[Color.white] ^ color[Color.black];
	}

	void set(const Board board) {
		const Board.Stack *s = &board.stack[board.ply];
		code = color[board.player];
		foreach (Square x; Square.a1 .. Square.size) code ^= square[board[x]][x];
		code ^= enpassant[s.enpassant];
		ulong rooks = s.castling; while (rooks) code ^= castling[popSquare(rooks)];
	}

	void update(const Board board, const Move move) {
		Square x = Square.none;
		const Color player = board.player;
		const Color enemy = opponent(player);
		const CPiece p = board[move.from];
		const Board.Stack *s = &board.stack[board.ply];
		ulong rooks = 0;

		code = s.key.code;
		code ^= play;
		if (move != 0) {
			if (board.isCastling(move)) {
				CPiece r = toCPiece(Piece.rook, player);
				code ^= square[p][move.from] ^ square[p][board.kingCastleTo[move.side][player]];
				code ^= square[r][move.to]   ^ square[r][board.rookCastleTo[move.side][player]];
				rooks = board.piece[Piece.rook] & board.color[player];
			} else {
				code ^= square[p][move.from] ^ square[p][move.to];
				code ^= square[board[move.to]][move.to];
				rooks = (move.from.toBit | move.to.toBit) & board.piece[Piece.rook];
				if (toPiece(p) == Piece.pawn) {
					if (move.promotion) code ^= square[p][move.to] ^ square[toCPiece(move.promotion, player)][move.to];
					else if (s.enpassant == move.to) code ^= square[toCPiece(Piece.pawn, enemy)][move.to.shift];
					else if (abs(move.to - move.from) == 16 && (board.mask[move.to].enpassant & (board.color[enemy] & board.piece[Piece.pawn]))) x = move.to.shift;				
				} else if (toPiece(p) == Piece.king) rooks |= board.piece[Piece.rook] & board.color[player];
			}
			rooks &= s.castling;
			while (rooks) code ^= castling[popSquare(rooks)];
		}
		code ^= enpassant[s.enpassant] ^ enpassant[x];
	}
}


/* Bitmask */
struct Mask {
	ulong diagonal, antidiagonal, file, rank;
	ulong [Color.size] pawnAttack, push;
	ulong enpassant, knight, king;
	ulong [Square.size] between;
	ubyte [Square.size] direction;
	ubyte castling;
}

/* Class board */
final class Board {
	static immutable Mask [Square.size] mask;
	static immutable ubyte [512] ranks;
	static immutable int [Piece.size] seeValue = [0, 1, 3, 3, 5, 9, 300];
	static immutable ulong [Color.size] rank8 = [rankMask[7], rankMask[0]], rank7 = [rankMask[6], rankMask[1]];
	static immutable int [Color.size] pushTable = [8, -8];
	static immutable Square [2][Color.size] kingCastleTo = [[Square.g1, Square.g8], [Square.c1, Square.c8]], rookCastleTo = [[Square.f1, Square.f8], [Square.d1, Square.d8]];

	struct Stack {
		ulong pins;
		ulong checkers;
		ulong castling;
		Key key;
		Square enpassant = Square.none;
		Piece victim;
		byte fifty;
		bool castled;
	}
	ulong [Piece.size] piece;
	ulong [Color.size] color;
	CPiece [Square.size] cpiece;
	Stack [Limits.game.size] stack;
	Square [Color.size] xKing;
	Color player;
	int ply, plyOffset;
	bool chess960;

	shared static this() {
		int b, y, z;
		byte [Square.size] d;
		static immutable int [2][8] knightDir = [[-2,-1], [-2,1], [-1,-2], [-1,2], [1,-2], [1,2], [2,-1], [2,1]];
		static immutable int [2][8] kingDir   = [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]];

		Square square (int f, int r) { return (0 <= r && r <= 7 && 0 <= f && f <= 7) ? toSquare(f, r) : Square.none; }

		ulong bit(int f, int r) { return (0 <= r && r <= 7 && 0 <= f && f <= 7) ? 1UL << toSquare(f, r) : 0; }

		foreach (x; allSquares) {

			const int f = file(x), r = rank(x);

			foreach (dir; kingDir)
			foreach (k; 1 .. 8) {
				y = square(f + k * dir[0], r + k * dir[1]);
				if (y == Square.none) break;
				d[y] = cast (byte) (dir[0] + 8 * dir[1]);
				mask[x].direction[y] = abs(d[y]);
				for (z = x + d[y]; z != y; z += d[y]) mask[x].between[y] |= 1UL << z;
			}

			for (y = x - 9; y >= 0 && d[y] == -9; y -= 9) mask[x].diagonal |= 1UL << y;
			for (y = x + 9; y < Square.size && d[y] == 9; y += 9) mask[x].diagonal |= 1UL << y;
			for (y = x - 7; y >= 0 && d[y] == -7; y -= 7) mask[x].antidiagonal |= 1UL << y;
			for (y = x + 7; y < Square.size && d[y] == 7; y += 7) mask[x].antidiagonal |= 1UL << y;
			mask[x].file = fileMask[file(x)] ^ x.toBit;
			mask[x].rank = rankMask[rank(x)] ^ x.toBit;

			mask[x].pawnAttack[Color.white] = bit(f - 1, r + 1) | bit (f + 1, r + 1);
			mask[x].pawnAttack[Color.black] = bit(f - 1, r - 1) | bit (f + 1, r - 1);
			mask[x].push[Color.white] |= bit(f, r + 1);
			mask[x].push[Color.black] |= bit(f, r - 1);
			if (r == 3 || r == 4) {
				if (f > 0) mask[x].enpassant |=  1UL << x - 1;
				if (f < 7) mask[x].enpassant |=  1UL << x + 1;
			}
			foreach (dir; knightDir) mask[x].knight |= bit(f + dir[0], r + dir[1]);
			foreach (dir; kingDir) mask[x].king   |= bit(f + dir[0], r + dir[1]);
		}

		foreach (o; 0 .. 64) {
			foreach (k; 0 .. 8) {
				y = 0;
				foreach_reverse (x; 0 .. k) {
					b = 1 << x;
					y |= b;
					if (((o << 1) & b) == b) break;
				}
				foreach (x; k + 1 .. 8) {
					b = 1 << x;
					y |= b;
					if (((o << 1) & b) == b) break;
				}
				ranks[o * 8 + k] = cast (ubyte) y;
			}
		}
	}

	bool canCastle(const Square kingFrom, const Square rookFrom, const ulong occupancy) const {
		const Color enemy = opponent(player);
		const Square kingTo = kingFrom < rookFrom ? kingCastleTo[Castling.kingSide][player] : kingCastleTo[Castling.queenSide][player];
		const Square rookTo = kingFrom < rookFrom ? rookCastleTo[Castling.kingSide][player] : rookCastleTo[Castling.queenSide][player];
		ulong kingPath = mask[kingFrom].between[kingTo] | kingTo.toBit, rookPath = mask[rookFrom].between[rookTo] | rookTo.toBit;

		if ((stack[ply].castling & rookFrom.toBit) == 0) return false;
		if (((kingPath | rookPath) & occupancy & ~rookFrom.toBit & ~kingFrom.toBit) != 0)  return false;
		while (kingPath) if (isSquareAttacked(popSquare(kingPath), enemy, occupancy ^ rookFrom.toBit)) return false;

		return true;
	}

	bool isCastling(const Move m) const {
		const CPiece f = cpiece[m.from], t = cpiece[m.to];
		return f.toColor == t.toColor && f.toPiece == Piece.king && t.toPiece == Piece.rook;
	}

	static ulong attack(const ulong occupancy, const Square x, const ulong m)  {
		const ulong o = occupancy & m;
		const ulong r = bswap(o);
		return ((o - x.toBit) ^ bswap(r - x.mirror.toBit)) & m;
	}

	static ulong rankAttack(const ulong occupancy, const Square x) {
		const int f = x & 7;
		const int r = x & 56;
		const ulong o = (occupancy >> r) & 126;
		return ulong(ranks[o * 4  + f]) << r;
	}

	static ulong fileAttack(const ulong occupancy, const Square x) { return attack(occupancy, x, mask[x].file);	}

	static ulong diagonalAttack(const ulong occupancy, const Square x) { return attack(occupancy, x, mask[x].diagonal);	}

	static ulong antidiagonalAttack(const ulong occupancy, const Square x) { return attack(occupancy, x, mask[x].antidiagonal);	}

	void setPinsCheckers(ref ulong checkers, ref ulong pins) {
		const Color enemy = opponent(player);
		const Square k = xKing[player];
		const ulong bq = (piece[Piece.bishop] + piece[Piece.queen]) & color[enemy];
		const ulong rq = (piece[Piece.rook] + piece[Piece.queen]) & color[enemy];
		const ulong occupancy = ~piece[Piece.none];
		ulong partialCheckers;
		ulong b;
		Square x;

		pins = 0;

		b = coverage(Piece.bishop, k, occupancy);
		checkers = partialCheckers = b & bq;
		b &= color[player];
		if (b) {
			b = attack(Piece.bishop, k, bq ^ partialCheckers, occupancy ^ b);
			while (b) {
				x = popSquare(b);
				pins |= mask[k].between[x] & color[player];
			}
		}

		b = coverage(Piece.rook, k, occupancy);
		checkers |= partialCheckers = b & rq;
		b &= color[player];
		if (b) {
			b = attack(Piece.rook, k, rq ^ partialCheckers, occupancy ^ b);
			while (b) {
				x = popSquare(b);
				pins |= mask[k].between[x] & color[player];
			}
		}

		checkers |= attack(Piece.knight, k, piece[Piece.knight]);
		checkers |= attack(Piece.pawn, k, piece[Piece.pawn], occupancy, player);
		checkers &= color[enemy];
	}

	void deplace(const Square from, const Square to, const Piece p) {
		const ulong M = from.toBit ^ to.toBit;
		piece[Piece.none] ^= M;
		piece[p] ^= M;
		color[player] ^= M;
		cpiece[from] = CPiece.none;
		cpiece[to] = toCPiece(p, player);
	}

	void capture(const Piece victim, const Square x, const Color enemy) {
		const ulong M = x.toBit;
		piece[Piece.none] ^= M;
		piece[victim] ^= M;
		color[enemy] ^= M;
	}

	void castle(const Square kingFrom, const Square kingTo, const Square rookFrom, const Square rookTo) {
		const ulong kingMask = kingFrom.toBit ^ kingTo.toBit, rookMask = rookFrom.toBit ^ rookTo.toBit;
		piece[Piece.none] ^= kingMask ^ rookMask;
		piece[Piece.king] ^= kingMask;
		piece[Piece.rook] ^= rookMask;
		color[player] ^= kingMask ^ rookMask;
		cpiece[kingFrom] = cpiece[rookFrom] = CPiece.none;
		cpiece[kingTo] = toCPiece(Piece.king, player);
		cpiece[rookTo] = toCPiece(Piece.rook, player);
	}

	bool isSquareAttacked(const Square x, const Color p, const ulong occupancy) const {
		return attack(Piece.bishop, x, color[p] & (piece[Piece.bishop] | piece[Piece.queen]), occupancy)
			|| attack(Piece.rook, x, color[p] & (piece[Piece.rook] | piece[Piece.queen]), occupancy)
			|| attack(Piece.knight, x, color[p] & piece[Piece.knight])
			|| attack(Piece.pawn, x, color[p] & piece[Piece.pawn], occupancy, opponent(p))
			|| attack(Piece.king, x, color[p] & piece[Piece.king]);
	}

	static void generateMoves(ref Moves moves, ulong attack, const Square from) {
		while (attack) {
			Square to = popSquare(attack);
			moves.push(from, to);
		}
	}

	static void generatePromotions(bool doQuiet = true)(ref Moves moves, ulong attack, const int dir) {
		while (attack) {
			Square to = popSquare(attack);
			Square from = to.shift(-dir);
			moves.pushPromotions!doQuiet(from, to);
		}
	}

	static void generatePawns(ref Moves moves, ulong attack, const int dir) {
		while (attack) {
			Square to = popSquare(attack);
			Square from = to.shift(-dir);
			moves.push(from, to);
		}
	}

	void generatePieceMoves(Piece p, ref Moves moves, ulong attacker, const ulong target) const {
		const ulong occupancy = ~piece[Piece.none];

		while (attacker) {
			Square from = popSquare(attacker);
			generateMoves(moves, attack(p, from, target, occupancy), from);
		}
	}

	static ulong coverage(Piece p, const Square x, const ulong occupancy = 0, const Color c = Color.white) {
		final switch (p) {
			case Piece.pawn:   return mask[x].pawnAttack[c];
			case Piece.knight: return mask[x].knight;
			case Piece.bishop: return diagonalAttack(occupancy, x) + antidiagonalAttack(occupancy, x);
			case Piece.rook:   return fileAttack(occupancy, x) + rankAttack(occupancy, x);
			case Piece.queen:  return diagonalAttack(occupancy, x) + antidiagonalAttack(occupancy, x) + fileAttack(occupancy, x) + rankAttack(occupancy, x);
			case Piece.king:   return mask[x].king;
			case Piece.none, Piece.size: return 0;
		}
	}

	static ulong attack(Piece p, const Square x, const ulong target, const ulong occupancy = 0, const Color c = Color.white) { return coverage(p, x, occupancy, c) & target; }

	void clear() {
		foreach (p; Piece.none .. Piece.size) piece[p] = 0;
		foreach (c; Color.white .. Color.size) color[c] = 0;
		foreach (x; allSquares) cpiece[x] = CPiece.none;
		stack[0] = Stack.init;
		xKing[0] = xKing[1] = Square.none;
		player = Color.white;
		ply = 0;
	}

	void set(string fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") {
		Square x;
		CPiece p;
		int r = 7, f;
		string [] s = fen.split();

		void error(string msg) { writeln("string info Error 'Bad FEN: ", msg, " in ", fen, "'"); set(); }

		clear();

		if (s.length < 4) return error("missing fields");

		foreach (c; s[0]) {
			if (c== '/') {
				if (r <= 0) return error("rank overflow");
				if (f != 8) return error("missing square");
				f = 0; --r;
			} else if (isDigit(c)) {
				f += c - '0';
				if (f > 8) return error("file overflow");
			} else {
				if (f > 8) return error("file overflow");
				x = toSquare(f, r);
				cpiece[x] = p = toCPiece(c);
				if (cpiece[x] == CPiece.size) return error("bad piece");
				piece[toPiece(p)] |= x.toBit;
				color[toColor(p)] |= x.toBit;
				if (toPiece(p) == Piece.king) xKing[toColor(p)] = x;
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
		chess960  = ((stack[ply].castling & 0x7EFFFFFFFFFFFF7EUL) != 0);
		chess960 |= ((stack[ply].castling & 0x8100000000000000UL) && xKing[Color.black] != Square.e8);
		chess960 |= ((stack[ply].castling & 0x0000000000000081UL) && xKing[Color.white] != Square.e1);

		if (s[3] != "-") {
			stack[ply].enpassant = toSquare(s[3]);
			if (stack[ply].enpassant == Square.none) return error("bad enpassant");
		}

		piece[Piece.none] = ~(color[Color.white] | color[Color.black]);
		setPinsCheckers(stack[ply].checkers, stack[ply].pins);
		stack[ply].key.set(this);
	}

	this() { set(); }

	CPiece opIndex(const Square x) const { return cpiece[x]; }

	Key key() const { return stack[ply].key; }

	bool inCheck() const { return stack[ply].checkers > 0; }

	bool isDraw() const  {
		int nRepetition = 0;
		const end = max(0, ply - stack[ply].fifty);
		for (int i = ply - 4; i >= end; i -= 2) if (stack[i].key.code == stack[ply].key.code && ++nRepetition >= 2) return true;

		if (stack[ply].fifty > 100) return true;

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

	bool isEnpassantCapture(const Move move) { return stack[ply].enpassant == move.to && cpiece[move.from] == toCPiece(Piece.pawn, player); }

	void update(bool quiet = true)(const Move move) {
		const to = move.to.toBit;
		const enemy = opponent(player);
		const p = toPiece(cpiece[move.from]);
		const Stack *u = &stack[ply];
		Stack *n = &stack[ply + 1];

		static if (quiet) n.key.update(this, move);
		n.castling = u.castling;
		n.enpassant = Square.none;
		n.fifty = cast (byte) (u.fifty + 1);
		n.castled = isCastling(move);

		if (move != 0) {
			if (n.castled) {
				n.castling &= ~(piece[Piece.rook] & color[player]);
				xKing[player] = kingCastleTo[move.side][player];
				castle(move.from, kingCastleTo[move.side][player], move.to, rookCastleTo[move.side][player]);
			} else {
				n.castling &= ~(move.from.toBit) & ~(move.to.toBit);
				n.victim = toPiece(cpiece[move.to]);
				deplace(move.from, move.to, p);
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
						const x = move.to.shift;
						capture(Piece.pawn, x, enemy);
						cpiece[x] = CPiece.none;
					} else if (abs(move.to - move.from) == 16 && (mask[move.to].enpassant & (color[enemy] & piece[Piece.pawn]))) {
						n.enpassant = move.to.shift;
					}
				} else if (p == Piece.king) {
					n.castling &= ~(piece[Piece.rook] & color[player]);
					xKing[player] = move.to;
				}
			}
		}		
		player = enemy;
		setPinsCheckers(n.checkers, n.pins);
		++ply;
	}

	void restore(const Move move) {
		const ulong to = move.to.toBit;
		const Color enemy = player;
		const p = move.promotion ? Piece.pawn : toPiece(cpiece[move.to]);
		const Stack *n = &stack[ply];
		const Stack *u = &stack[--ply];

		player = opponent(enemy);
		if (move != 0) {
			if (n.castled) {
				castle(kingCastleTo[move.side][player], move.from, rookCastleTo[move.side][player], move.to);
				xKing[player] = move.from;
			} else {
				deplace(move.to, move.from, p);
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
						const Square x = move.to.shift;
						capture(Piece.pawn, x, enemy);
						cpiece[x] = toCPiece(Piece.pawn, enemy);
					}
				} 
				if (p == Piece.king) xKing[player] = move.from;
			}
		}
	}

	void generateMoves(bool doQuiet = true)(ref Moves moves) const {
		const Color enemy = opponent(player);
		const ulong occupancy = ~piece[Piece.none];
		const ulong pinfree = color[player] & ~stack[ply].pins;
		const ulong bq = piece[Piece.bishop] | piece[Piece.queen];
		const ulong rq = piece[Piece.rook] | piece[Piece.queen];
		const ulong checkers = stack[ply].checkers;
		const Square k = xKing[player];
		const int push = pushTable[player];
		const int left = push - 1;
		const int right = push + 1;
		ulong empties = piece[Piece.none] , enemies = color[enemy];
		ulong target, attacker, o, attacked;
		Square from, to, x;
		int d;

		if (checkers) {
			if (hasSingleBit(checkers)) {
				x = firstSquare(checkers);
				empties = mask[k].between[x];
				enemies = checkers;
				target = enemies; static if (doQuiet) target |= empties;
			} else {
				target = enemies = empties = 0;
			}
		} else {
			target = enemies; static if (doQuiet) target |= empties;

			static if (doQuiet) {
				ulong rooks = stack[ply].castling & color[player];
				while (rooks) {
					x = popSquare(rooks);
					if (canCastle(k, x, occupancy)) moves.push(k, x);
				}
			}

			attacker = piece[Piece.pawn] & stack[ply].pins;
			while (attacker) {
				from = popSquare(attacker);
				d = mask[k].direction[from];
				if (d == abs(left) && toColor(cpiece[to = from.shift(left)]) == enemy) {
					if (rank(forward(from, player)) == 6) moves.pushPromotions(from, to);
					else moves.push(from, to);
				} else if (d == abs(right) && toColor(cpiece[to = from.shift(right)]) == enemy) {
					if (rank(forward(from, player)) == 6) moves.pushPromotions(from, to);
					else moves.push(from, to);
				} else if (doQuiet && d == abs(push) && cpiece[to = from.shift(push)] == CPiece.none) {
					moves.push(from, to);
					if (rank(forward(from, player)) == 1 && cpiece[to = to.shift(push)] == CPiece.none) moves.push(from, to);
				}
			}

			attacker = bq & stack[ply].pins;
			while (attacker) {
				from = popSquare(attacker);
				d = mask[k].direction[from];
				if (d == 9) generateMoves(moves, attack(Piece.bishop, from, target & mask[from].diagonal, occupancy), from);
				else if (d == 7) generateMoves(moves, attack(Piece.bishop, from, target & mask[from].antidiagonal, occupancy), from);
			}

			attacker = rq & stack[ply].pins;
			while (attacker) {
				from = popSquare(attacker);
				d = mask[k].direction[from];
				if (d == 1) generateMoves(moves, attack(Piece.rook, from, target & mask[from].rank, occupancy), from);
				else if (d == 8) generateMoves(moves, attack(Piece.rook, from, target & mask[from].file, occupancy), from);
			}
		}

		if (stack[ply].enpassant != Square.none && (!checkers || x == stack[ply].enpassant.shift)) {
			to = stack[ply].enpassant;
			x = to.shift;
			from = cast (Square) (x - 1);
			if (file(to) > 0 && cpiece[from] == toCPiece(Piece.pawn, player)) {
				o = occupancy ^ (from.toBit | x.toBit | to.toBit);
				if (!attack(Piece.bishop, k, bq & color[enemy], o) && !attack(Piece.rook, k, rq & color[enemy], o)) moves.push(from, to);
			}
			from = cast (Square) (x + 1);
			if (file(to) < 7 && cpiece[from] == toCPiece(Piece.pawn, player)) {
				o = occupancy ^ (from.toBit | x.toBit | to.toBit);
				if (!attack(Piece.bishop, k, bq & color[enemy], o) && !attack(Piece.rook, k, rq & color[enemy], o)) moves.push(from, to);
			}
		}

		attacker = piece[Piece.pawn] & pinfree;
		attacked = (player ? (attacker & ~fileMask[0]) >> 9 : (attacker & ~fileMask[0]) << 7) & enemies;
		generatePromotions!doQuiet(moves, attacked & rank8[player], left);
		generatePawns(moves, attacked & ~rank8[player], left);
		attacked = (player ? (attacker & ~fileMask[7]) >> 7 : (attacker & ~fileMask[7]) << 9) & enemies;
		generatePromotions!doQuiet(moves, attacked & rank8[player], right);
		generatePawns(moves, attacked & ~rank8[player], right);
		attacked = (player ? attacker >> 8 : attacker << 8) & piece[Piece.none];
		generatePromotions(moves, attacked & rank8[player] & empties, push);
		generatePawns(moves, attacked & rank7[player] & empties, push);
		static if (doQuiet) {
			generatePawns(moves, attacked & ~(rank8[player] | rank7[player]) & empties, push);
			attacked = (player ? (attacked & rankMask[5]) >> 8 : (attacked & rankMask[2]) << 8) & empties;
			generatePawns(moves, attacked, 2 * push);
		}

		generatePieceMoves(Piece.knight, moves, piece[Piece.knight] & pinfree, target); 
		generatePieceMoves(Piece.bishop, moves, bq & pinfree, target); 
		generatePieceMoves(Piece.rook, moves, rq & pinfree, target); 

		target = color[enemy]; static if (doQuiet) target |= piece[Piece.none];
		attacked = attack(Piece.king, k, target);
		o = occupancy ^ k.toBit;
		while (attacked) {
			to = popSquare(attacked);
			if (!isSquareAttacked(to, enemy, o)) moves.push(k, to);
		}
	}

	Piece nextAttacker(ref ulong [Color.size] board, const Square to, const Color player, const Color enemy, const Piece last) const {
		static immutable Piece [Piece.size] next = [Piece.none, Piece.pawn, Piece.knight, Piece.bishop, Piece.rook, Piece.bishop, Piece.size];
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

	int see(const Move move) const {
		const Color enemy = opponent(player);
		ulong [Color.size] board = color;
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
}

