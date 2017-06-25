/*
 * File board.d
 * Chess board representation, move generation, etc.
 * © 2016-2017 Richard Delorme
 */

module board;
import move, util;
import std.ascii, std.conv, std.format, std.stdio, std.string, std.uni;
import std.algorithm, std.getopt, std.math, std.random;

/* limits */
struct Limits {
	enum ply { max = 100 }
	enum game { size = 4096 }
}

/* Color */
enum Color : ubyte {white, black, size, none}

Color opponent(const Color c) { return cast (Color) !c; }

Color toColor(const char c) {
	size_t i = indexOf("wb", c);
	if (i == -1) i = Color.size;
	return cast (Color) i;
}

/* Piece */
enum Piece : ubyte {none, pawn, knight, bishop, rook, queen, king, size}

Piece toPiece(const char c) {
	size_t i = indexOf(".pnbrqk", c, CaseSensitive.no);
	if (i == -1) i = 0;
	return cast (Piece) i;
}

char toChar(const Piece p) { return ".PNBRQK?"[p]; }

/* Colored Piece */
enum CPiece : ubyte {none, wpawn, bpawn, wknight, bknight, wbishop, bbishop, wrook, brook, wqueen, bqueen, wking, bking, size}

CPiece toCPiece(const Piece p, const Color c) {	return cast (CPiece) (2 * p + c - 1); }

CPiece toCPiece(const char c) {
	size_t i = indexOf(".PpNnBbRrQqKk", c);
	if (i == -1) i = 0;
	return cast (CPiece) i;
}

Color toColor(const CPiece p) {
	static immutable Color[CPiece.size] c= [Color.none,
		Color.white, Color.black, Color.white, Color.black, Color.white, Color.black,
		Color.white, Color.black, Color.white, Color.black, Color.white, Color.black];
	return c[p];
}

Piece toPiece(const CPiece p) { return cast (Piece) ((p + 1) / 2); }

/* Square */
enum Square : ubyte {
	none = 65,
	a1 = 0, b1, c1, d1, e1, f1, g1, h1,
	a2, b2, c2, d2, e2, f2, g2, h2,
	a3, b3, c3, d3, e3, f3, g3, h3,
	a4, b4, c4, d4, e4, f4, g4, h4,
	a5, b5, c5, d5, e5, f5, g5, h5,
	a6, b6, c6, d6, e6, f6, g6, h6,
	a7, b7, c7, d7, e7, f7, g7, h7,
	a8, b8, c8, d8, e8, f8, g8, h8,
	size,
}

Square shift(const Square x, const int δ) { return cast (Square) (x + δ); }

Square forward(const Square x, const Color c) { return cast (Square) (x ^ (56 * c)); }

int rank(const Square x) { return x >> 3; }

int file(const Square x) { return x & 7; }

Square toSquare(const int f, const int r) {	return cast (Square) ((r << 3) + f); }

Square toSquare(string s) { return toSquare(s[0] - 'a', s[1] - '1'); }

Square popSquare(ref ulong b) { return cast (Square) popBit(b); }

Square firstSquare(const ulong b) { return cast (Square) firstBit(b); }

/* File mask */
enum File {
	A = 0x0101010101010101,
	H = 0x8080808080808080
}

/* Rank mask */
enum Rank {
	r1 = 0x00000000000000ffUL,
	r2 = 0x000000000000ff00UL,
	r3 = 0x0000000000ff0000UL,
	r6 = 0x0000ff0000000000UL,
	r7 = 0x00ff000000000000UL,
	r8 = 0xff00000000000000UL
}

/* Castling */
enum Castling : ubyte {none = 0, K = 1, Q = 2, k = 4, q = 8, size = 16}

int toCastling(const char c) {
	size_t i = indexOf("KQkq", c);
	if (i == -1) return 0;
	else return 1 << i;
}

/* Zobrist key */
struct Key {
	ulong code;

	static immutable ulong [Square.size][CPiece.size] square;
	static immutable ulong [Castling.size] castling;
	static immutable ulong [Square.none + 1] enpassant;
	static immutable ulong [Color.size] color;
	static immutable ulong play;

	shared static this() {
		Mt19937 r;
		r.seed(19937);
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) square[p][x] = uniform(ulong.min, ulong.max, r);
		foreach (c; Castling.K .. Castling.size) castling[c] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a4 .. Square.a6) enpassant[x] = uniform(ulong.min, ulong.max, r);
		foreach (c; Color.white .. Color.size) color[c] = uniform(ulong.min, ulong.max, r);
		play = color[Color.white] ^ color[Color.black];
	}

	void set(const Board board) {
		const Board.Stack *s = &board.stack[board.ply];
		code = color[board.player];
		foreach (Square x; Square.a1 .. Square.size) code ^= square[board[x]][x];
		code ^= enpassant[s.enpassant];
		code ^= castling[s.castling];
	}

	void update(const Board board, const Move move) {
		Square x = Square.none;
		const Color player = board.player;
		const Color enemy = opponent(player);
		const CPiece p = board[move.from];
		const Board.Stack *s = &board.stack[board.ply];

		code = s.key.code;
		code ^= play;
		if (move != 0) {
			code ^= square[p][move.from] ^ square[p][move.to];
			code ^= square[board[move.to]][move.to];
			if (toPiece(p) == Piece.pawn) {
				if (move.promotion) code ^= square[p][move.to] ^ square[toCPiece(move.promotion, player)][move.to];
				else if (s.enpassant == move.to) code ^= square[toCPiece(Piece.pawn, enemy)][toSquare(file(move.to), rank(move.from))];
				else if (abs(move.to - move.from) == 16 && (board.mask[move.to].enpassant & (board.color[enemy] & board.piece[Piece.pawn]))) x = cast (Square) ((move.from + move.to) / 2);
			} else if (toPiece(p) == Piece.king) {
				CPiece r = toCPiece(Piece.rook, board.player);
				if (move.to == move.from + 2) code ^= square[r][move.from + 3] ^ square[r][move.from + 1];
				else if (move.to == move.from - 2) code ^= square[r][move.from - 4] ^ square[r][move.from - 1];
			}
			code ^= enpassant[s.enpassant] ^ enpassant[x];
			code ^= castling[s.castling] ^ castling[s.castling & board.mask[move.from].castling & board.mask[move.to].castling];
		}
	}
}


/* Bitmask */
struct Mask {
	ulong bit, diagonal, antidiagonal, file;
	ulong [Color.size] pawnAttack, push;
	ulong enpassant, knight, king;
	ulong [Square.size] between;
	ubyte [Square.size] direction;
	ubyte castling;
}

/* Kind of move Generation */
enum Generate {all, capture, quiet}

/* Class board */
final class Board {
	static immutable Mask [Square.size] mask;
	static immutable ubyte [512] ranks;
	static immutable Castling [Color.size] kingside = [Castling.K, Castling.k];
	static immutable Castling [Color.size] queenside = [Castling.Q, Castling.q];
	static immutable int [Piece.size] seeValue = [0, 1, 3, 3, 5, 9, 300];
	static immutable Rank [Color.size] promotionRank = [Rank.r8, Rank.r1];
	static immutable int [Color.size] pushTable = [8, -8];

	struct Stack {
		ulong pins;
		ulong checkers;
		Key key;
		Square enpassant = Square.none;
		Piece victim;
		byte fifty;
		Castling castling;
	}
	ulong [Piece.size] piece;
	ulong [Color.size] color;
	CPiece [Square.size] cpiece;
	Stack [Limits.game.size] stack;
	Square [Color.size] xKing;
	Color player;
	int ply, plyOffset;

	shared static this() {
		int b, r, f, i, j, c, y, z;
		byte [Square.size][Square.size] d;
		static immutable ubyte [6] castling = [13, 12, 14, 7, 3, 11];
		static immutable Square [6] castlingX = [Square.a1, Square.e1, Square.h1, Square.a8, Square.e8, Square.h8];

		foreach (x; Square.a1 .. Square.size) {

			for (i = -1; i <= 1; ++i)
			for (j = -1; j <= 1; ++j) {
				if (i == 0 && j == 0) continue;
				for (r = (x >> 3) + i, f = (x & 7) + j; 0 <= r && r < 8 && 0 <= f && f < 8; r += i, f += j) {
			 		y = 8 * r + f;
					d[x][y] = cast (byte) (8 * i + j);
					mask[x].direction[y] = abs(d[x][y]);
					for (z = x + d[x][y]; z != y; z += d[x][y]) mask[x].between[y] |= 1UL << z;
			 	}
			}

			mask[x].bit = 1UL << x;

			for (y = x - 9; y >= 0 && d[x][y] == -9; y -= 9) mask[x].diagonal |= 1UL << y;
			for (y = x + 9; y < Square.size && d[x][y] == 9; y += 9) mask[x].diagonal |= 1UL << y;

			for (y = x - 7; y >= 0 && d[x][y] == -7; y -= 7) mask[x].antidiagonal |= 1UL << y;
			for (y = x + 7; y < Square.size && d[x][y] == 7; y += 7) mask[x].antidiagonal |= 1UL << y;

			for (y = x - 8; y >= 0; y -= 8) mask[x].file |= 1UL << y;
			for (y = x + 8; y < Square.size; y += 8) mask[x].file |= 1UL << y;

			f = x & 07;
			r = x >> 3;
			for (i = -1, c = 1; i <= 1; i += 2, c = 0) {
				for (j = -1; j <= 1; j += 2) {
					if (0 <= r + i && r + i < 8 && 0 <= f + j && f + j < 8) {
						 y = (r + i) * 8 + (f + j);
						 mask[x].pawnAttack[c] |= 1UL << y;
					}
				}
				if (0 <= r + i && r + i < 8) {
					y = (r + i) * 8 + f;
					mask[x].push[c] = 1UL << y;
				}
			}
			if (r == 3 || r == 4) {
				if (f > 0) mask[x].enpassant |=  1UL << x - 1;
				if (f < 7) mask[x].enpassant |=  1UL << x + 1;
			}

			for (i = -2; i <= 2; i = (i == -1 ? 1 : i + 1))
			for (j = -2; j <= 2; ++j) {
				if (i == j || i == -j || j == 0) continue;
				if (0 <= r + i && r + i < 8 && 0 <= f + j && f + j < 8) {
			 		y = 8 * (r + i) + (f + j);
			 		mask[x].knight |= 1UL << y;
				}
			}

			for (i = -1; i <= 1; ++i)
			for (j = -1; j <= 1; ++j) {
				if (i == 0 && j == 0) continue;
				if (0 <= r + i && r + i < 8 && 0 <= f + j && f + j < 8) {
			 		y = 8 * (r + i) + (f + j);
			 		mask[x].king |= 1UL << y;
				}
			}
			mask[x].castling = 15;
		}

		foreach (k; 0 .. 6) mask[castlingX[k]].castling = castling[k];

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

	bool canCastleKingside() const { return (stack[ply].castling & kingside[player]) != 0; }

	bool canCastleQueenside() const { return (stack[ply].castling & queenside[player]) != 0; }

	static ulong attack(const ulong occupancy, const Square x, const ulong m)  {
		const ulong o = occupancy & m;
		const ulong r = swapBytes(o);
		return ((o - mask[x].bit) ^ swapBytes(r - mask[x ^ 56].bit)) & m;
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

		b = coverage!(Piece.bishop)(k, occupancy);
		checkers = partialCheckers = b & bq;
		b &= color[player];
		if (b) {
			b = attack!(Piece.bishop)(k, bq ^ partialCheckers, occupancy ^ b);
			while (b) {
				x = popSquare(b);
				pins |= mask[k].between[x] & color[player];
			}
		}

		b = coverage!(Piece.rook)(k, occupancy);
		checkers |= partialCheckers = b & rq;
		b &= color[player];
		if (b) {
			b = attack!(Piece.rook)(k, rq ^ partialCheckers, occupancy ^ b);
			while (b) {
				x = popSquare(b);
				pins |= mask[k].between[x] & color[player];
			}
		}

		checkers |= attack!(Piece.knight)(k, piece[Piece.knight]);
		checkers |= attack!(Piece.pawn)(k, piece[Piece.pawn], occupancy, player);
		checkers &= color[enemy];
	}

	void deplace(const int from, const int to, const Piece p) {
		const ulong M = mask[from].bit | mask[to].bit;
		piece[Piece.none] ^= M;
		piece[p] ^= M;
		color[player] ^= M;
		cpiece[to] = cpiece[from];
		cpiece[from] = CPiece.none;
	}

	void capture(const Piece victim, const Square x, const Color enemy) {
		const ulong M = mask[x].bit;
		piece[Piece.none] ^= M;
		piece[victim] ^= M;
		color[enemy] ^= M;
	}

	bool isSquareAttacked(const Square x, const Color player) const {
		const ulong occupancy = ~piece[Piece.none];
		const ulong P = color[player];

		return attack!(Piece.bishop)(x, P & (piece[Piece.bishop] | piece[Piece.queen]), occupancy)
			|| attack!(Piece.rook)(x, P & (piece[Piece.rook] | piece[Piece.queen]), occupancy)
			|| attack!(Piece.knight)(x, P & piece[Piece.knight])
			|| attack!(Piece.pawn)(x, P & piece[Piece.pawn], occupancy, opponent(player))
			|| attack!(Piece.king)(x, P & piece[Piece.king]);
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
			Square from = cast (Square) (to - dir);
			moves.pushPromotions!doQuiet(from, to);
		}
	}

	static void generatePawns(ref Moves moves, ulong attack, const int dir) {
		while (attack) {
			Square to = popSquare(attack);
			Square from = cast (Square) (to - dir);
			moves.push(from, to);
		}
	}

	void generatePieceMoves(Piece p)(ref Moves moves, ulong attacker, const ulong target) const {
		const ulong occupancy = ~piece[Piece.none];

		while (attacker) {
			Square from = popSquare(attacker);
			generateMoves(moves, attack!(p)(from, target, occupancy), from);
		}
	}

	static ulong coverage(Piece p)(const Square x, const ulong occupancy = 0, const Color c = Color.white) {
		static if (p == Piece.pawn) return mask[x].pawnAttack[c];
		else static if (p == Piece.knight) return mask[x].knight;
		else static if (p == Piece.bishop) return diagonalAttack(occupancy, x) + antidiagonalAttack(occupancy, x);
		else static if (p == Piece.rook) return fileAttack(occupancy, x) + rankAttack(occupancy, x);
		else static if (p == Piece.queen) return diagonalAttack(occupancy, x) + antidiagonalAttack(occupancy, x) + fileAttack(occupancy, x) + rankAttack(occupancy, x);
		else static if (p == Piece.king) return mask[x].king;
	}

	static ulong attack(Piece p)(const Square x, const ulong target, const ulong occupancy = 0, const Color c = Color.white) { return coverage!p(x, occupancy, c) & target;	}

	void clear() {
		foreach (p; Piece.none .. Piece.size) piece[p] = 0;
		foreach (c; Color.white .. Color.size) color[c] = 0;
		foreach (x; Square.a1 .. Square.size) cpiece[x] = CPiece.none;
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

		void error(string msg) { throw new Exception("Bad FEN: " ~ msg ~ " ; fen "); }

		clear();

		if (s.length < 4) error("missing fields");

		foreach (c; s[0]) {
			if (c== '/') {
				if (r <= 0) error("rank overflow");
				if (f != 8) error("missing square");
				f = 0; --r;
			} else if (isDigit(c)) {
				f += c - '0';
				if (f > 8) error("file overflow");
			} else {
				if (f > 8) error("file overflow");
				x = toSquare(f, r);
				cpiece[x] = p = toCPiece(c);
				if (cpiece[x] == CPiece.size) error("bad piece");
				piece[toPiece(p)] |= mask[x].bit;
				color[toColor(p)] |= mask[x].bit;
				if (toPiece(p) == Piece.king) xKing[toColor(p)] = x;
				++f;
			}
		}
		if (r > 0 || f != 8) error("missing squares");

		player = toColor(s[1][0]);
		if (player == Color.size) error("bad player's turn");

		if (s.length > 5 && isNumeric(s[4])) {
			stack[ply].fifty = std.conv.to!ubyte(s[4]);
			plyOffset = 2 * (std.conv.to!int(s[5]) - 1) + player;
		}

		if (s[2] != "-") {
			foreach (c; s[2]) stack[ply].castling |= toCastling(c);
		}
		if (cpiece[Square.e1] == CPiece.wking) {
			if (cpiece[Square.h1] != CPiece.wrook) stack[ply].castling &= ~1;
			if (cpiece[Square.a1] != CPiece.wrook) stack[ply].castling &= ~2;
		} else stack[ply].castling &= ~3;
		if (cpiece[Square.e8] == CPiece.bking) {
			if (cpiece[Square.h8] != CPiece.brook) stack[ply].castling &= ~4;
			if (cpiece[Square.a8] != CPiece.brook) stack[ply].castling &= ~8;
		} else stack[ply].castling &= ~12;

		if (s[3] != "-") {
			stack[ply].enpassant = toSquare(s[3]);
			if (stack[ply].enpassant == Square.none) error("bad enpassant");
		}

		piece[Piece.none] = ~(color[Color.white] | color[Color.black]);
		setPinsCheckers(stack[ply].checkers, stack[ply].pins);
		stack[ply].key.set(this);
	}

	this() { set(); }

	CPiece opIndex(const Square x) const { return cpiece[x]; }

	Key key() const @property { return stack[ply].key; }

	bool inCheck() const @property { return stack[ply].checkers > 0; }

	bool isDraw() const  @property {
		int nRepetition = 0;
		const end = max(0, ply - stack[ply].fifty);
		for (int i = ply - 4; i >= end; i -= 2) if (stack[i].key.code == stack[ply].key.code && ++nRepetition >= 2) return true;

		if (stack[ply].fifty > 100) return true;

		if (piece[Piece.pawn] + piece[Piece.rook] + piece[Piece.queen] == 0) {
			const nMinor = countBits(piece[Piece.knight] + piece[Piece.bishop]);
			if (nMinor <= 1) return true;
			const diff = abs(countBits(color[Color.white]) - countBits(color[Color.black]));
			const blackSquares = 0x55aa55aa55aa55aa;
			const whiteSquares = ~blackSquares;
			if (diff == nMinor && piece[Piece.knight] == 0 && ((piece[Piece.bishop] & blackSquares) == piece[Piece.bishop] || (piece[Piece.bishop] & whiteSquares) == piece[Piece.bishop])) return true;
		}

		return false;
	}

	void update(const Move move) {
		const to = mask[move.to].bit;
		const enemy = opponent(player);
		const p = toPiece(cpiece[move.from]);
		const Stack *u = &stack[ply];
		Stack *n = &stack[ply + 1];

		n.key.update(this, move);
		n.castling = u.castling;
		n.enpassant = Square.none;
		n.fifty = cast (byte) (u.fifty + 1);

		if (move != 0) {
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
					const x = toSquare(file(move.to), rank(move.from));
					capture(Piece.pawn, x, enemy);
					cpiece[x] = CPiece.none;
				} else if (abs(move.to - move.from) == 16 && (mask[move.to].enpassant & (color[enemy] & piece[Piece.pawn]))) {
					n.enpassant = cast (Square) ((move.from + move.to) / 2);
				}
			} else if (p == Piece.king) {
				if (move.to == move.from + 2) deplace(move.from + 3, move.from + 1, Piece.rook);
				else if (move.to == move.from - 2) deplace(move.from - 4, move.from - 1, Piece.rook);
				xKing[player] = move.to;
			}
			n.castling &= (mask[move.from].castling & mask[move.to].castling);
		}

		player = enemy;
		setPinsCheckers(n.checkers, n.pins);
		++ply;
	}

	void restore(const Move move) {
		const ulong to = mask[move.to].bit;
		const Color enemy = player;
		const p = move.promotion ? Piece.pawn : toPiece(cpiece[move.to]);
		const Stack *n = &stack[ply];
		const Stack *u = &stack[--ply];

		player = opponent(enemy);
		if (move != 0) {
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
					const x = toSquare(file(move.to), rank(move.from));
					capture(Piece.pawn, x, enemy);
					cpiece[x] = toCPiece(Piece.pawn, enemy);
				}
			} else if (p == Piece.king) {
				if (move.to == move.from + 2) deplace(move.from + 1, move.from + 3, Piece.rook);
				else if (move.to == move.from - 2) deplace(move.from - 1, move.from - 4, Piece.rook);
				xKing[player] = move.from;
			}
		}
	}

	void generateMoves(bool doQuiet = true)(ref Moves moves) {
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
				if (canCastleKingside()
					&& (occupancy & mask[k].between[k + 3]) == 0
					&& !isSquareAttacked(cast (Square) (k + 1), enemy)
					&& !isSquareAttacked(cast (Square) (k + 2), enemy)) moves.push(k, cast (Square) (k + 2));
				if (canCastleQueenside()
					&& (occupancy & mask[k].between[k - 4]) == 0
					&& !isSquareAttacked(cast (Square) (k - 1), enemy)
					&& !isSquareAttacked(cast (Square) (k - 2), enemy)) moves.push(k, cast (Square) (k - 2));
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
				if (d == 9) generateMoves(moves, diagonalAttack(occupancy, from) & target, from);
				else if (d == 7) generateMoves(moves, antidiagonalAttack(occupancy, from) & target, from);
			}


			attacker = rq & stack[ply].pins;
			while (attacker) {
				from = popSquare(attacker);
				d = mask[k].direction[from];
				if (d == 1) generateMoves(moves, rankAttack(occupancy, from) & target, from);
				else if (d == 8) generateMoves(moves, fileAttack(occupancy, from) & target, from);
			}
		}

		if (stack[ply].enpassant != Square.none && (!checkers || x == stack[ply].enpassant)) {
			to = stack[ply].enpassant;
			x = to.shift(-push);
			from = cast (Square) (x - 1);
			if (file(to) > 0 && cpiece[from] == toCPiece(Piece.pawn, player)) {
				o = occupancy ^ (mask[from].bit | mask[x].bit | mask[to].bit);
				if (!attack!(Piece.bishop)(k, bq & color[enemy], o) && !attack!(Piece.rook)(k, rq & color[enemy], o)) moves.push(from, to);
			}
			from = cast (Square) (x + 1);
			if (file(to) < 7 && cpiece[from] == toCPiece(Piece.pawn, player)) {
				o = occupancy ^ (mask[from].bit | mask[x].bit | mask[to].bit);
				if (!attack!(Piece.bishop)(k, bq & color[enemy], o) && !attack!(Piece.rook)(k, rq & color[enemy], o)) moves.push(from, to);
			}
		}

		attacker = piece[Piece.pawn] & pinfree;
		attacked = (player ? (attacker & ~File.A) >> 9 : (attacker & ~File.A) << 7) & enemies;
		generatePromotions!doQuiet(moves, attacked & promotionRank[player], left);
		generatePawns(moves, attacked & ~promotionRank[player], left);
		attacked = (player ? (attacker & ~File.H) >> 7 : (attacker & ~File.H) << 9) & enemies;
		generatePromotions!doQuiet(moves, attacked & promotionRank[player], right);
		generatePawns(moves, attacked & ~promotionRank[player], right);
		attacked = (player ? attacker >> 8 : attacker << 8) & piece[Piece.none];
		generatePromotions(moves, attacked & promotionRank[player] & empties, push);
		static if (doQuiet) {
			generatePawns(moves, attacked & ~promotionRank[player] & empties, push);
			attacked = (player ? (attacked & Rank.r6) >> 8 : (attacked & Rank.r3) << 8) & empties;
			generatePawns(moves, attacked, 2 * push);
		}

		generatePieceMoves!(Piece.knight)(moves, piece[Piece.knight] & pinfree, target); 
		generatePieceMoves!(Piece.bishop)(moves, bq & pinfree, target); 
		generatePieceMoves!(Piece.rook)(moves, rq & pinfree, target); 

		target = color[enemy]; static if (doQuiet) target |= piece[Piece.none];
		piece[Piece.none] ^= mask[k].bit;
		attacked = attack!(Piece.king)(k, target);
		while (attacked) {
			to = popSquare(attacked);
			if (!isSquareAttacked(to, enemy)) moves.push(k, to);
		}
		piece[Piece.none] ^= mask[k].bit;
	}
}

