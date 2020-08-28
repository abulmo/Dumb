/*
 * File board.d
 * Chess board representation, move generation, etc.
 * © 2017-2020 Richard Delorme
 */

module board;
import move, util;
import std.ascii, std.conv, std.format, std.stdio, std.string, std.uni;
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

Square toSquare(const int f, const int r) {	return cast (Square) ((r << 3) + f); }

Square toSquare(string s) { return toSquare(s[0] - 'a', s[1] - '1'); }

Square popSquare(ref ulong b) { return cast (Square) popBit(b); }

Square firstSquare(const ulong b) { return cast (Square) bsf(b); }

/* rank/File mask */
immutable ulong [] rankMask = [ 0x00000000000000ff, 0x000000000000ff00, 0x0000000000ff0000, 0x00000000ff000000, 0x000000ff00000000, 0x0000ff0000000000, 0x00ff000000000000, 0xff00000000000000 ];
immutable ulong [] fileMask = [ 0x0101010101010101, 0x0202020202020202, 0x0404040404040404, 0x0808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080 ];

/* Castling */
enum Castling : ubyte {none = 0, K = 1, Q = 2, k = 4, q = 8, size = 16}

int toCastling(const char c) {
	size_t i = indexOf("KQkq", c);
	return i == -1 ? 0 : 1 << i;
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
		r.seed(19_937);
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) square[p][x] = uniform(ulong.min, ulong.max, r);
		foreach (c; Castling.K .. Castling.size) castling[c] = uniform(ulong.min, ulong.max, r);
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
				else if (s.enpassant == move.to) code ^= square[toCPiece(Piece.pawn, enemy)][move.to.shift];
				else if (abs(move.to - move.from) == 16 && (board.mask[move.to].enpassant & (board.color[enemy] & board.piece[Piece.pawn]))) x = move.to.shift;
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


/* Magic */
struct Attack {
	ulong mask, magic, shift;
	ulong [] attack;

	this(const Square x, const ulong mk, const ulong mg, const int [2][4] dir) {
		mask = mk;
		magic = mg;
		shift = 64 - popcnt(mk);
		attack.length = 1 << popcnt(mk);
		ulong o = 0; do {
			attack[Board.magicIndex(o, this)] = Board.computeAttack(x, o, dir);
		} while ((o = (o - mk) & mk) != 0);
	}
}

/* Bitmask */
struct Mask {
	ulong bit, diagonal, antidiagonal, file, rank;
	ulong [Color.size] pawnAttack, push;
	ulong enpassant, knight, king;
	Attack bishop, rook;
	ulong [Square.size] between;
	ubyte [Square.size] direction;
	ubyte castling;
}

/* Kind of move Generation */
enum Generate {all, capture, quiet}

/* Class board */
final class Board {
	static immutable Mask [Square.size] mask;
	static immutable Castling [Color.size] kingside = [Castling.K, Castling.k];
	static immutable Castling [Color.size] queenside = [Castling.Q, Castling.q];
	static immutable int [Piece.size] seeValue = [0, 1, 3, 3, 5, 9, 300];
	static immutable ulong [Color.size] promotionRank = [rankMask[7], rankMask[0]];
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

	shared static this() @trusted {
		int b, y, z;
		byte [Square.size] d;
		static immutable ubyte [6] castling = [13, 12, 14, 7, 3, 11];
		static immutable Square [6] castlingX = [Square.a1, Square.e1, Square.h1, Square.a8, Square.e8, Square.h8];
		static immutable int [2][8] knightDir = [[-2,-1], [-2,1], [-1,-2], [-1,2], [1,-2], [1,2], [2,-1], [2,1]];
	    static const int [2][4] bishopDir = [[-1, -1], [-1, 1], [1, -1], [1, 1]];
		static const int [2][4] rookDir = [[-1, 0], [0, -1], [0, 1], [1, 0]];
		static immutable int [2][8] kingDir   = [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]];
		static immutable ulong [Square.size] bishopMagic = [
			0x88b030028800d040, 0x018242044c008010, 0x0010008200440000, 0x4311040888800a00, 0x001910400000410a, 0x2444240440000000, 0x0cd2080108090008, 0x2048242410041004,
			0x8884441064080180, 0x00042131420a0240, 0x0028882800408400, 0x204384040b820200, 0x0402040420800020, 0x0000020910282304, 0x0096004b10082200, 0x4000a44218410802,
			0x0808034002081241, 0x00101805210e1408, 0x9020400208010220, 0x000820050c010044, 0x0024005480a00000, 0x0000200200900890, 0x808040049c100808, 0x9020202200820802,
			0x0410282124200400, 0x0090106008010110, 0x8001100501004201, 0x0104080004030c10, 0x0080840040802008, 0x2008008102406000, 0x2000888004040460, 0x00d0421242410410,
			0x8410100401280800, 0x0801012000108428, 0x0000402080300b04, 0x0c20020080480080, 0x40100e0201502008, 0x4014208200448800, 0x4050020607084501, 0x1002820180020288,
			0x800610040540a0c0, 0x0301009014081004, 0x2200610040502800, 0x0300442011002800, 0x0001022009002208, 0x0110011000202100, 0x1464082204080240, 0x0021310205800200,
			0x0814020210040109, 0xc102008208c200a0, 0xc100702128080000, 0x0001044205040000, 0x0001041002020000, 0x4200040408021000, 0x004004040c494000, 0x2010108900408080,
			0x0000820801040284, 0x0800004118111000, 0x0203040201108800, 0x2504040804208803, 0x0228000908030400, 0x0010402082020200, 0x00a0402208010100, 0x30c0214202044104
		];
		static immutable ulong [Square.size] rookMagic = [
			0x808000645080c000, 0x208020001480c000, 0x4180100160008048, 0x8180100018001680, 0x4200082010040201, 0x8300220400010008, 0x3100120000890004, 0x4080004500012180,
			0x01548000a1804008, 0x4881004005208900, 0x0480802000801008, 0x02e8808010008800, 0x08cd804800240080, 0x8a058002008c0080, 0x0514000c480a1001, 0x0101000282004d00,
			0x2048848000204000, 0x3020088020804000, 0x4806020020841240, 0x6080420008102202, 0x0010050011000800, 0xac00808004000200, 0x0000010100020004, 0x1500020004004581,
			0x0004c00180052080, 0x0220028480254000, 0x2101200580100080, 0x0407201200084200, 0x0018004900100500, 0x100200020008e410, 0x0081020400100811, 0x0000012200024494,
			0x8006c002808006a5, 0x0004201000404000, 0x0005402202001180, 0x0000081001002100, 0x0000100801000500, 0x4000020080800400, 0x4005050214001008, 0x810100118b000042,
			0x0d01020040820020, 0x000140a010014000, 0x0420001500210040, 0x0054210010030009, 0x0004000408008080, 0x0002000400090100, 0x0000840200010100, 0x0000233442820004,
			0x800a42002b008200, 0x0240200040009080, 0x0242001020408200, 0x4000801000480480, 0x2288008044000880, 0x000a800400020180, 0x0030011002880c00, 0x0041110880440200,
			0x0002001100442082, 0x01a0104002208101, 0x080882014010200a, 0x0000100100600409, 0x0002011048204402, 0x0012000168041002, 0x080100008a000421, 0x0240022044031182
		];

		Square square (int f, int r) { return (0 <= r && r <= 7 && 0 <= f && f <= 7) ? toSquare(f, r) : Square.none; }

		ulong bit(int f, int r) { return (0 <= r && r <= 7 && 0 <= f && f <= 7) ? 1UL << toSquare(f, r) : 0; }

		foreach (x; allSquares) {

			const int f = file(x), r = rank(x);
			const ulong inside = ~(((rankMask[0] | rankMask[7]) & ~rankMask[r]) | ((fileMask[0] | fileMask[7]) & ~fileMask[f]));

			mask[x].bit = 1UL << x;

			foreach (dir; kingDir) {
				foreach (k; 1 .. 8) {
					y = square(f + k * dir[0], r + k * dir[1]);
					if (y != Square.none) {
						d[y] = cast (byte) (dir[0] + 8 * dir[1]);
						mask[x].direction[y] = abs(d[y]);
						for (z = x + d[y]; z != y; z += d[y]) mask[x].between[y] |= 1UL << z;
					}
			 	}
			}

			for (y = x - 9; y >= 0 && d[y] == -9; y -= 9) mask[x].diagonal |= 1UL << y;
			for (y = x + 9; y < Square.size && d[y] == 9; y += 9) mask[x].diagonal |= 1UL << y;
			for (y = x - 7; y >= 0 && d[y] == -7; y -= 7) mask[x].antidiagonal |= 1UL << y;
			for (y = x + 7; y < Square.size && d[y] == 7; y += 7) mask[x].antidiagonal |= 1UL << y;
			mask[x].file = fileMask[file(x)] ^ mask[x].bit;
			mask[x].rank = rankMask[rank(x)] ^ mask[x].bit;

			mask[x].pawnAttack[Color.white] = bit(f - 1, r + 1) | bit (f + 1, r + 1);
			mask[x].pawnAttack[Color.black] = bit(f - 1, r - 1) | bit (f + 1, r - 1);
			mask[x].push[Color.white] |= bit(f, r + 1);
			mask[x].push[Color.black] |= bit(f, r - 1);
			if (r == 3 || r == 4) {
				if (f > 0) mask[x].enpassant |=  1UL << x - 1;
				if (f < 7) mask[x].enpassant |=  1UL << x + 1;
			}
			foreach (dir; knightDir) mask[x].knight |= bit(f + dir[0], r + dir[1]);
			foreach (dir; kingDir) 	 mask[x].king   |= bit(f + dir[0], r + dir[1]);
			mask[x].bishop = cast (immutable) Attack(x, (mask[x].diagonal | mask[x].antidiagonal) & inside, bishopMagic[x], bishopDir);
			mask[x].rook = cast (immutable) Attack(x, (mask[x].rank | mask[x].file) & inside, rookMagic[x], rookDir);

			mask[x].castling = 15;
		}

		foreach (k; 0 .. 6) mask[castlingX[k]].castling = castling[k];
	}

	bool canCastle(const Castling side, const int k, const int r, const ulong occupancy) const {
		const Color enemy = opponent(player);
		if ((stack[ply].castling & side) == 0 || (occupancy & mask[k].between[r]) != 0) return false;
		foreach (x ; (k > r ? k - 2 : k + 1) .. (k > r ? k : k + 3))  if (isSquareAttacked(cast (Square) x, enemy, occupancy)) return false;
		return true;
	}

	static ulong computeAttack(const Square x, const ulong o, const int [2][4] dir) {
		ulong a, b;

		foreach (d; dir) {
			for (int r = rank(x) + d[0], f = file(x) + d[1]; 0 <= r && r < 8 && 0 <= f && f < 8; r += d[0], f += d[1]) {
				a |= (b = 1UL << toSquare(f, r));
				if ((o & b) != 0) break;
			}
		}
		return a;
	}

	static ulong magicIndex(const ulong o, const ref Attack attack) { return ((o & attack.mask) * attack.magic) >> attack.shift; }

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
			case Piece.bishop: return mask[x].bishop.attack[magicIndex(occupancy, mask[x].bishop)];
			case Piece.rook:   return mask[x].rook.attack[magicIndex(occupancy, mask[x].rook)];
			case Piece.queen:  return mask[x].bishop.attack[magicIndex(occupancy, mask[x].bishop)] + mask[x].rook.attack[magicIndex(occupancy, mask[x].rook)];
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

	bool isMated() const {
		Moves moves = void;
		if (inCheck) {
			moves.n = 0;
			generateMoves(moves);
			return moves.n == 0;
		}
		return false;
	}

	bool isDraw() const  @property {
		int nRepetition = 0;
		const end = max(0, ply - stack[ply].fifty);
		for (int i = ply - 4; i >= end; i -= 2) if (stack[i].key.code == stack[ply].key.code && ++nRepetition >= 2) return true;

		if (stack[ply].fifty >= 100 && !isMated) return true;

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
					const x = move.to.shift;
					capture(Piece.pawn, x, enemy);
					cpiece[x] = CPiece.none;
				} else if (abs(move.to - move.from) == 16 && (mask[move.to].enpassant & (color[enemy] & piece[Piece.pawn]))) {
					n.enpassant = move.to.shift;
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
					const Square x = move.to.shift;
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
			} else {
				enemies = empties = 0;
			}
		} else {
			target = enemies; static if (doQuiet) target |= empties;

			static if (doQuiet) {
				if (canCastle(kingside[player], k, k + 3, occupancy)) moves.push(k, cast (Square) (k + 2));
				if (canCastle(queenside[player], k, k - 4, occupancy)) moves.push(k, cast (Square) (k - 2));
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
		
		target = enemies; static if (doQuiet) target |= empties;

		if (stack[ply].enpassant != Square.none && (!checkers || x == stack[ply].enpassant.shift)) {
			to = stack[ply].enpassant;
			x = to.shift;
			from = cast (Square) (x - 1);
			if (file(to) > 0 && cpiece[from] == toCPiece(Piece.pawn, player)) {
				o = occupancy ^ (mask[from].bit | mask[x].bit | mask[to].bit);
				if (!attack(Piece.bishop, k, bq & color[enemy], o) && !attack(Piece.rook, k, rq & color[enemy], o)) moves.push(from, to);
			}
			from = cast (Square) (x + 1);
			if (file(to) < 7 && cpiece[from] == toCPiece(Piece.pawn, player)) {
				o = occupancy ^ (mask[from].bit | mask[x].bit | mask[to].bit);
				if (!attack(Piece.bishop, k, bq & color[enemy], o) && !attack(Piece.rook, k, rq & color[enemy], o)) moves.push(from, to);
			}
		}

		attacker = piece[Piece.pawn] & pinfree;
		attacked = (player ? (attacker & ~fileMask[0]) >> 9 : (attacker & ~fileMask[0]) << 7) & enemies;
		generatePromotions!doQuiet(moves, attacked & promotionRank[player], left);
		generatePawns(moves, attacked & ~promotionRank[player], left);
		attacked = (player ? (attacker & ~fileMask[7]) >> 7 : (attacker & ~fileMask[7]) << 9) & enemies;
		generatePromotions!doQuiet(moves, attacked & promotionRank[player], right);
		generatePawns(moves, attacked & ~promotionRank[player], right);
		attacked = (player ? attacker >> 8 : attacker << 8) & piece[Piece.none];
		generatePromotions(moves, attacked & promotionRank[player] & empties, push);
		static if (doQuiet) {
			generatePawns(moves, attacked & ~promotionRank[player] & empties, push);
			attacked = (player ? (attacked & rankMask[5]) >> 8 : (attacked & rankMask[2]) << 8) & empties;
			generatePawns(moves, attacked, 2 * push);
		}

		generatePieceMoves(Piece.knight, moves, piece[Piece.knight] & pinfree, target); 
		generatePieceMoves(Piece.bishop, moves, bq & pinfree, target); 
		generatePieceMoves(Piece.rook, moves, rq & pinfree, target); 

		target = color[enemy]; static if (doQuiet) target |= piece[Piece.none];
		attacked = attack(Piece.king, k, target);
		o = occupancy ^ mask[k].bit;
		while (attacked) {
			to = popSquare(attacked);
			if (!isSquareAttacked(to, enemy, o)) moves.push(k, to);
		}
	}

}

