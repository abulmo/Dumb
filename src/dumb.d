/*
 * File dumb.d
 * Universal Chess Interface.
 * © 2017-2020 Richard Delorme
 */

module dumb;

import board, eval, move, search, util;
import std.algorithm, std.array, std.concurrency, std.conv, std.stdio, std.string;

void eventLoop(shared Event e) { e.loop(); }

/* Universal Chess Interface. */
class Uci {
	struct Time {
		double remaining = 0.0, increment = 0.0;
	}
	Search search;
	Board board;
	Moves moves;
	shared Event event;	
	Time [Color.size] time;
	int depthMax, movesToGo;
	ulong nodesMax;
	bool canPonder, isPondering, easy;

	this() {
		search = new Search;
		search.event = event = new shared Event;
		search.board = board = new Board;
		ucinewgame();
		canPonder = false;
	}

	double setTime() const {
		const p = board.player;
		double t = time[p].remaining;

		if (t > 0) {
			const int todo = movesToGo > 0 ? todo = movesToGo : 40;
			t += time[p].increment * todo;
			t = max(t - 1.0, 0.95 * t) / todo;
		} else {
			t = time[p].increment;
			t = t > 0 ? max(t - 1.0, 0.95 * t) : double.infinity;
		}

		return t;
	}

	void uci() const {
		writeln("id name dumb 1.5");
		writeln("id author Richard Delorme");
		writeln("option name Ponder type check default false");
		writeln("option name Hash type spin default 64 min 1 max 65536");
		writeln("uciok");
	}

	void setoption(string line) {
		const name = findBetween(line.chomp(), "name", "value").strip().toLower();
		findSkip(line, "value");
		string value = line.strip().toLower();
		if (name == "ponder") canPonder = to!bool(value);
		else if (name == "hash") search.resize(to!size_t(value) * 1024 * 1024);			
	}

	void ucinewgame() {
		search.clear();
		board.set();
		search.set();
	}

	void position(string line) {
		if (findSkip(line, "startpos")) board.set();
		else if (findSkip(line, "fen")) board.set(line);
		if (findSkip(line, "moves")) {
			auto words = line.split();
			foreach(w ; words) board.update(fromPan(w));
		}
		search.set();
	}

	void bestmove() {
		if (search.hint != 0 && canPonder) writeln("bestmove ", search.bestMove.toPan(), " ponder ", search.hint.toPan());
		else writeln("bestmove ", search.bestMove.toPan());
	}

	void bench(const int depth) {
		string [24] fens = [
			"1k1r4/pp1b1R2/3q2pp/4p3/2B5/4Q3/PPP2B2/2K5 b - -",
			"3r1k2/4npp1/1ppr3p/p6P/P2PPPP1/1NR5/5K2/2R5 w - - ",
			"2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - -",
			"rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq -",
			"r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - -",
			"2r3k1/pppR1pp1/4p3/4P1P1/5P2/1P4K1/P1P5/8 w - -",
			"1nk1r1r1/pp2n1pp/4p3/q2pPp1N/b1pP1P2/B1P2R2/2P1B1PP/R2Q2K1 w - -",
			"4b3/p3kp2/6p1/3pP2p/2pP1P2/4K1P1/P3N2P/8 w - -",
			"2kr1bnr/pbpq4/2n1pp2/3p3p/3P1P1B/2N2N1Q/PPP3PP/2KR1B1R w - -",
			"3rr1k1/pp3pp1/1qn2np1/8/3p4/PP1R1P2/2P1NQPP/R1B3K1 b - -",
			"2r1nrk1/p2q1ppp/bp1p4/n1pPp3/P1P1P3/2PBB1N1/4QPPP/R4RK1 w - -",
			"r3r1k1/ppqb1ppp/8/4p1NQ/8/2P5/PP3PPP/R3R1K1 b - -",
			"r2q1rk1/4bppp/p2p4/2pP4/3pP3/3Q4/PP1B1PPP/R3R1K1 w - -",
			"rnb2r1k/pp2p2p/2pp2p1/q2P1p2/8/1Pb2NP1/PB2PPBP/R2Q1RK1 w - -",
			"2r3k1/1p2q1pp/2b1pr2/p1pp4/6Q1/1P1PP1R1/P1PN2PP/5RK1 w - -",
			"r1bqkb1r/4npp1/p1p4p/1p1pP1B1/8/1B6/PPPN1PPP/R2Q1RK1 w kq -",
			"r2q1rk1/1ppnbppp/p2p1nb1/3Pp3/2P1P1P1/2N2N1P/PPB1QP2/R1B2RK1 b - -",
			"r1bq1rk1/pp2ppbp/2np2p1/2n5/P3PP2/N1P2N2/1PB3PP/R1B1QRK1 b - -",
			"3rr3/2pq2pk/p2p1pnp/8/2QBPP2/1P6/P5PP/4RRK1 b - -",
			"r4k2/pb2bp1r/1p1qp2p/3pNp2/3P1P2/2N3P1/PPP1Q2P/2KRR3 w - -",
			"3rn2k/ppb2rpp/2ppqp2/5N2/2P1P3/1P5Q/PB3PPP/3RR1K1 w - -",
			"2r2rk1/1bqnbpp1/1p1ppn1p/pP6/N1P1P3/P2B1N1P/1B2QPP1/R2R2K1 b - -",
			"r1bqk2r/pp2bppp/2p5/3pP3/P2Q1P2/2N1B3/1PP3PP/R4RK1 b kq -",
			"r2qnrnk/p2b2b1/1p1p2pp/2pPpp2/1PP1P3/PRNBB3/3QNPPP/5RK1 w - -"
		];
		const Option option = { {double.max}, {ulong.max}, {depth}, false };
		ulong n;
		double t = 0.0;

		moves.clear();

		foreach (fen; fens) {
			board.set(fen);
			search.set();
			search.go(option, moves);
			bestmove();
			n += search.nNodes;
			t += search.timer.time;
		}

		writeln("bench: ", n, " nodes in ", t, " s, ", cast (int) (n / t), " nps.");
	}

	ulong perft(bool div = false)(const int depth) {
		Moves ms = void;
		ulong count, total;
		Move m;
		Chrono t = void;

		static if (div) t.start();

		ms.generateAll(board);

		static if (!div) if (depth == 1) return ms.length;

		while ((m = ms.next.move) != 0) {
			board.update(m);
				if (depth == 1) count = 1; else count = perft(depth - 1);
				total += count;
				static if (div) writefln("%5s %16d", m.toPan(), count);
			board.restore(m);
		}

		static if (div) writefln("perft %d: %d leaves const %.3fs (%.0f leaves/s)", depth, total, t.time(), total / t.time());

		return total;
	}

	void test() {
		struct TestBoard {
			string comments, fen;
			int depth;
			ulong result;
		}
		TestBoard [] tests = [
			{"1. Initial position ", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 119_060_324},
			{"2. Kiwipete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -", 5, 193_690_690},
			{"3.", "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", 7, 178_633_661},
			{"4.", "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706_045_033},
			{"5.", "rnbqkb1r/pp1p1ppp/2p5/4P3/2B5/8/PPP1NnPP/RNBQK2R w KQkq - 0 6", 3, 53_392},
			{"6.", "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 6, 6_923_051_137},
			{"7.", "8/5bk1/8/2Pp4/8/1K6/8/8 w - d6 0 1", 6, 824_064},
			{"8. Enpassant capture gives check", "8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 6, 1_440_467},
			{"9. Short castling gives check", "5k2/8/8/8/8/8/8/4K2R w K - 0 1", 6, 661_072},
			{"10. Long castling gives check", "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", 6, 803_711},
			{"11. Castling", "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", 4, 1_274_206},
			{"12. Castling prevented", "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", 4, 1_720_476},
			{"13. Promote out of check", "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 6, 3_821_001},
			{"14. Discovered check", "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", 5, 1_004_658},
			{"15. Promotion gives check", "4k3/1P6/8/8/8/8/K7/8 w - - 0 1", 6, 217_342},
			{"16. Underpromotion gives check", "8/P1k5/K7/8/8/8/8/8 w - - 0 1", 6, 92_683},
			{"17. Self stalemate", "K1k5/8/P7/8/8/8/8/8 w - - 0 1", 6, 2_217},
			{"18. Stalemate/Checkmate", "8/k1P5/8/1K6/8/8/8/8 w - - 0 1", 7, 567_584},
			{"19. Double check", "8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", 4, 23_527},
		];

		writeln("Testing the move generator");
		foreach (test; tests) {
			write("Test ", test.comments, " ", test.fen); stdout.flush();
			board.set(test.fen);
			if (perft(test.depth) == test.result) writeln(" passed"); else writeln(" FAILED !");
		}
	}

	void go(string line) {
		Option option = { {double.max}, {ulong.max}, {Limits.ply.max}, false };
		bool isInfinite = isPondering = false;
		string [] words = line.split();

		moves.clear();
		foreach (c ; Color.white .. Color.size) time[c] = Time.init;
		foreach (i, ref w ; words) {
			if (w == "searchmoves") foreach(m ; words[i..$]) moves.push(fromPan(m));
			else if (w == "ponder") isPondering = true;
			else if (w == "wtime" && i + 1 < words.length) time[Color.white].remaining = 0.001 * to!double(words[i + 1]);
			else if (w == "btime" && i + 1 < words.length) time[Color.black].remaining = 0.001 * to!double(words[i + 1]);
			else if (w == "winc" && i + 1 < words.length) time[Color.white].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "binc" && i + 1 < words.length) time[Color.black].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "movestogo" && i + 1 < words.length) movesToGo = to!int(words[i + 1]);
			else if (w == "depth" && i + 1 < words.length) option.depth.max = to!int(words[i + 1]);
			else if (w == "nodes" && i + 1 < words.length) option.nodes.max = to!ulong(words[i + 1]);
			else if (w == "mate" && i + 1 < words.length) option.depth.max = to!int(words[i + 1]);
			else if (w == "movetime" && i + 1 < words.length) time[board.player].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "infinite") { isInfinite = true; option.depth.max =  Limits.ply.max; }
		}
		option.time.max = setTime();
		option.isPondering = isPondering;

		search.go(option, moves);
		if (!isInfinite && !isPondering) bestmove();
	}

	void loop() {
		spawn(&eventLoop, event);
		while (true) {
			auto line = event.wait();
			if (line == "" || line[0] == '#') continue;
			else if (findSkip(line, "ucinewgame")) ucinewgame();
			else if (findSkip(line, "uci")) uci();
			else if (findSkip(line, "isready")) writeln("readyok");
			else if (findSkip(line, "setoption")) setoption(line);
			else if (findSkip(line, "position")) position(line);
			else if (findSkip(line, "go")) go(line);
			else if ((findSkip(line, "stop") || (findSkip(line, "ponderhit")) && isPondering)) bestmove();
			else if (findSkip(line, "quit")) break;
			else if (findSkip(line, "debug")) {}
			else if (findSkip(line, "register")) {}
			else if (findSkip(line, "perft ")) perft!true(to!int(line));
			else if (findSkip(line, "bench ")) bench(to!int(line));
			else writeln("error unknown command: '", line, "'");
		}
	}
}

/* main function */
void main(string [] args) {
	stdout.setvbuf(4096, _IOLBF);
	Uci uci = new Uci;
	if (args.length == 3 && (args[1] == "--bench" || args[1] == "-b")) uci.bench(to!int(args[2]));
	else if (args.length == 2 && (args[1] == "--test" || args[1] == "-t")) uci.test();
	else if (args.length == 3 && (args[1] == "--perft" || args[1] == "-p")) uci.perft!true(to!int(args[2]));
	else if (args.length > 1) stderr.writeln(args[0], " [--bench|-b <depth>] [--test|-t] [--perft|-p <depth>]");
	else uci.loop();
}

