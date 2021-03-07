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
	bool canPonder, isPondering, isInfinite, chess960;

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
			const int todo = movesToGo > 0 ? movesToGo : 40;
			t += time[p].increment * todo;
			t = max(t - 1.0, 0.95 * t) / todo;
		} else {
			t = time[p].increment;
			t = t > 0 ? max(t - 1.0, 0.95 * t) : double.infinity;
		}

		return t;
	}

	void uci() const {
		writeln("id name dumber 1.1");
		writeln("id author Richard Delorme");
		writeln("option name Ponder type check default false");
		writeln("option name UCI_Chess960 type check default false");
		writeln("uciok");
	}

	void setoption(string line) {
		const name = findBetween(line.chomp(), "name", "value").strip().toLower();
		findSkip(line, "value");
		string value = line.strip().toLower();
		if (name == "ponder") canPonder = to!bool(value);
		else if (name == "uci_chess960") chess960 = to!bool(value);
	}

	void ucinewgame() {
		board.set();
		search.set();
	}

	void position(string line) {
		if (findSkip(line, "startpos")) board.set();
		else if (findSkip(line, "fen")) board.set(line);
		board.chess960 = (board.chess960 || chess960);
		if (findSkip(line, "moves")) {
			auto words = line.split();
			foreach(w ; words) board.update(w.fromPan(board));
		}
		search.set();
	}

	void bestmove() {
		if (search.hint != 0 && canPonder) writeln("bestmove ", search.bestMove.toPan(board), " ponder ", search.hint.toPan(board));
		else writeln("bestmove ", search.bestMove.toPan(board));
	}

	void bench(const int depth) {
		string [] fens = [
			"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
			"rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b HAha - 0 1",
			"2rr2k1/pb4pp/1p1qpn2/5p2/2PPn3/P2B1N1P/2Q2PP1/1RBR2K1 w - - 3 19",
			"r4rk1/p3bp1p/bqp1p1p1/3nP2P/1p6/5N2/PPBB1PP1/1K1RQ2R b - - 0 22",
			"8/pr1n1p2/2R1p3/1Pp3kp/2P5/1P1N1KP1/5P2/8 w - - 0 40",
			"8/7R/p1P2p2/P7/1p3k2/5r1p/2K5/8 b - - 7 65",
			"8/5ppk/7p/N1p5/PPP1p3/7R/rr6/4K3 w - - 5 64",
			"8/8/8/4K3/8/1pk5/8/8 b - - 1 78"
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
			n += search.nodes;
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
				static if (div) writefln("%5s %16d", m.toPan(board), count);
			board.restore(m);
		}

		static if (div) writefln("perft %d: %d leaves const %.3fs (%.0f leaves/s)", depth, total, t.time(), total / t.time());

		return total;
	}

	void go(string line) {
		Option option = { {double.max}, {ulong.max}, {Limits.ply.max}, false };
		isInfinite = isPondering = false;
		string [] words = line.split();

		moves.clear();
		foreach (c ; Color.white .. Color.size) time[c] = Time.init;
		foreach (i, ref w ; words) {
			if (w == "searchmoves") {
				foreach(m ; words[i + 1 .. $]) moves.push(m.fromPan(board));
				moves.push(0);
			} else if (w == "ponder") isPondering = true;
			else if (w == "wtime" && i + 1 < words.length) time[Color.white].remaining = 0.001 * to!double(words[i + 1]);
			else if (w == "btime" && i + 1 < words.length) time[Color.black].remaining = 0.001 * to!double(words[i + 1]);
			else if (w == "winc" && i + 1 < words.length) time[Color.white].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "binc" && i + 1 < words.length) time[Color.black].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "movestogo" && i + 1 < words.length) movesToGo = to!int(words[i + 1]);
			else if (w == "depth" && i + 1 < words.length) option.depth.max = to!int(words[i + 1]);
			else if (w == "nodes" && i + 1 < words.length) option.nodes.max = to!ulong(words[i + 1]);
			else if (w == "mate" && i + 1 < words.length) option.depth.max = 2 * to!int(words[i + 1]) - 1;
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
			else if (findSkip(line, "stop") || findSkip(line, "ponderhit")) { if (isInfinite || isPondering) bestmove(); }
			else if (findSkip(line, "quit")) break;
			else if (findSkip(line, "debug")) {}
			else if (findSkip(line, "register")) {}
			else if (findSkip(line, "perft ")) perft!true(to!int(line));
			else if (findSkip(line, "bench ")) bench(to!int(line));

			else writeln("string info Error unknown command: '", line, "'");
		}
	}
}

/* main function */
void main(string [] args) {
	version (Windows) stdout.setvbuf(0, _IONBF); else stdout.setvbuf(4096, _IOLBF);
	Uci uci = new Uci;
	if (args.length == 3 && (args[1] == "--bench" || args[1] == "-b")) uci.bench(to!int(args[2]));
	else if (args.length == 3 && (args[1] == "--perft" || args[1] == "-p")) uci.perft!true(to!int(args[2]));
	else if (args.length > 1) stderr.writeln("Usage: ", args[0], " [--bench|-b <depth>] | [--perft|-p <depth>] | []");
	else uci.loop();
}

