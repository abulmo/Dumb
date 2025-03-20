/**
 * File dumb.d
 *
 *  Program start and Universal Chess Interface implementation.
 *
 * Authors: Richard Delorme
 * Date: 2025
 *
 */

module dumb;

import board, eval, move, search, util;
import std.algorithm, std.array, std.concurrency, std.conv, std.format, std.math, std.stdio, std.string;

@safe:

/**
 * class Uci Universal Chess Interface (UCI)
 *
 * This class implement the UCI protocole to communicate with
 * a graphical user interface.
 *
 * For details, see https://www.shredderchess.com/download/div/uci.zip
 */
final class Uci {
	/**
	 * struct Uci.Time
	 * Contain the alloted time for searching
	 */
	struct Time {
		double remaining = 0.0; /// time left.
		double increment = 0.0; /// time increment.
	}
	Search search;          /// search struct.
	Board board;            /// board struct.
	Event event;            /// event class.
	Time [Color.size] time; /// time available for each player.
	int depthMax;           /// maximal depth to search.
	int movesToGo;          /// moves to go to the next time pool.
	ulong nodesMax;         /// maximal depth to search.
	bool canPonder;         /// a flag allowing pondering.
	bool isPondering;       /// a flag to ponder, ie to search during opponent time.
	bool isInfinite;        /// a flag to search for ever.
	bool chess960;          /// a flag to play Fisher Random Chess, or chess 960.
	bool quiet;             /// a flag to avoid displayiong the search result.

	/**
	 * constructor
	 * Allocate the main classes: search, board & event, and initialize everything.
	 */
	this() {
		search = new Search;
		search.event = event = new Event;
		search.board = board = new Board;
		ucinewgame();
		canPonder = false;
	}

	/**
	 * Compute the available time for the next move to search.
	 *
	 * The time alloted to think has three components:
	 *  - a remaining time to play until the next time control.
	 *  - an increment time that is added at each move.
	 *  - a number of moves to the next time control or the the end of the game.
	 * When the number of moves is 0, the remaining time is given for the full
	 * game. In this case, it is divided by 15 if the position is unknown or 20
	 * if the position is known, ie in the hash table, thus allotting more time
	 * to unknown positions. The increment is then added to the thinking time.
	 *
	 * returns: time available for the current move to search
	 */
	double setTime() pure nothrow const {
		const Color player = board.player;
		double allotedTime = time[player].remaining;
		Entry h;

		if (allotedTime > 0) {
			const bool knownPosition = (search.tt.probe(board.key, h) && h.bound == Bound.exact);
			const int todo = movesToGo > 0 ? movesToGo : 15 + 5 * knownPosition;
			allotedTime = allotedTime / todo + time[player].increment;
		} else if ((allotedTime = time[player].increment) == 0) return double.infinity;

		return max(allotedTime - 1.0, 0.95 * allotedTime);
	}

	/**
	 * Responses to the $I uci command.
	 * Display the name of the engine, its Author and a list of available options.
	 * As *Dumb* is minimalist, options are limited to pondering, chess 960 and
	 * the size of the transposition table.
	 */
	void uci() {
		event.send("id name Dumb 2.3");
		event.send("id author Richard Delorme");
		event.send("option name Ponder type check default false");
		event.send("option name Hash type spin default 64 min 1 max 65536");
		event.send("option name UCI_Chess960 type check default false");

		event.send("uciok");
	}

	/**
	 * Set an option from the $I setoption command.
	 * params: line = a string with the setoption arguments.
	 */
	void setoption(string line) pure {
		findSkip(line,"name");
		const name = split(line, "value")[0].strip().toLower();
		findSkip(line, "value");
		const value = line.strip().toLower();
		if (name == "ponder") canPonder = to!bool(value);
		else if (name == "hash") search.resize(clamp(to!size_t(value), 1, 65_536) << 20);
		else if (name == "uci_chess960") chess960 = to!bool(value);
	}

	/**
	 * Receive the $I ucinewgame command to start a new game: clear Dumb's search,
	 * set board to starting position.
	 */
	void ucinewgame() {
		search.clear();
		board.set();
		search.set();
	}

	/**
	 * Receive the $I position command : set a new position to search.
	 *  The position command can be followed by:
	 *  - $I startpos to set the standard chess starting position.
	 *  - $I fen followed by a FEN string to set a specific position.
	 *  - $I moves followed by a list of moves to play from one of the above positions.
	 *
	 *  params: line = the position command line.
	 */
	void position(string line) {
		if (findSkip(line, "startpos")) board.set();
		else if (findSkip(line, "fen")) board.set(line);
		board.chess960 = chess960;
		if (findSkip(line, "moves")) {
			auto words = line.split();
			foreach(w ; words) board.update(w.fromPan(board));
		}
		search.set();
	}

	/**
	 * Send the best move found once the search is terminated. If pondering is
	 * allowed, also send the move to ponder on.
	 */
	void bestmove() {
		string s = format("bestmove %s", search.bestMove.toPan(board));
		if (search.hint != 0 && canPonder) {
			board.update(search.bestMove);
				s ~= format(" ponder %s", search.hint.toPan(board));
			board.restore(search.bestMove);
		}
		event.send(s);
	}

	/**
	 * Do a small benchmark on various positions.
	 *
	 * params: depth Search depth to run the benchmark with.
	 */
	void bench(const int depth) {
		struct Fen {
			string fen;
			bool chess960;
		}
		Fen [] fens = [
			{ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", false },
			{ "nbrqknbr/pppppppp/8/8/4P3/8/PPPP1PPP/NBRQKNBR b BHbh - 0 1", true },
			{ "2rr2k1/pb4pp/1p1qpn2/5p2/2PPn3/P2B1N1P/2Q2PP1/1RBR2K1 w - - 3 19", false },
			{ "r4rk1/p3bp1p/bqp1p1p1/3nP2P/1p6/5N2/PPBB1PP1/1K1RQ2R b - - 0 22", false },
			{ "8/pr1n1p2/2R1p3/1Pp3kp/2P5/1P1N1KP1/5P2/8 w - - 0 40", false },
			{ "8/7R/p1P2p2/P7/1p3k2/5r1p/2K5/8 b - - 7 65", false },
			{ "8/5ppk/7p/N1p5/PPP1p3/7R/rr6/4K3 w - - 5 64", false },
			{ "8/8/8/4K3/8/1pk5/8/8 b - - 1 78", false }
		];
		const Option option = { {double.max}, {ulong.max}, {depth}, {0}, false, quiet };
		ulong nNodes;
		double totalTime = 0.0;
		Moves moves;

		foreach (fen; fens) {
			board.set(fen.fen);
			board.chess960 = fen.chess960;
			search.set();
			search.go(option, moves);
			if (!quiet) bestmove();
			nNodes += search.pvsNodes + search.qsNodes;
			totalTime += search.timer.time;
		}

		event.send("bench: ", nNodes, " nodes in ", totalTime, " s, ", cast (ulong) (nNodes / totalTime), " nps.");
	}

	/**
	 *  Do a (slow) perft from the current position to test the move generator.
	 *
	 *  params: depth: remaining depth to test at.
	 *  returns: the node count to reach the given depth.
	 */
	ulong perft(bool div = false)(const int depth) {
		ulong count, total;
		Move move;
		Chrono chrono = void;

		static if (div) chrono.start();

		Moves moves = Moves(board);

		while ((move = moves.next.move) != 0) {
			if (board.update(move)) {
				if (depth == 1) count = 1; else count = perft(depth - 1);
				total += count;
				static if (div) event.send(format("%5s %16d", move.toPan(board), count));
			}
			board.restore(move);
		}

		static if (div) event.send(format("perft %d: %d leaves const %.3fs (%.0f leaves/s)", depth, total, chrono.time(), total / chrono.time()));

		return total;
	}

	/**
	 *  Receive the $I go command : start a new search.
	 *
	 * params: line = input search parameters.
	 */
	void go(string line) {
		Option option = { {double.max}, {ulong.max}, {Limits.Ply.max}, {0}, false, false };
		Moves moves;
		isInfinite = isPondering = false;
		string [] words = line.split();

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
			else if (w == "mate" && i + 1 < words.length) option.mate.max = to!int(words[i + 1]);
			else if (w == "movetime" && i + 1 < words.length) time[board.player].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "infinite") { isInfinite = true; option.depth.max =  Limits.Ply.max; }
		}
		option.time.max = setTime();
		option.isPondering = isPondering;

		event.log!' '("[options: ", option, "]");
		event.log!' '("\n", board);

		search.go(option, moves);
		if (!isInfinite && !isPondering) bestmove();
	}

	/**
	 * Display the board or the evaluation function for debugging.
	 *
	 * params: line: input parameters to display the board or the eval
	 */
	void show(string line) {
		if (findSkip(line, "board")) event.send("Board:\n", board);
		else if (findSkip(line, "eval")) event.send("Eval:\n", search.eval.toString(board));
		else event.send("Board:\n", board, "\n\nEval:\n", search.eval.toString(board));
	}

	/**
	 * Main loop waiting for commands and processing them.
	 * The event handler is started in its own thread as a daemon,
	 * then an infinite loop wait for new events and react when a
	 * new command is received.
	 * Most of if not all standard UCI commands are recognized. Dumb also
	 * understands three more commands:
	 *  - $I perft to run a perft from the current position.
	 *  - $I bench to perform a small speed test on a few various positions.
	 *  - $I show to display the evaluation function and/or the chessboard.
	 */
	void loop() {
		event.daemon();
		while (true) {
			auto line = event.wait();
			if (line == "" || line[0] == '#') continue;
			else if (findSkip(line, "ucinewgame")) ucinewgame();
			else if (findSkip(line, "uci")) uci();
			else if (findSkip(line, "isready")) writeln("readyok");
			else if (findSkip(line, "setoption")) setoption(line);
			else if (findSkip(line, "position")) position(line);
			else if (findSkip(line, "go")) go(line);
			else if (findSkip(line, "stop") && (isInfinite || isPondering)) bestmove();
			else if (findSkip(line, "ponderhit") && isPondering) bestmove();
			else if (findSkip(line, "quit")) break;
			else if (findSkip(line, "debug")) event.logOn();
			else if (findSkip(line, "register")) {}
			// extension
			else if (findSkip(line, "perft")) perft!true(to!int(line.strip));
			else if (findSkip(line, "bench")) bench(to!int(line.strip));
			else if (findSkip(line, "show")) show(line.strip);

			else event.send("string info Error unknown command: '", line, "'");
		}
		event.logOff();
	}
}

/**
 * Main function
 *
 * This the main function that start the UCI interface and process the command line.
 * The following options are accepted:
 *  - $I--quiet or /-q/: To be used with the bench command, to make the search silent.
 *  - $I--bench or /-b/: Run a small speed test by searching a small set of unrelated positions at
 * the provided depth.
 *  - $I--perft or /-p/: Run a (slow) perft test from the starting position, for debugging purpose.
 *  - $I--debug or /-g/: Start in debug mode, which logs to a debug file.
 *  - $I--help or /-p/: Show a small usgae help.
 * With no option, will start the Uci main loop, waiting for instructions from a chess GUI.
 *
 * params: args: the command line arguments
 */
void main(string [] args) @trusted {
	version (Windows) stdout.setvbuf(0, _IONBF); else stdout.setvbuf(4096, _IOLBF);
	int do_bench = 0, do_perft = 0;
	Uci uci = new Uci;
	foreach (i; 1 .. args.length) {
		if (args[i] == "--quiet" || args[i] == "-q") uci.quiet = true;
		else if (i + 1 < args.length && (args[i] == "--bench" || args[i] == "-b")) do_bench = to!int(args[++i]);
		else if (i + 1 < args.length && (args[i] == "--perft" || args[i] == "-p")) do_perft = to!int(args[++i]);
		else if (args[i] == "--debug" || args[i] == "-g") uci.event.logOn();
		else if (args[i] == "--help" || args[i] == "-h") stderr.writeln("Usage: ", args[0], " [--bench|-b <depth>] | [--perft|-p <depth>] | [--debug|-g] [--help|-h] | [--quiet|-q]");
	}
	if (do_bench > 0) (uci.bench(do_bench));
	else if (do_perft > 0) (uci.perft!true(do_perft));
	else uci.loop();
}
