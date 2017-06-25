/*
 * File dumb.d
 * Universal Chess Interface.
 * © 2017 Richard Delorme
 */

module dumb;

import board, eval, move, search, util;
import std.algorithm, std.array, std.conv, std.stdio, std.string, std.concurrency;

void eventLoop(shared Event e) {
	e.loop();
}

/* Universal Chess Interface. */
class Uci {
	struct Time {
		double remaining, increment;
		void clear() { remaining = increment = 0.0;	}
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

	void send(T...) (T args) const {
		writeln(args);
		stdout.flush();
	}

	double setTime() const {
		const p = board.player;
		double t = time[p].remaining;
		int todo = 40;

		if (t > 0) {
			if (movesToGo > 0) todo = movesToGo;
			t += time[p].increment * todo;
			t = max(t - 1.0, 0.95 * t) / todo;
		} else {
			t = time[p].increment > 0 ? max(t - 1.0, 0.95 * time[p].increment) : double.infinity;
		}

		return t;
	}

	void uci() const {
		send("id name dumb 1.0");
		send("id author Richard Delorme");
		send("option name Ponder type check default false");
		send("option name Hash type spin default 64 min 1 max 65536");
		send("uciok");
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
		if (search.hint != 0 && canPonder) send("bestmove ", search.bestMove.toPan(), " ponder ", search.hint.toPan());
		else send("bestmove ", search.bestMove.toPan());
	}

	void go(string line) {
		Option option;
		string [] words = line.split();

		moves.clear();
		option.depth.max = Limits.ply.max;
		option.nodes.max = ulong.max;
		foreach(c ; Color.white .. Color.size) time[c].clear();
		isPondering = false;
		foreach(i, ref w ; words) {
			if (w == "searchmoves") foreach(m ; words) moves.push(fromPan(m));
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
			else if (w == "infinite") option.depth.max =  Limits.ply.max;
		}
		option.time.max = setTime();
		option.isPondering = isPondering;

		search.go(option, moves);
		if (!isPondering) bestmove();
	}

	void loop() {
		spawn(&eventLoop, event);
		while (true) {
			auto line = event.wait();
			if (line == "" || line[0] == '#') continue;
			else if (findSkip(line, "ucinewgame")) ucinewgame();
			else if (findSkip(line, "uci")) uci();
			else if (findSkip(line, "isready")) send("readyok");
			else if (findSkip(line, "setoption")) setoption(line);
			else if (findSkip(line, "position")) position(line);
			else if (findSkip(line, "go")) go(line);
			else if ((findSkip(line, "stop") || findSkip(line, "ponderhit")) && isPondering) bestmove();
			else if (findSkip(line, "quit")) break;
		}
	}
}

/* main function */
void main() {
	Uci uci = new Uci;
	uci.loop();
}

