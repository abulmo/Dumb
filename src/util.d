/*
 * File util.d
 * Fast implementation on X86_64, portable algorithm for other platform
 * of some bit functions.
 * © 2017-2020 Richard Delorme
 */

module util;
import std.stdio, std.array, std.string, std.datetime, std.format;
import core.bitop, core.time, core.thread, core.simd;

/* bit utilities */
bool hasSingleBit(const ulong b) { return (b & (b - 1)) == 0; }

int popBit(ref ulong b) {
	const int i = bsf(b);
	b &= b - 1;
	return i;
}

/* struct Chrono */
struct Chrono {
	TickDuration tick;

	void start() { tick = TickDuration.currSystemTick(); }

	double time() const { return 1e-7 * (TickDuration.currSystemTick() - tick).hnsecs; }
}

/* class Event */
final shared class Event {
	string [] ring;
	size_t first, last;
	class Lock {}
	Lock lock;

	this () {
		ring.length = 4;
		lock = new shared Lock;
	}

	bool empty() const @property { return first == last; }

	bool full() const @property { return first == (last + 1)  % ring.length; }

	bool has(string s) const { return !empty && ring[first] == s;	}

	void push(string s) {
		synchronized (lock) {
			if (full) {
				const l = ring.length, δ = l;
				ring.length = 2 * l;
				foreach (i ; 0 .. first) ring[i + δ] = ring[i];
				last = first + δ - 1;
			}
			ring[last] = s;
			last = (last + 1) % ring.length;
		}
	}

	string peek() {
		synchronized (lock) {
			string s;
			if (!empty) {
				s = ring[first];
				ring[first] = null;
				first = (first  + 1) % ring.length;
			}
			return s;
		}
	}

	string wait() {
		while (empty) Thread.sleep(1.msecs);
		return peek();
	}

	void loop() {
		string line;
		do {
			line = readln().chomp();
			if (line is null) line = "quit";
			push(line);
		} while (line != "quit");
	}
}

/* Miscellaneous utilities */
string findBetween(string s, string start, string end) {
	size_t i, j;

	for (; i < s.length - start.length; ++i) if (s[i .. i + start.length] == start) break;
	i += start.length;
	for (j = i; j < s.length - end.length; ++j) if (s[j .. j + end.length] == end) return s[i .. j];

	return s[i .. $];
}

