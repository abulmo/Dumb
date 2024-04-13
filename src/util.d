/*
 * File util.d
 * Miscellaneous utilities
 *
 * © 2017-2024 Richard Delorme
 */

module util;
import std.stdio, std.array, std.string, std.datetime, std.format;
import core.bitop, core.time, core.thread, core.simd;

/*
 * bit utilities
 */
/* Check if a number has a single bit set, ie is a power of 2 */
bool hasSingleBit(const ulong b) { return (b & (b - 1)) == 0; }

/* Remove a bit from a number and return its index */
int popBit(ref ulong b) {
	const int i = bsf(b);
	b &= b - 1;
	return i;
}

/*
 * struct Chrono
 * A simple chronometer
 */
struct Chrono {
	MonoTime clock;

	/* start the chronometer */
	void start() { clock = MonoTime.currTime; }

	/* return the time elapsed in seconds */
	double time() const { return 1e-7 * (MonoTime.currTime - clock).total!"hnsecs"; }
}

/*
 * class Event
 * A simple event management class.
 * An event is a string read from stdin.
 */
final shared class Event {
	string [] ring;
	size_t first, last;
	class Lock {}
	Lock lock;

	/* constructor */
	this () {
		ring.length = 4;
		lock = new shared Lock;
	}

	/* Return true if there is no waiting event */
	bool empty() const { return first == last; }

	/* Return true if the event storage is full */
	bool full() const { return first == (last + 1)  % ring.length; }

	/* Return true if there is at least one waiting event matching the input string */
	bool has(string s) const { return !empty && ring[first] == s;	}

	/* Queue a new event into the storage */
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

	/* Remove the oldest ushed event and return it */
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

	/* Wait until an event is available */
	string wait() {
		while (empty) Thread.sleep(1.msecs);
		return peek();
	}

	/* Infinitely read events from stdin until "quit" is received */
	void loop() {
		string line;
		do {
			line = readln().chomp();
			if (line is null) line = "quit";
			push(line);
		} while (line != "quit");
	}
}

