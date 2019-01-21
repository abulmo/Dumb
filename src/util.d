/*
 * File util.d
 * Fast implementation on X86_64, portable algorithm for other platform
 * of some bit functions.
 * © 2017-2019 Richard Delorme
 */

module util;
import std.stdio, std.array, std.string, std.datetime, std.format;
import core.bitop, core.time, core.thread, core.simd;

version (LDC) import ldc.intrinsics;
else version (GNU) import gcc.builtins;

/* bit utilities */
version (LDC) alias swapBytes = llvm_bswap;
else version(GNU) alias swapBytes = __builtin_bswap64;
else alias swapBytes = bswap;

bool hasSingleBit(const ulong b) { return (b & (b - 1)) == 0; }

version (LDC) int firstBit(ulong b) {return cast (int) llvm_cttz(b, true);}
else alias firstBit = bsf;

int popBit(ref ulong b) {
	const int i = firstBit(b);
	b &= b - 1;
	return i;
}

version (GNU) alias countBits = __builtin_popcountll;
else version (LDC) int countBits(const ulong b) {return cast (int) llvm_ctpop(b);}
else alias countBits = _popcnt;

/* struct Chrono */
struct Chrono {
	TickDuration tick;

	void start() { tick = TickDuration.currSystemTick(); }

	double time() const { return 1e-7 * (TickDuration.currSystemTick() - tick).hnsecs; }
}

/* class Event */
shared class Event {
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

	bool has(string s) { return !empty && ring[first] == s;	}

	void push(string s) {
		synchronized (lock) {
			if (full) {
				const l = ring.length, δ = l - 1;
				ring.length = 2 * l;
				foreach (i ; 0 .. first) ring[i + δ] = ring[i];
				last = first + δ;
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
			push(line);
		} while (line != "quit" && stdin.isOpen);
	}
}

/* Miscellaneous utilities */
string findBetween(string s, string start, string end) {
	size_t i, j;

	for (; i < s.length; ++i) if (s[i .. i + start.length] == start) break;
	i += start.length; if (i > s.length) i = s.length;
	for (j = i; j < s.length; ++j) if (s[j .. j + end.length] == end) break;

	return s[i .. j];
}

