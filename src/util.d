/**
 * File util.d
 *
 * Miscellaneous utilities
 *
 * Authors: Richard Delorme
 * Date: 2025
 */

module util;

import std.stdio, std.conv, std.array, std.string, std.datetime, std.format, std.process;
import core.bitop, core.time, core.thread;

@safe:

/**
 * Check if a number has a single bit set, ie is a power of 2.
 *
 * params: b = the number to check.
 * returns: true if the number has a single bit set.
 */
bool hasSingleBit(const ulong b) pure nothrow { return (b & (b - 1)) == 0; }

/**
 * Remove a bit from a number and return its index.
 * 
 * params: b = the number to modify.
 * returns: the index of the removed bit
 */
int popBit(ref ulong b) pure nothrow {
	const int i = bsf(b);
	b &= b - 1;
	return i;
}

/**
 * pext: parallel bit extract
 * Extract bits from a number according to a mask.
 *
 * params: b = the number to extract bits from.
 *       : mask = the mask to apply.
 * returns: the extracted bits.
 */
version (USE_BMI) {
	version(X86_64) {
		version(LDC) import ldc.gccbuiltins_x86;
		else version (GNU) import gcc.builtins;
		else static assert(false, "pext unsupported");
		ulong pext(ulong b, ulong mask) pure nothrow { return __builtin_ia32_pext_di(b, mask); }
	} else static assert(false, "pext unsupported");
}

/**
 * struct Chrono
 * A simple chronometer.
 */
struct Chrono {
	MonoTime startPoint; /// The time when the chronometer was started.

	/**
	 * start the chronometer.
	 */
	void start() nothrow { startPoint = MonoTime.currTime; }

	/**
	 * Get the time elapsed in seconds.
	 * returns: the time elapsed in seconds
	 */
	double time() nothrow const { return 1e-7 * (MonoTime.currTime - startPoint).total!"hnsecs"; }
}


/**
 * Get the current time as a string.
 * returns: the current time as a string
 */
string timeStamp() {
	SysTime t = Clock.currTime();
	return format("%d.%d.%d %0d:%0d:%0d.%03d", t.year, t.month, t.day, t.hour, t.minute, t.second, t.fracSecs.total!"msecs");
}

/**
 * class Event
 * A simple event management class, where event is a string read from stdin.
 */
final class Event {
	string [] ring;   /// A Ring
	size_t first;     /// First event index on the ring
	size_t last;      /// Last event index on the ring
	File logFile;     /// LogFile

	/**
	 * Constructor: create a ring of length 4.
	 */
	this() pure nothrow {
		ring.length = 4;
	}

	/**
	 * Launch the loop method inside a new thread.
	 */
	void daemon() @trusted {
		auto t = new Thread( (){ loop(); } );
		t.isDaemon = true;
		t.start();
	}

	/**
	 * Return true if there is no waiting event, ie the ring is empty.
	 * returns: true if there is no waiting event
	 */
	bool empty() pure nothrow const {
		return first == last;
	}

	/**
	 * Return true if the event storage is full.
	 * returns: true if the event storage is full
	 */
	bool full() pure nothrow const {
		return first == (last + 1)  % ring.length;
	}

	/**
	 * Return true if there is the first waiting event matches the input string.
	 * params: s = the string to check
	 * returns: true if the string s is the next event
	 */
	bool has(string s) pure nothrow const {
		return !empty && ring[first] == s;
	}

	/**
	 * Queue a new event into the event storage (the ring). In case the ring is
	 * full, double its size first.
	 * params s = the string to add into the event storage
	 */
	void push(string s) pure {
		synchronized (this) {
			if (full) {
				const oldLength = ring.length;
	
				ring.length = 2 * oldLength;
				foreach (i ; 0 .. first) ring[i + oldLength] = ring[i];
				last = first + oldLength - 1;
			}
			ring[last] = s;
			last = (last + 1) % ring.length;
		}
	}

	/**
	 * Remove the next event and return it.
	 *
	 * returns: the next event.
	 */
	string peek() {
		synchronized (this) {
			string s;
			if (!empty) {
				s = ring[first];
				first = (first  + 1) % ring.length;
			}
			if (s !is null) log!'<'(s);
			return s;
		}
	}

	/**
	 * Wait until an event is available.
	 * returns: the next event
	 */
	string wait() @trusted {
		while (empty) Thread.sleep(1.msecs); // Thread.sleep is unsafe ?
		return peek();
	}

	/**
	 * Infinitely read events from stdin until "quit" is received.
	 */
	void loop() @trusted {
		string line;
		do {
			line = readln().chomp(); // readln is unsafe ?
			if (line is null) line = "quit";
			push(line);
		} while (line != "quit");
	}

	/**
	 * send a message (to the user interface).
	 * params: args = the message to send
	 */
	void send(T...) (T args) {
		writeln(args);
		log!'>'(args);
	}


	/**
	 * Write to the logfile with a date stamp.
	 *
	 * params: tag = the tag to use
	 *         args = the message to log
	 *
	 */
	void log(const char tag = '#', T...) (T args) {
		if (logFile.isOpen) {
			logFile.writef("[%s] %s%c ", timeStamp(), "Dumb", tag);
			logFile.writeln(args);
			logFile.flush();
		}
	}

	/**
	 * Turn on logging for debugging purpose.
	 */
	void logOn() {
		if (!logFile.isOpen) logFile.open("Dumb-" ~ to!string(thisProcessID) ~ ".log", "w");
	}

	/**
	 * Turn off logging for debugging purpose.
	 */
	void logOff() {
		if (logFile.isOpen) logFile.close();
	}
}