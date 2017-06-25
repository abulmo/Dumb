#
# Makefile to compile dumb
#

SRC = dumb.d util.d board.d eval.d move.d search.d

ifeq ($(BUILD),)
	BUILD = fast
endif

DC = ldc2

#lcd2
ifeq ($(BUILD),fast)
	DFLAGS = -O3 -release -singleobj -w -dw -mcpu=sandybridge -mattr=+sse4.2,popcnt,avx
else ifeq ($(BUILD),nopopcount)
	DFLAGS = -O3 -release -singleobj -w -dw
else ifeq ($(BUILD),debug)
	DFLAGS = -O0 -g -gc
else ifeq ($(BUILD),cov)
	DFLAGS = -O1 -singleobj -w -dw -mcpu=sandybridge -mattr=+sse4.2,popcnt,avx -cov
endif
PGO_GEN = -fprofile-instr-generate
PGO_USE = -fprofile-instr-use=dumb.profdata
PGO_MERGE = ldc-profdata merge -output=dumb.profdata dumb-*.profraw
PGO_FILE = LLVM_PROFILE_FILE=dumb-%p.profraw

# executable target
ifeq ($(EXE),)
	EXE = dumb
endif

build :
	$(DC) $(DFLAGS) $(SRC)

build-pgo :
	$(MAKE) clean
	$(DC) $(DFLAGS) $(PGO_GEN) $(SRC)
	$(PGO_FILE) ./$(EXE) < bench.uci
	$(PGO_MERGE)
	$(DC) $(DFLAGS) $(PGO_USE) $(SRC)

debug : $(SRC)
	$(MAKE) BUILD=debug EXE=dumb	

pgo: $(SRC)
	$(MAKE) build-pgo EXE=dumb	

cov:$(SRC)
	$(MAKE) BUILD=cov EXE=dumb	

clean:
	$(RM) *.o trace.* *.log *.gcda *.profraw *.profdata *.lst

