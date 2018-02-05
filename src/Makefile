#
# Makefile to compile dumb
#

SRC = dumb.d util.d board.d eval.d move.d search.d

DC = ldc2

#lcd2
FFLAGS = -O3 -release -singleobj -w -dw -mcpu=sandybridge -mattr=+sse4.2,popcnt,avx  -flto=full
DFLAGS = -O0 -g -gc
CFLAGS = -O1 -singleobj -w -dw -mcpu=sandybridge -mattr=+sse4.2,popcnt,avx -cov -flto=full
PGO_GEN = -fprofile-instr-generate
PGO_USE = -fprofile-instr-use=dumb.profdata
PGO_MERGE = ldc-profdata merge -output=dumb.profdata dumb-*.profraw
PGO_FILE = LLVM_PROFILE_FILE=dumb-%p.profraw

# executable target
EXE = dumb

pgo:$(SRC)
	$(MAKE) clean
	$(DC) $(FFLAGS) $(PGO_GEN) $(SRC)
	$(PGO_FILE) ./$(EXE) -b 8
	$(PGO_MERGE)
	$(DC) $(FFLAGS) $(PGO_USE) $(SRC)

debug:$(SRC)
	$(DC) $(DFLAGS) $(SRC)

cov:$(SRC)
	$(DC) $(CFLAGS) $(SRC)

clean:
	$(RM) *.o *.profraw *.profdata *.lst

