#
# Makefile to compile dumb
#

SRC = dumb.d util.d board.d eval.d move.d search.d

ifeq ($(TARGET),)
	TARGET = native
endif

ifeq ($(DC),)
	DC = gdc
endif

#gdc
ifeq ($(DC),gdc)
	COMMON = -W -Wno-uninitialized -Wall -Wextra -march=$(TARGET) -o dumb
	FFLAGS = -O3 -frelease -fno-bounds-check -flto $(COMMON)
	DFLAGS = -O0 -g -fdebug $(COMMON)
	CFLAGS = $(DFLAGS) -gcov $(COMMON)
	PFLAGS = -O1 -frelease  fno-bounds-check -march=$(TARGET $(COMMON))
	PGO_GEN = -fprofile-generate -lgcov
	PGO_USE = -fprofile-use
	PGO_MERGE = 
	PGO_FILE =
endif

#ldc2
ifeq ($(DC),ldc2)
	FFLAGS = -O3 -release -boundscheck=off -singleobj -w -dw  -flto=full -mcpu=$(TARGET) -defaultlib=phobos2-ldc-lto,druntime-ldc-lto
	DFLAGS = -O0 -g -gc
	CFLAGS = $(DFLAGS) -cov
	PFLAGS = -O1 -release -singleobj -mcpu=$(TARGET)
	PGO_GEN = -fprofile-generate
	PGO_USE = -fprofile-use=dumb.profdata
	PGO_MERGE = ldc-profdata merge -output=dumb.profdata dumb-*.profraw
	PGO_FILE = LLVM_PROFILE_FILE=dumb-%p.profraw
endif

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

perf:$(SRC)
	$(DC) $(PFLAGS) $(SRC)

cov:$(SRC)
	$(DC) $(CFLAGS) $(SRC)

clean:
	$(RM) *.o *.profraw *.profdata *.lst

