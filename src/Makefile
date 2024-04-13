#
# Makefile to compile Dumb
#

SRC = dumb.d util.d board.d eval.d move.d search.d

ifeq ($(TARGET),)
	TARGET = native
endif

ifeq ($(DC),)
	DC = ldc2
endif

#gdc
ifeq ($(DC),gdc)
	COMMON = -W -Wno-uninitialized -Wall -Wextra -march=$(TARGET) -o dumb
	SFLAGS = -O3 -frelease -fno-bounds-check $(COMMON)
	FFLAGS = $(SFLAGS) -flto
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
	SFLAGS = -O3 -release -boundscheck=off -singleobj -w -dw -mcpu=$(TARGET)
	FFLAGS = $(SFLAGS) -flto=full -defaultlib=phobos2-ldc-lto,druntime-ldc-lto
	DFLAGS = -O0 -g -gc
	CFLAGS = $(DFLAGS) -cov
	PFLAGS = -O1 -release -boundscheck=off -flto=full -singleobj -mcpu=$(TARGET)
	PGO_GEN = -fprofile-generate
	PGO_USE = -fprofile-use=dumb.profdata
	PGO_MERGE = ldc-profdata merge -output=dumb.profdata dumb-*.profraw
	PGO_FILE = LLVM_PROFILE_FILE=dumb-%p.profraw
endif

#dmd
ifeq ($(DC),dmd)
	SFLAGS = -O -release -inline -boundscheck=off -w -mcpu=native
	FFLAGS = $SFLAGS
	DFLAGS = -w -g -gc -debug -mcpu=native
	CFLAGS = -O -release -boundscheck=off -cov -w -mcpu=native
	PFLAGS = -O -release -boundscheck=off -w -mcpu=native
	PGO_GEN = 
	PGO_USE = 
	PGO_MERGE = 
endif

# executable target
EXE = dumb

# portable compilation (no flto no pgo)
dumb:$(SRC)
	$(DC) $(SFLAGS) $(SRC)

#help 
help:
	@echo "Usage: make <build> <options>"
	@echo "build:"
	@echo "    make [dumb]: build dumb with portable optimization settings"
	@echo "    make pgo: build dumb with full optimization settings"
	@echo "    make debug: build dumb for debugging purpose"
	@echo "    make perf: build dumb for profiling"
	@echo "    make cov: build dumb to measure code coverage"
	@echo "    make clean: clean garbage files produced during the compilation"
	@echo "    make help: display this message"
	@echo "options:"
	@echo "    DC=<gdc|ldc2|dmd> use a specific compiler (default = ldc2)"
	@echo "    TARGET=<cpu name> target a specific compiler (default = native)"
	
# fully optimized compilation
pgo:$(SRC)
	$(MAKE) clean
	$(DC) $(FFLAGS) $(PGO_GEN) $(SRC)
	$(PGO_FILE) ./$(EXE) -b 18
	$(PGO_MERGE)
	$(DC) $(FFLAGS) $(PGO_USE) $(SRC)

#to debug the engine
debug:$(SRC)
	$(DC) $(DFLAGS) $(SRC)

#perf (for profiling) 
perf:$(SRC)
	$(DC) $(PFLAGS) $(SRC)

#to measure code coverage
cov:$(SRC)
	$(DC) $(CFLAGS) $(SRC)

#to remove garbage
clean:
	$(RM) *.o *.profraw *.profdata *.lst *.gcda


