
WARN_FLAGS = -Wall
# LLVM_FLAGS = -fllvm -optlo-O3
# Run with '+RTS -p -s'
# PROF_FLAGS = -prof -fprof-auto
GHC_FLAGS = $(PROF_FLAGS) $(WARN_FLAGS) $(LLVM_FLAGS) -rtsopts -O2 -H512m -threaded

# By default, we build and test
.PHONY : all
all: build test

# Syntax only, no binary output
.PHONY : check
check:
	ghc $(GHC_FLAGS) -fno-code --make Main

# Build the full program
.PHONY : build
build:
	ghc $(GHC_FLAGS) -o out --make Main

# Run the quick version of the internal test suite
.PHONY : test
test: build
	time ./out --quick-test --no-verbose +RTS -N -s

HSSRC = $(wildcard *.hs)
.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) trace/*.log out

