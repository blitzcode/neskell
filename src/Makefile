
WARN_FLAGS = -Wall -fno-warn-unused-imports
# LLVM_FLAGS = -fllvm -optlo-O3
# Run with '+RTS -p -s'
# PROF_FLAGS = -prof -fprof-auto
GHC_FLAGS = $(PROF_FLAGS) $(WARN_FLAGS) $(LLVM_FLAGS) -rtsopts -O2 -H512m -threaded

# By default, we build & test
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

# Run the internal test suite, measure runtime and memory performance
.PHONY : test
test: build
	time ./out --quick-test +RTS -s -N

HSSRC = $(wildcard *.hs)
.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) trace/*.log out

