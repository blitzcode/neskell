
WARN_FLAGS = -Wall -fno-warn-unused-imports
# LLVM_FLAGS = -fllvm -optlo-O3
# Run with '+RTS -p -s'
# PROF_FLAGS = -prof -fprof-auto
GHC_FLAGS = $(PROF_FLAGS) $(WARN_FLAGS) $(LLVM_FLAGS) -rtsopts -O2 -H512m

# Rule for producing the main program
.PHONY : out
out:
	ghc $(GHC_FLAGS) -o $@ --make Main
	time ./out --test +RTS -s

HSSRC = $(wildcard *.hs)
.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) trace.log out

