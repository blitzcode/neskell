
.PHONY : out
out:
	ghc -rtsopts -O2 -o $@ --make Main

HSSRC = $(wildcard *.hs)
.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) out

