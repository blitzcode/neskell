
.PHONY : out
out:
	ghc -rtsopts -O2 -o $@ -Wall -i../../ --make Main

# -ddump-splices

HSSRC = $(wildcard *.hs)
.PHONY : clean
clean:
	$(RM) $(HSSRC:.hs=.hi) $(HSSRC:.hs=.o) out

