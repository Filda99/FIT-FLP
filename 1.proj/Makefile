# Project Name: flp-fun
# Author:		Filip Jahn
# Date:			03.03.2024
# Usage: 		

GHC = ghc
FLAGS = -Wall -O --make
EXEC = flp-fun
SRCS = *.hs

$(EXEC): $(SRCS)
	$(GHC) $(FLAGS) -o $@ $^

clean: 
	rm -rf *.o *.hi flp-fun