# Projekt: 	flp-log
# Login: 	xjahnf00
# Autor: 	Filip Jahn

BIN = flp23-log
PROLOG = swipl
GOAL = main

.PHONY: clean

$(BIN): flp23-log.pl
	$(PROLOG) -o $@ -g $(GOAL) -c $^

clean:
	@rm -f $(BIN)