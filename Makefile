LEX = alex
#LFLAGS = --ghc
HFLAGS = -funbox-strict-fields
YACC = happy
YFLAGS = -i

SRC = Lexer Main Parser
GEN = $(addsuffix .hs, Lexer Parser)


all: main

main: $(GEN) Main.hs
	ghc $(HFLAGS) --make Main.hs -o $@

%.hs: %.x
	$(LEX) $(LFLAGS) $<

%.hs: %.y
	$(YACC) $(YFLAGS) $<

clean:
	$(RM) $(foreach sfx, .hi .o, $(addsuffix $(sfx), $(SRC)))

distclean: clean
	$(RM) $(GEN) Parser.info main

.PHONY: all clean distclean
