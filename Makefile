LEX = alex
#LFLAGS = --ghc
#HFLAGS = -funbox-strict-fields -fglasgow-exts
HFLAGS = -funbox-strict-fields
YACC = happy
YFLAGS = -i -a -g -c

SRC = Lexer Main Parser
GEN = $(addsuffix .hs, Lexer Parser)


all: main

main: $(GEN) Main.hs MonadP.hs
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
