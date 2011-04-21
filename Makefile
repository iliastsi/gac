LEX = alex
LFLAGS = --ghc
HFLAGS = -funbox-strict-fields -XBangPatterns -fglasgow-exts
HFLAGS += -Wall -fno-warn-name-shadowing -fno-warn-unused-matches
YACC = happy
YFLAGS = -i -a -g
#YFLAGS += -c

SRC = Lexer Main Parser
GEN = $(addsuffix .hs, Lexer Parser)
OUT = gac


all: $(OUT)

$(OUT): $(GEN) Main.hs
	ghc $(HFLAGS) --make Main.hs -o $@

%.hs: %.x
	$(LEX) $(LFLAGS) $<

%.hs: %.y
	$(YACC) $(YFLAGS) $<

clean:
	$(RM) $(foreach sfx, .hi .o, $(addsuffix $(sfx), $(SRC)))

distclean: clean
	$(RM) $(GEN) Parser.info $(OUT)

.PHONY: all clean distclean
