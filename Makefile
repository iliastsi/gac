LEX = alex
#LFLAGS = --ghc
HFLAGS = -funbox-strict-fields

SRC = Lexer Main
GEN = $(addsuffix .hs, Lexer)


all: main

main: $(GEN) Main.hs
	ghc $(HFLAGS) --make Main.hs -o $@

%.hs: %.x
	$(LEX) $(LFLAGS) $<

clean:
	$(RM) $(foreach sfx, .hi .o, $(addsuffix $(sfx), $(SRC)))

distclean: clean
	$(RM) $(GEN) main

.PHONY: all clean distclean
