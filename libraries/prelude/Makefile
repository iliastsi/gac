# -----------------------------------------------------------------------------
#
# (c) 2011-2012 Tsitsimpis Ilias
#
# This file is part of the GAC build system.
#
# To understand how the build system works and how to modify it, see HACKING
#
# -----------------------------------------------------------------------------

libdir =

ghc =
gcc =
ar  =
llc =
opt =
INSTALL =

.PHONY: all
all: libprelude.a

.PHONY: install
install: libprelude.a
	$(INSTALL) -D $< $(libdir)

##############################
libprelude.a: prelude.o
	$(ar) rcs $@ $<

prelude.o: prelude.s
	$(gcc) -c $< -o $@

prelude.s: prelude.bc
	$(llc) -O3 $< -o $@

prelude.bc: Main.hs
	$(ghc) -Wall $< -o Prelude
	./Prelude
	$(opt) -O3 $@ -o $@

##############################
.PHONY: clean
clean:
	$(RM) *.o *.hi *.a *.s *.bc Prelude
