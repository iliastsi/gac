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

gac =
ar  =
INSTALL =

.PHONY: all
all: libmath.a

.PHONY: install
install: libmath.a
	$(INSTALL) -D $< $(libdir)

##############################
libmath.a: abs.o
	$(ar) rcs $@ $^

abs.o: abs.alan
	$(gac) -XExplicitMain -c $<

##############################
.PHONY: clean
clean:
	$(RM) *.o *.a
