# -----------------------------------------------------------------------------
#
# (c) 2011 Tsitsimpis Ilias
#
# This file is part of the GAC build system.
#
# To understand how the build system works and how to modify it, see HACKING
#
# -----------------------------------------------------------------------------

# if you use a haskell compiler other than ghc change the above
# (eg for Hugs change it to runhugs)
RUNHS = runhaskell
BUILDDIR = ./dist
PREFIX = $(HOME)/.cabal
LFLAGS = --ghc
YFLAGS = -i -a -g #-c

.PHONY: all dist clean install test config build

all: build

build: config $(BUILDDIR)/build/gac/gac

$(BUILDDIR)/build/gac/gac:
	$(RUNHS) Setup build --builddir=$(BUILDDIR)

config: $(BUILDDIR)/setup-config

$(BUILDDIR)/setup-config:
	$(RUNHS) Setup configure --builddir=$(BUILDDIR) --prefix=$(PREFIX) \
		--alex-options="$(LFLAGS)" --happy-options="$(YFLAGS)" --user

dist: config
	$(RUNHS) Setup sdist --builddir=$(BUILDDIR)

clean:
	$(RUNHS) Setup clean --builddir=$(BUILDDIR)

install: build
	$(RUNHS) Setup install --builddir=$(BUILDDIR)

test:
	$(RUNHS) Setup test --builddir=$(BUILDDIR)
