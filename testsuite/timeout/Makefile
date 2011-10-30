TOP = ..

# If we're cleaning then we don't want to do all the GHC detection hardwork,
# and we certainly don't want to fail if GHC etc can't be found!
# However, we can't just put this conditional in boilerplate.mk, as
# some of the tests have a "clean" makefile target that relies on GHC_PKG
# being defined.
ifneq "$(MAKECMDGOALS)" "clean"
ifneq "$(MAKECMDGOALS)" "distclean"

include $(TOP)/mk/boilerplate.mk

TIMEOUT_PROGRAM = install-inplace/bin/timeout$(exeext)

PREFIX := $(abspath install-inplace)
$(eval $(call canonicalise,PREFIX))

ifneq "$(GCC)" ""
WITH_GCC = --with-gcc='$(GCC)'
endif

# Use a python timeout program, so that we don't have to worry about
# whether or not the compiler we're testing has built the timeout
# program correctly
$(TIMEOUT_PROGRAM): timeout.py
	rm -rf install-inplace
	mkdir install-inplace
	mkdir install-inplace/bin
	cp $< $@.py
	echo '#!/bin/sh' > $@
	echo 'exec "${PYTHON}" $$0.py "$$@"' >> $@
	chmod +x $@

boot all :: $(TIMEOUT_PROGRAM)

endif
endif

clean distclean maintainer-clean:
	$(RM) -rf install-inplace

