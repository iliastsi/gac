# -----------------------------------------------------------------------------
# Examples of use:
#
# 	make		-- run all the tests in the current directory
# 	make verbose	-- as make test, but up the verbosity
# 	make accept	-- run the tests, accepting the current output
#
# The following variables may be set on the make command line:
#
#	TEST		-- specific test to run
#	TESTS		-- specific tests to run (same as $TEST really)
#	EXTRA_AC_OPTS	-- extra flags to send to the Alan compiler
#	EXTRA_RUNTEST_OPTS -- extra flags to give the test driver
#	CONFIG		-- use a different configuration file
#	COMPILER	-- select a configuration file from config/
#       THREADS         -- run n tests at once
#
# -----------------------------------------------------------------------------

# export the value of $MAKE for invocation
export MAKE

RUNTESTS     = $(TOP)/driver/runtests.py
COMPILER     = gac
CONFIGDIR    = $(TOP)/config
CONFIG       = $(CONFIGDIR)/$(COMPILER)

RUNTEST_OPTS =

exeext =

$(eval $(call get-gac-feature-bool,GacWithNativeCodeGen,Have native code generator))
ifeq "$(GacWithNativeCodeGen)" "YES"
RUNTEST_OPTS += -e gac_with_native_codegen=1
else
RUNTEST_OPTS += -e gac_with_native_codegen=0
endif


ifneq "$(shell $(SHELL) -c 'llvmc --version | grep version' 2> /dev/null)" ""
  $(eval $(call get-gac-feature-bool,GacWithLlvmCodeGen,Have llvm code generator))
  ifeq "$(GacWithLlvmCodeGen)" "YES"
  RUNTEST_OPTS += -e gac_with_llvm=1
  else
  RUNTEST_OPTS += -e gac_with_llvm=0
  endif
else
  RUNTEST_OPTS += -e gac_with_llvm=0
endif

ifeq "$(WINDOWS)" "YES"
RUNTEST_OPTS += -e windows=True
else
RUNTEST_OPTS += -e windows=False
endif

ifeq "$(DARWIN)" "YES"
RUNTEST_OPTS += -e darwin=True
else
RUNTEST_OPTS += -e darwin=False
endif

ifeq "$(IN_TREE_COMPILER)" "YES"
RUNTEST_OPTS += -e in_tree_compiler=True
else
RUNTEST_OPTS += -e in_tree_compiler=False
endif

ifneq "$(THREADS)" ""
RUNTEST_OPTS += --threads=$(THREADS)
endif

RUNTEST_OPTS +=  \
	--rootdir=. \
	--config=$(CONFIG) \
	-e 'config.confdir="$(CONFIGDIR)"' \
	-e 'config.compiler="$(TEST_AC)"' \
	-e 'config.compiler_always_flags.append("$(EXTRA_AC_OPTS)")' \
	-e 'config.platform="$(TARGETPLATFORM)"' \
	-e 'config.os="$(TargetOS_CPP)"' \
	-e 'config.arch="$(TargetARCH_CPP)"' \
	-e 'config.wordsize="$(WORDSIZE)"' \
	-e 'default_testopts.cleanup="$(CLEANUP)"' \
	-e 'config.timeout=int($(TIMEOUT)) or config.timeout' \
	-e 'config.timeout_prog="$(TIMEOUT_PROGRAM)"' \
	-e 'config.exeext="$(exeext)"' \
	-e 'config.top="$(TOP_ABS)"'

ifneq "$(OUTPUT_SUMMARY)" ""
RUNTEST_OPTS +=  \
	--output-summary "$(OUTPUT_SUMMARY)"
endif

RUNTEST_OPTS +=  \
	$(EXTRA_RUNTEST_OPTS)

ifeq "$(fast)" "YES"
setfast = -e config.fast=1
else
setfast = 
endif

ifeq "$(accept)" "YES"
setaccept = -e config.accept=1
else
setaccept = 
endif

TESTS	=
TEST	= 
WAY 	=

.PHONY: all boot test verbose accept fast

all: test

TIMEOUT_PROGRAM = $(TOP)/timeout/install-inplace/bin/timeout$(exeext)

boot: $(TIMEOUT_PROGRAM)

$(TIMEOUT_PROGRAM) :
	@echo "Looks like you don't have timeout, building it first..."
	$(MAKE) -C $(TOP)/timeout all

test: $(TIMEOUT_PROGRAM)
	$(PYTHON) $(RUNTESTS) $(RUNTEST_OPTS) \
		$(patsubst %, --only=%, $(TEST)) \
		$(patsubst %, --only=%, $(TESTS)) \
		$(patsubst %, --way=%, $(WAY)) \
		$(patsubst %, --skipway=%, $(SKIPWAY)) \
		$(setfast) \
		$(setaccept)

verbose: test

accept:
	$(MAKE) accept=YES

fast:
	$(MAKE) fast=YES

