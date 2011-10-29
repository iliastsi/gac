
default: all

HAVE_EVAL := NO
$(eval HAVE_EVAL := YES)

ifeq "$(HAVE_EVAL)" "NO"
$(error Your make does not support eval. You need GNU make >= 3.81)
endif

ifeq "$(abspath /)" ""
$(error Your make does not support abspath. You need GNU make >= 3.81)
endif

show:
	@echo '$(VALUE)="$($(VALUE))"'

define canonicalise
# $1 = path variable
$1_CYGPATH := $$(shell $(SHELL) -c "cygpath -m '$$($1)'" 2> /dev/null)
ifneq "$$($1_CYGPATH)" ""
# We use 'override' in case we are trying to update a value given on
# the commandline (e.g. TEST_AC)
override $1 := $$($1_CYGPATH)
endif
endef

define canonicaliseExecutable
# $1 = program path variable
ifneq "$$(shell test -x '$$($1).exe' && echo exists)" ""
# We use 'override' in case we are trying to update a value given on
# the commandline (e.g. TEST_AC)
override $1 := $$($1).exe
endif
$(call canonicalise,$1)
endef

define get-gac-field # $1 = result variable, $2 = field name
$1 := $$(shell '$$(TEST_AC)' --info | grep '^ *.("$2",' | tr -d '\r' | sed -e 's/.*", *"//' -e 's/")$$$$//')
endef

define get-gac-feature-bool # $1 = result variable, $2 = field name
SHELL_RES := $$(shell '$$(TEST_AC)' --info | grep '^ *.("$2",' | tr -d '\r' | sed -e 's/.*", *"//' -e 's/")$$$$//')
$1 := $$(strip \
	  $$(if $$(SHELL_RES), \
         $$(if $$(subst YES,,$$(SHELL_RES)), \
            $$(if $$(subst NO,,$$(SHELL_RES)), \
               $$(warning gac info field not YES or NO: $2: $$(SHELL_RES)), \
               NO), \
            YES), \
         $$(warning gac info field not found: $2)))
endef

ifeq "$(TEST_AC)" ""

TEST_AC := $(abspath $(TOP)/../gac)

ifneq "$(wildcard $(TEST_AC) $(TEST_AC).exe)" ""
IN_TREE_COMPILER = YES

else
IN_TREE_COMPILER = NO
TEST_AC := $(shell which gac)
endif

else
IN_TREE_COMPILER = NO
# We want to support both "gac" and "/usr/bin/gac" as values of TEST_AC
# passed in by the user, but
#     which ghc          == /usr/bin/ghc
#     which /usr/bin/ghc == /usr/bin/ghc
# so we can just always 'which' it. We need to use 'override' in order
# to override a value given on the commandline.
override TEST_AC := $(shell which '$(TEST_AC)')
endif

# We can't use $(dir ...) here as TEST_AC might be in a path
# containing spaces
BIN_ROOT = $(shell dirname '$(TEST_AC)')

$(eval $(call canonicaliseExecutable,TEST_AC))
ifeq "$(shell test -x '$(TEST_AC)' && echo exists)" ""
$(error Cannot find gac: $(TEST_AC))
endif

TOP_ABS := $(abspath $(TOP))
$(eval $(call canonicalise,TOP_ABS))

CP = cp
RM = rm -f
PYTHON = python

WINDOWS = NO
DARWIN = NO
