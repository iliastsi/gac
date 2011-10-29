# 
# (c) Simon Marlow 2002
#

# -----------------------------------------------------------------------------
# Configuration info

# There is a single global instance of this structure, stored in the
# variable config below.  The fields of the structure are filled in by
# the appropriate config script(s) for this compiler/platform, in
# ../config.
# 
# Bits of the structure may also be filled in from the command line,
# via the build system, using the '-e' option to runtests.

class TestConfig:
    def __init__(self):

        # Where the testsuite root is
        self.top = ''

        # Directories below which to look for test description files (foo.T)
        self.rootdirs = []

        # Run these tests only (run all tests if empty)
        self.only = []

        # File in which to save the summary
        self.output_summary = ''

        # File in which to save the times
        self.times_file = ''

        # Verbosity level
        self.verbose = 1

        # Compiler type (ghc, hugs, nhc, etc.)
        self.compiler_type = ''

        # Path to the compiler
        self.compiler = ''

        # Compiler version info
        self.compiler_version = ''
        self.compiler_maj_version = ''
        self.compiler_tags = []

        # Flags we always give to this compiler
        self.compiler_always_flags = []
        
        # Which ways to run tests (when compiling and running respectively)
        # Other ways are added from the command line if we have the appropriate
        # libraries.
        self.compile_ways = []
        self.run_ways     = []
        self.other_ways   = []

        # The ways selected via the command line.
        self.cmdline_ways = []

        # Lists of flags for each way
        self.way_flags = {}

        # Do we have shared libraries?
        self.have_shared_libs = False

        # Are we testing an in-tree compiler?
        self.in_tree_compiler = True

        # the timeout program
        self.timeout_prog = ''
        self.timeout = 300
        
        # threads
        self.threads = 1
        self.use_threads = 0

global config
config = TestConfig()

def getConfig():
    return config

# -----------------------------------------------------------------------------
# Information about the current test run

class TestRun:
   def __init__(self):
       self.start_time = ''
       self.total_tests = 0
       self.total_test_cases = 0
       self.n_framework_failures = 0
       self.framework_failures = {}
       self.n_tests_skipped = 0
       self.tests_skipped = {}
       self.n_expected_passes = 0
       self.expected_passes = {}
       self.n_expected_failures = 0
       self.expected_failures = {}
       self.n_unexpected_passes = 0
       self.unexpected_passes = {}
       self.n_unexpected_failures = 0
       self.unexpected_failures = {}
       
global t
t = TestRun()

def getTestRun():
    return t

# -----------------------------------------------------------------------------
# Information about the current test

class TestOptions:
   def __init__(self):

       # skip this test?
       self.skip = 0;

       # skip these ways
       self.omit_ways = []

       # skip all ways except these ([] == do all ways)
       self.only_ways = []

       # add these ways to the default set
       self.extra_ways = []

       # the result we normally expect for this test
       self.expect = 'pass'

       # override the expected result for certain ways
       self.expect_fail_for = []

       # the stdin file that this test will use (empty for <name>.stdin)
       self.stdin = ''

       # don't compare output
       self.ignore_output = 0

       # don't give anything as stdin
       self.no_stdin = 0

       # extra compiler opts for this test
       self.extra_hc_opts = ''

       # extra run opts for this test
       self.extra_run_opts = ''

       # expected exit code
       self.exit_code = 0

       # should we clean up after ourselves?
       self.cleanup = ''

       # extra files to clean afterward
       self.clean_files = []

       # should we run this test alone, i.e. not run it in parallel with
       # any other threads
       self.alone = 0

       # Command to run before the test
       self.pre_cmd = None

       # Command to run for extra cleaning
       self.clean_cmd = None

       # Prefix to put on the command before running it
       self.cmd_prefix = ''

       # Prefix to put on the command before compiling it
       self.compile_cmd_prefix = ''

       # The directory the test is in
       self.testdir = '.'

# The default set of options
global default_testopts
default_testopts = TestOptions()

