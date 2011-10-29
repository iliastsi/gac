# 
# (c) Simon Marlow 2002
#

# ToDo:
#   GHCi tests
#   expect failure for some ways only

import sys
import os
import string
import getopt
import platform
import time
import re

from testutil import *
from testglobals import *

# Readline sometimes spews out ANSI escapes for some values of TERM,
# which result in test failures. Thus set TERM to a nice, simple, safe
# value.
os.environ['TERM'] = 'vt100'


global config
config = getConfig() # get it from testglobals

# -----------------------------------------------------------------------------
# cmd-line options

long_options = [
  "config=",  		# config file
  "rootdir=", 		# root of tree containing tests (default: .)
  "output-summary=", 	# file in which to save the (human-readable) summary
  "only=",		# just this test (can be give multiple --only= flags)
  "way=",		# just this way
  "skipway=",		# skip this way
  "threads=",           # threads to run simultaneously
  ]

opts, args = getopt.getopt(sys.argv[1:], "e:", long_options)
       
for opt,arg in opts:
    if opt == '--config':
        execfile(arg)

    # -e is a string to execute from the command line.  For example:
    # testframe -e 'config.compiler=gac-0.9'
    if opt == '-e':
        exec arg

    if opt == '--rootdir':
        config.rootdirs.append(arg)

    if opt == '--output-summary':
        config.output_summary = arg

    if opt == '--only':
        config.only.append(arg)

    if opt == '--way':
        if (arg not in config.run_ways and arg not in config.compile_ways and arg not in config.other_ways):
            sys.stderr.write("ERROR: requested way \'" +
                             arg + "\' does not exist\n")
            sys.exit(1)
        config.cmdline_ways = [arg] + config.cmdline_ways
        if (arg in config.other_ways):
            config.run_ways = [arg] + config.run_ways
            config.compile_ways = [arg] + config.compile_ways

    if opt == '--skipway':
        if (arg not in config.run_ways and arg not in config.compile_ways and arg not in config.other_ways):
            sys.stderr.write("ERROR: requested way \'" +
                             arg + "\' does not exist\n")
            sys.exit(1)
        config.other_ways = filter(neq(arg), config.other_ways)
        config.run_ways = filter(neq(arg), config.run_ways)
        config.compile_ways = filter(neq(arg), config.compile_ways)

    if opt == '--threads':
        config.threads = int(arg)
        config.use_threads = 1

if config.use_threads == 1:
    # Trac #1558 says threads don't work in python 2.4.4, but do
    # in 2.5.2. Probably >= 2.5 is sufficient, but let's be
    # conservative here.
    # Some versions of python have things like '1c1' for some of
    # these components (see trac #3091), but int() chokes on the
    # 'c1', so we drop it.
    (maj, min, pat) = platform.python_version_tuple()
    # We wrap maj, min, and pat in str() to work around a bug in python
    # 2.6.1
    maj = int(re.sub('[^0-9].*', '', str(maj)))
    min = int(re.sub('[^0-9].*', '', str(min)))
    pat = int(re.sub('[^0-9].*', '', str(pat)))
    if (maj, min, pat) < (2, 5, 2):
        print "Warning: Ignoring request to use threads as python version < 2.5.2"
        config.use_threads = 0
    if windows:
        print "Warning: Ignoring request to use threads as running on Windows"
        config.use_threads = 0

# Try to use UTF8
# Try and find a utf8 locale to use
# First see if we already have a UTF8 locale
h = os.popen('locale | grep LC_CTYPE | grep -i utf', 'r')
v = h.read()
h.close()
if v == '':
    # We don't, so now see if 'locale -a' works
    h = os.popen('locale -a', 'r')
    v = h.read()
    h.close()
    if v != '':
        # If it does then use the first utf8 locale that is available
        h = os.popen('locale -a | grep -i "utf8\|utf-8" 2>/dev/null', 'r')
        v = h.readline().strip()
        h.close()
        if v != '':
            os.environ['LC_ALL'] = v
            print "setting LC_ALL to", v
        else:
            print 'WARNING: No UTF8 locale found.'
            print 'You may get some spurious test failures.'

# This has to come after arg parsing as the args can change the compiler
get_compiler_info()

# Can't import this earlier as we need to know if threading will be
# enabled or not
from testlib import *

global testopts_local
testopts_local.x = TestOptions()

global thisdir_testopts
thisdir_testopts = getThisDirTestOpts()

if config.use_threads:
    t.lock = threading.Lock()
    t.thread_pool = threading.Condition(t.lock)
    t.running_threads = 0

# if timeout == -1 then we try to calculate a sensible value
if config.timeout == -1:
    config.timeout = 300

print 'Timeout is ' + str(config.timeout)

# -----------------------------------------------------------------------------
# The main dude

if config.rootdirs == []:
    config.rootdirs = ['.']

t_files = findTFiles(config.rootdirs)

print 'Found', len(t_files), '.T files...'

t = getTestRun()

t.start_time = chop(os.popen('date').read())

print 'Beginning test run at', t.start_time

# set stdout to unbuffered (is this the best way to do it?)
sys.stdout.flush()
sys.stdout = os.fdopen(sys.__stdout__.fileno(), "w", 0)

# First collect all the tests to be run
for file in t_files:
    print '====> Scanning', file
    newTestDir(os.path.dirname(file))
    try:
        execfile(file)
    except:
        print '*** framework failure: found an error while executing ', file, ':'
        t.n_framework_failures = t.n_framework_failures + 1
        traceback.print_exc()

# Now run all the tests
if config.use_threads:
    t.running_threads=0
for oneTest in allTests:
    oneTest()
if config.use_threads:
    t.thread_pool.acquire()
    while t.running_threads>0:
        t.thread_pool.wait()
    t.thread_pool.release()
        
summary(t, sys.stdout)

if config.output_summary != '':
    summary(t, open(config.output_summary, 'w'))

sys.exit(0)

