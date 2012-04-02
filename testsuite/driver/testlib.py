# 
# (c) Simon Marlow 2002
#

import sys
import os
import errno
import string
import re
import traceback
import copy
import glob
import types

have_subprocess = False
try:
    import subprocess
    have_subprocess = True
except:
    print "Warning: subprocess not found, will fall back to spawnv"

from string import join
from testglobals import *
from testutil import *

if config.use_threads:
    import threading
    import thread

# Options valid for all the tests in the current "directory".  After
# each test, we reset the options to these.  To change the options for
# multiple tests, the function setTestOpts() below can be used to alter
# these options.
global thisdir_testopts
thisdir_testopts = TestOptions()

def getThisDirTestOpts():
    return thisdir_testopts

# Options valid for the current test only (these get reset to
# testdir_testopts after each test).

global testopts_local
if config.use_threads:
    testopts_local = threading.local()
else:
    class TestOpts_Local:
        pass
    testopts_local = TestOpts_Local()

def getTestOpts():
    return testopts_local.x

def setLocalTestOpts(opts):
    global testopts_local
    testopts_local.x=opts

# This can be called at the top of a file of tests, to set default test options
# for the following tests.
def setTestOpts( f ):
    f( thisdir_testopts );

# -----------------------------------------------------------------------------
# Canned setup functions for common cases.  eg. for a test you might say
#
#      test('test001', normal, compile, [''])
#
# to run it without any options, but change it to
#
#      test('test001', expect_fail, compile, [''])
#
# to expect failure for this test.

def normal( opts ):
    return;

def skip( opts ):
    opts.skip = 1

def expect_fail( opts ):
    opts.expect = 'fail';

def reqlib( lib ):
    return lambda opts, l=lib: _reqlib (opts, l )

# Cache the results of looking to see if we have a library or not.
# This makes quite a difference, especially on Windows.
have_lib = {}

def _reqlib( opts, lib ):
    if have_lib.has_key(lib):
        got_it = have_lib[lib]
    else:
        if have_subprocess:
            # By preference we use subprocess, as the alternative uses
            # /dev/null which mingw doesn't have.
            p = subprocess.Popen([config.ghc_pkg, '--no-user-package-conf', 'describe', lib],
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
            # read from stdout and stderr to avoid blocking due to
            # buffers filling
            p.communicate()
            r = p.wait()
        else:
            r = os.system(config.ghc_pkg + ' describe ' + lib
                                         + ' > /dev/null 2> /dev/null')
        got_it = r == 0
        have_lib[lib] = got_it

    if not got_it:
        opts.expect = 'fail'

def req_profiling( opts ):
    if not config.have_profiling:
        opts.expect = 'fail'

def expect_broken( bug ):
    return lambda opts, b=bug: _expect_broken (opts, b )

def _expect_broken( opts, bug ):
    opts.expect = 'fail';

def ignore_output( opts ):
    opts.ignore_output = 1

def no_stdin( opts ):
    opts.no_stdin = 1

# -----

def expect_fail_for( ways ):
    return lambda opts, w=ways: _expect_fail_for( opts, w )

def _expect_fail_for( opts, ways ):
    opts.expect_fail_for = ways

def expect_broken_for( bug, ways ):
    return lambda opts, b=bug, w=ways: _expect_broken_for( opts, b, w )

def _expect_broken_for( opts, bug, ways ):
    opts.expect_fail_for = ways

# -----

def omit_ways( ways ):
    return lambda opts, w=ways: _omit_ways( opts, w )

def _omit_ways( opts, ways ):
    opts.omit_ways = ways

# -----

def only_ways( ways ):
    return lambda opts, w=ways: _only_ways( opts, w )

def _only_ways( opts, ways ):
    opts.only_ways = ways

# -----

def extra_ways( ways ):
    return lambda opts, w=ways: _extra_ways( opts, w )

def _extra_ways( opts, ways ):
    opts.extra_ways = ways

# -----

def omit_compiler_types( compiler_types ):
   return lambda opts, c=compiler_types: _omit_compiler_types(opts, c)

def _omit_compiler_types( opts, compiler_types ):
    if config.compiler_type in compiler_types:
	opts.skip = 1

# -----

def only_compiler_types( compiler_types ):
   return lambda opts, c=compiler_types: _only_compiler_types(opts, c)

def _only_compiler_types( opts, compiler_types ):
    if config.compiler_type not in compiler_types:
	opts.skip = 1

# -----

def set_stdin( file ):
   return lambda opts, f=file: _set_stdin(opts, f);

def _set_stdin( opts, f ):
   opts.stdin = f

# -----

def exit_code( val ):
    return lambda opts, v=val: _exit_code(opts, v);

def _exit_code( opts, v ):
    opts.exit_code = v

# -----

def extra_run_opts( val ):
    return lambda opts, v=val: _extra_run_opts(opts, v);

def _extra_run_opts( opts, v ):
    opts.extra_run_opts = v

# -----

def extra_clean( files ):
    return lambda opts, v=files: _extra_clean(opts, v);

def _extra_clean( opts, v ):
    opts.clean_files = v

# -----

def fail_if_no_utf8(opts):
  if not config.have_utf8:
      opts.expect = 'fail';

# ----

def skip_if_fast(opts):
  if config.fast:
      opts.skip = 1

# -----

def if_platform( plat, f ):
    if config.platform == plat:
        return f
    else:
        return normal

def if_not_platform( plat, f ):
    if config.platform != plat:
        return f
    else:
        return normal

# ---

def if_in_tree_compiler( f ):
    if config.in_tree_compiler:
        return f
    else:
        return normal

def unless_in_tree_compiler( f ):
    if config.in_tree_compiler:
        return normal
    else:
        return f

def if_compiler_type( compiler, f ):
    if config.compiler_type == compiler:
        return f
    else:
        return normal

def if_compiler_profiled( f ):
    if config.compiler_profiled:
        return f
    else:
        return normal

def unless_compiler_profiled( f ):
    if config.compiler_profiled:
        return normal
    else:
        return f

def if_compiler_lt( compiler, version, f ):
    if config.compiler_type == compiler and \
       version_lt(config.compiler_version, version):
        return f
    else:
        return normal

def if_compiler_le( compiler, version, f ):
    if config.compiler_type == compiler and \
       version_le(config.compiler_version, version):
        return f
    else:
        return normal

def if_compiler_gt( compiler, version, f ):
    if config.compiler_type == compiler and \
       version_gt(config.compiler_version, version):
        return f
    else:
        return normal

def if_compiler_ge( compiler, version, f ):
    if config.compiler_type == compiler and \
       version_ge(config.compiler_version, version):
        return f
    else:
        return normal

def namebase( nb ):
   return lambda opts, nb=nb: _namebase(opts, nb)

def _namebase( opts, nb ):
    opts.with_namebase = nb

# ---

def if_tag( tag, f ):
    if tag in config.compiler_tags:
        return f
    else:
        return normal

def unless_tag( tag, f ):
    if not (tag in config.compiler_tags):
        return f
    else:
        return normal

# ---
def alone(opts):
    opts.alone = 1

# ---
def literate( opts ):
    opts.literate = 1;

def c_src( opts ):
    opts.c_src = 1;

# ----

def pre_cmd( cmd ):
    return lambda opts, c=cmd: _pre_cmd(opts, cmd)

def _pre_cmd( opts, cmd ):
    opts.pre_cmd = cmd

# ----

def clean_cmd( cmd ):
    return lambda opts, c=cmd: _clean_cmd(opts, cmd)

def _clean_cmd( opts, cmd ):
    opts.clean_cmd = cmd

# ----

def cmd_prefix( prefix ):
    return lambda opts, p=prefix: _cmd_prefix(opts, prefix)

def _cmd_prefix( opts, prefix ):
    opts.cmd_prefix = prefix

# ----

def compile_cmd_prefix( prefix ):
    return lambda opts, p=prefix: _compile_cmd_prefix(opts, prefix)

def _compile_cmd_prefix( opts, prefix ):
    opts.compile_cmd_prefix = prefix

# ----

def normalise_slashes( opts ):
    opts.extra_normaliser = normalise_slashes_

def normalise_fun( fun ):
    return lambda opts, f=fun: _normalise_fun(opts, f)

def _normalise_fun( opts, f ):
    opts.extra_normaliser = f

# ----
# Function for composing two opt-fns together

def composes( fs ):
    return reduce(lambda f, g: compose(f, g), fs)

def compose( f, g ):
    return lambda opts, f=f, g=g: _compose(opts,f,g)

def _compose( opts, f, g ):    
    f(opts)
    g(opts)

# -----------------------------------------------------------------------------
# The current directory of tests

def newTestDir( dir ):
    global thisdir_testopts
    # reset the options for this test directory
    thisdir_testopts = copy.copy(default_testopts)
    thisdir_testopts.testdir = dir

# -----------------------------------------------------------------------------
# Actually doing tests

allTests = []
allTestNames = set([])

def runTest (opts, name, setup, func, args):
    n = 1

    if type(setup) is types.ListType:
       setup = composes(setup)

    setup(opts)

    if opts.alone:
        n = config.threads

    ok = 0

    if config.use_threads:
        t.thread_pool.acquire()
        try:
            while config.threads<(t.running_threads+n):
                t.thread_pool.wait()
            t.running_threads = t.running_threads+n
            ok=1
            t.thread_pool.release()
            thread.start_new_thread(test_common_thread, (n, name, opts, func, args))
        except:
            if not ok:
                t.thread_pool.release()
    else:
        test_common_work (name, opts, func, args)

# name  :: String
# setup :: TestOpts -> IO ()  
def test (name, setup, func, args):
    global allTests
    global allTestNames
    if name in allTestNames:
        framework_fail(name, 'duplicate', 'There are multiple tests with this name')
    myTestOpts = copy.copy(thisdir_testopts)
    allTests += [lambda : runTest(myTestOpts, name, setup, func, args)]
    allTestNames.add(name)

if config.use_threads:
    def test_common_thread(n, name, opts, func, args):
        t.lock.acquire()
        try:
            test_common_work(name,opts,func,args)
        finally:
            t.lock.release()
            t.thread_pool.acquire()
            t.running_threads = t.running_threads - n
            t.thread_pool.notify()
            t.thread_pool.release()
    

def test_common_work (name, opts, func, args):
    t.total_tests = t.total_tests+1
    setLocalTestOpts(opts)

    # All the ways we might run this test
    if func == compile:
        all_ways = config.compile_ways
    elif func == compile_and_run:
        all_ways = config.run_ways
    elif func == ghci_script:
        if 'ghci' in config.run_ways:
            all_ways = ['ghci']
        else:
            all_ways = []
    else:
        all_ways = ['normal']

    # A test itself can request extra ways by setting opts.extra_ways
    all_ways = all_ways + filter(lambda way: way not in all_ways,
                                 opts.extra_ways)

    t.total_test_cases = t.total_test_cases + len(all_ways)

    ok_way = lambda way: \
        not getTestOpts().skip \
        and (config.only == [] or name in config.only) \
        and (getTestOpts().only_ways == [] or way in getTestOpts().only_ways) \
        and (config.cmdline_ways == [] or way in config.cmdline_ways) \
        and way not in getTestOpts().omit_ways

    # Which ways we are asked to skip
    do_ways = filter (ok_way,all_ways)

    # In fast mode, we skip all but one way
    if config.fast and len(do_ways) > 0:
        do_ways = [do_ways[0]]

    # Run the required tests...
    for way in do_ways:
        do_test (name, way, func, args)

    for way in all_ways:
        if way not in do_ways:
            skiptest (name,way)

    if getTestOpts().cleanup != '':
        clean(map (lambda suff: name + suff,
                  ['', '.exe', '.exe.manifest', '.genscript',
                   '.stderr.normalised',        '.stdout.normalised',
                   '.run.stderr',               '.run.stdout',
                   '.run.stderr.normalised',    '.run.stdout.normalised',
                   '.comp.stderr',              '.comp.stdout',
                   '.comp.stderr.normalised',   '.comp.stdout.normalised',
                   '.o', '.s', '.bc']))

        clean(getTestOpts().clean_files)

        try:
            cleanCmd = getTestOpts().clean_cmd
            if cleanCmd != None:
                result = runCmd('cd ' + getTestOpts().testdir + ' && ' + cleanCmd)
                if result != 0:
                    framework_fail(name, 'cleaning', 'clean-command failed: ' + str(result))
        except e:
            framework_fail(name, 'cleaning', 'clean-command exception')

def clean(names):
    clean_full_paths(map (lambda name: in_testdir(name), names))

def clean_full_paths(names):
    for name in names:
        try:
            # Remove files...
            os.remove(name)
        except OSError, e1:
            try:
                # ... and empty directories
                os.rmdir(name)
            except OSError, e2:
                # We don't want to fail here, but we do want to know
                # what went wrong, so print out the exceptions.
                # ENOENT isn't a problem, though, as we clean files
                # that don't necessarily exist.
                if e1.errno != errno.ENOENT:
                    print e1
                if e2.errno != errno.ENOENT:
                    print e2

def do_test(name, way, func, args):
    full_name = name + '(' + way + ')'

    try:
        print '=====>', full_name, t.total_tests, 'of', len(allTests), \
                        str([t.n_unexpected_passes,   \
                             t.n_unexpected_failures, \
                             t.n_framework_failures])
        
        if config.use_threads:
            t.lock.release()

        try:
            preCmd = getTestOpts().pre_cmd
            if preCmd != None:
                result = runCmd('cd ' + getTestOpts().testdir + ' && ' + preCmd)
                if result != 0:
                    framework_fail(name, way, 'pre-command failed: ' + str(result))
        except e:
            framework_fail(name, way, 'pre-command exception')

        try:
            result = apply(func, [name,way] + args)
        finally:
            if config.use_threads:
                t.lock.acquire()
        
        if getTestOpts().expect != 'pass' and getTestOpts().expect != 'fail' or \
           result != 'pass' and result != 'fail':
            framework_fail(name, way, 'bad results ' + result)

        if result == 'pass':
            if getTestOpts().expect == 'pass' \
               and way not in getTestOpts().expect_fail_for:
                t.n_expected_passes = t.n_expected_passes + 1
                if name in t.expected_passes:
                    t.expected_passes[name].append(way)
                else:
                    t.expected_passes[name] = [way]
            else:
                print '*** unexpected pass for', full_name
                t.n_unexpected_passes = t.n_unexpected_passes + 1
                if name in t.unexpected_passes:
                    t.unexpected_passes[name].append(way)
                else:
                    t.unexpected_passes[name] = [way]
        else:
            if getTestOpts().expect == 'pass' \
               and way not in getTestOpts().expect_fail_for:
                print '*** unexpected failure for', full_name
                t.n_unexpected_failures = t.n_unexpected_failures + 1
                if name in t.unexpected_failures:
                    t.unexpected_failures[name].append(way)
                else:
                    t.unexpected_failures[name] = [way]
            else:
                t.n_expected_failures = t.n_expected_failures + 1
                if name in t.expected_failures:
                    t.expected_failures[name].append(way)
                else:
                    t.expected_failures[name] = [way]
    except:
        framework_fail(name, way, 'do_test exception')
        traceback.print_exc()

def skiptest (name, way):
    # print 'Skipping test \"', name, '\"'
    t.n_tests_skipped = t.n_tests_skipped + 1
    if name in t.tests_skipped:
        t.tests_skipped[name].append(way)
    else:
        t.tests_skipped[name] = [way]

def framework_fail( name, way, reason ):
    full_name = name + '(' + way + ')'
    print '*** framework failure for', full_name, reason, ':'
    t.n_framework_failures = t.n_framework_failures + 1
    if name in t.framework_failures:
        t.framework_failures[name].append(way)
    else:
        t.framework_failures[name] = [way]

# -----------------------------------------------------------------------------
# Generic command tests

# A generic command test is expected to run and exit successfully.
#
# The expected exit code can be changed via exit_code() as normal, and
# the expected stdout/stderr are stored in <testname>.stdout and
# <testname>.stderr.  The output of the command can be ignored
# altogether by using run_command_ignore_output instead of
# run_command.

def run_command( name, way, cmd ):
    return simple_run( name, '', cmd, '' )

# -----------------------------------------------------------------------------
# GHCi tests

def ghci_script( name, way, script ):
    # filter out -fforce-recomp from compiler_always_flags, because we're
    # actually testing the recompilation behaviour in the GHCi tests.
    flags = filter(lambda f: f != '-fforce-recomp', config.compiler_always_flags)
    flags.append(getTestOpts().extra_hc_opts)

    # We pass HC and HC_OPTS as environment variables, so that the
    # script can invoke the correct compiler by using ':! $HC $HC_OPTS'
    cmd = "HC='" + config.compiler + "' " + \
          "HC_OPTS='" + join(flags,' ') + "' " + \
          "'" + config.compiler + "'" + \
          ' --interactive -v0 -ignore-dot-ghci ' + \
          join(flags,' ')

    getTestOpts().stdin = script
    return simple_run( name, way, cmd, getTestOpts().extra_run_opts )

# -----------------------------------------------------------------------------
# Compile-only tests

def compile( name, way, extra_hc_opts ):
    return do_compile( name, way, 0, extra_hc_opts )

def compile_fail( name, way, extra_hc_opts ):
    return do_compile( name, way, 1, extra_hc_opts )

def do_compile( name, way, should_fail, extra_hc_opts ):
    # print 'Compile only, extra args = ', extra_hc_opts
    pretest_cleanup(name)
    result = simple_build( name, way, extra_hc_opts, should_fail, 0 )
    
    if result == 'fail':
        return result

    # the actual stderr should always match the expected, regardless
    # of whether we expected the compilation to fail or not (successful
    # compilations may generate warnings).

    if getTestOpts().with_namebase == None:
        namebase = name
    else:
        namebase = getTestOpts().with_namebase

    (platform_specific, expected_stderr_file) = platform_wordsize_qualify(namebase, 'stderr')
    actual_stderr_file = qualify(name, 'comp.stderr')

    if not compare_outputs('stderr', normalise_errmsg, normalise_whitespace, \
                           expected_stderr_file, actual_stderr_file):
        return 'fail'

    # no problems found, this test passed
    return 'pass'

# -----------------------------------------------------------------------------
# Compile-and-run tests

def compile_and_run__( name, way, extra_hc_opts, ):
    # print 'Compile and run, extra args = ', extra_hc_opts
    pretest_cleanup(name)

    # compiled...
    result = simple_build( name, way, extra_hc_opts, 0, 1 )
    if result == 'fail':
        return result

    cmd = './' + name;
    if getTestOpts().cmd_prefix != '':
        cmd = getTestOpts().cmd_prefix + ' ' + cmd;

    # we don't check the compiler's stderr for a compile-and-run test
    return simple_run( name, way, cmd, getTestOpts().extra_run_opts )

def compile_and_run( name, way, extra_hc_opts ):
    return compile_and_run__( name, way, extra_hc_opts, '')

# -----------------------------------------------------------------------------
# Build a single-module program

def simple_build( name, way, extra_hc_opts, should_fail, link ):
    opts = getTestOpts()
    errname = add_suffix(name, 'comp.stderr')
    rm_no_fail( errname )
    rm_no_fail( name )
    
    srcname = add_alan_suffix(name)

    to_do = ''
    if link:
        to_do = '-o ' + name
    else:
        to_do = '-c' # just compile

    if getTestOpts().compile_cmd_prefix == '':
        cmd_prefix = ''
    else:
        cmd_prefix = getTestOpts().compile_cmd_prefix + ' '

    cmd = 'cd ' + getTestOpts().testdir + " && " + cmd_prefix + "'" \
          + config.compiler + "' " \
          + join(config.compiler_always_flags,' ') + ' ' \
          + to_do + ' ' + srcname + ' ' \
          + join(config.way_flags[way],' ') + ' ' \
          + extra_hc_opts + ' ' \
          + opts.extra_hc_opts + ' ' \
          + '>' + errname + ' 2>&1'

    result = runCmd(cmd)

    if result != 0 and not should_fail:
        actual_stderr = qualify(name, 'comp.stderr')
        if_verbose(1,'Compile failed (status ' + `result` + ') errors were:')
        if_verbose(1,open(actual_stderr).read())

    # ToDo: if the sub-shell was killed by ^C, then exit

    if should_fail:
        if result == 0:
            return 'fail'
    else:
        if result != 0:
            return 'fail'

    return 'pass'

# -----------------------------------------------------------------------------
# Run a program and check its output
#
# If testname.stdin exists, route input from that, else
# from /dev/null.  Route output to testname.run.stdout and 
# testname.run.stderr.  Returns the exit code of the run.

def simple_run( name, way, prog, args ):
    opts = getTestOpts()

    # figure out what to use for stdin
    if opts.stdin != '':
        use_stdin = opts.stdin
    else:
        stdin_file = add_suffix(name, 'stdin')
        if os.path.exists(in_testdir(stdin_file)):
            use_stdin = stdin_file
        else:
            use_stdin = '/dev/null'

    run_stdout = add_suffix(name,'run.stdout')
    run_stderr = add_suffix(name,'run.stderr')

    rm_no_fail(qualify(name,'run.stdout'))
    rm_no_fail(qualify(name,'run.stderr'))
    rm_no_fail(qualify(name, 'hp'))
    rm_no_fail(qualify(name,'ps'))
    rm_no_fail(qualify(name, 'prof'))
   
    my_rts_flags = rts_flags(way)

    if opts.no_stdin:
        stdin_comes_from = ''
    else:
        stdin_comes_from = ' <' + use_stdin
    cmd = 'cd ' + getTestOpts().testdir + ' && ' \
	    + prog + ' ' + args + ' '  \
        + my_rts_flags + ' '       \
        + stdin_comes_from         \
        + ' >' + run_stdout        \
        + ' 2>' + run_stderr

    # run the command
    result = runCmd(cmd)

    exit_code = result >> 8
    signal    = result & 0xff

    # check the exit code
    if exit_code != opts.exit_code:
        print 'Wrong exit code (expected', opts.exit_code, ', actual', exit_code, ')'
        dump_stdout(name)
        dump_stderr(name)
        return 'fail'

    check_hp = my_rts_flags.find("-h") != -1
    check_prof = my_rts_flags.find("-p") != -1

    if not opts.ignore_output:
        if not check_stderr_ok(name):
            return 'fail'
        if not check_stdout_ok(name):
            return 'fail'
        # exit_code > 127 probably indicates a crash, so don't try to run hp2ps.
        if check_hp and (exit_code <= 127 or exit_code == 251) and not check_hp_ok(name):
            return 'fail'
        if check_prof and not check_prof_ok(name):
            return 'fail'

    return 'pass'

def rts_flags(way):
    if (way == ''):
        return ''
    else:
        args = config.way_rts_flags[way]

    if args == []:
        return ''
    else:
        return '+RTS ' + join(args,' ') + ' -RTS'

# -----------------------------------------------------------------------------
# Run a program in the interpreter and check its output

def interpreter_run( name, way, extra_hc_opts, compile_only ):
    outname = add_suffix(name, 'interp.stdout')
    errname = add_suffix(name, 'interp.stderr')
    rm_no_fail(outname)
    rm_no_fail(errname)
    rm_no_fail(name)
    
    if getTestOpts().cmd_prefix == '':
        cmd_prefix = ''
    else:
        cmd_prefix = getTestOpts().cmd_prefix + ' '

    srcname = add_alan_suffix(name)
        
    scriptname = add_suffix(name, 'genscript')
    qscriptname = in_testdir(scriptname)
    rm_no_fail(qscriptname)

    delimiter = '===== program output begins here\n'

    script = open(qscriptname, 'w')
    if not compile_only:
        # set the prog name and command-line args to match the compiled
        # environment.
        script.write(':set prog ' + name + '\n')
        script.write(':set args ' + getTestOpts().extra_run_opts + '\n')
        # Add marker lines to the stdout and stderr output files, so we
        # can separate GHCi's output from the program's.
        script.write(':! echo ' + delimiter)
        script.write(':! echo 1>&2 ' + delimiter)
        # Set stdout to be line-buffered to match the compiled environment.
        script.write('System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering\n')
        # wrapping in GHC.TopHandler.runIO ensures we get the same output
        # in the event of an exception as for the compiled program.
        script.write('GHC.TopHandler.runIOFastExit Main.main Prelude.>> Prelude.return ()\n')
    script.close()

    # figure out what to use for stdin
    if getTestOpts().stdin != '':
        stdin_file = in_testdir(getTestOpts().stdin)
    else:
        stdin_file = qualify(name, 'stdin')

    if os.path.exists(stdin_file):
        stdin = open(stdin_file, 'r')
        os.system('cat ' + stdin_file + ' >>' + qscriptname)
        
    script.close()

    cmd = 'cd ' + getTestOpts().testdir + " && " + cmd_prefix + "'" \
          + config.compiler + "' " \
          + join(config.compiler_always_flags,' ') + ' ' \
          + srcname + ' ' \
          + join(config.way_flags[way],' ') + ' ' \
          + extra_hc_opts + ' ' \
          + getTestOpts().extra_hc_opts + ' ' \
          + '<' + scriptname +  ' 1>' + outname + ' 2>' + errname

    result = runCmd(cmd)

    exit_code = result >> 8
    signal    = result & 0xff

    # split the stdout into compilation/program output
    split_file(in_testdir(outname), delimiter,
               qualify(name, 'comp.stdout'),
               qualify(name, 'run.stdout'))
    split_file(in_testdir(errname), delimiter,
               qualify(name, 'comp.stderr'),
               qualify(name, 'run.stderr'))

    # check the exit code
    if exit_code != getTestOpts().exit_code:
        print 'Wrong exit code (expected', getTestOpts().exit_code, ', actual', exit_code, ')'
        dump_stdout(name)
        dump_stderr(name)
        return 'fail'

    # ToDo: if the sub-shell was killed by ^C, then exit

    if getTestOpts().ignore_output or (check_stderr_ok(name) and
                                       check_stdout_ok(name)):
        return 'pass'
    else:
        return 'fail'


def split_file(in_fn, delimiter, out1_fn, out2_fn):
    infile = open(in_fn)
    out1 = open(out1_fn, 'w')
    out2 = open(out2_fn, 'w')

    line = infile.readline()
    line = re.sub('\r', '', line) # ignore Windows EOL
    while (re.sub('^\s*','',line) != delimiter and line != ''):
        out1.write(line)
        line = infile.readline()
	line = re.sub('\r', '', line)
    out1.close()

    line = infile.readline()
    while (line != ''):
        out2.write(line)
        line = infile.readline()
    out2.close()
    
# -----------------------------------------------------------------------------
# Utils

def check_stdout_ok( name ):
   if getTestOpts().with_namebase == None:
       namebase = name
   else:
       namebase = getTestOpts().with_namebase

   actual_stdout_file   = qualify(name, 'run.stdout')
   (platform_specific, expected_stdout_file) = platform_wordsize_qualify(namebase, 'stdout')

   def norm(str):
      if platform_specific:
         return str
      else:
         return normalise_output(str)

   return compare_outputs('stdout', norm, getTestOpts().extra_normaliser, \
                          expected_stdout_file, actual_stdout_file)

def dump_stdout( name ):
   print "Stdout:"
   print read_no_crs(qualify(name, 'run.stdout'))

def check_stderr_ok( name ):
   if getTestOpts().with_namebase == None:
       namebase = name
   else:
       namebase = getTestOpts().with_namebase

   actual_stderr_file   = qualify(name, 'run.stderr')
   (platform_specific, expected_stderr_file) = platform_wordsize_qualify(namebase, 'stderr')

   def norm(str):
      if platform_specific:
         return str
      else:
         return normalise_output(str)

   return compare_outputs('stderr', norm, getTestOpts().extra_normaliser, \
                          expected_stderr_file, actual_stderr_file)

def dump_stderr( name ):
   print "Stderr:"
   print read_no_crs(qualify(name, 'run.stderr'))

def read_no_crs(file):
    h = open(file)
    str = h.read()
    h.close
    return re.sub('\r', '', str)

def write_file(file, str):
    h = open(file, 'w')
    h.write(str)
    h.close

def check_hp_ok(name):

    # do not qualify for hp2ps because we should be in the right directory
    hp2psCmd = "cd " + getTestOpts().testdir + " && '" + config.hp2ps + "' " + name

    hp2psResult = runCmdExitCode(hp2psCmd)

    actual_ps_file = qualify(name, 'ps')
    
    if(hp2psResult == 0):
        if (os.path.exists(actual_ps_file)):
            if gs_working:
                gsResult = runCmdExitCode(genGSCmd(actual_ps_file))
                if (gsResult == 0):
                    return (True)
                else:
                    print "hp2ps output for " + name + "is not valid PostScript"
            else: return (True) # assume postscript is valid without ghostscript
        else: 
            print "hp2ps did not generate PostScript for " + name
            return (False) 
    else:
        print "hp2ps error when processing heap profile for " + name
        return(False)

def check_prof_ok(name):

    prof_file = qualify(name,'prof')

    if not os.path.exists(prof_file):
        print prof_file + " does not exist"
        return(False)

    if os.path.getsize(qualify(name,'prof')) == 0:
        print prof_file + " is empty"
        return(False)

    return(True)

# Compare expected output to actual output, and optionally accept the
# new output. Returns true if output matched or was accepted, false
# otherwise.
def compare_outputs( kind, normaliser, extra_normaliser,
                     expected_file, actual_file ):
    if os.path.exists(expected_file):
        expected_raw = read_no_crs(expected_file)
        expected_str = normaliser(expected_raw)
    else:
        expected_str = ''
        expected_file = ''

    actual_raw = read_no_crs(actual_file)
    actual_str = normaliser(actual_raw)

    if extra_normaliser(expected_str) != extra_normaliser(actual_str):
        print 'Actual ' + kind + ' output differs from expected:'

        if expected_file == '':
            expected_normalised_file = '/dev/null'

        else:
            expected_normalised_file = expected_file + ".normalised"
            write_file(expected_normalised_file, expected_str)
        actual_normalised_file = actual_file + ".normalised"
        write_file(actual_normalised_file, actual_str)

        # Ignore whitespace when diffing. We should only get to this
        # point if there are non-whitespace differences
        r = os.system( 'diff -uw ' + expected_normalised_file + \
                               ' ' + actual_normalised_file )

        # If for some reason there were no non-whitespace differences,
        # then do a full diff
        if r == 0:
            r = os.system( 'diff -u ' + expected_normalised_file + \
                                  ' ' + actual_normalised_file )

        if config.accept:
            if expected_file == '':
                print '*** cannot accept new output: ' + kind + \
                      ' file does not exist.'
                return 0
            else:
                print 'Accepting new output.'
                write_file(expected_file, actual_raw)
                return 1
        return 0
    return 1

def normalise_whitespace( str ):
    # Merge contiguous whitespace characters into a single space.
    str = re.sub('[ \t\n]+', ' ', str)
    return str

def normalise_errmsg( str ):
    # If somefile ends in ".exe" or ".exe:", zap ".exe" (for Windows)
    #    the colon is there because it appears in error messages; this
    #    hacky solution is used in place of more sophisticated filename
    #    mangling
    str = re.sub('([^\\s])\\.exe', '\\1', str)
    # The inplace ghc's are called ghc-bin-stage[123] to avoid filename
    # collisions, so we need to normalise that to just "ghc"
    # (this is for the old build system, I think, so should be removable)
    str = re.sub('ghc-bin-stage[123]', 'ghc', str)
    # The inplace ghc's are called ghc-stage[123] to avoid filename
    # collisions, so we need to normalise that to just "ghc"
    str = re.sub('ghc-stage[123]', 'ghc', str)
    # We sometimes see the name of the integer-gmp package on stderr,
    # but this can change (either the implementation name or the
    # version number), so we canonicalise it here
    str = re.sub('integer-[a-z]+', 'integer-impl', str)
    return str

def normalise_slashes_( str ):
    str = re.sub('\\\\', '/', str)
    return str

def normalise_output( str ):
    # Remove a .exe extension (for Windows)
    # This can occur in error messages generated by the program.
    str = re.sub('([^\\s])\\.exe', '\\1', str)
    return str

def if_verbose( n, str ):
    if config.verbose >= n:
        print str

# Guess flags suitable for the compiler.
def guess_compiler_flags():
   if config.compiler_type == 'ghc':
       return ['-fforce-recomp', '-dcore-lint', '-no-user-package-conf']
   elif config.compiler_type == 'nhc':
       return ['-an-nhc-specific-flag']
   else:
        return []

def rawSystem(cmd_and_args):
    # We prefer subprocess.call to os.spawnv as the latter
    # seems to send its arguments through a shell or something
    # with the Windows (non-cygwin) python. An argument "a b c"
    # turns into three arguments ["a", "b", "c"].

    # However, subprocess is new in python 2.4, so fall back to
    # using spawnv if we don't have it

    if have_subprocess:
        return subprocess.call(cmd_and_args)
    else:
        return os.spawnv(os.P_WAIT, cmd_and_args[0], cmd_and_args)

# cmd is a complex command in Bourne-shell syntax 
# e.g (cd . && 'c:/users/simonpj/darcs/HEAD/compiler/stage1/ghc-inplace' ...etc)
# Hence it must ultimately be run by a Bourne shell
# 
# Mostly it invokes the command wrapped in 'timeout' thus
#	timeout 300 'cd . && ...blah blah'
# so it's timeout's job to invoke the Bourne shell
#
# But watch out for the case when there is no timeout program!
# Then, when using the native Python, os.system will invoke the cmd shell

def runCmd( cmd ):
    if_verbose( 1, cmd )
    r = 0
    if config.platform == 'i386-unknown-mingw32':
	# On MinGW, we will always have timeout
        assert config.timeout_prog!=''

    if config.timeout_prog != '':
        r = rawSystem([config.timeout_prog, str(config.timeout), cmd])
    else:
        r = os.system(cmd)
    return r << 8

def runCmdExitCode( cmd ):
    return (runCmd(cmd) >> 8);


# -----------------------------------------------------------------------------
# checking if ghostscript is available for checking the output of hp2ps

def genGSCmd(psfile):
    return (config.gs + ' -dNODISPLAY -dBATCH -dQUIET -dNOPAUSE ' + psfile);

def gsNotWorking():
    global gs_working 
    print "GhostScript not available for hp2ps tests"

global gs_working
gs_working = 0
if config.have_profiling:
  if config.gs != '':
    resultGood = runCmdExitCode(genGSCmd(config.confdir + '/good.ps'));
    if resultGood == 0:
        resultBad = runCmdExitCode(genGSCmd(config.confdir + '/bad.ps'));
        if resultBad != 0:
            print "GhostScript available for hp2ps tests"
            gs_working = 1;
        else:
            gsNotWorking();
    else:
        gsNotWorking();
  else:
    gsNotWorking();

def rm_no_fail( file ):
   try:
       os.remove( file )    
   finally:
       return

def add_suffix( name, suffix ):
    if suffix == '':
        return name
    else:
        return name + '.' + suffix    

def add_alan_suffix(name):
    return add_suffix(name, 'alan')

def in_testdir( name ):
    return (getTestOpts().testdir + '/' + name)

def qualify( name, suff ):
    return in_testdir(add_suffix(name, suff))


# Finding the sample output.  The filename is of the form
#
#   <test>.stdout[-<compiler>][-<version>][-ws-<wordsize>][-<platform>]
#
# and we pick the most specific version available.  The <version> is
# the major version of the compiler (e.g. 6.8.2 would be "6.8").  For
# more fine-grained control use if_compiler_lt().
#
def platform_wordsize_qualify( name, suff ):

    basepath = qualify(name, suff)

    fns = [ lambda x: x + '-' + config.compiler_type,
            lambda x: x + '-' + config.compiler_maj_version]

    paths = [ basepath ]
    for fn in fns:
        paths = paths + map(fn, paths)

    paths.reverse()

    plat_paths = map (lambda x: x + '-' + config.platform, paths)

    dir = glob.glob(basepath + '*')
    dir = map (lambda d: normalise_slashes_(d), dir)

    for f in plat_paths:
       if f in dir:
            return (1,f)

    for f in paths:
       if f in dir:
            return (0,f)

    return (0, basepath)

# Clean up prior to the test, so that we can't spuriously conclude
# that it passed on the basis of old run outputs.
def pretest_cleanup(name):
   rm_no_fail(qualify(name,'comp.stderr'))
   rm_no_fail(qualify(name,'run.stderr'))
   rm_no_fail(qualify(name,'run.stdout'))
   rm_no_fail(qualify(name,'tix'))	# remove the old tix file
   # simple_build zaps the following:
   # rm_nofail(qualify("o"))
   # rm_nofail(qualify(""))
   # not interested in the return code

# -----------------------------------------------------------------------------
# Return a list of all the files ending in '.T' below the directory dir.

def findTFiles(roots):
    return concat(map(findTFiles_,roots))

def findTFiles_(path):    
    if os.path.isdir(path):
        paths = map(lambda x, p=path: p + '/' + x, os.listdir(path))
        return findTFiles(paths)
    elif path[-2:] == '.T':
        return [path]
    else:
        return []

# -----------------------------------------------------------------------------
# Output a test summary to the specified file object

def summary(t, file):

    file.write('\n')
    file.write('OVERALL SUMMARY for test run started at ' \
               + t.start_time + '\n'\
               + string.rjust(`t.total_tests`, 8) \
               + ' total tests, which gave rise to\n' \
               + string.rjust(`t.total_test_cases`, 8) \
               + ' test cases, of which\n' \
               + string.rjust(`t.n_framework_failures`, 8) \
               + ' caused framework failures\n' \
               + string.rjust(`t.n_tests_skipped`, 8)
               + ' were skipped\n\n' \
               + string.rjust(`t.n_expected_passes`, 8)
               + ' expected passes\n' \
               + string.rjust(`t.n_expected_failures`, 8) \
               + ' expected failures\n' \
               + string.rjust(`t.n_unexpected_passes`, 8) \
               + ' unexpected passes\n'
               + string.rjust(`t.n_unexpected_failures`, 8) \
               + ' unexpected failures\n'
               + '\n')

    if t.n_unexpected_passes > 0:
        file.write('Unexpected passes:\n')
        keys = t.unexpected_passes.keys()
        keys.sort()
        for test in keys:
            file.write('   ' + test + '(' + \
                       join(t.unexpected_passes[test],',') + ')\n')
        file.write('\n')
            
    if t.n_unexpected_failures > 0:
        file.write('Unexpected failures:\n')
        keys = t.unexpected_failures.keys()
        keys.sort()
        for test in keys:
            file.write('   ' + test + '(' + \
                       join(t.unexpected_failures[test],',') + ')\n')
        file.write('\n')

def getStdout(cmd):
    if have_subprocess:
        p = subprocess.Popen(cmd,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        (stdout, stderr) = p.communicate()
        r = p.wait()
        if r != 0:
            raise Exception("Command failed: " + str(cmd))
        if stderr != '':
            raise Exception("stderr from command: " + str(cmd))
        return stdout
    else:
        raise Exception("Need subprocess to get stdout, but don't have it")

