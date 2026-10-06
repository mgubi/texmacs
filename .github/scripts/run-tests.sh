#!/bin/sh
# Runs the TeXmacs regression suites headless and fails unless they pass.
# Usage: run-tests.sh <texmacs binary>   (from the top of the TeXmacs source
# tree, which contains TeXmacs/)
#
# Each suite runs in a TeXmacs of its own (tests/scheme/check.sh): a crash
# or a timeout loses only that suite, and each suite starts from a fresh
# TeXmacs. (This began as a workaround: with S7, all the suites in one
# TeXmacs once took more than the 16 GB of the Linux runner, a heap forced
# to grow by the macro cache of s7; since that fix, they take about 1 GB.)
# Without check.sh, they all run in one TeXmacs (run-tests.scm).

BIN=${1:-TeXmacs/bin/texmacs.bin}
TOP=$(pwd)
export QT_QPA_PLATFORM=offscreen
export TEXMACS_PATH="$TOP/TeXmacs"
# the programs of TeXmacs (fig2ps...), as the script texmacs puts them
export PATH="$TOP/TeXmacs/bin:$PATH"
export TEXMACS_HOME_PATH="${RUNNER_TEMP:-/tmp}/texmacs-home"
mkdir -p "$TEXMACS_HOME_PATH"

# On Linux, the memory of the TeXmacs which runs, every 15 seconds (MSYS2
# has a /proc/meminfo too, but neither ps -C nor MemAvailable)
monpid=""
memory_monitor () {
  [ "$(uname -s)" = Linux ] && [ -r /proc/meminfo ] || return 0
  ( while kill -0 $1 2> /dev/null; do
      sleep 15
      echo "memory: TeXmacs $(ps -C texmacs.bin -o rss= 2> /dev/null | tr -d ' ' | tr '\n' ' ')kB," \
           "available $(awk '/MemAvailable/ {print $2}' /proc/meminfo) kB"
    done ) &
  monpid=$!
}

# tests.log as it grows, while the process $1 runs, then the rest: the
# results as they come (the log is lost when the runner is stopped). Plain
# sh, where tail --pid is GNU only (macOS printed nothing)
follow_log () {
  n=0
  while kill -0 $1 2> /dev/null; do
    m=$(wc -l < tests.log)
    if [ $m -gt $n ]; then sed -n "$((n + 1)),${m}p" tests.log; n=$m; fi
    sleep 2
  done
  sed -n "$((n + 1)),\$p" tests.log
  # (a log which does not end a line: what follows on a line of its own)
  [ -z "$(tail -c 1 tests.log)" ] || echo
}

if [ -f tests/scheme/check.sh ]; then
  # the suites of run-all-tests: the names in regression-suites (or those
  # of TM_CI_SUITES)
  SUITES=${TM_CI_SUITES:-$(awk '/^\(define regression-suites/ {on=1}
                on && /^\(define [^r]|^\(tm-define/ && !/regression-suites/ {on=0}
                on' TeXmacs/progs/check/check-master.scm |
           grep -oE "^[ ']*\(*\(\"[a-z0-9-]+\" [a-z]" |
           sed -E 's/.*"([^"]+)".*/\1/')}
  echo "suites: $(echo $SUITES | wc -w)"
  # (300 s each: the 43 suites stay within the 60 minutes of the job)
  TM_TEST_HOME="$TEXMACS_HOME_PATH" TM_TEST_TIMEOUT=300 \
    sh tests/scheme/check.sh $SUITES > tests.log 2>&1 &
  pid=$!
  memory_monitor $pid
  follow_log $pid
  wait $pid
  status=$?
  kill $monpid 2> /dev/null
  echo "check.sh exited with status $status"
  exit $status
fi

# On Windows (MSYS2), TeXmacs needs a native path in the Scheme string;
# MSYS2 converts the environment variables above, but not this string
SCRIPT="$(cd "$(dirname "$0")" && pwd)/run-tests.scm"
if command -v cygpath > /dev/null 2>&1; then SCRIPT=$(cygpath -m "$SCRIPT"); fi

# A script that fails to load must not leave TeXmacs waiting
CMD="(catch #t (lambda () (load \"$SCRIPT\"))
  (lambda args (display* \"CI-TESTS-FAILED: \" args \"\\n\") (quit-TeXmacs)))"

# perl's alarm gives a portable timeout (GNU timeout is missing on macOS)
perl -e 'alarm shift; exec @ARGV' 600 "$BIN" -x "$CMD" > tests.log 2>&1 &
pid=$!
memory_monitor $pid
follow_log $pid | grep --line-buffered "^Test suite of\|FAILED\|^Total:\|Throwing"
wait $pid
status=$?
kill $monpid 2> /dev/null
echo "texmacs exited with status $status"

grep -v "approximating font\|propagateSizeHints\|does not support" tests.log
grep -q "CI-TESTS-OK" tests.log
