#!/bin/sh
# Runs the TeXmacs regression suites headless and fails unless they pass.
# Usage: run-tests.sh <texmacs binary>   (from the top of the TeXmacs source
# tree, which contains TeXmacs/)
#
# Each suite runs in a TeXmacs of its own (tests/scheme/check.sh): with S7,
# the memory of TeXmacs grows from suite to suite and is never given back,
# so that all the suites in one TeXmacs took more than the 16 GB of the
# Linux runner, which GitHub then stopped (a shutdown signal, and no log).
# Without check.sh, they all run in one TeXmacs (run-tests.scm).

BIN=${1:-TeXmacs/bin/texmacs.bin}
TOP=$(pwd)
export QT_QPA_PLATFORM=offscreen
export TEXMACS_PATH="$TOP/TeXmacs"
# the programs of TeXmacs (fig2ps...), as the script texmacs puts them
export PATH="$TOP/TeXmacs/bin:$PATH"
export TEXMACS_HOME_PATH="${RUNNER_TEMP:-/tmp}/texmacs-home"
mkdir -p "$TEXMACS_HOME_PATH"

# On Linux, the memory of the TeXmacs which runs, every 15 seconds
monpid=""
memory_monitor () {
  [ -r /proc/meminfo ] || return 0
  ( while kill -0 $1 2> /dev/null; do
      sleep 15
      echo "memory: TeXmacs $(ps -C texmacs.bin -o rss= 2> /dev/null | tr -d ' ' | tr '\n' ' ')kB," \
           "available $(awk '/MemAvailable/ {print $2}' /proc/meminfo) kB"
    done ) &
  monpid=$!
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
  TM_TEST_HOME="$TEXMACS_HOME_PATH" TM_TEST_TIMEOUT=600 \
    sh tests/scheme/check.sh $SUITES > tests.log 2>&1 &
  pid=$!
  # the results as they come (the log is lost when the runner is stopped)
  tail --pid=$pid -n +1 -f tests.log 2> /dev/null &
  tailpid=$!
  memory_monitor $pid
  wait $pid
  status=$?
  sleep 1
  kill $tailpid $monpid 2> /dev/null
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
tail --pid=$pid -n +1 -f tests.log 2> /dev/null |
  grep --line-buffered "^Test suite of\|FAILED\|^Total:\|Throwing" &
tailpid=$!
memory_monitor $pid
wait $pid
status=$?
sleep 1
kill $tailpid $monpid 2> /dev/null
echo "texmacs exited with status $status"

grep -v "approximating font\|propagateSizeHints\|does not support" tests.log
grep -q "CI-TESTS-OK" tests.log
