#!/bin/sh
# Runs the TeXmacs regression suites headless and fails unless they pass.
# Usage: run-tests.sh <texmacs binary>   (from the top of the TeXmacs source
# tree, which contains TeXmacs/)

BIN=${1:-TeXmacs/bin/texmacs.bin}
TOP=$(pwd)
export QT_QPA_PLATFORM=offscreen
export TEXMACS_PATH="$TOP/TeXmacs"
export TEXMACS_HOME_PATH="${RUNNER_TEMP:-/tmp}/texmacs-home"
mkdir -p "$TEXMACS_HOME_PATH"

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
# the suites as they run (the log is lost when the runner itself is
# stopped), and on Linux the memory of TeXmacs every 15 seconds
tail --pid=$pid -n +1 -f tests.log 2> /dev/null |
  grep --line-buffered "^Test suite of\|FAILED\|^Total:\|Throwing" &
tailpid=$!
monpid=""
if [ -r /proc/meminfo ]; then
  ( while kill -0 $pid 2> /dev/null; do
      sleep 15
      echo "memory: TeXmacs $(ps -o rss= -p $pid 2> /dev/null) kB," \
           "available $(awk '/MemAvailable/ {print $2}' /proc/meminfo) kB"
    done ) &
  monpid=$!
fi
wait $pid
status=$?
sleep 1
kill $tailpid $monpid 2> /dev/null
echo "texmacs exited with status $status"

grep -v "approximating font\|propagateSizeHints\|does not support" tests.log
grep -q "CI-TESTS-OK" tests.log
