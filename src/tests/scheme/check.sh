#!/bin/sh
# Run Scheme test suites of TeXmacs without a window, with an exit status.
#
# usage: tests/scheme/check.sh [suite...]
#
#   suite   glue (the default) runs the tests of the glue between C++ and
#           Scheme (TeXmacs/progs/check/glue-test.scm); all runs
#           run-all-tests, the regression tests of check-master.scm
#
# TeXmacs is run with TEXMACS_PATH set to the source tree and a scratch
# TEXMACS_HOME_PATH under tests/build/scheme. An error in a -x expression
# keeps TeXmacs from quitting, so the expressions catch every error and
# exit themselves, and a run which takes longer than TM_TEST_TIMEOUT
# seconds (default 600) is stopped and fails.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/scheme"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="$out/home"
mkdir -p "$TEXMACS_HOME_PATH"
timeout=${TM_TEST_TIMEOUT:-600}

[ $# -eq 0 ] && set -- glue
status=0
for suite in "$@"; do
  case $suite in
    glue) expr="(min 1 (glue-test-failures))" ;;
    all)  expr="(begin (run-all-tests) 0)" ;;
    *)    echo "$suite: unknown suite"; status=1; continue ;;
  esac
  log="$out/$suite.log"
  perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" \
    -x "(exit (catch #t (lambda () $expr)
                        (lambda args (display* \"error: \" args \"\\n\") 2)))" \
    -q > "$log" 2>&1
  code=$?
  grep -E '^ *(FAILED|Total|Test suite|error:|Regression failure)' "$log" \
    | sed 's/^/  /'
  case $code in
    0)   echo "$suite: ok" ;;
    1)   echo "$suite: FAILED (see $log)"; status=1 ;;
    142) echo "$suite: FAILED, stopped after $timeout s (see $log)"; status=1 ;;
    *)   echo "$suite: FAILED, exit status $code (see $log)"; status=1 ;;
  esac
done
exit $status
