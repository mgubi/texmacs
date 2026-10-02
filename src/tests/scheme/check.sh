#!/bin/sh
# Run the Scheme test suites of TeXmacs without a window, with an exit status.
#
# usage: tests/scheme/check.sh [suite...]
#
#   all          the regression suites of check-master.scm (the default)
#   integration  the integration suites (server backup, cache...)
#   <name>       one suite of check-master.scm, such as glue or tmhtml
#   <file.scm>   a test module which is not listed yet: foo-test.scm is
#                loaded and its suite foo-test-failures is run
#
# TeXmacs is run with TEXMACS_PATH set to the source tree and a scratch
# TEXMACS_HOME_PATH (tests/build/scheme/home, or TM_TEST_HOME). An error in
# a -x expression keeps TeXmacs from quitting, so the expression catches
# every error and exits itself, and a run which takes longer than
# TM_TEST_TIMEOUT seconds (default 600) is stopped and fails.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/scheme"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
mkdir -p "$out" "$TEXMACS_HOME_PATH"
timeout=${TM_TEST_TIMEOUT:-600}

[ $# -eq 0 ] && set -- all
status=0
for suite in "$@"; do
  case $suite in
    all)         expr="(run-all-tests)"; name=all ;;
    integration) expr="(run-integration-tests)"; name=integration ;;
    *.scm)
      file=$(cd "$(dirname "$suite")" && pwd)/$(basename "$suite")
      name=$(basename "$suite" .scm)
      expr="(begin (load \"$file\") ($name-failures))" ;;
    *)           expr="(run-regression-suite \"$suite\")"; name=$suite ;;
  esac
  log="$out/$name.log"
  perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" \
    -x "(exit (catch #t (lambda () (min 1 $expr))
                        (lambda args (display* \"error: \" args \"\\n\") 2)))" \
    -q > "$log" 2>&1
  code=$?
  grep -E '^ *(FAILED|FAIL |Total|Test suite|Suites:|error|Regression failure)' \
    "$log" | sed 's/^/  /'
  case $code in
    0)   echo "$name: ok" ;;
    1)   echo "$name: FAILED (see $log)"; status=1 ;;
    142) echo "$name: FAILED, stopped after $timeout s (see $log)"; status=1 ;;
    *)   echo "$name: FAILED, exit status $code (see $log)"; status=1 ;;
  esac
done
exit $status
