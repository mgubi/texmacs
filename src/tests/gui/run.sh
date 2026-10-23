#!/bin/sh
#
# Run the tests of the interface which run on every port:
#
#   tests/gui/run.sh [test...]
#
# from the top of the source tree of a build (Vue, Qt or Cocoa); all the
# tests of this directory when none is named. Each runs through
# tests/Plugins/Vue/run.sh (a home of its own, PASS or FAIL), without a
# script (they are steps of the driver in their .scm, and some have the
# name of a script of Vue); the last line counts the failures, and so does
# the status. SECS= gives the time of a test at most (40 s); the logs and
# snapshots go to OUTDIR/<test> (/tmp/gui-tests).

set -u
D=tests/gui
[ -d "$D" ] || { echo "run me from the top of the source tree"; exit 2; }
tests="$*"
[ -z "$tests" ] && tests=$(ls $D/*.scm | sed 's|.*/||; s|\.scm$||')
failed=0
for t in $tests; do
  r=$(SCM="$PWD/$D/$t.scm" SCRIPT="$PWD/$D/no-script" \
      OUT="${OUTDIR:-/tmp/gui-tests}/$t" \
      sh tests/Plugins/Vue/run.sh "$t" "${SECS:-40}" | grep -aE "^(PASS|FAIL) ")
  [ -n "$r" ] || r="FAIL $t: no result"
  echo "$r"
  case "$r" in FAIL*) failed=$((failed + 1)) ;; esac
done
echo "$failed failed"
[ "$failed" = 0 ]
