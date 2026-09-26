#!/bin/sh
#
# Run one scripted test of the Vue GUI:
#
#   src/Plugins/Vue/tests/run.sh <test> [seconds]
#
# from the top of the source tree. It loads <test>.scm and replays
# <test>.script, and prints what the test prints (choice:, got:, ...), the
# errors and the end of the script; the snapshots go to OUT (default
# /tmp/vue-tests/<test>). SCM= and SCRIPT= name another pair, for the tests
# which share one (SCM=wheel-travel.scm SCRIPT=scroll-shift.script).
#
# TeXmacs runs with a home of its own, a copy of ~/.TeXmacs made for the
# run (copy on write where the file system can): a test must not change
# the preferences, the recent files or anything else of the user's -- a
# command like change-zoom-factor saves the zoom as a preference, and
# load-buffer adds to the recent files. The copy is removed afterwards
# (KEEP=1 keeps it).

set -u
T=src/Plugins/Vue/tests
test=${1:?usage: run.sh <test> [seconds]}
secs=${2:-25}
OUT=${OUT:-/tmp/vue-tests/$test}
SCM=${SCM:-$test.scm}
SCRIPT=${SCRIPT:-$test.script}
BIN=TeXmacs/bin/texmacs.bin
[ -x "$BIN" ] || { echo "run me from the top of the source tree"; exit 2; }

rm -rf "${OUT:?}"; mkdir -p "$OUT"
HOMEDIR="$OUT/home"
if [ -d "$HOME/.TeXmacs" ]; then
  cp -Rc "$HOME/.TeXmacs" "$HOMEDIR" 2>/dev/null || cp -R "$HOME/.TeXmacs" "$HOMEDIR" || {
    echo "cannot make a home for TeXmacs in $HOMEDIR"; exit 2; }
else mkdir -p "$HOMEDIR"; fi

load=""
[ -f "$T/$SCM" ] && load="(load \"$T/$SCM\")"
script=""
[ -f "$T/$SCRIPT" ] && script="$PWD/$T/$SCRIPT"

TEXMACS_PATH="$PWD/TeXmacs" TEXMACS_HOME_PATH="$HOMEDIR" \
TEXMACS_VUE_SNAPSHOT="$OUT" TEXMACS_VUE_SCRIPT="$script" \
  "$BIN" ${load:+-x "$load"} > "$OUT/run.log" 2>&1 &
p=$!
sleep "$secs"
kill -9 $p 2>/dev/null
wait $p 2>/dev/null

grep -E "choice:|got:|enum:|answer:|Error message|Invalid situation|vue script: done" "$OUT/run.log"
ls "$OUT"/*.png 2>/dev/null | sed "s|^|snapshot: |"
[ "${KEEP:-0}" = 1 ] || rm -rf "${HOMEDIR:?}"
