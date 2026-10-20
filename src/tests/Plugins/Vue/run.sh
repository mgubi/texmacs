#!/bin/sh
#
# Run one scripted test of the Vue GUI (or of another port, with BIN= the
# texmacs.bin of its build, run from the top of its source tree):
#
#   tests/Plugins/Vue/run.sh <test> [seconds]
#
# from the top of the source tree. It loads <test>.scm and replays
# <test>.script, and prints what the test prints (choice:, got:, ...), the
# errors and the end of the script; the snapshots go to OUT (default
# /tmp/vue-tests/<test>). SCM= and SCRIPT= name another pair, for the tests
# which share one (SCM=wheel-travel.scm SCRIPT=scroll-shift.script), in the
# directory of the tests or by an absolute path.
#
# The last line says PASS or FAIL, and so does the status: a test fails
# when TeXmacs crashed, when its script did not end, when a check of the
# script failed, or when a line which an "expect" line of the script asks
# for is missing from the output. A script with "expect" lines stops as
# soon as it ended and they all showed; the others run for the given time
# (docs/gui-scripting.md).
#
# TeXmacs runs with a home of its own, a copy of ~/.TeXmacs_Upsilon made for the
# run (copy on write where the file system can): a test must not change
# the preferences, the recent files or anything else of the user's -- a
# command like change-zoom-factor saves the zoom as a preference, and
# load-buffer adds to the recent files. The copy is removed afterwards
# (KEEP=1 keeps it).

set -u
T=tests/Plugins/Vue
test=${1:?usage: run.sh <test> [seconds]}
secs=${2:-25}
OUT=${OUT:-/tmp/vue-tests/$test}
SCM=${SCM:-$test.scm}
SCRIPT=${SCRIPT:-$test.script}
BIN=${BIN:-TeXmacs/bin/texmacs.bin} # BIN=<path>: another build (CMake)
# TM_ARGS: options of TeXmacs (e.g. -tls-no-verify)
[ -x "$BIN" ] || { echo "run me from the top of the source tree"; exit 2; }

rm -rf "${OUT:?}"; mkdir -p "$OUT"
HOMEDIR="$OUT/home"
if [ -d "$HOME/.TeXmacs_Upsilon" ]; then
  cp -Rc "$HOME/.TeXmacs_Upsilon" "$HOMEDIR" 2>/dev/null ||
  { rm -rf "${HOMEDIR:?}"; cp -R "$HOME/.TeXmacs_Upsilon" "$HOMEDIR"; } || {
    echo "cannot make a home for TeXmacs in $HOMEDIR"; exit 2; }
else mkdir -p "$HOMEDIR"; fi

# SCM and SCRIPT are in the directory of the tests, or absolute paths
case "$SCM" in /*) ;; *) SCM="$PWD/$T/$SCM" ;; esac
case "$SCRIPT" in /*) ;; *) SCRIPT="$PWD/$T/$SCRIPT" ;; esac
load=""
[ -f "$SCM" ] && load="(load \"$SCM\")"
# a test made of steps of the driver in its .scm (gui-test-run) must reach
# their end as a script must
driver=0
[ -f "$SCM" ] && grep -q "gui-test-run" "$SCM" && driver=1
script=""
[ -f "$SCRIPT" ] && script="$SCRIPT"

TEXMACS_PATH="$PWD/TeXmacs" TEXMACS_HOME_PATH="$HOMEDIR" \
TEXMACS_VUE_SNAPSHOT="$OUT" TEXMACS_VUE_SCRIPT="$script" \
TEXMACS_TEST_SNAPSHOT="$OUT" TEXMACS_TEST_SCRIPT="$script" \
  "$BIN" ${TM_ARGS:-} ${load:+-x "$load"} > "$OUT/run.log" 2>&1 &
p=$!

# the lines the output must have (expect <text>), and whether they are all
# there; the lines of the script itself (vue script: ..., qt script: ...)
# do not count
expects="$OUT/expects"
: > "$expects"
[ -n "$script" ] && tr -d '\r' < "$script" |
  sed -n 's/^[[:space:]]*expect[[:space:]][[:space:]]*//p' > "$expects"
missing () {
  # (on another port than that of a "port" line, the lines after it are
  # skipped: their expect lines too)
  if [ -n "$script" ] && grep -aq "script: skipped: for the port" "$OUT/run.log"
  then tr -d '\r' < "$script" |
    sed -n -e '/^[[:space:]]*port[[:space:]]/q' \
           -e 's/^[[:space:]]*expect[[:space:]][[:space:]]*//p' > "$expects"
  fi
  grep -avE "^[a-z]+ script:" "$OUT/run.log" > "$OUT/output"
  while IFS= read -r e; do
    grep -qaF -- "$e" "$OUT/output" || printf '%s\n' "$e"
  done < "$expects"
}

ended=0 # TeXmacs ended by itself
t=0
while [ $t -lt $((secs * 2)) ]; do
  sleep 0.5
  t=$((t + 1))
  if ! kill -0 $p 2>/dev/null; then ended=1; break; fi
  if [ -s "$expects" ] && grep -aqE "^[a-z]+ script: done" "$OUT/run.log" &&
     [ -z "$(missing)" ]; then sleep 1; break; fi # (the last snapshot)
done
status=0
if [ $ended = 1 ]; then wait $p 2>/dev/null; status=$?
else kill -9 $p 2>/dev/null; wait $p 2>/dev/null; fi

grep -avE "^[a-z]+ script: expect" "$OUT/run.log" |
  grep -aE "choice:|got:|enum:|answer:|Error message|Invalid situation|FAILED|^[a-z]+ script: done"
ls "$OUT"/*.png 2>/dev/null | sed "s|^|snapshot: |"
[ "${KEEP:-0}" = 1 ] || rm -rf "${HOMEDIR:?}"

why=""
grep -aq "Error message" "$OUT/run.log" && why="$why; crashed"
{ [ -n "$script" ] || [ $driver = 1 ]; } &&
  ! grep -aqE "^[a-z]+ script: done" "$OUT/run.log" &&
  why="$why; the script did not end"
grep -aq "FAILED \[" "$OUT/run.log" && why="$why; a check failed"
[ $ended = 1 ] && [ $status != 0 ] && why="$why; exit status $status"
m=$(missing)
[ -n "$m" ] && why="$why; missing: $(printf '%s' "$m" | tr '\n' '|')"
if [ -z "$why" ]; then echo "PASS $test"; exit 0; fi
echo "FAIL $test:${why#;}"
exit 1
