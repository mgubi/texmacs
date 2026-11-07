#!/bin/bash
# Test the shared files (src/docs/shared-files.md): TeXmacs processes of one
# user which open the same file stay in sync, with no server.
#
# usage: tests/shared/check.sh [scenario...]
#
# Each scenario (s-<name>.scm) is run by two TeXmacs processes at once, A and
# B, headless (-headless -X) on the offscreen platform, which meet through
# marker files (see lib.scm). They share a scratch TMPDIR, so that the per-user
# folder of the entries is a scratch one, and each has a home of its own, with
# the preference "share-open-files" on (off for the scenario "off").
#
#   basic       join, edits both ways, a save asked by the participant
#   concurrent  edits at once in different paragraphs: rebased by the host
#   refuse      the host deletes what the participant edits: refused
#   handover    the host closes the file: the participant takes over
#   death       the host is killed: the participant takes over its changes
#   takeover    an interactive process joins a headless host and takes over
#   takeover-window  the same with a window (offscreen) for B
#   off         the preference off: nothing is shared
#   stale       an entry of a dead process; a wrong token; garbage
#   meta        style and initial environment shared as the body is
#   conflict    edits at once: merged, or the person's text saved when lost
#   save        Save and close in a participant; a buffer removed by C++
#   hostage     a stalled new host is not replaced by the old one
#   corrupt     an entry is not trusted (empty, unserved); case of names
#   late        a target that stalls gets the lock later: one host at a time
#   lockheld    a lock held by a process that is not TeXmacs: nobody hosts
#   lockdel     the lock file removed under a host
#   race        a joiner waits for the seeking participants of a gone host
#   seeksave    Save with no host is queued
#   closeunconn a participant closed with no host saves its whole document
#   lockerr     the lock can not be made again: the host steps down
#   diffdir     one folder whatever TMPDIR and XDG_RUNTIME_DIR
#   dupe        a patch sent twice; a replaced document is saved whole
#   slow        a host that is alive but stalled is not replaced
#   attack      oversize, bad and endless packets before the token
#   agent       the host applies a mode (propose, direct, read-only) and the content
#               rules to a participant that is an agent; proposals accepted, rejected, withdrawn
#   agentplain  a participant that is a person (marked so by the host) is not filtered
#   agentmode   the mode chosen by the person is kept when the file is handed over and comes back
#   fuzz        random patches with hostile integers through the patch functions: no crash
#   edfuzz      random patches applied to a real buffer (editor and typesetter): no crash
#   malformed   patches and messages of a wrong shape are refused and the host goes on
#
# TM_SHARED_PREF=off runs them with the preference off (they must then fail,
# except "off"). Every process is stopped above TM_TEST_RSS_KB (default 2 GB) of memory and
# after TM_TEST_TIMEOUT seconds (default 120); the home is TM_TEST_HOME (not
# under /tmp, which is a link on macOS) or tests/build/shared.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
base="${TM_TEST_HOME:-$top/tests/build/shared}"
timeout=${TM_TEST_TIMEOUT:-120}
rsscap=${TM_TEST_RSS_KB:-2000000}
run="$base/run-$$"
mkdir -p "$run" || exit 1
export TEXMACS_PATH="$top/TeXmacs"
export QT_QPA_PLATFORM=offscreen
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
unset XDG_RUNTIME_DIR TEXMACS_SHARED_DIR

[ $# -eq 0 ] && set -- basic concurrent conflict meta refuse save handover death takeover takeover-window off stale slow attack hostage corrupt late lockheld lockdel race seeksave diffdir dupe closeunconn lockerr agent agentplain malformed agentmode fuzz edfuzz
status=0

# wait for the processes (pids), stopping those which use too much memory or
# run for too long
watch() {
  local start=$(date +%s) alive p rss
  while true; do
    alive=0
    for p in "$@"; do
      if kill -0 "$p" 2>/dev/null; then
        alive=1
        rss=$(ps -o rss= -p "$p" 2>/dev/null | tr -d ' ')
        if [ -n "$rss" ] && [ "$rss" -gt "$rsscap" ]; then
          echo "  process $p stopped: $rss KB" >&2; kill "$p" 2>/dev/null
        fi
      fi
    done
    [ $alive -eq 0 ] && break
    if [ $(( $(date +%s) - start )) -gt "$timeout" ]; then
      echo "  timeout: stopping $*" >&2; kill "$@" 2>/dev/null; break
    fi
    sleep 0.3
  done
}

for name in "$@"; do
  scen=$name; pref=on; bflags="-headless -X"; dead=""
  case $name in
    takeover-window) scen=takeover; bflags="" ;;
    off)             pref=off ;;
    death|seeksave|closeunconn)  dead=A ;;
  esac
  pref=${TM_SHARED_PREF:-$pref}
  w="$run/$name"
  mkdir -p "$w/sync" "$w/tmp" "$w/homeA/system" "$w/homeB/system"
  for h in homeA homeB; do
    printf '("share-open-files" "%s")\n' "$pref" > "$w/$h/system/preferences.scm"
  done
  cp "$here/fixture.tm" "$w/f.tm"
  pids=""
  for role in A B; do
    flags="-headless -X"
    [ $role = B ] && flags="$bflags"
    extra=""
    [ $name = takeover-window ] && [ $role = B ] && extra="SH_WINDOW=1"
    # (the folder of the entries and locks is /tmp/texmacs-<uid>/shared for every
    # process; TEXMACS_SHARED_DIR gives the tests a folder of their own, except
    # in diffdir, where the processes differ by TMPDIR and XDG_RUNTIME_DIR)
    shdir="TEXMACS_SHARED_DIR=$w/tmp"; tmpd="$w/tmp"
    if [ $name = diffdir ]; then
      mkdir -p "$w/xdg$role" "$w/tmp$role"; shdir="XDG_RUNTIME_DIR=$w/xdg$role"; tmpd="$w/tmp$role"
    fi
    env $shdir TMPDIR="$tmpd" TEXMACS_HOME_PATH="$w/home$role" SH_ROLE=$role \
        SH_SYNC="$w/sync" SH_FILE="$w/f.tm" SH_OUT="$w/out-$role" \
        SH_DIR="$here" $extra \
        "$bin" $flags -x "(load \"$here/s-$scen.scm\")" \
        > "$w/log-$role" 2>&1 &
    pids="$pids $!"
    [ $role = A ] && sleep 0.2
  done
  watch $pids
  wait 2>/dev/null
  chmod -R u+rwx "$w" 2>/dev/null   # (a killed scenario may leave a folder closed)
  fails=0
  for role in A B; do
    if [ "$role" = "$dead" ]; then continue; fi
    if [ "$(cat "$w/out-$role" 2>/dev/null)" != ok ]; then fails=$((fails+1)); fi
    grep -E "CHECK .* FAIL|ERROR|shared file error" "$w/log-$role" | sed "s/^/    /"
  done
  if [ -f "$w/sync/A-body" ] && [ -f "$w/sync/B-body" ]; then
    if ! cmp -s "$w/sync/A-body" "$w/sync/B-body"; then
      echo "    the documents of A and B differ"; fails=$((fails+1))
    fi
  fi
  if [ $fails -eq 0 ]; then
    n=$(cat "$w"/log-* | grep -c "^CHECK .* PASS")
    echo "PASS $name ($n checks)"
  else
    echo "FAIL $name (logs in $w)"; status=1
  fi
done

# With the preference off nothing is made: no folder of entries
if [ -d "$run/off/tmp" ] && ls "$run"/off/tmp/texmacs-* >/dev/null 2>&1; then
  echo "FAIL off: a folder of entries was made"; status=1
fi

# The per-user folder of the entries is private (mode 0700)
for d in "$run"/*/tmp/texmacs-*/shared; do
  [ -d "$d" ] || continue
  mode=$(stat -f %Lp "$d" 2>/dev/null || stat -c %a "$d" 2>/dev/null)
  if [ "$mode" != 700 ]; then echo "FAIL folder $d has mode $mode"; status=1; fi
  break
done
exit $status
