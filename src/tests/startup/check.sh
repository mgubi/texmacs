#!/bin/sh
# Check the order in which TeXmacs processes its command line: the commands
# of -x, the files, -q and the automatic quit of the headless mode.
#
# usage: tests/startup/check.sh
#
# TeXmacs is run without a window (-headless, or the offscreen platform of
# Qt), with TEXMACS_PATH set to the source tree and a scratch
# TEXMACS_HOME_PATH (tests/build/startup/home, or TM_TEST_HOME). Each case
# is a command line and the lines which it must print, in this order; a
# case which takes longer than TM_TEST_TIMEOUT seconds (default 60) is
# stopped and fails. The cases replace quit-TeXmacs by a function which
# prints the names of the buffers before it exits, to see what was loaded
# when TeXmacs quits.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/startup"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
export QT_QPA_PLATFORM=offscreen
mkdir -p "$out" "$TEXMACS_HOME_PATH"
timeout=${TM_TEST_TIMEOUT:-60}

cat > "$out/startup-a.tm" <<'EOF'
<TeXmacs|2.1.4>

<style|generic>

<\body>
  A document of the startup checks.
</body>
EOF
cp "$out/startup-a.tm" "$out/startup-b.tm"

# the names of the buffers, without the scratch buffer of the first window
names='(list-filter (map (lambda (u) (url->string (url-tail u))) (buffer-list)) (lambda (s) (string-starts? s "startup-")))'
atquit="(set! quit-TeXmacs (lambda () (display* \"CHECK quit \" $names \"\\n\") (exit 0)))"
show () { echo "(display* \"CHECK $1 \" $names \"\\n\")"; }

status=0
total=0
failed=0
# check <name> <expected lines> <arguments of TeXmacs...>
check () {
  name=$1; expected=$2; shift 2
  total=$((total + 1))
  got=$(cd "$out" && perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" "$@" \
          2> "$out/$total.err" | grep -a '^CHECK ' | sed 's/^CHECK //')
  if [ "$got" = "$expected" ]; then
    echo "  ok     $name"
  else
    echo "  FAILED $name"
    echo "$expected" | sed 's/^/    expected: /'
    echo "$got" | sed 's/^/    got:      /'
    failed=$((failed + 1))
    status=1
  fi
}

check "headless: the files are loaded before the automatic quit" \
      "quit (startup-a.tm)" \
      -headless -x "$atquit" startup-a.tm

check "headless: the commands after -q are executed" \
      "one ()
two ()
quit ()" \
      -headless -x "$atquit" -x "$(show one)" -q -x "$(show two)"

check "-q quits after the files, wherever it is" \
      "quit (startup-b.tm startup-a.tm)" \
      -q -x "$atquit" startup-a.tm startup-b.tm

check "headless: -X does not quit" \
      "one ()
two ()" \
      -headless -X -x "$(show one)" -x "(delayed (delayed $(show two) (exit 0)))"

echo "Total: $total checks, $failed failed"
exit $status
