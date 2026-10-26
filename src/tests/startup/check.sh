#!/bin/sh
# Check the order in which TeXmacs processes its command line: the commands
# of -x and the files in their order, -q and the automatic quit of the
# headless mode after them, and the exit of TeXmacs after a script.
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
# when TeXmacs quits. A run which does not exit with status 0 (a crash at
# the exit) prints the line "exit status <n>" after the expected ones.

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
  (cd "$out" && perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" "$@" \
     > "$out/$total.out" 2> "$out/$total.err")
  code=$?
  got=$(grep -a '^CHECK ' "$out/$total.out" | sed 's/^CHECK //')
  [ $code -eq 0 ] || got=$(printf '%s\nexit status %s' "$got" "$code")
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

check "headless: -test-suite without its argument quits" \
      "quit ()" \
      -headless -x "$atquit" -test-suite

check "-q quits after the files, wherever it is" \
      "quit (startup-b.tm startup-a.tm)" \
      -q -x "$atquit" startup-a.tm startup-b.tm

check "the commands and the files in the order of the command line" \
      "one ()
two (startup-a.tm)
three (startup-b.tm startup-a.tm)
quit (startup-b.tm startup-a.tm)" \
      -headless -x "$atquit" -x "$(show one)" startup-a.tm -x "$(show two)" \
      startup-b.tm -x "$(show three)"

check "a command after a file has it as the current buffer" \
      "current startup-a.tm" \
      -headless startup-a.tm \
      -x '(display* "CHECK current " (url->string (url-tail (current-buffer))) "\n")'

check "headless: -X does not quit" \
      "one ()
two ()" \
      -headless -X -x "$(show one)" -x "(delayed (delayed $(show two) (exit 0)))"

# A script which leaves a buffer shown again with its cursor in a generated
# bibliography: its focus toolbar has an input field with a completer, and
# the toolbar it had before is deleted later, by the event loop of Qt, which
# a script given by -x runs before. The deletion of that toolbar at the exit
# crashed in QCompletionModel::filter (#462); the exit deletes nothing now.
cat > "$out/startup-refs.bib" <<'EOF'
@article{knuth, author={Knuth, Donald E.}, title={The Art}, journal={J}, year=1968}
EOF
cat > "$out/startup-bib.tm" <<'EOF'
<TeXmacs|2.1.4>

<style|generic>

<\body>
  See <cite|knuth>.

  <\bibliography|bib|tm-plain|startup-refs>
  </bibliography>
</body>
EOF
cat > "$out/startup-bib.scm" <<'EOF'
(let ((u (url-append (url-pwd) "startup-bib.tm"))
      (o (url-append (url-pwd) "startup-a.tm")))
  (load-buffer u)
  (switch-to-buffer* u)
  (update-forced)
  (generate-all-aux)
  (update-forced)
  (with l (select (buffer-tree) '(:* bib-list))
    (when (pair? l) (tree-go-to (car l) 1 :start)))
  (update-forced)
  (display* "CHECK in " (if (tree-innermost 'bib-list) "bib-list" "?") "\n")
  (load-buffer o)
  (switch-to-buffer* o)
  (update-forced)
  (switch-to-buffer* u)
  (update-forced)
  (buffer-pretend-saved u)
  (buffer-close o)
  (buffer-close u))
EOF

check "a script which leaves a focus toolbar with a completer exits (#462)" \
      "in bib-list
quit ()" \
      -x "$atquit" -x '(load "startup-bib.scm")' -q

echo "Total: $total checks, $failed failed"
exit $status
