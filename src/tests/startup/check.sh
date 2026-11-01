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
# stopped and fails. The cases add to the exit of TeXmacs (on-exit) the
# printing of the names of the buffers, to see what was loaded when TeXmacs
# quits. A run which does not exit with status 0 (a crash at the exit)
# prints the line "exit status <n>" after the expected ones.
#
# A warning "conditional master routine" in a run also fails the case: a
# module which adds a variant to a routine must load the module of its
# master routine first, else the master replaces the variant when it comes
# (#684). (Printed after the expected lines, like the exit status.)
#
# The harness must give the same output under Guile and s7: the lists of
# strings are printed as one string, (a b), since display* prints the
# quotes of the strings in a list under s7; the exit is hooked by on-exit,
# not by redefining quit-TeXmacs, since the automatic quit of -q and of the
# headless mode is a closure made before the commands of -x run, and under
# s7 it keeps calling the original quit-TeXmacs (#562).

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
# (printed as one string, the same under Guile and s7)
names='(string-append "(" (string-recompose (list-filter (map (lambda (u) (url->string (url-tail u))) (buffer-list)) (lambda (s) (string-starts? s "startup-"))) " ") ")")'
atquit="(on-exit (display* \"CHECK quit \" $names \"\\n\"))"
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
  warned=$(grep -ah 'conditional master routine' "$out/$total.out" "$out/$total.err")
  [ -z "$warned" ] || got=$(printf '%s\n%s' "$got" "$warned")
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

# (the modules of the modes are loaded as needed: another order of loading)
cat > "$out/startup-math.tm" <<'EOF'
<TeXmacs|2.1.4>

<style|generic>

<\body>
  <math|x+y>
</body>
EOF
printf 'int f () {\n  return 1;\n}\n' > "$out/startup-code.cpp"

check "a file of program code, with no warning at the start (#684)" \
      "quit (startup-code.cpp)" \
      -q -x "$atquit" startup-code.cpp

check "headless: a file of program code, with no warning at the start" \
      "quit (startup-code.cpp)" \
      -headless -x "$atquit" startup-code.cpp

printf '{"a": true}\n' > "$out/startup-data.json"

check "a JSON file, with no warning at the start" \
      "quit (startup-data.json)" \
      -q -x "$atquit" startup-data.json

check "a document with math, with no warning at the start" \
      "quit (startup-math.tm)" \
      -q -x "$atquit" startup-math.tm

# A script which leaves a buffer shown again with its cursor in a generated
# bibliography: its focus toolbar has an input field with a completer, and
# the toolbar it had before is deleted later, by the event loop of Qt, which
# a script given by -x runs before. The deletion of that toolbar at the exit
# crashed in QCompletionModel::filter (#462); the exit deletes nothing now.
# The case also checks that the focus toolbar has a field with a completer:
# the label of the bibliography item, with its proposal.
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
;; the proposals of the input fields of the focus toolbar which get a
;; completer (qt_input_text_widget_rep::as_qwidget): not a password, and
;; proposals other than none or one empty string
(define (completer-proposals x)
  (cond ((and (pair? x) (== (car x) 'input) (>= (length x) 4))
         (let ((type (caddr x))
               (p (cadddr x)))
           (cond ((== type "password") '())
                 ((and (string? p) (!= p "")) (list p))
                 ((and (list? p) (pair? p) (!= p '(""))) p)
                 (else '()))))
        ((pair? x) (append-map completer-proposals x))
        (else '())))
(define (focus-completers)
  (completer-proposals (menu-expand '(link texmacs-focus-icons))))
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
  (display* "CHECK completer (" (string-recompose (focus-completers) " ") ")\n")
  (buffer-pretend-saved u)
  (buffer-close o)
  (buffer-close u))
EOF

check "a script which leaves a focus toolbar with a completer exits (#462)" \
      "in bib-list
completer (bib-knuth)
quit ()" \
      -x "$atquit" -x '(load "startup-bib.scm")' -q

# An application started from the Finder or the Dock has only the PATH of
# launchd, without /usr/local/bin or /opt/homebrew/bin: TeXmacs adds the
# PATH of a login shell, $SHELL -l, so that Ghostscript is found. The Vue
# build did not, and File > Preview made no PDF (#546). The login shells
# are scripts: one prints a banner and an error before its PATH (only the
# PATH is taken), one prints a list separated by spaces (not taken), and
# one hangs (stopped after 5 seconds, its PATH not taken).
if [ "$(uname)" = Darwin ]; then
  cat > "$out/login-shell" <<'EOF2'
#!/bin/sh
echo "Last login: today"
echo "zsh: command not found: pyenv" >&2
echo /tm-startup-login/bin:/tm-startup-login/sbin
EOF2
  cat > "$out/login-list" <<'EOF2'
#!/bin/sh
echo /tm-startup-login/bin /tm-startup-login/sbin
EOF2
  cat > "$out/login-hang" <<'EOF2'
#!/bin/sh
/bin/sleep 30
echo /tm-startup-login/bin
EOF2
  chmod +x "$out/login-shell" "$out/login-list" "$out/login-hang"
  inpath='(display* "CHECK login path " (cond ((string-contains? (getenv "PATH") ":/tm-startup-login/bin:/tm-startup-login/sbin:") "yes") ((string-contains? (getenv "PATH") "tm-startup-login") "garbled") ((string-contains? (getenv "PATH") "Last login") "banner") (else "no")) "\n")'
  old_shell=$SHELL old_path=$PATH
  export PATH=/usr/bin:/bin:/usr/sbin:/sbin
  export SHELL="$out/login-shell"
  check "macOS: the PATH of a login shell is added, without its messages (#546)" \
        "login path yes" \
        -headless -x "$inpath"
  export SHELL="$out/login-list"
  check "macOS: a PATH separated by spaces is not taken" \
        "login path no" \
        -headless -x "$inpath"
  export SHELL="$out/login-hang"
  start=$(date +%s)
  check "macOS: a login shell which hangs is stopped" \
        "login path no" \
        -headless -x "$inpath"
  took=$(( $(date +%s) - start ))
  if [ $took -ge 20 ]; then
    echo "  FAILED a login shell which hangs delays the start by ${took}s"
    failed=$((failed + 1)); status=1
  fi
  export SHELL="$old_shell" PATH="$old_path"
fi

# The clipboard commands under -headless, which runs a QCoreApplication: the
# clipboard of Qt has no platform there, and copying crashed (#708). The
# selections are kept by TeXmacs itself: a copy, also exported to LaTeX, is
# read back (also by the key extern) and pasted, a cleared selection is
# empty, and no image is reported as put on the clipboard.
cat > "$out/startup-clip.scm" <<'EOF'
(define (copies)
  ;; the number of copies of the sentence of startup-a.tm in the buffer
  (with s (object->string (tree->stree (buffer-tree)))
    (let loop ((pos 0) (n 0))
      (with i (string-search-forwards "startup checks" pos s)
        (if (< i 0) n (loop (+ i 1) (+ n 1)))))))
(define (clipboard-kept? which)
  (string-contains? (object->string (tree->stree (clipboard-get which)))
                    "startup checks"))
(let ((u (url-append (url-pwd) "startup-a.tm")))
  (load-buffer u)
  (switch-to-buffer* u)
  (update-forced)
  (select-all)
  (clipboard-copy "primary")
  (display* "CHECK copy " (clipboard-kept? "primary") "\n")
  ;; "extern" is "primary", as with a window (ai-paste reads it)
  (display* "CHECK extern " (clipboard-kept? "extern") "\n")
  (clipboard-copy-export "latex" "primary")
  (display* "CHECK latex " (clipboard-kept? "primary") "\n")
  (go-end)
  (clipboard-paste "primary")
  (update-forced)
  (display* "CHECK paste " (copies) "\n")
  (clipboard-clear "primary")
  (display* "CHECK clear " (tree->stree (clipboard-get "primary")) "\n")
  ;; no image is put on a clipboard which is not there
  (display* "CHECK graphics "
            (graphics-file-to-clipboard
             (system->url (string-append (getenv "TEXMACS_PATH")
                                         "/misc/images/fancy-c.png")))
            "\n")
  (buffer-pretend-saved u)
  (buffer-close u))
EOF

check "headless: the clipboard commands keep the selection (#708)" \
      "copy #t
extern #t
latex #t
paste 2
clear none
graphics #f
quit ()" \
      -headless -x "$atquit" -x '(load "startup-clip.scm")'

# A copy in a buffer which was never typeset (a script, no update-forced):
# the selection is found in the boxes, which did not exist yet, and the
# copy crashed in find_check_selection (#729).
cat > "$out/startup-untypeset.scm" <<'EOF'
(let ((u (url-append (url-pwd) "startup-a.tm")))
  (load-buffer u)
  (switch-to-buffer* u)
  (select-all)
  (clipboard-copy "primary")
  (display* "CHECK copy "
            (string-contains? (object->string (tree->stree
                                               (clipboard-get "primary")))
                              "startup checks")
            "\n")
  (clipboard-clear "primary")
  (buffer-pretend-saved u)
  (buffer-close u))
EOF

check "headless: a copy in a buffer which was never typeset (#729)" \
      "copy #t
quit ()" \
      -headless -x "$atquit" -x '(load "startup-untypeset.scm")'

echo "Total: $total checks, $failed failed"
exit $status
