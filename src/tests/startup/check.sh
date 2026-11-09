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

# A buffer which is closed gives back the memory of its editor and of its
# boxes. The canvas which shows an editor (QTMWidget) holds that editor: a
# window which showed another view kept the canvas of the former one, so the
# editor of a closed view, with its boxes, was never deleted, and each open
# and close of a long document took more memory (#713). The canvas is
# deleted later, by the event loop of Qt: the steps of the script run from
# it (exec-delayed), as when the user opens and closes the document. The
# memory after the closes 3 to 5 may not exceed the one after the closes 1
# and 2 by half of what an open takes (a canvas may wait one more close
# before it is deleted). The cost of an open is measured from the first
# close, as some memory is taken once, at the first open.
awk 'BEGIN { print "<TeXmacs|2.1.4>\n\n<style|generic>\n\n<\\body>";
             for (i = 0; i < 3000; i++)
               print "  Line " i " of a long document of the startup checks.\n";
             print "</body>" }' > "$out/startup-long.tm"
cat > "$out/startup-close.scm" <<'EOF'
(define u (url-append (url-pwd) "startup-long.tm"))
(define (memory) (gc) (texmacs-memory))
(define opens '())   ; the memory with the document shown, by cycle
(define closes '())  ; the memory after it was closed, by cycle
(define (at l k) (list-ref (reverse l) k))
(define done #f)
(define (later f)
  ;; the next step, from the event loop; an error stops the script (the
  ;; exit is outside the catch: under Guile, exit throws quit)
  (exec-delayed
    (lambda ()
      (catch #t f
        (lambda args (display* "CHECK error " args "\n") (set! done #t)))
      (when done (exit 0)))))
(define (step-open)
  (load-buffer u)
  (switch-to-buffer* u)
  (later step-shown))
(define (step-shown)
  (update-forced)
  (set! opens (cons (memory) opens))
  (buffer-close u)
  (later step-closed))
(define (step-closed)
  (set! closes (cons (memory) closes))
  (if (< (length closes) 6)
      (later step-open)
      (let ((cost (- (at opens 1) (at closes 0)))
            (kept (- (max (at closes 3) (at closes 4) (at closes 5))
                     (max (at closes 1) (at closes 2)))))
        (display* "memory: opens " (reverse opens) "\n")
        (display* "memory: closes " (reverse closes) "\n")
        (display* "CHECK opened " (> cost 0) "\n")
        (display* "CHECK freed " (< (* 2 kept) cost) "\n")
        (set! done #t))))
(later step-open)
EOF

check "a closed buffer gives back the memory of its editor (#713)" \
      "opened #t
freed #t" \
      -x '(load "startup-close.scm")'

# The command which closes an embedded editor (texmacs-input) when its
# window is deleted kept the view of its buffer: when the buffer had been
# closed before, it read the deleted view and crashed (#775). A widget made
# again with the same name takes over the buffer and its view: deleting the
# window of the former widget must not close the buffer which the new one
# shows. The steps run from the event loop, which deletes the windows.
cat > "$out/startup-embedded.scm" <<'EOF'
(define done #f)
(define (later f)
  (exec-delayed
    (lambda ()
      (catch #t f
        (lambda args (display* "CHECK error " args "\n") (set! done #t)))
      (when done (exit 0)))))
(define (embedded name doc)
  ;; a window with a texmacs-input on the buffer name
  (let ((win (alt-window-handle))
        (in (widget-texmacs-input doc '(style "generic") name)))
    (alt-window-create-plain win in "startup-embedded")
    (alt-window-show win)
    win))
(define u1 (string->url "tmfs://aux/startup-embedded-1"))
(define u2 (string->url "tmfs://aux/startup-embedded-2"))
(define w1 #f)
(define w2 #f)
(define w3 #f)
(define (step1)
  (set! w1 (embedded u1 '(document "one")))
  (later step2))
(define (step2)
  (buffer-close u1)
  (display* "CHECK buffer closed " (buffer-exists? u1) "\n")
  (alt-window-delete w1)
  (set! w2 (embedded u2 '(document "two")))
  (later step3))
(define (step3)
  (set! w3 (embedded u2 '(document "three")))
  (later step4))
(define (step4)
  (alt-window-delete w2)
  (display* "CHECK reused buffer kept " (buffer-exists? u2) "\n")
  (alt-window-delete w3)
  (display* "CHECK reused buffer closed " (buffer-exists? u2) "\n")
  (set! done #t))
(later step1)
EOF

check "an embedded buffer closed before its window, and one made again (#775)" \
      "buffer closed #f
reused buffer kept #t
reused buffer closed #f" \
      -x '(load "startup-embedded.scm")'

# Side tools made again let their former widgets go: the cache of the
# window kept every widget of the side tools, without ever using them, and
# an embedded editor (texmacs-input) closes its buffer only when its widget
# is destroyed by its window. So each new content of a side tool with a
# texmacs-input without a name left a buffer, a view and an editor behind
# (#776). The former widgets are let go at garbage collections, then close
# their buffers from the event queue: the steps wait for that, with a
# bound. A texmacs-input with a name has one editor per opening of its
# tool: made again while the tool is shown, it keeps its view, so what was
# typed can be undone, while new contents replacing an untouched one (a
# field which shows results) are not kept. When the tool is closed, its
# fields end at once: their views are deleted, the buffers without a name
# closed, the named buffer stays (the search toolbar uses the buffer of the
# search tool). The tool opened again at once, without a garbage
# collection, gets a new view with an empty history: the views and the
# histories do not pile up over the reopenings.
cat > "$out/startup-side-tools.scm" <<'EOF'
(define done #f)
(define (later f)
  (exec-delayed
    (lambda ()
      (catch #t f
        (lambda args (display* "CHECK error " args "\n") (set! done #t)))
      (when done (exit 0)))))
(define (inputs)
  ;; the number of buffers of texmacs-input without a name
  (with prefix "tmfs://aux/TeXmacs-input-"
    (length (list-filter (buffer-list)
                         (lambda (u)
                           (string-starts? (url->string u) prefix))))))
(define (wait-until ok? n next)
  ;; garbage collections from the event loop until (ok?), at most n
  (lambda ()
    (gc)
    (if (or (ok?) (= n 0)) (next) (later (wait-until ok? (- n 1) next)))))
(define round 0)
(tm-tool (startup-side-tool win)
  (:name "startup side tool")
  (text (number->string round))
  (texmacs-input `(document ,(number->string round)) '(style "generic") #f))
(define (step)
  (set! round (+ round 1))
  (tool-select :right 'startup-side-tool)
  (update-menus)
  (later step2))
(define (step2)
  (update-forced)
  (if (< round 6)
      (later step)
      (later (wait-until (lambda () (= (inputs) 1)) 100 step3))))
(define (step3)
  (display* "CHECK buffers left " (inputs) "\n")
  (named-start))
(define named (string->url "tmfs://aux/startup-side-named"))
(define named-round 0)
(define named-keep #f)
(define (in-named thunk)
  ;; as with-buffer (which is not loaded yet)
  (let ((old (current-buffer)))
    (and (buffer-focus named)
         (let ((r (thunk)))
           (buffer-focus old)
           r))))
(define (named-views) (length (buffer->views named)))
(define (named-undo)
  (in-named (lambda ()
              (archive-state) (start-editing) (end-editing)
              (undo-possibilities))))
(tm-tool (startup-named-tool win)
  (:name "startup named tool")
  (text (number->string named-round))
  (texmacs-input (if named-keep (tree->stree (buffer-get-body named))
                     `(document ,(number->string named-round)))
                 '(style "generic") named)
  (texmacs-input '(document "sentinel") '(style "generic") #f))
(define (named-start)
  (set! named-round (+ named-round 1))
  (tool-select :right 'startup-named-tool)
  (update-menus)
  (later named-shown))
(define (typed s)
  ;; text typed in the named input, as one action
  (in-named (lambda ()
              (archive-state) (start-editing) (insert s) (end-editing))))
(define (named-shown)
  (update-forced)
  (cond ((= named-round 1) (typed "typed "))
        ((= named-round 2)
         ;; made again while it is shown: the input keeps its history, and
         ;; the new content can be undone
         (display* "CHECK named undo kept " (> (named-undo) 0) "\n")))
  (if (< named-round 8) (later named-start) (later named-close)))
(define (named-close)
  ;; 6 new contents while nothing was typed: they are not kept
  (display* "CHECK named undo after 6 results " (named-undo) "\n")
  (display* "CHECK named views " (named-views) "\n")
  (set-window-tools (current-window) :right '())
  (update-menus)
  ;; the end of the opening of the tool: its fields end at once
  (display* "CHECK closed: views " (named-views) ", sentinels " (inputs)
            ", named buffer kept " (buffer-exists? named) "\n")
  ;; as the search toolbar does with the buffer of the search tool
  (buffer-set-body named (stree->tree '(document "toolbar")))
  (display* "CHECK named buffer usable "
            (== (tree->stree (buffer-get-body named)) '(document "toolbar"))
            "\n")
  ;; from now on, the tool shows what was typed (a content which differs
  ;; from the former one would clear an untouched history)
  (set! named-keep #t)
  (later named-reopen))
(define reopened 0)
(define reopen-ok #t)
(define (named-reopen)
  ;; opened again at once, without a garbage collection
  (tool-select :right 'startup-named-tool)
  (update-menus)
  (update-forced)
  ;; opened anew: a new editor, without the history of the former one
  (when (or (!= (named-views) 1) (!= (named-undo) 0))
    (display* "CHECK reopened " reopened ": views " (named-views)
              ", undo " (named-undo) "\n")
    (set! reopen-ok #f))
  (set! reopened (+ reopened 1))
  (typed "again ")
  (when (!= (named-undo) 1) (set! reopen-ok #f))
  (set-window-tools (current-window) :right '())
  (update-menus)
  (when (!= (named-views) 0) (set! reopen-ok #f))
  (if (< reopened 6)
      (later named-reopen)
      (begin
        (display* "CHECK named reopened " reopened " times " reopen-ok "\n")
        (set! done #t))))
(later step)
EOF

check "side tools made again close the buffers of their texmacs-input (#776)" \
      "buffers left 1
named undo kept #t
named undo after 6 results 0
named views 1
closed: views 0, sentinels 0, named buffer kept #t
named buffer usable #t
named reopened 6 times #t" \
      -x '(load "startup-side-tools.scm")'

# A dialog whose refreshable makes a named texmacs-input again: the newer
# widget takes the buffer over without being registered for the dialog, so
# the former one, let go later, must leave the registration of the dialog,
# or Close buffer in the field closes the buffer instead of the dialog
# (#776). An unnamed input next to it shows when the former was let go.
cat > "$out/startup-dialog-refresh.scm" <<'EOF'
(define done #f)
(define (later f)
  (exec-delayed
    (lambda ()
      (catch #t f
        (lambda args (display* "CHECK error " args "\n") (set! done #t)))
      (when done (exit 0)))))
(define (inputs)
  ;; the number of buffers of texmacs-input without a name
  (with prefix "tmfs://aux/TeXmacs-input-"
    (length (list-filter (buffer-list)
                         (lambda (u)
                           (string-starts? (url->string u) prefix))))))
(define (wait-until ok? n next)
  ;; garbage collections from the event loop until (ok?), at most n
  (lambda ()
    (gc)
    (if (or (ok?) (= n 0)) (next) (later (wait-until ok? (- n 1) next)))))
(define name (string->url "tmfs://aux/startup-dialog-refresh"))
(define cnt 0)
(tm-widget ((startup-dialog) quit)
  (refreshable "startup-dialog-refresh"
    (resize "300px" "100px"
      (texmacs-input `(document ,(number->string cnt))
                     '(style "generic") name))
    (texmacs-input '(document "sentinel") '(style "generic") #f)))
(define (step1)
  (dialogue-window (startup-dialog) noop "Startup dialog")
  (later step2))
(define (step2)
  (display* "CHECK embedded " (buffer-embedded? name) "\n")
  (set! cnt 1)
  (refresh-now "startup-dialog-refresh")
  (later (wait-until (lambda () (= (inputs) 1)) 100 step3)))
(define (step3)
  (display* "CHECK sentinels left " (inputs) "\n")
  (display* "CHECK embedded after refresh " (buffer-embedded? name) "\n")
  (set! done #t))
(later step1)
EOF

check "a dialog keeps its named texmacs-input made again by a refresh (#776)" \
      "embedded #t
sentinels left 1
embedded after refresh #t" \
      -x '(load "startup-dialog-refresh.scm")'

# A dialog with a named texmacs-input in a refreshable: one editor per
# opening of the dialog (#776). A refresh keeps the editor, so what was
# typed can be undone; when the dialog is closed, the views of its fields
# are deleted at once; opened again at once, without a garbage collection,
# the field starts with an empty history, and the views do not pile up.
cat > "$out/startup-dialog-reopen.scm" <<'EOF'
(define done #f)
(define (later f)
  (exec-delayed
    (lambda ()
      (catch #t f
        (lambda args (display* "CHECK error " args "\n") (set! done #t)))
      (when done (exit 0)))))
(define name (string->url "tmfs://aux/startup-dialog-reopen"))
(define (in-name thunk)
  ;; as with-buffer (which is not loaded yet)
  (let ((old (current-buffer)))
    (and (buffer-focus name)
         (let ((r (thunk)))
           (buffer-focus old)
           r))))
(define (views)
  (if (buffer-exists? name) (length (buffer->views name)) 0))
(define (undos)
  (in-name (lambda ()
             (archive-state) (start-editing) (end-editing)
             (undo-possibilities))))
(define close-dialog #f)
(define opened 0)
(define content "x")
(tm-widget ((startup-reopen) quit)
  (refreshable "startup-dialog-reopen"
    (resize "300px" "100px"
      (texmacs-input (begin (set! close-dialog quit) `(document ,content))
                     '(style "generic") name))))
(define (open-it)
  (dialogue-window (startup-reopen) noop "Startup dialog")
  (set! opened (+ opened 1))
  (later shown))
(define (shown)
  (update-forced)
  (display* "CHECK opening " opened ": views " (views) ", undo " (undos)
            "\n")
  (in-name (lambda ()
             (archive-state) (start-editing) (insert "typed") (end-editing)))
  ;; a refresh with another content: what was typed can be undone
  (set! content (string-append "x" (number->string opened)))
  (refresh-now "startup-dialog-reopen")
  (later refreshed))
(define (refreshed)
  (display* "CHECK refreshed " opened ": views " (views) ", undo kept "
            (> (undos) 0) "\n")
  (close-dialog)
  (display* "CHECK closed " opened ": views " (views) "\n")
  (if (< opened 3) (open-it) (set! done #t)))
(later open-it)
EOF

check "a dialog opened again starts with an empty history (#776)" \
      "opening 1: views 1, undo 0
refreshed 1: views 1, undo kept #t
closed 1: views 0
opening 2: views 1, undo 0
refreshed 2: views 1, undo kept #t
closed 2: views 0
opening 3: views 1, undo 0
refreshed 3: views 1, undo kept #t
closed 3: views 0" \
      -x '(load "startup-dialog-reopen.scm")'

# The same tool with a named texmacs-input in two windows: two openings,
# so two views of the buffer, each with its editor (#776), where one view
# was taken over by the other before. What is typed in one is in the buffer
# once (no change announced twice, #796). As for two windows on one
# document, each editor records the changes of the buffer made while it
# exists, also those typed in the other. A window which is closed ends the
# openings of its tools at once.
cat > "$out/startup-two-windows.scm" <<'EOF'
(define done #f)
(define (later f)
  (exec-delayed
    (lambda ()
      (catch #t f
        (lambda args (display* "CHECK error " args "\n") (set! done #t)))
      (when done (exit 0)))))
(define name (string->url "tmfs://aux/startup-two-windows"))
(define (in-name thunk)
  ;; as with-buffer (which is not loaded yet): the first view of the buffer
  (let ((old (current-buffer)))
    (and (buffer-focus name)
         (let ((r (thunk)))
           (buffer-focus old)
           r))))
(define (views) (length (buffer->views name)))
(define (undos)
  (in-name (lambda ()
             (archive-state) (start-editing) (end-editing)
             (undo-possibilities))))
(define (typed s)
  (in-name (lambda ()
             (archive-state) (start-editing) (insert s) (end-editing))))
(define (body) (tree->stree (buffer-get-body name)))
(tm-tool (startup-two-tool win)
  (:name "startup two tool")
  (texmacs-input '(document "x") '(style "generic") name))
(define w1 #f)
(define w2 #f)
(define (step1)
  (set! w1 (current-window))
  (tool-select :right 'startup-two-tool)
  (update-menus)
  (later step2))
(define (step2)
  (update-forced)
  (set! w2 (open-window))
  (tool-select :right 'startup-two-tool w2)
  (update-menus)
  (later step3))
(define (step3)
  (update-forced)
  (display* "CHECK views " (views) "\n")
  (typed "a")
  (display* "CHECK typed in one " (cadr (body)) ", undo " (undos) "\n")
  ;; the first window ends: the view of the second is left
  (kill-window w1)
  (display* "CHECK window closed: views " (views) ", undo " (undos) "\n")
  (typed "b")
  (display* "CHECK typed in the other " (string-length (cadr (body)))
            ", undo " (undos) "\n")
  (set! done #t))
(later step1)
EOF

check "the same tool in two windows has two editors (#776)" \
      "views 2
typed in one ax, undo 1
window closed: views 1, undo 1
typed in the other 3, undo 1" \
      -x '(load "startup-two-windows.scm")'

# A link whose target has a wildcard: the pattern was loaded as the name of
# a buffer, and url-glue (the autosave names) threw a C++ exception which
# nothing catches under -x, so that TeXmacs aborted (#771). The check is
# here, not in the suites, since a crash would take a whole suite run down;
# here it is the status 134 of this one case. The files which match are the
# candidates of the link: several give the page of disambiguation, one is
# loaded (also with a query and as a file: url), none gives a message; a
# wildcard in a relative link of a document of the web gives a message too.
mkdir -p "$out/startup-wild"
printf 'a\nb\nc\nd\n' > "$out/startup-wild/wild1.scm"
printf 'x\n' > "$out/startup-wild/wild2.scm"
printf 'y\n' > "$out/startup-wild/solo1.scm"
cat > "$out/startup-wild.scm" <<'EOF'
(use-modules (link link-navigate))
(define d (url->system (url-append (url-pwd) "startup-wild")))
(define (page-text)
  (with l (list-filter (buffer-list)
                       (lambda (b) (== (url->string (url-tail b))
                                       "Disambiguation page")))
    (if (null? l) "" (object->string (tree->stree (buffer-get-body (car l)))))))
(define (listed? s) (string-contains? (page-text) s))
(go-to-url (string-append d "/wild*.scm"))
(display* "CHECK several " (listed? "wild1.scm") " " (listed? "wild2.scm")
          " " (listed? "solo1.scm") "\n")
(define n (length (buffer-list)))
(go-to-url (string-append d "/nothing*.scm"))
(display* "CHECK none " (== n (length (buffer-list))) "\n")
(go-to-url (string-append d "/solo*.scm"))
(display* "CHECK one " (url->string (url-tail (current-buffer))) "\n")
(buffer-close (current-buffer))
(go-to-url (string-append d "/wild*.scm?line=3"))
(display* "CHECK several-query " (listed? "wild1.scm") "\n")
(go-to-url (string-append "file://" d "/wild*.scm"))
(display* "CHECK file-root " (listed? "wild2.scm") "\n")
;; a relative link in a document of the web: url-complete knows only files
(define web-buffer (string->url "http://localhost:1/x/y.tm"))
(define real-current-buffer current-buffer)
(define m (length (buffer-list)))
(set! current-buffer (lambda () web-buffer))
(go-to-url "docs/a*.html")
(set! current-buffer real-current-buffer)
(display* "CHECK web-relative " (== m (length (buffer-list))) "\n")
(display* "CHECK survived\n")
EOF

check "a link with a wildcard in its target does not abort TeXmacs (#771)" \
      "several #t #t #f
none #t
one solo1.scm
several-query #t
file-root #t
web-relative #t
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-wild.scm")'

# A wildcard url reached url-glue and url-unglue, which threw a C++
# exception for it (nothing catches it under -x: TeXmacs aborted), and
# load-buffer, which glues the names of the autosave files (#790). Each case
# is a run of its own, since a crash ends the run: the statuses 134 and 139
# show as "exit status" lines.
mkdir -p "$out/startup-glue"
printf 'a\n' > "$out/startup-glue/one1.scm"
printf 'a\n' > "$out/startup-glue/one1.scm~"
cat > "$out/startup-glue.scm" <<'EOF'
(define d (url->system (url-append (url-pwd) "startup-glue")))
(define w (system->url (string-append d "/one*.scm")))
(define (ends? u s) (string-ends? (url->system u) s))
(define (found u) (url->system (url-tail (url-complete u "fr"))))
(define g (url-glue w "~"))
(display* "CHECK glue " (ends? g "one*.scm~") " " (found g) "\n")
(define u (url-unglue g 1))
(display* "CHECK unglue " (ends? u "one*.scm") " " (found u) "\n")
(display* "CHECK unglue-3 " (ends? (url-unglue w 3) "one*.") "\n")
(display* "CHECK survived\n")
EOF

check "url-glue and url-unglue of a wildcard apply to the pattern (#790)" \
      "glue #t one1.scm~
unglue #t one1.scm
unglue-3 #t
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-glue.scm")'

# load-buffer of a wildcard: the files which it matches are the candidates;
# none gives a message, several give a message and load nothing (a script
# can't answer a page of disambiguation), one is loaded, with the question
# for a large file (a stub answers it, since no window can ask it).
yes 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' \
  | head -c 5400000 > "$out/startup-wild/huge1.scm"
cat > "$out/startup-wild-load.scm" <<'EOF'
(define d (url->system (url-append (url-pwd) "startup-wild")))
(define n (length (buffer-list)))
(define (same?) (== n (length (buffer-list))))
(define (names)
  (string-recompose (list-filter (map (lambda (u) (url->string (url-tail u)))
                                      (buffer-list))
                                 (lambda (s) (string-starts? s "wild")))
                    " "))
(load-buffer (string-append d "/nothing*.scm"))
(display* "CHECK none " (same?) "\n")
(load-buffer (string-append d "/wild*.scm"))
(display* "CHECK several " (same?) " [" (names) "]\n")
(load-buffer (string-append d "/solo*.scm"))
(display* "CHECK one " (url->string (url-tail (current-buffer))) " "
          (== (+ n 1) (length (buffer-list))) "\n")
(buffer-close (current-buffer))
(define asked 0)
(set-large-load-test-interactive!
 (lambda (prompt default cont) (set! asked (+ asked 1)) (cont #f)))
(load-buffer (string-append d "/huge*.scm") :confirm-large)
(display* "CHECK large asked " asked " loaded " (not (same?)) "\n")
(set-large-load-test-interactive! #f)
(display* "CHECK survived\n")
EOF

check "load-buffer of a wildcard: none, several and one match (#790)" \
      "none #t
several #t []
one solo1.scm #t
large asked 1 loaded #f
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-wild-load.scm")'

# A wildcard in the other roots (tmfs, web) is not completed: url-exists? and
# url-complete threw "wildcards only implemented for files" (an abort), for
# a link, for load-buffer, and for a relative name in a document of tmfs (a
# help page) (#790, after #771). The message "No file matches" is given and
# nothing is loaded or fetched. No case reaches default-open: a wildcard
# stops at the message, and the files of the other cases are .scm and .tm.
cat > "$out/startup-wild-root.scm" <<'EOF'
(use-modules (link link-navigate))
(define what (getenv "TM_WILD_CASE"))
(define m (length (buffer-list)))
(define real-current-buffer current-buffer)
(cond ((== what "go-tmfs") (go-to-url "tmfs://help/x*y"))
      ((== what "load-tmfs") (load-buffer "tmfs://aux/x*y"))
      ((== what "load-http") (load-buffer "http://localhost:1/x*y.tm"))
      ((== what "link-relative")
       (set! current-buffer (lambda () (string->url "tmfs://help/foo/bar")))
       (go-to-url "a*.tm")
       (set! current-buffer real-current-buffer))
      ((== what "load-relative")
       (set! current-buffer (lambda () (string->url "tmfs://help/foo/bar")))
       (load-buffer "a*.tm")
       (set! current-buffer real-current-buffer)))
(display* "CHECK " what " " (== m (length (buffer-list))) "\n")
(display* "CHECK survived\n")
EOF

for w in go-tmfs load-tmfs load-http link-relative load-relative; do
  TM_WILD_CASE=$w check "a wildcard of tmfs or the web gives a message: $w (#790)" \
        "$w #t
survived
quit ()" \
        -headless -x "$atquit" -x '(load "startup-wild-root.scm")'
done

# A buffer named with a wildcard (Save as a*.tm, a name in tmfs://aux): the
# views of the buffer are found as they were, not as the plain name of a file
# with a star (#790).
mkdir -p "$out/startup-save"
cat > "$out/startup-aux.scm" <<'EOF'
(open-auxiliary "x*y" '(document "x"))
(define u (string->url "tmfs://aux/x*y"))
(open-buffer-in-window u (buffer-get u) "")
(define vs (buffer->views u))
(display* "CHECK aux views "
          (and (pair? vs) (list-and (map (lambda (v) (== (view->buffer v) u))
                                         vs)))
          " current " (== (current-buffer) u) "\n")
(display* "CHECK survived\n")
EOF

check "a buffer of tmfs named with a star has its views (#790)" \
      "aux views #t current #t
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-aux.scm")'

# A file whose name contains a star (not on Windows, which can't have one):
# url-complete made a wildcard of the name of the entry again, which matched
# itself, until the stack overflowed (#790, status 139).
case $(uname -s) in
  MINGW*|MSYS*|CYGWIN*) ;;
  *)
mkdir -p "$out/startup-star" "$out/startup-star1"
printf 'a\n' > "$out/startup-star/a*b.scm"
printf 'a\n' > "$out/startup-star/axc.scm"
printf 'a\n' > "$out/startup-star/zzz.scm"
printf 'a\n' > "$out/startup-star1/a*b.scm"
cat > "$out/startup-star.scm" <<'EOF'
(define d (url->system (url-append (url-pwd) "startup-star")))
(define (found pattern)
  (string-recompose
   (sort (map (lambda (u) (url->system (url-tail u)))
              (url->list (url-expand (url-complete (system->url pattern) "fr"))))
         string<?)
   " "))
(display* "CHECK complete [" (found (string-append d "/a*.scm")) "]\n")
(display* "CHECK complete-name [" (found (string-append d "/a*b.scm")) "]\n")
(display* "CHECK complete-any [" (found (string-append d "/*")) "]\n")
(display* "CHECK survived\n")
EOF

check "url-complete in a directory with a file with a star in its name (#790)" \
      "complete [a*b.scm axc.scm]
complete-name [a*b.scm]
complete-any [a*b.scm axc.scm zzz.scm]
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-star.scm")'

# The file is reached by a link and by load-buffer, and its buffer is found
# again (the name of a view was parsed again into a wildcard).
cat > "$out/startup-star-link.scm" <<'EOF'
(use-modules (link link-navigate))
(define d1 (url->system (url-append (url-pwd) "startup-star1")))
(go-to-url (string-append d1 "/a*.scm"))
(display* "CHECK link " (url->string (url-tail (current-buffer))) "\n")
(buffer-close (current-buffer))
(load-buffer (string-append d1 "/a*.scm"))
(display* "CHECK load " (url->string (url-tail (current-buffer))) "\n")
(display* "CHECK survived\n")
EOF

check "a file with a star in its name is reached by a link and load-buffer (#790)" \
      "link a*b.scm
load a*b.scm
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-star-link.scm")'

# Save as with a star in the name: the pattern is the name of the buffer
cat > "$out/startup-save.scm" <<'EOF'
(define d (url->system (url-append (url-pwd) "startup-save")))
(save-buffer-as-main (string->url (string-append d "/sv*.tm")) :overwrite)
(display* "CHECK saved " (string-contains? (url->system (current-buffer)) "sv*.tm")
          "\n")
(display* "CHECK survived\n")
EOF

check "Save as with a star in the name renames the buffer (#790)" \
      "saved #t
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-save.scm")'

# The subdirectories with a star are found as plain names too (url-search-
# upwards looks into them): the url had a wildcard in place of the name
mkdir -p "$out/startup-star2/a*b"
printf 'a\n' > "$out/startup-star2/a*b/target.txt"
cat > "$out/startup-star-search.scm" <<'EOF'
(define d (url-append (url-pwd) "startup-star2"))
(define f (url-search-upwards d "target.txt" '()))
(display* "CHECK search " (string-ends? (url->system f) "a*b/target.txt") " "
          (url-exists? f) "\n")
(display* "CHECK survived\n")
EOF

check "a subdirectory with a star in its name is searched as a plain name (#790)" \
      "search #t #t
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-star-search.scm")'

# A file a*b.scm loaded as a plain name, and a buffer saved as a*b.scm, which
# is named with the wildcard: their views have the same name, each is found
# in its own buffer
mkdir -p "$out/startup-star3"
printf 'a\n' > "$out/startup-star3/a*b.scm"
cat > "$out/startup-star-both.scm" <<'EOF'
(define d (url->system (url-append (url-pwd) "startup-star3")))
(load-buffer (string-append d "/a*b.scm"))
(define P (current-buffer))
(define pv (car (buffer->views P)))
;; the views are numbered by name: the second view of new.tm has number 2,
;; the first view of a*b.scm number 1 (the same number, with the same name
;; of view, can't be told apart: both buffers are named a*b.scm)
(load-buffer (string-append d "/new.tm"))
(buffer-close (current-buffer))
(load-buffer (string-append d "/new.tm"))
(save-buffer-as-main (string->url (string-append d "/a*b.scm")) :overwrite)
(define W (current-buffer))
(define wv (car (buffer->views W)))
(display* "CHECK both P " (== (view->buffer pv) P) " W "
          (== (view->buffer wv) W) "\n")
(display* "CHECK survived\n")
EOF

check "views of a plain a*b.scm and of a buffer saved as a*b.scm (#790)" \
      "both P #t W #t
survived
quit ()" \
      -headless -x "$atquit" -x '(load "startup-star-both.scm")'
  ;;
esac

# A side tool closed before the editor of its texmacs-input was typeset:
# the editor stays attached to its window, whose canvas is gone, and when
# its changes are applied, qt_simple_widget_rep::send (SLOT_SCROLL_POSITION)
# used the canvas without checking it, as the other slots do (#825). The
# search tool of the side tools is made as open-search makes it there,
# without the preferences which select the side tools.
cat > "$out/startup-early-close.scm" <<'EOF'
(use-modules (generic search-widgets))
(define done #f)
(define (later f)
  (exec-delayed
    (lambda ()
      (catch #t f
        (lambda args (display* "CHECK error " args "\n") (set! done #t)))
      (when done (exit 0)))))
(define tries 0)
(define (step1)
  ;; the search tool in the side tools, as open-search makes it there
  (let* ((u (current-buffer))
         (aux (search-buffer))
         (tool (list 'search-tool u (embedded-style-list "macro-editor")
                     (list) aux)))
    (buffer-set-master aux u)
    (tool-select :bottom-right tool))
  (later step2))
(define (step2)
  (update-menus)
  (set! tries (+ tries 1))
  (if (and (null? (buffer->views (search-buffer))) (< tries 40))
      (later step2)
      (begin
        (display* "CHECK shown " (length (buffer->views (search-buffer))) "\n")
        ;; closed before its editor was typeset
        (set-window-tools (current-window) :bottom-right '())
        (update-menus)
        ;; since #776, the end of the tool deletes the view of its field at
        ;; once, so that its editor no longer reaches the canvas
        (display* "CHECK views after the close "
                  (length (buffer->views (search-buffer))) "\n")
        (later (wait 10)))))
(define (wait n)
  (lambda ()
    (if (> n 0)
        (later (wait (- n 1)))
        (begin
          (display* "CHECK survived\n")
          (set! done #t)))))
(later step1)
EOF

before=$failed
check "a side tool closed before its texmacs-input was typeset (#825)" \
      "shown 1
views after the close 0
survived" \
      -x '(load "startup-early-close.scm")'

# the case reached the canvas which is gone: without it, it would pass
# without the crash of #825, if the editor were typeset before the close
# (checked only when the case passed, so that a failure counts once; since
# #776 the editor ends with its tool, so this only applies when the view
# was left)
if [ $failed -eq $before ] &&
   ! grep -aq "views after the close 0" "$out/$total.out" &&
   ! grep -aq "cannot set scroll position" "$out/$total.out"; then
  echo "  FAILED a side tool closed before its texmacs-input was typeset (#825):"
  echo "         the scroll position was not sent to the closed tool"
  failed=$((failed + 1))
  status=1
fi

echo "Total: $total checks, $failed failed"
exit $status
