;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-menu-test.scm
;; DESCRIPTION : tests of the keyboard maps and of the menus
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keyboard part looks up the bindings of the keyboard maps
;; (kbd-find-key-binding, in text, math and programs) and types key
;; sequences through keyboard-press, the function which the GUI calls for
;; each key, then reads the document. The keys are written as in the
;; keyboard maps (a var, math f, accent:hat e) and rewritten with
;; kbd-pre-rewrite into the keys of the look and feel.
;;
;; The menu part expands every menu and icon bar of TeXmacs in text, math,
;; a table, a program and graphics, the way make-menu-widget does it,
;; without making the widgets: it follows the links, evaluates the
;; conditions, loops, promises and the values of the input fields, computes
;; the check marks, whether the actions apply and their keyboard shortcuts,
;; and opens all the submenus. It checks that each button has an action
;; which can be called without arguments, but never runs it. Each error,
;; with the path of submenus which leads to it, is written to the log.

(texmacs-module (check kbd-menu-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cork n)
  ;; the string of the character n of the Cork encoding
  (string (integer->char n)))

(define (in-buffer doc path thunk)
  ;; run thunk in a new buffer holding doc, with the cursor at path
  (let* ((old (current-buffer))
         (u (new-buffer)))
    ;; new-buffer shows the buffer; switching to it again would make a
    ;; second view (see editing-test)
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (go-to (append (buffer-path) path))
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old))
      r)))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (cursor)
  (list-tail (cursor-path) (length (buffer-path))))

(define (edit-step thunk)
  ;; one user action, wrapped as the event loop wraps a key press
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define (press-keys keys)
  ;; type the key sequence keys, such as "a var" or "math f", through
  ;; keyboard-press, the entry point of the GUI for each key; the
  ;; symbolic prefixes (var, math, accent:hat...) are rewritten into the
  ;; keys of the look and feel first
  (for-each (lambda (k) (edit-step (lambda () (keyboard-press k 0))))
            (string-decompose (kbd-pre-rewrite keys) " ")))

(define (typed-in doc path keys)
  ;; the body of doc after typing keys at path
  (with r #f
    (in-buffer doc path
      (lambda ()
        (press-keys keys)
        (set! r (body))))
    r))

(define (typed keys)
  ;; the paragraph obtained by typing keys in an empty text document
  (and-with b (typed-in '(document "") '(0 0) keys)
    (cadr b)))

(define (typed-math keys)
  ;; the formula obtained by typing keys in an empty formula
  (and-with b (typed-in '(document (math "")) '(0 0 0) keys)
    (with m (cadr b)
      (if (func? m 'math 1) (cadr m) (list 'not-math m)))))

(define (typed-prog keys)
  ;; the program obtained by typing keys in an empty C++ program
  (and-with b (typed-in '(document (cpp-code (document ""))) '(0 0 0 0) keys)
    (with c (cadr b)
      (if (match? c '(cpp-code (document :%1))) (cadr (cadr c))
          (list 'not-program c)))))

(define (binding-source b)
  ;; the string or the command which a key binding inserts or runs
  (cond ((not b) #f)
        ((string? (car b)) (car b))
        ((procedure? (car b))
         (or (promise-source (car b)) (procedure-source (car b))))
        (else b)))

(define (binding-in doc path keys)
  ;; the binding of keys at path in doc
  (with r #f
    (in-buffer doc path
      (lambda ()
        (set! r (binding-source
                 (kbd-find-key-binding (kbd-pre-rewrite keys))))))
    r))

(define (text-binding keys) (binding-in '(document "") '(0 0) keys))
(define (math-binding keys) (binding-in '(document (math "")) '(0 0 0) keys))
(define (cpp-binding keys)
  (binding-in '(document (cpp-code (document ""))) '(0 0 0 0) keys))
(define (scheme-binding keys)
  (binding-in '(document (scm-code (document ""))) '(0 0 0 0) keys))
(define (python-binding keys)
  (binding-in '(document (python-code (document ""))) '(0 0 0 0) keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting of key names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keyboard maps are written with symbolic prefixes, which
;; kbd-pre-rewrite turns into keys: var and unvar are the variant keys tab
;; and S-tab whatever the look and feel, the modifiers are put in a
;; canonical order, and the prefixes of a look and feel (math, text,
;; accent:hat...) are rewritten consistently.
(define (test-rewrite)
  (check-group "key rewriting")
  (check= (kbd-pre-rewrite "a var") "a tab")
  (check= (kbd-pre-rewrite "a var var") "a tab tab")
  (check= (kbd-pre-rewrite "a unvar") "a S-tab")
  (check= (kbd-pre-rewrite "x") "x")
  (check= (kbd-pre-rewrite "- - -") "- - -")
  (check= (kbd-pre-rewrite "S-C-x") "C-S-x")
  (check= (kbd-pre-rewrite "C-A-x") "A-C-x")
  (check= (kbd-pre-rewrite "C-M-x") "M-C-x")
  (check= (kbd-pre-rewrite "A-M-x") "M-A-x")
  (check= (kbd-pre-rewrite "S-A-x") "A-S-x")
  ;; the composite prefixes are made of the simple ones
  (check= (kbd-pre-rewrite "math f") (kbd-pre-rewrite "cmd f"))
  (check= (kbd-pre-rewrite "text f") (kbd-pre-rewrite "cmd f"))
  (check= (kbd-pre-rewrite "accent:hat") (kbd-pre-rewrite "accent ^"))
  (check= (kbd-pre-rewrite "accent:acute") (kbd-pre-rewrite "accent '"))
  (check= (kbd-pre-rewrite "text:symbol s") (kbd-pre-rewrite "symbol s"))
  (check= (kbd-pre-rewrite "font b") (kbd-pre-rewrite "altcmd f b"))
  (check-false (string-occurs? "var" (kbd-pre-rewrite "math f var")))
  (check-true (string-ends? (kbd-pre-rewrite "math f var") " tab"))
  ;; kbd-post-rewrite leaves plain keys alone
  (check= (kbd-post-rewrite "a" #f) "a")
  (check= (kbd-post-rewrite "a tab" #f) "a tab")
  (check= (kbd-post-rewrite "C-f" #f) "C-f"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The bindings of the keyboard maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kbd-find-key-binding gives the binding of a key in the mode at the
;; cursor: a string to insert or a command, as the kbd-map of
;; generic-kbd, text-kbd, math-kbd and prog-kbd define them.
(define (test-bindings-generic)
  (check-group "generic bindings")
  ;; the same in all modes
  (for-each
    (lambda (b)
      (check= (text-binding (car b)) (cadr b))
      (check= (math-binding (car b)) (cadr b)))
    '(("left" (kbd-left))
      ("right" (kbd-right))
      ("return" (kbd-return))
      ("tab" (kbd-tab))
      ("backspace" (kbd-backspace))
      ("delete" (kbd-delete))
      ("std z" (undo 0))
      ("std c" (kbd-copy))
      ("std v" (kbd-paste))
      ("std x" (kbd-cut))
      ("$ var" "$")
      ("\\ var" "\\")
      ("\\ var var" "<setminus>")))
  ;; a key which is bound nowhere
  (check-false (text-binding "C-F12 C-F12 x"))
  (check-false (math-binding "C-F12 C-F12 x")))

(define (test-bindings-text)
  (check-group "text bindings")
  (check= (text-binding "$") '(make 'math))
  (check= (text-binding "\"") '(insert-quote))
  (check= (text-binding "'") '(insert-apostrophe #f))
  (check= (text-binding "- -") (cork #x15))
  (check= (text-binding "- - -") (cork #x16))
  (check= (text-binding "- - var") "--")
  (check= (text-binding "- var") '(make 'nbhyph))
  (check= (text-binding "- - - -") '(make 'hrule))
  (check= (text-binding "` `") (cork #x10))
  (check= (text-binding "' '") (cork #x11))
  (check= (text-binding ", ,") (cork #x12))
  (check= (text-binding "< <") (cork #x13))
  (check= (text-binding "> >") (cork #x14))
  (check= (text-binding "<") "<less>")
  (check= (text-binding ">") "<gtr>")
  (check= (text-binding ". . .") '(make 'text-dots))
  (check= (text-binding "space var") '(make 'nbsp))
  (check= (text-binding "_ var") '(make-script #f #t))
  (check= (text-binding "^ var") '(make-script #t #t))
  (check= (text-binding "_") "_")
  (check= (text-binding "^") "^")
  (check= (text-binding "# # var") '(make 'section))
  (check= (text-binding "# # # var") '(make 'subsection))
  (check= (text-binding "+ var") '(make-tmlist 'itemize))
  (check= (text-binding "1 . var") '(make-tmlist 'enumerate))
  (check= (text-binding "* * var") '(make-with "font-series" "bold"))
  (check= (text-binding "* var") '(make-with "font-shape" "italic"))
  (check= (text-binding "text 1") '(make-section 'section))
  (check= (text-binding "text 2") '(make-section 'subsection))
  (check= (text-binding "font b") '(make-with "font-series" "bold"))
  (check= (text-binding "font i") '(make-with "font-shape" "italic"))
  (check= (text-binding "accent:hat e") (cork #xEA))
  (check= (text-binding "accent:acute e") (cork #xE9))
  (check= (text-binding "accent:grave a") (cork #xE0))
  (check= (text-binding "accent:umlaut o") (cork #xF6))
  (check= (text-binding "accent:tilde n") (cork #xF1))
  (check= (text-binding "accent:cedilla c") (cork #xE7))
  (check= (text-binding "text:symbol s") (cork #xFF))
  ;; the letters themselves have no binding in text: they are inserted
  (check-false (text-binding "a"))
  (check-false (text-binding "a var"))
  ;; the mathematical bindings do not apply in text
  (check-false (text-binding "- >"))
  (check-false (text-binding "< =")))

(define (test-bindings-math)
  (check-group "math bindings")
  (check= (math-binding "$") '(math-make-math))
  (check= (math-binding "_") '(make-script #f #t))
  (check= (math-binding "^") '(make-script #t #t))
  (check= (math-binding "_ var") "_")
  (check= (math-binding "^ var") "^")
  (check= (math-binding "math f") '(make-fraction))
  (check= (math-binding "math f var") '(make 'tfrac))
  (check= (math-binding "math f var var") '(make 'dfrac))
  (check= (math-binding "math s") '(make-sqrt))
  (check= (math-binding "math s var") '(make-var-sqrt))
  (check= (math-binding "math V") '(make-wide "<vect>"))
  (check= (math-binding "math B") '(make-wide "<bar>"))
  (check= (math-binding "math ~") '(make-wide "~"))
  (check= (math-binding "'") '(make-rprime "'"))
  (check= (math-binding "(") '(math-bracket-open "(" ")" 'default))
  (check= (math-binding "[") '(math-bracket-open "[" "]" 'default))
  (check= (math-binding "a var") "<alpha>")
  (check= (math-binding "b var") "<beta>")
  (check= (math-binding "e var") "<varepsilon>")
  (check= (math-binding "e var var") "<mathe>")
  (check= (math-binding "e var var var") "<epsilon>")
  (check= (math-binding "p var") "<pi>")
  (check= (math-binding "p var var var") "<varpi>")
  (check= (math-binding "G var") "<Gamma>")
  (check= (math-binding "A var var") "<forall>")
  (check= (math-binding "E var var") "<exists>")
  (check= (math-binding "math:greek a") "<alpha>")
  (check= (math-binding "math:greek g") "<gamma>")
  (check= (math-binding "< =") "<leqslant>")
  (check= (math-binding "> =") "<geqslant>")
  (check= (math-binding "- >") "<rightarrow>")
  (check= (math-binding "< -") "<leftarrow>")
  (check= (math-binding "< - >") "<leftrightarrow>")
  (check= (math-binding "= >") "<Rightarrow>")
  (check= (math-binding "< <") "<ll>")
  (check= (math-binding "+ -") "<pm>")
  (check= (math-binding "- +") "<mp>")
  (check= (math-binding "< var") "<in>")
  (check= (math-binding "* var") "<times>")
  (check= (math-binding "@ var") "<box>")
  (check= (math-binding ". .") "<ldots>")
  (check= (math-binding ". . var") "<cdots>")
  (check= (math-binding "- -") "<longminus>")
  ;; the prefixes of the variants are bound to themselves
  (check= (math-binding "a") "a")
  ;; text bindings do not apply in math
  (check-false (math-binding "# # var"))
  (check-false (math-binding "text:symbol s")))

(define (test-bindings-prog)
  (check-group "program bindings")
  ;; programs override the typographic shortcuts of text
  (check= (cpp-binding "$") '(insert "$"))
  (check= (cpp-binding "$ var") '(make 'math))
  (check= (cpp-binding "\\") "\\")
  (check= (cpp-binding "\\ var") '(make 'hybrid))
  (check= (cpp-binding "- -") "--")
  (check= (cpp-binding "- - -") "---")
  (check= (cpp-binding "' '") "''")
  (check= (cpp-binding "` `") "``")
  (check= (cpp-binding "< <") "<less><less>")
  (check= (cpp-binding "space var") '(insert-tabstop))
  (check= (cpp-binding "cmd i") '(program-indent #f))
  (check= (cpp-binding "std z") '(undo 0))
  (check= (cpp-binding "left") '(kbd-left))
  (check-false (cpp-binding "- >"))
  (check-false (cpp-binding "a var"))
  ;; each language has its brackets
  (check= (cpp-binding "(") '(cpp-bracket-open "(" ")"))
  (check= (cpp-binding "{") '(cpp-bracket-open "{" "}"))
  (check= (cpp-binding ")") '(cpp-bracket-close "(" ")"))
  (check= (cpp-binding "\"") '(cpp-bracket-open "\"" "\""))
  (check= (scheme-binding "(") '(scheme-bracket-open "(" ")"))
  (check= (scheme-binding "\"") '(scheme-bracket-open "\"" "\""))
  (check= (python-binding "'") '(python-bracket-open "'" "'"))
  (check= (python-binding "[") '(python-bracket-open "[" "]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each key goes through keyboard-press, as from the GUI, and then through
;; key-press, which tries the key and the keys typed before it as a
;; shortcut: a shortcut which extends the previous one (- - after -)
;; replaces what the previous one inserted.
(define (test-typing-text)
  (check-group "typing text")
  (check= (typed "a b c") "abc")
  (check= (typed "H e l l o space w o r l d") "Hello world")
  (check= (typed "a b backspace c") "ac")
  (check= (typed "$") '(math ""))
  (check= (typed "$ x") '(math "x"))
  (check= (typed "$ var") "$")
  ;; quotes and dashes
  (check= (typed "\" a \"") (string-append (cork #x10) "a" (cork #x11)))
  (check= (typed "` `") (cork #x10))
  (check= (typed "' '") (cork #x11))
  (check= (typed ", ,") (cork #x12))
  (check= (typed "< <") (cork #x13))
  (check= (typed "> >") (cork #x14))
  (check= (typed "- -") (cork #x15))
  (check= (typed "- - -") (cork #x16))
  (check= (typed "- - var") "--")
  (check= (typed "a - b") "a-b")
  (check= (typed "- var") '(nbhyph))
  (check= (typed "- - - -") '(hrule))
  (check= (typed "< <") (cork #x13))
  (check= (typed "<") "<less>")
  (check= (typed ">") "<gtr>")
  (check= (typed ". .") "..")
  (check= (typed ". . .") '(text-dots))
  (check= (typed ". . . var") "...")
  ;; spaces, scripts and backslashes
  (check= (typed "space var") '(nbsp))
  (check= (typed "x _ 2") "x_2")
  (check= (typed "x ^ 2") "x^2")
  (check= (typed "_ var") '(rsub ""))
  (check= (typed "^ var") '(rsup ""))
  (check= (typed "\\ var") "\\")
  (check= (typed "\\ var var") "<setminus>")
  ;; accents and special letters
  (check= (typed "accent:hat e") (cork #xEA))
  (check= (typed "accent:acute e") (cork #xE9))
  (check= (typed "accent:grave a") (cork #xE0))
  (check= (typed "accent:umlaut o") (cork #xF6))
  (check= (typed "accent:tilde n") (cork #xF1))
  (check= (typed "accent:cedilla c") (cork #xE7))
  (check= (typed "text:symbol s") (cork #xFF))
  (check= (typed "text:symbol") "")
  (check= (typed "accent:hat") "^")
  (check= (typed "accent:hat space") "^")
  ;; in text, tab after a word completes it with the words of the open
  ;; buffers (all of them: the words of the suites which ran before count,
  ;; hence the unusual words); without a completion, it inserts nothing
  (check= (typed "q z var") "qz")
  (check= (typed-in '(document "xylophonic ") '(0 11) "x y var")
          '(document "xylophonic xylophonic"))
  ;; markup shortcuts
  (check= (typed "# # var") '(section ""))
  (check= (typed "# # # var") '(subsection ""))
  (check= (typed "* * var") '(with "font-series" "bold" ""))
  (check= (typed "* var") '(with "font-shape" "italic" ""))
  (check= (typed "font b x") '(with "font-series" "bold" "x"))
  (check= (typed "_ _ var") '(underline "")))

(define (test-typing-math)
  (check-group "typing math")
  (check= (typed-math "x") "x")
  (check= (typed-math "x + 1") "x+1")
  (check= (typed-math "a space b") "a b")
  (check= (typed-math "1 / 2") "1/2")
  (check= (typed-math "x ^ 2") '(concat "x" (rsup "2")))
  (check= (typed-math "x _ i") '(concat "x" (rsub "i")))
  (check= (typed-math "x _ i ^ 2") '(concat "x" (rsub (concat "i" (rsup "2")))))
  (check= (typed-math "^ var") "^")
  (check= (typed-math "_ var") "_")
  ;; fractions, roots and accents
  (check= (typed-math "math f") '(frac "" ""))
  (check= (typed-math "math f 1") '(frac "1" ""))
  (check= (typed-math "math f var") '(tfrac "" ""))
  (check= (typed-math "math f var var") '(dfrac "" ""))
  (check= (typed-math "math s") '(sqrt ""))
  (check= (typed-math "math s x") '(sqrt "x"))
  (check= (typed-math "math s var") '(sqrt "" ""))
  (check= (typed-math "math V") '(wide "" "<vect>"))
  (check= (typed-math "math B") '(wide "" "<bar>"))
  (check= (typed-math "math ~") '(wide "" "~"))
  (check= (typed-math "math ^") '(wide "" "^"))
  ;; Greek letters and the other variants
  (check= (typed-math "a var") "<alpha>")
  (check= (typed-math "b var") "<beta>")
  (check= (typed-math "G var") "<Gamma>")
  (check= (typed-math "p var") "<pi>")
  (check= (typed-math "p var var") "<mathpi>")
  (check= (typed-math "p var var var") "<varpi>")
  (check= (typed-math "e var") "<varepsilon>")
  (check= (typed-math "e var var") "<mathe>")
  (check= (typed-math "e var var var") "<epsilon>")
  (check= (typed-math "e var var var var") "<backepsilon>")
  ;; past the last variant, tab comes back to the letter
  (check= (typed-math "a var var") "a")
  (check= (typed-math "e var var var var var") "e")
  (check= (typed-math "e var var unvar") "<varepsilon>")
  (check= (typed-math "a unvar") "<alpha>")
  (check= (typed-math "x a var") "x<alpha>")
  (check= (typed-math "math:greek a") "<alpha>")
  (check= (typed-math "math:greek g") "<gamma>")
  ;; a prefix alone inserts nothing
  (check= (typed-math "math:greek") "")
  (check= (typed-math "math") "")
  (check= (typed-math "A var var") "<forall>")
  (check= (typed-math "E var var") "<exists>")
  ;; symbols made of several keys
  (check= (typed-math "< =") "<leqslant>")
  (check= (typed-math "> =") "<geqslant>")
  (check= (typed-math "- >") "<rightarrow>")
  (check= (typed-math "< -") "<leftarrow>")
  (check= (typed-math "< - >") "<leftrightarrow>")
  (check= (typed-math "= >") "<Rightarrow>")
  (check= (typed-math "< <") "<ll>")
  (check= (typed-math "+ -") "<pm>")
  (check= (typed-math "- +") "<mp>")
  (check= (typed-math ". .") "<ldots>")
  (check= (typed-math ". . var") "<cdots>")
  (check= (typed-math "< var") "<in>")
  (check= (typed-math "@ var") "<box>")
  (check= (typed-math "* var") "<times>")
  (check= (typed-math "x < = y") "x<leqslant>y")
  ;; primes and brackets
  (check= (typed-math "x '") '(concat "x" (rprime "'")))
  (check= (typed-math "( x )") '(around* "(" "x" ")"))
  (check= (typed-math "[") '(around* "[" "" "]"))
  (check= (typed-math "{") '(around* "{" "" "}")))

;; Without a window, the environment at the cursor (get-env, in-math?)
;; is the one of the last typesetting. Once the keyboard maps of programs
;; are loaded (by a key lookup in a program), the first key after $ in
;; text still sees the text mode: "$ x ^ 2" then gives (math "x^2"). This
;; does not happen before, so these checks come before those of programs.
(define (test-typing-math-mode)
  (check-group "entering and leaving math")
  ;; $ in a formula leaves it, $ in text makes one
  (check= (typed-in '(document (math "x")) '(0 0 1) "$")
          '(document (math "x")))
  (in-buffer '(document (math "x")) '(0 0 1)
    (lambda ()
      (check-true (in-math?))
      (press-keys "$")
      (check-false (in-math?))
      (check= (cursor) '(0 1))
      (press-keys "a")
      (check= (body) '(document (concat (math "x") "a")))))
  (in-buffer '(document "") '(0 0)
    (lambda ()
      (check-false (in-math?))
      (press-keys "$")
      (check-true (in-math?))
      (check= (get-env "mode") "math")
      (check= (cursor) '(0 0 0))
      (press-keys "a var")
      (check= (body) '(document (math "<alpha>")))))
  (check= (typed "$ x ^ 2") '(math (concat "x" (rsup "2"))))
  (check= (typed "x $ a var") '(concat "x" (math "<alpha>"))))

(define (test-typing-prog)
  (check-group "typing programs")
  (check= (typed-prog "a b") "ab")
  (check= (typed-prog "$") "$")
  (check= (typed-prog "$ var") '(math ""))
  (check= (typed-prog "\\") "\\")
  (check= (typed-prog "- -") "--")
  (check= (typed-prog "- - -") "---")
  (check= (typed-prog "' '") "''")
  (check= (typed-prog "` `") "``")
  (check= (typed-prog "\"") "\"")
  (check= (typed-prog "< <") "<less><less>")
  (check= (typed-prog "( x )") "(x)")
  (check= (typed-prog "space var") "    ")
  (check= (typed-prog "x space var") "x   ")
  (check= (typed-prog "q z var") "qz"))

(define (test-typing-table)
  (check-group "typing in tables")
  (in-buffer '(document (tabular (table (row (cell "a") (cell "b"))
                                        (row (cell "c") (cell "d")))))
             '(0 0 0 0 0 1)
    (lambda ()
      (check-true (in-table?))
      (press-keys "x")
      (check= (body) '(document (tabular (table (row (cell "ax") (cell "b"))
                                                (row (cell "c") (cell "d"))))))
      (press-keys "$ y")
      (check= (body) '(document (tabular (table (row (cell (concat "ax" (math "y")))
                                                     (cell "b"))
                                                (row (cell "c") (cell "d")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining keyboard maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The bindings of the suite are on A-F12, which TeXmacs does not use;
;; the last ones apply only while kbd-test-on? holds.
(define kbd-test-on? #f)

(define (test-kbd-map)
  (check-group "kbd-map")
  (check-false (text-binding "A-F12"))
  (kbd-map
    (:mode in-text?)
    ("A-F12 A-F12 a" "<alpha>")
    ("A-F12 A-F12 b" (insert "bee"))
    ("A-F12 A-F12 b var" "<beta>"))
  (check= (text-binding "A-F12 A-F12 a") "<alpha>")
  (check= (text-binding "A-F12 A-F12 b") '(insert "bee"))
  (check= (text-binding "A-F12 A-F12 b var") "<beta>")
  ;; the bindings are those of text only
  (check-false (math-binding "A-F12 A-F12 a"))
  ;; the prefixes of a binding are bound to the keys themselves
  (check= (text-binding "A-F12") "A-F12")
  (check= (text-binding "A-F12 A-F12") "A-F12A-F12")
  ;; typing them
  (check= (typed "A-F12 A-F12 a") "<alpha>")
  (check= (typed "A-F12 A-F12 b") "bee")
  (check= (typed "A-F12 A-F12 b var") "<beta>")
  (check= (typed "x A-F12 A-F12 a y") "x<alpha>y")
  ;; the inverse binding of a command
  (in-buffer '(document "") '(0 0)
    (lambda ()
      (check= (kbd-find-inv-binding '(insert "bee")) "A-F12 A-F12 b")))
  ;; the modeless reverse binding, as the documentation shows it
  (check= (kbd-find-rev-binding "(insert \"bee\")") "A-F12 A-F12 b")
  ;; removing them
  (kbd-unmap
    (:mode in-text?)
    "A-F12 A-F12 a" "A-F12 A-F12 b" "A-F12 A-F12 b var")
  (check-false (text-binding "A-F12 A-F12 a"))
  (check-false (text-binding "A-F12 A-F12 b"))
  (check-false (text-binding "A-F12 A-F12 b var"))
  (in-buffer '(document "") '(0 0)
    (lambda ()
      (check= (kbd-find-inv-binding '(insert "bee")) "")))
  (check-false (kbd-find-rev-binding "(insert \"bee\")"))
  ;; kbd-unmap rewrites the keys as kbd-map does
  (kbd-map ("A-F12 n var" "y") ("S-C-F12 n" "w"))
  (check= (text-binding "A-F12 n var") "y")
  (check= (text-binding "S-C-F12 n") "w")
  (kbd-unmap "A-F12 n var" "S-C-F12 n")
  (check-false (text-binding "A-F12 n var"))
  (check-false (text-binding "S-C-F12 n"))
  ;; redefining or removing the binding of a key keeps the reverse
  ;; bindings of the other keys of its command
  (kbd-map ("A-F12 u" (insert "uu")) ("A-F12 v" (insert "uu")))
  (kbd-map ("A-F12 v" (insert "vv")))
  (check= (kbd-find-rev-binding "(insert \"uu\")") "A-F12 u")
  (check= (kbd-find-rev-binding "(insert \"vv\")") "A-F12 v")
  (kbd-unmap "A-F12 v")
  (check= (kbd-find-rev-binding "(insert \"uu\")") "A-F12 u")
  (check-false (kbd-find-rev-binding "(insert \"vv\")"))
  (kbd-unmap "A-F12 u")
  (check-false (kbd-find-rev-binding "(insert \"uu\")"))
  (kbd-map ("A-F12 w" "ww"))
  (check= (kbd-find-rev-binding "ww") "A-F12 w")
  (kbd-unmap "A-F12 w")
  (check-false (kbd-find-rev-binding "ww"))
  ;; bindings under a condition
  (kbd-map
    (:require kbd-test-on?)
    ("A-F12 A-F12 c" "<gamma>"))
  (set! kbd-test-on? #f)
  (check-false (text-binding "A-F12 A-F12 c"))
  (set! kbd-test-on? #t)
  (check= (text-binding "A-F12 A-F12 c") "<gamma>")
  (check= (math-binding "A-F12 A-F12 c") "<gamma>")
  (check= (typed "A-F12 A-F12 c") "<gamma>")
  ;; kbd-unmap with the same condition removes them
  (kbd-map (:require kbd-test-on?) ("A-F12 k" "x"))
  (check= (text-binding "A-F12 k") "x")
  (kbd-unmap (:require kbd-test-on?) "A-F12 k")
  (check-false (text-binding "A-F12 k"))
  ;; a binding "j var" after "j" in the same block keeps that of "j"
  (kbd-map (:require kbd-test-on?) ("A-F12 j" "y") ("A-F12 j var" "z"))
  (check= (text-binding "A-F12 j") "y")
  (check= (text-binding "A-F12 j var") "z")
  (kbd-unmap (:require kbd-test-on?) "A-F12 j var" "A-F12 j")
  (check-false (text-binding "A-F12 j"))
  (check-false (text-binding "A-F12 j var"))
  (set! kbd-test-on? #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define menu-errors '())
(define menu-buttons 0)
(define menu-submenus 0)

(define (label->string l)
  (cond ((string? l) l)
        ((and (pair? l) (in? (car l) '(balloon check shortcut extend)))
         (label->string (cadr l)))
        ((and (pair? l) (== (car l) 'style) (= (length l) 3))
         (label->string (caddr l)))
        ((and (pair? l) (== (car l) 'icon)) (object->string (cadr l)))
        ((and (pair? l) (== (car l) 'text) (= (length l) 3)) (caddr l))
        (else (object->string l))))

(define (where->string where)
  (string-recompose (map label->string (reverse where)) " > "))

(define (menu-error where msg x)
  (set! menu-errors
        (cons (string-append (where->string where) ": " msg ": "
                             (object->string x))
              menu-errors)))

(define (guarded where msg thunk default)
  ;; the value of thunk, or default after recording the error
  (catch #t thunk
    (lambda args
      (menu-error where msg args)
      default)))

(define (call0 where msg f)
  (if (procedure? f)
      (guarded where msg f #f)
      (begin (menu-error where (string-append msg " is not a procedure") f)
             #f)))

(define (translatable? s)
  (or (string? s) (func? s 'concat) (func? s 'verbatim) (func? s 'replace)))

(define (arity-ok? f n)
  ;; f can be called with n arguments
  (with a (procedure-property f 'arity)
    (or (not a)
        (and (<= (car a) n)
             (or (caddr a) (<= n (+ (car a) (cadr a))))))))

(define (check-procedure where what f n)
  (cond ((not (procedure? f))
         (menu-error where (string-append what " is not a procedure") f))
        ((not (arity-ok? f n))
         (menu-error where (string-append what " has a wrong arity")
                     (procedure-source f)))))

(define (icon-exists? name)
  (let ((png (string-append (substring name 0 (max 0 (- (string-length name) 4)))
                            ".png")))
    (or (url-exists? (url-resolve (url-append "$TEXMACS_PIXMAP_PATH" name) "r"))
        (url-exists? (url-resolve (url-append "$TEXMACS_PIXMAP_PATH" png) "r")))))

(define (walk-label l where)
  ;; the parts of a label which the menu widgets evaluate
  (cond ((translatable? l) (noop))
        ((func? l 'balloon 2) (walk-label (cadr l) where))
        ((func? l 'icon 1)
         (when (not (icon-exists? (cadr l)))
           (menu-error where "missing icon" (cadr l))))
        ((func? l 'text 2) (noop))
        ((func? l 'color 5) (noop))
        ((func? l 'style 2) (walk-label (caddr l) where))
        ((and (pair? l) (== (car l) 'extend) (pair? (cdr l)))
         (walk-label (cadr l) where)
         (walk-items (cddr l) where #f 0))
        ((and (func? l 'check 3) (string? (caddr l)))
         (walk-label (cadr l) where)
         (call0 where "check mark predicate" (cadddr l)))
        ((and (func? l 'shortcut 2) (string? (caddr l)))
         (walk-label (cadr l) where)
         (guarded where "shortcut"
                  (lambda () (kbd-system-rewrite (caddr l))) #f))
        (else (menu-error where "invalid label" l))))

(define (walk-entry p where inert?)
  ;; a button: as make-menu-entry, compute the check mark, whether the
  ;; action applies and its keyboard shortcut, but do not run it
  (set! menu-buttons (+ menu-buttons 1))
  (let* ((label (car p))
         (action (cAr p))
         (where* (cons label where)))
    (walk-label label where*)
    (check-procedure where* "action" action 0)
    (when (procedure? action)
      (with source (promise-source action)
        (when (pair? source)
          (and-with prop (property (car source) :check-mark)
            (guarded where* "check mark"
                     (lambda () (apply (cadr prop) (cdr source))) #f))
          (and-with prop (property (car source) :applicable)
            (guarded where* "applicable" (lambda () (apply (car prop) '())) #f))
          (when (not (pair? label))
            (guarded where* "shortcut"
                     (lambda ()
                       (with sh (kbd-find-inv-binding source)
                         (when (!= sh "") (kbd-system-rewrite sh))))
                     #f)))))))

(define (walk-items l where inert? depth)
  (cond ((> depth 60) (menu-error where "menu too deep" depth))
        ((list? l) (for-each (lambda (x) (walk x where inert? depth)) l))
        (else (menu-error where "not a list of menu items" l))))

(define (shape? p pattern)
  (match? (cdr p) pattern))

(define (walk p where inert? . opt-depth)
  ;; the items of the menu p, the way make-menu-items makes the widgets
  (guarded where "unexpected error"
           (lambda () (walk-item p where inert? opt-depth))
           #f))

(define (walk-item p where inert? opt-depth)
  (let* ((depth (if (null? opt-depth) 0 (+ (car opt-depth) 1)))
         (sub (lambda (l) (walk-items l where inert? depth))))
    (cond ((in? p (list '--- (string->symbol "|") '())) (noop))
          ((npair? p) (menu-error where "invalid menu item" p))
          ((match? p '(input :%1 :string? :%1 :string?))
           (check-procedure where "input command" (cadr p) 1)
           (call0 where "input proposals" (cadddr p)))
          ((translatable? (car p)) (walk-entry p where inert?))
          ((symbol? (car p)) (walk-tagged p where inert? depth sub))
          ((match? (car p) ':menu-wide-label) (walk-entry p where inert?))
          (else (sub p)))))

(define (walk-tagged p where inert? depth sub)
  (with tag (car p)
    (cond ((and (== tag 'link) (shape? p '(:%1)))
           (with r (guarded where "link"
                            (lambda () ((eval (cadr p)))) '())
             (walk-items r (cons (symbol->string (cadr p)) where)
                         inert? depth)))
          ((and (== tag 'dynamic) (shape? p '(:%1)))
           (sub (guarded where "dynamic" (lambda () (eval (cadr p))) '())))
          ((and (in? tag '(-> =>)) (shape? p '(:%1 :*)))
           (set! menu-submenus (+ menu-submenus 1))
           (walk-label (cadr p) (cons (cadr p) where))
           (walk-items (cddr p) (cons (cadr p) where) inert? depth))
          ((and (== tag 'if) (shape? p '(:%1 :*)))
           (when (call0 where "if predicate" (cadr p))
             (sub (cddr p))))
          ((and (== tag 'when) (shape? p '(:%1 :*)))
           (with ok? (or inert? (call0 where "when predicate" (cadr p)))
             (walk-items (cddr p) where (or inert? (not ok?)) depth)))
          ((and (== tag 'for) (shape? p '(:%1 :%1)))
           (with vals (call0 where "for values" (caddr p))
             (if (list? vals)
                 (for-each
                   (lambda (v)
                     (sub (guarded where "for body"
                                   (lambda () ((cadr p) v)) '())))
                   vals)
                 (menu-error where "for values are not a list" vals))))
          ((and (== tag 'promise) (shape? p '(:%1)))
           (with v (call0 where "promise" (cadr p))
             (if (match? v ':menu-item) (walk v where inert? depth)
                 (menu-error where "promise did not yield a menu" v))))
          ((and (== tag 'mini) (shape? p '(:%1 :*)))
           (call0 where "mini predicate" (cadr p))
           (sub (cddr p)))
          ((and (== tag 'refresh) (shape? p '(:%1 :string?))) (noop))
          ((and (== tag 'refreshable) (shape? p '(:%1 :*)))
           (call0 where "refreshable kind" (cadr p))
           (sub (cddr p)))
          ((and (== tag 'cached) (shape? p '(:%1 :%1 :*)))
           (call0 where "cached kind" (cadr p))
           (call0 where "cached validity" (caddr p))
           (sub (cdddr p)))
          ((and (in? tag '(glue)) (shape? p '(:boolean? :boolean? :integer? :integer?)))
           (noop))
          ((and (== tag 'color)
                (shape? p '(:%1 :boolean? :boolean? :integer? :integer?)))
           (noop))
          ((and (in? tag '(group text invisible)) (shape? p '(:%1))) (noop))
          ((and (== tag 'symbol) (shape? p '(:string? :*)))
           (when (nnull? (cddr p))
             (check-procedure where "symbol command" (caddr p) 0)))
          ((and (== tag 'texmacs-output) (shape? p '(:%2)))
           (call0 where "output document" (cadr p))
           (call0 where "output style" (caddr p)))
          ((and (== tag 'texmacs-input) (shape? p '(:%3)))
           (call0 where "input document" (cadr p))
           (call0 where "input style" (caddr p))
           (call0 where "input name" (cadddr p)))
          ((and (== tag 'enum) (shape? p '(:%3 :string?)))
           (check-procedure where "enum command" (cadr p) 1)
           (call0 where "enum values" (caddr p))
           (call0 where "enum value" (cadddr p)))
          ((and (== tag 'setting-enum) (shape? p '(:%5)))
           (check-procedure where "setting-enum command" (cadr p) 1)
           (call0 where "setting-enum values" (cadddr p))
           (call0 where "setting-enum value" (fifth p)))
          ((and (== tag 'setting-group) (shape? p '(:%1 :*)))
           (call0 where "setting-group name" (cadr p))
           (sub (cddr p)))
          ((and (in? tag '(choice choices)) (shape? p '(:%3)))
           (check-procedure where "choice command" (cadr p) 1)
           (call0 where "choice values" (caddr p))
           (call0 where "choice value" (cadddr p)))
          ((and (== tag 'filtered-choice) (shape? p '(:%4)))
           (check-procedure where "filtered-choice command" (cadr p) 2)
           (call0 where "filtered-choice values" (caddr p))
           (call0 where "filtered-choice value" (cadddr p))
           (call0 where "filtered-choice filter" (fifth p)))
          ((and (== tag 'color-input) (shape? p '(:%3)))
           (check-procedure where "color-input command" (cadr p) 1)
           (call0 where "color-input proposals" (cadddr p)))
          ((and (== tag 'tree-view) (shape? p '(:%3)))
           (call0 where "tree-view data" (caddr p))
           (call0 where "tree-view roles" (cadddr p)))
          ((and (== tag 'toggle) (shape? p '(:%2)))
           (check-procedure where "toggle command" (cadr p) 1)
           (call0 where "toggle value" (caddr p)))
          ((and (== tag 'setting-toggle) (shape? p '(:%3)))
           (check-procedure where "setting-toggle command" (cadr p) 1)
           (call0 where "setting-toggle value" (cadddr p)))
          ((and (in? tag '(horizontal vertical hlist vlist aligned tabs tab
                           icon-tabs icon-tab responsive-tabs responsive-tab
                           responsive-icon-tabs responsive-icon-tab minibar
                           scrollable))
                (shape? p '(:*)))
           (sub (cdr p)))
          ((and (== tag 'aligned-item) (shape? p '(:%2)))
           (sub (cdr p)))
          ((and (in? tag '(division class)) (shape? p '(:%1 :*)))
           (when (== tag 'division) (call0 where "division name" (cadr p)))
           (sub (cddr p)))
          ((and (== tag 'extend) (shape? p '(:%1 :*)))
           (sub (cdr p)))
          ((and (== tag 'style) (shape? p '(:%1 :*)))
           (sub (cddr p)))
          ((and (== tag 'tile) (shape? p '(:integer? :*)))
           (sub (cddr p)))
          ((and (== tag 'resize) (shape? p '(:%2 :*)))
           (call0 where "resize width" (cadr p))
           (call0 where "resize height" (caddr p))
           (sub (cdddr p)))
          ((and (in? tag '(hsplit vsplit)) (shape? p '(:%2)))
           (sub (cdr p)))
          ((and (== tag 'ink) (shape? p '(:%1)))
           (check-procedure where "ink command" (cadr p) 1))
          (else
            ;; as in make-menu-items, the item is read as a list of items
            (sub p)))))

(define (walk-menu thunk)
  ;; the errors found in the menu which thunk makes
  (set! menu-errors '())
  (with m (guarded '() "making the menu" thunk '())
    (walk-items m '() #f 0)
    (reverse menu-errors)))

(define (deliberate-error? e)
  ;; the debug menu provokes an error on purpose
  (string-occurs? "Miscellaneous > Provoke menu error: " e))

(define (check-menu name thunk)
  ;; one check: the menu expands without errors
  (let* ((all (walk-menu thunk))
         (errs (list-filter all (lambda (e) (not (deliberate-error? e)))))
         (n (length errs)))
    (for-each (lambda (e) (display* "    menu error: " e "\n")) errs)
    (check-report (null? errs) (string-append "the menu " name)
                  (string-append (number->string n) " errors, the first: "
                                 (if (null? errs) "" (car errs))))))

(define (check-menu-expand name m)
  ;; menu-expand gives the GUI the key of its cache of menu widgets; it
  ;; evaluates the conditions of the menu, but not its submenus
  (with r (check-run (lambda () (menu-expand `(vertical (link ,m)))))
    (check-report (and (pair? r) (== (car r) 'vertical))
                  (string-append "menu-expand of " (symbol->string m)
                                 " in " name)
                  (object->string r))))

(define (sub-term? x t)
  (or (== x t)
      (and (pair? t) (or (sub-term? x (car t)) (sub-term? x (cdr t))))))

(define (visible-labels m)
  ;; the labels of the submenus of the menu bar m, after the conditions
  (append-map
    (lambda (p)
      (cond ((and (pair? p) (in? (car p) '(=> ->)) (string? (cadr p)))
             (list (cadr p)))
            ((and (pair? p) (== (car p) 'if))
             (if ((cadr p)) (visible-labels (cddr p)) '()))
            ((and (pair? p) (== (car p) 'link))
             (visible-labels ((eval (cadr p)))))
            (else '())))
    m))

(define main-menus
  '(texmacs-menu texmacs-alternative-popup-menu texmacs-popup-menu
    texmacs-main-icons texmacs-mode-icons texmacs-focus-icons
    texmacs-extra-icons texmacs-extra-menu))

(define all-menus
  '(file-menu edit-menu insert-menu focus-menu format-menu document-menu
    view-menu go-menu tools-menu help-menu preferences-menu
    developer-menu debug-menu source-menu link-menu version-menu
    dynamic-menu tmdoc-menu db-menu))

(define tool-menus
  '(texmacs-bottom-tools texmacs-side-tools texmacs-left-tools))

(define (check-menus context roots)
  ;; as the GUI does before building the menus
  (lazy-initialize-force)
  (for-each
    (lambda (m)
      (check-menu (string-append (symbol->string m) " in " context)
                  (lambda () ((eval m)))))
    roots)
  (for-each
    (lambda (m)
      (check-menu-expand context m))
    main-menus)
  (with win (current-window)
    (for-each
      (lambda (m)
        (check-menu (string-append (symbol->string m) " in " context)
                    (lambda () ((eval m) win))))
      tool-menus)))

(define text-doc '(document "Hello world"))
(define math-doc
  '(document (concat "Let " (math (concat "x+" (frac "1" "2"))) ".")))
(define table-doc
  '(document (tabular (table (row (cell "a") (cell "b"))
                             (row (cell "c") (cell "d"))))))
(define prog-doc '(document (cpp-code (document "int x;"))))
(define graphics-doc '(document (graphics "" (point "0" "0"))))

(define standard-bar
  '("File" "Edit" "Insert" "Focus" "Format" "Document" "View" "Go" "Help"))

(define (test-menus-text)
  (check-group "menus in text")
  (in-buffer text-doc '(0 3)
    (lambda ()
      (check-true (in-text?))
      (with l (visible-labels (texmacs-menu))
        (check= (list-filter l (lambda (x) (in? x standard-bar))) standard-bar)
        (check-false (in? "Manual" l)))
      (check-menus "text" (append main-menus all-menus))
      ;; the menu bars as the GUI makes them
      (for-each
        (lambda (m)
          (check-true (make-menu-widget `(horizontal (link ,m)) 0)))
        '(texmacs-menu texmacs-main-icons texmacs-mode-icons
          texmacs-focus-icons))
      (check-true (make-menu-widget '(vertical (link file-menu)) 0)))))

(define (test-menus-math)
  (check-group "menus in math")
  (in-buffer math-doc '(0 1 0 0 1)
    (lambda ()
      (check-true (in-math?))
      (with l (visible-labels (texmacs-menu))
        (check= (list-filter l (lambda (x) (in? x standard-bar))) standard-bar))
      (check-menus "math" (append main-menus all-menus)))))

(define (test-menus-table)
  (check-group "menus in a table")
  (in-buffer table-doc '(0 0 0 0 0 0)
    (lambda ()
      (check-true (in-table?))
      (check-menus "table" (append main-menus all-menus)))))

(define (test-menus-prog)
  (check-group "menus in a program")
  (in-buffer prog-doc '(0 0 0 2)
    (lambda ()
      (check-true (in-prog?))
      (check-menus "program" (append main-menus all-menus)))))

(define (test-menus-graphics)
  (check-group "menus in graphics")
  (in-buffer graphics-doc '(0 1)
    (lambda ()
      (check-true (in-graphics?))
      ;; the menus of graphics work before the lazy menus are loaded
      ;; (when TeXmacs is idle, never without a window): texmacs-menu
      ;; links graphics-insert-menu and graphics-focus-menu of (graphics
      ;; graphics-menu), and the edit menu calls graphics-selection-active?
      ;; of (graphics graphics-group)
      (check-true (lazy-declared? '(graphics graphics-menu)
                                  'graphics-insert-menu))
      (check-true (lazy-declared? '(graphics graphics-menu)
                                  'graphics-focus-menu))
      (check-true (lazy-declared? '(graphics graphics-group)
                                  'graphics-selection-active?))
      (with l (visible-labels (texmacs-menu))
        (check-true (in? "Insert" l))
        (check-true (in? "Focus" l))
        (check-false (in? "Format" l)))
      (check-menus "graphics"
                   (append main-menus
                           '(graphics-insert-menu graphics-focus-menu
                             graphics-icons graphics-focus-icons))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-forms s)
  ;; the Scheme forms in the string s
  (call-with-input-string s
    (lambda (port)
      (let loop ((acc '()))
        (with x (read port)
          (if (eof-object? x) (reverse acc)
              (loop (cons x acc))))))))

(define (lazy-declarations kind)
  ;; the (module . names) of the declarations (kind module . names) at
  ;; the top level of init-texmacs.scm
  (with forms (read-forms (string-load "$TEXMACS_PATH/progs/init-texmacs.scm"))
    (map cdr (list-filter forms
                          (lambda (x) (and (pair? x) (== (car x) kind)
                                           (pair? (cdr x))))))))

(define (lazy-declared? module name)
  ;; is name declared by lazy-menu or lazy-define of module?
  (with decls (append (lazy-declarations 'lazy-menu)
                      (lazy-declarations 'lazy-define))
    (list-or (map (lambda (d) (and (== (car d) module) (in? name (cdr d))))
                  decls))))

(define (lazy-stub? f)
  ;; a function of lazy-define which the module did not replace
  (and (procedure? f)
       (with src (procedure-source f)
         (and src (string-occurs? "Could not retrieve"
                                  (object->string src))))))

(define (menu-name? name)
  ;; lazy-menu also declares commands, such as open-preferences, which
  ;; open windows and are not run here
  (with s (symbol->string name)
    (or (string-ends? s "-menu") (string-ends? s "-icons")
        (string-ends? s "-toolbar") (string-ends? s "-toolbars")
        (== s "print-menu-inline"))))

(define (defined-by-module? name)
  (with f (catch #t (lambda () (eval name)) (lambda args #f))
    (and (procedure? f) (not (lazy-stub? f)))))

;; FIXME: these menus are declared by lazy-menu in init-texmacs.scm, but
;; their modules do not define them: graphics-menu (init-texmacs.scm:323,
;; (graphics graphics-menu)), prog-menu (init-texmacs.scm:283, (prog
;; prog-menu)), plugin-eval-menu, plugin-eval-toggle-menu and
;; plugin-plot-menu (init-texmacs.scm:363, (dynamic scripts-menu)). Once
;; the module is loaded, the stub of lazy-define finds itself in the public
;; interface and calls itself for ever: (prog-menu) hangs TeXmacs,
;; expected the menu, or an error. Nothing calls them in TeXmacs itself.
(define stale-lazy-menus
  '(graphics-menu prog-menu plugin-eval-menu plugin-eval-toggle-menu
    plugin-plot-menu))

;; Each menu which init-texmacs.scm declares with lazy-menu is a function
;; which loads its module and is then replaced by the function of the
;; module. The GUI loads these modules when it is idle (and the suite does
;; it here); a name which the module does not define stays a stub, which
;; calls itself for ever once the module is loaded.
(define (test-lazy-menus)
  (check-group "lazy menus")
  (with decls (lazy-declarations 'lazy-menu)
    (check-true (>= (length decls) 30))
    (for-each
      (lambda (decl)
        (let* ((module (car decl))
               (names (cdr decl))
               (loaded (check-run (lambda () (module-provide module) #t)))
               (names* (list-difference names stale-lazy-menus))
               (missing (list-filter names*
                                     (lambda (n) (not (defined-by-module? n))))))
          (check-report (and (== loaded #t) (null? missing))
                        (string-append "the menus of " (object->string module)
                                       " are defined")
                        (string-append "loading: " (object->string loaded)
                                       ", not defined: "
                                       (object->string missing)))))
      decls))
  ;; the walker sees a missing tool
  (check-true (string-occurs? "Missing '"
                (object->string (texmacs-side-tool (current-window)
                                                   '(kbd-menu-no-such-tool)
                                                   :title))))
  ;; the side tools of lazy-tool: texmacs-side-tool knows them once their
  ;; module is loaded, and those without other arguments than the window
  ;; expand without errors (in a table, for the table tools)
  (in-buffer table-doc '(0 0 0 0 0 0)
    (lambda ()
      (with win (current-window)
        (for-each
          (lambda (decl)
            (check-run (lambda () (module-provide (car decl))))
            (for-each
              (lambda (tool)
                (let* ((name (if (pair? tool) (car tool) tool))
                       (side (lambda () (texmacs-side-tool win (list name)
                                                           :title)))
                       (r (check-run side))
                       (f (catch #t (lambda () (eval name)) (lambda a #f))))
                  (check-report (not (string-occurs? "Missing '"
                                                     (object->string r)))
                                (string-append "the tool "
                                               (symbol->string name))
                                (object->string r))
                  (when (and (procedure? f) (arity-ok? f 1)
                             (not (arity-ok? f 2)))
                    (check-menu (string-append "the tool "
                                               (symbol->string name))
                                side))))
              (cdr decl)))
          (lazy-declarations 'lazy-tool)))))
  ;; once loaded, the menus (and icon bars) of each module expand without
  ;; errors, in text
  (in-buffer text-doc '(0 3)
    (lambda ()
      (for-each
        (lambda (decl)
          (let* ((names (list-filter (cdr decl)
                          (lambda (n)
                            (and (nin? n stale-lazy-menus)
                                 (menu-name? n)
                                 (defined-by-module? n)
                                 (arity-ok? (eval n) 0)))))
                 (menus (lambda ()
                          (map (lambda (n)
                                 (cons* '-> (symbol->string n)
                                        (guarded (list (symbol->string n))
                                                 "making the menu"
                                                 (eval n) '())))
                               names))))
            ;; FIXME: (path->tree #f) crashes TeXmacs (segmentation
            ;; fault) instead of raising an error: tmscm_is_path takes the
            ;; car of anything which is not the empty list (glue.cpp:505-508).
            ;; graphics-focus-icons outside graphics comes to it through
            ;; (graphics-get-property "gr-color"), where graphics-graphics-path
            ;; is #f (graphics-utils.scm:335). The icons of graphics are
            ;; checked in graphics above.
            (when (and (nnull? names)
                       (!= (car decl) '(graphics graphics-menu)))
              (check-menu (string-append "the menus of "
                                         (object->string (car decl)))
                          menus))))
        (lazy-declarations 'lazy-menu)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kbd-menu-test-flag #f)

(menu-bind kbd-menu-test-menu
  ("First" (noop))
  ---
  (-> "Sub"
      ("Second" (noop))
      ("Third" (noop)))
  (if kbd-menu-test-flag ("Hidden" (noop)))
  (when kbd-menu-test-flag ("Greyed" (noop)))
  (link kbd-menu-test-linked))

(menu-bind kbd-menu-test-linked
  ("Linked" (noop)))

(tm-menu (kbd-menu-test-loop l)
  (for (x l)
    ((eval x) (noop))))

;; menu-bind defines a function which gives the menu as a list of items:
;; a button is its label and a procedure without arguments (the action,
;; which is not run), a submenu is (-> label . items), the conditions are
;; procedures; the walker of the suite finds errors which it is shown.
(define (test-menu-define)
  (check-group "defining menus")
  (with m (kbd-menu-test-menu)
    (check= (length m) 6)
    (check= (car (list-ref m 0)) "First")
    (check-true (procedure? (cadr (list-ref m 0))))
    (check= (promise-source (cadr (list-ref m 0))) '(noop))
    (check= (list-ref m 1) '---)
    (check= (car (list-ref m 2)) '->)
    (check= (cadr (list-ref m 2)) "Sub")
    (check= (map car (cddr (list-ref m 2))) '("Second" "Third"))
    (check= (car (list-ref m 3)) 'if)
    (check-true (procedure? (cadr (list-ref m 3))))
    (check= (car (list-ref m 4)) 'when)
    (check= (list-ref m 5) '(link kbd-menu-test-linked))
    (check-true (match? m ':menu-item-list))
    (check= (walk-menu kbd-menu-test-menu) '()))
  ;; menu-expand evaluates the conditions and links
  (set! kbd-menu-test-flag #f)
  ;; (the buttons of the active items lose their action, those of the
  ;; inactive ones keep its source)
  (with r (menu-expand '(vertical (link kbd-menu-test-menu)))
    (check= (car r) 'vertical)
    (check-true (sub-term? '("First") r))
    (check-false (sub-term? '("Hidden") r))
    (check-true (sub-term? '("Linked") r))
    (check-true (sub-term? '(when #f ("Greyed" (lambda () (noop)))) r))
    (check-true (sub-term? '(-> "Sub" ("Second" (lambda () (noop)))
                                ("Third" (lambda () (noop))))
                           r)))
  (set! kbd-menu-test-flag #t)
  (with r (menu-expand '(vertical (link kbd-menu-test-menu)))
    (check-true (sub-term? '("Hidden") r))
    (check-true (sub-term? '(when #t ("Greyed")) r)))
  (set! kbd-menu-test-flag #f)
  ;; a menu with an argument
  (with m (kbd-menu-test-loop '("a" "b" "c"))
    (check= (length m) 1)
    (check= (car (car m)) 'for)
    (check= (walk-menu (lambda () m)) '())
    (check= (map car (append-map (cadr (car m)) ((caddr (car m)))))
            '("a" "b" "c")))
  ;; the walker reports the errors which make-menu-widget would hit
  (check= (length (walk-menu (lambda () (list (list "Bad" 42))))) 1)
  (check= (length (walk-menu (lambda () (list (list "Bad" (lambda (x) x))))))
          1)
  (check= (length (walk-menu (lambda () '((link kbd-menu-test-undefined))))) 1)
  (check= (length (walk-menu
                   (lambda () (list (list 'if (lambda () (car '()))
                                          (list "x" (lambda () (noop))))))))
          1)
  (check= (length (walk-menu (lambda () '(42)))) 1)
  (check= (length (walk-menu (lambda () '((-> "Sub" 42))))) 1)
  (check= (length (walk-menu (lambda () (list (list 'for (lambda (x) '())
                                                    (lambda () 42))))))
          1)
  (check= (length (walk-menu (lambda () '(((icon "tm-no-such-icon.xpm")
                                           (noop))))))
          2)
  (check= (walk-menu (lambda () (list (list '(icon "tm_new.xpm")
                                            (lambda () (noop))))))
          '())
  ;; the deliberate error of the debug menu is found
  (check-true (list-find (walk-menu debug-menu) deliberate-error?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-menu-test-failures)
  (:synopsis "Run the tests of the keyboard maps and menus")
  (check-suite "kbd-menu")
  (test-rewrite)
  (test-bindings-generic)
  (test-bindings-text)
  (test-bindings-math)
  (test-typing-text)
  (test-typing-math)
  (test-typing-math-mode)
  (test-typing-table)
  (test-kbd-map)
  ;; the keyboard maps of programs come last: see test-typing-math-mode
  (test-bindings-prog)
  (test-typing-prog)
  (test-menu-define)
  (test-menus-text)
  (test-menus-math)
  (test-menus-table)
  (test-menus-prog)
  (test-menus-graphics)
  (test-lazy-menus)
  (check-group "menu items")
  ;; all the menus above, each time they were expanded
  (check-true (> menu-buttons 10000))
  (check-true (> menu-submenus 1000))
  (check-end))
