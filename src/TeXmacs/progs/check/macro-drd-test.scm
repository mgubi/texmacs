;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : macro-drd-test.scm
;; DESCRIPTION : tests of the macro language, the style packages and the DRD
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Three layers of TeXmacs markup, without a window:
;;
;;   - the macro language of the style files (Typeset/Env/env_exec.cpp):
;;     macros and their arguments, quoting and evaluation, Scheme calls
;;     (extern), the environment (assign, with, provide, value), the
;;     primitives on strings and tuples, the number formats, the dates and
;;     the counters of std-counter.ts. texmacs-expand evaluates in the
;;     environment at the start of the current buffer and forgets the
;;     assignments afterwards; texmacs-exec* evaluates in the environment
;;     at the cursor. typeset-test.scm checks the basic arithmetic,
;;     conditionals, macros and lengths, which are not repeated here;
;;   - the style packages: every .ts file of TeXmacs/styles and
;;     TeXmacs/packages is executed with use-package, and its top level
;;     definitions must be there afterwards; the main styles are set on a
;;     buffer, typeset, and their typical macros expand as expected;
;;   - the DRD (Data/Drd): arity, types and accessibility of the children
;;     of the tags, as the builtin tags, the style packages (drd-props) and
;;     the heuristics on the macros of a document give them, the tag groups
;;     of the Scheme side (define-group in text-drd, math-drd...) and the
;;     cursor, which is accessible only in the accessible children.
;;
;; A buffer whose style is changed by buffer-set gets a new DRD when the
;; style was not computed before in the session, and the DRD used by the
;; Scheme queries (the_drd) is the one of the view selected last: the
;; suite selects the view again after setting the style, as switching
;; buffers in the editor does.
;;
;; The temporary files go to macro-drd-tmp in the temporary directory.

(texmacs-module (check macro-drd-test)
  (:use (check check-lib)
        (utils library tree)
        (utils edit variants)
        (text text-drd)
        (math math-drd)
        (source source-drd)
        (dynamic dynamic-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ev t)
  ;; the value of the TeXmacs expression @t, as a Scheme tree
  (tree->stree (texmacs-expand t)))

(define (ev-last t)
  ;; the value of the last item of the concatenation @t
  (with r (ev t)
    (if (and (pair? r) (== (car r) 'concat)) (cAr r) r)))

(define (T x) (stree->tree x))

(define (at . l)
  ;; the absolute path of @l in the current buffer
  (append (buffer-path) l))

(define (rel p)
  ;; the path @p relative to the current buffer
  (list-tail p (length (buffer-path))))

(define (cursor) (rel (cursor-path)))

(define (sub . l) (path->tree (apply at l)))

(define (nr-errors)
  ;; the number of error and warning messages so far
  (tree-arity (get-debug-messages "Errors" 1000000)))

(define macro-drd-dir
  (string-append (url->system (url-temp-dir)) "/macro-drd-tmp"))

(define (tmp-file name)
  (string-append macro-drd-dir "/" name))

(define (fresh-tmp-dir)
  (let ((d (system->url macro-drd-dir)))
    (when (url-exists? d)
      (for (f (url-read-directory d "*"))
        (system-remove f))
      (system-rmdir d))
    (system-mkdir d)))

(define (remove-tmp-dir)
  (let ((d (system->url macro-drd-dir)))
    (when (url-exists? d)
      (for (f (url-read-directory d "*"))
        (system-remove f))
      (system-rmdir d))))

(define (with-document doc thunk)
  ;; run @thunk in a new buffer holding the document @doc (with its style
  ;; and preamble), typeset, then close the buffers it opened
  (let* ((old (current-buffer))
         (before (buffer-list))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set u (stree->tree doc))
    (update-current-buffer)
    (update-forced)
    ;; the Scheme queries of the DRD use the one of the view selected
    ;; last, which a new style may have replaced: select the view again
    (when (buffer-exists? old)
      (switch-to-buffer old)
      (switch-to-buffer u))
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (for (b (buffer-list))
        (when (nin? b before) (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (doc-with style body . init)
  ;; a document of style @style (a list) with @body and the macros @init
  `(document (TeXmacs "2.1") (style (tuple ,@style))
             (body ,body)
             ,@(if (null? init) '()
                   `((initial (collection
                               ,@(map (lambda (x) `(associate ,@x))
                                      init)))))))

(define (in-generic thunk)
  (with-document (doc-with '("generic") '(document "")) thunk))

(define (with-exact-environments thunk)
  ;; get-env-tree-at and texmacs-exec* with the full evaluation of the
  ;; document up to the path, which sees the counters (see typeset-test)
  (let ((fast? (== (get-preference "fast environments") "on")))
    (set-fast-environments #f)
    (with r (check-run thunk)
      (set-fast-environments fast?)
      r)))

;; Scheme functions for extern; a macro may only call secure functions
(tm-define (macro-drd-test-wrap x)
  (:secure #t)
  (string-append "<" (tree->string x) ">"))

(tm-define (macro-drd-test-swap x y)
  (:secure #t)
  `(concat ,(tree->string y) ,(tree->string x)))

;; a tag group of the suite, as the drd files of progs define them
(define-group macro-drd-test-group
  macro-drd-test-a macro-drd-test-b (theorem-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The macro language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macro names its arguments, xmacro has one name for the list of all
;; arguments, reached by number with arg or quote-arg; arg with a path
;; goes into an argument; map-args applies a macro to the arguments (from
;; a first and up to a last one); get-label and get-arity look at an
;; argument; compound applies a macro given by name or as a value.
(define (test-macro-arguments)
  (check-group "macro arguments")
  (check= (ev '(compound (macro "a" "b" (concat (arg "b") (arg "a"))) "1" "2"))
          "21")
  (check= (ev '(compound "strong" "x"))
          '(with "font-series" "bold" "math-font-series" "bold" "x"))
  (check= (ev '(compound (macro "x") "1")) "x")
  (check= (ev '(with "f" (macro "x" (arg "x" "1")) (f (tuple "a" "b"))))
          '(with "f" (quote (macro "x" (arg "x" "1"))) "b"))
  (check= (cAr (ev '(with "f" (macro "x" (arg "x" "1" "0"))
                      (f (tuple "a" (tuple "b" "c"))))))
          "b")
  ;; an arg whose path leaves the argument is an error, which names it
  (check= (cAr (ev '(with "f" (macro "x" (arg "x" "5")) (f (tuple "a")))))
          '(error "arg x"))
  (check= (cAr (ev '(with "f" (macro "x" (arg "x" "0" "0")) (f (tuple "a")))))
          '(error "arg x"))
  (check= (cAr (ev '(with "f" (xmacro "x" (concat (arg "x" "2") (arg "x" "0")))
                      (f "a" "b" "c"))))
          "ca")
  (check= (cAr (ev '(with "f" (xmacro "x" (get-arity (quote-arg "x")))
                      (f "a" "b" "c"))))
          "3")
  (check= (cAr (ev '(with "f" (xmacro "x" (get-label (quote-arg "x")))
                      (f "a"))))
          "f")
  (check= (cAr (ev '(with "f" (macro "x" (get-label (arg "x")))
                      (f (frac "a" "b")))))
          "frac")
  (check= (cAr (ev '(with "f" (macro "x" (get-arity (arg "x")))
                      (f (tuple "a" "b" "c" "d")))))
          "4")
  (check= (cAr (ev '(with "f" (macro "x" (quote-arg "x")) (f (plus "1" "2")))))
          '(plus "1" "2"))
  (check= (cAr (ev '(with "f" (macro "x" (arg "x")) (f (plus "1" "2")))))
          "3")
  (check= (cAr (ev '(with "f" (xmacro "x" (map-args "strong" "concat" "x"))
                      (f "a" "b"))))
          '(concat (with "font-series" "bold" "math-font-series" "bold" "a")
                   (with "font-series" "bold" "math-font-series" "bold" "b")))
  (check= (cAr (ev '(with "f" (xmacro "x" (map-args "g" "tuple" "x" "1"))
                          "g" (macro "y" (concat "[" (arg "y") "]"))
                      (f "a" "b" "c"))))
          '(tuple "[b]" "[c]"))
  (check= (cAr (ev '(with "f" (xmacro "x" (map-args "g" "tuple" "x" "1" "2"))
                          "g" (macro "y" (concat "[" (arg "y") "]"))
                      (f "a" "b" "c"))))
          '(tuple "[b]"))
  ;; a macro with an argument it does not use
  (check= (cAr (ev '(with "f" (macro "a" "b" (arg "a")) (f "x" "y")))) "x")
  ;; an unknown macro is an error, which names it
  (check= (ev '(no-such-macro-xyz "a")) '(error "compound no-such-macro-xyz"))
  (check= (ev '(arg "x")) '(error "arg x")))

;; quote leaves its argument as it is, eval evaluates it again; quasi
;; evaluates the unquoted parts and then the whole, quasiquote only the
;; unquoted parts; unquote* splices a tuple; expand-as shows its second
;; argument.
(define (test-quoting)
  (check-group "quoting and evaluation")
  (check= (ev '(quote (plus "1" "2"))) '(plus "1" "2"))
  (check= (ev '(eval (quote (plus "1" "2")))) "3")
  (check= (ev '(eval "abc")) "abc")
  (check= (ev '(quasi (tuple (unquote (plus "1" "2")) (plus "1" "2"))))
          '(tuple "3" "3"))
  (check= (ev '(quasiquote (tuple (unquote (plus "1" "2")) (plus "1" "2"))))
          '(tuple "3" (plus "1" "2")))
  (check= (ev '(quasi (tuple (unquote* (tuple "a" "b")) "c")))
          '(tuple "a" "b" "c"))
  (check= (ev '(quasiquote (tuple (unquote* (tuple "a" "b")) "c")))
          '(tuple "a" "b" "c"))
  (check= (cAr (ev '(with "x" "5" (quasi (tuple (unquote (value "x"))
                                                (value "x"))))))
          '(tuple "5" "5"))
  ;; a macro built with quasi: the name of the variable comes from the
  ;; argument (as new-counter does)
  (check= (ev '(concat (assign "mk" (macro "n" (quasi (assign (unquote (merge (arg "n") "-x")) "7"))))
                       (mk "q") (value "q-x")))
          '(concat (assign "mk" (macro "n" (quasi (assign (unquote (merge (arg "n") "-x")) "7"))))
                   (assign "q-x" "7") "7"))
  (check= (ev '(expand-as "a" "b")) "b")
  (check= (ev '(eval-args "x")) '(error "nil argument")))

;; extern calls a secure Scheme function with the trees of its arguments;
;; the result is a string or a Scheme tree. A function which is not
;; defined gives an error.
(define (test-extern)
  (check-group "extern")
  (check= (ev '(extern "macro-drd-test-wrap" "abc")) "<abc>")
  (check= (ev '(extern "macro-drd-test-swap" "a" "b")) "ba")
  (check= (ev '(extern "macro-drd-test-wrap" (merge "a" "b"))) "<ab>")
  (check= (ev '(extern "(lambda (x) \"z\")" "a")) "z")
  (check= (cAr (ev '(with "f" (macro "x" (extern "macro-drd-test-wrap" (arg "x")))
                      (f "y"))))
          "<y>")
  (check= (ev '(extern "macro-drd-test-no-such-function" "a"))
          '(error "compound unbound-variable")))

;; assign changes the environment for what follows (until the end of the
;; evaluation), with only for its body; value reads a variable and
;; quote-value reads it without evaluation, or-value takes the first
;; variable which is defined. The arguments of a macro are evaluated where
;; they are used, in the environment of the body of the macro.
(define (test-environment)
  (check-group "environment")
  (check= (ev '(concat (assign "x" "1") (with "x" "2" (value "x")) (value "x")))
          '(concat (assign "x" "1") (with "x" "2" "2") "1"))
  (check= (ev '(concat (with "x" "2" (assign "x" "3")) (value "x")))
          '(concat (with "x" "2" (assign "x" "3")) (uninit)))
  (check= (ev '(with "x" "1" (concat (assign "x" "2") (value "x"))))
          '(with "x" "1" (concat (assign "x" "2") "2")))
  (check= (ev '(concat (assign "x" "1") (assign "x" (plus (value "x") "1"))
                       (value "x")))
          '(concat (assign "x" "1") (assign "x" "2") "2"))
  ;; the values of a with are evaluated before any of them is set
  (check= (ev '(with "x" "a" "y" (value "x") (value "y")))
          '(with "x" "a" "y" (quote (uninit)) (uninit)))
  (check= (ev '(value "no-such-variable-xyz")) '(uninit))
  (check= (ev '(provides "no-such-variable-xyz")) "false")
  (check= (ev '(concat (provide "macro-drd-new" "12") (value "macro-drd-new")))
          '(concat (assign "macro-drd-new" "12") "12"))
  ;; FIXME: provide assigns a variable which is already defined
  ;; (Typeset/Env/env_exec.cpp:625 tests provides (t->label), the label of
  ;; the provide tree, instead of r->label): (concat (provide "x" "1")
  ;; (provide "x" "2") (value "x")) gives "2", expected "1", and
  ;; (concat (provide "font-base-size" "12") (value "font-base-size"))
  ;; gives "12", expected "10".
  (check= (ev '(quote-value "font-base-size")) "10")
  (check= (ev '(with "q" (plus "1" "2") (quote-value "q")))
          '(with "q" "3" "3"))
  (check= (ev '(or-value "no-such-variable-xyz" "font-base-size")) "10")
  (check= (ev '(or-value "no-such-variable-xyz" "no-such-variable-zyx")) "")
  (check= (ev '(with "f" (macro "x" (with "y" "2" (concat (arg "x") (value "y"))))
                     "y" "1"
                 (concat (f (value "y")) (value "y"))))
          '(with "f" (quote (macro "x" (with "y" "2" (concat (arg "x") (value "y")))))
                 "y" "1" (concat (with "y" "2" "22") "1")))
  ;; a macro is a value too
  (check= (ev '(with "f" (macro "x" (arg "x")) (is-tuple (value "f"))))
          '(with "f" (quote (macro "x" (arg "x"))) "false"))
  (check= (ev '(drd-props "foo" "arity" "2")) '(drd-props "foo" "arity" "2")))

;; The primitives on strings and tuples.
(define (test-strings-tuples)
  (check-group "strings and tuples")
  (check= (ev '(look-up (tuple "a" "b" "c") "0")) "a")
  (check= (ev '(look-up (tuple "a" "b" "c") "2")) "c")
  (check= (ev '(look-up (tuple "a" "b" "c") "5"))
          '(error "index out of range in look up"))
  (check= (ev '(range (tuple "a" "b" "c" "d") "1" "3")) '(tuple "b" "c"))
  (check= (ev '(range (tuple "a" "b") "0" "9")) '(tuple "a" "b"))
  (check= (ev '(range "abcdef" "4" "10")) "ef")
  (check= (ev '(range "abcdef" "3" "1")) "")
  (check= (ev '(range "abcdef" "0" "0")) "")
  (check= (ev '(range "abc" "a" "1")) '(error "bad range"))
  (check= (ev '(merge "a" "b" "c")) "abc")
  (check= (ev '(merge (tuple "a") (tuple "b" "c"))) '(tuple "a" "b" "c"))
  (check= (ev '(merge "a" "")) "a")
  (check= (ev '(length "")) "0")
  (check= (ev '(length (tuple))) "0")
  (check= (ev '(length (tuple "a" (tuple "b" "c")))) "2")
  (check= (ev '(is-tuple (tuple))) "true")
  (check= (ev '(equal (tuple "a") (tuple "a"))) "true")
  (check= (ev '(equal (tuple "a") (tuple "b"))) "false")
  ;; occurs-inside looks for a tree in an argument of the macro, which it
  ;; names; a string is a leaf, this is not a substring test
  (check= (cAr (ev '(with "f" (macro "x" (occurs-inside "b" "x"))
                      (f (tuple "a" (strong "b"))))))
          "true")
  (check= (cAr (ev '(with "f" (macro "x" (occurs-inside "z" "x"))
                      (f (tuple "a" "b")))))
          "false")
  (check= (cAr (ev '(with "f" (macro "x" (occurs-inside "b" "x")) (f "abc"))))
          "false")
  (check= (ev '(change-case "hello World" "UPCASE")) "HELLO WORLD")
  (check= (ev '(change-case "Hello World" "locase")) "hello world")
  (check= (ev '(change-case "hello world" "Upcase")) "Hello world")
  (check= (ev '(change-case "hello" "first")) "h")
  (check= (ev '(change-case (concat "ab" "cd") "UPCASE")) "ABCD")
  (check= (ev '(translate "Theorem" "english" "german")) "Satz")
  (check= (ev '(translate "Theorem" "english" "english")) "Theorem")
  (check= (ev '(translate "Theorem" "english" "french"))
          (string-append "Th" (string (integer->char 233)) "or"
                         (string (integer->char 232)) "me"))
  (check= (ev '(with "language" "german" (localize "Theorem")))
          '(with "language" "german" "Satz"))
  (check= (ev '(while "false" "x")) "")
  (check= (ev '(xor "true" "true")) "false")
  (check= (ev '(xor "true" "false")) "true")
  (check= (ev '(copy "abc")) "abc"))

;; The number formats: roman numbers up to 3999, letters beyond z, the
;; footnote symbols, and an error for an unknown format.
(define (test-numbers)
  (check-group "number formats")
  (check= (ev '(number "1994" "roman")) "mcmxciv")
  (check= (ev '(number "1994" "Roman")) "MCMXCIV")
  (check= (ev '(number "3999" "Roman")) "MMMCMXCIX")
  (check= (ev '(number "4000" "roman")) "?")
  (check= (ev '(number "0" "roman")) "o")
  (check= (ev '(number "-4" "roman")) "-iv")
  (check= (ev '(number "26" "alpha")) "z")
  (check= (ev '(number "27" "alpha")) "aa")
  (check= (ev '(number "52" "alpha")) "az")
  (check= (ev '(number "53" "Alpha")) "BA")
  (check= (ev '(number "702" "alpha")) "zz")
  (check= (ev '(number "703" "alpha")) "aaa")
  (check= (ev '(number "0" "alpha")) "0")
  (check= (ev '(number "007" "arabic")) "7")
  (check= (ev '(number "1" "fnsymbol")) '(with "mode" "math" (rigid "<asterisk>")))
  (check= (ev '(number "3" "fnsymbol")) '(with "mode" "math" (rigid "<ddag>")))
  (check= (ev '(number "7" "fnsymbol"))
          '(with "mode" "math" (rigid "<asterisk><asterisk>")))
  ;; there is no Chinese (hanzi) format among the primitive ones
  (check= (ev '(number "3" "hanzi")) '(error "bad number"))
  (check= (ev '(number "3" "nonsense")) '(error "bad number"))
  (check= (ev '(number (tuple "3") "roman")) '(error "bad number")))

;; The date with a strftime format (starting with %) or a Qt one, and in
;; the default format of a language; the current date is compared with
;; the one of Scheme.
(define (test-dates)
  (check-group "dates")
  (let* ((now (localtime (current-time)))
         (fmt (lambda (f) (strftime f now))))
    (check= (ev '(date "%Y-%m-%d")) (fmt "%Y-%m-%d"))
    (check= (ev '(date "%Y")) (fmt "%Y"))
    (check= (ev '(date "yyyy")) (fmt "%Y"))
    (check= (ev '(date "MM/dd" "english")) (fmt "%m/%d"))
    (check= (ev '(date "MMMM" "english")) (fmt "%B"))
    (check= (ev '(date)) (ev '(date "MMMM d, yyyy" "british")))
    (check= (ev '(date "" "german")) (ev '(date "d. MMMM yyyy" "german")))
    (check= (ev '(date "" "french")) (ev '(date "d MMMM yyyy" "french")))
    (check= (ev '(date "" "chinese"))
            (string-append (fmt "%Y") "<#5e74>"
                           (number->string (+ 1 (tm:mon now))) "<#6708>"
                           (number->string (tm:mday now)) "<#65e5>"))
    (check= (ev '(with "language" "german" (date "" "german")))
            `(with "language" "german" ,(ev '(date "" "german"))))
    (check= (ev '(date "a" "b" "c")) '(error "bad date"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-counter . l)
  ;; the value of @l after the creation of the counter mdtc
  (ev-last `(concat (new-counter "mdtc") ,@l)))

;; new-counter defines the variable mdtc-nr and the macros display-,
;; counter-, the-, reset-, inc- and next-mdtc; next- increments the
;; counter, the- shows it with display-, reset- sets it to 0; the
;; generic macros inc-counter, value-counter, reset-counter and
;; next-counter take the name of the counter. The counters of the
;; standard styles: a subsection is reset by its section, the theorems
;; share one counter.
(define (test-counters)
  (check-group "counters")
  (check= (cadr (ev '(new-counter "mdtc"))) '(assign "mdtc-nr" "0"))
  (check= (map cadr (cdr (ev '(new-counter "mdtc"))))
          '("mdtc-nr" "display-mdtc" "counter-mdtc" "the-mdtc" "reset-mdtc"
            "inc-mdtc" "next-mdtc"))
  (check= (ev '(provides "the-mdtc")) "false")
  (check= (with-counter '(provides "the-mdtc")) "true")
  (check= (with-counter '(provides "next-mdtc")) "true")
  (check= (with-counter '(provides "display-mdtc")) "true")
  (check= (with-counter '(the-mdtc)) "0")
  (check= (with-counter '(next-mdtc) '(next-mdtc) '(the-mdtc)) "2")
  (check= (with-counter '(next-mdtc) '(next-mdtc) '(value "mdtc-nr")) "2")
  (check= (with-counter '(next-mdtc) '(reset-mdtc) '(the-mdtc)) "0")
  (check= (with-counter '(next-mdtc) '(reset-mdtc) '(inc-mdtc) '(the-mdtc)) "1")
  (check= (with-counter '(inc-mdtc) '(inc-mdtc) '(inc-mdtc) '(the-mdtc)) "3")
  (check= (with-counter '(assign "display-mdtc" (macro "nr" (number (arg "nr") "roman")))
                        '(next-mdtc) '(next-mdtc) '(next-mdtc) '(the-mdtc))
          "iii")
  (check= (with-counter '(assign "display-mdtc" (macro "nr" (number (arg "nr") "Alpha")))
                        '(inc-counter "mdtc") '(the-mdtc))
          "A")
  ;; value-counter is the name of the variable of the counter
  (check= (with-counter '(value-counter "mdtc")) "mdtc-nr")
  (check= (with-counter '(inc-counter "mdtc") '(inc-counter "mdtc")
                        '(value (value-counter "mdtc")))
          "2")
  (check= (with-counter '(inc-counter "mdtc") '(reset-counter "mdtc")
                        '(the-mdtc))
          "0")
  (check= (with-counter '(next-counter "mdtc") '(next-counter "mdtc")
                        '(the-mdtc))
          "2")
  ;; next- also sets the binding of a following label
  (check= (ev '(concat (new-counter "mdtc") (next-mdtc)))
          '(concat (assign "mdtc-nr" "0")
                   (assign "display-mdtc" (macro "x" (arg "x")))
                   (assign "counter-mdtc" (macro "mdtc-nr"))
                   (assign "the-mdtc" (macro (compound "display-mdtc"
                                                       (value (compound "counter-mdtc")))))
                   (assign "reset-mdtc" (macro (assign (compound "counter-mdtc") "0")))
                   (assign "inc-mdtc" (macro (assign (compound "counter-mdtc")
                                                     (plus (value (compound "counter-mdtc")) "1"))))
                   (assign "next-mdtc" (macro (concat (compound "inc-mdtc")
                                                      (set-binding (compound "the-mdtc")))))
                   (assign "mdtc-nr" "1")
                   (hidden-binding (tuple) "1")))
  ;; the counters of the standard styles
  (check= (ev '(the-section)) "0")
  (check= (ev-last '(concat (next-section) (next-section) (the-section))) "2")
  (check= (ev-last '(concat (next-section) (next-subsection) (next-subsection)
                            (the-subsection)))
          "2")
  (check= (ev-last '(concat (next-section) (next-subsection) (next-section)
                            (the-subsection)))
          "1")
  (check= (ev-last '(concat (next-section) (next-section) (next-subsection)
                            (value "section-nr")))
          "2")
  (check= (ev-last '(concat (next-equation) (the-equation))) "1")
  (check= (ev-last '(concat (next-theorem) (next-lemma) (the-theorem))) "2")
  (check= (ev-last '(concat (next-theorem) (next-theorem)
                            (value "theorem-env-nr")))
          "2")
  (check= (ev-last '(concat (next-footnote) (next-footnote) (the-footnote))) "2")
  ;; the assignments of texmacs-expand do not stay
  (check= (ev '(value "section-nr")) "0"))

;; In a typeset document, the assignments of a paragraph are seen by the
;; following ones: a counter of the document, the counters of the
;; sections, the labels which next- binds.
(define (test-document-counters)
  (check-group "counters in a document")
  (with-document
   (doc-with '("generic")
             '(document (new-counter "mdtc")
                        (concat (next-mdtc) (label "c1"))
                        (concat (next-mdtc) (next-mdtc) (label "c3"))
                        (section "A") (section "B")
                        (concat "x" (assign "mdtv" "5"))
                        "y"))
   (lambda ()
     (with-exact-environments
      (lambda ()
        (check= (tree->stree (get-env-tree-at "mdtc-nr" (at 1 0))) "0")
        (check= (tree->stree (get-env-tree-at "mdtc-nr" (at 2 0))) "1")
        (check= (tree->stree (get-env-tree-at "mdtc-nr" (at 3 0 0))) "3")
        (check= (tree->stree (get-env-tree-at "mdtc-nr" (at 5 0))) "3")
        (check= (tree->stree (get-env-tree-at "section-nr" (at 6 0))) "2")
        (check= (tree->stree (get-env-tree-at "mdtv" (at 6 0))) "5")
        (go-to (at 6 0))
        (check= (tree->stree (texmacs-exec* '(the-mdtc))) "3")
        (check= (tree->stree (texmacs-exec* '(plus (value "mdtc-nr") "1"))) "4")
        (check= (tree->stree (texmacs-exec* '(value "section-nr"))) "2")
        (check= (tree->stree (texmacs-exec* '(value "mdtv"))) "5")))
     (check= (cadr (tree->stree (get-reference "c1"))) "1")
     (check= (cadr (tree->stree (get-reference "c3"))) "3")
     ;; texmacs-expand takes the start of the document
     (check= (ev '(value "mdtc-nr")) '(uninit))
     (check= (ev '(value "section-nr")) "0"))))

;; The environment around the cursor: get-env and texmacs-exec* see the
;; variables set by the with around it, texmacs-expand the start of the
;; document; the macros of the preamble are seen everywhere.
(define (test-exec)
  (check-group "evaluation in a document")
  (with-document
   (doc-with '("generic")
             '(document "a"
                        (with "font-shape" "italic" "mdtw" "7" (document "b"))
                        (math "c")
                        (mdt-twice "d"))
             '("mdt-twice" (macro "x" (concat (arg "x") (arg "x"))))
             '("mdt-var" "9"))
   (lambda ()
     (go-to (at 1 4 0 0))
     (check= (cursor) '(1 4 0 0))
     (check= (get-env "font-shape") "italic")
     (check= (get-env "mdtw") "7")
     (check= (tree->stree (texmacs-exec* '(value "mdtw"))) "7")
     (check= (tree->stree (texmacs-exec* '(plus (value "mdtw") (value "mdt-var"))))
             "16")
     (check= (tree->stree (texmacs-exec* '(mdt-twice "z"))) '(concat "z" "z"))
     (go-to (at 2 0 0))
     (check= (get-env "mode") "math")
     (check= (get-env "font-shape") "right")
     (go-to (at 0 0))
     (check= (get-env "mode") "text")
     (check= (ev '(value "mdt-var")) "9")
     (check= (ev '(mdt-twice "q")) "qq")
     (check= (ev '(value "mdtw")) '(uninit))
     (check= (get-init "mdt-var") "9")
     (check-true (style-has? "mdt-twice"))
     (check-true (style-has? "section"))
     (check-false (style-has? "no-such-macro-xyz"))
     (check= (tree->stree (get-init-tree "mdt-twice"))
             '(macro "x" (concat (arg "x") (arg "x")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ts-files u)
  ;; the .ts files in the directory @u and its subdirectories
  (append-map (lambda (x)
                (cond ((url-directory? x) (ts-files x))
                      ((string-ends? (url->system x) ".ts") (list x))
                      (else '())))
              (url-read-directory u "*")))

(define (top-assigns doc)
  ;; the variables which the package @doc assigns at its top level
  (if (and (pair? doc) (== (car doc) 'document))
      (filter-map (lambda (x)
                    (and (pair? x) (== (car x) 'assign) (string? (cadr x))
                         (cadr x)))
                  (cdr doc))
      '()))

(define (package-problem name)
  ;; #f when the package @name loads, else what goes wrong
  (let* ((doc (tree->stree (tree-load-style name)))
         (vars (top-assigns doc))
         (e0 (nr-errors))
         (r (check-run
             (lambda ()
               (ev `(tuple (use-package ,name)
                           ,@(map (lambda (v) `(provides ,v)) vars))))))
         (e1 (nr-errors)))
    (cond ((not (and (pair? doc) (== (car doc) 'document)
                     (> (length doc) 2)))
           "not a document")
          ((and (pair? r) (== (car r) 'error)) r)
          ((!= e1 e0) "error messages")
          ((not (and (pair? r) (== (car r) 'tuple)
                     (== (length r) (+ 2 (length vars)))))
           r)
          (else
           (with missing (list-filter (map (lambda (v x) (and (!= x "true") v))
                                           vars (cddr r))
                                      identity)
             (and (nnull? missing) (list "not defined" missing)))))))

;; Every style and package is a TeXmacs document; executing it (as the
;; style of a document does, use-package) gives no error message and
;; defines all its top level macros and variables.
;; Some packages show pictures of the artwork of the TeXmacs server
;; (tmfs://artwork/...) or of the web as a background, which is evaluated
;; when the package is loaded: loading them downloads the picture, which
;; may take minutes without a network. They are left out, with all
;; packages which use them.

(define (net-pattern? t)
  ;; does @t hold a pattern of a remote picture?
  (and (pair? t)
       (or (and (== (car t) 'pattern) (pair? (cdr t)) (string? (cadr t))
                (or (string-starts? (cadr t) "tmfs://")
                    (string-starts? (cadr t) "http")))
           (list-or (map net-pattern? (cdr t))))))

(define (used-packages t)
  ;; the packages which @t uses
  (cond ((not (pair? t)) '())
        ((== (car t) 'use-package) (list-filter (cdr t) string?))
        (else (append-map used-packages (cdr t)))))

(define net-table (make-ahash-table))

(define (net-package? name)
  ;; does loading the package @name download a picture?
  (if (ahash-ref net-table name)
      (== (ahash-ref net-table name) 'yes)
      (let* ((doc (tree->stree (tree-load-style name)))
             (top (if (and (pair? doc) (== (car doc) 'document)) (cdr doc) '())))
        (ahash-set! net-table name 'no)
        (with r (or (list-or (map (lambda (x)
                                    (and (pair? x) (== (car x) 'assign)
                                         (= (length x) 3)
                                         (not (func? (caddr x) 'macro))
                                         (net-pattern? (caddr x))))
                                  top))
                    (list-or (map net-package? (used-packages doc))))
          (ahash-set! net-table name (if r 'yes 'no))
          r))))

(define (test-all-packages)
  (check-group "all packages")
  (let* ((root (url->system (get-texmacs-path)))
         (styles (ts-files (system->url (string-append root "/styles"))))
         (packages (ts-files (system->url (string-append root "/packages"))))
         (bad '()))
    (check-true (>= (length styles) 50))
    (check-true (>= (length packages) 250))
    (for (f (append styles packages))
      (let* ((name (url->string (url-basename f)))
             (pb (and (not (net-package? name)) (package-problem name))))
        (when pb
          (display* "  package " name " (" (url->system f) "): "
                    (object->string pb) "\n")
          (set! bad (cons name bad)))))
    (check= (reverse bad) '())
    (display* "  left out: "
              (map (lambda (f) (url->string (url-basename f)))
                   (list-filter (append styles packages)
                                (lambda (f) (net-package?
                                             (url->string (url-basename f))))))
              "\n")
    ;; the packages which are left out
    (check-true (net-package? "blackboard-scene"))
    (check-true (net-package? "blackboard-combo"))
    (check-false (net-package? "std-frame"))
    (check-false (net-package? "article"))
    (check-true (< (length (list-filter (append styles packages)
                                        (lambda (f)
                                          (net-package?
                                           (url->string (url-basename f))))))
                   20))
    ;; a missing package is not an error, and defines nothing
    (check= (tree->stree (tree-load-style "macro-drd-no-such-package"))
            '(document ""))
    (check= (ev '(tuple (use-package "macro-drd-no-such-package")
                        (provides "section")))
            '(tuple "" "true"))
    (check= (package-problem "macro-drd-no-such-package") "not a document")
    (check= (top-assigns (tree->stree (tree-load-style "preview-ref")))
            '("reference" "pageref"))
    (check= (ev '(tuple (use-package "preview-ref") (provides "reference")))
            '(tuple "" "true"))))

;; A package of the suite, used by the style of a document: its macros and
;; its drd-props are those of the document.
(define (test-own-package)
  (check-group "own package")
  (let ((pkg (tmp-file "mdt-pkg.ts")))
    (string-save
     (string-append
      "<TeXmacs|2.1>\n\n<style|source>\n\n<\\body>\n"
      "  <assign|mdt-two|<macro|a|b|<arg|a><arg|b>>>\n\n"
      "  <drd-props|mdt-two|arity|2|accessible|0>\n\n"
      "  <assign|mdt-hid|<macro|a|b|<arg|a>>>\n\n"
      "  <assign|mdt-var|<macro|x|<arg|x>>>\n\n"
      "  <drd-props|mdt-var|arity|<tuple|repeat|1|1>|accessible|all>\n\n"
      "  <assign|mdt-none|<macro|a|<arg|a>>>\n\n"
      "  <drd-props|mdt-none|arity|1|accessible|none>\n\n"
      "  <assign|mdt-col|red>\n\n"
      "  <drd-props|mdt-col|macro-parameter|color>\n\n"
      "  <drd-props|mdt-len|parameter|length>\n\n"
      "  <assign|mdt-sz|<macro|x|<arg|x>>>\n\n"
      "  <drd-props|mdt-sz|arity|1|length|0>\n"
      "</body>\n\n<initial|<\\collection>\n</collection>>\n")
     (system->url pkg))
    (with-document
     (doc-with `("generic" ,pkg)
               '(document (mdt-two "ab" "cd") (mdt-hid "ef" "gh")
                          (mdt-var "a" "b" "c") (mdt-none "x")
                          (mdt-sz "1cm") "end"))
     (lambda ()
       (check= (get-style-list) `("generic" ,pkg))
       (check= (ev '(mdt-two "1" "2")) "12")
       (check= (ev '(value "mdt-col")) "red")
       (check= (tag-minimal-arity 'mdt-two) 2)
       (check= (tag-maximal-arity 'mdt-two) 2)
       (check-true (tree-accessible-child? (sub 0) 0))
       (check-true (tree-accessible-child? (sub 0) 1))
       ;; the heuristics: an argument which the macro does not show
       (check-true (tree-accessible-child? (sub 1) 0))
       (check-false (tree-accessible-child? (sub 1) 1))
       (check= (tag-minimal-arity 'mdt-var) 1)
       (check= (tag-maximal-arity 'mdt-var) 2147483647)
       (check-true (tag-possible-arity? 'mdt-var 5))
       (check-false (tag-possible-arity? 'mdt-var 0))
       (check-true (tree-accessible-child? (sub 2) 2))
       (check-false (tree-accessible-child? (sub 3) 0))
       (check-true (tree-none-accessible? (sub 3)))
       (check= (tree-child-type (sub 4) 0) "length")
       (check= (tree-label-type 'mdt-col) "color")
       (check-true (tree-label-parameter? 'mdt-col))
       (check-true (tree-label-macro? 'mdt-col))
       (check= (tree-label-type 'mdt-len) "length")
       (check-true (tree-label-parameter? 'mdt-len))
       (check-false (tree-label-macro? 'mdt-len))
       (check-true (tree-label-macro? 'mdt-two))
       (check-false (tree-label-parameter? 'mdt-two))))
    (system-remove (system->url pkg))))

(define (style-check style thunk)
  ;; run @thunk in a typeset buffer of the style @style
  (with-document
   (doc-with (list style) '(document (section "A") "x"))
   (lambda ()
     (check= (get-style-list) (list style))
     (check= (get-page-count) 1)
     (thunk))))

(define section-title-generic
  '(surround (no-indent) (specific "texmacs" (htab "0fn" "first"))
             (concat (with "font-series" "bold" "math-font-series" "bold"
                       (concat (vspace* "1.5fn") (with "font-size" "1.414" "x")
                               (vspace "0.5fn")))
                     (no-page-break) (no-indent*))))

(define strong-x '(with "font-series" "bold" "math-font-series" "bold" "x"))

;; The main styles, set on a buffer: the page and paragraph parameters and
;; the rendering of a few standard macros differ from one to the other.
(define (test-main-styles)
  (check-group "main styles")
  (let ((e0 (nr-errors)))
    (style-check
     "generic"
     (lambda ()
       (check= (ev '(value "page-type")) "a4")
       (check= (ev '(value "page-medium")) "papyrus")
       (check= (ev '(value "par-first")) "0fn")
       (check= (ev '(value "magnification")) "1")
       (check= (ev '(provides "chapter")) "true")
       (check= (ev '(provides "opening")) "false")
       (check= (ev '(provides "tmdoc-title")) "false")
       (check= (ev '(section-title "x")) section-title-generic)
       (check= (ev '(strong "x")) strong-x)
       (check= (ev '(em "x")) '(with "font-shape" "italic" "x"))
       (check= (ev '(theorem-name "x")) strong-x)))
    (style-check
     "article"
     (lambda ()
       (check= (ev '(value "par-first")) "1.5fn")
       (check= (ev '(section-title "x"))
               '(surround (no-indent) (specific "texmacs" (htab "0fn" "first"))
                          (concat (with "font-series" "bold" "math-font-series" "bold"
                                    (concat (vspace* "3fn") (with "font-size" "1.414" "x")
                                            (vspace "1fn")))
                                  (no-page-break) (no-indent*))))
       (check= (ev '(strong "x")) strong-x)))
    (style-check
     "book"
     (lambda ()
       (check= (ev '(value "par-first")) "1.5fn")
       (check= (ev '(chapter-title "x"))
               '(concat (new-dpage*) (no-indent) (new-line) (no-indent)
                        (vspace* "5fn")
                        (with "math-font-series" "bold" "font-series" "bold"
                          (with "font-size" "2" "x"))
                        (vspace "2fn") (no-page-break) (no-indent*)))))
    (style-check
     "letter"
     (lambda ()
       (check= (ev '(provides "opening")) "true")
       (check= (ev '(provides "closing")) "true")
       (check= (ev '(value "par-first")) "0fn")))
    (style-check
     "beamer"
     (lambda ()
       (check= (ev '(value "page-type")) "4:3")
       (check= (ev '(value "page-medium")) "beamer")
       (check= (ev '(value "page-orientation")) "landscape")
       (check= (ev '(value "magnification")) "1.7")
       (check= (ev '(provides "slide")) "true")
       (check= (ev '(strong "x")) `(with "color" "#602060" ,strong-x))))
    (style-check
     "seminar"
     (lambda ()
       (check= (ev '(value "magnification")) "2")
       (check= (ev '(em "x")) '(with "color" "blue" "x"))))
    (style-check
     "source"
     (lambda ()
       (check= (ev '(provides "src-title")) "true")
       (check= (ev '(provides "section")) "false")
       (check= (ev '(strong "x")) '(error "compound strong"))))
    (style-check
     "exam"
     (lambda ()
       (check= (ev '(value "par-first")) "0tab")
       (check= (ev '(provides "exercise")) "true")))
    (style-check
     "browser"
     (lambda ()
       (check= (ev '(value "page-medium")) "automatic")))
    (style-check
     "poster"
     (lambda ()
       (check= (ev '(value "page-type")) "a0")
       (check= (ev '(value "magnification")) "3.2")
       (check= (ev '(value "par-columns")) "2")))
    (style-check
     "tmdoc"
     (lambda ()
       (check= (ev '(provides "tmdoc-title")) "true")
       (check= (ev '(value "font")) "pagella")
       (check= (ev '(theorem-name "x")) '(with "font-shape" "small-caps" "x"))))
    (style-check
     "amsart"
     (lambda ()
       (check= (ev '(section-title "x"))
               '(surround (concat (no-indent) (htab "0fn" "last"))
                          (htab "0fn" "first")
                          (concat (vspace* (tmlen "0.7bls" "0.7bls" "1.7bls"))
                                  (with "font-shape" "small-caps" "x")
                                  (vspace "0.5bls") (no-page-break))))))
    ;; a style with a package
    (with-document
     (doc-with '("article" "number-long-article") '(document "x"))
     (lambda ()
       (check= (get-style-list) '("article" "number-long-article"))
       (check= (ev '(provides "section")) "true")))
    (check= (nr-errors) e0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The DRD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The arities of the builtin tags and of the macros of the standard
;; style; 2147483647 is the maximal arity of a tag with any number of
;; children.
(define (test-drd-arity)
  (check-group "drd arity")
  (check= (tag-minimal-arity 'frac) 2)
  (check= (tag-maximal-arity 'frac) 2)
  (check= (tag-minimal-arity 'concat) 1)
  (check= (tag-maximal-arity 'concat) 2147483647)
  (check= (tag-minimal-arity 'document) 1)
  (check= (tag-minimal-arity 'tuple) 0)
  (check= (tag-minimal-arity 'plus) 2)
  (check= (tag-minimal-arity 'with) 1)
  (check-true (tag-possible-arity? 'with 3))
  (check-true (tag-possible-arity? 'with 5))
  (check-false (tag-possible-arity? 'with 2))
  (check-false (tag-possible-arity? 'with 4))
  (check= (tag-minimal-arity 'if) 2)
  (check= (tag-maximal-arity 'if) 3)
  (check= (tag-minimal-arity 'date) 0)
  (check= (tag-maximal-arity 'date) 2)
  (check= (tag-minimal-arity 'hlink) 2)
  (check= (tag-maximal-arity 'hlink) 2)
  (check= (tag-minimal-arity 'image) 5)
  (check= (tag-minimal-arity 'macro) 1)
  (check= (tag-minimal-arity 'label) 1)
  (check= (tag-maximal-arity 'label) 1)
  (check= (tag-minimal-arity 'section) 1)
  (check= (tag-maximal-arity 'section) 1)
  (check= (tag-minimal-arity 'theorem) 1)
  (check= (tag-maximal-arity 'theorem) 1)
  (check= (tag-minimal-arity 'big-figure) 2)
  (check= (tag-maximal-arity 'big-figure) 2)
  (check= (tag-minimal-arity 'summarized) 2)
  (check= (tree-minimal-arity (T '(frac "a" "b"))) 2)
  (check= (tree-maximal-arity (T '(frac "a" "b"))) 2)
  (check-true (tree-possible-arity? (T '(frac "a" "b")) 2))
  (check-false (tree-possible-arity? (T '(frac "a" "b")) 3)))

;; The types: of the value of a tag (tree-label-type) and of its children
;; (tree-child-type), and the names of the children of a macro.
(define (test-drd-types)
  (check-group "drd types")
  (check= (tree-label-type 'plus) "numeric")
  (check= (tree-label-type 'equal) "boolean")
  (check= (tree-label-type 'number) "string")
  (check= (tree-label-type 'date) "string")
  (check= (tree-label-type 'translate) "string")
  (check= (tree-label-type 'merge) "adhoc")
  (check= (tree-label-type 'rgb-color) "color")
  (check= (tree-label-type 'cm-length) "length")
  (check= (tree-label-type 'length) "integer")
  (check= (tree-label-type 'point) "graphical")
  (check= (tree-label-type 'frac) "regular")
  (check= (tree-label-type 'section) "regular")
  (check= (tree-label-type 'concat) "regular")
  (check= (tree-child-type (T '(frac "a" "b")) 0) "regular")
  (check= (tree-child-type (T '(with "a" "b" "c")) 0) "variable")
  (check= (tree-child-type (T '(with "a" "b" "c")) 2) "regular")
  (check= (tree-child-type (T '(assign "a" "b")) 0) "variable")
  (check= (tree-child-type (T '(value "a")) 0) "variable")
  (check= (tree-child-type (T '(arg "a")) 0) "argument")
  (check= (tree-child-type (T '(macro "a" "b")) 0) "argument")
  (check= (tree-child-type (T '(macro "a" "b")) 1) "regular")
  (check= (tree-child-type (T '(label "a")) 0) "identifier")
  (check= (tree-child-type (T '(reference "x")) 0) "identifier")
  (check= (tree-child-type (T '(hlink "a" "b")) 1) "url")
  (check= (tree-child-type (T '(include "x")) 0) "url")
  (check= (tree-child-type (T '(image "a" "1cm" "1cm" "" "")) 1) "length")
  (check= (tree-child-type (T '(hspace "1cm")) 0) "length")
  (check= (tree-child-type (T '(rgb-color "1" "2" "3")) 0) "integer")
  (check= (tree-child-type (T '(if "a" "b")) 0) "boolean")
  (check= (tree-child-type (T '(section "a")) 0) "regular"))

;; Accessible children: the cursor may go into them. A with gives access
;; to its body only, a label and a reference to nothing, a link to its
;; text, a folded or summarized environment to its visible part.
(define (test-drd-access)
  (check-group "drd accessibility")
  (check-true (tree-accessible-child? (T '(frac "a" "b")) 1))
  (check-true (tree-all-accessible? (T '(frac "a" "b"))))
  (check-false (tree-accessible-child? (T '(with "a" "b" "c")) 0))
  (check-true (tree-accessible-child? (T '(with "a" "b" "c")) 2))
  (check= (map tree->stree (tree-accessible-children (T '(with "a" "b" "c"))))
          '("c"))
  (check-false (tree-accessible-child? (T '(label "a")) 0))
  (check-true (tree-none-accessible? (T '(label "a"))))
  (check-false (tree-accessible-child? (T '(reference "a")) 0))
  (check-true (tree-accessible-child? (T '(hlink "a" "b")) 0))
  (check-false (tree-accessible-child? (T '(hlink "a" "b")) 1))
  (check-true (tree-accessible-child? (T '(theorem "a")) 0))
  (check-false (tree-accessible-child? (T '(assign "a" "b")) 0))
  (check-false (tree-accessible-child? (T '(assign "a" "b")) 1))
  (check-true (tree-accessible-child? (T '(summarized "a" "b")) 0))
  (check-false (tree-accessible-child? (T '(summarized "a" "b")) 1))
  (check-true (tree-accessible-child? (T '(folded "a" "b")) 0))
  (check-false (tree-accessible-child? (T '(folded "a" "b")) 1))
  ;; drd-props of std-markup and std-utils
  (check-false (tree-accessible-child? (T '(suppressed "a")) 0))
  (check-false (tree-accessible-child? (T '(with-screen-color "red" "a")) 0))
  ;; the environment of the children
  (check= (tree->stree (tree-child-env (T '(math "a")) 0 "mode" ""))
          "math")
  (check= (tree->stree (tree-child-env (T '(text "a")) 0 "mode" ""))
          "text")
  (check= (tree->stree (tree-child-env (T '(frac "a" "b")) 0 "math-display" ""))
          "false")
  ;; paragraphs
  (check-true (tree-multi-paragraph? (T '(document "a" "b"))))
  (check-false (tree-multi-paragraph? (T '(concat "a" "b"))))
  (check-true (tree-multi-paragraph? (T '(theorem (document "a" "b")))))
  (check-false (tree-multi-paragraph? (T '(strong "a")))))

;; Variables and macros: a variable of the typesetter is a parameter, a
;; tag is a macro.
(define (test-drd-kinds)
  (check-group "drd kinds of tags")
  (check-true (tree-label-parameter? 'font-shape))
  (check-false (tree-label-macro? 'font-shape))
  (check-true (tree-label-macro? 'section))
  (check-false (tree-label-parameter? 'section))
  (check-true (tree-label-extension? 'section))
  (check-false (tree-label-extension? 'frac))
  (check-false (tree-label-extension? 'concat))
  (check-false (tree-is-dynamic? (T '(frac "a" "b")))))

;; The macros of the preamble of a document: their arity and the
;; accessibility of their children are guessed from their definition (an
;; argument which the body does not show is not accessible), the names of
;; the children are the names of the arguments.
(define (test-drd-user-macros)
  (check-group "drd of user macros")
  (with-document
   (doc-with '("generic")
             '(document (mfoo "ab" "cd") (mbar "ef" "gh") (mrep "a" "b" "c")
                        (mquote "r") "z")
             '("mfoo" (macro "a" "b" (concat (arg "a") "-")))
             '("mbar" (macro "a" "b" (concat (arg "b") (arg "a"))))
             '("mquote" (macro "a" (concat (quote-arg "a"))))
             '("mrep" (xmacro "x" (arg "x" "0"))))
   (lambda ()
     (check= (tag-minimal-arity 'mfoo) 2)
     (check= (tag-maximal-arity 'mfoo) 2)
     (check= (tag-minimal-arity 'mrep) 1)
     (check= (tag-maximal-arity 'mrep) 2147483647)
     (check-true (tree-label-macro? 'mfoo))
     (check-false (tree-label-parameter? 'mfoo))
     (check-true (tree-label-extension? 'mfoo))
     (check-true (tree-accessible-child? (sub 0) 0))
     (check-false (tree-accessible-child? (sub 0) 1))
     (check= (map tree->stree (tree-accessible-children (sub 0))) '("ab"))
     (check-false (tree-all-accessible? (sub 0)))
     (check-false (tree-none-accessible? (sub 0)))
     (check-true (tree-all-accessible? (sub 1)))
     (check= (map tree->stree (tree-accessible-children (sub 1))) '("ef" "gh"))
     (check-true (tree-accessible-child? (sub 2) 0))
     (check-false (tree-accessible-child? (sub 2) 2))
     (check-true (tree-accessible-child? (sub 3) 0))
     (check= (tree-child-name (sub 0) 0) "a")
     (check= (tree-child-name (sub 1) 1) "b")
     (check= (tree-child-type (sub 0) 0) "regular")
     (check-true (tree-possible-arity? (sub 0) 2))
     (check-false (tree-possible-arity? (sub 0) 3)))))

;; The accessible children decide where the cursor can be: go-to puts the
;; cursor anywhere, cursor-accessible? tells whether it may stay there.
(define (test-drd-cursor)
  (check-group "drd and the cursor")
  (with-document
   (doc-with '("generic")
             '(document (mfoo "ab" "cd") (mbar "ef" "gh") (label "lab")
                        (frac "a" "b") (with "font-shape" "italic" "it")
                        (summarized "s" "t") "z")
             '("mfoo" (macro "a" "b" (concat (arg "a") "-")))
             '("mbar" (macro "a" "b" (concat (arg "b") (arg "a")))))
   (lambda ()
     (go-to (at 0 0 1))
     (check= (cursor) '(0 0 1))
     (check-true (cursor-accessible?))
     (go-to (at 0 1 1))
     (check= (cursor) '(0 1 1))
     (check-false (cursor-accessible?))
     (go-to (at 1 1 1))
     (check-true (cursor-accessible?))
     (go-to (at 2 0 1))
     (check-false (cursor-accessible?))
     (go-to (at 3 1 0))
     (check-true (cursor-accessible?))
     (go-to (at 4 0 1))
     (check-false (cursor-accessible?))
     (go-to (at 4 2 1))
     (check-true (cursor-accessible?))
     (go-to (at 5 0 0))
     (check-true (cursor-accessible?))
     (go-to (at 5 1 0))
     (check-false (cursor-accessible?))
     (go-to (at 6 1))
     (check-true (cursor-accessible?))
     (go-to (at 0 1))
     (check-true (cursor-accessible?))
     ;; path-next and path-previous skip the children which are not
     ;; accessible, and go into those which are
     (check= (path-next (buffer-tree) '(2 0)) '(2 1))
     (check= (path-previous (buffer-tree) '(2 1)) '(2 0))
     (check= (path-next (buffer-tree) '(0 0 2)) '(0 1))
     (check= (path-previous (buffer-tree) '(0 0 0)) '(0 0))
     (check= (path-next (buffer-tree) '(5 0 1)) '(5 1))
     (check= (path-next (buffer-tree) '(3 0)) '(3 0 0))
     (check= (path-next (buffer-tree) '(5 0)) '(5 0 0))
     (go-to (at 2 0))
     (check-true (cursor-accessible?))
     (go-to (at 2 1))
     (check-true (cursor-accessible?))
     (go-to (at 0 1 1))
     (go-end-of 'mfoo)
     (check= (cursor) '(0 1)))))

;; The tag groups of the Scheme side (define-group): the membership
;; predicates and lists, the subgroups, tree-in? on a group, and a group
;; of the suite.
(define (test-groups)
  (check-group "tag groups")
  (check= (section-tag-list)
          '(part chapter appendix section subsection subsubsection
                 paragraph subparagraph))
  (check= (theorem-tag-list) '(theorem proposition lemma corollary conjecture))
  (check-true (section-tag? 'section))
  (check-false (section-tag? 'section*))
  (check-true (section*-tag? 'section*))
  (check-true (enunciation-tag? 'theorem))
  (check-true (enunciation-tag? 'remark))
  (check-true (enunciation-tag? 'exercise))
  (check-false (enunciation-tag? 'proof))
  (check-true (auto-titled-tag? 'proof))
  (check-true (list-tag? 'enumerate-roman))
  (check-true (itemize-tag? 'itemize-dot))
  (check-false (itemize-tag? 'enumerate))
  (check= (enumerate-tag-list)
          '(enumerate enumerate-numeric enumerate-roman enumerate-Roman
                      enumerate-alpha enumerate-Alpha))
  (check= (strong-tag-list) '(strong em dfn underline))
  (check= (equation-tag-list) '(equation eqnarray))
  (check-true (numbered-tag? 'section))
  (check-true (numbered-tag? 'equation))
  (check-false (numbered-tag? 'strong))
  (check-true (symbol-numbered? 'lemma))
  (check-true (symbol-unnumbered? 'lemma*))
  (check= (symbol-toggle-number 'lemma) 'lemma*)
  (check= (symbol-toggle-number 'lemma*) 'lemma)
  (check= (fraction-tag-list) '(frac tfrac dfrac frac* cfrac))
  (check-true (textual-operator-tag? 'math-bf))
  (check= (assign-tag-list) '(assign provide))
  (check-true (summarized-tag? 'summarized))
  (check= (group-find 'lemma 'enunciation-tag) 'theorem-tag)
  (check= (group-find 'section 'section-tag) 'section-tag)
  (check= (group-find 'strong 'enunciation-tag) #f)
  (check-true (tree-in? (T '(theorem "a")) (enunciation-tag-list)))
  (check-false (tree-in? (T '(strong "a")) (enunciation-tag-list)))
  (check-true (tree-in? (T '(concat "x" (lemma "a"))) 1 (theorem-tag-list)))
  (check-true (tree-is? (T '(concat "x" (lemma "a"))) 1 'lemma))
  (check-true (numbered-context? (T '(lemma "a"))))
  (check-true (numbered-unnumbered? (T '(lemma* "a"))))
  (check-false (numbered-context? (T '(strong "a"))))
  (check-true (alternate-first? (T '(wide "a" "b"))))
  (check-true (alternate-second? (T '(wide* "a" "b"))))
  (check= (macro-drd-test-group-list)
          (append '(macro-drd-test-a macro-drd-test-b) (theorem-tag-list)))
  (check-true (macro-drd-test-group? 'macro-drd-test-b))
  (check-true (macro-drd-test-group? 'corollary))
  (check-false (macro-drd-test-group? 'remark))
  (check= (group-find 'lemma 'macro-drd-test-group) 'theorem-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-group thunk)
  ;; an error in a group counts as one failure and the suite goes on
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (macro-drd-test-failures)
  (:synopsis "Run the tests of macros, style packages and the DRD")
  (check-suite "macro-drd")
  (fresh-tmp-dir)
  (in-generic
   (lambda ()
     (run-group test-macro-arguments)
     (run-group test-quoting)
     (run-group test-extern)
     (run-group test-environment)
     (run-group test-strings-tuples)
     (run-group test-numbers)
     (run-group test-dates)
     (run-group test-counters)
     (run-group test-drd-arity)
     (run-group test-drd-types)
     (run-group test-drd-access)
     (run-group test-drd-kinds)
     (run-group test-groups)
     (run-group test-all-packages)))
  (run-group test-document-counters)
  (run-group test-exec)
  (run-group test-own-package)
  (run-group test-main-styles)
  (run-group test-drd-user-macros)
  (run-group test-drd-cursor)
  (remove-tmp-dir)
  (check-end))
