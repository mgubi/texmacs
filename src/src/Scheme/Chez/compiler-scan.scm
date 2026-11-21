;;; TODO: UNDER REVIEWED AI GENERATED CODE.
;;;
;;; compiler-scan.scm -- Scan TeXmacs Scheme sources into compiler-db records.
;;;
;;; This library depends only on compiler-db and Chez Scheme. It does not load
;;; the TeXmacs runtime.

(library (compiler-scan)
  (export read-all-forms
          scan-interface-hash
          scan-source-file
          scan-source-files
          scan-source-files->db)
  (import (chezscheme) (compiler-db))

  ;;; -------------------------------------------------------------------------
  ;;; Source reader
  ;;; -------------------------------------------------------------------------

  ;; TeXmacs sources are Guile-flavoured: they write \xNN escapes without
  ;; the trailing semicolon Chez insists on, and use #\" as a character
  ;; literal. Normalise the text before reading it, so every consumer of
  ;; this library sees the same forms the transpiler sees.

  ;; A hex digit, or #f for anything else (including end of input).
  (define (hex-digit? c)
    (and c
         (or (char<=? #\0 c #\9)
             (char<=? #\a c #\f)
             (char<=? #\A c #\F))))

  ;; Copy STR, adding the semicolon Chez demands after \xNN and rewriting the
  ;; `#\"` character literal. Everything else is copied through untouched.
  (define (normalize-guile-string-escapes str)
    (define len (string-length str))
    (define out (open-output-string))
    (define (at i) (and (< i len) (string-ref str i)))
    (define (emit x) (display x out))

    ;; Index of the newline that ends the line starting at I, or LEN.
    (define (line-end i)
      (let scan ((j i))
        (cond ((>= j len) len)
              ((char=? (string-ref str j) #\newline) j)
              (else (scan (+ j 1))))))

    ;; Length of the octal digits of a Guile character literal #\NN or
    ;; #\NNN starting at I (at least two digits, then a delimiter), or #f.
    (define (octal-char-length i)
      (let count ((j i))
        (let ((d (at j)))
          (if (and d (char<=? #\0 d #\7) (< (- j i) 3))
              (count (+ j 1))
              (and (>= (- j i) 2)
                   (or (not d) (char-whitespace? d) (memv d '(#\( #\))))
                   (- j i))))))

    ;; Is there a \xNN escape at I?
    (define (hex-escape? i)
      (and (memv (at (+ i 1)) '(#\x #\X))
           (hex-digit? (at (+ i 2)))
           (hex-digit? (at (+ i 3)))))

    ;; Copy the escape at I, adding a semicolon if it lacks one; next index.
    (define (copy-hex-escape i)
      (let* ((terminated? (eqv? (at (+ i 4)) #\;))
             (end (if terminated? (+ i 5) (+ i 4))))
        (emit (substring str i end))
        (unless terminated? (emit ";"))
        end))

    (let loop ((i 0) (in-string? #f))
      (let ((c (at i)))
        (cond
          ((not c)
           (get-output-string out))

          ;; `#\"` is a character literal, not the start of a string.
          ((and (not in-string?) (eqv? c #\#)
                (eqv? (at (+ i 1)) #\\) (eqv? (at (+ i 2)) #\"))
           (emit "#\\\"")
           (loop (+ i 3) #f))

          ;; Other character literals are copied whole (#\| #\; #\( ...),
          ;; except Guile's octal ones, #\00 to #\377, which become #\xNN.
          ((and (not in-string?) (eqv? c #\#) (eqv? (at (+ i 1)) #\\))
           (let ((n (octal-char-length (+ i 2))))
             (if n
                 (begin
                   (emit "#\\x")
                   (emit (number->string
                          (string->number (substring str (+ i 2) (+ i 2 n)) 8)
                          16))
                   (loop (+ i 2 n) #f))
                 (begin
                   (emit (substring str i (min len (+ i 3))))
                   (loop (+ i 3) #f)))))

          ;; Guile's extended symbols #{a b}# are Chez's |a b|.
          ((and (not in-string?) (eqv? c #\#) (eqv? (at (+ i 1)) #\{))
           (let scan ((j (+ i 2)))
             (cond ((>= (+ j 1) len) (emit c) (loop (+ i 1) #f))
                   ((and (eqv? (at j) #\}) (eqv? (at (+ j 1)) #\#))
                    (emit "|")
                    (emit (substring str (+ i 2) j))
                    (emit "|")
                    (loop (+ j 2) #f))
                   (else (scan (+ j 1))))))

          ;; In Guile | is an ordinary symbol character ('| , left| , ||),
          ;; in Chez it quotes a part of a symbol: escape it.
          ((and (not in-string?) (eqv? c #\|))
           (emit "\\x7c;")
           (loop (+ i 1) #f))

          ;; Sources are read byte for byte (Latin-1, see boot-chez.scm): a
          ;; byte of a UTF-8 symbol such as 0x85 or 0xA0 would be read as
          ;; whitespace; written \xHH; it stays in the symbol
          ((and (not in-string?) (fx>= (char->integer c) 128))
           (emit "\\x")
           (emit (number->string (char->integer c) 16))
           (emit ";")
           (loop (+ i 1) #f))

          ;; Guile reads [ and ] as symbols too (latex-command-drd.scm),
          ;; Chez as parentheses
          ((and (not in-string?) (memv c '(#\[ #\])))
           (emit (if (eqv? c #\[) "\\x5b;" "\\x5d;"))
           (loop (+ i 1) #f))

          ;; A quote opens a string outside one and closes it inside one.
          ((eqv? c #\")
           (emit c)
           (loop (+ i 1) (not in-string?)))

          ;; Comments are copied whole, so a quote inside one stays put.
          ((and (not in-string?) (eqv? c #\;))
           (let ((end (line-end i)))
             ;; without its non-ASCII bytes, which may be line breaks in
             ;; Latin-1 (0x85)
             (do ((j i (+ j 1))) ((= j end))
               (let ((d (string-ref str j)))
                 (emit (if (char<? d #\x80) d #\?))))
             (loop end #f)))

          ;; Escapes only matter inside a string.
          ((and in-string? (eqv? c #\\) (hex-escape? i))
           (loop (copy-hex-escape i) #t))

          ((and in-string? (eqv? c #\\) (at (+ i 1)))
           (emit c)
           (emit (at (+ i 1)))
           (loop (+ i 2) #t))

          ;; A backslash at the very end of the input.
          ((and in-string? (eqv? c #\\))
           (emit c)
           (loop (+ i 1) #t))

          (else
           (emit c)
           (loop (+ i 1) in-string?))))))

  (define (read-raw-forms file-path)
    (let* ((port (open-file-input-port file-path
                                  (file-options) (buffer-mode block)
                                  (make-transcoder (latin-1-codec)
                                                   (eol-style none))))
           (content (get-string-all port)))
      (close-port port)
      (let* ((str (if (eof-object? content) "" content))
             (normalized (normalize-guile-string-escapes str))
             (in-port (open-string-input-port normalized)))
        (let loop ((acc '()))
          (let ((x (read in-port)))
            (if (eof-object? x)
                (reverse acc)
                (loop (cons x acc))))))))

  ;;; -------------------------------------------------------------------------
  ;;; Dialect selection
  ;;;
  ;;; TeXmacs/progs is shared by Guile, s7, femtolisp and Chez: code that has
  ;;; to differ tests (s7-scheme?) or (femtolisp-scheme?), and the Guile
  ;;; branch is the default one.  Chez takes the Guile branch, which is the
  ;;; code this compiler was written for, so both predicates are false here.
  ;;; They are folded before the transpiler sees the forms, with the
  ;;; conditionals that test them: an R6RS body cannot hold a definition
  ;;; under an `if', and the other branch may use syntax Chez does not have.
  ;;; -------------------------------------------------------------------------

  (define dialect-constants
    '((s7-scheme? . #f) (femtolisp-scheme? . #f) (chez-scheme? . #t)))

  (define definers
    '(define define-public provide-public define-macro define-public-macro
      tm-define tm-define-macro define-syntax lambda))

  (define (fold-dialect x)
    (define (const? v) (boolean? v))
    (define (unspecified) '(if #f #f))
    (cond
      ((not (pair? x)) x)
      ((memq (car x) '(quote quasiquote)) x)
      ;; the head of a definition is not a call
      ((and (memq (car x) definers) (pair? (cdr x)) (pair? (cadr x)))
       (cons* (car x) (cadr x) (map fold-dialect (cddr x))))
      ((and (symbol? (car x)) (null? (cdr x)) (assq (car x) dialect-constants))
       => cdr)
      (else
       (let ((y (let walk ((l x))
                  (cond ((pair? l) (cons (fold-dialect (car l)) (walk (cdr l))))
                        (else l)))))
         (case (car y)
           ((not)
            (if (and (= (length y) 2) (const? (cadr y))) (not (cadr y)) y))
           ((if)
            (if (and (>= (length y) 3) (const? (cadr y)))
                (cond ((cadr y) (caddr y))
                      ((pair? (cdddr y)) (cadddr y))
                      (else (unspecified)))
                y))
           ((or and)
            (let* ((stop (eq? (car y) 'or))           ; the absorbing value
                   (args (filter (lambda (a) (not (eq? a (not stop)))) (cdr y))))
              (cond ((memq stop args)
                     ;; the operands before it are predicates without effects
                     (if (memq stop (filter const? args)) stop y))
                    ((null? args) (not stop))
                    ((null? (cdr args)) (car args))
                    (else (cons (car y) args)))))
           ;; Chez modules have no Guile `with-module': the public definitions
           ;; it wraps are global anyway
           ((with-module)
            (if (pair? (cdr y)) (cons 'begin (cddr y)) y))
           ((when unless)
            (if (and (>= (length y) 2) (const? (cadr y)))
                (if (eq? (cadr y) (eq? (car y) 'when))
                    (cons 'begin (cddr y))
                    (unspecified))
                y))
           ((cond)
            (let clauses ((cs (cdr y)) (acc '()))
              (cond ((null? cs) (cons 'cond (reverse acc)))
                    ((and (pair? (car cs)) (eq? (caar cs) #f))
                     (clauses (cdr cs) acc))
                    ((and (pair? (car cs)) (eq? (caar cs) #t))
                     (cons 'cond (reverse (cons (cons 'else (cdar cs)) acc))))
                    (else (clauses (cdr cs) (cons (car cs) acc))))))
           (else y))))))

  ;; A top-level cond choosing between groups of definitions, as an `if'
  ;; chain, which the transpiler knows how to handle.
  (define (cond->if f)
    (if (and (pair? f) (eq? (car f) 'cond)
             (for-all (lambda (c) (and (pair? c) (pair? (cdr c))
                                       (not (eq? (cadr c) '=>))))
                      (cdr f)))
        (let chain ((cs (cdr f)))
          (cond ((null? cs) '(if #f #f))
                ((eq? (caar cs) 'else) (cons 'begin (cdar cs)))
                (else `(if ,(caar cs)
                           ,(cons 'begin (cdar cs))
                           ,(chain (cdr cs))))))
        f))

  ;; Top-level forms: fold, splice the `begin's the folding exposes (or that
  ;; were there), and drop what folded to nothing.
  (define (fold-dialect-forms forms)
    (let loop ((fs forms) (acc '()))
      (if (null? fs)
          (reverse acc)
          (let ((f (cond->if (fold-dialect (car fs)))))
            (cond ((equal? f '(if #f #f)) (loop (cdr fs) acc))
                  ((and (pair? f) (eq? (car f) 'begin))
                   (loop (append (cdr f) (cdr fs)) acc))
                  (else (loop (cdr fs) (cons f acc))))))))

  ;; chez-compat.scm provides, before the kernel, some definitions of later
  ;; kernel modules (list-fold, keyword->string, ...).  Its definition is the
  ;; one Chez uses: the module's own is dropped, rather than deleted from the
  ;; sources shared with the other interpreters.
  (define (defined-public-name f)
    (and (pair? f) (pair? (cdr f))
         (memq (car f) '(define-public define-public-macro provide-public))
         (let ((h (cadr f)))
           (let head ((h h)) (if (pair? h) (head (car h)) (and (symbol? h) h))))))

  ;; Definitions the Guile branch makes of what Chez has natively, or
  ;; boot-chez.scm (module-load).
  (define chez-native-names '(when unless case-lambda module-load))

  (define chez-compat-names #f)

  (define (compat-file? file-path)
    (let ((n (string-length file-path)) (suffix "kernel/boot/chez-compat.scm"))
      (and (>= n (string-length suffix))
           (string=? (substring file-path (- n (string-length suffix)) n) suffix))))

  (define (chez-compat-path file-path)
    ;; FILE-PATH is .../progs/<rel>; find .../progs from the module name
    (let loop ((i (- (string-length file-path) 1)))
      (cond ((< i 6) #f)
            ((string=? (substring file-path (- i 5) (+ i 1)) "progs/")
             (string-append (substring file-path 0 (+ i 1))
                            "kernel/boot/chez-compat.scm"))
            (else (loop (- i 1))))))

  (define (compat-names file-path)
    (or chez-compat-names
        (let ((p (chez-compat-path file-path)))
          (and p (file-exists? p)
               (begin
                 (set! chez-compat-names
                       (filter (lambda (x) x)
                               (map (lambda (f)
                                      (or (defined-public-name f)
                                          (and (pair? f) (eq? (car f) 'define-syntax)
                                               (pair? (cdr f)) (symbol? (cadr f))
                                               (cadr f))))
                                    (fold-dialect-forms (read-raw-forms p)))))
                 chez-compat-names)))))

  ;; Private helpers that macros of other modules call when they expand.
  ;; The expansion happens outside the defining module here, so Chez needs
  ;; them public; the other interpreters keep them private.
  (define chez-promoted-names
    '(cur-props lambda* property-rewrite lazy-define-table
      tm-define-overloaded-chez lazy-define-one-chez
      process-translate
      kbd-add-condition kbd-map-one kbd-map-body kbd-remove-one kbd-remove-body
      is-assume? quote-all logic-table-rule
      save-learned retrieve-learned
      define-preference define-preference-call-back
      lazy-input-converter-force))

  (define (promote-private f)
    (if (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
             (let ((h (cadr f)))
               (memq (if (pair? h) (car h) h) chez-promoted-names)))
        (cons 'define-public (cdr f))
        f))

  (define (read-all-forms file-path)
    (let ((forms (map promote-private
                      (fold-dialect-forms (read-raw-forms file-path)))))
      (if (compat-file? file-path)
          forms
          (let ((names (append chez-native-names
                               (or (compat-names file-path) '()))))
            (filter (lambda (f)
                      (let ((n (defined-public-name f)))
                        (not (and n (memq n names)))))
                    forms)))))

  ;;; -------------------------------------------------------------------------
  ;;; File and form helpers
  ;;; -------------------------------------------------------------------------


  (define (scan-read-text path)
    (guard (e (else #f))
      (call-with-input-file path
        (lambda (port)
          (get-string-all port)))))

  (define (scan-header forms)
    (let loop ((rest forms))
      (cond ((null? rest) #f)
            ((and (pair? (car rest))
                  (eq? (caar rest) 'texmacs-module))
             (car rest))
            (else (loop (cdr rest))))))

  (define (scan-header->deps header mod-name)
    (let ((deps '()))
      (for-each
       (lambda (opt)
         (when (and (pair? opt) (memq (car opt) '(:use :inherit)))
           (for-each
            (lambda (s)
              (unless (or (equal? s mod-name) (member s deps))
                (set! deps (cons s deps))))
            (cdr opt))))
       (cddr header))
      (reverse deps)))

  (define (scan-leftmost-symbol x)
    (cond ((symbol? x) x)
          ((pair? x) (scan-leftmost-symbol (car x)))
          (else #f)))

  (define (scan-defined-name form head)
    (or (scan-leftmost-symbol head)
        (error 'compiler-scan
               "declaration has no valid name"
               form)))

  (define (scan-macro-impl-name name)
    (string->symbol (string-append (symbol->string name) "$impl")))

  ;; `bib-define-style' does not name its export directly: the transpiler
  ;; synthesises bib-<style>? from the style string (prepare-forms in
  ;; compile-texmacs.scm). Mirror that here so the static index agrees with the
  ;; emitted module header.
  (define (scan-bib-predicate-name form)
    (let ((style (and (pair? (cdr form)) (cadr form))))
      (if (string? style)
          (string->symbol (string-append "bib-" style "?"))
          (error 'compiler-scan
                 "bib-define-style has no valid style name"
                 form))))

  (define (scan-flatten-begins forms)
    (let loop ((rest forms) (acc '()))
      (cond ((null? rest) (reverse acc))
            ((and (pair? (car rest)) (eq? (caar rest) 'begin))
             (loop (append (cdr (car rest)) (cdr rest)) acc))
            (else
             (loop (cdr rest) (cons (car rest) acc))))))

  ;;; -------------------------------------------------------------------------
  ;;; Declaration collection
  ;;; -------------------------------------------------------------------------

  (define (scan-collect forms)
    (let loop ((rest (scan-flatten-begins forms))
               (exports '())
               (globals '())
               (macros '())
               (impls '()))
      (if (null? rest)
          ;; a tm-define may be overloaded in the module which defines it
          ;; (check/define-test.scm): each name is exported once
          (values (let dedup ((l (reverse exports)) (seen '()))
                    (cond ((null? l) (reverse seen))
                          ((memq (car l) seen) (dedup (cdr l) seen))
                          (else (dedup (cdr l) (cons (car l) seen)))))
                  (reverse globals)
                  (reverse macros)
                  (reverse impls))
          (let* ((form (car rest))
                 (head (and (pair? form) (car form)))
                 (arg (and (pair? form) (pair? (cdr form)) (cadr form))))
            (case head
              ;; define-module-macro and define-export-macro are public macro
              ;; definers too. prepare-forms counts the former as public and
              ;; rewrites the latter into define-public-macro, so the static
              ;; index must record the same names as exports.
              ((define-public provide-public define-public-macro
                define-module-macro define-export-macro)
               (loop (cdr rest)
                     (cons (scan-defined-name form arg) exports)
                     globals macros impls))
              ((bib-define-style)
               (loop (cdr rest)
                     (cons (scan-bib-predicate-name form) exports)
                     globals macros impls))
              ((export)
               (loop (cdr rest)
                     (append (cdr form) exports)
                     globals macros impls))
              ((tm-define)
               (loop (cdr rest)
                     exports
                     (cons (scan-defined-name form arg) globals)
                     macros impls))
              ((tm-define-macro)
               (let* ((name (scan-defined-name form arg))
                      (impl (scan-macro-impl-name name)))
                 (loop (cdr rest)
                       exports
                       globals
                       (cons name macros)
                       (cons impl impls))))
              (else
               (loop (cdr rest) exports globals macros impls)))))))

  ;;; -------------------------------------------------------------------------
  ;;; Canonical hashes
  ;;; -------------------------------------------------------------------------

  (define (scan-sort-symbols syms)
    (sort (lambda (a b)
            (string<? (symbol->string a) (symbol->string b)))
          syms))

  (define (scan-canonical-symbols syms)
    (let loop ((rest (scan-sort-symbols syms)) (acc ""))
      (cond ((null? rest) acc)
            (else
             (loop (cdr rest)
                   (string-append acc (symbol->string (car rest)) "\n"))))))

  (define (scan-interface-hash exports global-macros macro-impls)
    (string-hash
     (string-append "E\n" (scan-canonical-symbols exports)
                    "G\n" (scan-canonical-symbols global-macros)
                    "M\n" (scan-canonical-symbols macro-impls))))

  (define (scan-source-hash text)
    (and text (string-hash text)))

  ;;; -------------------------------------------------------------------------
  ;;; One file
  ;;; -------------------------------------------------------------------------

  ;; A file that cannot be read raises: the build stops where the problem is,
  ;; instead of silently dropping a module from the database.
  (define (scan-source-file progs-dir rel-path)
    (let* ((full (string-append progs-dir "/" rel-path))
           (forms (read-all-forms full))
           (header (scan-header forms)))
      (cond
       ((not header)
        (display "SCAN-SKIP no-header " (current-error-port))
        (display rel-path (current-error-port))
        (newline (current-error-port))
        #f)
       (else
        (let* ((mod-name (cadr header))
               (deps (scan-header->deps header mod-name))
               (text (scan-read-text full)))
          (call-with-values
           (lambda () (scan-collect forms))
           (lambda (exports globals macros impls)
             (make-module-info
              mod-name
              rel-path
              exports
              globals
              macros
              impls
              deps
              '()                          ; compile-uses
              '()                          ; runtime-uses
              (scan-interface-hash exports macros impls)
              (scan-source-hash text)
              #f                           ; artifact-hash
              (time-second (current-time))))))))))

  ;;; -------------------------------------------------------------------------
  ;;; Multiple files and trees
  ;;; -------------------------------------------------------------------------

  (define (scan-source-files progs-dir rel-paths)
    (let loop ((rest rel-paths) (acc '()))
      (cond ((null? rest) (reverse acc))
            (else
             (let ((info (scan-source-file progs-dir (car rest))))
               (loop (cdr rest)
                     (if info (cons info acc) acc)))))))

  (define (scan-source-files->db progs-dir rel-paths)
    (let ((db (make-empty-compiler-db)))
      (for-each
       (lambda (rel-path)
         (let ((info (scan-source-file progs-dir rel-path)))
           (when info
             (compiler-db-set! db info))))
       rel-paths)
      db))
)
