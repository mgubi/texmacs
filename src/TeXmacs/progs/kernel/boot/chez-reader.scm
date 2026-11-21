;;; -*- mode: scheme -*-
;;
;; MODULE      : chez-reader.scm
;; DESCRIPTION : Reading the Guile-flavoured sources of TeXmacs on Chez Scheme
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the
;; file LICENSE in the root directory or
;; <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; The sources of TeXmacs/progs, of the plug-ins and of the user are written
;; for Guile (and run on s7 and femtolisp too).  Before Chez reads them, their
;; text is normalized (Guile's lexical syntax) and the dialect tests are
;; folded.  Both the module compiler (src/Scheme/Chez/compiler-scan.scm) and
;; the loader of sources at run time (tm-load-file, boot-chez.scm) read them
;; through this library, so that they see the same forms.

(library (chez-reader)
  (export read-raw-forms fold-dialect fold-dialect-forms
          normalize-guile-string-escapes tm-read-guile-file)
  (import (chezscheme))

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

          ;; In Guile a backslash is an ordinary symbol character (left\{ in
          ;; tmtex.scm), in Chez it escapes the next one
          ((and (not in-string?) (eqv? c #\\))
           (emit "\\x5c;")
           (loop (+ i 1) #f))

          ;; Guile reads [ and ] as symbols too (latex-command-drd.scm),
          ;; Chez as parentheses
          ;; and so are { and }, which Chez reserves
          ((and (not in-string?) (memv c '(#\[ #\] #\{ #\})))
           (emit (case c
                   ((#\[) "\\x5b;") ((#\]) "\\x5d;")
                   ((#\{) "\\x7b;") (else "\\x7d;")))
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
      ;; Guile's curried definition: (define ((f a) b) body) is
      ;; (define (f a) (lambda (b) body))
      ((and (memq (car x) '(define define-public provide-public))
            (pair? (cdr x)) (pair? (cadr x)) (pair? (caadr x)))
       (fold-dialect `(,(car x) ,(caadr x) (lambda ,(cdadr x) ,@(cddr x)))))
      ;; the head of a definition is not a call
      ((and (memq (car x) definers) (pair? (cdr x)) (pair? (cadr x)))
       (cons* (car x) (cadr x) (map fold-dialect (cddr x))))
      ((and (symbol? (car x)) (null? (cdr x)) (assq (car x) dialect-constants))
       => cdr)
      (else
       (let ((y (let walk ((l x))
                  (cond ((pair? l) (cons (fold-dialect (car l)) (walk (cdr l))))
                        (else l)))))
         ;; simplify only where a dialect test was folded: the arguments
         ;; of a macro may be data, such as (or) in a grammar
         (cond
          ;; Chez modules have no Guile `with-module': the public
          ;; definitions it wraps are global anyway
          ((and (eq? (car y) 'with-module) (pair? (cdr y)))
           (cons 'begin (cddr y)))
          ((equal? y x) y)
          (else
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
           (else y))))))))

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

  ;; Guile and s7 evaluate the initial values of a let from left to right,
  ;; and TeXmacs code relies on it (tmdoc-rewrite: two calls which fill the
  ;; same table); Chez evaluates them in any order.  A let whose values are
  ;; calls gets them in sequence, through temporaries, with the same scope:
  ;;   (let ((a (f)) (b (g))) body)
  ;;   => (let ((%let-1 (f))) (let ((%let-2 (g))) (let ((a %let-1) (b %let-2)) body)))
  ;; Named lets too; quoted data is left alone.
  (define let-counter 0)

  (define (fresh-let-temp)
    (set! let-counter (+ let-counter 1))
    (string->symbol (string-append "%let-" (number->string let-counter))))

  (define (binding-list? l)
    (and (list? l)
         (for-all (lambda (b) (and (pair? b) (symbol? (car b))
                                   (pair? (cdr b)) (null? (cddr b))))
                  l)))

  (define (sequence-let bindings wrap)
    ;; wrap: the let (or named let) taking the new bindings
    (if (< (length (filter (lambda (b) (pair? (cadr b))) bindings)) 2)
        (wrap bindings)
        (let ((temps (map (lambda (b) (if (pair? (cadr b)) (fresh-let-temp) #f))
                          bindings)))
          (let loop ((bs bindings) (ts temps))
            (cond ((null? bs)
                   (wrap (map (lambda (b t) (if t (list (car b) t) b))
                              bindings temps)))
                  ((car ts) `(let ((,(car ts) ,(cadar bs))) ,(loop (cdr bs) (cdr ts))))
                  (else (loop (cdr bs) (cdr ts))))))))

  (define (sequence-lets x)
    (cond
      ((not (pair? x)) x)
      ((memq (car x) '(quote quasiquote)) x)
      ((and (eq? (car x) 'let) (pair? (cdr x)) (binding-list? (cadr x))
            (pair? (cddr x)))
       (let ((bindings (map (lambda (b) (list (car b) (sequence-lets (cadr b))))
                            (cadr x)))
             (body (map sequence-lets (cddr x))))
         (sequence-let bindings (lambda (bs) `(let ,bs ,@body)))))
      ((and (eq? (car x) 'let) (pair? (cdr x)) (symbol? (cadr x))
            (pair? (cddr x)) (binding-list? (caddr x)) (pair? (cdddr x)))
       (let ((name (cadr x))
             (bindings (map (lambda (b) (list (car b) (sequence-lets (cadr b))))
                            (caddr x)))
             (body (map sequence-lets (cdddr x))))
         (sequence-let bindings (lambda (bs) `(let ,name ,bs ,@body)))))
      (else
       (let walk ((l x))
         (cond ((pair? l) (cons (sequence-lets (car l)) (walk (cdr l))))
               (else l))))))

  ;; The forms of a source file, as Chez should see them
  (define (tm-read-guile-file file-path)
    (map sequence-lets (fold-dialect-forms (read-raw-forms file-path)))))
