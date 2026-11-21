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

          ;; A quote opens a string outside one and closes it inside one.
          ((eqv? c #\")
           (emit c)
           (loop (+ i 1) (not in-string?)))

          ;; Comments are copied whole, so a quote inside one stays put.
          ((and (not in-string?) (eqv? c #\;))
           (let ((end (line-end i)))
             (emit (substring str i end))
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

  (define (read-all-forms file-path)
    (let* ((port (open-input-file file-path))
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
          (values (reverse exports)
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
