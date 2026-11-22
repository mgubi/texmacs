;;; macro-closure-report.scm -- record every macro's transitive helper set.
;;;
;;;   chez --script src/Scheme/Chez/macro-closure-report.scm [out.tsv]
;;;
;;; This is the offline counterpart of linter phase 4.  It needs no TeXmacs
;;; process and no build: it reads the progs tree, indexes it, computes the
;;; dependency closure of every macro definition and writes the sets out.
;;;
;;;   chez --script src/Scheme/Chez/macro-closure-report.scm            # ~1 s
;;;   TEXMACS_PATH=/path/to/TeXmacs chez --script ... /tmp/closure.tsv
;;;
;;; It is NOT a build-order tool.  A closure says which definitions must be
;;; live before a macro can be *expanded*; it says nothing about which module
;;; must be *compiled* first, and a form still needs its own module context to
;;; expand at all.  The authoritative numbers come from `TeXmacs -lint`, which
;;; runs inside a real kernel.

(define here
  (let loop ((d (current-directory)))
    (cond ((file-exists? (string-append d "/compiler-oracle.scm")) d)
          ((file-exists? (string-append d "/src/Scheme/Chez/compiler-oracle.scm"))
           (string-append d "/src/Scheme/Chez"))
          ((file-exists? (string-append d "/../src/Scheme/Chez/compiler-oracle.scm"))
           (string-append d "/../src/Scheme/Chez"))
          (else d))))

(load (string-append here "/tarjan.scm"))
(load (string-append here "/compiler-oracle.scm"))

;;; ------------------------------------------------------------------- args

(define args (command-line-arguments))
(define *mcr-out* (if (pair? args) (car args) "macro-closure.tsv"))

(define *mcr-root*
  (let ((p (getenv "TEXMACS_PATH")))
    (cond ((and p (file-exists? (string-append p "/progs")))
           (string-append p "/progs"))
          ((file-exists? "TeXmacs/progs") "TeXmacs/progs")
          ;; this file lives in <repo>/src/Scheme/Chez
          ((file-exists? (string-append here "/../../../TeXmacs/progs"))
           (string-append here "/../../../TeXmacs/progs"))
          (else (error 'macro-closure-report "cannot find TeXmacs/progs")))))

;;; ------------------------------------------------------------- file walk

(define (scm? p)
  (let ((n (string-length p)))
    (and (> n 4) (string=? (substring p (- n 4) n) ".scm"))))

(define (walk-files dir)
  (define acc '())
  (define (walk rel)
    (let ((full (if (string=? rel "") dir (string-append dir "/" rel))))
      (if (file-directory? full)
          (for-each (lambda (n)
                      (unless (member n '("." ".."))
                        (walk (if (string=? rel "") n (string-append rel "/" n)))))
                    (directory-list full))
          (when (scm? rel) (set! acc (cons rel acc))))))
  (walk "")
  (sort (lambda (a b) (string<? a b)) acc))

(define (read-all-forms path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((acc '()))
        (let ((f (guard (e (#t 'read-error)) (read port))))
          (cond ((eof-object? f) (reverse acc))
                ((eq? f 'read-error) (reverse acc))
                (else (loop (cons f acc)))))))))

;; A file with no (texmacs-module ...) header is bootstrap glue (the four
;; init-*.scm scripts), not part of the module tree.  Indexing them is actively
;; harmful: init-linter.scm defines a two-argument (func? p kind) stub that
;; sorts before kernel/library/base.scm's real three-argument definition, so the
;; index would record the stub and every (func? x 'tag n) call would look wrong.
(define (has-header? rel)
  (and (find (lambda (f) (and (pair? f) (eq? (car f) 'texmacs-module)))
             (read-all-forms (string-append *mcr-root* "/" rel)))
       #t))

(define *mcr-files*
  (mc-filter has-header?
             (mc-filter (lambda (p)
                          (not (and (>= (string-length p) 11)
                                    (string=? (substring p 0 11) "kernel/boot"))))
                        (walk-files *mcr-root*))))

;;; ------------------------------------------------------------------ index
;;
;; A site is #(name kind inner top file module).  INNER is the define form; TOP
;; is the enclosing top-level form, the unit that has to be evaluated to
;; install it.

(define macro-definers
  '(define-syntax define-macro define-public-macro define-module-macro
    define-export-macro tm-define-macro))
(define value-definers
  '(define define-public provide-public tm-define))

(define defs (make-eq-hashtable))
(define duplicates '())
(define n-sites 0)

(define (site-name s)   (vector-ref s 0))
(define (site-kind s)   (vector-ref s 1))
(define (site-inner s)  (vector-ref s 2))
(define (site-top s)    (vector-ref s 3))
(define (site-file s)   (vector-ref s 4))
(define (site-module s) (vector-ref s 5))

(define (leftmost-symbol x) (if (pair? x) (leftmost-symbol (car x)) x))

(define (index-form! top form file module)
  (define (walk x)
    (when (pair? x)
      (cond
        ((memq (car x) '(quote syntax syntax-rules quasiquote quasisyntax)) (void))
        (else
         (when (and (symbol? (car x))
                    (or (memq (car x) macro-definers) (memq (car x) value-definers))
                    (pair? (cdr x)))
           (let ((n (leftmost-symbol (cadr x))))
             (when (symbol? n)
               (set! n-sites (+ n-sites 1))
               (if (hashtable-ref defs n #f)
                   (unless (memq n duplicates) (set! duplicates (cons n duplicates)))
                   (hashtable-set! defs n
                                  (vector n
                                          (if (memq (car x) macro-definers) 'macro 'value)
                                          x top file module))))))
         (mc-walk-each x walk)))))
  (walk form))

(define (module-of forms)
  (let ((h (find (lambda (f) (and (pair? f) (eq? (car f) 'texmacs-module))) forms)))
    (if (and h (pair? (cdr h))) (cadr h) '(no-header))))

(define (index-file! rel)
  (let* ((path (string-append *mcr-root* "/" rel))
         (forms (read-all-forms path))
         (mod (module-of forms)))
    (for-each (lambda (f) (index-form! f f rel mod)) forms)))

;;; ------------------------------------------------------- dependency rule
;;
;; What a site needs in order to be *installed*, plus -- for a macro -- what its
;; consumers need in order to *expand* it.

(define (site-deps site)
  (let* ((kind (site-kind site))
         (inner (site-inner site))
         (top (site-top site))
         (head (and (pair? top) (car top))))
    (append
      ;; the definer itself (define-public-macro, tm-define, ...)
      (if (and (symbol? head) (hashtable-ref defs head #f)) (list head) '())
      (if (eq? kind 'macro)
          (or (mc-macro-deps inner) '())
          (mc-free-references top)))))

;;; ------------------------------------------------------------------ main

(define (sort-syms xs)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) xs))

(define (join-syms xs sep)
  (cond ((null? xs) "")
        ((null? (cdr xs)) (symbol->string (car xs)))
        (else (string-append (symbol->string (car xs)) sep
                             (join-syms (cdr xs) sep)))))

(define (show-list xs limit)
  (let ((n (length xs)))
    (if (<= n limit)
        (join-syms xs " ")
        (string-append (join-syms (list-head xs limit) " ")
                       (format " ... (~a more)" (- n limit))))))

(printf "~n============================================================~n")
(printf " macro dependency closure~n")
(printf "============================================================~n")
(printf "root  ~a~n" *mcr-root*)

(let ((t0 (real-time)))
  (for-each index-file! *mcr-files*)
  (printf "indexed ~a module files, ~a definitions (~a distinct names) in ~a ms~n"
          (length *mcr-files*) n-sites (hashtable-size defs) (- (real-time) t0)))
(printf "duplicate names: ~a (first site wins)~n" (length duplicates))

(define targets
  (sort-syms (mc-filter (lambda (n) (eq? (site-kind (hashtable-ref defs n #f)) 'macro))
                        (vector->list (hashtable-keys defs)))))
(printf "macro targets: ~a~n" (length targets))

(let-values (((order closures cycles back) (mc-closures defs site-deps targets)))
  (define closure-sizes
    (map (lambda (t) (length (hashtable-ref closures t '()))) targets))
  (define total (apply + closure-sizes))

  (printf "~ninstall order: ~a definitions~n" (length order))
  (printf "stop rule fired (revisited nodes): ~a~n" back)
  (printf "cycles (SCCs with more than one member): ~a~n" (length cycles))
  (when (pair? cycles)
    (printf "  largest: ~a names~n" (apply max (map length cycles)))
    (for-each (lambda (c)
                (printf "    ~a: ~a~n" (length c) (show-list (sort-syms c) 8)))
              (list-head (sort (lambda (a b) (> (length a) (length b))) cycles)
                         (min 8 (length cycles)))))

  (printf "~nper-macro closure: total ~a entries, mean ~a, max ~a, empty ~a~n"
          total (quotient total (max 1 (length targets)))
          (apply max (cons 0 closure-sizes))
          (length (mc-filter (lambda (n) (= n 0)) closure-sizes)))

  (printf "~n--- 10 macros with the largest closure ---~n")
  (for-each
    (lambda (t)
      (let ((c (hashtable-ref closures t '())))
        (printf "  ~4d  ~a~n        ~a~n" (length c) t (show-list (sort-syms c) 10))))
    (list-head (sort (lambda (a b) (> (length (hashtable-ref closures a '()))
                                      (length (hashtable-ref closures b '()))))
                     targets)
               (min 10 (length targets))))

  ;; ---- record ------------------------------------------------------------
  (when (file-exists? *mcr-out*) (delete-file *mcr-out*))
  (call-with-output-file *mcr-out*
    (lambda (port)
      (fprintf port "name\tdefiner\tfile\tmodule\tclosure-size\tclosure~n")
      (for-each
        (lambda (t)
          (let* ((s (hashtable-ref defs t #f))
                 (c (sort-syms (hashtable-ref closures t '()))))
            (fprintf port "~a\t~a\t~a\t~a\t~a\t~a~n"
                     t (car (site-inner s)) (site-file s) (site-module s)
                     (length c)
                     (if (null? c) "-" (join-syms c ",")))))
        targets)))
  (printf "~nWROTE ~a (~a rows)~n" *mcr-out* (length targets))
  (printf "DONE~n")
  (flush-output-port (current-output-port)))
