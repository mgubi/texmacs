;;; TODO: UNDER REVIEWED AI GENERATED CODE.
;;;
;;; compile-graph-test.scm -- Tests for compile-graph.scm.
;;;
;;; Run from the repository root or from src/Scheme/Chez:
;;;   chez --script src/Scheme/Chez/compile-graph-test.scm
;;;
;;; Exits with status 0 when every check passes, 1 otherwise.

(define (first-existing paths)
  (cond ((null? paths) #f)
        ((file-exists? (car paths)) (car paths))
        (else (first-existing (cdr paths)))))

(define compiler-dir
  (let ((db (first-existing '("compiler-db.scm"
                              "src/Scheme/Chez/compiler-db.scm"
                              "texmacs/src/Scheme/Chez/compiler-db.scm"))))
    (and db
         (let loop ((i (- (string-length db) 1)))
           (cond ((< i 0) ".")
                 ((char=? (string-ref db i) #\/) (substring db 0 i))
                 (else (loop (- i 1))))))))

(unless compiler-dir
  (error 'compile-graph-test "cannot find compiler-db.scm"))

(load (string-append compiler-dir "/tarjan.scm"))
(import (tarjan))
(load (string-append compiler-dir "/compiler-db.scm"))
(import (compiler-db))
(load (string-append compiler-dir "/compiler-scan.scm"))
(import (compiler-scan))
(load (string-append compiler-dir "/compile-graph.scm"))
(import (compile-graph))
(load (string-append compiler-dir "/compile-inventory.scm"))
(import (compile-inventory))

(define checks 0)
(define failures 0)

(define (check label expected actual)
  (set! checks (+ checks 1))
  (if (equal? expected actual)
      (begin
        (display "PASS ") (display label) (newline))
      (begin
        (set! failures (+ failures 1))
        (display "FAIL ") (display label)
        (display " expected ") (write expected)
        (display " got ") (write actual)
        (newline))))

(define (check-true label actual)
  (check label #t (and actual #t)))

(define (mod-name<? a b)
  (cond ((null? a) (pair? b))
        ((null? b) #f)
        ((eq? (car a) (car b)) (mod-name<? (cdr a) (cdr b)))
        (else (string<? (symbol->string (car a))
                        (symbol->string (car b))))))

;;; Synthetic modules ---------------------------------------------------------

(define (mk name deps . opt-macros)
  (make-module-info name "synthetic.scm" '() '()
                    (if (null? opt-macros) '() (car opt-macros))
                    '() deps '() '() #f #f #f #f))

(define (graph . infos)
  (make-compile-graph-from-modules infos))

;;; Construction --------------------------------------------------------------

(define linear (graph (mk '(t a) '())
                      (mk '(t b) '((t a)))
                      (mk '(t c) '((t b)))))

(check "nodes are sorted" '((t a) (t b) (t c)) (compile-graph-nodes linear))
(check "prereqs of c" '((t b)) (hashtable-ref (compile-graph-prereqs linear) '(t c) #f))
(check "dependents of a" '((t b)) (hashtable-ref (compile-graph-dependents linear) '(t a) #f))
(check "leaf has no dependents" '() (hashtable-ref (compile-graph-dependents linear) '(t c) #f))

;; A dependency outside the graph is dropped rather than invented.
(define with-outsider (graph (mk '(t a) '((ice-9 rdelim) (t a)))))
(check "external dep is not a node" #f
       (and (member '(ice-9 rdelim) (compile-graph-nodes with-outsider)) #t))
(check "self dep survives as a self loop" '((t a))
       (hashtable-ref (compile-graph-prereqs with-outsider) '(t a) #f))

;;; Tarjan and topological order ----------------------------------------------

(check "linear scc" '(((t a)) ((t b)) ((t c))) (compile-graph-scc linear))
(check "linear topological" '(((t a)) ((t b)) ((t c)))
       (compile-graph-topological-sort linear))
(check "linear has no cycles" '() (compile-graph-cycles linear))

(define diamond (graph (mk '(t a) '())
                       (mk '(t b) '((t a)))
                       (mk '(t c) '((t a)))
                       (mk '(t d) '((t b) (t c)))))

(check "diamond waves" '(((t a)) ((t b) (t c)) ((t d)))
       (compile-graph-waves diamond))
(check "diamond topological" '(((t a)) ((t b)) ((t c)) ((t d)))
       (compile-graph-topological-sort diamond))

(define forest (graph (mk '(t a) '())
                      (mk '(t b) '((t a)))
                      (mk '(t x) '())
                      (mk '(t y) '((t x)))))

(check "forest wave 0" '((t a) (t x)) (car (compile-graph-waves forest)))
(check "forest wave 1" '((t b) (t y)) (cadr (compile-graph-waves forest)))

(define two-cycle (graph (mk '(t a) '((t b)))
                         (mk '(t b) '((t a)))
                         (mk '(t c) '((t b)))))

(check "two-cycle is one component" '((t a) (t b))
       (sort mod-name<? (car (compile-graph-cycles two-cycle))))
(check "two-cycle waves" '(((t a) (t b)) ((t c))) (compile-graph-waves two-cycle))

(define figure-eight (graph (mk '(t a) '((t b)))
                            (mk '(t b) '((t a) (t c)))
                            (mk '(t c) '((t b)))))

(check "figure-8 is one component" 1 (length (compile-graph-cycles figure-eight)))
(check "figure-8 covers three nodes" 3 (length (car (compile-graph-cycles figure-eight))))

(define self-loop (graph (mk '(t s) '((t s)))))
(check "self loop is a cycle" '(((t s))) (compile-graph-cycles self-loop))
(check "self loop wave" '(((t s))) (compile-graph-waves self-loop))

;;; Subgraphs ----------------------------------------------------------------

(define sub (compile-graph-subgraph linear '((t b) (t c))))
(check "subgraph nodes" '((t b) (t c)) (compile-graph-nodes sub))
(check "subgraph drops the outside edge" '()
       (hashtable-ref (compile-graph-prereqs sub) '(t b) #f))
(check "subgraph waves" '(((t b)) ((t c))) (compile-graph-waves sub))
(check "subgraph ignores unknown mod-names" '((t b) (t c))
       (compile-graph-nodes (compile-graph-subgraph linear '((t b) (t c) (nope nope)))))

;;; Macro safety -------------------------------------------------------------

(define macros (graph (mk '(t a) '())
                      (mk '(t b) '((t a)) '(mac-a))
                      (mk '(t c) '((t b)))))

(check "macro nodes" '((t b)) (compile-graph-macro-nodes macros))
(check-true "a is macro free" (compile-graph-macro-free? macros '((t a))))
(check "b defines a macro" #f (compile-graph-macro-free? macros '((t b))))
(check "whole graph is not macro free" #f (compile-graph-macro-free? macros))
(check-true "macro-free subset" (compile-graph-macro-free? macros '((t a) (t c))))

;;; Live TeXmacs tree ---------------------------------------------------------

(define (live-tests progs)
  ;; The whitelist is a library now, so it is imported rather than reconstructed
  ;; by reading the driver's `define` forms. Same list, one source.
  (define files *source-files*)

  (define db (scan-source-files->db progs files))
  (define g (make-compile-graph-from-db db))
  (define nodes (compile-graph-nodes g))
  (define waves (compile-graph-waves g))
  (define cycles (compile-graph-cycles g))
  (define prereqs (compile-graph-prereqs g))

  (define (wave-of mod-name)
    (let loop ((ws waves) (i 0))
      (cond ((null? ws) #f)
            ((member mod-name (car ws)) i)
            (else (loop (cdr ws) (+ i 1))))))

  (define (same-cycle? a b)
    (and (exists (lambda (c) (and (member a c) (member b c))) cycles) #t))

  (define (same-wave? . mod-names)
    (let ((ws (map wave-of mod-names)))
      (and (car ws) (apply = ws) #t)))

  (check "one node per whitelisted file" (length files) (length nodes))

  (check "wave partition covers every node" nodes
         (sort mod-name<? (apply append waves)))

  ;; A module may depend on its own wave (that is what an SCC is) but never on
  ;; a later one.
  (check "no module depends on a later wave" '()
         (let loop ((ws waves) (seen '()) (bad '()))
           (if (null? ws)
               (reverse bad)
               (let ((reachable (append (car ws) seen)))
                 (loop (cdr ws)
                       reachable
                       (append
                        (reverse
                         (filter (lambda (s)
                                   (exists (lambda (p) (not (member p reachable)))
                                           (hashtable-ref prereqs s '())))
                                 (car ws)))
                        bad))))))

  (check "wave 0 has no prerequisites" '()
         (filter (lambda (s) (pair? (hashtable-ref prereqs s '()))) (car waves)))

  (check-true "cycle tm-modes/tm-plugins"
              (same-cycle? '(kernel texmacs tm-modes) '(kernel texmacs tm-plugins)))
  (check-true "cycle link-navigate/link-extern"
              (same-cycle? '(link link-navigate) '(link link-extern)))
  (check-true "cycle tm-print/tm-files"
              (same-cycle? '(texmacs texmacs tm-print) '(texmacs texmacs tm-files)))
  (check-true "cycle latex-tools/texout"
              (same-cycle? '(convert latex latex-tools) '(convert latex texout)))
  (check-true "cycle program-edit/program-menu"
              (same-cycle? '(dynamic program-edit) '(dynamic program-menu)))

  ;; bibtex: plain needs bib-utils, the styles need both.
  (check-true "plain comes after bib-utils"
              (> (wave-of '(bibtex plain)) (wave-of '(bibtex bib-utils))))
  (check-true "the bibtex styles share one wave"
              (same-wave? '(bibtex abbrv) '(bibtex acm)
                          '(bibtex siam) '(bibtex unsrt)))

  ;; text/cyrillic: five independent keyboard maps, all in the same wave.
  (check-true "the cyrillic maps share one wave"
              (same-wave? '(text cyrillic cp1251-kbd)
                          '(text cyrillic jcuken-kbd)
                          '(text cyrillic koi8-kbd)
                          '(text cyrillic translit-kbd)
                          '(text cyrillic yawerty-kbd)))

  ;; prog: the language modes all wait for prog-edit.
  (check-true "python-edit comes after prog-edit"
              (> (wave-of '(prog python-edit)) (wave-of '(prog prog-edit)))))

(define progs
  (let* ((root (or (getenv "TEXMACS_PATH") ""))
         (p (first-existing (list (string-append root "/progs")
                                  "TeXmacs/progs"
                                  "texmacs/TeXmacs/progs"
                                  ;; running from src/Scheme/Chez
                                  "../../../TeXmacs/progs"))))
    (and p (file-exists? p) p)))

(if progs
    (live-tests progs)
    (begin
      (display "SKIP live tree checks: set TEXMACS_PATH or run from the repository root")
      (newline)))

;;; Summary -------------------------------------------------------------------

(display "------------------------------------------------------------") (newline)
(display checks) (display " checks, ")
(display failures) (display " failures") (newline)
(exit (if (= failures 0) 0 1))
