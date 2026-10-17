
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-native.scm
;; DESCRIPTION : the parse states built in C++ (Plugins/TreeSitter/ts_tables.cpp)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The LR(1) construction (ts-tables.scm) and the stages of build-tables!
;; which act on its states (the error state, the merging of compatible
;; states, the unit reductions, the unused states, the order) are done by a
;; port in C++ when TeXmacs has it: the same tables, much faster for large
;; grammars (C++: about 100000 states before merging). The token conflicts,
;; the keywords and the lex tables stay here.
;;
;; The syntax grammar goes to C++ as a text of integers and strings LEN:BYTES
;; (write-syntax-grammar); C++ keeps the states and gives back, as data for
;; read, the tokens which may follow each token and the coincident tokens,
;; then, given the token conflicts and the keywords, the final states.

(texmacs-module (treesit ts-native)
  (:use (treesit ts-base) (treesit ts-grammar) (treesit ts-nfa) (treesit ts-tables)))

(define native-enabled #t)

(define-public (ts-set-native-tables! on?)
  ;; #f: the Scheme code builds the states (to compare the two)
  (set! native-enabled on?))

(define-public (ts-native-tables?)
  (and native-enabled
       (defined? 'tree-sitter-tables-start)
       (not (and (defined? 'getenv) (getenv "TS_SCHEME_TABLES")))))

(define (w-int n port) (display n port) (display " " port))
(define (w-str s port)
  (display (string-length s) port) (display ":" port) (display s port) (display " " port))
(define (w-syms l port) (w-int (length l) port) (for-each (lambda (s) (w-int s port)) l))

(define (w-step s port)
  (w-int (step-symbol s) port)
  (let ((p (step-prec s)))
    (cond ((not p) (w-int 0 port))
          ((integer? p) (w-int 1 port) (w-int p port))
          (else (w-int 2 port) (w-str p port))))
  (w-int (case (step-assoc s) ((left) 1) ((right) 2) (else 0)) port)
  (let ((a (step-alias s)))
    (if a (begin (w-int 1 port) (w-str (car a) port) (w-int (if (cdr a) 1 0) port))
        (w-int 0 port)))
  (let ((f (step-field s)))
    (if f (begin (w-int 1 port) (w-str f port)) (w-int 0 port)))
  (w-int (step-reserved s) port))

(define (write-syntax-grammar g port)
  (let ((lexical (g-ref g 'lexical))
        (externals (g-ref g 'external-tokens))
        (n (g-variable-count g))
        (prods (g-ref g 'productions)))
    (display "tstab1 " port)
    (w-int (vector-length lexical) port)
    (w-int (length externals) port)
    (w-int n port)
    (w-int (or (g-ref g 'word) -1) port)
    (do ((i 0 (+ i 1))) ((= i n))
      (w-str (g-variable-name g i) port)
      (w-int (g-variable-kind g i) port))
    (do ((i 0 (+ i 1))) ((= i (vector-length lexical)))
      (w-str (vector-ref (vector-ref lexical i) 0) port)
      (w-int (vector-ref (vector-ref lexical i) 1) port))
    (for-each (lambda (e)
                (w-str (vector-ref e 0) port)
                (w-int (vector-ref e 1) port)
                (w-int (or (vector-ref e 2) -1) port))
              externals)
    (w-int (vector-length prods) port)
    (do ((i 0 (+ i 1))) ((= i (vector-length prods)))
      (let ((p (vector-ref prods i)))
        (w-int (prod-dynamic p) port)
        (w-int (if (prod-eof? p) 1 0) port)
        (w-int (prod-length p) port)
        (do ((k 0 (+ k 1))) ((= k (prod-length p)))
          (w-step (prod-step p k) port))))
    (do ((i 0 (+ i 1))) ((= i n))
      (let ((r (g-var-prods g i)))
        (w-int (car r) port) (w-int (cdr r) port)))
    (let ((inlines (ahash-table->list (g-ref g 'inlines))))
      (w-int (length inlines) port)
      (for-each (lambda (kv)
                  (w-int (car (car kv)) port) (w-int (cdr (car kv)) port)
                  (w-syms (cdr kv) port))
                inlines))
    (w-syms (g-ref g 'inline) port)
    (w-syms (g-ref g 'extra-symbols) port)
    (let ((conflicts (g-ref g 'conflicts)))
      (w-int (length conflicts) port)
      (for-each (lambda (c) (w-syms c port)) conflicts))
    (let ((precs (g-ref g 'precedences)))
      (w-int (length precs) port)
      (for-each (lambda (l)
                  (w-int (length l) port)
                  (for-each (lambda (e)
                              (w-int (if (eq? (car e) 'name) 0 1) port)
                              (w-str (cdr e) port))
                            l))
                precs))
    (let ((sets (g-ref g 'reserved-word-sets)))
      (w-int (vector-length sets) port)
      (do ((i 0 (+ i 1))) ((= i (vector-length sets)))
        (w-syms (vector-ref sets i) port)))
    (do ((i 0 (+ i 1))) ((= i n))
      (let ((fs (variable-fields g i)))
        (w-int (length fs) port)
        (for-each (lambda (f) (w-str f port)) fs)))
    (w-syms (map car (ahash-table->list (g-ref g 'default-aliases))) port)
    (w-syms (g-ref g 'supertypes) port)
    ;; the NFA of the tokens, for the lex tables
    (let ((nfa (g-ref g 'nfa)))
      (w-int (vector-length nfa) port)
      (do ((i 0 (+ i 1))) ((= i (vector-length nfa)))
        (let ((s (vector-ref nfa i)))
          (case (vector-ref s 0)
            ((advance)
             (w-int 0 port)
             (w-int (length (vector-ref s 1)) port)
             (for-each (lambda (r) (w-int (car r) port) (w-int (cdr r) port)) (vector-ref s 1))
             (w-int (vector-ref s 2) port)
             (w-int (vector-ref s 3) port)
             (w-int (if (vector-ref s 4) 1 0) port))
            ((split) (w-int 1 port) (w-int (vector-ref s 1) port) (w-int (vector-ref s 2) port))
            (else (w-int 2 port) (w-int (vector-ref s 1) port) (w-int (vector-ref s 2) port))))))
    (do ((i 0 (+ i 1))) ((= i (vector-length lexical)))
      (w-int (lex-implicit-precedence (vector-ref lexical i)) port)
      (w-int (lex-start-state (vector-ref lexical i)) port))
    (display "end" port)))

(define (bits->vector s)
  (let ((v (make-vector (string-length s) #f)))
    (do ((i 0 (+ i 1))) ((= i (string-length s)) v)
      (vector-set! v i (char=? (string-ref s i) #\1)))))

(define (state-from-data d)
  ;; (id core eof-gated? reserved terminal-entries non-terminal-entries lex)
  (let ((s (make-state (car d) (list-ref d 1))))
    (vector-set! s 7 (list-ref d 2))
    (vector-set! s 3 (list-ref d 3))
    (vector-set! s 4 (list-ref d 6))
    (for-each (lambda (e) (om-set! (state-terminals s) (car e) (vector (cddr e) (cadr e))))
              (list-ref d 4))
    (for-each (lambda (e) (om-set! (state-nonterminals s) (car e) (cdr e)))
              (list-ref d 5))
    s))

(define (lex-table-from-data l)
  ;; a list of (accept eof-action advance-actions), as in ts-build.scm
  (let ((v (make-gvec)))
    (for-each (lambda (d) (gvec-push! v (vector (car d) (cadr d) (caddr d)))) l)
    v))

(define-public (ts-native-build g identify-keywords make-token-conflict-map . keep?)
  ;; the parse table as build-tables! makes it, with its lex tables:
  ;; (t tc ci keywords lex); with keep? the states stay in C++ and the
  ;; result is (id . t), t without the states, for tree-sitter-tables-export
  (compute-variable-fields! g)
  (let* ((text (call-with-output-string (lambda (port) (write-syntax-grammar g port))))
         (r (string->object (tree-sitter-tables-start text))))
    (when (eq? (car r) 'error) (ts-error (cadr r)))
    (ts-profile "lr")
    ;; r = (ok id following contains without-word)
    (let* ((id (list-ref r 1))
           (following (list->vector (list-ref r 2)))
           (n (vector-length following))
           (tc (make-token-conflict-map g following))
           (ci (begin (ts-profile "token conflicts")
                      (vector n (bits->vector (list-ref r 3)) (bits->vector (list-ref r 4)))))
           (keywords (identify-keywords g tc ci))
           (input (call-with-output-string
                   (lambda (port)
                     (let ((m (vector-ref tc 1)))
                       (do ((i 0 (+ i 1))) ((= i (vector-length m)))
                         (w-int (vector-ref m i) port)))
                     (w-syms keywords port)
                     (when (pair? keep?) (w-int 1 port)))))
           (d (begin (ts-profile "keywords")
                     (string->object (tree-sitter-tables-finish id input))))
           (t (make-ahash-table)))
      (when (eq? (car d) 'error) (ts-error (cadr d)))
      ;; d = (ok states symbols production-infos max-aliased used-conflicts
      ;;      main-lex-table keyword-lex-table)
      (when (list-ref d 1)
        (let ((states (make-gvec)))
          (for-each (lambda (sd) (gvec-push! states (state-from-data sd))) (list-ref d 1))
          (ahash-set! t 'states states)))
      (ahash-set! t 'symbols (list-ref d 2))
      (ahash-set! t 'production-infos (list->vector (list-ref d 3)))
      (ahash-set! t 'max-aliased-production-length (list-ref d 4))
      (ahash-set! t 'external-lex-states '())
      (ahash-set! t 'builder #f)
      (when (pair? (list-ref d 5))
        (g-set! g 'used-conflicts (append (list-ref d 5) (or (g-ref g 'used-conflicts) '()))))
      (ts-profile "states and lex tables (C++)")
      (if (pair? keep?) (cons id t)
          (list t tc ci keywords
                (cons (lex-table-from-data (list-ref d 6)) (lex-table-from-data (list-ref d 7))))))))
