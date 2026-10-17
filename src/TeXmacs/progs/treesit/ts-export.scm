
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-export.scm
;; DESCRIPTION : the tables of a language for the C runtime of tree-sitter
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tables of ts-language.scm written for ts_dynamic.c, which fills a
;; TSLanguage of the C runtime of tree-sitter with them (all the states are
;; "large" ones, the lexers are interpreted).
;;
;; The data is a sequence of tokens separated by spaces: integers, and
;; strings written as LENGTH:BYTES. In order:
;;
;;   "tsdyn1" name
;;   symbol-count alias-count token-count external-token-count state-count
;;   field-count production-id-count max-alias-sequence-length
;;   keyword-capture-token
;;   names (symbols, then aliases), then their metadata (visible 1, named 2,
;;     supertype 4)
;;   field names (without the empty field 0)
;;   action lists: count, then for each: reusable count action...,
;;     an action being 0 state extra repetition (shift), 1 symbol
;;     child-count dynamic-precedence production-id (reduce), 2 (accept)
;;     or 3 (recover); the list 0 is the empty one
;;   for each state: entry count, then symbol value (the action list of a
;;     token, the next state of a non-terminal)
;;   for each state: lex-state external-lex-state reserved-word-set
;;   reserved word sets: count max-size, then the symbols of each, padded
;;     with 0
;;   alias sequences: for each production id, max-alias-sequence-length
;;     symbols
;;   field maps: for each production id, count, then field child inherited
;;   external tokens: the symbol of each, then for each external lex state
;;     the external-token-count booleans
;;   the main lex table and the keyword lex table: state count, then for
;;     each state accept (-1 for none) eof-state (-1) transition count, a
;;     transition being next skip range-count start end ...

(texmacs-module (treesit ts-export)
  (:use (treesit ts-base) (treesit ts-language)))

(define (export-string s port)
  (display (string-length s) port)
  (display ":" port)
  (display s port)
  (display " " port))

(define (export-int n port)
  (display n port)
  (display " " port))

(define (max-child-counts l)
  ;; for each production id, the largest child count of its reductions
  (let ((v (make-vector (vector-length (lang-alias-sequences l)) 0)))
    (for-each
     (lambda (kv)
       (when (vector? (cdr kv))
         (let ((actions (vector-ref (cdr kv) 0)))
           (do ((i 0 (+ i 1))) ((= i (vector-length actions)))
             (let ((a (vector-ref actions i)))
               (when (eq? (vector-ref a 0) 'reduce)
                 (let ((p (vector-ref a 4)))
                   (vector-set! v p (max (vector-ref v p) (vector-ref a 2))))))))))
     (ahash-table->list (lang-table l)))
    v))

(define (export-lex-table table port)
  (export-int (vector-length table) port)
  (do ((i 0 (+ i 1))) ((= i (vector-length table)))
    (let ((s (vector-ref table i)))
      (export-int (or (vector-ref s 0) -1) port)
      (export-int (or (vector-ref s 1) -1) port)
      (export-int (length (vector-ref s 2)) port)
      (for-each (lambda (a)
                  (export-int (vector-ref a 1) port)
                  (export-int (if (vector-ref a 2) 0 1) port)
                  (export-int (length (vector-ref a 0)) port)
                  (for-each (lambda (r) (export-int (car r) port) (export-int (cdr r) port))
                            (vector-ref a 0)))
                (vector-ref s 2)))))

(define-public (ts-export-language l port)
  ;; write the tables of the language l for ts_dynamic.c
  (let* ((n (lang-symbol-count l))
         (states (lang-state-count l))
         (names (lang-names l))
         (metadata (lang-metadata l))
         (field-names (vector->list (lang-field-names l)))
         (aliases (lang-alias-sequences l))
         (children (max-child-counts l))
         (max-alias (let loop ((i 0) (m 1))
                      (if (= i (vector-length aliases)) m
                          (loop (+ i 1) (max m (vector-length (vector-ref aliases i))
                                             (vector-ref children i))))))
         (lists (make-ahash-table))
         (list-order (make-gvec))
         (reserved-ids (make-ahash-table))
         (reserved-order (make-gvec)))
    (define (list-id e)
      (let ((key (list (vector->list (vector-ref e 0)) (vector-ref e 1))))
        (or (ahash-ref lists key)
            (let ((id (gvec-push! list-order e)))
              (ahash-set! lists key id)
              id))))
    (define (reserved-id set)
      (or (ahash-ref reserved-ids set)
          (let ((id (gvec-push! reserved-order set)))
            (ahash-set! reserved-ids set id)
            id)))
    (gvec-push! list-order (vector (vector) #f))
    (ahash-set! lists (list '() #f) 0)
    (reserved-id '())
    ;; the entries of the states, numbering the action lists on the way
    (let ((entries (make-vector states '())))
      (for-each
       (lambda (kv)
         (let ((state (quotient (car kv) n))
               (symbol (remainder (car kv) n))
               (v (cdr kv)))
           (vector-set! entries state
                        (cons (cons symbol (if (vector? v) (list-id v) v))
                              (vector-ref entries state)))))
       (ts-sort (ahash-table->list (lang-table l)) (lambda (a b) (< (car a) (car b)))))
      (do ((i 0 (+ i 1))) ((= i states))
        (reserved-id (vector-ref (lang-lex-mode l i) 2)))
      (display "tsdyn1 " port)
      (export-string (lang-name l) port)
      (for-each (lambda (x) (export-int x port))
                (list n (lang-alias-count l) (lang-token-count l)
                      (vector-length (lang-external-symbols l)) states
                      (length field-names) (vector-length aliases) max-alias
                      (lang-keyword-capture l)))
      (do ((i 0 (+ i 1))) ((= i (vector-length names)))
        (export-string (vector-ref names i) port))
      (do ((i 0 (+ i 1))) ((= i (vector-length metadata)))
        (export-int (vector-ref metadata i) port))
      (for-each (lambda (f) (export-string f port)) field-names)
      ;; the action lists
      (export-int (gvec-length list-order) port)
      (gvec-for-each
       (lambda (e)
         (let ((actions (vector-ref e 0)))
           (export-int (if (vector-ref e 1) 1 0) port)
           (export-int (vector-length actions) port)
           (do ((i 0 (+ i 1))) ((= i (vector-length actions)))
             (let ((a (vector-ref actions i)))
               (case (vector-ref a 0)
                 ((shift) (export-int 0 port) (export-int (vector-ref a 1) port)
                  (export-int (if (vector-ref a 2) 1 0) port)
                  (export-int (if (vector-ref a 3) 1 0) port))
                 ((reduce) (export-int 1 port)
                  (do ((k 1 (+ k 1))) ((= k 5)) (export-int (vector-ref a k) port)))
                 ((accept) (export-int 2 port))
                 ((recover) (export-int 3 port)))))))
       list-order)
      ;; the parse table
      (do ((i 0 (+ i 1))) ((= i states))
        (let ((es (reverse (vector-ref entries i))))
          (export-int (length es) port)
          (for-each (lambda (e) (export-int (car e) port) (export-int (cdr e) port)) es)))
      ;; the lex modes
      (do ((i 0 (+ i 1))) ((= i states))
        (let ((m (lang-lex-mode l i)))
          (export-int (if (< (vector-ref m 0) 0) 65535 (vector-ref m 0)) port)
          (export-int (vector-ref m 1) port)
          (export-int (reserved-id (vector-ref m 2)) port))))
    ;; the reserved words
    (let ((size (let loop ((l (gvec->list reserved-order)) (m 0))
                  (if (null? l) m (loop (cdr l) (max m (length (car l))))))))
      (export-int (gvec-length reserved-order) port)
      (export-int size port)
      (gvec-for-each (lambda (set)
                       (for-each (lambda (s) (export-int s port)) set)
                       (do ((k (length set) (+ k 1))) ((>= k size)) (export-int 0 port)))
                     reserved-order))
    ;; the aliases and the fields of the productions
    (do ((i 0 (+ i 1))) ((= i (vector-length aliases)))
      (let ((seq (vector-ref aliases i)))
        (do ((k 0 (+ k 1))) ((= k max-alias))
          (export-int (if (< k (vector-length seq)) (vector-ref seq k) 0) port))))
    (do ((i 0 (+ i 1))) ((= i (vector-length aliases)))
      (let ((fields (vector-ref (lang-field-maps l) i)))
        (export-int (length fields) port)
        (for-each (lambda (f)
                    (export-int (+ 1 (ts-position (vector-ref f 0) field-names)) port)
                    (export-int (vector-ref f 1) port)
                    (export-int (if (vector-ref f 2) 1 0) port))
                  fields)))
    ;; the external tokens
    (let ((ext (lang-external-symbols l)))
      (do ((i 0 (+ i 1))) ((= i (vector-length ext))) (export-int (vector-ref ext i) port))
      (export-int (vector-length (lang-external-states l)) port)
      (do ((i 0 (+ i 1))) ((= i (vector-length (lang-external-states l))))
        (let ((v (vector-ref (lang-external-states l) i)))
          (do ((k 0 (+ k 1))) ((= k (vector-length v)))
            (export-int (if (vector-ref v k) 1 0) port)))))
    ;; the lexers
    (export-lex-table (lang-lex-table l) port)
    (export-lex-table (lang-keyword-table l) port)
    (newline port)))

(define-public (ts-export-language-file l file)
  (call-with-output-file file (lambda (port) (ts-export-language l port))))

(define-public (ts-export-language->string l)
  (call-with-output-string (lambda (port) (ts-export-language l port))))

(define-public (ts-c-language l)
  ;; the language l in the C runtime of tree-sitter (an integer), or #f
  (and (defined? 'tree-sitter-language-new)
       (let ((id (tree-sitter-language-new (ts-export-language->string l))))
         (and (>= id 0) id))))
