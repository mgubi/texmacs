
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-language.scm
;; DESCRIPTION : the tables of a tree-sitter language, as the parser uses them
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What render.rs writes into parser.c, as Scheme data: the symbols are
;; renumbered as the runtime numbers them (end, tokens, externals, then the
;; non-terminals and the aliases), the parse actions of the tokens and the
;; gotos of the non-terminals are kept in one table indexed by
;; state * symbol-count + symbol, and the lex tables are vectors of lex
;; states.
;;
;; A language is a vector, see the accessors below. It is plain data (strings,
;; numbers, booleans, vectors and lists), so that it can be written to a file
;; and read back (ts-save-language, ts-load-language).
;;
;; Actions are vectors:
;;
;;   #(shift state extra? repetition?)  #(reduce symbol count dynamic production)
;;   #(accept)  #(recover)

(texmacs-module (treesit ts-language)
  (:use (treesit ts-base) (treesit ts-grammar) (treesit ts-nfa) (treesit ts-tables)))

(define-public ts-sym-error -1)
(define-public ts-sym-error-repeat -2)

;; language fields
(define-public (lang-name l) (vector-ref l 0))
(define-public (lang-symbol-count l) (vector-ref l 1))
(define-public (lang-token-count l) (vector-ref l 2))
(define-public (lang-state-count l) (vector-ref l 3))
(define-public (lang-names l) (vector-ref l 4))          ; by symbol (and alias)
(define-public (lang-metadata l) (vector-ref l 5))       ; visible 1, named 2, supertype 4
(define-public (lang-table l) (vector-ref l 6))          ; key -> entry or state
(define-public (lang-lex-modes l) (vector-ref l 7))      ; #(lex external reserved)
(define-public (lang-lex-table l) (vector-ref l 8))
(define-public (lang-keyword-table l) (vector-ref l 9))
(define-public (lang-keyword-capture l) (vector-ref l 10))
(define-public (lang-alias-sequences l) (vector-ref l 11))
(define-public (lang-field-maps l) (vector-ref l 12))
(define-public (lang-external-symbols l) (vector-ref l 13))  ; external index -> symbol
(define-public (lang-external-states l) (vector-ref l 14))   ; id -> vector of booleans
(define-public (lang-field-names l) (vector-ref l 15))
(define-public (lang-supertypes l) (vector-ref l 16))
(define-public (lang-external-scanner l) (vector-ref l 17))
(define-public (set-lang-external-scanner! l s) (vector-set! l 17 s))
(define-public (lang-alias-count l) (vector-ref l 18))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From the tables of the generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbol-metadata g s)
  ;; (name . kind)
  (cond ((or (= s sym-end) (= s sym-eont)) (cons "end" vt-hidden))
        ((non-terminal? s) (cons (g-variable-name g (sym-index s)) (g-variable-kind g (sym-index s))))
        ((terminal? s) (let ((v (vector-ref (g-ref g 'lexical) (sym-index s))))
                         (cons (lex-name v) (lex-kind v))))
        (else (let ((e (list-ref (g-ref g 'external-tokens) (sym-index s))))
                (cons (vector-ref e 0) (vector-ref e 1))))))

(define-public (tables->language g t)
  (let* ((symbols (ahash-ref t 'symbols))
         (n-symbols (length symbols))
         (ids (make-ahash-table))
         (default-aliases (g-ref g 'default-aliases))
         (supertypes (g-ref g 'supertypes)))
    (ts-map-index (lambda (i s) (ahash-set! ids s i)) symbols)
    (ahash-set! ids sym-eont 0)
    (let* ((token-count (length (ts-filter (lambda (s) (not (non-terminal? s))) symbols)))
           (names (make-gvec))
           (metadata (make-gvec))
           (unique-aliases (make-gvec))
           (alias-ids (make-ahash-table)))
      (for-each
       (lambda (s)
         (let ((alias (ahash-ref default-aliases s))
               (md (symbol-metadata g s)))
           (gvec-push! names (if alias (car alias) (car md)))
           (gvec-push! metadata
                       (if alias (if (cdr alias) 3 1)
                           (let ((k (cdr md)))
                             (cond ((= k vt-named) 3)
                                   ((= k vt-anonymous) 1)
                                   ((= k vt-hidden) (if (member s supertypes) 6 2))
                                   (else 0)))))))
       symbols)
      ;; the aliases of the productions
      (define (alias-id alias)
        (or (ahash-ref alias-ids alias)
            (let* ((kind (if (cdr alias) vt-named vt-anonymous))
                   (existing (ts-list-index
                              (lambda (s)
                                (let ((a (ahash-ref default-aliases s)))
                                  (if a (equal? a alias)
                                      (let ((md (symbol-metadata g s)))
                                        (and (equal? (car md) (car alias)) (= (cdr md) kind))))))
                              symbols))
                   (id (or existing
                           (let ((k (gvec-length unique-aliases)))
                             (gvec-push! unique-aliases alias)
                             (+ n-symbols k)))))
              (ahash-set! alias-ids alias id)
              id)))
      (let* ((infos (ahash-ref t 'production-infos))
             (alias-sequences
              (list->vector
               (ts-map (lambda (info) (list->vector (ts-map (lambda (a) (if a (alias-id a) 0))
                                                            (car info))))
                       (vector->list infos))))
             (field-names (ts-sort (ts-dedup (apply append (ts-map (lambda (info) (map car (cdr info)))
                                                                   (vector->list infos))))
                                   string<?))
             (field-maps
              (list->vector
               (ts-map (lambda (info)
                         (apply append
                                (ts-map (lambda (f)
                                          (ts-map (lambda (loc) (vector (car f) (car loc) (cdr loc)))
                                                  (cdr f)))
                                        (cdr info))))
                       (vector->list infos)))))
        (gvec-for-each (lambda (a)
                         (gvec-push! names (car a))
                         (gvec-push! metadata (if (cdr a) 3 1)))
                       unique-aliases)
        (let* ((states (ahash-ref t 'states))
               (n-states (gvec-length states))
               (table (make-ahash-table))
               (lex-modes (make-vector n-states #f))
               (word (g-ref g 'word)))
          (define (convert-action a)
            (case (car a)
              ((shift) (vector 'shift (cadr a) #f (caddr a)))
              ((shift-extra) (vector 'shift 0 #t #f))
              ((recover) (vector 'recover))
              ((accept) (vector 'accept))
              ((reduce) (vector 'reduce (ahash-ref ids (cadr a)) (caddr a) (cadddr a) (list-ref a 4)))
              (else (ts-error "unknown action" a))))
          (do ((i 0 (+ i 1))) ((= i n-states))
            (let ((s (gvec-ref states i)))
              (om-for-each
               (lambda (sym e)
                 (ahash-set! table (+ (* i n-symbols) (ahash-ref ids sym))
                             (vector (list->vector (ts-map convert-action (entry-actions e)))
                                     (entry-reusable? e))))
               (state-terminals s))
              (om-for-each
               (lambda (sym a)
                 (ahash-set! table (+ (* i n-symbols) (ahash-ref ids sym))
                             (if (eq? (car a) 'goto) (cadr a) i)))
               (state-nonterminals s))
              (vector-set! lex-modes i
                           (vector (if (om-has? (state-terminals s) sym-eont) -1 (state-lex s))
                                   (state-external-lex s)
                                   (ts-map (lambda (r) (ahash-ref ids r))
                                           (ts-filter (lambda (r) (ahash-ref ids r))
                                                      (state-reserved s)))))))
          (define (convert-lex-table lt)
            (list->vector
             (ts-map (lambda (s)
                       (vector (let ((a (vector-ref s 0))) (if a (ahash-ref ids a) #f))
                               (let ((e (vector-ref s 1))) (if e (car e) #f))
                               (ts-map (lambda (a) (vector (car a) (cadr a) (cddr a)))
                                       (vector-ref s 2))))
                     (gvec->list lt))))
          (let* ((externals (g-ref g 'external-tokens))
                 (external-symbols
                  (list->vector
                   (ts-map-index (lambda (i e)
                                   (or (ahash-ref ids (external i))
                                       (and (vector-ref e 2) (ahash-ref ids (vector-ref e 2)))
                                       0))
                                 externals)))
                 (external-states
                  (list->vector
                   (ts-map (lambda (set)
                             (let ((v (make-vector (length externals) #f)))
                               (for-each (lambda (s) (vector-set! v (sym-index s) #t)) set)
                               v))
                           (vector->list (ahash-ref t 'external-lex-states))))))
            (vector (g-ref g 'name) n-symbols token-count n-states
                    (gvec->vector names) (gvec->vector metadata) table lex-modes
                    (convert-lex-table (ahash-ref t 'lex-table))
                    (convert-lex-table (ahash-ref t 'keyword-lex-table))
                    (if (and word (ahash-ref ids word)) (ahash-ref ids word) 0)
                    alias-sequences field-maps external-symbols external-states
                    (list->vector field-names)
                    (ts-map (lambda (s) (ahash-ref ids s))
                            (ts-filter (lambda (s) (ahash-ref ids s)) supertypes))
                    #f
                    (gvec-length unique-aliases))))))))

(define-public (ts-generate-language json-string)
  ;; the language of the text of a grammar.json
  (let ((g (parse-grammar-json (string->json json-string))))
    (prepare-syntax-grammar! g)
    (expand-tokens! g)
    (tables->language g (build-tables! g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (language->data l)
  ;; the language as a list which write and read keep
  (let ((table '()))
    (for-each (lambda (kv) (set! table (cons (list (car kv) (cdr kv)) table)))
              (ahash-table->list (lang-table l)))
    (let ((v (vector-copy** l)))
      (vector-set! v 6 (ts-sort table (lambda (a b) (< (car a) (car b)))))
      (vector-set! v 17 #f)
      (list 'tree-sitter-language 1 v))))

(define-public (data->language d)
  (let* ((v (vector-copy** (caddr d)))
         (table (make-ahash-table)))
    (for-each (lambda (e) (ahash-set! table (car e) (cadr e))) (vector-ref v 6))
    (vector-set! v 6 table)
    v))

(define-public (ts-write-data x port)
  ;; write without the abbreviations of the printers (print-length of s7)
  (cond ((pair? x)
         (display "(" port)
         (let loop ((l x) (first #t))
           (cond ((null? l) #t)
                 ((pair? l)
                  (unless first (display " " port))
                  (ts-write-data (car l) port)
                  (loop (cdr l) #f))
                 (else (display " . " port) (ts-write-data l port))))
         (display ")" port))
        ((vector? x)
         (display "#(" port)
         (do ((i 0 (+ i 1))) ((= i (vector-length x)))
           (unless (= i 0) (display " " port))
           (ts-write-data (vector-ref x i) port))
         (display ")" port))
        ((null? x) (display "()" port))
        (else (write x port))))

(define-public (ts-save-language l file)
  (call-with-output-file file
    (lambda (port) (ts-write-data (language->data l) port))))

(define-public (ts-load-language file)
  (data->language (call-with-input-file file read)))

(define (vector-copy** v)
  (let* ((n (vector-length v)) (r (make-vector n #f)))
    (do ((i 0 (+ i 1))) ((= i n) r) (vector-set! r i (vector-ref v i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-entry (vector (vector) #f))

(define-public (lang-entry l state symbol)
  ;; the table entry #(actions reusable?) of a token
  (if (< symbol 0) empty-entry
      (or (ahash-ref (lang-table l) (+ (* state (lang-symbol-count l)) symbol))
          empty-entry)))

(define-public (lang-has-actions? l state symbol)
  (and (>= symbol 0)
       (ahash-ref (lang-table l) (+ (* state (lang-symbol-count l)) symbol))
       #t))

(define-public (lang-has-reduce-action? l state symbol)
  (let ((actions (vector-ref (lang-entry l state symbol) 0)))
    (and (> (vector-length actions) 0)
         (eq? (vector-ref (vector-ref actions 0) 0) 'reduce))))

(define-public (lang-next-state l state symbol)
  (cond ((or (< symbol 0) (>= symbol (lang-symbol-count l)) (>= state (lang-state-count l))) 0)
        ((< symbol (lang-token-count l))
         (let ((actions (vector-ref (lang-entry l state symbol) 0)))
           (if (= (vector-length actions) 0) 0
               (let ((a (vector-ref actions (- (vector-length actions) 1))))
                 (if (eq? (vector-ref a 0) 'shift)
                     (if (vector-ref a 2) state (vector-ref a 1))
                     0)))))
        (else (or (ahash-ref (lang-table l) (+ (* state (lang-symbol-count l)) symbol)) 0))))

(define-public (lang-lex-mode l state) (vector-ref (lang-lex-modes l) state))

(define-public (lang-reserved-word? l state symbol)
  (and (memv symbol (vector-ref (lang-lex-mode l state) 2)) #t))

(define-public (lang-symbol-name l symbol)
  (cond ((= symbol ts-sym-error) "ERROR")
        ((= symbol ts-sym-error-repeat) "_ERROR")
        (else (vector-ref (lang-names l) symbol))))

(define-public (lang-visible? l symbol)
  (cond ((= symbol ts-sym-error) #t)
        ((< symbol 0) #f)
        (else (odd? (vector-ref (lang-metadata l) symbol)))))

(define-public (lang-named? l symbol)
  (cond ((= symbol ts-sym-error) #t)
        ((< symbol 0) #f)
        (else (>= (remainder (vector-ref (lang-metadata l) symbol) 4) 2))))

(define-public (lang-supertype? l symbol)
  (and (>= symbol 0) (>= (vector-ref (lang-metadata l) symbol) 4)))

(define-public (lang-alias-sequence l production-id)
  (vector-ref (lang-alias-sequences l) production-id))

(define-public (lang-alias-at l production-id index)
  (let ((seq (lang-alias-sequence l production-id)))
    (if (< index (vector-length seq)) (vector-ref seq index) 0)))

(define-public (lang-symbol-for-name l name named?)
  ;; the first symbol with this name, or #f
  (let ((names (lang-names l)))
    (let loop ((i 0))
      (cond ((= i (vector-length names)) #f)
            ((and (equal? (vector-ref names i) name)
                  (eq? (lang-named? l i) named?)
                  (lang-visible? l i))
             i)
            (else (loop (+ i 1)))))))

(define-public (lang-lookaheads l state)
  ;; the tokens which have actions in the state, in order
  (let ((n (lang-token-count l)))
    (let loop ((s (- n 1)) (acc '()))
      (if (< s 0) acc
          (loop (- s 1) (if (lang-has-actions? l state s) (cons s acc) acc))))))
