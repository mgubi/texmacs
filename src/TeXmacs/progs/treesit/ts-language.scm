
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
  (:use (treesit ts-base) (treesit ts-grammar) (treesit ts-nfa) (treesit ts-tables)
        (treesit ts-build)))

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
;; for the queries of the C runtime (render.rs: ts_non_terminal_alias_map,
;; ts_primary_state_ids, the supertype map, ts_symbol_map)
(define-public (lang-alias-map l) (vector-ref l 19))       ; list of (symbol public alias ...)
(define-public (lang-primary-states l) (vector-ref l 20))  ; vector
(define-public (lang-supertype-map l) (vector-ref l 21))   ; list of (supertype subtype ...)
(define-public (lang-public-symbols l) (vector-ref l 22))  ; vector

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

(define identifier-names
  ;; render.rs: the names of the characters in the C identifiers
  '((126 . "TILDE") (96 . "BQUOTE") (33 . "BANG") (64 . "AT") (35 . "POUND")
    (36 . "DOLLAR") (37 . "PERCENT") (94 . "CARET") (38 . "AMP") (42 . "STAR")
    (40 . "LPAREN") (41 . "RPAREN") (45 . "DASH") (43 . "PLUS") (61 . "EQ")
    (123 . "LBRACE") (125 . "RBRACE") (91 . "LBRACK") (93 . "RBRACK")
    (92 . "BSLASH") (124 . "PIPE") (58 . "COLON") (59 . "SEMI") (34 . "DQUOTE")
    (39 . "SQUOTE") (60 . "LT") (62 . "GT") (44 . "COMMA") (46 . "DOT")
    (63 . "QMARK") (47 . "SLASH") (10 . "LF") (13 . "CR") (9 . "TAB") (0 . "NULL")
    (1 . "SOH") (2 . "STX") (3 . "ETX") (4 . "EOT") (5 . "ENQ") (6 . "ACK")
    (7 . "BEL") (8 . "BS") (11 . "VTAB") (12 . "FF") (14 . "SO") (15 . "SI")
    (16 . "DLE") (17 . "DC1") (18 . "DC2") (19 . "DC3") (20 . "DC4") (21 . "NAK")
    (22 . "SYN") (23 . "ETB") (24 . "CAN") (25 . "EM") (26 . "SUB") (27 . "ESC")
    (28 . "FS") (29 . "GS") (30 . "RS") (31 . "US") (127 . "DEL") (#xfeff . "BOM")))

(define (hex-digits n width)
  (let ((h (number->string n 16)))
    (string-append (make-string (max 0 (- width (string-length h))) #\0) h)))

(define (separator acc)
  ;; _ before the name of a character, unless acc is empty or ends with _
  (let ((n (string-length acc)))
    (if (or (= n 0) (char=? (string-ref acc (- n 1)) #\_)) "" "_")))

(define (sanitize-identifier name)
  ;; render.rs (sanitize_identifier)
  (let loop ((l (utf8->codes name)) (acc ""))
    (if (null? l) acc
        (let ((c (car l)))
          (cond ((or (and (>= c 48) (<= c 57)) (and (>= c 65) (<= c 90))
                     (and (>= c 97) (<= c 122)) (= c 95))
                 (loop (cdr l) (string-append acc (string (integer->char c)))))
                ((and (= c 32) (= (string-length name) 1))
                 (loop (cdr l) (string-append acc (separator acc)
                                              "SPACE")))
                ((assv c identifier-names)
                 => (lambda (p)
                      (loop (cdr l) (string-append acc (separator acc)
                                                   (cdr p)))))
                ((and (>= c #x80) (<= c #xffff))
                 (loop (cdr l) (string-append acc "u" (hex-digits c 4))))
                ((> c #xffff)
                 (loop (cdr l) (string-append acc "U" (hex-digits c 8))))
                (else (loop (cdr l) acc)))))))

(define (symbol-identifiers g symbols)
  ;; render.rs (assign_symbol_id): the C identifier of each symbol, a number
  ;; telling apart those of the same name
  (let ((used (make-ahash-table)))
    (list->vector
     (ts-map (lambda (s)
               (let ((id (if (= s sym-end) "ts_builtin_sym_end"
                             (let* ((md (symbol-metadata g s))
                                    (k (cdr md))
                                    (base (string-append
                                           (cond ((= k vt-auxiliary) "aux_sym_")
                                                 ((= k vt-anonymous) "anon_sym_")
                                                 (else "sym_"))
                                           (sanitize-identifier (car md)))))
                               (let suffix ((id base) (n 1))
                                 (if (ahash-ref used id)
                                     (suffix (string-append base (number->string (+ n 1))) (+ n 1))
                                     id))))))
                 (ahash-set! used id #t)
                 id))
             symbols))))

(define (string-set-insert x l)
  ;; insert x in the sorted list of strings l, if it is not in it
  (cond ((null? l) (list x))
        ((string=? x (car l)) l)
        ((string<? x (car l)) (cons x l))
        (else (cons (car l) (string-set-insert x (cdr l))))))

;; the part of a language which depends on the symbols only (not on the
;; states); the native export (ts-export.scm) sends it to C++
(define-public (sp-ids sp) (vector-ref sp 0))
(define-public (sp-token-count sp) (vector-ref sp 1))
(define-public (sp-names sp) (vector-ref sp 2))
(define-public (sp-metadata sp) (vector-ref sp 3))
(define-public (sp-alias-sequences sp) (vector-ref sp 4))
(define-public (sp-field-names sp) (vector-ref sp 5))
(define-public (sp-field-maps sp) (vector-ref sp 6))
(define-public (sp-external-symbols sp) (vector-ref sp 7))
(define-public (sp-supertypes sp) (vector-ref sp 8))
(define-public (sp-alias-count sp) (vector-ref sp 9))
(define-public (sp-alias-map sp) (vector-ref sp 10))
(define-public (sp-supertype-map sp) (vector-ref sp 11))
(define-public (sp-public-symbols sp) (vector-ref sp 12))
(define-public (sp-keyword-capture sp) (vector-ref sp 13))

(define-public (ts-language-symbols g symbols infos)
  ;; symbols as the tables order them, infos the production infos; as
  ;; render.rs, the aliases and the supertypes go through the C identifiers
  ;; of the symbols, which decide their order
  (let* ((n-symbols (length symbols))
         (symbol-vector (list->vector symbols))
         (ids (make-ahash-table))
         (default-aliases (g-ref g 'default-aliases))
         (supertypes (g-ref g 'supertypes)))
    (ts-map-index (lambda (i s) (ahash-set! ids s i)) symbols)
    (ahash-set! ids sym-eont 0)
    (let* ((token-count (length (ts-filter (lambda (s) (not (non-terminal? s))) symbols)))
           (public (public-symbol-map g symbols ids default-aliases))
           (identifiers (symbol-identifiers g symbols))
           (names (make-gvec))
           (metadata (make-gvec)))
      (define (symbols-for-alias alias)
        ;; the positions of the symbols which are the node of alias
        (let ((kind (if (cdr alias) vt-named vt-anonymous)))
          (let loop ((i 0) (acc '()))
            (if (= i n-symbols) (reverse acc)
                (let* ((s (vector-ref symbol-vector i))
                       (a (ahash-ref default-aliases s))
                       (ok (if a (equal? a alias)
                               (let ((md (symbol-metadata g s)))
                                 (and (equal? (car md) (car alias)) (= (cdr md) kind))))))
                  (loop (+ i 1) (if ok (cons i acc) acc)))))))
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
      ;; the aliases of the productions: the public symbol of the first symbol
      ;; which is their node, else an alias of its own, sorted by name
      (let* ((alias-table (make-ahash-table))   ; alias -> (id . identifier)
             (unique
              (let loop ((l (apply append (ts-map car (vector->list infos)))) (acc '()))
                (cond ((null? l) acc)
                      ((or (not (car l)) (ahash-ref alias-table (car l)) (member (car l) acc))
                       (loop (cdr l) acc))
                      ((pair? (symbols-for-alias (car l)))
                       (let ((p (vector-ref public (car (symbols-for-alias (car l))))))
                         (ahash-set! alias-table (car l) (cons p (vector-ref identifiers p)))
                         (loop (cdr l) acc)))
                      (else (loop (cdr l) (cons (car l) acc))))))
             (unique (ts-sort unique
                              (lambda (a b)
                                (or (string<? (car a) (car b))
                                    (and (string=? (car a) (car b)) (not (cdr a)) (cdr b))))))
             (by-identifier (make-ahash-table)))
        (ts-map-index (lambda (k a)
                        (ahash-set! alias-table a
                                    (cons (+ n-symbols k)
                                          (string-append (if (cdr a) "alias_sym_" "anon_alias_sym_")
                                                         (sanitize-identifier (car a))))))
                      unique)
        ;; (not a do loop: under S7, the counter of a do loop whose body
        ;; calls ahash-set! is changed in place, and all the values stored
        ;; would be its last one)
        (let loop ((i 0))
          (when (< i n-symbols)
            (ahash-set! by-identifier (vector-ref identifiers i) i)
            (loop (+ i 1))))
        (for-each (lambda (a) (let ((e (ahash-ref alias-table a)))
                                (ahash-set! by-identifier (cdr e) (car e))))
                  unique)
        (for-each (lambda (a)
                    (gvec-push! names (car a))
                    (gvec-push! metadata (if (cdr a) 3 1)))
                  unique)
        (let* ((alias-sequences
                (list->vector
                 (ts-map (lambda (info)
                           (list->vector (ts-map (lambda (a) (if a (car (ahash-ref alias-table a)) 0))
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
                         (vector->list infos))))
               (word (g-ref g 'word))
               (externals (g-ref g 'external-tokens))
               (external-symbols
                (list->vector
                 (ts-map-index (lambda (i e)
                                 (or (ahash-ref ids (external i))
                                     (and (vector-ref e 2) (ahash-ref ids (vector-ref e 2)))
                                     0))
                               externals)))
               (supertype-ids (ts-map (lambda (s) (ahash-ref ids s))
                                      (ts-filter (lambda (s) (ahash-ref ids s)) supertypes))))
          (vector ids token-count (gvec->vector names) (gvec->vector metadata)
                  alias-sequences (list->vector field-names) field-maps
                  external-symbols supertype-ids (length unique)
                  (query-alias-map g ids public identifiers alias-table by-identifier
                                   default-aliases)
                  (query-supertype-map g ids identifiers alias-table by-identifier
                                       symbols-for-alias)
                  public
                  (if (and word (ahash-ref ids word)) (ahash-ref ids word) 0)))))))

(define-public (tables->language g t)
  (let* ((symbols (ahash-ref t 'symbols))
         (n-symbols (length symbols))
         (sp (ts-language-symbols g symbols (ahash-ref t 'production-infos)))
         (ids (sp-ids sp)))
    (let* ((states (ahash-ref t 'states))
           (n-states (gvec-length states))
           (table (make-ahash-table))
           (lex-modes (make-vector n-states #f)))
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
      (ts-profile "parse table")
      (define (convert-lex-table lt)
        (list->vector
         (ts-map (lambda (s)
                   (vector (let ((a (vector-ref s 0))) (if a (ahash-ref ids a) #f))
                           (let ((e (vector-ref s 1))) (if e (car e) #f))
                           (ts-map (lambda (a) (vector (car a) (cadr a) (cddr a)))
                                   (vector-ref s 2))))
                 (gvec->list lt))))
      (let* ((externals (g-ref g 'external-tokens))
             (external-states
              (list->vector
               (ts-map (lambda (set)
                         (let ((v (make-vector (length externals) #f)))
                           (for-each (lambda (s) (vector-set! v (sym-index s) #t)) set)
                           v))
                       (vector->list (ahash-ref t 'external-lex-states))))))
        (vector (g-ref g 'name) n-symbols (sp-token-count sp) n-states
                (sp-names sp) (sp-metadata sp) table lex-modes
                (convert-lex-table (ahash-ref t 'lex-table))
                (convert-lex-table (ahash-ref t 'keyword-lex-table))
                (sp-keyword-capture sp)
                (sp-alias-sequences sp) (sp-field-maps sp) (sp-external-symbols sp)
                external-states (sp-field-names sp) (sp-supertypes sp)
                #f
                (sp-alias-count sp)
                (sp-alias-map sp)
                (primary-states states)
                (sp-supertype-map sp)
                (sp-public-symbols sp))))))

(define (query-alias-map g ids public identifiers alias-table by-identifier default-aliases)
  ;; render.rs: the aliases (other than the default one) under which each
  ;; non-terminal appears in the productions, in the order of their C
  ;; identifiers, after the public symbol of the non-terminal
  (let ((table (make-ahash-table)))
    (vector-for-each***
     (lambda (p)
       (do ((i 0 (+ i 1))) ((= i (prod-length p)))
         (let* ((step (prod-step p i)) (s (step-symbol step)) (a (step-alias step)))
           (when (and a (non-terminal? s) (ahash-ref ids s)
                      (not (equal? a (ahash-ref default-aliases s)))
                      (ahash-ref alias-table a))
             (ahash-set! table s (string-set-insert (cdr (ahash-ref alias-table a))
                                                    (or (ahash-ref table s) '())))))))
     (g-ref g 'productions))
    (ts-map (lambda (kv)
              (let ((id (ahash-ref ids (car kv))))
                (cons id (cons (vector-ref public id)
                               (ts-map (lambda (x) (ahash-ref by-identifier x)) (cdr kv))))))
            (ts-sort (ahash-table->list table) (lambda (a b) (< (car a) (car b)))))))

(define (vector-for-each*** fun v)
  (do ((i 0 (+ i 1))) ((= i (vector-length v))) (fun (vector-ref v i))))

(define (primary-states states)
  ;; the first state of the same core as each state
  (let ((first (make-ahash-table))
        (v (make-vector (gvec-length states) 0)))
    (do ((i 0 (+ i 1))) ((= i (gvec-length states)) v)
      (let ((c (state-core (gvec-ref states i))))
        (unless (ahash-ref first c) (ahash-set! first c (+ i 1)))
        (vector-set! v i (- (ahash-ref first c) 1))))))

(define (query-supertype-map g ids identifiers alias-table by-identifier symbols-for-alias)
  ;; render.rs: each supertype with its visible children, the supertypes and
  ;; the children in the order of their C identifiers; an aliased child is
  ;; the alias of the productions, else the symbols which are its node
  (let* ((entries
          (ts-filter
           (lambda (x) x)
           (ts-map
            (lambda (s)
              (and (non-terminal? s) (ahash-ref ids s)
                   (let ((children '()))
                     (for-each
                      (lambda (t)
                        (for-each (lambda (x) (set! children (string-set-insert x children)))
                                  (cond ((not (pair? t))
                                         (if (ahash-ref ids t)
                                             (list (vector-ref identifiers (ahash-ref ids t)))
                                             '()))
                                        ((ahash-ref alias-table t) => (lambda (e) (list (cdr e))))
                                        (else (ts-map (lambda (p) (vector-ref identifiers p))
                                                      (symbols-for-alias t))))))
                      (vector-ref (g-ref g 'variable-children) (sym-index s)))
                     (list (vector-ref identifiers (ahash-ref ids s)) (ahash-ref ids s) children))))
            (g-ref g 'supertypes)))))
    (ts-map (lambda (e) (cons (cadr e) (ts-map (lambda (x) (ahash-ref by-identifier x)) (caddr e))))
            (ts-sort entries (lambda (a b) (string<? (car a) (car b)))))))

(define (public-symbol-map g symbols ids default-aliases)
  ;; render.rs: the symbols which are the same node to the public API (the
  ;; same alias, or tokens with the same name and kind) share a symbol
  (let ((mapping (make-ahash-table)))
    (for-each
     (lambda (s)
       (let ((a (ahash-ref default-aliases s))
             (m s))
         (cond (a
                (let ((kind (if (cdr a) vt-named vt-anonymous)))
                  (let loop ((l symbols))
                    (when (pair? l)
                      (let ((o (car l)) (oa (ahash-ref default-aliases (car l))))
                        (cond (oa (when (and (< o m) (equal? oa a)) (set! m o)) (loop (cdr l)))
                              ((equal? (symbol-metadata g o) (cons (car a) kind)) (set! m o))
                              (else (loop (cdr l)))))))))
               ((terminal? s)
                (let ((md (symbol-metadata g s)))
                  (let loop ((l symbols))
                    (when (pair? l)
                      (if (equal? (symbol-metadata g (car l)) md)
                          (unless (eqv? (ahash-ref mapping (car l)) s) (set! m (car l)))
                          (loop (cdr l))))))))
         (ahash-set! mapping s m)))
     symbols)
    (list->vector (ts-map (lambda (s) (ahash-ref ids (ahash-ref mapping s))) symbols))))

(define-public (ts-read-grammar json-string)
  ;; the grammar of the text of a grammar.json, ready for the tables
  (ts-profile "start")
  (let ((g (parse-grammar-json (string->json json-string))))
    (prepare-syntax-grammar! g)
    (expand-tokens! g)
    g))

(define-public (ts-generate-language json-string)
  ;; the language of the text of a grammar.json
  (let* ((g (ts-read-grammar json-string))
         (l (tables->language g (build-tables! g))))
    (ts-profile "language")
    l))

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
