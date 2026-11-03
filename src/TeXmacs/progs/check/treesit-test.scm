
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : treesit-test.scm
;; DESCRIPTION : tests of tree-sitter in Scheme
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The parser generator and the parser of progs/treesit:
;;
;;   - the helpers: JSON, character sets, regular expressions;
;;   - the tables of the JSON grammar of misc/tree-sitter, whose counts
;;     are those of tree-sitter generate 0.27.1 (32 states, 25 symbols,
;;     15 tokens, 44 lex states, 2 production infos);
;;   - the trees of JSON texts, with errors, the corpus tests of
;;     tree-sitter-json, and reparsing after edits;
;;   - a small grammar written here, with precedences and fields;
;;   - the same languages run by the C runtime of tree-sitter
;;     (Plugins/TreeSitter), when TeXmacs is built with it.

(texmacs-module (check treesit-test)
  (:use (check check-lib)
        (treesit ts-base) (treesit ts-grammar) (treesit ts-nfa)
        (treesit ts-tables) (treesit ts-build) (treesit ts-language)
        (treesit ts-parse) (treesit ts-export) (treesit ts-registry)
        (treesit ts-highlight) (treesit ts-scanner-cpp) (treesit ts-native)
        (treesit ts-scan) (treesit ts-edit)))

(define (json-grammar-file)
  (url->system (url-concretize "$TEXMACS_PATH/misc/tree-sitter/json/grammar.json")))

(define json-language #f)

(define (json-lang)
  (unless json-language
    (set! json-language (ts-generate-language (ts-read-file (json-grammar-file)))))
  json-language)

(define (parse s) (ts-tree->string (ts-parse-string (json-lang) s)))

(define (bytes . l) (list->string (map integer->char l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-helpers)
  (check= (string->json "{\"a\": [1, true, null, \"x\\ny\"]}")
          `(obj ("a" . (arr 1 #t null ,(string-append "x" (string #\newline) "y")))))
  (check= (utf8->codes (bytes 97 195 169)) '(97 233))
  (check= (codes->utf8 '(97 233 8364)) (bytes 97 195 169 226 130 172))
  (check= (cs-add-range (list (cons 1 3) (cons 6 8)) 3 6) (list (cons 1 8)))
  (check= (cs-negate (list (cons 10 11))) (list (cons 0 10) (cons 11 cs-end)))
  (check= (cs-intersect (list (cons 0 10)) (list (cons 5 20))) (list (cons 5 10)))
  ;; the simplifications of regex_syntax
  (check= (parse-pattern "a|b|c" #f) (list 'class (list (cons 97 100))))
  (check= (parse-pattern "ab|ac" #f) (list 'alt (list 'lit 97 98) (list 'lit 97 99)))
  (check= (parse-pattern "[^*]*\\*+" #f)
          (list 'concat (list 'rep 0 #f (list 'class (list (cons 0 42) (cons 43 cs-end))))
                (list 'rep 1 #f (list 'lit 42))))
  (check= (parse-pattern "x{2}" #f) (list 'rep 2 2 (list 'lit 120)))
  (check= (parse-pattern "(?i)k" #f) (list 'class (list (cons 75 76) (cons 107 108))))
  ;; the operations on the items of a class (tree-sitter-julia uses &&)
  (check= (parse-pattern "[a-z&&[^aeiou]]" #f)
          (list 'class (list (cons 98 101) (cons 102 105) (cons 106 111) (cons 112 117)
                             (cons 118 123))))
  (check= (parse-pattern "[a-f--c]" #f) (list 'class (list (cons 97 99) (cons 100 103))))
  (check= (parse-pattern "[a-c~~b-d]" #f) (list 'class (list (cons 97 98) (cons 100 101))))
  (check= (parse-pattern "[^a-z&&b]" #f) (list 'class (list (cons 0 98) (cons 99 cs-end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The tables of the JSON grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-tables)
  (let* ((g (parse-grammar-json (string->json (ts-read-file (json-grammar-file))))))
    (prepare-syntax-grammar! g)
    (expand-tokens! g)
    (let ((t (build-tables! g)))
      (check= (gvec-length (ahash-ref t 'states)) 32)
      (check= (length (ahash-ref t 'symbols)) 25)
      (check= (+ 1 (vector-length (g-ref g 'lexical))) 15)
      (check= (gvec-length (ahash-ref t 'lex-table)) 44)
      (check= (vector-length (ahash-ref t 'production-infos)) 2)))
  (let ((l (json-lang)))
    (check= (lang-state-count l) 32)
    (check= (lang-symbol-count l) 25)
    (check= (lang-token-count l) 15)
    (check= (vector-length (lang-field-names l)) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-parse)
  (check= (parse "{\"a\": 1}")
          "(document (object (pair key: (string (string_content)) value: (number))))")
  (check= (parse "[1, \"s\\n\", true, false, null]")
          (string-append "(document (array (number) (string (string_content) "
                         "(escape_sequence)) (true) (false) (null)))"))
  (check= (parse "// c\n{\"a\": /* x */ 1.5e3}")
          (string-append "(document (comment) (object (pair key: (string "
                         "(string_content)) (comment) value: (number))))"))
  (check= (parse "") "(document)")
  ;; errors
  (check= (parse "[1, 2") "(document (array (number) (number) (MISSING \"]\")))")
  (check= (parse "{\"a\" 1}") "(document (object (ERROR (string (string_content)) (number))))")
  (check= (parse "[1 2 3]") "(document (array (ERROR (number) (number)) (number)))")
  ;; the nodes
  (let* ((tree (ts-parse-string (json-lang) "[10, 200]"))
         (root (tree-root-node tree))
         (array (car (node-children (json-lang) root)))
         (children (node-children (json-lang) array)))
    (check= (node-type (json-lang) array) "array")
    (check= (map (lambda (n) (node-type (json-lang) n)) children) '("[" "number" "," "number" "]"))
    (check= (map (lambda (n) (list (node-start-offset n) (node-end-offset n))) children)
            '((0 1) (1 3) (3 4) (5 8) (8 9)))))

(define corpus
  ;; some of test/corpus/main.txt of tree-sitter-json (Copyright (c) 2014
  ;; Max Brunsfeld, MIT license: see misc/tree-sitter/json/LICENSE)
  '(("\n[\n  345,\n  10.1,\n  10,\n  -10,\n  null,\n  true,\n  false,\n  { \"stuff\": \"good\" }\n]\n"
     "(document (array (number) (number) (number) (number) (null) (true) (false) (object (pair (string (string_content)) (string (string_content))))))")
    ("\n[\n  \"\",\n  \"abc\",\n  \"def\\n\",\n  \"ghi\\t\",\n  \"jkl\\f\"\n]\n"
     "(document (array (string) (string (string_content)) (string (string_content) (escape_sequence)) (string (string_content) (escape_sequence)) (string (string_content) (escape_sequence))))")
    ("\n{\n  \"key1\": \"value1\",\n  \"key2\": 1\n}\n"
     "(document (object (pair (string (string_content)) (string (string_content))) (pair (string (string_content)) (number))))")
    ("\n[\n  \"\\\"\",\n  \"\\\\\",\n  \"\\/\",\n  \"\\b\\f\\n\\r\\t\",\n  \"\\u00e9\"\n]\n"
     "(document (array (string (escape_sequence)) (string (escape_sequence)) (string (escape_sequence)) (string (escape_sequence) (escape_sequence) (escape_sequence) (escape_sequence) (escape_sequence)) (string (escape_sequence) (string_content))))")))

(define (strip-fields s)
  ;; without the field names, as in the corpus tests
  (let loop ((cs (string->list s)) (acc '()))
    (cond ((null? cs) (list->string (reverse acc)))
          ((and (char=? (car cs) #\space)
                (let field ((l (cdr cs)))
                  (and (pair? l)
                       (or (and (char=? (car l) #\:) (pair? (cdr l)) (char=? (cadr l) #\space)
                                (cddr l))
                           (and (or (char-alphabetic? (car l)) (char=? (car l) #\_))
                                (field (cdr l)))))))
           => (lambda (rest) (loop rest (cons #\space acc))))
          (else (loop (cdr cs) (cons (car cs) acc))))))

(define (test-corpus)
  (for (c corpus)
    (check= (strip-fields (parse (car c))) (cadr c))))

(define (splice v start end ins)
  (list->vector (append (vector->list (subvector* v 0 start)) ins
                        (vector->list (subvector* v end (vector-length v))))))

(define (subvector* v start end)
  (let ((r (make-vector (- end start) 0)))
    (do ((i start (+ i 1))) ((= i end) r)
      (vector-set! r (- i start) (vector-ref v i)))))

(define (test-edits)
  ;; reparsing after an edit gives the tree of a full parse
  (let* ((l (json-lang))
         (text (list->vector (utf8->codes "{\"a\": [1, 2, 3], \"b\": {\"c\": null}}")))
         (tree (ts-parse-codes l text)))
    (for (e (list (list 8 9 '(52 50))        ; 2 -> 42
                  (list 7 7 '(48 44 32))     ; insert "0, "
                  (list 15 15 '(93))         ; an extra ]
                  (list 2 2 '(120))          ; a -> xa
                  (list 0 1 '())))           ; remove {
      (let* ((new (splice text (car e) (cadr e) (caddr e)))
             (edited (ts-tree-edit tree new (car e) (cadr e) (+ (car e) (length (caddr e)))))
             (incremental (ts-parse-codes l new edited))
             (full (ts-parse-codes l new)))
        (check= (ts-tree->string incremental) (ts-tree->string full))
        (set! text new)
        (set! tree incremental)))))

(define (test-save)
  ;; the language written and read back parses the same way
  (let ((file (url->system (url-concretize "$TEXMACS_HOME_PATH/system/tmp/json.tslang"))))
    (ts-save-language (json-lang) file)
    (let ((l (ts-load-language file)))
      (check= (lang-state-count l) 32)
      (check= (ts-tree->string (ts-parse-string l "[1, {\"a\": 2}]"))
              (parse "[1, {\"a\": 2}]")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A grammar written here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define calc-grammar
  ;; expressions with precedences, associativity, fields, keywords, an
  ;; alias and a hidden rule
  (string-append
   "{\"name\": \"calc\", \"word\": \"identifier\","
   " \"extras\": [{\"type\": \"PATTERN\", \"value\": \"\\\\s\"}],"
   " \"rules\": {"
   "  \"program\": {\"type\": \"REPEAT\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"_statement\"}},"
   "  \"_statement\": {\"type\": \"CHOICE\", \"members\": ["
   "    {\"type\": \"SYMBOL\", \"name\": \"let\"},"
   "    {\"type\": \"SEQ\", \"members\": [{\"type\": \"SYMBOL\", \"name\": \"_expression\"},"
   "                                    {\"type\": \"STRING\", \"value\": \";\"}]}]},"
   "  \"let\": {\"type\": \"SEQ\", \"members\": ["
   "    {\"type\": \"STRING\", \"value\": \"let\"},"
   "    {\"type\": \"FIELD\", \"name\": \"name\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"identifier\"}},"
   "    {\"type\": \"STRING\", \"value\": \"=\"},"
   "    {\"type\": \"FIELD\", \"name\": \"value\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"_expression\"}},"
   "    {\"type\": \"STRING\", \"value\": \";\"}]},"
   "  \"_expression\": {\"type\": \"CHOICE\", \"members\": ["
   "    {\"type\": \"SYMBOL\", \"name\": \"binary\"},"
   "    {\"type\": \"SYMBOL\", \"name\": \"number\"},"
   "    {\"type\": \"ALIAS\", \"named\": true, \"value\": \"variable\","
   "     \"content\": {\"type\": \"SYMBOL\", \"name\": \"identifier\"}},"
   "    {\"type\": \"SEQ\", \"members\": [{\"type\": \"STRING\", \"value\": \"(\"},"
   "      {\"type\": \"SYMBOL\", \"name\": \"_expression\"}, {\"type\": \"STRING\", \"value\": \")\"}]}]},"
   "  \"binary\": {\"type\": \"CHOICE\", \"members\": ["
   "    {\"type\": \"PREC_LEFT\", \"value\": 1, \"content\": {\"type\": \"SEQ\", \"members\": ["
   "      {\"type\": \"FIELD\", \"name\": \"left\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"_expression\"}},"
   "      {\"type\": \"FIELD\", \"name\": \"op\", \"content\": {\"type\": \"CHOICE\", \"members\": ["
   "        {\"type\": \"STRING\", \"value\": \"+\"}, {\"type\": \"STRING\", \"value\": \"-\"}]}},"
   "      {\"type\": \"FIELD\", \"name\": \"right\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"_expression\"}}]}},"
   "    {\"type\": \"PREC_LEFT\", \"value\": 2, \"content\": {\"type\": \"SEQ\", \"members\": ["
   "      {\"type\": \"FIELD\", \"name\": \"left\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"_expression\"}},"
   "      {\"type\": \"FIELD\", \"name\": \"op\", \"content\": {\"type\": \"STRING\", \"value\": \"*\"}},"
   "      {\"type\": \"FIELD\", \"name\": \"right\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"_expression\"}}]}},"
   "    {\"type\": \"PREC_RIGHT\", \"value\": 3, \"content\": {\"type\": \"SEQ\", \"members\": ["
   "      {\"type\": \"FIELD\", \"name\": \"left\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"_expression\"}},"
   "      {\"type\": \"FIELD\", \"name\": \"op\", \"content\": {\"type\": \"STRING\", \"value\": \"^\"}},"
   "      {\"type\": \"FIELD\", \"name\": \"right\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"_expression\"}}]}}]},"
   "  \"number\": {\"type\": \"PATTERN\", \"value\": \"\\\\d+\"},"
   "  \"identifier\": {\"type\": \"PATTERN\", \"value\": \"[a-z]+\"}}}"))

(define (test-calc)
  (let ((l (ts-generate-language calc-grammar)))
    (define (p s) (ts-tree->string (ts-parse-string l s)))
    (check= (p "1 + 2 * 3;")
            (string-append "(program (binary left: (number) right: (binary left: (number) "
                           "right: (number))))"))
    (check= (p "1 - 2 - 3;")
            (string-append "(program (binary left: (binary left: (number) right: (number)) "
                           "right: (number)))"))
    (check= (p "2 ^ 3 ^ 4;")
            (string-append "(program (binary left: (number) right: (binary left: (number) "
                           "right: (number))))"))
    (check= (p "let x = (y + 1) * 2;")
            (string-append "(program (let name: (identifier) value: (binary left: (binary "
                           "left: (variable) right: (number)) right: (number))))"))
    ;; let is a keyword, lets an identifier
    (check= (p "lets;") "(program (variable))")
    (check= (p "1 +;") "(program (binary left: (number) right: (MISSING variable)))")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The C runtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-c-runtime)
  ;; the C runtime gives the trees of the Scheme parser
  (when (defined? 'tree-sitter-language-new)
    (let ((c (ts-c-language (json-lang)))
          (calc (ts-generate-language calc-grammar)))
      (check-true (integer? c))
      (for (s (append (map car corpus)
                      (list "{\"a\": 1}" "[1, 2" "{\"a\" 1}" "[1 2 3]" "")))
        (check= (tree-sitter-parse-sexp c s) (parse s)))
      (let ((cc (ts-c-language calc)))
        (for (s (list "1 + 2 * 3;" "2 ^ 3 ^ 4;" "let x = (y + 1) * 2;" "lets;" "1 +;"))
          (check= (tree-sitter-parse-sexp cc s)
                  (ts-tree->string (ts-parse-string calc s)))))
      (check= (tree-sitter-language-new "nonsense") -1)
      (test-c-queries c (ts-c-language calc)))))

(define (captures q text)
  ;; the captures as (start end name)
  (let ((names (tree-sitter-query-capture-names q)))
    (let loop ((l (tree-sitter-query-captures q text)) (acc '()))
      (if (null? l) (reverse acc)
          (loop (cdddr l) (cons (list (car l) (cadr l) (list-ref names (caddr l))) acc))))))

(define (test-c-queries c cc)
  ;; the highlighting query of tree-sitter-json, and the predicates
  (let ((q (tree-sitter-query-new
            c (ts-read-file (url->system (url-concretize
                                          "$TEXMACS_PATH/misc/tree-sitter/json/highlights.scm"))))))
    (check-true (>= q 0))
    (check= (captures q "{\"a\": [1, true, null]}")
            '((1 4 "string.special.key") (1 4 "string") (7 8 "number")
              (10 14 "constant.builtin") (16 20 "constant.builtin"))))
  (check= (tree-sitter-query-new c "(nonsense) @x") -1)
  (let ((q (tree-sitter-query-new
            cc (string-append "((variable) @short (#match? @short \"^[a-z]$\"))"
                              "((variable) @long (#not-match? @long \"^[a-z]$\"))"
                              "((variable) @x (#eq? @x \"x\"))"
                              "((variable) @ab (#any-of? @ab \"a\" \"b\"))"))))
    (check= (captures q "a + xy * x;")
            '((0 1 "short") (0 1 "ab") (4 6 "long") (9 10 "short") (9 10 "x")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-registry)
  ;; the grammars shipped in misc/tree-sitter, and the cache of the tables
  ;; (made here without the background process of treesit-prepare)
  (check-false (treesit-supported? "fortran"))
  (for (lan treesit-languages)
    (check-true (url-exists? (treesit-grammar-url lan))))
  ;; each highlighted language has its grammar and its query
  (for (p treesit-highlight-languages)
    (check-true (in? (cdr p) treesit-languages))
    (check-true (url-exists? (url-concretize
                              (string-append "$TEXMACS_PATH/misc/tree-sitter/"
                                             (cdr p) "/highlights.scm")))))
  (when (defined? 'tree-sitter-language-load)
    (check-true (treesit-supported? "json"))
    (let ((file (treesit-cache-url "json")))
      (check= (url-suffix file) "tsdyn")
      (treesit-generate-cache "json")
      (check-true (url-exists? file))
      (let ((id (treesit-language "json")))
        (check-true (integer? id))
        (check= (tree-sitter-parse-sexp id "[1, 2")
                "(document (array (number) (number) (MISSING \"]\")))")
        (check= (treesit-language "json") id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code regions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (split-lines s)
  ;; the lines without their ends, also "\r\n" (checkouts on Windows)
  (define (line start i)
    (if (and (> i start) (char=? (string-ref s (- i 1)) #\return))
        (substring s start (- i 1))
        (substring s start i)))
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length s)) (reverse (cons (line start i) acc)))
          ((char=? (string-ref s i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (line start i) acc)))
          (else (loop (+ i 1) start acc)))))

(define (join-lines l)
  (if (null? l) ""
      (apply string-append (car l) (map (lambda (x) (string-append "\n" x)) (cdr l)))))

(define (region-text t)
  ;; the text of a region, in UTF-8
  (join-lines (map (lambda (l) (cork->utf8 (tree->string l))) (tree-children t))))

(define (letter? c) (or (char-alphabetic? c) (char=? c #\-)))

(define (safe-line? s)
  ;; a line which stays valid when split or joined
  (not (or (string-index s #\;) (string-index s #\") (string-index s #\#)
           (string-index s #\\) (string-index s #\|) (string-index s #\<))))

(define seed 12345)
(define (random-below n)
  (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
  (modulo (quotient seed 65536) n))

(define (test-region-edits id file edits)
  ;; edits which keep the program valid: letters typed in symbols, lines
  ;; split at a space and joined; after each one the tree of the region,
  ;; brought up to date incrementally, is that of a full parse
  (let* ((t (stree->tree (cons 'document (map utf8->cork (split-lines (string-load file))))))
         (n (tree-arity t)))
    (check= (syntax-region-sexp t id) (tree-sitter-parse-sexp id (region-text t)))
    (do ((k 0 (+ k 1))) ((= k edits))
      (let* ((i (random-below (tree-arity t)))
             (s (tree->string (tree-ref t i)))
             (kind (random-below 3)))
        (cond ((and (= kind 0) (> (string-length s) 1))
               (let ((p (+ 1 (random-below (- (string-length s) 1)))))
                 (when (and (letter? (string-ref s (- p 1))) (letter? (string-ref s p)))
                   (tree-insert! (tree-ref t i) p "x"))))
              ((and (= kind 1) (safe-line? s) (string-index s #\space))
               (let ((p (string-index s #\space)))
                 (tree-split! t i p)))
              ((and (= kind 2) (< (+ i 1) (tree-arity t)) (safe-line? s)
                    (safe-line? (tree->string (tree-ref t (+ i 1)))))
               (tree-insert! (tree-ref t i) (string-length s) " ")
               (tree-join! t i)))
        (check= (syntax-region-sexp t id) (tree-sitter-parse-sexp id (region-text t)))))
    (check= (car (syntax-region-statistics t id)) 1)
    (check-true (> (cadr (syntax-region-statistics t id)) 0))
    (syntax-region-forget t)))

(define (test-regions)
  (when (defined? 'syntax-region-sexp)
    (let ((json (treesit-language "json")))
      ;; entities and Cork: columns are positions in the TeXmacs strings
      (let ((t (stree->tree `(document "[\"a<less>b\", " ,(string-append "\"" (utf8->cork (bytes 195 169)) "t\\u00e9\"]")))))
        (check= (syntax-region-sexp t json)
                (string-append "(document (array (string (string_content)) "
                               "(string (string_content) (escape_sequence) (string_content))))"))
        (check= (car (syntax-region-node-at t json 0 3)) '("string_content" #t (0 2) (0 10) #f))
        (check= (car (syntax-region-node-at t json 1 3)) '("escape_sequence" #t (1 3) (1 5) #f)))
      ;; a single string is a region too
      (check= (syntax-region-sexp (string->tree "[true") json)
              "(document (array (true) (MISSING \"]\")))")
      (check= (syntax-region-errors (stree->tree '(document "[1, 2")) json)
              '(("missing" (0 5) (0 5))))
      ;; edits of a JSON document
      (let ((t (stree->tree '(document "{\"a\": [1, 2]," " \"b\": null}"))))
        (syntax-region-sexp t json)
        (tree-insert! (tree-ref t 0) 11 ", 3")
        (tree-set! t 1 " \"b\": {}}")
        (check= (syntax-region-sexp t json) (tree-sitter-parse-sexp json (region-text t)))
        (check= (syntax-region-statistics t json) '(1 1))))
    (treesit-generate-cache "scheme")
    (let ((scheme (treesit-language "scheme")))
      (check-true (integer? scheme))
      (test-region-edits scheme (url-concretize "$TEXMACS_PATH/progs/treesit/ts-parse.scm") 40)
      ;; <less> is the symbol <
      (let ((t (stree->tree '(document "(if (<less> a b) a b)"))))
        (check= (car (syntax-region-node-at t scheme 0 5)) '("symbol" #t (0 5) (0 11) #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-test-buffer doc thunk)
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (thunk u (buffer-get-body u))
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))))

;; a long region is parsed by parts (syntax_region.cpp): its colours after
;; edits are those of a fresh copy, and the answers have the rows of the
;; whole region
(define (long-c-lines n)
  (append-map (lambda (i)
                (let ((f (string-append "f" (number->string i))))
                  (list (string-append "int " f " (int x) {")
                        (string-append "  return x + " (number->string i) ";")
                        "}")))
              (iota n)))

(define (test-long-regions)
  (check-group "long regions")
  (let* ((u (string->url "tmfs://aux/treesit-long"))
         (v (string->url "tmfs://aux/treesit-long-copy"))
         (old (current-buffer))
         (lan (tree-sitter-program-language "c"))
         (colors (lambda (d rows)
                   (map (lambda (i) (tree-sitter-line-colors "c" (tree-ref d i)))
                        rows)))
         (rows '(0 1 2 2999 3000 3001 4500 4501 6000 8997 8998 8999))
         (fresh (lambda (d)
                  (buffer-set-body v (stree->tree (tree->stree d)))
                  (switch-to-buffer v)
                  (let ((r (colors (buffer-get-body v) rows)))
                    (switch-to-buffer u)
                    (buffer-close v)
                    r))))
    (buffer-set-body u (stree->tree `(document ,@(long-c-lines 3000))))
    (switch-to-buffer u)
    (let ((d (buffer-get-body u)))
      (tree-sitter-line-colors "c" (tree-ref d 0))
      (check-true (> (length (syntax-region-statistics d lan)) 2))
      ;; the rows of the answers are those of the whole region
      (with n (syntax-region-node-at d lan 4501 3)
        (check-true (and (pair? n) (== (car (list-ref (car n) 2)) 4501))))
      ;; a key in a name, lines inserted, an unclosed comment, closed
      (tree-insert! (tree-ref d 4501) 9 "y")
      (tree-sitter-line-colors "c" (tree-ref d 4501))
      (tree-insert! d 3000 '("int k;" "int m;"))
      (tree-sitter-line-colors "c" (tree-ref d 3000))
      (tree-remove! d 3000 2)
      (tree-insert! d 2999 '("/* open"))
      (tree-sitter-line-colors "c" (tree-ref d 2999))
      (tree-remove! d 2999 1)
      (tree-sitter-line-colors "c" (tree-ref d 2999))
      ;; an edit over a cut: several lines removed around the middle
      (tree-remove! d 4400 300)
      (tree-insert! d 4400 (sublist (long-c-lines 3000) 4400 4700))
      (tree-sitter-line-colors "c" (tree-ref d 4400))
      (check= (colors d rows) (fresh d))
      ;; an error, at its row in the region
      (tree-insert! (tree-ref d 6001) 2 "@")
      (tree-sitter-line-colors "c" (tree-ref d 6001))
      (check-true (in? 6001 (map (lambda (e) (car (cadr e)))
                                 (syntax-region-errors d lan)))))
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))))

;; the colours of the lines of a buffer after edits (the text, the offsets
;; and the colours of the region are changed in place by a keystroke, made
;; again when lines come or go) are those of a fresh copy of its lines
(define (test-highlight-edits)
  (check-group "colours after edits")
  (let* ((lines '("int f (int n) {" "  /* a comment */" "  return n + 1;" "}"
                  "char *s = \"text\";" "int g;"))
         (u (string->url "tmfs://aux/treesit-edits"))
         (v (string->url "tmfs://aux/treesit-edits-copy"))
         (old (current-buffer))
         (colors (lambda (d) (map (lambda (i) (tree-sitter-line-colors "c" (tree-ref d i)))
                                  (iota (tree-arity d))))))
    (buffer-set-body u (stree->tree `(document ,@lines)))
    (switch-to-buffer u)
    (let ((d (buffer-get-body u)))
      (tree-sitter-line-colors "c" (tree-ref d 0))
      ;; keystrokes in a comment, in a name, a string opened and closed
      (tree-insert! (tree-ref d 1) 5 "x")
      (tree-sitter-line-colors "c" (tree-ref d 1))
      (tree-insert! (tree-ref d 2) 9 "m")
      (tree-sitter-line-colors "c" (tree-ref d 2))
      (tree-insert! (tree-ref d 5) 4 "\"")
      (tree-sitter-line-colors "c" (tree-ref d 5))
      (tree-remove! (tree-ref d 5) 4 1)
      (tree-sitter-line-colors "c" (tree-ref d 5))
      ;; lines which come and go
      (tree-insert! d 2 '("  int k = 0;" "  /* b"))
      (tree-sitter-line-colors "c" (tree-ref d 2))
      (tree-remove! d 3 1)
      (tree-sitter-line-colors "c" (tree-ref d 3))
      (let ((now (colors d)))
        (buffer-set-body v (stree->tree (tree->stree d)))
        (switch-to-buffer v)
        (check= now (colors (buffer-get-body v)))
        (buffer-close v)))
    ;; an opening bracket whose closing one is MISSING at the end stays in
    ;; the colour of errors after an edit of its line
    (buffer-set-body u (stree->tree '(document "int main () {" "  return 0;")))
    (switch-to-buffer u)
    (let ((d (buffer-get-body u))
          (errors (lambda (l) (list-filter l (lambda (r) (== (caddr r) "error"))))))
      (check= (errors (tree-sitter-line-colors "c" (tree-ref d 0))) '((12 13 "error")))
      (tree-insert! (tree-ref d 0) 6 "x")
      (check= (errors (tree-sitter-line-colors "c" (tree-ref d 0))) '((13 14 "error"))))
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))))

(define (test-highlight)
  (check= (treesit-capture-class "keyword") "keyword")
  (check= (treesit-capture-class "keyword.control.return") "keyword_control")
  (check= (treesit-capture-class "string.special.key") "declare_identifier")
  (check= (treesit-capture-class "variable") "")
  (check= (treesit-capture-class "stringy") "")
  (when (defined? 'tree-sitter-line-colors)
    (treesit-generate-cache "c")
    ;; Scheme, with <less> as the operator <
    (let ((d (stree->tree '(document "(define (f x)" "  (if (<less> x 1) \"a\\n\" 'b)) ; c"))))
      (tree-sitter-line-colors "scheme" d)
      (check= (tree-sitter-line-colors "scheme" (tree-ref d 0))
              '((0 1 "operator_openclose") (1 7 "keyword") (8 9 "operator_openclose")
                (9 10 "variable_function") (12 13 "operator_openclose")))
      (check= (tree-sitter-line-colors "scheme" (tree-ref d 1))
              '((2 3 "operator_openclose") (3 5 "keyword") (6 7 "operator_openclose")
                (7 13 "operator") (16 17 "constant_number") (17 18 "operator_openclose")
                (19 21 "constant_string") (21 23 "constant_char") (23 24 "constant_string")
                (25 27 "constant") (27 29 "operator_openclose") (30 33 "comment"))))
    ;; a comment over several lines of C
    (let ((d (stree->tree '(document "int x = 1;" "/* a" "b */ return 0;"))))
      (tree-sitter-line-colors "c" d)
      (check= (tree-sitter-line-colors "c" (tree-ref d 0))
              '((0 3 "declare_type") (6 7 "operator") (8 9 "constant_number")
                (9 10 "operator_field")))
      (check= (tree-sitter-line-colors "c" (tree-ref d 1)) '((0 4 "comment")))
      (check= (tree-sitter-line-colors "c" (tree-ref d 2))
              '((0 4 "comment") (5 11 "keyword") (12 13 "constant_number")
                (13 14 "operator_field"))))
    ;; the typesetting cuts the lines at the tokens of the tree, and at the
    ;; spaces (also in comments); the cuts are not colours
    (let ((d (stree->tree '(document "int xy = f(10, \"a b\"); // c d" "/* a" "b */ return 0;"))))
      (tree-sitter-line-colors "c" d)
      (check= (tree-sitter-line-tokens "c" (tree-ref d 0))
              '("int" " " "xy" " " "=" " " "f" "(" "10" "," " " "\"" "a" " " "b"
                "\"" ")" ";" " " "//" " " "c" " " "d"))
      (check= (tree-sitter-line-tokens "c" (tree-ref d 2))
              '("b" " " "*/" " " "return" " " "0" ";"))
      (check= (tree-sitter-line-colors "c" (tree-ref d 1)) '((0 4 "comment"))))
    ;; unmatched brackets have the colour of errors
    (let ((colors (lambda (lan line)
                    (let ((d (stree->tree (list 'document line))))
                      (tree-sitter-line-colors lan d)
                      (list-filter (tree-sitter-line-colors lan (tree-ref d 0))
                                   (lambda (r) (== (caddr r) "error")))))))
      (check= (colors "c" "int x = (1 + 2;") '((8 9 "error")))
      (check= (colors "c" "int f() { return 1; } }") '((22 23 "error")))
      (check= (colors "scheme" "(define (f x) (g x)") '((0 1 "error")))
      (check= (colors "c" "int x = (1 + 2);") '()))
    ;; injections: the script and the style of HTML in JavaScript and CSS,
    ;; the arguments of a macro of Rust in Rust
    ;; (another suite may have asked for a highlighter before its tables
    ;; existed: it is installed here, the tables being made)
    (for-each (lambda (lan) (treesit-generate-cache lan) (treesit-highlighter-request lan))
              '("html" "javascript" "css" "rust"))
    (let ((d (stree->tree '(document "<less>script<gtr>var x = 1;<less>/script<gtr>"
                                     "<less>style<gtr>a { color: red }<less>/style<gtr>"))))
      (tree-sitter-line-colors "html" d)
      (check= (tree-sitter-line-colors "html" (tree-ref d 0))
              '((0 6 "operator_openclose") (12 17 "operator_openclose") (17 20 "keyword")
                (23 24 "operator") (25 26 "constant_number") (26 27 "operator_field")
                (27 34 "operator_openclose") (40 45 "operator_openclose")))
      (check= (tree-sitter-line-colors "html" (tree-ref d 1))
              '((0 6 "operator_openclose") (11 16 "operator_openclose")
                (18 19 "operator_openclose") (20 25 "variable_identifier")
                (25 26 "operator_field") (31 39 "operator_openclose")
                (44 49 "operator_openclose"))))
;; injections in injections (CSS in a template of the script of HTML),
    ;; and, after edits in the script (its tree parsed again incrementally),
    ;; the colors of a fresh highlighting
    (with-test-buffer '(document (html-code (document "<less>script<gtr>"
                                                      "var s = css`a { color: red }`;"
                                                      "var t = 1;"
                                                      "<less>/script<gtr>")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (all (lambda (doc)
                      (tree-sitter-line-colors "html" doc)
                      (map (lambda (i) (tree-sitter-line-colors "html" (tree-ref doc i)))
                           (iota (tree-arity doc))))))
          (check-true (in? '(16 21 "variable_identifier")   ; color
                           (tree-sitter-line-colors "html" (tree-ref code 1))))
          (all code)
          (tree-insert! (tree-ref code 2) 0 "/* ")
          (all code)
          (tree-insert! (tree-ref code 1) 0 "*/ ")
          (all code)
          (tree-remove! (tree-ref code 2) 0 3)
          (check= (all code) (all (stree->tree (tree->stree code))))
          (tree-sitter-pending-updates))))   ; (not for the tests below)
        (let ((d (stree->tree '(document "println!(\"{}\", x + 1);"))))
      (tree-sitter-line-colors "rust" d)
      (check= (tree-sitter-line-colors "rust" (tree-ref d 0))
              '((0 8 "preprocessor") (8 9 "operator_openclose") (9 13 "constant_string")
                (13 14 "operator_field") (19 20 "constant") (20 21 "operator_openclose")
                (21 22 "operator_field"))))
    ;; in a buffer, a modified line is highlighted with its region (found by
    ;; its ip), and the other lines whose colours change are to be typeset
    ;; again
    (with-test-buffer '(document "int x;" "y = 1;" "z */")
      (lambda (u b)
        (tree-sitter-line-colors "c" (tree-ref b 0))
        (check= (tree-sitter-line-colors "c" (tree-ref b 1))
                '((2 3 "operator") (4 5 "constant_number") (5 6 "operator_field")))
        (tree-insert! (tree-ref b 0) 0 "/* ")
        (check= (tree-sitter-line-colors "c" (tree-ref b 0)) '((0 9 "comment")))
        (check= (tree-sitter-line-colors "c" (tree-ref b 1)) '((0 6 "comment")))
        (let ((p (tree-sitter-pending-updates)))
          (check= (map tree->string p) '("y = 1;" "z */"))
          (check-true (tree-eq? (car p) (tree-ref b 1))))))
    ;; after edits, the colours (only the lines out of date are coloured
    ;; again) are those of a fresh highlighting
    (with-test-buffer '(document "int a = 1;" "char* s = \"x\";" "int b = 2;"
                                 "/* c */ int c;" "return a + b;")
      (lambda (u b)
        (let ((all (lambda (doc)
                     (tree-sitter-line-colors "c" doc)
                     (map (lambda (i) (tree-sitter-line-colors "c" (tree-ref doc i)))
                          (iota (tree-arity doc))))))
          (all b)
          (tree-insert! (tree-ref b 0) 0 "/* ")
          (all b)
          (tree-insert! (tree-ref b 2) 10 " */")
          (all b)
          (tree-remove! (tree-ref b 0) 0 3)
          (all b)
          (tree-insert! b 2 '("x = \"open;" "y;"))
          (all b)
          (tree-remove! b 1 2)
          (check= (all b) (all (stree->tree (tree->stree b)))))))
    ;; the typesetter highlights the code of scm-code
    (with-test-buffer '(document (scm-code (document "(define x 1)" "(if x \"a\" 'b)")))
      (lambda (u b)
        (update-forced)
        (let ((code (tree-ref b 0 0)))
          (tree-insert! (tree-ref code 1) 1 "when ")
          (update-forced)
          ;; made by the typesetter, then updated after the edit
          (check= (syntax-region-statistics code (treesit-language "scheme")) '(1 1))
          (check= (tree-sitter-line-colors "scheme" (tree-ref code 1))
                  '((0 1 "operator_openclose") (1 5 "keyword") (11 14 "constant_string")
                    (15 17 "constant") (17 18 "operator_openclose"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing with the syntax trees (ts-edit.scm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-edit)
  (when (defined? 'syntax-region-node-range)
    (treesit-generate-cache "python")
    (let ((py (treesit-language "python"))
          (d (stree->tree '(document "f(a, \"(x\")" "if b:" "    g([1, 2])"))))
      ;; brackets, not those of strings
      (check= (syntax-matching-bracket d py '(0 1)) '(0 9))
      (check= (syntax-matching-bracket d py '(0 9)) '(0 1))
      (check= (syntax-matching-bracket d py '(0 6)) #f)
      (check= (syntax-matching-bracket d py '(0 0)) #f)
      (check= (syntax-matching-bracket d py '(2 6)) '(2 11))
      (check= (syntax-matching-bracket d py '(2 12)) '(2 5))
      ;; the selection grows by nodes
      (check= (syntax-enlarge-range d py '(2 7) '(2 7)) '((2 7) (2 8)))
      (check= (syntax-enlarge-range d py '(2 7) '(2 8)) '((2 6) (2 12)))
      (check= (syntax-enlarge-range d py '(2 6) '(2 12)) '((2 5) (2 13)))
      (check= (syntax-enlarge-range d py '(0 0) '(2 13)) #f))
    ;; inputs of sessions which more lines must complete
    (let ((py (treesit-language "python"))
          (incomplete? (lambda (lines lan)
                         (syntax-incomplete? (stree->tree (cons 'document lines)) lan))))
      (check-true (incomplete? '("def f(x):") py))
      (check-true (incomplete? '("f(1," "  2") py))
      (check-true (incomplete? '("x = [1, 2" "") py))
      (check-false (incomplete? '("x = 1") py))
      (check-false (incomplete? '("def f(x):" "    return x") py))
      (check-false (incomplete? '("x = ) + 1") py))
      (check-false (incomplete? '("") py))
      (treesit-generate-cache "scheme")
      (let ((scm (treesit-language "scheme")))
        (check-true (incomplete? '("(define (f x)") scm))
        (check-false (incomplete? '("(+ 1 2)") scm))))
    ;; Return in a Python session: a new line while the input is open
    (with-test-buffer '(document (session "python" "default"
                                   (document (input "<gtr><gtr><gtr> " (document "def f(x):")))))
      (lambda (u b)
        (update-forced)
        (let* ((in (tree-ref b 0 2 0))
               (line (tree->path (tree-ref in 1 0))))
          (when (>= (tree-sitter-program-language "python") 0)
            (go-to (rcons line 9))
            ;; (kbd-enter is called directly: a key in program code would
            ;; load the keyboard of programs, which changes the suites run
            ;; afterwards in the same TeXmacs)
            (kbd-enter in #f)
            ;; (the new line is indented after the colon once the module
            ;; prog-edit is loaded, as it is by any file of a program)
            (check= (tree->stree (tree-ref in 1 0)) "def f(x):")
            (check-true (in? (tree->stree (tree-ref in 1 1)) '("" "    ")))))))
    ;; indentation: the indentation of each line of a program, computed
    (let ((indents
           (lambda (lan lines)
             (unless (treesit-cached? lan) (treesit-generate-cache lan))
             (let ((id (treesit-language lan))
                   (d (stree->tree (cons 'document lines))))
               (map (lambda (row) (syntax-language-indentation d row lan id 4))
                    (iota (length lines)))))))
      ;; (the lines are indented as computed, except the blank ones)
      (check= (indents "python" '("def f(x):" "" "    if x:" "        y = [1]" ""
                                  "    else:" "        return g(1," ""))
              ;; (aligned with the open bracket, by the query of Neovim)
              '(0 4 4 8 8 4 8 17))
      (check= (indents "c" '("int f(int x) {" "" "    if (x)" ""
                             "        return 1;" "}" "" "int g();"))
              '(0 4 4 8 8 0 0 0))
      ;; by the queries of Neovim: else, case, the closing brace; in a string
      ;; of Python, the indentation of the previous line (@indent.auto)
      (check= (indents "c" '("int f(int x) {" "    if (x)" "        g();" "    else"
                             "        h();" "    switch (x) {" "    case 1:" "        k();"
                             "    }" "}" ""))
              '(0 4 8 4 8 4 8 12 4 0 0))
      (check= (indents "python" '("s = \"\"\"a" "  b" "\"\"\"" "def f():" "    return 1"))
              '(0 0 2 0 4))
;; Scheme as prog/scheme-edit.scm indents it (the keywords of tm-mode.el
      ;; and their arity, the alignment with the arguments), on the tree: a
      ;; character #\) or a quote in a string does not count
;; LaTeX: environments and groups indent, \end goes back; Haskell: one
      ;; step after a line which ends with where, do, = ..., else as the line
      ;; before (a comment at its end does not count)
      (check= (indents "latex" '("\\begin{itemize}" "  \\item a" "  \\begin{equation}"
                                 "    x" "  \\end{equation}" "\\end{itemize}"))
              '(0 4 4 6 2 0))
      (check= (indents "haskell" '("f x = do" "    let y = 1" "    print y" "  where"
                                   "    z = 3 -- where" ""))
              '(0 4 4 4 6 4))
      ;; (a symbol may start with digits, as 2d-list in TeXmacs)
      (check= (indents "scheme" '("(define 2d-list" "  (list 1))")) '(0 2))
      (check= (indents "scheme" '("(define (f x)" "  (let ((y x)" "        (z 1))"
                                  "    (g y" "       z)))" "(when x" "  y)"
                                  "(with a b" "  c)" "(let" "    ((x 1))"
                                  "  (list #\\) \"(\"" "        x))"))
              '(0 2 8 4 7 0 2 0 2 0 4 2 8))
      (check= (indents "javascript" '("function f() {" "    x = [1," "        2];" "}" ""))
              '(0 4 8 0 0))
      (check= (indents "julia" '("function f(x)" "    if x" "        1" "    else"
                                 "        2" "    end" "end"))
              '(0 4 8 4 8 4 0))
      ;; shell code, with the grammar of bash
      (treesit-generate-cache "bash")
      (let ((id (treesit-language "bash"))
            (d (stree->tree '(document "f() {" "    if [ -n $1 ]; then" "        echo a"
                                       "    else" "        echo b" "    fi" "}" ""))))
        (check= (map (lambda (row) (syntax-language-indentation d row "shell" id 4))
                     (iota 8))
                '(0 4 8 4 8 4 0 0))
        (check= (map cadr (syntax-outline d "shell" id)) '("f")))
      ;; Go (case at the indentation of switch), Rust, Lua and Lean
      (check= (indents "go" '("func f(x int) {" "    switch x {" "    case 1:" "        g()"
                              "    }" "}"))
              '(0 4 4 8 4 0))
      (check= (indents "rust" '("fn f() {" "    match x {" "        1 => g()," "    }" "}"))
              '(0 4 8 4 0))
      (check= (indents "lua" '("function f(x)" "    if x then" "        g()" "    else"
                               "        h()" "    end" "end"))
              '(0 4 8 4 8 4 0))
      (when (treesit-cached? "lean")
        (check= (indents "lean" '("def f (x : Nat) : Nat := by" "  simp" ""))
                '(0 4 4)))
      ;; TypeScript, CSS, TOML, HTML (an end tag closes) and YAML (a blank
      ;; line after key: value stays, after key: it is indented)
      (check= (indents "typescript" '("function f(x: number) {" "    return x;" "}"))
              '(0 4 0))
      (check= (indents "css" '("a {" "    color: red;" "}")) '(0 4 0))
      (check= (indents "toml" '("x = [" "    1," "]")) '(0 4 0))
      (check= (indents "html" '("<less>div<gtr>" "    <less>p<gtr>a<less>/p<gtr>"
                                "<less>/div<gtr>"))
              '(0 4 0))
      (check= (indents "yaml" '("a:" "" "    b: 1" "" "    c:" "        - x" ""
                                "d: [1," ""))
              '(0 4 4 4 4 8 8 0 4))
      (let ((outline (lambda (lan lines)
                       (unless (treesit-cached? lan) (treesit-generate-cache lan))
                       (map (lambda (d) (list (car d) (cadr d)))
                            (syntax-outline (stree->tree (cons 'document lines)) lan
                                            (treesit-language lan))))))
        (check= (outline "go" '("package p" "func F() {}" "type T struct {}"))
                '(("function" "F") ("type" "T")))
        (check= (outline "rust" '("struct S;" "fn f() {}"))
                '(("class" "S") ("function" "f")))
        (check= (outline "lua" '("function f() end" "local function g() end"))
                '(("function" "f") ("function" "g")))
        (when (treesit-cached? "lean")
          (check= (outline "lean" '("theorem t : True := trivial" "def f := 1"))
                  '(("theorem" "t") ("function" "f"))))
        (check= (outline "typescript" '("interface I {}" "function f(): void {}"))
                '(("interface" "I") ("function" "f")))
        (check= (outline "ocaml" '("let f x = x" "module M = struct end"))
                '(("function" "f") ("module" "M")))
;; outlines of ours for CSS, TOML, YAML, JSON, HTML and LaTeX
        (for-each treesit-generate-cache '("css" "toml" "yaml" "json" "html" "latex"))
        (check= (outline "css" '("a, b:hover { color: red }" "@keyframes spin { }"))
                '(("rule" "a, b:hover") ("keyframes" "spin")))
        (check= (outline "toml" '("x = 1" "[server]" "host = 1" "[[items]]"))
                '(("table" "server") ("table" "items")))
        (check= (outline "yaml" '("name: x" "jobs:" "  build: y"))
                '(("key" "name") ("key" "jobs") ("key" "build")))
        (check= (outline "json" '("{\"a\": 1, \"b\": {\"c\": 2}}"))
                '(("key" "a") ("key" "b") ("key" "c")))
        (check= (outline "html" '("<less>h1<gtr>Title<less>/h1<gtr>"
                                  "<less>div id=\"main\"<gtr><less>/div<gtr>"))
                '(("heading" "Title") ("element" "main")))
        (check= (outline "latex" '("\\section{Intro}\\label{sec:i}" "\\newcommand{\\foo}{x}"))
                '(("section" "Intro") ("label" "sec:i") ("macro" "\\foo")))
        ;; the two equations of f are one definition
        (check= (outline "haskell" '("f 0 = 1" "f n = n" "data D = D" "class C a where"))
                '(("function" "f") ("class" "D") ("interface" "C")))))
    ;; the formats of the source files
    (check= (map format-from-suffix '("go" "rs" "lua" "lean"))
            '("go" "rust" "lua" "lean"))
    (check= (map format-from-suffix '("tsx" "mts" "css" "toml" "yaml" "yml" "ml" "mli" "hs"))
            '("typescript" "typescript" "css" "toml" "yaml" "yaml" "ocaml" "ocaml" "haskell"))
    ;; shell scripts are source files of the language shell, highlighted by
    ;; the grammar bash (#734)
    (check= (map format-from-suffix '("sh" "bash" "zsh" "SH"))
            '("shell" "shell" "shell" "shell"))
    (check-true (format? "shell"))
    (treesit-generate-cache "bash")
    (treesit-highlighter-request "shell")
    (let ((d (stree->tree '(document "for f in *.txt; do" "  echo \"$f\" | wc -l # c" "done"))))
      (tree-sitter-line-colors "shell" d)
      (check= (tree-sitter-line-colors "shell" (tree-ref d 0))
              '((0 3 "keyword") (4 5 "variable_identifier") (6 8 "keyword")
                (16 18 "keyword")))
      (check= (tree-sitter-line-colors "shell" (tree-ref d 1))
              '((2 6 "variable_function") (7 8 "constant_string") (8 9 "operator")
                (9 10 "variable_identifier") (10 11 "constant_string")
                (12 13 "operator") (14 16 "variable_function") (17 19 "constant")
                (20 23 "comment"))))
    ;; the indentation at the cursor, in C++ code (as Return asks it)
    (treesit-generate-cache "cpp")
    (with-test-buffer '(document (cpp-code (document "int f() {" "return 0;" "}")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (line (tree->path (tree-ref code 1))))
          (when (>= (tree-sitter-program-language "cpp") 0)
            (go-to (rcons line 0))
            (check= (map (lambda (row) (syntax-compute-indentation code row 4))
                         '(0 1 2))
                    '(0 4 0))))))
    ;; the definitions (tags.scm)
    (let ((outline (lambda (lan lines)
                     (unless (treesit-cached? lan) (treesit-generate-cache lan))
                     (map (lambda (d) (list (car d) (cadr d) (caddr d)))
                          (syntax-outline (stree->tree (cons 'document lines)) lan
                                          (treesit-language lan))))))
      (check= (outline "python" '("class A:" "    def f(self):" "        g(1)" "X = 1"))
              ;; (not X: as with tree-sitter query, the pattern of the
              ;; constants of tags.scm finds nothing, expression_statement
              ;; being a supertype)
              '(("class" "A" (0 6)) ("function" "f" (1 8))))
      (check= (outline "c" '("struct s { int a; };" "int f(int x) { return x; }"))
              '(("class" "s" (0 7)) ("function" "f" (1 4))))
      ;; (a name for two definitions: a function in a module is a method and
      ;; a function; the next definition keeps its own name)
      (check= (outline "rust" '("mod m { fn x() { struct P; } }"))
              '(("module" "m" (0 4)) ("method" "x" (0 11)) ("class" "P" (0 24))))
      (check= (outline "c" '("typedef struct N { int a; } M;"))
              '(("type" "N" (0 15))))
      ;; (a very long line: the definition after it is not taken for the one
      ;; which has the same column modulo 2^20)
      (check= (outline "javascript"
                       (list (string-append (make-string 1048567 #\space)
                                            "function a(){} function")
                             "b(){}"))
              '(("function" "a" (0 1048576)) ("function" "b" (1 0))))
      (check= (outline "julia" '("module M" "f(x) = 1" "function g(y)" "end" "end"))
              '(("module" "M" (0 7)) ("function" "f" (1 0)) ("function" "g" (2 9))))
      (check= (outline "scheme" '("(define (f x) x)" "(tm-define g 1)"))
              '(("function" "f" (0 9)) ("variable" "g" (1 11))))
      (check= (outline "json" '("{}")) '()))
    ;; comments
    (check= (toggle-line-comments '("  a" "" "    b") "#") '("  # a" "" "  #   b"))
    (check= (toggle-line-comments '("  # a" "" "  #   b") "#") '("  a" "" "    b"))
    (check= (toggle-line-comments '("// a" "b") "//") '("// // a" "// b"))
    ;; with block comments, for the languages which have no line comments
    (check= (toggle-block-comments '("a {" "" "  b;") '("/*" . "*/"))
            '("/* a { */" "" "  /* b; */"))
    (check= (toggle-block-comments '("/* a { */" "" "  /* b; */") '("/*" . "*/"))
            '("a {" "" "  b;"))
    ;; at the cursor, in the code of a buffer
    (with-test-buffer '(document (scm-code (document "(define x 1)" "(if x \"(\" 'b)")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (line (tree->path (tree-ref code 1))))
          (check-true (integer? (tree-sitter-program-language "scheme")))
          (go-to (rcons line 4))
          (check= (map cadr (syntax-definitions-at-cursor)) '("x"))
          (check-true (syntax-go-to-definition))
          (check= (cursor-path) (rcons (tree->path (tree-ref code 0)) 8))
          (go-to (rcons line 4))
          (check-true (syntax-select-enlarge))
          (check= (list (selection-get-start) (selection-get-end))
                  (list (rcons line 4) (rcons line 5)))
          (check-true (syntax-select-enlarge))
          (check= (list (selection-get-start) (selection-get-end))
                  (list (rcons line 0) (rcons line 13)))
          (selection-cancel)
          (go-to (rcons line 0))
          (check-true (syntax-select-brackets))
          (check= (get-alt-selection "brackets")
                  (list (rcons line 0) (rcons line 1) (rcons line 12) (rcons line 13)))
          ;; the ( of the string has no match
          (go-to (rcons line 7))
          (syntax-select-brackets)
          (check= (get-alt-selection "brackets") '())
          ;; comments
          (check-true (syntax-toggle-comment))
          (check= (tree->string (tree-ref code 1)) "; (if x \"(\" 'b)")
          (syntax-toggle-comment)
          (check= (tree->string (tree-ref code 1)) "(if x \"(\" 'b)"))))
    ;; balanced brackets: pairs, typing over a closing bracket, deleting an
    ;; empty pair, selecting a pair with content; in strings, characters
    (with-test-buffer '(document (scm-code (document "(f x)" "" "(display \"ab\")" "")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (at (lambda (row col) (rcons (tree->path (tree-ref code row)) col)))
               (line (lambda (row) (tree->string (tree-ref code row)))))
          (go-to (at 1 0))
          (check-true (syntax-bracket-open "(" ")"))
          (check= (line 1) "()")
          (check= (cursor-path) (at 1 1))
          (insert "a")
          (check-true (syntax-bracket-close "(" ")"))
          (check= (line 1) "(a)")
          (check= (cursor-path) (at 1 3))
          (check-true (syntax-delete-bracket #f))   ; selects (a)
          (check= (list (selection-get-start) (selection-get-end))
                  (list (at 1 0) (at 1 3)))
          (selection-cancel)
          (go-to (at 1 2))
          (syntax-bracket-open "[" "]")
          (check= (line 1) "(a[])")
          (check-true (syntax-delete-bracket #f))   ; deletes []
          (check= (line 1) "(a)")
          (check= (cursor-path) (at 1 2))
          ;; before a word, an opening bracket alone
          (go-to (at 0 3))
          (syntax-bracket-open "(" ")")
          (check= (line 0) "(f (x)")
          ;; in a string, plain characters, and its closing quote typed over
          (go-to (at 2 11))
          (syntax-bracket-open "(" ")")
          (check= (line 2) "(display \"a(b\")")
          (go-to (at 2 13))
          (syntax-bracket-open "\"" "\"")
          (check= (line 2) "(display \"a(b\")")
          (check= (cursor-path) (at 2 14))
          ;; quotes in code come in pairs, and an empty string goes at once
          (go-to (at 3 0))
          (syntax-bracket-open "\"" "\"")
          (check= (line 3) "\"\"")
          (check-true (syntax-delete-bracket #f))
          (check= (line 3) ""))))
;; structural edits: swap, delete, raise, remove brackets, wrap
    (with-test-buffer '(document (scm-code (document "(list a (f b) c)" "(g (h x))"
                                                     "(k y)" "(m (p q)" "   r)")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (at (lambda (row col) (rcons (tree->path (tree-ref code row)) col)))
               (line (lambda (row) (tree->string (tree-ref code row)))))
          (go-to (at 0 6))
          (check-true (syntax-swap-node #t))
          (check= (line 0) "(list (f b) a c)")
          (check= (cursor-path) (at 0 12))
          (check-true (syntax-swap-node #f))
          (check= (line 0) "(list a (f b) c)")
          (check= (cursor-path) (at 0 6))
          (check-true (syntax-delete-node #t))
          (check= (line 0) "(list  (f b) c)")
          (go-to (at 0 10))
          (check-true (syntax-raise-node))
          (check= (line 0) "(list  b c)")
          (check= (cursor-path) (at 0 7))
          (go-to (at 1 6))
          (check-true (syntax-splice-brackets))
          (check= (line 1) "(g h x)")
          (check= (cursor-path) (at 1 5))
          (go-to (at 2 3))
          (check-true (syntax-wrap-node "[" "]"))
          (check= (line 2) "(k [y])")
          ;; on a closing bracket there is no node; backwards, the node which
          ;; ends at the cursor
          (go-to (at 2 6))
          (check-false (syntax-delete-node #t))
          (go-to (at 2 2))
          (check-true (syntax-delete-node #f))
          (check= (line 2) "( [y])")
          ;; over two lines
          (go-to (at 3 3))
          (check-true (syntax-swap-node #t))
          (check= (list (line 3) (line 4)) '("(m r" "   (p q))"))
          (check= (cursor-path) (at 4 3)))))
    ;; folds: a state of the lines, not of the document; the cursor arriving
    ;; in a hidden line unfolds
    (with-test-buffer '(document (scm-code (document "(define (f x)" "  (let ((y x))"
                                                     "    y))" "(define (g) 1)")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (at (lambda (row col) (rcons (tree->path (tree-ref code row)) col)))
               (states (lambda () (map (lambda (i) (tree-sitter-fold-state (tree-ref code i)))
                                       (iota 4)))))
          (go-to (at 1 3))
          (check-true (syntax-toggle-fold))
          (check= (states) '(0 0 1 0))
          (check= (cursor-path) (at 1 2))
          (check-true (syntax-toggle-fold))
          (check= (states) '(0 0 0 0))
          (check-true (syntax-fold-definitions))
          (check= (states) '(0 1 1 0))
          (check= (tree->stree code)
                  '(document "(define (f x)" "  (let ((y x))" "    y))" "(define (g) 1)"))
          (go-to (at 2 1))
          (check= (states) '(0 0 0 0))
          (syntax-fold-definitions)
          (check-true (syntax-unfold-all))
          (check= (states) '(0 0 0 0)))))
    ;; folds nest: unfolding the outer one keeps the inner one
    (for-each (lambda (l) (treesit-generate-cache l) (treesit-highlighter-request l))
              '("cpp" "css"))
    (with-test-buffer '(document (cpp-code (document "int f () {" "  if (x) {" "    a ();"
                                                     "  }" "}")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (at (lambda (row col) (rcons (tree->path (tree-ref code row)) col)))
               (states (lambda () (map (lambda (i) (tree-sitter-fold-state (tree-ref code i)))
                                       (iota 5)))))
          (go-to (at 2 4))
          (check-true (syntax-fold-block))
          (check= (states) '(0 0 1 0 0))
          (go-to (at 0 0))
          (check-true (syntax-fold-block))
          (check= (states) '(0 1 2 1 0))
          (check-false (syntax-fold-block))   ; folded already
          (check-true (syntax-unfold-block))
          (check= (states) '(0 0 1 0 0))
          (syntax-fold-block)
          (go-to (at 2 2))                   ; into both folds
          (check= (states) '(0 0 0 0 0)))))
    ;; without definitions (CSS has no tags.scm), the blocks at the top level
    (with-test-buffer '(document (css-code (document "a {" "  color: red;" "}" "b {"
                                                     "  x: y;" "}")))
      (lambda (u b)
        (update-forced)
        (let ((code (tree-ref b 0 0)))
          (go-to (rcons (tree->path (tree-ref code 0)) 0))
          (check-true (syntax-fold-definitions))
          (check= (map (lambda (i) (tree-sitter-fold-state (tree-ref code i))) (iota 6))
                  '(0 1 0 0 1 0)))))
    ;; closing keywords and tags: after Return, the keyword which closes the
    ;; block opened by the line before (the first of the grammar which makes
    ;; fewer errors), the \end of a \begin; the end tag of HTML after the >
    ;; of a start tag (not for void elements)
    (for-each (lambda (l) (treesit-generate-cache l) (treesit-highlighter-request l))
              '("lua" "bash" "latex" "html"))
    (let ((closed (lambda (tag lines)
                    (let ((result #f))
                      (with-test-buffer (list 'document (list tag (cons 'document lines)))
                        (lambda (u b)
                          (update-forced)
                          (let ((code (tree-ref b 0 0)))
                            (go-to (rcons (tree->path (tree-ref code 1)) 2))
                            (syntax-close-block)
                            (set! result (tree->stree code)))))
                      result))))
      (check= (closed 'lua-code '("function f()" "  "))
              '(document "function f()" "  " "end"))
      (check= (closed 'shell-code '("if true; then" "  "))
              '(document "if true; then" "  " "fi"))
      (check= (closed 'shell-code '("for i in a b; do" "  "))
              '(document "for i in a b; do" "  " "done"))
      (check= (closed 'latex-code '("\\begin{itemize}" "  "))
              '(document "\\begin{itemize}" "  " "\\end{itemize}"))
      ;; nothing to close
      (check= (closed 'lua-code '("function f()" "  " "end"))
              '(document "function f()" "  " "end")))
    (with-test-buffer '(document (html-code (document "<less>div<gtr>" "<less>br<gtr>")))
      (lambda (u b)
        (update-forced)
        (let ((code (tree-ref b 0 0)))
          ;; (positions in the TeXmacs strings: <less>div<gtr> has 14)
          (go-to (rcons (tree->path (tree-ref code 0)) 14))
          (check-true (syntax-close-tag))
          (check= (tree->string (tree-ref code 0)) "<less>div<gtr><less>/div<gtr>")
          (check= (cursor-path) (rcons (tree->path (tree-ref code 0)) 14))
          (go-to (rcons (tree->path (tree-ref code 1)) 13))
          (check-false (syntax-close-tag)))))
    ;; comments of nodes: a block comment for a part of a line, line comments
    ;; for whole lines; the same command uncomments
    (with-test-buffer '(document (scm-code (document "(define (f x)" "  (g x))")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (at (lambda (row col) (rcons (tree->path (tree-ref code row)) col)))
               (lines (lambda () (cdr (tree->stree code)))))
          (go-to (at 1 2))
          (check-true (syntax-toggle-comment-node))
          (check= (lines) '("(define (f x)" "  #| (g x) |#)"))
          (go-to (at 1 6))
          (check-true (syntax-toggle-comment-node))
          (check= (lines) '("(define (f x)" "  (g x))"))
          (go-to (at 0 0))
          (check-true (syntax-toggle-comment-node))
          (check= (lines) '("; (define (f x)" ";   (g x))"))
          (go-to (at 0 3))
          (check-true (syntax-toggle-comment-node))
          (check= (lines) '("(define (f x)" "  (g x))")))))
        ;; structured moves: from node to node, around, inside, and from
    ;; definition to definition
    (with-test-buffer '(document (scm-code (document "(define (f x) x)"
                                                     "(define (g y) (f y))"
                                                     "(display 1)")))
      (lambda (u b)
        (update-forced)
        (let* ((code (tree-ref b 0 0))
               (at (lambda (row col) (rcons (tree->path (tree-ref code row)) col))))
          (go-to (at 0 0))
          (check-true (syntax-move-node 1))
          (check= (cursor-path) (at 1 0))
          (check-true (syntax-move-node 1))
          (check= (cursor-path) (at 2 0))
          (check-false (syntax-move-node 1))
          (check-true (syntax-move-node -1))
          (check= (cursor-path) (at 1 0))
          (go-to (at 1 5))
          (check-true (syntax-move-node -1))   ; the start of define
          (check= (cursor-path) (at 1 1))
          (check-true (syntax-move-node 2))
          (check= (cursor-path) (at 1 0))
          (check-true (syntax-move-node -2))
          (check= (cursor-path) (at 1 1))
          (go-to (at 0 0))
          (check-true (syntax-move-definition 1))
          (check= (cursor-path) (at 0 9))
          (check-true (syntax-move-definition 1))
          (check= (cursor-path) (at 1 9))
          (check-false (syntax-move-definition 1))
          (check-true (syntax-move-definition -1))
          (check= (cursor-path) (at 0 9))
          (go-to (at 1 16))
          (check-true (syntax-select-definition))
          (check= (list (selection-get-start) (selection-get-end))
                  (list (at 1 0) (at 1 20)))
          (selection-cancel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External scanners in the C runtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define raw-grammar
  ;; the raw strings of tree-sitter-cpp, with its external tokens
  "{\"name\": \"raw\", \"extras\": [{\"type\": \"PATTERN\", \"value\": \"\\\\s\"}], \"externals\": [{\"type\": \"SYMBOL\", \"name\": \"raw_string_delimiter\"}, {\"type\": \"SYMBOL\", \"name\": \"raw_string_content\"}], \"rules\": {\"program\": {\"type\": \"REPEAT\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"raw_string_literal\"}}, \"raw_string_literal\": {\"type\": \"SEQ\", \"members\": [{\"type\": \"CHOICE\", \"members\": [{\"type\": \"STRING\", \"value\": \"R\\\"\"}, {\"type\": \"STRING\", \"value\": \"LR\\\"\"}]}, {\"type\": \"CHOICE\", \"members\": [{\"type\": \"SEQ\", \"members\": [{\"type\": \"FIELD\", \"name\": \"delimiter\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"raw_string_delimiter\"}}, {\"type\": \"STRING\", \"value\": \"(\"}, {\"type\": \"SYMBOL\", \"name\": \"raw_string_content\"}, {\"type\": \"STRING\", \"value\": \")\"}, {\"type\": \"SYMBOL\", \"name\": \"raw_string_delimiter\"}]}, {\"type\": \"SEQ\", \"members\": [{\"type\": \"STRING\", \"value\": \"(\"}, {\"type\": \"SYMBOL\", \"name\": \"raw_string_content\"}, {\"type\": \"STRING\", \"value\": \")\"}]}]}, {\"type\": \"STRING\", \"value\": \"\\\"\"}]}}}")

(define (scan-lexer s)
  ;; a lexer of ts-parse.scm on the text s
  (let ((text (list->vector (map char->integer (string->list s)))))
    (vector text (vector-length text) 0 0 0 0 -1 0 0)))

(define (test-scan)
  ;; the helpers of ts-scan.scm, on the lexer in Scheme
  (let ((lx (scan-lexer "abcx")))
    (check= (scan-advance-until! lx "xy" #f) 3)
    (check-true (scan-char? lx #\x))
    (check= (scan-advance-until! lx "q" #f) 1)
    (check= (scan-advance-until! lx "q" #f) 0))
  (let ((lx (scan-lexer "abac")))
    (check= (scan-advance-while! lx "ab" #f) 3)
    (check= (lexer-lookahead lx) (char->integer #\c)))
  (let ((lx (scan-lexer (string #\space #\tab #\newline #\x))))
    (check= (scan-skip-spaces! lx #t #f) 2)
    (check= (scan-skip-spaces! lx #t #t) 1))
  (let ((lx (scan-lexer "elsewhere")))
    (check-true (scan-word! lx "else"))
    (check= (lexer-position lx) 4)
    (check-false (scan-word! lx "wx")))
  (let ((lx (scan-lexer "x")))
    (check-true (scan-accept! lx 3))
    (check= (vector-ref lx 8) 3))
  ;; the state of a record scanner: #f for the initial one
  (let* ((sc (record-scanner (vector '(0) #f) (lambda (p lx valid) #f)))
         (p ((vector-ref sc 0))))
    (check= ((vector-ref sc 3) p) #f)
    (vector-set! p 0 '(4 0))
    (let ((state ((vector-ref sc 3) p)))
      (check= state '((4 0) #f))
      ((vector-ref sc 2) p #f)
      (check= (vector-ref p 0) '(0))
      ((vector-ref sc 2) p state)
      (check= (vector-ref p 0) '(4 0)))))

(define (test-scanners)
  (when (defined? 'tree-sitter-set-scanner)
    ;; the scanner of C++ (in Scheme) called by the C runtime; the state
    ;; (the delimiter) goes through the buffer of tree-sitter
    (let* ((l (ts-generate-language raw-grammar))
           (id (tree-sitter-language-new (ts-export-language->string l))))
      (check-true (tree-sitter-set-scanner id (ts-cpp-scanner)))
      (check= (tree-sitter-parse-sexp id "R\"x(a)\"b)x\" R\"(c)\"")
              (string-append "(program (raw_string_literal delimiter: (raw_string_delimiter) "
                             "(raw_string_content) (raw_string_delimiter)) "
                             "(raw_string_literal (raw_string_content)))"))
      ;; and the same with the parser in Scheme
      (set-lang-external-scanner! l (ts-cpp-scanner))
      (check= (ts-tree->string (ts-parse-string l "R\"x(a)\"b)x\""))
              "(program (raw_string_literal delimiter: (raw_string_delimiter) (raw_string_content) (raw_string_delimiter)))"))
    ;; Python: indentation, newlines and strings by its scanner
    (treesit-generate-cache "python")
    (let ((py (treesit-language "python")))
      (check-true (integer? py))
      (check= (tree-sitter-parse-sexp py "if x:\n    y = '''a\nb'''\nz\n")
              (string-append "(module (if_statement condition: (identifier) consequence: "
                             "(block (assignment left: (identifier) "
                             "right: (string (string_start) (string_content) (string_end)))))"
                             " (identifier))"))
      (let ((d (stree->tree '(document "def f(x):" "    s = \"\"\"a" "b\"\"\"" "    return x + 1  # c"))))
        (tree-sitter-line-colors "python" d)
        (check= (tree-sitter-line-colors "python" (tree-ref d 0))
                '((0 3 "keyword") (4 5 "variable_function")))
        (check= (tree-sitter-line-colors "python" (tree-ref d 2)) '((0 4 "constant_string")))
        (check= (tree-sitter-line-colors "python" (tree-ref d 3))
                '((4 10 "keyword") (13 14 "operator") (15 16 "constant_number") (18 21 "comment")))))
    ;; JavaScript: automatic semicolons and template strings by its scanner
    (treesit-generate-cache "javascript")
    (check= (tree-sitter-parse-sexp (treesit-language "javascript") "a = 1\nb = `x${a}`\n")
            (string-append "(program (expression_statement (assignment_expression left: (identifier) "
                           "right: (number))) (expression_statement (assignment_expression "
                           "left: (identifier) right: (template_string (string_fragment) "
                           "(template_substitution (identifier))))))"))
    ;; R: the newline inside parentheses and the raw string by its scanner
    (treesit-generate-cache "r")
    (check= (tree-sitter-parse-sexp (treesit-language "r") "x <- f(1,\n  r\"-(a)-\")\ny")
            (string-append "(program (binary_operator lhs: (identifier) rhs: (call function: "
                           "(identifier) arguments: (arguments argument: (argument value: (float)) "
                           "(comma) argument: (argument value: (string open: (string_open) "
                           "content: (string_content) close: (string_close)))))) (identifier))"))
    ;; Bash: a heredoc and a function, as tree-sitter parse gives them
    (treesit-generate-cache "bash")
    (check= (tree-sitter-parse-sexp (treesit-language "bash")
                                    "cat <<EOF\nhi $x\nEOF\nf() {\n  echo \"a b\" > /dev/null\n}\n")
            (string-append "(program (redirected_statement body: (command name: (command_name "
                           "(word))) redirect: (heredoc_redirect (heredoc_start) (heredoc_body "
                           "(simple_expansion (variable_name)) (heredoc_content)) (heredoc_end))) "
                           "(function_definition name: (word) body: (compound_statement "
                           "(redirected_statement body: (command name: (command_name (word)) "
                           "argument: (string (string_content))) redirect: (file_redirect "
                           "destination: (word))))))"))
    ;; Go, Rust, Lua and Lean, as tree-sitter parse gives them
    (treesit-generate-cache "go")
    (check= (tree-sitter-parse-sexp (treesit-language "go") "package main\n\nfunc f(x int) int {\n\treturn x + 1\n}\n")
            "(source_file (package_clause (package_identifier)) (function_declaration name: (identifier) parameters: (parameter_list (parameter_declaration name: (identifier) type: (type_identifier))) result: (type_identifier) body: (block (statement_list (return_statement (expression_list (binary_expression left: (identifier) right: (int_literal))))))))")
    (treesit-generate-cache "rust")
    (check= (tree-sitter-parse-sexp (treesit-language "rust") "fn main() {\n    let s = r#\"a\"#; // c\n    let x = 1.5e3;\n}\n")
            "(source_file (function_item name: (identifier) parameters: (parameters) body: (block (let_declaration pattern: (identifier) value: (raw_string_literal (string_content))) (line_comment) (let_declaration pattern: (identifier) value: (float_literal)))))")
    (treesit-generate-cache "lua")
    (check= (tree-sitter-parse-sexp (treesit-language "lua") "local s = [[a]] --[[ c ]]\nprint(s)\n")
            "(chunk local_declaration: (variable_declaration (assignment_statement (variable_list name: (identifier)) (expression_list value: (string content: (string_content))))) (comment content: (comment_content)) (function_call name: (identifier) arguments: (arguments (identifier))))")
    ;; the tables of all the grammars are shipped (making them all takes more
    ;; than a minute, Lean alone a minute and gigabytes); those of the
    ;; present grammars and generator must be there (ship-tables.sh)
    (check= (list-filter treesit-languages
                         (lambda (lan) (not (url-exists? (treesit-shipped-url lan)))))
            '())
    ;; and they are those which the generator makes
    (for (lan '("json" "toml"))
      (let ((tmp (url-temp)))
        (treesit-generate-cache lan)
        (check-true (tree-sitter-unpack (treesit-shipped-url lan) tmp))
        (check-true (== (string-load tmp) (string-load (treesit-cache-url lan))))
        (system-remove tmp)))
    (when (url-exists? (treesit-shipped-url "lean"))
      (let ((tmp (url-temp)))
        (check-true (tree-sitter-unpack (treesit-shipped-url "lean") tmp))
        (check-true (string-starts? (string-load tmp) "tsdyn1"))
        (system-remove tmp)))
    ;; and a file which is not compressed by xz is refused
    (let ((tmp (url-temp)))
      (check-false (tree-sitter-unpack (treesit-grammar-url "lean") tmp)))
    (when (treesit-cached? "lean")
      (check= (tree-sitter-parse-sexp (treesit-language "lean") "def f (x : Nat) : Nat := by\n  simp\n")
              "(module (declaration (def name: (identifier) (binders (explicit_binder name: (identifier) type: (identifier))) type: (identifier) body: (by stmt: (identifier)))))"))
    ;; TypeScript, CSS, TOML, HTML, OCaml and LaTeX, as tree-sitter parse gives them
    (treesit-generate-cache "typescript")
    (check= (tree-sitter-parse-sexp (treesit-language "typescript") "let x: number = 1\nlet y = `a${x}`\n")
            "(program (lexical_declaration (variable_declarator name: (identifier) type: (type_annotation (predefined_type)) value: (number))) (lexical_declaration (variable_declarator name: (identifier) value: (template_string (string_fragment) (template_substitution (identifier))))))")
    (treesit-generate-cache "css")
    (check= (tree-sitter-parse-sexp (treesit-language "css") "a b:hover { color: red }\n")
            "(stylesheet (rule_set (selectors (pseudo_class_selector (descendant_selector (tag_name) (tag_name)) (class_name (identifier)))) (block (declaration (property_name) (plain_value)))))")
    (treesit-generate-cache "toml")
    (check= (tree-sitter-parse-sexp (treesit-language "toml") "s = \"\"\"a\nb\"\"\"\nt = 1\n")
            "(document (pair (bare_key) (string)) (pair (bare_key) (integer)))")
    (treesit-generate-cache "html")
    (check= (tree-sitter-parse-sexp (treesit-language "html") "<ul><li>a<li>b</ul><script>x < 1</script>\n")
            "(document (element (start_tag (tag_name)) (element (start_tag (tag_name)) (text)) (element (start_tag (tag_name)) (text)) (end_tag (tag_name))) (script_element (start_tag (tag_name)) (raw_text) (end_tag (tag_name))))")
    (treesit-generate-cache "ocaml")
    (check= (tree-sitter-parse-sexp (treesit-language "ocaml") "(* c (* n *) *)\nlet s = {id|x|id}\n")
            "(compilation_unit (comment) (value_definition (let_binding pattern: (value_name) body: (quoted_string (quoted_string_content)))))")
    (treesit-generate-cache "latex")
    (check= (tree-sitter-parse-sexp (treesit-language "latex") "\\begin{verbatim}\\x\\end{verbatim}\n")
            "(source_file (verbatim_environment begin: (begin name: (curly_group_text text: (text word: (word)))) verbatim: (comment) end: (end name: (curly_group_text text: (text word: (word))))))")
    ;; Haskell: layouts, operators, qualified names
    (treesit-generate-cache "haskell")
    (check= (tree-sitter-parse-sexp (treesit-language "haskell")
                                    "f :: Int -> Int\nf 0 = 1\nf n = n * f (n - 1)\nmain = do\n  let x = M.lookup 1 m\n  print $ f 3\n")
            "(haskell declarations: (declarations (signature name: (variable) type: (function parameter: (name) result: (name))) (function name: (variable) patterns: (patterns (literal (integer))) match: (match expression: (literal (integer)))) (function name: (variable) patterns: (patterns (variable)) match: (match expression: (infix left_operand: (variable) operator: (operator) right_operand: (apply function: (variable) argument: (parens expression: (infix left_operand: (variable) operator: (operator) right_operand: (literal (integer)))))))) (bind name: (variable) match: (match expression: (do statement: (let binds: (local_binds decl: (bind name: (variable) match: (match expression: (apply function: (apply function: (qualified module: (module (module_id)) id: (variable)) argument: (literal (integer))) argument: (variable)))))) statement: (exp (infix left_operand: (variable) operator: (operator) right_operand: (apply function: (variable) argument: (literal (integer))))))))))")
    ;; YAML (its scanner does almost everything)
    (treesit-generate-cache "yaml")
    (check= (tree-sitter-parse-sexp (treesit-language "yaml") "a: 1\nb:\n  - x\n  - 'y'\n")
            "(stream (document (block_node (block_mapping (block_mapping_pair key: (flow_node (plain_scalar (string_scalar))) value: (flow_node (plain_scalar (integer_scalar)))) (block_mapping_pair key: (flow_node (plain_scalar (string_scalar))) value: (block_node (block_sequence (block_sequence_item (flow_node (plain_scalar (string_scalar)))) (block_sequence_item (flow_node (single_quote_scalar))))))))))")
    ;; Java, without scanner
    (treesit-generate-cache "java")
    (check= (tree-sitter-parse-sexp (treesit-language "java") "class A { int f() { return 1; } }")
            (string-append "(program (class_declaration name: (identifier) body: (class_body "
                           "(method_declaration type: (integral_type) name: (identifier) "
                           "parameters: (formal_parameters) body: (block (return_statement "
                           "(decimal_integer_literal)))))))"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The parse states built in C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tables-both-ways grammar)
  ;; the exported tables of grammar, with the states built in C++ and in Scheme
  (let ((native (ts-export-language->string (ts-generate-language grammar))))
    (ts-set-native-tables! #f)
    (let ((scheme (catch #t
                    (lambda () (ts-export-language->string (ts-generate-language grammar)))
                    (lambda args (ts-set-native-tables! #t) (apply throw args)))))
      (ts-set-native-tables! #t)
      (cons native scheme))))

(define (test-native)
  (when (ts-native-tables?)
    ;; the same tables, byte for byte (precedences, fields, aliases, a
    ;; keyword, conflicts and inlined rules, externals)
    (for (g (list (ts-read-file (json-grammar-file)) calc-grammar raw-grammar
                  (string-load (url-concretize "$TEXMACS_PATH/misc/tree-sitter/scheme/grammar.json"))))
      (let ((r (tables-both-ways g)))
        (check-true (string=? (car r) (cdr r)))
        ;; and the same text when C++ writes it
        (when (defined? 'tree-sitter-tables-export)
          (check-true (string=? (ts-generate-tsdyn g) (car r))))))
    ;; the errors of the construction
    (let ((bad (string-append
                "{\"name\": \"bad\", \"rules\": {\"e\": {\"type\": \"CHOICE\", \"members\": ["
                "{\"type\": \"SEQ\", \"members\": [{\"type\": \"SYMBOL\", \"name\": \"e\"},"
                " {\"type\": \"STRING\", \"value\": \"+\"}, {\"type\": \"SYMBOL\", \"name\": \"e\"}]},"
                "{\"type\": \"STRING\", \"value\": \"x\"}]}}}")))
      (check= (catch #t (lambda () (ts-generate-language bad) "no error")
                (lambda args (if (string-index (object->string args) #\*) "conflict" args)))
              "conflict"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The names of the definitions, without a grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (outline-items defs names)
  ;; (kind name) with the name given by the place of its capture
  (map (lambda (i) (list (car i) (cadr i)))
       (syntax-outline-items defs names syntax-point<=?
                             (lambda (start end) (object->string start)))))

(define (cpp-line n)
  (apply string-append
         (map (lambda (i) (string-append "int f" (number->string i) "(){} "))
              (.. 0 n))))

(define (test-outline-items)
  (check-group "the names of the definitions")
  (let ((n1 '((0 4) (0 5)))
        (n2 '((3 4) (3 5))))
    ;; a name is that of one definition only, also when they are not one
    ;; after the other
    (check= (outline-items '(("function" (0 0) (5 0)) ("class" (0 0) (5 0))
                             ("function" (0 1) (5 0)))
                           (list n1))
            '(("function" "(0 4)")))
    ;; the name of the second definition is the same one, not the next one,
    ;; which stays for the third
    (check= (outline-items '(("function" (0 0) (5 0)) ("class" (0 0) (5 0))
                             ("type" (3 0) (4 0)))
                           (list n1 n2))
            '(("function" "(0 4)") ("type" "(3 4)")))
    ;; the same kind and name just after itself
    (check= (map cadr (syntax-outline-items
                       '(("function" (0 0) (1 0)) ("function" (2 0) (3 0)))
                       '(((0 0) (0 1)) ((2 0) (2 1)))
                       syntax-point<=? (lambda (start end) "f")))
            '("f"))
    ;; no name inside; a name which goes beyond the definition
    (check= (outline-items '(("function" (0 0) (0 3))) (list n1)) '())
    (check= (outline-items '(("function" (0 0) (0 5))
                             ("class" (1 0) (2 0)))
                           '(((0 4) (0 9)) ((1 2) (1 3))))
            '(("class" "(1 2)")))
    ;; points on lines of any length
    (check= (outline-items '(("function" (0 1048576) (0 1048580))
                             ("function" (1 0) (1 4)))
                           '(((0 1048576) (0 1048577)) ((1 0) (1 1))))
            '(("function" "(0 1048576)") ("function" "(1 0)"))))
  ;; once through the names: the number of comparisons, not the time
  (let* ((n 3000)
         (count 0)
         (leq? (lambda (a b) (set! count (+ count 1)) (syntax-point<=? a b)))
         (rows (.. 0 n))
         (defs (map (lambda (i) (list "function" (list i 0) (list i 9))) rows))
         (names (map (lambda (i) (list (list i 4) (list i 5))) rows)))
    (check= (length (syntax-outline-items defs names leq?
                                          (lambda (start end)
                                            (object->string start))))
            n)
    (check-true (< count (* 6 n)))
    ;; ... also for definitions without a name, whose search stops at once
    (set! count 0)
    (check= (syntax-outline-items
             (map (lambda (i) (list "function" (list i 0) (list i 1))) rows)
             names leq? (lambda (start end) ""))
            '())
    (check-true (< count (* 6 n)))))

(define (strings-taken thunk)
  ;; how many copies of a line @thunk takes for its places
  (let ((n (syntax-row-strings-taken)))
    (thunk)
    (- (syntax-row-strings-taken) n)))

;; (syntax errors: one line broken at many places)
(define (test-one-line-errors)
  (check-group "the syntax errors on one long line")
  (unless (treesit-cached? "cpp") (treesit-generate-cache "cpp"))
  (with-test-buffer
      `(document (cpp-code (document ,(apply string-append
                                             (map (lambda (i) "int ) ; ")
                                                  (.. 0 100))))))
    (lambda (u b)
      (update-forced)
      (apply-changes)
      (let ((leaf (let down ((x (buffer-tree)))
                    (cond ((tree-atomic? x)
                           (if (> (string-length (tree->string x)) 100) x #f))
                          (else (list-any down (tree-children x)))))))
        (tree-go-to leaf 0)
        (update-forced)
        (apply-changes)
        (when (>= (tree-sitter-program-language "cpp") 0)
          (let* ((r '())
                 (calls (strings-taken
                         (lambda () (set! r (syntax-errors-at-cursor))))))
            (check-true (> (length r) 10))
            (check= calls 1)))))))

(define (test-one-line-outline)
  ;; many names on one line, as in a minified file: one copy of the line, not
  ;; one per name, which took gigabytes (not the time nor the memory here)
  (check-group "the definitions on one long line")
  (unless (treesit-cached? "javascript") (treesit-generate-cache "javascript"))
  (let* ((n 200)
         (line (apply string-append
                      (map (lambda (i)
                             (string-append "function f" (number->string i)
                                            "(){} "))
                           (.. 0 n))))
         (doc (stree->tree `(document ,line)))
         (id (treesit-language "javascript"))
         (r '())
         (calls (strings-taken
                 (lambda () (set! r (syntax-outline doc "javascript" id))))))
    (check= (length r) n)
    (check= (cadr (car r)) "f0")
    (check= (cadr (cAr r)) (string-append "f" (number->string (- n 1))))
    (check= calls 1))
  ;; the same at the cursor, with the paths, in lines and in inline code
  (for (doc (list `(document (cpp-code (document ,(cpp-line 200))))
                  `(document (concat "a" (cpp ,(cpp-line 200))))))
    (unless (treesit-cached? "cpp") (treesit-generate-cache "cpp"))
    (with-test-buffer doc
      (lambda (u b)
        (update-forced)
        (apply-changes)
        (let* ((t* (let down ((x (buffer-tree)))
                     (cond ((tree-atomic? x)
                            (if (> (string-length (tree->string x)) 100) x #f))
                           (else (list-any down (tree-children x)))))))
          (tree-go-to t* 0)
          (update-forced)
          (apply-changes)
          (when (>= (tree-sitter-program-language "cpp") 0)
            (let* ((r '())
                   (calls (strings-taken
                           (lambda () (set! r (syntax-outline-at-cursor))))))
              (check= (length r) 200)
              ;; (the names, and the paths in inline code)
              (check-true (<= calls 2))
              (check-true (<= (strings-taken syntax-definitions-at-cursor) 2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (treesit-test-failures)
  (check-suite "treesit")
  (test-helpers)
  (test-tables)
  (test-parse)
  (test-corpus)
  (test-edits)
  (test-save)
  (test-calc)
  (test-c-runtime)
  (test-registry)
  (test-regions)
  (test-highlight)
  (test-highlight-edits)
  (test-long-regions)
  (test-scan)
  (test-edit)
  (test-scanners)
  (test-native)
  (test-outline-items)
  (test-one-line-outline)
  (test-one-line-errors)
  (check-end))
