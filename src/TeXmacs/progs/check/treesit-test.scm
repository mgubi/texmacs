
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
        (treesit ts-highlight) (treesit ts-scanner-cpp) (treesit ts-native)))

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
  (check= (parse-pattern "(?i)k" #f) (list 'class (list (cons 75 76) (cons 107 108)))))

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
  ;; some of test/corpus/main.txt of tree-sitter-json
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
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length s)) (reverse (cons (substring s start i) acc)))
          ((char=? (string-ref s i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
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
;; External scanners in the C runtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define raw-grammar
  ;; the raw strings of tree-sitter-cpp, with its external tokens
  "{\"name\": \"raw\", \"extras\": [{\"type\": \"PATTERN\", \"value\": \"\\\\s\"}], \"externals\": [{\"type\": \"SYMBOL\", \"name\": \"raw_string_delimiter\"}, {\"type\": \"SYMBOL\", \"name\": \"raw_string_content\"}], \"rules\": {\"program\": {\"type\": \"REPEAT\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"raw_string_literal\"}}, \"raw_string_literal\": {\"type\": \"SEQ\", \"members\": [{\"type\": \"CHOICE\", \"members\": [{\"type\": \"STRING\", \"value\": \"R\\\"\"}, {\"type\": \"STRING\", \"value\": \"LR\\\"\"}]}, {\"type\": \"CHOICE\", \"members\": [{\"type\": \"SEQ\", \"members\": [{\"type\": \"FIELD\", \"name\": \"delimiter\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"raw_string_delimiter\"}}, {\"type\": \"STRING\", \"value\": \"(\"}, {\"type\": \"SYMBOL\", \"name\": \"raw_string_content\"}, {\"type\": \"STRING\", \"value\": \")\"}, {\"type\": \"SYMBOL\", \"name\": \"raw_string_delimiter\"}]}, {\"type\": \"SEQ\", \"members\": [{\"type\": \"STRING\", \"value\": \"(\"}, {\"type\": \"SYMBOL\", \"name\": \"raw_string_content\"}, {\"type\": \"STRING\", \"value\": \")\"}]}]}, {\"type\": \"STRING\", \"value\": \"\\\"\"}]}}}")

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
                '((4 10 "keyword") (13 14 "operator") (15 16 "constant_number") (18 21 "comment")))))))

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
  (test-scanners)
  (test-native)
  (check-end))
