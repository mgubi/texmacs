
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
        (treesit ts-parse) (treesit ts-export)))

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
  (check-end))
