;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-edit.scm
;; DESCRIPTION : editing programs with their syntax trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The editing functions of prog-edit.scm ask here first: when the code
;; around the cursor has a syntax tree (its program language is highlighted
;; by tree-sitter), the selection grows by syntax nodes and brackets match
;; by the tree, so that those in strings and comments are left alone.
;;
;; The code is a region (Plugins/TreeSitter/syntax_region.cpp): a document
;; of lines or a single string. Positions in it are points (row column), the
;; column being the position in the TeXmacs string of the line. This module
;; only uses the glue, so that loading it does not load the generator.

(texmacs-module (treesit ts-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points and nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (point<? a b)
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cadr a) (cadr b)))))

(define (point<=? a b) (not (point<? b a)))

;; a node is (type named? start end field)
(define (node-type n) (car n))
(define (node-named? n) (cadr n))
(define (node-start n) (caddr n))
(define (node-end n) (cadddr n))

(define-public (syntax-enlarge-range root lan start end)
  ;; the range (start end) of the smallest node which contains start..end
  ;; and is larger, or #f
  (let loop ((l (syntax-region-node-range root lan (car start) (cadr start)
                                          (car end) (cadr end))))
    (and (pair? l)
         (let ((s (node-start (car l))) (e (node-end (car l))))
           (if (and (point<=? s start) (point<=? end e)
                    (or (point<? s start) (point<? end e)))
               (list s e)
               (loop (cdr l)))))))

(define brackets '(("(" . ")") ("[" . "]") ("{" . "}")))

(define (closing-of type) (assoc-ref brackets type))

(define (opening-of type)
  (let ((p (list-find brackets (lambda (b) (== (cdr b) type)))))
    (and p (car p))))

(define (bracket-at root lan p)
  ;; the bracket token which starts at p, with its parent: (node . parent)
  (let ((l (syntax-region-node-at root lan (car p) (cadr p))))
    (and (pair? l) (pair? (cdr l))
         (let ((n (car l)))
           (and (not (node-named? n))
                (== (node-start n) p)
                (or (closing-of (node-type n)) (opening-of (node-type n)))
                (cons n (cadr l)))))))

(define-public (syntax-matching-bracket root lan p)
  ;; when a bracket token of the tree starts at the point p, the point of
  ;; the bracket which matches it, else #f
  (let ((b (bracket-at root lan p)))
    (and b
         (let* ((type (node-type (car b)))
                (parent (cdr b))
                (close (closing-of type))
                (q (if close
                       (let ((e (node-end parent)))
                         (list (car e) (- (cadr e) 1)))
                       (node-start parent)))
                (b* (and (>= (cadr q) 0) (bracket-at root lan q))))
           (and b* (== (node-type (car b*)) (or close (opening-of type)))
                q)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (path-context p lan)
  ;; (root-path root row column) for the position p in a string of code
  (and (pair? p) (pair? (cdr p))
       (let* ((leaf (cDr p))
              (t (path->tree leaf)))
         (and (tree-atomic? t)
              (let ((parent (and (pair? leaf) (path->tree (cDr leaf)))))
                (if (and parent (tree-is? parent 'document))
                    (list (cDr leaf) parent (cAr leaf) (cAr p))
                    (list leaf t 0 (cAr p))))))))

(define (context-path ctx point)
  ;; the path of a point of the region of ctx (a point after the last line,
  ;; such as the end of the root node, is the end of that line)
  (let ((root (cadr ctx)))
    (cond ((not (tree-is? root 'document))
           (append (car ctx) (list (min (cadr point) (string-length (tree->string root))))))
          ((< (car point) (tree-arity root))
           (append (car ctx) (list (car point) (cadr point))))
          (else
           (let ((last (- (tree-arity root) 1)))
             (append (car ctx)
                     (list last (string-length (tree->string (tree-ref root last))))))))))

(define-public (syntax-cursor-language)
  ;; the language of tree-sitter of the code at the cursor, or #f
  (and (in-prog?)
       (let ((id (tree-sitter-program-language (get-env "prog-language"))))
         (and (>= id 0) id))))

(define-public (syntax-select-enlarge)
  ;; select the smallest syntax node around the selection (or the cursor);
  ;; #f when the code has no syntax tree
  (let* ((lan (syntax-cursor-language))
         (sel? (selection-active-normal?))
         (a (and lan (path-context (if sel? (selection-get-start) (cursor-path)) lan)))
         (b (and a (path-context (if sel? (selection-get-end) (cursor-path)) lan))))
    (and a b (== (car a) (car b))
         (let ((r (syntax-enlarge-range (cadr a) lan (cddr a) (cddr b))))
           (when r
             (selection-set (context-path a (car r)) (context-path a (cadr r))))
           #t))))

(define (path++ p) (rcons (cDr p) (+ (cAr p) 1)))

(define-public (syntax-select-brackets)
  ;; highlight the bracket at the cursor, or before it, and its match; #f
  ;; when the code has no syntax tree
  (let* ((lan (syntax-cursor-language))
         (ctx (and lan (path-context (cursor-path) lan))))
    (and ctx
         (let* ((root (cadr ctx))
                (p (cddr ctx))
                (p* (list (car p) (- (cadr p) 1)))
                (here (syntax-matching-bracket root lan p))
                (before (and (not here) (>= (cadr p*) 0)
                             (syntax-matching-bracket root lan p*)))
                (from (if here p p*))
                (to (or here before)))
           (if (not to)
               (when (nnull? (get-alt-selection "brackets"))
                 (cancel-alt-selection "brackets"))
               (let ((x (context-path ctx from)) (y (context-path ctx to)))
                 (set-alt-selection "brackets"
                                    (list x (path++ x) y (path++ y)))))
           #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs of sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (text-end body)
  ;; the point after the last character which is not a space
  (let loop ((row (- (tree-arity body) 1)))
    (if (< row 0) '(0 0)
        (let* ((line (tree-ref body row))
               (s (if (tree-atomic? line) (tree->string line) ""))
               (n (let trim ((n (string-length s)))
                    (if (and (> n 0) (char-whitespace? (string-ref s (- n 1))))
                        (trim (- n 1))
                        n))))
          (if (> n 0) (list row n) (loop (- row 1)))))))

(define-public (syntax-incomplete? body lan)
  ;; whether the program of body (a document of lines) has an error which
  ;; reaches its end (an open bracket), or ends with an empty node (the
  ;; block after a line which starts one, in Python): then only more lines
  ;; can complete it
  (and (tree-is? body 'document)
       (let ((end (text-end body)))
         (and (not (== end '(0 0)))
              (or (list-or (map (lambda (e) (point<=? end (caddr e)))
                                (syntax-region-errors body lan)))
                  (let ((l (syntax-region-last-nodes body lan)))
                    (and (pair? l)
                         (list-or (map (lambda (n)
                                         (and (node-named? n)
                                              (== (node-start n) (node-end n))))
                                       (cdr l))))))))))

(define-public (syntax-input-incomplete? body)
  ;; the same for the input of a session, in the language at the cursor
  (let ((id (tree-sitter-program-language (get-env "prog-language"))))
    (and (>= id 0) (syntax-incomplete? body id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A line is indented one step more than the line where the innermost node
;; of an indenting type which is still open around it starts; as much as that
;; line when it starts with the token which closes that node (}, ), end) or
;; with a branch (else, except...). For each program language: the indenting
;; types, the branches, and for an offside language (Python, whose blocks
;; end where the indentation goes back) the types of its brackets: only
;; those are closed before a line.

(define c-indenting
  '("compound_statement" "field_declaration_list" "declaration_list"
    "enumerator_list" "initializer_list" "argument_list" "parameter_list"
    "case_statement" "if_statement" "for_statement" "while_statement"
    "do_statement" "else_clause" "parenthesized_expression"))

(define syntax-indent-rules
  `(("c" ,c-indenting ("else_clause") #f)
    ("cpp" ,(append c-indenting '("lambda_expression" "try_statement"))
     ("else_clause") #f)
    ("python"
     ("function_definition" "class_definition" "if_statement" "for_statement"
      "while_statement" "with_statement" "try_statement" "match_statement"
      "case_clause" "elif_clause" "else_clause" "except_clause"
      "finally_clause" "argument_list" "parameters" "list" "dictionary" "set"
      "tuple" "parenthesized_expression")
     ("elif_clause" "else_clause" "except_clause" "finally_clause")
     ("argument_list" "parameters" "list" "dictionary" "set" "tuple"
      "parenthesized_expression"))
    ("java"
     ("block" "class_body" "interface_body" "enum_body" "argument_list"
      "formal_parameters" "array_initializer" "switch_block"
      "switch_block_statement_group" "if_statement" "for_statement"
      "while_statement" "do_statement" "parenthesized_expression")
     () #f)
    ("javascript"
     ("statement_block" "object" "array" "arguments" "formal_parameters"
      "class_body" "switch_body" "switch_case" "switch_default"
      "template_substitution" "parenthesized_expression" "if_statement"
      "for_statement" "while_statement" "do_statement" "else_clause")
     ("else_clause") #f)
    ("json" ("object" "array") () #f)
    ("r"
     ("braced_expression" "call_arguments" "parameters"
      "parenthesized_expression" "if_statement" "for_statement"
      "while_statement" "repeat_statement" "function_definition")
     () #f)
    ("julia"
     ("function_definition" "macro_definition" "module_definition"
      "struct_definition" "if_statement" "for_statement" "while_statement"
      "let_statement" "try_statement" "do_clause" "compound_statement"
      "quote_statement" "else_clause" "elseif_clause" "catch_clause"
      "finally_clause" "vector_expression" "tuple_expression"
      "parenthesized_expression" "call_expression")
     ("else_clause" "elseif_clause" "catch_clause" "finally_clause") #f)
    ("shell"
     ("compound_statement" "do_group" "if_statement" "elif_clause"
      "else_clause" "case_statement" "case_item" "subshell" "array"
      "command_substitution")
     ("elif_clause" "else_clause") #f)
    ("go"
     ("block" "literal_value" "argument_list" "parameter_list"
      "expression_switch_statement" "type_switch_statement" "select_statement"
      "expression_case" "default_case" "type_case" "communication_case"
      "field_declaration_list" "interface_type" "import_spec_list"
      "const_declaration" "var_declaration" "parenthesized_expression")
     ;; gofmt writes case at the indentation of switch
     ("expression_case" "default_case" "type_case" "communication_case") #f)
    ("rust"
     ("block" "declaration_list" "field_declaration_list" "enum_variant_list"
      "match_block" "match_arm" "arguments" "parameters" "array_expression"
      "tuple_expression" "field_initializer_list" "use_list" "token_tree"
      "ordered_field_declaration_list" "where_clause")
     () #f)
    ("lua"
     ("function_declaration" "function_definition" "if_statement"
      "elseif_statement" "else_statement" "while_statement" "repeat_statement"
      "for_statement" "do_statement" "table_constructor" "arguments"
      "parameters" "parenthesized_expression")
     ("elseif_statement" "else_statement") #f)
    ("lean"
     ("def" "theorem" "instance" "abbrev" "example" "structure" "inductive"
      "by" "do_block" "match" "match_alt" "fun" "have" "let" "where_decls"
      "if_then_else" "tactic_case" "paren" "anon_ctor" "list_lit" "array_lit"
      "struct_lit")
     ()
     ("paren" "anon_ctor" "list_lit" "array_lit" "struct_lit"))))

(define (line-string doc row)
  (let ((t (tree-ref doc row)))
    (if (and t (tree-atomic? t)) (tree->string t) "")))

(define (line-indent doc row)
  (let* ((s (line-string doc row)) (n (string-length s)))
    (let loop ((i 0))
      (if (and (< i n) (== (string-ref s i) #\space)) (loop (+ i 1)) i))))

(define (first-non-space s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((= i n) #f)
            ((char-whitespace? (string-ref s i)) (loop (+ i 1)))
            (else i)))))

(define (previous-text-end doc row)
  ;; the point after the last character which is not a space, before row
  (let loop ((r (- row 1)))
    (if (< r 0) #f
        (let* ((s (line-string doc r))
               (n (let trim ((n (string-length s)))
                    (if (and (> n 0) (char-whitespace? (string-ref s (- n 1))))
                        (trim (- n 1))
                        n))))
          (if (> n 0) (list r n) (loop (- r 1)))))))

(define-public (syntax-indentation doc row lan rules tab)
  ;; the indentation of the line row of doc (a document of lines), by the
  ;; rules of its language (an entry of syntax-indent-rules)
  (let* ((indenting (cadr rules))
         (branches (caddr rules))
         (brackets (cadddr rules))
         (s (line-string doc row))
         (c0 (first-non-space s))
         (prev (and (not c0) (previous-text-end doc row)))
         (p (cond (c0 (list row c0))
                  (prev (list (car prev) (- (cadr prev) 1)))
                  (else #f))))
    (if (not p) 0
        (let* ((nodes (syntax-region-node-range doc lan (car p) (cadr p)
                                                (car p) (cadr p)))
               (errors (if prev (syntax-region-errors doc lan) '()))
               (closed? (lambda (n)
                          ;; complete before the (blank) line
                          (and prev
                               (point<=? (node-end n) prev)
                               (or (not brackets) (in? (node-type n) brackets))
                               (not (list-or
                                     (map (lambda (e)
                                            (and (point<=? (node-start n) (cadr e))
                                                 (point<=? (caddr e) (node-end n))))
                                          errors))))))
               ;; after an unfinished construct (an unclosed bracket),
               ;; error recovery leaves an ERROR node instead of its node
               (a (list-find nodes
                             (lambda (n)
                               (and (or (in? (node-type n) indenting)
                                        (and prev (== (node-type n) "ERROR")))
                                    (< (car (node-start n)) row)
                                    (not (closed? n)))))))
          (if (not a) 0
              (let* ((n0 (car nodes))
                     (parent (and (pair? (cdr nodes)) (cadr nodes)))
                     (closing? (and c0 (not (node-named? n0))
                                    (== (node-start n0) p)
                                    (== parent a)
                                    (== (node-end n0) (node-end a))))
                     (first (let loop ((l nodes) (best #f))
                              (if (and (pair? l) (== (node-start (car l)) p))
                                  (loop (cdr l) (car l))
                                  best)))
                     (branch? (and c0 first (in? (node-type first) branches))))
                (+ (line-indent doc (car (node-start a)))
                   (if (or closing? branch?) 0 tab))))))))

(define-public (syntax-language-indentation doc row lan id tab)
  ;; the same for the program language lan, whose language of tree-sitter
  ;; is id; #f without rules for lan
  (let ((rules (assoc lan syntax-indent-rules)))
    (and rules (tree-is? doc 'document)
         (syntax-indentation doc row id rules tab))))

(define-public (syntax-compute-indentation doc row tab)
  ;; the indentation of a line of the program at the cursor, or #f when
  ;; its language has no syntax tree or no rules
  (let* ((lan (get-env "prog-language"))
         (id (tree-sitter-program-language lan)))
    (and (>= id 0) (syntax-language-indentation doc row lan id tab))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outline: the definitions of a program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The definitions are the captures @definition.<kind> of the query
;; misc/tree-sitter/<language>/tags.scm, as in tree-sitter-tags, each with
;; the first capture @name inside it.

(define tags-queries '())  ; ((id . query or #f) ...)

;; the grammars whose name is not that of the program language
(define grammar-names '(("shell" . "bash")))

(define (tags-query lan id)
  ;; the query of the definitions of the program language lan, or #f
  (let ((known (assoc id tags-queries)))
    (if known (cdr known)
        (let* ((g (or (assoc-ref grammar-names lan) lan))
               (u (url-concretize
                   (string-append "$TEXMACS_PATH/misc/tree-sitter/" g "/tags.scm")))
               (q (and (url-exists? u)
                       (tree-sitter-query-new id (string-load u))))
               (r (and q (>= q 0) q)))
          (set! tags-queries (cons (cons id r) tags-queries))
          r))))

(define (region-text root start end)
  ;; the text between two points of the same line
  (let ((line (if (tree-is? root 'document) (tree-ref root (car start)) root)))
    (if (and line (tree-atomic? line) (= (car start) (car end)))
        (let ((s (tree->string line)))
          (substring s (min (cadr start) (string-length s))
                     (min (cadr end) (string-length s))))
        "")))

(define-public (syntax-outline root lan id)
  ;; the definitions of a region of the program language lan (language id
  ;; of tree-sitter): (kind name start end) in the order of the text, kind
  ;; being "function", "class"... and start the position of the name
  (let ((q (tags-query lan id)))
    (if (not q) '()
        (let* ((names (tree-sitter-query-capture-names q))
               (caps (syntax-region-captures root id q))
               (cap-name (lambda (c) (list-ref names (car c))))
               (defs (list-filter caps (lambda (c) (string-starts? (cap-name c)
                                                                   "definition."))))
               (nms (list-filter caps (lambda (c) (== (cap-name c) "name")))))
          (let loop ((l defs) (acc '()))
            (if (null? l) (reverse acc)
                (let* ((d (car l))
                       (n (list-find nms (lambda (n) (and (point<=? (cadr d) (cadr n))
                                                          (point<=? (caddr n) (caddr d))))))
                       (item (and n (list (string-drop (cap-name d) 11)
                                          (region-text root (cadr n) (caddr n))
                                          (cadr n) (caddr d)))))
                  (loop (cdr l)
                        (if (and item (not (list-find acc (lambda (x)
                                                            (== (caddr x) (caddr item))))))
                            (cons item acc)
                            acc)))))))))

(define (cursor-outline)
  ;; (ctx id definitions) for the code at the cursor, or #f
  (let* ((lan (get-env "prog-language"))
         (id (syntax-cursor-language))
         (ctx (and id (path-context (cursor-path) id))))
    (and ctx (list ctx id (syntax-outline (cadr ctx) lan id)))))

(define-public (syntax-go-to-point ctx point)
  (go-to (context-path ctx point)))

(define-public (syntax-definitions-at-cursor)
  ;; the definitions of the code at the cursor: (kind name path)
  (let ((o (cursor-outline)))
    (if (not o) '()
        (map (lambda (d) (list (car d) (cadr d) (context-path (car o) (caddr d))))
             (caddr o)))))

(define-public (syntax-go-to-definition)
  ;; go to the definition of the name at the cursor in the same code; #f
  ;; when there is none
  (let ((o (cursor-outline)))
    (and o
         (let* ((ctx (car o))
                (l (syntax-region-node-at (cadr ctx) (cadr o) (caddr ctx) (cadddr ctx)))
                (n (and (pair? l) (node-named? (car l)) (car l)))
                (name (and n (region-text (cadr ctx) (node-start n) (node-end n))))
                (d (and name (list-find (caddr o) (lambda (d) (== (cadr d) name))))))
           (and d (begin (go-to (context-path ctx (caddr d))) #t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define line-comments
  '(("c" . "//") ("cpp" . "//") ("java" . "//") ("javascript" . "//")
    ("python" . "#") ("r" . "#") ("julia" . "#") ("scheme" . ";")
    ("shell" . "#") ("go" . "//") ("rust" . "//") ("lua" . "--")
    ("lean" . "--")))

(define-public (syntax-comment-prefix lan)
  (assoc-ref line-comments lan))

(define-public (toggle-line-comments lines prefix)
  ;; comment the strings lines with prefix, or uncomment them when they all
  ;; are (blank lines aside); the new lines
  (let* ((full (list-filter lines (lambda (s) (first-non-space s))))
         (commented? (lambda (s) (let ((i (first-non-space s)))
                                   (string-starts? (substring s i (string-length s))
                                                   prefix))))
         (all? (and (nnull? full) (list-and (map commented? full))))
         (indent (if (null? full) 0 (apply min (map first-non-space full))))
         (k (string-length prefix)))
    (map (lambda (s)
           (let ((i (first-non-space s)))
             (cond ((not i) s)
                   (all?
                    (let* ((j (+ i k))
                           (j* (if (and (< j (string-length s))
                                        (== (string-ref s j) #\space))
                                   (+ j 1) j)))
                      (string-append (substring s 0 i)
                                     (substring s j* (string-length s)))))
                   (else
                    (string-append (substring s 0 indent) prefix " "
                                   (substring s indent (string-length s)))))))
         lines)))

(define-public (syntax-toggle-comment)
  ;; comment or uncomment the lines of the selection, or the line of the
  ;; cursor, in program code; #f when its language has no line comments
  (let* ((prefix (syntax-comment-prefix (get-env "prog-language")))
         (sel? (selection-active-normal?))
         (a (and prefix (path-context (if sel? (selection-get-start) (cursor-path)) 0)))
         (b (and a (path-context (if sel? (selection-get-end) (cursor-path)) 0))))
    (and a b (== (car a) (car b)) (tree-is? (cadr a) 'document)
         (let* ((root (cadr a))
                (rows (map (lambda (i) (+ i (caddr a)))
                           (iota (+ (- (caddr b) (caddr a)) 1))))
                (lines (map (lambda (r) (tree->string (tree-ref root r))) rows))
                (new (toggle-line-comments lines prefix)))
           (for-each (lambda (r s) (when (!= s (tree->string (tree-ref root r)))
                                     (tree-set (tree-ref root r) s)))
                     rows new)
           #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (syntax-definitions-menu)
  (for (d (syntax-definitions-at-cursor))
    ((eval (string-append (cadr d) " (" (car d) ")"))
     (go-to (caddr d)))))

(tm-menu (focus-extra-menu t)
  (:require (and (in-prog?) (syntax-cursor-language)))
  (-> "Definitions" (dynamic (syntax-definitions-menu)))
  ("Go to definition" (syntax-go-to-definition))
  (assuming (syntax-comment-prefix (get-env "prog-language"))
    ("Toggle comment" (syntax-toggle-comment))))
