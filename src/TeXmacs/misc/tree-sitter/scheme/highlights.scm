; Highlighting of Scheme in TeXmacs, after queries/highlights.scm of
; tree-sitter-scheme (MIT, see LICENSE), with the forms of TeXmacs.
; As in tree-sitter-highlight, the last pattern which captures a node wins.
; In the strings of TeXmacs, < and > are the entities <less> and <gtr>.

(symbol) @variable

["(" ")" "[" "]"] @punctuation.bracket
(dot) @punctuation.delimiter

(number) @number
(character) @character
(boolean) @boolean
(keyword) @constant

(list
  .
  (symbol) @function)

((symbol) @operator
 (#any-of? @operator "+" "-" "*" "/" "=" "<less>" "<gtr>" "<less>=" "<gtr>="
   "<" ">" "<=" ">="))

(list
  .
  (symbol) @function.builtin
  (#any-of? @function.builtin
   "car" "cdr" "cons" "list" "append" "reverse" "length" "map" "for-each"
   "apply" "assoc" "assq" "member" "memq" "not" "null?" "pair?" "list?"
   "eq?" "eqv?" "equal?" "==" "!=" "string-append" "substring"
   "string-length" "string->symbol" "symbol->string" "number->string"
   "string->number" "display" "display*" "newline" "write" "error" "values"
   "vector" "vector-ref" "vector-set!" "caar" "cadr" "cdar" "cddr" "caddr"
   "cadddr" "list-ref" "list-tail" "string-ref" "string?" "symbol?"
   "number?" "integer?" "procedure?" "boolean?" "char?" "vector?"
   "tree-ref" "tree->stree" "stree->tree" "tree-label" "tree-children"
   "tree-arity" "tree-atomic?" "tree-compound?" "tree-is?" "url-concretize"
   "string-load" "string-save" "get-preference" "set-preference"))

(list
  .
  (symbol) @keyword
  (#any-of? @keyword
   "define" "define-syntax" "define-macro" "define-record-type" "lambda" "λ"
   "let" "let*" "letrec" "letrec*" "let-values" "let*-values"
   "let-syntax" "letrec-syntax" "syntax-rules" "named-lambda" "case-lambda"
   "if" "cond" "case" "else" "=>" "and" "or" "when" "unless" "do" "begin"
   "set!" "quote" "quasiquote" "unquote" "unquote-splicing" "delay" "force"
   "guard" "catch" "dynamic-wind" "call/cc" "call-with-current-continuation"
   "receive" "with" "for" "while" "repeat" "and-with" "with-global"
   "tm-define" "tm-define-macro" "tm-property" "tm-menu" "tm-widget"
   "tm-build" "tm-build-macro" "define-public" "define-public-macro"
   "texmacs-module" "use-modules" "inherit-modules" "lazy-define"
   "lazy-menu" "lazy-keyboard" "lazy-format" "lazy-define-force"
   "define-preferences" "define-group" "define-format" "define-language"
   "menu-bind" "kbd-map" "delayed" "assuming" "former"))

; 'datum, '(datum ...), two levels deep
(quote
  _ @constant)

(quote
  (_ _* @constant))

(quote
  (_ (_ _* @constant)))

(quasiquote
  _ @constant)

(string) @string

(escape_sequence) @string.escape

[
  (comment)
  (block_comment)
  (sexp_comment)
  (directive)
] @comment
