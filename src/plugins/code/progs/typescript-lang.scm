;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : typescript-lang.scm
;; DESCRIPTION : the TypeScript language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords for the highlighting by the tokenizer of TeXmacs, until the
;; tables of tree-sitter are ready (progs/treesit); this file also makes
;; TypeScript a program language of TeXmacs (prog_lang_exists).

(texmacs-module (typescript-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "typescript") (== key "keyword")))
  `(,(string->symbol key)
    (constant "true" "false" "null" "undefined" "this")
    (declare_function "function")
    (declare_module "import" "export" "from" "namespace" "module" "declare")
    (declare_type "class" "interface" "type" "enum" "extends" "implements")
    (keyword "const" "let" "var" "new" "async" "await" "yield" "public"
      "private" "protected" "readonly" "static" "abstract" "as" "keyof"
      "typeof" "instanceof" "in" "of")
    (keyword_conditional "if" "else" "switch" "case" "default" "for" "while"
      "do" "break" "continue")
    (keyword_control "return" "throw" "try" "catch" "finally")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "typescript") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "*" "/" "%" "=" "==" "===" "!=" "!==" "<" ">" "<=" ">=" "&&" "||" "!" "?" ":" ";" "," "=>" "...")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "typescript") (== key "string")))
  `(,(string->symbol key)
    (escape_sequences "\\" "\"" "'" "n" "t")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "typescript") (== key "comment")))
  `(,(string->symbol key)
    (inline "//")
    (multi_line "/*" "*/")))
