;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ocaml-lang.scm
;; DESCRIPTION : the OCaml language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords for the highlighting by the tokenizer of TeXmacs, until the
;; tables of tree-sitter are ready (progs/treesit); this file also makes
;; OCaml a program language of TeXmacs (prog_lang_exists).

(texmacs-module (ocaml-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "ocaml") (== key "keyword")))
  `(,(string->symbol key)
    (constant "true" "false")
    (declare_function "fun" "function" "let" "rec" "and" "in")
    (declare_module "module" "open" "include" "struct" "sig" "functor")
    (declare_type "type" "of" "class" "object" "method" "inherit" "val"
      "mutable" "private" "virtual" "constraint" "exception")
    (keyword "end" "as" "external" "lazy" "assert" "new" "initializer")
    (keyword_conditional "if" "then" "else" "match" "with" "when" "for" "to"
      "downto" "while" "do" "done" "begin")
    (keyword_control "raise" "try")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "ocaml") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "*" "/" "=" "<>" "==" "!=" "<" ">" "<=" ">=" "&&" "||" "::" ":=" "->" "<-" "|" ";" "," "@" "^" "!")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "ocaml") (== key "string")))
  `(,(string->symbol key)
    (escape_sequences "\\" "\"" "'" "n" "t")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "ocaml") (== key "comment")))
  `(,(string->symbol key)
    (multi_line "(*" "*)")))
