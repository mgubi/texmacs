;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : go-lang.scm
;; DESCRIPTION : the Go language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords for the highlighting by the tokenizer of TeXmacs, until the
;; tables of tree-sitter are ready (progs/treesit); this file also makes
;; Go a program language of TeXmacs (prog_lang_exists).

(texmacs-module (go-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "go") (== key "keyword")))
  `(,(string->symbol key)
    (constant "true" "false" "nil" "iota")
    (declare_function "func")
    (declare_module "package" "import")
    (declare_type "type" "struct" "interface" "map" "chan")
    (keyword "var" "const" "go" "defer" "select" "range")
    (keyword_conditional "if" "else" "switch" "case" "default" "for"
      "break" "continue" "fallthrough" "goto")
    (keyword_control "return")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "go") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "*" "/" "%" "&" "|" "^" "<<" ">>" "&^" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" "&^=" "&&" "||" "<-" "++" "--" "==" "<" ">" "=" "!" "!=" "<=" ">=" ":=" "..." ";" "," ":")
    (operator_field ".")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "go") (== key "number")))
  `(,(string->symbol key)
    (bool_features "prefix_0x" "prefix_0b" "prefix_0o" "sci_notation")
    (separator "_")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "go") (== key "string")))
  `(,(string->symbol key)
    (bool_features "hex_with_8_bits")
    (escape_sequences "\\" "\"" "'" "a" "b" "f" "n" "r" "t" "v")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "go") (== key "comment")))
  `(,(string->symbol key)
    (inline "//")
    (multi_line "/*" "*/")))
