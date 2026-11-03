;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lean-lang.scm
;; DESCRIPTION : the Lean language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords for the highlighting by the tokenizer of TeXmacs, until the
;; tables of tree-sitter are ready (progs/treesit); this file also makes
;; Lean a program language of TeXmacs (prog_lang_exists).

(texmacs-module (lean-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lean") (== key "keyword")))
  `(,(string->symbol key)
    (constant "true" "false" "True" "False" "Type" "Prop" "Sort")
    (declare_function "def" "theorem" "lemma" "example" "abbrev" "instance"
      "axiom" "opaque" "fun")
    (declare_module "import" "open" "namespace" "section" "end" "export"
      "variable" "universe")
    (declare_type "structure" "class" "inductive" "where" "deriving")
    (keyword "let" "have" "show" "from" "by" "do" "at" "with" "calc"
      "noncomputable" "private" "protected" "partial" "mutual" "unsafe")
    (keyword_conditional "if" "then" "else" "match" "for" "in" "unless")
    (keyword_control "return")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lean") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "*" "/" "%" "^" "!" "&&" "||" "=" "==" "!=" "<" ">" "<=" ">=" ":=" "->" "<-" "=>" "|" "$" "<|" "|>" ";" "," ":")
    (operator_field ".")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lean") (== key "number")))
  `(,(string->symbol key)
    (bool_features "prefix_0x" "prefix_0b" "sci_notation")
    (separator "_")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lean") (== key "string")))
  `(,(string->symbol key)
    (bool_features "hex_with_8_bits")
    (escape_sequences "\\" "\"" "'" "n" "t")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lean") (== key "comment")))
  `(,(string->symbol key)
    (inline "--")
    (multi_line "/-" "-/")))
