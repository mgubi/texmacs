;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : haskell-lang.scm
;; DESCRIPTION : the Haskell language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords for the highlighting by the tokenizer of TeXmacs, until the
;; tables of tree-sitter are ready (progs/treesit); this file also makes
;; Haskell a program language of TeXmacs (prog_lang_exists).

(texmacs-module (haskell-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "haskell") (== key "keyword")))
  `(,(string->symbol key)
    (constant "True" "False" "Nothing" "Just")
    (declare_module "module" "import" "qualified" "as" "hiding")
    (declare_type "data" "type" "newtype" "class" "instance" "deriving"
      "family")
    (keyword "where" "let" "in" "do" "forall" "infix" "infixl" "infixr"
      "foreign")
    (keyword_conditional "if" "then" "else" "case" "of")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "haskell") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "=" "->" "<-" "=>" "::" "|" "\\" "." "$" "<$>" "<*>" ">>=")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "haskell") (== key "string")))
  `(,(string->symbol key)
    (escape_sequences "\\" "\"" "'" "n" "t")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "haskell") (== key "comment")))
  `(,(string->symbol key)
    (inline "--")))
