;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lua-lang.scm
;; DESCRIPTION : the Lua language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords for the highlighting by the tokenizer of TeXmacs, until the
;; tables of tree-sitter are ready (progs/treesit); this file also makes
;; Lua a program language of TeXmacs (prog_lang_exists).

(texmacs-module (lua-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lua") (== key "keyword")))
  `(,(string->symbol key)
    (constant "true" "false" "nil")
    (declare_function "function")
    (keyword "local" "and" "or" "not" "in" "end")
    (keyword_conditional "if" "then" "else" "elseif" "while" "do" "for"
      "repeat" "until" "break" "goto")
    (keyword_control "return")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lua") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "*" "/" "//" "%" "^" "#" "&" "~" "|" "<<" ">>" "==" "~=" "<=" ">=" "<" ">" "=" ".." "..." ";" "," ":")
    (operator_field "." ":")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lua") (== key "number")))
  `(,(string->symbol key)
    (bool_features "prefix_0x" "sci_notation")
    (separator "_")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lua") (== key "string")))
  `(,(string->symbol key)
    (bool_features "hex_with_8_bits")
    (escape_sequences "\\" "\"" "'" "a" "b" "f" "n" "r" "t" "v")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "lua") (== key "comment")))
  `(,(string->symbol key)
    (inline "--")
    (multi_line "--[[" "]]")))
