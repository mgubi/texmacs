;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : rust-lang.scm
;; DESCRIPTION : the Rust language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords for the highlighting by the tokenizer of TeXmacs, until the
;; tables of tree-sitter are ready (progs/treesit); this file also makes
;; Rust a program language of TeXmacs (prog_lang_exists).

(texmacs-module (rust-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "rust") (== key "keyword")))
  `(,(string->symbol key)
    (constant "true" "false" "Some" "None" "Ok" "Err" "self" "Self")
    (declare_function "fn")
    (declare_module "mod" "use" "crate" "extern" "pub" "super")
    (declare_type "struct" "enum" "trait" "impl" "type" "union" "dyn")
    (keyword "let" "mut" "const" "static" "ref" "move" "unsafe" "async"
      "await" "where" "as" "in")
    (keyword_conditional "if" "else" "match" "loop" "while" "for" "break"
      "continue")
    (keyword_control "return")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "rust") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "*" "/" "%" "^" "!" "&" "|" "&&" "||" "<<" ">>" "+=" "-=" "*=" "/=" "%=" "^=" "&=" "|=" "<<=" ">>=" "=" "==" "!=" ">" "<" ">=" "<=" "=>" "->" "?" ";" "," ":")
    (operator_field "." "::")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "rust") (== key "number")))
  `(,(string->symbol key)
    (bool_features "prefix_0x" "prefix_0b" "prefix_0o" "sci_notation")
    (separator "_")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "rust") (== key "string")))
  `(,(string->symbol key)
    (bool_features "hex_with_8_bits")
    (escape_sequences "\\" "\"" "'" "n" "r" "t" "0")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "rust") (== key "comment")))
  `(,(string->symbol key)
    (inline "//")
    (multi_line "/*" "*/")))
