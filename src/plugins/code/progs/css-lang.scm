;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : css-lang.scm
;; DESCRIPTION : the CSS language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords for the highlighting by the tokenizer of TeXmacs, until the
;; tables of tree-sitter are ready (progs/treesit); this file also makes
;; CSS a program language of TeXmacs (prog_lang_exists).

(texmacs-module (css-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "css") (== key "keyword")))
  `(,(string->symbol key)
    (constant "important" "inherit" "initial" "unset" "none" "auto")
    (keyword "@media" "@import" "@font-face" "@keyframes" "@supports")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "css") (== key "operator")))
  `(,(string->symbol key)
    (operator
      ":" ";" "," ">" "+" "~" "*" "=")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "css") (== key "string")))
  `(,(string->symbol key)
    (escape_sequences "\\" "\"" "'" "n" "t")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "css") (== key "comment")))
  `(,(string->symbol key)
    (multi_line "/*" "*/")))
