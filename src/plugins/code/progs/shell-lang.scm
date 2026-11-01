;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : shell-lang.scm
;; DESCRIPTION : the shell language (sh, bash, zsh)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file makes shell a program language of TeXmacs (prog_lang_exists), so
;; that a .sh file is opened as code (#734). It has no tables of the
;; tokenizer on purpose: the shell is highlighted by tree-sitter (grammar
;; bash), and a tokenizer would take $# and ${#a} for comments. Without
;; tree-sitter (switched off, or its tables not ready yet) the tokenizer of
;; the program languages applies with its defaults, not as for verbatim text:
;; numbers are colored (and tokens split at them: $1, 3;), strings in quotes
;; are colored (an unbalanced ' to the end of the line), and there are no
;; comments.

(texmacs-module (shell-lang)
  (:use (prog default-lang)))

;; (no comments: the default ones of default-lang.scm are // and /* */, which
;; in the shell are in the paths of files: curl https://x, ls /tmp/*.txt; the
;; # of the shell is also in $# and ${#a}, which a tokenizer cannot tell)
(tm-define (parser-feature lan key)
  (:require (and (== lan "shell") (== key "comment")))
  `(,(string->symbol key)))
