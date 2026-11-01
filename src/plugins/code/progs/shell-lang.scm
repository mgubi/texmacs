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
;; bash); without it, shell is not colored and its lines break as before (a
;; tokenizer for the shell would take $# and ${#a} for comments).

(texmacs-module (shell-lang)
  (:use (prog default-lang)))
