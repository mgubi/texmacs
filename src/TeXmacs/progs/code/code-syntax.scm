
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-syntax.scm
;; DESCRIPTION : the editing of program code by its syntax tree, if any
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The functions of the editing of program code by its syntax tree
;; (syntax-cursor-language, syntax-input-incomplete?...): those of
;; tree-sitter when it is part of the build (its glue), else those of
;; code-syntax-none.scm, which answer that the code has no syntax tree. The
;; modules which call them use this one (never treesit/ts-edit.scm itself:
;; its functions, which are public, would replace the others). It inherits
;; the module it chooses: with Guile, where the public names of a module are
;; seen only by the modules which use it, the users of this one see them.

(texmacs-module (code code-syntax))

(if (defined? 'tree-sitter-program-language)
    (inherit-modules (treesit ts-edit))
    (inherit-modules (code code-syntax-none)))
