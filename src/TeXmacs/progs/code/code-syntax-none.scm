
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-syntax-none.scm
;; DESCRIPTION : program code without syntax trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In a build without tree-sitter (src/docs/code-features.md), the functions of
;; its syntax trees which the editing of programs calls (prog-edit.scm, the
;; indentation of some languages, the inputs of sessions) answer that the
;; code has no syntax tree: the editing of programs is then that of
;; TeXmacs without them. Loaded instead of progs/treesit/ts-edit.scm.

(texmacs-module (code code-syntax-none))

(define-public (syntax-cursor-language) #f)
(define-public (syntax-compute-indentation . args) #f)
(define-public (syntax-input-incomplete? . args) #f)
(define-public (syntax-bracket-open . args) #f)
(define-public (syntax-bracket-close . args) #f)
(define-public (syntax-close-block . args) #f)
(define-public (syntax-close-tag . args) #f)
(define-public (syntax-delete-bracket . args) #f)
(define-public (syntax-delete-node . args) #f)
(define-public (syntax-move-definition . args) #f)
(define-public (syntax-move-node . args) #f)
(define-public (syntax-select-brackets . args) #f)
(define-public (syntax-select-enlarge . args) #f)
(define-public (syntax-splice-brackets . args) #f)
