
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-code.scm
;; DESCRIPTION : language servers as providers of the features of program code
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The adapter of the language servers for the features of program code
;; (progs/code/code-features.scm, doc/code-features.md): loaded by them when
;; it is there. Its rank, 10, comes first for the features which tree-sitter
;; gives too (a server sees the other files of a project); it serves in the
;; source files which a server follows.

(texmacs-module (lsp lsp-code)
  (:use (code code-features)
        (lsp lsp-fold)
        (lsp lsp-edit)
        (lsp lsp-navigate)))

(code-provider-set! 'fold-ranges 'server 10 lsp-can-fold? lsp-fold-ranges)
(code-provider-set! 'fold-all-ranges 'server 10 lsp-can-fold? lsp-all-fold-ranges)
(code-provider-set! 'outline 'server 10 lsp-can-outline? lsp-outline-at-cursor)
(code-provider-set! 'definition 'server 10 lsp-can-find-definitions?
                    lsp-definition-at-cursor)
