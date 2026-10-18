
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-menu.scm
;; DESCRIPTION : the menu of the features of program code
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The entries of the menu Focus in program code: those of the features
;; which a provider can serve at the cursor (code-features.scm), whichever
;; it is, then those of a server only (code-server-menu, which the adapter
;; of the servers gives: progs/lsp/lsp-code.scm), then those of the syntax
;; tree (code-syntax-menu, which tree-sitter gives: progs/treesit/ts-edit.scm).
;; Without one of them, its entries are not there.

(texmacs-module (code code-menu)
  (:use (code code-features)
        (code code-fold)
        (code code-navigate)
        (code code-diagnostics)
        (code code-complete)))

(tm-menu (code-server-menu))
(tm-menu (code-syntax-menu))

(tm-menu (code-diagnostics-menu)
  (when (nnull? (code-diagnostics))
    ("Next diagnostic" (code-next-diagnostic #t))
    ("Previous diagnostic" (code-next-diagnostic #f))))

(tm-menu (focus-extra-menu t)
  (:require (and (in-prog?) (code-cursor-lines)))
  (when (code-available? 'outline)
    (-> "Definitions" (dynamic (code-definitions-menu)))
    ("Outline" (code-outline)))
  (when (code-available? 'definition)
    ("Go to definition" (code-go-to-definition)))
  (when (code-can-fold?)
    (-> "Folds"
        ("Fold block" (code-fold))
        ("Unfold block" (code-unfold))
        ("Fold all blocks" (code-fold-all))
        ("Unfold all" (code-unfold-all))))
  (dynamic (code-diagnostics-menu))
  (dynamic (code-server-menu))
  (dynamic (code-syntax-menu))
  ;; (the entries of a session or a program input, whose code this is)
  (dynamic (former t)))
