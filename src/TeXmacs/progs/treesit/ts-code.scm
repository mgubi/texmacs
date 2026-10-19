
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-code.scm
;; DESCRIPTION : tree-sitter as a provider of the features of program code
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The adapter of tree-sitter for the features of program code
;; (progs/code/code-features.scm, src/docs/code-features.md): loaded by them when
;; it is there. Its rank, 20, comes after the servers (10) for the features
;; which both give (a server sees the other files); it serves where the code
;; has a syntax tree (syntax-cursor-language), also in code blocks.

(texmacs-module (treesit ts-code)
  (:use (code code-features)
        (treesit ts-edit)))

(code-provider-set! 'fold-ranges 'syntax 20
                    (lambda () (and (syntax-cursor-language) #t))
                    syntax-fold-ranges)

(code-provider-set! 'fold-all-ranges 'syntax 20
                    (lambda () (and (syntax-cursor-language) #t))
                    syntax-fold-all-ranges)

(code-provider-set! 'outline 'syntax 20
                    (lambda () (and (syntax-cursor-language) #t))
                    syntax-outline-at-cursor)

(code-provider-set! 'definition 'syntax 20
                    (lambda () (and (syntax-cursor-language) #t))
                    syntax-go-to-definition)

(code-provider-set! 'syntax-errors 'syntax 20
                    (lambda () (and (syntax-cursor-language) #t))
                    syntax-errors-at-cursor)

(code-provider-set! 'completion-context 'syntax 20
                    (lambda () (and (syntax-cursor-language) #t))
                    syntax-context-at-cursor)

(code-provider-set! 'local-names 'syntax 20
                    (lambda () (and (syntax-cursor-language) #t))
                    syntax-local-names)
