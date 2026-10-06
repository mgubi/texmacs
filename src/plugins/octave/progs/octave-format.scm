
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : octave-format.scm
;; DESCRIPTION : Octave file format
;; COPYRIGHT   : (C) 2026  The TeXmacs team
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (octave-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Octave source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format octave
  (:name "Octave source code")
  (:suffix "m"))

(define (texmacs->octave x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (octave->texmacs x . opts)
  (code->texmacs x))

(define (octave-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree octave-document
  (:function texmacs->octave))

(converter octave-document texmacs-tree
  (:function octave->texmacs))
  
(converter texmacs-tree octave-snippet
  (:function texmacs->octave))

(converter octave-snippet texmacs-tree
  (:function octave-snippet->texmacs))
