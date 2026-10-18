
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathemagix-edit.scm
;; DESCRIPTION : editing mathemagix programs
;; COPYRIGHT   : (C) 2008  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (mathemagix-edit)
  (:use (mmx-indent)))

(tm-define (kbd-variant t forward?)
  ;; (not in the field of a search: tab inserts a tab there)
  (:require (and (in-prog-mathemagix?) (not (inside? 'session))
                 (not (and (defined? 'inside-search-or-replace-buffer?)
                           (inside-search-or-replace-buffer?)))))
  (mmx-indent))

(tm-define (insert-return)
  (:mode in-prog-mathemagix?)
  (insert-raw-return)
  (mmx-indent))
