
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : part-menu.scm
;; DESCRIPTION : menus for multi-file documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (part part-menu)
  (:use (generic document-part)
	(part part-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (goto-include buf incl)
  (load-document (part-url buf (url-relative buf incl))))

(tm-define (part-menu-entries m)
  ;; the (label file) of the master @m and its included files in the menu
  ;; Document > Part; a star marks the files with unsaved changes
  (let* ((t (tree-import m "texmacs"))
         (b (tmfile-get t 'body))
         (l (tm-get-includes b))
         (label (lambda (s u)
                  (if (part-file-unsaved? m u) (string-append s " *") s))))
    (cons (list (label (url->string (url-tail m)) m) #f)
          (map (lambda (incl)
                 (list (label incl (url-relative m incl)) incl))
               l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The dynamic document part menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-master-menu
  (link preamble-menu)
  ---
  (for (incl (buffer-get-includes))
    ((eval incl) (goto-include (current-buffer) incl))))

(menu-bind document-part-menu
  (:require (url-rooted-tmfs-protocol? (current-buffer) "part"))
  (let* ((name (part-open-name (current-buffer)))
         (m (part-master name))
         (l (part-menu-entries m)))
    ((eval (car (car l))) (load-document m))
    ---
    (for (e (cdr l))
      ((eval (car e)) (goto-include m (cadr e))))))
