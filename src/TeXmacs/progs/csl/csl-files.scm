
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-files.scm
;; DESCRIPTION : the files of the CSL styles and locales
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Where the styles are and which ones are installed. This module is small
;; on purpose: the menus and the bibliography dialog list the styles with
;; it, without loading the processor.

(texmacs-module (csl csl-files))

(tm-define csl-extra-path '())

(tm-define (csl-directories sub)
  (:synopsis "The directories where the files of the kind @sub are sought")
  (append (map (lambda (d) (url-append d sub)) csl-extra-path)
          (list (url-append "$TEXMACS_HOME_PATH/csl" sub)
                (url-append "$TEXMACS_PATH/misc/csl" sub))))

(tm-define (csl-style-name? style)
  (:synopsis "Is @style the name of a CSL style, csl-NAME?")
  (and (string? style) (string-starts? style "csl-")
       (> (string-length style) 4)))

(tm-define (csl-available-styles)
  (:synopsis "The names csl-NAME of the CSL styles which are installed")
  (let* ((files (append-map
                 (lambda (d)
                   (if (url-exists? d) (url-read-directory d "*.csl") '()))
                 (csl-directories "styles")))
         (names (map (lambda (u) (url->string (url-basename u))) files)))
    (map (cut string-append "csl-" <>)
         (sort (list-remove-duplicates names) string<?))))
