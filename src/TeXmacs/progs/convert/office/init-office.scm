
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-office.scm
;; DESCRIPTION : setup the converters of the office formats
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert office init-office))

;; The documents of Word (.docx) and the texts of OpenDocument (.odt) are
;; zip archives: the "document" of these formats is the archive, a string
;; of bytes. Both are read into the same office tree (office-tools.scm).

(define-format docx
  (:name "Word")
  (:suffix "docx"))

(define-format odt
  (:name "OpenDocument")
  (:suffix "odt"))

(lazy-define (convert office docxin) parse-docx-document)
(lazy-define (convert office odtin) parse-odt-document)
(lazy-define (convert office officetm) office->texmacs)

(converter docx-document office-stree
  (:function parse-docx-document))

(converter odt-document office-stree
  (:function parse-odt-document))

(converter office-stree texmacs-stree
  (:function office->texmacs))

(lazy-define (convert office tmoffice) texmacs->office)

(converter texmacs-stree office-stree
  (:function-with-options texmacs->office))

(lazy-define (convert office docxout) serialize-docx-document)

(converter office-stree docx-document
  (:function serialize-docx-document))

(lazy-define (convert office odtout) serialize-odt-document)

(converter office-stree odt-document
  (:function serialize-odt-document))
