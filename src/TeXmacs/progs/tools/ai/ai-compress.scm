
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-compress.scm
;; DESCRIPTION : the tables of the compressed markup, for AI and LanguageTool
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compress-html and compress-tree replace the markup which is not text by
;; codes, kept in a table until the answer which uses them is decompressed
;; (see compress.cpp).  A request makes a table of its own with
;; compress-table-new, and frees it once its answer has been decompressed:
;; compress-table-callback, around the function which gets the answer.

(texmacs-module (tools ai ai-compress))

(tm-define (with-compress-table name thunk)
  (:synopsis "Call @thunk with the compression codes of the table @name")
  (let ((prev ""))
    (dynamic-wind
      (lambda () (set! prev (compress-table-select name)))
      thunk
      (lambda () (compress-table-select prev)))))

(tm-define (compress-table-callback name fun)
  (:synopsis "@fun, called with the table @name, which is freed after")
  (lambda args
    (dynamic-wind
      (lambda () (noop))
      (lambda () (with-compress-table name (lambda () (apply fun args))))
      (lambda () (compress-table-free name)))))
