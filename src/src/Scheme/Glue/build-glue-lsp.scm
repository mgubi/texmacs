
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : build-glue-lsp.scm
;; DESCRIPTION : Building the glue of the language servers (lsp_link)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (compiled only when the feature is part of the build, see
;; src/docs/code-features.md and glue.cpp)

(output-copyright "build-glue-lsp.scm")

(build
  ""
  "initialize_glue_lsp"

  ;; language servers (Language Server Protocol, progs/lsp)
  (lsp-start lsp_start (object string object))
  (lsp-send lsp_send (bool int string))
  (lsp-listen lsp_listen (void int int))
  (lsp-alive? lsp_alive (bool int))
  (lsp-stop lsp_stop (void int))
  (lsp-document-text lsp_document_text (string tree)))
