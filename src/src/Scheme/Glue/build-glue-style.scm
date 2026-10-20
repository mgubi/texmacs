
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : build-glue-style.scm
;; DESCRIPTION : Building the glue of the experimental style rewriter (Style)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (compiled only with --enable-experimental, see src/docs/pure-style.md
;; and glue.cpp)

(output-copyright "build-glue-style.scm")

(build
  "get_current_editor()->"
  "initialize_glue_style"

  (style-rewrite-buffer style_rewrite (tree))
  (style-rewrite-copy style_rewrite_copy (tree))
  (style-rewrite-statistics style_rewrite_statistics (array_int))
  (style-environment-update environment_update (void)))
