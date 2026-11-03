
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-tikz.scm
;; DESCRIPTION : Initialize TikZ plugin
;; COPYRIGHT   : (C) 2021 Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tikz-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (tikz-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_HOME_PATH")
                     "/plugins/tmpy/session/tm_tikz.py\"")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_PATH")
                     "/plugins/tmpy/session/tm_tikz.py\"")))

;; In a browser, the plugin is a Web Worker which runs TikZJax, TeX in
;; WebAssembly (web/tm-tikz.js, copied to tikzjax/ next to the page; see
;; src/docs/wasm/tikzjax.md); elsewhere a Python program which runs latex
(define (tikz-in-browser?)
  (defined? 'web-files))

(define (tikz-engine)
  (if (tikz-in-browser?)
      `((:worker "tikzjax/tm-tikz.js"))
      `((:launch ,(tikz-launcher)))))

(plugin-configure tikz
  ;; (the test of tikz-in-browser? written out: the requirements are also
  ;; evaluated outside of this file, e.g. by the plugins suite)
  (:require (or (defined? 'web-files)
                (and (python-command) (!= (python-command) "")
                     (url-exists-in-path? "latex"))))
  ,@(tikz-engine)
  (:serializer ,tikz-serialize)
  (:session "TikZ")
  (:scripts "TikZ"))

;; the text of the pictures made in a browser: editable, and put back into
;; the source of an executable fold (tikz-edit.scm)
(when (and (tikz-in-browser?) (supports-tikz?))
  (import-from (tikz-edit)))

