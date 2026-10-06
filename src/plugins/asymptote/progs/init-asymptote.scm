
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-asymptote.scm
;; DESCRIPTION : Initialize Asymptote plugin
;; COPYRIGHT   : (C) Yann Dirson <ydirson at altern dot org>.
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (asy-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (asy-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_HOME_PATH")
                     "/plugins/tmpy/session/tm_asy.py\"")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_PATH")
                     "/plugins/tmpy/session/tm_asy.py\"")))

;; In a browser, the plugin is a Web Worker which runs Asymptote compiled to
;; WebAssembly (web/tm-asy.mjs, copied to asymptote/ next to the page; see
;; docs/wasm/asymptote.md); elsewhere a Python program which runs asy
(define (asymptote-in-browser?)
  (defined? 'web-files))

(define (asymptote-engine)
  (if (asymptote-in-browser?)
      `((:worker "asymptote/tm-asy.mjs"))
      `((:winpath "Asymptote" ".")
        (:launch ,(asy-launcher)))))

(plugin-configure asymptote
  (:require (or (asymptote-in-browser?)
                (and (url-exists-in-path? "asy") (!= (python-command) ""))))
  ,@(asymptote-engine)
  (:serializer ,asy-serialize)
  (:session "Asymptote")
  (:scripts "Asymptote"))

(when (supports-asymptote?)
  (import-from (asymptote-menus))
  (import-from (utils plugins plugin-convert)))

;; the labels of the pictures made in a browser: set by TeXmacs from their
;; LaTeX, editable, and put back into the source of an executable fold
;; (asymptote-edit.scm)
(when (and (asymptote-in-browser?) (supports-asymptote?))
  (import-from (asymptote-edit)))
