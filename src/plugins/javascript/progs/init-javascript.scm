
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-javascript.scm
;; DESCRIPTION : Initialize the JavaScript plugin (the browser build)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The JavaScript of the page which runs TeXmacs, in a session (or in
;; executable folds): a plugin of the page itself (web/tm-javascript.js,
;; misc/wasm/workers.js), not a Web Worker, so that it sees and changes the
;; page, and TeXmacs through TeXmacs.scheme (misc/wasm/javascript.js).
;; Only in a browser, where the Vue plugin defines web-files.

(define (javascript-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u) "SourceCode")
      (string-append s "\n<EOF>\n"))))

(define (javascript-in-browser?)
  (in-browser?))

;; the script of the plugin, in the file system of TeXmacs
(define (javascript-plugin-script)
  (string-append "page:"
                 (url->system
                  (url-concretize
                   "$TEXMACS_PATH/plugins/javascript/web/tm-javascript.js"))))

(plugin-configure javascript
  (:require (javascript-in-browser?))
  (:worker ,(javascript-plugin-script))
  (:serializer ,javascript-serialize)
  (:session "JavaScript")
  (:scripts "JavaScript"))
