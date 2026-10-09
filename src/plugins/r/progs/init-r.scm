
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-r.scm
;; DESCRIPTION : Initialize GNU R plugin
;; COPYRIGHT   : (C) 1999  Michael Lachmann and Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (r-serialize lan t)
  (if (run-via-jupyter? "r")
    (jupyter-serialize lan t)
    (with u (pre-serialize lan t)
        (with s (texmacs->code u)
        (string-append (escape-verbatim
    		      (string-replace s "\n" ";;")) "\n")))))

(define (r-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/r")
      (system-setenv "TEXMACS_SEND"
              "source(paste(Sys.getenv(\"TEXMACS_HOME_PATH\"),\"/plugins/r/texmacs.r\",sep=\"\"))\n"))
  "tm_r")

(tm-widget (plugin-preferences-widget name)
  (:require (== name "r"))
  (aligned
    (meti (hlist // (text "Run via Jupyter"))
      (toggle (run-via-jupyter "r" answer)
              (run-via-jupyter? "r")))))

;; In a browser, the plugin is a Web Worker which runs R in WebAssembly
;; (webR: web/tm-r.mjs, copied to r/ next to the page), whose inputs end
;; with a line <EOF>; elsewhere the R program of the computer
(define (r-in-browser?)
  (in-browser?))

(define (r-serialize-web lan t)
  (with u (pre-serialize lan t)
    (string-append (texmacs->code u) "\n<EOF>\n")))

(define (r-engine)
  (if (r-in-browser?)
      `((:worker "r/tm-r.mjs")
        (:serializer ,r-serialize-web))
      `((:serializer ,r-serialize)
        (:launch ,(r-launcher))
        (:tab-completion #t))))

(plugin-configure r
  (:winpath "R-*" "bin")
  (:winpath "R/R*" "bin")
  (:require (or (r-in-browser?) (url-exists-in-path? "R")))
  ,@(r-engine)
  (:preferences (and (not (r-in-browser?)) (supports-jupyter?)))
  (:session "R")
  (:scripts "R"))

(texmacs-modes
  (in-r% (== (get-env "prog-language") "r"))
  (in-prog-r% #t in-prog% in-r%))

(lazy-keyboard (r-edit) in-prog-r?)

(when (supports-r?)
  (lazy-input-converter (r-input) r)

  (menu-bind r-menu
    ("update menu" (insert "t.update.menus(max.len=30)"))
    ("R help in TeXmacs" (insert "t.start.help()")))

  ;; (not in a browser: its commands are those of the program tm_r)
  (menu-bind plugin-menu
    (:require (and (in-r?) (not (r-in-browser?))))
    (=> "R" (link r-menu))))
