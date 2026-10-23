;; The font selector in a window of its own: it opens, it closes (as from
;; its title bar), and the editor goes on getting the keys. Runs on every
;; port with the hooks of the tests (docs/gui-scripting.md):
;;   SCM=$PWD/tests/gui/font.scm sh tests/Plugins/Vue/run.sh font 40
;; (open-font-selector would use a side tool when the "side tools"
;; preference is on)
(use-modules (check gui-driver) (fonts font-new-widgets))
(gui-test-run
 (list (gt-wait 6000)
       (gt-eval-later "(open-font-selector-window)")
       (gt-window "Font")
       (gt-check "the window opened" (lambda () (gt-window? "Font")))
       (gt-raw "close")
       (gt-wait 500)
       (gt-idle)
       (gt-check "the window closed" (lambda () (not (gt-window? "Font"))))
       (gt-window "main")
       (gt-raw "focus")
       (gt-idle)
       (gt-raw "text abc")
       (gt-idle)
       (gt-check "the editor got the keys"
                 (lambda () (gt-document-has? "abc")))
       (gt-exit)))
