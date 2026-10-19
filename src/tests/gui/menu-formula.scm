;; A test of the interface which runs on every port with the hooks of the
;; tests (docs/gui-scripting.md): Insert > Mathematics > Displayed formula
;; through the menu bar, a letter typed, then the document is checked.
;; Run it from the top of the source tree of a build, with
;;   SCM=$PWD/tests/gui/menu-formula.scm sh tests/Plugins/Vue/run.sh menu-formula 40
;; it quits by itself, with the status 1 if a check failed.
(use-modules (check gui-driver))
(gui-test-run
 (list (gt-wait 8000)
       (gt-raw "window main")
       (gt-raw "focus")
       (gt-idle)
       (gt-activate "menubar > Insert")
       (gt-idle)
       (gt-activate "Insert > Mathematics")
       (gt-idle)
       (gt-activate "Mathematics > Displayed formula")
       (gt-idle)
       (gt-raw "text x")
       (gt-idle)
       (gt-check "the formula holds the letter"
                 (lambda ()
                   (== (tree->stree (buffer-tree))
                       '(document (equation* (document "x"))))))
       (gt-exit)))
