;; An interactive prompt (Name, Age): its fields are filled from the
;; keyboard (Tab goes to the next one), Return answers; the keys typed then
;; in the editor go to the editor. Runs on every port with the hooks of the
;; tests (docs/gui-scripting.md):
;;   SCM=$PWD/tests/gui/dialog.scm sh tests/Plugins/Vue/run.sh dialog 40
(use-modules (check gui-driver))
(gui-test-run
 (list (gt-wait 6000)
       ;; (from the loop of TeXmacs: a modal dialog does not hold a step)
       (gt-eval-later "(interactive (lambda (name age) (gt-record name \" / \" age)) (list \"Name\" \"string\" \"Joe\" \"Jane\") \"Age\")")
       (gt-window "Enter data")
       (gt-raw "focus")
       (gt-idle)
       (gt-raw "key Backspace")
       (gt-raw "key Backspace")
       (gt-raw "key Backspace")
       (gt-raw "text Bob")
       (gt-idle)
       (gt-raw "key Tab")
       (gt-idle)
       (gt-raw "text 42")
       (gt-idle)
       (gt-raw "key Return")
       (gt-idle)
       (gt-check "the answers" (lambda () (== (gt-recorded) '("Bob / 42"))))
       (gt-window "main")
       (gt-raw "focus")
       (gt-idle)
       (gt-raw "text xyz")
       (gt-idle)
       (gt-check "the editor got the keys"
                 (lambda () (gt-document-has? "xyz")))
       (gt-exit)))
