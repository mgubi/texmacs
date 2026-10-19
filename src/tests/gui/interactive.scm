;; The question in the footer: with "interactive questions" set to "footer"
;; the editor asks in the footer instead of a dialog, and the answer typed
;; there comes back. Runs on every port with the hooks of the tests
;; (docs/gui-scripting.md):
;;   SCM=$PWD/tests/gui/interactive.scm sh tests/Plugins/Vue/run.sh interactive 40
;; The preference is set and put back in the same turn (it is only read
;; while (interactive ...) decides which form to use), in the copy of the
;; home which run.sh makes anyway.
(use-modules (check gui-driver))
(gui-test-run
 (list (gt-wait 6000)
       (gt-raw "window main")
       (gt-raw "focus")
       (gt-idle)
       (gt-eval-later "(begin (set-preference \"interactive questions\" \"footer\") (interactive (lambda (answer) (gt-record answer)) \"Your name\") (reset-preference \"interactive questions\"))")
       (gt-wait 1000)
       (gt-idle)
       (gt-raw "text Bob")
       (gt-idle)
       (gt-raw "key Return")
       (gt-idle)
       (gt-check "the answer" (lambda () (== (gt-recorded) '("Bob"))))
       (gt-exit)))
