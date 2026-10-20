;; (named "name" ...) in the menus: two buttons with the same label, told
;; apart by the names of the groups they are in ("#first > Same", or
;; "#second" alone for what it holds), and a button in two names, one in
;; the other ("#outer > #inner > Same"); a test of the driver (gt-activate)
;; with steps in this file, without a script:
;;   sh tests/Plugins/Vue/run.sh named 40
;; it quits by itself, with the status 1 if a check failed.
(use-modules (check gui-driver))
(define named-got '())
(tm-widget (named-test-widget)
  (vertical
    (named "first"
      ("Same" (set! named-got (cons "first" named-got))))
    (named "second"
      ("Same" (set! named-got (cons "second" named-got))))
    (named "outer"
      (named "inner"
        ("Same" (set! named-got (cons "nested" named-got)))))))
(gui-test-run
 (list (gt-wait 3000)
       (gt-eval (lambda () (top-window named-test-widget "Named test")))
       (gt-wait 3000)
       (gt-raw "window Named test")
       (gt-raw "focus")
       (gt-idle)
       (gt-activate "#second")
       (gt-idle)
       (gt-activate "#first > Same")
       (gt-idle)
       (gt-activate "#outer > #inner > Same")
       (gt-idle)
       (gt-check "the buttons were told apart"
                 (lambda () (== named-got '("nested" "first" "second"))))
       (gt-exit)))
