;; A tab widget: each tab shows its page, and only it. Runs on every port
;; with the hooks of the tests (docs/gui-scripting.md):
;;   SCM=$PWD/tests/gui/tabs.scm sh tests/Plugins/Vue/run.sh tabs 40
(use-modules (check gui-driver))
(tm-widget (gui-tabs-test)
  (padded
    (tabs
      (tab (text "Small") (text "A small page"))
      (tab (text "Large")
        (vlist
          (text "A larger page with several lines")
          (text "line 2")
          (text "line 3")))
      (tab (text "Third") (text "Third page")))))
(gui-test-run
 (list (gt-wait 6000)
       (gt-eval-later "(top-window gui-tabs-test \"Tabs test\")")
       (gt-window "Tabs test")
       (gt-idle)
       (gt-see "A small page")
       (gt-check "the other pages are hidden"
                 (lambda () (not (or (gt-find-text "line 2")
                                     (gt-find-text "Third page")))))
       (gt-activate "Large")
       (gt-idle)
       (gt-see "line 2")
       (gt-activate "Third")
       (gt-idle)
       (gt-see "Third page")
       (gt-check "the second page is hidden"
                 (lambda () (not (gt-find-text "line 2"))))
       (gt-exit)))
