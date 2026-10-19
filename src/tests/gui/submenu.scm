;; Pull-down menus of a window, the first with a submenu: an item of the
;; submenu runs, then an item of the second menu. Runs on every port with
;; the hooks of the tests (docs/gui-scripting.md):
;;   SCM=$PWD/tests/gui/submenu.scm sh tests/Plugins/Vue/run.sh submenu 40
(use-modules (check gui-driver))
(tm-widget (gui-submenu-test)
  (padded
    (hlist
      (=> "First"
          ("Alpha" (gt-record "alpha"))
          (-> "More"
              ("Beta" (gt-record "beta"))
              ("Gamma" (gt-record "gamma")))
          ("Delta" (gt-record "delta")))
      // //
      (=> "Second"
          ("Epsilon" (gt-record "epsilon")))
      >>)))
(gui-test-run
 (list (gt-wait 6000)
       (gt-eval-later "(top-window gui-submenu-test \"Submenus\")")
       (gt-wait 1500)
       (gt-raw "window Submenus")
       ;; (Vue draws the menus in their window, which is sized to its
       ;; buttons: room for them)
       (gt-raw "resize 400 260")
       (gt-raw "focus")
       (gt-idle)
       (gt-activate "First")
       (gt-idle)
       (gt-activate "First > More")
       (gt-idle)
       (gt-activate "More > Beta")
       (gt-idle)
       (gt-activate "Second")
       (gt-idle)
       (gt-activate "Second > Epsilon")
       (gt-idle)
       (gt-check "the items ran" (lambda () (== (gt-recorded) '("beta" "epsilon"))))
       (gt-exit)))
