;; a menu bar with two pulldown menus, the first holding a submenu: opening
;; the submenu must keep its parent open, and opening the second menu must
;; close the first one with its submenu
(tm-widget (submenu-test)
  (padded
    (hlist
      (=> "First"
          ("Alpha" (display* "SUB alpha\n"))
          (-> "More"
              ("Beta" (display* "SUB beta\n"))
              ("Gamma" (display* "SUB gamma\n")))
          ("Delta" (display* "SUB delta\n")))
      // //
      (=> "Second"
          ("Epsilon" (display* "SUB epsilon\n")))
      >>)))

(delayed (:pause 2500) (top-window submenu-test "Submenus"))
