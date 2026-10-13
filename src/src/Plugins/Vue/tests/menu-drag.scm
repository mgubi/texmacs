;; a menu bar whose first menu has a submenu, followed by a rule, a disabled
;; item and the title of a group: pressing a title opens its menu, dragging
;; to an item and releasing there chooses it; the pointer resting on the
;; rule, the disabled item or the group title closes the open submenu
(tm-widget (menu-drag-test)
  (padded
    (hlist
      (=> "First"
          ("Alpha" (display* "SUB alpha\n"))
          (-> "More"
              ("Beta" (display* "SUB beta\n"))
              ("Gamma" (display* "SUB gamma\n")))
          ---
          (when #f ("Disabled" (display* "SUB disabled\n")))
          (group "A group")
          ("Delta" (display* "SUB delta\n")))
      // //
      (=> "Second"
          ("Epsilon" (display* "SUB epsilon\n")))
      >>)))

(delayed (:pause 2500) (top-window menu-drag-test "Menu drag"))
