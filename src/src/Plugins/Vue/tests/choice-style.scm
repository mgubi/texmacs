
;; the style flags of the choice widgets: a plain list, a mini list, a
;; monospaced one, and a greyed (inert) one which does not react to clicks
(tm-widget (choice-style-test)
  (padded
    (hlist
      (vlist (text "plain")
             (choice (display* "plain: " answer "\n")
                     '("First" "Second" "Third") "Second"))
      // //
      (vlist (text "mini")
             (mini #t (choice (display* "mini: " answer "\n")
                              '("First" "Second" "Third") "Second")))
      // //
      (vlist (text "monospaced")
             (mono (choice (display* "mono: " answer "\n")
                           '("First" "Second" "Third") "Second")))
      // //
      (vlist (text "inert")
             (inert (choice (display* "inert: " answer "\n")
                            '("First" "Second" "Third") "Second"))))))

(delayed (:pause 2500) (top-window choice-style-test "Choice styles"))
