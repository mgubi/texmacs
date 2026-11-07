;; the themes: a window with the usual widgets, shown under the theme named
;; by TEXMACS_VUE_THEME (see the "gui theme" preference)
(tm-widget (theme-test)
  (padded
    (hlist
      (vlist (text "A label") (bold (text "bold")) (grey (text "grey")))
      // //
      (vlist ("Push button" (noop))
             (toggle (noop) #t)
             (enum (noop) '("Alpha" "Beta") "Alpha" "8em"))
      // //
      (vlist (choice (noop) '("First" "Second" "Third") "Second"))
      // //
      (vlist (input (noop) "string" '("an input") "8em")))))

(delayed (:pause 2500) (top-window theme-test "Theme"))
