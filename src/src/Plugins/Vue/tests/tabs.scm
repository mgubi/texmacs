(use-modules (generic document-widgets) (texmacs menus preferences-widgets))
;; tab widgets: pages of different sizes, plus the real dialogs using tabs
(tm-widget (vue-tabs)
  (padded
    (tabs
      (tab (text "Small") (text "A small page"))
      (tab (text "Large")
        (vlist
          (text "A larger page with several lines")
          (text "line 2")
          (text "line 3")
          (input (noop) "string" '("input") "15em")))
      (tab (text "Third") (text "Third page")))))
(delayed (:idle 1500)
  (top-window vue-tabs "Vue tabs")
  (open-style-selector)
  (open-preferences-window))
