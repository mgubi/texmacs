(use-modules (kernel gui menu-test))
;; a window whose size comes from the default size of a resize widget
(tm-widget (vue-resize)
  (padded
    (text "A resizable list:")
    ===
    (resize '("150px" "400px" "900px") '("100px" "300px" "900px")
      (scrollable
        (choice (display* "choice: " answer "\n")
                '("First" "Second" "Third" "Fourth" "Fifth" "Sixth"
                  "Seventh" "Eighth" "Ninth" "Tenth") "Third")))))
(delayed (:pause 1500) (top-window vue-resize "Vue resize"))
