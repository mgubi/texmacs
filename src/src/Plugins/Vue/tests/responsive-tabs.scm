;; the responsive tabs (as in the console of the Debug menu) in the mode
;; given by TEXMACS_VUE_TAB_MODE, which stands for the preference
;; "gui:responsive tab mode" (top, side, mobile, grid): see
;; responsive-tabs.script
(tm-widget (vue-responsive-tabs)
  (resize "460px" "260px"
    (responsive-tabs
      (responsive-tab (text "First")
        (vertical (text "The first page") (text "with two lines")))
      (responsive-tab (text "Second")
        (vertical (text "The second page")))
      (responsive-tab (text "Third")
        (vertical (text "The third page"))))))
(delayed (:pause 1500) (top-window vue-responsive-tabs "Vue tabs"))
