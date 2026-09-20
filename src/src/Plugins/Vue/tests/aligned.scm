(use-modules (kernel gui menu-test))
(tm-widget (vue-test3)
  (padded
    (aligned
      (item (text "Name:") (input (noop) "string" '("Joe") "12em"))
      (item (text "A much longer prompt:") (toggle (noop) #t))
      (item (text "Size:") (enum (noop) '("small" "large") "small" "6em")))
    ===
    (resize "400px" "150px"
      (hsplit (text "left pane") (vsplit (text "top") (text "bottom"))))))
(tm-define (show-plain menu-promise name w h)
  (let* ((win (alt-window-handle))
         (men (menu-promise))
         (wid (make-menu-widget* (list 'vertical men) 0)))
    (alt-window-create-plain win wid name)
    (alt-window-set-size win w h)
    (alt-window-show win)))
(delayed (:idle 1500) (top-window vue-test3 "Vue test 3"))
