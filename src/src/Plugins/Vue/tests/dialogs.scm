(use-modules (kernel gui menu-test))

(tm-widget (vue-test2)
  (padded
    (division "title-bar" (text "A title bar division"))
    ===
    (minibar (text "mini") // (text "bar") // ("Btn" (noop)))
    ===
    (hlist (extend (text "short") (text "a much longer label")) // (text "|after extend"))
    ===
    (division "plain" (text "plain division"))))

(tm-define (show-plain menu-promise name w h)
  (let* ((win (alt-window-handle))
         (men (menu-promise))
         (wid (make-menu-widget* (list 'vertical men) 0)))
    (alt-window-create-plain win wid name)
    (alt-window-set-size win w h)
    (alt-window-show win)
    win))

(tm-define (show-raw wid name w h)
  (let* ((win (alt-window-handle)))
    (alt-window-create-plain win wid name)
    (alt-window-set-size win w h)
    (alt-window-show win)
    win))

(tm-define (show-popup menu-promise name x y)
  (let* ((win (alt-window-handle))
         (men (menu-promise))
         (wid (make-menu-widget* (list 'vertical men) 0)))
    (alt-window-create-popup win wid name)
    (alt-window-set-position win x y)
    (alt-window-show win)
    win))

(tm-widget (vue-popup)
  ("First item" (noop))
  ("Second item" (noop))
  ---
  ("Third item" (noop)))

(delayed (:idle 1500)
  (show-plain vue-test2 "Vue test 2" 500 300)
  (show-raw (widget-color-picker (object->command (lambda (t) (display* "color: " t "\n"))) #f
                                 (list (stree->tree "red") (stree->tree "blue")))
            "Colors" 500 400)
  (show-raw (widget-printer (object->command (lambda x (display* "print done\n")))
                            (string->url "/tmp/document.pdf"))
            "Print" 400 150)
  (show-popup vue-popup "Popup" 700 -300))
