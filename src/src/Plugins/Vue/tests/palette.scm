;; the colour palette: the tile of explicit buttons of standard-color-menu,
;; the one the "Color" menu shows. Each cell is a coloured rectangle and
;; nothing else, so it must be drawn flat, not framed as a push button.
(use-modules (kernel gui menu-test))

(tm-define (show-plain menu-promise name w h)
  (let* ((win (alt-window-handle))
         (men (menu-promise))
         (wid (make-menu-widget* (list 'vertical men) 0)))
    (alt-window-create-plain win wid name)
    (alt-window-set-size win w h)
    (alt-window-show win)
    win))

(delayed (:pause 1500)
  (show-plain (lambda () (standard-color-menu
                          (lambda (col) (display* "color: " col "\n"))))
              "Palette" 400 300))
