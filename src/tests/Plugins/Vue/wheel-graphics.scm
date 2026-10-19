;; in graphics mode the editor captures the wheel (wheel-capture?): the
;; canvas sends it a "wheel" event instead of scrolling the view, as
;; QTMWidget::wheelEvent (wheel-graphics.script)
(delayed (:pause 2000)
  (insert (stree->tree `(document ,@(map (lambda (i) (string-append "Line " (number->string i))) (.. 1 100)))))
  (go-start)
  (make-graphics))
(tm-define (wheel-event x y)
  (display* "got: wheel " x " " y " scroll " (get-scroll-y) "\n"))
(kbd-map ("F12" (display* "got: scroll " (get-scroll-y)
                          " graphics " (in-graphics?) " capture " (wheel-capture?) "\n")))
