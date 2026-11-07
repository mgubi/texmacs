;; in graphics mode the editor captures the wheel (wheel-capture?): the
;; canvas sends it a "wheel" event instead of scrolling the view, as
;; QTMWidget::wheelEvent (wheel-graphics.script); in a graphics the editor
;; moves the origin of the graphics with it (graphics-wheel, called by
;; edit_graphics_rep::mouse_graphics), which then does not reach the
;; wheel-event of the other modes
(delayed (:pause 2000)
  (insert (stree->tree `(document ,@(map (lambda (i) (string-append "Line " (number->string i))) (.. 1 100)))))
  (go-start)
  (make-graphics))
;; (graphics-kbd is loaded when the cursor is first in a graphics, and would
;; then replace this definition: it is loaded first)
(use-modules (graphics graphics-kbd))
(tm-define (graphics-wheel dx dy)
  (display* "got: graphics-wheel " dx " " dy " scroll " (get-scroll-y) "\n")
  (former dx dy))
(kbd-map ("F12" (display* "got: scroll " (get-scroll-y)
                          " graphics " (in-graphics?) " capture " (wheel-capture?) "\n")))
