;; the initial scrolling position of a resize (hpos, vpos): a box which
;; starts at the bottom of its contents (a log showing its last lines)
;; and one in the middle of a wide row, both with their scroll bars
;; (resize-pos.script)
(define (resize-pos-lines)
  (map (lambda (i) (string-append "Line " (number->string i))) (.. 1 21)))
(tm-widget (vue-resize-pos)
  (padded
    (resize "120px" '("120px" "120px" "120px" "bottom")
      (vertical (for (s (resize-pos-lines)) (text s))))
    ===
    (resize '("250px" "250px" "250px" "center") "30px"
      (hlist (for (s (resize-pos-lines)) (text s) //)))))
(delayed (:pause 1500) (top-window vue-resize-pos "Vue resize"))
