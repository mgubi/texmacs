;; a long document for a drag selection past the bottom of the view
;; (drag-scroll.script); F12 prints the scroll position and the selection
(delayed (:pause 2000)
  (insert (stree->tree `(document ,@(map (lambda (i) (string-append "Line " (number->string i) " lorem ipsum dolor sit amet")) (.. 1 200)))))
  (go-start))
(kbd-map ("F12" (display* "got: scroll " (get-scroll-y)
                          " selection " (selection-get-start)
                          " " (selection-get-end) "\n")))
