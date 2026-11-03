;; a long document on a patterned page, with text filled with the same
;; pattern: scrolling must move the pattern with the page, the tiles of the
;; fills and of the glyphs meeting where they did (the MuPDF renderer's
;; placed_pattern and draw_bis); driven by scroll-shift.script
(delayed (:pause 2000)
  (init-env-tree "bg-color"
    (stree->tree '(pattern "geometric/cubes.png" "" "")))
  (insert (stree->tree
    `(document
       ,@(map (lambda (i)
                (if (== (modulo i 3) 0)
                    `(with "font-size" "2" "font-series" "bold"
                       "color" (pattern "geometric/cubes.png" "" "")
                       ,(string-append "Line " (number->string i) " patterned"))
                    (string-append "Line " (number->string i)
                                   " lorem ipsum dolor sit amet")))
              (.. 1 120))))))
