;; the MuPDF renderer: paper mode (neutral pattern around the pages) and
;; text filled with a pattern (draw_bis)
(delayed (:pause 3000)
  (init-env "page-medium" "paper")
  (insert (stree->tree
    '(document
       (with "font-size" "3" "color" (pattern "neutral-pattern.png" "100%" "100%") "Pattern text")
       (with "font-size" "3" "color" (pattern "vertical-white-black.png" "20%" "100@") "Stripes")
       "Plain paragraph on paper."))))
