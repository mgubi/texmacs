;; a pattern fill (a graphics rectangle) and pattern glyphs with the same
;; tile, a 16 pixel period of black and white, at zoom 1 and then at zoom 2:
;; the fill and the glyphs must keep the same period at both, which they do
;; only if the MuPDF renderer keys its patterns by their size as well.
;; The zoom is set with set-window-zoom-factor, not change-zoom-factor,
;; which may save it as a preference: a test must not change the settings.
(delayed (:pause 2000)
  (set-window-zoom-factor 1.0)
  (with tile (url->string (url-append (url-pwd) "src/Plugins/Vue/tests/stripes.png"))
    (insert (stree->tree
      `(document
         (with "font-size" "2" "font-series" "bold"
               "color" (pattern ,tile "" "")
           "MMMMMM")
         (graphics
           (with "color" "none" "fill-color" (pattern ,tile "" "")
             (cline (point "-3" "-0.5") (point "3" "-0.5")
                    (point "3" "0.5") (point "-3" "0.5"))))))))
  (go-start)
  (display* "zoom: " (get-window-zoom-factor) "\n")
  (delayed (:pause 12000)
    (set-window-zoom-factor 2.0)
    (go-start)
    (display* "zoom: " (get-window-zoom-factor) "\n")))
