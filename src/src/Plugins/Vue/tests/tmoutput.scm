;; the extent of a texmacs-output: on its own it takes the size of the box
;; it typesets, and inside a "resize" it fills the pane it was given. The
;; second case was wrong: the documentation pane of the macro editors was
;; as tall as its one line of text instead of the 220px it asked for.
(tm-widget (tmoutput-test)
  (padded
    (vlist
      (bold (text "on their own: each box is as wide as its contents"))
      (texmacs-output `(document "Hello") '(style "generic"))
      ===
      (texmacs-output
       `(document (concat "x" (rsup "2") "+y" (rsup "2") "=z" (rsup "2")))
       '(style "generic"))
      ===
      (texmacs-output
       `(document "The quick brown fox jumps over the lazy dog.")
       '(style "generic"))
      ======
      (bold (text "in a resize: the pane is filled, 400 by 150 points"))
      (resize "400px" "150px"
        (texmacs-output `(document "Hello") '(style "generic"))))))

(delayed (:pause 2500) (top-window tmoutput-test "TmOutput"))
