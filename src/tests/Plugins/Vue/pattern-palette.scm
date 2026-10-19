;; the "Color" menu of the document with its pattern palette (as in the Qt
;; port): the colours, then the patterns of misc/patterns/vintage drawn in
;; their cells, then "Palette", "Pattern" and "Other"; a click on a pattern
;; colours the text inserted next with it
(use-modules (kernel gui menu-test))

(tm-define (show-popup menu-promise name x y)
  (let* ((win (alt-window-handle))
         (men (menu-promise))
         (wid (make-menu-widget* (list 'vertical men) 0)))
    (alt-window-create-popup win wid name)
    (alt-window-set-position win x y)
    (alt-window-show win)
    win))

(delayed (:pause 1500)
  (show-popup (lambda () (color-menu)) "Palette" 400 -300))

(delayed (:pause 9000)
  (insert "Hello patterns")
  (display* "got: " (tree->stree (buffer-tree)) "\n"))
