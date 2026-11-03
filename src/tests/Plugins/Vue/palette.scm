;; the colour palette of the document "Color" menu, in a popup sized to its
;; contents as a real menu is. Its cells are a coloured rectangle and
;; nothing else: they must be drawn flat rather than framed as push
;; buttons, and they must not reserve the column this menu keeps for the
;; marks of its items, which would spread the palette by a mark per column.
(use-modules (kernel gui menu-test))

(tm-define (show-popup menu-promise name x y)
  (let* ((win (alt-window-handle))
         (men (menu-promise))
         (wid (make-menu-widget* (list 'vertical men) 0)))
    (alt-window-create-popup win wid name)
    (alt-window-set-position win x y)
    (alt-window-show win)
    win))

;; a popup sized to its contents, as a real menu is
(delayed (:pause 1500)
  (show-popup (lambda () (document-foreground-color-menu)) "Palette" 400 -300))
