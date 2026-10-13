;; a typographic palette of the "Color" menu (the preference "typographic
;; palette set", set in the home of the test only): the families of hues in
;; columns, their tones in rows, grouped as Text, Accents and Backgrounds
(use-modules (kernel gui menu-test))
(set-preference "typographic palette set" "Muted")
;; a set of the user, computed from a colour per column, as it would be
;; defined in ~/.TeXmacs/progs/my-init-texmacs.scm
(define-typographic-palette-from-colors "Sea"
  (list "#5f6b73" "#c0504d" "#b07a45" "#c9a44a"
        "#4f8a6b" "#2f8f9d" "#3b6ea8" "#6c5fa8"))
(display* "got: " (typographic-palette-names) "\n")
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
