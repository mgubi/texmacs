;; the widget styles: mini, monospaced, grey, inert, bold, centered.
;; Compare with the Qt port, whose to_qfont maps the same flags onto a
;; QFont. Bold is the one which used to be lost here: the UI font was asked
;; for through a name whose translation rule drops the series.
(tm-widget (vue-styles)
  (padded
    (text "Normal text")
    (bold (text "Bold text"))
    (mini #t (text "Mini text (normal size unless minibars are on)"))
    (mono (text "Monospaced text"))
    ;; no bold typewriter in the EC fonts: this comes out in the regular
    ;; weight, where Qt would ask the system for a synthetic bold
    (bold (mono (text "Bold monospaced text (no bold in the EC typewriter)")))
    (grey (text "Grey text"))
    (inert (text "Inert text"))
    (resize "360px" "40px" (centered (text "Centered text")))
    ===
    (hlist (toggle (noop) #t) // (mono (input (noop) "string" '("mono input") "10em")))
    ===
    (explicit-buttons (hlist ("Normal" (noop)) // (inert ("Inert" (noop)))))))
(delayed (:pause 1500) (top-window vue-styles "Vue styles"))
