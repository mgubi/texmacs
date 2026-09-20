;; the widget styles: mini, monospaced, grey, inert, bold, centered
(tm-widget (vue-styles)
  (padded
    (text "Normal text")
    (bold (text "Bold text"))
    (mini #t (text "Mini text (normal size unless minibars are on)"))
    (mono (text "Monospaced text"))
    (grey (text "Grey text"))
    (inert (text "Inert text"))
    (resize "360px" "40px" (centered (text "Centered text")))
    ===
    (hlist (toggle (noop) #t) // (mono (input (noop) "string" '("mono input") "10em")))
    ===
    (explicit-buttons (hlist ("Normal" (noop)) // (inert ("Inert" (noop)))))))
(delayed (:idle 1500) (top-window vue-styles "Vue styles"))
