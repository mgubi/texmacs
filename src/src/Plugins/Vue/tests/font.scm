;; the font selector in a window of its own (open-font-selector would use a
;; side tool when the "side tools" preference is on)
(use-modules (fonts font-new-widgets))
(delayed (:idle 2500) (open-font-selector-window))
