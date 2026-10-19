;; the macros editor (Tools > Macros > Edit macros): embedded input and
;; output editors in a side tool (or a dialog when side tools are off)
(use-modules (source macro-widgets))
(delayed (:pause 3000) (open-macros-editor :global))
