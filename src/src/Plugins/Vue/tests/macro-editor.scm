;; the editor of a single macro (Focus > Edit macro): an embedded input
;; editor in a side tool (or a dialog when side tools are off)
(use-modules (source macro-widgets))
(delayed (:pause 3000) (open-macro-editor "strong" :global))
