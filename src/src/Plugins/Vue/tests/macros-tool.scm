;; the macros editor as a side tool (side-tools? forced): the filtered list
;; and the embedded editors must stay inside their resize boxes
(use-modules (source macro-widgets))
(tm-define (side-tools?) #t)
(delayed (:pause 3000) (open-macros-editor :global))
