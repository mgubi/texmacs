;; the macro editor as a side tool (as with the "side tools" preference on,
;; forced here without touching the preferences)
(use-modules (source macro-widgets))
(tm-define (side-tools?) #t)
(delayed (:pause 3000) (open-macro-editor "strong" :global))
