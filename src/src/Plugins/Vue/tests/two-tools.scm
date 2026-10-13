;; several tools on the same side: one at the top, one at the bottom
(use-modules (texmacs menus preferences-widgets) (generic document-widgets))
(delayed (:pause 3000) (display* "tool 1\n") (tool-select :right 'preferences-tool))
(delayed (:pause 5500) (display* "tool 2\n") (tool-select :bottom-right 'document-paragraph-tool))
(delayed (:pause 8000) (display* "tool 3\n") (tool-select :left 'document-page-tool))
