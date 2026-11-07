;; replacing a tool, adding a bottom one and closing the top one
(use-modules (texmacs menus preferences-widgets) (generic document-widgets) (generic format-tools))
(delayed (:pause 3000) (display* "tool 1\n") (tool-select :right 'preferences-tool))
(delayed (:pause 6000) (display* "tool 2\n") (tool-select :right 'document-paragraph-tool))
(delayed (:pause 9000) (display* "tool 3\n") (tool-select :bottom-right 'subsections-tool))
