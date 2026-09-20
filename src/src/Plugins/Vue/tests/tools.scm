;; the tool areas of the main window: a tool on the right, one on the left
;; and one at the bottom
(use-modules (generic format-widgets) (generic document-widgets))
(delayed (:idle 3000)
  (tool-select :right 'format-paragraph-tool)
  (tool-select :left 'document-paragraph-tool)
  (tool-select :bottom 'buffer-tool)
  (display* "tools: bottom? " (has-bottom-tools?)
            " visible " (visible-bottom-tools? 0)
            " side " (visible-side-tools? 0) " " (visible-side-tools? 1) "\n"))
