;; replace a tool which embeds an editor (the font tool has a sample text):
;; the old widgets are freed by a command, before the redraw of the frame
(use-modules (fonts font-new-widgets) (generic document-widgets))
(delayed (:pause 3000) (display* "font tool\n") (open-font-tool "Font" get-env make-multi-with #f))
(delayed (:pause 7000) (display* "paragraph tool\n") (tool-select :right 'document-paragraph-tool))
