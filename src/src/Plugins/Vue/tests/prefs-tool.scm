;; the preferences as a side tool: its tabs must react to clicks
(use-modules (texmacs menus preferences-widgets))
(delayed (:idle 3000) (tool-select :right 'preferences-tool))
