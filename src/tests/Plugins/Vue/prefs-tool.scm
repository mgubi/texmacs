;; the preferences as a side tool: its tabs must react to clicks
(use-modules (texmacs menus preferences-widgets))
(delayed (:pause 3000) (tool-select :right 'preferences-tool))
