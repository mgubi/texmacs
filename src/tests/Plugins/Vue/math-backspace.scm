;; F12 prints the tree of the document (math-backspace.script)
(kbd-map ("F12" (display* "tree: " (tree->stree (buffer-tree)) "\n")))
