;; the search toolbar takes the keyboard, and gives it back to the document
;; when it closes (keyboard-focus-on "canvas", SLOT_KEYBOARD_FOCUS of the
;; texmacs widget, which was dropped: the typing went on into the bar).
;; F12 prints the tree of the document (search-return.script)
(delayed (:pause 3000) (toolbar-search-start))
(kbd-map ("F12" (display* "got: " (tree->stree (buffer-tree)) "\n")))
