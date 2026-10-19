;; the search toolbar asks for the keyboard with (keyboard-focus-on "search"),
;; which needs SLOT_KEYBOARD_FOCUS_ON: without it the typing went to the
;; document instead of the search field
(delayed (:pause 3000) (toolbar-search-start))
