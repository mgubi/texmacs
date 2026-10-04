;; drag and drop: trace what the editor receives and insert it, as the
;; default handler of kbd-handlers.scm does
(tm-define (mouse-drop-event x y obj)
  (display* "DROP at " x "," y " -> " (tm->stree obj) "\n")
  (insert obj))
