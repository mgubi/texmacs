;; the window of the error messages: the editor of its messages is the
;; whole page of a tab, and the window is laid out as an editor window: it
;; opens at the size of a new editor window (800x600) whatever the messages
;; are (it was as wide as the longest line), and the editor fills it and
;; follows its size (the typeset box of before kept its natural size, three
;; lines tall in a grey window which could not be made smaller than it)
(delayed (:pause 2500)
  (for (i (.. 0 12))
    (debug-message "std-warning"
      (string-append "warning " (number->string i) ": "
                     (make-string 400 #\x) "\n")))
  (open-error-messages))
