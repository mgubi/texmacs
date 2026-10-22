;; the window of the error messages: its typeset box is the whole page of a
;; tab, which behaves as an editor: the window opens at the size of a new
;; editor window (800x600) whatever the messages are (it was as wide as the
;; longest line), and the box fills it and follows its size (it kept its
;; natural size, three lines tall in a grey window which could not be made
;; smaller than it)
(delayed (:pause 2500)
  (for (i (.. 0 12))
    (debug-message "std-warning"
      (string-append "warning " (number->string i) ": "
                     (make-string 400 #\x) "\n")))
  (open-error-messages))
