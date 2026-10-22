;; the window of the error messages: its typeset box is the whole page of a
;; tab, which fills the window and follows its size (it kept its natural
;; size, three lines tall in a grey window which could not be made smaller
;; than it), and the window opens no larger than 2/3 of the screen (it was
;; as wide as the screen, for the long lines)
(delayed (:pause 2500)
  (for (i (.. 0 12))
    (debug-message "std-warning"
      (string-append "warning " (number->string i) ": "
                     (make-string 400 #\x) "\n")))
  (open-error-messages))
