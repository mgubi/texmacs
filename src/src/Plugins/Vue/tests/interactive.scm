;; the query line of the footer: with "interactive questions" set to
;; "footer" the editor asks in the footer instead of opening a dialog.
;; It used to abort, because the slot which reports the mode was not
;; implemented and the fallback query returned nothing.
;;
;; The preference is set and put back in the same turn: set-preference
;; writes to ~/.TeXmacs, and a test must not leave the settings changed.
;; It is only read while (interactive ...) decides which form to use.
(delayed (:pause 3500)
  (set-preference "interactive questions" "footer")
  (interactive (lambda (answer) (display* "answer: " answer "\n"))
               "Your name")
  (reset-preference "interactive questions")
  (save-preferences))
