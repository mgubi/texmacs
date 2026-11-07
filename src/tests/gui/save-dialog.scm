;; Save as, through the file dialog of the system (macOS; skipped where the
;; port does not reach it): a document saved under a name without a suffix
;; is a .tm, beside a file of that name without a suffix which stays as it
;; is, with no question about it (#400); a name whose .tm exists gets the
;; alert to replace it, whose Cancel keeps the file and Replace replaces
;; it. The buttons are those of an English system. Runs on every port with
;; the hooks of the tests (docs/gui-scripting.md):
;;   tests/gui/run.sh save-dialog
(use-modules (check gui-driver))

(define sd-dir (string-append (getenv "TEXMACS_HOME_PATH") "/dialog-files"))
(define (sd-file name) (system->url (string-append sd-dir "/" name)))
(define (sd-content name)
  (if (url-exists? (sd-file name)) (string-load (sd-file name)) ""))
(define sd-text "Saved through the dialog")
(define sd-save-as
  "(choose-file save-buffer-as \"Save TeXmacs file\" \"texmacs\")")

(define (sd-save-as-steps name)
  ;; Save as, in the folder of the test, under the name typed
  (list (gt-eval-later sd-save-as)
        (gt-file-dialog (string-append "folder " sd-dir))
        (gt-idle)
        (gt-file-dialog (string-append "name " name))
        (gt-idle)
        (gt-file-dialog "press Save")
        (gt-idle)))

(define (sd-close)
  ;; what a failure left open, so that the next steps run
  (when (gt-file-dialog-shown?)
    (display* "closing: " (gui-test-file-dialog) "\n"))
  (when (gt-file-dialog-has? "" "alert-button")
    (gui-test-file-dialog-do "press Cancel"))
  (when (gt-file-dialog-shown?)
    (gui-test-file-dialog-do "press Cancel")))

(gui-test-run
 (append
  (list (gt-wait 6000)
        (gt-file-dialogs)
        (gt-window "main")
        (gt-eval (lambda ()
                   (system-mkdir (system->url sd-dir))
                   (string-save "keep" (sd-file "test"))
                   (string-save "old" (sd-file "test2.tm"))
                   (new-buffer)
                   (insert sd-text)))
        (gt-idle))
  ;; a name without a suffix, where a file has that name
  (sd-save-as-steps "test")
  (list (gt-check "no alert about the file without a suffix"
                  (lambda () (not (gt-file-dialog-has? "" "alert-label"))))
        (gt-eval sd-close)
        (gt-file-dialog-closed)
        (gt-idle)
        (gt-check "saved as test.tm"
                  (lambda () (string-contains? (sd-content "test.tm") sd-text)))
        (gt-check "test is left as it was"
                  (lambda () (== (sd-content "test") "keep"))))
  ;; a name whose .tm exists: the alert, Cancel
  (sd-save-as-steps "test2")
  (list (gt-file-dialog-see "already exists")
        (gt-check "the alert is about test2.tm"
                  (lambda () (gt-file-dialog-has? "test2.tm" "alert-label")))
        (gt-file-dialog "press Cancel")
        (gt-idle)
        (gt-check "Cancel in the alert goes back to the dialog"
                  (lambda () (and (gt-file-dialog-shown?)
                                  (not (gt-file-dialog-has? "" "alert-label")))))
        (gt-eval sd-close)
        (gt-file-dialog-closed)
        (gt-idle)
        (gt-check "test2.tm is kept"
                  (lambda () (== (sd-content "test2.tm") "old"))))
  ;; again, Replace
  (sd-save-as-steps "test2")
  (list (gt-file-dialog-see "already exists")
        (gt-file-dialog "press Replace")
        (gt-file-dialog-closed)
        (gt-idle)
        (gt-check "test2.tm is replaced"
                  (lambda () (string-contains? (sd-content "test2.tm") sd-text)))
        (gt-eval sd-close)
        (gt-exit))))
