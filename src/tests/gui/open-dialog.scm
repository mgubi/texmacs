;; Load, through the file dialog of the system (macOS; skipped where the
;; port does not reach it): Cancel loads nothing; a document chosen in the
;; list by typing its name, then Open, is loaded. The buttons are those of
;; an English system. Runs on every port with the hooks of the tests
;; (docs/gui-scripting.md):
;;   tests/gui/run.sh open-dialog
(use-modules (check gui-driver))

(define od-dir (string-append (getenv "TEXMACS_HOME_PATH") "/dialog-files"))
(define (od-file name) (system->url (string-append od-dir "/" name)))
(define od-text "Opened through the dialog")
(define od-doc
  (string-append "<TeXmacs|2.1.4>\n\n<style|generic>\n\n<\\body>\n  "
                 od-text "\n</body>\n\n<initial|<\\collection>\n</collection>>"))

(define (od-loaded? name)
  (in? (od-file name) (buffer-list)))

(define (od-close)
  ;; what a failure left open, so that the next steps run
  (when (gt-file-dialog-shown?)
    (display* "closing: " (gui-test-file-dialog) "\n")
    (gui-test-file-dialog-do "press Cancel")))

(gui-test-run
 (list (gt-wait 6000)
       (gt-file-dialogs)
       (gt-window "main")
       (gt-eval (lambda ()
                  (system-mkdir (system->url od-dir))
                  (string-save od-doc (od-file "chosen.tm"))
                  (string-save od-doc (od-file "other.tm"))))
       (gt-idle)
       ;; Cancel
       (gt-eval-later "(open-buffer)")
       (gt-file-dialog (string-append "folder " od-dir))
       (gt-idle)
       (gt-file-dialog "press Cancel")
       (gt-file-dialog-closed)
       (gt-idle)
       (gt-check "Cancel loads nothing"
                 (lambda () (not (or (od-loaded? "chosen.tm")
                                     (od-loaded? "other.tm")))))
       ;; a document chosen by its name in the list
       (gt-eval-later "(open-buffer)")
       (gt-file-dialog (string-append "folder " od-dir))
       (gt-idle)
       (gt-file-dialog "type chosen")
       (gt-idle)
       (gt-file-dialog "press Open")
       (gt-file-dialog-closed)
       (gt-idle)
       (gt-check "chosen.tm is loaded" (lambda () (od-loaded? "chosen.tm")))
       (gt-check "and shown" (lambda () (gt-document-has? od-text)))
       (gt-check "other.tm is not" (lambda () (not (od-loaded? "other.tm"))))
       (gt-eval od-close)
       (gt-exit)))
