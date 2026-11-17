;; A file changed on disk (#818): the host's own saves do not make it
;; "changed on disk", for the host nor for the participant (the message
;; (saved) gives the participant the date of the file); a write by another
;; program is said by the host only (a participant never reads the file, and
;; its Revert would send the file to everybody); a Revert in the host reaches
;; the participant.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (file-date) (url-last-modified (the-name)))
(define (in-sync?) (== (buffer-last-save (the-name)) (file-date)))

(define outside
  (string-append "<TeXmacs|2.1.5>\n\n<style|generic>\n\n<\\body>\n"
                 "  Outside text.\n</body>\n\n<initial|<\\collection>\n</collection>>\n"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "host" (check "A hosts" (== (role-of) 'host)))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (timed-wait 1100)
   (act "edit and save" (insert-text! 1 5 "AA")
        ;; (headless, an edit of the tree does not mark the buffer modified)
        (buffer-pretend-modified (the-name))
        (save-buffer-main (the-name)))
   (step "A saved" (file-has? (the-name) "HelloAA"))
   (act "check"
        (check "the host's save is its own: the buffer has the date of the file" (in-sync?))
        (check "the host's save is not a change on disk"
               (not (buffer-changed-on-disk? (the-name))))
        (disk-check-forget)
        (check "no notice after the host's save" (not (disk-change-footer-text)))
        (sync-set! "a-saved"))
   (wait-sync "b-seen")
   ;; another program writes the file, with a later date (dates are in seconds)
   (timed-wait 1100)
   (act "write from outside" (string-save outside (the-name)))
   (step "the host sees the change" (buffer-changed-on-disk? (the-name)))
   (act "check"
        (disk-check-forget)
        (with s (disk-change-footer-text)
          (check "the host says it" (and (string? s) (string-contains? s "changed on disk"))))
        (sync-set! "a-external"))
   (wait-sync "b-checked")
   (act "revert" (revert-buffer-revert (the-name)))
   (act "check"
        (check "the host has the disk's version" (== (para-text 0) "Outside text."))
        (check "no change on disk after the revert" (not (buffer-changed-on-disk? (the-name))))
        (sync-set! "a-reverted"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "participant" (check "B participates" (== (role-of) 'participant)))
   (act "check" (check "B joined at the date of the file" (in-sync?))
        (sync-set! "b-joined"))
   (wait-sync "a-saved")
   ;; the message (saved) gives B the date of the file the host wrote
   (step "B hears of the save" (in-sync?))
   (act "check"
        (check "the host's save is not a change on disk for B"
               (not (buffer-changed-on-disk? (the-name))))
        (sync-set! "b-seen"))
   (wait-sync "a-external")
   (act "check"
        (check "the file is newer than B's last save" (> (file-date) (buffer-last-save (the-name))))
        (check "B does not say it: the host does"
               (not (buffer-changed-on-disk? (the-name))))
        (disk-check-forget)
        (check "no notice in B" (not (disk-change-footer-text)))
        (sync-set! "b-checked"))
   (wait-sync "a-reverted")
   (step "B gets the host's revert" (== (para-text 0) "Outside text."))
   (act "done" (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
