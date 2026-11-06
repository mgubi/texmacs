;; Two processes whose TMPDIR and XDG_RUNTIME_DIR differ (check.sh gives them
;; different ones) share the files all the same: the folder is one for the user.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE) (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (act "check" (check "the folder does not depend on the environment" (and (== (role-of) 'host) (= (peers-count) 1))))
   (wait-sync "b-done")
   (act "clean up" (buffer-close (the-name))
        (system-remove (string->url (shared-entry-path (the-name))))
        (system-remove (string->url (shared-lock-path (the-name)))))))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "check" (check "B joins A" (== (role-of) 'participant))
        (check "the folder is /tmp/texmacs-<uid>/shared"
               (string-contains? (shared-entry-path (the-name)) "/tmp/texmacs-"))
        (sync-set! "b-joined"))
   (timed-wait 500)
   (act "done" (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
