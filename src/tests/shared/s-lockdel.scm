;; The lock file is removed under a host. If nobody has made a new one the
;; host makes it again; if another process has, that one hosts, and the first
;; stops, saves its document, and joins it.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (lock-exists?) (url-exists? (string->url (shared-lock-path (the-name)))))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "delete the lock" (check "A hosts" (== (role-of) 'host))
        (system-remove (string->url (shared-lock-path (the-name)))))
   (timed-wait 1800)
   (act "check" (check "the host made the lock again" (and (lock-exists?) (>= (shared-stat 'lock-renewed) 1)))
        (check "and still hosts" (== (role-of) 'host))
        (sync-set! "a-renewed"))
   (wait-sync "b-joined")
   (act "check" (check "B joins the host, with the new lock" (= (peers-count) 1)))
   (wait-sync "b-left")
   ;; the lock is removed again, and, meanwhile, another process hosts
   (act "pause, delete, edit" (shared-pause! #t)
        (system-remove (string->url (shared-lock-path (the-name))))
        (insert-text! 1 5 "AA")
        (sync-set! "a-deleted"))
   (wait-sync "b-hosts")
   (act "resume" (shared-pause! #f))
   (step "A stops hosting" (== (role-of) 'participant))
   (act "check" (check "A knew that it lost the lock" (>= (shared-stat 'lock-lost) 1))
        (check "A's document is saved in the recovered folder"
               (list-or (map (lambda (f) (string-contains? (string-load (string->url f)) "HelloAA"))
                             (recovered-files))))
        (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-renewed")
   (act "load" (load-buffer FILE) (sync-set! "b-joined"))
   (timed-wait 400)
   (act "leave" (buffer-close (the-name)) (sync-set! "b-left"))
   (wait-sync "a-deleted")
   (act "load" (load-buffer FILE))
   (step "B hosts from the file" (== (role-of) 'host))
   (act "signal" (sync-set! "b-hosts"))
   (wait-sync "a-done")
   (step "A is a participant of B" (= (peers-count) 1))
   (act "check" (check "B is the only host" (== (text-of 1) "Hello world."))
        (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
