;; The host closes the file and opens it again at once, while its participant
;; (which has confirmed work that is not on the disk) has not yet taken the
;; lock: it must not host from the disk, but wait for the participant to host,
;; and join it. The buffer is read-only meanwhile.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE) (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (step "A has B's work" (== (text-of 1) "HelloBB world."))
   (act "sig" (sync-set! "a-has"))
   (wait-sync "b-paused")
   (act "close and open again at once" (buffer-close (the-name)) (load-buffer FILE)
        (check "the file is read-only while the other host is awaited" (buffer-read-only? (the-name)))
        (check "and not hosted from the disk" (not (shared-file? (the-name))))
        (sync-set! "a-reopened"))
   (step "A joins the new host" (== (role-of) 'participant))
   (act "check" (check "A has the work of B" (== (text-of 1) "HelloBB world."))
        (check "and can edit again" (not (buffer-read-only? (the-name))))
        (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE) (sync-set! "b-joined"))
   (act "edit" (insert-text! 1 5 "BB"))
   (wait-sync "a-has")
   (act "pause" (shared-pause! #t) (sync-set! "b-paused"))
   (wait-sync "a-reopened")
   (timed-wait 400)
   (act "resume" (shared-pause! #f))
   (step "B hosts" (and (== (role-of) 'host) (= (peers-count) 1)))
   (act "check" (check "B has its work" (== (text-of 1) "HelloBB world.")))
   (wait-sync "a-done")
   (act "done" (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
