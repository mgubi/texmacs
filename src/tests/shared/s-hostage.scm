;; Hosting moves to an interactive process which then stalls for longer than
;; the old host waits: the old host does not take its place back (two hosts
;; would write the file), and joins it when it answers again.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "headless" (shared-set-interactive! #f))
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-paused")
   (timed-wait 7000)
   (act "check" (check "A did not take the place of the stalled host"
                       (and (== (role-of) 'participant)
                            (not (== (list-ref (shared-entry (the-name)) 4) (shared-process-id)))))
        (sync-set! "a-waited"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (act "interactive" (shared-set-interactive! #t))
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (step "B hosts" (== (role-of) 'host))
   (act "pause" (shared-pause! #t) (sync-set! "b-paused"))
   (wait-sync "a-waited")
   (act "resume" (shared-pause! #f))
   (step "A is a participant of B again" (and (== (role-of) 'host) (= (peers-count) 1)))
   (act "check" (check "B is the only host" (== (list-ref (shared-entry (the-name)) 4) (shared-process-id)))
        (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
