;; The process asked to host is busy for longer than the host waits: it must
;; not become host afterwards (the host went on): there is one host.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "headless" (shared-set-interactive! #f))
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-paused")
   (timed-wait 6500)
   (act "waited" (sync-set! "a-waited"))
   (wait-sync "b-resumed")
   (timed-wait 2500)
   (act "check" (check "A is still the host" (== (role-of) 'host))
        (check "the entry is A's" (== (list-ref (shared-entry (the-name)) 4) (shared-process-id)))
        (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (act "interactive" (shared-set-interactive! #t))
   (wait-sync "a-loaded")
   (act "load and stop" (load-buffer FILE) (shared-pause! #t) (sync-set! "b-paused"))
   (wait-sync "a-waited")
   (act "resume" (shared-pause! #f) (sync-set! "b-resumed"))
   (wait-sync "a-done")
   (act "check" (check "B did not become a second host" (== (role-of) 'participant))
        (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
