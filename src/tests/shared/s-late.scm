;; Hosting moves to an interactive process which then stalls. Only the lock
;; decides who hosts: the old host, a participant of the new one, gets the
;; lock back when the target does not take it; its changes are kept; when the
;; target is back it is offered the file again and gets it. At no time are
;; there two hosts.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "headless" (shared-set-interactive! #f))
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-paused")
   (timed-wait 2500)
   (act "A hosts again and works" (check "A got the lock back while B stalls" (== (role-of) 'host))
        (insert-text! 1 5 "AA") (sync-set! "a-edited"))
   (wait-sync "b-resumed")
   (timed-wait 7000)
   (act "check" (check "after the retry the interactive process hosts" (== (role-of) 'participant))
        (check "A's change is kept" (== (text-of 1) "HelloAA world."))
        (check "the entry is B's" (not (== (list-ref (shared-entry (the-name)) 4) (shared-process-id))))
        (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (act "interactive" (shared-set-interactive! #t))
   (wait-sync "a-loaded")
   (act "load and stop" (load-buffer FILE) (shared-pause! #t) (sync-set! "b-paused"))
   (wait-sync "a-edited")
   (timed-wait 3000)
   (act "resume" (shared-pause! #f) (sync-set! "b-resumed"))
   (wait-sync "a-done")
   (act "check" (check "B hosts at the end, once" (and (== (role-of) 'host) (= (peers-count) 1)))
        (check "B has A's change" (== (text-of 1) "HelloAA world."))
        (check "nothing lost" (== (shared-stat 'lost) 0))
        (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
