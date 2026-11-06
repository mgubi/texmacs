;; Both sides edit at once (different paragraphs): the patch of the participant,
;; made against an older state, is rebased by the host.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (act "pause, edit" (shared-pause! #t) (insert-text! 1 5 "AA") (sync-set! "a-ready"))
   (wait-sync "b-submitted")
   (act "resume" (shared-pause! #f))
   (step "converged" (and (== (text-of 1) "HelloAA world.") (== (text-of 3) "ThirdBB one.")
                          (>= (shared-stat 'rebased) 1)))
   (act "check" (check "host rebased the patch of the participant" (== (shared-stat 'rebased) 1))
        (check "nothing refused" (== (shared-stat 'refused) 0)))
   (act "out" (publish-body!) (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "b-joined"))
   (wait-sync "a-ready")
   (act "pause, edit" (shared-pause! #t) (insert-text! 3 5 "BB"))
   (act "resume" (shared-pause! #f))
   (step "submitted" (>= (shared-stat 'submitted) 1))
   (act "sig" (sync-set! "b-submitted"))
   (step "converged" (and (== (text-of 1) "HelloAA world.") (== (text-of 3) "ThirdBB one.")
                          (== (pending-count) 0)))
   (act "check" (check "participant transformed the patch of the host"
                       (>= (shared-stat 'transformed) 1)))
   (wait-sync "a-done")
   (act "out" (publish-body!) (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
