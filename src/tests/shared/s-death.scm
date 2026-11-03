;; The host dies (kill -9) while the participant has a change which the host
;; has not seen: the participant becomes the host from its own copy, and
;; keeps the change.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (act "edit" (insert-text! 1 5 "AA"))
   (wait-sync "b-edited")
   (act "die" (die!))))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "b-joined"))
   (step "A's edit" (== (text-of 1) "HelloAA world."))
   (act "edit while the host does not hear"
        (shared-pause! #t) (insert-text! 3 5 "BB") (sync-set! "b-edited"))
   (step "A is dead" (let ((e (shared-entry (the-name))))
                       (not (shared-process-alive? (list-ref e 4)))))
   (act "resume" (shared-pause! #f))
   (step "B is the host" (== (role-of) 'host))
   (step "committed" (== (pending-count) 0))
   (act "check" (check "B has both changes" (and (== (text-of 1) "HelloAA world.")
                                                  (== (text-of 3) "ThirdBB one.")))
        (check "entry points to B" (== (list-ref (shared-entry (the-name)) 4)
                                       (shared-process-id))))
   (act "save" (save-buffer-main (the-name)))
   (act "saved" (check "B wrote the file"
                       (and (string-contains? (string-load (the-name)) "HelloAA")
                            (string-contains? (string-load (the-name)) "ThirdBB"))))
   (act "out" (publish-body!))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
