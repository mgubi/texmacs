;; The host deletes the paragraph which the participant edits: the patch of
;; the participant is refused (it is told, and its edit is undone); the other
;; way round, the participant drops its edit when it hears of the deletion.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   ;; round 1: the participant is first
   (act "pause, delete" (shared-pause! #t) (tree-remove (body) 2 1) (sync-set! "a-ready"))
   (wait-sync "b-submitted")
   (act "resume" (shared-pause! #f))
   (step "refused" (>= (shared-stat 'refused) 1))
   (act "check 1" (check "host refused the patch" (== (shared-stat 'refused) 1))
        (check "host's own deletion kept" (== (para-count) 3))
        (sync-set! "a-round1"))
   ;; round 2: the host is first
   (wait-sync "b-ready2")
   (act "delete" (tree-remove (body) 2 1) (sync-set! "a-deleted2"))
   (wait-sync "b-done2")
   (act "out" (publish-body!) (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "b-joined"))
   (wait-sync "a-ready")
   (act "pause, edit" (shared-pause! #t) (insert-text! 2 3 "XX"))
   (act "resume" (shared-pause! #f))
   (step "submitted" (>= (shared-stat 'submitted) 1))
   (act "sig" (sync-set! "b-submitted"))
   (step "dropped" (>= (shared-stat 'dropped) 1))
   (wait-sync "a-round1")
   (step "in sync" (and (== (para-count) 3) (== (pending-count) 0)))
   (act "check 1" (check "participant's edit undone" (== (text-of 2) "Third one."))
        (check "participant dropped it when it heard of the deletion"
               (== (shared-stat 'dropped) 1)))
   ;; round 2: edit the paragraph and let the host's deletion arrive first
   (act "pause, edit" (shared-pause! #t) (insert-text! 2 3 "YY") (sync-set! "b-ready2"))
   (wait-sync "a-deleted2")
   (act "resume" (shared-pause! #f))
   (step "dropped" (>= (shared-stat 'dropped) 2))
   (step "in sync" (and (== (para-count) 2) (== (pending-count) 0)))
   (act "check 2" (check "participant dropped its edit again" (== (shared-stat 'dropped) 2))
        (sync-set! "b-done2"))
   ;; round 3: the host refuses a patch which has a state it does not know
   (act "bad base" (shared-test-bad-base! (the-name) #t) (insert-text! 1 2 "ZZ"))
   (step "rolled back" (>= (shared-stat 'rolled-back) 1))
   (act "check 3" (check "refused patch rolled back by the participant"
                         (and (== (text-of 1) "Hello world.") (== (pending-count) 0))))
   (wait-sync "a-done")
   (act "out" (publish-body!) (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
