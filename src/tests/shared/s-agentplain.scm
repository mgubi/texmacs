;; Patches of a participant that is not an agent are not filtered and not
;; turned into proposals, whatever the mode: the person's own second window,
;; another TeXmacs of the person.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "interactive" (shared-set-interactive! #t))
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (act "no agents" (check "no agent on the file" (null? (shared-file-agents (the-name)))))
   (wait-sync "b-edited")
   (step "A sees" (string-contains? (object->string (body-stree)) "javascript"))
   (act "check" (check "the link is applied, as the person's own" #t)
        (check "no proposal, no refusal" (and (null? (shared-proposals (the-name))) (= (shared-stat 'agent-refused) 0)
                                               (= (shared-stat 'agent-proposed) 0)))
        (check "the gate did not run" (= (car (shared-file-gate-stats (the-name))) 0))
        (publish-body!) (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "participant" (check "B participates" (== (role-of) 'participant)))
   (act "sig" (sync-set! "b-joined"))
   (act "edit" (tree-var-insert (body) 4 (stree->tree '(document (hlink "x" "javascript:alert(1)")))) (insert-text! 1 5 "AA"))
   (step "sent" (and (== (pending-count) 0) (> (shared-stat 'submitted) 0)))
   (act "sig" (sync-set! "b-edited"))
   (wait-sync "a-done")
   (act "out" (publish-body!) (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
