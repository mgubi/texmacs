;; A participant that does not say it is an agent but is headless is treated
;; as one (the host does not rely on the id the participant chose); the person
;; can mark it as a person (shared-treat-as-person!, Tools > Agents), and then
;; its patches are not filtered nor turned into proposals.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

;; (the person refuses at once what the host asks about: a link of a scheme that is not accepted)
(define (auto-refuse!)
  (exec-delayed-pause
   (lambda ()
     (for (q (or (shared-asks (the-name)) '()))
       (shared-ask-answer! (the-name) (car q) "refuse"))
     20)))

(define (A-steps)
  (list
   (act "interactive" (shared-set-interactive! #t))
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded") (auto-refuse!))
   (wait-sync "b-joined")
   (step "listed" (= (length (shared-file-agents (the-name))) 1))
   (act "check 0" (check "the headless participant is taken for an agent although its id does not say so"
                         (not (shared-agent-id? (car (shared-file-agents (the-name))))))
        (check "and the person is told" (= (shared-stat 'unmarked-joined) 1)))
   (wait-sync "b-refused")
   (act "check 1" (check "its javascript link was refused" (not (string-contains? (object->string (body-stree)) "javascript")))
        (check "counted" (= (shared-stat 'agent-refused) 1))
        (check "marked as a person" (shared-treat-as-person! (the-name) (car (shared-file-agents (the-name))) #t))
        (sync-set! "a-marked"))
   (wait-sync "b-edited")
   (step "A sees" (string-contains? (object->string (body-stree)) "javascript"))
   (act "check 2" (check "the link of a person is applied" #t)
        (check "no proposal, no more refusal" (and (null? (shared-proposals (the-name))) (= (shared-stat 'agent-refused) 1)
                                                    (= (shared-stat 'agent-proposed) 0)))
        (check "a person's patch with a cell decoration is refused by the host too" (= (shared-stat 'arity-refused) 1))
        (publish-body!) (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "participant" (check "B participates" (== (role-of) 'participant)))
   (act "sig" (sync-set! "b-joined"))
   (act "edit" (tree-var-insert (body) 4 (stree->tree '(document (hlink "x" "javascript:alert(1)")))))
   (step "refused" (> (shared-stat 'rolled-back) 0))
   (act "v" (check "refused with the rule" (let ((v (shared-file-verdict (the-name)))) (and v (eq? (car v) 'refused)))))
   (step "clean" (and (== (para-count) 4) (== (pending-count) 0)))
   (act "sig" (sync-set! "b-refused"))
   (wait-sync "a-marked")
   (act "edit" (tree-var-insert (body) 4 (stree->tree '(document (hlink "x" "javascript:alert(1)")))) (insert-text! 1 5 "AA"))
   (step "sent" (and (== (pending-count) 0) (> (shared-stat 'submitted) 1)))
   ;; what a person (a participant marked so) may not do either: a cell decoration hangs the typesetter
   (act "bad table" (tree-var-insert (body) (para-count) (stree->tree '(document (tformat (cwith "1" "1" "1" "1" "cell-decoration" "x") (table (row (cell "a"))))))))
   (step "taken back" (and (>= (shared-stat 'rolled-back) 2) (== (pending-count) 0)))
   (act "sig" (sync-set! "b-edited"))
   (wait-sync "a-done")
   (act "out" (publish-body!) (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
