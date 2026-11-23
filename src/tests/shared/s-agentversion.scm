;; The person (the host A, interactive) resolves the proposals of an agent (B)
;; with the Version tools (the Review bar: the cursor in a difference, Keep
;; Old or New), not with Tools > Agents: the host finds out which version was
;; kept and the agent hears accepted or rejected (#867). An insertion (its old
;; version is empty) and a deletion (its new version is empty) as well; an
;; undo brings the markup back and the proposal is pending again.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (has-version? st)
  (and (pair? st)
       (or (and (memq (car st) '(version-both version-old version-new)) #t)
           (list-or (map has-version? (cdr st))))))
(define (version-in-doc?) (has-version? (body-stree)))
(define (sync-n n s) (string-append "r" (number->string n) "-" s))
(define (insert-block! i st)
  (tree-var-insert (body) i (stree->tree (list 'document st))))

(define (status-of id)
  (with l (list-filter (or (shared-proposals (the-name)) '()) (lambda (e) (== (car e) id)))
    (and (nnull? l) (cadr (car l)))))

(define (first-version t)
  (cond ((tree-atomic? t) #f)
        ((version-context? t) t)
        (else (list-or (map first-version (tree-children t))))))

(define (keep! which)
  ;; the Review bar of Version > Differences: the cursor in the first
  ;; difference, then Keep Old (0) or New (1)
  (with-buffer (the-name)
    (let ((vt (first-version (body))))
      (and vt
           (begin
             (tree-go-to vt (if (tree-is? vt 'version-old) 0 1) :end)
             (and (inside-version?)
                  (begin
                    (start-editing)
                    (version-retain which)
                    (end-editing)
                    #t)))))))

(define (A-steps)
  (list
   (act "interactive" (shared-set-interactive! #t))
   (act "load" (load-buffer FILE))
   (act "host" (check "A hosts" (== (role-of) 'host)))
   (act "go" (switch-to-buffer (the-name)) (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (step "agent listed" (= (length (shared-file-agents (the-name))) 1))
   ;; 1. a word changed (word by word markup), kept New: accepted, one string
   (wait-sync (sync-n 1 "b"))
   (step "p1" (== (status-of "p1") "pending"))
   (act "keep new" (check "Keep New in the difference" (keep! 1)))
   (step "p1 settled" (and (not (version-in-doc?)) (!= (status-of "p1") "pending")))
   (act "check 1" (check "kept New with the Version tools: accepted" (== (status-of "p1") "accepted"))
        (check "the paragraph is one string" (== (para-text 1) "Hello big world."))
        (sync-set! (sync-n 1 "a")))
   ;; 2. an inserted paragraph, kept Old: rejected
   (wait-sync (sync-n 2 "b"))
   (step "p2" (== (status-of "p2") "pending"))
   (act "keep old" (check "Keep Old in the difference" (keep! 0)))
   (step "p2 settled" (and (not (version-in-doc?)) (!= (status-of "p2") "pending")))
   (act "check 2" (check "an insertion kept Old: rejected" (== (status-of "p2") "rejected"))
        (check "the document is as before" (and (= (para-count) 4) (== (para-text 3) "Third one.")))
        (check "p1 is still accepted" (== (status-of "p1") "accepted"))
        (sync-set! (sync-n 2 "a")))
   ;; 3. an insertion kept New, undone, kept Old: pending again, then rejected
   (wait-sync (sync-n 3 "b"))
   (step "p3" (== (status-of "p3") "pending"))
   (act "keep new 3" (check "Keep New" (keep! 1)))
   (step "p3 settled" (and (not (version-in-doc?)) (!= (status-of "p3") "pending")))
   (act "check 3" (check "an insertion kept New: accepted" (== (status-of "p3") "accepted"))
        (check "the paragraph is in" (== (para-text 3) "Inserted one.")))
   (act "undo 3" (with-buffer (the-name) (undo 0)))
   (step "markup back" (version-in-doc?))
   (step "p3 pending" (== (status-of "p3") "pending"))
   (act "check 3b" (check "the undo makes the proposal pending again" (== (status-of "p3") "pending")))
   (act "keep old 3" (check "Keep Old" (keep! 0)))
   (step "p3 settled again" (and (not (version-in-doc?)) (!= (status-of "p3") "pending")))
   (act "check 3c" (check "then kept Old: rejected" (== (status-of "p3") "rejected"))
        (check "the document is as before" (and (= (para-count) 4) (== (para-text 3) "Third one.")))
        (sync-set! (sync-n 3 "a")))
   ;; 4. a deleted paragraph kept New: accepted
   (wait-sync (sync-n 4 "b"))
   (step "p4" (== (status-of "p4") "pending"))
   (act "keep new 4" (check "Keep New" (keep! 1)))
   (step "p4 settled" (and (not (version-in-doc?)) (!= (status-of "p4") "pending")))
   (act "check 4" (check "a deletion kept New: accepted" (== (status-of "p4") "accepted"))
        (check "the paragraph is gone" (= (para-count) 3))
        (sync-set! (sync-n 4 "a")))
   (wait-sync "b-done")
   (act "A out" (publish-body!) (sync-set! "a-done"))))

(define (verdict-kind) (let ((v (shared-file-verdict (the-name)))) (and v (car v))))
(define rb0 0)

(define (propose-and-wait n edit!)
  ;; edit, wait for the host's verdict (proposed: the edit is taken back) and
  ;; for the markup
  (list (act (sync-n n "edit") (set! rb0 (shared-stat 'rolled-back)) (edit!))
        (step (sync-n n "verdict") (and (> (shared-stat 'rolled-back) rb0) (eq? (verdict-kind) 'proposed)))
        (step (sync-n n "markup") (version-in-doc?))
        (act (sync-n n "b") (sync-set! (sync-n n "b")))))

(define (agent-hears n id status)
  (list (wait-sync (sync-n n "a"))
        (step (sync-n n "hear") (== (status-of id) status))
        (act (sync-n n "heard") (check (string-append "the agent hears: " id " " status) #t))))

(define (B-steps)
  (append
   (list
    (wait-sync "a-loaded")
    (act "agent" (shared-set-agent! #t))
    (act "load" (load-buffer FILE))
    (act "participant" (check "B participates" (== (role-of) 'participant)))
    (step "mode" (eq? (shared-agent-mode (the-name)) 'propose))
    (act "sig" (sync-set! "b-joined")))
   (propose-and-wait 1 (lambda () (insert-text! 1 6 "big ")))
   (agent-hears 1 "p1" "accepted")
   (propose-and-wait 2 (lambda () (insert-block! 3 "New paragraph.")))
   (agent-hears 2 "p2" "rejected")
   (propose-and-wait 3 (lambda () (insert-block! 3 "Inserted one.")))
   (agent-hears 3 "p3" "rejected")
   (propose-and-wait 4 (lambda () (tree-remove (body) 3 1)))
   (agent-hears 4 "p4" "accepted")
   (list
    (act "out" (publish-body!) (sync-set! "b-done"))
    (wait-sync "a-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
