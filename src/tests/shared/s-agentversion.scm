;; The person (the host A, interactive) resolves the proposals of an agent (B)
;; with the Version tools (the Review bar: the cursor in a difference, Keep
;; Old or New), not with Tools > Agents: the host finds out which version was
;; kept and the agent hears accepted or rejected (#867). An insertion (its old
;; version is empty) and a deletion (its new version is empty) as well, next
;; to equal blocks; an undo brings the markup back and the proposal is
;; pending again (also an undone withdrawal).
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

(define (texts) (map version-normalize (cdr (body-stree))))

(define (a-round n id which expected . checks)
  ;; wait for the proposal @id of the agent, keep the version @which with the
  ;; Review bar, check that it is @expected, and the @checks (name thunk)
  (list
   (wait-sync (sync-n n "b"))
   (step (sync-n n "pending") (== (status-of id) "pending"))
   (act (sync-n n "keep") (check (string-append id ": Keep " (if (= which 1) "New" "Old")) (keep! which)))
   (step (sync-n n "settled") (and (not (version-in-doc?)) (!= (status-of id) "pending")))
   (act (sync-n n "check")
        (check (string-append id ": " expected) (== (status-of id) expected))
        (for-each (lambda (c) (check (string-append id ": " (car c)) ((cadr c)))) checks)
        (sync-set! (sync-n n "a")))))

(define (person! n edit!)
  ;; an edit of the person, then the agent may go on
  (list (act (sync-n n "person") (with-buffer (the-name) (edit!)) (sync-set! (sync-n n "p")))))

(define (A-steps)
  (append
   (list
    (act "interactive" (shared-set-interactive! #t))
    (act "load" (load-buffer FILE))
    (act "host" (check "A hosts" (== (role-of) 'host)))
    (act "go" (switch-to-buffer (the-name)) (sync-set! "a-loaded"))
    (wait-sync "b-joined")
    (step "agent listed" (= (length (shared-file-agents (the-name))) 1)))
   ;; 1. a word changed (word by word markup), kept New: accepted (the
   ;; Version tools leave the text in several strings)
   (a-round 1 "p1" 1 "accepted"
            (list "the new text" (lambda () (== (version-normalize (para-text 1)) "Hello big world."))))
   ;; 2. an inserted paragraph, kept Old: rejected
   (a-round 2 "p2" 0 "rejected"
            (list "the document is as before" (lambda () (== (texts) '((section "Intro") "Hello big world." "Second paragraph." "Third one."))))
            (list "p1 is still accepted" (lambda () (== (status-of "p1") "accepted"))))
   ;; 3. an insertion kept New, undone (pending again), kept Old: rejected
   (a-round 3 "p3" 1 "accepted"
            (list "the paragraph is in" (lambda () (== (para-text 3) "Inserted one."))))
   (list
    (act "undo 3" (with-buffer (the-name) (undo 0)))
    (step "markup back" (version-in-doc?))
    (step "p3 pending" (== (status-of "p3") "pending"))
    (act "check 3b" (check "the undo makes the proposal pending again" (== (status-of "p3") "pending"))))
   (a-round 31 "p3" 0 "rejected"
            (list "the document is as before" (lambda () (== (texts) '((section "Intro") "Hello big world." "Second paragraph." "Third one.")))))
   ;; 4. a deleted paragraph kept New: accepted
   (a-round 4 "p4" 1 "accepted"
            (list "the paragraph is gone" (lambda () (== (texts) '((section "Intro") "Hello big world." "Second paragraph.")))))
   ;; 5. the deletion of a paragraph equal to its neighbour, kept Old: rejected
   (person! 5 (lambda () (insert-block! 3 "Second paragraph.") (insert-block! 4 "Third one.")))
   (a-round 5 "p5" 0 "rejected"
            (list "both paragraphs are there" (lambda () (== (texts) '((section "Intro") "Hello big world." "Second paragraph." "Second paragraph." "Third one.")))))
   ;; 6. two empty paragraphs: an insertion between them kept Old, the
   ;; deletion of one kept Old, then New
   (person! 6 (lambda () (insert-block! 5 "") (insert-block! 6 "") (insert-block! 7 "End.")))
   (a-round 6 "p6" 0 "rejected"
            (list "the empty paragraphs are there" (lambda () (== (list-tail (texts) 5) '("" "" "End.")))))
   (a-round 61 "p7" 0 "rejected"
            (list "the empty paragraphs are there" (lambda () (== (list-tail (texts) 5) '("" "" "End.")))))
   (a-round 62 "p8" 1 "accepted"
            (list "one empty paragraph is gone" (lambda () (== (list-tail (texts) 5) '("" "End.")))))
   ;; 7. the insertion of a paragraph equal to the one before, kept Old, then New
   (a-round 7 "p9" 0 "rejected"
            (list "one paragraph" (lambda () (== (list-tail (texts) 4) '("Third one." "" "End.")))))
   (a-round 71 "p10" 1 "accepted"
            (list "two paragraphs" (lambda () (== (list-tail (texts) 4) '("Third one." "Third one." "" "End.")))))
   ;; 8. a proposal withdrawn by the agent, the withdrawal undone by the
   ;; person: pending again
   (list
    (wait-sync (sync-n 8 "b"))
    (step "p11 withdrawn" (== (status-of "p11") "withdrawn"))
    (act "undo 8" (with-buffer (the-name) (undo 0)))
    (step "markup back 8" (version-in-doc?))
    (step "p11 pending" (== (status-of "p11") "pending"))
    (act "check 8" (check "an undone withdrawal: pending again" (== (status-of "p11") "pending"))
         (sync-set! (sync-n 8 "a"))))
   (a-round 81 "p11" 0 "rejected")
   (list
    (wait-sync "b-done")
    (act "A out" (publish-body!) (sync-set! "a-done")))))

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

(define (round n edit! id status)
  (append (propose-and-wait n edit!) (agent-hears n id status)))

(define (B-steps)
  (append
   (list
    (wait-sync "a-loaded")
    (act "agent" (shared-set-agent! #t))
    (act "load" (load-buffer FILE))
    (act "participant" (check "B participates" (== (role-of) 'participant)))
    (step "mode" (eq? (shared-agent-mode (the-name)) 'propose))
    (act "sig" (sync-set! "b-joined")))
   (round 1 (lambda () (insert-text! 1 6 "big ")) "p1" "accepted")
   (round 2 (lambda () (insert-block! 3 "New paragraph.")) "p2" "rejected")
   (round 3 (lambda () (insert-block! 3 "Inserted one.")) "p3" "accepted")
   (list (step "p3 pending again" (== (status-of "p3") "pending"))
         (act "31b" (sync-set! (sync-n 31 "b"))))
   (agent-hears 31 "p3" "rejected")
   (round 4 (lambda () (tree-remove (body) 3 1)) "p4" "accepted")
   (list (wait-sync (sync-n 5 "p")) (step "5 there" (= (para-count) 5)))
   (round 5 (lambda () (tree-remove (body) 3 1)) "p5" "rejected")
   (list (wait-sync (sync-n 6 "p")) (step "6 there" (= (para-count) 8)))
   (round 6 (lambda () (insert-block! 6 "Between.")) "p6" "rejected")
   (round 61 (lambda () (tree-remove (body) 6 1)) "p7" "rejected")
   (round 62 (lambda () (tree-remove (body) 6 1)) "p8" "accepted")
   (round 7 (lambda () (insert-block! 5 "Third one.")) "p9" "rejected")
   (round 71 (lambda () (insert-block! 5 "Third one.")) "p10" "accepted")
   (propose-and-wait 8 (lambda () (insert-text! 2 6 ",")))
   (list
    (act "withdraw" (check "withdraw sent" (shared-file-send-withdraw (the-name) "p11")))
    (step "withdrawn" (and (== (status-of "p11") "withdrawn") (not (version-in-doc?)))))
   (agent-hears 8 "p11" "pending")
   (list
    (act "81b" (sync-set! (sync-n 81 "b"))))
   (agent-hears 81 "p11" "rejected")
   (list
    (act "out" (publish-body!) (sync-set! "b-done"))
    (wait-sync "a-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
