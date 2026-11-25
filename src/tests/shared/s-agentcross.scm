;; Proposals of an agent (B) next to changes of the person (the host A)
;; and undo (#957, third audit of #960): an undo of an unrelated change
;; leaves a resolved proposal resolved and the others pending in their
;; blocks; a withdrawal after it takes nothing else away; undo and redo of
;; two resolutions never give a confident wrong answer; a change of the
;; agent (or of a person) that crosses a Keep in another block, or in the
;; same block, is not lost.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (has-version? st)
  (and (pair? st)
       (or (and (memq (car st) '(version-both version-old version-new)) #t)
           (list-or (map has-version? (cdr st))))))
(define (version-in-doc?) (has-version? (body-stree)))
(define (sync-n n s) (string-append "r" (number->string n) "-" s))
(define (insert-block! i st)
  (tree-var-insert (body) i (stree->tree (list 'document st))))

(define (entry-of id)
  (with l (list-filter (or (shared-proposals (the-name)) '()) (lambda (e) (== (short-id (car e)) id)))
    (and (nnull? l) (car l))))
(define (index-of id) (and-with e (entry-of id) (caddr e)))
(define (status-of id) (and-with e (entry-of id) (cadr e)))

(define (first-version t)
  (cond ((tree-atomic? t) #f)
        ((version-context? t) t)
        (else (list-or (map first-version (tree-children t))))))

(define (keep-in! i which)
  ;; the Review bar: the cursor in the first difference of block @i, Keep
  ;; Old (0) or New (1)
  (with-buffer (the-name)
    (let ((vt (and i (>= i 0) (first-version (para i)))))
      (and vt
           (begin
             (tree-go-to vt (if (tree-is? vt 'version-old) 0 1) :end)
             (and (inside-version?)
                  (begin (start-editing) (version-retain which) (end-editing) #t)))))))

(define (undo!) (with-buffer (the-name) (undo 0)) #t)
(define (step-undo!)
  ;; an undo as the event loop makes it: a step of its own
  (with-buffer (the-name) (archive-state) (start-editing) (undo 0) (end-editing))
  #t)
(define (keep-step! id which)
  (with-buffer (the-name) (archive-state))
  (keep-in! (index-of id) which))
(define (undo-until! s)
  ;; undo, one step at a time, until the paragraph @s is there (at most 4;
  ;; an undo of a change of the agent goes on to the previous step of the
  ;; person, which the checks do not look at)
  (let ((n 0))
    (cons (string-append "undo until " s)
          (lambda ()
            (cond ((find-para s) #t)
                  ((>= n 4) #t)
                  (else (set! n (+ n 1)) (step-undo!) #f))))))
(define (redo!) (with-buffer (the-name) (redo 0)) #t)

(define (texts) (map version-normalize (cdr (body-stree))))
(define (find-para s)
  (let loop ((i 0))
    (cond ((>= i (para-count)) #f)
          ((== (version-normalize (para-text i)) s) i)
          (else (loop (+ i 1))))))
(define (holds-markup? id)
  ;; the proposal @id is pending in a block with version markup
  (and (== (status-of id) "pending")
       (with i (index-of id)
         (and i (>= i 0) (has-version? (para-text i))))))

(define (person! n edit!)
  ;; an edit of the person, then the agent may go on
  (list (act (sync-n n "person") (with-buffer (the-name) (edit!)) (sync-set! (sync-n n "p")))))

(define (step-redo!)
  (with-buffer (the-name) (archive-state) (start-editing) (redo 0) (end-editing))
  #t)
(define (undos n)
  (map (lambda (k) (act (string-append "undo " (number->string k)) (step-undo!))) (iota n)))
(define (redos n)
  (map (lambda (k) (act (string-append "redo " (number->string k)) (step-redo!))) (iota n)))

(define (markup-blocks)
  (let loop ((i 0) (acc '()))
    (if (>= i (para-count)) (reverse acc)
        (loop (+ i 1) (if (has-version? (para-text i)) (cons i acc) acc)))))
(define (consistent? ids)
  ;; every pending proposal has markup at its index, every block with markup
  ;; is that of a pending proposal
  (let* ((pend (list-filter ids (lambda (id) (== (status-of id) "pending"))))
         (idx (map index-of pend)))
    (and (list-and (map holds-markup? pend))
         (list-and (map (lambda (i) (in? i idx)) (markup-blocks))))))
(define (type! s c)
  (with-buffer (the-name) (archive-state) (start-editing)
    (tree-assign (para (find-para s)) (string-append c s)) (end-editing))
  #t)
(define (consistent-step tag ids)
  (list (timed-wait 600)
        (act tag (check (string-append tag ": consistent") (consistent? ids)))))
(define (text-has? i s)
  (and i (>= i 0) (>= (string-search-forwards s 0 (object->string (para-text i))) 0)))
(define refused0 0)

(define (A-steps)
  (append
   (list
    (act "interactive" (shared-set-interactive! #t))
    (act "load" (load-buffer FILE))
    (act "host" (check "A hosts" (== (role-of) 'host)))
    (act "go" (switch-to-buffer (the-name)) (sync-set! "a-loaded"))
    (wait-sync "b-joined")
    (step "agent listed" (= (length (shared-file-agents (the-name))) 1)))
   ;; 1. a word change and an insertion after it; Keep Old on the insertion,
   ;; a word typed elsewhere and undone: the insertion stays rejected, the
   ;; word change pending; Tools > Agents cannot resolve the insertion again
   (list
    (wait-sync (sync-n 1 "b"))
    (step "p1 p2" (and (== (status-of "p1") "pending") (== (status-of "p2") "pending")))
    (act "keep 1" (check "p2: Keep Old" (keep-step! "p2" 0)))
    (step "settled 1" (== (status-of "p2") "rejected"))
    (act "type 1" (type! "Third one." "T"))
    (timed-wait 600)
    (act "undo 1" (step-undo!))
    (timed-wait 600)
    (act "check 1" (check "p2: still rejected after the undo of the typing" (== (status-of "p2") "rejected"))
         (check "p1: pending in its block" (holds-markup? "p1"))
         (check "consistent" (consistent? '("p1" "p2"))))
    (act "redo 1" (step-redo!))
    (timed-wait 600)
    (act "check 1b" (check "p2: Tools > Agents does not resolve it again"
                           (not (shared-proposal-resolve! (the-name) (prop-id "p2") 'new)))
         (check "p1: still pending in its block" (holds-markup? "p1"))
         (sync-set! (sync-n 1 "a"))))
   ;; 2. an insertion rejected in Tools > Agents, a word typed and undone,
   ;; then the agent withdraws it: the other proposal keeps its markup
   (list
    (wait-sync (sync-n 2 "b"))
    (step "p3" (== (status-of "p3") "pending"))
    (act "reject 2" (with-buffer (the-name) (archive-state))
         (check "p3: Reject" (shared-proposal-resolve! (the-name) (prop-id "p3") 'old)))
    (timed-wait 300)
    (act "type 2" (type! "TThird one." "U"))
    (timed-wait 600)
    (act "undo 2" (step-undo!))
    (timed-wait 600)
    (act "check 2" (check "p3: still rejected after the undo of the typing" (== (status-of "p3") "rejected"))
         (sync-set! (sync-n 2 "undone")))
    (wait-sync (sync-n 2 "withdrawn"))
    (timed-wait 1500)
    (act "check 2b" (check "p3: still rejected after the withdrawal" (== (status-of "p3") "rejected"))
         (check "p1: still pending in its block" (holds-markup? "p1"))
         (check "consistent" (consistent? '("p1" "p2" "p3")))
         (sync-set! (sync-n 2 "a"))))
   ;; 3. a word change and an insertion after it: Keep Old on the insertion,
   ;; Keep New on the word change; undo twice, redo twice, undo three times,
   ;; redo three times: always consistent, and the outcomes come back
   (list
    (wait-sync (sync-n 3 "b"))
    (step "p4" (== (status-of "p4") "pending"))
    (act "keep 3" (check "p4: Keep Old" (keep-step! "p4" 0)))
    (timed-wait 300)
    (act "keep 3b" (check "p1: Keep New" (keep-step! "p1" 1)))
    (step "settled 3" (and (== (status-of "p1") "accepted") (== (status-of "p4") "rejected"))))
   (apply append
          (map (lambda (x)
                 (append (list (act (car x) ((cadr x))))
                         (consistent-step (car x) '("p1" "p2" "p3" "p4"))))
               (list (list "undo 3a" step-undo!) (list "undo 3b" step-undo!)
                     (list "redo 3a" step-redo!) (list "redo 3b" step-redo!)
                     (list "undo 3c" step-undo!) (list "undo 3d" step-undo!) (list "undo 3e" step-undo!)
                     (list "redo 3c" step-redo!) (list "redo 3d" step-redo!) (list "redo 3e" step-redo!))))
   (list
    (act "check 3" (check "p1: accepted after the redos" (== (status-of "p1") "accepted"))
         (check "p4: not pending, not accepted" (in? (status-of "p4") '("rejected" "resolved")))
         (sync-set! (sync-n 3 "a"))))
   ;; 4. an insertion after block 1 kept Old while the agent changes block 1
   ;; at the same time: the change of the agent is not refused (it is a new
   ;; proposal)
   (list
    (wait-sync (sync-n 4 "b"))
    (step "p5" (== (status-of "p5") "pending"))
    (act "pause 4" (set! refused0 (shared-stat 'refused)) (shared-pause! #t)
         (check "p5: Keep Old" (keep-step! "p5" 0)) (sync-set! (sync-n 4 "ready")))
    (wait-sync (sync-n 4 "submitted"))
    (act "resume 4" (shared-pause! #f))
    (timed-wait 2500)
    (act "check 4" (check "the crossing change of the agent is not refused" (== (shared-stat 'refused) refused0))
         (check "it is proposed" (list-or (map (lambda (i) (text-has? i "HelloXX")) (markup-blocks))))
         (check "p5: rejected" (== (status-of "p5") "rejected"))
         (sync-set! (sync-n 4 "a"))))
   ;; 5. a proposal with two differences; the participant is taken for a
   ;; person and edits that block while the host keeps New on the first
   ;; difference: the edit of the person is kept
   (person! 5 (lambda () (tree-assign (para (find-para "Second paragraph.")) "one two three four five six seven eight nine ten.")))
   (list
    (wait-sync (sync-n 5 "b"))
    (step "p7" (== (status-of "p7") "pending"))
    (act "mark 5" (check "marked a person" (shared-treat-as-person! (the-name) (car (shared-file-agents (the-name))) #t))
         (sync-set! (sync-n 5 "marked")))
    (wait-sync (sync-n 5 "b-ready"))
    (act "pause 5" (set! refused0 (shared-stat 'refused)) (shared-pause! #t)
         (check "p7: Keep New on the first" (keep-step! "p7" 1)) (sync-set! (sync-n 5 "ready")))
    (wait-sync (sync-n 5 "submitted"))
    (act "resume 5" (shared-pause! #f))
    (timed-wait 2500)
    (act "check 5" (check "the edit of the person is not refused" (== (shared-stat 'refused) refused0))
         (check "the edit of the person is in" (text-has? (index-of "p7") "very "))
         (sync-set! (sync-n 5 "a"))))
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

(define (propose-and-wait* n edit1! edit2!)
  ;; two proposals pending at once
  (list (act (sync-n n "edit1") (set! rb0 (shared-stat 'rolled-back)) (edit1!))
        (step (sync-n n "verdict1") (and (> (shared-stat 'rolled-back) rb0) (eq? (verdict-kind) 'proposed)))
        (step (sync-n n "markup1") (version-in-doc?))
        (act (sync-n n "edit2") (set! rb0 (shared-stat 'rolled-back)) (edit2!))
        (step (sync-n n "verdict2") (and (> (shared-stat 'rolled-back) rb0) (eq? (verdict-kind) 'proposed)))
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
   (propose-and-wait* 1 (lambda () (insert-text! 1 6 "big ")) (lambda () (insert-block! 2 "Inserted after.")))
   (agent-hears 1 "p2" "rejected")
   (propose-and-wait 2 (lambda () (insert-block! 2 "Inserted again.")))
   (list (wait-sync (sync-n 2 "undone"))
         (act "withdraw 2" (shared-file-send-withdraw (the-name) (prop-id "p3")) (sync-set! (sync-n 2 "withdrawn"))))
   (agent-hears 2 "p3" "rejected")
   (propose-and-wait 3 (lambda () (insert-block! 2 "Inserted after.")))
   (agent-hears 3 "p1" "accepted")
   (propose-and-wait 4 (lambda () (insert-block! 2 "Inserted X.")))
   (list (wait-sync (sync-n 4 "ready"))
         (act "pause 4" (shared-pause! #t) (set! rb0 (shared-stat 'submitted))
              (tree-assign (para 1) "HelloXX big world."))
         (act "resume 4" (shared-pause! #f))
         (step "submitted 4" (> (shared-stat 'submitted) rb0))
         (act "sig 4" (sync-set! (sync-n 4 "submitted"))))
   (agent-hears 4 "p5" "rejected")
   (list (wait-sync (sync-n 5 "p")) (step "5 there" (find-para "one two three four five six seven eight nine ten.")))
   (propose-and-wait 5 (lambda () (tree-assign (para (find-para "one two three four five six seven eight nine ten."))
                                              "ONE two three four five six seven eight nine TEN.")))
   (list (wait-sync (sync-n 5 "marked")) (timed-wait 1500) (act "ready 5" (sync-set! (sync-n 5 "b-ready")))
         (wait-sync (sync-n 5 "ready"))
         (act "pause 5" (shared-pause! #t) (set! rb0 (shared-stat 'submitted))
              (let* ((i (list-find (iota (para-count))
                                   (lambda (i) (>= (string-search-forwards "TEN." 0 (object->string (para-text i))) 0))))
                     (blk (para i)))
                (when (and (tree-is? blk 'concat) (tree-atomic? (tree-ref blk 1)))
                  (tree-insert (tree-ref blk 1) 5 "very "))))
         (act "resume 5" (shared-pause! #f))
         (step "submitted 5" (> (shared-stat 'submitted) rb0))
         (act "sig 5" (sync-set! (sync-n 5 "submitted"))))
   (list (wait-sync (sync-n 5 "a"))
         (act "out" (publish-body!) (sync-set! "b-done"))
         (wait-sync "a-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
