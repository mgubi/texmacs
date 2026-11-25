;; The person (the host A) resolves proposals of an agent (B), undoes past
;; the change of the agent and redoes everything (#957): the change of the
;; agent undone reads the neutral "resolved" (its block was replaced); after
;; the redos, a proposal resolved inside its block or by keeping a block is
;; found again from its block (accepted), one whose block was removed reads
;; "resolved"; two blocks of pending proposals joined by hand stay pending,
;; and Keep New resolves both.
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

(define (A-steps)
  (append
   (list
    (act "interactive" (shared-set-interactive! #t))
    (act "load" (load-buffer FILE))
    (act "host" (check "A hosts" (== (role-of) 'host)))
    (act "go" (switch-to-buffer (the-name)) (sync-set! "a-loaded"))
    (wait-sync "b-joined")
    (step "agent listed" (= (length (shared-file-agents (the-name))) 1)))
   ;; 1. a word change kept New; undo: pending; undo again (the change of
   ;; the agent): rejected; undo once more, then redo everything: accepted
   (list
    (wait-sync (sync-n 1 "b"))
    (step "p1" (== (status-of "p1") "pending"))
    (act "keep 1" (check "p1: Keep New" (keep-step! "p1" 1)))
    (step "settled 1" (== (status-of "p1") "accepted"))
    (act "undo 1" (step-undo!))
    (timed-wait 600)
    (act "check 1" (check "p1: pending after undo" (holds-markup? "p1")))
    (act "undo 1b" (step-undo!))
    (timed-wait 600)
    (act "check 1b" (check "p1: the change of the agent undone: resolved (neutral)"
                           (and (find-para "Hello world.") (== (status-of "p1") "resolved"))))
    (act "undo 1c" (step-undo!))
    (timed-wait 600)
    (act "redo 1" (step-redo!))
    (timed-wait 600)
    (act "check 1c" (check "p1: pending after redo of the change of the agent" (holds-markup? "p1")))
    (act "redo 1b" (step-redo!))
    (timed-wait 600)
    (act "redo 1c" (step-redo!))
    (timed-wait 600)
    (act "check 1d" (check "p1: accepted after redoing everything" (== (status-of "p1") "accepted"))
         (check "p1: the text" (find-para "Hello big world."))
         (sync-set! (sync-n 1 "a"))))
   ;; 2. a word change and an insertion; Keep New on the first, Reject the
   ;; second in Tools > Agents, a word typed elsewhere; six undos, six
   ;; redos: accepted and rejected again (p1 too)
   (list
    (wait-sync (sync-n 2 "b"))
    (step "p2 p3" (and (== (status-of "p2") "pending") (== (status-of "p3") "pending")))
    (act "keep 2" (check "p2: Keep New" (keep-step! "p2" 1)))
    (timed-wait 300)
    (act "reject 2" (with-buffer (the-name) (archive-state))
         (check "p3: Reject" (shared-proposal-resolve! (the-name) (prop-id "p3") 'old)))
    (timed-wait 300)
    (act "type 2" (with-buffer (the-name) (archive-state) (start-editing)
                    (insert-text! (find-para "Third one.") 0 "T") (end-editing))))
   (undos 6)
   (list (timed-wait 600)
         (act "check 2" (check "p2: resolved (neutral) after undoing the change of the agent" (== (status-of "p2") "resolved"))))
   (redos 6)
   (list
    (timed-wait 600)
    (act "check 2b" (check "p2: accepted after redoing everything" (== (status-of "p2") "accepted"))
         (check "p3: its block removed, resolved (neutral) after redoing everything" (== (status-of "p3") "resolved"))
         (check "p1: still accepted" (== (status-of "p1") "accepted"))
         (check "the texts" (== (texts) '((section "Intro") "Hello big world." "Second new paragraph." "TThird one.")))
         (sync-set! (sync-n 2 "a"))))
   ;; 3. two blocks of pending proposals joined by hand (Delete at the end of
   ;; the first): both stay pending; undo, redo; Keep New twice: both
   ;; accepted
   (list
    (wait-sync (sync-n 3 "b"))
    (step "p4 p5" (and (== (status-of "p4") "pending") (== (status-of "p5") "pending")))
    (act "join 3" (with-buffer (the-name) (archive-state) (start-editing)
                    (tree-join (body) (index-of "p4")) (end-editing)))
    (timed-wait 600)
    (act "check 3" (check "p4: pending after the join" (holds-markup? "p4"))
         (check "p5: pending after the join" (holds-markup? "p5")))
    (act "undo 3" (step-undo!))
    (timed-wait 600)
    (act "check 3b" (check "p4: pending after undo" (holds-markup? "p4"))
         (check "p5: pending after undo" (holds-markup? "p5")))
    (act "redo 3" (step-redo!))
    (timed-wait 600)
    (act "check 3c" (check "p4: pending after redo" (holds-markup? "p4"))
         (check "p5: pending after redo" (holds-markup? "p5")))
    (act "keep 3" (check "Keep New" (keep-step! "p5" 1)))
    (timed-wait 300)
    (act "keep 3b" (check "Keep New again" (keep-step! "p5" 1)))
    (step "settled 3" (not (version-in-doc?)))
    (timed-wait 300)
    (act "check 3d" (check "p4: accepted" (== (status-of "p4") "accepted"))
         (check "p5: accepted" (== (status-of "p5") "accepted"))
         (sync-set! (sync-n 3 "a"))))
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
   (round 1 (lambda () (insert-text! 1 6 "big ")) "p1" "accepted")
   (propose-and-wait* 2 (lambda () (insert-text! 2 7 "new ")) (lambda () (insert-block! 4 "Inserted.")))
   (agent-hears 2 "p2" "accepted")
   (propose-and-wait* 3 (lambda () (tree-assign (para (find-para "Second new paragraph.")) "A second new paragraph."))
                      (lambda () (tree-assign (para (find-para "TThird one.")) "B third one.")))
   (agent-hears 3 "p5" "accepted")
   (list
    (act "out" (publish-body!) (sync-set! "b-done"))
    (wait-sync "a-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
