;; The person (the host A, interactive) resolves the proposals of an agent (B)
;; with the Version tools or Tools > Agents, then undoes and redoes (#957):
;; an undo that brings back the markup of a resolved proposal makes it
;; pending again, and a redo resolves it again as before; the undo of a
;; Keep that made the body again leaves the other pending proposals in their
;; blocks; a difference whose choice was undone and that is then typed over
;; by hand makes the outcome "resolved".
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
  (with l (list-filter (or (shared-proposals (the-name)) '()) (lambda (e) (== (car e) id)))
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

(define (A-steps)
  (append
   (list
    (act "interactive" (shared-set-interactive! #t))
    (act "load" (load-buffer FILE))
    (act "host" (check "A hosts" (== (role-of) 'host)))
    (act "go" (switch-to-buffer (the-name)) (sync-set! "a-loaded"))
    (wait-sync "b-joined")
    (step "agent listed" (= (length (shared-file-agents (the-name))) 1)))
   ;; 1. a word change and an insertion pending; Keep New on the insertion
   ;; (the body is made again), undo: both pending in their blocks; redo:
   ;; the insertion accepted again, the word change still pending; Keep Old
   ;; on it: rejected
   (list
    (wait-sync (sync-n 1 "b"))
    (step "p1 p2" (and (== (status-of "p1") "pending") (== (status-of "p2") "pending")))
    (act "keep 1" (check "p2: Keep New" (keep-in! (index-of "p2") 1)))
    (step "settled 1" (== (status-of "p2") "accepted"))
    (act "undo 1" (undo!))
    (timed-wait 600)
    (timed-wait 300)
    (act "check 1" (check "p2: pending again after undo, in its block" (holds-markup? "p2"))
         (check "p2: at its place" (== (index-of "p2") 3))
         (check "p1: still pending, in its block" (holds-markup? "p1"))
         (check "p1: at its place" (== (index-of "p1") 1)))
    (act "redo 1" (redo!))
    (timed-wait 600)
    (timed-wait 300)
    (act "check 1b" (check "p2: accepted again after redo" (== (status-of "p2") "accepted"))
         (check "p1: still pending after redo, in its block" (holds-markup? "p1")))
    (act "keep 1b" (check "p1: Keep Old" (keep-in! (index-of "p1") 0)))
    (step "settled 1b" (and (not (version-in-doc?)) (!= (status-of "p1") "pending")))
    (act "check 1c" (check "p1: rejected" (== (status-of "p1") "rejected"))
         (check "p2: accepted" (== (status-of "p2") "accepted"))
         (check "the texts" (== (texts) '((section "Intro") "Hello world." "Second paragraph." "Inserted." "Third one.")))
         (sync-set! (sync-n 1 "a"))))
   ;; 2. a word change kept New, undo: pending again, and Tools > Agents can
   ;; reject it
   (list
    (wait-sync (sync-n 2 "b"))
    (step "p3" (== (status-of "p3") "pending"))
    (act "keep 2" (check "p3: Keep New" (keep-in! (index-of "p3") 1)))
    (step "settled 2" (== (status-of "p3") "accepted"))
    (act "undo 2" (undo!))
    (timed-wait 600)
    (act "check 2" (check "p3: pending again after undo, in its block" (holds-markup? "p3"))
         (check "p3: Reject in Tools > Agents" (shared-proposal-resolve! (the-name) "p3" 'old)))
    (step "settled 2b" (not (version-in-doc?)))
    (act "check 2b" (check "p3: rejected" (== (status-of "p3") "rejected"))
         (check "the text" (find-para "Second paragraph."))
         (sync-set! (sync-n 2 "a"))))
   ;; 3. an insertion accepted with Tools > Agents, undo: pending; redo:
   ;; accepted
   (list
    (wait-sync (sync-n 3 "b"))
    (step "p4" (== (status-of "p4") "pending"))
    (act "accept 3" (check "p4: Accept in Tools > Agents" (shared-proposal-resolve! (the-name) "p4" 'new)))
    (act "undo 3" (undo!))
    (timed-wait 600)
    (act "check 3" (check "p4: pending again after undo, in its block" (holds-markup? "p4")))
    (act "redo 3" (redo!))
    (timed-wait 600)
    (act "check 3b" (check "p4: accepted again after redo" (== (status-of "p4") "accepted"))
         (check "no markup" (not (version-in-doc?)))
         (check "the text" (== (find-para "Appended.") 5))
         (sync-set! (sync-n 3 "a"))))
   ;; 4. an insertion kept Old (its block removed), undo: pending in its
   ;; block; Keep New: accepted
   (list
    (wait-sync (sync-n 4 "b"))
    (step "p5" (== (status-of "p5") "pending"))
    (act "keep 4" (check "p5: Keep Old" (keep-in! (index-of "p5") 0)))
    (step "settled 4" (== (status-of "p5") "rejected"))
    (act "undo 4" (undo!))
    (timed-wait 600)
    (act "check 4" (check "p5: pending again after undo, in its block" (holds-markup? "p5"))
         (check "p5: at its place" (== (index-of "p5") 1)))
    (act "keep 4b" (check "p5: Keep New" (keep-in! (index-of "p5") 1)))
    (step "settled 4b" (and (not (version-in-doc?)) (!= (status-of "p5") "pending")))
    (act "check 4b" (check "p5: accepted" (== (status-of "p5") "accepted"))
         (check "the text" (== (find-para "Removed.") 1))
         (sync-set! (sync-n 4 "a"))))
   ;; 5. two differences; Keep New on the first, undo, the first typed over
   ;; by hand, Keep New on the second: resolved
   (person! 5 (lambda () (tree-assign (para (find-para "Appended.")) "one two three.")))
   (list
    (wait-sync (sync-n 5 "b"))
    (step "p6" (== (status-of "p6") "pending"))
    (act "keep 5" (check "p6: Keep New on the first" (keep-in! (index-of "p6") 1)))
    (timed-wait 300)
    (act "undo 5" (undo!))
    (timed-wait 300)
    (act "hand 5" (with-buffer (the-name)
                    (let ((vt (first-version (para (index-of "p6")))))
                      (start-editing) (tree-assign vt "one ") (end-editing))))
    (timed-wait 300)
    (act "check 5" (check "p6: still pending" (== (status-of "p6") "pending")))
    (act "keep 5b" (check "p6: Keep New on the second" (keep-in! (index-of "p6") 1)))
    (step "settled 5" (and (not (version-in-doc?)) (!= (status-of "p6") "pending")))
    (act "check 5b" (check "p6: resolved" (== (status-of "p6") "resolved"))
         (check "the text" (find-para "one two THREE."))
         (sync-set! (sync-n 5 "a"))))
   ;; 6. two differences; Keep New on the first, undo, redo, Accept in Tools
   ;; > Agents: accepted
   (person! 6 (lambda () (tree-assign (para (find-para "one two THREE.")) "one two three.")))
   (list
    (wait-sync (sync-n 6 "b"))
    (step "p7" (== (status-of "p7") "pending"))
    (act "keep 6" (check "p7: Keep New on the first" (keep-in! (index-of "p7") 1)))
    (timed-wait 300)
    (act "undo 6" (undo!))
    (timed-wait 300)
    (act "redo 6" (redo!))
    (timed-wait 300)
    (act "accept 6" (check "p7: Accept" (shared-proposal-resolve! (the-name) "p7" 'new)))
    (step "settled 6" (not (version-in-doc?)))
    (act "check 6" (check "p7: accepted" (== (status-of "p7") "accepted"))
         (check "the text" (find-para "ONE two THREE."))
         (sync-set! (sync-n 6 "a"))))
   ;; 7. a deletion and a word change after it pending; Keep Old on the
   ;; deletion (the body is made again), undo: both pending in their blocks;
   ;; redo: rejected again, the word change still pending; Keep New on it:
   ;; accepted
   (list
    (wait-sync (sync-n 7 "b"))
    (step "p8 p9" (and (== (status-of "p8") "pending") (== (status-of "p9") "pending")))
    (act "keep 7" (check "p8: Keep Old" (keep-in! (index-of "p8") 0)))
    (step "settled 7" (== (status-of "p8") "rejected"))
    (timed-wait 300)
    (act "check 7" (check "p9: still pending after Keep, in its block" (holds-markup? "p9")))
    (act "undo 7" (undo!))
    (timed-wait 600)
    (timed-wait 300)
    (act "check 7b" (check "p8: pending again after undo, in its block" (holds-markup? "p8"))
         (check "p9: still pending after undo, in its block" (holds-markup? "p9"))
         (check "p9: after p8" (> (index-of "p9") (index-of "p8"))))
    (act "redo 7" (redo!))
    (timed-wait 600)
    (timed-wait 300)
    (act "check 7c" (check "p8: rejected again after redo" (== (status-of "p8") "rejected"))
         (check "p9: still pending after redo, in its block" (holds-markup? "p9")))
    (act "keep 7b" (check "p9: Keep New" (keep-in! (index-of "p9") 1)))
    (step "settled 7b" (and (not (version-in-doc?)) (!= (status-of "p9") "pending")))
    (act "check 7d" (check "p9: accepted" (== (status-of "p9") "accepted"))
         (check "p8: rejected" (== (status-of "p8") "rejected"))
         (check "the texts" (and (find-para "Inserted.") (find-para "Third big one.")))
         (sync-set! (sync-n 7 "a"))))
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
   (propose-and-wait* 1 (lambda () (insert-text! 1 6 "big ")) (lambda () (insert-block! 3 "Inserted.")))
   (agent-hears 1 "p1" "rejected")
   (round 2 (lambda () (insert-text! 2 7 "new ")) "p3" "rejected")
   (round 3 (lambda () (insert-block! 5 "Appended.")) "p4" "accepted")
   (round 4 (lambda () (insert-block! 1 "Removed.")) "p5" "accepted")
   (list (wait-sync (sync-n 5 "p")) (step "5 there" (find-para "one two three.")))
   (round 5 (lambda () (tree-assign (para (find-para "one two three.")) "ONE two THREE.")) "p6" "resolved")
   (list (wait-sync (sync-n 6 "p")) (step "6 there" (find-para "one two three.")))
   (round 6 (lambda () (tree-assign (para (find-para "one two three.")) "ONE two THREE.")) "p7" "accepted")
   (propose-and-wait* 7 (lambda () (tree-remove (body) (find-para "Inserted.") 1))
                      (lambda () (insert-text! (find-para "Third one.") 6 "big ")))
   (agent-hears 7 "p9" "accepted")
   (list
    (act "out" (publish-body!) (sync-set! "b-done"))
    (wait-sync "a-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
