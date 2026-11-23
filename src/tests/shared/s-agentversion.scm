;; The person (the host A, interactive) resolves the proposals of an agent (B)
;; with the Version tools (the Review bar: the cursor in a difference, Keep
;; Old or New), not with Tools > Agents: the host finds out which version was
;; kept and the agent hears accepted or rejected (#867). An insertion (its old
;; version is empty) and a deletion (its new version is empty) as well, next
;; to equal blocks; several proposals resolved at once; a comparison made by
;; the person afterwards is not taken for a proposal.
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

;; Retain on the whole document: all selected, Keep (the Review bar), or
;; Version > Retain all
(define (keep-selected! which)
  (with-buffer (the-name)
    (select-all)
    (start-editing) (version-retain which) (end-editing)
    (selection-cancel) #t))
(define (keep-all! which)
  (with-buffer (the-name)
    (start-editing) (version-retain-all which) (end-editing) #t))

(define (a-round-all n ids keep expected . checks)
  ;; wait for the proposals @ids, resolve them all at once with @keep, check
  ;; that each is @expected
  (list
   (wait-sync (sync-n n "b"))
   (step (sync-n n "pending") (list-and (map (lambda (id) (== (status-of id) "pending")) ids)))
   (act (sync-n n "keep") (check (sync-n n "Retain") (keep)))
   (step (sync-n n "settled") (and (not (version-in-doc?)) (not (list-or (map (lambda (id) (== (status-of id) "pending")) ids)))))
   (act (sync-n n "check")
        (for-each (lambda (id e) (check (string-append id ": " e) (== (status-of id) e))) ids expected)
        (for-each (lambda (c) (check (car c) ((cadr c)))) checks)
        (sync-set! (sync-n n "a")))))

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
   ;; 3. an insertion kept New: accepted
   (a-round 3 "p3" 1 "accepted"
            (list "the paragraph is in" (lambda () (== (para-text 3) "Inserted one."))))
   ;; (the person takes it out)
   (person! 32 (lambda () (tree-remove (body) 3 1)))
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
   ;; 9. an insertion and a deletion, all selected, Keep New: both accepted
   (a-round-all 9 '("p11" "p12") (lambda () (keep-selected! 1)) '("accepted" "accepted")
                (list "both changes are in" (lambda () (== (texts) '((section "Intro") "Hello big world." "Brand new." "Second paragraph." "Second paragraph." "Third one." "" "End.")))))
   ;; 10. the same with Version > Retain all
   (a-round-all 10 '("p13" "p14") (lambda () (keep-all! 1)) '("accepted" "accepted")
                (list "both changes are in" (lambda () (== (texts) '((section "Intro") "Hello big world." "Brand two." "Brand new." "Second paragraph." "Second paragraph." "" "End.")))))
   ;; 11. two adjacent insertions (the second before the first, which equals
   ;; the paragraph before), Retain all Old: both rejected
   (a-round-all 11 '("p15" "p16") (lambda () (keep-all! 0)) '("rejected" "rejected")
                (list "the document is as before" (lambda () (== (texts) '((section "Intro") "Hello big world." "Brand two." "Brand new." "Second paragraph." "Second paragraph." "" "End.")))))
   ;; 12. a proposal accepted with Tools > Agents, then the person compares
   ;; the document with the saved file: the same markup, but the person's,
   ;; and the proposal stays accepted
   (list
    (act "save 12" (save-buffer-main (the-name)))
    (step "saved 12" (not (buffer-modified? (the-name))))
    (act "12p" (sync-set! (sync-n 12 "p")))
    (wait-sync (sync-n 12 "b"))
    (step "p17" (== (status-of "p17") "pending"))
    (act "accept 17" (check "p17: accepted with Tools > Agents" (shared-proposal-resolve! (the-name) "p17" 'new)))
    (act "compare" (with-buffer (the-name) (compare-with-older (string->url FILE))))
    (step "compared" (version-in-doc?))
    (timed-wait 600)
    (act "check 12" (check "p17: still accepted after Compare" (== (status-of "p17") "accepted"))
         (check "no proposal is pending" (not (list-or (map (lambda (e) (== (cadr e) "pending")) (shared-proposals (the-name)))))))
    (act "retain 12" (keep-all! 1))
    (step "no markup 12" (not (version-in-doc?)))
    (timed-wait 300)
    (act "check 12b" (check "p17: accepted" (== (status-of "p17") "accepted"))
         (sync-set! (sync-n 12 "a"))))
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
   (round 2 (lambda () (insert-block! 3 "New paragraph.")) "p2" "rejected")
   (round 3 (lambda () (insert-block! 3 "Inserted one.")) "p3" "accepted")
   (list (wait-sync (sync-n 32 "p")) (step "32 there" (= (para-count) 4)))
   (round 4 (lambda () (tree-remove (body) 3 1)) "p4" "accepted")
   (list (wait-sync (sync-n 5 "p")) (step "5 there" (= (para-count) 5)))
   (round 5 (lambda () (tree-remove (body) 3 1)) "p5" "rejected")
   (list (wait-sync (sync-n 6 "p")) (step "6 there" (= (para-count) 8)))
   (round 6 (lambda () (insert-block! 6 "Between.")) "p6" "rejected")
   (round 61 (lambda () (tree-remove (body) 6 1)) "p7" "rejected")
   (round 62 (lambda () (tree-remove (body) 6 1)) "p8" "accepted")
   (round 7 (lambda () (insert-block! 5 "Third one.")) "p9" "rejected")
   (round 71 (lambda () (insert-block! 5 "Third one.")) "p10" "accepted")
   (propose-and-wait* 9 (lambda () (insert-block! 2 "Brand new.")) (lambda () (tree-remove (body) 5 1)))
   (agent-hears 9 "p12" "accepted")
   (propose-and-wait* 10 (lambda () (insert-block! 2 "Brand two.")) (lambda () (tree-remove (body) 6 1)))
   (agent-hears 10 "p14" "accepted")
   (propose-and-wait* 11 (lambda () (insert-block! 6 "Second paragraph.")) (lambda () (insert-block! 6 "Other.")))
   (agent-hears 11 "p16" "rejected")
   (list (wait-sync (sync-n 12 "p")))
   (propose-and-wait 12 (lambda () (insert-text! 4 6 "X")))
   (agent-hears 12 "p17" "accepted")
   (list
    (act "out" (publish-body!) (sync-set! "b-done"))
    (wait-sync "a-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
