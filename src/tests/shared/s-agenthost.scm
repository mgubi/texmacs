;; The host (the person: A, interactive) has proposals of an agent (B): one
;; accepted, one pending. A closes the file: B, which held the list of its
;; proposals told by A, hosts the file. The new host has no record of the
;; proposals (#956: it used to read that list as records, "shared file error"
;; at each tick): the pending markup is plain version markup in the document,
;; nobody lists it. A opens the file again and hosts it: no record of the old
;; proposals either, B's list is empty; a new proposal of B works, and the old
;; markup is resolved with the Version tools. An edit of B in the old markup
;; is refused with a reason that names the person (B cannot withdraw it); a
;; withdraw by the id of its old pending proposal touches none of its new ones
;; (the ids of each host carry a tag of their own: with p1, p2... again, the
;; new p2 was taken back).
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (has-version? st)
  (and (pair? st)
       (or (and (memq (car st) '(version-both version-old version-new)) #t)
           (list-or (map has-version? (cdr st))))))
(define (version-in-doc?) (has-version? (body-stree)))
(define (sync-n n s) (string-append "r" (number->string n) "-" s))
(define (props) (shared-proposals (the-name)))
(define (plain i)
  ;; the text of the paragraph @i (Retain may leave a concat of strings)
  (let ((st (para-text i)))
    (if (and (pair? st) (== (car st) 'concat) (list-and (map string? (cdr st))))
        (apply string-append (cdr st))
        st)))
(define (statuses) (map cadr (or (props) '())))

(define (A-steps)
  (list
   (act "interactive" (shared-set-interactive! #t))
   (act "load" (load-buffer FILE))
   (act "host" (check "A hosts" (== (role-of) 'host)) (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   ;; 1. a proposal, accepted
   (wait-sync (sync-n 1 "b"))
   (step "proposal 1" (nnull? (props)))
   (act "accept 1" (check "p1 accepted" (shared-proposal-resolve! (the-name) (prop-id "p1") 'new))
        (sync-set! (sync-n 1 "a")))
   ;; 2. a proposal, left pending
   (wait-sync (sync-n 2 "b"))
   (step "proposal 2" (= (length (props)) 2))
   (act "check 2" (check "A: p1 accepted, p2 pending" (equal? (statuses) '("accepted" "pending")))
        (sync-set! (sync-n 2 "a")))
   ;; 3. A closes the file: B hosts
   (wait-sync "b-listed")
   (act "close" (buffer-close (the-name)) (sync-set! "a-closed"))
   (wait-sync "b-hosted")
   ;; 4. A comes back and hosts again
   (act "reload" (load-buffer FILE))
   (step "hosts again" (== (role-of) 'host))
   (act "check 4" (check "A: no record of the former proposals" (null? (props)))
        (check "A: the pending markup is in the document" (version-in-doc?))
        (check "A: nothing pending for the person" (= (shared-file-pending-count (the-name)) 0))
        (sync-set! "a-back"))
   ;; 5. two new proposals of B, then a withdraw by the id of its old pending
   ;; one; the old markup resolved with the Version tools
   (wait-sync (sync-n 5 "b"))
   (step "proposal 5" (= (length (props)) 2))
   (act "check 5" (check "A: two records, the new proposals of B, still pending"
                         (and (equal? (statuses) '("pending" "pending"))
                              (list-and (map (lambda (e) (== (list-ref e 3) (car (shared-file-agents (the-name)))))
                                             (props)))))
        (check "A: the new ids are not those of the former host"
               (not (list-or (map (lambda (e) (== (car e) (sync-read "old-id"))) (props)))))
        (check "A: the old markup is still there" (version-in-doc?))
        (check "A: accept them" (list-and (map (lambda (e) (shared-proposal-resolve! (the-name) (car e) 'new))
                                               (props))))
        (with-buffer (the-name)
          (start-editing) (version-retain-all 1) (end-editing))
        (check "A: the old markup resolved by Retain all" (not (version-in-doc?)))
        (check "A: the texts" (and (== (plain 1) "HelloAA world.")
                                   (== (plain 2) "SecondZZ paragraph.")
                                   (== (plain 3) "ThirdQQ one.")
                                   (string-contains? (object->string (para-text 0)) "XIntro")))
        (sync-set! (sync-n 5 "a")))
   (wait-sync "b-done")
   (act "out" (publish-body!) (sync-set! "a-done"))))

(define last-verdict #f)
(define rb0 0)
(define (edit-and-wait n edit!)
  ;; edit, and wait for the host's verdict (the edit is taken back)
  (list (act (sync-n n "edit") (set! rb0 (shared-stat 'rolled-back)) (edit!))
        (step (sync-n n "verdict")
              (and (> (shared-stat 'rolled-back) rb0)
                   (begin (set! last-verdict (shared-file-verdict (the-name))) last-verdict)))))

(define (B-steps)
  (append
   (list
    (wait-sync "a-loaded")
    (act "agent" (shared-set-agent! #t))
    (act "load" (load-buffer FILE))
    (step "mode" (eq? (shared-agent-mode (the-name)) 'propose))
    (act "sig" (sync-set! "b-joined")))
   (edit-and-wait 1 (lambda () (insert-text! 1 5 "AA")))
   (list
    (step "markup 1" (version-in-doc?))
    (act "b1" (sync-set! (sync-n 1 "b")))
    (wait-sync (sync-n 1 "a"))
    (step "accepted" (and (not (version-in-doc?)) (equal? (statuses) '("accepted")))))
   (edit-and-wait 2 (lambda () (insert-text! 2 6 "ZZ")))
   (list
    (step "markup 2" (version-in-doc?))
    (act "b2" (sync-set! (sync-n 2 "b")))
    (wait-sync (sync-n 2 "a"))
    (step "listed" (equal? (statuses) '("accepted" "pending")))
    (act "check 2" (check "B is told: p1 accepted, p2 pending" #t)
         (sync-write! "old-id" (car (cadr (props))))
         (sync-set! "b-listed"))
    ;; 3. B hosts
    (wait-sync "a-closed")
    (step "B hosts" (== (role-of) 'host))
    (timed-wait 600)
    (act "check 3" (check "B: no record of the proposals" (null? (props)))
         (check "B: nothing pending" (= (shared-file-pending-count (the-name)) 0))
         (check "B: the pending markup stays in the document" (version-in-doc?))
         (sync-set! "b-hosted"))
    ;; 4. A is back
    (wait-sync "a-back")
    (step "participant again" (== (role-of) 'participant))
    (act "check 4" (check "B: the list of the former host is forgotten" (null? (props)))
         (check "B: the drop is counted (the MCP server tells the agent)"
                (>= (shared-proposals-forgotten (the-name)) 1))))
   ;; an edit of the old markup: refused, only the person resolves it
   (edit-and-wait 7 (lambda () (tree-assign (para 2) (stree->tree "Replaced."))))
   (list
    (act "v7" (check "B: an edit of the old markup is refused, for the person to resolve"
                     (and last-verdict (eq? (car last-verdict) 'refused)
                          (string-contains? (object->string (caddr last-verdict)) "only the person")))))
   (edit-and-wait 5 (lambda () (insert-text! 3 5 "QQ")))
   (list
    (act "v5" (check "B: proposed again" (and last-verdict (eq? (car last-verdict) 'proposed)))))
   (edit-and-wait 6 (lambda () (tree-insert (tree-ref (para 0) 0) 0 "X")))
   (list
    (act "v6" (check "B: a second new proposal" (and last-verdict (eq? (car last-verdict) 'proposed))))
    (step "two listed" (equal? (statuses) '("pending" "pending")))
    (act "withdraw old" (shared-file-send-withdraw (the-name) (sync-read "old-id")))
    (timed-wait 800)
    (act "b5" (check "B: the withdraw of the old id took nothing back" (equal? (statuses) '("pending" "pending")))
         (sync-set! (sync-n 5 "b")))
    (wait-sync (sync-n 5 "a"))
    (step "resolved" (and (not (version-in-doc?)) (== (para-text 3) "ThirdQQ one.")
                          (equal? (statuses) '("accepted" "accepted"))))
    (act "check 5" (check "B: told its new proposals are accepted" #t)
         (publish-body!) (sync-set! "b-done"))
    (wait-sync "a-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
