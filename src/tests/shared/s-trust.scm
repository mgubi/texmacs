;; Trusted agents (docs/mcp-server.md): the host (A, the person, interactive)
;; asks when a participant that is an agent breaks a rule of a level that can be
;; granted. B is a hand-written client of the local link: it claims grants
;; that it does not have (messages `grant', `trusted', a name), and must be
;; refused or asked all the same; what the person grants for the session
;; belongs to the connection, to the level, and to the file; code and limits
;; are never asked about. The person is played by A: the policy file says what
;; it answers.
(load (string-append (getenv "SH_DIR") "/lib.scm"))
(define STEP-TIMEOUT 90000)   ; (the hand-written client waits for the host's answers)

(define (policy) (if (sync-has? "policy") (sync-read "policy") "refuse"))

(define (person!)
  ;; answers the questions according to the policy, and counts them
  (exec-delayed-pause
   (lambda ()
     (for (q (or (shared-asks (the-name)) '()))
       (let ((pol (string-tokenize-by-char (policy) #\space)))
         (sync-write! "last-question" (object->string q))
         (apply shared-ask-answer! (cons (the-name) (cons (car q) pol)))))
     20)))

(define (A-steps)
  (list
   (act "interactive" (shared-set-interactive! #t) (shared-set-ask-max! 1))
   (act "load" (load-buffer FILE))
   (act "direct" (shared-set-agent-mode! (the-name) 'direct))
   (act "go" (sync-set! "a-loaded") (person!))
   (wait-sync "b-done")
   (act "check"
        (let ((s (object->string (body-stree))))
          (check "the first link (granted for the session) is in the document" (string-contains? s "ftp://a.org"))
          (check "the definition that was never granted is not" (not (string-contains? s "\"foo\"")))
          (check "the link of the second connection is not" (not (string-contains? s "ftp://c.org")))
          (check "nor the links that waited for the person" (not (string-contains? s "ftp://cap")))
          (check "no code" (not (string-contains? s "extern")))
          (check "the host asked at least 4 times" (>= (shared-stat 'agent-asked) 4))
          (check "and nothing is trusted any more (the connections are gone)" (null? (shared-trust (the-name))))
          (check "level code is not grantable" (not (shared-grantable-level? "code")))
          (check "limits are not grantable" (not (shared-grantable-level? "limits")))
          (check "style and links are" (and (shared-grantable-level? "style") (shared-grantable-level? "links"))))
        (publish-body!) (sync-set! "a-done"))))

(define (recv-all c ms)
  (let ((end (+ (texmacs-time) ms)))
    (let loop ((acc '()))
      (let ((s (local-receive c)))
        (cond ((!= s "") (loop (cons s acc)))
              ((> (texmacs-time) end) (reverse acc))
              (else (loop acc)))))))

(define (pp ins inv) (list 'pair ins inv))
(define (ins-block i st) (pp `(insert (1 ,i) (document ,st)) `(remove (1 ,i 1) "")))

(define cur #f)
(define nblocks 4)
(define serial 0)

(define (connect! id name junk?)
  (let* ((e (shared-entry (the-name))) (c (local-connect (caddr e))))
    (local-send c (object->string (list 'hello (cadddr e) (cadr e) id 4242 #f #f #f)))
    (let* ((w (let loop ((i 0)) (let ((r (recv-all c 600))) (if (or (pair? r) (> i 5)) r (loop (+ i 1))))))
           (wm (and (pair? w) (string->object (car w)))))
      (set! cur (cadr wm))
      (when name (local-send c (object->string (list 'agent-name name))))
      (when junk?
        ;; (a client claims what it does not have: these mean nothing to the host)
        (local-send c (object->string (list 'grant "style")))
        (local-send c (object->string (list 'grant "links")))
        (local-send c (object->string (list 'trusted "style" "links")))
        (local-send c (object->string (list 'mode 'direct #f)))
        (local-send c (object->string (list 'agent-name "someone-else"))))
      c)))

(define (send-patch! c st)
  ;; send the patch that inserts the block st at the end; the frames received, as strings
  (set! serial (+ serial 1))
  (local-send c (object->string (list 'patch (string-append "agent.t/" (number->string serial)) cur
                                      (ins-block nblocks st))))
  (let ((fr (recv-all c 2500)))
    (for (s fr)
      (let ((m (catch #t (lambda () (string->object s)) (lambda args #f))))
        (when (and (pair? m) (== (car m) 'applied)) (set! cur (caddr m)) (set! nblocks (+ nblocks 1)))
        (when (and (pair? m) (memq (car m) '(refused proposed)) (> (length m) 2)) (set! cur (caddr m)))))
    fr))

(define (any-has? fr str) (list-or (map (lambda (s) (string-contains? s str)) fr)))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "agent" (shared-set-agent! #t))
   (act "load" (load-buffer FILE))
   (act "participant" (check "B participates" (== (role-of) 'participant)))
   (act "t1" (sync-write! "policy" "refuse")
        (let* ((c (connect! "agent.rawtrust1" "raw" #t))
               (fr (send-patch! c '(hlink "x" "ftp://a.org"))))
          (check "a client that claims grants is asked about, not trusted" (any-has? fr "asking"))
          (check "the person refused: told so (asked, refused, grantable)" (and (any-has? fr "refused") (any-has? fr "#t \"refused\" #t")))
          (check "and nothing was applied" (not (any-has? fr "applied")))
          ;; the person grants links for the session
          (sync-write! "policy" "session links")
          (let ((fr2 (send-patch! c '(hlink "x" "ftp://a.org"))))
            (check "after the person grants links for the session: applied" (any-has? fr2 "applied")))
          ;; but not style
          (sync-write! "policy" "refuse")
          (let ((fr3 (send-patch! c '(assign "foo" "bar"))))
            (check "a grant for links does not allow a definition: asked and refused" (and (any-has? fr3 "asking") (any-has? fr3 "refused"))))
          ;; code is never asked about
          (sync-write! "policy" "session style")
          (let ((fr4 (send-patch! c '(extern "x"))))
            (check "code is refused at once, without a question" (and (any-has? fr4 "refused") (not (any-has? fr4 "asking")))))
          (local-send c "(leave)")
          (local-close c)))
   (timed-wait 500)
   ;; the grant belonged to the connection, whatever name the next one claims
   (act "t5" (sync-write! "policy" "refuse")
        (let* ((c (connect! "agent.rawtrust2" "raw" #f))
               (fr (send-patch! c '(hlink "y" "ftp://c.org"))))
          (check "a new connection with the same name is asked again" (any-has? fr "asking"))
          (check "and refused" (and (any-has? fr "refused") (not (any-has? fr "applied"))))
          (local-send c "(leave)")
          (local-close c)))
   ;; the number of open questions is capped (3; the test lowers it to 1): the next is refused as pending
   (act "cap" (sync-write! "policy" "hold")
        (let* ((c1 (connect! "agent.cap1" "raw" #f)) (f1 (send-patch! c1 '(hlink "a" "ftp://cap1.org")))
               (c2 (connect! "agent.cap2" "raw" #f)) (f2 (send-patch! c2 '(hlink "a" "ftp://cap2.org"))))
          (check "a question is open" (any-has? f1 "asking"))
          (check "the next is refused as pending, not asked" (and (any-has? f2 "pending") (not (any-has? f2 "asking"))))
          (sync-write! "policy" "refuse")
          (recv-all c1 1500)
          (for (c (list c1 c2)) (local-send c "(leave)") (local-close c))))
   (act "done" (publish-body!) (sync-set! "b-done"))
   (wait-sync "a-done")))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
