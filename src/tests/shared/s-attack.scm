;; A process that has no token cannot make a host use memory or hold links:
;; too long packets, bad lengths, the old "!" exchange, data with no end.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-done")
   (act "check" (check "the host works" (and (== (role-of) 'host) (== (peers-count) 0)
                                              (== (text-of 1) "Hello world."))))))

(define links '())
(define (attack! raw? s)
  (let* ((e (shared-entry (the-name))) (c (local-connect (caddr e))))
    (set! links (cons c links))
    (if raw? (local-send-raw c s) (local-send c s))))
(define (junk n) (make-string n #\x))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "attacks"
        (attack! #f (junk 200000))            ; a whole packet, too long
        (attack! #t "99999999999\n")          ; a length too long
        (attack! #t (string-append "5000000\n" (junk 100000)))  ; a long packet announced
        (attack! #t "-5\n")                   ; a negative length
        (attack! #t "!3\nabc")                ; the encrypted links of tm_link
        (attack! #t (junk 300000)))           ; data with no end
   (timed-wait 1200)
   (act "check" (check "every such link is closed" (list-and (map (lambda (c) (not (local-alive? c))) links))))
   (act "done" (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
