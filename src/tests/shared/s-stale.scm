;; An entry of a dead process is replaced; a link with a wrong token, or one
;; which does not speak the protocol, is closed.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "stale entry"
        (let* ((key (shared-real-path FILE))
               (path (shared-entry-path (the-name))))
          (shared-create-file path (object->string (list 'entry key 1 "oldtoken" 2147000 #f "x" 0)))
          (check "stale entry made" (and (shared-entry (the-name)) #t))))
   (act "load" (load-buffer FILE))
   (act "host" (check "A hosts in spite of the stale entry" (== (role-of) 'host))
        (check "the entry is A's" (let ((e (shared-entry (the-name))))
                                    (and e (== (list-ref e 4) (shared-process-id))
                                         (!= (cadddr e) "oldtoken")))))
   (act "wrong token"
        (let* ((e (shared-entry (the-name)))
               (c (local-connect (caddr e))))
          (set! bad-link c)
          (local-send c (object->string (list 'hello "wrong token" (list-ref e 1) "x" 1 #t #f)))))
   (step "closed" (not (local-alive? bad-link)))
   (act "check" (check "link with a wrong token closed" (not (local-alive? bad-link)))
        (check "no participant" (== (peers-count) 0)))
   (act "garbage"
        (let* ((e (shared-entry (the-name)))
               (c (local-connect (caddr e))))
          (set! bad-link c)
          (local-send c "this is not a message")))
   (step "closed" (not (local-alive? bad-link)))
   (act "check" (check "garbage link closed" (not (local-alive? bad-link)))
        (check "no participant" (== (peers-count) 0))
        (check "the host works" (== (text-of 1) "Hello world.")))
   (act "close" (buffer-close (the-name)))
   (act "check" (check "entry removed at close" (not (shared-entry (the-name)))))))

(define bad-link #f)
(run-steps (if (== ROLE "A") (A-steps) (list)))
