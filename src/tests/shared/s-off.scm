;; With the preference off (the default), nothing is shared.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "check" (check "A does not share" (not (shared-file? (the-name)))))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-loaded")
   (act "edit" (insert-text! 1 5 "AA") (sync-set! "a-edited"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "check" (check "B does not share" (not (shared-file? (the-name))))
        (check "B read the file" (== (text-of 1) "Hello world."))
        (sync-set! "b-loaded"))
   (wait-sync "a-edited")
   (step "settle" (begin (if (not off-t) (set! off-t (texmacs-time)))
                         (> (- (texmacs-time) off-t) 600)))
   (act "check" (check "A's edit did not reach B" (== (text-of 1) "Hello world."))
        (check "no entry" (not (shared-entry (the-name))))
        (sync-set! "b-done"))))

(define off-t #f)
(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
