;; A host which is alive but does not answer in time is not replaced: the
;; joiner opens the file without sharing, quickly, and the entry stays.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define t0 #f)
(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "pause" (shared-pause! #t) (sync-set! "a-loaded"))
   (wait-sync "b-loaded")
   (act "resume" (shared-pause! #f))
   (timed-wait 1000)
   (act "check" (check "A is still the host" (== (role-of) 'host))
        (check "the entry is still A's" (== (list-ref (shared-entry (the-name)) 4) (shared-process-id)))
        (check "nobody joined" (== (peers-count) 0))
        (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (set! t0 (texmacs-time)) (load-buffer FILE))
   (act "check" (check "B did not wait long" (< (- (texmacs-time) t0) 1500))
        (check "B did not take the place of A" (not (shared-file? (the-name))))
        (check "B has the document from the file" (== (text-of 1) "Hello world."))
        (check "the entry is A's" (not (== (list-ref (shared-entry (the-name)) 4) (shared-process-id))))
        (sync-set! "b-loaded"))
   (wait-sync "a-done")
   (act "done" (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
