;; A patch sent twice is applied once; a document of the host that replaces
;; one that differs is first saved whole in the recovered folder.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE) (sync-set! "a-loaded"))
   (wait-sync "b-sent-twice")
   (timed-wait 500)
   (act "check" (check "the patch is in once" (== (text-of 1) "HelloBB world."))
        (check "the second copy was seen" (== (shared-stat 'duplicate) 1))
        (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "edit" (insert-text! 1 5 "BB"))
   (step "sent and confirmed" (and (>= (shared-stat 'submitted) 1) (== (pending-count) 0)))
   (act "send it again" (shared-test-resend-last! (the-name)) (sync-set! "b-sent-twice"))
   (wait-sync "a-done")
   (act "the host's document replaces a different one"
        (shared-test-reset! (the-name) '(document (section "Other") "Replaced.")))
   (act "check" (check "the whole old document is saved first"
                       (list-or (map (lambda (f) (and (string-contains? f "-document.tm")
                                                      (string-contains? (string-load (string->url f)) "Second paragraph.")))
                                     (recovered-files))))
        (check "and replaced" (== (text-of 1) "Replaced."))
        (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
