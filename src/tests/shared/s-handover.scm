;; The host closes the file: the participant takes over (clean handover),
;; with the last change of the host; it saves; the old host opens the file
;; again and joins the new host.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (timed-wait 1300)
   ;; the host saves, then changes the file once more and closes it: the
   ;; participant, which becomes the host, must not be asked "the file has
   ;; changed on disk" when it saves
   (act "edit, save" (insert-text! 1 5 "AA") (buffer-pretend-modified (the-name))
        (save-buffer-main (the-name)))
   (act "edit and close" (insert-text! 3 5 "CC") (buffer-close (the-name))
        (sync-set! "a-closed"))
   (act "closed" (check "A left the file" (not (shared-file? (the-name)))))
   (wait-sync "b-saved")
   (act "reload" (load-buffer FILE))
   (act "rejoin" (check "A joins the new host" (== (role-of) 'participant))
        (check "A has the content" (and (== (text-of 1) "HelloAA world.")
                                         (== (text-of 2) "Second parBBagraph."))))
   (act "out" (publish-body!) (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "b-joined"))
   (wait-sync "a-closed")
   (step "B is the host" (== (role-of) 'host))
   (act "check" (check "B has the last changes of A" (and (== (text-of 1) "HelloAA world.") (== (text-of 3) "ThirdCC one.")))
        (check "entry points to B" (and-with e (shared-entry (the-name))
                                      (== (list-ref e 4) (shared-process-id)))))
   (act "edit, save" (insert-text! 2 10 "BB") (save-buffer-main (the-name)))
   (act "saved" (check "B wrote the file"
                       (and (string-contains? (string-load (the-name)) "HelloAA")
                            (string-contains? (string-load (the-name)) "ThirdCC")
                            (string-contains? (string-load (the-name)) "parBBagraph")))
        (check "once" (== (shared-stat 'saves) 1))
        (sync-set! "b-saved"))
   (wait-sync "a-done")
   (act "out" (publish-body!) (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
