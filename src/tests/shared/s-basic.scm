;; Join, edits on both sides, a save by the host.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "host" (check "A hosts" (== (role-of) 'host)))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (act "edit A" (insert-text! 1 5 "AA"))
   (act "sig" (sync-set! "a-edited"))
   (wait-sync "b-edited")
   (step "A sees B" (and (== (para-text 2) "Second parBBagraph.") #t))
   (act "A text" (check "A has both edits" (and (== (para-text 1) "HelloAA world.")
                                                  (== (para-text 2) "Second parBBagraph."))))
   (wait-sync "b-saved-req")
   (step "A saved" (string-contains? (string-load (the-name)) "parBBagraph"))
   (act "A once" (check "the host saved once" (== (shared-stat 'saves) 1)))
   (act "A out" (sync-write! "A-body" (object->string (body-stree))) (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "participant" (check "B participates" (== (role-of) 'participant)))
   (act "content" (check "B got the document" (== (para-text 1) "Hello world.")))
   (act "sig" (sync-set! "b-joined"))
   (wait-sync "a-edited")
   (step "B sees A" (== (para-text 1) "HelloAA world."))
   (act "edit B" (insert-text! 2 10 "BB"))
   (act "sig" (sync-set! "b-edited"))
   
   (act "B save" (save-buffer-main (the-name)) (sync-set! "b-saved-req"))
   (wait-sync "a-done")
   (act "B out" (sync-write! "B-body" (object->string (body-stree))) (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
