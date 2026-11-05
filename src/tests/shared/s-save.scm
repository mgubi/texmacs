;; A Save in a participant waits for its changes to reach the host; closing
;; a participant sends them; a buffer removed without buffer-close leaves the
;; sharing.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE))
   (act "go" (sync-set! "a-loaded"))
   (wait-sync "b-saving")
   (step "file has B's text" (string-contains? (string-load (the-name)) "SaveBB"))
   (act "check" (check "host wrote once" (== (shared-stat 'saves) 1)))
   ;; a change is sent and a second one is made before the host has taken the
   ;; first: Save must wait for both
   (wait-sync "b-saving2")
   (step "file has both" (let ((f (string-load (the-name))))
                           (and (string-contains? f "S1") (string-contains? f "S2"))))
   (act "check" (check "host wrote twice" (== (shared-stat 'saves) 2)))
   (wait-sync "b-closed")
   (step "A has B's last edit" (== (text-of 2) "SecCLOSEond paragraph."))
   (wait-sync "b-rejoined")
   (act "A closes with the C++ call" (cpp-buffer-close (the-name)) (sync-set! "a-gone"))
   (wait-sync "b-done")))

(define n0 0)
(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "edit and save at once" (insert-text! 1 5 "SaveBB") (save-buffer-main (the-name))
        (sync-set! "b-saving"))
   (step "A saved" (string-contains? (string-load (the-name)) "SaveBB"))
   (act "two edits and a save"
        (set! n0 (shared-stat 'submitted)) (insert-text! 1 5 "S1"))
   (step "second edit while the first is on its way"
         (and (> (shared-stat 'submitted) n0)
              (begin (insert-text! 1 5 "S2") (save-buffer-main (the-name))
                     (sync-set! "b-saving2") #t)))
   (step "A saved again" (let ((f (string-load (the-name)))) (and (string-contains? f "S1") (string-contains? f "S2"))))
   (act "edit and close at once" (insert-text! 2 3 "CLOSE") (buffer-close (the-name))
        (sync-set! "b-closed"))
   (act "check" (check "B left" (not (shared-file? (the-name)))))
   (act "load again" (load-buffer FILE))
   (act "rejoined" (check "B joins A again, with its last edit"
                          (and (== (role-of) 'participant) (== (text-of 2) "SecCLOSEond paragraph.")))
        (sync-set! "b-rejoined"))
   (wait-sync "a-gone")
   (step "B hosts" (== (role-of) 'host))
   (act "done" (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
