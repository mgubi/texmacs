;; Concurrent edits in the same paragraph at different places, or one around
;; a deletion elsewhere, are merged with nothing lost or undone; edits at the
;; same place conflict: the person's text is then saved in a document, and
;; the person is told.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (steps)
  (append
   (if (== ROLE "A")
       (list (act "load" (load-buffer FILE)) (act "go" (sync-set! "a-loaded"))
             (wait-sync "b-joined"))
       (list (wait-sync "a-loaded") (act "load" (load-buffer FILE))
             (act "old file" (let ((d (string-append (getenv "TEXMACS_HOME_PATH") "/system/recovered")))
                               (system-mkdir (string->url d))
                               (string-save "x" (string->url (string-append d "/shared-lost-100-1-1.tm")))))
             (act "go" (sync-set! "b-joined"))))
   ;; round 1: the same paragraph, different places
   (round-steps 1 (lambda () (insert-text! 1 2 "XX")) (lambda () (insert-text! 1 8 "YY")))
   (if (== ROLE "B")
       (list (act "check 1" (check "different places: merged" (== (text-of 1) "HeXXllo woYYrld."))
                  (check "nothing undone" (and (== (shared-stat 'dropped) 0) (== (shared-stat 'lost) 0)))
                  (check "nothing left pending" (== (pending-count) 0))))
       (list (act "check 1" (check "host has both" (== (text-of 1) "HeXXllo woYYrld.")))))
   ;; round 2: the same place
   (round-steps 2 (lambda () (insert-text! 2 6 "AA")) (lambda () (insert-text! 2 6 "BB")))
   (if (== ROLE "B")
       (list (act "check 2"
                  (check "same place: host's text kept" (== (text-of 2) "SecondAA paragraph."))
                  (check "told" (== (shared-stat 'lost) 1))
                  (check "text saved" (and (== (shared-stat 'lost-saved) 1)
                                           (list-or (map (lambda (f) (string-contains? (string-load (string->url f)) "BB"))
                                                         (recovered-files)))))))
       (list (act "check 2" (check "host unchanged" (== (text-of 2) "SecondAA paragraph.")))))
   ;; round 3: a deletion in one place, an insertion in another
   (round-steps 3 (lambda () (tree-remove (body) 1 1)) (lambda () (insert-text! 3 3 "ZZ")))
   (if (== ROLE "B")
       (list (act "check 3" (check "deletion and insertion merged" (== (text-of 2) "ThiZZrd one."))
                  (check "no more loss" (== (shared-stat 'lost) 1))))
       (list (act "check 3" (check "host has both" (== (text-of 2) "ThiZZrd one.")))))
   ;; round 4: both delete the same text; what B deleted is in its document
   (round-steps 4 (lambda () (tree-remove (para 1) 4 3)) (lambda () (tree-remove (para 1) 4 2)))
   (if (== ROLE "B")
       (list (act "check 4"
                  (check "the deleted text is recovered, and the old file pruned"
                         (and (== (shared-stat 'lost-saved) 2)
                              (list-or (map (lambda (f) (string-contains? (string-load (string->url f)) "Deleted"))
                                            (recovered-files)))
                              (not (list-or (map (lambda (f) (string-contains? f "shared-lost-100-")) (recovered-files))))))))
       '())
   (list (act "out" (publish-body!) (sync-set! (string-append (string-downcase ROLE) "-fin")))
         (wait-sync (if (== ROLE "A") "b-fin" "a-fin")))))

(run-steps (steps))
