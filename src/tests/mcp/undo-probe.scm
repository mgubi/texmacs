;; An undo in a headless process (it crashed: the beep of the undo that
;; restores the saved state): one process, no sharing; the undo of the editor
;; undoes the steps of its own author and skips those of another author
;; which commute.
(define (show-ok name ok) (display* "CHECK " name " " (if ok "PASS" "FAIL") "\n"))
(define f (getenv "UNDO_FILE"))
(catch #t
  (lambda ()
    (load-buffer f)
    (let* ((u (system->url f)) (b (buffer-get-body u)) (other (new-author)))
      (with-buffer u
        (archive-state) (clear-undo-history)
        (start-editing) (tree-insert (tree-ref b 0) 0 "AG-") (end-editing)
        (with-author other (tree-insert (tree-ref b 1) 6 "BB-"))
        (start-editing) (tree-insert (tree-ref b 0) 0 "AG2-") (end-editing)
        (undo 0)
        (show-ok "undo of the own last step" (== (tm->stree (tree-ref b 0)) "AG-First paragraph."))
        (undo 0)
        (show-ok "undo skips the step of the other author which commutes"
                 (and (== (tm->stree (tree-ref b 0)) "First paragraph.")
                      (== (tm->stree (tree-ref b 1)) "SecondBB- paragraph.")))
        (undo 0)
        (show-ok "an undo back to the saved state does not crash" #t))))
  (lambda args (display* "CHECK exception " args " FAIL\n")))
(display* "UNDO-DONE\n")
(quit-TeXmacs)
