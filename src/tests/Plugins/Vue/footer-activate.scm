;; footer-variants written as steps of the driver (check gui-driver), which
;; find the menus by their texts rather than by positions, and check the
;; result: with the cursor in an inline formula inside a text of a
;; displayed formula, the displayed formula becomes inline from its menu in
;; the footer (Displayed formula > Inline formula), and a letter typed then
;; lands in the inner formula. No script: run it with
;;   sh tests/Plugins/Vue/run.sh footer-activate 40
;; it quits by itself, with the status 1 if a check failed.
;;
;; The preference is set in the home of the run (run.sh makes a copy of
;; the home for it and removes it afterwards).
(use-modules (check gui-driver))
(set-preference "interactive footer" "on")
(gui-test-run
 (list (gt-wait 3000)
       (gt-eval (lambda ()
                  (insert "Some text")
                  (make 'equation*)
                  (insert "a+")
                  (make 'text)
                  (insert "if ")
                  (make 'math)
                  (insert "x")))
       (gt-wait 5000)
       (gt-raw "window #2")
       (gt-raw "focus")
       (gt-raw "key Escape")
       (gt-raw "key Left")
       (gt-wait 300)
       (gt-raw "key Right")
       (gt-idle)
       (gt-activate "equation*")
       (gt-idle)
       (gt-activate "Displayed formula")
       (gt-idle)
       (gt-activate "Inline formula")
       (gt-idle)
       (gt-raw "text y")
       (gt-idle)
       (gt-check "the displayed formula is inline"
                 (lambda ()
                   (== (tree->stree (buffer-tree))
                       '(document (concat "Some text "
                                          (math (concat "a+" (text (concat "if " (math "xy"))))))))))
       (gt-check "the cursor is back in the inner formula"
                 (lambda () (== (cursor-path) '(0 0 1 0 1 0 1 0 2))))
       (gt-exit)))
