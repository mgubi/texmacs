;; the commands of the menus of the tags of the footer act on their tag,
;; also those which look for the innermost tag of their kind around the
;; cursor (footer-run-in in footer-menu.scm): with the cursor in an inline
;; formula inside a text of a displayed formula, footer-variants.script
;; makes the displayed formula inline from its menu (Displayed formula >
;; Inline formula), then types a letter, which must land in the inner
;; formula (the cursor goes back where it was); the end says what came out:
;;   got: (math text math) (document (concat "Some text " (math (concat "a+" (text (concat "if " (math "xy"))))))) (0 0 1 0 1 0 1 0 2)
;;
;; The preference is set in the home of the run (run.sh makes a copy of
;; the home for it and removes it afterwards).
(set-preference "interactive footer" "on")
(delayed (:pause 3000)
  (insert "Some text")
  (make 'equation*)
  (insert "a+")
  (make 'text)
  (insert "if ")
  (make 'math)
  (insert "x"))
(delayed (:pause 21000)
  (display* "got: " (map tree-label (footer-context-trees)) " "
            (tree->stree (buffer-tree)) " " (cursor-path) "\n"))
