;; the menus of the interactive footer (footer-menu.scm): on the left the
;; Document menu, on the right the tags around the cursor, each a menu
;; which selects it or changes it where it is (its Focus menu), without
;; moving the cursor. footer-menus.script opens the menu of the theorem,
;; makes it a lemma, types a letter (which must land in the strong) and
;; opens the Document menu; the end says what came out:
;;   got: (lemma strong) (0 0 0 0 1 0 2)
;;
;; The preference is set in the home of the run (run.sh makes a copy of
;; the home for it and removes it afterwards).
(set-preference "interactive footer" "on")
(delayed (:pause 3000)
  (make 'theorem)
  (insert "Let ")
  (make 'strong)
  (insert "x"))
(delayed (:pause 21000)
  (display* "got: " (map tree-label (footer-context-trees)) " " (cursor-path)
            "\n"))
