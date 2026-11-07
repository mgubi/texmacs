;; a tree view follows its tree: nodes inserted and removed by Scheme
;; after the window is shown appear without any event (the snapshot taken
;; at each redraw shows "zero", "one", "two" and no "three")
(define tree-observe-data
  (stree->tree '(root (item "one" (item "one.a") (item "one.b"))
                      (item "three"))))
(tm-widget (vue-tree-observe)
  (resize "200px" "200px"
    (tree-view (lambda x (display* "tree: " x "\n"))
               tree-observe-data
               (stree->tree '(tuple (item "DisplayRole"))))))
(delayed (:pause 1500) (top-window vue-tree-observe "Vue tree"))
(delayed (:pause 5000)
  (tree-insert! tree-observe-data 0 (list (stree->tree '(item "zero"))))
  (tree-insert! tree-observe-data 2 (list (stree->tree '(item "two"))))
  (tree-remove! tree-observe-data 3 1)
  (display* "got: modified " (tree->stree tree-observe-data) "\n"))
