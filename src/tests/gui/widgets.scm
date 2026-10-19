;; The widgets of the dialogs: a choice list, an enum (its list), a
;; filtered choice and a tree; the choices made come back to the callbacks.
;; The two lists have the same items: they are told apart by names
;; (named ...). Runs on every port with the hooks of the tests
;; (docs/gui-scripting.md):
;;   SCM=$PWD/tests/gui/widgets.scm sh tests/Plugins/Vue/run.sh widgets 40
(use-modules (check gui-driver))
(tm-widget (gui-widgets-test)
  (padded
    (hlist
      (named "single"
        (resize "200px" "120px"
          (choice (gt-record "choice: " answer)
                  '("First" "Second" "Third" "Fourth") "Third")))
      ///
      (named "multiple"
        (resize "200px" "120px"
          (choices (gt-record "choices")
                   '("First" "Second" "Third" "Fourth") '("Third")))))
    ===
    (hlist
      (text "Enum:") //
      (enum (gt-record "enum: " answer) '("Alpha" "Beta" "Gamma") "Beta" "10em"))
    ===
    (resize "400px" "160px"
      (filtered-choice (gt-record "filtered: " answer)
                       '("apple" "apricot" "banana" "cherry" "grape")
                       "cherry" ""))
    ===
    (resize "400px" "160px"
      (tree-view (lambda x (gt-record "tree"))
                 (stree->tree '(root (item "one") (item "two")))
                 (stree->tree '(tuple (item "DisplayRole")))))))
(define (got? s) (in? s (gt-recorded)))
(gui-test-run
 (list (gt-wait 6000)
       (gt-eval-later "(top-window gui-widgets-test \"Widgets test\")")
       (gt-wait 1500)
       (gt-raw "window Widgets test")
       (gt-raw "focus")
       (gt-idle)
       (gt-activate "#single > Second")
       (gt-idle)
       (gt-check "the choice" (lambda () (got? "choice: Second")))
       (gt-activate "Beta")
       (gt-idle)
       (gt-activate "Gamma")
       (gt-idle)
       (gt-check "the enum" (lambda () (got? "enum: Gamma")))
       (gt-activate "banana")
       (gt-idle)
       (gt-check "the filtered choice" (lambda () (got? "filtered: banana")))
       (gt-activate "two")
       (gt-idle)
       (gt-check "the tree" (lambda () (got? "tree")))
       (gt-exit)))
