;; The widgets of the dialogs: a choice list, an enum (its list), a
;; filtered choice and a tree; the choices made come back to the callbacks
;; (the tree with the strings of its command and user roles).
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
      ;; the callback gets the mouse buttons (-1 for the keyboard), the
      ;; string of the command role (else the subtree), then those of the
      ;; user roles, UserRole:1 first (QTMTreeView::callOnChange)
      (tree-view (lambda x
                   (gt-record "tree"
                              (if (and (>= (length x) 3)
                                       (string? (cadr x))
                                       (string? (caddr x)))
                                  (string-append ": " (cadr x) " " (caddr x))
                                  "")))
                 (stree->tree '(root (item "one" "cmd-one" "user-one")
                                     (item "two" "cmd-two" "user-two")))
                 (stree->tree '(tuple (item "DisplayRole" "CommandRole"
                                            "UserRole:1")))))))
(define (got? s) (in? s (gt-recorded)))
(define before-tree 0) ; what was recorded before the click on the tree
(gui-test-run
 (list (gt-wait 6000)
       (gt-eval-later "(top-window gui-widgets-test \"Widgets test\")")
       (gt-window "Widgets test")
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
       (gt-eval (lambda () (set! before-tree (length (gt-recorded)))))
       (gt-activate "two")
       (gt-idle)
       (gt-check "the tree, with its command and user roles (#393)"
                 (lambda () (in? "tree: cmd-two user-two"
                                 (list-tail (gt-recorded) before-tree))))
       (gt-exit)))
