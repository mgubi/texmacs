(use-modules (kernel gui menu-test))

(tm-widget (vue-test)
  (padded
    (hlist
      (resize "200px" "120px"
        (choice (display* "choice: " answer "\n")
                '("First" "Second" "Third" "Fourth") "Third"))
      ///
      (resize "200px" "120px"
        (choices (display* "choices: " answer "\n")
                 '("First" "Second" "Third" "Fourth") '("Third"))))
    ===
    (hlist
      (text "Enum:") //
      (enum (display* "enum: " answer "\n") '("Alpha" "Beta" "Gamma") "Beta" "10em")
      ///
      (toggle (display* "toggle: " answer "\n") #f))
    ===
    (resize "400px" "200px"
      (filtered-choice (display* "filtered: " answer "\n")
                       '("apple" "apricot" "banana" "cherry" "grape" "melon"
                         "orange" "peach" "pear" "plum")
                       "cherry" "a"))
    ===
    (resize "400px" "200px"
      (tree-view (lambda x (display* "tree: " x "\n"))
                 (stree->tree '(root (item "one" (item "one.a") (item "one.b"))
                                     (item "two" (item "two.a"))))
                 (stree->tree '(tuple (item "DisplayRole")))))
    ===
    (ink (display* "ink: " answer "\n"))))

(tm-define (show-big menu-promise name)
  (let* ((win (alt-window-handle))
         (men (menu-promise))
         (wid (make-menu-widget* (list 'vertical men) 0)))
    (alt-window-create-plain win wid name)
    (alt-window-set-size win 520 1000)
    (alt-window-show win)))

(delayed (:pause 1500) (show-big vue-test "Vue test"))
