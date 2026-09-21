(delayed (:pause 1500)
  (interactive (lambda (name age) (display* "got: " name " / " age "\n"))
    (list "Name" "string" "Joe" "Jane") "Age"))
