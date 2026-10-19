;; a long document, so that the wheel has something to scroll
(delayed (:pause 2000)
  (insert (stree->tree `(document ,@(map (lambda (i) (string-append "Line " (number->string i) " lorem ipsum dolor sit amet")) (.. 1 200))))))
