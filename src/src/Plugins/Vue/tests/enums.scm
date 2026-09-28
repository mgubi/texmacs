;; the enums: an editable one (the last value is empty, as in Qt), one whose
;; values are of very different widths (it is as wide as the widest), a
;; choice list longer than its box (it scrolls), and a long enum at the
;; bottom of the window (its list opens above it, fits in the window and
;; scrolls)
(define (enum-items n)
  (if (= n 0) '()
      (append (enum-items (- n 1))
              (list (string-append "Item " (number->string n))))))

(tm-widget (vue-enums)
  (padded
    (hlist
      (text "Editable:") //
      (enum (display* "enum: " answer "\n")
            '("10pt" "11pt" "12pt" "") "11pt" "")
      >>)
    ===
    (hlist
      (text "Widths:") //
      (enum (display* "enum: width " answer "\n")
            '("a" "a much longer value" "b") "a" "")
      >>)
    ===
    (hlist
      (resize "200px" "100px"
        (choice (display* "choice: " answer "\n")
                (enum-items 12) "Item 2"))
      >>)
    (glue #f #t 0 0)
    (hlist
      (text "Long:") //
      (enum (display* "enum: long " answer "\n") (enum-items 40) "Item 3" "")
      >>)))

(tm-define (show-enums)
  (let* ((win (alt-window-handle))
         (wid (make-menu-widget* (list 'vertical (vue-enums)) 0)))
    (alt-window-create-plain win wid "Vue enums")
    (alt-window-set-size win 420 520)
    (alt-window-show win)))

(delayed (:pause 1500) (show-enums))
