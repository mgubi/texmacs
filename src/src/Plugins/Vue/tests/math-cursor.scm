;; The cursor in a formula with primes, wide accents and negations, which the
;; OpenType math fonts put in boxes of their own (C5 of
;; src/doc/opentype-audit.md): the box-to-tree direction, which needs a
;; window. math-cursor.script (made by math-cursor.py) presses Right through
;; the formula and records each step (path, position), then clicks at each
;; position: the click gives back the path of the step, except where two
;; paths share a position (the end of an accent and after it), which is
;; reported and the same in every font; in the TeX font and in STIX Two Math
;; the steps are the same. One action per line of the script, so that the
;; editor has a turn of its loop between them.

(define c5-formula
  '(math (concat "x" (rprime "'") "+f" (rprime "''") "+" (wide "a" "^")
                 "+" (wide "xy" "~") "<neq>b<nin>C" (neg "<sim>") "d")))

(define c5-fonts
  (list "roman" "math=STIX Two Math,Stix Two Text"))

(define c5-steps '())       ; this font, ((path x y) ...), reversed
(define c5-runs '())        ; (font . paths) of the fonts done
(define c5-bad 0)

(define (c5-out . l) (apply display* (cons "C5 " l)) (display "\n"))

(define (c5-setup)
  (new-document)
  (insert c5-formula))

(define (c5-font i)
  (init-env "font" (list-ref c5-fonts i))
  (set! c5-steps '())
  (set! c5-bad 0)
  (tree-go-to (car (select (buffer-tree) '(:* math))) 0 :start))

(define (c5-record)
  (with s (list (cursor-path) (get-cursor-x) (get-cursor-y))
    (when (and (inside? 'math)
               (or (null? c5-steps) (!= (car s) (caar c5-steps))))
      (set! c5-steps (cons s c5-steps)))))

(define (c5-click-press j)
  (let ((l (reverse c5-steps)))
    (when (< j (length l))
      (with s (list-ref l j)
        (mouse-any "press-left" (+ (cadr s) 64) (+ (caddr s) 256) 1 0.0 '())
        (mouse-any "release-left" (+ (cadr s) 64) (+ (caddr s) 256) 0 0.0 '())))))

(define (c5-click-check j)
  (let ((l (reverse c5-steps)))
    (when (< j (length l))
      (with s (list-ref l j)
        (when (!= (cursor-path) (car s))
          (set! c5-bad (+ c5-bad 1))
          (c5-out "click " j " at " (cadr s) "," (caddr s)
                  " gave " (cursor-path) " not " (car s)))))))

(define (c5-font-done i)
  (with l (reverse c5-steps)
    (for (j (.. 0 (length l)))
      (with s (list-ref l j)
        (c5-out (list-ref c5-fonts i) " step " j " " (car s) " "
                (cadr s) " " (caddr s))))
    (c5-out (list-ref c5-fonts i) " summary steps " (length l)
            " clicks-wrong " c5-bad)
    (set! c5-runs (append c5-runs (list (map car l))))))

(define (c5-end)
  (for (i (.. 1 (length c5-runs)))
    (c5-out (list-ref c5-fonts i) " same-steps-as-roman "
            (== (list-ref c5-runs i) (car c5-runs))))
  (c5-out "done"))
