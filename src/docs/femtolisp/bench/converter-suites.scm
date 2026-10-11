;; The time of the regression suites of the Markdown and office converters.
(define t0 (texmacs-time))
(define f (run-regression-suite "markdown"))
(define t1 (texmacs-time))
(define g (run-regression-suite "office"))
(define t2 (texmacs-time))
(display* "BENCH markdown-suite " (- t1 t0) " failures " f
          " office-suite " (- t2 t1) " failures " g "\n")
