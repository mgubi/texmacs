;; The time of the regression suites csl and bibtex.
(define t0 (texmacs-time))
(define f (run-regression-suite "csl"))
(define t1 (texmacs-time))
(define g (run-regression-suite "bibtex"))
(define t2 (texmacs-time))
(display* "BENCH csl-suite " (- t1 t0) " failures " f
          " bibtex-suite " (- t2 t1) " failures " g "\n")
