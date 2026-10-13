;; Benchmark of the CSL processor (branch wip_csl): loading, the fixtures
;; of the CSL test suite, and bibliographies of 300 references in three
;; styles. The directory $BENCH_CSL holds
;;   humans/    processor-tests/humans of citation-style-language/test-suite
;;   locales/   the files locales-*.xml of citation-style-language/locales
;;   big.bib    made by mkbib.py
(define data (getenv "BENCH_CSL"))
(define (now) (texmacs-time))
(define t0 (now))
(use-modules (csl csl-utils) (csl csl-style) (csl csl-fixtures) (csl csl-bib)
             (convert bibtex init-bibtex))
(define t1 (now))
(set! csl-extra-path (list (string->url data)))
(define report (string-append data "/report.txt"))
(define r (csl-run-fixtures (string->url (string-append data "/humans")) (string->url report)))
(define t2 (now))
(define bib (with d (convert (string-load (string->url (string-append data "/big.bib"))) "bibtex-document" "bibtex-stree")
              (cadr (assoc 'body (cdadr d)))))
(define t3 (now))
(define (count-entries x)
  (cond ((func? x 'bib-list) (length (cdr (caddr x))))
        ((pair? x) (apply + (map count-entries (cdr x))))
        (else 0)))
(define n (apply + (map (lambda (style) (count-entries (csl-bib-process "bib" style bib '() "en-US")))
                        '("csl-apa" "csl-chicago-author-date" "csl-ieee"))))
(define t4 (now))
(display* "BENCH load " (- t1 t0) " fixtures " (- t2 t1) " parse " (- t3 t2) " bib " (- t4 t3)
          " results " r " entries " n "\n")
