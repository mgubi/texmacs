
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : check-master.scm
;; DESCRIPTION : regression tests
;; COPYRIGHT   : (C) 2014  Joris van der Hoeven
;;                   2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (check check-master)
  (:use (convert html htmltm-test)
        (convert html tmhtml-test)
        (convert tools xmltm-test)
        (convert tools tmlength-test)
        (convert tools environment-test)
        (convert mathml mathtm-test)
        (convert tmml tmmltm-test)
        (prog prog-format-test)
        (server server-cache-test)
        (server server-backup-test)
        (server server-notifications-test)
        (server server-tmfs-test)
        (utils cite cite-sort-test)
        (kernel texmacs tm-convert-test)
        (check glue-test)
        (check lists-test)
        (check base-test)
        (check trees-test)
        (check define-test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test LaTeX export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (run-pdflatex tex-file)
  (and (url-exists? tex-file)
       (let* ((tex-dir  (url-head tex-file))
              (pdf-file (url-glue (url-unglue tex-file 4) ".pdf"))
              (log-file (url-glue (url-unglue tex-file 4) ".log"))
              (cmd1 (string-append "cd " (system-url->string tex-dir)))
              (cmd2 (string-append "pdflatex -interaction=batchmode "
                                   (url->string (url-tail tex-file))))
              (cmd  (string-append cmd1 "; " cmd2 " > /dev/null")))
         (system-remove pdf-file)
         (system-remove log-file)
         (system cmd)
         (and (url-exists? log-file)
              (list (url-exists? pdf-file)
                    (number-latex-errors log-file)
                    (number-latex-pages log-file))))))

(define (check-latex-export-one tm-file)
  (display* "Checking LaTeX export of " (url->string tm-file) "...\n")
  (with latex-file (url-glue (url-unglue tm-file 3) ".tex")
    (with-aux tm-file
      (system-remove latex-file)
      (if (url? latex-file) (set! current-save-target latex-file))
      (export-buffer-main (current-buffer) latex-file
                          "latex" (list :overwrite))
      (display* "Checking LaTeX on " (url->string latex-file) "...\n")
      (with status (run-pdflatex latex-file)
        (cond ((not status) (display* "  LaTeX export failed\n"))
              ((not (car status)) (display* "  PdfLaTeX failed\n"))
              (else
                (with (ok? errs pages) status
                  (when (or (!= errs 0) (<= pages 0))
                    (display* "  PdfLaTeX encountered " errs " error(s)\n")
                    (display* "  PdfLaTeX produced " pages " page(s)\n")))))))))

(tm-define (check-latex-export u)
  (:synopsis "Try to export and LaTeX all TeXmacs files inside @u")
  (let* ((tm-files (url-append u (url-append (url-any) "*.tm")))
         (l (url->list (url-expand (url-complete tm-files "fr")))))
    (for (x l)
      (check-latex-export-one x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All regression tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (check-all u)
  (:synopsis "Run all regression tests in directory @u")
  (check-latex-export u))

(tm-define (run-checks)
  (check-latex-export "$TEXMACS_CHECKS/latex-export"))

;; The suites: a name, the function which runs it, and how it reports its
;; failures: count (it returns their number), error (it raises an error at
;; the first one) or integration (integration-test-group adds them to
;; integration-failure-total).
(define regression-suites
  '(("htmltm" regtest-htmltm error)
    ("xmltm" regtest-xmltm error)
    ("tmlength" regtest-tmlength error)
    ("environment" regtest-environment error)
    ("mathtm" regtest-mathtm error)
    ("tmhtml" regtest-tmhtml error)
    ("tmmltm" regtest-tmmltm error)
    ("prog-format" regtest-prog-format error)
    ("cite-sort" regtest-cite-sort error)
    ("tm-convert" regtest-tm-convert error)
    ("glue" glue-test-failures count)
    ("lists" lists-test-failures count)
    ("base" base-test-failures count)
    ("trees" trees-test-failures count)
    ;; last, since it defines modes and functions in the running TeXmacs
    ("tm-define" define-test-failures count)))

(define integration-suites
  '(("deletion-plan" regtest-deletion-plan integration)
    ("server-notifications" regtest-server-notifications integration)
    ("server-backup" regtest-server-backup integration)
    ("server-cache" regtest-server-cache integration)))

;; the number of failures of a suite; an error which escapes the suite
;; counts as one, and does not stop the suites after it
(define (suite-failures suite)
  (let ((f (eval (cadr suite)))
        (kind (caddr suite)))
    (set! integration-failure-total 0)
    (catch #t
      (lambda ()
        (with r (f)
          (cond ((== kind 'count) r)
                ((== kind 'integration) integration-failure-total)
                (else 0))))
      (lambda (key . args)
        (display* "  error in suite " (car suite) ": "
                  (object->string (cons key args)) "\n")
        (+ integration-failure-total 1)))))

(define (run-suites suites)
  (let ((failed '()))
    (for (suite suites)
      (with n (suite-failures suite)
        (when (> n 0) (set! failed (cons (car suite) failed)))))
    (display* "Suites: " (number->string (length suites)) ", failed: "
              (if (null? failed) "none"
                  (string-recompose (reverse failed) ", "))
              "\n")
    (length failed)))

(tm-define (test-suite-names)
  (:synopsis "The names of the test suites")
  (map car (append regression-suites integration-suites)))

(tm-define (run-regression-suite name)
  (:synopsis "Run the test suite @name and return its number of failures")
  (with suite (or (assoc name regression-suites)
                  (assoc name integration-suites))
    (if suite (suite-failures suite)
        (begin (display* "no test suite " name "\n") 1))))

(tm-define (run-all-tests)
  (:synopsis "Run the regression tests; the number of failed suites")
  (run-suites regression-suites))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integration tests (side-effecting, with setup/teardown)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (run-integration-tests)
  (:synopsis "Run the integration tests; the number of failed suites")
  (run-suites integration-suites))
