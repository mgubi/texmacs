
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
  (:use (convert latex tmtex-pdflatex)
        (kernel boot abbrevs-test)
        (kernel logic logic-engine-test)
        (kernel texmacs tm-define-test)
        (kernel texmacs tm-dialogue-test)
        (kernel texmacs tm-convert-test)
        (kernel texmacs tm-glue-test)
        (convert html htmltm-test)
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
        (kernel regexp regexp-test)
        (kernel logic logic-test)
        (check glue-test)
        (check lists-test)
        (check base-test)
        (check trees-test)
        (check define-test)
        (check latex-test)
        (check formats-test)
        (check markdown-test)
        (check office-test)
        (check editing-test)
        (check typeset-test)
        (check bibtex-test)
        (check csl-test)
        (check zotero-test)
        (check database-test)
        (check math-edit-test)
        (check table-test)
        (check text-structure-test)
        (check graphics-edit-test)
        (check version-test)
        (check git-test)
        (check links-test)
        (check misc-modules-test)
        (check remote-test)
        (check kbd-menu-test)
        (check structures-test)
        (check convert-more-test)
        (check parse-test)
        (check macro-drd-test)
        (check crypto-test)
        (check plugins-test)
        (check ai-test)))

;; test suites which only make sense with S7
(if (s7-scheme?)
    (use-modules (kernel boot compat-s7-test) (kernel boot boot-s7-test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test LaTeX export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (append
   (if (s7-scheme?)
       '(("compat-s7" regtest-compat-s7 error)
         ("boot-s7" regtest-boot-s7 error))
       '())
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
    ("regexp" regtest-regexp error)
    ("logic-query" regtest-logic-queries error)
    ("glue" glue-test-failures count)
    ("lists" lists-test-failures count)
    ("base" base-test-failures count)
    ("trees" trees-test-failures count)
    ("latex" latex-test-failures count)
    ("formats" formats-test-failures count)
    ("markdown" markdown-test-failures count)
    ("office" office-test-failures count)
    ;; opens buffers and edits them
    ("editing" editing-test-failures count)
    ("typeset" typeset-test-failures count)
    ;; after editing: generating a bibliography or the auxiliary data of a
    ;; document processes the pending GUI events, among which those of the
    ;; views which editing closed (#174)
    ("bibtex" bibtex-test-failures count)
    ("csl" csl-test-failures count)
    ;; generates the auxiliary data of documents in the temporary directory
    ("links" links-test-failures count)
    ("structures" structures-test-failures count)
    ("convert-more" convert-more-test-failures count)
    ("math-edit" math-edit-test-failures count)
    ("table" table-test-failures count)
    ("text-structure" text-structure-test-failures count)
    ("graphics-edit" graphics-edit-test-failures count)
    ;; makes a throwaway git repository in the temporary directory
    ("version" version-test-failures count)
    ;; makes throwaway git repositories in the temporary directory, and
    ;; runs this TeXmacs as their merge driver
    ("git" git-test-failures count)
    ("misc-modules" misc-modules-test-failures count)
    ;; loads the keyword tables of the program languages
    ("parse" parse-test-failures count)
    ("database" database-test-failures count)
    ;; with a fake Zotero; opens documents (so after links) and loads the
    ;; modules of the bibliographic database (so after database); writes
    ;; bibliographies and a database in the temporary directory
    ("zotero" zotero-test-failures count)
    ("crypto" crypto-test-failures count)
    ;; the answers of the AI engines, converted without a network
    ("ai" ai-test-failures count)
    ;; server and clients in this process, with databases in the temporary
    ;; directory and the server files of the (scratch) home, which it cleans
    ("remote" remote-test-failures count)
    ;; starts plugin processes (shell, python when present) and stops them
    ("plugins" plugins-test-failures count)
    ;; loads every lazy menu, and maps and unmaps test keys
    ("kbd-menu" kbd-menu-test-failures count)
    ;; loads every style package, and with them the Scheme modules they use
    ("macro-drd" macro-drd-test-failures count)
    ("tm-define-regression" regtest-tm-define error)
    ("tm-dialogue" regtest-tm-dialogue error)
    ("abbrevs" regtest-abbrevs error)
    ("logic" regtest-logic error)
    ("tm-glue" regtest-tm-glue error)
    ;; last, since it defines modes and functions in the running TeXmacs
    ("tm-define" define-test-failures count))))

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
