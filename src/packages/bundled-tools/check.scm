;; The TeXmacs side of packages/bundled-tools/check.sh: the Ghostscript and
;; the Aspell of the package, as TeXmacs uses them (#662).
;;
;;   texmacs -x '(load "check.scm")' -x '(bundled-tools-check DIR "ghostscript"|"aspell"...)'
;;
;; DIR is a scratch folder, with test.eps in it. The results are lines
;; "CHECK name value" on the standard output, read by check.sh.

(use-modules (convert images init-images))

(define (cbt-line name value)
  (display* "CHECK " name " " value "\n"))

;; an EPS image typeset in a document and written to PDF: TeXmacs
;; converts the EPS with Ghostscript (image_to_pdf, gs_to_pdf); the program
;; it runs is the one named by gs-binary, which looks in $TEXMACS_PATH/bin
;; first, as the C++ code does (gs_embedded)
(define (cbt-eps dir)
  (let* ((doc (string-append dir "/eps-doc.tm"))
         (pdf (string-append dir "/eps-doc.pdf"))
         (u (system->url doc)))
    ;; the command lines of the conversions are in the log
    (debug-set "convert" #t)
    (cbt-line "gs-binary" (gs-binary))
    (cbt-line "appdir" (or (getenv "APPDIR") ""))
    (cbt-line "tmpdir" (or (getenv "TMPDIR") ""))
    (cbt-line "has-gs" (if (has-gs?) "yes" "no"))
    (string-save
     (string-append
      "<TeXmacs|2.1.4>\n\n<style|generic>\n\n<\\body>\n"
      "  <image|test.eps|3cm|||>\n</body>\n")
     doc)
    (load-buffer u)
    (switch-to-buffer u)
    (update-forced)
    (print-to-file (system->url pdf))
    (cbt-line "pdf" (if (url-exists? (system->url pdf)) "yes" "no"))
    (cbt-line "pdf-size"
              (if (url-exists? (system->url pdf))
                  (number->string (string-length (string-load (system->url pdf))))
                  "0"))))

;; the spell checker: dictionaries found, words known and unknown
(define (cbt-spell)
  (for-each
   (lambda (t)
     (let* ((lan (car t))
            (r (single-spell-start lan)))
       (cbt-line (string-append "spell-" lan "-start") r)
       (when (== r "ok")
         (cbt-line (string-append "spell-" lan "-good")
                   (if (spell-check? lan (cadr t)) "yes" "no"))
         (cbt-line (string-append "spell-" lan "-bad")
                   (if (spell-check? lan (caddr t)) "yes" "no"))
         (single-spell-done lan))))
   '(("english" "language" "lnaguage")
     ("french" "bonjour" "bonjuor")
     ("german" "Sprache" "Spracle"))))

;; a language which the dictionaries of the package do not have (Greek): the
;; external spell checker which the user has (hunspell, aspell), else a clear
;; error
(define (cbt-spell-external)
  (let ((r (single-spell-start "greek")))
    (cbt-line "ext-greek-start" r)
    (when (== r "ok")
      (cbt-line "ext-greek-good" (if (spell-check? "greek" "kalimera") "yes" "no"))
      (cbt-line "ext-greek-bad" (if (spell-check? "greek" "zzzz") "yes" "no"))
      (single-spell-done "greek"))))

(define (bundled-tools-check dir . what)
  (catch #t
    (lambda ()
      (when (member "ghostscript" what) (cbt-eps dir))
      (when (member "aspell" what) (cbt-spell) (cbt-spell-external))
      (when (member "external" what) (cbt-spell-external)))
    (lambda args (cbt-line "error" args)))
  (cbt-line "done" "yes")
  (quit-TeXmacs))
