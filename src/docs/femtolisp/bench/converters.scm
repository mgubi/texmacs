;; Benchmark of the converters: a set of documents of the TeXmacs tree is
;; exported to Markdown, Word (.docx), OpenDocument (.odt), LaTeX and HTML,
;; and imported back from the first three. Five rounds in one process: the
;; first one also loads the converters.
(define (now) (texmacs-time))
(define root (url->string (url-concretize "$TEXMACS_PATH")))
(define files
  '("doc/about/changes/change-log.en.tm"
    "doc/devel/source/gui.en.tm"
    "doc/devel/source/renderer-api.en.tm"
    "doc/devel/source/font-database-selection.en.tm"
    "doc/main/links/man-zotero.en.tm"
    "doc/main/links/man-bibliography.en.tm"
    "doc/main/math/man-math.en.tm"
    "doc/main/table/man-table.en.tm"
    "doc/main/text/man-text.en.tm"
    "doc/main/start/man-getting-started.en.tm"))
(define docs
  (map (lambda (f)
         (convert (string-load (string->url (string-append root "/" f)))
                  "texmacs-document" "texmacs-stree"))
       (list-filter files (lambda (f) (url-exists? (string->url (string-append root "/" f)))))))
(define (size x)
  (cond ((string? x) (string-length x))
        ((pair? x) (apply + (map size x)))
        (else 1)))
(define (do-export fm) (map (lambda (t) (convert t "texmacs-stree" fm)) docs))
(define (do-import fm l) (map (lambda (s) (convert s fm "texmacs-stree")) l))
(define (timed thunk)
  (let* ((t0 (now)) (r (thunk))) (cons (- (now) t0) r)))
(define (run-round nr)
  (let* ((md (timed (lambda () (do-export "markdown-document"))))
         (md* (timed (lambda () (do-import "markdown-document" (cdr md)))))
         (docx (timed (lambda () (do-export "docx-document"))))
         (docx* (timed (lambda () (do-import "docx-document" (cdr docx)))))
         (odt (timed (lambda () (do-export "odt-document"))))
         (odt* (timed (lambda () (do-import "odt-document" (cdr odt)))))
         (tex (timed (lambda () (do-export "latex-document"))))
         (html (timed (lambda () (do-export "html-document")))))
    (display* "BENCH round " nr
              " md-out " (car md) " md-in " (car md*)
              " docx-out " (car docx) " docx-in " (car docx*)
              " odt-out " (car odt) " odt-in " (car odt*)
              " latex-out " (car tex) " html-out " (car html)
              " sizes " (map size (list (cdr md) (cdr md*) (cdr docx*) (cdr odt*)
                                        (cdr tex) (cdr html)))
              " docs " (length docs) "\n")))
(for-each run-round (list 1 2 3 4 5))
