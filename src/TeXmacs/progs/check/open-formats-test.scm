;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : open-formats-test.scm
;; DESCRIPTION : tests of the opening of the files of other formats
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A file is opened in the format of its suffix, whatever gives it: the Open
;; panel, the Finder, the Dock, open -a (load-buffer-from-system), a drop on
;; a document (dropped-documents) or the Recent menu (load-buffer):
;;   - the formats which can be opened (format-openable?) and the documents
;;     (document-formats, the formats of the Import menu);
;;   - Markdown, LaTeX, Html, BibTeX, Word and OpenDocument files open in
;;     their format, also empty ones, and go to the recent files;
;;   - a format without a converter to TeXmacs (pdf, png), or a converter
;;     which fails, gives a message and no buffer, not an empty document;
;;   - the files of a drop which are all documents are opened, the others
;;     are inserted as before;
;;   - the document types of the application on macOS (CFBundleDocumentTypes
;;     of packages/macos/Info.plist.in) are the suffixes of document-formats.

(texmacs-module (check open-formats-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-group thunk)
  ;; an error in a group is one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(define (tmp-dir) (url-append (url-temp-dir) "open-formats"))

(define (tmp-file name) (url-append (tmp-dir) name))

(define (clean-tmp-dir)
  ;; the files of the suite are removed (the directory stays)
  (when (url-exists? (tmp-dir))
    (for-each (lambda (u) (when (url-test? u "f") (system-remove u)))
              (url-read-directory (tmp-dir) "*")))
  (when (not (url-exists? (tmp-dir))) (system-mkdir (tmp-dir))))

(define (write-file name s)
  (with u (tmp-file name)
    (string-save s (url->system u))
    u))

(define (stree-has-string? t s)
  ;; does a string of the stree @t contain @s?
  (cond ((string? t) (string-occurs? s t))
        ((pair? t) (list-or (map (lambda (x) (stree-has-string? x s)) t)))
        (else #f)))

(define (with-messages thunk)
  ;; the messages of set-message during @thunk
  (let* ((l '()))
    (with-global set-message
        (lambda (msg . rest)
          (set! l (cons (if (string? msg) msg (object->string msg)) l)))
      (thunk))
    (reverse l)))

(define (open-and-check u text)
  ;; opens @u (as the Open panel does), checks that its document has @text
  ;; and that it is a recent file, then closes it
  (let* ((old (current-buffer)))
    (load-buffer u)
    (check-true (buffer-exists? u))
    (check= (current-buffer) u)
    (when (buffer-exists? u)
      (when text
        (check-true (stree-has-string? (tm->stree (buffer-get-body u)) text)))
      (check-true (in? (url->unix u)
                       (map cdar (learned-interactive "recent-buffer"))))
      (buffer-close u))
    (when (and old (buffer-exists? old)) (switch-to-buffer old))))

(define (check-not-opened u)
  ;; opening @u gives a message and no buffer
  (let* ((old (current-buffer))
         (msgs (with-messages (lambda () (load-buffer u)))))
    (check-false (buffer-exists? u))
    (check= (current-buffer) old)
    (check-true (list-or (map (lambda (m) (string-occurs? "Could not load" m))
                              msgs)))
    (when (buffer-exists? u) (buffer-close u))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The formats which can be opened
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-openable)
  (check-group "openable formats")
  (for-each (lambda (fm) (check-true (format-openable? fm)))
            '("texmacs" "stm" "tmml" "generic" "verbatim" "markdown" "latex"
              "html" "tmbib" "docx" "odt" "python" "scheme"))
  (for-each (lambda (fm) (check-false (format-openable? fm)))
            '("pdf" "png" "postscript" "gpg" "no-such-format"))
  (check= (url-format (string->url "a.md")) "markdown")
  (check= (url-format (string->url "a.MD")) "markdown")
  (check= (url-format (string->url "a.tex")) "latex")
  (check= (url-format (string->url "a.bib")) "tmbib")
  (check= (url-format (string->url "a.docx")) "docx")
  (check= (url-format (string->url "a.odt")) "odt")
  (check= (url-format (string->url "a.htm")) "html"))

(define (test-document-formats)
  (check-group "document formats")
  (with l (document-formats)
    (for-each (lambda (fm) (check-true (in? fm l)))
              '("texmacs" "stm" "tmml" "markdown" "latex" "html" "tmbib"
                "docx" "odt"))
    ;; programs are opened, but are not documents; images and pdf are not
    ;; opened at all
    (for-each (lambda (fm) (check-false (in? fm l)))
              '("python" "scheme" "cpp" "pdf" "png" "generic")))
  (with l (document-format-suffixes)
    (for-each (lambda (s) (check-true (in? s l)))
              '("tm" "md" "markdown" "tex" "html" "bib" "docx" "odt"))
    (for-each (lambda (s) (check-false (in? s l)))
              '("py" "scm" "pdf" "png" "jpg" "zip" ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (office-file name fm)
  ;; a Word or OpenDocument file written by TeXmacs
  (let* ((doc (stree->tree '(document (TeXmacs "2.1")
                                      (style (tuple "generic"))
                                      (body (document "Office paragraph")))))
         (s (convert doc "texmacs-tree" (string-append fm "-document"))))
    (and (string? s) (write-file name s))))

(define (test-open-by-suffix)
  (check-group "open by suffix")
  (open-and-check (write-file "a.md" "# Title\n\nSome *Markdown* text.\n")
                  "Markdown")
  (open-and-check (write-file "b.tex"
                              (string-append "\\documentclass{article}\n"
                                             "\\begin{document}\nLaTeX text"
                                             "\n\\end{document}\n"))
                  "LaTeX text")
  (open-and-check (write-file "c.html"
                              "<html><body><p>Html text</p></body></html>\n")
                  "Html text")
  (open-and-check (write-file "d.bib"
                              "@article{key, author={A. Author}, title={Bib title}, year=2000}\n")
                  "key")
  (with u (office-file "e.docx" "docx")
    (check-true (url? u))
    (when u (open-and-check u "Office paragraph")))
  (with u (office-file "f.odt" "odt")
    (check-true (url? u))
    (when u (open-and-check u "Office paragraph")))
  ;; empty files are empty documents, not failures
  (open-and-check (write-file "empty.md" "") #f)
  (open-and-check (write-file "empty.tex" "") #f)
  (open-and-check (write-file "empty.html" "") #f)
  (open-and-check (write-file "empty.bib" "") #f)
  (open-and-check (write-file "empty.tm" "") #f)
  (open-and-check (write-file "blank.tm" "  \n") #f)
  (open-and-check (write-file "empty.stm" "") #f)
  (open-and-check (write-file "empty.tmml" "") #f))

(define (test-no-converter)
  (check-group "no converter")
  ;; no converter to TeXmacs: a message, no empty document
  (check-not-opened (write-file "g.pdf" "%PDF-1.4\n%%EOF\n"))
  (check-not-opened (write-file "h.png" "\x89PNG\r\n\x1a\n"))
  ;; nor a new empty document for a missing file of such a format
  (check-not-opened (tmp-file "missing.pdf"))
  ;; a converter which fails (as one whose program is missing):
  ;; generic->texmacs gives (error "bad format or data")
  (with u (write-file "i.md" "# Failing\n")
    (with-global generic->texmacs
        (lambda (s fm) (stree->tree '(error "bad format or data")))
      (check-not-opened u)))
  ;; a broken Word file opens with the message of the converter
  (with u (write-file "j.docx" "not a zip archive")
    (open-and-check u "This file is not a Word document"))
  ;; the files of the system: the same message, nothing else changed
  (with u (tmp-file "g.pdf")
    (let* ((old (current-buffer))
           (msgs (with-messages (lambda () (load-buffer-from-system u)))))
      (check-false (buffer-exists? u))
      (check= (current-buffer) old)
      (check-true (list-or (map (lambda (m) (string-occurs? "no converter" m))
                                msgs)))
      (startup-buffer-release)))
  ;; a file which cannot be opened does not take the untouched buffer of
  ;; the start: the next file of the system still goes there
  (let* ((pdf (tmp-file "g.pdf"))
         (md (write-file "p.md" "System\n"))
         (old (current-buffer))
         (u (new-buffer))
         (calls '()))
    (switch-to-buffer* u)
    (startup-buffer-release)
    (with-global load-buffer
        (lambda (name . opts) (set! calls (rcons calls (list 'current name))))
      (with-global load-buffer-in-new-window
          (lambda (name . opts) (set! calls (rcons calls (list 'new name))))
        (load-buffer-from-system pdf)
        (load-buffer-from-system md)))
    (check= calls (list (list 'new pdf) (list 'current md)))
    (startup-buffer-release)
    (when (buffer-exists? u) (buffer-close u))
    (when (and old (buffer-exists? old)) (switch-to-buffer old))))

(define (failing-import s fm)
  (stree->tree '(error "bad format or data")))

(define (test-failing-reloads)
  (check-group "failing reloads")
  ;; revert: the document stays as it was, it is not emptied (and then
  ;; marked as saved, which a save would write over the file)
  (let* ((u (write-file "q.md" "Revert me\n"))
         (old (current-buffer)))
    (load-buffer u)
    (check-true (buffer-exists? u))
    (when (buffer-exists? u)
      (with msgs (with-messages
                   (lambda ()
                     (with-global generic->texmacs failing-import
                       (revert-buffer-revert u))))
        (check-true (list-or (map (lambda (m) (string-occurs? "Could not load" m))
                                  msgs))))
      (check-true (stree-has-string? (tm->stree (buffer-get-body u))
                                     "Revert me"))
      (buffer-close u))
    (when (and old (buffer-exists? old)) (switch-to-buffer old)))
  ;; the autosave file of a file whose converter fails: no empty document
  ;; in place of the autosave, a message, and the file is not loaded either
  (let* ((u (write-file "r.md" "Autosaved\n"))
         (a (write-file "r.md~" "Autosaved later\n"))
         (old (current-buffer)))
    (system (string-append "touch -t 200001010000 "
                           (escape-shell (url->system u))))
    (let* ((msgs
            (with-messages
              (lambda ()
                (with-global user-confirm (lambda (q default k) (k #t))
                  (with-global generic->texmacs failing-import
                    (load-buffer u)))))))
      (check-false (buffer-exists? u))
      (check-true (list-or (map (lambda (m) (string-occurs? "autosave" m))
                                msgs))))
    (when (buffer-exists? u) (buffer-close u))
    (system-remove a)
    (when (and old (buffer-exists? old)) (switch-to-buffer old))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-drops)
  (check-group "drops")
  (let* ((md (write-file "k.md" "Dropped\n"))
         (tex (write-file "l.tex" "Dropped\n"))
         (png (write-file "m.png" "\x89PNG\r\n\x1a\n"))
         (txt (write-file "n.xyz" "Dropped\n"))
         (py (write-file "o.py" "x = 1\n"))
         (name (lambda (u) (utf8->cork (url->system u)))))
    ;; the names given by the interfaces (TMView, QTMWidget)
    (check= (dropped-documents (name md)) (list md))
    (check= (dropped-documents `(concat ,(name md) " " ,(name tex)))
            (list md tex))
    ;; not all documents: the names are inserted as before
    (check-false (dropped-documents `(concat ,(name md) " " ,(name png))))
    (check-false (dropped-documents `(image ,(name png) "" "" "" "")))
    (check-false (dropped-documents (name txt)))
    (check-false (dropped-documents (name py)))
    (check-false (dropped-documents (name (tmp-dir))))
    (check-false (dropped-documents (name (tmp-file "missing.md"))))
    (check-false (dropped-documents "some text"))
    ;; a text is not taken for a file: a name next to the document, a
    ;; relative name, a web address (never fetched)
    (let* ((host (write-file "host.md" "Host\n"))
           (old (current-buffer)))
      (load-buffer host)
      (check= (current-buffer) host)
      (check-false (dropped-documents "k.md"))
      (check-false (dropped-documents "./k.md"))
      (when (buffer-exists? host) (buffer-close host))
      (when (and old (buffer-exists? old)) (switch-to-buffer old)))
    (check-false (dropped-documents "https://example.org/a.md"))
    (check-false (dropped-documents "file:///tmp/a.md"))
    ;; the styles and packages of TeXmacs are not documents (.ts is also
    ;; TypeScript)
    (check-false (dropped-documents (name (write-file "s.ts" "x\n"))))
    (check-false (dropped-documents '(hlink "x" "https://example.org/a.md")))
    ;; the drop event opens them as the files of the system, after the event
    (let* ((opened '())
           (delayed '())
           (old (current-buffer))
           (scratch (new-buffer)))
      ;; (in a buffer of its own: what a broken override would insert)
      (switch-to-buffer* scratch)
      (with-global exec-delayed (lambda (cmd) (set! delayed (cons cmd delayed)))
        (with-global load-buffer-from-system
            (lambda (u) (set! opened (rcons opened u)))
          (mouse-drop-event 0 0 (stree->tree `(concat ,(name md) " "
                                                      ,(name tex))))
          (check= opened '())
          (check= (length delayed) 1)
          (for-each (lambda (cmd) (cmd)) delayed)
          (check= opened (list md tex))))
      (check= (tm->stree (buffer-get-body scratch)) '(document ""))
      (buffer-pretend-saved scratch)
      (buffer-close scratch)
      (when (and old (buffer-exists? old)) (switch-to-buffer old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; texmacs -c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-and-check in out ok?)
  ;; convert-file, as texmacs -c does; closes what it opened
  (let* ((old (current-buffer))
         (before (buffer-list))
         (r (convert-file in out)))
    (check= r ok?)
    (check= (url-exists? out) ok?)
    (for-each (lambda (b) (when (nin? b before) (buffer-close b)))
              (buffer-list))
    (when (url-exists? out) (system-remove out))
    (when (and old (buffer-exists? old)) (switch-to-buffer old))))

(define (test-convert-file)
  (check-group "convert file")
  (write-file "t.md" "# Converted\n")
  ;; (the names as texmacs -c gives them: strings of full names)
  (let* ((in (lambda (name) (string-append (url->system (tmp-dir)) "/" name))))
    ;; (a star in a url is a wildcard, also for string-save: the file is
    ;; written by the shell)
    ;; (Windows forbids a star in a file name: that case is left out there)
    (unless (or (os-mingw?) (os-win32?))
      (system (string-append "echo Star > " (escape-shell (in "st*ar.md")))))
    (convert-and-check (in "t.md") (tmp-file "t-out.tm") #t)
    ;; load-buffer loads another name than the one given: a wildcard which
    ;; matches one file, and a name with a star (#790)
    (convert-and-check (in "t*.md") (tmp-file "t2-out.tm") #t)
    (unless (or (os-mingw?) (os-win32?))
      (convert-and-check (in "st*ar.md") (tmp-file "t3-out.tm") #t))
    ;; no converter, and no file: nothing is written
    (convert-and-check (in "g.pdf") (tmp-file "t4-out.tm") #f)
    (convert-and-check (in "missing.md") (tmp-file "t5-out.tm") #f)
    ;; the same file twice (two -c of one source, or a file which is open):
    ;; the second conversion finds its buffer
    (let* ((old (current-buffer))
           (before (buffer-list))
           (o1 (tmp-file "t6-out.tm"))
           (o2 (tmp-file "t6-out.html")))
      (check-true (convert-file (in "t.md") o1))
      (check-true (convert-file (in "t.md") o2))
      (check-true (url-exists? o1))
      (check-true (url-exists? o2))
      ;; after a failure, the buffer of before is not exported in its place
      (check-false (convert-file (in "g.pdf") (tmp-file "t7-out.tm")))
      (check-false (url-exists? (tmp-file "t7-out.tm")))
      (for-each (lambda (b) (when (nin? b before) (buffer-close b)))
                (buffer-list))
      (when (and old (buffer-exists? old)) (switch-to-buffer old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The document types of the application on macOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plist-extensions s)
  ;; the strings of the arrays CFBundleTypeExtensions of the plist @s
  (let loop ((lines (string-tokenize-by-char s #\newline))
             (in? #f)
             (r '()))
    (if (null? lines) (reverse r)
        (let* ((l (tm-string-trim-both (car lines))))
          (cond ((== l "<key>CFBundleTypeExtensions</key>")
                 (loop (cdr lines) #t r))
                ((and in? (== l "</array>"))
                 (loop (cdr lines) #f r))
                ((and in? (string-starts? l "<string>")
                      (string-ends? l "</string>"))
                 (loop (cdr lines) #t
                       (cons (substring l 8 (- (string-length l) 9)) r)))
                (else (loop (cdr lines) in? r)))))))

(define (test-plist)
  (check-group "macOS document types")
  ;; (the sources only: an installed TeXmacs has no packages folder)
  (let* ((tp (getenv "TEXMACS_PATH"))
         (p (and tp (string-append tp "/../packages/macos/Info.plist.in"))))
    (when (and p (url-exists? (system->url p)))
      (let* ((exts (plist-extensions (string-load (system->url p))))
             (sufs (document-format-suffixes)))
        (check-true (nnull? exts))
        ;; each suffix once, and the same as the registry
        (check= (length exts) (length (list-remove-duplicates exts)))
        (check= (list-filter sufs (lambda (s) (nin? s exts))) '())
        (check= (list-filter exts (lambda (s) (nin? s sufs))) '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-formats-test-failures)
  (check-suite "open-formats")
  (clean-tmp-dir)
  (let* ((before (buffer-list))
         (recent (learned-interactive "recent-buffer")))
    (run-group test-openable)
    (run-group test-document-formats)
    (run-group test-open-by-suffix)
    (run-group test-no-converter)
    (run-group test-failing-reloads)
    (run-group test-drops)
    (run-group test-convert-file)
    (run-group test-plist)
    ;; no buffer is left open
    (for-each (lambda (b) (when (nin? b before) (buffer-close b)))
              (buffer-list))
    (check= (length (buffer-list)) (length before))
    ;; the recent files are those of before the suite
    (forget-interactive "recent-buffer")
    (for-each (lambda (e) (learn-interactive 'recent-buffer e))
              (reverse recent))
    (check= (learned-interactive "recent-buffer") recent))
  (clean-tmp-dir)
  (system-rmdir (tmp-dir))
  (check-end))
