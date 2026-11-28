
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-print.scm
;; DESCRIPTION : routines for printing documents
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-print)
  (:use (texmacs texmacs tm-files) (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to obtain the papersize in this order from
;; - the environment variable PAPERSIZE
;; - the contents of the file specified by the PAPERCONF environment variable
;; - the contents of the file "/etc/papersize"
;; or else default to "a4"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define supported-sizes
  '("a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9"
    "b0" "b1" "b2" "b3" "b4" "b5" "b6" "b7" "b8" "b9"
    "archA" "archB" "archC" "archD" "archE"
    "10x14" "11x17" "C5" "Comm10" "DL" "executive" "halfletter"
    "halfexecutive" "ledger" "legal" "letter" "Monarch"
    "csheet" "dsheet" "flsa" "flse" "folio"
    "lecture note" "note" "quarto" "statement" "tabloid"
    "16:9" "8:5" "3:2" "4:3" "5:4"
    "user"))

(define standard-sizes
  '("a0" "a1" "a2" "a3" "a4" "a5" "a6"
    "b3" "b4" "b5" "b6"
    "ledger" "legal" "letter" "folio"))

(define (get-default-paper-size-bis)
  (with psize (getenv "PAPERSIZE")
    (if (and psize (!= psize "")) psize
        (with papersizefile (or (getenv "PAPERCONF") "/etc/papersize")
          (and (not (and (os-mingw?)
                         (== papersizefile "/etc/papersize")))
               (url-test? papersizefile "r")
               (with pps-port (open-input-file papersizefile)
                 (with size (read-line pps-port)
                   (close-input-port pps-port)
                   size)))))))

(tm-define (correct-paper-size s)
  (if (and (string? s) (in? s supported-sizes)) s "a4"))

(tm-define (standard-paper-size s)
  (if (and (string? s) (in? s standard-sizes)) s "user"))

(tm-define (get-default-paper-size)
  (correct-paper-size (get-default-paper-size-bis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define preview-command "default")

(define (notify-preview-command var val)
  (set! preview-command val))

(define (notify-printing-command var val)
  (set-printing-command val))

(define (notify-paper-type var val)
  (set-printer-paper-type (locase-first val)))

(define (notify-printer-dpi var val)
  (set-printer-dpi val))

(define-preferences
  ("native pdf" "on" noop)
  ("native postscript" "on" noop)
  ("texmacs->pdf:expand slides" "off" noop)
  ("texmacs->pdf:check" "off" noop)
  ("texmacs->pdf:tagged" "on" noop)   ; (PDF/UA, docs/pdf-tagging.md)
  ("preview command" "default" notify-preview-command)
  ("printing command" (get-default-printing-command) notify-printing-command)
  ("paper type" (get-default-paper-size) notify-paper-type)
  ("printer dpi" "1200" notify-printer-dpi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing wrapper for slides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wrapped-print-to-file fname . opt-prepare)
  ;; a presentation is printed from a copy with its slides made; the
  ;; optional procedure is called in the copy before they are made
  ;; (pdf-tag-check marks its pictures), and the copy is closed, the
  ;; document shown again, also on an error
  (if (screens-buffer?)
      (let* ((cur (current-buffer))
             (buf (buffer-new)))
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           (buffer-copy cur buf)
           (buffer-set-master buf cur)
           (switch-to-buffer buf)
           (set-drd cur)
           (when (nnull? opt-prepare) ((car opt-prepare)))
           (dynamic-make-slides)
           ;; typeset the new buffer, so that its links get registered
           (update-forced)
           (print-to-file fname))
         (lambda ()
           (switch-to-buffer cur)
           (buffer-close buf))))
      (print-to-file fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A PDF with a password (the MuPDF renderer only, see pdf-encryption?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (pdf-encryption?)
  ;; whether the PDF will be written by the MuPDF renderer, which encrypts
  ;; (mupdf_writes_pdf in glue.cpp): anything else would ignore the
  ;; passwords and write a PDF which is not protected
  (mupdf-writes-pdf?))

(define pdf-password-target #f)

(tm-define (print-to-pdf-with-password upw opw perm)
  (:synopsis "Export to a PDF which asks for a password")
  (:argument upw "password" "Password to open the PDF")
  (:argument opw "password" "Owner password, which lifts the restrictions (empty: the same)")
  (:argument perm "string" "Allowed without it (all, or print, copy, modify, annotate...)")
  (:proposals perm '("all" "print,copy" "print" "none"))
  ;; The passwords go to the renderer through the environment, for this one
  ;; export only (mupdf_pdf_renderer.cpp, pdf_encryption): neither the
  ;; preferences nor the document keep them, which would put them on the
  ;; disk in clear.
  (when pdf-password-target
    (let ((fname pdf-password-target)
          (clear (lambda ()
                   (system-setenv "TEXMACS_PDF_USER_PASSWORD" "")
                   (system-setenv "TEXMACS_PDF_OWNER_PASSWORD" "")
                   (system-setenv "TEXMACS_PDF_PERMISSIONS" ""))))
      (set! pdf-password-target #f)
      (dynamic-wind
        (lambda ()
          (system-setenv "TEXMACS_PDF_USER_PASSWORD" upw)
          (system-setenv "TEXMACS_PDF_OWNER_PASSWORD" opw)
          (system-setenv "TEXMACS_PDF_PERMISSIONS" perm))
        (lambda () (wrapped-print-to-file fname))
        clear))))

(tm-define (choose-pdf-with-password fname)
  (set! pdf-password-target fname)
  (interactive print-to-pdf-with-password))

(tm-define (wrapped-print-to-pdf-embeded-with-tm fname)
    (unless (string=? (url-suffix fname) "pdf")
      (texmacs-error "Wrapped-print-to-pdf-embeded-with-tm" "fname is not a pdf"))
    (if (screens-buffer?)
      (let* ((cur (current-buffer))
             (buf (buffer-new)))
        (buffer-copy cur buf)
        (buffer-set-master buf cur)
        (switch-to-buffer buf)
        (set-drd cur)
        (dynamic-make-slides)
        ;; typeset the new buffer, so that its links get registered
        (update-forced)
        (print-to-file fname)
        (unless (attach-doc-to-exported-pdf fname)
          (notify-now "Fail to attach tm to pdf"))
        (switch-to-buffer cur)
        (buffer-close buf))
      (begin
      (print-to-file fname)
      (unless (attach-doc-to-exported-pdf fname)
          (notify-now "Fail to attach tm to pdf")))))

(define (pdf-embedded-name s)
  ;; a linked file by its name alone, when it is given by a path
  (if (or (string-index s #\/) (string-index s #\\))
      (url->string (url-tail (system->url s)))
      s))

(define (pdf-embedded-bare-names s)
  ;; The copy of the document which goes into the PDF names the files it
  ;; links to by their names alone: they go in next to it, and come out
  ;; next to it (wrapped-import-pdf-embeded-with-tm), in either build. The
  ;; paths pdf-replace-linked-path leaves are the author's, absolute ones --
  ;; they worked, but every PDF told where the author's files are kept.
  ;; On the stree, and a new one made: a subtree of a tree which is in no
  ;; buffer cannot be assigned (tree-assign only rebinds it).
  (cond ((not (pair? s)) s)
        ((and (in? (car s) '(image include)) (pair? (cdr s)) (string? (cadr s)))
         (cons* (car s) (pdf-embedded-name (cadr s))
                (map pdf-embedded-bare-names (cddr s))))
        ((and (== (car s) 'style) (pair? (cdr s)))
         (with a (cadr s)
           (cons* 'style
                  (cond ((string? a) (pdf-embedded-name a))
                        ((and (pair? a) (== (car a) 'tuple))
                         (cons 'tuple
                               (map (lambda (x)
                                      (if (string? x) (pdf-embedded-name x) x))
                                    (cdr a))))
                        (else a))
                  (cddr s))))
        (else (cons (car s) (map pdf-embedded-bare-names (cdr s))))))

(tm-define (attach-doc-to-exported-pdf fname)
  ;; The document goes in with its linked files (images, included documents,
  ;; styles of its own), and the copy which goes in names them by their file
  ;; names alone (pdf-embedded-bare-names), which is where they come out.
  ;; It is given a copy of the document, since it changes the tree it is
  ;; given in place -- given the tree of the buffer, it rewrote the paths of
  ;; the open document, behind the editor's back, and the next save kept
  ;; them -- and it is that copy which goes in.
  (let* ((tem-url (buffer-new))
         (new-url (url-relative tem-url (string-append (url-basename fname) ".tm")))
         (cur-url (current-buffer-url))
         (cur-tree (tree-copy (buffer-get cur-url)))
         (linked-file (pdf-get-linked-file-paths cur-tree cur-url))
         (linked-file-with-main (array-url-append new-url linked-file))
         (new-tree (pdf-replace-linked-path cur-tree cur-url)))
    (buffer-rename tem-url new-url)
    (buffer-copy cur-url new-url)
    (buffer-set new-url
                (stree->tree (pdf-embedded-bare-names (tree->stree new-tree))))
    ;; copy also attachments and auxiliary data
    (with-buffer cur-url
      (let* ((attl (list-attachments)) 
             (atts (map get-attachment attl))
             (auxl (list-auxiliaries))
             (auxs (map get-auxiliary auxl)))
        (with-buffer new-url
          (for-each set-attachment attl atts)
          (for-each set-auxiliary auxl auxs))))
    (buffer-save new-url)
    (pdf-make-attachments fname linked-file-with-main fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (propose-print-file-name)
  (let ((name (propose-name-buffer))
	(suf (printer-file-suffix)))
    (if (string-ends? name ".tm")
	(string-append (string-drop-right name 3) "." suf)
	name)))

(tm-property (print-to-file name)
  (:synopsis "Print to file")
  (:argument name print-file "File name")
  (:default  name (propose-print-file-name)))

(tm-property (print-pages first last)
  (:synopsis "Print page selection")
  (:argument  first "First page")
  (:proposals first (list "1" ""))
  (:argument  last "Last page")
  (:proposals last  (list (number->string (get-page-count)) "")))

(tm-property (print-pages-to-file name first last)
  (:synopsis "Print page selection to file")
  (:argument  name print-file "File name")
  (:default   name (propose-print-file-name))
  (:argument  first "First page")
  (:proposals first (list "1" ""))
  (:argument  last "Last page")
  (:proposals last  (list (number->string (get-page-count)) "")))

(tm-define (preview-command-line s)
  (:synopsis "The shell command which shows the file @s, or #f if none")
  ;; s comes from url-sys-concretize, which already escapes it for the
  ;; shell (escape_sh): it is passed as it is
  (cond ((!= preview-command "default")
         (string-append preview-command " " s " &"))
        ((or (os-mingw?) (os-win32?)) s)
        ((os-macos?) (string-append "open " s))
        ((url-exists-in-path? "xdg-open")
         (string-append "xdg-open " s))
        ((url-exists-in-path? "ggv") (string-append "ggv " s " &"))
        ((url-exists-in-path? "ghostview") (string-append "ghostview " s " &"))
        ((url-exists-in-path? "gv") (string-append "gv " s " &"))
        (else #f)))

(tm-define (preview-file u)
  (let* ((s (url-sys-concretize u))
         (cmd (and (not (defined? 'web-open-pdf)) (preview-command-line s))))
    (cond ((defined? 'web-open-pdf)
           ;; in the browser: a tab of its own (misc/wasm/print.js)
           (web-open-pdf s (url->string (url-tail u))))
          (cmd (shell cmd))
          (else (set-message
                 "Error: ghostview does not seem to be installed on your system"
                 "preview")))))

(define (web-print-name)
  (with name (url->string (url-tail (current-buffer)))
    (string-append (if (string-ends? name ".tm") (string-drop-right name 3) name)
                   ".pdf")))

(tm-define (preview-buffer)
  (with file (cond ((defined? 'web-open-pdf)
                    ;; in the browser: the PDF, in the files of the page
                    ;; which are not kept (not in the home directory)
                    (system->url "/tmp/texmacs-print.pdf"))
                   ((os-mingw?)
                    (let* ((p (getenv "TEXMACS_HOME_PATH"))
                           (f (string-append p "\\system\\tmp\\preview.pdf")))
                      (system->url f)))
                   ((or (os-macos?) (== (printer-file-format) "pdf"))
                    "$TEXMACS_HOME_PATH/system/tmp/preview.pdf")
                   (else "$TEXMACS_HOME_PATH/system/tmp/preview.ps"))
    ;; the file of the last preview is removed first: a PDF which could not
    ;; be made (Ghostscript not found, #546) is then reported, instead of
    ;; showing the old one, or nothing at all
    (when (url-exists? file) (system-remove file))
    (print-to-file file)
    (cond ((not (url-exists? file))
           (with name (url->system (url-tail file))
             (set-message
              (if (or (== (printer-file-format) "pdf")
                      (url-exists-in-path? "gs"))
                  `(concat "Error: the preview " (verbatim ,name)
                           " could not be made")
                  `(concat "Error: the preview " (verbatim ,name)
                           " could not be made (Ghostscript was not found)"))
              "preview")))
          ((defined? 'web-open-pdf)
           (web-open-pdf (url-sys-concretize file) (web-print-name)))
          (else (preview-file file)))))

(tm-define (choose-file-and-print-page-selection start end)
  (:argument  start "First page")
  (:proposals start (list "1" ""))
  (:argument  end "Last page")
  (:proposals end (list (number->string (get-page-count)) ""))
  (choose-file (lambda (name) (print-pages-to-file name start end))
	       "Print page selection to file" (printer-file-format)
	       "Print:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tagged PDF (PDF/UA, docs/pdf-tagging.md): the structure of a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The standard structure type of a tag; "H:n" is a heading of rank n (H1,
;; H2... after the headings above it in the document), "H:A" an appendix (a
;; chapter in a book, a section elsewhere). A tag which is
;; not here is transparent, except at the level of a paragraph (a child of
;; a document), where it is a P, or a Div when it holds paragraphs. A Sect
;; (a bibliography, a table of contents, an index) has its title, which its
;; macro writes, as a heading of the rank of an appendix.
(define pdf-tag-roles
  '((doc-data . "Div") (doc-title . "H:0") (doc-subtitle . "P")
    (doc-author . "P") (doc-date . "P") (doc-note . "P") (doc-misc . "P")
    (abstract-data . "Div") (abstract . "Div")
    (abstract-keywords . "P") (abstract-msc . "P")
    (part . "H:1") (part* . "H:1") (chapter . "H:2") (chapter* . "H:2")
    (appendix . "H:A") (appendix* . "H:A") (section . "H:3") (section* . "H:3")
    (subsection . "H:4") (subsection* . "H:4")
    (subsubsection . "H:5") (subsubsection* . "H:5")
    (paragraph . "H:6") (paragraph* . "H:6")
    (subparagraph . "H:7") (subparagraph* . "H:7")
    (math . "Formula") (equation . "Formula") (equation* . "Formula")
    (eqnarray . "Formula") (eqnarray* . "Formula")
    (align . "Formula") (align* . "Formula")
    (gather . "Formula") (gather* . "Formula")
    (multline . "Formula") (multline* . "Formula")
    (alignat . "Formula") (alignat* . "Formula")
    (flalign . "Formula") (flalign* . "Formula")
    (item . "Lbl") (item* . "Lbl") (bib-list . "L")
    (bibitem . "Lbl") (bibitem* . "Lbl")
    (tabular . "Table") (tabular* . "Table") (block . "Table")
    (block* . "Table") (wide-tabular . "Table") (wide-block . "Table")
    (row . "TR") (cell . "TD")
    (hlink . "Link") (href . "Link") (footnote . "Note")
    (image . "Figure") (graphics . "Figure")
    (bibliography . "Sect") (bibliography* . "Sect")
    (table-of-contents . "Sect") (the-index . "Sect") (the-glossary . "Sect")
    (list-of-figures . "Sect") (list-of-tables . "Sect") (tit . "H:3")
    (big-figure . "Div") (small-figure . "Div")
    (big-table . "Div") (small-table . "Div")
    (quote-env . "BlockQuote") (quotation . "BlockQuote")
    (verse . "BlockQuote")))

(define pdf-tag-caption-tags
  '("big-figure" "small-figure" "big-table" "small-table"))

(tm-define (pdf-tag-role l)
  (:synopsis "The structure type of the tag @l in a tagged PDF, or \"\"")
  (let* ((s (if (string? l) l (symbol->string l)))
         (p (assq (string->symbol s) pdf-tag-roles)))
    (cond (p (cdr p))
          ((or (string-starts? s "itemize") (string-starts? s "enumerate")
               (string-starts? s "description")) "L")
          (else ""))))

(tm-define (pdf-tag-child-role l i n)
  (:synopsis "The structure type of the child @i of @n of a tag @l, or \"\"")
  (if (and (in? l pdf-tag-caption-tags) (== i (- n 1))) "Caption" ""))

(define (pdf-tag-drop-labels s)
  ;; @s without its \label{...}
  (with i (string-search-forwards "\\label{" 0 s)
    (if (< i 0) s
        (with j (string-search-forwards "}" i s)
          (if (< j 0) s
              (pdf-tag-drop-labels
               (string-append (substring s 0 i)
                              (substring s (+ j 1) (string-length s)))))))))

(define pdf-tag-environments
  '("equation" "eqnarray" "align" "gather" "multline" "flalign" "alignat"))

(define (pdf-tag-strip s)
  ;; the formula of a LaTeX equation: without its environment and labels
  (let loop ((s (pdf-tag-drop-labels s)) (l pdf-tag-environments))
    (if (null? l)
        (string-trim-both (string-replace s "  " " "))
        (let* ((e (car l))
               (s1 (string-replace s (string-append "\\begin{" e "*}") ""))
               (s2 (string-replace s1 (string-append "\\end{" e "*}") ""))
               (s3 (string-replace s2 (string-append "\\begin{" e "}") ""))
               (s4 (string-replace s3 (string-append "\\end{" e "}") "")))
          (loop s4 (cdr l))))))

(define (pdf-tag-latex t)
  (let* ((s (convert (tree->stree t) "texmacs-stree" "latex-snippet"))
         (s1 (string-replace (string-replace s "\n" " ") "\t" " "))
         (s2 (pdf-tag-strip (string-trim-both (cork->utf8 s1)))))
    (cond ((and (string-starts? s2 "$") (string-ends? s2 "$")
                (> (string-length s2) 1))
           (string-trim-both (substring s2 1 (- (string-length s2) 1))))
          ((and (string-starts? s2 "\\[") (string-ends? s2 "\\]"))
           (string-trim-both (substring s2 2 (- (string-length s2) 2))))
          (else s2))))

(tm-define (pdf-tag-alt t role)
  (:synopsis "The alternate description of @t, an element of type @role")
  (catch #t
    (lambda ()
      (cond ((== role "Formula") (pdf-tag-latex t))
            ((and (== role "Figure") (tree-is? t 'image)
                  (tree-atomic? (tree-ref t 0)))
             (string-append "Image " (cork->utf8 (tree->string (tree-ref t 0)))))
            ((== role "Figure") "Image")
            (else "")))
    (lambda args (if (== role "Formula") "Formula" "Image"))))

(tm-define (pdf-tag-text t)
  (:synopsis "The text of @t, in UTF-8 on one line (a caption)")
  (catch #t
    (lambda ()
      (let* ((s (convert (tree->stree t) "texmacs-stree" "verbatim-snippet"))
             (s1 (string-replace (string-replace s "\n" " ") "\t" " ")))
        (string-trim-both (string-replace s1 "  " " "))))
    (lambda args "")))

(tm-define (pdf-tag-language lan)
  (:synopsis "The language @lan of TeXmacs as a code (de-DE) for a PDF")
  (string-replace (language-to-locale lan) "_" "-"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking a document for a tagged PDF (Tools > Accessibility): what an
;; accessible PDF wants and the document does not say. The export itself
;; warns of nothing (docs/pdf-tagging.md).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The check prints the document as Export as PDF does (wrapped-print-to-file,
;; which makes the slides of a presentation in a copy), tagged, to a file of
;; its own, and reads the pictures which the tagger met without an
;; alternative text of the user's (pdf-tag-printed-places, print_doc):
;; exactly those which the PDF describes by a caption or a file name.

(define (pdf-tag-images t)
  ;; the images of @t, in their order
  (cond ((tree-atomic? t) '())
        ((tree-is? t 'image) (list t))
        (else (append-map pdf-tag-images (tree-children t)))))

(define (pdf-tag-mark-copy)
  ;; in the copy of a presentation, whose body is that of the document
  ;; until its slides are made: the image i in <with|pdf-tag-source|i|..>,
  ;; which the slides keep and print_doc reads (pdf-tag-printed-sources)
  (let ((l (pdf-tag-images (buffer-tree))))
    (for-each (lambda (t i)
                (tree-insert-node! t 2 `(with "pdf-tag-source"
                                              ,(number->string i))))
              l (iota (length l)))))

(define (pdf-tag-render)
  ;; #t when a tagged PDF of the document was made, else #f or the error
  (let ((pdf (url-append (url-temp-dir) "pdf-tag-check.pdf"))
        (env (getenv "TEXMACS_PDF_TAGGED"))
        (env-check (getenv "TEXMACS_PDF_TAG_CHECK"))
        (err #f))
    (system-setenv "TEXMACS_PDF_TAGGED" "1")
    ;; (print_doc keeps the pictures for the check only; no "Done printing")
    (system-setenv "TEXMACS_PDF_TAG_CHECK" "1")
    (catch #t
      (lambda () (wrapped-print-to-file pdf pdf-tag-mark-copy))
      (lambda args (set! err args)))
    (system-setenv "TEXMACS_PDF_TAGGED" (or env ""))
    (system-setenv "TEXMACS_PDF_TAG_CHECK" (or env-check ""))
    (when (url-exists? pdf) (system-remove pdf))
    (or err (pdf-tag-printed?))))

(define (pdf-tag-error-text err)
  ;; the error @err (the arguments of a catch) said shortly: its key and
  ;; its message, when the message is a plain string
  (let* ((key (if (and (pair? err) (symbol? (car err)))
                  (symbol->string (car err)) "error"))
         (strings (let walk ((x (if (pair? err) (cdr err) '())))
                    (cond ((string? x) (list x))
                          ((pair? x) (append (walk (car x)) (walk (cdr x))))
                          (else '()))))
         (msg (list-find strings
                         (lambda (s) (and (!= s "")
                                          (not (string-starts? s "["))
                                          (not (string-occurs? "~" s)))))))
    (if msg (string-append key ": " msg) key)))

(define (pdf-tag-locate body slides?)
  ;; for each picture printed: the image of the buffer (#f for none) and
  ;; the included file which holds it ("" for none). For a presentation,
  ;; the image of its mark (pdf-tag-mark-copy); else the image at the same
  ;; place (the body printed differs from the body only by the expanded
  ;; includes)
  (let ((all (if slides? (list->vector (pdf-tag-images body)) #f)))
    (map (lambda (p img file source)
           (cond (slides?
                  (with i (and (!= source "") (string->number source))
                    (if (and i (< i (vector-length all)))
                        (cons (vector-ref all i) "")
                        ;; (no mark: an included document, or a macro)
                        (cons #f file))))
                 ((!= file "") (cons #f file))
                 (else
                  (with t (pdf-tag-tree-at body p)
                    (cons (and t (tree-is? t 'image)
                               (== (tree->stree t) (tree->stree img)) t)
                          "")))))
         (pdf-tag-printed-places) (pdf-tag-printed-images)
         (pdf-tag-printed-files) (pdf-tag-printed-sources))))

(define (pdf-tag-signature)
  ;; the images of the buffer, by their places and whether they have an
  ;; alternative text: what an edit which may give a picture to describe
  ;; changes (a walk of the body, no print; pdf-tag-pictures)
  (map (lambda (t) (cons (tree->path t) (pdf-tag-own-alt? t)))
       (pdf-tag-images (buffer-tree))))

(define (pdf-tag-tree-at t p)
  ;; the subtree of @t at @p, or #f
  (cond ((null? p) t)
        ((and (tree-compound? t) (< (car p) (tree-arity t)))
         (pdf-tag-tree-at (tree-ref t (car p)) (cdr p)))
        (else #f)))

(define (pdf-tag-distinct l)
  ;; @l without the trees met before (eq?)
  (let loop ((l l) (r '()))
    (cond ((null? l) (reverse r))
          ((memq (car l) r) (loop (cdr l) r))
          (else (loop (cdr l) (cons (car l) r))))))

(define (pdf-tag-count l)
  ;; the (x . n) of the elements x of @l, in their order
  (let loop ((l l) (r '()))
    (cond ((null? l) (reverse r))
          ((assoc (car l) r)
           (loop (cdr l) (map (lambda (x) (if (== (car x) (car l))
                                              (cons (car x) (+ (cdr x) 1)) x))
                              r)))
          (else (loop (cdr l) (cons (cons (car l) 1) r))))))

(define (pdf-tag-known-language? lan)
  ;; (pdf-tag-language gives en-US for a language it does not know)
  (or (in? lan '("english" "american"))
      (!= (language-to-locale lan) "en_US")))

(define pdf-tag-found '())   ; the buffer, its pictures, its signature and
                             ; the error of the last check

(tm-define (pdf-tag-check)
  (:synopsis "What the current document lacks for an accessible tagged PDF")
  ;; an association list:
  ;;   tagged: whether a tagged PDF could be made (a native PDF writer);
  ;;   error: the error which stopped it, else #f;
  ;;   drawn: how many pictures it describes by a caption or a file name;
  ;;   pictures: the places of those in the body, in their order;
  ;;   included: the (file . n) of those drawn from included files;
  ;;   elsewhere: how many have no place of their own (drawn by a macro);
  ;;   title: whether the document has a title of its own (else the PDF
  ;;     has the name of the file);
  ;;   language: its language when a PDF cannot say it, else #f
  (let* ((buf (current-buffer))
         (body (buffer-tree))
         (slides? (screens-buffer?))
         (res (pdf-tag-render))
         (err (and (pair? res) res))
         (tagged? (== res #t))
         (found (if tagged? (pdf-tag-locate body slides?) '()))
         (_ (pdf-tag-printed-reset))   ; (what print_doc kept is read)
         (trees (pdf-tag-distinct (list-filter (map car found) identity)))
         (paths (sort (map tree->path trees) path-inf?))
         (files (list-filter (map cdr found) (lambda (f) (!= f ""))))
         (else-n (- (length found) (length files)
                    (length (list-filter (map car found) identity))))
         (ttl (get-metadata "title"))
         (own-title? (and (!= ttl "")
                          (!= ttl (utf8->cork (url->string (url-tail buf))))))
         (lan (get-init "language")))
    (set! pdf-tag-found (list buf trees (pdf-tag-signature)
                              (and err (pdf-tag-error-text err))))
    (list (cons 'tagged tagged?)
          (cons 'error (and err (pdf-tag-error-text err)))
          (cons 'drawn (length found))
          (cons 'pictures paths)
          (cons 'included (pdf-tag-count files))
          (cons 'elsewhere else-n)
          (cons 'title own-title?)
          (cons 'language (and (not (pdf-tag-known-language? lan)) lan)))))

(tm-define (pdf-tag-check-lines r)
  (:synopsis "The lines which say what the check @r of pdf-tag-check found")
  (let* ((pics (length (assoc-ref r 'pictures)))
         (incl (assoc-ref r 'included))
         (else-n (assoc-ref r 'elsewhere))
         (lan (assoc-ref r 'language))
         (plural (lambda (n one many)
                   (string-append (number->string n) " "
                                  (if (== n 1) one many))))
         (l (append
             (cond ((assoc-ref r 'error)
                    (list "The document could not be printed, the pictures"
                          (string-append "  were not checked ("
                                         (assoc-ref r 'error) ")")))
                   ((assoc-ref r 'tagged) '())
                   (else
                    (list "No tagged PDF can be made here (a native PDF"
                          "  writer is needed): the pictures were not checked")))
             (if (> pics 0)
                 (list (string-append
                        (plural pics "picture has" "pictures have")
                        " no alternative text (Focus > Alternative text)")
                       (string-append
                        "  (Tools > Accessibility > Next picture without"
                        " alternative text goes to " (if (== pics 1) "it" "them")
                        ")"))
                 '())
             (map (lambda (x)
                    (string-append "The included file " (car x) ": "
                                   (plural (cdr x) "picture" "pictures")
                                   " without alternative text"))
                  incl)
             (if (> else-n 0)
                 (list (string-append
                        (plural else-n "picture" "pictures")
                        " without alternative text drawn by macros,")
                       "  with no place of their own in the document")
                 '())
             (if (assoc-ref r 'title) '()
                 (list (string-append "No title (Document > Metadata > Title,"
                                      " or a title in the document):")
                       "  the PDF has the name of the file as its title"))
             (if lan
                 (list (string-append "The language " lan " has no code"
                                      " for a PDF: it is said to be English"))
                 '()))))
    (if (null? l) (list "Nothing is missing for an accessible tagged PDF")
        l)))

(define (pdf-tag-own-alt? t)
  ;; whether an image-alt of the user's is around the image @t now
  (let loop ((u (tree-up t)))
    (and u
         (or (and (tree-is? u 'with)
                  (let scan ((i 0))
                    (and (< (+ i 1) (tree-arity u))
                         (or (and (== (tree->string (tree-ref u i)) "image-alt")
                                  (tree-atomic? (tree-ref u (+ i 1)))
                                  (!= (tree->string (tree-ref u (+ i 1))) ""))
                             (scan (+ i 2))))))
             (loop (tree-up u))))))

(define (pdf-tag-pictures)
  ;; the places of the pictures of the last check of the current buffer
  ;; which still have no alternative text (the trees follow the edits).
  ;; It is checked again for another buffer, after a check which failed,
  ;; when one of them is no longer in the buffer (deleted, moved, replaced
  ;; by an undo: the new check keeps the trees which are), and when none
  ;; is left and the images of the buffer changed since (pdf-tag-signature:
  ;; a picture added, an alternative text taken away), so that pressing
  ;; Next again when nothing is left does not print the document again
  (define (valid)
    (list-filter (cadr pdf-tag-found)
                 (lambda (t) (and (tree->path t) (tree-is? t 'image)
                                  (not (pdf-tag-own-alt? t))))))
  ;; (valid computed once a press, again only after a new check)
  (let ((v (and (nnull? pdf-tag-found) (== (car pdf-tag-found) (current-buffer))
                (not (cadddr pdf-tag-found))
                (not (list-find (cadr pdf-tag-found)
                                (lambda (t) (not (tree->path t)))))
                (valid))))
    (when (or (not v)
              (and (null? v)
                   (!= (caddr pdf-tag-found) (pdf-tag-signature))))
      (pdf-tag-check)
      (set! v (valid)))
    (sort (map tree->path v) path-inf?)))

(define pdf-tag-last-picture #f)   ; the picture pdf-tag-go-to went to

(define (pdf-tag-go-to p)
  ;; the cursor before the picture at @p (shown if it is in hidden markup,
  ;; a slide), the picture in focus
  (go-to (rcons p 0))
  (cursor-show-if-hidden)
  (with t (path->tree p)
    (when (tree? t) (tree-focus t))
    (set! pdf-tag-last-picture t)))

(define (pdf-tag-at-picture? cur p)
  ;; whether the cursor @cur is at the picture at @p: in it, or where the
  ;; editor puts a cursor before or after an image in a line once it is
  ;; typeset (the end of the string before it, the start of the one after)
  (or (list-starts? cur p)
      (let* ((up (cDr p))
             (i (cAr p))
             (par (path->tree up)))
        (and (tree? par) (tree-is? par 'concat)
             (or (and (> i 0)
                      (tree-atomic? (tree-ref par (- i 1)))
                      (== cur (rcons (rcons up (- i 1))
                                     (string-length
                                      (tree->string (tree-ref par (- i 1)))))))
                 (and (< (+ i 1) (tree-arity par))
                      (tree-atomic? (tree-ref par (+ i 1)))
                      (== cur (rcons (rcons up (+ i 1)) 0))))))))

(tm-define (pdf-tag-next-picture)
  (:synopsis "Go to the next picture without an alternative text")
  ;; among those of the last check (pdf-tag-pictures): the one after the
  ;; picture in focus or which the cursor is at (the last one gone to: the
  ;; editor may move the cursor out of it, before an image in a line or a
  ;; specific), else the first one at or after the cursor, else the first
  ;; one; #f when there is none
  (let* ((l (pdf-tag-pictures))
         (cur (cursor-path))
         (lt pdf-tag-last-picture)
         (last (and lt (tree->path lt)))
         (ft (focus-tree))
         (fp (and (tree? ft) (tree->path ft)))
         (here (or (and fp (member fp l))
                   (and last (pdf-tag-at-picture? cur last) (member last l))))
         (next (if here
                   (and (pair? (cdr here)) (cadr here))
                   ;; (path-inf?: the order of the nodes, p being one)
                   (list-find l (lambda (p) (or (list-starts? cur p)
                                                (path-inf? cur p)))))))
    (cond ((cadddr pdf-tag-found)
           (set-message (string-append "The document could not be checked: "
                                       (cadddr pdf-tag-found))
                        "Check for a tagged PDF")
           #f)
          ((null? l)
           (set-message "No picture without an alternative text" "")
           #f)
          (else
           (let* ((p (or next (car l)))
                  (i (+ 1 (list-find-index l (lambda (q) (== q p))))))
             (pdf-tag-go-to p)
             (set-message (string-append "Picture " (number->string i) " of "
                                         (number->string (length l))
                                         " without an alternative text")
                          "Focus > Alternative text")
             p)))))

(tm-widget ((pdf-tag-check-widget lines) done)
  (padded
    (for (x lines)
      (hlist (text x) >>))
    ===
    (bottom-buttons >> ("Ok" (done)))))

(tm-define (pdf-tag-show-check)
  (:synopsis "Check the document for an accessible tagged PDF")
  (:interactive #t)
  (let* ((r (pdf-tag-check))
         (l (assoc-ref r 'pictures)))
    (when (nnull? l) (pdf-tag-go-to (car l)))
    (dialogue-window (pdf-tag-check-widget (pdf-tag-check-lines r)) noop
                     "Check for a tagged PDF")))
