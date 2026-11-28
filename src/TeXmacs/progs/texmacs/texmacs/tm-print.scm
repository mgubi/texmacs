
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

(tm-define (wrapped-print-to-file fname)
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
        (switch-to-buffer cur)
        (buffer-close buf))
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

(define (pdf-tag-includes t)
  ;; the files of the includes of @t, in their order, outside the markup
  ;; which a tagged PDF does not expand (edit_main.cpp, expand_includes)
  (cond ((tree-atomic? t) '())
        ((tree-in? t '(inactive inactive* var-inactive raw-data)) '())
        ((and (tree-is? t 'include) (tree-atomic? (tree-ref t 0)))
         (list (tree->string (tree-ref t 0))))
        (else (append-map pdf-tag-includes (tree-children t)))))

(define (pdf-tag-included-pictures base t seen)
  ;; the (file . count) of the included files of @t (relative to @base),
  ;; and of theirs, which have pictures without an alternative text
  (append-map
   (lambda (f)
     (let ((u (url-relative base f)))
       (if (or (in? (url->string u) seen) (not (url-exists? u))) '()
           (let* ((d (catch #t (lambda () (tree-import u "texmacs"))
                       (lambda args #f)))
                  (b (and d (tmfile-get d 'body))))
             (if (not (tree? b)) '()
                 (let ((n (length (pdf-tag-pictures-without-alt b)))
                       (more (pdf-tag-included-pictures
                              u b (cons (url->string u) seen))))
                   (if (> n 0) (cons (cons f n) more) more)))))))
   (pdf-tag-includes t)))

(define (pdf-tag-known-language? lan)
  ;; (pdf-tag-language gives en-US for a language it does not know)
  (or (in? lan '("english" "american"))
      (!= (language-to-locale lan) "en_US")))

(tm-define (pdf-tag-check)
  (:synopsis "What the current document lacks for an accessible tagged PDF")
  ;; an association list: the places of the pictures which a tagged PDF
  ;; describes by a caption or a file name (as the export does, through
  ;; the tagger: pdf-tag-pictures-without-alt), the included files which
  ;; have such pictures and how many, whether the document has a title
  ;; of its own (else the PDF has the name of the file), and the language
  ;; of the document when a PDF cannot say it (else #f)
  (let* ((body (buffer-tree))
         (rp (tree->path body))
         (pics (map (lambda (p) (append rp p))
                    (pdf-tag-pictures-without-alt body)))
         (buf (current-buffer))
         (incl (pdf-tag-included-pictures buf body (list (url->string buf))))
         (ttl (get-metadata "title"))
         (own-title? (and (!= ttl "")
                          (!= ttl (utf8->cork (url->string (url-tail buf))))))
         (lan (get-init "language")))
    (list (cons 'pictures pics)
          (cons 'included incl)
          (cons 'title own-title?)
          (cons 'language (and (not (pdf-tag-known-language? lan)) lan)))))

(tm-define (pdf-tag-check-lines r)
  (:synopsis "The lines which say what the check @r of pdf-tag-check found")
  (let* ((pics (length (assoc-ref r 'pictures)))
         (incl (assoc-ref r 'included))
         (lan (assoc-ref r 'language))
         (plural (lambda (n one many)
                   (string-append (number->string n) " " (if (== n 1) one many))))
         (l (append
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

(define (pdf-tag-go-to p)
  ;; the cursor before the picture at @p, the picture in focus
  (go-to (rcons p 0))
  (with t (path->tree p)
    (when (tree? t) (tree-focus t))))

(tm-define (pdf-tag-next-picture)
  (:synopsis "Go to the next picture without an alternative text")
  ;; after the cursor, else the first one again; #f when there is none
  (let* ((l (assoc-ref (pdf-tag-check) 'pictures))
         (cur (cursor-path))
         (next (list-find l (lambda (p) (path-less? cur (rcons p 0))))))
    (cond ((null? l)
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
