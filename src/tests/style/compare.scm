;; Typeset a document and its rewriting by the style rewriter, for
;; tests/style/compare.sh: for each line "document directory" of a list, the
;; document is printed to directory/orig.pdf and a copy of it whose body is
;; the rewritten body to directory/rewritten.pdf.
;;
;; usage: texmacs.bin -headless -x '(load "compare.scm")' -x '(compare-list "jobs")'

(define (print-buffer u out)
  (switch-to-buffer u)
  (update-forced)
  (update-current-buffer)
  (update-forced)
  (print-to-file (system->url out)))

(define (with-current-version doc)
  ;; the rewritten body is in the current format, so the copy has the
  ;; current version (else it would be upgraded again when it is loaded)
  (cons (car doc)
        (map (lambda (x)
               (if (and (pair? x) (== (car x) 'TeXmacs))
                   (list 'TeXmacs (texmacs-version))
                   x))
             (cdr doc))))

(define (compare-one in dir)
  (let* ((u (system->url in))
         (copy (system->url (string-append dir "/rewritten.tm"))))
    (load-buffer u)
    (switch-to-buffer u)
    (update-forced)
    ;; (a copy which keeps the subtrees shared in the rewritten tree
    ;; shared: the typesetter makes a hard-id from the address of its
    ;; argument, which the locus and the link of a hyperlink share; with
    ;; tree-copy the links were lost, and the PostScript, which Ghostscript
    ;; turns into the PDF, was otherwise cut into text runs)
    (let ((r (style-rewrite-copy))
          (doc (tree->stree (tree-import u "texmacs"))))
      (print-buffer u (string-append dir "/orig.pdf"))
      ;; the copy is written with the body of the document, and its body is
      ;; set to the rewritten tree once loaded: written as text, adjacent
      ;; strings of a concat would be read back as one string (which kerns
      ;; otherwise); relative links and images are resolved from the document
      (tree-export (stree->tree (with-current-version doc)) copy "texmacs")
      (load-buffer copy)
      (buffer-set-master copy u)
      (buffer-set-body copy r)
      (print-buffer copy (string-append dir "/rewritten.pdf"))
      (buffer-close copy)
      (buffer-close u))))

(define (compare-list file)
  ;; the tree-sitter highlighter colours markup inside inline code only when
  ;; it comes from a macro body, which the rewriter expands into the
  ;; document: the comparison is made without it (see src/docs/pure-style.md)
  (when (defined? 'tree-sitter-set-highlighting)
    (tree-sitter-set-highlighting #f))
  (for (line (string-tokenize-by-char (string-load (system->url file)) #\newline))
    (with l (string-tokenize-by-char line #\space)
      (when (== (length l) 2)
        (catch #t
          (lambda () (compare-one (car l) (cadr l)))
          (lambda args (display* "error: " (car l) " " args "\n")))))))
