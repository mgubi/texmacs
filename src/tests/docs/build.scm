;; Typeset the TeXmacs documentation without a window and report problems.
;; Loaded by tests/docs/check.sh, see there for usage.
;;
;;   (docs-check-files "list" "out")  typesets every file listed (one
;;       absolute path per line) and appends one line per problem to out
;;   (docs-build-book "root.tm" "out.pdf" "out")  expands a book the way
;;       Help > Full manuals does (tmfs://help/book/...), generates the
;;       table of contents and the index, exports it to PDF and reports
;;       its problems
;;
;; Lines of the out file, separated by tabs:
;;   BEGIN <file>                  before a file is handled (a crash after
;;                                 this line is attributed to the file)
;;   PROBLEM <file> <kind> <what>  one problem
;;   END <file> <seconds>          the file is done
;;   INFO <file> <key> <value>     statistics (books)
;; The same BEGIN and END lines are written to the standard output so that
;; the messages of the log can be attributed to files.

(define docs-out #f)

(define (docs-write . l)
  (with s (string-append (apply string-append l) "\n")
    (string-append-to-file s docs-out)
    (display s)
    (force-output)))

(define (docs-problem file kind what)
  (docs-write "PROBLEM\t" file "\t" kind "\t" what))

(define (docs-time) (texmacs-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading the list of files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docs-lines s)
  (list-filter (string-tokenize-by-char s #\newline)
               (lambda (x) (!= x ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Walking trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; content which is shown as source code: tags inside are not typeset
(define docs-quoted
  '(inactive inactive* raw-data verbatim-code tm-fragment-source
    src-macro-source))

(define (docs-walk t f)
  ;; call f on every compound subtree of the stree t which is typeset
  (when (pair? t)
    (f t)
    (when (not (in? (car t) docs-quoted))
      (for-each (lambda (x) (docs-walk x f)) (cdr t)))))

(define (docs-strings t)
  ;; the string of an atomic stree, #f otherwise
  (and (string? t) t))

(define (docs-assigned t)
  ;; names defined in the document itself (preamble or body)
  (with h (make-ahash-table)
    (docs-walk t
      (lambda (x)
        (when (and (in? (car x) '(assign provide)) (pair? (cdr x))
                   (string? (cadr x)))
          (ahash-set! h (cadr x) #t))
        (when (== (car x) 'with)
          (let loop ((l (cdr x)))
            (when (and (pair? l) (pair? (cdr l)))
              (when (string? (car l)) (ahash-set! h (car l) #t))
              (loop (cddr l)))))
        (when (and (in? (car x) '(macro xmacro)))
          (for-each (lambda (a) (when (string? a) (ahash-set! h a #t)))
                    (cDr (cdr x))))))
    h))

(define (docs-labels t)
  (with h (make-ahash-table)
    (docs-walk t
      (lambda (x)
        (when (and (== (car x) 'label) (pair? (cdr x)) (string? (cadr x)))
          (ahash-set! h (cadr x) #t))))
    h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unknown tags and wrong numbers of arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docs-check-tags file t)
  ;; t is the tree of the typeset current buffer
  (let* ((s (tree->stree t))
         (local (docs-assigned s))
         (candidates (make-ahash-table))
         (seen (make-ahash-table)))
    (define (report kind what)
      (with key (string-append kind " " what)
        (when (not (ahash-ref seen key))
          (ahash-set! seen key #t)
          (docs-problem file kind what))))
    (define (visit x)
      (with lab (car x)
        (cond ((not (symbol? lab)) (noop))
              ((tree-label-extension? lab)
               (with name (symbol->string lab)
                 (when (not (or (style-has? name) (ahash-ref local name)))
                   (ahash-set! candidates lab #t))))
              (else
               (with n (length (cdr x))
                 (when (not (tree-possible-arity? (stree->tree x) n))
                   (report "bad-arity"
                           (string-append (symbol->string lab) "/"
                                          (number->string n)))))))))
    (docs-walk s visit)
    ;; a tag may still be defined where it is used, by a package which the
    ;; body loads (use-package) for instance: ask the typesetter there
    ;; (exactly: the fast approximation of the environment ignores the
    ;; assignments made by the body)
    (when (nnull? (ahash-set->list candidates))
      (set-fast-environments #f)
      (docs-walk-trees t
        (lambda (x)
          (with lab (tree-label x)
            (when (ahash-ref candidates lab)
              (ahash-remove! candidates lab)
              (with v (tree->stree (get-env-tree-at (symbol->string lab)
                                                    (tree->path x)))
                (when (or (== v '(uninit)) (== v ""))
                  (report "unknown-tag" (symbol->string lab))))))))
      (set-fast-environments
       (!= (get-preference "fast environments") "off")))))

(define (docs-walk-trees t f)
  (when (tree-compound? t)
    (f t)
    (when (not (in? (tree-label t) docs-quoted))
      (for-each (lambda (x) (docs-walk-trees x f)) (tree-children t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Links, branches and images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define docs-language-suffixes
  '(("english" . "en") ("french" . "fr") ("german" . "de")
    ("spanish" . "es") ("italian" . "it") ("polish" . "pl")
    ("portuguese" . "pt") ("chinese" . "zh") ("russian" . "ru")
    ("japanese" . "ja") ("korean" . "ko") ("dutch" . "nl")
    ("taiwanese" . "tw") ("ukrainian" . "uk") ("czech" . "cs")
    ("hungarian" . "hu") ("greek" . "gr") ("swedish" . "sv")))

(define (docs-file-suffix file)
  ;; "en" for foo.en.tm
  (let* ((n (string-length file))
         (m (- n 6)))
    (if (and (>= m 0) (string-ends? file ".tm")
             (== (string-ref file m) #\.))
        (substring file (+ m 1) (- n 3))
        "")))

(define (docs-relative cur name)
  ;; where tmdoc-expand looks for a branch (tmdoc-relative in doc/tmdoc.scm)
  (with rel (url-relative cur name)
    (if (url-regular? rel) rel
        (let* ((lan (docs-file-suffix (url->system cur)))
               (nfile (url-search-upwards (url-head cur)
                                          (string-append name "." lan ".tm")
                                          (list "doc" "web" "texmacs"))))
          (if (not (url-none? nfile)) nfile
              (let ((efile (url-search-upwards (url-head cur)
                                               (string-append name ".en.tm")
                                               (list "doc" "web" "texmacs"))))
                (if (not (url-none? efile)) efile rel)))))))

(define docs-label-cache (make-ahash-table))

(define (docs-file-labels u)
  (with key (url->system u)
    (or (ahash-ref docs-label-cache key)
        (with h (docs-labels (tree->stree (tree-import u "texmacs")))
          (ahash-set! docs-label-cache key h)
          h))))

(define (docs-external? dest)
  (or (string-starts? dest "http:") (string-starts? dest "https:")
      (string-starts? dest "ftp:") (string-starts? dest "mailto:")
      (string-starts? dest "tmfs:") (string-starts? dest "file:")
      (string-starts? dest "www.")))

(define (docs-check-link file cur dest own-labels)
  (let* ((pos (string-search-forwards "#" 0 dest))
         (path (if (>= pos 0) (substring dest 0 pos) dest))
         (anchor (and (>= pos 0) (substring dest (+ pos 1)
                                            (string-length dest)))))
    (cond ((docs-external? dest) (noop))
          ((== path "")
           (when (and anchor (not (ahash-ref own-labels anchor)))
             (docs-problem file "broken-anchor" dest)))
          (else
           (let* ((u0 (url-relative cur path))
                  (u (if (url-regular? u0) u0 (url-expand (url-complete u0 "r")))))
             (cond ((not (url-exists? u))
                    (docs-problem file "broken-link" dest))
                   ((and anchor (== (url-suffix u) "tm")
                         (not (ahash-ref (docs-file-labels u) anchor)))
                    (docs-problem file "broken-anchor" dest))))))))

(define (docs-check-links file s)
  (let* ((cur (system->url file))
         (labels (docs-labels s)))
    (docs-walk s
      (lambda (x)
        (cond ((and (in? (car x) '(branch continue extra-branch
                                   optional-branch))
                    (= (length x) 3) (string? (caddr x)))
               (with u (docs-relative cur (caddr x))
                 (when (not (url-exists? u))
                   (docs-problem file "broken-branch" (caddr x)))))
              ((and (in? (car x) '(hlink hyper-link))
                    (= (length x) 3) (string? (caddr x)))
               (docs-check-link file cur (caddr x) labels))
              ((and (== (car x) 'image) (pair? (cdr x)) (string? (cadr x)))
               (with u (url-relative cur (cadr x))
                 (when (not (url-exists? u))
                   (docs-problem file "missing-image" (cadr x))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docs-check-parse file)
  ;; the file as an stree, or #f if it is not a TeXmacs document
  (let* ((u (system->url file))
         (src (string-load u))
         (doc (tree->stree (tree-import u "texmacs"))))
    (cond ((not (string-starts? src "<TeXmacs|"))
           (docs-problem file "parse" "does not start with <TeXmacs|")
           #f)
          ((not (and (func? doc 'document) (assoc 'body (cdr doc))))
           (docs-problem file "parse" "no body")
           #f)
          (else
           ;; a missing > makes the parser take the rest of the file,
           ;; initial and attachments included, as the body
           (with body (cadr (assoc 'body (cdr doc)))
             (for-each (lambda (tag)
                         (when (nnull? (select body (list :* tag)))
                           (docs-problem file "parse"
                             (string-append "<" (symbol->string tag)
                                            "> inside the body"
                                            " (unbalanced brackets?)"))))
                       '(initial style body references auxiliary
                         attachments)))
           (let* ((lan (tmfile-language doc))
                  (suf (docs-file-suffix file))
                  (exp (and-with p (assoc lan docs-language-suffixes) (cdr p))))
             (when (and exp (!= exp suf))
               (docs-problem file "language"
                             (string-append lan " in a ." suf ".tm file"))))
           doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individual files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docs-check-file file)
  (let ((start (docs-time)))
    (docs-write "BEGIN\t" file)
    (with doc (docs-check-parse file)
      (when doc
        (let* ((u (system->url file))
               (old (current-buffer)))
          (load-buffer u)
          (switch-to-buffer u)
          (update-forced)
          (docs-check-tags file (buffer-tree))
          (docs-check-links file (tree->stree (buffer-tree)))
          (buffer-close u)
          (when (and old (buffer-exists? old)) (switch-to-buffer old)))))
    (docs-write "END\t" file "\t"
                (number->string (/ (- (docs-time) start) 1000.0)))))

(define (docs-check-files list out)
  (set! docs-out (system->url out))
  (for-each (lambda (file)
              (catch #t
                (lambda () (docs-check-file file))
                (lambda args
                  (docs-problem file "scheme-error" (object->string args))
                  (docs-write "END\t" file "\t0"))))
            (docs-lines (string-load (system->url list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Books
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docs-check-refs file s)
  ;; references to labels which the typesetter does not know; the labels
  ;; made automatically for the table of contents and the index (auto-N)
  ;; are counted, since their numbers change with every edit
  (let ((labels (make-ahash-table))
        (seen (make-ahash-table))
        (autos 0))
    (for-each (lambda (l) (ahash-set! labels l #t)) (list-references))
    (docs-walk s
      (lambda (x)
        (when (and (in? (car x) '(reference pageref eqref))
                   (pair? (cdr x)) (string? (cadr x))
                   (not (ahash-ref labels (cadr x)))
                   (not (ahash-ref seen (cadr x))))
          (ahash-set! seen (cadr x) #t)
          (if (string-starts? (cadr x) "auto-")
              (set! autos (+ autos 1))
              (docs-problem file "undefined-reference" (cadr x))))))
    (when (> autos 0)
      (docs-problem file "undefined-reference"
                    (string-append "auto-* (" (number->string autos)
                                   " page numbers of the table of contents"
                                   " or the index are ?)")))))

(define (docs-count tag s)
  (with n 0
    (docs-walk s (lambda (x) (when (== (car x) tag) (set! n (+ n 1)))))
    n))

(define (docs-aux-size name)
  ;; the number of entries of a generated table of contents or index
  (with t (tree->stree (get-auxiliary name))
    (if (pair? t) (length (cdr t)) 0)))

(define (docs-build-book* root pdf)
  (let* ((start (docs-time))
         (u (system->url root)))
    (docs-write "BEGIN\t" root)
    ;; what Help > Full manuals > User manual does (load-help-book and
    ;; tmdoc-expand-help-manual in doc/help-funcs.scm and doc/tmdoc.scm),
    ;; with the idle-time steps taken directly
    (tmdoc-expand-help u "book")
    (update-forced)
    (for-each (lambda (pass)
                (generate-all-aux)
                (update-current-buffer)
                (update-forced))
              '(1 2 3))
    (let* ((buf (current-buffer))
           (s (tree->stree (buffer-tree))))
      (docs-write "INFO\t" root "\tbuffer\t" (url->system buf))
      (docs-write "INFO\t" root "\tsections\t"
                  (number->string (+ (docs-count 'chapter s)
                                     (docs-count 'section s))))
      (docs-write "INFO\t" root "\tpages\t"
                  (number->string (get-page-count)))
      (for-each (lambda (aux)
                  (with n (docs-aux-size aux)
                    (docs-write "INFO\t" root "\t" aux "\t" (number->string n))
                    (when (== n 0)
                      (docs-problem root "empty-aux" aux))))
                '("toc" "idx"))
      (docs-check-tags root (buffer-tree))
      (docs-check-refs root s)
      (print-to-file (system->url pdf))
      (buffer-pretend-saved buf)
      (buffer-close buf))
    (docs-write "END\t" root "\t"
                (number->string (/ (- (docs-time) start) 1000.0)))))

(define (docs-build-book root pdf out)
  (set! docs-out (system->url out))
  (catch #t
    (lambda () (docs-build-book* root pdf))
    (lambda args
      (docs-problem root "scheme-error" (object->string args))
      (docs-write "END\t" root "\t0"))))
