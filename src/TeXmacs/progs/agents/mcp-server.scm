;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mcp-server.scm
;; DESCRIPTION : `texmacs --mcp': TeXmacs as an MCP server over stdio
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See src/docs/mcp-server.md. The Model Context Protocol (JSON-RPC 2.0, one
;; message per line) is served on the standard input and output of a headless
;; TeXmacs started as `texmacs --mcp'. C++ only gives a non-blocking read of
;; the input and a write of the output (mcp_stdio.cpp); everything else, the
;; protocol and the tools, is here, so that every build shares it.
;;
;; The input is polled on the main thread by a delayed command, as the shared
;; files are. A tool reads documents which this process has open (shared with
;; the person's TeXmacs when it has the file open too, see shared-files.scm)
;; and gives opaque handles (n17) to their nodes: tree pointers, which follow
;; their node through the edits, with a hash of its content. Nothing here
;; evaluates code, nor reaches anything but the documents.

(texmacs-module (agents mcp-server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The reader and the writer are those of C++ (json->typed-tree and
;; typed-tree->json). An object is (attr key value ...), an array (tuple ...),
;; a number (json-number "3"), a boolean (json-boolean "true") and the null
;; (json-null); strings are in utf8.

(define (obj . l) (cons 'attr l))
(define (arr l) (cons 'tuple l))
(define (num n) (list 'json-number (number->string n)))
(define (bool b) (list 'json-boolean (if b "true" "false")))
(define jnull '(json-null))

(define (jref t . keys)
  ;; the value of @t at the keys (strings) or indices, or #f
  (cond ((null? keys) t)
        ((and (string? (car keys)) (func? t 'attr))
         (let loop ((l (cdr t)))
           (cond ((or (null? l) (null? (cdr l))) #f)
                 ((== (car l) (car keys)) (apply jref (cadr l) (cdr keys)))
                 (else (loop (cddr l))))))
        ((and (integer? (car keys)) (func? t 'tuple)
              (>= (car keys) 0) (< (car keys) (length (cdr t))))
         (apply jref (list-ref (cdr t) (car keys)) (cdr keys)))
        (else #f)))

(define (jnull? t) (or (not t) (func? t 'json-null)))
(define (jstring t) (and (string? t) t))
(define (jint t)
  (and (func? t 'json-number 1)
       (with n (string->number (cadr t))
         (and n (integer? n) (inexact->exact n)))))
(define (jbool t default)
  (cond ((jnull? t) default)
        ((func? t 'json-boolean 1) (== (cadr t) "true"))
        (else default)))

(define (json-balanced? s)
  ;; do the brackets and quotes of @s match? (the reader of C++ is lenient:
  ;; it reads some text which is not JSON, so that a message which is cut
  ;; or garbled is found here)
  (let ((n (string-length s)))
    (let loop ((i 0) (depth 0) (in-string? #f))
      (if (>= i n)
          (and (== depth 0) (not in-string?))
          (let ((c (string-ref s i)))
            (cond (in-string?
                   (cond ((char=? c #\\) (loop (+ i 2) depth #t))
                         ((char=? c #\") (loop (+ i 1) depth #f))
                         (else (loop (+ i 1) depth #t))))
                  ((char=? c #\") (loop (+ i 1) depth #t))
                  ((or (char=? c #\{) (char=? c #\[)) (loop (+ i 1) (+ depth 1) #f))
                  ((or (char=? c #\}) (char=? c #\]))
                   (and (> depth 0) (loop (+ i 1) (- depth 1) #f)))
                  (else (loop (+ i 1) depth #f))))))))

(define (json-parse s)
  (and (string? s) (!= s "") (json-balanced? s)
       (catch #t
         (lambda () (tree->stree (json->typed-tree s)))
         (lambda args #f))))

(define (json-write t)
  (typed-tree->json (stree->tree t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (everything printed goes to $TEXMACS_HOME_PATH/system/mcp.log: C++ moved
;; the descriptors 1 and 2 there; the protocol has its own output)

(define mcp-debug? (and (getenv "TEXMACS_MCP_DEBUG") #t))

(define (mcp-log . l)
  (apply display* (cons "mcp: " l))
  (display* "\n"))

(define (log-debug . l)
  (when mcp-debug? (apply mcp-log l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Texts: Cork inside, utf8 outside
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (u8 s) (cork->utf8 s))

(define (continuation-byte? c)
  (let ((b (char->integer c))) (and (>= b 128) (< b 192))))

(define (utf8-prefix s n)
  ;; the first @n bytes of @s, not cutting a character of utf8 in two
  (if (<= (string-length s) n)
      s
      (let loop ((k n))
        (if (and (> k 0)
                 (continuation-byte? (string-ref s k)))
            (loop (- k 1))
            (substring s 0 k)))))

(define (utf8-suffix s n)
  ;; the last @n bytes of @s, not cutting a character of utf8 in two
  (let ((len (string-length s)))
    (if (<= len n)
        s
        (let loop ((k (- len n)))
          (if (and (< k len)
                   (continuation-byte? (string-ref s k)))
              (loop (+ k 1))
              (substring s k len))))))

(define (hex-digits n)
  (let ((s (number->string n 16))) s))

(define (ascii-snippet s)
  ;; a string of the .tm syntax (Cork) as ascii: a byte above 127 is written
  ;; as the character \<#xxxx\>
  (let loop ((i 0) (acc '()) (plain 0))
    (define (flush) (if (< plain i) (cons (substring s plain i) acc) acc))
    (cond ((>= i (string-length s))
           (string-recompose (reverse (if (< plain i)
                                          (cons (substring s plain i) acc)
                                          acc))
                             ""))
          ((< (char->integer (string-ref s i)) 128) (loop (+ i 1) acc plain))
          (else
            (let* ((c (substring s i (+ i 1)))
                   (u (cork->utf8 c))
                   (code (utf8-code u)))
              (loop (+ i 1)
                    (cons (string-append "\\<#" (hex-digits code) "\\>")
                          (flush))
                    (+ i 1)))))))

(define (utf8-code u)
  ;; the code point of the first character of @u
  (let* ((b0 (char->integer (string-ref u 0))) (n (string-length u)))
    (cond ((< b0 128) b0)
          ((and (>= b0 192) (< b0 224) (>= n 2))
           (+ (* 64 (- b0 192)) (- (char->integer (string-ref u 1)) 128)))
          ((and (>= b0 224) (< b0 240) (>= n 3))
           (+ (* 4096 (- b0 224))
              (* 64 (- (char->integer (string-ref u 1)) 128))
              (- (char->integer (string-ref u 2)) 128)))
          ((and (>= b0 240) (>= n 4))
           (+ (* 262144 (- b0 240))
              (* 4096 (- (char->integer (string-ref u 1)) 128))
              (* 64 (- (char->integer (string-ref u 2)) 128))
              (- (char->integer (string-ref u 3)) 128)))
          (else 63))))

(define (stree-u8 t)
  ;; the stree @t with its strings in utf8
  (cond ((string? t) (u8 t))
        ((pair? t) (map stree-u8 t))
        (else t)))

(define (squeeze s)
  ;; blanks of @s reduced to single spaces
  (let loop ((i 0) (acc '()) (space? #f))
    (cond ((>= i (string-length s))
           (list->string (reverse acc)))
          ((memv (string-ref s i) '(#\space #\newline #\tab #\return))
           (loop (+ i 1) acc #t))
          (else
            (loop (+ i 1)
                  (cons (string-ref s i)
                        (if (and space? (pair? acc)) (cons #\space acc) acc))
                  #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The text of a node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define invisible-tags
  '(label flag hspace vspace vspace* hspace* new-page new-line line-break
    no-break no-page-break page-break page-break* new-dpage tmlen assign
    glue tag-formatting-hook index-complex index-line index-1 index-2 index
    document-at toc-main-1 toc-main-2 toc-normal-1 toc-normal-2 toc-small-1
    toc-small-2 toc-dots toc-strong-1 toc-strong-2 write))

(define math-tags
  '(math equation equation* eqnarray eqnarray* align align* gather gather*
    multline multline* split flalign flalign*))

(define (math-text t)
  ;; (the converter to text knows the symbols of mathematics)
  (catch #t
    (lambda ()
      (utf8->cork (squeeze (convert (tm->stree t) "texmacs-stree" "verbatim-snippet"))))
    (lambda args "")))

(define (ntext t)
  ;; the same in utf8 (what the client gets)
  (u8 (node-text t)))

(define (search-text t)
  ;; the text of @t as Cork with one form for each character (a character
  ;; may be a byte of Cork or a symbol <#e9>), to be searched
  (utf8->cork (ntext t)))

(define (join-texts l sep)
  (string-recompose (list-filter l (lambda (s) (!= s ""))) sep))

(define (node-text t)
  ;; the text that a reader sees in @t, in Cork, paragraphs on lines
  (cond ((tree-atomic? t) (tree->string t))
        ((tm-is? t 'document)
         (join-texts (map node-text (tree-children t)) "\n"))
        ((in? (tree-label t) math-tags) (math-text t))
        ((in? (tree-label t) invisible-tags) "")
        ((tm-is? t 'with) (node-text (tree-ref t (- (tree-arity t) 1))))
        ((tm-is? t 'table)
         (join-texts (map node-text (tree-children t)) "\n"))
        ((tm-is? t 'row)
         (join-texts (map node-text (tree-children t)) " | "))
        ((in? (tree-label t) '(reference eqref pageref))
         (string-append "(ref " (node-text (tree-ref t 0)) ")"))
        ((in? (tree-label t) '(cite nocite cite-detail))
         (string-append "[" (join-texts (map node-text (tree-children t)) ", ")
                        "]"))
        ((in? (tree-label t) '(hlink href))
         (node-text (tree-ref t 0)))
        ((in? (tree-label t) '(image postscript postscript*))
         "[image]")
        ((tm-is? t 'tformat)
         (node-text (tree-ref t (- (tree-arity t) 1))))
        ((in? (tree-label t) '(doc-data))
         (join-texts (map node-text (tree-children t)) "\n"))
        (else
          (join-texts (map node-text (tree-children t))
                      (if (in? (tree-label t) '(concat)) "" " ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A document of the session: (id url path), id d1, d2... The agent refers to
;; it by the id or by its path.

(define mcp-docs '())                       ; in the order of opening
(define mcp-doc-count 0)

(define (doc-id d) (car d))
(define (doc-url d) (cadr d))
(define (doc-path d) (caddr d))

(define mcp-tool-error-message "")

(define (tool-error msg)
  ;; (the message is kept aside: what a handler of catch gets of the
  ;; arguments of throw differs between the Schemes)
  (set! mcp-tool-error-message msg)
  (throw 'mcp-tool-error))

(define (canonical-path s)
  (with r (shared-real-path s)
    (if (and (string? r) (!= r "")) r s)))

(define (absolute-path? s)
  (or (string-starts? s "/")
      (and (> (string-length s) 2) (== (string-ref s 1) #\:)
           (memv (string-ref s 2) '(#\\ #\/)))))

(define (find-doc ref)
  ;; the document named by the id or the path @ref, or #f
  (or (list-find mcp-docs (lambda (d) (== (doc-id d) ref)))
      (and (absolute-path? ref)
           (with p (canonical-path ref)
             (list-find mcp-docs (lambda (d) (== (doc-path d) p)))))))

(define (open-doc! path)
  (let* ((p (canonical-path path)) (old (find-doc path)))
    (cond (old old)
          ((not (url-exists? (string->url path)))
           (tool-error (string-append "no such file: " path)))
          (else
            (let ((u (string->url path)))
              (when (not (buffer-exists? u))
                (load-buffer u))
              (when (not (buffer-exists? u))
                (tool-error (string-append "could not open " path)))
              (set! mcp-doc-count (+ mcp-doc-count 1))
              (let ((d (list (string-append "d" (number->string mcp-doc-count))
                             u p)))
                (set! mcp-docs (append mcp-docs (list d)))
                d))))))

(define (require-doc ref)
  ;; the document @ref (an id or a path), opened if it is a path
  (cond ((not (string? ref))
         (tool-error "the argument `document' is needed (an id or a path)"))
        ((find-doc ref))
        ((absolute-path? ref) (open-doc! ref))
        (else (tool-error (string-append "unknown document: " ref
                                         " (see list_documents)")))))

(define (doc-refresh! d)
  ;; serve the links of the shared files, so that what a host has sent is in
  (when (shared-file? (doc-url d))
    (shared-poll)))

(define (doc-body d)
  (buffer-get-body (doc-url d)))

(define (doc-shared d)
  ;; how the document is shared, an object, or null
  (with i (shared-file-info (doc-url d))
    (if i
        (obj "role" (symbol->string (car i))
             "state" (let ((s (cadr i))) (if (number? s) (num s) (object->string s)))
             "participants" (num (+ 1 (caddr i))))
        jnull)))

(define (doc-info d)
  (let ((u (doc-url d)))
    (obj "document" (doc-id d)
         "path" (doc-path d)
         "file" (u8 (buffer-get-title u))
         "title" (let ((t (title-of-document d))) (if t t jnull))
         "style" (let ((s (tm->stree (buffer-get-style u))))
                   (arr (map u8 (cond ((string? s) (list s))
                                      ((pair? s) (list-filter (cdr s) string?))
                                      (else '())))))
         "modified" (bool (buffer-modified? u))
         "shared" (doc-shared d)
         "blocks" (num (tree-arity (doc-body d)))
         "hash" (hash-short (doc-body d)))))

(define (hash-short t)
  (utf8-prefix (tree-hash t) 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A handle (n17) is a tree pointer of this process, which follows its node
;; through the edits of the document, and the document it is in.

(define mcp-handles (make-ahash-table))     ; "n17" -> (pointer . document id)
(define mcp-handle-count 0)
(define mcp-handle-cap 40000)

(define (handle-number h)
  (and (string? h) (> (string-length h) 1) (== (string-ref h 0) #\n)
       (string->number (substring h 1 (string-length h)))))

(define (collect-handles!)
  ;; (the oldest half goes when there are too many)
  (when (> (ahash-size mcp-handles) mcp-handle-cap)
    (let ((limit (- mcp-handle-count (quotient mcp-handle-cap 2))))
      (for (e (ahash-table->list mcp-handles))
        (when (< (or (handle-number (car e)) 0) limit)
          (tree-pointer-detach (cadr e))
          (ahash-remove! mcp-handles (car e)))))))

(define (make-handle! d t)
  (collect-handles!)
  (set! mcp-handle-count (+ mcp-handle-count 1))
  (let ((h (string-append "n" (number->string mcp-handle-count))))
    (ahash-set! mcp-handles h (list (tree->tree-pointer t) (doc-id d) t))
    h))

(define (handle-node d h)
  ;; the node of the handle @h of the document @d. A tree pointer moves to
  ;; the nearest tree when its node is removed: a handle is good only while
  ;; its pointer is at the tree it was made for (a node replaced as a whole
  ;; is lost too; the agent reads it again)
  (let ((e (ahash-ref mcp-handles h)))
    (cond ((not e) (tool-error (string-append "unknown handle " h)))
          ((!= (cadr e) (doc-id d))
           (tool-error (string-append "the handle " h " is of the document "
                                      (cadr e))))
          (else
            (let ((t (tree-pointer->tree (car e))))
              (if (and (tree? t) (tree-eq? t (caddr e)) (inside-body? d t))
                  t
                  (tool-error (string-append "the node of " h
                                             " does not exist any more"))))))))

(define (inside-body? d t)
  (let ((root (doc-body d)))
    (or (tree-eq? t root)
        (let ((p (tree->path t)) (r (tree->path root)))
          (and (list? p) (list? r) (> (length p) (length r))
               (== (list-head p (length r)) r))))))

(define (tree-valid? t)
  (not (tm-is? t 'uninit)))

(define (node-tag t)
  (if (tree-atomic? t) "string" (symbol->string (tree-label t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define section-levels
  '((part . 0) (chapter . 1) (section . 2) (subsection . 3)
    (subsubsection . 4) (paragraph . 5) (subparagraph . 6)
    (part* . 0) (chapter* . 1) (section* . 2) (subsection* . 3)
    (subsubsection* . 4) (paragraph* . 5) (subparagraph* . 6)))

(define (section-level t)
  (and (tree-compound? t)
       (with e (assoc (tree-label t) section-levels)
         (and e (cdr e)))))

(define (tree-labels t)
  ;; the names of the labels in @t
  (cond ((tree-atomic? t) '())
        ((and (tm-is? t 'label) (== (tree-arity t) 1)
              (tree-atomic? (tree-ref t 0)))
         (list (u8 (tree->string (tree-ref t 0)))))
        (else (append-map tree-labels (tree-children t)))))

(define (append-map f l)
  (apply append (map f l)))

(define (top-level-ancestor d t)
  ;; the child of the body which contains @t (or the body itself)
  (let* ((root (doc-body d)) (r (tree->path root)) (p (tree->path t)))
    (if (and (list? p) (list? r) (> (length p) (length r)))
        (path->tree (list-head p (+ 1 (length r))))
        t)))

(define (block-of t)
  ;; the paragraph (a child of a document) which contains @t
  (let loop ((x t))
    (with p (tree-up x)
      (cond ((not p) x)
            ((tm-is? p 'document) x)
            (else (loop p))))))

(define (index-in-body d t)
  (with r (tree->path (doc-body d))
    (with p (tree->path t)
      (and (list? p) (list? r) (> (length p) (length r))
           (list-ref p (length r))))))

(define (title-of-document d)
  ;; the text of the title in the doc-data, or #f
  (let loop ((l (tree-children (doc-body d))))
    (cond ((null? l) #f)
          ((tm-is? (car l) 'doc-data)
           (let find ((c (tree-children (car l))))
             (cond ((null? c) #f)
                   ((tm-is? (car c) 'doc-title) (ntext (car c)))
                   (else (find (cdr c))))))
          (else (loop (cdr l))))))

(define (outline-entries d)
  ;; ((index level node) ...) of the sections in the body
  (let loop ((i 0) (l (tree-children (doc-body d))) (acc '()))
    (cond ((null? l) (reverse acc))
          ((section-level (car l))
           (loop (+ i 1) (cdr l)
                 (cons (list i (section-level (car l)) (car l)) acc)))
          (else (loop (+ i 1) (cdr l) acc)))))

(define (section-of d t)
  ;; the title of the section (of the body) in which @t is, or #f
  (let ((i (index-in-body d t)))
    (and i
         (let loop ((l (outline-entries d)) (best #f))
           (cond ((or (null? l) (> (car (car l)) i)) best)
                 (else (loop (cdr l) (ntext (caddr (car l))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define read-formats '("texmacs" "stree" "latex" "markdown" "text"))

(define (convert-node t fmt)
  ;; the content of the tree @t in the format @fmt, in utf8 (ascii for texmacs)
  (let ((st (tm->stree t)))
    (cond ((== fmt "stree")
           (object->string (stree-u8 st)))
          ((== fmt "text") (ntext t))
          ((== fmt "texmacs")
           (ascii-snippet (convert st "texmacs-stree" "texmacs-snippet")))
          ((== fmt "latex")
           (u8 (convert st "texmacs-stree" "latex-snippet")))
          ((== fmt "markdown")
           (u8 (convert st "texmacs-stree" "markdown-snippet")))
          (else (tool-error (string-append "unknown format " fmt))))))

(define (convert-checked t fmt)
  (catch #t
    (lambda () (convert-node t fmt))
    (lambda args
      (if (and (pair? args) (eq? (car args) 'mcp-tool-error))
          (throw (quote mcp-tool-error))
          (tool-error (string-append "the conversion to " fmt " failed"))))))

(define (estimate-size t)
  ;; (cheap) about the size of the content of @t
  (if (tree-atomic? t)
      (string-length (tree->string t))
      (+ 8 (apply + (map estimate-size (tree-children t))))))

(define (children-window t start budget)
  ;; the children of @t from @start, as many as seem to fit in @budget
  (let loop ((l (list-tail (tree-children t) start)) (n 0) (size 0))
    (cond ((null? l) n)
          ((and (> n 0) (> (+ size (estimate-size (car l))) budget)) n)
          (else (loop (cdr l) (+ n 1) (+ size (estimate-size (car l))))))))

(define (window-tree t start n)
  (tm->tree `(document ,@(map tree->stree
                              (sublist (tree-children t) start (+ start n))))))

(define (window-content t start n fmt max-size)
  ;; the content of the children start..start+n-1 of the document @t; fewer
  ;; of them when it is longer than @max-size. Gives (content . n).
  (let loop ((n n) (tries 0))
    (let* ((content (convert-checked (window-tree t start n) fmt))
           (len (string-length content)))
      (if (and (> len max-size) (> n 1) (< tries 6))
          (loop (max 1 (inexact->exact
                        (floor (* n (/ max-size len) 0.9))))
                (+ tries 1))
          (cons content n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools: documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tool-list-documents args)
  (let ((mine (map (lambda (d)
                     (doc-refresh! d)
                     (with i (doc-info d)
                       (append i (list "opened_by" "agent"))))
                   mcp-docs)))
    (obj "documents" (arr mine)
         "open_elsewhere"
         (arr (map (lambda (e)
                     (obj "path" (u8 (car e))
                          "person" (bool (cadr e))
                          "pid" (num (caddr e))))
                   (list-filter (shared-files-in-use)
                                (lambda (e)
                                  (not (find-doc (car e))))))))))

(define (tool-open-document args)
  (let ((path (jstring (jref args "path"))))
    (when (not path) (tool-error "the argument `path' is needed"))
    (when (not (absolute-path? path))
      (tool-error "the path must be absolute"))
    (let ((d (open-doc! path)))
      (doc-refresh! d)
      (doc-info d))))

(define (tool-get-outline args)
  (let* ((d (require-doc (jstring (jref args "document")))))
    (doc-refresh! d)
    (let ((entries (outline-entries d)) (body (doc-body d)))
      (obj "document" (doc-id d)
           "title" (let ((t (title-of-document d))) (if t t jnull))
           "hash" (hash-short body)
           "blocks" (num (tree-arity body))
           "sections"
           (arr (let loop ((l entries) (acc '()))
                  (if (null? l)
                      (reverse acc)
                      (let* ((e (car l)) (t (caddr e))
                             (next (if (null? (cdr l))
                                       (tree-arity body)
                                       (car (cadr l))))
                             (chars (apply +
                                      (map estimate-size
                                           (sublist (tree-children body)
                                                    (car e) next)))))
                        (loop (cdr l)
                              (cons (obj "handle" (make-handle! d t)
                                         "tag" (node-tag t)
                                         "level" (num (cadr e))
                                         "title" (ntext t)
                                         "labels" (arr (tree-labels t))
                                         "index" (num (car e))
                                         "chars" (num chars)
                                         "hash" (hash-short t))
                                    acc))))))))))

(define (child-summary d t i)
  (obj "handle" (make-handle! d t)
       "index" (num i)
       "tag" (node-tag t)
       "hash" (hash-short t)
       "preview" (utf8-prefix (squeeze (ntext t)) 80)))

(define default-max-size 12000)

(define (tool-read args)
  (let* ((d (require-doc (jstring (jref args "document"))))
         (fmt (or (jstring (jref args "format")) "text"))
         (max-size (min 400000 (max 200 (or (jint (jref args "max_size"))
                                              default-max-size))))
         (start (max 0 (or (jint (jref args "start")) 0)))
         (h (jstring (jref args "node"))))
    (when (not (member fmt read-formats))
      (tool-error (string-append "unknown format " fmt ", use one of: "
                                 (string-recompose read-formats ", "))))
    (doc-refresh! d)
    (let* ((t (if h (handle-node d h) (doc-body d)))
           (state (let ((i (shared-file-info (doc-url d)))) (if i (cadr i) #f)))
           (head (list "document" (doc-id d)
                       "node" (if h h jnull)
                       "tag" (node-tag t)
                       "hash" (hash-short t)
                       "format" fmt)))
      (cond ((tree-atomic? t)
             (let ((s (convert-checked t fmt)))
               (apply obj (append head
                                  (list "content" (utf8-prefix s max-size)
                                        "truncated"
                                        (bool (> (string-length s) max-size)))))))
            ((and (tm-is? t 'document) (> (tree-arity t) 0))
             (let* ((total (tree-arity t))
                    (start (min start (- total 1)))
                    (n (children-window t start max-size))
                    (r (window-content t start n fmt max-size))
                    (n (cdr r))
                    (content (car r))
                    (cut? (> (string-length content) max-size))
                    (end (+ start n)))
               (apply obj
                 (append head
                   (list "content" (utf8-prefix content max-size)
                         "start" (num start)
                         "end" (num end)
                         "blocks" (num total)
                         "truncated" (bool (or cut? (< end total)))
                         "next_start" (if (< end total) (num end) jnull)
                         "children"
                         (arr (let loop ((i start) (acc '()))
                                (if (or (>= i end) (>= (- i start) 200))
                                    (reverse acc)
                                    (loop (+ i 1)
                                          (cons (child-summary
                                                 d (tree-ref t i) i) acc))))))))))
            (else
              (let ((s (convert-checked t fmt)))
                (apply obj
                  (append head
                    (list "content" (utf8-prefix s max-size)
                          "truncated" (bool (> (string-length s) max-size))
                          "children"
                          (arr (let loop ((i 0) (acc '()))
                                 (if (or (>= i (tree-arity t)) (>= i 50))
                                     (reverse acc)
                                     (loop (+ i 1)
                                           (cons (child-summary
                                                  d (tree-ref t i) i)
                                                 acc))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools: find
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (match-context text query)
  ;; a piece of @text around the first occurrence of @query (Cork; the
  ;; search ignores the case), or #f
  (let* ((low (locase-all text)) (q (locase-all query))
         (i (string-search-forwards q 0 low)))
    (and (>= i 0)
         (let* ((a (max 0 (- i 50)))
                (e (+ i (string-length q))) (b (min (string-length text) (+ e 50))))
           (string-append (if (> a 0) "..." "")
                          (substring text a b)
                          (if (< b (string-length text)) "..." ""))))))

(define (find-text-blocks t query)
  ;; the innermost paragraphs (children of documents) of @t which contain
  ;; @query: ((node . context) ...)
  (let loop ((l (if (tm-is? t 'document) (tree-children t) (list t)))
             (acc '()))
    (if (null? l)
        (reverse acc)
        (let* ((c (car l)) (text (search-text c)) (ctx (match-context text query)))
          (if (not ctx)
              (loop (cdr l) acc)
              (let ((inner (find-inner c query)))
                (loop (cdr l)
                      (append (reverse (if (null? inner) (list (cons c ctx)) inner))
                              acc))))))))

(define (find-inner t query)
  ;; the matches in the documents inside @t
  (cond ((tree-atomic? t) '())
        ((tm-is? t 'document) (find-text-blocks t query))
        (else (append-map (lambda (c) (find-inner c query))
                          (tree-children t)))))

(define (find-tags t tag)
  ;; the nodes of @t with the label @tag (a symbol)
  (cond ((tree-atomic? t) '())
        (else (append (if (== (tree-label t) tag) (list t) '())
                      (append-map (lambda (c) (find-tags c tag))
                                  (tree-children t))))))

(define (find-label-blocks t name)
  (let ((labels (find-tags t 'label)))
    (map (lambda (l) (block-of l))
         (list-filter labels
                      (lambda (l) (and (== (tree-arity l) 1)
                                       (== (tree->string* (tree-ref l 0)) name)))))))

(define (tree->string* t)
  (if (tree-atomic? t) (tree->string t) ""))

(define find-limit 50)

(define (cork-prefix s n)
  (if (> (string-length s) n) (substring s 0 n) s))

(define (hit d t context)
  (obj "handle" (make-handle! d t)
       "tag" (node-tag t)
       "hash" (hash-short t)
       "section" (let ((s (section-of d t))) (if s s jnull))
       "context" (squeeze (u8 context))))

(define (tool-find args)
  (let* ((d (require-doc (jstring (jref args "document"))))
         (query (jstring (jref args "query")))
         (limit (min 500 (max 1 (or (jint (jref args "limit")) find-limit)))))
    (when (or (not query) (== query ""))
      (tool-error "the argument `query' is needed"))
    (doc-refresh! d)
    (let* ((body (doc-body d))
           (cq (utf8->cork query))
           (all
            (cond
              ((string-starts? query "tag:")
               (let ((tag (string->symbol (substring query 4 (string-length query)))))
                 (map (lambda (t) (cons t (cork-prefix (squeeze (search-text t)) 100)))
                      (find-tags body tag))))
              ((string-starts? query "label:")
               (let ((name (substring query 6 (string-length query))))
                 (map (lambda (t) (cons t (cork-prefix (squeeze (search-text t)) 100)))
                      (find-label-blocks body (utf8->cork name)))))
              ((string-starts? query "section:")
               (let ((q (locase-all
                         (utf8->cork (tm-string-trim-both
                                      (substring query 8 (string-length query)))))))
                 (map (lambda (e) (cons (caddr e) (search-text (caddr e))))
                      (list-filter (outline-entries d)
                                   (lambda (e)
                                     (>= (string-search-forwards
                                          q 0 (locase-all (search-text (caddr e))))
                                         0))))))
              (else
                (find-text-blocks body cq)))))
      (obj "document" (doc-id d)
           "query" query
           "count" (num (length all))
           "truncated" (bool (> (length all) limit))
           "matches" (arr (map (lambda (e) (hit d (car e) (cdr e)))
                               (if (> (length all) limit)
                                   (sublist all 0 limit)
                                   all)))))))

(define (tree->string-cork t)
  ;; the text of @t in Cork (for searching with a query in Cork)
  (cond ((tree-atomic? t) (tree->string t))
        (else (string-recompose (map tree->string-cork (tree-children t)) ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools: references
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reference-tags '(reference pageref eqref))
(define citation-tags '(cite nocite cite-detail))

(define (tool-get-references args)
  (let* ((d (require-doc (jstring (jref args "document")))))
    (doc-refresh! d)
    (let* ((body (doc-body d))
           (labels (append-map (lambda (l) (if (and (== (tree-arity l) 1)
                                                      (tree-atomic? (tree-ref l 0)))
                                                 (list (cons (tree->string (tree-ref l 0)) l))
                                                 '()))
                               (find-tags body 'label)))
           (refs (append-map
                  (lambda (tag)
                    (append-map
                     (lambda (r) (if (and (>= (tree-arity r) 1)
                                          (tree-atomic? (tree-ref r 0)))
                                     (list (cons (tree->string (tree-ref r 0)) r))
                                     '()))
                     (find-tags body tag)))
                  reference-tags))
           (cites (append-map
                   (lambda (tag)
                     (append-map
                      (lambda (c)
                        (map (lambda (k) (cons k c))
                             (list-filter
                              (map tree->string* (tree-children c))
                              (lambda (k) (!= k "")))))
                      (find-tags body tag)))
                   citation-tags))
           (names (map car labels))
           (dups (list-filter (list-remove-duplicates names)
                              (lambda (n) (> (length (list-filter names (lambda (m) (== m n)))) 1)))))
      (obj "document" (doc-id d)
           "labels"
           (arr (map (lambda (e)
                       (obj "name" (u8 (car e))
                            "handle" (make-handle! d (block-of (cdr e)))
                            "section" (let ((s (section-of d (cdr e)))) (if s s jnull))))
                     labels))
           "references"
           (arr (map (lambda (e)
                       (obj "name" (u8 (car e))
                            "tag" (node-tag (cdr e))
                            "handle" (make-handle! d (block-of (cdr e)))
                            "defined" (bool (member (car e) names))))
                     refs))
           "citations"
           (arr (map (lambda (e)
                       (obj "key" (u8 (car e))
                            "handle" (make-handle! d (block-of (cdr e)))))
                     cites))
           "broken_references"
           (arr (list-remove-duplicates
                 (map (lambda (e) (u8 (car e)))
                      (list-filter refs (lambda (e) (not (member (car e) names)))))))
           "duplicate_labels" (arr (map u8 dups))))))

(define (list-remove-duplicates l)
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc))
          ((member (car l) acc) (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The table of tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prop type descr) (obj "type" type "description" descr))

(define document-prop
  (prop "string" "The document: the id given by list_documents or open_document (d1...), or the absolute path of a file (opened if it is not)."))

(define (schema props required)
  (obj "type" "object"
       "properties" (apply obj (apply append (map (lambda (p) (list (car p) (cdr p))) props)))
       "required" (arr required)))

(define mcp-tools
  (list
   (list "list_documents" "List documents"
         "The documents this session has open (with their ids), and the files that other TeXmacs processes of the user have open (those the person works on): they can be opened with open_document, which then shares them live when the person's TeXmacs allows it."
         (schema '() '())
         tool-list-documents)
   (list "open_document" "Open a document"
         "Opens a TeXmacs file (absolute path) and returns its id and basic facts: title, style, number of top-level blocks, content hash, and how it is shared. A file which the person has open in TeXmacs is joined live (if sharing of open files is on in both): reads then show the person's latest edits."
         (schema (list (cons "path" (prop "string" "Absolute path of the file."))) '("path"))
         tool-open-document)
   (list "get_outline" "Outline of a document"
         "The sections of a document with their level, title, labels, a handle, the index of the block and an estimate of the size in characters: a cheap map of a long document, to decide what to read."
         (schema (list (cons "document" document-prop)) '("document"))
         tool-get-outline)
   (list "read" "Read a document or a node"
         "Reads the whole document (no `node'), or the node of a handle, in a format: text (the visible text, cheapest), texmacs (the .tm syntax of a snippet, exact), stree (the Scheme form, exact), latex or markdown (converted, lossy for general documents). Long documents are cut at `max_size' characters between blocks: `next_start' tells where to continue (argument `start'). The result has handles for the blocks it shows, with a hash of their content and a preview."
         (schema (list (cons "document" document-prop)
                       (cons "node" (prop "string" "Handle of a node (n17), from a previous result. Default: the whole document."))
                       (cons "format" (prop "string" "text (default), texmacs, stree, latex or markdown."))
                       (cons "max_size" (prop "integer" "Longest content to return, in characters (default 12000)."))
                       (cons "start" (prop "integer" "Index of the first block to read, to continue a long document (default 0).")))
                 '("document"))
         tool-read)
   (list "find" "Find in a document"
         "Finds the blocks of a document: by text (case-insensitive, in the visible text), by `tag:theorem' (all nodes with that tag, e.g. equation, figure), by `label:name' (the block holding the label), or by `section:title' (sections whose title contains the text). Each match has a handle, its tag, the section it is in and a short context."
         (schema (list (cons "document" document-prop)
                       (cons "query" (prop "string" "Text, or tag:<name>, label:<name>, section:<title>."))
                       (cons "limit" (prop "integer" "Most matches to return (default 50).")))
                 '("document" "query"))
         tool-find)
   (list "get_references" "Labels, references and citations"
         "The labels (with the block that holds each), the references and the citations of a document, the references to labels which do not exist, and the labels defined twice."
         (schema (list (cons "document" document-prop)) '("document"))
         tool-get-references)))

(define (tool-name t) (car t))

(define (tools-list-result)
  (obj "tools"
       (arr (map (lambda (t)
                   (obj "name" (car t)
                        "title" (cadr t)
                        "description" (caddr t)
                        "inputSchema" (cadddr t)
                        "annotations" (obj "readOnlyHint" (bool #t)
                                           "destructiveHint" (bool #f)
                                           "idempotentHint" (bool #t)
                                           "openWorldHint" (bool #f))))
                 mcp-tools))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define supported-versions '("2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05"))
(define latest-version "2025-06-18")

(define server-instructions
  "TeXmacs documents. Start with list_documents or open_document, then get_outline to see the structure of a long document, then read (format text is the cheapest; texmacs is exact) the blocks you need, using the handles of earlier results. find locates text, tags (tag:theorem), labels and sections; get_references lists labels, references and citations and the broken ones. A file the person has open in TeXmacs is joined live, so reads show their latest edits. This server only reads.")

(define (result-message id result)
  (obj "jsonrpc" "2.0" "id" id "result" result))

(define (error-message id code message)
  (obj "jsonrpc" "2.0" "id" (if id id jnull)
       "error" (obj "code" (num code) "message" message)))

(define (send! msg)
  (let ((s (json-write msg)))
    (log-debug "-> " (utf8-prefix s 300))
    (mcp-stdout-write (string-append s "\n"))))

(define (text-result text error?)
  (obj "content" (arr (list (obj "type" "text" "text" text)))
       "isError" (bool error?)))

(define (call-tool name args)
  (let ((tool (list-find mcp-tools (lambda (t) (== (car t) name)))))
    (if (not tool)
        'unknown
        (catch #t
          (lambda ()
            (text-result (json-write ((list-ref tool 4) args)) #f))
          (lambda args*
            (if (and (pair? args*) (eq? (car args*) 'mcp-tool-error))
                (text-result mcp-tool-error-message #t)
                (begin
                  (mcp-log "error in tool " name ": " args*)
                  (text-result (string-append "internal error in " name) #t))))))))

(define (handle-request id method params)
  ;; the response to the request @method, an stree
  (cond
    ((== method "initialize")
     (let* ((v (jstring (jref params "protocolVersion")))
            (version (if (and v (member v supported-versions)) v latest-version)))
       (result-message
        id
        (obj "protocolVersion" version
             "capabilities" (obj "tools" (obj "listChanged" (bool #f)))
             "serverInfo" (obj "name" "texmacs"
                               "title" "TeXmacs"
                               "version" (texmacs-version))
             "instructions" server-instructions))))
    ((== method "ping") (result-message id (obj)))
    ((== method "tools/list") (result-message id (tools-list-result)))
    ((== method "tools/call")
     (let* ((name (jstring (jref params "name")))
            (args (let ((a (jref params "arguments"))) (if (func? a 'attr) a (obj))))
            (r (and name (call-tool name args))))
       (cond ((not name) (error-message id -32602 "missing tool name"))
             ((eq? r 'unknown)
              (error-message id -32602 (string-append "unknown tool: " name)))
             (else (result-message id r)))))
    ((or (== method "resources/list"))
     (result-message id (obj "resources" (arr '()))))
    ((== method "prompts/list")
     (result-message id (obj "prompts" (arr '()))))
    (else (error-message id -32601 (string-append "method not found: " method)))))

(tm-define (mcp-handle-line line)
  (:synopsis "Handle a message (a line of JSON) of the client, give the response")
  ;; (the response is an stree, or #f when there is none: a notification)
  (let ((msg (json-parse line)))
    (cond ((not msg) (error-message #f -32700 "parse error"))
          ((not (func? msg 'attr))
           (error-message #f -32600 "invalid request"))
          (else
            (let ((id (jref msg "id")) (method (jstring (jref msg "method"))))
              (cond ((not method)
                     ;; a response to a request of ours (we make none)
                     #f)
                    ((or (not id) (jnull? id))
                     (log-debug "notification " method)
                     #f)
                    (else
                      (log-debug "<- " method)
                      (catch #t
                        (lambda ()
                          (handle-request id method
                                          (let ((p (jref msg "params")))
                                            (if (func? p 'attr) p (obj)))))
                        (lambda args
                          (mcp-log "error in " method ": " args)
                          (error-message id -32603 "internal error"))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mcp-input "")
(define mcp-running? #f)
(define mcp-max-line 33554432)

(define (process-line! line)
  (let ((line (if (and (> (string-length line) 0)
                       (== (string-ref line (- (string-length line) 1)) #\return))
                  (substring line 0 (- (string-length line) 1))
                  line)))
    (when (!= line "")
      (let ((r (mcp-handle-line line)))
        (when r (send! r))))))

(define (process-input!)
  ;; the complete lines of the input are handled
  (let loop ()
    (let ((i (string-search-forwards "\n" 0 mcp-input)))
      (cond ((>= i 0)
             (let ((line (substring mcp-input 0 i)))
               (set! mcp-input (substring mcp-input (+ i 1)
                                          (string-length mcp-input)))
               (process-line! line)
               (loop)))
            ((> (string-length mcp-input) mcp-max-line)
             (mcp-log "a line is too long, dropped")
             (set! mcp-input "")
             (send! (error-message #f -32600 "message too long")))))))

(define (mcp-finish!)
  (mcp-log "end of input, exit")
  (for (d mcp-docs)
    (catch #t
      (lambda () (buffer-close (doc-url d)))
      (lambda args (noop))))
  (set! mcp-docs '())
  (set! mcp-running? #f)
  (quit-TeXmacs))

(define (mcp-tick)
  (catch #t
    (lambda ()
      (let ((s (mcp-stdin-read)))
        (when (!= s "")
          (set! mcp-input (string-append mcp-input s)))
        (process-input!)
        (cond ((and (mcp-stdin-eof?) (< (string-search-forwards "\n" 0 mcp-input) 0))
               (when (!= mcp-input "")
                 (let ((rest mcp-input))
                   (set! mcp-input "")
                   (process-line! rest)))
               (mcp-finish!)
               #t)
              (else 10))))
    (lambda args
      (mcp-log "error in the loop: " args)
      10)))

(tm-define (mcp-server-start)
  (:synopsis "Serve MCP on the standard input and output, until the input ends")
  (when (not mcp-running?)
    (set! mcp-running? #t)
    (mcp-log "started, " (texmacs-version))
    (exec-delayed-pause mcp-tick)))
