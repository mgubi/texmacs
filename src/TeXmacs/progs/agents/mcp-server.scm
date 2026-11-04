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

(define (json-parse s)
  (and (string? s) (!= s "") (mcp-json-valid? s)
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

(define (u8 s)
  ;; the Cork string @s in utf8; the newlines and tabs stay as they are
  ;; (cork->utf8 maps the bytes 10 and 9 to other characters)
  (define (has-ctl?)
    (let loop ((i 0))
      (and (< i (string-length s))
           (or (memv (string-ref s i) '(#\newline #\tab))
               (loop (+ i 1))))))
  (if (not (has-ctl?))
      (cork->utf8 s)
      (let loop ((i 0) (from 0) (acc '()))
        (define (seg) (cork->utf8 (substring s from i)))
        (cond ((>= i (string-length s))
               (apply string-append (reverse (cons (seg) acc))))
              ((memv (string-ref s i) '(#\newline #\tab))
               (loop (+ i 1) (+ i 1)
                     (cons (string (string-ref s i)) (cons (seg) acc))))
              (else (loop (+ i 1) from acc))))))

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

(define (first-text t)
  (if (> (tree-arity t) 0) (node-text (tree-ref t 0)) ""))

(define (last-text t)
  (if (> (tree-arity t) 0) (node-text (tree-ref t (- (tree-arity t) 1))) ""))

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
        ((tm-is? t 'with) (last-text t))
        ((tm-is? t 'table)
         (join-texts (map node-text (tree-children t)) "\n"))
        ((tm-is? t 'row)
         (join-texts (map node-text (tree-children t)) " | "))
        ((in? (tree-label t) '(reference eqref pageref))
         (string-append "(ref " (first-text t) ")"))
        ((in? (tree-label t) '(cite nocite cite-detail))
         (string-append "[" (join-texts (map node-text (tree-children t)) ", ")
                        "]"))
        ((in? (tree-label t) '(hlink href))
         (first-text t))
        ((in? (tree-label t) '(image postscript postscript*))
         "[image]")
        ((tm-is? t 'tformat) (last-text t))
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

(define (mcp-roots)
  ;; the folders the agent may open files in (TEXMACS_MCP_ROOTS, separated
  ;; by colons, canonical), or #f for no limit
  (let ((e (getenv "TEXMACS_MCP_ROOTS")))
    (and e (!= e "")
         (map (lambda (r)
                (let ((c (canonical-path r)))
                  (if (and (> (string-length c) 1)
                           (string-ends? c "/"))
                      (substring c 0 (- (string-length c) 1))
                      c)))
              (list-filter (string-tokenize-by-char e #\:)
                           (lambda (r) (!= r "")))))))

(define (path-allowed? p)
  ;; may the file with the canonical path @p be opened?
  (let ((roots (mcp-roots)))
    (or (not roots)
        (list-find roots
                   (lambda (r)
                     (or (== r "/") (== p r)
                         (string-starts? p (string-append r "/"))))))))

(define (path-url path)
  ;; the url of the file @path. (A colon separates the alternatives of a
  ;; path in a url of TeXmacs: a name that has one cannot be written as a
  ;; url, so that open-doc! refuses it. Wildcards in a name are plain.)
  (system->url path))

(define (dotdot-in-path? path)
  ;; is there a component `..' in @path?
  (let loop ((l (string-tokenize-by-char
                 (string-replace path "\\" "/") #\/)))
    (cond ((null? l) #f)
          ((== (car l) "..") #t)
          (else (loop (cdr l))))))

(define (colon-in-path? path)
  ;; (after the letter of a drive, as in C:\dir)
  (let ((rest (if (and (> (string-length path) 1) (== (string-ref path 1) #\:))
                  (substring path 2 (string-length path))
                  path)))
    (>= (string-search-forwards ":" 0 rest) 0)))

(define (find-buffer u)
  ;; the buffer with the url @u. (buffer-exists? takes a name with a
  ;; wildcard, as a*b.tm, for a pattern.)
  (let ((name (url->string u)))
    (list-find (buffer-list) (lambda (b) (== (url->string b) name)))))

(define (open-doc! path)
  ;; The file that is checked is the file that is loaded: the canonical path
  ;; (symbolic links resolved) of a file which exists; a path with a
  ;; component `..' is refused (it would be resolved, in a url, by
  ;; names that do not exist).
  (let* ((old (find-doc path)))
    (cond (old old)
          ((dotdot-in-path? path)
           (tool-error "a path with a component `..' is not accepted"))
          ((colon-in-path? path)
           (tool-error "a path with a colon (:) cannot be opened: TeXmacs urls use it as a separator"))
          (else
            (let ((p (shared-real-path path)))
              (cond ((or (not (string? p)) (== p ""))
                     (tool-error (string-append "no such file: " path)))
                    ((not (path-allowed? p))
                     (tool-error (string-append "not allowed: " path
                                                " is outside the folders of this server")))
                    ((colon-in-path? p)
                     (tool-error "a path with a colon (:) cannot be opened: TeXmacs urls use it as a separator"))
                    (else
                      (let* ((u0 (path-url p))
                             (bu (or (find-buffer u0)
                                     (begin (load-buffer u0) (find-buffer u0)))))
                        (when (not bu)
                          (tool-error (string-append "could not open " path)))
                        (set! mcp-doc-count (+ mcp-doc-count 1))
                        (let ((d (list (string-append "d" (number->string mcp-doc-count))
                                       bu p)))
                          (set! mcp-docs (append mcp-docs (list d)))
                          (snapshot! d)
                          d)))))))))

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
         "file" (buffer-get-title u)
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

(define (find-section t)
  ;; the section in the block @t: the block itself, or the first one in a
  ;; concatenation or nested document (a section followed by its label is
  ;; a concatenation)
  (cond ((tree-atomic? t) #f)
        ((section-level t) t)
        ((or (tm-is? t 'concat) (tm-is? t 'document))
         (let loop ((l (tree-children t)))
           (cond ((null? l) #f)
                 ((find-section (car l)))
                 (else (loop (cdr l))))))
        (else #f)))

(define (outline-entries d)
  ;; ((index level node block) ...) of the sections in the body
  (let loop ((i 0) (l (tree-children (doc-body d))) (acc '()))
    (cond ((null? l) (reverse acc))
          ((find-section (car l))
           => (lambda (sec)
                (loop (+ i 1) (cdr l)
                      (cons (list i (section-level sec) sec (car l)) acc))))
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
           (convert st "texmacs-stree" "markdown-snippet"))
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
                     (obj "path" (car e)
                          "person" (bool (cadr e))
                          "pid" (num (caddr e))))
                   (list-filter (shared-files-in-use)
                                (lambda (e)
                                  (and (path-allowed? (car e))
                                       (not (find-doc (car e)))))))))))

(define (tool-open-document args)
  (let ((path (opt-string args "path" #f)))
    (when (not path) (tool-error "the argument `path' is needed"))
    (when (not (absolute-path? path))
      (tool-error "the path must be absolute"))
    (let ((d (open-doc! path)))
      (doc-refresh! d)
      (doc-info d))))

(define default-list-limit 300)

(define (opt-int args key default lo hi)
  ;; an integer argument, between lo and hi, or the default
  (let ((v (jref args key)))
    (cond ((or (not v) (jnull? v)) default)
          ((jint v) (min hi (max lo (jint v))))
          (else (param-error (string-append "the argument `" key
                                            "' must be an integer"))))))

(define (opt-string args key default)
  (let ((v (jref args key)))
    (cond ((or (not v) (jnull? v)) default)
          ((string? v) v)
          (else (param-error (string-append "the argument `" key
                                            "' must be a string"))))))

(define (tool-get-outline args)
  (let* ((d (require-doc (opt-string args "document" #f)))
         (limit (opt-int args "limit" default-list-limit 1 2000))
         (start (opt-int args "start" 0 0 1000000000)))
    (doc-refresh! d)
    (let* ((entries (outline-entries d)) (body (doc-body d))
           (total (length entries))
           (start (min start total))
           (end (min total (+ start limit))))
      (obj "document" (doc-id d)
           "title" (let ((t (title-of-document d))) (if t t jnull))
           "hash" (hash-short body)
           "blocks" (num (tree-arity body))
           "total" (num total)
           "start" (num start)
           "next_start" (if (< end total) (num end) jnull)
           "sections"
           (arr (let loop ((l (list-tail entries start)) (k start) (acc '()))
                  (if (or (null? l) (>= k end))
                      (reverse acc)
                      (let* ((e (car l)) (t (caddr e))
                             (next (if (null? (cdr l))
                                       (tree-arity body)
                                       (car (cadr l))))
                             (chars (apply +
                                      (map estimate-size
                                           (sublist (tree-children body)
                                                    (car e) next)))))
                        (loop (cdr l) (+ k 1)
                              (cons (obj "handle" (make-handle! d t)
                                         "tag" (node-tag t)
                                         "level" (num (cadr e))
                                         "title" (ntext t)
                                         "labels" (arr (tree-labels (cadddr e)))
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
  (let* ((d (require-doc (opt-string args "document" #f)))
         (fmt (opt-string args "format" "text"))
         (max-size (opt-int args "max_size" default-max-size 200 400000))
         (start (opt-int args "start" 0 0 1000000000))
         (h (opt-string args "node" #f)))
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
  (let* ((d (require-doc (opt-string args "document" #f)))
         (query (opt-string args "query" #f))
         (limit (opt-int args "limit" find-limit 1 500)))
    (when (or (not query) (== query ""))
      (tool-error "the argument `query' is needed"))
    (when (member query '("tag:" "label:" "section:"))
      (tool-error (string-append "nothing after `" query "'")))
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
  (let* ((d (require-doc (opt-string args "document" #f)))
         (limit (opt-int args "limit" 200 1 2000))
         (start (opt-int args "start" 0 0 1000000000)))
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
           (counts (make-ahash-table))
           (known (for (e labels)
                    (ahash-set! counts (car e)
                                (+ 1 (or (ahash-ref counts (car e)) 0)))))
           (defined? (lambda (n) (and (ahash-ref counts n) #t)))
           (dups (list-remove-duplicates
                  (map car (list-filter labels
                                        (lambda (e) (> (ahash-ref counts (car e)) 1))))))
           (broken (list-remove-duplicates
                    (map car (list-filter refs (lambda (e) (not (defined? (car e))))))))
           (page (lambda (l)
                   (if (>= start (length l)) '()
                       (sublist l start (min (length l) (+ start limit))))))
           (more? (lambda (l) (> (length l) (+ start limit)))))
      (obj "document" (doc-id d)
           "start" (num start)
           "limit" (num limit)
           "truncated" (bool (or (more? labels) (more? refs) (more? cites)
                                 (> (length broken) limit) (> (length dups) limit)))
           "labels_total" (num (length labels))
           "references_total" (num (length refs))
           "citations_total" (num (length cites))
           "labels"
           (arr (map (lambda (e)
                       (obj "name" (u8 (car e))
                            "handle" (make-handle! d (block-of (cdr e)))
                            "section" (let ((s (section-of d (cdr e)))) (if s s jnull))))
                     (page labels)))
           "references"
           (arr (map (lambda (e)
                       (obj "name" (u8 (car e))
                            "tag" (node-tag (cdr e))
                            "handle" (make-handle! d (block-of (cdr e)))
                            "defined" (bool (defined? (car e)))))
                     (page refs)))
           "citations"
           (arr (map (lambda (e)
                       (obj "key" (u8 (car e))
                            "handle" (make-handle! d (block-of (cdr e)))))
                     (page cites)))
           "broken_references" (arr (map u8 (sublist broken 0 (min limit (length broken)))))
           "duplicate_labels" (arr (map u8 (sublist dups 0 (min limit (length dups)))))))))

(define (list-remove-duplicates l)
  (let ((seen (make-ahash-table)))
    (let loop ((l l) (acc '()))
      (cond ((null? l) (reverse acc))
            ((ahash-ref seen (car l)) (loop (cdr l) acc))
            (else (ahash-set! seen (car l) #t)
                  (loop (cdr l) (cons (car l) acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing (step 3): replace, insert, delete, edit_text, undo_agent, save,
;; close, create
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A tool call that edits is one step: the edit is checked first (the node, the
;; expected hash, the content), then done between mark-start and mark-end by
;; the editor of the buffer, so that the modifications are of its author, one
;; undo step, and, in a shared file, patches of this process (an agent) to the
;; host. A failure in the middle is cancelled (mark-cancel). The host may
;; refuse the patch (the document it was made for has changed too much): the
;; tool waits for its answer and says `refused'. The agent undoes its own steps
;; with undo_agent: each step keeps what is needed to undo it (the nodes it
;; made, with their hashes, and the old content), and the undo is refused when
;; someone changed these nodes meanwhile. (The undo of the editor skips the
;; changes of the others only when they commute, and undoes them when they do
;; not: it is not used for the agent.)

(define (mcp-read-only?)
  (let ((e (getenv "TEXMACS_MCP_READONLY")))
    (and e (!= e "") (!= e "0"))))

(define (require-writable)
  (when (mcp-read-only?)
    (tool-error "this server is read-only (TEXMACS_MCP_READONLY is set): it does not edit, save, create or close documents")))

;;; The content

(define edit-formats '("texmacs" "stree" "latex" "markdown"))
(define max-content-size 4194304)

(define (valid-stree? x depth)
  (cond ((string? x) #t)
        ((> depth 200) #f)
        ((and (pair? x) (list? x) (symbol? (car x)))
         (let loop ((l (cdr x)))
           (or (null? l)
               (and (valid-stree? (car l) (+ depth 1)) (loop (cdr l))))))
        (else #f)))

(define (stree-cork t)
  ;; the stree @t with its strings from utf8 to Cork
  (cond ((string? t) (utf8->cork t))
        ((pair? t) (cons (car t) (map stree-cork (cdr t))))
        (else t)))

(define (utf8-escape s)
  ;; the utf8 string @s written in the syntax of a .tm snippet: a character
  ;; beyond ASCII becomes \<#xxxx\> (utf8->cork would also turn a `<' of
  ;; the markup into a character)
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()))
      (if (>= i n)
          (apply string-append (reverse acc))
          (let ((b (char->integer (string-ref s i))))
            (if (< b 128)
                (loop (+ i 1) (cons (string (string-ref s i)) acc))
                (let* ((len (cond ((>= b 240) 4) ((>= b 224) 3) ((>= b 192) 2) (else 1)))
                       (end (min n (+ i len)))
                       (code (utf8-code (substring s i end))))
                  (loop end (cons (string-append "\\<#" (hex-digits code) "\\>") acc)))))))))

(define (brackets-balanced? s open close)
  ;; the characters @open and @close match in @s (those after a backslash
  ;; are plain)
  (let ((n (string-length s)))
    (let loop ((i 0) (depth 0))
      (cond ((>= i n) (== depth 0))
            ((char=? (string-ref s i) #\\) (loop (+ i 2) depth))
            ((char=? (string-ref s i) open) (loop (+ i 1) (+ depth 1)))
            ((char=? (string-ref s i) close)
             (and (> depth 0) (loop (+ i 1) (- depth 1))))
            (else (loop (+ i 1) depth))))))

;;; Collecting violations: the rules of the filter call violate!, which refuses
;;; (as tool-error) or, when collecting, keeps the reason: the filter of a
;;; document compares the violations before and after an edit.

(define collecting? #f)
(define collected '())

(define (violate! msg)
  (if collecting?
      (set! collected (cons msg collected))
      (tool-error msg)))

(define (collect-violations thunk)
  ;; the reasons (strings) of the violations of the rules run by @thunk
  (let ((old-c collecting?) (old-l collected))
    (set! collecting? #t)
    (set! collected '())
    (catch #t thunk (lambda args (set! collected (cons "the check failed" collected))))
    (let ((r (reverse collected)))
      (set! collecting? old-c)
      (set! collected old-l)
      r)))

;;; What content is refused (design 5.6: no evaluation of code, nothing but the
;;; documents is reached). Every primitive of the DRD (src/Data/Drd/drd_std.cpp)
;;; is classified by its real name: refused as code or as a definition of the
;;; whole document, refused as a loop or a resource, with a file argument, with
;;; a link destination, or safe (all the others). The table is in
;;; docs/mcp-server.md; the lists below are its source, and the suite checks
;;; that every name in them is a tag of the DRD (mcp-filter-unknown-names).

(define drd-primitives
  '(
    string unknown uninit error raw-data document para surround concat rigid
    hgroup hidden freeze unfreeze hspace vspace* vspace space htab move shift
    resize clipped repeat repeat* float datoms dlines dpages dbox line-note
    page-note if-page-break with-limits line-break new-line next-line
    no-break no-indent yes-indent no-indent* yes-indent* page-break*
    page-break no-page-break* no-page-break no-break-here* no-break-here
    no-break-start no-break-end new-page* new-page new-dpage* new-dpage
    around around* big-around left mid right big long-arrow lprime rprime
    below above lsub lsup rsub rsup frac sqrt wide wide* neg tree syntax
    tformat twith cwith tmarker table row cell subtable assign provide with
    provides value quote-value or-value macro drd-props quote-arg arg
    compound xmacro get-label get-arity map-args eval-args new-theme
    copy-theme apply-theme select-theme mark mark* expand-as eval quote quasi
    quasiquote unquote unquote* copy if if* case dynamic-case while for-each
    extern include* include with-package use-package use-module or xor and
    not plus minus times over div mod minimum maximum math-sqrt exp log pow
    cos sin tan merge length range number date translate change-case
    find-file find-file-upwards is-tuple look-up occurs-inside equal unequal
    less lesseq greater greatereq blend rgb-color rgb-access cm-length
    mm-length in-length pt-length bp-length dd-length pc-length cc-length
    fs-length fbs-length em-length ln-length sep-length yfrac-length
    ex-length fn-length fns-length bls-length fnbot-length fntop-length
    spc-length xspc-length par-length paw-length pag-length gw-length
    gh-length gu-length tmpt-length px-length guipx-length lcorner-length
    bcorner-length rcorner-length tcorner-length ms-length s-length
    msec-length sec-length min-length hr-length filter-style style-with
    style-with* style-only style-only* active active* inactive inactive*
    rewrite-inactive inline-tag open-tag middle-tag close-tag symbol latex
    hybrid locus id hard-id link url script observer find-accessible hlink
    action relay set-binding get-binding has-binding hidden-binding label
    reference pageref get-attachment write toc-notify auto-id cache-ref tuple
    attr tmlen collection associate backup pattern gradient specific flag
    hyphenate-as anim-static anim-dynamic morph anim-time anim-portion
    anim-compose anim-repeat anim-constant anim-accelerate anim-translate
    anim-progressive video sound graphics superpose gr-group gr-transform
    gr-effect text-at math-at document-at point line cline arc carc spline
    spline* cspline bezier cbezier smooth csmooth penscript calligraphy fill
    image box-info frame-direct frame-inverse is-equal is-intersection
    on-curve on-text-border on-grid transform-3d object-3d triangle-3d
    light-3d light-diffuse light-specular eff-move eff-magnify eff-bubble
    eff-crop eff-turbulence eff-fractal-noise eff-hatch eff-dots eff-gaussian
    eff-oval eff-rectangular eff-motion eff-blur eff-outline eff-thicken
    eff-erode eff-degrade eff-distort eff-gnaw eff-superpose eff-add eff-sub
    eff-mul eff-min eff-max eff-mix eff-normalize eff-monochrome
    eff-color-matrix eff-gradient eff-make-transparent eff-make-opaque
    eff-recolor eff-skin canvas ornament art-box format line-sep split delay
    hold release old-matrix old-table old-mosaic old-mosaic-item set reset
    expand expand* hide-expand apply begin end func env authorize postscript ))

(define code-reasons
  '((extern . "can run a command") (hybrid . "calls a function by its name")
    (authorize . "authorizes the evaluation of code") (observer . "calls a function")
    (action . "runs an action when clicked") (script . "can run code")
    (eval . "evaluates code") (eval-args . "evaluates code")
    (quasi . "evaluates code") (quasiquote . "evaluates code")
    (unquote . "evaluates code") (unquote* . "evaluates code")
    (compound . "calls a macro by its name") (apply . "calls a macro by its name")
    (expand . "calls a macro by its name") (expand* . "calls a macro by its name")
    (hide-expand . "calls a macro by its name") (begin . "calls a macro by its name")
    (end . "calls a macro by its name") (map-args . "evaluates the arguments of a macro")
    (assign . "changes the definitions of the whole document")
    (set . "changes the environment of the whole document")
    (reset . "changes the environment of the whole document")
    (provide . "changes the definitions of the whole document")
    (macro . "defines a macro (a macro can call itself without end)")
    (xmacro . "defines a macro") (func . "defines a function")
    (drd-props . "changes the properties of the tags")
    (with-package . "loads a package") (use-package . "loads a package")
    (use-module . "loads a module")
    (new-theme . "changes the themes") (copy-theme . "changes the themes")
    (apply-theme . "changes the themes") (select-theme . "changes the themes")
    (style-with . "changes the style") (style-with* . "changes the style")
    (style-only . "changes the style") (style-only* . "changes the style")
    (filter-style . "changes the style")
    (raw-data . "is binary data")
    (find-file . "looks for files") (find-file-upwards . "looks for files")
    (uninit . "is an internal marker") (unknown . "is an internal marker")
    (while . "is a loop without a limit (it can use all the memory)")
    (for-each . "is a loop (it can use all the memory)")
    (range . "makes a list of any size")))

;; tags that are not primitives (macros of the standard styles): code of
;; sessions and of scripts
(define code-macro-reasons
  '((session . "is a session that runs code") (input . "is the input of a session that runs code")
    (output . "is the output of a session") (script-input . "can run code")
    (script-output . "is the output of code") (script-eval . "can run code")))

(define (suffix-reason tag)
  ;; plugin inputs and outputs: scheme-input, shell-output...
  (let ((s (symbol->string tag)))
    (cond ((string-ends? s "-input") "is the input of a session that runs code")
          ((string-ends? s "-output") "is the output of a session")
          (else #f))))

;; (tag . position of the first file in the arguments); the macros have no
;; entry in the DRD of the primitives; for `bibliography' all the arguments
;; are looked at (the importer of LaTeX gives it the files alone)
(define file-primitives
  '((image . 0) (postscript . 0) (include . 0) (include* . 0) (video . 0)
    (sound . 0) (pattern . 0)))
(define file-macros
  '((tmdoc-include . 0) (include-section . 0) (attachment . 0) (ocr . 0)
    (bibliography . 0)))

;; (tag . position of the destination; last = the last argument)
(define link-primitives '((hlink . 1) (link . last) (url . last)))
(define link-macros '((href . 0) (slink . 0)))

(define web-file-tags '(image postscript pattern))

;; Macros of the standard styles which run commands, read files or act on
;; click through their arguments (the bodies in TeXmacs/styles and packages
;; use action, find-file, include*, script, observer, hybrid, authorize, or
;; extern with a function that reads files or runs programs). They are refused
;; although the style defines them. Found by scanning every <assign|...|<macro|
;; of the .ts files (the script is described in docs/mcp-server.md).
(define refused-style-macros
  '(
    block-comment button calc-check carbon-comment chunk-link dir-entry-icon
    dir-header-cell doc-module-synopsis doxygen-get doxygen-load doxygen-ref
    edit-macro folded-comment icon inline-comment link-to-module-doc
    link-to-module-source live-io live-io* mc-popup mirror mirror-comment
    radio-box radio-button remote-big-icon remote-email remote-menu
    remote-name remote-picture screenshot shared spell-error
    spell-propositions tmdoc-file tmdoc-image tmdoc-translation tmdoc-web
    tmweb tmweb-link toggle-box toggle-button widget-cmd widget-value ))

;; The tags whose string arguments are names: they are written as they are
;; (labels, keys, addresses, files, environment variables) in the exports to
;; LaTeX and HTML, so that a `}' or a `\\' in them is not text.
(define name-tags
  '(label reference pageref eqref cite nocite cite-detail bibitem smart-ref ref
    include include* image postscript postscript* pattern video sound
    tmdoc-include include-section attachment ocr bibliography))

(define (filter-lists)
  ;; for the tests: the names of all the lists (symbols)
  (append (map car code-reasons) (map car file-primitives) (map car link-primitives)
          '(specific latex)))

(tm-define (mcp-filter-unknown-names)
  (:synopsis "The names of the lists of refused content which are not tags of the DRD")
  (list-filter (filter-lists)
               (lambda (n) (not (known-primitive? n)))))

(define (known-primitive? tag) (and (memq tag drd-primitives) #t))

(define (check-arity st d)
  ;; refuse a tree in which a tag known to the DRD has a number of arguments
  ;; it cannot have: a primitive (a `with' with none crashes the typesetter),
  ;; or a macro of the style, when the DRD knows its arity (a macro the
  ;; document assigns itself is not known before it is typeset)
  (cond ((string? st) #t)
        ((pair? st)
         (let ((n (length (cdr st))) (tag (car st)))
           (when (and (not (== tag 'document))
                      (or (known-primitive? tag) (known-macro-arity? tag))
                      (not (tag-possible-arity? tag n)))
             (violate! (string-append "the tag `" (symbol->string tag)
                                        "' cannot have " (number->string n)
                                        " argument(s) (nothing was changed)")))
           (for (c (cdr st)) (check-arity c d))))
        (else #t)))

(define (known-macro-arity? tag)
  ;; does the DRD give a macro an arity (it is not the default of the tags
  ;; it does not know, which have only the arity 0)? Then it is the macro's.
  (let loop ((n 1))
    (and (<= n 16)
         (or (tag-possible-arity? tag n) (loop (+ n 1))))))

(define (document-folder base)
  (let ((r (shared-real-path base)))
    (if (and (string? r) (!= r "")) r base)))

(define (inside-folder? p folder)
  (or (== p folder)
      (string-starts? p (string-append (if (string-ends? folder "/")
                                           (substring folder 0 (- (string-length folder) 1))
                                           folder) "/"))))

(define (scheme-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\+ #\- #\.))))

(define (url-scheme s)
  ;; the scheme of an address (letters, digits, + - . before a colon, with
  ;; nothing like / ? # before it), in lowercase, or #f. Blanks at the start
  ;; are skipped (a browser does).
  (let* ((n (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i n) (memv (string-ref s i) '(#\space #\tab #\newline)))
                      (loop (+ i 1)) i))))
    (let loop ((i start))
      (cond ((>= i n) #f)
            ((char=? (string-ref s i) #\:)
             (and (> i start) (char-alphabetic? (string-ref s start))
                  (locase-all (substring s start i))))
            ((scheme-char? (string-ref s i)) (loop (+ i 1)))
            (else #f)))))

(define (percent-dots? s)
  ;; %2e or %2f (a dot or a slash written in hexadecimal)
  (let ((l (locase-all s)))
    (or (>= (string-search-forwards "%2e" 0 l) 0)
        (>= (string-search-forwards "%2f" 0 l) 0)
        (>= (string-search-forwards "%5c" 0 l) 0))))

(define (resolve-existing path)
  ;; the canonical form of @path, also when the end of it does not exist: the
  ;; longest part that exists is resolved (a link to another folder leads
  ;; there), and the rest is added
  (let loop ((p path) (rest ""))
    (let ((real (shared-real-path p)))
      (if (and (string? real) (!= real ""))
          (string-append real rest)
          (let ((i (let scan ((k (- (string-length p) 1)))
                     (cond ((<= k 0) #f) ((char=? (string-ref p k) #\/) k) (else (scan (- k 1)))))))
            (if i
                (loop (substring p 0 i) (string-append (substring p i (string-length p)) rest))
                (string-append p rest)))))))

(define (file-name-ok? name base web?)
  ;; may the document refer to the file @name? A relative name, without `..',
  ;; that stays inside the folders of the server (the roots), or, with no
  ;; roots, inside the folder of the document; with @web?, an address
  ;; http: or https: is good too
  (let ((scheme (url-scheme name)))
    (cond ((and scheme web? (member scheme '("http" "https"))) #t)
          (scheme #f)
          ((== name "") #t)
          ((or (string-starts? name "/") (string-starts? name "\\")
               (string-starts? name "~") (colon-in-path? name)
               (dotdot-in-path? name) (percent-dots? name))
           #f)
          (else
            (let* ((folder (document-folder base))
                   (joined (string-append folder "/" name))
                   (p (resolve-existing joined)))
              (if (mcp-roots)
                  (path-allowed? p)
                  (inside-folder? p folder)))))))

(define (link-target-ok? s)
  ;; a destination of a link: http, https, mailto, or a relative address
  ;; (a path, a #label, other.tm#label); not a network path (//host)
  (let ((scheme (url-scheme s)))
    (cond (scheme (and (member scheme '("http" "https" "mailto")) #t))
          ((or (string-starts? (tm-string-trim-both s) "//")
               (string-starts? (tm-string-trim-both s) "\\"))
           #f)
          (else #t))))

(define (name-chars-ok? s kind)
  (and (name-chars-ok-1? s kind)
       (or (not (== kind 'address)) (not (entity-in? s)))))

(define (name-chars-ok-1? s kind)
  ;; may the string @s be written in an export as a name? No `\\', `{', `}', no
  ;; control character, no `^^'; for a label or a key also no % # $ & ^ ~; for
  ;; a file also no %, #; for an address a `%' only before two hexadecimal
  ;; digits and a `#' (a fragment); `&', `$', `~' and a single `^' are fine in
  ;; an address (queries, paths of users), but not `^^' 
  (let ((n (string-length s)))
    (and (< (string-length s) 4096)
         (let loop ((i 0))
           (or (>= i n)
               (let ((c (string-ref s i)))
                 (and (>= (char->integer c) 32) (not (== (char->integer c) 127))
                      (not (memv c '(#\\ #\{ #\} #\" #\' #\< #\>)))
                      (or (not (== kind 'address)) (not (char=? c #\space)))
                      (cond ((== kind 'label) (not (memv c '(#\% #\# #\$ #\& #\^ #\~))))
                            ((== kind 'file) (not (memv c '(#\% #\# #\$ #\& #\^ #\~))))
                            (else
                              (and (or (not (char=? c #\^))
                                       (not (and (< (+ i 1) n) (char=? (string-ref s (+ i 1)) #\^))))
                                   (or (not (char=? c #\%))
                                       (and (<= (+ i 3) n)
                                            (char-hexdigit? (string-ref s (+ i 1)))
                                            (char-hexdigit? (string-ref s (+ i 2))))))))
                      (loop (+ i 1)))))))))

(define (char-hexdigit? c)
  (or (char-numeric? c) (and (char-alphabetic? c) (memv (char-downcase c) '(#\a #\b #\c #\d #\e #\f)))))

(define (entity-in? s)
  ;; an HTML entity (&#106; &colon;): a browser reads it in an address
  (let ((n (string-length s)))
    (let loop ((i 0))
      (and (< i n)
           (or (and (char=? (string-ref s i) #\&)
                    (let scan ((j (+ i 1)))
                      (cond ((>= j n) #f)
                            ((char=? (string-ref s j) #\;) (> j (+ i 1)))
                            ((or (char-alphabetic? (string-ref s j)) (char-numeric? (string-ref s j))
                                 (char=? (string-ref s j) #\#))
                             (scan (+ j 1)))
                            (else #f))))
               (loop (+ i 1)))))))

(define (name-kind tag)
  (cond ((memq tag '(href slink hlink url link)) 'address)
        ((memq tag '(include include* image postscript postscript* pattern video sound
                     tmdoc-include include-section attachment ocr bibliography))
         'file)
        (else 'label)))

(define (refuse-tag tag why)
  (violate! (string-append "the tag `" (symbol->string tag) "' " why
                             ": the content is refused (nothing was changed)")))

(define (argument st k)
  ;; the argument number k of the tree (last: the last one), or #f
  (let ((args (cdr st)))
    (cond ((null? args) #f)
          ((eq? k 'last) (car (reverse args)))
          ((< k (length args)) (list-ref args k))
          (else #f))))

(define (check-file-argument tag a base web?)
  (cond ((string? a)
         (when (not (and (file-name-ok? a base web?)
                         (name-chars-ok? a (if (url-scheme a) 'address 'file))))
           (violate! (string-append "the tag `" (symbol->string tag) "' refers to the file `"
                                      (utf8-prefix (u8 a) 60)
                                      "': only relative names without .. inside the folders of the server (or of the document), and http(s) images, are accepted (nothing was changed)"))))
        (else
          (violate! (string-append "the tag `" (symbol->string tag)
                                     "' has a file that is not written as a plain name: refused (nothing was changed)")))))

(define safety-defined? #f)

(define macro-by-name-tags
  '(map enum-tag group-individual-counters inc-counter item-tag marginal-note
    next-counter ornament-render-shadow render-box render-button render-marker
    reset-counter rsection* value-counter gpg-passphrase-decrypted-block))

(define (dangerous-name? name)
  ;; the name of a tag that the content may not use
  (let ((tag (string->symbol name)))
    (or (assq tag code-reasons) (assq tag code-macro-reasons) (suffix-reason tag)
        (assq tag file-primitives) (assq tag file-macros)
        (assq tag link-primitives) (assq tag link-macros)
        (memq tag refused-style-macros) (memq tag name-tags)
        (memq tag '(specific latex)))))

(define (tag-name? name)
  ;; does the string name a tag or a macro (not a plain variable of the
  ;; environment)? `<value|href>' or `<with|em|...>' would make an alias of it
  (let ((tag (string->symbol name)))
    (or (known-primitive? tag) (memq tag refused-style-macros)
        (memq tag name-tags) (assq tag link-macros) (assq tag file-macros)
        (assq tag code-macro-reasons) (known-macro-arity? tag)
        )))

(define value-chars-extra '(#\space #\. #\_ #\- #\+ #\* #\/ #\% #\# #\, #\: #\= #\@))

(define (value-chars-ok? v)
  ;; a value of the environment: letters, digits, blanks and . _ - + * / % # , : =
  ;; (no quote, bracket, backslash, brace or control character)
  (let ((n (string-length v)))
    (and (< n 200)
         (let loop ((i 0))
           (or (>= i n)
               (let ((c (string-ref v i)))
                 (and (or (char-alphabetic? c) (char-numeric? c) (>= (char->integer c) 128)
                          (memv c value-chars-extra))
                      (loop (+ i 1)))))))))

(define (hex-color? v)
  (and (>= (string-length v) 4) (<= (string-length v) 9) (char=? (string-ref v 0) #\#)
       (let loop ((i 1))
         (or (>= i (string-length v))
             (and (char-hexdigit? (string-ref v i)) (loop (+ i 1)))))))

(define (number-string? v)
  (let ((n (string-length v)))
    (and (> n 0)
         (let loop ((i 0) (dot? #f))
           (or (>= i n)
               (let ((c (string-ref v i)))
                 (cond ((char-numeric? c) (loop (+ i 1) dot?))
                       ((and (char=? c #\.) (not dot?)) (loop (+ i 1) #t))
                       (else #f))))))))

(define (env-value-ok? var v)
  ;; may the variable @var of the environment be given the string @v? What is
  ;; written in an export (a colour in an HTML attribute, a size) is typed
  (cond ((not (value-chars-ok? v)) #f)
        ((or (== var "color") (string-ends? var "-color"))
         (or (hex-color? v)
             (let loop ((i 0))
               (or (>= i (string-length v))
                   (and (or (char-alphabetic? (string-ref v i)) (char-numeric? (string-ref v i))
                            (memv (string-ref v i) '(#\space #\-)))
                        (loop (+ i 1)))))))
        ((member var '("font-size" "font-base-size" "magnification" "dpi"))
         (number-string? v))
        (else #t)))

(define (check-env-pairs tag vars-and-values)
  ;; (var value var value ...) of a with, cwith or twith
  (let loop ((l vars-and-values))
    (when (and (pair? l) (pair? (cdr l)))
      (let ((var (car l)) (val (cadr l)))
        (when (string? var)
          (when (and (not (string? val)) (pair? val)
                     (or (member var '("font-size" "font-base-size" "magnification" "dpi"))
                         (and (or (== var "color") (string-ends? var "-color"))
                              (not (memq (car val) '(pattern gradient rgb-color blend tuple quote))))))
            (violate! (string-append "a `" (symbol->string tag) "' sets `" (utf8-prefix (u8 var) 40)
                                     "' to something that is not plain text: refused (nothing was changed)")))
          (when (tag-name? var)
            (violate! (string-append "a `" (symbol->string tag) "' that sets `" (utf8-prefix (u8 var) 40)
                                       "' (the name of a tag or of a macro) is refused: it would make an alias of the tag (nothing was changed)")))
          (when (and (string? val) (not (env-value-ok? var val)))
            (violate! (string-append "a `" (symbol->string tag) "' sets `" (utf8-prefix (u8 var) 40)
                                       "' to a value that is not accepted: `" (utf8-prefix (u8 val) 40)
                                       "' (letters, digits, blanks and . _ - + * / % # , : = only; a colour is #hex or a name, a size a number) (nothing was changed)")))))
      (loop (cddr l)))))

(define (check-safety st base)
  ;; refuse content with markup that can run code, loop, or read what it
  ;; should not
  (cond
    ((string? st) #t)
    ((pair? st)
     (let ((tag (car st)))
       ;; (the arguments first: what is inside is named in the reason)
       (for (c (cdr st)) (check-safety c base))
       ;; the tags that are not primitives: only those the style of the
       ;; document (or the document itself, by assign) defines, and not the
       ;; ones that run commands: the rest would be written as raw macros
       ;; in an export to LaTeX (\\verbatiminput, \\write18...)
       (when (and (not (known-primitive? tag)) (not (assq tag code-reasons))
                  (not (assq tag code-macro-reasons)) (not (suffix-reason tag)))
         (cond ((memq tag refused-style-macros)
                (refuse-tag tag "is a macro of the styles that runs commands, reads files or acts on a click"))
               ((not (and safety-defined? (safety-defined? tag)))
                (violate! (string-append "unknown tag `" (symbol->string tag)
                                           "' (not defined by the style of the document): the content is refused (nothing was changed)")))))
       ;; (aliases, and what is written into the exports as attributes)
       (cond ((eq? tag 'with)
              (when (pair? (cdr st))
                (check-env-pairs tag (list-head (cdr st) (- (length (cdr st)) 1)))))
             ((eq? tag 'cwith)
              (when (>= (length (cdr st)) 6)
                (check-env-pairs tag (list-tail (cdr st) 4))))
             ((eq? tag 'twith)
              (check-env-pairs tag (cdr st)))
             ((memq tag '(value quote-value or-value))
              (when (and (pair? (cdr st)) (string? (cadr st)) (tag-name? (cadr st)))
                (violate! (string-append "a `" (symbol->string tag) "' of `" (utf8-prefix (u8 (cadr st)) 40)
                                           "' (the name of a tag or of a macro) is refused: it would make an alias of the tag (nothing was changed)")))))
       ;; macros which call the macro named by an argument
       (when (memq tag macro-by-name-tags)
         (for (a (cdr st))
           (when (and (string? a) (dangerous-name? a))
             (violate! (string-append "a `" (symbol->string tag) "' that calls `" (utf8-prefix (u8 a) 40)
                                        "' is refused: that macro is not accepted in the content (nothing was changed)")))))
       (when (memq tag name-tags)
         (for (a (cdr st))
           (when (and (string? a) (not (name-chars-ok? a (name-kind tag))))
             (violate! (string-append "a string in a `" (symbol->string tag) "' has a character that cannot be in a name (a backslash, a brace, a control character, % # $ & ^ ~ ...): `"
                                        (utf8-prefix (u8 a) 40) "' (nothing was changed)")))))
       (cond
         ((assq tag code-reasons) => (lambda (e) (refuse-tag tag (cdr e))))
         ((assq tag code-macro-reasons) => (lambda (e) (refuse-tag tag (cdr e))))
         ((suffix-reason tag) => (lambda (why) (refuse-tag tag why))))
       (let ((f (or (assq tag file-primitives) (assq tag file-macros))))
         (when f
           (let loop ((l (list-tail (cdr st) (min (cdr f) (length (cdr st))))) (first? #t))
             (when (pair? l)
               (cond (first?
                      (check-file-argument tag (car l) base (and (memq tag web-file-tags) #t)))
                     ((and (memq tag '(bibliography)) (string? (car l)))
                      ;; (the files of a bibliography, the style: names; a list
                      ;; of files separated by commas)
                      (for (name (split-commas (car l)))
                        (check-file-argument tag (tm-string-trim-both name) base #f))))
               (loop (cdr l) #f)))))
       (let ((l (or (assq tag link-primitives) (assq tag link-macros))))
         (when l
           (let ((a (argument st (cdr l))))
             (when a
               (cond ((not (string? a))
                      (violate! (string-append "a `" (symbol->string tag)
                                                 "' whose destination is not written as plain text is refused (nothing was changed)")))
                     ((not (name-chars-ok? a 'address))
                      (violate! (string-append "a `" (symbol->string tag)
                                                 "' with the target `" (utf8-prefix (u8 a) 60)
                                                 "' is refused: a backslash, a brace, a control character or a bare % cannot be in an address (nothing was changed)")))
                     ((not (link-target-ok? a))
                      (violate! (string-append "a `" (symbol->string tag)
                                                 "' with the target `" (utf8-prefix (u8 a) 60)
                                                 "' is refused: only http, https, mailto and relative targets are accepted (nothing was changed)"))))))))
       (cond
         ((== tag 'specific)
          ;; (the body is for the program that exports it, unless the format is
          ;; one of TeXmacs: then it is TeXmacs markup, checked as the rest)
          (let ((fmt (argument st 0)))
            (when (not (and (string? fmt) (member fmt '("texmacs" "screen" "printer"))))
              (violate! (string-append "a `specific' for the format `"
                                         (if (string? fmt) (utf8-prefix (u8 fmt) 20) "?")
                                         "' (text for another program) is refused: only texmacs, screen and printer (nothing was changed)")))))
         ((== tag 'latex)
          (for (a (cdr st))
            (when (string? a)
              (when (>= (string-search-forwards "^^" 0 a) 0)
                (violate! "a `latex' with ^^ (a way to write any character) is refused (nothing was changed)"))
              (check-latex-source a)
              (for (c (latex-commands a))
                (when (member c latex-tag-forbidden)
                  (violate! (string-append "the LaTeX command \\" c " in a `latex' tag is refused (it takes a file or an address; the text is written as it is in an export) (nothing was changed)"))))))))))
    (else #t)))

(define allowed-scripts '("mouse-fold" "mouse-unfold"))

(define (url-destination x)
  ;; the destination of a link: a string, or (url string); else #f
  (cond ((string? x) x)
        ((and (func? x 'url 1) (string? (cadr x))) (cadr x))
        (else #f)))

(define (check-expanded-file tag a base)
  ;; a file after the evaluation: the evaluator gives the full name of a file
  ;; that exists, which must be inside the roots (or the folder of the document)
  (cond ((and (string? a) (string-starts? a "/") (not (dotdot-in-path? a)))
         (let ((p (resolve-existing a)))
           (when (not (if (mcp-roots)
                          (path-allowed? p)
                          (inside-folder? p (document-folder base))))
             (violate! (string-append "the content expands to a `" (symbol->string tag)
                                        "' of the file `" (utf8-prefix (u8 a) 60)
                                        "', outside the folders of the server (or of the document): refused (nothing was changed)")))))
        (else (check-file-argument tag a base (and (memq tag web-file-tags) #t)))))

(define (check-expanded t base)
  ;; The expanded content (the macros of the style evaluated down to the
  ;; primitives, in the environment of the document): the links, the files,
  ;; the commands of the scripts and the raw texts, whatever macro or alias
  ;; made them, are checked with the rules of the primitives.
  (cond
    ((string? t) #t)
    ((pair? t)
     (let ((tag (car t)) (args (cdr t)))
       (for (c args) (check-expanded c base))
       (cond
         ((and (eq? tag 'link) (>= (length args) 3))
          (let* ((dest (car (reverse args))) (u (url-destination dest)))
            (cond ((func? dest 'script)
                   (when (not (and (pair? (cdr dest)) (member (cadr dest) allowed-scripts)))
                     (violate! "the content expands to a link that runs a script: refused (nothing was changed)")))
                  ((not u)
                   (when (and (pair? dest) (not (memq (car dest) '(id tuple))))
                     (violate! "the content expands to a link whose destination is not plain text: refused (nothing was changed)")))
                  ((or (not (name-chars-ok? u 'address)) (not (link-target-ok? u)))
                   (violate! (string-append "the content expands to a link to `" (utf8-prefix (u8 u) 60)
                                              "': only http, https, mailto and relative targets are accepted (nothing was changed)"))))))
         ((and (eq? tag 'hlink) (= (length args) 2))
          (let ((u (url-destination (cadr args))))
            (when (or (not u) (not (name-chars-ok? u 'address)) (not (link-target-ok? u)))
              (violate! "the content expands to a link whose destination is not accepted: refused (nothing was changed)"))))
         ((memq tag '(image postscript postscript* include include* pattern video sound))
          (when (pair? args)
            (check-expanded-file tag (car args) base)))
         ((eq? tag 'script)
          (when (not (and (pair? args) (member (car args) allowed-scripts)))
            (violate! "the content expands to a script: refused (nothing was changed)")))
         ((eq? tag 'action)
          (when (not (and (>= (length args) 2) (member (cadr args) allowed-scripts)))
            (violate! "the content expands to an action: refused (nothing was changed)")))
         ((eq? tag 'error)
          (violate! (string-append "the content expands to an error: "
                                   (if (and (pair? args) (string? (car args))) (utf8-prefix (u8 (car args)) 60) "?")
                                   " (nothing was changed)")))
         ((memq tag '(extern hybrid authorize observer eval-args))
          (refuse-tag tag "is left after the expansion of the content"))
         ((eq? tag 'specific)
          (when (not (and (pair? args) (member (car args) '("texmacs" "screen" "printer" "screen*" "printer*"))))
            (violate! "the content expands to a text for another program (specific): refused (nothing was changed)")))
         ((eq? tag 'latex)
          (for (a args)
            (when (string? a)
              (check-latex-source a)
              (for (c (latex-commands a))
                (when (member c latex-tag-forbidden)
                  (violate! "the content expands to LaTeX that takes a file or an address: refused (nothing was changed)")))))))))
    (else #t)))

(define (assign-nodes t)
  ;; the assigns of the document (its preamble), as strees
  (cond ((tree-atomic? t) '())
        ((tm-is? t 'assign) (list (tm->stree t)))
        (else (append-map assign-nodes (tree-children t)))))

(define (expand-in-document st d)
  ;; the content evaluated in the environment of the document (after its
  ;; own assigns), or #f if it cannot be
  (catch #t
    (lambda ()
      (let ((pre (assign-nodes (doc-body d))))
        (with-buffer (doc-url d)
          (tm->stree (texmacs-exec (stree->tree (cons 'concat (append pre (list st)))))))))
    (lambda args #f)))

(define latex-forbidden
  '("input" "include" "includeonly" "InputIfFileExists" "verbatiminput" "lstinputlisting"
    "openin" "openout" "immediate" "write" "read" "readline" "catcode" "scantokens"
    "csname" "directlua" "luaexec" "pdfshellescape" "newwrite" "externaldocument"
    ;; (ways to build a command name, or a definition that may hide one)
    "string" "expandafter" "noexpand" "def" "edef" "gdef" "xdef" "futurelet"
    "makeatletter" "makeatother"
    ;; (they become tags of the export, or load files or programs)
    "inputminted" "import" "subimport" "includepdf" "ShellEscape" "write18" "pgfplotstableread"
    "CatchFileDef" "DTLloaddb" "addplot" "loadglsentries" "lstset" "input@path"))

(define latex-tag-forbidden
  ;; more, for the text of a <latex|> tag, which is written as it is in an
  ;; export: the commands that take a file or an address
  '("includegraphics" "bibliography" "IfFileExists" "ExecuteMetaData" "graphicspath"
    "epsfbox" "epsffile" "psfig" "href" "url" "hyperref" "includestandalone"
    "pgfplotstableread" "lstinputlisting" "inputencoding" "openin"
    "pdfximage" "pdfobj" "pdfliteral" "pdfannot" "special" "VerbatimInput" "BVerbatimInput"
    "includesvg" "attachfile" "markdownInput" "latelua" "minted" "inputminted" "usemintedstyle"
    "write" "newwrite" "openout" "immediate"))

(define (latex-commands s)
  ;; the names of the commands (\name) in @s
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()))
      (cond ((>= i n) acc)
            ((char=? (string-ref s i) #\\)
             (let scan ((j (+ i 1)))
               (if (and (< j n) (char-alphabetic? (string-ref s j)))
                   (scan (+ j 1))
                   (loop (max (+ i 2) j)
                         (if (> j (+ i 1)) (cons (substring s (+ i 1) j) acc) acc)))))
            (else (loop (+ i 1) acc))))))

(define plain-arg-commands
  ;; commands whose arguments must be plain names (packages, classes, styles,
  ;; bibliography files), not paths
  '("usepackage" "RequirePackage" "documentclass" "bibliographystyle" "bibliography"
    "addbibresource"))

(define (plain-name-token? t)
  ;; letters, digits, - _ . = and blanks; not starting with a dot; no ..
  (let ((n (string-length t)))
    (and (> n 0) (< n 200)
         (not (char=? (string-ref t 0) #\.))
         (< (string-search-forwards ".." 0 t) 0)
         (let loop ((i 0))
           (or (>= i n)
               (let ((c (string-ref t i)))
                 (and (or (char-alphabetic? c) (char-numeric? c)
                          (memv c '(#\- #\_ #\. #\= #\space)))
                      (< (char->integer c) 128)
                      (loop (+ i 1)))))))))

(define (split-commas t)
  (let loop ((i 0) (from 0) (acc '()))
    (cond ((>= i (string-length t))
           (reverse (cons (substring t from i) acc)))
          ((char=? (string-ref t i) #\,)
           (loop (+ i 1) (+ i 1) (cons (substring t from i) acc)))
          (else (loop (+ i 1) from acc)))))

(define (latex-group s i open close)
  ;; the text of the group opened at i (open ... close, without nesting) and
  ;; the position after it, or #f
  (and (< i (string-length s)) (char=? (string-ref s i) open)
       (let loop ((j (+ i 1)))
         (cond ((>= j (string-length s)) #f)
               ((char=? (string-ref s j) close) (cons (substring s (+ i 1) j) (+ j 1)))
               ((memv (string-ref s j) '(#\{ #\} #\\ #\[ #\])) #f)
               (else (loop (+ j 1)))))))

(define (skip-blanks s i)
  (let loop ((i i))
    (if (and (< i (string-length s)) (memv (string-ref s i) '(#\space #\newline #\tab)))
        (loop (+ i 1)) i)))

(define latex-forbidden-packages
  '("minted" "catchfile" "shellesc" "pythontex" "sagetex" "gnuplottex" "bashful" "runcode"
    "verbatimcopy" "filecontents" "markdown" "write18" "epstopdf" "pgfplotstable" "datatool"
    "attachfile" "embedfile" "standalone" "luacode" "luatex85" "currfile" "import"
    "subfiles" "docmute" "combine" "includepdf" "pdfpages" "svg"))

(define (check-plain-arguments s name end)
  ;; the optional [options] and the {names} after the command: plain names
  (let* ((i (skip-blanks s end))
         (opt (latex-group s i #\[ #\]))
         (j (if opt (skip-blanks s (cdr opt)) i))
         (arg (latex-group s j #\{ #\})))
    (for (t (append (if opt (split-commas (car opt)) '())
                    (if arg (split-commas (car arg)) '())))
      (when (or (member (locase-all (tm-string-trim-both t)) latex-forbidden-packages)
                (string-contains? (locase-all t) "shell")
                (string-starts? (locase-all (tm-string-trim-both t)) "convert")
                (string-contains? (locase-all t) "write18"))
        (violate! (string-append "the LaTeX command \\" name " asks for `" (utf8-prefix t 40)
                                   "', which runs programs or reads files: refused (nothing was changed)")))
      (when (not (plain-name-token? (tm-string-trim-both t)))
        (violate! (string-append "the LaTeX command \\" name " has an argument that is not a plain name (a package, a class, a style or a file name of letters, digits, - _ . only; no path): `"
                                   (utf8-prefix t 40) "' (nothing was changed)"))))
    (when (and (not arg) (!= "" (tm-string-trim-both (substring s end (string-length s)))))
      (violate! (string-append "the LaTeX command \\" name " is not followed by a {name}: refused (nothing was changed)")))))

(define (latex-occurrences s)
  ;; ((name . position after the name) ...) of the commands in @s
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()))
      (cond ((>= i n) (reverse acc))
            ((char=? (string-ref s i) #\\)
             (let scan ((j (+ i 1)))
               (if (and (< j n) (char-alphabetic? (string-ref s j)))
                   (scan (+ j 1))
                   (loop (max (+ i 2) j)
                         (if (> j (+ i 1)) (cons (cons (substring s (+ i 1) j) j) acc) acc)))))
            (else (loop (+ i 1) acc))))))

(define (check-latex-source s)
  (when (string-contains? (locase-all s) "filecontents")
    (violate! "the LaTeX environment filecontents writes files: refused (nothing was changed)"))
  ;; the importer of LaTeX reads the files of \input and the like: not accepted
  (for (o (latex-occurrences s))
    (when (member (car o) latex-forbidden)
      (violate! (string-append "the LaTeX command \\" (car o) " is refused (it reads files, runs code or hides a command name): the content is refused (nothing was changed)")))
    (when (member (car o) plain-arg-commands)
      (check-plain-arguments s (car o) (cdr o)))))

(define (latex-extract-bibliography src)
  ;; (the text without \bibliographystyle, \bibliography and \addbibresource,
  ;; the style, the files): the importer would give a bibliography without its
  ;; files; they become the bibliography of the standard style afterwards
  (let loop ((occ (reverse (latex-occurrences src))) (text src) (style #f) (files '()))
    (cond
      ((null? occ) (list text style files))
      ((member (car (car occ)) '("bibliographystyle" "bibliography" "addbibresource"))
       (let* ((name (car (car occ))) (end (cdr (car occ)))
              (start (- end (string-length name) 1))
              (g (latex-group src (skip-blanks src end) #\{ #\})))
         (if (not g)
             (loop (cdr occ) text style files)
             (let ((gend (cdr g)) (names (map tm-string-trim-both (split-commas (car g)))))
               (loop (cdr occ)
                     (string-append (substring text 0 start)
                                    (substring text gend (string-length text)))
                     (if (and (== name "bibliographystyle") (not style)) (car names) style)
                     (if (== name "bibliography") (append names files) files))))))
      (else (loop (cdr occ) text style files)))))

(define (latex-document-source? src)
  ;; has the text a title or a document environment: then the importer of
  ;; whole documents (title, authors, date) is used
  (let ((cmds (map car (latex-occurrences src))))
    (or (member "maketitle" cmds) (member "title" cmds) (member "author" cmds)
        (member "date" cmds)
        (>= (string-search-forwards "\\\\begin{document}" 0 src) 0))))

(define (latex-convert src)
  ;; the stree of the LaTeX text @src (without bibliography commands)
  (if (latex-document-source? src)
      (let* ((whole (if (>= (string-search-forwards "\\\\begin{document}" 0 src) 0)
                        src
                        (string-append "\\\\documentclass{article}\\\\begin{document}" src
                                       "\\\\end{document}")))
             (r (convert whole "latex-document" "texmacs-stree")))
        (if (and (func? r 'document) (list-find (cdr r) (lambda (x) (func? x 'body 1))))
            (cadr (list-find (cdr r) (lambda (x) (func? x 'body 1))))
            (convert src "latex-snippet" "texmacs-stree")))
      (convert src "latex-snippet" "texmacs-stree")))

(define (latex-normalize st)
  ;; what the importer leaves of LaTeX: the preamble commands are dropped (the
  ;; style of the document says them), \autoref is a reference, \caption outside
  ;; a figure is its text, \thanks a footnote, \hline and \maketitle go
  (define (drop? t)
    (or (func? t 'documentclass) (func? t 'usepackage) (func? t 'addbibresource)
        (func? t 'bibliographystyle) (func? t 'RequirePackage)
        (func? t 'documentclass*) (func? t 'usepackage*) (func? t 'RequirePackage*)
        (func? t 'hline) (func? t 'maketitle) (func? t 'title) (func? t 'author)
        (func? t 'date)))
  (define (rewrite t)
    (cond ((not (pair? t)) t)
          ((func? t 'autoref 1) `(reference ,(rewrite (cadr t))))
          ((func? t 'caption 1) (rewrite (cadr t)))
          ((func? t 'thanks 1) `(footnote ,(rewrite (cadr t))))
          (else
            (cons (car t)
                  (let loop ((l (cdr t)) (acc '()))
                    (cond ((null? l) (reverse acc))
                          ((drop? (car l)) (loop (cdr l) acc))
                          (else (loop (cdr l) (cons (rewrite (car l)) acc)))))))))
  (define (clean t)
    (cond ((not (pair? t)) t)
          (else
            (let* ((kids (map clean (cdr t)))
                   (kids (if (memq (car t) '(document concat))
                             (list-filter kids (lambda (k) (not (or (equal? k '(concat)) (equal? k "")))))
                             kids)))
              (cond ((and (eq? (car t) 'concat) (null? kids)) "")
                    ((and (eq? (car t) 'concat) (null? (cdr kids))) (car kids))
                    (else (cons (car t) kids)))))))
  (clean (rewrite st)))

(define (latex-import src)
  ;; the stree of the LaTeX text @src, with a bibliography of the standard style
  ;; at the end if it had one
  (let* ((x (latex-extract-bibliography src))
         (r (latex-normalize (latex-convert (car x))))
         (style (cadr x)) (files (caddr x))
         (items (cond ((func? r 'document) (cdr r)) ((equal? r "") '()) (else (list r))))
         (bib (if (null? files) '()
                  (list `(bibliography "bib" ,(string-append "tm-" (or style "plain"))
                                       ,(string-recompose files ",") (document ""))))))
    (when (and (null? items) (null? bib))
      (tool-error "the content has only commands of the preamble (class, packages, styles): there is nothing to insert (nothing was changed)"))
    (cons 'document (append items bib))))

(define (assigned-names t)
  ;; the names that the document itself defines (assign): macros of its
  ;; preamble or of its text
  (cond ((tree-atomic? t) '())
        ((and (tm-is? t 'assign) (> (tree-arity t) 0) (tree-atomic? (tree-ref t 0)))
         (cons (tree->string (tree-ref t 0))
               (append-map assigned-names (tree-children t))))
        (else (append-map assigned-names (tree-children t)))))

(define (defined-predicate d)
  ;; is the tag defined for the document: by its style (and packages) as this
  ;; process has loaded them, or by an assign of the document
  (let ((locals (assigned-names (doc-body d))))
    (lambda (tag)
      (let ((name (symbol->string tag)))
        (or (and (member name locals) #t)
            (with-buffer (doc-url d) (style-has? name)))))))

(define (parse-content content fmt base d)
  ;; the stree of @content, written in the format @fmt; an error when it
  ;; does not parse, has a tag with a wrong number of arguments, or has
  ;; markup which is refused (@base is the folder of the document)
  (when (not (string? content))
    (param-error "the argument `content' must be a string"))
  (when (> (string-length content) max-content-size)
    (tool-error "the content is too long"))
  (when (not (member fmt edit-formats))
    (tool-error (string-append "unknown format " fmt ", use one of: "
                               (string-recompose edit-formats ", "))))
  (when (== fmt "latex") (check-latex-source content))
  (let ((st (catch #t
              (lambda ()
                (cond ((== fmt "stree")
                       (let ((x (string->object content)))
                         (and (valid-stree? x 0) (stree-cork x))))
                      ((== fmt "texmacs")
                       (and (brackets-balanced? content #\< #\>)
                            (convert (utf8-escape content)
                                     "texmacs-snippet" "texmacs-stree")))
                      ((== fmt "latex")
                       (and (brackets-balanced? content #\{ #\})
                            (list 'document "")))
                      (else
                        (convert content "markdown-snippet" "texmacs-stree"))))
              (lambda args #f))))
    (when (or (not st) (not (valid-stree? st 0)))
      (tool-error (string-append "the content does not parse as " fmt
                                 " (nothing was changed)")))
    (when (== fmt "latex")
      (set! st (let ((r (catch #t (lambda () (latex-import content))
                          (lambda args (if (eq? (car args) 'mcp-tool-error) 'tool-error #f)))))
                 (cond ((eq? r 'tool-error) (throw 'mcp-tool-error))
                       ((not r) (tool-error (string-append "the content does not parse as latex (nothing was changed)")))
                       (else r)))))
    (set! safety-defined? (and d (defined-predicate d)))
    (let ((r (catch #t
               (lambda () (check-safety st base) (check-arity st d) 'ok)
               (lambda args 'failed))))
      (set! safety-defined? #f)
      (when (!= r 'ok) (throw 'mcp-tool-error)))
    st))

(define (content-items st)
  ;; the blocks of the content: the children of a document, else the content
  (cond ((func? st 'document)
         (when (null? (cdr st)) (tool-error "the content is empty"))
         (cdr st))
        (else (list st))))

(define (flatten-inline items)
  ;; items of a concatenation: nested concatenations flattened, strings
  ;; merged, empty strings dropped
  (let loop ((l items) (acc '()))
    (cond ((null? l) (reverse acc))
          ((func? (car l) 'concat) (loop (append (cdr (car l)) (cdr l)) acc))
          ((and (string? (car l)) (== (car l) "")) (loop (cdr l) acc))
          ((and (string? (car l)) (pair? acc) (string? (car acc)))
           (loop (cdr l) (cons (string-append (car acc) (car l)) (cdr acc))))
          (else (loop (cdr l) (cons (car l) acc))))))

(define (inline-stree items)
  (let ((l (flatten-inline items)))
    (cond ((null? l) "")
          ((null? (cdr l)) (car l))
          (else (cons 'concat l)))))

(define (tree-of-stree st) (stree->tree st))

;;; The answer

(define (current-content t fmt)
  (let ((f (if (member fmt read-formats) fmt "texmacs")))
    (utf8-prefix (convert-checked t f) 4000)))

(define (node-report d t)
  (obj "handle" (make-handle! d t)
       "tag" (node-tag t)
       "hash" (hash-short t)
       "preview" (utf8-prefix (squeeze (ntext t)) 80)))

(define (conflict-result d t h expect fmt)
  (obj "status" "conflict"
       "document" (doc-id d)
       "node" h
       "expected" expect
       "current_hash" (hash-short t)
       "handle" (make-handle! d t)
       "current" (current-content t fmt)
       "message" "the node changed since you read it: nothing was changed. Look at `current', then retry with the new hash (or without `expect' to accept merging)."))

(define (check-expect d t h expect fmt)
  ;; #f if the node is as expected (or nothing is expected), else the
  ;; conflict to give as the result
  (and expect (!= expect (hash-short t))
       (conflict-result d t h expect fmt)))

;;; The steps of an agent, to undo them

(define mcp-steps (make-ahash-table))       ; document id -> list of undo steps
(define mcp-step-marks (make-ahash-table))  ; document id -> counters at the last step

(define (disturbances d)
  ;; how many times the host refused a patch of this document, took the
  ;; document away or replaced it in this process: the steps before are not
  ;; safe to undo
  (let ((u (doc-url d)))
    (+ (shared-file-stat u 'rolled-back) (shared-file-stat u 'lost)
       (shared-file-stat u 'replaced) (shared-file-stat u 'dropped)
       (shared-file-stat u 'resync))))

(define (steps-of d) (or (ahash-ref mcp-steps (doc-id d)) '()))
(define (push-step! d undo)
  (ahash-set! mcp-steps (doc-id d) (cons undo (steps-of d)))
  (ahash-set! mcp-step-marks (doc-id d) (disturbances d)))
(define (forget-steps! d)
  (ahash-remove! mcp-steps (doc-id d)))

(define (settle! d before)
  ;; wait (at most 10 s) until the host has taken the changes of this process:
  ;; ok, refused (the host did not take them, they are undone), timeout
  (let ((u (doc-url d)))
    (if (not (shared-file-participant? u))
        'ok
        (let ((t0 (texmacs-time)))
          (let loop ((n 0))
            (shared-poll)
            (let ((i (shared-file-info u)))
              (cond ((> (disturbances d) before) 'refused)
                    ((not i) 'ok)
                    ((and (== (list-ref i 5) 0) (not (list-ref i 6)) (> n 0)) 'ok)
                    ((> (- (texmacs-time) t0) 10000) 'timeout)
                    (else (loop (+ n 1))))))))))

;;; The filter of the document: an edit may not introduce a violation
;;; The content is checked as it is written, but what it does depends on the
;;; document (the macros of the style, the assigns of the preamble, the `with'
;;; around it), and an edit of a leaf or of a definition changes what existing
;;; text expands to. So the document is checked after the edit, tentatively
;;; made inside the transaction of the editor (mark-start, mark-cancel), and a
;;; violation that was not there before the edit cancels it. The violations that
;;; were already there (the preamble of the person, their own links) stay.
;;; The unit is the top-level block that holds the edit (and the blocks the edit
;;; adds), with the assigns of the blocks before it; when an assign is changed
;;; the whole document is compared. Evaluation has a budget: what does not
;;; evaluate within it is a violation.

(define block-budget 1500000)
(define document-budget 40000000)

(define (expansion-violations st base d assigns budget)
  ;; the violations of the rules for the expansion of @st (a stree), in the
  ;; environment of the document after @assigns
  (let* ((u (doc-url d))
         (r (catch #t
              (lambda ()
                (exec-budget-start budget)
                ;; (the assigns come first, to define; what is checked is the last
                ;; part, the expansion of the content)
                (let ((x (with-buffer u
                           (tm->stree (texmacs-exec (stree->tree (cons 'concat (append assigns (list (list 'document st))))))))))
                  (if (and (pair? x) (eq? (car x) 'concat) (pair? (cdr x)))
                      (car (reverse (cdr x)))
                      x)))
              (lambda args #f)))
         (left (exec-budget-end)))
    (cond ((< left 0) (list "the content does not evaluate within the budget (it expands too much)"))
          ((not r) (list "the content cannot be evaluated"))
          (else (collect-violations (lambda () (check-expanded r base)))))))

(define (literal-violations st base d)
  (set! safety-defined? (and d (defined-predicate d)))
  (let ((r (collect-violations (lambda () (check-safety st base) (check-arity st d)))))
    (set! safety-defined? #f)
    r))

(define (assigns-before d i)
  ;; the assigns of the blocks before the block number @i (and of the other
  ;; place where the document defines something: the blocks are in order)
  (let loop ((l (tree-children (doc-body d))) (k 0) (acc '()))
    (if (or (null? l) (>= k i))
        acc
        (loop (cdr l) (+ k 1) (append acc (assign-nodes (car l)))))))

(define (block-violations d blk i)
  ;; the violations of the top-level block @blk (a tree) of index @i
  (let ((st (tm->stree blk)) (base (doc-base d)))
    (append (literal-violations st base d)
            (expansion-violations st base d (assigns-before d i) block-budget))))

(define (document-violations d)
  ;; the violations of the whole document
  (let* ((body (doc-body d)) (st (tm->stree body)) (base (doc-base d)))
    (append (literal-violations st base d)
            (expansion-violations st base d '() document-budget))))

(define (assigns-signature d)
  (tree-hash (stree->tree (cons 'tuple (assign-nodes (doc-body d))))))

(define (new-violations before after)
  ;; the reasons of @after that are not in @before (with their number)
  (let loop ((l after) (pool before) (acc '()))
    (cond ((null? l) (reverse acc))
          ((member (car l) pool)
           (loop (cdr l) (let rm ((p pool)) (cond ((null? p) '()) ((equal? (car p) (car l)) (cdr p)) (else (cons (car p) (rm (cdr p)))))) acc))
          (else (loop (cdr l) pool (cons (car l) acc))))))

(define (guard-pre d t)
  ;; before an edit of the node @t (or of the document: #f): what is needed to
  ;; compare after it
  (let* ((body (doc-body d))
         (blk (and t (not (body-root? d t)) (top-level-ancestor d t)))
         (i (and blk (index-in-body d blk)))
         (defs? (and blk (pair? (assign-nodes blk)))))
    (list (and blk (ptr blk))
          (and blk i (block-violations d blk i))
          (assigns-signature d)
          (and defs? (document-violations d)))))

(define (guard-fail! reasons)
  (set! edit-reason
        (string-append "the edit is refused: it would introduce what the content filter refuses ("
                       (utf8-prefix (u8 (car reasons)) 200)
                       (if (pair? (cdr reasons)) (string-append ", and " (number->string (length (cdr reasons))) " more") "")
                       "); nothing was changed"))
  (throw 'mcp-guard-refused))

(define (guard-post d pre new-nodes skip-block?)
  ;; after the edit, inside its transaction: refuse it if it introduced a violation
  (let* ((old-ptr (car pre)) (old-viol (cadr pre))
         (blk (and old-ptr (not skip-block?) (deref old-ptr)))
         (blocks (let loop ((l (append (if (and blk (tree? blk) (inside-body? d blk)
                                                (not (body-root? d blk)))
                                           (list (top-level-ancestor d blk)) '())
                                       (map (lambda (n) (and (tree? n) (inside-body? d n)
                                                             (not (body-root? d n))
                                                             (top-level-ancestor d n)))
                                            new-nodes)))
                           (acc '()))
                   (cond ((null? l) (reverse acc))
                         ((not (car l)) (loop (cdr l) acc))
                         ((list-find acc (lambda (x) (tree-eq? x (car l)))) (loop (cdr l) acc))
                         (else (loop (cdr l) (cons (car l) acc)))))))
    (for (b blocks)
      (let* ((i (index-in-body d b))
             (after (block-violations d b i))
             (before (if (and blk (tree? blk) (tree-eq? b (top-level-ancestor d blk))) (or old-viol '()) '()))
             (new (new-violations before after)))
        (when (pair? new) (guard-fail! new))))
    (when (!= (assigns-signature d) (caddr pre))
      (let ((full-before (cadddr pre)))
        (when (not full-before)
          (guard-fail! (list "it changes the definitions of the document")))
        (let ((new (new-violations full-before (document-violations d))))
          (when (pair? new) (guard-fail! new)))))))

(define edit-reason #f)
(define current-guard #f)

(define (set-guard! d t get-nodes skip-block?)
  ;; the next run-edit! checks the document before and after (see guard-post)
  (set! current-guard (list d (guard-pre d t) get-nodes skip-block?)))

(define (run-edit! d thunk)
  ;; do @thunk (it changes the tree) as one step of the editor of the buffer,
  ;; all or nothing; gives its value
  (let ((u (doc-url d)) (failed #f) (value #f) (g current-guard))
    (set! current-guard #f)
    (with-buffer u
      (start-editing)
      (let ((a (get-author)))
        (mark-start a)
        (set! value (catch #t thunk
                      (lambda args
                        (mark-cancel a)
                        (set! failed #t)
                        #f)))
        ;; (the document is checked as it is after the edit; a violation that was
        ;; not there before cancels the edit)
        (when (and g (not failed))
          (catch #t
            (lambda () (guard-post (car g) (cadr g) ((caddr g)) (cadddr g)))
            (lambda args
              (when (not edit-reason)
                (set! edit-reason "the check of the document after the edit failed or ran out of budget: nothing was changed"))
              (mark-cancel a)
              (set! failed #t))))
        (when (not failed) (mark-end a))
        (end-editing)))
    (when failed
      (let ((why edit-reason))
        (set! edit-reason #f)
        (tool-error (or why "the edit failed while it was made and was cancelled: nothing was changed"))))
    value))

(define (filter-live d nodes)
  (list-filter nodes (lambda (t) (and (tree? t) (inside-body? d t)))))

(define (finish-edit! d dist changed undo)
  ;; after the edit: wait for the host, say what happened. @changed is a
  ;; list of nodes, @undo the way to undo the step (or #f)
  (let* ((outcome (settle! d dist))
         (body (doc-body d)))
    (cond ((== outcome 'refused)
           (forget-steps! d)
           (with-buffer (doc-url d) (clear-undo-history))
           (snapshot! d)
           (obj "status" "refused"
                "document" (doc-id d)
                "reason" "the host (the TeXmacs of the person) refused the change: the document had changed too much around it. The document is back to what the host has; nothing of yours is kept. Read again and retry."
                "hash" (hash-short body)))
          (else
            (when undo (push-step! d undo))
            (snapshot! d)
            (let ((live (filter-live d changed)))
              (apply obj
                (append
                  (list "status" (if (== outcome 'timeout) "pending" "applied")
                        "document" (doc-id d)
                        "hash" (hash-short body)
                        "changed" (arr (map (lambda (t) (node-report d t)) live))
                        "undo_steps" (num (length (steps-of d))))
                  (if (== outcome 'timeout)
                      (list "message" "the host did not answer within 10 s: the change is kept here and is sent when the host answers")
                      '())
                  (if (< (length live) (length changed))
                      (list "message" "some of the changed nodes were changed again by someone else at the same time: read them again")
                      '()))))))))

;;; Notes: what changed since the last call about the document

(define mcp-snaps (make-ahash-table))       ; document id -> hashes of the blocks

(define (block-hashes d)
  (map (lambda (t) (tree-hash t)) (tree-children (doc-body d))))

(define (snapshot! d)
  (ahash-set! mcp-snaps (doc-id d) (block-hashes d)))

(define (common-prefix a b)
  (let loop ((a a) (b b) (n 0))
    (if (and (pair? a) (pair? b) (== (car a) (car b)))
        (loop (cdr a) (cdr b) (+ n 1))
        n)))

(define (change-note d)
  ;; a note when the blocks of @d changed since the last time (by others),
  ;; else #f; the snapshot is brought up to date
  (let ((old (ahash-ref mcp-snaps (doc-id d))))
    (and old
         (let ((new (block-hashes d)))
           (and (!= old new)
                (let* ((p (common-prefix old new))
                       (s (common-prefix (reverse (list-tail old p))
                                         (reverse (list-tail new p))))
                       (n-old (- (length old) p s)) (n-new (- (length new) p s))
                       (body (doc-body d))
                       (sections
                        (let loop ((i p) (acc '()))
                          (if (>= i (+ p (max 1 n-new)))
                              (reverse acc)
                              (let ((sec (and (< i (tree-arity body))
                                              (section-of d (tree-ref body i)))))
                                (loop (+ i 1)
                                      (if (and sec (not (member sec acc)))
                                          (cons sec acc) acc)))))))
                  (ahash-set! mcp-snaps (doc-id d) new)
                  (string-append
                   "The document changed since your last call (by someone else, or a resynchronization): "
                   (number->string n-old) " block(s) from index " (number->string p)
                   " replaced by " (number->string n-new)
                   (if (null? sections) ""
                       (string-append ", in the section(s): "
                                      (string-recompose
                                       (sublist sections 0 (min 5 (length sections))) "; ")))
                   ". Its hash is now " (hash-short body)
                   ". Handles to blocks you read before may be stale.")))))))

;;; The nodes

(define (editing-context args)
  (require-writable)
  (let ((d (require-doc (opt-string args "document" #f))))
    (doc-refresh! d)
    d))

(define (body-root? d t) (tree-eq? t (doc-body d)))

(define (parent-of d t)
  (let ((p (tree-up t)))
    (and p (inside-body? d p) p)))

(define (doc-base d)
  ;; the folder of the document
  (let* ((p (doc-path d))
         (i (let loop ((k (- (string-length p) 1)))
              (cond ((< k 0) #f) ((char=? (string-ref p k) #\/) k) (else (loop (- k 1)))))))
    (if (and i (> i 0)) (substring p 0 i) "/")))

(define (target d args key)
  ;; (handle . node) of the argument @key
  (let ((h (opt-string args key #f)))
    (when (not h) (tool-error (string-append "the argument `" key "' (a handle) is needed")))
    (cons h (handle-node d h))))

(define (ptr t) (tree->tree-pointer t))
(define (deref p) (tree-pointer->tree p))

(define (pointed-ok? d p hash)
  ;; does the pointer @p still point at a node of the document with that hash?
  ;; (A pointer follows its node when it is replaced as a whole, and goes to
  ;; a neighbour when it is removed: the hash tells.)
  (let ((t (deref p)))
    (and (tree? t) (inside-body? d t) (== (tree-hash t) hash))))

(define (same-nodes? d ptrs hashes)
  (list-and (map (lambda (p h) (pointed-ok? d p h)) ptrs hashes)))

;;; replace

(define (undo-replace d new-nodes old-items)
  ;; the step that put @new-nodes (siblings, in order) in place of @old-items
  (let ((ptrs (map ptr new-nodes)) (hashes (map tree-hash new-nodes)))
    (cons
     (lambda ()
       (or (same-nodes? d ptrs hashes)
           "the nodes this step made were changed since"))
     (lambda ()
       (let* ((first (deref (car ptrs))) (parent (tree-up first)) (i (tree-index first))
              (k (length ptrs)))
         (tree-assign first (tree-of-stree
                             (if (tm-is? parent 'document)
                                 (car old-items)
                                 (inline-stree old-items))))
         (when (tm-is? parent 'document)
           (when (> k 1) (tree-remove parent (+ i 1) (- k 1)))
           (when (pair? (cdr old-items))
             (tree-var-insert parent (+ i 1)
                              (tree-of-stree (cons 'document (cdr old-items))))))
         #t)))))

(define (tool-replace args)
  (let* ((d (editing-context args))
         (fmt (opt-string args "format" "texmacs"))
         (hn (target d args "node")) (h (car hn)) (t (cdr hn))
         (expect (opt-string args "expect" #f))
         (st (parse-content (opt-string args "content" #f) fmt (doc-base d) d))
         (items (content-items st)))
    (cond
      ((body-root? d t)
       (tool-error "the whole document cannot be replaced: replace its blocks, or create a new document"))
      ((check-expect d t h expect fmt))
      (else
        (let* ((parent (parent-of d t)) (i (tree-index t))
               (old (tm->stree t))
               (dist (disturbances d))
               (block? (tm-is? parent 'document))
               (inline? (tm-is? parent 'concat))
               (nodes '()))
          (when (and (not block?) (pair? (cdr items)))
            (tool-error "the node is part of a paragraph: the content must be one item, not several blocks"))
          (cond
            (block?
             (set-guard! d t (lambda () nodes) #f)
             (run-edit! d
               (lambda ()
                 (tree-assign t (tree-of-stree (car items)))
                 (when (pair? (cdr items))
                   (tree-var-insert parent (+ i 1)
                                    (tree-of-stree (cons 'document (cdr items)))))
                 (set! nodes (map (lambda (k) (tree-ref parent (+ i k)))
                                  (iota (length items))))))
             (finish-edit! d dist nodes (undo-replace d nodes (list old))))
            (inline?
             ;; (the paragraph is rebuilt, so that the strings are merged)
             (let* ((pold (tm->stree parent))
                    (kids (cdr pold))
                    (new (inline-stree (append (sublist kids 0 i) items
                                               (list-tail kids (+ i 1))))))
               (set-guard! d t (lambda () nodes) #f)
               (run-edit! d
                 (lambda ()
                   (tree-assign parent (tree-of-stree new))
                   (set! nodes (list parent))))
               (finish-edit! d dist nodes (undo-replace d nodes (list pold)))))
            (else
              (set-guard! d t (lambda () nodes) #f)
              (run-edit! d
                (lambda ()
                  (tree-assign t (tree-of-stree (car items)))
                  (set! nodes (list (tree-ref parent i)))))
              (finish-edit! d dist nodes (undo-replace d nodes (list old))))))))))

;;; insert

(define (undo-insert d nodes)
  ;; the step that inserted the sibling @nodes
  (let ((ptrs (map ptr nodes)) (hashes (map tree-hash nodes)))
    (cons
     (lambda ()
       (or (same-nodes? d ptrs hashes)
           "the nodes this step made were changed since"))
     (lambda ()
       (let* ((first (deref (car ptrs))) (parent (tree-up first)) (i (tree-index first)))
         (tree-remove parent i (length ptrs))
         #t)))))

(define (tool-insert args)
  (let* ((d (editing-context args))
         (fmt (opt-string args "format" "texmacs"))
         (where (opt-string args "where" #f))
         (ah (opt-string args "anchor" #f))
         (anchor (if ah (handle-node d ah) (doc-body d)))
         (expect (opt-string args "expect" #f))
         (st (parse-content (opt-string args "content" #f) fmt (doc-base d) d))
         (items (content-items st)))
    (when (not (member where '("before" "after" "first" "last")))
      (param-error "the argument `where' must be before, after, first or last"))
    (cond
      ((and (not ah) (member where '("before" "after")))
       (tool-error "`before' and `after' need an anchor"))
      ((and ah (body-root? d anchor) (member where '("before" "after")))
       (tool-error "the document itself has no neighbours: use first or last"))
      ((check-expect d anchor (or ah "the document") expect fmt))
      ((and (tree-atomic? anchor) (member where '("first" "last")))
       (tool-error "a text node has no children: use before or after, or edit_text"))
      (else
        (let* ((dist (disturbances d))
               (side? (member where '("before" "after")))
               (parent (if side? (parent-of d anchor) anchor))
               (pos (cond ((== where "before") (tree-index anchor))
                          ((== where "after") (+ 1 (tree-index anchor)))
                          ((== where "first") 0)
                          (else (tree-arity anchor))))
               (nodes '()))
          (cond
            ((tm-is? parent 'document)
             (set-guard! d (if ah anchor #f) (lambda () nodes) #f)
             (run-edit! d
               (lambda ()
                 (tree-var-insert parent pos (tree-of-stree (cons 'document items)))
                 (set! nodes (map (lambda (k) (tree-ref parent (+ pos k)))
                                  (iota (length items))))))
             (finish-edit! d dist nodes (undo-insert d nodes)))
            ((and (tm-is? parent 'concat) (not (tree-atomic? parent)))
             ;; inside a paragraph: it is rebuilt, so that the strings are merged
             (let* ((old (tm->stree parent)) (kids (cdr old))
                    (new (inline-stree (append (sublist kids 0 pos) items
                                               (list-tail kids pos)))))
               (set-guard! d (if ah anchor #f) (lambda () nodes) #f)
               (run-edit! d
                 (lambda ()
                   (tree-assign parent (tree-of-stree new))
                   (set! nodes (list parent))))
               (finish-edit! d dist nodes (undo-replace d nodes (list old)))))
            (else
              (tool-error "new content can be inserted among the blocks of a document and among the parts of a paragraph; here, replace the node instead"))))))))

;;; delete

(define (undo-delete d parent prev next old)
  ;; the step that deleted the block @old between the siblings @prev and
  ;; @next (nodes, or #f)
  (let ((pp (and prev (ptr prev))) (hp (and prev (tree-hash prev)))
        (pn (and next (ptr next))) (hn (and next (tree-hash next))))
    (define (prev-ok?) (and prev (pointed-ok? d pp hp)))
    (define (next-ok?) (and next (pointed-ok? d pn hn)))
    (cons
     (lambda ()
       (or (prev-ok?) (next-ok?)
           "the neighbours of the deleted block changed since"))
     (lambda ()
       (cond ((prev-ok?)
              (let ((p (deref pp)))
                (tree-var-insert (tree-up p) (+ 1 (tree-index p))
                                 (tree-of-stree (list 'document old)))))
             (else
               (let ((n (deref pn)))
                 (tree-var-insert (tree-up n) (tree-index n)
                                  (tree-of-stree (list 'document old))))))
       #t))))

(define (tool-delete args)
  (let* ((d (editing-context args))
         (fmt (opt-string args "format" "texmacs"))
         (hn (target d args "node")) (h (car hn)) (t (cdr hn))
         (expect (opt-string args "expect" #f)))
    (cond
      ((body-root? d t) (tool-error "the whole document cannot be deleted"))
      ((check-expect d t h expect fmt))
      (else
        (let* ((parent (parent-of d t)) (i (tree-index t))
               (old (tm->stree t))
               (dist (disturbances d)))
          (cond
            ((tm-is? parent 'document)
             (let ((prev (and (> i 0) (tree-ref parent (- i 1))))
                   (next (and (< (+ i 1) (tree-arity parent)) (tree-ref parent (+ i 1)))))
               (if (and (not prev) (not next))
                   ;; (a document is never empty: its only block becomes an empty text)
                   (let ((nodes '()))
                     (set-guard! d t (lambda () (quote ())) (body-root? d parent))
                     (run-edit! d (lambda ()
                                    (tree-assign t (tree-of-stree ""))
                                    (set! nodes (list (tree-ref parent 0)))))
                     (finish-edit! d dist '() (undo-replace d nodes (list old))))
                   (begin
                     (set-guard! d t (lambda () (quote ())) (body-root? d parent))
                     (run-edit! d (lambda () (tree-remove parent i 1)))
                     (finish-edit! d dist '()
                                   (undo-delete d parent prev next old))))))
            ((tm-is? parent 'concat)
             (let* ((pold (tm->stree parent))
                    (kids (cdr pold))
                    (new (inline-stree (append (sublist kids 0 i) (list-tail kids (+ i 1)))))
                    (nodes '()))
               (set-guard! d t (lambda () (quote ())) (body-root? d parent))
               (run-edit! d (lambda ()
                              (tree-assign parent (tree-of-stree new))
                              (set! nodes (list parent))))
               (finish-edit! d dist nodes (undo-replace d nodes (list pold)))))
            (else
              (tool-error "this node is an argument of its tag and cannot be deleted by itself: replace it (with an empty text, say)"))))))))

;;; edit_text

(define skipped-text-tags '(label reference pageref eqref cite nocite cite-detail
                            hlink href image postscript postscript* assign))

(define (text-leaves t)
  ;; the strings of the visible text of @t
  (cond ((tree-atomic? t) (list t))
        ((in? (tree-label t) skipped-text-tags) '())
        (else (append-map text-leaves (tree-children t)))))

(define (occurrences s what)
  ;; the positions of @what in the string @s
  (let loop ((from 0) (acc '()))
    (let ((i (if (> from (string-length s)) -1 (string-search-forwards what from s))))
      (if (< i 0)
          (reverse acc)
          (loop (+ i 1) (cons i acc))))))

(define (undo-edit-text d leaf pos old new)
  ;; the step that replaced @old by @new at @pos in the text @leaf (called
  ;; after the change): it is undone if @new, and the text around it, are as
  ;; it left them (the text around is what tells when @new is empty)
  (let* ((pl (ptr leaf)) (text (tree->string leaf))
         (nl (string-length new))
         (left (substring text (max 0 (- pos 12)) pos))
         (right (substring text (+ pos nl) (min (string-length text) (+ pos nl 12)))))
    (cons
     (lambda ()
       (let ((l (deref pl)))
         (or (and (tree? l) (inside-body? d l) (tree-atomic? l)
                  (let ((now (tree->string l)))
                    (and (<= (+ pos nl) (string-length now))
                         (>= pos (string-length left))
                         (== (substring now pos (+ pos nl)) new)
                         (== (substring now (- pos (string-length left)) pos) left)
                         (<= (+ pos nl (string-length right)) (string-length now))
                         (== (substring now (+ pos nl) (+ pos nl (string-length right))) right))))
             "the text this step changed was changed since")))
     (lambda ()
       (let ((l (deref pl)))
         (when (> nl 0) (tree-remove l pos nl))
         (when (> (string-length old) 0) (tree-insert l pos old))
         #t)))))

(define (tool-edit-text args)
  (let* ((d (editing-context args))
         (hn (target d args "node")) (h (car hn)) (t (cdr hn))
         (expect (opt-string args "expect" #f))
         (old (opt-string args "old" #f))
         (new (opt-string args "new" #f)))
    (when (or (not old) (== old ""))
      (tool-error "the argument `old' (the text to replace) is needed"))
    (when (not new)
      (tool-error "the argument `new' (the new text, possibly empty) is needed"))
    (cond
      ((check-expect d t h expect "texmacs"))
      (else
        (let* ((old* (utf8->cork old)) (new* (utf8->cork new))
               (hits (append-map
                      (lambda (leaf)
                        (map (lambda (p) (cons leaf p))
                             (occurrences (tree->string leaf) old*)))
                      (text-leaves t))))
          (cond
            ((null? hits)
             (tool-error "the text `old' was not found in the node (it must lie in one stretch of text without markup inside, and be written as it is)"))
            ((pair? (cdr hits))
             (tool-error (string-append "the text `old' occurs " (number->string (length hits))
                                        " times in the node: give more of the text around it, so that it occurs once")))
            (else
              (let* ((leaf (car (car hits))) (pos (cdr (car hits)))
                     (dist (disturbances d)))
                (set-guard! d t (lambda () (list t)) #f)
                (run-edit! d
                  (lambda ()
                    (tree-remove leaf pos (string-length old*))
                    (when (> (string-length new*) 0) (tree-insert leaf pos new*))))
                (finish-edit! d dist (list t)
                              (undo-edit-text d leaf pos old* new*))))))))))

;;; undo_agent

(define (tool-undo-agent args)
  (let* ((d (editing-context args))
         (steps (opt-int args "steps" 1 1 50))
         (stack (steps-of d)))
    (cond
      ((null? stack)
       (tool-error "there is no step of this session to undo in this document"))
      ((> (disturbances d) (or (ahash-ref mcp-step-marks (doc-id d)) 0))
       (forget-steps! d)
       (tool-error "the document was resynchronized or a change was refused since: the steps before cannot be undone safely"))
      (else
        (let* ((n (min steps (length stack)))
               (todo (sublist stack 0 n))
               (dist (disturbances d)))
          ;; each step is checked on the document as it is when its turn
          ;; comes (the newer steps are undone first); if one cannot be
          ;; undone everything is cancelled and nothing is changed
          (set! edit-reason #f)
          (run-edit! d
            (lambda ()
              (for (u todo)
                (let ((c ((car u))))
                  (when (string? c)
                    (set! edit-reason (string-append "not undone: " c " (the step number " (number->string (- (length stack) (length (memq u stack)) -1)) " from the newest); nothing was changed"))
                    (throw 'mcp-undo-refused))
                  ((cdr u))))))
          (ahash-set! mcp-steps (doc-id d) (list-tail stack n))
          (let ((r (finish-edit! d dist '() #f)))
            (apply obj (append (cdr r) (list "undone" (num n))))))))))

;;; save, close, create

(define (tool-save-document args)
  (let* ((d (editing-context args)) (u (doc-url d)))
    (settle! d (disturbances d))
    (save-buffer-main u)
    ;; (a participant asks the host: wait for it)
    (let loop ((n 0))
      (when (and (shared-file-participant? u) (buffer-modified? u) (< n 500))
        (shared-poll)
        (loop (+ n 1))))
    (obj "status" (if (buffer-modified? u) "pending" "saved")
         "document" (doc-id d)
         "path" (doc-path d)
         "shared" (doc-shared d)
         "hash" (hash-short (doc-body d)))))

(define (tool-close-document args)
  (let* ((d (editing-context args)) (u (doc-url d))
         (discard (jbool (jref args "discard") #f)))
    (when (and (buffer-modified? u) (not discard))
      (tool-error "the document has unsaved changes: save_document first, or close it with discard=true"))
    (when (buffer-modified? u) (buffer-pretend-saved u))
    (for (e (ahash-table->list mcp-handles))
      (when (== (caddr e) (doc-id d))
        (tree-pointer-detach (cadr e))
        (ahash-remove! mcp-handles (car e))))
    (forget-steps! d)
    (ahash-remove! mcp-snaps (doc-id d))
    (set! mcp-docs (list-filter mcp-docs (lambda (x) (!= (doc-id x) (doc-id d)))))
    (catch #t (lambda () (buffer-close u)) (lambda args #f))
    (obj "status" "closed" "document" (doc-id d))))

(define (tool-create-document args)
  (require-writable)
  (let* ((path (opt-string args "path" #f))
         (style (opt-string args "style" "generic"))
         (content (opt-string args "content" #f))
         (fmt (opt-string args "format" "texmacs")))
    (when (not path) (tool-error "the argument `path' is needed"))
    (when (not (absolute-path? path)) (tool-error "the path must be absolute"))
    (when (dotdot-in-path? path) (tool-error "a path with a component `..' is not accepted"))
    (when (colon-in-path? path) (tool-error "a path with a colon (:) cannot be used"))
    (when (not (string-ends? path ".tm")) (tool-error "the file name must end with .tm"))
    (when (not (let loop ((i 0))
                 (or (>= i (string-length style))
                     (and (or (char-alphabetic? (string-ref style i))
                              (char-numeric? (string-ref style i))
                              (memv (string-ref style i) '(#\- #\_)))
                          (loop (+ i 1))))))
      (tool-error "the style is the name of a style, such as article"))
    (let* ((slash (let loop ((i (- (string-length path) 1)))
                    (cond ((< i 0) #f)
                          ((char=? (string-ref path i) #\/) i)
                          (else (loop (- i 1))))))
           (dir (and slash (substring path 0 (max 1 slash))))
           (rdir (and dir (shared-real-path dir))))
      (when (or (not rdir) (== rdir ""))
        (tool-error "the folder of the file does not exist"))
      (let ((target (string-append (if (== rdir "/") "" rdir) "/"
                                   (substring path (+ slash 1) (string-length path)))))
        (when (not (path-allowed? target))
          (tool-error (string-append "not allowed: " path
                                     " is outside the folders of this server")))
        (when (shared-lexists? target)
          (tool-error "the file exists already (or is a link): open it instead"))
        ;; (made with O_EXCL: no existing path is followed or overwritten)
        (when (not (shared-create-document
                    target
                    (string-append "<TeXmacs|" (texmacs-version) ">\n\n<style|<tuple|"
                                   style ">>\n\n<\\body>\n  \n</body>\n")))
          (tool-error "the file could not be created"))
        (set! mcp-tool-error-message "")
        (let* ((failed #f)
               (d (catch #t
                    (lambda ()
                      ;; (the content is read for the style of the new document)
                      (let* ((d (open-doc! target))
                             (items (and content
                                         (content-items
                                          (parse-content content fmt rdir d)))))
                        (when items
                          (run-edit! d
                            (lambda ()
                              (tree-assign (doc-body d)
                                           (tree-of-stree (cons 'document items)))))
                          (forget-steps! d)
                          ;; (a new document has nothing but the content)
                          (let ((v (document-violations d)))
                            (when (pair? v)
                              (tool-error (string-append "the content is refused: "
                                                         (utf8-prefix (u8 (car v)) 200))))))
                        d))
                    (lambda args
                      (set! failed #t)
                      #f))))
          (when failed
            ;; (what was made is removed, with the reason that was given)
            (let ((why mcp-tool-error-message))
              (for (x mcp-docs)
                (when (== (doc-path x) (canonical-path target))
                  (catch #t (lambda () (buffer-close (doc-url x))) (lambda args #f))
                  (set! mcp-docs (list-filter mcp-docs (lambda (y) (!= (doc-id y) (doc-id x)))))))
              (catch #t (lambda () (system-remove (system->url target))) (lambda args #f))
              (tool-error (if (and (string? why) (!= why ""))
                              why
                              "the document could not be made: the file was removed"))))
            (snapshot! d)
            (doc-info d))))))

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
         (schema (list (cons "document" document-prop)
                       (cons "limit" (prop "integer" "Most sections to return (default 300)."))
                       (cons "start" (prop "integer" "Index of the first section, to continue (next_start).")))
                 '("document"))
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
         (schema (list (cons "document" document-prop)
                       (cons "limit" (prop "integer" "Most items per list (default 200)."))
                       (cons "start" (prop "integer" "Index of the first item of each list, to continue.")))
                 '("document"))
         tool-get-references)
   ;; the tools which edit (a seventh element: #t)
   (list "replace" "Replace a node"
         "Replaces the node of a handle by content (one or several blocks if the node is a block of the document; one item if it is inside a paragraph). `expect' is the hash of the node from your last read: if the node changed since, nothing is done and the result is a `conflict' with its current content and a fresh handle (leave `expect' out to accept merging with the other change). One call is one undo step, all or nothing: content which does not parse is refused. Gives the new handles, or `refused' if the person's TeXmacs refused the change."
         (schema (list (cons "document" document-prop)
                       (cons "node" (prop "string" "Handle of the node to replace."))
                       (cons "content" (prop "string" "The new content."))
                       (cons "format" (prop "string" "Format of the content: texmacs (default, the .tm syntax of a snippet), stree, latex or markdown."))
                       (cons "expect" (prop "string" "The hash of the node, as read.")))
                 '("document" "node" "content"))
         tool-replace #t)
   (list "insert" "Insert content"
         "Inserts content before or after a node (a block of the document, or a part of a paragraph), or as its first or last child; without `anchor' in the document itself (where first or last). Several blocks can be inserted among blocks. Optional `expect' is the hash of the anchor. One undo step."
         (schema (list (cons "document" document-prop)
                       (cons "anchor" (prop "string" "Handle of the node next to which to insert (none: the document)."))
                       (cons "where" (prop "string" "before, after, first or last."))
                       (cons "content" (prop "string" "The content to insert."))
                       (cons "format" (prop "string" "texmacs (default), stree, latex or markdown."))
                       (cons "expect" (prop "string" "The hash of the anchor, as read.")))
                 '("document" "where" "content"))
         tool-insert #t)
   (list "delete" "Delete a node"
         "Deletes a block of the document, or a part of a paragraph. `expect' is the hash of the node as read: if it changed, the result is a `conflict'. One undo step."
         (schema (list (cons "document" document-prop)
                       (cons "node" (prop "string" "Handle of the node to delete."))
                       (cons "expect" (prop "string" "The hash of the node, as read.")))
                 '("document" "node"))
         tool-delete #t)
   (list "edit_text" "Replace some text inside a node"
         "Replaces one occurrence of a text by another inside a node, keeping the markup around it and the other text of the paragraph: the way to change a word or a sentence. The old text must occur exactly once in the node, in one stretch of text without markup inside (an error tells how many times it occurs). `expect` is the hash of the node as read. One undo step."
         (schema (list (cons "document" document-prop)
                       (cons "node" (prop "string" "Handle of the node (a paragraph, a theorem...)."))
                       (cons "old" (prop "string" "The text to replace (plain text, once in the node)."))
                       (cons "new" (prop "string" "The new text (may be empty)."))
                       (cons "expect" (prop "string" "The hash of the node, as read.")))
                 '("document" "node" "old" "new"))
         tool-edit-text #t)
   (list "undo_agent" "Undo your own last steps"
         "Undoes the last steps (tool calls that edited) of this session in the document, newest first, as one step. It never touches the changes of others: it is refused, and nothing is done, if someone changed what a step made since."
         (schema (list (cons "document" document-prop)
                       (cons "steps" (prop "integer" "How many steps (default 1).")))
                 '("document"))
         tool-undo-agent #t)
   (list "save_document" "Save a document"
         "Saves the document to its file (asks the host when the file is shared)."
         (schema (list (cons "document" document-prop)) '("document"))
         tool-save-document #t)
   (list "close_document" "Close a document"
         "Closes the document in this session (a shared file is left, the others keep it). Refused if it has unsaved changes, unless discard is true."
         (schema (list (cons "document" document-prop)
                       (cons "discard" (prop "boolean" "Close although there are unsaved changes.")))
                 '("document"))
         tool-close-document #t)
   (list "create_document" "Create a document"
         "Creates a new .tm file (the path must be absolute, the folder must exist, the file must not) with a style, optionally with initial content, and opens it."
         (schema (list (cons "path" (prop "string" "Absolute path of the new file, ending in .tm."))
                       (cons "style" (prop "string" "Document style, e.g. generic (default), article, book, beamer."))
                       (cons "content" (prop "string" "Initial content (optional)."))
                       (cons "format" (prop "string" "Format of the content: texmacs (default), stree, latex or markdown.")))
                 '("path"))
         tool-create-document #t)))

(define (tool-name t) (car t))

(define (editing-tool? t) (and (> (length t) 5) (list-ref t 5)))

(define (tools-list-result)
  (obj "tools"
       (arr (map (lambda (t)
                   (obj "name" (car t)
                        "title" (cadr t)
                        "description" (caddr t)
                        "inputSchema" (cadddr t)
                        "annotations" (obj "readOnlyHint" (bool (not (editing-tool? t)))
                                           "destructiveHint" (bool (and (editing-tool? t)
                                                                        (member (car t) '("replace" "delete" "undo_agent"))
                                                                        #t))
                                           "idempotentHint" (bool (not (editing-tool? t)))
                                           "openWorldHint" (bool #f))))
                 (list-filter mcp-tools
                              (lambda (t) (or (not (editing-tool? t))
                                              (not (mcp-read-only?)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define supported-versions '("2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05"))
(define latest-version "2025-06-18")

(define server-instructions
  "TeXmacs documents. Start with list_documents or open_document, then get_outline to see the structure of a long document, then read (format text is the cheapest; texmacs is exact) the blocks you need, using the handles of earlier results. find locates text, tags (tag:theorem), labels and sections; get_references lists labels, references and citations and the broken ones. A file the person has open in TeXmacs is joined live, so reads show their latest edits. Editing: replace, insert, delete and edit_text (the best for changing a word or a sentence) take the handle of a node and, for safety, `expect', the hash you read it with: if someone changed the node meanwhile you get a `conflict' with its current content and nothing is changed. Each call is one undo step, all or nothing; undo_agent undoes yours. Answers may carry a `note' when the document changed since your last call: read again what it names. save_document writes the file.")

(define (result-message id result)
  (obj "jsonrpc" "2.0" "id" id "result" result))

(define (error-message id code message)
  (obj "jsonrpc" "2.0" "id" (if id id jnull)
       "error" (obj "code" (num code) "message" message)))

(define (send! msg)
  (let ((s (json-write msg)))
    (log-debug "-> " (utf8-prefix s 300))
    (when (not (mcp-stdout-write (string-append s "\n")))
      ;; the client is gone
      (mcp-log "cannot write to the client")
      (mcp-finish!))))

(define (text-result text error?)
  (obj "content" (arr (list (obj "type" "text" "text" text)))
       "isError" (bool error?)))

(define mcp-param-error-message #f)

(define (param-error msg)
  ;; an argument of the wrong type: a JSON-RPC error (-32602)
  (set! mcp-param-error-message msg)
  (throw 'mcp-param-error))

(define (call-tool name args)
  ;; the result of the tool @name, 'unknown, or (invalid . message)
  (let ((tool (list-find mcp-tools (lambda (t) (== (car t) name)))))
    (if (not tool)
        'unknown
        (catch #t
          (lambda ()
            (set! mcp-param-error-message #f)
            (when (and (editing-tool? tool) (mcp-read-only?)) (require-writable))
            (let* ((dref (let ((x (jref args "document"))) (and (string? x) x)))
                   (d (and dref (find-doc dref)))
                   (note (and d (begin (doc-refresh! d) (change-note d))))
                   (r ((list-ref tool 4) args)))
              (text-result (json-write (if (and note (func? r 'attr))
                                           (append r (list "note" note))
                                           r))
                           #f)))
          (lambda args*
            (cond ((and (pair? args*) (eq? (car args*) 'mcp-tool-error))
                   (text-result mcp-tool-error-message #t))
                  ((and (pair? args*) (eq? (car args*) 'mcp-param-error))
                   (cons 'invalid mcp-param-error-message))
                  (else
                    (mcp-log "error in tool " name ": " args*)
                    (text-result (string-append "internal error in " name)
                                 #t))))))))

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
            (a (jref params "arguments"))
            (args (if (func? a 'attr) a (obj))))
       (cond ((not name) (error-message id -32602 "missing tool name"))
             ((and a (not (jnull? a)) (not (func? a 'attr)))
              (error-message id -32602 "the arguments must be an object"))
             (else
               (let ((r (call-tool name args)))
                 (cond ((eq? r 'unknown)
                        (error-message id -32602
                                       (string-append "unknown tool: " name)))
                       ((and (pair? r) (eq? (car r) 'invalid))
                        (error-message id -32602 (cdr r)))
                       (else (result-message id r))))))))
    ((== method "resources/list")
     (result-message id (obj "resources" (arr '()))))
    ((== method "prompts/list")
     (result-message id (obj "prompts" (arr '()))))
    (else (error-message id -32601 (string-append "method not found: " method)))))

(define (valid-id? id)
  (or (string? id) (func? id 'json-number 1)))

(tm-define (mcp-handle-line line)
  (:synopsis "Handle a message (a line of JSON) of the client, give the response")
  ;; (the response is an stree, or #f when there is none: a notification)
  (let ((msg (json-parse line)))
    (cond ((not msg) (error-message #f -32700 "parse error"))
          ((not (func? msg 'attr))
           (error-message #f -32600 "invalid request"))
          (else
            (let ((id (jref msg "id")) (method (jref msg "method")))
              (cond ((and id (not (jnull? id)) (not (valid-id? id)))
                     (error-message #f -32600 "invalid request: bad id"))
                    ((and (not method)
                          (or (jref msg "result") (jref msg "error")))
                     ;; a response to a request of ours (we make none)
                     #f)
                    ((not (string? method))
                     (error-message (and (valid-id? id) id) -32600
                                    "invalid request: no method"))
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

;; The pieces of the line being read are kept in a list and only the new piece
;; is searched for a newline, so that a long line costs no more than its length.

(define mcp-chunks '())                     ; the pieces of the line (reversed)
(define mcp-chunks-length 0)
(define mcp-skipping? #f)                   ; after a line which was too long
(define mcp-running? #f)
(define mcp-max-line 33554432)
(define mcp-last-activity 0)

(define (strip-cr line)
  (if (and (> (string-length line) 0)
           (== (string-ref line (- (string-length line) 1)) #\return))
      (substring line 0 (- (string-length line) 1))
      line))

(define (process-line! line)
  (let ((line (strip-cr line)))
    (when (!= line "")
      (let ((r (mcp-handle-line line)))
        (when r (send! r))))))

(define (take-line! piece)
  ;; the line that ends with the beginning of @piece
  (let ((line (apply string-append (reverse (cons piece mcp-chunks)))))
    (set! mcp-chunks '())
    (set! mcp-chunks-length 0)
    line))

(define (feed! s)
  ;; @s was read from the input
  (let loop ((s s))
    (when (!= s "")
      (let ((i (string-search-forwards "\n" 0 s)))
        (cond (mcp-skipping?
               ;; (the end of a line which was dropped)
               (when (>= i 0)
                 (set! mcp-skipping? #f)
                 (loop (substring s (+ i 1) (string-length s)))))
              ((>= i 0)
               (if (> (+ mcp-chunks-length i) mcp-max-line)
                   (begin
                     (mcp-log "a line is too long, dropped")
                     (set! mcp-chunks '())
                     (set! mcp-chunks-length 0)
                     (send! (error-message #f -32600 "message too long")))
                   (let ((line (take-line! (substring s 0 i))))
                     (process-line! line)))
               (loop (substring s (+ i 1) (string-length s))))
              (else
                (set! mcp-chunks (cons s mcp-chunks))
                (set! mcp-chunks-length (+ mcp-chunks-length (string-length s)))
                (when (> mcp-chunks-length mcp-max-line)
                  (mcp-log "a line is too long, dropped")
                  (set! mcp-chunks '())
                  (set! mcp-chunks-length 0)
                  (set! mcp-skipping? #t)
                  (send! (error-message #f -32600 "message too long")))))))))

(define (mcp-finish!)
  (when mcp-running?
    (mcp-log "exit")
    (set! mcp-running? #f)
    (for (d mcp-docs)
      (catch #t
        (lambda () (buffer-close (doc-url d)))
        (lambda args (noop))))
    (set! mcp-docs '())
    (quit-TeXmacs)))

(define (mcp-tick)
  (if (not mcp-running?)
      #t
      (catch #t
        (lambda ()
          (let ((s (mcp-stdin-read)))
            (when (!= s "")
              (set! mcp-last-activity (texmacs-time))
              (feed! s))
            (cond ((not mcp-running?) #t)
                  ((mcp-stdin-eof?)
                   (when (and (not mcp-skipping?) (> mcp-chunks-length 0))
                     (process-line! (take-line! "")))
                   (mcp-finish!)
                   #t)
                  ;; (polling is cheaper when nothing has come for a while)
                  ((< (- (texmacs-time) mcp-last-activity) 2000) 10)
                  (else 50))))
        (lambda args
          (mcp-log "error in the loop: " args)
          10))))

(tm-define (mcp-server-start)
  (:synopsis "Serve MCP on the standard input and output, until the input ends")
  (when (not mcp-running?)
    (set! mcp-running? #t)
    (shared-set-agent! #t)
    (set! mcp-last-activity (texmacs-time))
    (mcp-log "started, " (texmacs-version))
    (exec-delayed-pause mcp-tick)))
