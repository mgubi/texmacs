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

(define (doc-not-joined d)
  ;; why the file is open in the person's TeXmacs and this server has not
  ;; joined it (then it is read-only), or #f
  (shared-file-not-joined (doc-url d)))

(define (doc-agent-mode d)
  ;; what the host of the file does with this agent's edits: propose (they
  ;; become proposals of the person to accept), direct, read-only; direct
  ;; when nobody else has the file
  (let ((u (doc-url d)))
    (cond ((shared-file-participant? u)
           (let ((m (shared-agent-mode u))) (if m (symbol->string m) "unknown")))
          ((doc-not-joined d) "read-only")
          (else "direct"))))

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
         "agent_mode" (doc-agent-mode d)
         "blocks" (num (tree-arity (doc-body d)))
         "hash" (hash-short (doc-body d)))))

(define (not-joined-note d)
  (let ((w (doc-not-joined d))) (and w (string-append w "; read-only"))))

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
      (let ((i (doc-info d)) (n (not-joined-note d)))
        (if (and n (func? i 'attr)) (append i (list "note" n)) i)))))

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

(define cur-node #f)           ; the node whose rule is being applied

(define (node-key)
  ;; the whole content of the node, hashed: two violations of the same rule in
  ;; the same place are the same only if the node is the same
  (if cur-node (tree-hash (stree->tree cur-node)) "-"))

(define (violate! msg)
  (if collecting?
      (set! collected (cons (string-append msg " #" (node-key)) collected))
      (tool-error msg)))

(define (reason-text r)
  ;; a reason without the key of its node
  (let ((i (let loop ((k (- (string-length r) 1)))
             (cond ((< k 0) #f) ((char=? (string-ref r k) #\#) k) (else (loop (- k 1)))))))
    (if (and i (> i 0)) (substring r 0 (- i 1)) r)))

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
           (set! cur-node st)
           (when (and (not (== tag 'document))
                      (or (known-primitive? tag) (known-macro-arity? tag))
                      (not (tag-possible-arity? tag n)))
             (violate! (string-append "the tag `" (symbol->string tag)
                                        "' cannot have " (number->string n)
                                        " argument(s) (nothing was changed)")))
           (when (table-structure-bad? st)
             (violate! (string-append "the tag `" (symbol->string tag)
                                        "' has a string where a row, a cell or a table is needed (nothing was changed)")))
           (if (memq tag '(macro xmacro))
               (with-global in-macro-body? #t (for (c (cdr st)) (check-arity c d)))
               (for (c (cdr st)) (check-arity c d)))))
        (else #t)))

(define (table-structure-bad? st)
  ;; What the typesetter and the editor cannot take (found by experiment: the editor
  ;; crashes on a key press at the edge of a table whose children are not all rows,
  ;; whatever they are, a macro that expands to a row included, and on a row insertion
  ;; in a tformat that has a string after its table): a table with no row, a row with no
  ;; cell, a child of a table that is not a `row' with something in it, a tformat that
  ;; does not end with a table (a string is allowed if there is no table in it, and
  ;; inside a macro an `arg' or an `extern', which the corpus has 70 and 6 times), and a document with no
  ;; paragraph. The children of a row are free (a `with', a `document', a string...).
  (let ((tag (car st)) (kids (cdr st)))
    (or (and (eq? tag 'document) (null? kids))   ; (the editor puts a document with an empty paragraph in its place)
        (and (memq tag '(table row)) (null? kids))
        (and (eq? tag 'table)
             (or (list-or (map (lambda (k) (not (and (pair? k) (eq? (car k) 'row) (pair? (cdr k))))) kids))
                 ;; rows of unequal length (the editor aborts on a column operation in one whose
                 ;; first row is wider than a later one, and the corpus has none)
                 (not (list-and (map (lambda (k) (= (length k) (length (car kids)))) kids)))))
        (and (eq? tag 'tformat) (pair? kids)
             (let ((l (cAr kids)))
               (cond ((string? l) (list-or (map (lambda (k) (and (pair? k) (eq? (car k) 'table))) kids)))
                     ((pair? l) (not (or (eq? (car l) 'table)
                                         (and in-macro-body? (memq (car l) '(arg extern))))))
                     (else #t)))))))

(define in-macro-body? #f)   ; (the bodies of macros hold tables whose rows come from arguments)

(tm-define (mcp-stree-arity-ok? st)
  ;; no tag that the DRD knows as a primitive has a number of children it cannot
  ;; have (the bridges of the typesetter index the children of `ornament',
  ;; `canvas', `hlink', `with'...). For any tree that comes from another process.
  (cond ((not (pair? st)) #t)
        ((not (symbol? (car st))) #f)
        ((table-structure-bad? st) #f)
        ((and (memq (car st) '(with cwith twith tformat)) (attribute-problem st)) #f)
        (else
          (and (or (eq? (car st) 'document)
                   ;; (an empty collection is the initial environment of a file that
                   ;; has none set: every file the editor saves has one)
                   (eq? (car st) 'collection)
                   (not (known-primitive? (car st)))
                   (tag-possible-arity? (car st) (length (cdr st))))
               (let loop ((l (cdr st)))
                 (or (not (pair? l))
                     (and (if (memq (car st) '(macro xmacro))
                              (with-global in-macro-body? #t (mcp-stree-arity-ok? (car l)))
                              (mcp-stree-arity-ok? (car l)))
                          (loop (cdr l)))))))))

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
    (cond ((and (== scheme "tmfs") (string-starts? s "tmfs://help/article/tm/doc/")
                (not (dotdot-in-path? s)))
           ;; (the manuals of TeXmacs, the form that the help-link of the manuals
           ;; makes; not tmfs://help/file/ or http/, which open any file)
           #t)
          (scheme (and (member scheme '("http" "https" "mailto")) #t))
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

(define definitional-tags
  '(assign set reset provide macro xmacro func drd-props with-package use-package
    use-module new-theme copy-theme apply-theme select-theme style-with style-with*
    style-only style-only* filter-style))

(define (refuse-tag tag why)
  ;; (a definition of the document is a violation that stays when the person
  ;; changes it: its reason is not tied to the content of the node)
  (when (memq tag definitional-tags) (set! cur-node #f))
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
                                      (utf8-prefix (u8 a) 80)
                                      "': only relative names without .. inside the folders of the server (or of the document), and http(s) images, are accepted (nothing was changed)"))))
        (else
          (violate! (string-append "the tag `" (symbol->string tag)
                                     "' has a file that is not written as a plain name: refused (nothing was changed)")))))

(define safety-defined? #f)
(define safety-doc #f)           ; the document, for the calls of dangerous macros

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

(define (number-bounded? v)
  ;; a value that is a number or a length (a sign, digits, a point, a unit) has at most
  ;; 4 digits before the point and 6 after it: numbers drive loops and sizes
  (let* ((n (string-length v))
         (k (if (and (> n 0) (memv (string-ref v 0) '(#\+ #\-))) 1 0))
         (digits (lambda (from) (let loop ((i from)) (if (and (< i n) (char-numeric? (string-ref v i))) (loop (+ i 1)) i)))))
    (if (and (< k n) (or (char-numeric? (string-ref v k)) (and (char=? (string-ref v k) #\.) (< (+ k 1) n))))
        (let* ((e (digits k))
               (rest-start (if (and (< e n) (char=? (string-ref v e) #\.)) (digits (+ e 1)) e))
               (frac (if (> rest-start e) (- rest-start e 1) 0))
               (tail (substring v rest-start n)))
          ;; (only when the rest is a unit: letters or %, so that 2026-11-04 or 3 pages stay text)
          (if (or (string=? tail "") (list-find (string->list tail) (lambda (c) #f))
                  (let loop ((l (string->list tail)))
                    (or (null? l) (and (or (char-alphabetic? (car l)) (char=? (car l) #\%)) (loop (cdr l))))))
              (and (<= (- e k) 4) (<= frac 6))
              #t))
        #t)))

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
        ((assoc var env-number-limits)
         ;; (a number that multiplies the work of the typesetter: bounded)
         (and (number-string? v) (number-bounded? v)
              (let ((x (string->number v))) (and x (<= x (cdr (assoc var env-number-limits)))))))
        (else (number-bounded? v))))

(define env-number-limits
  ;; the variables whose value multiplies the work: columns, a size factor, the base size
  ;; (in points), the magnification, the resolution
  ;; (magnification and font-size multiply each other: 4 times 8 is the limit of the product)
  '(("par-columns" . 12) ("font-size" . 8) ("font-base-size" . 100)
    ("magnification" . 4) ("dpi" . 2400)))

(define (env-limit-text var)
  (let ((x (assoc var env-number-limits)))
    (string-append (if x (string-append "a number up to " (number->string (cdr x)) " for `" var "'; ") "")
                   "other numbers and lengths have at most 4 digits before the point and 6 after it; ")))

;; Table attributes (found by experiment, with the editor and the typesetter): the
;; decoration of a cell hangs the typesetter, whatever its value; a span that is not a
;; positive integer (or that goes beyond the table) crashes them.
(define (positive-integer-string? v)
  (and (string? v) (> (string-length v) 0) (< (string-length v) 4)
       (list-and (map char-numeric? (string->list v)))
       (> (string->number v) 0)))

(define span-vars '("cell-row-span" "cell-col-span"))

(define (table-attribute-problem tag var val)
  ;; why the table attribute @var may not be set to @val by a @tag, or #f
  (cond ((== var "cell-decoration")
         (string-append "a `" (symbol->string tag) "' sets `cell-decoration': the decoration of a cell is not accepted in the content (nothing was changed)"))
        ((and (member var span-vars)
              (or (not (positive-integer-string? val)) (and (not (eq? tag 'cwith)) (!= val "1"))))
         (string-append "a `" (symbol->string tag) "' sets `" var "' to something that is not a positive whole number"
                        (if (eq? tag 'cwith) "" " (only a cwith of a table can set a span)")
                        ": refused (nothing was changed)"))
        (else #f)))

(define (check-table-attribute tag var val)
  (let ((p (table-attribute-problem tag var val)))
    (when p (violate! p))))

(define (inside-range? a b n)
  ;; is the range a..b of a cwith (1-based, negative from the end) wholly inside n rows or columns?
  (let ((x (string->number a)) (y (string->number b)))
    (and x y (integer? x) (integer? y)
         (let ((i1 (if (>= x 0) (- x 1) (+ n x))) (i2 (if (> y 0) (- y 1) (+ n y))))
           (and (<= 0 i1) (<= i1 i2) (< i2 n))))))

(define (decode-range a b n)
  ;; the first and last cell (0-based) of the range a..b of a cwith in n rows or columns
  (let ((x (string->number a)) (y (string->number b)))
    (and x y (integer? x) (integer? y)
         (cons (max 0 (if (>= x 0) (- x 1) (+ n x)))
               (min (- n 1) (if (> y 0) (- y 1) (+ n y)))))))

(define (spans-problem st)
  ;; the cwith of a tformat that gives a span: it must fit in the table that the tformat
  ;; holds. Why not, or #f.
  (let* ((kids (cdr st))
         (tbl (and (pair? kids) (let ((l (cAr kids))) (and (pair? l) (eq? (car l) 'table) l))))
         (rows (and tbl (cdr tbl)))
         (nr (and rows (length rows)))
         (nc (and rows (pair? rows) (list-and (map (lambda (r) (and (pair? r) (eq? (car r) 'row))) rows))
                  (apply min (map (lambda (r) (length (cdr r))) rows))))
         (problem #f))
    (for (c kids)
      (when (and (not problem) (pair? c) (eq? (car c) 'cwith) (>= (length c) 7))
        (let ((args (cdr c)))
          (let loop ((l (list-tail args 4)))
            (when (and (not problem) (pair? l) (pair? (cdr l)))
              (let ((var (car l)) (val (cadr l)))
                ;; (a row span must fit; a column span may run over the table, as in the corpus,
                ;; where the typesetter clips it: the tables have rows of the same length, see
                ;; table-structure-bad?; a span of 0 or text, as while its value is edited in
                ;; source view, is clamped to 1 by the typesetter)
                ;; (a span whose cells are not all in the table, as written: the editor writes the
                ;; format of the cells in an array of the size of the table and aborts)
                (when (and (string? var) (member var span-vars) nr nc (every-string? (list-head args 4))
                           (not (and (inside-range? (car args) (cadr args) nr)
                                     (inside-range? (caddr args) (cadddr args) nc))))
                  (set! problem
                        (string-append "a `cwith' gives `" var "' to cells that are not all in the table: refused (nothing was changed)")))
                (when (and (not problem) (string? var) (== var "cell-row-span") (positive-integer-string? val)
                           (> (string->number val) 1))
                  (let* ((n (string->number val))
                         (rr (and nr (every-string? (list-head args 4))
                                  (decode-range (car args) (cadr args) nr))))
                    (when (not (and rr (<= (+ (cdr rr) n) nr)))
                      (set! problem
                            (string-append "a `cwith' gives `" var "' " val
                                           " that does not fit in the table (the table must be written next to it, and the cells must have room): refused (nothing was changed)")))))
                (loop (cddr l))))))))
    problem))

(define (check-spans st)
  (let ((p (spans-problem st)))
    (when p (violate! p))))

(define (attribute-problem st)
  ;; why a with, cwith, twith or tformat @st sets a table attribute it may not, or #f
  (let ((tag (car st)) (args (cdr st)))
    (cond ((eq? tag 'tformat) (spans-problem st))
          ((memq tag '(with cwith twith))
           (let* ((pairs (cond ((eq? tag 'with) (if (pair? args) (list-head args (- (length args) 1)) '()))
                               ((eq? tag 'cwith) (if (>= (length args) 4) (list-tail args 4) '()))
                               (else args))))
             (let loop ((l pairs))
               (and (pair? l) (pair? (cdr l))
                    (or (and (string? (car l)) (table-attribute-problem tag (car l) (cadr l)))
                        (loop (cddr l)))))))
          (else #f))))

(define (every-string? l) (list-and (map string? l)))

(define (check-env-pairs tag vars-and-values)
  ;; (var value var value ...) of a with, cwith or twith
  (let loop ((l vars-and-values))
    (when (and (pair? l) (pair? (cdr l)))
      (let ((var (car l)) (val (cadr l)))
        (when (string? var) (check-table-attribute tag var val))
        (when (string? var)
          (when (and (not (string? val)) (pair? val)
                     (or (assoc var env-number-limits)
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
                                       "' (" (env-limit-text var)
                                       "letters, digits, blanks and . _ - + * / % # , : = only; a colour is #hex or a name, a size a number) (nothing was changed)")))))
      (loop (cddr l)))))

(define (check-link-destination tag a)
  ;; the destination @a of a link made by the tag @tag, as written
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
                                    "' is refused: only http, https, mailto and relative targets are accepted (nothing was changed)")))))

(define (check-safety st base)
  ;; refuse content with markup that can run code, loop, or read what it
  ;; should not
  (cond
    ((string? st) #t)
    ((pair? st)
     (let ((tag (car st)))
       ;; (the arguments first: what is inside is named in the reason)
       (for (c (cdr st)) (check-safety c base))
       (set! cur-node st)
       ;; (version markup is what the person's TeXmacs writes for a proposal)
       (when (memq tag '(version-both version-old version-new version-suppressed))
         (violate! (string-append "version markup (`" (symbol->string tag)
                                    "') is written by the person's TeXmacs for a proposal: the content may not contain it (nothing was changed)")))
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
       ;; (a macro that passes an argument to a link, a file, a script or a
       ;; macro name is not called by the content, with a few standard exceptions)
       (when (and safety-doc (not (known-primitive? tag)) (not (assq tag code-reasons))
                  (dangerous-call? safety-doc st))
         (violate! (string-append "a call of the macro `" (symbol->string tag)
                                    "' is refused: it passes an argument (or the value of a variable) to a link, a file, a script or the name of a macro, which the filter cannot check (nothing was changed)")))
       ;; (aliases, and what is written into the exports as attributes)
       (cond ((eq? tag 'with)
              (when (pair? (cdr st))
                (check-env-pairs tag (list-head (cdr st) (- (length (cdr st)) 1)))))
             ((eq? tag 'cwith)
              (when (>= (length (cdr st)) 6)
                (check-env-pairs tag (list-tail (cdr st) 4))))
             ((eq? tag 'twith)
              (check-env-pairs tag (cdr st)))
             ((eq? tag 'tformat) (check-spans st))
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
             (when a (check-link-destination tag a)))))
       ;; (a standard macro of the table that passes an argument to a link or a
       ;; file: the argument is checked here, with the rules of the tags)
       (when (and safety-doc (not (known-primitive? tag)) (safe-macro? safety-doc tag))
         (check-safe-macro-arguments st base))
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
  (cond ((and (string? a) (string-starts? a "local:$TEXMACS_PATH/misc/images/")
              (not (dotdot-in-path? a)))
         ;; (an image of the installation of TeXmacs, such as the logo of the titles of the manuals)
         #t)
        ((and (string? a) (string-starts? a "/") (not (dotdot-in-path? a)))
         (let ((p (resolve-existing a)))
           (when (not (if (mcp-roots)
                          (path-allowed? p)
                          (inside-folder? p (document-folder base))))
             (violate! (string-append "the content expands to a `" (symbol->string tag)
                                        "' of the file `" (utf8-prefix (u8 a) 80)
                                        "', outside the folders of the server (or of the document): refused (nothing was changed)")))))
        (else (check-file-argument tag a base (and (memq tag web-file-tags) #t)))))

(define benign-errors '("bad assign"))

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
       ;; (what the evaluator makes has generated ids: a violation here is keyed by
       ;; its reason and by the whole content that matters, the destination, the
       ;; file, the text of the script or of the LaTeX, without those ids)
       (set! cur-node (cons tag args))
       (cond
         ((and (eq? tag 'link) (>= (length args) 3))
          (let* ((dest (car (reverse args))) (u (url-destination dest)))
            (set! cur-node dest)
            (cond ((func? dest 'script)
                   (when (not (and (pair? (cdr dest)) (member (cadr dest) allowed-scripts)))
                     (violate! (string-append "the content expands to a link that runs a script: "
                                              (if (and (pair? (cdr dest)) (string? (cadr dest))) (utf8-prefix (u8 (cadr dest)) 80) "?")
                                              " (nothing was changed)"))))
                  ((not u)
                   (when (and (pair? dest) (not (memq (car dest) '(id tuple))))
                     (violate! "the content expands to a link whose destination is not plain text: refused (nothing was changed)")))
                  ((or (not (name-chars-ok? u 'address)) (not (link-target-ok? u)))
                   (violate! (string-append "the content expands to a link to `" (utf8-prefix (u8 u) 80)
                                              "': only http, https, mailto and relative targets are accepted (nothing was changed)"))))))
         ((and (eq? tag 'hlink) (= (length args) 2))
          (set! cur-node (cadr args))
          (let ((u (url-destination (cadr args))))
            (when (or (not u) (not (name-chars-ok? u 'address)) (not (link-target-ok? u)))
              (violate! (string-append "the content expands to a link whose destination is not accepted: "
                                       (if u (utf8-prefix (u8 u) 80) "?") " (nothing was changed)")))))
         ((memq tag '(image postscript postscript* include include* pattern video sound))
          (when (pair? args)
            (set! cur-node (car args))
            (check-expanded-file tag (car args) base)))
         ((eq? tag 'script)
          (when (not (and (pair? args) (member (car args) allowed-scripts)))
            (violate! (string-append "the content expands to a script: "
                                     (if (and (pair? args) (string? (car args))) (utf8-prefix (u8 (car args)) 80) "?")
                                     " (nothing was changed)"))))
         ((eq? tag 'action)
          (when (not (and (>= (length args) 2) (member (cadr args) allowed-scripts)))
            (violate! (string-append "the content expands to an action: "
                                     (if (and (>= (length args) 2) (string? (cadr args))) (utf8-prefix (u8 (cadr args)) 80) "?")
                                     " (nothing was changed)"))))
         ((eq? tag 'error)
          ;; (an error is the evaluator failing, and what it dropped is not checked:
          ;; refused, except the messages that standard macros give in the context
          ;; of the evaluator and that are no danger: `bad assign' of the counters
          ;; of the chapters of the book style)
          (when (not (and (pair? args) (member (car args) benign-errors)))
            (violate! (string-append "the content expands to an error ("
                                     (if (and (pair? args) (string? (car args))) (utf8-prefix (u8 (car args)) 60) "?")
                                     "): what it dropped cannot be checked, so the content is refused (nothing was changed)"))))
         ((memq tag '(extern hybrid authorize observer eval-args))
          (refuse-tag tag "is left after the expansion of the content"))
         ((eq? tag 'specific)
          (when (not (and (pair? args) (member (car args) '("texmacs" "screen" "printer" "screen*" "printer*" "even" "odd"))))
            (violate! (string-append "the content expands to a text for another program (specific): "
                                     (if (and (pair? args) (string? (car args))) (utf8-prefix (u8 (car args)) 80) "?")
                                     (if (and (pair? args) (pair? (cdr args)) (string? (cadr args))) (string-append " " (utf8-prefix (u8 (cadr args)) 80)) "")
                                     " (nothing was changed)"))))
         ((eq? tag 'latex)
          (for (a args)
            (when (string? a)
              (check-latex-source a)
              (for (c (latex-commands a))
                (when (member c latex-tag-forbidden)
                  (violate! "the content expands to LaTeX that takes a file or an address: refused (nothing was changed)")))))))))
    (else #t)))

(define (assign-nodes t)
  ;; the definitions of the document: its assigns, and its sets and resets
  ;; of the environment (the context of the blocks after them)
  ;; the assigns of the document (its preamble), as strees
  (cond ((tree-atomic? t) '())
        ((or (tm-is? t 'assign) (tm-is? t 'set) (tm-is? t 'reset)) (list (tm->stree t)))
        (else (append-map assign-nodes (tree-children t)))))

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
  (let ((locals (if host-cache
                    (append-map assigned-names-st (apply append host-cache))
                    (assigned-names (doc-body d)))))
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

(define block-budget 1500000)        ; steps of the evaluator for one block
(define document-budget 12000000)    ; steps for the blocks of a whole comparison
(define document-size-limit 3000000) ; nodes of the results of a whole comparison
(define batch-size 20000)            ; nodes of the blocks evaluated together

(define (stree-size x)
  (if (pair? x) (+ 1 (apply + (map stree-size (cdr x)))) 1))

(define over-budget-reason "the content does not evaluate within the budget (it expands too much)")
(define cannot-evaluate-reason "the content cannot be evaluated")

(define (over-budget? reasons)
  (list-find reasons (lambda (r) (or (string-starts? r over-budget-reason)
                                     (string-starts? r cannot-evaluate-reason)))))

(define (context-definitions defs)
  ;; the definitions of the document as the evaluator takes them: a `set' of the
  ;; person (the evaluator does not do it, the typesetter does) is an assign of
  ;; the variable, a `reset' is left out
  (filter-map (lambda (a)
                (cond ((eq? (car a) 'set) (cons 'assign (cdr a)))
                      ((eq? (car a) 'reset) #f)
                      (else a)))
              defs))

(define (expand-blocks d blocks-st assigns budget)
  ;; the expansions (strees, in the order) of the strees @blocks-st in the
  ;; environment of the document after @assigns; (steps-used . results), or
  ;; #f if the budget was exceeded or the evaluation failed
  (let* ((u (doc-url d))
         (r (catch #t
              (lambda ()
                (exec-budget-start budget)
                ;; (the assigns come first, to define; the blocks follow, each in a
                ;; document of its own, which the results keep apart)
                (with-buffer u
                  (tm->stree (texmacs-exec (stree->tree
                                            (cons 'concat
                                                  (append (context-definitions assigns)
                                                          (map (lambda (b) (list 'document b))
                                                               blocks-st))))))))
              (lambda args #f)))
         (left (exec-budget-end)))
    (cond ((< left 0) #f)
          ((not r) #f)
          (else
            (cons (- budget left)
                  (let ((items (if (and (pair? r) (eq? (car r) 'concat)) (cdr r) (list r))))
                    (list-filter items (lambda (x) (func? x 'document)))))))))

(define (expansion-violations st base d assigns budget)
  ;; the violations of the rules for the expansion of the stree @st
  (let ((r (expand-blocks d (list st) assigns budget)))
    (cond ((not r) (list over-budget-reason))
          (else (collect-violations
                 (lambda () (for (x (cdr r)) (check-expanded x base))))))))

(define (literal-violations st base d)
  (set! safety-defined? (and d (defined-predicate d)))
  (set! safety-doc d)
  (let ((r (collect-violations (lambda () (check-safety st base) (check-arity st d)))))
    (set! safety-defined? #f)
    (set! safety-doc #f)
    r))

(define host-cache #f)   ; during the check of a host: the definitions of each block

(define (assigned-names-st st)
  (cond ((not (pair? st)) '())
        ((and (eq? (car st) 'assign) (pair? (cdr st)) (string? (cadr st)))
         (cons (cadr st) (append-map assigned-names-st (cddr st))))
        (else (append-map assigned-names-st (cdr st)))))

(define (assigns-before d i)
  ;; the definitions of the blocks before the block number @i
  (if host-cache
      (apply append (sublist host-cache 0 (min i (length host-cache))))
      (assigns-before* d i)))

(define (assigns-before* d i)
  (let loop ((l (tree-children (doc-body d))) (k 0) (acc '()))
    (if (or (null? l) (>= k i))
        acc
        (loop (cdr l) (+ k 1) (append acc (assign-nodes (car l)))))))

(define (block-literal-violations d blk i)
  ;; the violations of the top-level block @blk as written (nothing is evaluated)
  (let ((st (tm->stree blk)) (base (doc-base d)))
    (reset-danger!)
    (literal-violations st base d)))

(define (block-violations d blk i . opt)
  ;; the violations of the top-level block @blk (a tree) of index @i; with the
  ;; violations as written, when they are known
  (let* ((st (tm->stree blk)) (base (doc-base d))
         (lit (if (pair? opt) (car opt) (block-literal-violations d blk i))))
    (append lit (expansion-violations st base d (assigns-before d i) block-budget))))

(define (document-literal-violations d)
  ;; the violations of the blocks as written (nothing is evaluated)
  (reset-danger!)
  (let ((base (doc-base d)))
    (append-map (lambda (blk) (literal-violations (tm->stree blk) base d))
                (tree-children (doc-body d)))))

(define (document-violations d . opt)
  ;; the violations of the blocks of the document, evaluated by batches whose
  ;; results are discarded at once (the memory stays low); with a predicate,
  ;; only the blocks it accepts. Beyond the budgets: a violation.
  (reset-danger!)
  (let* ((select (if (pair? opt) (car opt) #f))
         (base (doc-base d))
         (viol '()) (used 0) (total 0) (failed #f)
         (pre '()) (batch '()) (batch-assigns '()) (size 0))
    (define (flush!)
      (when (and (pair? batch) (not failed))
        (let ((r (expand-blocks d (reverse batch) pre block-budget-for-batch)))
          (if (not r)
              (set! failed #t)
              (begin
                (set! used (+ used (car r)))
                (set! total (+ total (apply + (map stree-size (cdr r)))))
                (set! viol (append viol (collect-violations
                                         (lambda () (for (x (cdr r)) (check-expanded x base))))))
                (when (or (> used document-budget) (> total document-size-limit))
                  (set! failed #t))))))
      (set! pre (append pre batch-assigns))
      (set! batch '()) (set! batch-assigns '()) (set! size 0))
    (for (blk (tree-children (doc-body d)))
      (when (not failed)
        (if (or (not select) (select blk))
            (let ((st (tm->stree blk)))
              (set! viol (append viol (literal-violations st base d)))
              (when (and (pair? batch) (> (+ size (stree-size st)) batch-size))
                (flush!))
              (set! batch (cons st batch))
              (set! batch-assigns (append batch-assigns (assign-nodes blk)))
              (set! size (+ size (stree-size st))))
            (begin
              (flush!)
              (set! pre (append pre (assign-nodes blk)))))))
    (flush!)
    (if failed (cons over-budget-reason viol) viol)))

(define block-budget-for-batch 6000000)

(define (assigns-signature d)
  (tree-hash (stree->tree (cons 'tuple (assign-nodes (doc-body d))))))

(define (new-violations before after)
  ;; the reasons of @after that are not in @before (with their number)
  (let loop ((l after) (pool before) (acc '()))
    (cond ((null? l) (reverse acc))
          ((member (car l) pool)
           (loop (cdr l) (let rm ((p pool)) (cond ((null? p) '()) ((equal? (car p) (car l)) (cdr p)) (else (cons (car p) (rm (cdr p)))))) acc))
          (else (loop (cdr l) pool (cons (car l) acc))))))

;; The tags whose content is code or text for another program: an edit inside
;; them, or of them, is refused, whatever the violations before and after
;; (a violation that was there is hidden by a change inside the same node).
(define opaque-tags
  '(extern script eval eval-args quasi quasiquote unquote unquote* hybrid authorize
    observer action compound apply expand expand* hide-expand begin end map-args
    find-file find-file-upwards latex session input output raw-data))

(define (opaque-node? t)
  (and (tree-compound? t)
       (let ((tag (tree-label t)))
         (or (memq tag opaque-tags)
             (suffix-reason tag)
             (and (eq? tag 'specific)
                  (not (and (> (tree-arity t) 0) (tree-atomic? (tree-ref t 0))
                            (member (tree->string (tree-ref t 0)) '("texmacs" "screen" "printer")))))))))

(define (inside-opaque? d t self?)
  ;; is @t (if @self?) or one of its ancestors up to the block an opaque node?
  (let loop ((x (if self? t (tree-up t))))
    (and x (tree? x) (inside-body? d x) (not (body-root? d x))
         (or (opaque-node? x) (loop (tree-up x))))))

;;; Macros that pass an argument to something sensitive (static, no evaluation)
;;; The evaluator is not the typesetter, so what it makes keeps missing things.
;;; So, besides the other rules, the content may not call a macro whose body,
;;; with the macros it calls (the style's and the document's, read at the time of
;;; the check), carries an argument (or the value of a variable, which the content
;;; can set with `with') to a link destination, a file, a script, a command or the
;;; name of a macro. The kind of the place is kept: link, file, code, name, or pass
;;; (an argument handed to a macro called by a computed name, which can be any
;;; macro of the style). The standard macros of `safe-macros' are the exception:
;;; the arguments that they pass to a link or a file are checked as such where the
;;; call is written (check-safe-macro-arguments), derived from this analysis, and a
;;; test checks that no other kind remains but the declared ones.

(define sensitive-positions
  ;; (tag kind . which arguments are in such a place: a position, last, all)
  '((hlink link 1) (link link last) (url link 0)
    (image file 0) (postscript file 0) (postscript* file 0) (include file 0)
    (include* file 0) (pattern file 0) (video file 0) (sound file 0)
    (attachment file 0) (ocr file 0) (bibliography file all)
    ;; (action: the script is the second argument; extern: see extern-functions)
    ;; (the content may not define anything: a name that is made of an argument)
    (assign define 0) (set define 0) (reset define 0) (provide define 0) (drd-props define 0)
    (script code all) (action code (1)) (hybrid code all)
    (authorize code all) (observer code all) (eval code all) (eval-args code all)
    (specific code all) (latex code all)
    (find-file code all) (find-file-upwards code all)
    (compound name 0) (apply name 0) (expand name 0) (expand* name 0) (hide-expand name 0)))

(define call-tags '(compound apply expand expand* hide-expand map-args))

(define safe-prefixes '("#" "http://" "https://" "mailto:"))

(define (var-source? e)
  ;; does the expression read an environment variable (that the content can set)?
  (and (pair? e)
       (or (memq (car e) '(value quote-value or-value get-binding))
           (list-find (cdr e) var-source?))
       #t))

(define (param-refs e params)
  ;; the parameters (strings) that the expression @e uses
  (cond ((not (pair? e)) '())
        ((and (memq (car e) '(arg quote-arg)) (pair? (cdr e)) (string? (cadr e)))
         (if (member (cadr e) params) (list (cadr e)) '()))
        (else (apply append (map (lambda (c) (param-refs c params)) (cdr e))))))

(define (fixed-prefix? e)
  ;; does @e begin with an address that cannot be a script (written, or merged)?
  (cond ((and (func? e 'url) (pair? (cdr e))) (fixed-prefix? (cadr e)))
        ((string? e) (and (list-find safe-prefixes (lambda (p) (string-starts? e p))) #t))
        ((and (func? e 'merge) (pair? (cdr e))) (fixed-prefix? (cadr e)))
        (else #f)))

(define macro-table #f)          ; the local definitions: name -> definition
(define danger-cache #f)         ; name -> positions (solved)
(define def-cache #f)            ; name -> macro tree, or 'none
(define void-cache #f)           ; name -> does its closure have a local macro
(define danger-doc #f)

(define (reset-danger!)
  (set! macro-table #f)
  (set! danger-cache #f)
  (set! def-cache #f)
  (set! void-cache #f))

(define (danger-tables! d)
  (when (or (not macro-table) (not (eq? danger-doc d)))
    (set! danger-doc d)
    (set! macro-table (make-ahash-table))
    (set! danger-cache (make-ahash-table))
    (set! def-cache (make-ahash-table))
    (set! void-cache (make-ahash-table))
    (for (a (if host-cache (apply append host-cache) (assign-nodes (doc-body d))))
      (when (and (eq? (car a) 'assign) (pair? (cdr a)) (string? (cadr a)) (pair? (cddr a)))
        (ahash-set! macro-table (cadr a) (caddr a))))))

(define (local-macro? name)
  (and macro-table
       (let ((x (ahash-ref macro-table name)))
         (and (pair? x) (memq (car x) '(macro xmacro)) #t))))

(define (macro-definition d name)
  ;; the macro tree of @name for the document: its own assign, else the
  ;; style's; #f if there is none
  (let ((c (ahash-ref def-cache name)))
    (cond ((eq? c 'none) #f)
          (c c)
          (else
            (let* ((def (if (ahash-ref macro-table name)
                            (ahash-ref macro-table name)
                            (with-buffer (doc-url d)
                              (let ((x (tm->stree (get-init-tree name)))) (and (pair? x) x)))))
                   (r (and (pair? def) (memq (car def) '(macro xmacro)) def)))
              (ahash-set! def-cache name (or r 'none))
              r)))))

(define (macro-parameters def)
  (if (eq? (car def) 'xmacro)
      (list (cadr def))
      (reverse (cdr (reverse (cdr def))))))

(define (literal-names e)
  ;; the names that a name expression can have, if it is a written name or a
  ;; choice (if) between written names; else #f
  (cond ((string? e) (list e))
        ((and (func? e 'if) (= (length (cdr e)) 3))
         (let ((x (literal-names (caddr e))) (y (literal-names (cadddr e))))
           (and x y (append x y))))
        ((and (func? e 'if) (= (length (cdr e)) 2))
         (literal-names (caddr e)))
        (else #f)))

(define (local-names-with-prefix prefix)
  (list-filter (map car (ahash-table->list macro-table))
               (lambda (n) (and (string-starts? n prefix) (local-macro? n)))))

(define (called-names body)
  ;; the names of the macros that @body can call: tags that are not primitives,
  ;; the names of the compounds (the local macros that begin as a computed name)
  (let ((out '()))
    (define (add! n) (when (not (member n out)) (set! out (cons n out))))
    (define (visit e)
      (when (and (pair? e) (symbol? (car e)))
        (let ((tag (car e)) (args (cdr e)))
          (cond ((memq tag '(compound apply expand expand* hide-expand))
                 (when (pair? args)
                   (let ((ns (literal-names (car args))))
                     (if ns
                         (for (n ns) (add! n))
                         (let ((prefix (fixed-name-prefix (car args))))
                           (for (n (if prefix (local-names-with-prefix prefix) (map car (ahash-table->list macro-table))))
                             (add! n)))))))
                ((eq? tag 'map-args) (when (and (pair? args) (string? (car args))) (add! (car args))))
                ((memq tag '(macro xmacro func)) #f)
                ((known-primitive? tag) #f)
                (else (add! (symbol->string tag)))))
        (for (c (cdr e)) (visit c))))
    (visit body)
    out))

;; The reviewed list of the functions of `extern' (the macros of the standard styles and
;; packages that call a Scheme function on their arguments): the arguments of a
;; function on the list are data, those of the other ones count as code. For each
;; function: its name, why it is harmless, and the places of its arguments (after the name) that
;; are the names of macros that it calls. Each function was read in TeXmacs/progs:
;; builders of trees from the arguments, without evaluation, files, or commands.
(define extern-functions
  '(("doc-data" "builds the title block (select and tree builders: title-markup.scm)")
    ("author-data" "builds the author block (select)")
    ("abstract-data" "builds the abstract (select)")
    ("doc-data-exam" "the title block of the exam style, doc-data with a prefix")
    ("ext-concat-tuple" "concatenation of the children of a tuple with a separator")
    ("ext-tm-ref" "a child of the tree by a path of up, next, previous, first, last or numbers")
    ("ext-tm-arity" "arity of such a node")
    ("ext-tm-index" "index of such a node")
    ("ext-tm-last?" "is such a node the last child")
    ("ext-numbered" "adds numbered-line around the lines of a document")
    ("ext-listing" "builds the table of a numbered listing")
    ("texmacs-version-release*" "the number of the version")
    ("texmacs-version" "the version, no argument")
    ("ext-language-suffix" "the two letters of the language")
    ("natbib-get" "author or year of a natbib citation: string operations")
    ("ext-typed-ref" "reference with a type: builds concat and reference trees")
    ("ext-typed-ref*" "same, with the macro that is applied to the references (argument 1: a name)" 1)
    ("ext-map" "applies the macro named by argument 0 to each element of the tuple (argument 1)" 0)
    ("screens-index" "index of the screen") ("screens-arity" "number of screens")
    ("screens-bar" "bar of screens: numbers and the fixed command screens-switch-to")
    ("math-check" "packrat check of a math cell") ("math-check-table" "same for the cells of a table")
    ("ext-vertical-items" "table of items, one row for each child")
    ("tmdoc-key" "the keys of a shortcut, rewritten") ("tmdoc-key*" "same")
    ("tmdoc-shortcut" "the shortcut bound to a command, rewritten")
    ("tmdoc-render-keys" "render-key trees") ("tmdoc-menu-font" "the font of the menus")
    ("gui-hlist-table" "table of a horizontal list, in a tag named by argument 0" 0)
    ("gui-vlist-table" "table of a vertical list, in a tag named by argument 0" 0)
    ("ext-apply-on-paragraphs" "applies the macro named by argument 0 to the paragraphs of the tree" 0)
    ("ext-smart-ref" "smart references: picks the macro <type>-ref from the beginning or the end of each label (checked at the call: see smart-ref-ok?)")))

(define (bad-names-with-prefix? prefix)
  ;; does a macro that the content may not call begin with @prefix?
  (list-find (append (map car code-reasons) (map car code-macro-reasons)
                     (map car file-primitives) (map car file-macros)
                     (map car link-primitives) (map car link-macros)
                     refused-style-macros name-tags)
             (lambda (x) (string-starts? (symbol->string x) prefix))))

(define (fixed-name-prefix e)
  ;; the written beginning (at least 3 characters) of a name that is made by a merge
  (and (func? e 'merge) (pair? (cdr e)) (string? (cadr e)) (>= (string-length (cadr e)) 3)
       (cadr e)))

(define (kinds-add kinds k) (if (memq k kinds) kinds (cons k kinds)))

(define (flow-of body params lookup)
  ;; the parameters that reach a sensitive place in @body, as an alist (parameter
  ;; . kinds of the places); the key `var' is for a variable that does
  (let ((out '()))
    (define (add! x k)
      (let ((e (assoc x out)))
        (if e
            (set-cdr! e (kinds-add (cdr e) k))
            (set! out (cons (list x k) out)))))
    (define (sink! a k)
      (when (not (and (eq? k 'link) (fixed-prefix? a)))
        (for (x (param-refs a params)) (add! x k))
        (when (var-source? a) (add! 'var k))))
    (define (call! name args)
      (for (en (lookup name))
        (cond ((eq? (car en) 'var) (for (k (cdr en)) (add! 'var k)))
              ((eq? (car en) 'all) (for (a args) (for (k (cdr en)) (sink! a k))))
              ((< (car en) (length args))
               (for (k (cdr en)) (sink! (list-ref args (car en)) k))))))
    (define (computed-call! name-expr args)
      ;; a call to a macro whose name is made. With a fixed beginning, the macros
      ;; that can be meant are the style's (trusted: a macro of the style that is
      ;; not dangerous stays so) and the local ones with that beginning (analysed).
      ;; A name that comes from a parameter or a variable is a sensitive place. Else the
      ;; macro is unknown, and the arguments are handed to it.
      (let ((prefix (fixed-name-prefix name-expr)))
        (cond ((and prefix (bad-names-with-prefix? prefix))
               ;; (the family has a macro that the content may not call)
               (sink! name-expr 'name)
               (add! 'var 'name))
              (prefix
               (for (n (local-names-with-prefix prefix)) (call! n args)))
              ((or (pair? (param-refs name-expr params)) (var-source? name-expr))
               (sink! name-expr 'name))
              (else
                (for (a args) (sink! a 'pass))))))
    (define (visit e)
      (when (and (pair? e) (symbol? (car e)))
        (let* ((tag (car e)) (args (cdr e)) (sp (assq tag sensitive-positions)))
          (when (and sp (not (memq tag call-tags)))
            (let ((k (cadr sp)) (which (caddr sp)))
              (cond ((eq? which 'all) (for (a args) (sink! a k)))
                    ((eq? which 'last) (when (pair? args) (sink! (car (reverse args)) k)))
                    ((pair? which)
                     (for (i which) (when (< i (length args)) (sink! (list-ref args i) k))))
                    ((< which (length args)) (sink! (list-ref args which) k)))))
          (cond
            ((memq tag '(compound apply expand expand* hide-expand))
             (when (pair? args)
               (let ((names (literal-names (car args))))
                 (if names
                     (for (n names) (call! n (cdr args)))
                     (computed-call! (car args) (cdr args))))))
            ((eq? tag 'extern)
             ;; the function named is run with the other arguments as data: only the
             ;; functions of the reviewed list take data, the other ones take code
             (let ((f (and (pair? args) (string? (car args)) (assoc (car args) extern-functions))))
               (cond ((and f (equal? (car f) "ext-smart-ref"))
                      (for (a (cdr args)) (sink! a 'ref)))
                     (f
                      (for (i (cddr f))
                        (when (< i (length (cdr args))) (sink! (list-ref (cdr args) i) 'name))))
                     (else (for (a args) (sink! a 'code))))))
            ((eq? tag 'map-args)
             (when (and (pair? args) (string? (car args)))
               (let ((en (lookup (car args))))
                 (when (and (pair? en) (>= (length args) 3) (string? (caddr args))
                            (member (caddr args) params))
                   (for (e en) (for (k (cdr e)) (add! (caddr args) k)))))))
            ((memq tag '(macro xmacro func)) #f)
            ((known-primitive? tag) #f)
            (else (call! (symbol->string tag) args))))
        (for (c (cdr e)) (visit c))))
    (visit body)
    out))

(define (normalize-positions l)
  ;; a canonical form (for the comparison of two results)
  (define (kinds k) (sort k (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
  (let ((idx (lambda (x) (cond ((number? x) x) ((eq? x 'all) 1000) (else 1001)))))
    (sort (map (lambda (e) (cons (car e) (kinds (cdr e)))) l)
          (lambda (a b) (< (idx (car a)) (idx (car b)))))))

(define (positions-of d name lookup)
  (let ((def (macro-definition d name)))
    (if (not def)
        '()
        (let* ((params (macro-parameters def))
               (hits (flow-of (car (reverse def)) params lookup)))
          (normalize-positions
           (append
            (list-filter (map (lambda (h) (and (eq? (car h) 'var) (cons 'var (cdr h)))) hits) (lambda (x) x))
            (if (eq? (car def) 'xmacro)
                (let ((ks (apply append (map cdr (list-filter hits (lambda (h) (!= (car h) 'var)))))))
                  (if (pair? ks) (list (cons 'all ks)) '()))
                (let loop ((l params) (i 0) (acc '()))
                  (cond ((null? l) (reverse acc))
                        ((assoc (car l) hits)
                         (loop (cdr l) (+ i 1) (cons (cons i (cdr (assoc (car l) hits))) acc)))
                        (else (loop (cdr l) (+ i 1) acc)))))))))))

(define (solve-danger! d name)
  ;; the positions of @name and of the macros it reaches, solved together (cycles
  ;; included) by a worklist that starts from nothing and grows
  (let ((reach '()) (callers (make-ahash-table)) (cur (make-ahash-table)))
    (let loop ((todo (list name)))
      (when (pair? todo)
        (let ((n (car todo)))
          (if (or (member n reach) (ahash-ref danger-cache n) (not (macro-definition d n)))
              (loop (cdr todo))
              (let ((cs (called-names (car (reverse (macro-definition d n))))))
                (set! reach (cons n reach))
                (for (c cs) (ahash-set! callers c (cons n (or (ahash-ref callers c) '()))))
                (loop (append (cdr todo) cs)))))))
    (let ((lookup (lambda (n) (or (ahash-ref danger-cache n) (ahash-ref cur n) '()))))
      (let loop ((queue (reverse reach)) (queued (make-ahash-table)))
        (when (pair? queue)
          (let* ((n (car queue)) (new (positions-of d n lookup)))
            (ahash-remove! queued n)
            (if (equal? new (or (ahash-ref cur n) '()))
                (loop (cdr queue) queued)
                (begin
                  (ahash-set! cur n new)
                  (let ((more (list-filter (or (ahash-ref callers n) '())
                                           (lambda (c) (and (member c reach) (not (ahash-ref queued c)))))))
                    (for (c more) (ahash-set! queued c #t))
                    (loop (append (cdr queue) more) queued)))))))
      (for (n reach) (ahash-set! danger-cache n (or (ahash-ref cur n) '()))))))

(define (dangerous-positions d name)
  ;; the places of the macro @name (a list of (position . kinds); `var': a
  ;; variable, `all': every argument) that reach something sensitive; () if none
  ;; or if @name is not a macro
  (danger-tables! d)
  (when (not (ahash-ref danger-cache name))
    (if (macro-definition d name)
        (solve-danger! d name)
        (ahash-set! danger-cache name '())))
  (or (ahash-ref danger-cache name) '()))

;; The standard macros that the content may call although they pass an argument to
;; a link or a file: the arguments they pass on are checked where the call is written
;; (check-safe-macro-arguments, from the places the analysis finds in them). A macro
;; that the document defines itself is never one of them, nor one whose closure (the
;; macros it calls, again and again) has a macro that the document defines: then it is
;; analysed like any other.
(define safe-macros
  '("author-affiliation-note" "author-email-note" "author-homepage-note"
    "author-misc-note" "custom-footnote-text" "custom-note-ref" "custom-note-text"
    "doc-footnote-text" "doc-note-ref" "doc-note-text"
    "hlink*" "href" "slink" "include"
    ;; the manuals (the tmdoc style)
    "simple-link" "help-link" "hyper-link*" "tmdoc-link" "tmdoc-include"))

;; What the analysis cannot give for a macro of the table, checked all the same where
;; the call is written (position, kinds): `tmdoc-link' gives its destination to
;; `tmdoc-file' (a search of the manuals by a name), `tmdoc-include' to a `find-file'.
(define safe-extra-checks
  '(("tmdoc-link" (1 link)) ("tmdoc-include" (0 file))))

;; The macros of the table that have, for the analysis, a kind of place other than
;; link, file and pass (the test checks that there are no others)
(define safe-code-allowed '("tmdoc-link" "tmdoc-include"))

(define (closure-has-local? d name)
  ;; does a macro that the document defines lie in the closure of @name?
  (let ((c (ahash-ref void-cache name)))
    (cond ((eq? c 'yes) #t)
          ((eq? c 'no) #f)
          (else
            (let ((seen (make-ahash-table)) (found #f))
              (let loop ((todo (list name)))
                (when (and (pair? todo) (not found))
                  (let ((n (car todo)))
                    (cond ((ahash-ref seen n) (loop (cdr todo)))
                          ((local-macro? n) (set! found #t))
                          (else
                            (ahash-set! seen n #t)
                            (let ((def (macro-definition d n)))
                              (loop (if def
                                        (append (cdr todo) (called-names (car (reverse def))))
                                        (cdr todo)))))))))
              (ahash-set! void-cache name (if found 'yes 'no))
              found)))))

(define (safe-macro? d tag)
  ;; is @tag a macro of the table (not one that the document redefines, nor one
  ;; that reaches a macro of the document)?
  (let ((name (symbol->string tag)))
    (danger-tables! d)
    (and (member name safe-macros)
         (not (closure-has-local? d name)))))

;; smart-ref picks the macro `<type>-ref' of each label from the letters at its
;; beginning or its end: it is harmless when every label is written, and the macros
;; of those names that exist (the style's or the document's) are not dangerous.
(define (smart-ref-ok? d en args)
  (let ((as (if (eq? (car en) 'all) args
                (if (and (number? (car en)) (< (car en) (length args))) (list (list-ref args (car en))) '()))))
    (and (list-find as (lambda (a) #t)) #t
         (not (list-find as (lambda (a) (not (string? a)))))
         (not (list-find
               as
               (lambda (a)
                 (list-find (label-types a)
                            (lambda (tp)
                              (let ((m (string-append tp "-ref")))
                                (or (dangerous-name? m)
                                    (and (macro-definition d m)
                                         (pair? (dangerous-positions d m)))))))))))))

(define (label-types s0)
  ;; the letters at the beginning and at the end of the label (the types that smart-ref
  ;; tries; it lower-cases the label first)
  (let* ((s (locase-all s0))
         (n (string-length s))
         ;; (ASCII letters only, as smart-ref-type does: an accented letter ends the type)
         (alpha? (lambda (c) (or (and (char>=? c #\a) (char<=? c #\z)) (and (char>=? c #\A) (char<=? c #\Z)))))
         (i (let loop ((k 0)) (if (and (< k n) (alpha? (string-ref s k))) (loop (+ k 1)) k)))
         (j (let loop ((k n)) (if (and (> k 0) (alpha? (string-ref s (- k 1)))) (loop (- k 1)) k))))
    (list-filter (list (substring s 0 i) (substring s j n)) (lambda (x) (> (string-length x) 0)))))

(define (dangerous-call? d st)
  ;; does the node @st call a macro that is dangerous? A place that is only the name of
  ;; a macro (`map') is harmless when the name is written and is that of a macro that
  ;; is not dangerous itself.
  (let* ((tag (car st)) (name (symbol->string tag)) (args (cdr st)))
    (danger-tables! d)
    (and (not (safe-macro? d tag))
         (list-find (dangerous-positions d name)
                    (lambda (en)
                      (not (or (and (number? (car en)) (equal? (cdr en) '(name))
                                    (< (car en) (length args))
                                    (let ((a (list-ref args (car en))))
                                      (and (string? a)
                                           (not (dangerous-name? a))
                                           (not (member a safe-macros))
                                           (null? (dangerous-positions d a)))))
                               (and (equal? (cdr en) '(ref))
                                    (smart-ref-ok? d en args))))))
         #t)))

(define (check-safe-macro-arguments st base)
  ;; the arguments that a macro of the table passes to a link or a file are
  ;; checked with the rules of the link and file tags
  (let ((tag (car st)) (args (cdr st)))
    (for (en (append (dangerous-positions safety-doc (symbol->string tag))
                     (let ((x (assoc (symbol->string tag) safe-extra-checks))) (if x (cdr x) '()))))
      (when (not (eq? (car en) 'var))
        (let ((as (if (eq? (car en) 'all)
                      args
                      (if (< (car en) (length args)) (list (list-ref args (car en))) '()))))
          (for (a as)
            (when (memq 'link (cdr en)) (check-link-destination tag a))
            (when (memq 'file (cdr en))
              (if (string? a)
                  (check-file-argument tag a base #f)
                  (violate! (string-append "a `" (symbol->string tag)
                                             "' has a file that is not written as a plain name: refused (nothing was changed)"))))))))))

(define (definition-node? x)
  (and (tree-compound? x) (memq (tree-label x) definitional-tags) #t))

(define (contains-definition? x)
  (and (tree? x)
       (or (definition-node? x)
           (and (tree-compound? x) (list-find (tree-children x) contains-definition?)))
       #t))

(define (inside-definition? d t self?)
  ;; is @t (if @self?) or one of its ancestors inside the document body a definition?
  (let loop ((x (if self? t (tree-up t))))
    (and x (tree? x) (inside-body? d x) (not (body-root? d x))
         (or (definition-node? x) (loop (tree-up x))))))

(define (guard-pre d t delete? . opt)
  ;; before an edit of the node @t (or of the document: #f): what is needed to
  ;; compare after it. Refuses an edit inside or of code, and in a block that
  ;; cannot be evaluated.
  (let* ((mode (if (pair? opt) (car opt) 'replace))
         (blk (and t (not (body-root? d t)) (top-level-ancestor d t)))
         (i (and blk (index-in-body d blk)))
         (defs (and blk (assign-nodes blk))))
    ;; (the definitions belong to the person: no edit inside one, and none that
    ;; replaces or deletes a node with one inside)
    (when (and t (not (body-root? d t))
               (or (inside-definition? d t (not (eq? mode 'sibling)))
                   (and (memq mode '(replace)) (contains-definition? t))))
      (tool-error "the definitions of the document (assign, set, macro...) belong to the person: the content filter refuses edits inside them, and edits that replace or delete a node that has one inside: the edit is refused (nothing was changed)"))
    (when (and t (inside-opaque? d t (not delete?)))
      (tool-error "this node is code or text for another program, or inside one (extern, script, hybrid, action, specific, latex...): the content filter refuses edits in it (nothing was changed)"))
    (let* ((lit (and blk i (block-literal-violations d blk i)))
           (viol (and blk i (block-violations d blk i lit))))
      (when (and viol (over-budget? viol))
        (tool-error "the block does not evaluate within the budget: the content filter refuses edits in it (nothing was changed)"))
      ;; (the definitions cannot be changed by the content, so the rest of the
      ;; document is not compared: only the block of the edit and the new blocks)
      (list (and blk (ptr blk)) viol (assigns-signature d) lit))))

(define (filter-map f l)
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc))
          ((f (car l)) => (lambda (x) (loop (cdr l) (cons x acc))))
          (else (loop (cdr l) acc)))))

(define (guard-fail! reasons)
  (set! edit-reason
        (string-append "the edit is refused: it would introduce what the content filter refuses ("
                       (utf8-prefix (u8 (reason-text (car reasons))) 200)
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
             (same? (and blk (tree? blk) (tree-eq? b (top-level-ancestor d blk))))
             ;; (what is wrong as written is told before anything is evaluated: the
             ;; evaluation of content that is refused could run what it should not)
             (lit-after (block-literal-violations d b i))
             (lit-new (new-violations (if same? (or (cadddr pre) '()) '()) lit-after))
             (_ (when (pair? lit-new) (guard-fail! lit-new)))
             (after (block-violations d b i lit-after))
             (before (if same? (or old-viol '()) '()))
             (new (new-violations before after)))
        (when (over-budget? after) (guard-fail! (list over-budget-reason)))
        (when (pair? new) (guard-fail! new))))
    (when (!= (assigns-signature d) (caddr pre))
      (guard-fail! (list "it changes the definitions of the document")))))

(define edit-reason #f)
(define current-guard #f)

(define (set-guard! d t get-nodes skip-block? . opt)
  ;; the next run-edit! checks the document before and after (see guard-post);
  ;; the optional arguments: whether the edit deletes the node, and its mode
  ;; (replace, sibling: new content beside @t, insert: inside @t, text: a change
  ;; of text inside @t)
  (set! current-guard (list d (guard-pre d t (and (pair? opt) (car opt))
                                         (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) 'replace))
                            get-nodes skip-block?)))

(define (run-edit! d thunk)
  ;; do @thunk (it changes the tree) as one step of the editor of the buffer,
  ;; all or nothing; gives its value
  (let ((u (doc-url d)) (failed #f) (value #f) (g current-guard)
        (mark (shared-collect-mark (doc-url d))))
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
    ;; (an edit that was cancelled never reaches the host: not even with its
    ;; inverse)
    (when failed (shared-collect-restore! u mark))
    (when failed
      (let ((why edit-reason))
        (set! edit-reason #f)
        (tool-error (or why "the edit failed while it was made and was cancelled: nothing was changed"))))
    value))

(define (filter-live d nodes)
  (list-filter nodes (lambda (t) (and (tree? t) (inside-body? d t)))))

(define (wait-for-markup! d s0)
  ;; the host has made the proposal in its document and sends it: wait (at
  ;; most 3 s) until the state of the document is not the state @s0 any more
  (let* ((u (doc-url d)) (t0 (texmacs-time)))
    (let loop ()
      (shared-poll)
      (let ((i (shared-file-info u)))
        (when (and i (equal? (cadr i) s0) (< (- (texmacs-time) t0) 3000))
          (loop))))))

(define (finish-edit! d dist changed undo)
  ;; after the edit: wait for the host, say what happened. @changed is a
  ;; list of nodes, @undo the way to undo the step (or #f)
  (let* ((outcome (settle! d dist))
         (body (doc-body d)))
    (cond ((== outcome 'refused)
           (forget-steps! d)
           (with-buffer (doc-url d) (clear-undo-history))
           (let ((v (shared-file-verdict (doc-url d))))
             (when (and v (eq? (car v) 'proposed)) (wait-for-markup! d (cadddr v)))
             (snapshot! d)
             (cond
               ((and v (eq? (car v) 'proposed))
                (obj "status" "proposed"
                     "document" (doc-id d)
                     "proposal" (caddr v)
                     "message" "the person's TeXmacs keeps your change as a proposal (old and new version side by side): the person accepts or rejects it. Your copy of the document is back to the host's, with the proposal in it. list_proposals tells what became of it; withdraw_proposal takes it back."
                     "hash" (hash-short body)))
               ((and v (eq? (car v) 'refused))
                (obj "status" "refused"
                     "document" (doc-id d)
                     "reason" (u8 (string-append "the host (the person's TeXmacs) refused the change: "
                                                  (caddr (car (caddr v)))
                                                  (if (pair? (cdr (caddr v)))
                                                      (string-append " (and " (number->string (length (cdr (caddr v)))) " more)")
                                                      "")
                                                  ". The document is back to what the host has; nothing of yours is kept."))
                     "rules" (arr (map (lambda (r) (obj "rule" (car r) "level" (cadr r) "content" (u8 (caddr r))))
                                       (caddr v)))
                     "hash" (hash-short body)))
               (else
                 (obj "status" "refused"
                      "document" (doc-id d)
                      "reason" "the host (the TeXmacs of the person) refused the change: the document had changed too much around it. The document is back to what the host has; nothing of yours is kept. Read again and retry."
                      "hash" (hash-short body))))))
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

;;; The host's check of the patches of agents (the person's TeXmacs, see
;;; shared-files.scm): the same rules, applied to the document before and
;;; after a patch, which is applied to a copy (a tree shared with the
;;; document, nothing is changed). The changed blocks are @range (first n-old
;;; n-new); the violations of the blocks after, as written and as they expand,
;;; that were not in the blocks before are the answer, as (rule level text).

(define (violation-class r)
  ;; (rule level) of the reason @r: the level is what a person could be asked
  ;; to allow (style, links) or never (code, limits, mode)
  (let ((has (lambda (s) (>= (string-search-forwards s 0 r) 0))))
    (cond ((has "definitions of the document") (list "definitions" "style"))
          ((has "version markup") (list "version-markup" "mode"))
          ((or (has "budget") (has "digits") (has "at most") (has "loop") (has "nesting")
               (has "too deep") (has "recursion"))
           (list "limits" "limits"))
          ((or (has "a call of the macro") (has "passes an argument") (has "runs commands"))
           (list "dangerous-macro" "code"))
          ((and (or (has "link") (has "hlink") (has "target"))
                (not (has "runs a script")) (not (has "expands to an action")))
           (list "link" "links"))
          ((or (has "script") (has "action") (has "extern") (has "specific") (has "LaTeX")
               (has "latex") (has "another program") (has "hybrid") (has "eval")
               (has "another program"))
           (list "code" "code"))
          ((or (has "file") (has "image") (has "include") (has "a path")) (list "file" "links"))
          ((or (has "character") (has "backslash")) (list "names" "links"))
          ((or (has "defin") (has "alias") (has "not defined by the style") (has "unknown tag")
               (has "package") (has "style"))
           (list "style" "style"))
          (else (list "other" "code")))))

(define (range-blocks body range . new?)
  ;; the blocks of @body in the range (the old ones, or the new ones)
  (sublist (tree-children body) (car range)
           (+ (car range) (if (pair? new?) (caddr range) (cadr range)))))

(define (assign-signature-of blocks)
  ;; (the blocks outside the range are the same in both: the definitions
  ;; differ only if those of the blocks in it do)
  (map-append assign-nodes blocks))

(define (opaque-strees t)
  (cond ((tree-atomic? t) '())
        ((opaque-node? t) (list (tm->stree t)))
        (else (append-map opaque-strees (tree-children t)))))

(define (map-append f . ls) (apply append (apply map (cons f ls))))

(define (host-block-violations d old new p n-old n-new)
  (let* ((kn (sublist (tree-children new) p (+ p n-new)))
         (ko (sublist (tree-children old) p (+ p n-old)))
         (inew (map (lambda (k) (+ p k)) (iota n-new)))
         (iold (map (lambda (k) (+ p k)) (iota n-old)))
         (base (doc-base d)))
    (reset-danger!)
    (let* ((lit-new (map (lambda (b) (literal-violations (tm->stree b) base d)) kn))
           (lit-old (map (lambda (b) (literal-violations (tm->stree b) base d)) ko))
           (fresh (new-violations (apply append lit-old) (apply append lit-new))))
      (cond
        ((pair? fresh) fresh)
        ((pair? (new-violations (append-map opaque-strees ko) (append-map opaque-strees kn)))
         (list "it adds or changes code or text for another program (extern, script, latex, specific...): only the person writes that"))
        (else
          (let ((after (map-append (lambda (b i lit) (block-violations d b i lit)) kn inew lit-new)))
            (cond ((null? after) '())
                  ((over-budget? after) (list over-budget-reason))
                  (else
                    (new-violations
                     (map-append (lambda (b i lit) (block-violations d b i lit)) ko iold lit-old)
                     after)))))))))

(tm-define (mcp-host-check u old new range)
  ;; the new violations of the patched body @new (a tree) against @old, for
  ;; the document @u: a list of (rule level text), empty if the change is fine
  (let* ((d (list "host" u (url->system u)))
         (_ (set! host-cache (map assign-nodes (tree-children old))))
         (found (catch #t
                  (lambda ()
                    (if (!= (assign-signature-of (range-blocks old range))
                            (assign-signature-of (range-blocks new range #t)))
                        (list "it changes the definitions of the document")
                        (host-block-violations d old new (car range) (cadr range) (caddr range))))
                  (lambda args (set! host-cache #f) (apply throw args)))))
    (reset-danger!)
    (set! host-cache #f)
    (map (lambda (r)
           (let ((c (violation-class r)) (t (reason-text r)))
             (list (car c) (cadr c) (if (> (string-length t) 400) (substring t 0 400) t))))
         found)))

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

(define (editing-context args . keep)
  (require-writable)
  (let ((d (require-doc (opt-string args "document" #f))))
    (doc-refresh! d)
    (when (and (null? keep) (doc-not-joined d))
      (tool-error (string-append "read-only: " (doc-not-joined d)
                                 ". Nothing is edited or saved while the person's TeXmacs holds the file")))
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
             (set-guard! d (if ah anchor #f) (lambda () nodes) #f #f (if side? 'sibling 'insert))
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
               (set-guard! d (if ah anchor #f) (lambda () nodes) #f #f (if side? 'sibling 'insert))
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
                     (set-guard! d t (lambda () (quote ())) (body-root? d parent) #t)
                     (run-edit! d (lambda ()
                                    (tree-assign t (tree-of-stree ""))
                                    (set! nodes (list (tree-ref parent 0)))))
                     (finish-edit! d dist '() (undo-replace d nodes (list old))))
                   (begin
                     (set-guard! d t (lambda () (quote ())) (body-root? d parent) #t)
                     (run-edit! d (lambda () (tree-remove parent i 1)))
                     (finish-edit! d dist '()
                                   (undo-delete d parent prev next old))))))
            ((tm-is? parent 'concat)
             (let* ((pold (tm->stree parent))
                    (kids (cdr pold))
                    (new (inline-stree (append (sublist kids 0 i) (list-tail kids (+ i 1)))))
                    (nodes '()))
               (set-guard! d t (lambda () (quote ())) (body-root? d parent) #t)
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
        ((or (in? (tree-label t) skipped-text-tags) (memq (tree-label t) definitional-tags)
             (opaque-node? t))
         '())
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
                (when (or (inside-definition? d leaf #t) (inside-opaque? d leaf #t))
                  (tool-error "the text is inside a definition, or code or text for another program: the content filter refuses edits in it (nothing was changed)"))
                (set-guard! d t (lambda () (list t)) #f #f 'text)
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

;;; Proposals (the mode `propose' of the host: shared-files.scm)

(define mcp-prop-seen (make-ahash-table))    ; "d1/p3" -> the status told

(define (proposal-entries d)
  ;; (id status index) of the proposals of this agent on the document
  (if (shared-file-participant? (doc-url d))
      (map (lambda (e) (list (car e) (cadr e) (caddr e)))
           (or (shared-proposals (doc-url d)) '()))
      '()))

(define (proposal-key d id) (string-append (doc-id d) "/" id))

(define (proposal-note d)
  ;; what became of the proposals since the last call, or #f
  (let ((l (list-filter (proposal-entries d)
                        (lambda (e) (and (!= (cadr e) "pending")
                                         (!= (ahash-ref mcp-prop-seen (proposal-key d (car e)))
                                             (cadr e)))))))
    (and (nnull? l)
         (begin
           (for (e l) (ahash-set! mcp-prop-seen (proposal-key d (car e)) (cadr e)))
           (string-recompose
            (map (lambda (e)
                   (string-append "Your proposal " (car e) " was "
                                  (cond ((== (cadr e) "accepted") "accepted by the person")
                                        ((== (cadr e) "rejected") "rejected by the person")
                                        ((== (cadr e) "withdrawn") "withdrawn")
                                        (else "resolved by the person (changed, neither as proposed nor as before)"))
                                  "."))
                 l)
            " ")))))

(define (proposal-where d e)
  (let ((i (caddr e)) (body (doc-body d)))
    (if (and (>= i 0) (< i (tree-arity body)))
        (let ((sec (section-of d (tree-ref body i))))
          (apply obj (append (list "index" (num i))
                             (if sec (list "section" (u8 sec)) '()))))
        (obj "index" (num i)))))

(define (tool-list-proposals args)
  (let ((d (require-doc (opt-string args "document" #f))))
    (doc-refresh! d)
    (obj "document" (doc-id d)
         "agent_mode" (doc-agent-mode d)
         "proposals"
         (arr (map (lambda (e)
                     (ahash-set! mcp-prop-seen (proposal-key d (car e)) (cadr e))
                     (obj "id" (car e) "status" (cadr e) "where" (proposal-where d e)))
                   (proposal-entries d))))))

(define (tool-withdraw-proposal args)
  (let* ((d (editing-context args)) (u (doc-url d))
         (id (opt-string args "id" #f)))
    (when (not id) (tool-error "the argument `id' (a proposal of list_proposals) is needed"))
    (let ((e (list-find (proposal-entries d) (lambda (e) (== (car e) id)))))
      (when (not e) (tool-error (string-append "no proposal " id " of yours on this document")))
      (when (== (cadr e) "pending")
        (shared-file-send-withdraw u id)
        (let ((t0 (texmacs-time)))
          (let loop ()
            (shared-poll)
            (when (and (== (cadr (or (list-find (proposal-entries d) (lambda (x) (== (car x) id))) e)) "pending")
                       (< (- (texmacs-time) t0) 5000))
              (loop)))))
      (let ((now (or (list-find (proposal-entries d) (lambda (x) (== (car x) id))) e)))
        (ahash-set! mcp-prop-seen (proposal-key d id) (cadr now))
        (obj "status" (if (== (cadr now) "pending") "pending" (cadr now))
             "proposal" id
             "document" (doc-id d))))))

;;; save, close, create

(define mcp-save-refused-reason #f)

(define (tool-save-document args)
  (set! mcp-save-refused-reason #f)
  (let* ((d (editing-context args)) (u (doc-url d)))
    (settle! d (disturbances d))
    (save-buffer-main u)
    ;; (a participant asks the host: wait for it)
    (let loop ((n 0))
      (when (and (shared-file-participant? u) (buffer-modified? u) (< n 500)
                 (not (and (shared-poll) #f))
                 (let ((why (shared-file-save-refused u)))
                   (if why (begin (set! mcp-save-refused-reason why) #f) #t)))
        (loop (+ n 1))))
    (let ((why (shared-file-save-refused u)))
      (when why (set! mcp-save-refused-reason why)))
    (apply obj
      (append
       (list "status" (cond (mcp-save-refused-reason "refused")
                            ((buffer-modified? u) "pending")
                            (else "saved"))
             "document" (doc-id d)
             "path" (doc-path d)
             "shared" (doc-shared d)
             "hash" (hash-short (doc-body d)))
       (if mcp-save-refused-reason (list "reason" (u8 mcp-save-refused-reason)) '())))))

(define (tool-close-document args)
  (let* ((d (editing-context args #t)) (u (doc-url d))
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
                          (let ((v (let ((lv (document-literal-violations d)))
                                     (if (pair? lv) lv (document-violations d)))))
                            (when (pair? v)
                              (tool-error (string-append "the content is refused: "
                                                         (utf8-prefix (u8 (reason-text (car v))) 200))))))
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
   (list "list_proposals" "Your pending proposals"
         "When the person has the document open and its mode for agents is `propose' (see agent_mode in list_documents), each of your edits is kept as a proposal: old and new version side by side in the person's document, which they accept or reject. This lists yours: id, status (pending, accepted, rejected, withdrawn, resolved) and where (index of the block, section)."
         (schema (list (cons "document" document-prop)) '("document"))
         tool-list-proposals)
   (list "withdraw_proposal" "Withdraw a pending proposal"
         "Takes back a proposal that is still pending: the person's document goes back to the old version."
         (schema (list (cons "document" document-prop)
                       (cons "id" (prop "string" "The proposal (p1...), from the answer of the edit or from list_proposals.")))
                 '("document" "id"))
         tool-withdraw-proposal #t)
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
  "TeXmacs documents. Start with list_documents or open_document, then get_outline to see the structure of a long document, then read (format text is the cheapest; texmacs is exact) the blocks you need, using the handles of earlier results. find locates text, tags (tag:theorem), labels and sections; get_references lists labels, references and citations and the broken ones. A file the person has open in TeXmacs is joined live, so reads show their latest edits. Editing: replace, insert, delete and edit_text (the best for changing a word or a sentence) take the handle of a node and, for safety, `expect', the hash you read it with: if someone changed the node meanwhile you get a `conflict' with its current content and nothing is changed. Each call is one undo step, all or nothing; undo_agent undoes yours. Answers may carry a `note' when the document changed since your last call, or when the person accepted or rejected a proposal: read again what it names. When the person has the document open, your edits may come back as `status: proposed' (the person decides; see list_proposals and withdraw_proposal), or as `refused' with the rules in `rules'; `agent_mode' of list_documents says which. save_document writes the file.")

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
                   (note (and d (begin (doc-refresh! d)
                                       (let ((a (change-note d)) (b (proposal-note d)))
                                         (cond ((and a b) (string-append a " " b))
                                               (else (or a b)))))))
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

(tm-define (mcp-dangerous-names doc names)
  (:synopsis "For the docs and the tests: which of @names (macros of the style of @doc) are dangerous")
  (let ((d (or (find-doc doc) (list "probe" (system->url doc) doc))))
    (reset-danger!)
    (let ((r (list-filter (map (lambda (n) (cons n (dangerous-positions d n))) names)
                          (lambda (e) (pair? (cdr e))))))
      (reset-danger!)
      r)))

(tm-define (mcp-safe-report doc)
  (:synopsis "For the tests: the macros of the table of standard macros that the document's style defines: (name number-of-parameters positions), and the names allowed other kinds")
  (let ((d (or (find-doc doc) (list "probe" (system->url doc) doc))))
    (reset-danger!)
    (danger-tables! d)
    (let ((r (filter-map
              (lambda (n)
                (let ((def (macro-definition d n)))
                  (and def
                       (list n (if (eq? (car def) 'xmacro) -1 (length (macro-parameters def)))
                             (dangerous-positions d n)))))
              safe-macros)))
      (reset-danger!)
      (list r safe-code-allowed))))

(tm-define (mcp-extern-allowlist)
  (:synopsis "For the tests: the names of the functions of extern whose arguments are data")
  (map car extern-functions))

(tm-define (mcp-defined-names doc names)
  (:synopsis "For the tests: which of @names are macros for the style of @doc")
  (let ((d (or (find-doc doc) (list "probe" (system->url doc) doc))))
    (reset-danger!)
    (danger-tables! d)
    (let ((r (list-filter names (lambda (n) (macro-definition d n)))))
      (reset-danger!)
      r)))

(tm-define (mcp-refused-style-macros)
  (:synopsis "For the tests: the macros of the styles that the content may not call")
  (map symbol->string refused-style-macros))
