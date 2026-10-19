
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-blocks.scm
;; DESCRIPTION : the code blocks of documents, as documents of the servers
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A code block of a document (cpp-code, python-code... whose body is a
;; document of lines) of a language with a server is, when the preference
;; "lsp:code blocks" is on, a document of that server of its own: a virtual
;; file next to the document, .<name>-block<k>.<suffix> (name with its
;; suffix: notes.tm and notes.html have blocks of their own), which is never
;; written (the server has its text from didOpen and didChange), in the
;; project of the document. Its positions are those of its lines, as in a
;; source file, see src/docs/code-features.md (phase 7). A block is known by its
;; tree; when it is removed, or its document closed, its virtual document
;; is closed on the server.
;;
;; With the preference "lsp:join code blocks" (on by default), the blocks
;; of a language in a document are one virtual file instead,
;; .<name>-blocks.<suffix>, a unit: their lines one after the
;; other, in the order of the document, so that the names of a block are
;; known in the next ones (a notebook). A line of the unit is a line of one
;; of its blocks (lsp-unit-path-of, lsp-unit-lo-of).

(texmacs-module (lsp lsp-blocks)
  (:use (lsp lsp-base)
        (lsp lsp-client)))

;; #t or #f instead of the preference (for the tests, which do not write
;; the preferences), or 'pref
(define blocks-forced 'pref)

(tm-define (lsp-blocks-force! on)
  (:synopsis "Follow the code blocks (#t), not (#f), as the preference says ('pref)")
  (set! blocks-forced on))

(define joined-forced 'pref)

(tm-define (lsp-blocks-join-force! on)
  (:synopsis "Join the code blocks (#t), not (#f), as the preference says ('pref)")
  (set! joined-forced on))

(tm-define (lsp-blocks-joined?)
  (:synopsis "Are the code blocks of a language in a document one file?")
  (if (== joined-forced 'pref)
      (!= (get-preference "lsp:join code blocks") "off")
      joined-forced))

(tm-define (lsp-blocks-on?)
  (:synopsis "Do language servers follow the code blocks of documents?")
  (and (!= (get-preference "lsp") "off")
       (if (== blocks-forced 'pref)
           (== (get-preference "lsp:code blocks") "on")
           blocks-forced)))

;; tag -> language, for the languages with a block tag and a server
(define block-languages #f)

(define (block-language-table)
  (when (not block-languages)
    (set! block-languages (make-ahash-table))
    (for (lan (program-languages))
      (let ((tag (program-language-block-tag lan)))
        (when (and tag (lsp-server-known? lan))
          (ahash-set! block-languages tag lan)))))
  block-languages)

(tm-define (lsp-block-language t)
  (:synopsis "The language of the code block @t if a server may follow it")
  (and (tree-compound? t) (== (tree-arity t) 1)
       (tree-is? (tree-ref t 0) 'document)
       (ahash-ref (block-language-table) (tree-label t))))

(tm-define (lsp-document-blocks body)
  (:synopsis "The code blocks of @body which a server may follow: (root lan)")
  ;; (root: the document of the lines of the block)
  (map (lambda (t) (list (tree-ref t 0) (lsp-block-language t)))
       (tree-search body lsp-block-language)))

(tm-define (lsp-block-at p)
  (:synopsis "(root lan) of the code block whose line holds the place @p")
  ;; (the cursor in one of the lines of the block, not in a tag of a line)
  (and (pair? p) (pair? (cdr p)) (pair? (cddr p))
       (let* ((root (path->tree (cDr (cDr p))))
              (block (and root (tree-is? root 'document) (tree-up root))))
         (and block (lsp-block-language block)
              (list root (lsp-block-language block))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The virtual files of the blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer name -> list of (root lan path) of its blocks given to servers
(define block-files (make-ahash-table))
;; buffer name -> the number of the next block
(define block-counts (make-ahash-table))

(define (lan-suffix lan)
  (or (and (defined? 'format-default-suffix)
           (with s (format-default-suffix lan)
             (and (string? s) (!= s "") s)))
      lan))

(define (new-block-path doc lan)
  ;; doc: the system path of the document
  (let* ((base (url->string (url-tail (system->url doc))))
         (k (or (ahash-ref block-counts doc) 1)))
    (ahash-set! block-counts doc (+ k 1))
    (url->system (url-append (url-head (system->url doc))
                             (string-append "." base "-block"
                                            (number->string k) "."
                                            (lan-suffix lan))))))

(define (entry-of entries root lan)
  (list-find entries (lambda (e) (and (tree-eq? (car e) root) (== (cadr e) lan)))))

(tm-define (lsp-block-path doc root lan)
  (:synopsis "The virtual file of the block @root of the document @doc")
  ;; (doc: the system path of the document; given once, then kept; a block
  ;; whose language changes gets another one, and the former one is closed
  ;; by the next sync, lsp-documents.scm)
  (let* ((entries (or (ahash-ref block-files doc) '()))
         (e (entry-of entries root lan)))
    (if e (caddr e)
        (with p (new-block-path doc lan)
          (ahash-set! block-files doc (cons (list root lan p) entries))
          p))))

(tm-define (lsp-block-entries doc)
  (:synopsis "The blocks of @doc given to servers: (root lan path)")
  (or (ahash-ref block-files doc) '()))

(tm-define (lsp-block-entries-set! doc entries)
  (if (null? entries) (ahash-remove! block-files doc)
      (ahash-set! block-files doc entries)))

(tm-define (lsp-block-of-path path)
  (:synopsis "(doc root lan) of the block whose virtual file is @path, or #f")
  (let loop ((l (ahash-table->list block-files)))
    (and (nnull? l)
         (with e (list-find (cdar l) (lambda (e) (== (caddr e) path)))
           (if e (list (caar l) (car e) (cadr e)) (loop (cdr l)))))))

(tm-define (lsp-virtual-block-path? path)
  (:synopsis "Is @path the name of the virtual file of a code block?")
  ;; (also of a block of a document which is closed: it is not on disk)
  (let ((name (url->string (url-tail (system->url path)))))
    (and (string-starts? name ".") (string-contains? name "-block")
         (not (url-exists? (system->url path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The units: the blocks of a language in a document, as one file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doc path + language -> unit, a vector #(doc lan path lines roots counts
;; synced): synced is (roots counts) as the server was last given them,
;; with which its diagnostics are placed until the next sync;
;; lines is a document of the lines of the blocks (a tree of its own, which
;; the server is given: its snapshots compare its lines), roots the blocks
;; in the order of the document, counts their numbers of lines
(define units (make-ahash-table))

(define (unit-key doc lan) (string-append doc "\n" lan))

(tm-define (lsp-unit-doc u) (vector-ref u 0))
(tm-define (lsp-unit-language u) (vector-ref u 1))
(tm-define (lsp-unit-path u) (vector-ref u 2))
(tm-define (lsp-unit-lines u) (vector-ref u 3))
(tm-define (lsp-unit-roots u) (vector-ref u 4))

(define (unit-path doc lan)
  (url->system (url-append (url-head (system->url doc))
                           (string-append "." (url->string (url-tail (system->url doc)))
                                          "-blocks." (lan-suffix lan)))))

(define (line-text l)
  ;; the text of a line (one with markup: as the server sees it)
  (if (tree-atomic? l) (tree->string l) (car (lsp-lines l))))

(define (block-lines root)
  (if (tree-is? root 'document)
      (map line-text (tree-children root))
      (lsp-lines root)))

(define (set-lines! body lines)
  ;; the document @body gets the lines @lines (those which differ: a line
  ;; with markup whose text is the same stays as it is)
  (let* ((old (map line-text (tree-children body)))
         (n (length old)) (m (length lines)))
    (let loop ((i 0) (o old) (l lines))
      (when (and (< i n) (< i m))
        (when (!= (car o) (car l))
          (tree-remove! body i 1)
          (tree-insert! body i (list (car l))))
        (loop (+ i 1) (cdr o) (cdr l))))
    (cond ((> m n) (tree-insert! body n (list-tail lines n)))
          ((> n m) (tree-remove! body m (- n m))))))

(define (path<? p q)
  (cond ((null? p) (nnull? q))
        ((null? q) #f)
        ((< (car p) (car q)) #t)
        ((> (car p) (car q)) #f)
        (else (path<? (cdr p) (cdr q)))))

(tm-define (lsp-unit-set-roots! u roots)
  (:synopsis "The blocks of the unit @u are @roots; its lines follow them")
  ;; (the roots which are no longer in a document are left out; in the
  ;; order of the document)
  (let* ((in (list-filter roots
                          (lambda (r)
                            ;; (still in the document, a block of the language)
                            (and (tree->path r) (tree-up r)
                                 (== (lsp-block-language (tree-up r))
                                     (vector-ref u 1))))))
         (sorted (sort in (lambda (a b) (path<? (tree->path a) (tree->path b)))))
         (ls (map block-lines sorted)))
    (vector-set! u 4 sorted)
    (vector-set! u 5 (map length ls))
    (set-lines! (vector-ref u 3) (apply append ls))
    u))

(tm-define (lsp-unit doc lan)
  (:synopsis "The unit of the blocks of @lan in the document @doc (made if needed)")
  (with key (unit-key doc lan)
    (or (ahash-ref units key)
        (with u (vector doc lan (unit-path doc lan)
                        (stree->tree '(document)) '() '() (list '() '()))
          (ahash-set! units key u)
          u))))

(define (refresh-block! u root)
  ;; the lines of the unit @u which are those of its block @root, when its
  ;; blocks and their numbers of lines are the same; whether they were
  ;; (a query at the cursor, at each key: not all the blocks again)
  (let loop ((rs (vector-ref u 4)) (cs (vector-ref u 5)) (start 0) (ok #t))
    (cond ((null? rs) ok)
          ((not (and (tree->path (car rs)) (tree-up (car rs))
                     (tree-is? (car rs) 'document)
                     (== (lsp-block-language (tree-up (car rs))) (vector-ref u 1))
                     (== (tree-arity (car rs)) (car cs))))
           #f)
          ((tree-eq? (car rs) root)
           (let ((lines (vector-ref u 3)) (new (block-lines root)))
             (for (i (iota (car cs)))
               (with l (list-ref new i)
                 (when (!= (tree->string (tree-ref lines (+ start i))) l)
                   (tree-remove! lines (+ start i) 1)
                   (tree-insert! lines (+ start i) (list l)))))
             (loop (cdr rs) (cdr cs) (+ start (car cs)) ok)))
          (else (loop (cdr rs) (cdr cs) (+ start (car cs)) ok)))))

(tm-define (lsp-unit-at doc root lan)
  (:synopsis "The unit of the block @root, with it among its blocks")
  (let ((u (lsp-unit doc lan)))
    (cond ((not (list-find (lsp-unit-roots u) (lambda (r) (tree-eq? r root))))
           (lsp-unit-set-roots! u (cons root (lsp-unit-roots u))))
          ((refresh-block! u root) u)
          (else (lsp-unit-set-roots! u (lsp-unit-roots u))))))

(tm-define (lsp-units-of doc)
  (:synopsis "The units of the document @doc")
  (list-filter (map cdr (ahash-table->list units))
               (lambda (u) (== (lsp-unit-doc u) doc))))

(tm-define (lsp-unit-forget u)
  (ahash-remove! units (unit-key (lsp-unit-doc u) (lsp-unit-language u))))

(tm-define (lsp-unit-of-path path)
  (:synopsis "The unit whose virtual file is @path, or #f")
  (list-find (map cdr (ahash-table->list units))
             (lambda (u) (== (lsp-unit-path u) path))))

(tm-define (lsp-unit-synced! u)
  (:synopsis "The server has the lines of @u as they are now")
  (vector-set! u 6 (list (vector-ref u 4) (vector-ref u 5))))

(tm-define (lsp-unit-path-of u l o . opt)
  (:synopsis "The path in the document of the offset @o of the line @l of @u")
  ;; (#f beyond its blocks; with 'synced in @opt, as the blocks were when
  ;; the server was last given them: for its diagnostics)
  (let loop ((rs (if (pair? opt) (car (vector-ref u 6)) (vector-ref u 4)))
             (cs (if (pair? opt) (cadr (vector-ref u 6)) (vector-ref u 5)))
             (l l))
    (cond ((null? rs) #f)
          ((< l (car cs))
           (with p (tree->path (car rs)) (and p (append p (list l o)))))
          (else (loop (cdr rs) (cdr cs) (- l (car cs)))))))

(tm-define (lsp-unit-lo-of u p)
  (:synopsis "The line and offset in @u of the path @p, or #f")
  (let loop ((rs (vector-ref u 4)) (cs (vector-ref u 5)) (start 0))
    (and (nnull? rs)
         (let* ((b (tree->path (car rs)))
                (r (and b (list-starts? p b) (list-tail p (length b)))))
           (if (and r (= (length r) 2) (< (car r) (car cs)))
               (list (+ start (car r)) (cadr r))
               (loop (cdr rs) (cdr cs) (+ start (car cs))))))))

(tm-define (lsp-unit-write-back! u lines)
  (:synopsis "The blocks of @u get the lines @lines of the unit; #f if they cannot")
  ;; (edits of a server: they may change the number of the lines of one
  ;; block only, else they are not applied)
  (let* ((old (apply append (map block-lines (vector-ref u 4))))
         (n (length old)) (m (length lines))
         (cs (vector-ref u 5)))
    (if (== n m)
        (let loop ((rs (vector-ref u 4)) (cs cs) (ls lines))
          (when (nnull? rs)
            (set-lines! (car rs) (sublist ls 0 (car cs)))
            (loop (cdr rs) (cdr cs) (list-tail ls (car cs))))
          #t)
        ;; the lines which differ: in one block
        (let* ((p (let pre ((i 0) (a old) (b lines))
                    (if (and (pair? a) (pair? b) (== (car a) (car b)))
                        (pre (+ i 1) (cdr a) (cdr b)) i)))
               (s (let suf ((i 0) (a (reverse old)) (b (reverse lines)))
                    (if (and (pair? a) (pair? b) (< i (- (min n m) p))
                             (== (car a) (car b)))
                        (suf (+ i 1) (cdr a) (cdr b)) i))))
          (let loop ((rs (vector-ref u 4)) (cs cs) (start 0))
            (cond ((null? rs) #f)
                  ((and (<= start p) (<= (- n s) (+ start (car cs))))
                   (let* ((delta (- m n))
                          (new (sublist lines start (+ start (car cs) delta))))
                     ;; (a block keeps a line)
                     (set-lines! (car rs) (if (null? new) (list "") new))
                     #t))
                  (else (loop (cdr rs) (cdr cs) (+ start (car cs))))))))))

(tm-define (lsp-unit-of-lines t)
  (:synopsis "The unit whose lines are the tree @t, or #f")
  (list-find (map cdr (ahash-table->list units))
             (lambda (u) (tree-eq? (lsp-unit-lines u) t))))
