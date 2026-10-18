
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
;; file next to the document, .<name>-block<k>.<suffix>, which is never
;; written (the server has its text from didOpen and didChange), in the
;; project of the document. Its positions are those of its lines, as in a
;; source file, see doc/code-features.md (phase 7). A block is known by its
;; tree; when it is removed, or its document closed, its virtual document
;; is closed on the server.

(texmacs-module (lsp lsp-blocks)
  (:use (lsp lsp-base)
        (lsp lsp-client)))

;; #t or #f instead of the preference (for the tests, which do not write
;; the preferences), or 'pref
(define blocks-forced 'pref)

(tm-define (lsp-blocks-force! on)
  (:synopsis "Follow the code blocks (#t), not (#f), as the preference says ('pref)")
  (set! blocks-forced on))

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
  (let* ((base (url-basename (system->url doc)))
         (k (or (ahash-ref block-counts doc) 1)))
    (ahash-set! block-counts doc (+ k 1))
    (url->system (url-append (url-head (system->url doc))
                             (string-append "." base "-block"
                                            (number->string k) "."
                                            (lan-suffix lan))))))

(define (entry-of entries root)
  (list-find entries (lambda (e) (tree-eq? (car e) root))))

(tm-define (lsp-block-path doc root lan)
  (:synopsis "The virtual file of the block @root of the document @doc")
  ;; (doc: the system path of the document; given once, then kept)
  (let* ((entries (or (ahash-ref block-files doc) '()))
         (e (entry-of entries root)))
    (if (and e (== (cadr e) lan)) (caddr e)
        (with p (new-block-path doc lan)
          (ahash-set! block-files doc
                      (cons (list root lan p)
                            (list-filter entries
                                         (lambda (x) (not (tree-eq? (car x) root))))))
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
