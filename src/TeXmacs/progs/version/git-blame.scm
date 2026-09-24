
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-blame.scm
;; DESCRIPTION : who changed which paragraphs of a document, and when
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Instead of the lines of the source file, as git blame does, we follow
;; the paragraphs of the document through its history: going back from the
;; current version, a paragraph is attributed to the first commit before
;; which it no longer occurs (as determined by longest common subsequences
;; of the lists of paragraphs of successive versions).

(texmacs-module (version git-blame)
  (:use (version version-git)
        (version version-merge)))

(define-preferences
  ("git blame depth" "30" noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attribution of paragraphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (paragraphs body)
  (if (tm-is? body 'document) (cdr body) (list body)))

(define (revision-paragraphs root c)
  ;; The paragraphs of the document at the commit c of its history
  (let* ((path (car (git-commit-files c)))
         (s (git-show-file root (git-commit-hash c) path))
         (u (git-absolute root path)))
    (and (!= s "")
         (and-with body (document-body (tree->stree (tree-import-loaded
                                                     s u "texmacs")))
           (paragraphs body)))))

(tm-define (git-blame name body)
  (:synopsis "Attribution of the paragraphs of @body, a version of @name")
  ;; Returns a list with, for each paragraph, the commit which introduced
  ;; it in its current form, or #f if the paragraph was not committed yet.
  ;; The second value is the oldest commit which was examined, if the
  ;; history was truncated (so that its paragraphs may be even older).
  (let* ((root (git-root name))
         (depth (or (string->number (get-preference "git blame depth")) 30))
         (commits (or (git-file-log name) '()))
         (hist (list-filter (sublist commits 0 (min depth (length commits)))
                            (lambda (c) (nnull? (git-commit-files c)))))
         (cur (paragraphs body))
         (n (length cur))
         (attr (make-vector n #f))
         (pos (make-vector n #f))
         (alive (make-vector n #t)))
    (if (null? hist)
        (values (vector->list attr) #f)
        (let loop ((l hist)
                   (prev cur)
                   (first? #t))
          (with ps (revision-paragraphs root (car l))
            (if (not ps)
                (values (vector->list attr) (if first? #f (car l)))
                (let ((m (version-match prev ps))
                      (c (car l)))
                  ;; follow the paragraphs into the version at commit c
                  (do ((k 0 (+ k 1))) ((>= k n))
                    (when (vector-ref alive k)
                      (let* ((j (if first? k (vector-ref pos k)))
                             (j2 (and j (vector-ref m j))))
                        (if j2
                            (vector-set! pos k j2)
                            (begin
                              (vector-set! alive k #f)
                              (vector-set! attr k
                                           (if first? #f
                                               (vector-ref attr k))))))))
                  ;; paragraphs still present are attributed to c so far
                  (do ((k 0 (+ k 1))) ((>= k n))
                    (when (vector-ref alive k)
                      (vector-set! attr k c)))
                  (if (null? (cdr l))
                      (values (vector->list attr)
                              (and (< (length hist) (length commits)) c))
                      (loop (cdr l) ps #f)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The blame page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define blame-colors
  '("#1f4e9a" "#a0301f" "#20703a" "#7a3a9a" "#1f7a8a" "#8a6a10" "#5a5a5a"))

(define (author-colors attr)
  ;; Colors for the authors, in the order of their first appearance
  (with t (make-ahash-table)
    (for (c attr)
      (when (and c (not (ahash-ref t (git-commit-author c))))
        (ahash-set! t (git-commit-author c)
                    (list-ref blame-colors
                              (modulo (length (ahash-table->list t))
                                      (length blame-colors))))))
    t))

(define (author-color colors c)
  (if c (ahash-ref colors (git-commit-author c)) "#909090"))

(define (blame-note root c earlier? color)
  `(git-note ,color
     ,(if (not c)
          "Not committed yet"
          `(concat (hlink ,(string-take (git-commit-hash c) 7)
                          ,(tmfs-url-commit root (git-commit-hash c)))
                   " " (strong ,(utf8->cork (git-commit-author c)))
                   ", " ,(git-commit-date c)
                   ,(if earlier? " or earlier" "")
                   ": " ,(utf8->cork (git-commit-subject c))))))

(define (blame-legend attr colors)
  ;; The authors with the number of paragraphs which they last changed
  (let* ((names (list-remove-duplicates
                 (map (lambda (c) (if c (git-commit-author c) #f)) attr)))
         (count (lambda (n)
                  (length (list-filter attr
                                       (lambda (c)
                                         (== (and c (git-commit-author c))
                                             n)))))))
    `(concat
      ,@(list-intersperse
         (map (lambda (n)
                `(git-note ,(if n (ahash-ref colors n) "#909090")
                   (concat (strong ,(if n (utf8->cork n) "Not committed"))
                           " " ,(number->string (count n)))))
              names)
         "   "))))

(define (list-intersperse l sep)
  (cond ((or (null? l) (null? (cdr l))) l)
        (else (cons* (car l) sep (list-intersperse (cdr l) sep)))))

(define (add-git-pages doc)
  ;; Add the style package for the notes to the document @doc
  (map (lambda (x)
         (cond ((and (tm-func? x 'style 1) (tm-is? (cadr x) 'tuple))
                `(style (tuple ,@(cdadr x) "git-pages")))
               ((tm-func? x 'style 1)
                `(style (tuple ,(cadr x) "git-pages")))
               (else x)))
       doc))

(define (blame-document name)
  (let* ((root (git-root name))
         (doc (tree->stree (if (buffer-exists? name)
                               (buffer-get name)
                               (tree-import name "texmacs"))))
         (body (or (document-body doc) '(document ""))))
    (receive (attr oldest) (git-blame name body)
      (with colors (author-colors attr)
        (let loop ((l (paragraphs body)) (a attr) (last 'none)
                   (acc (list (blame-legend attr colors))))
          (if (null? l)
              (add-git-pages
               (document-set-body doc (cons 'document (reverse acc))))
              (let* ((c (car a))
                     (same? (and (!= last 'none)
                                 (== (and c (git-commit-hash c))
                                     (and last (git-commit-hash last)))))
                     (earlier? (and oldest c (== c oldest)))
                     (acc* (if same? acc
                               (cons (blame-note root c earlier?
                                                 (author-color colors c))
                                     acc))))
                (loop (cdr l) (cdr a) c (cons (car l) acc*)))))))))

(tm-define (git-show-blame name)
  (:synopsis "Show who last changed each paragraph of @name")
  (cursor-history-add (cursor-path))
  (revert-buffer-revert (string-append "tmfs://blame/"
                                       (url->tmfs-string name))))

(tmfs-title-handler (blame name doc)
  (with u (tmfs-string->url name)
    (string-append (url->system (url-tail u)) " - Blame")))

(tmfs-format-handler (blame name)
  "texmacs")

(tmfs-load-handler (blame name)
  (blame-document (tmfs-string->url name)))
