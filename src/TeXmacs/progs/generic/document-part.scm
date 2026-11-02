
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-part.scm
;; DESCRIPTION : managing document parts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-part)
  (:use (generic document-edit)
        (text text-structure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flatten old-style projects into one file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inclusion-children t)
  ;; NOTE: keep the 'with' holding the initial environment of the
  ;; included file (chapter settings) around its body as one paragraph
  (cond ((tree-is? t 'document) (tree-children t))
	(else (list t))))

(define (inclusion? t)
  (and (tree-in? t '(include include*)) (tree-atomic? (tree-ref t 0))))

(define (expand-includes-one t r)
  (cond ((inclusion? t)
         (with u (url-relative r (unix->url (tree->string (tree-ref t 0))))
           (inclusion-children (tree-load-inclusion u))))
        ((and (tree-is? t 'with) (inclusion? (cAr (tree-children t))))
         (let* ((vars (map tree-copy (cDr (tree-children t))))
                (body (expand-includes-one (cAr (tree-children t)) r)))
           (list `(with ,@vars (document ,@body)))))
        (else (list (expand-includes t r)))))

(define (expand-includes t r)
  (cond ((tree-atomic? t) t)
	((tree-is? t 'document)
	 (with l (map (lambda (x) (expand-includes-one x r)) (tree-children t))
	   (cons 'document (apply append l))))
	(else
	 (with l (map (lambda (x) (expand-includes x r)) (tree-children t))
	   (cons (tree-label t) l)))))

(tm-define (buffer-expand-includes)
  (with t (buffer-tree)
    (tree-assign! t (expand-includes (buffer-tree) (buffer-master)))))

(define (buffer-master?) (== (get-init "project-flag") "true"))
(tm-define (buffer-toggle-master)
  (:synopsis "Toggle using current buffer as master file of project")
  (:check-mark "v" buffer-master?)
  (init-env "project-flag"
            (if (== (get-init "project-flag") "true") "false" "true")))

(define (project-attach* u)
  (with name (url->unix (url-delta (current-buffer) u))
    (project-attach name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main internal representations for document parts:
;;   :preamble -> (document (show-preamble preamble) (ignore body))
;;   :one, :several -> (document [optional (hide-preamble pre)] hide-show-list)
;;   :all -> (document [optional (hide-preamble preamble)] body)
;; In the case of :one and :several, the hide-show-list contains items
;;   (show-part id body alt-body)
;;   (hide-part id body alt-body)
;; Here id is an identifier for referencing purposes
;; The alt-body is evaluated in the background in the case of hidden parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define part-mode-table (make-ahash-table))

(define (buffer-body-paragraphs)
  (with t (buffer-tree)
    (if (tree-in? (tree-ref t 0) '(show-preamble hide-preamble))
	(cdr (tree-children t))
	(tree-children t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving paragraphs into and out of wrappers
;;
;; The paragraphs are moved with the node operations split, join,
;; insert-node and remove-node: they keep their identity, and the undo step
;; of a change of mode only records the wrappers which are made or removed,
;; not the paragraphs themselves.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (document-split-groups! t i sizes)
  ;; split the child @i of @t, a document, into consecutive documents
  ;; with the numbers of children in @sizes
  (when (> (length sizes) 1)
    (let* ((h (quotient (length sizes) 2))
	   (left (list-head sizes h)))
      (tree-split t i (apply + left))
      (document-split-groups! t (+ i 1) (list-tail sizes h))
      (document-split-groups! t i left))))

(define (document-join-range! t i n)
  ;; join the @n documents among the children of @t from @i into one;
  ;; neighbours are joined pairwise, so that each paragraph moves about
  ;; log n times
  (when (> n 1)
    (with h (quotient n 2)
      (for (k (.. 0 h))
	(tree-join t (+ i k)))
      (document-join-range! t i (- n h)))))

(define (buffer-wrap-body)
  ;; (document [preamble] b1 ... bn) -> (document [preamble] (document b1 ... bn))
  (let* ((pre? (buffer-has-preamble?))
	 (t (tree-insert-node (buffer-tree) 0 '(document))))
    (when pre?
      (tree-split t 0 1)
      (tree-remove-node (tree-ref t 0) 0))))

(define (buffer-unwrap-body)
  ;; (document [preamble] (document b1 ... bn)) -> (document [preamble] b1 ... bn)
  (with t (buffer-tree)
    (when (== (tree-arity t) 2)
      (tree-insert-node (tree-ref t 0) 0 '(document))
      (tree-join t 0))
    (tree-remove-node (buffer-tree) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing and hiding the preamble, making and flattening the parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (buffer-show-preamble)
  ;; (document (hide-preamble P) b1 ... bn)
  ;; -> (document (show-preamble P) (ignore (document b1 ... bn)))
  (when (tree-is? (tree-ref (buffer-tree) 0) 'hide-preamble)
    (buffer-flatten-parts)
    (tree-assign-node (tree-ref (buffer-tree) 0) 'show-preamble)
    (buffer-wrap-body)
    (tree-insert-node (tree-ref (buffer-tree) 1) 0 '(ignore))))

(define (buffer-hide-preamble)
  (with t (buffer-tree)
    (when (match? t '(document (show-preamble :%1) (ignore (document :*))))
      (tree-assign-node (tree-ref t 0) 'hide-preamble)
      (tree-remove-node (tree-ref t 1) 0)
      (buffer-unwrap-body))))

(tm-define (kbd-remove t forwards?)
  (:require (and (tree-is? t 'show-preamble) (tree-empty? (tree-ref t 0))))
  (buffer-set-part-mode :all)
  (when (buffer-has-preamble?)
    (tree-remove (buffer-tree) 0 1)))

(define (part-paragraphs t)
  ;; the paragraphs of the body of the part @t, which may be a string
  (with b (tree-ref t 1)
    (if (tree-atomic? b) (list b) (tree-children b))))

(define (buffer-flatten-subpart t)
  (if (tree-in? t '(show-part hide-part))
      (part-paragraphs t)
      (list t)))

(define (buffer-flatten-part t)
  (if (tree-in? t '(show-part hide-part))
      (append-map buffer-flatten-subpart (part-paragraphs t))
      (list t)))

(define (document-part? t)
  (tree-in? t '(show-part hide-part)))

(define (part-with-document? t)
  ;; a part whose body is a document, whose subparts have documents as bodies
  (and (== (tree-arity t) 3)
       (tree-is? (tree-ref t 1) 'document)
       (not (list-find (tree-children (tree-ref t 1))
		       (lambda (x)
			 (and (document-part? x)
			      (not (and (== (tree-arity x) 3)
					(tree-is? (tree-ref x 1) 'document)))))))))

(define (document-flatten-part! t i)
  ;; the child @i of @t becomes a document of the paragraphs which
  ;; buffer-flatten-part gives for it
  (if (not (document-part? (tree-ref t i)))
      (tree-insert-node (tree-ref t i) 0 '(document))
      (with b (tree-remove-node (tree-ref t i) 1)
	(when (list-find (tree-children b) document-part?)
	  (for (k (.. 0 (tree-arity b)))
	    (if (document-part? (tree-ref b k))
		(tree-remove-node (tree-ref b k) 1)
		(tree-insert-node (tree-ref b k) 0 '(document))))
	  (document-join-range! b 0 (tree-arity b))
	  (tree-remove-node b 0)))))

(define (buffer-flatten-parts)
  (when (document-part? (car (buffer-body-paragraphs)))
    (let* ((t (buffer-tree))
	   (l (buffer-body-paragraphs))
	   (preamble? (tree-in? (tree-ref t 0) '(show-preamble hide-preamble))))
      (if (list-find l (lambda (x)
			 (and (document-part? x) (not (part-with-document? x)))))
	  ;; parts which were not made by buffer-make-parts
	  (let* ((parts (apply append (map buffer-flatten-part l)))
		 (r (if preamble? (cons (tree-ref t 0) parts) parts)))
	    (tree-assign! t `(document ,@r)))
	  (let* ((s (if preamble? 1 0))
		 (n (length l)))
	    (for (k (.. 0 n))
	      (document-flatten-part! t (+ s k)))
	    (document-join-range! t s n)
	    (buffer-unwrap-body))))))

;; The parts which principal-sections-to-document-parts makes are
;;   (show-part id (document first subpart-1 ... subpart-m) alt)
;; with subparts (show-part id' (document b1 ... bk) alt'), or
;;   (show-part id (document first b2 ... bk) alt)
;; when there are no subparts.

(define (subpart-form? p)
  (and (pair? p) (== (car p) 'show-part) (== (length p) 4)
       (pair? (caddr p)) (== (car (caddr p)) 'document)))

(define (part-form? p)
  (and (subpart-form? p)
       (pair? (cdr (caddr p)))
       (with rest (cddr (caddr p))
	 (or (not (list-find rest pair?))
	     (not (list-find rest (lambda (x) (not (subpart-form? x)))))))))

(define (part-subparts p)
  (with rest (cddr (caddr p))
    (if (and (pair? rest) (pair? (car rest))) rest '())))

(define (subpart-size p)
  (length (cdr (caddr p))))

(define (part-size p)
  (with subs (part-subparts p)
    (if (null? subs) (subpart-size p)
	(+ 1 (apply + (map subpart-size subs))))))

(define (document-make-part! t i p)
  ;; the child @i of @t, a document of the paragraphs of the part @p,
  ;; becomes the part @p
  (with subs (part-subparts p)
    (when (nnull? subs)
      (with w (tree-insert-node (tree-ref t i) 0 '(document))
	(tree-split w 0 1)
	(tree-remove-node (tree-ref w 0) 0)
	(document-split-groups! w 1 (map subpart-size subs))
	(for (k (.. 0 (length subs)))
	  (with sp (list-ref subs k)
	    (tree-insert-node (tree-ref w (+ k 1)) 1
			      `(show-part ,(cadr sp) ,(cadddr sp))))))))
  (tree-insert-node (tree-ref t i) 1 `(show-part ,(cadr p) ,(cadddr p))))

(define (buffer-make-parts)
  (when (not (document-part? (car (buffer-body-paragraphs))))
    (let* ((t (buffer-tree))
	   (l (buffer-body-paragraphs))
	   (parts (buffer-new-parts l))
	   (preamble? (tree-in? (tree-ref t 0) '(show-preamble hide-preamble))))
      (if (or (list-find parts (lambda (p) (not (part-form? p))))
	      (!= (apply + (map part-size parts)) (length l)))
	  (with r (if preamble? (cons (tree-ref t 0) parts) parts)
	    (tree-assign! t `(document ,@r)))
	  (with s (if preamble? 1 0)
	    (buffer-wrap-body)
	    (document-split-groups! (buffer-tree) s (map part-size parts))
	    (for (k (.. 0 (length parts)))
	      (document-make-part! (buffer-tree) (+ s k) (list-ref parts k))))))))

(tm-define (buffer-get-part-mode)
  (:synopsis "Get the mode for document part selections")
  (cond ((tree-is? (tree-ref (buffer-tree) 0) 'show-preamble) :preamble)
	((tree-in? (car (buffer-body-paragraphs)) '(show-part hide-part))
	 (or (ahash-ref part-mode-table (current-buffer)) :one))
	(else :all)))

(define (buffer-test-part-mode? mode)
  (== (buffer-get-part-mode) mode))

(tm-define (buffer-set-part-mode mode)
  (:synopsis "Set the mode for document part selections")
  (:check-mark "v" buffer-test-part-mode?)
  (with old-mode (buffer-get-part-mode)
    (cond ((== mode old-mode) (noop))
          ((and (== mode :preamble) (not (buffer-has-preamble?)))
           (buffer-make-preamble))
	  ((== mode :preamble)
	   (when (tree-is? (tree-ref (buffer-tree) 0) 'hide-preamble)
	     (buffer-show-preamble)
	     (tree-go-to (buffer-tree) 0 0 :start)))
	  ((== mode :all)
	   (buffer-hide-preamble)
	   (buffer-flatten-parts)
	   (tree-go-to (car (buffer-body-paragraphs)) :start)
	   (update-current-buffer))
	  (else
	   (buffer-hide-preamble)
	   (buffer-make-parts)
	   (ahash-set! part-mode-table (current-buffer) mode)
	   (with first (car (buffer-parts-list #f))
	     (if (== mode :one)
		 (buffer-show-part first)
		 (buffer-go-to-part first)))
	   (update-current-buffer)))))

(tm-define (in-preamble-mode?)
  (tree-is? (tree-ref (buffer-tree) 0) 'show-preamble))

(tm-define (toggle-preamble-mode)
  (:synopsis "Toggle the preamble mode for the document")
  (:check-mark "v" in-preamble-mode?)
  (if (in-preamble-mode?)
      (buffer-set-part-mode :all)
      (buffer-set-part-mode :preamble)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listing the document parts
;;
;; A part is known by its identifier, the first argument of show-part and
;; hide-part: its number ("1", "2", ...), which is also what the references
;; to the labels of a hidden part name (#2, see go_to_label in
;; edit_cursor.cpp). The title of a part is only shown, with the name of
;; its file when it starts with an included file: two parts may have the
;; same title. For compatibility, buffer-show-part, buffer-toggle-part and
;; buffer-go-to-part also accept the title of a part when no part has it as
;; identifier.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (document-has-preamble? t)
  (:synopsis "Does the document tree @t contain a preamble?")
  (tree-in? (tree-ref t 0) '(show-preamble hide-preamble)))

(tm-define (document-get-preamble t)
  (:synopsis "Obtain the preamble of the document tree @t")
  (if (document-has-preamble? t) (tree-ref t 0 0) `(document "")))

(tm-define (buffer-has-preamble?)
  (:synopsis "Does the current buffer contain a preamble?")
  (document-has-preamble? (buffer-tree)))

(tm-define (buffer-get-preamble)
  (:synopsis "Obtain the preamble of the current buffer")
  (document-get-preamble (buffer-tree)))

(define (paragraph-file t)
  ;; the name of the file included by the paragraph @t, relative to the
  ;; master of the buffer, or #f
  (cond ((and (tm-func? t 'shared 3) (tm-atomic? (tm-ref t 1)))
         (url->unix (url-delta (buffer-master)
                               (unix->url (tm->string (tm-ref t 1))))))
        ((and (tm-in? t '(include include*)) (tm-atomic? (tm-ref t 0)))
         ;; (an include of the master is relative to it: ./pb.tm is pb.tm)
         (with u (url-relative (buffer-master)
                               (unix->url (tm->string (tm-ref t 0))))
           (url->unix (url-delta (buffer-master) u))))
        (else #f)))

(define (part-body-file b)
  ;; the file with which the body @b of a part starts, or #f
  (and (tm-func? b 'document) (> (tm-arity b) 0)
       (paragraph-file (tm-ref b 0))))

(define (buffer-new-parts l)
  ;; the parts which buffer-make-parts makes of the paragraphs @l
  (principal-sections-to-document-parts l))

(define (part-id t)
  ;; the identifier of the part @t
  (if (tree-atomic? (tree-ref t 0)) (tree->string (tree-ref t 0)) ""))

(define (document-part-name t)
  (if (== (tree-ref t 1) (string->tree ""))
      (string->tree "no title")
      (principal-section-title (tree-ref t 1))))

(define (part-title t)
  ;; the title of the part @t, as a string
  (with s (document-part-name t)
    (if (tree? s) (tree->string s) s)))

(define (document-get-part-ids l all?)
  ;; the identifiers of the parts among the paragraphs @l: the parts shown,
  ;; or all of them when @all?
  (append-map (lambda (t)
                (cond ((tree-atomic? t) '())
                      ((or (tree-is? t 'show-part)
                           (and all? (tree-is? t 'hide-part)))
                       (list (part-id t)))
                      ((tree-in? t '(document ignore))
                       (document-get-part-ids (tree-children t) all?))
                      (else '())))
              l))

(define (buffer-content-paragraphs)
  ;; the paragraphs of the body, also when the preamble is shown
  (with l (buffer-body-paragraphs)
    (if (match? l '((ignore (document :*))))
        (tree-children (tree-ref (car l) 0))
        l)))

(tm-define (buffer-parts-list all?)
  (:synopsis "Get the identifiers of the document parts of the current buffer")
  ;; when the buffer is not shown in parts: the identifiers of the parts
  ;; which it would have
  (with l (buffer-content-paragraphs)
    (if (document-part? (car l))
        (document-get-part-ids l all?)
        (map cadr (buffer-new-parts l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection of specific document parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (document-find-part-such t pred?)
  ;; the first part in @t which satisfies @pred?
  (cond ((tree-atomic? t) #f)
	((tree-in? t '(show-part hide-part))
	 (and (pred? t) t))
	((not (tree-in? t '(document ignore))) #f)
	(else (list-any (lambda (x) (document-find-part-such x pred?))
                        (tree-children t)))))

(define (document-find-part t id)
  ;; the part of @t with the identifier @id, else the first one with the
  ;; title @id
  (or (document-find-part-such t (lambda (x) (== (part-id x) id)))
      (document-find-part-such t (lambda (x) (== (part-title x) id)))))

(define (document-select-part t p)
  ;; show the part @p of @t and hide the others
  (cond ((tree-atomic? t) (noop))
	((tree-in? t '(show-part hide-part))
	 (with show? (== (tree->path t) (tree->path p))
	   (if (and show? (tree-is? t 'hide-part))
	       (tree-assign-node! t 'show-part))
	   (if (and (not show?) (tree-is? t 'show-part))
	       (tree-assign-node! t 'hide-part))))
	((not (tree-is? t 'document)) (noop))
	(else (for-each (lambda (x) (document-select-part x p))
			(tree-children t)))))

(define (buffer-part-body id)
  ;; the body of the document part @id
  (with l (buffer-content-paragraphs)
    (if (document-part? (car l))
        (and-with t (document-find-part (buffer-tree) id)
          (tree-ref t 1))
        (and-with p (list-find (buffer-new-parts l)
                               (lambda (p) (== (cadr p) id)))
          (caddr p)))))

(tm-define (buffer-part-title id)
  (:synopsis "The title of the document part with identifier @id")
  (and-with b (buffer-part-body id)
    (if (tm-atomic? b) "no title" (principal-section-title b))))

(tm-define (buffer-part-label id)
  (:synopsis "The label of the document part @id in the menus")
  ;; its title, followed by its file when it starts with an included file
  (let* ((title (or (buffer-part-title id) "no title"))
         (file (and-with b (buffer-part-body id) (part-body-file b)))
         (first? (== id (car (buffer-parts-list #t)))))
    (cond ((and file (== title "no title")) file)
          (file (string-append (upcase-first title) " (" file ")"))
          ((and (== title "no title") first?) "front matter")
          (else (upcase-first title)))))

(tm-define (buffer-go-to-part id)
  (:synopsis "Go to the part with identifier @id")
  (with t (document-find-part (buffer-tree) id)
    (and t (tree-go-to t 1 :start))))

(tm-define (buffer-show-part id)
  (:synopsis "Show the document part with identifier @id")
  (when (== (buffer-get-part-mode) :one)
    (and-with t (document-find-part (buffer-tree) id)
      (document-select-part (buffer-tree) t)
      (tree-go-to t 1 :start))))

(tm-define (buffer-toggle-part id)
  (:synopsis "Toggle the visibility of the document part with identifier @id")
  (with t (and (== (buffer-get-part-mode) :several)
               (document-find-part (buffer-tree) id))
    (when (and t (list-find (buffer-parts-list #f)
                            (lambda (x) (!= x (part-id t)))))
      (cond ((tree-is? t 'show-part)
	     (tree-assign-node! t 'hide-part)
	     (buffer-go-to-part (car (buffer-parts-list #f))))
	    ((tree-is? t 'hide-part)
	     (tree-assign-node! t 'show-part)
	     (tree-go-to t 1 :start))))))

(tm-define (buffer-make-preamble)
  (:synopsis "Create a preamble for the current document")
  (when (not (buffer-has-preamble?))
    (with t (buffer-tree)
      (tree-insert! t 0 '((hide-preamble (document ""))))
      (buffer-set-part-mode :preamble))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The cursor after undo and redo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (part-hidden-tag? t)
  (tree-in? t '(hide-preamble ignore hide-part)))

(define (part-hidden-ancestors t p)
  ;; when the path @p from @t goes through a hidden node, the pairs
  ;; (node . index) of the path down to that node, innermost first
  (let loop ((t t) (p p) (acc '()))
    (if (or (tree-atomic? t) (null? p) (null? (cdr p))
	    (>= (car p) (tree-arity t)))
	#f
	(let* ((i (car p))
	       (acc* (cons (cons t i) acc)))
	  (if (or (part-hidden-tag? (tree-ref t i))
		  (and (tree-is? t 'show-part) (!= i 1)))
	      acc*
	      (loop (tree-ref t i) (cdr p) acc*))))))

(define (part-visible-border t start?)
  ;; a visible cursor position at the start or the end of @t, or #f
  (cond ((part-hidden-tag? t) #f)
	((tree-is? t 'show-part)
	 (and (> (tree-arity t) 1)
	      (part-visible-border (tree-ref t 1) start?)))
	((tree-is? t 'document)
	 (list-any (lambda (x) (part-visible-border x start?))
		   (if start? (tree-children t)
		       (reverse (tree-children t)))))
	(start? (path-start (root-tree) (tree->path t)))
	(else (path-end (root-tree) (tree->path t)))))

(define (part-visible-near ancestors)
  ;; a visible cursor position near the hidden child of the innermost
  ;; node of @ancestors: in a sibling after it, else in one before it,
  ;; else near that node itself
  (and (nnull? ancestors)
       (let* ((t (caar ancestors))
	      (i (cdar ancestors)))
	 (or (cond ((tree-is? t 'document)
		    (let ((l (tree-children t)))
		      (or (list-any (lambda (x) (part-visible-border x #t))
				    (list-tail l (+ i 1)))
			  (list-any (lambda (x) (part-visible-border x #f))
				    (reverse (list-head l i))))))
		   ((tree-is? t 'show-part)
		    (and (> (tree-arity t) 1)
			 (part-visible-border (tree-ref t 1) #t)))
		   (else #f))
	     (part-visible-near (cdr ancestors))))))

(define (document-has-hidden-parts? t)
  ;; can a part of the document tree @t be hidden?
  (and (> (tree-arity t) 0)
       (or (tree-in? (tree-ref t 0)
		     '(show-preamble hide-preamble show-part hide-part))
	   (and (> (tree-arity t) 1)
		(tree-in? (tree-ref t 1) '(show-part hide-part))))))

(tm-define (notify-undo-redo)
  ;; undo and redo put the cursor at the change, which may be hidden in
  ;; a preamble or in a part which is not shown; move it to a place
  ;; nearby where typing is seen; in source mode, everything is shown
  (:require (and (document-has-hidden-parts? (buffer-tree))
		 (!= (get-init "preamble") "true")))
  (let* ((bp (buffer-path))
	 (c (cursor-path)))
    (when (list-starts? c bp)
      (and-with a (part-hidden-ancestors (buffer-tree)
					 (list-tail c (length bp)))
	(and-with p (part-visible-near a)
	  (go-to p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making hidden parts visible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-show-hidden t)
  (:require (and (tree-is? t 'hide-part) (== (tree-up t) (buffer-tree))))
  (if (== (buffer-get-part-mode) :one)
      (begin
	(document-select-part (tree-up t) t)
	;; only this part is shown now: a cursor outside it is hidden
	(when (not (list-starts? (cursor-path) (tree->path t)))
	  (tree-go-to t 1 :start)))
      (tree-assign-node! t 'show-part)))

(tm-define (show-hidden-part id)
  (:synopsis "Make hidden part with identifier @id visible")
  (with search? (lambda (t) (match? t `(hide-part ,id :%2)))
    (and-with t (list-find (tree-children (buffer-tree)) search?)
      (tree-show-hidden t)
      #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer with included files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-include? t)
  (and (tm-func? t 'include 1)
       (tm-atomic? (tm-ref t 0))))

(tm-define (tm-get-includes doc)
  (cond ((tm-func? doc 'with)
	 (tm-get-includes (tm-ref doc :last)))
	((tm-func? doc 'document)
	 (append-map tm-get-includes (tm-children doc)))
	((tm-include? doc)
	 (list (tm->string (tm-ref doc 0))))
	(else (list))))

(tm-define (buffer-get-includes)
  (tm-get-includes (buffer-tree)))

(tm-define (buffer-contains-includes?)
  (nnull? (buffer-get-includes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The dynamic document part menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (document-parts-menu)
  (let* ((all (buffer-parts-list #t))
	 (active (buffer-parts-list #f)))
    (for (id all)
      ((check (eval (buffer-part-label id)) "v" (in? id active))
       (if (== (buffer-get-part-mode) :one)
           (buffer-show-part id)
           (buffer-toggle-part id))))))

(menu-bind preamble-menu
  (if (buffer-has-preamble?)
      ("Show preamble" (toggle-preamble-mode)))
  (if (not (buffer-has-preamble?))
      ("Create preamble" (toggle-preamble-mode)))
  ("Show main document" (buffer-set-part-mode :all)))

(menu-bind document-part-menu
  (if (buffer-has-preamble?)
      ("Show preamble" (toggle-preamble-mode)))
  (if (not (buffer-has-preamble?))
      ("Create preamble" (toggle-preamble-mode)))
  ("Show one part" (buffer-set-part-mode :one))
  ("Show several parts" (buffer-set-part-mode :several))
  ("Show all parts" (buffer-set-part-mode :all))
  (if (or (in? (buffer-get-part-mode) '(:one :several))
	  (!= (get-init-tree "sectional-short-style") (tree 'macro "false")))
      ---
      (when (in? (buffer-get-part-mode) '(:one :several))
	(link document-parts-menu))))

(menu-bind document-part-menu
  (:require (buffer-contains-includes?))
  (link document-master-menu))

(menu-bind project-manage-menu
  (if (!= (url-suffix (current-buffer)) "tp")
      ("Use as master" (buffer-toggle-master)))
  (when (buffer-contains-includes?)
    ("Expand inclusions" (buffer-expand-includes)))
  ---
  (when (not (project-attached?))
    ("Attach master"
     (choose-file project-attach* "Attach master file for project" "texmacs")))
  (when (project-attached?)
    ("Detach master" (project-detach))))
