;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-edit.scm
;; DESCRIPTION : editing programs with their syntax trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The editing functions of prog-edit.scm ask here first: when the code
;; around the cursor has a syntax tree (its program language is highlighted
;; by tree-sitter), the selection grows by syntax nodes and brackets match
;; by the tree, so that those in strings and comments are left alone.
;;
;; The code is a region (Plugins/TreeSitter/syntax_region.cpp): a document
;; of lines or a single string. Positions in it are points (row column), the
;; column being the position in the TeXmacs string of the line. This module
;; only uses the glue, so that loading it does not load the generator.

(texmacs-module (treesit ts-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points and nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (point<? a b)
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cadr a) (cadr b)))))

(define (point<=? a b) (not (point<? b a)))

;; a node is (type named? start end field)
(define (node-type n) (car n))
(define (node-named? n) (cadr n))
(define (node-start n) (caddr n))
(define (node-end n) (cadddr n))

(define-public (syntax-enlarge-range root lan start end)
  ;; the range (start end) of the smallest node which contains start..end
  ;; and is larger, or #f
  (let loop ((l (syntax-region-node-range root lan (car start) (cadr start)
                                          (car end) (cadr end))))
    (and (pair? l)
         (let ((s (node-start (car l))) (e (node-end (car l))))
           (if (and (point<=? s start) (point<=? end e)
                    (or (point<? s start) (point<? end e)))
               (list s e)
               (loop (cdr l)))))))

(define brackets '(("(" . ")") ("[" . "]") ("{" . "}")))

(define (closing-of type) (assoc-ref brackets type))

(define (opening-of type)
  (let ((p (list-find brackets (lambda (b) (== (cdr b) type)))))
    (and p (car p))))

(define (bracket-at root lan p)
  ;; the bracket token which starts at p, with its parent: (node . parent)
  (let ((l (syntax-region-node-at root lan (car p) (cadr p))))
    (and (pair? l) (pair? (cdr l))
         (let ((n (car l)))
           (and (not (node-named? n))
                (== (node-start n) p)
                (or (closing-of (node-type n)) (opening-of (node-type n)))
                (cons n (cadr l)))))))

(define-public (syntax-matching-bracket root lan p)
  ;; when a bracket token of the tree starts at the point p, the point of
  ;; the bracket which matches it, else #f
  (let ((b (bracket-at root lan p)))
    (and b
         (let* ((type (node-type (car b)))
                (parent (cdr b))
                (close (closing-of type))
                (q (if close
                       (let ((e (node-end parent)))
                         (list (car e) (- (cadr e) 1)))
                       (node-start parent)))
                (b* (and (>= (cadr q) 0) (bracket-at root lan q))))
           (and b* (== (node-type (car b*)) (or close (opening-of type)))
                q)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (path-context p lan)
  ;; (root-path root row column) for the position p in a string of code
  (and (pair? p) (pair? (cdr p))
       (let* ((leaf (cDr p))
              (t (path->tree leaf)))
         (and (tree-atomic? t)
              (let ((parent (and (pair? leaf) (path->tree (cDr leaf)))))
                (if (and parent (tree-is? parent 'document))
                    (list (cDr leaf) parent (cAr leaf) (cAr p))
                    (list leaf t 0 (cAr p))))))))

(define (context-path ctx point)
  ;; the path of a point of the region of ctx (a point after the last line,
  ;; such as the end of the root node, is the end of that line)
  (let ((root (cadr ctx)))
    (cond ((not (tree-is? root 'document))
           (append (car ctx) (list (min (cadr point) (string-length (tree->string root))))))
          ((< (car point) (tree-arity root))
           (append (car ctx) (list (car point) (cadr point))))
          (else
           (let ((last (- (tree-arity root) 1)))
             (append (car ctx)
                     (list last (string-length (tree->string (tree-ref root last))))))))))

(define-public (syntax-cursor-language)
  ;; the language of tree-sitter of the code at the cursor, or #f
  (and (in-prog?)
       (let ((id (tree-sitter-program-language (get-env "prog-language"))))
         (and (>= id 0) id))))

(define-public (syntax-select-enlarge)
  ;; select the smallest syntax node around the selection (or the cursor);
  ;; #f when the code has no syntax tree
  (let* ((lan (syntax-cursor-language))
         (sel? (selection-active-normal?))
         (a (and lan (path-context (if sel? (selection-get-start) (cursor-path)) lan)))
         (b (and a (path-context (if sel? (selection-get-end) (cursor-path)) lan))))
    (and a b (== (car a) (car b))
         (let ((r (syntax-enlarge-range (cadr a) lan (cddr a) (cddr b))))
           (when r
             (selection-set (context-path a (car r)) (context-path a (cadr r))))
           #t))))

(define (path++ p) (rcons (cDr p) (+ (cAr p) 1)))

(define-public (syntax-select-brackets)
  ;; highlight the bracket at the cursor, or before it, and its match; #f
  ;; when the code has no syntax tree
  (let* ((lan (syntax-cursor-language))
         (ctx (and lan (path-context (cursor-path) lan))))
    (and ctx
         (let* ((root (cadr ctx))
                (p (cddr ctx))
                (p* (list (car p) (- (cadr p) 1)))
                (here (syntax-matching-bracket root lan p))
                (before (and (not here) (>= (cadr p*) 0)
                             (syntax-matching-bracket root lan p*)))
                (from (if here p p*))
                (to (or here before)))
           (if (not to)
               (when (nnull? (get-alt-selection "brackets"))
                 (cancel-alt-selection "brackets"))
               (let ((x (context-path ctx from)) (y (context-path ctx to)))
                 (set-alt-selection "brackets"
                                    (list x (path++ x) y (path++ y)))))
           #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs of sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (text-end body)
  ;; the point after the last character which is not a space
  (let loop ((row (- (tree-arity body) 1)))
    (if (< row 0) '(0 0)
        (let* ((line (tree-ref body row))
               (s (if (tree-atomic? line) (tree->string line) ""))
               (n (let trim ((n (string-length s)))
                    (if (and (> n 0) (char-whitespace? (string-ref s (- n 1))))
                        (trim (- n 1))
                        n))))
          (if (> n 0) (list row n) (loop (- row 1)))))))

(define-public (syntax-incomplete? body lan)
  ;; whether the program of body (a document of lines) has an error which
  ;; reaches its end (an open bracket), or ends with an empty node (the
  ;; block after a line which starts one, in Python): then only more lines
  ;; can complete it
  (and (tree-is? body 'document)
       (let ((end (text-end body)))
         (and (not (== end '(0 0)))
              (or (list-or (map (lambda (e) (point<=? end (caddr e)))
                                (syntax-region-errors body lan)))
                  (let ((l (syntax-region-last-nodes body lan)))
                    (and (pair? l)
                         (list-or (map (lambda (n)
                                         (and (node-named? n)
                                              (== (node-start n) (node-end n))))
                                       (cdr l))))))))))

(define-public (syntax-input-incomplete? body)
  ;; the same for the input of a session, in the language at the cursor
  (let ((id (tree-sitter-program-language (get-env "prog-language"))))
    (and (>= id 0) (syntax-incomplete? body id))))
