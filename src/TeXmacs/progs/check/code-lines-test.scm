
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-lines-test.scm
;; DESCRIPTION : tests of the lines of source code (the tag code-lines)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <code-lines|<document|line|...>> holds lines of source code, typeset by
;; one bridge as one box, without a paragraph per line
;; (Typeset/Bridge/bridge_code.cpp, Typeset/Boxes/Basic/code_boxes.cpp). It
;; must look and behave as the same lines in a document: the same places,
;; the same cursor movements, the same editing and the same text when
;; exported. Box based movements need the changes applied to the view
;; (apply-changes, see editing-test.scm).

(texmacs-module (check code-lines-test)
  (:use (check check-lib)))

(define lines '("int a;" "  b = 2;" "c" "" "return a + b;"))

(define (with-body doc thunk)
  ;; run @thunk in a new buffer holding @doc, typeset, then close it
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (update-forced)
    (apply-changes)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

;; (in a monospaced font, as the files of source code: a space is a glyph
;; there, and the lines all have the height of the font)
(define (plain-doc)
  `(document (with "font-family" "tt" "language" "verbatim"
               (document ,@lines))))
(define (code-doc)
  `(document (with "font-family" "tt" "language" "verbatim"
               (code-lines (document ,@lines)))))

(define (line-path code? i)
  ;; the path of the line @i in the body of the current buffer
  (if code? (list 0 4 0 i) (list 0 4 i)))

(define (at . l) (append (buffer-path) l))

(define (rel p) (list-tail p (length (buffer-path))))

(define (step thunk)
  (thunk) (update-forced) (apply-changes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The places of the lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rects code?)
  ;; the rectangles of the non empty lines, relative to the first one
  (let* ((r0 (tree-bounding-rectangle
              (path->tree (apply at (line-path code? 0))))))
    (map (lambda (i)
           (with r (tree-bounding-rectangle
                    (path->tree (apply at (line-path code? i))))
             (list (- (car r) (car r0)) (- (cadr r) (cadr r0))
                   (- (caddr r) (car r0)) (- (cadddr r) (cadr r0)))))
         (list 0 1 2 4))))

(define (test-places)
  (check-group "the lines are where they are in a document")
  (let ((plain #f) (code #f))
    (with-body (plain-doc) (lambda () (set! plain (rects #f))))
    (with-body (code-doc) (lambda () (set! code (rects #t))))
    (check-true (list? plain))
    (check= code plain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (moves code?)
  ;; the cursor after some box based movements, as (line pos)
  (let* ((r '())
         (note (lambda ()
                 (with p (rel (cursor-path))
                   (set! r (cons (list-tail p (if code? 3 2)) r))))))
    (go-to (apply at (append (line-path code? 1) (list 4))))
    (step (lambda () #t)) (note)
    (step kbd-down) (note)
    (step kbd-down) (note)
    (step kbd-up) (note)
    (step kbd-right) (note)
    (step kbd-end-line) (note)
    (step kbd-left) (note)
    (reverse r)))

(define (test-cursor)
  (check-group "the cursor moves as in a document")
  (let ((plain #f) (code #f))
    (with-body (plain-doc) (lambda () (set! plain (moves #f))))
    (with-body (code-doc) (lambda () (set! code (moves #t))))
    (check= (car code) '(1 4))
    (check= code plain)
    (check= (length code) 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing and export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-editing)
  (check-group "editing the lines")
  (with-body (code-doc)
    (lambda ()
      (go-to (at 0 4 0 0 3))
      (step (lambda () (insert "XY")))
      (check= (tree->stree (path->tree (at 0 4 0 0))) "intXY a;")
      (step insert-return)
      (check= (tree->stree (path->tree (at 0 4 0 0))) "intXY")
      (check= (tree->stree (path->tree (at 0 4 0 1))) " a;")
      (check= (rel (cursor-path)) '(0 4 0 1 0))
      (check= (tree-arity (path->tree (at 0 4 0))) 6)
      (step kbd-backspace)
      (check= (tree->stree (path->tree (at 0 4 0 0))) "intXY a;")
      (check= (tree-arity (path->tree (at 0 4 0))) 5)
      (check= (rel (cursor-path)) '(0 4 0 0 5))
      ;; the cursor still goes through the box after the edits
      (step kbd-down)
      (check= (rel (cursor-path)) '(0 4 0 1 5)))))

;; a block without lines, and lines which are not strings (markup put
;; inside by hand), are typeset without an error
(define (test-odd-contents)
  (check-group "odd contents")
  (with-body '(document (code-lines (document)))
    (lambda () (check-true (tree? (buffer-get-body (current-buffer))))))
  (with-body '(document (code-lines (document "a" (strong "b") "c")))
    (lambda ()
      (go-to (at 0 0 2 1))
      (step kbd-up)
      (check-true (list? (cursor-path))))))

(define (test-export)
  (check-group "export and import")
  (with-body (code-doc)
    (lambda ()
      (check= (texmacs->code (path->tree (at 0 4)) "utf-8")
              (string-join lines "\n"))))
  ;; the importer makes the tag only when TEXMACS_CODE_LINES is 1
  (let ((old (getenv "TEXMACS_CODE_LINES")))
    (setenv "TEXMACS_CODE_LINES" "1")
    (let ((t (tree->stree (cpp-verbatim->texmacs "a\nb" #f "utf-8"))))
      (check= (cadr (assoc 'body (cdr t)))
              '(document (code-lines (document "a" "b")))))
    (setenv "TEXMACS_CODE_LINES" "")
    (let ((t (tree->stree (cpp-verbatim->texmacs "a\nb" #f "utf-8"))))
      (check= (cadr (assoc 'body (cdr t))) '(document "a" "b")))
    (setenv "TEXMACS_CODE_LINES" (or old ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (code-lines-test-failures)
  (:synopsis "Run the tests of the tag code-lines, return the number of failures")
  (check-suite "code-lines")
  (test-places)
  (test-cursor)
  (test-editing)
  (test-odd-contents)
  (test-export)
  (check-end))
