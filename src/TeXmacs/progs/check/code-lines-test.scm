
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

;; lines which are not strings (markup put inside by hand) are typeset
;; without an error
(define (test-odd-contents)
  (check-group "odd contents")
  (with-body '(document (code-lines (document "a" (strong "b") "c")))
    (lambda ()
      (go-to (at 0 0 2 1))
      (step kbd-up)
      (check-true (list? (cursor-path))))))

;; In paper mode, the lines which did not change are used again from one
;; pass to the next (bridge_code.cpp), with the paths which they keep: after
;; lines are inserted, changed and joined, every line is where it is in a
;; document which had the same edits, and the cursor goes where it goes there
(define (all-rects code?)
  (let* ((n (tree-arity (path->tree (apply at (if code? '(0 4 0) '(0 4))))))
         (r0 (tree-bounding-rectangle
              (path->tree (apply at (line-path code? 0))))))
    (map (lambda (i)
           (with t (path->tree (apply at (line-path code? i)))
             (if (== (tree->string t) "") '()
                 (with r (tree-bounding-rectangle t)
                   (list i (- (car r) (car r0)) (- (cadr r) (cadr r0))
                         (- (caddr r) (car r0)) (- (cadddr r) (cadr r0)))))))
         (.. 0 n))))

(define (edits code?)
  (let* ((r '())
         (note (lambda ()
                 (set! r (cons (list (list-tail (rel (cursor-path))
                                                (if code? 3 2))
                                     (all-rects code?))
                               r))))
         (goto (lambda (i pos)
                 (go-to (apply at (append (line-path code? i) (list pos))))
                 (step (lambda () #t)))))
    (init-env "page-medium" "paper")
    (step (lambda () #t))
    (goto 0 0) (note)
    (goto 1 8) (step insert-return) (note)
    (step (lambda () (insert "x= 1;"))) (note)
    (goto 4 0) (step (lambda () (insert "Q"))) (note)
    (goto 3 0) (step kbd-backspace) (note)
    (step kbd-down) (note)
    (goto 0 3) (step insert-return) (note)
    (reverse r)))

(define (test-paper-edits)
  (check-group "editing in paper mode")
  (let ((plain #f) (code #f))
    (with-body (plain-doc) (lambda () (set! plain (edits #f))))
    (with-body (code-doc) (lambda () (set! code (edits #t))))
    (check= (length code) 7)
    (check= (car (list-ref code 1)) '(2 0))
    (for-each (lambda (k)
                (check= (list-ref code k) (list-ref plain k)))
              (.. 0 7))))

;; A line wider than the paragraph is broken into rows: after the last space
;; which fits, or where the row is full (a word which does not fit). The
;; cursor goes from row to row in the line, and the rows are at the same
;; places in paper mode (a page item per row) and in papyrus mode (one box)
(define soft-line
  (string-append "  x = " (apply string-append (make-list 40 "alpha + ")) "1;"))
(define hard-line
  (string-append "  s= \"" (make-string 300 #\x) "\";"))
(define wrap-lines (list "int a;" soft-line "c" hard-line "d" "e"))

(define (wrap-run medium)
  (let* ((r '())
         (cur (lambda () (list-tail (rel (cursor-path)) 3)))
         (goto (lambda (i pos)
                 (go-to (at 0 4 0 i pos))
                 (step (lambda () #t)))))
    (init-env "page-medium" medium)
    (step (lambda () #t))
    (goto 0 0)
    (set! r (cons (all-rects #t) r))
    (goto 1 3)
    (for-each (lambda (k)
                (step kbd-down)
                (set! r (cons (cur) r)))
              (.. 0 6))
    (goto 3 2) (step kbd-down) (set! r (cons (cur) r))
    (step kbd-up) (set! r (cons (cur) r))
    (reverse r)))

(define (test-wrapping)
  (check-group "soft line breaks")
  (let* ((doc `(document (with "font-family" "tt" "language" "verbatim"
                           (code-lines (document ,@wrap-lines)))))
         (paper #f) (papyrus #f))
    (with-body doc (lambda () (set! paper (wrap-run "paper"))))
    (with-body doc (lambda () (set! papyrus (wrap-run "papyrus"))))
    (check= (length paper) 9)
    ;; the soft line: four more rows, then the next lines
    (check= (map car (list-head (cdr paper) 6)) '(1 1 1 1 2 3))
    (check-true (> (cadr (list-ref paper 1)) (string-length "  x = ")))
    ;; the hard line: its second row is in the same line, further (the
    ;; first row ends after the space)
    (check= (list-ref paper 7) '(3 7))
    (check= (list-ref paper 8) '(3 2))
    ;; papyrus as paper: the cursor, and the lines after the wrapped ones
    ;; (the outline of a selection of several rows is not the same)
    (check= (cdr papyrus) (cdr paper))
    (check= (map (lambda (i) (list-ref (car papyrus) i)) '(0 2 4 5))
            (map (lambda (i) (list-ref (car paper) i)) '(0 2 4 5)))))

;; In paper mode an edit makes again the rows of the lines which changed
;; only (and of the first and the last line: a paragraph with what is
;; around), also in a block of many lines and with lines which are wrapped
(define (test-rows-made)
  (check-group "rows made again in paper mode")
  (let* ((many (map (lambda (i)
                      (if (== (modulo i 50) 7) soft-line
                          (string-append "  line " (number->string i) ";")))
                    (.. 0 300)))
         (doc `(document (with "font-family" "tt" "language" "verbatim"
                           (code-lines (document ,@many))))))
    (with-body doc
      (lambda ()
        (init-env "page-medium" "paper")
        (step (lambda () #t))
        (go-to (at 0 4 0 150 3))
        (step (lambda () #t))
        (let ((n0 (typeset-code-rows-made)))
          (step (lambda () (insert "Q")))
          (check-true (<= (- (typeset-code-rows-made) n0) 4))
          (check= (tree->string (path->tree (at 0 4 0 150))) "  lQine 150;"))
        (let ((n0 (typeset-code-rows-made)))
          (step insert-return)
          (check-true (<= (- (typeset-code-rows-made) n0) 6)))
        (go-to (at 0 4 0 157 3))
        (step (lambda () #t))
        (let ((n0 (typeset-code-rows-made)))
          ;; (the line 157 was the line 156: a wrapped line)
          (step (lambda () (insert "Q")))
          (check-true (<= (- (typeset-code-rows-made) n0) 8)))))))

;; The records of the lines (their soft line breaks) follow the lines when
;; lines are inserted, removed, split and joined (the notifications of the
;; editor, see bridge_code.cpp): an edit makes the record of the line which
;; changed only, also for the wrapped lines after it
(define (test-records)
  (check-group "records of the lines")
  (let* ((many (map (lambda (i)
                      (if (== (modulo i 10) 3) soft-line
                          (string-append "  line " (number->string i) ";")))
                    (.. 0 100)))
         (doc `(document (with "font-family" "tt" "language" "verbatim"
                           (code-lines (document ,@many))))))
    (with-body doc
      (lambda ()
        (init-env "page-medium" "papyrus")
        (step (lambda () #t))
        (let ((made (lambda (thunk)
                      (let ((n0 (typeset-code-records-made)))
                        (step thunk)
                        (- (typeset-code-records-made) n0)))))
          (go-to (at 0 4 0 1 3))
          (step (lambda () #t))
          ;; a key in a short line, Return, a key in the new line, joining
          ;; it again, removing a wrapped line: no record made again but
          ;; for a wrapped line which changed
          (check= (made (lambda () (insert "Q"))) 0)
          (check= (made insert-return) 0)
          (check= (made (lambda () (insert "R"))) 0)
          (check= (made (lambda () (kbd-backspace) (kbd-backspace))) 0)
          (check= (tree->string (path->tree (at 0 4 0 1))) "  lQine 1;")
          (go-to (at 0 4 0 13 4))
          (step (lambda () #t))
          (check= (made (lambda () (insert "S"))) 1)
          (check= (made insert-return) 1)
          (check= (made kbd-backspace) 1)
          (check= (tree->string (path->tree (at 0 4 0 13)))
                  (string-append (substring soft-line 0 4) "S"
                                 (substring soft-line 4
                                            (string-length soft-line))))
          ;; the same document typeset from the start has the same rows
          (check= (typeset-check-mismatches) mismatches))))))

;; The colours of a line depend on the lines before it (a comment of several
;; lines, a line of the preprocessor continued with a backslash): the lexer
;; of a block keeps the state at the start of each line, and an edit changes
;; them from the line which changed (bridge_code.cpp, code_boxes.cpp). With
;; TEXMACS_TYPESET_CACHES=check (tests/scheme/check.sh code-lines-check),
;; the states are compared at each pass with the states made from the first
;; line
(define (test-colour-states)
  (check-group "states of the colours")
  (let* ((c-lines (append (list "#define TWICE(x) \\" "  ((x) + (x))"
                                "int a; /* one" "two */ int b;")
                          (map (lambda (i)
                                 (string-append "int v" (number->string i)
                                                " = " (number->string i) ";"))
                               (.. 0 40))))
         (doc `(document (with "font-family" "tt" "language" "verbatim"
                                "mode" "prog" "prog-language" "cpp"
                           (code-lines (document ,@c-lines))))))
    (with-body doc
      (lambda ()
        (let ((line (lambda (i) (at 0 8 0 i))))
          ;; open a comment, which goes to the next star and slash
          (go-to (append (line 10) '(0)))
          (step (lambda () (insert "/* ")))
          (go-to (append (line 20) '(0)))
          (step (lambda () (insert "*/ ")))
          ;; remove the opening again, and join lines
          (go-to (append (line 10) '(3)))
          (step (lambda () (kbd-backspace) (kbd-backspace) (kbd-backspace)))
          (go-to (append (line 30) '(0)))
          (step kbd-backspace)
          ;; a line of the preprocessor continued, then not
          (go-to (append (line 0) (list (string-length "#define TWICE(x) "))))
          (step (lambda () (kbd-delete)))
          (step insert-return)
          (check= (tree->string (path->tree (line 0))) "#define TWICE(x) ")
          (check= (typeset-check-mismatches) mismatches))))))

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

(define mismatches 0)

(tm-define (code-lines-test-failures)
  (:synopsis "Run the tests of the tag code-lines, return the number of failures")
  (check-suite "code-lines")
  (set! mismatches (typeset-check-mismatches))
  (test-places)
  (test-cursor)
  (test-editing)
  (test-odd-contents)
  (test-paper-edits)
  (test-wrapping)
  (test-rows-made)
  (test-records)
  (test-colour-states)
  (test-export)
  ;; with TEXMACS_TYPESET_CACHES=check, the lines used again in paper mode
  ;; are compared with every line typeset as a paragraph
  (check-group "the lines used again are the lines made again")
  (check= (typeset-check-mismatches) mismatches)
  (check-end))
