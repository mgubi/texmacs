
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-lines-test.scm
;; DESCRIPTION : tests of the lines of source code (code-lines)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A document typeset with the environment variable code-lines = true (a
;; source file, a text: verbatim.cpp) holds lines of source code, typeset by
;; one bridge as one box, without a paragraph per line
;; (Typeset/Bridge/bridge_document.cpp hands it to
;; Typeset/Bridge/bridge_code.cpp, Typeset/Boxes/Basic/code_boxes.cpp). It
;; must look and behave as the same lines in a document: the same places,
;; the same cursor movements, the same editing and the same text when
;; exported. Box based movements need the changes applied to the view
;; (apply-changes, see editing-test.scm).

(texmacs-module (check code-lines-test)
  (:use (check check-lib)))

(define lines '("int a;" "  b = 2;" "c" "" "return a + b;"))

(define (code-lines-env v)
  ;; TEXMACS_CODE_LINES (#f or "": as not set, the preference "code lines")
  (system-setenv "TEXMACS_CODE_LINES" (or v "")))

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
               (with "code-lines" "true" (document ,@lines)))))

(define (line-path code? i)
  ;; the path of the line @i in the body of the current buffer
  (if code? (list 0 4 2 i) (list 0 4 i)))

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
      (go-to (at 0 4 2 0 3))
      (step (lambda () (insert "XY")))
      (check= (tree->stree (path->tree (at 0 4 2 0))) "intXY a;")
      (step insert-return)
      (check= (tree->stree (path->tree (at 0 4 2 0))) "intXY")
      (check= (tree->stree (path->tree (at 0 4 2 1))) " a;")
      (check= (rel (cursor-path)) '(0 4 2 1 0))
      (check= (tree-arity (path->tree (at 0 4 2))) 6)
      (step kbd-backspace)
      (check= (tree->stree (path->tree (at 0 4 2 0))) "intXY a;")
      (check= (tree-arity (path->tree (at 0 4 2))) 5)
      (check= (rel (cursor-path)) '(0 4 2 0 5))
      ;; the cursor still goes through the box after the edits
      (step kbd-down)
      (check= (rel (cursor-path)) '(0 4 2 1 5)))))

;; lines which are not strings (markup put inside by hand) are typeset
;; without an error
(define (test-odd-contents)
  (check-group "odd contents")
  (with-body '(document (with "code-lines" "true" (document "a" (strong "b") "c")))
    (lambda ()
      (go-to (at 0 2 2 1))
      (step kbd-up)
      (check-true (list? (cursor-path))))))

;; In paper mode, the lines which did not change are used again from one
;; pass to the next (bridge_code.cpp), with the paths which they keep: after
;; lines are inserted, changed and joined, every line is where it is in a
;; document which had the same edits, and the cursor goes where it goes there
(define (all-rects code?)
  (let* ((n (tree-arity (path->tree (apply at (if code? '(0 4 2) '(0 4))))))
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
                 (go-to (at 0 4 2 i pos))
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
                           (with "code-lines" "true" (document ,@wrap-lines)))))
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
                           (with "code-lines" "true" (document ,@many))))))
    (with-body doc
      (lambda ()
        (init-env "page-medium" "paper")
        (step (lambda () #t))
        (go-to (at 0 4 2 150 3))
        (step (lambda () #t))
        (let ((n0 (typeset-code-rows-made)))
          (step (lambda () (insert "Q")))
          (check-true (<= (- (typeset-code-rows-made) n0) 4))
          (check= (tree->string (path->tree (at 0 4 2 150))) "  lQine 150;"))
        (let ((n0 (typeset-code-rows-made)))
          (step insert-return)
          (check-true (<= (- (typeset-code-rows-made) n0) 6)))
        (go-to (at 0 4 2 157 3))
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
                           (with "code-lines" "true" (document ,@many))))))
    (with-body doc
      (lambda ()
        (init-env "page-medium" "papyrus")
        (step (lambda () #t))
        (let ((made (lambda (thunk)
                      (let ((n0 (typeset-code-records-made)))
                        (step thunk)
                        (- (typeset-code-records-made) n0)))))
          (go-to (at 0 4 2 1 3))
          (step (lambda () #t))
          ;; a key in a short line, Return, a key in the new line, joining
          ;; it again, removing a wrapped line: no record made again but
          ;; for a wrapped line which changed
          (check= (made (lambda () (insert "Q"))) 0)
          (check= (made insert-return) 0)
          (check= (made (lambda () (insert "R"))) 0)
          (check= (made (lambda () (kbd-backspace) (kbd-backspace))) 0)
          (check= (tree->string (path->tree (at 0 4 2 1))) "  lQine 1;")
          (go-to (at 0 4 2 13 4))
          (step (lambda () #t))
          (check= (made (lambda () (insert "S"))) 1)
          (check= (made insert-return) 1)
          (check= (made kbd-backspace) 1)
          (check= (tree->string (path->tree (at 0 4 2 13)))
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
                           (with "code-lines" "true" (document ,@c-lines))))))
    (with-body doc
      (lambda ()
        (let ((line (lambda (i) (at 0 8 2 i))))
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
          (check= (typeset-check-mismatches) mismatches)))))
  ;; lines removed at the start of the block, and inserted in a comment, at
  ;; the level of the document (as the edits of a language server): the
  ;; states of the lines after them are made again (in Scala: a language
  ;; with states and no tree-sitter, whose colours, as those of C++ where
  ;; tree-sitter is built, would not come from the lexer of the block)
  (let* ((s-lines (append (list "/* one" "two" "three */")
                          (map (lambda (i)
                                 (string-append "int w" (number->string i)
                                                ";"))
                               (.. 0 20))))
         (doc `(document (with "font-family" "tt" "language" "verbatim"
                                "mode" "prog" "prog-language" "scala"
                           (with "code-lines" "true" (document ,@s-lines))))))
    (with-body doc
      (lambda ()
        (let ((d (path->tree (at 0 8 2))))
          (go-to (at 0 8 2 10 0))
          (step (lambda () #t))
          (step (lambda () (tree-remove! d 0 1)))
          (step (lambda () (tree-insert! d 0 '("/* again"))))
          (step (lambda () (tree-insert! d 1 '("in" "comment"))))
          (step (lambda () (tree-remove! d 0 2)))
          (check= (tree->string (tree-ref d 0)) "comment")
          (check= (typeset-check-mismatches) mismatches))))))

;; A line with markup is typeset by the concater and put on the grid of the
;; cells (bridge_code.cpp): a string in strong, em, a colour... takes a cell
;; for each character (a wider bold font too: each character in its cell),
;; math and other boxes the cells which they need. The cursor goes from a
;; plain line to the same column of a line with markup, and into its markup;
;; edits go into the markup; a higher line (a fraction) pushes the lines
;; after it, at the same places in paper mode and in papyrus mode
(define markup-lines
  `("int abcdef = 1;"
    (concat "int " (strong "abcdef") " = 2;")
    (concat "x= " (math (concat "a+" (frac "b" "c"))) "; // " (em "note"))
    "int after = 3;"
    "int more = 4;"))

(define (markup-run medium)
  (let* ((r '())
         (note (lambda (x) (set! r (cons x r))))
         (cur (lambda () (list-tail (rel (cursor-path)) 3)))
         (goto (lambda (l)
                 (go-to (apply at (append '(0 4 2) l)))
                 (step (lambda () #t)))))
    (init-env "page-medium" medium)
    (step (lambda () #t))
    ;; the places of the plain lines after the line with a fraction
    (let* ((y (lambda (i) (cadr (tree-bounding-rectangle
                                 (path->tree (at 0 4 2 i))))))
           (y0 (y 0)))
      (note (list (- (y 3) y0) (- (y 4) y0))))
    ;; down from the column 6 of a plain line: the column 6 of the next,
    ;; in strong (its third character)
    (goto '(0 6))
    (step kbd-down)
    (note (cur))
    ;; an edit in strong
    (step (lambda () (insert "Z")))
    (note (tree->stree (path->tree (at 0 4 2 1))))
    ;; into the math, and an edit there
    (goto '(2 1 0 0 1))
    (step (lambda () (insert "q")))
    (note (tree->stree (path->tree (at 0 4 2 2 1 0 0))))
    (note (cur))
    (reverse r)))

(define (test-markup)
  (check-group "lines with markup")
  (let* ((doc `(document (with "font-family" "tt" "language" "verbatim"
                           (with "code-lines" "true" (document ,@markup-lines)))))
         (paper #f) (papyrus #f))
    (with-body doc (lambda () (set! paper (markup-run "paper"))))
    (with-body doc (lambda () (set! papyrus (markup-run "papyrus"))))
    (check= (list-ref paper 1) '(1 1 0 2))
    (check= (list-ref paper 2) '(concat "int " (strong "abZcdef") " = 2;"))
    (check= (list-ref paper 3) "aq+")
    (check= (list-ref paper 4) '(2 1 0 0 2))
    ;; the line with the fraction is higher than a line, in both modes
    ;; (paper mode shoves it between the lines around it, as the stacker
    ;; does: the places are not the same)
    (for-each (lambda (places)
                (check-true (> (- (car places) (cadr places)) 0))
                (check-true (< (car places) (* 3 (- (cadr places)
                                                    (car places))))))
              (list (car paper) (car papyrus)))
    (check= (cdr papyrus) (cdr paper))))

;; With code-tall-objects "shrink", the fraction is scaled down to the
;; height of a row: the lines after it are where they are when the line is
;; plain text, and the cursor and the edits still go into it
(define (shrink-run medium)
  (let* ((r '())
         (note (lambda (x) (set! r (cons x r))))
         (cur (lambda () (list-tail (rel (cursor-path)) 3)))
         (y (lambda (i) (cadr (tree-bounding-rectangle
                               (path->tree (at 0 4 2 i)))))))
    (init-env "page-medium" medium)
    (init-env "code-tall-objects" "shrink")
    (step (lambda () #t))
    (note (list (- (y 3) (y 1)) (- (y 4) (y 1))))
    ;; (the plain document: the places only)
    (when (not (tree-atomic? (path->tree (at 0 4 2 2))))
      (go-to (at 0 4 2 2 1 0 1 0 0))
      (step (lambda () #t))
      (step kbd-right)
      (note (cur))
      (step (lambda () (insert "q")))
      (note (tree->stree (path->tree (at 0 4 2 2 1 0 1 0))))
      (go-to (at 0 4 2 1 0 2))
      (step (lambda () #t))
      (step kbd-down) (step kbd-down)
      (note (cur)))
    (reverse r)))

(define (test-shrink)
  (check-group "tall objects shrunk to a row")
  (let* ((doc `(document (with "font-family" "tt" "language" "verbatim"
                           (with "code-lines" "true" (document ,@markup-lines)))))
         (plain (map (lambda (l) (if (string? l) l "x= a+b; // note"))
                     markup-lines))
         (pdoc `(document (with "font-family" "tt" "language" "verbatim"
                            (with "code-lines" "true" (document ,@plain)))))
         (runs '()))
    (for-each (lambda (medium)
                (with-body doc
                  (lambda () (set! runs (cons (shrink-run medium) runs))))
                (with-body pdoc
                  (lambda ()
                    (with places (car (shrink-run medium))
                      (check= (car (car runs)) places)))))
              '("paper" "papyrus"))
    (for-each
      (lambda (run)
        (check= (cadr run) '(2 1 0 1 0 1))
        (check= (caddr run) "bq")
        (check= (cadddr run) '(3 2)))
      runs)))

(define (test-export)
  (check-group "export and import")
  (with-body (code-doc)
    (lambda ()
      (check= (texmacs->code (path->tree (at 0 4)) "utf-8")
              (string-join lines "\n"))))
  ;; the importer gives the lines, typeset as a block (the variable
  ;; code-lines in the initial environment), unless the preference "code
  ;; lines" is "off" or TEXMACS_CODE_LINES is 0 (1: always)
  (let ((old (getenv "TEXMACS_CODE_LINES"))
        (body (lambda ()
                (with t (tree->stree (cpp-verbatim->texmacs "a\nb" #f "utf-8"))
                  (list (cadr (assoc 'body (cdr t)))
                        (and (in? '(associate "code-lines" "true")
                                  (cdadr (assoc 'initial (cdr t))))
                             #t))))))
    (code-lines-env "1")
    (check= (body) '((document "a" "b") #t))
    (code-lines-env #f)
    (when (!= (get-preference "code lines") "off")
      (check= (body) '((document "a" "b") #t)))
    (code-lines-env "0")
    (check= (body) '((document "a" "b") #f))
    ;; (#f: as it was, not set)
    (code-lines-env old)
    (check= (or (getenv "TEXMACS_CODE_LINES") "") (or old ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabs and invisible characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A tab (<#9>, as the importer keeps it) goes to the next stop, every
;; prog-tab-width characters (8); another control character is a character
;; wide. The cursor goes up and down to the same column, across the tabs
(define tab-lines
  (list "<#9>abc" "        xyz" "ab<#9>c" "<#1>bcdefghij"))

(define (tab-run medium invis width)
  (let* ((r '())
         (cur (lambda () (list-tail (rel (cursor-path)) 3)))
         (note (lambda () (set! r (cons (cur) r))))
         (goto (lambda (i pos)
                 (go-to (at 0 4 2 i pos))
                 (step (lambda () #t)))))
    (init-env "page-medium" medium)
    (init-env "code-invisibles" invis)
    (when width (init-env "prog-tab-width" width))
    (step (lambda () #t))
    (goto 1 9) (step kbd-up) (note)      ; after the a of line 0
    (step kbd-down) (note)               ; back under it
    (goto 1 8) (step kbd-down) (note)    ; after the tab of line 2
    (step kbd-down) (note)               ; (<#1> is one character wide)
    (goto 0 0) (step kbd-right) (note)   ; over the tab, not into <#9>
    (reverse r)))

(define (test-tabs)
  (check-group "tabs and invisible characters")
  (let* ((doc `(document (with "font-family" "tt" "language" "verbatim"
                           (with "code-lines" "true" (document ,@tab-lines)))))
         (expected '((0 5) (1 9) (2 6) (3 11) (0 4)))
         (runs '()))
    (for-each
      (lambda (medium)
        (for-each
          (lambda (invis)
            (with-body doc
              (lambda ()
                (set! runs (cons (tab-run medium invis #f) runs)))))
          '("false" "true")))
      '("paper" "papyrus"))
    (for-each (lambda (run) (check= run expected)) runs)
    ;; tabs of 4: the a of line 0 is above the fifth space of line 1
    (with-body doc
      (lambda ()
        (init-env "prog-tab-width" "4")
        (step (lambda () #t))
        (go-to (at 0 4 2 1 5))
        (step (lambda () #t))
        (step kbd-up)
        (check= (list-tail (rel (cursor-path)) 3) '(0 5)))))
  ;; a line of tabs is broken into rows as wide as the page
  (let* ((tabs (apply string-append (make-list 40 "<#9>x")))
         (doc `(document (with "font-family" "tt" "language" "verbatim"
                           (with "code-lines" "true" (document "a" ,tabs "b"))))))
    (with-body doc
      (lambda ()
        (init-env "page-medium" "papyrus")
        (step (lambda () #t))
        (go-to (at 0 4 2 1 0))
        (step (lambda () #t))
        (step kbd-down)
        (check= (car (list-tail (rel (cursor-path)) 3)) 1))))
  ;; the importer keeps the tabs and the control characters, the exporter
  ;; gives them back
  (let ((old (getenv "TEXMACS_CODE_LINES"))
        (text (string-append "a" (string #\tab) "b\n"
                             (string (integer->char 1)) "c")))
    (code-lines-env "1")
    (with t (cpp-verbatim->texmacs text #f "utf-8")
      (with body (cadr (assoc 'body (cdr (tree->stree t))))
        (check= body '(document "a<#9>b" "<#1>c"))
        (check= (texmacs->code (stree->tree body) "utf-8") text)))
    (code-lines-env old)
    (check= (or (getenv "TEXMACS_CODE_LINES") "") (or old ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With the features of programs (src/docs/code-features.md)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A source file is loaded as its lines, typeset as a block (the variable
;; code-lines in its initial environment), and saved as it was loaded (its
;; tabs, its line ends)
(define (test-lines-root)
  (check-group "the lines of a source file")
  (let ((old (getenv "TEXMACS_CODE_LINES"))
        (text (string-append "int f ()\r\n{\r\n" (string #\tab)
                             "return 0;\r\n}\r\n")))
    (code-lines-env "1")
    (with t (tree->stree (cpp-source->texmacs text))
      (check= (cadr (assoc 'body (cdr t)))
              '(document "int f ()" "{" "<#9>return 0;" "}" ""))
      (check-true (in? '(associate "code-lines" "true")
                       (cdadr (assoc 'initial (cdr t))))))
    ;; (saved as source-files-test does: loaded in a buffer, exported)
    (let* ((dir (url-append (url-temp-dir) "code-lines-test"))
           (u (url-append dir "a.cpp"))
           (v (url-append dir "out-a.cpp")))
      (system-mkdir dir)
      (string-save text u)
      (when (buffer-exists? u) (buffer-close u))
      (buffer-load u)
      (buffer-export u v (url-format u))
      (buffer-close u)
      (check= (string-load v) text)
      (system-remove u)
      (system-remove v))
    (code-lines-env old)
    (check= (or (getenv "TEXMACS_CODE_LINES") "") (or old ""))))

;; A source file opens with the cursor in its first line, typed text goes
;; there, and the start and the end of the document are those of its lines
;; (they were before and after a tag of the block, when the block was one)
(define (test-cursor-in-block)
  (check-group "the cursor in a source file")
  (let* ((old (getenv "TEXMACS_CODE_LINES"))
         (prev (current-buffer))
         (dir (url-append (url-temp-dir) "code-lines-test"))
         (u (url-append dir "start.c")))
    (code-lines-env "1")
    (system-mkdir dir)
    (string-save "int a;\nint b;\n" u)
    (when (buffer-exists? u) (buffer-close u))
    (load-buffer u)
    (update-forced)
    (apply-changes)
    (with r (check-run
             (lambda ()
               (let ((ins (lambda (s) (insert s) (update-forced)
                                  (apply-changes))))
                 (ins "x")
                 (go-end) (ins "z")
                 (go-start) (ins "y")
                 (list (get-init "code-lines")
                       (tree->stree (buffer-get-body u))))))
      (check= r '("true" (document "yxint a;" "int b;" "z"))))
    (buffer-close u)
    (when (buffer-exists? prev) (switch-to-buffer prev))
    (system-remove u)
    (code-lines-env old)))

;; A block exports as the document of its lines: a paragraph per line, as
;; before the block (HTML and LaTeX dropped it, or made an unknown macro)
(define (test-export-formats)
  (check-group "export of a block to other formats")
  (let ((block (stree->tree '(document (with "code-lines" "true" (document "x = 1" "y <less> 2")))))
        (plain (stree->tree '(document "x = 1" "y <less> 2"))))
    (for-each
     (lambda (fm)
       (check= (convert block "texmacs-tree" fm)
               (convert plain "texmacs-tree" fm)))
     '("html-snippet" "latex-snippet" "verbatim-snippet" "markdown-snippet"))
    (check-true (string-contains? (convert block "texmacs-tree" "html-snippet")
                                  "x = 1")))
  ;; a text or a source file, whose lines are verbatim content (language
  ;; verbatim): the same LaTeX as without the block (it was an empty alltt)
  (let ((old (getenv "TEXMACS_CODE_LINES"))
        (dir (url-append (url-temp-dir) "code-lines-test")))
    (system-mkdir dir)
    (for (f '("t.txt" "b.c"))
      (for (fm '("latex" "html" "markdown"))
        (let* ((u (url-append dir f))
               (v (url-append dir (string-append f "." fm)))
               (out (lambda (cl)
                      (code-lines-env cl)
                      (when (buffer-exists? u) (buffer-close u))
                      (buffer-load u)
                      (buffer-export u v fm)
                      (buffer-close u)
                      (string-load v))))
          (string-save "int a;\nb = 2;\n" u)
          (with with-block (out "1")
            (check-true (string-contains? with-block "b = 2"))
            (check= with-block (out "0")))
          (system-remove u)
          (system-remove v))))
    (code-lines-env old)))

;; Texts and source files: their tabs in LaTeX (expanded to the stops, as
;; when a text was imported without the block), the menu of a text (Format
;; > Source code), the switch, which does not make a source file modified,
;; and a file of one line, also typeset as lines
(define (test-text-files)
  (check-group "texts and source files")
  (let ((old (getenv "TEXMACS_CODE_LINES"))
        (prev (current-buffer))
        (dir (url-append (url-temp-dir) "code-lines-test")))
    (system-mkdir dir)
    ;; the tabs in LaTeX
    (for (f '("tab.txt" "tab.c"))
      (let* ((u (url-append dir f))
             (v (url-append dir (string-append f ".tex")))
             (out (lambda (cl)
                    (code-lines-env cl)
                    (when (buffer-exists? u) (buffer-close u))
                    (buffer-load u)
                    (buffer-export u v "latex")
                    (buffer-close u)
                    (string-load v))))
        (string-save (string-append "a" (string #\tab) "b\n"
                                    (string #\tab) "c\n") u)
        (with block (out "1")
          (check-true (string-contains? block "a       b"))
          (check-true (string-contains? block "        c")))
        (system-remove u)
        (system-remove v)))
    ;; a text: the menu, the switch (not modified)
    (code-lines-env "1")
    (let ((u (url-append dir "menu.txt")))
      (string-save "a\nb\n" u)
      (when (buffer-exists? u) (buffer-close u))
      (load-buffer u)
      (update-forced)
      (with r (check-run
               (lambda ()
                 (let ((a (list (code-lines-text?) (code-lines?))))
                   (toggle-code-lines)
                   (update-forced)
                   (append a (list (code-lines?) (code-lines-text?)
                                   (buffer-modified? u))))))
        (check= r '(#t #t #f #t #f)))
      (buffer-close u)
      (system-remove u))
    ;; a file of one line
    (let ((u (url-append dir "one.py")))
      (string-save "x = 1" u)
      (with t (tree->stree (tree-import u "python"))
        (check-true (in? '(associate "code-lines" "true")
                         (cdadr (assoc 'initial (cdr t))))))
      (system-remove u))
    (when (buffer-exists? prev) (switch-to-buffer prev))
    (code-lines-env old)))

;; A block of one line with markup, in paper mode, is a paragraph of its
;; items (it had no rows, and was neither drawn nor reached by the cursor)
(define (test-one-rich-line)
  (check-group "a block of one line with markup")
  (let ((widths '()))
    (for-each
     (lambda (medium)
       (with-body `(document (with "font-family" "tt" "language" "verbatim"
                               (with "code-lines" "true"
                                (document (concat "a " (strong "bc"))))))
         (lambda ()
           (init-env "page-medium" medium)
           (step (lambda () #t))
           ;; (its text is typeset: as wide in paper mode as in papyrus)
           (with r (tree-bounding-rectangle (path->tree (at 0 4 2 0 1 0)))
             (set! widths (cons (- (caddr r) (car r)) widths)))
           (go-to (at 0 4 2 0 1 0 1))
           (step (lambda () (insert "Q")))
           (check= (tree->stree (path->tree (at 0 4 2 0)))
                   '(concat "a " (strong "bQc"))))))
     '("paper" "papyrus"))
    (check-true (> (car widths) 1000))
    (check= (cadr widths) (car widths))))

;; Folds (code-fold-lines): the hidden lines take no room, the cursor goes
;; over them; the lines are all alike, so that their places are the same
(define (fold-run medium)
  (let* ((r '())
         (note (lambda (x) (set! r (cons x r))))
         (cur (lambda () (list-tail (rel (cursor-path)) 3)))
         (root (path->tree (at 0 4 2)))
         (y (lambda (i) (cadr (tree-bounding-rectangle
                               (path->tree (at 0 4 2 i)))))))
    (init-env "page-medium" medium)
    (step (lambda () #t))
    (with y1 (y 1)
      (note (code-fold-lines root 0 2))
      (step (lambda () #t))
      ;; the line 3 is where the line 1 was
      (note (== (y 3) y1))
      ;; down from the line 0: over the fold (papyrus), or into it, which
      ;; opens it (code-cursor-in-fold, paper: as in a document of lines
      ;; without the block); never in a hidden line
      (go-to (at 0 4 2 0 1))
      (step (lambda () #t))
      (step kbd-down)
      (note (or (and (== (cur) '(3 1)) (code-hidden? root 1))
                (and (== (cur) '(1 1)) (not (code-hidden? root 1)))))
      (code-unfold-all)
      (step (lambda () #t))
      (note (< (y 3) y1)))
    (reverse r)))

(define (test-folds)
  (check-group "folds of lines")
  (let ((doc `(document (with "font-family" "tt" "language" "verbatim"
                          (with "code-lines" "true" (document "x" "x" "x" "x" "x"))))))
    (for (medium '("paper" "papyrus"))
      (with-body doc
        (lambda ()
          (check= (cons medium (fold-run medium))
                  (list medium #t #t #t #t)))))))

;; Layers of a line (code-set-line-layers: the colours and the inlay hints
;; of a language server): the line is typeset by the concater, the hints
;; between its characters
(define (test-layers)
  (check-group "layers of lines")
  (let ((doc `(document (with "font-family" "tt" "language" "verbatim"
                               "mode" "prog" "prog-language" "cpp"
                          (with "code-lines" "true" (document "abc" "abc" "abc"))))))
    (for (medium '("paper" "papyrus"))
      (with-body doc
        (lambda ()
          (init-env "page-medium" medium)
          (step (lambda () #t))
          ;; (the block is the argument 8 of the with)
          (let* ((line (path->tree (at 0 8 2 1)))
                 (x (lambda () (car (tree-bounding-rectangle line))))
                 (x0 (x)))
            ;; a hint in a token comes before the token (typeset_layered_
            ;; string): the text of the line is further
            (code-set-line-layers line
                                  (stree->tree '(tuple (tuple "0" "1" "red")))
                                  (stree->tree '(tuple (tuple "1" "hint:"))))
            (update-path (tree->path line))
            (step (lambda () #t))
            (check-true (> (x) x0))
            (go-to (at 0 8 2 1 1))
            (step (lambda () #t))
            (step kbd-right)
            (check= (list-tail (rel (cursor-path)) 3) '(1 2))
            (code-clear-line-layers line)
            (update-path (tree->path line))
            (step (lambda () #t))
            (check= (x) x0)))))))


;; Tree-sitter: the region of a block is the document of its lines
;; (treesit_language_rep::highlight, code_lines_root), which follows an edit
;; by an incremental parse; it was the body of the buffer, whose one line
;; was the block, flattened at each key (#797)
(define (test-tree-sitter)
  (check-group "tree-sitter in a block")
  (when (defined? 'tree-sitter-program-language)
    (with-body `(document (with "mode" "prog" "prog-language" "c"
                                "font-family" "tt" "code-lines" "true"
                            (document "/* a" "b */" "int x;")))
      (lambda ()
        (let* ((inner (path->tree (at 0 8)))
               (colors (tree-sitter-line-colors "c" (tree-ref inner 1)))
               (lan (tree-sitter-program-language "c")))
          ;; (the second line of the comment: the region has the lines)
          (check= colors '((0 4 "comment")))
          (tree-insert! (tree-ref inner 2) 0 " ")
          (step (lambda () #t))
          (with st (syntax-region-statistics inner lan)
            (check-true (and (pair? st) (pair? (cdr st))
                             (>= (cadr st) 1)))))))))

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
  (test-markup)
  (test-export)
  (test-tabs)
  (test-shrink)
  (test-lines-root)
  (test-cursor-in-block)
  (test-text-files)
  (test-export-formats)
  (test-one-rich-line)
  (test-folds)
  (test-layers)
  (test-tree-sitter)
  ;; with TEXMACS_TYPESET_CACHES=check, the lines used again in paper mode
  ;; are compared with every line typeset as a paragraph
  (check-group "the lines used again are the lines made again")
  (check= (typeset-check-mismatches) mismatches)
  (check-end))
