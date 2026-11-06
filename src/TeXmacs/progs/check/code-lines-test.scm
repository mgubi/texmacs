
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
  ;; a text is imported as without the block: its tabs are spaces (a text
  ;; is not a program; a tab <#9> would be an error in a line with markup
  ;; or in paragraphs), the source files keep theirs (test-lines-root)
  (let ((old (getenv "TEXMACS_CODE_LINES"))
        (text (string-append "a" (string #\tab) "b\nc")))
    (code-lines-env "1")
    (with t (cpp-verbatim->texmacs text #f "utf-8")
      (with body (cadr (assoc 'body (cdr (tree->stree t))))
        (check= body '(document "a       b" "c"))))
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
                                  "x = 1"))
    ;; the column of a tab in LaTeX goes on across the markup of its line
    (check-true (string-contains?
                 (convert (stree->tree '(verbatim (document
                                                   (concat "a" (with "color" "red" "bc")
                                                           "<#9>d"))))
                          "texmacs-tree" "latex-snippet")
                 "abc     d"))
    ;; a backslash or a brace, escaped before (\textbackslash , \{), is
    ;; one column; the column starts at 0 in each verbatim content
    (let ((tex (convert (stree->tree '(document
                                       (verbatim (document "a\\<#9>b" "{<#9>c"))
                                       (verbatim (document "xyz"))
                                       (specific "latex" "Y<#9>Z")))
                        "texmacs-tree" "latex-snippet")))
      (check-true (string-contains? tex "a\\textbackslash       b"))
      (check-true (string-contains? tex "\\{       c"))
      (check-true (string-contains? tex "Y       Z")))
    ;; in raw LaTeX, \{ is the two characters typed
    (check-true (string-contains?
                 (convert (stree->tree '(specific "latex" "\\{<#9>X"))
                          "texmacs-tree" "latex-snippet")
                 "\\{      X")))
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

;; Fixes of the audit of 14707e2593: the rectangles of lines with spaces
;; (as in a document, measured from a line without spaces), a block whose
;; lines are all removed by the tree (an edit of a language server: the
;; editor looped on a line which the box had and the tree had not), a block
;; in paper mode which goes below 4 lines (the records of paper mode were
;; kept: code-lines-check), and the switch of the lines of code, which is a
;; setting of the whole document, not offered in a code block of a document
(define (test-audit-5)
  (check-group "spaces, empty blocks, short blocks, code blocks")
  (let* ((ls '("c" "zz zz" "int " " " "x" "<#9>int x;" "<#9>" "<#9>a"
               " <#9> "))
         ;; (in prog mode, where a tab is an arrow in a paragraph too)
         (doc (lambda (cl)
                `(document (with "font-family" "tt" "language" "verbatim"
                             "mode" "prog" "prog-language" "c"
                             "code-lines" ,cl (document ,@ls)))))
         (rel (lambda ()
                (let ((r0 (tree-bounding-rectangle (path->tree (at 0 10 0)))))
                  (map (lambda (i)
                         (with r (tree-bounding-rectangle
                                  (path->tree (at 0 10 i)))
                           (list (- (cadr r) (cadr r0))
                                 (- (cadddr r) (cadr r0)))))
                       '(1 2 4 5 6 7 8)))))
         (plain #f) (code #f))
    (with-body (doc "false") (lambda () (set! plain (rel))))
    (with-body (doc "true") (lambda () (set! code (rel))))
    (check= code plain))
  ;; all the lines removed (as an edit of the editor, which corrects the
  ;; cursor), the arrows, then one line inserted
  (let ((edit (lambda (thunk)
                (start-editing) (thunk) (end-editing)
                (update-forced) (apply-changes))))
    (with-body '(document "before"
                          (with "font-family" "tt" "code-lines" "true"
                            (document "a" "b" "c" "d" "e"))
                          "after")
      (lambda ()
        (init-env "page-medium" "papyrus")
        (edit (lambda () #t))
        (go-to (at 1 4 2 1))
        (let ((d (path->tree (at 1 4))))
          (edit (lambda () (tree-remove! d 0 5)))
          (check= (tree-arity d) 0)
          (edit kbd-down)
          (edit kbd-up)
          (check-true (list? (cursor-path)))
          (edit (lambda () (tree-insert! d 0 '("again"))))
          (check= (tree->stree d) '(document "again"))))))
  ;; paper mode, from 4 lines to 3 (code-lines-check compares the records)
  (with-body '(document (with "font-family" "tt" "code-lines" "true"
                          (document "a" "b" "c" "d")))
    (lambda ()
      (init-env "page-medium" "paper")
      (step (lambda () #t))
      (go-to (at 0 4 3 0))
      (step (lambda () #t))
      (step kbd-backspace)
      (check= (tree->stree (path->tree (at 0 4))) '(document "a" "b" "cd"))
      (check= (typeset-check-mismatches) mismatches)))
  ;; a code block of a document: not offered there
  (with-body '(document "Some text."
                        (cpp-code (document "int x;" "return 0;")))
    (lambda ()
      (go-to (at 1 0 0 2))
      (step (lambda () #t))
      (check-true (in-prog?))
      (check-false (code-lines-document?)))))

;; Fixes of the audit of 2f1ad9b0d1: a document inside a line of a block is
;; typeset as paragraphs (code-lines is the mode of the block's document
;; only: its long lines wrap), and a selection over several lines is as
;; high as in paragraphs (the bounding rectangle of the block)
(define (test-audit-7)
  (check-group "documents in lines, selections of several lines")
  (let* ((long (string-append "A long sentence of a paragraph inside a line,"
                              " which wraps at the width of the page as any"
                              " paragraph does, and is not a line of code"
                              " running off the page."))
         (doc (lambda (cl)
                `(document "before"
                           (with "font-family" "tt" "language" "verbatim"
                             "code-lines" ,cl
                             (document "a" "b"
                                       (with "color" "blue"
                                         (document ,long "second"))
                                       "c" "d"))
                           "after")))
         (measure (lambda ()
                    (let ((p (tree-bounding-rectangle
                              (path->tree (at 1 6 2 2 0))))
                          (b (tree-bounding-rectangle
                              (path->tree (at 1 6 0)))))
                      (list (- (caddr p) (car p)) (- (cadddr p) (cadr p))))))
         (plain #f) (code #f))
    (for (medium '("papyrus" "paper"))
      (with-body (doc "false")
        (lambda () (init-env "page-medium" medium) (step (lambda () #t))
                (set! plain (measure))))
      (with-body (doc "true")
        (lambda () (init-env "page-medium" medium) (step (lambda () #t))
                (set! code (measure))))
      (check= code plain)))
  ;; the bounding rectangle of a block (its lines selected) is that of its
  ;; lines as paragraphs, at the top and at the bottom
  (let* ((doc (lambda (cl)
                `(document "before"
                           (with "font-family" "tt" "language" "verbatim"
                             "mode" "prog" "prog-language" "c"
                             "code-lines" ,cl
                             (document "<#9>int x;" "a<#9>b" "<#9>" "end"))
                           "after")))
         (ys (lambda ()
               (with r (tree-bounding-rectangle (path->tree (at 1 10)))
                 (list (cadr r) (cadddr r)))))
         (plain #f) (code #f))
    (with-body (doc "false")
      (lambda () (init-env "page-medium" "papyrus") (step (lambda () #t))
              (set! plain (ys))))
    (with-body (doc "true")
      (lambda () (init-env "page-medium" "papyrus") (step (lambda () #t))
              (set! code (ys))))
    (check= code plain)))

;; The environment computed by exec_until (without the fast environments)
;; inside a document nested in a block which is typeset as paragraphs: the
;; assignments before the place are kept (the nested document is typeset
;; as paragraphs, and exec_until goes through it as such; it took code-lines
;; from the environment of exec_until and dropped its paragraphs)
(define (test-audit-9)
  (check-group "environments in blocks, without the fast environments")
  (define fast? (== (get-preference "fast environments") "on"))
  (set-fast-environments #f)
  (with-body '(document (with "code-lines" "true"
                          (document "a"
                                    (with "color" "red"
                                      (document "x" (assign "zz" "7") "y"))
                                    "b")))
    (lambda ()
      (go-to (at 0 2 1 2 2 1))
      (step (lambda () #t))
      (check= (get-env "zz") "7")
      (step (lambda () (insert "Q")))
      (check= (get-env "zz") "7")))
  ;; inside the markup of a line of a block (the line is walked as the
  ;; concater typesets it: bridge_code_rep::my_exec_until)
  (for (medium '("papyrus" "paper"))
    (with-body '(document (with "code-lines" "true"
                            (document "a"
                                      (concat "x " (with "color" "red" "abc")
                                              " " (math "a+b"))
                                      "b")))
      (lambda ()
        (init-env "page-medium" medium)
        (step (lambda () #t))
        (go-to (at 0 2 1 1 2 1))
        (step (lambda () #t))
        (check= (get-env "color") "red")
        (go-to (at 0 2 1 3 0 1))
        (step (lambda () #t))
        (check= (get-env "mode") "math"))))
  (set-fast-environments fast?))

;; The lines with layers (the colours and the hints of a language server,
;; on almost all the lines of a file) are drawn by the box, as the colours
;; of the syntax: no line with layers is typeset by the concater, at a key
;; or when lines are inserted before them (code_boxes.cpp)
(define (test-layered-lines)
  (check-group "lines with layers drawn by the box")
  (for (medium '("papyrus" "paper"))
    (let* ((ls (map (lambda (i) (string-append "int x" (number->string i)
                                               " = f (" (number->string i)
                                               ");"))
                    (.. 0 200)))
           (doc `(document (with "font-family" "tt" "language" "verbatim"
                             "code-lines" "true" (document ,@ls))))
           (layers (stree->tree '(tuple (tuple "0" "3" "type")
                                        (tuple "4" "6" "variable"))))
           (rect (lambda (i) (tree-bounding-rectangle
                              (path->tree (at 0 6 i))))))
      (with-body doc
        (lambda ()
          (init-env "page-medium" medium)
          (step (lambda () #t))
          (with n0 (typeset-code-rich-made)
            (for (t (tree-children (path->tree (at 0 6))))
              (code-set-line-layers t layers
                                    (stree->tree '(tuple (tuple "4" "v:")))))
            (update-current-buffer)
            (step (lambda () #t))
            ;; (no line typeset by the concater, also the first time)
            (check= (- (typeset-code-rich-made) n0) 0))
          (go-to (at 0 6 100 3))
          (step (lambda () #t))
          (let ((n0 (typeset-code-rich-made)))
            (step (lambda () (insert "a")))
            (step (lambda () (insert "b")))
            (check= (- (typeset-code-rich-made) n0) 0))
          ;; a line inserted before: the lines after it at their places
          (with r (rect 150)
            (go-to (at 0 6 10 0))
            (step insert-return)
            (check= (rect 151) (begin (update-current-buffer)
                                      (step (lambda () #t))
                                      (rect 151)))
            (check-true (< (cadr (rect 151)) (cadr r)))))))))

;; The inlay hints drawn by the box (code_line_hints, code_xpos): the
;; position of a hint is before it, its character after it; a hint at the
;; start of a line or of a row takes its room at the start, one inside a
;; token between its characters (the audit of f09a27ca48: a hint at the
;; start of a row was drawn in the margin, without room)
(define (test-hints)
  (check-group "inlay hints in a block")
  (for (medium '("papyrus" "paper"))
    (let* ((long (apply string-append
                        (map (lambda (i) (string-append "word" (number->string i)
                                                        " "))
                             (.. 0 40))))
           (doc `(document (with "font-family" "tt" "mode" "prog"
                             "prog-language" "cpp" "code-lines" "true"
                             (document "abcdefgh" "abcdefgh" ,long))))
           (x (lambda (l i)
                (go-to (at 0 8 l i)) (step (lambda () #t))
                (get-cursor-x)))
           (y (lambda (l i)
                (go-to (at 0 8 l i)) (step (lambda () #t))
                (get-cursor-y))))
      (with-body doc
        (lambda ()
          (init-env "page-medium" medium)
          (step (lambda () #t))
          (let ((blk (path->tree (at 0 8))))
            (code-set-line-layers (tree-ref blk 0) (stree->tree '(tuple))
                                  (stree->tree '(tuple (tuple "0" "S")
                                                       (tuple "3" "HH"))))
            (update-path (tree->path (tree-ref blk 0)))
            ;; a hint before each word of the long line (the rows start
            ;; at words: each row starts with a hint)
            (code-set-line-layers
             (tree-ref blk 2) (stree->tree '(tuple))
             (stree->tree
              `(tuple ,@(let loop ((i 0) (pos 0) (acc '()))
                          (if (>= i 40) (reverse acc)
                              (loop (+ i 1)
                                    (+ pos (string-length
                                            (string-append
                                             "word" (number->string i) " ")))
                                    (cons `(tuple ,(number->string pos) "p:")
                                          acc)))))))
            (update-path (tree->path (tree-ref blk 2)))
            (step (lambda () #t))
            (let* ((cell (- (x 1 1) (x 1 0))))
              (check= (x 0 0) (x 1 0))
              (check= (- (x 0 1) (x 0 0)) (* 2 cell))
              (check= (- (x 0 3) (x 0 2)) cell)
              (check= (- (x 0 4) (x 0 3)) (* 3 cell))
              ;; the first position of the second row of the long line: at
              ;; the start of the row, and its hint (if any) after it
              (let* ((y0 (y 2 0))
                     (b (let loop ((i 1))
                          (if (or (>= i (string-length long))
                                  (!= (y 2 i) y0)) i (loop (+ i 1))))))
                (check-true (< b (string-length long)))
                (check= (x 2 b) (x 2 0))
                (check= (- (x 2 (+ b 1)) (x 2 b)) (* 3 cell))))))))
    ;; a click on the left half of a character after a hint: before it
    ;; (pos_at measures the character after its hints)
    (with-body `(document (with "font-family" "tt" "mode" "prog"
                            "prog-language" "cpp" "code-lines" "true"
                            (document "f(10, 20)" "abcdefgh")))
      (lambda ()
        (init-env "page-medium" medium)
        (step (lambda () #t))
        (let ((blk (path->tree (at 0 8))))
          (code-set-line-layers (tree-ref blk 0) (stree->tree '(tuple))
                                (stree->tree '(tuple (tuple "2" "a: "))))
          (update-path (tree->path (tree-ref blk 0)))
          (step (lambda () #t))
          (go-to (at 0 8 1 1)) (step (lambda () #t))
          (let* ((cell (- (get-cursor-x)
                          (begin (go-to (at 0 8 1 0)) (step (lambda () #t))
                                 (get-cursor-x))))
                 (x2 (begin (go-to (at 0 8 0 2)) (step (lambda () #t))
                            (get-cursor-x)))
                 (y2 (get-cursor-y)))
            ;; (the "1" starts 3 cells after the position 2)
            (mouse-any "press-left" (+ x2 (* 3 cell) (quotient cell 4))
                       (+ y2 100) 1 1000.0 '())
            (mouse-any "release-left" (+ x2 (* 3 cell) (quotient cell 4))
                       (+ y2 100) 0 1100.0 '())
            (step (lambda () #t))
            (check= (rel (cursor-path)) '(0 8 0 2)))))))
  ;; the cursor after a block in a with: at the end of its last line, as
  ;; after the same lines as paragraphs (the markers of the with are put
  ;; there, not after the box of the block, as wide as the page)
  (for (medium '("papyrus" "paper"))
    (let ((after (lambda (cl)
                   (let ((r #f))
                     (with-body `(document (with "font-family" "tt"
                                             "code-lines" ,cl
                                             (document "short" "tail")))
                       (lambda ()
                         (init-env "page-medium" medium)
                         (step (lambda () #t))
                         (go-to (at 0 4 1 4))
                         (step (lambda () #t))
                         (step kbd-right)
                         (set! r (list (rel (cursor-path))
                                       (get-cursor-x) (get-cursor-y)))))
                     r))))
      (if (== medium "paper")
          (check= (after "true") (after "false"))
          ;; (papyrus: the block is one box, as wide as the page; the
          ;; cursor is on the baseline of the last line, after the box)
          (let ((t (after "true")) (f (after "false")))
            (check= (car t) (car f))
            (check= (caddr t) (caddr f))))))
  ;; a block is not aligned by the paragraph mode: its lines start at the
  ;; left margin (with lines of different widths, which the rows of paper
  ;; mode, made from those of line 1, would not follow)
  (for (medium '("papyrus" "paper"))
    (for (pm '("center" "right"))
      (let ((xs '()))
        (with-body `(document (with "font-family" "tt" "par-mode" ,pm
                                "code-lines" "true"
                                (document "a" ,(make-string 38 #\b) "cc"
                                          ,(make-string 12 #\d) "e")))
          (lambda ()
            (init-env "page-medium" medium)
            (step (lambda () #t))
            (set! xs (map (lambda (i)
                            (go-to (at 0 6 i 0))
                            (step (lambda () #t))
                            (get-cursor-x))
                          (.. 0 5)))))
        (check= (length (list-remove-duplicates xs)) 1)))))

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
                 (x (lambda () (caddr (tree-bounding-rectangle line))))
                 (x0 (x)))
            ;; a hint takes room before its character (the box draws it,
            ;; code_line_hints): the end of the line is further
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

;; The places before and after a block nested in markup: Up, Down and
;; clicks around it, and the cursor after it when its last line is wrapped
;; markup or folded, are those of the same lines as paragraphs
(define block-long-markup
  `(concat "x " (strong ,(apply string-append (make-list 60 "word ")))))

(define (block-ends-run cl medium lines fold)
  (let ((r '()))
    (with-body `(document "before text"
                          (with "font-family" "tt" "code-lines" ,cl
                            (document ,@lines))
                          "next paragraph")
      (lambda ()
        (init-env "page-medium" medium)
        (step (lambda () #t))
        (when fold
          (code-fold-lines (path->tree (at 1 4)) 0 fold)
          (step (lambda () #t)))
        (let* ((cxy (lambda (p)
                      (go-to (apply at p)) (step (lambda () #t))
                      (list (get-cursor-x) (get-cursor-y))))
               (mv (lambda (from keys)
                     (go-to (apply at from)) (step (lambda () #t))
                     (map (lambda (k) (step k) (rel (cursor-path))) keys)))
               (click (lambda (x y)
                        (mouse-any "press-left" x y 1 1000.0 '())
                        (mouse-any "release-left" x y 0 1100.0 '())
                        (step (lambda () #t))
                        (rel (cursor-path)))))
          (set! r (list (cxy '(1 1))
                        (mv '(1 1) (list kbd-up kbd-up kbd-down kbd-down))
                        (mv '(2 2) (list kbd-up kbd-up))))
          (when (not fold)
            (let ((p1 (cxy '(1 4 1 0))) (p0 (cxy '(1 4 0 0))))
              (set! r (append r
                        (list (click (- (car p1) 2000) (+ (cadr p1) 100))
                              (click (- (car p0) 2000)
                                     (+ (cadr p0) 100))))))))))
    r))

(define (block-end-and-line cl medium lines fold end)
  ;; the cursor after the block, and at a place of its last line
  (let ((r #f))
    (with-body `(document "before text"
                          (with "font-family" "tt" "code-lines" ,cl
                            (document ,@lines))
                          "next paragraph")
      (lambda ()
        (init-env "page-medium" medium)
        (step (lambda () #t))
        (when fold
          (code-fold-lines (path->tree (at 1 4)) 0 fold)
          (step (lambda () #t)))
        (let ((cxy (lambda (p)
                     (go-to (apply at p)) (step (lambda () #t))
                     (list (get-cursor-x) (get-cursor-y)))))
          (set! r (list (cxy '(1 1)) (cxy end))))))
    r))

(define (block-selection-heights cl medium from to)
  ;; the bottom and the top of the rectangles of a selection
  (let ((r #f))
    (with-body `(document "before text"
                          (with "font-family" "tt" "code-lines" ,cl
                            (document "aaaa" "bbbbbb" "cc" "dddd"))
                          "next paragraph")
      (lambda ()
        (init-env "page-medium" medium)
        (step (lambda () #t))
        (selection-set (apply at from) (apply at to))
        (step (lambda () #t))
        (let loop ((l (selection-rectangles)) (lo #f) (hi #f))
          (if (null? l) (set! r (list lo hi))
              (let ((y1 (cadr l)) (y2 (cadddr l)))
                (loop (list-tail l 4)
                      (if lo (min lo y1) y1) (if hi (max hi y2) y2)))))
        (selection-cancel)
        (step (lambda () #t))))
    r))

(define (block-qed-and-line medium)
  ;; the y of the end of a QED after a block, and of its last line
  (let ((r #f))
    (with-body `(document "first"
                          (surround "" (concat (htab "5mm") "QED")
                            (with "font-family" "tt" "code-lines" "true"
                              (document "aaa" "bb" "c" "dd")))
                          "next")
      (lambda ()
        (init-env "page-medium" medium)
        (step (lambda () #t))
        (let ((y (lambda (p)
                   (go-to (apply at p)) (step (lambda () #t))
                   (get-cursor-y))))
          (set! r (list (y '(1 1 1 3)) (y '(1 2 4 3 2)))))))
    r))

(define (block-moves cl medium)
  (let ((r '()))
    (with-body `(document "prev"
                          (with "font-family" "tt" "code-lines" ,cl
                            (document "short" "c"
                                      (concat "x " (strong "short"))))
                          "next")
      (lambda ()
        (init-env "page-medium" medium)
        (step (lambda () #t))
        (let ((mv (lambda (from keys)
                    (go-to (apply at from)) (step (lambda () #t))
                    (map (lambda (k) (step k) (rel (cursor-path))) keys))))
          (set! r (list (mv '(1 4 2 1 1) (list kbd-up kbd-up))
                        (mv '(1 4 2 1 1) (list kbd-down))
                        (mv '(1 4 2 1 1) (list kbd-right kbd-right)))))))
    (with-body `(document "first"
                          (surround "Ab" ""
                            (with "font-family" "tt" "code-lines" ,cl
                              (document "aaa" "bb")))
                          "next")
      (lambda ()
        (init-env "page-medium" medium)
        (step (lambda () #t))
        (let ((mv (lambda (from keys)
                    (go-to (apply at from)) (step (lambda () #t))
                    (map (lambda (k) (step k) (rel (cursor-path))) keys))))
          (set! r (append r
                    (list (mv '(1 0 1) (list kbd-right kbd-right kbd-right))
                          (mv '(1 2 4 0 1)
                              (list kbd-left kbd-left kbd-left))))))))
    r))

(define (test-block-ends)
  (check-group "the places before and after a block")
  ;; paper mode: Up, Down, clicks left of the lines, the cursor after the
  ;; block as with paragraphs; papyrus mode: the cursor after the block on
  ;; the baseline of its last line (but after the box, at the right margin:
  ;; see the known differences in docs/code-lines.md)
  (let ((lines '("short" "longer line here" "cc")))
    (check= (block-ends-run "true" "paper" lines #f)
            (block-ends-run "false" "paper" lines #f))
    (check= (cadr (car (block-ends-run "true" "papyrus" lines #f)))
            (cadr (car (block-ends-run "false" "papyrus" lines #f)))))
  ;; a last line with markup, wrapped (at other places than as a
  ;; paragraph: a grid of cells): after the block is at its end (papyrus:
  ;; on its baseline)
  (for (medium '("papyrus" "paper"))
    (let ((r (block-end-and-line "true" medium (list "a" "b" block-long-markup)
                                 #f '(1 4 2 1 1))))
      (if (== medium "paper")
          (check= (car r) (cadr r))
          (check= (cadr (car r)) (cadr (cadr r))))))
  ;; a selection across an end of the block, from or to its line 2: its
  ;; top or bottom as with paragraphs (a row of a block is selected a little
  ;; lower in paper mode; the whole block is several lines more)
  (for (medium '("papyrus" "paper"))
    (for (ends '(((1 4 2 1) (2 3)) ((1 4 2 1) (1 1)) ((0 2) (1 4 2 1))))
      (let ((c (block-selection-heights "true" medium (car ends) (cadr ends)))
            (p (block-selection-heights "false" medium (car ends)
                                        (cadr ends))))
        (if (== (car ends) '(1 4 2 1))
            (check= (cadr c) (cadr p))
            (check-true (< (abs (- (car c) (car p))) 10000))))))
;; Right and Left through a block wrapped in two tags: the places of
  ;; paragraphs, none skipped, no cycle
  (for (medium '("papyrus" "paper"))
    (let ((walk (lambda (cl key from)
                  (let ((r '()))
                    (with-body `(document "prev"
                                  (with "color" "red"
                                    (with "font-family" "tt" "code-lines" ,cl
                                      (document "ab" "c" "de")))
                                  "next")
                      (lambda ()
                        (init-env "page-medium" medium)
                        (step (lambda () #t))
                        (go-to (apply at from)) (step (lambda () #t))
                        (set! r (map (lambda (i) (step key) (rel (cursor-path)))
                                     (.. 0 16)))))
                    r))))
      (check= (walk "true" kbd-right '(0 2)) (walk "false" kbd-right '(0 2)))
      (check= (walk "true" kbd-left '(2 3)) (walk "false" kbd-left '(2 3)))))
;; paper mode: the first row after "Proof.", the cursor after a block whose
  ;; last line is empty, as with paragraphs
  (let ((places
         (lambda (cl doc ps)
           (let ((r #f))
             (with-body (doc cl)
               (lambda ()
                 (init-env "page-medium" "paper")
                 (step (lambda () #t))
                 (set! r (map (lambda (p)
                                (go-to (apply at p)) (step (lambda () #t))
                                (list (get-cursor-x) (get-cursor-y)))
                              ps))))
             r))))
    (for (c (list (list (lambda (cl)
                          `(document "prev"
                             (proof (document
                                      (with "font-family" "tt" "code-lines" ,cl
                                        (document "ab" "c" "de"))))
                             "next"))
                        '((1 0) (1 0 0 4 0 0) (1 0 0 1)))
                  (list (lambda (cl)
                          `(document "prev"
                             (with "font-family" "tt" "code-lines" ,cl
                               (document "" "c" ""))
                             "next"))
                        '((1 4 0 0) (1 4 2 0) (1 1)))))
      (check= (places "true" (car c) (cadr c))
              (places "false" (car c) (cadr c)))))
  ;; Right and Left through a wrapped line with markup (its rows found by
  ;; y), papyrus, as with paragraphs
  (let ((walk
         (lambda (cl key from)
           (let ((r '()))
             (with-body `(document (with "font-family" "tt" "code-lines" ,cl
                                     (document "ab"
                                       (concat ,(apply string-append
                                                       (make-list 30 "word "))
                                               (em "e")))))
               (lambda ()
                 (init-env "page-medium" "papyrus")
                 (step (lambda () #t))
                 (go-to (apply at from)) (step (lambda () #t))
                 (set! r (map (lambda (i) (step key) (rel (cursor-path)))
                              (.. 0 160)))))
             r))))
    (check= (walk "true" kbd-right '(0 4 1 0 0))
            (walk "false" kbd-right '(0 4 1 0 0)))
    (check= (walk "true" kbd-left '(0 4 1 1 1))
            (walk "false" kbd-left '(0 4 1 1 1))))
  ;; Up and Down from the end of a last line with markup, Right and Left
  ;; across the start of a block after a text, as with paragraphs
  (for (medium '("papyrus" "paper"))
    (check= (block-moves "true" medium)
            (block-moves "false" medium)))
  ;; a selection to a hidden line (the last lines folded): drawn on the
  ;; line shown, not below it
  (with-body `(document "prev" (with "font-family" "tt" "code-lines" "true"
                                 (document "a" "b" "c" "d"))
                        "next")
    (lambda ()
      (init-env "page-medium" "papyrus")
      (step (lambda () #t))
      (code-fold-lines (path->tree (at 1 4)) 0 3)
      (step (lambda () #t))
      (selection-set (at 1 4 0 0) (at 1 4 0 1))
      (step (lambda () #t))
      (let ((one (selection-rectangles)))
        (selection-set (at 1 4 0 0) (at 1 4 3 1))
        (step (lambda () #t))
        (check= (selection-rectangles) one))
      (selection-cancel)
      (step (lambda () #t))))
  ;; what comes after the block (a QED): on the baseline of its last line
  (let ((r (block-qed-and-line "papyrus")))
    (check= (car r) (cadr r)))
  ;; an empty last line is reached by Left and by a click (papyrus; a
  ;; click right of the left end of the block, which is the place before it)
  (with-body `(document "xx" (with "font-family" "tt" "code-lines" "true"
                               (document "aa" ""))
                        "yy")
    (lambda ()
      (init-env "page-medium" "papyrus")
      (step (lambda () #t))
      (go-to (at 2 0)) (step (lambda () #t))
      (step kbd-left) (step kbd-left)
      (check= (rel (cursor-path)) '(1 4 1 0))
      (go-to (at 1 4 1 0)) (step (lambda () #t))
      (let ((x (get-cursor-x)) (y (get-cursor-y)))
        (go-to (at 0 0)) (step (lambda () #t))
        (mouse-any "press-left" (+ x 2000) (+ y 100) 1 1000.0 '())
        (mouse-any "release-left" (+ x 2000) (+ y 100) 0 1100.0 '())
        (step (lambda () #t))
        (check= (rel (cursor-path)) '(1 4 1 0)))))
  ;; the last lines folded (papyrus; in paper mode, as with paragraphs, the
  ;; hidden lines are clipped to a point): after the block is on the
  ;; baseline of the line shown
  (let ((r (block-end-and-line "true" "papyrus" '("a" "b" "c" "d") 3
                               '(1 4 0 1))))
    (check= (cadr (car r)) (cadr (cadr r)))))

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
  (test-audit-5)
  (test-audit-7)
  (test-audit-9)
  (test-layered-lines)
  (test-hints)
  (test-export-formats)
  (test-one-rich-line)
  (test-block-ends)
  (test-folds)
  (test-layers)
  (test-tree-sitter)
  ;; with TEXMACS_TYPESET_CACHES=check, the lines used again in paper mode
  ;; are compared with every line typeset as a paragraph
  (check-group "the lines used again are the lines made again")
  (check= (typeset-check-mismatches) mismatches)
  (check-end))
