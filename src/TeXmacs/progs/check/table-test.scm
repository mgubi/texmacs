;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table-test.scm
;; DESCRIPTION : tests of table editing, without a window
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite edits tables the way the Table menu, the focus bar and the
;; keyboard do (table/table-edit.scm, table/table-menu.scm and the C++
;; editor of Edit/Modify/edit_table.cpp): it inserts tables, moves between
;; cells, inserts and removes rows and columns, sets cell and table formats,
;; joins cells, copies and pastes parts of tables, and reads the resulting
;; trees back exactly.
;;
;; A table is a macro (tabular, block, matrix...) around a tformat, whose
;; last child is the table and whose other children are the formats: twith
;; for the whole table, cwith for a range of cells, with the rows and
;; columns counted from 1, or from the end when negative (-1 is the last
;; one). The path of a cell is thus the path of the tformat, then the index
;; of the table in it (the number of formats), the row and the column.
;;
;; As in editing-test.scm, each step is wrapped the way the event loop
;; wraps a key press (edit-step), and the buffers are never shown in a
;; window, so that:
;;
;;   - the movements through boxes (go-left...) are not used; the table
;;     movements (table-go-to, structured-left...) go through the tree;
;;   - keep-table-selection restores a table selection in a delayed
;;     command, which never runs: after setting the format of a selection,
;;     the selection is gone;
;;   - the environment at a cursor path is cached by the editor, and the
;;     cache is cleared when the changes are applied to a view, which needs
;;     a window: the cache at the first position of a new buffer is filled
;;     before the style is known, so that the macros of the style (block,
;;     wide-tabular...) are not seen there and make inserts them as plain
;;     tags. The tables are therefore inserted in a second paragraph.

(texmacs-module (check table-test)
  (:use (check check-lib)
        (table table-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-step thunk)
  ;; one user action, as the event loop wraps a key press or a menu action
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define-macro (edit . body)
  `(edit-step (lambda () ,@body)))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (at . l)
  ;; the absolute path of @l in the current buffer
  (append (buffer-path) l))

(define (rel p)
  ;; the path @p relative to the current buffer
  (and (list? p) (>= (length p) (length (buffer-path)))
       (list-tail p (length (buffer-path)))))

(define (cursor) (rel (cursor-path)))

(define (with-table-body doc p thunk)
  ;; run @thunk in a new buffer holding @doc, with the cursor at @p
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    ;; the body set above is not an editing step of its own: without this
    ;; one, the first undo would also undo it
    (edit (noop))
    (clear-undo-history)
    (go-to (apply at p))
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (selection-cancel)
      (set-cell-mode "cell")
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (rows . l)
  ;; the table with the rows @l, each one a list of cell contents
  `(table ,@(map (lambda (r) `(row ,@(map (lambda (c) `(cell ,c)) r))) l)))

(define (tab fm . l)
  ;; a tabular with the formats @fm and the rows @l
  `(tabular (tformat ,@fm ,(apply rows l))))

(define abc '(("a" "b" "c") ("d" "e" "f") ("g" "h" "i")))

(define (tab3 . fm)
  ;; the 3x3 tabular with the cells a to i and the formats @fm
  (apply tab fm abc))

(define (cwith r1 r2 c1 c2 var val)
  `(cwith ,r1 ,r2 ,c1 ,c2 ,var ,val))

(define (twith var val) `(twith ,var ,val))

(define (rect . l)
  ;; the rectangle (x1 y1 x2 y2) of the subtree at @l of the current buffer
  (tree-bounding-rectangle (path->tree (apply at l))))

(define (near? x y tol) (<= (abs (- x y)) tol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make with a table macro inserts the macro around a 1x1 table, with the
;; cursor in its only cell (edit_dynamic.cpp, make_compound, make_table);
;; the formats of the macro are read back by table-get-format-all; a macro
;; of a wide table, whose cells are wrapped, gets a document around the
;; table and in the cell (table-block yes, cell-hyphen t).
(define (test-insert-text)
  (define (insert-table tag)
    (let ((r #f))
      (with-table-body '(document "x" "") '(1 0)
        (lambda ()
          (edit (make tag))
          (set! r (list (body) (cursor) (table-get-extents)
                        (tree->stree (table-get-format-all))))))
      r))
  (define (borders)
    (list (cwith "1" "-1" "1" "-1" "cell-rborder" "1ln")
          (cwith "1" "-1" "1" "-1" "cell-bborder" "1ln")
          (cwith "1" "1" "1" "-1" "cell-tborder" "1ln")
          (cwith "1" "-1" "1" "1" "cell-lborder" "1ln")))
  (define (wide)
    (list (twith "table-width" "1par")
          (twith "table-hmode" "exact")
          (twith "table-block" "yes")
          (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
          (cwith "1" "-1" "1" "-1" "cell-hpart" "0.001")))
  (check-group "insert in text")
  (with r (insert-table 'tabular)
    (check= (car r) '(document "x" (tabular (tformat (table (row (cell "")))))))
    (check= (cadr r) '(1 0 0 0 0 0 0))
    (check= (caddr r) '(1 1))
    (check= (cadddr r) '(tformat)))
  (with r (insert-table 'tabular*)
    (check= (car r) '(document "x" (tabular* (tformat (table (row (cell "")))))))
    (check= (cadr r) '(1 0 0 0 0 0 0))
    (check= (cadddr r)
            `(tformat ,(cwith "1" "-1" "1" "-1" "cell-halign" "c"))))
  (with r (insert-table 'block)
    (check= (car r) '(document "x" (block (tformat (table (row (cell "")))))))
    (check= (cadr r) '(1 0 0 0 0 0 0))
    (check= (caddr r) '(1 1))
    (check= (cadddr r) `(tformat ,@(borders))))
  (with r (insert-table 'block*)
    (check= (car r) '(document "x" (block* (tformat (table (row (cell "")))))))
    (check= (cadddr r)
            `(tformat ,@(borders)
                      ,(cwith "1" "-1" "1" "-1" "cell-halign" "c"))))
  (with r (insert-table 'wide-tabular)
    (check= (car r) '(document "x" (wide-tabular
                                    (document
                                     (tformat
                                      (table (row (cell (document "")))))))))
    (check= (cadr r) '(1 0 0 0 0 0 0 0 0))
    (check= (caddr r) '(1 1))
    (check= (cadddr r)
            `(tformat ,@(wide)
                      ,(cwith "1" "-1" "1" "1" "cell-lsep" "0fn")
                      ,(cwith "1" "-1" "-1" "-1" "cell-rsep" "0fn"))))
  (with r (insert-table 'wide-block)
    (check= (car r) '(document "x" (wide-block
                                    (document
                                     (tformat
                                      (table (row (cell (document "")))))))))
    (check= (cadddr r) `(tformat ,@(wide) ,@(borders))))
  ;; a small table of the Insert menu: a tabular in the first argument
  (with-table-body '(document "x" "") '(1 0)
    (lambda ()
      (check-true (style-has? "env-float-dtd"))
      (edit (insert-go-to '(small-table "" "") '(0 0))
            (make 'tabular))
      (check= (body) '(document "x" (small-table
                                     (tabular (tformat (table (row (cell "")))))
                                     "")))
      (check= (cursor) '(1 0 0 0 0 0 0 0))))
  ;; a table in a cell of a table: the cursor goes to the inner one
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("c" "d")))
                   '(1 0 0 0 0 0 1)
    (lambda ()
      (edit (make 'tabular))
      (check= (body)
              `(document "x" ,(tab '() `((concat "a" ,(tab '() '(""))) "b")
                                   '("c" "d"))))
      (check= (cursor) '(1 0 0 0 0 0 1 0 0 0 0 0 0))
      (check= (table-get-extents) '(1 1))
      (edit (insert "n") (table-insert-row #t))
      (check= (body)
              `(document "x" ,(tab '() `((concat "a" ,(tab '() '("n") '(""))) "b")
                                   '("c" "d"))))
      ;; the outer table has not changed
      (edit (go-to (at 1 0 0 1 1 0 0)))
      (check= (table-get-extents) '(2 2)))))

;; In math, the Insert menu offers matrices, determinants, choices and
;; stacks, whose cells are math; an eqnarray* is a wide table with three
;; columns.
(define (test-insert-math)
  (check-group "insert in math")
  (for-each
   (lambda (tag)
     (with-table-body '(document "x" (math "")) '(1 0 0)
       (lambda ()
         (edit (make tag))
         (check= (body) `(document "x" (math (,tag (tformat
                                                    (table (row (cell ""))))))))
         (check= (cursor) '(1 0 0 0 0 0 0 0))
         (check= (table-get-extents) '(1 1))
         (check-true (inside? tag)))))
   '(matrix det choice stack))
  (with-table-body '(document "x" (math "")) '(1 0 0)
    (lambda ()
      (edit (make 'matrix))
      (check= (tree->stree (table-get-format-all))
              `(tformat ,(cwith "1" "-1" "1" "-1" "cell-halign" "c")
                        ,(cwith "1" "-1" "1" "-1" "cell-swell" "0.9ex")))
      (check= (get-env "mode") "math")
      (edit (insert "a"))
      (edit (table-insert-column #t))
      (edit (make-fraction))
      (edit (insert "1"))
      (check= (body) '(document "x" (math (matrix (tformat
                                                   (table (row (cell "a")
                                                               (cell (frac "1" "")))))))))
      (check= (cursor) '(1 0 0 0 0 1 0 0 1))
      (edit (table-insert-row #t))
      (check= (cursor) '(1 0 0 0 1 1 0 0))
      (edit (make 'sqrt) (insert "x"))
      (check= (body) '(document "x" (math (matrix (tformat
                                                   (table (row (cell "a")
                                                               (cell (frac "1" "")))
                                                          (row (cell "")
                                                               (cell (sqrt "x")))))))))
      (check= (get-env "mode") "math")))
  (with-table-body '(document "x" "") '(1 0)
    (lambda ()
      (edit (make 'eqnarray*))
      (check= (body) '(document "x" (eqnarray* (document
                                                (tformat
                                                 (table (row (cell "") (cell "")
                                                             (cell ""))))))))
      (check= (cursor) '(1 0 0 0 0 0 0 0))
      (check= (table-get-extents) '(1 3))
      (check-true (table-inside? 'eqnarray*))
      (check-false (table-inside? 'matrix))
      (check= (get-env "mode") "math"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The table around the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The extents of the table, the cell of the cursor counted from 1, the
;; paths of the cells (counted from the end when negative), and what the
;; table commands answer outside a table.
(define (test-queries)
  (check-group "queries")
  (with-table-body `(document "x" ,(tab (list (cwith "1" "1" "1" "1"
                                                     "cell-halign" "r"))
                                        '("a" "b" "c") '("d" "e" "f")))
                   '(1 0 1 1 2 0 1)
    (lambda ()
      (check= (table-nr-rows) 2)
      (check= (table-nr-columns) 3)
      (check= (table-get-extents) '(2 3))
      (check= (table-which-row) 2)
      (check= (table-which-column) 3)
      (check= (table-which-cells) '(2 2 3 3))
      (check= (rel (table-cell-path 1 1)) '(1 0 1 0 0 0))
      (check= (rel (table-cell-path 2 3)) '(1 0 1 1 2 0))
      (check= (rel (table-cell-path -1 -1)) '(1 0 1 1 2 0))
      (check= (rel (table-cell-path -2 1)) '(1 0 1 0 0 0))
      (check= (table-cell-path 3 1) '())
      (check= (table-cell-path 1 4) '())
      (check= (tree->stree (path->tree (table-cell-path 1 2))) "b")
      (check-true (inside? 'table))
      (check-true (inside? 'tabular))
      (check-true (inside? 'cell))
      (check= (tree-label (tree-innermost 'tabular)) 'tabular)
      (check-true (table-markup-context? (tree-innermost 'tabular)))
      (check-true (table-markup-context? (tree-innermost 'tformat)))
      (check-false (table-markup-context? (tree-innermost 'cell)))
      (check= (tree->stree (table-get-format-all))
              `(tformat ,(cwith "1" "1" "1" "1" "cell-halign" "r")))
      (check= (get-cell-mode) "cell")
      ;; outside the table
      (edit (go-to (at 0 1)))
      (check-false (inside? 'table))
      (check= (table-nr-rows) -1)
      (check= (table-nr-columns) -1)
      (check= (table-get-extents) '())
      (check= (table-which-row) 0)
      (check= (table-which-column) 0)
      (check= (table-which-cells) '())
      (check= (table-cell-path 1 1) '())
      (check= (table-get-format "table-halign") "")
      (check= (cell-get-format "cell-halign") "")
      ;; the commands do nothing
      (edit (table-insert-row #t) (table-insert-column #t)
            (cell-set-format "cell-halign" "c"))
      (check= (body) `(document "x" ,(tab (list (cwith "1" "1" "1" "1"
                                                       "cell-halign" "r"))
                                          '("a" "b" "c") '("d" "e" "f")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving between cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; table-go-to goes to the end of a cell, counted from 1 or from the end;
;; a cell outside the table is ignored. The structured movements of the
;; focus bar and the keyboard go to the neighbouring cell and stop at the
;; borders; the extremal ones go to the first or last cell of the row or
;; the column.
(define (test-moving)
  (check-group "moving")
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (edit (table-go-to 1 1))
      (check= (cursor) '(0 0 0 0 0 0 1))
      (edit (table-go-to 3 2))
      (check= (cursor) '(0 0 0 2 1 0 1))
      (check= (table-which-row) 3)
      (check= (table-which-column) 2)
      (edit (table-go-to -1 -1))
      (check= (cursor) '(0 0 0 2 2 0 1))
      (edit (table-go-to -3 1))
      (check= (cursor) '(0 0 0 0 0 0 1))
      ;; outside the table: no move
      (edit (table-go-to 4 1))
      (check= (cursor) '(0 0 0 0 0 0 1))
      (edit (table-go-to 1 0))
      (check= (cursor) '(0 0 0 0 0 0 1))
      (edit (table-go-to -4 1))
      (check= (cursor) '(0 0 0 0 0 0 1))
      ;; structured movements from the center
      (edit (table-go-to 2 2))
      (edit (structured-right))
      (check= (cursor) '(0 0 0 1 2 0 1))
      (edit (structured-right))
      (check= (cursor) '(0 0 0 1 2 0 1))
      (edit (structured-left))
      (check= (cursor) '(0 0 0 1 1 0 1))
      (edit (structured-left) (structured-left))
      (check= (cursor) '(0 0 0 1 0 0 1))
      (edit (structured-down))
      (check= (cursor) '(0 0 0 2 0 0 1))
      (edit (structured-down))
      (check= (cursor) '(0 0 0 2 0 0 1))
      (edit (structured-up) (structured-up))
      (check= (cursor) '(0 0 0 0 0 0 1))
      (edit (structured-up))
      (check= (cursor) '(0 0 0 0 0 0 1))
      ;; extremal movements
      (edit (table-go-to 2 2))
      (edit (structured-start))
      (check= (cursor) '(0 0 0 1 0 0 0))
      (edit (structured-end))
      (check= (cursor) '(0 0 0 1 2 0 1))
      (edit (structured-top))
      (check= (cursor) '(0 0 0 0 2 0 0))
      (edit (structured-bottom))
      (check= (cursor) '(0 0 0 2 2 0 1))
      ;; traversal up and down
      (edit (traverse-up))
      (check= (cursor) '(0 0 0 1 2 0 1))
      (edit (traverse-down))
      (check= (cursor) '(0 0 0 2 2 0 1))))
  ;; backspace at the start of a cell goes to the end of the previous one,
  ;; delete at the end of a cell to the start of the next one, and out of
  ;; the table after the last one; the cells keep their contents
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("c" "d")))
                   '(1 0 0 1 0 0 0)
    (lambda ()
      (edit (kbd-backspace))
      (check= (cursor) '(1 0 0 0 1 0 1))
      (edit (kbd-delete))
      (check= (cursor) '(1 0 0 1 0 0 0))
      (edit (go-to (at 1 0 0 1 1 0 1)))
      (edit (kbd-delete))
      (check= (cursor) '(1 1))
      (check= (body) `(document "x" ,(tab '() '("a" "b") '("c" "d")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting rows and columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A new row or column is empty, goes before or after the cursor, and the
;; cursor goes to its cell in the same column or row; return in a table
;; inserts a row below and goes to its first cell.
(define (test-insert-rows-columns)
  (check-group "insert rows and columns")
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (edit (table-insert-row #f))
      (check= (body) `(document ,(tab '() '("a" "b" "c") '("" "" "")
                                      '("d" "e" "f") '("g" "h" "i"))))
      (check= (cursor) '(0 0 0 1 1 0 0))
      (check= (table-get-extents) '(4 3))
      (edit (table-go-to 3 2))
      (edit (table-insert-row #t))
      (check= (body) `(document ,(tab '() '("a" "b" "c") '("" "" "")
                                      '("d" "e" "f") '("" "" "")
                                      '("g" "h" "i"))))
      (check= (cursor) '(0 0 0 3 1 0 0))
      (edit (table-go-to 1 1))
      (edit (table-insert-column #f))
      (check= (body) `(document ,(tab '() '("" "a" "b" "c") '("" "" "" "")
                                      '("" "d" "e" "f") '("" "" "" "")
                                      '("" "g" "h" "i"))))
      (check= (cursor) '(0 0 0 0 0 0 0))
      (edit (table-go-to -1 -1))
      (edit (table-insert-column #t))
      (check= (table-get-extents) '(5 5))
      (check= (cursor) '(0 0 0 4 4 0 0))
      (check= (tree->stree (path->tree (at 0 0 0 0)))
              '(row (cell "") (cell "a") (cell "b") (cell "c") (cell "")))))
  ;; at the borders: a row above the first one, below the last one
  (with-table-body `(document ,(tab '() '("a" "b") '("c" "d"))) '(0 0 0 0 0 0 1)
    (lambda ()
      (edit (table-insert-row #f))
      (check= (body) `(document ,(tab '() '("" "") '("a" "b") '("c" "d"))))
      (check= (cursor) '(0 0 0 0 0 0 0))
      (edit (table-go-to -1 -1) (table-insert-row #t))
      (check= (body) `(document ,(tab '() '("" "") '("a" "b") '("c" "d")
                                      '("" ""))))
      (check= (cursor) '(0 0 0 3 1 0 0))))
  ;; the structured insertions of the focus bar, and return
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 1)
    (lambda ()
      (edit (structured-insert-right))
      (check= (body) `(document ,(tab '() '("a" "b" "" "c") '("d" "e" "" "f")
                                      '("g" "h" "" "i"))))
      (check= (cursor) '(0 0 0 1 2 0 0))
      (edit (structured-insert-left))
      (check= (body) `(document ,(tab '() '("a" "b" "" "" "c")
                                      '("d" "e" "" "" "f")
                                      '("g" "h" "" "" "i"))))
      (check= (cursor) '(0 0 0 1 2 0 0))
      (edit (structured-insert-down))
      (check= (table-get-extents) '(4 5))
      (check= (cursor) '(0 0 0 2 2 0 0))
      (edit (structured-insert-up))
      (check= (table-get-extents) '(5 5))
      (check= (cursor) '(0 0 0 2 2 0 0))
      (check= (tree->stree (path->tree (at 0 0 0 4)))
              '(row (cell "g") (cell "h") (cell "") (cell "") (cell "i")))))
  (with-table-body `(document ,(tab '() '("a" "b") '("c" "d"))) '(0 0 0 0 1 0 1)
    (lambda ()
      (edit (kbd-return))
      (check= (body) `(document ,(tab '() '("a" "b") '("" "") '("c" "d"))))
      (check= (cursor) '(0 0 0 1 0 0 0))
      (edit (insert "z"))
      (check= (body) `(document ,(tab '() '("a" "b") '("z" "") '("c" "d"))))))
  ;; the size of the table, set at once
  (with-table-body `(document ,(tab '() '("a" "b") '("c" "d"))) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (table-set-extents 3 4))
      (check= (body) `(document ,(tab '() '("a" "b" "" "") '("c" "d" "" "")
                                      '("" "" "" ""))))
      (check= (table-get-extents) '(3 4))
      (edit (table-set-extents 1 1))
      (check= (body) `(document ,(tab '() '("a"))))
      (edit (table-set-rows "2"))
      (check= (body) `(document ,(tab '() '("a") '(""))))
      (edit (table-set-columns "3"))
      (check= (body) `(document ,(tab '() '("a" "" "") '("" "" ""))))
      ;; at least one row and column
      (edit (table-set-extents 0 0))
      (check= (body) `(document ,(tab '() '("a"))))))
  ;; blank rows and columns, of the Table menu, have no borders nor padding
  ;; and a given size
  (with-table-body `(document ,(tab '() '("a" "b") '("c" "d"))) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (table-insert-blank-row "1em"))
      (check= (body)
              `(document
                ,(tab (map (lambda (x) (cwith "2" "2" "1" "-1" (car x) (cadr x)))
                           '(("cell-background" "") ("cell-lborder" "0ln")
                             ("cell-rborder" "0ln") ("cell-lsep" "0ln")
                             ("cell-rsep" "0ln") ("cell-tsep" "0ln")
                             ("cell-bsep" "0ln") ("cell-vcorrect" "n")
                             ("cell-vmode" "exact") ("cell-height" "1em")))
                      '("a" "b") '("" "") '("c" "d"))))
      (check= (cursor) '(0 0 10 1 0 0 0))
      ;; the cell mode is restored
      (check= (get-cell-mode) "cell")))
  (with-table-body `(document ,(tab '() '("a" "b") '("c" "d"))) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (table-insert-blank-column "1em"))
      (check= (body)
              `(document
                ,(tab (map (lambda (x) (cwith "1" "-1" "2" "2" (car x) (cadr x)))
                           '(("cell-background" "") ("cell-tborder" "0ln")
                             ("cell-bborder" "0ln") ("cell-lsep" "0ln")
                             ("cell-rsep" "0ln") ("cell-tsep" "0ln")
                             ("cell-bsep" "0ln") ("cell-vcorrect" "n")
                             ("cell-hmode" "exact") ("cell-width" "1em")))
                      '("a" "" "b") '("c" "" "d"))))
      (check= (cursor) '(0 0 10 0 1 0 0))
      (check= (get-cell-mode) "cell"))))

;; The formats of cells move with the cells when rows and columns are
;; inserted; a format of a whole row or column (-1 for the last one) grows
;; with it.
(define (test-insert-formats)
  (check-group "insert and formats")
  (with-table-body `(document ,(tab3 (cwith "2" "2" "2" "2"
                                            "cell-background" "red")))
                   '(0 0 1 1 1 0 0)
    (lambda ()
      (edit (table-insert-row #f))
      (check= (body)
              `(document ,(tab (list (cwith "3" "3" "2" "2"
                                            "cell-background" "red"))
                               '("a" "b" "c") '("" "" "") '("d" "e" "f")
                               '("g" "h" "i"))))
      (edit (table-insert-column #f))
      (check= (body)
              `(document ,(tab (list (cwith "3" "3" "3" "3"
                                            "cell-background" "red"))
                               '("a" "" "b" "c") '("" "" "" "")
                               '("d" "" "e" "f") '("g" "" "h" "i"))))
      ;; below and to the right: no change
      (edit (table-go-to -1 -1) (table-insert-row #t) (table-insert-column #t))
      (check= (tree->stree (table-get-format-all))
              `(tformat ,(cwith "3" "3" "3" "3" "cell-background" "red")))
      (check= (table-get-extents) '(5 5))))
  (with-table-body `(document ,(tab3 (cwith "1" "1" "1" "-1" "cell-halign" "r")
                                     (cwith "1" "-1" "3" "3" "cell-valign" "t")))
                   '(0 0 2 1 1 0 0)
    (lambda ()
      (edit (table-insert-column #t))
      (check= (tree->stree (table-get-format-all))
              `(tformat ,(cwith "1" "1" "1" "-1" "cell-halign" "r")
                        ,(cwith "1" "-1" "4" "4" "cell-valign" "t")))
      (edit (table-insert-row #f))
      (check= (tree->stree (table-get-format-all))
              `(tformat ,(cwith "1" "1" "1" "-1" "cell-halign" "r")
                        ,(cwith "1" "-1" "4" "4" "cell-valign" "t")))
      (check= (body)
              `(document ,(tab (list (cwith "1" "1" "1" "-1" "cell-halign" "r")
                                     (cwith "1" "-1" "4" "4" "cell-valign" "t"))
                               '("a" "b" "" "c") '("" "" "" "")
                               '("d" "e" "" "f") '("g" "h" "" "i")))))))

;; The limits of the size of a table (table-min-rows...) are kept by the
;; insertions and table-set-extents.
(define (test-limits)
  (check-group "limits")
  (with-table-body `(document ,(tab (list (twith "table-max-rows" "2"))
                                    '("a" "b") '("c" "d")))
                   '(0 0 1 1 0 0 0)
    (lambda ()
      (check= (table-get-format "table-max-rows") "2")
      (edit (table-insert-row #t))
      (check= (table-get-extents) '(2 2))
      (edit (table-insert-column #t))
      (check= (table-get-extents) '(2 3))))
  (with-table-body `(document ,(tab (list (twith "table-max-cols" "2"))
                                    '("a" "b") '("c" "d") '("e" "f")))
                   '(0 0 1 0 0 0 0)
    (lambda ()
      (edit (table-insert-column #t))
      (check= (table-get-extents) '(3 2))
      (check= (body) `(document ,(tab (list (twith "table-max-cols" "2"))
                                      '("a" "b") '("c" "d") '("e" "f"))))))
  ;; FIXME: the maximal number of columns is compared with the minimal
  ;; number of rows (src/Edit/Modify/edit_table.cpp:447, table_get_limits,
  ;; "if (j2<i1)" instead of "if (j2<j1)"): with table-min-rows 3 and
  ;; table-max-cols 2, table-insert-column in a 3x2 table gives 3 columns,
  ;; expected 2.
  (with-table-body `(document ,(tab (list (twith "table-min-rows" "2")
                                          (twith "table-max-cols" "3"))
                                    '("a" "b") '("c" "d")))
                   '(0 0 2 0 0 0 0)
    (lambda ()
      (edit (table-set-extents 1 5))
      (check= (table-get-extents) '(2 3))
      (edit (table-insert-column #t))
      (check= (table-get-extents) '(2 3))
      (check= (body) `(document ,(tab (list (twith "table-min-rows" "2")
                                            (twith "table-max-cols" "3"))
                                      '("a" "b" "") '("c" "d" "")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing rows and columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Removing backwards removes the row above or the column on the left of
;; the cursor, which stays in its cell; removing forwards removes the row
;; or column of the cursor and goes to the next one (table-menu.scm and the
;; documentation in table-doc.scm). Outside the table at the borders.
(define (test-remove-rows-columns)
  (check-group "remove rows and columns")
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (edit (table-remove-row #f))
      (check= (body) `(document ,(tab '() '("d" "e" "f") '("g" "h" "i"))))
      (check= (cursor) '(0 0 0 0 1 0 0))
      (edit (table-remove-column #f))
      (check= (body) `(document ,(tab '() '("e" "f") '("h" "i"))))
      (check= (cursor) '(0 0 0 0 0 0 0))
      (edit (table-remove-row #t))
      (check= (body) `(document ,(tab '() '("h" "i"))))
      (check= (cursor) '(0 0 0 0 0 0 0))
      (edit (table-remove-column #t))
      (check= (body) `(document ,(tab '() '("i"))))
      (check= (cursor) '(0 0 0 0 0 0 0))))
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 1)
    (lambda ()
      (edit (structured-remove-left))
      (check= (body) `(document ,(tab '() '("b" "c") '("e" "f") '("h" "i"))))
      (check= (cursor) '(0 0 0 1 0 0 1))
      (edit (structured-remove-right))
      (check= (body) `(document ,(tab '() '("c") '("f") '("i"))))
      (check= (cursor) '(0 0 0 1 0 0 0))
      (edit (structured-remove-up))
      (check= (body) `(document ,(tab '() '("f") '("i"))))
      (check= (cursor) '(0 0 0 0 0 0 0))
      (edit (structured-remove-down))
      (check= (body) `(document ,(tab '() '("i"))))
      (check= (cursor) '(0 0 0 0 0 0 0))))
  ;; at the borders: removing backwards at the top or the left leaves the
  ;; table at its start, removing forwards the last row or column goes
  ;; after the table
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("c" "d")) "y")
                   '(1 0 0 0 0 0 1)
    (lambda ()
      (edit (table-remove-row #f))
      (check= (body) `(document "x" ,(tab '() '("a" "b") '("c" "d")) "y"))
      (check= (cursor) '(1 0))))
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("c" "d")) "y")
                   '(1 0 0 1 0 0 0)
    (lambda ()
      (edit (table-remove-column #f))
      (check= (body) `(document "x" ,(tab '() '("a" "b") '("c" "d")) "y"))
      (check= (cursor) '(1 0))))
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("c" "d")) "y")
                   '(1 0 0 1 0 0 0)
    (lambda ()
      (edit (table-remove-row #t))
      (check= (body) `(document "x" ,(tab '() '("a" "b")) "y"))
      (check= (cursor) '(1 1))))
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("c" "d")) "y")
                   '(1 0 0 0 1 0 0)
    (lambda ()
      (edit (table-remove-column #t))
      (check= (body) `(document "x" ,(tab '() '("a") '("c")) "y"))
      (check= (cursor) '(1 1)))))

;; The formats of the removed cells go, the others move with their cells.
(define (test-remove-formats)
  (check-group "remove and formats")
  (with-table-body `(document ,(tab3 (cwith "2" "2" "2" "2" "cell-background" "red")
                                     (cwith "3" "3" "3" "3" "cell-halign" "r")
                                     (cwith "1" "-1" "1" "1" "cell-valign" "t")))
                   '(0 0 3 0 0 0 0)
    (lambda ()
      (edit (table-remove-row #t))
      (check= (tree->stree (table-get-format-all))
              `(tformat ,(cwith "1" "1" "2" "2" "cell-background" "red")
                        ,(cwith "2" "2" "3" "3" "cell-halign" "r")
                        ,(cwith "1" "-1" "1" "1" "cell-valign" "t")))
      (edit (table-remove-column #t))
      (check= (body)
              `(document ,(tab (list (cwith "1" "1" "1" "1" "cell-background" "red")
                                     (cwith "2" "2" "2" "2" "cell-halign" "r"))
                               '("e" "f") '("h" "i"))))
      (edit (table-remove-column #t))
      (check= (body)
              `(document ,(tab (list (cwith "2" "2" "1" "1" "cell-halign" "r"))
                               '("f") '("i")))))))

;; Removing the only row or column removes the table; so does removing a
;; row of a table which has its minimal number of rows.
(define (test-delete-table)
  (check-group "delete table")
  (with-table-body `(document "x" ,(tab '() '("a" "b")) "y") '(1 0 0 0 1 0 0)
    (lambda ()
      (edit (table-remove-row #t))
      (check= (body) '(document "x" "" "y"))
      (check= (cursor) '(1 0))
      (check-false (inside? 'table))))
  (with-table-body `(document "x" ,(tab '() '("a") '("b")) "y") '(1 0 0 1 0 0 0)
    (lambda ()
      (edit (table-remove-column #f))
      (check= (body) '(document "x" "" "y"))))
  (with-table-body `(document "x" ,(tab (list (twith "table-min-rows" "2"))
                                        '("a" "b") '("c" "d")))
                   '(1 0 1 0 0 0 0)
    (lambda ()
      (edit (table-remove-row #t))
      (check= (body) '(document "x" ""))))
  ;; a block goes with its table
  (with-table-body `(document "x" (block (tformat (table (row (cell "a"))))))
                   '(1 0 0 0 0 0 0)
    (lambda ()
      (edit (table-remove-column #t))
      (check= (body) '(document "x" ""))))
  ;; backspace in an empty table removes the empty rows, then the empty
  ;; columns, then the table
  (with-table-body `(document "x" ,(tab '() '("" "") '("" ""))) '(1 0 0 0 0 0 0)
    (lambda ()
      (edit (kbd-backspace))
      (check= (body) `(document "x" ,(tab '() '("" ""))))
      (check= (cursor) '(1 0 0 0 1 0 0))
      (edit (kbd-backspace))
      (check= (body) `(document "x" ,(tab '() '(""))))
      (check= (cursor) '(1 0 0 0 0 0 0))
      (edit (kbd-backspace))
      (check= (body) '(document "x" ""))
      (check= (cursor) '(1 0))))
  (with-table-body `(document "x" ,(tab '() '("a" "") '("" ""))) '(1 0 0 1 1 0 0)
    (lambda ()
      (edit (kbd-backspace))
      (check= (body) `(document "x" ,(tab '() '("a" ""))))
      (check= (cursor) '(1 0 0 0 1 0 0))
      (edit (kbd-backspace))
      (check= (body) `(document "x" ,(tab '() '("a"))))
      (check= (cursor) '(1 0 0 0 0 0 1))
      (edit (kbd-backspace))
      (check= (body) `(document "x" ,(tab '() '(""))))
      (edit (kbd-delete))
      (check= (body) '(document "x" "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cell formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The formats of a cell, as the environment gives them, and as cwith set
;; them for the cell, its row, its column or the whole table, depending on
;; the cell mode of the Cell menu.
(define (test-cell-formats)
  (check-group "cell formats")
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      ;; the defaults (Typeset/Env/env_default.cpp)
      (check= (cell-get-format "cell-halign") "l")
      (check= (cell-get-format "cell-valign") "B")
      (check= (cell-get-format "cell-hmode") "auto")
      (check= (cell-get-format "cell-width") "")
      (check= (cell-get-format "cell-lborder") "0ln")
      (check= (cell-get-format "cell-lsep") "1spc")
      (check= (cell-get-format "cell-tsep") "1sep")
      (check= (cell-get-format "cell-hyphen") "n")
      (check= (cell-get-format "cell-block") "auto")
      (check= (cell-get-format "cell-vcorrect") "a")
      (check= (cell-get-format "cell-background") "")
      (check= (cell-get-format "cell-row-span") "1")
      (check= (cell-get-format "cell-col-span") "1")
      (check-false (cell-spans-more?))
      ;; a cell
      (edit (cell-set-format "cell-background" "red"))
      (check= (body) `(document ,(tab3 (cwith "2" "2" "2" "2"
                                              "cell-background" "red"))))
      (check= (cell-get-format "cell-background") "red")
      (edit (table-go-to 1 1))
      (check= (cell-get-format "cell-background") "")
      ;; setting it again replaces the format
      (edit (table-go-to 2 2) (cell-set-format "cell-background" "blue"))
      (check= (body) `(document ,(tab3 (cwith "2" "2" "2" "2"
                                              "cell-background" "blue"))))
      ;; a row, a column, the whole table
      (edit (set-cell-mode "row") (cell-set-format "cell-halign" "r"))
      (check= (get-cell-mode) "row")
      (check= (cell-get-format "cell-halign") "r")
      (edit (set-cell-mode "column") (cell-set-format "cell-valign" "t"))
      (edit (set-cell-mode "table") (cell-set-format "cell-lsep" "1em"))
      (check= (cell-get-format "cell-lsep") "1em")
      (edit (set-cell-mode "cell"))
      (check= (body)
              `(document ,(tab3 (cwith "2" "2" "2" "2" "cell-background" "blue")
                                (cwith "2" "2" "1" "-1" "cell-halign" "r")
                                (cwith "1" "-1" "2" "2" "cell-valign" "t")
                                (cwith "1" "-1" "1" "-1" "cell-lsep" "1em"))))
      (edit (table-go-to 3 3))
      (check= (cell-get-format "cell-lsep") "1em")
      (check= (cell-get-format "cell-halign") "l")
      (check= (cell-get-format "cell-valign") "B")
      (edit (table-go-to 2 3))
      (check= (cell-get-format "cell-halign") "r")
      (edit (table-go-to 1 2))
      (check= (cell-get-format "cell-valign") "t")
      ;; removing the formats of a cell does not remove those of the row,
      ;; column or table around it
      (edit (table-go-to 2 2) (cell-del-format "cell-background"))
      (edit (cell-del-format ""))
      (check= (body)
              `(document ,(tab3 (cwith "2" "2" "1" "-1" "cell-halign" "r")
                                (cwith "1" "-1" "2" "2" "cell-valign" "t")
                                (cwith "1" "-1" "1" "-1" "cell-lsep" "1em"))))
      (edit (set-cell-mode "row") (cell-del-format ""))
      (check= (body)
              `(document ,(tab3 (cwith "1" "-1" "2" "2" "cell-valign" "t")
                                (cwith "1" "-1" "1" "-1" "cell-lsep" "1em"))))
      (edit (set-cell-mode "table") (cell-del-format ""))
      (check= (body) `(document ,(tab3)))))
  ;; the formats of a selection of cells; a range which reaches the last
  ;; row or column is written with -1
  (with-table-body `(document ,(tab3)) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (table-select-cells 1 2 2 3))
      (check-true (selection-active-table?))
      (check= (table-which-cells) '(1 2 2 3))
      (edit (cell-set-format "cell-background" "blue"))
      (check= (body) `(document ,(tab3 (cwith "1" "2" "2" "-1"
                                              "cell-background" "blue"))))
      (edit (selection-cancel) (table-select-cells 2 3 1 1))
      (edit (cell-set-format "cell-halign" "c"))
      (check= (body) `(document ,(tab3 (cwith "1" "2" "2" "-1"
                                              "cell-background" "blue")
                                       (cwith "2" "-1" "1" "1"
                                              "cell-halign" "c"))))
      (edit (selection-cancel) (table-select-cells 1 3 1 3))
      (edit (cell-del-format "cell-background"))
      (check= (body) `(document ,(tab3 (cwith "2" "-1" "1" "1"
                                              "cell-halign" "c"))))
      (edit (selection-cancel)))))

;; The commands of the Cell menu and of the focus bar.
(define (test-cell-commands)
  (check-group "cell commands")
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (define (fmt) (tree->stree (table-get-format-all)))
      (define (cell22 . l)
        `(tformat ,@(map (lambda (x) (cwith "2" "2" "2" "2" (car x) (cadr x)))
                         l)))
      ;; horizontal alignment: left, center, right
      (edit (cell-halign-right))
      (check= (cell-get-format "cell-halign") "c")
      (edit (cell-halign-right))
      (check= (cell-get-format "cell-halign") "r")
      (edit (cell-halign-right))
      (check= (cell-get-format "cell-halign") "r")
      (edit (cell-halign-left))
      (check= (cell-get-format "cell-halign") "c")
      (edit (cell-halign-left))
      (check= (cell-get-format "cell-halign") "l")
      (edit (cell-halign-left))
      (check= (cell-get-format "cell-halign") "l")
      ;; vertical alignment: top, center, baseline, bottom
      (edit (cell-valign-up))
      (check= (cell-get-format "cell-valign") "c")
      (edit (cell-valign-up))
      (check= (cell-get-format "cell-valign") "t")
      (edit (cell-valign-up))
      (check= (cell-get-format "cell-valign") "t")
      (edit (cell-valign-down))
      (check= (cell-get-format "cell-valign") "c")
      (edit (cell-valign-down))
      (check= (cell-get-format "cell-valign") "B")
      (edit (cell-valign-down))
      (check= (cell-get-format "cell-valign") "b")
      (edit (cell-valign-down))
      (check= (cell-get-format "cell-valign") "b")
      (edit (cell-valign-up))
      (check= (cell-get-format "cell-valign") "B")
      (check= (fmt) (cell22 '("cell-halign" "l") '("cell-valign" "B")))
      (edit (cell-del-format ""))
      (check= (fmt) '(tformat))
      (edit (cell-set-halign "r") (cell-set-valign "t"))
      (check= (fmt) (cell22 '("cell-halign" "r") '("cell-valign" "t")))
      (edit (cell-del-format ""))
      ;; a width makes the width mode exact, the automatic mode empties it
      (edit (cell-set-format* "cell-width" "2cm"))
      (check= (fmt) (cell22 '("cell-width" "2cm") '("cell-hmode" "exact")))
      (edit (cell-set-format* "cell-hmode" "auto"))
      (check= (fmt) (cell22 '("cell-hmode" "auto") '("cell-width" "")))
      (edit (cell-del-format ""))
      (edit (cell-set-format* "cell-height" "1cm"))
      (check= (fmt) (cell22 '("cell-height" "1cm") '("cell-vmode" "exact")))
      (edit (cell-del-format ""))
      (edit (cell-set-exact-width "1cm"))
      (check= (fmt) (cell22 '("cell-width" "1cm") '("cell-hmode" "exact")))
      (check-true (cell-test-exact-width?))
      (check-false (cell-test-minimal-width?))
      (edit (cell-set-minimal-width "2cm"))
      (check= (fmt) (cell22 '("cell-width" "2cm") '("cell-hmode" "max")))
      (check-true (cell-test-minimal-width?))
      (edit (cell-set-maximal-width "3cm"))
      (check= (fmt) (cell22 '("cell-width" "3cm") '("cell-hmode" "min")))
      (check-true (cell-test-maximal-width?))
      (edit (cell-set-automatic-width))
      (check= (fmt) (cell22 '("cell-width" "") '("cell-hmode" "auto")))
      (edit (cell-del-format ""))
      (edit (cell-set-minimal-height "1cm"))
      (check= (fmt) (cell22 '("cell-height" "1cm") '("cell-vmode" "max")))
      (check-true (cell-test-minimal-height?))
      (edit (cell-del-format ""))
      ;; padding
      (edit (cell-set-padding "1pt"))
      (check= (fmt) (cell22 '("cell-lsep" "1pt") '("cell-rsep" "1pt")
                            '("cell-bsep" "1pt") '("cell-tsep" "1pt")))
      (edit (cell-del-format ""))
      (edit (cell-set-hpadding "2pt"))
      (check= (fmt) (cell22 '("cell-lsep" "2pt") '("cell-rsep" "2pt")))
      (edit (cell-del-format ""))
      (edit (cell-set-vpadding "3pt"))
      (check= (fmt) (cell22 '("cell-bsep" "3pt") '("cell-tsep" "3pt")))
      (edit (cell-del-format ""))
      ;; background, height correction
      (edit (cell-set-background "yellow"))
      (check= (fmt) (cell22 '("cell-background" "yellow")))
      (edit (cell-del-format ""))
      (edit (cell-set-vcorrect "n"))
      (check= (fmt) (cell22 '("cell-vcorrect" "n")))
      (edit (cell-del-format ""))
      (check= (body) `(document ,(tab3)))))
  ;; line wrapping and block content make a cell a document, and the
  ;; document goes away with them
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (check-false (cell-test-wrap?))
      (edit (cell-toggle-wrap))
      (check-true (cell-test-wrap?))
      (check= (body) `(document ,(tab (list (cwith "2" "2" "2" "2" "cell-hyphen" "t"))
                                      '("a" "b" "c") '("d" (document "e") "f")
                                      '("g" "h" "i"))))
      (edit (cell-toggle-wrap))
      (check= (body) `(document ,(tab3 (cwith "2" "2" "2" "2" "cell-hyphen" "n"))))
      (edit (cell-del-format ""))
      (edit (cell-set-block "yes"))
      (check= (body) `(document ,(tab (list (cwith "2" "2" "2" "2" "cell-block" "yes"))
                                      '("a" "b" "c") '("d" (document "e") "f")
                                      '("g" "h" "i"))))
      (edit (cell-del-format ""))
      (check= (body) `(document ,(tab3)))
      (edit (set-cell-mode "column") (cell-set-hyphen "c") (set-cell-mode "cell"))
      (check= (body) `(document ,(tab (list (cwith "1" "-1" "2" "2" "cell-hyphen" "c"))
                                      '("a" (document "b") "c")
                                      '("d" (document "e") "f")
                                      '("g" (document "h") "i"))))
      ;; return in a wrapped cell starts a new paragraph of the cell
      (edit (table-go-to 2 2) (kbd-return) (insert "x"))
      (check= (tree->stree (path->tree (table-cell-path 2 2)))
              '(document "e" "x"))
      (check= (table-get-extents) '(3 3)))))

;; Borders are set on the cells and on their neighbours, so that the line
;; between two cells is the same seen from both of them.
(define (test-cell-borders)
  (check-group "cell borders")
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (edit (cell-set-border "1ln"))
      (check= (body)
              `(document ,(tab3 (cwith "2" "2" "2" "2" "cell-tborder" "1ln")
                                (cwith "2" "2" "2" "2" "cell-bborder" "1ln")
                                (cwith "2" "2" "2" "2" "cell-lborder" "1ln")
                                (cwith "2" "2" "2" "2" "cell-rborder" "1ln")
                                (cwith "1" "1" "2" "2" "cell-bborder" "1ln")
                                (cwith "3" "3" "2" "2" "cell-tborder" "1ln")
                                (cwith "2" "2" "1" "1" "cell-rborder" "1ln")
                                (cwith "2" "2" "3" "3" "cell-lborder" "1ln"))))
      (check= (cell-get-format "cell-lborder") "1ln")
      (edit (table-go-to 2 1))
      (check= (cell-get-format "cell-rborder") "1ln")))
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (edit (cell-set-lborder "1ln"))
      (check= (body)
              `(document ,(tab3 (cwith "2" "2" "2" "2" "cell-lborder" "1ln")
                                (cwith "2" "2" "1" "1" "cell-rborder" "1ln"))))))
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (edit (cell-set-bborder "2ln"))
      (check= (body)
              `(document ,(tab3 (cwith "2" "2" "2" "2" "cell-bborder" "2ln")
                                (cwith "3" "3" "2" "2" "cell-tborder" "2ln"))))))
  ;; at the border of the table, there is no neighbour
  (with-table-body `(document ,(tab3)) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (cell-set-tborder "2ln"))
      (check= (body)
              `(document ,(tab3 (cwith "1" "1" "1" "1" "cell-tborder" "2ln"))))
      (edit (cell-set-rborder "1ln"))
      (check= (tree->stree (table-get-format-all))
              `(tformat ,(cwith "1" "1" "1" "1" "cell-tborder" "2ln")
                        ,(cwith "1" "1" "1" "1" "cell-rborder" "1ln")
                        ,(cwith "1" "1" "2" "2" "cell-lborder" "1ln")))))
  ;; the outer borders of a selection, as the border icons of the Cell menu
  (with-table-body `(document ,(tab3)) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (table-select-cells 1 2 1 2))
      (edit (cell-set-borders "1ln" "1ln" "1ln" "1ln" #f #f #f #f))
      (check= (body)
              `(document ,(tab3 (cwith "1" "1" "1" "2" "cell-tborder" "1ln")
                                (cwith "2" "2" "1" "2" "cell-bborder" "1ln")
                                (cwith "3" "3" "1" "2" "cell-tborder" "1ln")
                                (cwith "1" "2" "1" "1" "cell-lborder" "1ln")
                                (cwith "1" "2" "2" "2" "cell-rborder" "1ln")
                                (cwith "1" "2" "3" "3" "cell-lborder" "1ln"))))))
  ;; the pen width of the border icons
  (with-table-body `(document ,(tab3)) '(0 0 0 0 0 0 0)
    (lambda ()
      (check= (cell-get-pen-width) "1ln")
      (cell-set-pen-width "2ln")
      (check= (cell-get-pen-width) "2ln")
      (cell-set-pen-width "1ln")
      (check= (cell-get-pen-width) "1ln"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The formats of the whole table, set with twith by the Table menu; setting
;; a format to "" removes it.
(define (test-table-formats)
  (check-group "table formats")
  (with-table-body `(document ,(tab '() '("a" "b") '("c" "d"))) '(0 0 0 0 0 0 0)
    (lambda ()
      (define (fmt) (tree->stree (table-get-format-all)))
      ;; the defaults
      (check= (table-get-format "table-width") "")
      (check= (table-get-format "table-hmode") "auto")
      (check= (table-get-format "table-halign") "l")
      (check= (table-get-format "table-valign") "f")
      (check= (table-get-format "table-hyphen") "n")
      (check= (table-get-format "table-lborder") "0ln")
      (check= (table-get-format "table-min-rows") "")
      ;; set, replace, remove
      (edit (table-set-format "table-width" "10cm"))
      (check= (body) `(document ,(tab (list (twith "table-width" "10cm"))
                                      '("a" "b") '("c" "d"))))
      (check= (table-get-format "table-width") "10cm")
      (edit (table-set-format "table-width" "5cm"))
      (check= (fmt) `(tformat ,(twith "table-width" "5cm")))
      (edit (table-set-format "table-width" ""))
      (check= (fmt) '(tformat))
      (check= (table-get-format "table-width") "")
      ;; width
      (edit (table-set-exact-width "1par"))
      (check= (fmt) `(tformat ,(twith "table-width" "1par")
                              ,(twith "table-hmode" "exact")))
      (check-true (table-test-exact-width?))
      (check-true (table-test-exact-width? "1par"))
      (check-false (table-test-exact-width? "2par"))
      (edit (table-set-minimal-width "3cm"))
      (check= (fmt) `(tformat ,(twith "table-width" "3cm")
                              ,(twith "table-hmode" "max")))
      (check-true (table-test-minimal-width?))
      (edit (table-set-maximal-width "4cm"))
      (check= (fmt) `(tformat ,(twith "table-width" "4cm")
                              ,(twith "table-hmode" "min")))
      (check-true (table-test-maximal-width?))
      (edit (table-set-automatic-width))
      (check= (fmt) `(tformat ,(twith "table-hmode" "auto")))
      (edit (table-set-format* "table-width" "3cm"))
      (check= (fmt) `(tformat ,(twith "table-width" "3cm")
                              ,(twith "table-hmode" "exact")))
      (edit (table-set-format* "table-hmode" "auto"))
      (check= (fmt) `(tformat ,(twith "table-hmode" "auto")))
      (edit (table-del-format ""))
      (check= (fmt) '(tformat))
      (edit (table-toggle-parwidth))
      (check= (fmt) `(tformat ,(twith "table-width" "1par")
                              ,(twith "table-hmode" "exact")))
      (check-true (table-test-parwidth?))
      (edit (table-toggle-parwidth))
      (check= (fmt) '(tformat))
      (check-false (table-test-parwidth?))
      ;; height
      (edit (table-set-exact-height "2cm"))
      (check= (fmt) `(tformat ,(twith "table-height" "2cm")
                              ,(twith "table-vmode" "exact")))
      (edit (table-set-minimal-height "2cm"))
      (check= (fmt) `(tformat ,(twith "table-height" "2cm")
                              ,(twith "table-vmode" "max")))
      (check-true (table-test-minimal-height?))
      (edit (table-set-automatic-height))
      (check= (fmt) `(tformat ,(twith "table-vmode" "auto")))
      (edit (table-del-format ""))
      ;; alignment
      (edit (table-set-halign "c") (table-set-valign "t"))
      (check= (fmt) `(tformat ,(twith "table-halign" "c")
                              ,(twith "table-valign" "t")))
      (check= (table-get-format "table-halign") "c")
      (edit (table-del-format "table-halign"))
      (check= (fmt) `(tformat ,(twith "table-valign" "t")))
      (edit (table-del-format ""))
      (edit (table-specific-halign "2"))
      (check= (fmt) `(tformat ,(twith "table-col-origin" "2")
                              ,(twith "table-halign" "O")))
      (edit (table-del-format ""))
      (edit (table-specific-valign "1"))
      (check= (fmt) `(tformat ,(twith "table-row-origin" "1")
                              ,(twith "table-valign" "O")))
      (edit (table-del-format ""))
      ;; borders and padding
      (edit (table-set-border "1ln"))
      (check= (fmt) `(tformat ,(twith "table-lborder" "1ln")
                              ,(twith "table-rborder" "1ln")
                              ,(twith "table-bborder" "1ln")
                              ,(twith "table-tborder" "1ln")))
      (edit (table-del-format ""))
      (edit (table-set-padding "2pt"))
      (check= (fmt) `(tformat ,(twith "table-lsep" "2pt")
                              ,(twith "table-rsep" "2pt")
                              ,(twith "table-bsep" "2pt")
                              ,(twith "table-tsep" "2pt")))
      (edit (table-del-format ""))
      ;; page breaking
      (edit (toggle-table-hyphen))
      (check= (fmt) `(tformat ,(twith "table-hyphen" "y")))
      (check= (table-get-format "table-hyphen") "y")
      (edit (toggle-table-hyphen))
      (check= (fmt) `(tformat ,(twith "table-hyphen" "n")))
      (edit (table-del-format ""))
      (check= (body) `(document ,(tab '() '("a" "b") '("c" "d"))))))
  ;; the twith and the cwith together
  (with-table-body `(document ,(tab '() '("a" "b") '("c" "d"))) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (cell-set-format "cell-halign" "r"))
      (edit (table-set-format "table-halign" "c"))
      (check= (body) `(document ,(tab (list (cwith "1" "1" "1" "1" "cell-halign" "r")
                                            (twith "table-halign" "c"))
                                      '("a" "b") '("c" "d"))))
      ;; table-del-format leaves the cwith
      (edit (table-del-format ""))
      (check= (body) `(document ,(tab (list (cwith "1" "1" "1" "1" "cell-halign" "r"))
                                      '("a" "b") '("c" "d"))))
      ;; the formats of the macro are seen, and are not in the tree
      (check= (table-get-format "table-hmode") "auto")))
  (with-table-body '(document "x" (wide-tabular (document (tformat (table (row (cell (document "a")))))))) '(1 0 0 0 0 0 0 0 1)
    (lambda ()
      (check= (table-get-format "table-width") "1par")
      (check= (table-get-format "table-block") "yes")
      (check= (cell-get-format "cell-hyphen") "t"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joined cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Joining cells sets the span of the upper left cell of the selection
;; (cell-row-span, cell-col-span); the cells it covers stay in the tree, and
;; going to one of them goes to the joined cell. Dissociating sets the
;; spans back to 1.
(define (test-joined-cells)
  (check-group "joined cells")
  (with-table-body `(document ,(tab3)) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (table-select-cells 1 2 1 2))
      (edit (cell-set-span-selection))
      (check= (body) `(document ,(tab3 (cwith "1" "1" "1" "1" "cell-row-span" "2")
                                       (cwith "1" "1" "1" "1" "cell-col-span" "2"))))
      (check-false (selection-active-any?))
      (check= (cursor) '(0 0 2 0 0 0 1))
      (check= (cell-get-format "cell-row-span") "2")
      (check= (cell-get-format "cell-col-span") "2")
      (check-true (cell-spans-more?))
      ;; the covered cells
      (edit (table-go-to 2 2))
      (check= (cursor) '(0 0 2 0 0 0 1))
      (check= (table-which-row) 1)
      (check= (table-which-column) 1)
      (edit (table-go-to 1 2))
      (check= (cursor) '(0 0 2 0 0 0 1))
      (edit (table-go-to 2 1))
      (check= (cursor) '(0 0 2 0 0 0 1))
      ;; the cells around
      (edit (table-go-to 2 3))
      (check= (cursor) '(0 0 2 1 2 0 1))
      (check-false (cell-spans-more?))
      (edit (table-go-to 3 1))
      (check= (cursor) '(0 0 2 2 0 0 1))
      ;; FIXME: the structured movements do not leave a joined cell
      ;; (TeXmacs/progs/table/table-edit.scm:190, cell-move-relative moves
      ;; by one cell from the upper left one, and table-go-to brings the
      ;; covered cell back to the joined one): in the cell (1,1) spanning
      ;; two rows and two columns, structured-right gives the cursor
      ;; (0 0 2 0 0 0 1) in the same cell, expected (0 0 2 0 2 0 1) in the
      ;; cell (1,3); structured-down likewise stays, expected the cell (3,1).
      ;; From the cells around, they do go into it:
      (edit (table-go-to 1 3) (structured-left))
      (check= (cursor) '(0 0 2 0 0 0 1))
      (edit (table-go-to 3 1) (structured-up))
      (check= (cursor) '(0 0 2 0 0 0 1))
      ;; dissociate
      (edit (table-go-to 1 1) (cell-reset-span))
      (check= (body) `(document ,(tab3 (cwith "1" "1" "1" "1" "cell-row-span" "1")
                                       (cwith "1" "1" "1" "1" "cell-col-span" "1"))))
      (check-false (cell-spans-more?))
      (edit (table-go-to 2 2))
      (check= (cursor) '(0 0 2 1 1 0 1))))
  (with-table-body `(document ,(tab3)) '(0 0 0 0 0 0 0)
    (lambda ()
      (edit (cell-set-span "2" "3"))
      (check= (body) `(document ,(tab3 (cwith "1" "1" "1" "1" "cell-row-span" "2")
                                       (cwith "1" "1" "1" "1" "cell-col-span" "3"))))
      (edit (table-go-to 2 3))
      (check= (cursor) '(0 0 2 0 0 0 1))
      (edit (table-go-to 3 3))
      (check= (cursor) '(0 0 2 2 2 0 1))
      (edit (table-go-to 1 1) (cell-set-row-span "1") (cell-set-column-span "2"))
      (check= (body) `(document ,(tab3 (cwith "1" "1" "1" "1" "cell-row-span" "1")
                                       (cwith "1" "1" "1" "1" "cell-col-span" "2"))))
      (edit (table-go-to 1 3))
      (check= (cursor) '(0 0 2 0 2 0 1))
      ;; removing the column of a joined cell removes its spans
      (edit (table-go-to 1 1) (table-remove-column #t))
      (check= (body) `(document ,(tab '() '("b" "c") '("e" "f") '("h" "i"))))
      (check= (cursor) '(0 0 0 0 0 0 0))))
  ;; NOTE: inserting or removing a row or a column inside a joined cell does
  ;; not change its span, so that the joined cell then covers other cells:
  ;; table-insert-row #t in a cell spanning two rows inserts the new row
  ;; between the two (edit_table.cpp, table_insert_row).
  (with-table-body `(document ,(tab3 (cwith "1" "1" "1" "1" "cell-row-span" "2")))
                   '(0 0 1 2 2 0 0)
    (lambda ()
      ;; a row below the joined cell
      (edit (table-go-to 2 3) (table-insert-row #t))
      (check= (body) `(document ,(tab (list (cwith "1" "1" "1" "1"
                                                   "cell-row-span" "2"))
                                      '("a" "b" "c") '("d" "e" "f") '("" "" "")
                                      '("g" "h" "i"))))
      (edit (table-go-to 4 2) (table-remove-row #f))
      (check= (body) `(document ,(tab3 (cwith "1" "1" "1" "1" "cell-row-span" "2"))))))
  ;; the joined cell is typeset across the columns it spans: right aligned,
  ;; it ends where the last of them ends
  (with-table-body `(document "x" ,(tab (list (cwith "1" "1" "1" "1" "cell-col-span" "2")
                                              (cwith "1" "1" "1" "1" "cell-halign" "r"))
                                        '("a" "hidden") '("cccc" "dddd")) "y")
                   '(0 0)
    (lambda ()
      (let ((ra (rect 1 0 2 0 0 0))
            (rc (rect 1 0 2 1 0 0))
            (rd (rect 1 0 2 1 1 0)))
        (check-true (near? (caddr ra) (caddr rd) 2))
        (check-true (> (car ra) (caddr rc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structure in cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Subtables, numbered equations, deactivated tables and extracted formats.
(define (test-structure)
  (check-group "structure")
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("c" "d"))) '(1 0 0 0 0 0 1)
    (lambda ()
      (edit (make-subtable))
      (check= (body) `(document "x" ,(tab '() '((subtable (tformat (table (row (cell "")))))
                                                "b")
                                          '("c" "d"))))
      (check= (cursor) '(1 0 0 0 0 0 0 0 0 0 0 0))
      (check= (table-get-extents) '(1 1))
      (edit (insert "s") (table-insert-column #t))
      (check= (body) `(document "x" ,(tab '() '((subtable (tformat (table (row (cell "s")
                                                                              (cell "")))))
                                                "b")
                                          '("c" "d"))))
      (check= (cursor) '(1 0 0 0 0 0 0 0 0 1 0 0))
      ;; the subtable goes when its only column is removed
      (edit (table-remove-column #f) (table-remove-column #t))
      (check= (body) `(document "x" ,(tab '() '("" "b") '("c" "d"))))))
  (with-table-body '(document "x" "") '(1 0)
    (lambda ()
      (edit (make 'eqnarray*))
      (check-false (numbered-numbered? (tree-innermost 'eqnarray*)))
      (edit (numbered-toggle (tree-innermost 'eqnarray*)))
      (check= (body) '(document "x" (eqnarray* (document
                                                (tformat
                                                 (table (row (cell "") (cell "")
                                                             (cell (eq-number)))))))))
      (check-true (numbered-numbered? (tree-innermost 'eqnarray*)))
      (edit (numbered-toggle (tree-innermost 'eqnarray*)))
      (check= (body) '(document "x" (eqnarray* (document
                                                (tformat
                                                 (table (row (cell "") (cell "")
                                                             (cell ""))))))))))
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("c" "d"))) '(1 0 0 0 0 0 1)
    (lambda ()
      (edit (table-deactivate))
      (check= (body) `(document "x" (tabular (inactive (tformat ,(rows '("a" "b")
                                                                    '("c" "d")))))))))
  (with-table-body `(document "x" ,(tab (list (twith "table-halign" "c")
                                              (cwith "1" "1" "1" "1" "cell-halign" "r"))
                                        '("a")))
                   '(1 0 2 0 0 0 1)
    (lambda ()
      (edit (table-extract-format))
      (check= (body) `(document "x" (tformat ,(twith "table-halign" "c")
                                             ,(cwith "1" "1" "1" "1" "cell-halign" "r")
                                             "")))
      (check= (cursor) '(1 2 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy and paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A selection of cells is a subtable with its formats; pasted in a table,
;; it is written over the cells from the cursor on, and the table grows when
;; needed; pasted outside a table, it becomes a tabular; cut empties the
;; cells. The clipboard is a private one.
(define (test-clipboard)
  (check-group "clipboard")
  (with-table-body `(document "x" ,(tab (list (cwith "2" "2" "1" "1" "cell-halign" "r"))
                                        '("a" "b" "c") '("d" "e" "f"))
                              "y")
                   '(1 0 1 0 0 0 0)
    (lambda ()
      (edit (table-select-cells 1 2 1 2))
      (check-true (selection-active-table?))
      (check= (tree->stree (selection-tree))
              `(tformat ,(cwith "2" "2" "1" "1" "cell-halign" "r")
                        ,(rows '("a" "b") '("d" "e"))))
      (edit (clipboard-copy "table-test"))
      (edit (selection-cancel) (table-go-to 2 3))
      (edit (clipboard-paste "table-test"))
      (check= (body) `(document "x"
                                ,(tab (list (cwith "2" "2" "1" "1" "cell-halign" "r")
                                            (cwith "3" "3" "3" "3" "cell-halign" "r"))
                                      '("a" "b" "c" "") '("d" "e" "a" "b")
                                      '("" "" "d" "e"))
                                "y"))
      (edit (go-to (at 2 1)))
      (edit (clipboard-paste "table-test"))
      (check= (tree->stree (path->tree (at 2)))
              `(concat "y" ,(tab (list (cwith "2" "2" "1" "1" "cell-halign" "r"))
                                 '("a" "b") '("d" "e"))))
      (clipboard-clear "table-test")))
  (with-table-body `(document "x" ,(tab '() '("a" "b" "c") '("d" "e" "f")) "y")
                   '(1 0 0 0 0 0 0)
    (lambda ()
      (edit (table-select-cells 1 1 1 3))
      (edit (clipboard-cut "table-test"))
      (check= (body) `(document "x" ,(tab '() '("" "" "") '("d" "e" "f")) "y"))
      (check= (tree->stree (clipboard-get "table-test"))
              ;; the language of the document: that of the locale
              `(tuple "texmacs" (tformat ,(rows '("a" "b" "c"))) "text"
                      ,(get-preference "language")))
      (clipboard-clear "table-test"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each table command is one step of the undo history.
(define (test-undo)
  (check-group "undo")
  (with-table-body `(document ,(tab3)) '(0 0 0 1 1 0 0)
    (lambda ()
      (let* ((s0 (body))
             (s1 (begin (edit (table-insert-row #t)) (body)))
             (s2 (begin (edit (table-remove-column #f)) (body)))
             (s3 (begin (edit (cell-set-format "cell-halign" "c")) (body))))
        (check= s1 `(document ,(tab '() '("a" "b" "c") '("d" "e" "f")
                                    '("" "" "") '("g" "h" "i"))))
        (check= s2 `(document ,(tab '() '("b" "c") '("e" "f")
                                    '("" "") '("h" "i"))))
        (check= s3 `(document ,(tab (list (cwith "3" "3" "1" "1" "cell-halign" "c"))
                                    '("b" "c") '("e" "f") '("" "") '("h" "i"))))
        (edit (undo 0))
        (check= (body) s2)
        (edit (undo 0))
        (check= (body) s1)
        (edit (undo 0))
        (check= (body) s0)
        (check= (undo-possibilities) 0)
        (edit (redo 0))
        (check= (body) s1)
        (edit (redo 0) (redo 0))
        (check= (body) s3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typesetting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The typeset table follows the edits: the alignment of a cell moves its
;; contents, a new row makes the table taller, a new column wider. The
;; rectangles are those of the contents of the cells.
(define (test-typeset)
  (check-group "typeset")
  (with-table-body `(document "x" ,(tab '() '("a" "b") '("cccc" "dddd")) "y") '(0 0)
    (lambda ()
      (let ((ra (rect 1 0 0 0 0 0))
            (rb (rect 1 0 0 0 1 0))
            (rc (rect 1 0 0 1 0 0))
            (rt (rect 1)))
        ;; left aligned, the rows go downwards, the columns to the right
        (check-true (near? (car ra) (car rc) 2))
        (check-true (< (cadddr rc) (cadr ra)))
        (check-true (> (car rb) (caddr rc)))
        ;; the table is between the paragraphs around it
        (check-true (< (cadddr rt) (cadr (rect 0))))
        (check-true (> (cadr rt) (cadddr (rect 2))))
        (edit (go-to (at 1 0 0 0 0 0 0)) (cell-set-format "cell-halign" "r"))
        (with ra2 (rect 1 0 1 0 0 0)
          (check-true (near? (caddr ra2) (caddr rc) 2))
          (check-true (> (car ra2) (car ra))))
        (edit (cell-set-format "cell-halign" "c"))
        (with ra3 (rect 1 0 1 0 0 0)
          (check-true (near? (/ (+ (car ra3) (caddr ra3)) 2)
                             (/ (+ (car rc) (caddr rc)) 2) 2)))
        (edit (table-insert-row #t))
        (with rt2 (rect 1)
          (check-true (< (cadr rt2) (cadr rt)))
          (check-true (near? (cadddr rt2) (cadddr rt) 2))
          (check-true (near? (caddr rt2) (caddr rt) 2)))
        (edit (table-insert-column #t) (insert "eeee"))
        (with rt3 (rect 1)
          (check-true (> (caddr rt3) (caddr rt)))
          (check-true (near? (car rt3) (car rt) 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Left out: the decorations of rows and columns (table-row-decoration,
;; table-column-decoration of the Table menu), since typesetting a table
;; with a cell-decoration corrupts the memory (see the report of the suite).

(tm-define (table-test-failures)
  (:synopsis "Run the tests of table editing and return the number of failures")
  (check-suite "table")
  (test-insert-text)
  (test-insert-math)
  (test-queries)
  (test-moving)
  (test-insert-rows-columns)
  (test-insert-formats)
  (test-limits)
  (test-remove-rows-columns)
  (test-remove-formats)
  (test-delete-table)
  (test-cell-formats)
  (test-cell-commands)
  (test-cell-borders)
  (test-table-formats)
  (test-joined-cells)
  (test-structure)
  (test-clipboard)
  (test-undo)
  (test-typeset)
  (check-end))
