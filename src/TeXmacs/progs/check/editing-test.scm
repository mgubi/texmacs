;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : editing-test.scm
;; DESCRIPTION : tests of editing in a buffer, without a window
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite drives TeXmacs the way a user or a plugin does: it opens
;; buffers, types, moves the cursor, selects, cuts and pastes, makes
;; structure, changes the environment, undoes and redoes, saves and exports,
;; all through the editing commands and without a window.
;;
;; Without a window, three things which the event loop does are missing,
;; and the suite does them itself (see edit-step):
;;
;;   - a key press or a menu action is wrapped in archive-state,
;;     start-editing and end-editing; the last one confirms the changes
;;     to the undo history, without which nothing can be undone and the
;;     buffer does not count as modified;
;;   - nothing typesets the buffer, so update-forced is called after each
;;     step (the environment at the cursor, get-env, needs it);
;;   - the changes are never applied to a view (apply_changes runs only for
;;     views with a window), so that the editor keeps believing that the
;;     tree has changed since the last typesetting, and the cursor
;;     movements which go through the boxes (go-left, go-right, go-up,
;;     go-down, go-start-line, go-end-line, kbd-left...) do nothing. They
;;     need a window and are left out; the movements which go through the
;;     tree (go-start, go-end, go-to-next-word, go-end-paragraph,
;;     tree-go-to...) are checked. For the same reason get-env does not see
;;     a change of the initial environment (init-env), nor the initial
;;     environment of a loaded file, which only apply_changes merges in;
;;     get-env is checked on the tags around the cursor.
;;
;; switch-to-buffer always shows a view of the buffer which is not in a
;; window, and makes a new one when there is none: switching to the buffer
;; which is already shown (as new-buffer and load-buffer leave it) makes a
;; second view, with its own undo history, and the buffer is modified as
;; soon as one of its views is. The suite uses switch-to-buffer*, which does
;; nothing for the current buffer.
;;
;; Paths are absolute: the root tree holds all buffers, and a path in the
;; current buffer starts with (buffer-path). The clipboards used are private
;; ones, never "primary", which is the clipboard of the system.

(texmacs-module (check editing-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the temporary files go to editing-tmp in the temporary directory
(define editing-dir
  (string-append (url->system (url-temp-dir)) "/editing-tmp"))

(define (tmp-file name)
  (system->url (string-append editing-dir "/" name)))

(define editing-files '())

(define (tmp-file* name)
  ;; a temporary file which the suite removes at the end
  (with u (tmp-file name)
    (set! editing-files (cons u editing-files))
    u))

(define (remove-tmp-files)
  (for-each (lambda (u) (when (url-exists? u) (system-remove u)))
            editing-files)
  (set! editing-files '())
  ;; the directory goes as well, if nothing else is left in it
  (when (null? (url-read-directory (system->url editing-dir) "*"))
    (system-rmdir (system->url editing-dir))))

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
  (list-tail p (length (buffer-path))))

(define (cursor) (rel (cursor-path)))

(define (with-buffer-body doc thunk)
  ;; run @thunk in a new buffer holding @doc, then close the buffers it
  ;; opened, also when it renamed the new buffer (save-buffer-as)
  (let* ((old (current-buffer))
         (before (buffer-list))
         (u (new-buffer)))
    ;; new-buffer shows the buffer in the current window; switching to it
    ;; again would make a second view (see test-buffers)
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (go-start)
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (for (b (buffer-list))
        (when (nin? b before) (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A new buffer is empty, becomes the current buffer, is listed among the
;; buffers and is unmodified; its body can be read in the various ways
;; (buffer-get-body, buffer-tree, the subtree of the root tree) and set;
;; closing it removes it from the list.
(define (test-buffers)
  (check-group "buffers")
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (check-true (url? u))
    (check-true (buffer-exists? u))
    (check-true (in? u (buffer-list)))
    (check-false (== u old))
    (check= (current-buffer) u)
    (check= (length (buffer->views u)) 1)
    (switch-to-buffer* u)
    (check= (length (buffer->views u)) 1)
    (switch-to-buffer old)
    (check= (current-buffer) old)
    ;; the view which old left is reused
    (switch-to-buffer u)
    (check= (current-buffer) u)
    (check= (length (buffer->views u)) 1)
    (check= (tree->stree (buffer-get-body u)) '(document ""))
    (check= (tree->stree (buffer-tree)) '(document ""))
    (check-false (buffer-modified? u))
    (buffer-set-body u (stree->tree '(document "one" "two")))
    (check= (tree->stree (buffer-get-body u)) '(document "one" "two"))
    (check= (tree->stree (buffer-tree)) '(document "one" "two"))
    (check= (tree->stree (path->tree (buffer-path))) '(document "one" "two"))
    (check= (tree->stree (tree-ref (root-tree) (car (buffer-path))))
            '(document "one" "two"))
    (check= (tree->path (buffer-tree)) (buffer-path))
    (check-true (tree-is-buffer? (buffer-tree)))
    (check-false (tree-is-buffer? (tree-ref (buffer-tree) 0)))
    ;; buffer-pretend-modified and buffer-pretend-saved set the flag
    (buffer-pretend-modified u)
    (check-true (buffer-modified? u))
    (buffer-pretend-saved u)
    (check-false (buffer-modified? u))
    (buffer-close u)
    (check-false (buffer-exists? u))
    (check-false (in? u (buffer-list)))
    (when (buffer-exists? old) (switch-to-buffer old))
    (check= (current-buffer) old)))

;; A document written to a .tm file and loaded: the body, the style and the
;; initial environment of the file are those of the buffer, which is not
;; modified by loading.
(define (test-load)
  (check-group "load")
  (let ((f (tmp-file* "load.tm"))
        (old (current-buffer)))
    (string-save
     (string-append
      "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
      "  Hello <em|world>.\n\n  <section|Title>\n\n"
      "  <\\theorem>\n    Statement.\n  </theorem>\n</body>\n\n"
      "<initial|<\\collection>\n<associate|font-base-size|12>\n"
      "</collection>>\n")
     (url->system f))
    ;; load-buffer shows the buffer in the current window
    (load-buffer f)
    (check= (current-buffer) f)
    (switch-to-buffer* f)
    (update-forced)
    (check= (current-buffer) f)
    (check= (length (buffer->views f)) 1)
    (check-true (buffer-exists? f))
    (check-false (buffer-modified? f))
    (check= (body)
            '(document (concat "Hello " (em "world") ".")
                       (section "Title")
                       (theorem (document "Statement."))))
    (check= (get-style-list) '("generic"))
    (check= (get-init "font-base-size") "12")
    (check-true (init-has? "font-base-size"))
    (check-false (init-has? "par-width"))
    ;; the cursor starts at the beginning of the document
    (check= (cursor) '(0 0 0))
    (buffer-close f)
    (check-false (buffer-exists? f))
    (when (buffer-exists? old) (switch-to-buffer old))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insert puts text or markup at the cursor and leaves the cursor after it,
;; or at the given path inside it; make makes a tag with empty arguments
;; and puts the cursor in the first one; insert-go-to puts the cursor at an
;; explicit path; key-press types as a user does.
(define (test-insert)
  (check-group "insert")
  (with-buffer-body '(document "")
    (lambda ()
      (edit (insert "hello"))
      (check= (body) '(document "hello"))
      (check= (cursor) '(0 5))
      (check-true (buffer-modified? (current-buffer)))
      (edit (insert " world"))
      (check= (body) '(document "hello world"))
      (check= (cursor) '(0 11))
      (edit (go-to (at 0 5)) (insert ","))
      (check= (body) '(document "hello, world"))
      (check= (cursor) '(0 6))
      ;; markup: the cursor goes after the inserted tree, at position 1
      ;; of the em tag in the concat
      (edit (go-end) (insert '(em "x")))
      (check= (body) '(document (concat "hello, world" (em "x"))))
      (check= (cursor) '(0 1 1))
      ;; insert with a path inside the inserted tree
      (edit (insert '(frac "a" "b") 1 0))
      (check= (body) '(document (concat "hello, world" (em "x") (frac "a" "b"))))
      (check= (cursor) '(0 2 1 0))
      ;; make: empty arguments, cursor in the first one
      (edit (go-end) (make 'sqrt))
      (check= (body) '(document (concat "hello, world" (em "x")
                                        (frac "a" "b") (sqrt ""))))
      (check= (cursor) '(0 3 0 0))
      (edit (insert "2"))
      (check= (cursor) '(0 3 0 1))
      ;; insert-go-to with an explicit path
      (edit (go-end) (insert-go-to '(strong "ab") '(0 1)))
      (check= (tree->stree (tree-ref (buffer-tree) 0 4)) '(strong "ab"))
      (check= (cursor) '(0 4 0 1))
      ;; a new paragraph
      (edit (go-end) (insert-return))
      (check= (tm-arity (buffer-tree)) 2)
      (check= (cursor) '(1 0))
      ;; typing
      (edit (key-press "a") (key-press "b") (key-press "c"))
      (check= (tree->stree (tree-ref (buffer-tree) 1)) "abc")
      (check= (cursor) '(1 3))
      (key-press "backspace")
      (update-forced)
      (check= (tree->stree (tree-ref (buffer-tree) 1)) "ab"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The movements which follow the tree: go-to, go-start, go-end, by words,
;; to the ends of a paragraph and of a tag, tree-go-to on a subtree. The
;; movements by characters and lines go through the boxes and need a window
;; (see the head of the file).
(define (test-cursor)
  (check-group "cursor")
  (with-buffer-body '(document "hello world foo" "second line"
                               (concat "x" (frac "a" "b") "y"))
    (lambda ()
      (check= (cursor) '(0 0))
      (edit (go-end))
      (check= (cursor) '(2 2 1))
      (edit (go-start))
      (check= (cursor) '(0 0))
      (edit (go-to (at 1 3)))
      (check= (cursor) '(1 3))
      (check= (cursor-path) (at 1 3))
      (check= (tree->stree (cursor-tree)) "second line")
      ;; by words
      (edit (go-start) (go-to-next-word))
      (check= (cursor) '(0 5))
      (edit (go-to-next-word))
      (check= (cursor) '(0 11))
      (edit (go-to-previous-word))
      (check= (cursor) '(0 6))
      ;; to the ends of a paragraph
      (edit (go-to (at 0 3)) (go-end-paragraph))
      (check= (cursor) '(0 15))
      (edit (go-start-paragraph))
      (check= (cursor) '(0 0))
      ;; tree-go-to on a subtree
      (with t (tree-ref (buffer-tree) 2 1)
        (edit (tree-go-to t 1 :end))
        (check= (cursor) '(2 1 1 1))
        (check-true (cursor-inside? t))
        ;; the position before the fraction is written as the end of the
        ;; string before it
        (edit (tree-go-to t :start))
        (check= (cursor) '(2 0 1))
        (edit (tree-go-to t 0 :start))
        (check= (cursor) '(2 1 0 0))
        ;; go-start-of and go-end-of the innermost tag
        (edit (go-end-of 'frac))
        (check= (cursor) '(2 1 1))
        (edit (tree-go-to t 0 :end) (go-start-of 'frac))
        (check= (cursor) '(2 0 1)))
      (edit (go-to (at 0 3)))
      (check-true (cursor-accessible?))
      ;; the box movements need a window: (go-right) from (0 3) would give
      ;; (0 4) and (go-down) would reach paragraph 1, but here they do not
      ;; move (see the head of the file)
      (edit (go-right))
      (check= (cursor) '(0 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection and clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; select-all, a selection set by paths and read back, copy, cut and paste
;; through a private clipboard, the removal of a selection by backspace.
(define (test-selection)
  (check-group "selection")
  (with-buffer-body '(document "hello world" "second")
    (lambda ()
      (check-false (selection-active-any?))
      (edit (select-all))
      (check-true (selection-active-any?))
      (check= (tree->stree (selection-tree)) '(document "hello world" "second"))
      (edit (selection-cancel))
      (check-false (selection-active-any?))
      ;; a selection inside a string
      (edit (selection-set (at 0 6) (at 0 11)))
      (check-true (selection-active-normal?))
      (check-true (selection-active-small?))
      (check= (tree->stree (selection-tree)) "world")
      (check= (selection-get-start) (at 0 6))
      (check= (selection-get-end) (at 0 11))
      ;; copy and paste; the selection stays after a copy, and go-to does
      ;; not cancel it (a paste would replace it), so it is cancelled first
      (edit (clipboard-copy "editing-test"))
      (check= (tree->stree (clipboard-get "editing-test"))
              '(tuple "texmacs" "world" "text" "english"))
      (check= (body) '(document "hello world" "second"))
      (check-true (selection-active-any?))
      (edit (selection-cancel) (go-to (at 1 6)) (clipboard-paste "editing-test"))
      (check= (body) '(document "hello world" "secondworld"))
      (check= (cursor) '(1 11))
      ;; cut
      (edit (selection-set (at 0 0) (at 0 6)) (clipboard-cut "editing-test"))
      (check= (body) '(document "world" "secondworld"))
      (check-false (selection-active-any?))
      (check= (tree->stree (clipboard-get "editing-test"))
              '(tuple "texmacs" "hello " "text" "english"))
      (edit (go-end) (clipboard-paste "editing-test"))
      (check= (body) '(document "world" "secondworldhello "))
      ;; a paste replaces the selection
      (edit (selection-set (at 0 0) (at 0 3))
            (clipboard-paste "editing-test"))
      (check= (body) '(document "hello ld" "secondworldhello "))
      ;; backspace removes the selection
      (edit (selection-set (at 1 6) (at 1 11)) (kbd-backspace))
      (check= (body) '(document "hello ld" "secondhello "))
      ;; a selection across paragraphs
      (edit (selection-set (at 0 2) (at 1 3)))
      (check= (tree->stree (selection-tree)) '(document "llo ld" "sec"))
      (edit (clipboard-cut "editing-test"))
      (check= (body) '(document "heondhello "))
      (edit (clipboard-clear "editing-test"))
      (check= (tree->stree (clipboard-get "editing-test")) "none"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Removing text forwards and backwards (kbd-backspace, kbd-delete,
;; remove-text), and a tag around the cursor (remove-structure-upwards).
(define (test-remove)
  (check-group "remove")
  (with-buffer-body '(document "abcdef" (concat "x" (em "yz") "w"))
    (lambda ()
      (edit (go-to (at 0 3)) (kbd-backspace))
      (check= (tree->stree (tree-ref (buffer-tree) 0)) "abdef")
      (check= (cursor) '(0 2))
      (edit (kbd-delete))
      (check= (tree->stree (tree-ref (buffer-tree) 0)) "abef")
      (check= (cursor) '(0 2))
      (edit (remove-text #t))
      (check= (tree->stree (tree-ref (buffer-tree) 0)) "abf")
      (edit (remove-text #f))
      (check= (tree->stree (tree-ref (buffer-tree) 0)) "af")
      ;; backspace at the start of a paragraph joins it to the previous one
      (edit (go-to (at 1 0 0)) (kbd-backspace))
      (check= (body) '(document (concat "afx" (em "yz") "w")))
      ;; the em around the cursor is removed, its content is kept
      (edit (tree-go-to (tree-ref (buffer-tree) 0 1) 0 1)
            (remove-structure-upwards))
      (check= (body) '(document "afxyzw")))))

;; Making structure: fractions, square roots, scripts, mathematics,
;; sections (make-section), environments (make with a selection wraps it),
;; numbered/unnumbered toggles and variants (numbered-toggle,
;; variant-circulate).
(define (test-structure)
  (check-group "structure")
  (with-buffer-body '(document "")
    (lambda ()
      (edit (make 'math))
      (check= (body) '(document (math "")))
      (check= (get-env "mode") "math")
      (edit (insert "x") (make-fraction) (insert "1"))
      (check= (body) '(document (math (concat "x" (frac "1" "")))))
      (check= (cursor) '(0 0 1 0 1))
      (edit (tree-go-to (tree-ref (buffer-tree) 0 0 1) 1 :start) (insert "2"))
      (check= (body) '(document (math (concat "x" (frac "1" "2")))))
      (edit (tree-go-to (tree-ref (buffer-tree) 0 0 1) :end)
            (make-script #t #t) (insert "n"))
      (check= (body) '(document (math (concat "x" (frac "1" "2") (rsup "n")))))
      (edit (tree-go-to (tree-ref (buffer-tree) 0) :end) (make-sqrt))
      (check= (tree->stree (tree-ref (buffer-tree) 0))
              '(concat (math (concat "x" (frac "1" "2") (rsup "n"))) (sqrt "")))
      ;; the fraction becomes a text fraction and back: variants
      (with t (tree-ref (buffer-tree) 0 0 0 1)
        (edit (variant-circulate t #t))
        (check= (tree->stree (tree-ref (buffer-tree) 0 0 0 1))
                '(tfrac "1" "2"))
        (edit (variant-circulate (tree-ref (buffer-tree) 0 0 0 1) #f))
        (check= (tree->stree (tree-ref (buffer-tree) 0 0 0 1))
                '(frac "1" "2")))))
  (with-buffer-body '(document "Intro" "body text")
    (lambda ()
      ;; a section made on an empty paragraph
      (edit (go-to (at 0 5)) (insert-return) (make-section 'section)
            (insert "Title"))
      (check= (body) '(document "Intro" (section "Title") "body text"))
      (with t (tree-ref (buffer-tree) 1)
        (check-true (numbered-context? t))
        (check-true (numbered-numbered? t))
        (edit (numbered-toggle t))
        (check= (tree->stree (tree-ref (buffer-tree) 1)) '(section* "Title"))
        (check-false (numbered-numbered? (tree-ref (buffer-tree) 1)))
        (edit (numbered-toggle (tree-ref (buffer-tree) 1)))
        (check= (tree->stree (tree-ref (buffer-tree) 1)) '(section "Title"))
        (edit (variant-circulate (tree-ref (buffer-tree) 1) #t))
        (check= (tree->stree (tree-ref (buffer-tree) 1)) '(subsection "Title"))
        (edit (variant-circulate (tree-ref (buffer-tree) 1) #f))
        (check= (tree->stree (tree-ref (buffer-tree) 1)) '(section "Title")))
      ;; an environment around a selected paragraph
      (edit (selection-set (at 2 0) (at 2 9)) (make 'theorem))
      (check= (tree->stree (tree-ref (buffer-tree) 2))
              '(theorem (document "body text")))
      (with t (tree-ref (buffer-tree) 2)
        (edit (numbered-toggle t))
        (check= (tree->stree (tree-ref (buffer-tree) 2))
                '(theorem* (document "body text")))
        (edit (variant-circulate (tree-ref (buffer-tree) 2) #t))
        (check-true (tree-in? (tree-ref (buffer-tree) 2)
                              (variants-of 'theorem*)))
        (check= (tree->stree (tree-ref (buffer-tree) 2 0))
                '(document "body text"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The initial environment of the document (init-env, get-init, init-has?,
;; init-default), the environment at the cursor (get-env) and with on the
;; selection (make-with).
(define (test-environment)
  (check-group "environment")
  (with-buffer-body '(document (concat "plain " (em "italic") " "
                                       (strong "bold")))
    (lambda ()
      (check-false (init-has? "font-base-size"))
      (with def (get-init "font-base-size")
        (edit (init-env "font-base-size" "12"))
        (check= (get-init "font-base-size") "12")
        (check-true (init-has? "font-base-size"))
        ;; get-env does not see the change: the environment of the
        ;; document is recomputed (typeset_invalidate_env) only when the
        ;; changes are applied to a view with a window
        (check-true (buffer-modified? (current-buffer)))
        (check= (tree->stree (get-init-tree "font-base-size")) "12")
        (edit (init-env-tree "par-first" (stree->tree "2fn")))
        (check= (get-init "par-first") "2fn")
        (edit (init-default "font-base-size" "par-first"))
        (check-false (init-has? "font-base-size"))
        (check-false (init-has? "par-first"))
        (check= (get-init "font-base-size") def))
      ;; the environment at the cursor
      (edit (go-to (at 0 0 2)))
      (check= (get-env "font-shape") "right")
      (check= (get-env "mode") "text")
      (edit (tree-go-to (tree-ref (buffer-tree) 0 1) 0 2))
      (check= (get-env "font-shape") "italic")
      (edit (tree-go-to (tree-ref (buffer-tree) 0 3) 0 2))
      (check= (get-env "font-series") "bold")
      ;; with on a selection
      (edit (selection-set (at 0 0 0) (at 0 0 5)) (make-with "color" "red"))
      (check= (tree->stree (tree-ref (buffer-tree) 0 0))
              '(with "color" "red" "plain"))
      (edit (tree-go-to (tree-ref (buffer-tree) 0 0) 2 2))
      (check= (get-env "color") "red")
      ;; with at the cursor, without a selection: an empty with
      (edit (go-end) (make-with "font-series" "bold") (insert "B"))
      (check= (tree->stree (cAr (tree-children (tree-ref (buffer-tree) 0))))
              '(with "font-series" "bold" "B"))
      (check= (get-env "font-series") "bold"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo and redo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each step can be undone and redone, restoring the exact trees and the
;; cursor; undoing back to the saved state makes the buffer unmodified.
(define (test-undo)
  (check-group "undo")
  (with-buffer-body '(document "")
    (lambda ()
      (let* ((s0 (body))
             (s1 (begin (edit (insert "abc")) (body)))
             (c1 (cursor))
             (s2 (begin (edit (make 'frac) (insert "x")) (body)))
             (s3 (begin (edit (go-end) (insert-return) (insert "d")) (body)))
             (s4 (begin (edit (go-to (at 0 0 1)) (kbd-backspace)) (body))))
        (check= s1 '(document "abc"))
        (check= s2 '(document (concat "abc" (frac "x" ""))))
        (check= s3 '(document (concat "abc" (frac "x" "")) "d"))
        (check= s4 '(document (concat "bc" (frac "x" "")) "d"))
        (check-true (> (undo-possibilities) 0))
        (check= (redo-possibilities) 0)
        (edit (undo 0))
        (check= (body) s3)
        (edit (undo 0))
        (check= (body) s2)
        (check-true (> (redo-possibilities) 0))
        (edit (undo 0))
        (check= (body) s1)
        (check= (cursor) c1)
        (edit (redo 0))
        (check= (body) s2)
        (edit (redo 0))
        (check= (body) s3)
        (edit (redo 0))
        (check= (body) s4)
        (check= (redo-possibilities) 0)
        ;; back to the start
        (edit (undo 0)) (edit (undo 0)) (edit (undo 0)) (edit (undo 0))
        (check= (body) s0)
        (check= (undo-possibilities) 0)
        ;; nothing more to undo: no change
        (edit (undo 0))
        (check= (body) s0)
        ;; a new edit after an undo forgets the redo
        (edit (redo 0))
        (check= (body) s1)
        (edit (insert "z"))
        (check= (body) '(document "abcz"))
        (check= (redo-possibilities) 0)
        ;; several changes in one step are undone together
        (edit (insert "1") (insert "2") (insert-return) (insert "3"))
        (check= (body) '(document "abcz12" "3"))
        (edit (undo 0))
        (check= (body) '(document "abcz"))
        ;; the initial environment is not part of the undo history:
        ;; init-env changes a table of the editor, not the document tree
        (edit (init-env "font-base-size" "14"))
        (edit (undo 0))
        (check= (body) '(document "abc"))
        (check= (get-init "font-base-size") "14")))))

;; Saving and undo: after a save the buffer is unmodified, an edit makes it
;; modified, and undoing the edit makes it unmodified again.
(define (test-undo-save)
  (check-group "undo after save")
  (with-buffer-body '(document "")
    (lambda ()
      (let ((f (tmp-file* "undo-save.tm")))
        (edit (insert "saved text"))
        (check-true (buffer-modified? (current-buffer)))
        (edit (save-buffer-as f))
        (check= (current-buffer) f)
        (check-false (buffer-modified? f))
        (edit (insert " more"))
        (check-true (buffer-modified? f))
        (edit (undo 0))
        (check= (body) '(document "saved text"))
        (check-false (buffer-modified? f))
        (edit (redo 0))
        (check-true (buffer-modified? f))
        (edit (undo 0))
        ;; undoing past the save makes it modified again
        (edit (undo 0))
        (check= (body) '(document ""))
        (check-true (buffer-modified? f))
        (edit (redo 0))
        (check-false (buffer-modified? f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and exporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-contains? s what)
  (and (string? s) (string-search-forwards what 0 s) #t))

(define (doc-part doc tag)
  ;; the part of a document read from a file, such as (body ...)
  (list-find (cdr doc) (lambda (x) (and (pair? x) (== (car x) tag)))))

(define saved-doc
  '(document (section "Results")
             (concat "Some " (em "emphasized") " text and "
                     (math (frac "a" "b")) ".")
             (itemize (document (concat (item) "first")
                                (concat (item) "second")))
             (theorem (document "A statement."))))

;; save-buffer-as renames the buffer and writes the file; the file loads
;; back into the same tree; save-buffer writes later changes; export-buffer
;; writes LaTeX, HTML and plain text holding the text of the document.
(define (test-save)
  (check-group "save")
  (with-buffer-body saved-doc
    (lambda ()
      (let ((f (tmp-file* "save.tm")))
        (edit (init-env "font-base-size" "11"))
        (check-true (buffer-modified? (current-buffer)))
        (edit (save-buffer-as f))
        (check= (current-buffer) f)
        (check-true (url-exists? f))
        (check-false (buffer-modified? f))
        (check-true (string-contains? (string-load f) "emphasized"))
        ;; the file read back
        (with doc (tree->stree (tree-import f "texmacs"))
          (check= (doc-part doc 'body) `(body ,saved-doc))
          ;; the style, followed by the language package of the locale
          (check= (cadr (cadr (doc-part doc 'style))) "generic")
          (check-true (string-contains? (object->string (doc-part doc 'initial))
                                        "font-base-size")))
        ;; save-buffer after a change
        (edit (go-end) (insert-return) (insert "Appended."))
        (check-true (buffer-modified? f))
        (edit (save-buffer))
        (check-false (buffer-modified? f))
        (check-true (string-contains? (string-load f) "Appended."))
        ;; reload in a new buffer: the same body and initial environment
        (let ((b (body))
              (g (tmp-file* "save-copy.tm")))
          (system-copy f g)
          (load-buffer g)
          (switch-to-buffer* g)
          (update-forced)
          (check= (body) b)
          (check= (get-init "font-base-size") "11")
          (check-false (buffer-modified? g))
          (buffer-close g)
          (switch-to-buffer f))))))

(define (test-export)
  (check-group "export")
  (with-buffer-body saved-doc
    (lambda ()
      (let ((tex (tmp-file* "export.tex"))
            (html (tmp-file* "export.html"))
            (txt (tmp-file* "export.txt"))
            ;; the HTML export writes the formula as an image
            (img (tmp-file* "export-1.png")))
        (edit (export-buffer tex))
        (check-true (url-exists? tex))
        (with s (string-load tex)
          (check-true (string-contains? s "\\section{Results}"))
          (check-true (string-contains? s "emphasized"))
          (check-true (string-contains? s "\\frac{a}{b}"))
          (check-true (string-contains? s "\\begin{itemize}"))
          (check-true (string-contains? s "\\begin{document}")))
        (edit (export-buffer html))
        (check-true (url-exists? html))
        (with s (string-load html)
          (check-true (string-contains? s "<html"))
          (check-true (string-contains? s "Results"))
          (check-true (string-contains? s "emphasized"))
          (check-true (string-contains? s "<li")))
        (edit (export-buffer txt))
        (check-true (url-exists? txt))
        (with s (string-load txt)
          (check-true (string-contains? s "Results"))
          (check-true (string-contains? s "Some emphasized text"))
          (check-true (string-contains? s "A statement.")))
        ;; exporting does not rename the buffer or change it (the body was
        ;; set by buffer-set-body, which does not modify the buffer)
        (check-false (== (current-buffer) tex))
        (check-false (buffer-modified? (current-buffer)))
        (check= (body) saved-doc)))))

;; The text which a copy gives to the other programs (verbatim-snippet, as
;; selection_set makes it): in code (a verbatim-code, the font tt), ... and
;; the backquote are kept as they are, in UTF-8 where the conversion makes
;; an ellipsis and a quote of them in text.
(define (test-copy-as-text)
  (check-group "copy as text")
  (with-buffer-body '(document (verbatim-code (document "x in {0,...,5} `a`")))
    (lambda ()
      (tree-go-to (tree-ref (buffer-tree) 0 0 0) 3)
      (check= (get-env "font-family") "tt")
      (with r (convert (stree->tree "x in {0,...,5} `a`")
                       "texmacs-tree" "verbatim-snippet"
                       (cons "texmacs->verbatim:encoding" "utf-8"))
        (check= r "x in {0,...,5} `a`")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (editing-test-failures)
  (:synopsis "Run the tests of editing and return the number of failures")
  (check-suite "editing")
  (system-mkdir (system->url editing-dir))
  (test-buffers)
  (test-load)
  (test-insert)
  (test-cursor)
  (test-selection)
  (test-remove)
  (test-structure)
  (test-environment)
  (test-undo)
  (test-undo-save)
  (test-save)
  (test-export)
  (test-copy-as-text)
  (remove-tmp-files)
  (check-end))
