
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : read-only-test.scm
;; DESCRIPTION : tests of read-only buffers and of the consoles
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A read-only buffer (buffer-set-read-only) can be read, selected and
;; copied, but what is done in its editor does not change it: a key, a mouse
;; action or a menu command runs between start-editing and end-editing (see
;; edit-step), and what it changed is cancelled at end-editing; the cursor
;; still moves, and undo and redo do nothing. What the program changes
;; outside of an event stays (the messages of the consoles).
;;
;; The consoles (debug-widgets.scm) show the messages in read-only embedded
;; editors, one buffer per tab, to which the new messages are added.

(texmacs-module (check read-only-test)
  (:use (check check-lib)
        (utils library cursor)
        (debug debug-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-step thunk)
  ;; one user action, as the event loop wraps a key press or a menu action
  ;; (edit_keyboard.cpp, before_menu_action and after_menu_action)
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define-macro (edit . body)
  `(edit-step (lambda () ,@body)))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (cursor) (list-tail (cursor-path) (length (buffer-path))))

(define (with-buffer-body doc thunk)
  ;; run @thunk in a new buffer holding @doc, then close the buffers it
  ;; opened (see editing-test.scm)
  (let* ((old (current-buffer))
         (before (buffer-list))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (go-start)
    (update-forced)
    (archive-state) (start-editing) (end-editing) (clear-undo-history)
    (with r (check-run (lambda () (thunk u)))
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (for (b (buffer-list))
        (when (nin? b before) (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read-only buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-flag)
  (check-group "the flag")
  (with-buffer-body '(document "abc")
    (lambda (u)
      (check-false (buffer-read-only? u))
      (buffer-set-read-only u #t)
      (check-true (buffer-read-only? u))
      (buffer-set-read-only u #f)
      (check-false (buffer-read-only? u))
      ;; (a buffer which does not exist)
      (check-false (buffer-read-only?
                     (string->url "tmfs://aux/read-only-test-none"))))))

(define (test-control)
  (check-group "not read-only")
  (with-buffer-body '(document "abc")
    (lambda (u)
      (edit (go-end) (insert "d"))
      (check= (body) '(document "abcd"))
      (check-true (buffer-modified? u))
      (edit (undo 0))
      (check= (body) '(document "abc")))))

(define (test-edits)
  (check-group "edits cancelled")
  (with-buffer-body '(document "abc" "def")
    (lambda (u)
      (buffer-set-read-only u #t)
      ;; a key or a menu command which inserts
      (edit (go-end) (insert "x"))
      (check= (body) '(document "abc" "def"))
      (check-false (buffer-modified? u))
      ;; the cursor moves all the same
      (check= (cursor) '(1 3))
      (edit (go-start))
      (check= (cursor) '(0 0))
      ;; removals and structure
      (edit (go-end) (kbd-backspace) (kbd-backspace))
      (check= (body) '(document "abc" "def"))
      (edit (go-start) (insert-return))
      (check= (body) '(document "abc" "def"))
      (edit (go-start) (make 'frac) (insert "y"))
      (check= (body) '(document "abc" "def"))
      (check-false (buffer-modified? u))
      ;; selections
      (edit (select-all))
      (check-true (selection-active-any?))
      (edit (clipboard-cut "read-only-test"))
      (check= (body) '(document "abc" "def"))
      (check-false (buffer-modified? u))
      ;; the change by the program stays, also after the next event
      (tree-insert! (buffer-get-body u) 2 '("ghi"))
      (check= (body) '(document "abc" "def" "ghi"))
      (edit (go-end))
      (check= (body) '(document "abc" "def" "ghi"))
      (edit (go-end) (insert "z"))
      (check= (body) '(document "abc" "def" "ghi"))
      (tree-assign (tree-ref (buffer-get-body u) 0) "ABC")
      (edit (go-start) (insert "w"))
      (check= (body) '(document "ABC" "def" "ghi"))
      ;; read-only off again: the edits stay
      (buffer-set-read-only u #f)
      (edit (go-end) (insert "z"))
      (check= (body) '(document "ABC" "def" "ghiz")))))

(define (test-undo)
  (check-group "undo and redo")
  (with-buffer-body '(document "abc")
    (lambda (u)
      (edit (go-end) (insert "d"))
      (edit (go-end) (insert-return) (insert "e"))
      (edit (undo 0))
      (check= (body) '(document "abcd"))
      (buffer-set-read-only u #t)
      (check-true (> (undo-possibilities) 0))
      (check-true (> (redo-possibilities) 0))
      (edit (undo 0))
      (check= (body) '(document "abcd"))
      (edit (redo 0))
      (check= (body) '(document "abcd"))
      (buffer-set-read-only u #f)
      (edit (redo 0))
      (check= (body) '(document "abcd" "e"))
      (edit (undo 0))
      (check= (body) '(document "abcd")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The consoles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define console-all (string->url "tmfs://aux/console-error-messages-all"))

(define (console-lines u)
  ;; the messages shown by the console @u, as strings
  (let* ((b (tree->stree (buffer-get-body u)))
         (w (and (pair? b) (pair? (cdr b)) (cadr b)))
         (d (and (pair? w) (cAr w))))
    (if (and (pair? d) (== (car d) 'document))
        (map (lambda (x) (if (string? x) x (object->string x))) (cdr d))
        '())))

(define (console-has? u s)
  (with l (console-lines u)
    (list-or (map (lambda (x) (string-contains? x s)) l))))

(define (string-contains? s what)
  (>= (string-search-forwards what 0 s) 0))

(define (console-window u)
  (with l (alt-window-search u)
    (and (pair? l) (car l))))

(define (test-console)
  (check-group "console")
  (let* ((old (current-buffer))
         (before (buffer-list)))
    (debug-message "std-warning" "read-only-test: first\n")
    (open-error-messages)
    (check-true (buffer-exists? console-all))
    (check-true (buffer-read-only? console-all))
    (check-false (buffer-modified? console-all))
    (check-true (console-has? console-all "read-only-test: first"))
    ;; the document stays the current buffer
    (check= (current-buffer) old)
    ;; a new message is added to the same buffer
    (let* ((n (length (buffer-list)))
           (t (buffer-get-body console-all)))
      (debug-message "std-warning" "read-only-test: second\n")
      (check-true (console-has? console-all "read-only-test: second"))
      (check-true (console-has? console-all "read-only-test: first"))
      (check= (length (buffer-list)) n)
      (check-true (== (buffer-get-body console-all) t))
      (check-false (buffer-modified? console-all)))
    ;; at most "console size" messages
    (let* ((size (or (string->number (get-preference "console size")) 100)))
      (for (i (.. 0 (+ size 10)))
        (debug-message "std-warning"
                       (string-append "read-only-test: many "
                                      (number->string i) "\n")))
      (check-true (<= (length (console-lines console-all)) size))
      (check-false (console-has? console-all "read-only-test: first"))
      (check-true (console-has? console-all
                                (string-append "read-only-test: many "
                                               (number->string (+ size 9))))))
    ;; an edit in the console is cancelled
    (with-buffer console-all
      (let* ((s (tree->stree (buffer-get-body console-all))))
        (edit (go-end) (insert "typed"))
        (check-true (== (tree->stree (buffer-get-body console-all)) s))
        (check-false (string-contains?
                       (object->string
                         (tree->stree (buffer-get-body console-all)))
                       "typed"))
        (check-false (buffer-modified? console-all))))
    ;; closing the window closes its buffers (the close button of the
    ;; window also calls its quit command, which forgets the console)
    (let* ((consoles (list-filter (buffer-list)
                                  (lambda (b)
                                    (string-starts? (url->unix b)
                                                    "tmfs://aux/console-"))))
           (w (console-window console-all)))
      (check-true (>= (length consoles) 2))
      (check-true (and w #t))
      (when w (alt-window-delete w))
      (ahash-remove! console-active? "Error messages")
      (check= (list-filter consoles buffer-exists?) '()))
    (for (b (buffer-list))
      (when (nin? b before) (buffer-close b)))
    (when (buffer-exists? old) (switch-to-buffer old))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-group thunk)
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (read-only-test-failures)
  (:synopsis "Run the tests of read-only buffers and return the failures")
  (check-suite "read-only")
  (run-group test-flag)
  (run-group test-control)
  (run-group test-edits)
  (run-group test-undo)
  (run-group test-console)
  (check-end))
