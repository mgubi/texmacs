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

;; A buffer which has a view can be renamed to a relative name, and to a
;; name such as "/dir/doc.tm" (relative on Windows): its views still belong
;; to it and edit it. On Windows, the name in the url of a view decoded a
;; relative name as an absolute one, so that the views of the renamed
;; buffer were lost and the rename overflowed the stack (#432: decode_url
;; of new_view.cpp).
(define (test-rename-relative)
  (check-group "renaming to a relative name")
  (for (name (list (url-append "tm-editing-test-relative" "doc.tm")
                   (system->url "/tm-editing-test-relative/doc.tm")))
    (with-buffer-body '(document "a")
      (lambda ()
        (let* ((u (current-buffer))
               (n (length (buffer->views u))))
          (check-true (> n 0))
          (buffer-rename u name)
          (check-true (buffer-exists? name))
          (check= (current-buffer) name)
          (check= (length (buffer->views name)) n)
          (for (v (buffer->views name))
            (check= (view->buffer v) name))
          (edit (go-end) (insert "b"))
          (check= (tree->stree (buffer-get-body name)) '(document "ab"))
          (with v (view-new name)
            (check= (view->buffer v) name)
            (check= (length (buffer->views name)) (+ n 1))
            (view-delete v))
          (check= (length (buffer->views name)) n)
          (buffer-pretend-saved name))))))

;; Deleting a view of a buffer removes it from the views of the buffer and
;; leaves the others, which still show and edit the buffer. delete_view
;; went on reading the views after removing one, one past the end of the
;; shortened array when the view was not the last one (#547: new_view.cpp);
;; the read is caught only under ASan (with NO_FAST_ALLOC).
(define (test-delete-view)
  (check-group "deleting a view")
  (with-buffer-body '(document "a")
    (lambda ()
      (let* ((u (current-buffer))
             (v0 (current-view-url))
             (v1 (view-new u))
             (v2 (view-new u)))
        (check= (buffer->views u) (list v0 v1 v2))
        ;; the window shows v1, so that the first view is not in a window
        (window-set-view (current-window) v1 #t)
        (check= (current-view-url) v1)
        (check-true (url-none? (view->window-url v0)))
        (view-delete v0)
        (check= (buffer->views u) (list v1 v2))
        (check-false (in? v0 (view-list)))
        (check= (view->buffer v1) u)
        (check= (view->buffer v2) u)
        ;; both views edit the buffer
        (edit (go-end) (insert "b"))
        (check= (body) '(document "ab"))
        (window-set-view (current-window) v2 #t)
        (check= (current-view-url) v2)
        (check= (current-buffer) u)
        (check= (body) '(document "ab"))
        (edit (go-end) (insert "c"))
        (check= (tree->stree (buffer-get-body u)) '(document "abc"))
        (check= (buffer->views u) (list v1 v2))
        ;; deleting the current view, which the window shows: the window
        ;; shows another view of the buffer, which becomes the current one;
        ;; before, both kept the deleted view (#559: delete_view of new_view.cpp)
        (with win (current-window)
          (view-delete v2)
          (check= (buffer->views u) (list v1))
          (check= (current-view-url) v1)
          (check= (window->view win) v1)
          (check= (view->window-url v1) win)
          (check= (current-window) win))
        (edit (go-end) (insert "d"))
        (check= (body) '(document "abcd"))
        ;; also with a view which was never in a window
        (with v3 (view-new u)
          (view-delete v1)
          (check= (buffer->views u) (list v3))
          (check= (current-view-url) v3)
          (check= (window->view (current-window)) v3))
        (edit (go-end) (insert "e"))
        (check= (body) '(document "abcde")))))
  ;; the views of a renamed buffer keep their numbers, and a new view does
  ;; not take one of them, nor the number of a view deleted after the
  ;; rename (#560: two views had the same url, and Clone window after Save
  ;; as aborted)
  (with-buffer-body '(document "a")
    (lambda ()
      (let* ((u (current-buffer))
             (new-name (tmp-file "renamed-views.tm")))
        (view-new u)
        (buffer-rename u new-name)
        (let ((gone (car (list-filter (buffer->views new-name)
                                      (lambda (v)
                                        (url-none? (view->window-url v)))))))
          (view-delete gone)
          (let* ((vs (buffer->views new-name))
                 (v2 (view-new new-name))
                 (vs* (buffer->views new-name)))
            (check= (length vs*) (+ (length vs) 1))
            (check-false (in? v2 vs))
            (check-false (== v2 gone))
            (check= (view->buffer v2) new-name)
            (view-delete v2)
            (check= (buffer->views new-name) vs)))))))

;; A tree pointer or a position which is collected while it is still
;; attached to a tree (a process whose callback was never called, a
;; position-new without position-delete after an error) is held by the
;; list of the observers of the tree alone; when the tree is detached, by
;; replacing the body of its buffer or closing it, the observer moves to
;; the closest tree, and it was freed while it did so (#537: set_tree of
;; tree_pointer.cpp and tree_addendum.cpp, set_position of
;; tree_position.cpp). A use of freed memory does not always crash: on the
;; Guile/Qt build of macOS these checks aborted without the fix, but they
;; are reliable only under ASan (with NO_FAST_ALLOC).
(define (attach-forgotten-pointer)
  ;; (in a function of its own, so that nothing on the stack keeps it)
  (tree->tree-pointer (tree-ref (buffer-tree) 0))
  #t)

(define (attach-forgotten-position)
  (position-new-path (append (buffer-path) '(0 0 1)))
  #t)

(define (forgotten-observers-collected)
  (do ((i 0 (+ i 1))) ((= i 20000)) (make-vector 50 i))
  (gc)
  #t)

(define (test-tree-pointers)
  (check-group "tree pointers")
  ;; replacing the body
  (with-buffer-body '(document (concat "a " (em "b")) "c")
    (lambda ()
      (attach-forgotten-pointer)
      (forgotten-observers-collected)
      (buffer-set-body (current-buffer) (stree->tree '(document "d")))
      (check= (tree->stree (buffer-tree)) '(document "d"))))
  ;; closing the buffer (with-buffer-body closes it)
  (with-buffer-body '(document (concat "e " (em "f")))
    (lambda ()
      (attach-forgotten-pointer)
      (forgotten-observers-collected)
      (check= (tree->stree (buffer-tree)) '(document (concat "e " (em "f"))))))
  (forgotten-observers-collected)
  ;; a position, the body replaced twice, then the buffer closed
  (with-buffer-body '(document (concat "ab " (em "b")) "c")
    (lambda ()
      (attach-forgotten-position)
      (forgotten-observers-collected)
      (buffer-set-body (current-buffer) (stree->tree '(document "d")))
      (forgotten-observers-collected)
      (buffer-set-body (current-buffer) (stree->tree '(document "e" "f")))
      (check= (tree->stree (buffer-tree)) '(document "e" "f"))))
  (forgotten-observers-collected)
  ;; a pointer which is detached is still fine
  (with-buffer-body '(document "g" "h")
    (lambda ()
      (with tp (tree->tree-pointer (tree-ref (buffer-tree) 1))
        (check= (tree->stree (tree-pointer->tree tp)) "h")
        (tree-pointer-detach tp)))))

;; The buffer of an embedded TeXmacs widget (texmacs-input) is embedded in an
;; alternative window when the widget is made while that window is being
;; built, between alt-window-handle and alt-window-create, as the dialogues
;; do: alt-window-search then finds the window to close. A widget made at
;; another time, as those of the side tools of a main window, is in no
;; alternative window, whatever windows are open. (The widgets are not put
;; into the windows here: the window of an embedded widget is only freed in
;; the event loop.)
(define (test-embedded)
  (check-group "embedded buffers")
  (let* ((old (current-buffer))
         (w1 (alt-window-handle))
         (wid1 (make-menu-widget '(vertical (text "x")) 0))
         (u1 (string->url "tmfs://aux/editing-test-embedded-1"))
         (u2 (string->url "tmfs://aux/editing-test-embedded-2")))
    (alt-window-create-plain w1 wid1 "editing-test-1")
    (with in1 (widget-texmacs-input '(document "a") '(style "generic") u1)
      (check-true (buffer-exists? u1))
      (check-false (buffer-embedded? u1))
      (check= (alt-window-search u1) '())
      (let* ((w2 (alt-window-handle))
             (in2 (widget-texmacs-input '(document "b") '(style "generic") u2))
             (wid2 (make-menu-widget '(vertical (text "y")) 0)))
        ;; (the window does not exist yet)
        (check-false (buffer-embedded? u2))
        (alt-window-create-plain w2 wid2 "editing-test-2")
        (check-true (buffer-embedded? u2))
        (check= (alt-window-search u2) (list w2))
        (check-false (buffer-embedded? u1))
        (alt-windows-delete (alt-window-search u2))
        (check-false (buffer-embedded? u2))
        (check= (alt-window-search u2) '())))
    (alt-window-delete w1)
    (buffer-close u1)
    (buffer-close u2)
    (check-false (buffer-exists? u1))
    (check-false (buffer-exists? u2))
    (when (buffer-exists? old) (switch-to-buffer old))
    (check= (current-buffer) old)))

;; Closing an embedded TeXmacs widget (the console, a dialog with a
;; texmacs-input) deletes its buffer and so its view, which is shown in the
;; window of the widget. That window is not in the window table: deleting
;; the view tried to detach it from that window when no other view was free,
;; and aborted with "widget should be attached" when its widget was out of
;; any window, as in the Cocoa port when the dialog closes (#619). Here the
;; widget is put in no window, and the free views are deleted first.
(define (test-embedded-close)
  (check-group "closing an embedded buffer")
  (let* ((old (current-buffer))
         (u (string->url "tmfs://aux/editing-test-embedded-close")))
    (widget-texmacs-input '(document "a") '(style "generic") u)
    (check-true (buffer-exists? u))
    (for (v (view-list))
      (when (url-none? (view->window-url v)) (view-delete v)))
    (check-false (list-find (view-list)
                            (lambda (v) (url-none? (view->window-url v)))))
    (buffer-close u)
    (check-false (buffer-exists? u))
    (check= (current-buffer) old)
    (check= (view->buffer (window->view (current-window))) old)))

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
    (when (buffer-exists? old) (switch-to-buffer old)))
  ;; a long document: loading it does not go through all its subtrees (the
  ;; test for an encrypted document, in load-buffer-open, overflowed the
  ;; Guile stack from a few thousand paragraphs on, #418)
  (let ((f (tmp-file* "load-long.tm"))
        (old (current-buffer))
        (n 20000))
    (string-save
     (apply string-append
            "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
            (append
             (map (lambda (i)
                    (string-append "  Paragraph " (number->string i)
                                   " with <hlink|a link|https://example.org/"
                                   (number->string i) ">.\n\n"))
                  (iota n))
             (list "</body>\n")))
     (url->system f))
    (load-buffer f)
    (check= (current-buffer) f)
    (check= (tree-arity (buffer-get-body f)) n)
    (check= (tree->stree (tree-ref (buffer-get-body f) (- n 1)))
            `(concat ,(string-append "Paragraph " (number->string (- n 1))
                                     " with ")
                     (hlink "a link" ,(string-append "https://example.org/"
                                                     (number->string (- n 1))))
                     "."))
    (check-false (buffer-modified? f))
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

;; Clicks and a drag (mouse-any, as the events of the window) at the left
;; or at the right of the lines. Outside paper mode, the lines of a
;; document are merged by chunks (chunk_lines in Typeset/Bridge/bridge.cpp)
;; when it has 512 paragraphs or more, or when a line carries a footnote, a
;; float or several columns; each chunk is a group box with the path of the
;; whole document, and a point out of its extents put the cursor at the
;; start or at the end of the document (#584, #585). The cursor position in
;; pixels needs apply-changes (see the head of the file). The events need
;; increasing times: mouse_any drops those older than the last one.
(define mouse-time 1000.0)

(define (mouse key x y mods)
  (set! mouse-time (+ mouse-time 100.0))
  (mouse-any key x y mods mouse-time '()))

(define (mouse-point-at . p)
  ;; the position on the screen of the cursor at @p
  (edit (go-to (apply at p)))
  (apply-changes)
  (list (get-cursor-x) (get-cursor-y)))

(define (mouse-update)
  (update-forced)
  (apply-changes))

(define (mouse-click x y)
  (mouse "press-left" x y 1)
  (mouse "release-left" x y 0)
  (mouse-update)
  (cursor))

(define (mouse-drag x0 y0 x y)
  ;; the selection made by a drag from (@x0, @y0) to (@x, @y)
  (mouse "press-left" x0 y0 1)
  (mouse "start-drag-left" x0 y0 1)
  (mouse "dragging-left" x y 1)
  (mouse "end-drag-left" x y 0)
  (mouse-update)
  (list (rel (selection-get-start)) (rel (selection-get-end))))

;; A cursor path which is not in the document (go-to from a script with a
;; wrong path: below a string, a negative or out of range index, more
;; children than a tag has) used to be kept as the cursor: the report of
;; the error raised when the changes were applied shifted it and looped
;; (#592), and the next insert crashed TeXmacs (#599). go-to now corrects
;; such a path to the closest cursor inside the document. The insert is
;; only done when the cursor is in the document, and the buffer is closed
;; with a sane cursor, so that a regression fails these checks instead of
;; crashing the whole suite.
(define (path-in-stree? t p)
  ;; whether @p is a cursor position inside the stree @t (as is_inside of
  ;; tree_cursor.cpp, without the Cork characters)
  (cond ((null? p) #f)
        ((string? t)
         (and (null? (cdr p)) (>= (car p) 0) (<= (car p) (string-length t))))
        ((null? (cdr p)) (in? (car p) '(0 1)))
        (else (and (>= (car p) 0) (< (car p) (length (cdr t)))
                   (path-in-stree? (list-ref (cdr t) (car p)) (cdr p))))))

(define (check-bad-go-to doc bad expected after)
  ;; go to the path @bad of @doc, which is not in it: the cursor goes to
  ;; @expected, and an insert there gives the body @after
  (with-buffer-body doc
    (lambda ()
      (edit (go-to (apply at bad)))
      (check= (cursor) expected)
      (check= (rel (cursor-path*)) expected)
      (if (not (path-in-stree? (body) (cursor)))
          (begin
            (check-report #f "insert after (go-to bad path)"
                          "the cursor is not in the document")
            ;; closing the buffer with that cursor crashes as well
            (edit (go-start)))
          (begin
            (edit (insert "X"))
            (check= (body) after)
            (check-true (path-in-stree? (body) (cursor))))))))

(define (test-bad-cursor)
  (check-group "a cursor path outside the document")
  (let* ((table '(document (tabular (table (row (cell "a") (cell "b"))))))
         (table-x (lambda (a b)
                    `(document (tabular (table (row (cell ,a) (cell ,b))))))))
    ;; below a string
    (check-bad-go-to '(document "abc") '(0 0 0) '(0 0) '(document "Xabc"))
    (check-bad-go-to '(document "abc") '(0 2 5 1) '(0 2) '(document "abXc"))
    ;; below the string of a table cell (a bus error)
    (check-bad-go-to table '(0 0 0 1 0 0 0) '(0 0 0 1 0 0)
                     (table-x "a" "Xb"))
    ;; a negative index
    (check-bad-go-to table '(0 0 -1 0) '(0 0 0 0 0 0) (table-x "Xa" "b"))
    (check-bad-go-to '(document "abc") '(0 -2) '(0 0) '(document "Xabc"))
    ;; out of range: a paragraph, a position in a string
    (check-bad-go-to '(document "abc") '(5 0) '(0 3) '(document "abcX"))
    (check-bad-go-to '(document "abc") '(0 9) '(0 3) '(document "abcX"))
    ;; more children than the tag has (a row of a one-row table)
    (check-bad-go-to table '(0 0 7 0 0) '(0 0 0 1 0 1) (table-x "a" "bX"))
    ;; the path of the buffer itself, the node of the document
    (check-bad-go-to '(document "abc") '() '(0 0) '(document "Xabc")))
  ;; the empty path is not in the buffer: go-to ignores it
  (with-buffer-body '(document "abc")
    (lambda ()
      (edit (go-to (at 0 2)))
      (edit (go-to '()))
      (check= (cursor) '(0 2))
      (edit (insert "X"))
      (check= (body) '(document "abXc"))))
  ;; a path in the document is kept
  (with-buffer-body '(document "abc" (frac "x" "y"))
    (lambda ()
      (edit (go-to (at 0 2)))
      (check= (cursor) '(0 2))
      (edit (go-to (at 1 1 1)))
      (check= (cursor) '(1 1 1))
      (edit (insert "X"))
      (check= (body) '(document "abc" (frac "x" "yX"))))))

(define (test-mouse-long)
  (check-group "mouse in a long document")
  (with-buffer-body
      `(document ,@(map (lambda (i) (string-append "line " (number->string i)))
                        (.. 0 600)))
    (lambda ()
      (edit (init-env "page-medium" "papyrus"))
      (with (x y) (mouse-point-at 300 8)
        (check= (mouse-click x y) '(300 8))
        ;; at the right of the line: its end, not the end of the document
        (check= (mouse-click (+ x 300000) y) '(300 8))
        ;; a drag upwards to the left of line 290 (#585): from its start,
        ;; not from the start of the document
        (with (x1 y1) (mouse-point-at 290 0)
          (check= (mouse-drag x y (- x1 20000) y1) '((290 0) (300 8)))))
      (with (x y) (mouse-point-at 300 0)
        ;; at its left: its start, not the start of the document
        (check= (mouse-click (- x 20000) y) '(300 0)))))
  ;; a footnote makes chunks in a short document too
  (with-buffer-body
      `(document ,@(map (lambda (i)
                          (if (== i 5) '(concat "f" (footnote "a note"))
                              (string-append "line " (number->string i))))
                        (.. 0 40)))
    (lambda ()
      (edit (init-env "page-medium" "papyrus"))
      (with (x y) (mouse-point-at 20 0)
        (check= (mouse-click x y) '(20 0)))
      (with (x y) (mouse-point-at 1 0)
        (check= (mouse-click (+ x 150000) y) '(1 6))))))

;; Several paragraphs inserted at the border of a tag which has no cursor
;; position there (an inner border, as hidden): the editor corrected the
;; cursor into the tag while it made the new paragraph, then refused, and
;; the paragraphs were lost; Return did nothing either (#675)
(define (test-insert-paragraphs)
  (check-group "insert paragraphs")
  (let* ((by '(document "P" "Q"))
         (check-at
          (lambda (how doc p res)
            (with-buffer-body doc
              (lambda ()
                (edit (go-to (apply at p)))
                (edit (how))
                (check= (body) res)
                ;; (nothing changed: nothing to undo)
                (when (!= res doc)
                  (edit (undo 0))
                  (check= (body) doc)))))))
    (for-each
      (lambda (how)
        (for-each
          (lambda (tag)
            (check-at how `(document "u" ,tag "v") '(1 0)
                      `(document "u" "P" (concat "Q" ,tag) "v"))
            (check-at how `(document "u" ,tag "v") '(1 1)
                      `(document "u" (concat ,tag "P") "Q" "v"))
            (check-at how `(document (concat ,tag "z")) '(0 0 0)
                      `(document "P" (concat "Q" ,tag "z")))
            (check-at how `(document (concat "z" ,tag)) '(0 1 1)
                      `(document (concat "z" ,tag "P") "Q")))
          '((hidden "b") (freeze "b") (hidden (document "b" "c")) (strong "b")))
        ;; no place for paragraphs: nothing changes
        (for-each
          (lambda (x)
            (check-at how `(document ,(car x)) (cdr x) `(document ,(car x))))
          '(((concat "t " (strong "ab")) 0 1 0 1)
            ((concat "t " (math "ab")) 0 1 0 1)
            ((tabular (tformat (table (row (cell "ab")))))
             0 0 0 0 0 0 1))))
      (list (lambda () (insert by))
            (lambda ()
              (clipboard-set "editing-test" (stree->tree by))
              (clipboard-paste "editing-test"))))
    (clipboard-clear "editing-test")
    ;; a block goes before or after the tag, not into it
    (let* ((block '(quotation (document "P" "Q"))))
      (for-each
        (lambda (tag)
          (check-at (lambda () (insert block))
                    `(document "u" ,tag "v") '(1 0)
                    `(document "u" ,block ,tag "v"))
          (check-at (lambda () (insert block))
                    `(document "u" ,tag "v") '(1 1)
                    `(document "u" ,tag ,block "v")))
        '((hidden "b") (hidden (document "b" "c")) (strong "b"))))
    ;; Return
    (for-each
      (lambda (tag)
        (check-at kbd-return `(document "u" ,tag "v") '(1 0)
                  `(document "u" "" ,tag "v"))
        (check-at kbd-return `(document "u" ,tag "v") '(1 1)
                  `(document "u" ,tag "" "v")))
      '((hidden "b") (hidden (document "b" "c")) (strong "b")))
    ;; a refused insertion does not remove the selection
    (with-buffer-body '(document (concat "t " (strong "abc")))
      (lambda ()
        (edit (selection-set (at 0 1 0 1) (at 0 1 0 2)))
        (edit (insert by))
        (check= (body) '(document (concat "t " (strong "abc"))))
        (check-true (selection-active-any?))
        (edit (selection-cancel))))
    ;; where it is possible, a selection is replaced
    (with-buffer-body '(document "abc")
      (lambda ()
        (edit (selection-set (at 0 1) (at 0 2)))
        (edit (insert by))
        (check= (body) '(document "aP" "Qc"))))
    ;; a selection cut up to a tag with an inner border (it starts right
    ;; after the tag, or ends right before it): the cut leaves the cursor in
    ;; the tag, the paragraphs go at the border of the tag (#711)
    (let* ((block '(quotation (document "P" "Q")))
           (try (lambda (how doc a b res)
                  (with-buffer-body doc
                    (lambda ()
                      (clipboard-set "editing-test" (stree->tree by))
                      (edit (selection-set (apply at a) (apply at b)))
                      (edit (how))
                      (check= (body) res)
                      (edit (undo 0))
                      (check= (body) doc))))))
      (for-each
        (lambda (tag)
          (for-each
            (lambda (how)
              (try how `(document (concat "z" ,tag "abc")) '(0 2 0) '(0 2 3)
                   `(document (concat "z" ,tag "P") "Q"))
              (try how `(document (concat "z" ,tag "abc")) '(0 2 0) '(0 2 2)
                   `(document (concat "z" ,tag "P") "Qc"))
              (try how `(document (concat "abc" ,tag "z")) '(0 0 0) '(0 0 3)
                   `(document "P" (concat "Q" ,tag "z")))
              (try how `(document (concat "z" ,tag "abc")) '(0 0 0) '(0 0 1)
                   `(document "P" (concat "Q" ,tag "abc"))))
            (list (lambda () (insert by))
                  (lambda () (clipboard-paste "editing-test"))))
          (try (lambda () (insert block))
               `(document (concat "z" ,tag "abc")) '(0 2 0) '(0 2 3)
               `(document (concat "z" ,tag) ,block))
          (try (lambda () (insert block))
               `(document (concat "abc" ,tag "z")) '(0 0 0) '(0 0 3)
               `(document ,block (concat ,tag "z"))))
        '((hidden "b") (freeze "b") (unfreeze "b") (ignore "b") (shown "b")
          (table (row (cell "b"))) (expand* "foo" "b"))))
    ;; a selection of cells: the cut empties the cells, which then have no
    ;; place for paragraphs; nothing changes, and the selection is kept
    (let* ((cells '(document (tabular (tformat (table (row
                      (cell (document "ef" "gh")) (cell (strong "ij"))))))))
           (a '(0 0 0 0 0 0 0 0)) (b '(0 0 0 0 1 0 0 2)))
      (for-each
        (lambda (how)
          (with-buffer-body cells
            (lambda ()
              (buffer-pretend-saved (current-buffer))
              (clipboard-set "editing-test" (stree->tree by))
              (let ((n (undo-possibilities)))
                (edit (selection-set (apply at a) (apply at b)))
                (edit (how))
                (check= (body) cells)
                (check-true (selection-active-any?))
                ;; (no undo entry, and not modified)
                (check= (undo-possibilities) n)
                (check-false (buffer-modified? (current-buffer))))
              (edit (selection-cancel)))))
        (list (lambda () (insert by))
              (lambda () (insert '(quotation (document "P" "Q"))))
              (lambda () (clipboard-paste "editing-test"))))
      ;; text still replaces the cells
      (with-buffer-body cells
        (lambda ()
          (edit (selection-set (apply at a) (apply at b)))
          (edit (insert "xx"))
          (check= (body) '(document (tabular (tformat (table (row
                                       (cell "xx"))))))))))
    ;; the refusal cancels the cut only: changes made earlier in the same
    ;; editing step, here and in another buffer, stay and can be undone;
    ;; the redo list is kept
    (let* ((doc '(document "hello" (tabular (tformat (table (row
                   (cell (document "ef" "gh")) (cell (strong "ij"))))))))
           (typed (cons 'document (cons "helloXY" (cddr doc))))
           (ca '(1 0 0 0 0 0 0 0)) (cb '(1 0 0 0 1 0 0 2)))
      (for-each
        (lambda (how)
          (with-buffer-body doc
            (lambda ()
              (clipboard-set "editing-test" (stree->tree by))
              (edit (go-to (at 0 5))
                    (insert "XY")
                    (selection-set (apply at ca) (apply at cb))
                    (how))
              (check= (body) typed)
              (check-true (selection-active-table?))
              (edit (selection-cancel))
              (edit (undo 0))
              (check= (body) doc))))
        (list (lambda () (insert by))
              (lambda () (insert '(quotation (document "P" "Q"))))
              (lambda () (clipboard-paste "editing-test"))))
      (with-buffer-body doc
        (lambda ()
          (let* ((a (current-buffer)) (b (new-buffer)))
            (switch-to-buffer* b)
            (buffer-set-body b (stree->tree '(document "bee")))
            (go-start)
            (update-forced)
            (switch-to-buffer* a)
            (update-forced)
            (clipboard-set "editing-test" (stree->tree by))
            (edit (switch-to-buffer* b)
                  (go-to (append (buffer-path) '(0 3)))
                  (insert "ZZ")
                  (switch-to-buffer* a)
                  (selection-set (apply at ca) (apply at cb))
                  (insert by))
            (check= (body) doc)
            (switch-to-buffer* b)
            (check= (body) '(document "beeZZ"))
            (switch-to-buffer* a)
            (edit (selection-cancel)))))
      ;; no earlier change: nothing to undo, redo kept, a saved buffer stays
      ;; saved and the selection is the same
      (with-buffer-body doc
        (lambda ()
          (clipboard-set "editing-test" (stree->tree by))
          (edit (go-to (at 0 5)) (insert "XY"))
          (edit (undo 0))
          (buffer-pretend-saved (current-buffer))
          (let ((n (undo-possibilities)) (r (redo-possibilities)))
            (edit (selection-set (apply at ca) (apply at cb)))
            (let ((s (list (selection-get-start) (selection-get-end))))
              (edit (insert by))
              (check= (body) doc)
              (check= (undo-possibilities) n)
              (check= (redo-possibilities) r)
              (check-false (buffer-modified? (current-buffer)))
              (check= (list (selection-get-start) (selection-get-end)) s))
            (edit (selection-cancel))))))
    ;; block cells (hyphenated cells, wide-tabular): the cleared first cell
    ;; holds paragraphs again, so they go in when it was a document; with a
    ;; plain first cell nothing changes
    (let* ((rows (lambda (c1) `(table (row (cell ,c1) (cell "cd"))
                                      (row (cell "ef") (cell "gh")))))
           ;; (all the cells are made documents)
           (done (lambda (c1) `(table (row (cell ,c1) (cell (document "")))
                                      (row (cell (document "ef"))
                                           (cell (document "gh"))))))
           (hy (lambda (m c1) `(document (tabular (tformat
                  (cwith "1" "-1" "1" "-1" "cell-hyphen" ,m) ,c1)))))
           (wide (lambda (c1) `(document (wide-tabular (tformat ,c1)))))
           (tried (lambda (doc a b how res)
                    (with-buffer-body doc
                      (lambda ()
                        (clipboard-set "editing-test" (stree->tree by))
                        (let ((before (body)))
                          (edit (selection-set (apply at a) (apply at b)))
                          (edit (how))
                          (check= (body) (or res before))
                          (when res
                            (edit (undo 0))
                            (check= (body) before))
                          (when (not res)
                            (check-true (selection-active-any?)))
                          (edit (selection-cancel))))))))
      (for-each
        (lambda (how)
          (for-each
            (lambda (m)
              (tried (hy m (rows '(document "ab" "a2"))) '(0 0 1 0 0 0 0 0)
                     '(0 0 1 0 1 0 1) how
                     (hy m (done '(document "P" "Q"))))
              (tried (hy m (rows "ab")) '(0 0 1 0 0 0 0) '(0 0 1 0 1 0 1)
                     how #f))
            '("t" "b" "c"))
          (tried (wide (rows '(document "ab" "a2"))) '(0 0 0 0 0 0 0 0)
                 '(0 0 0 0 1 0 1) how (wide (done '(document "P" "Q"))))
          (tried (wide (rows "ab")) '(0 0 0 0 0 0 0) '(0 0 0 0 1 0 1)
                 how #f))
        (list (lambda () (insert by))
              (lambda () (clipboard-paste "editing-test")))))
    (clipboard-clear "editing-test")
    ;; a selection which starts inside a tag and ends outside it: the cut
    ;; starts before the tag, where paragraphs can go
    ;; (setting a clipboard cancels the selection)
    (clipboard-set "editing-test" (stree->tree by))
    (for-each
      (lambda (x)
        (let* ((how (car x)) (res (cdr x)))
          (with-buffer-body '(document (concat "some " (strong "bold")
                                               " text after"))
            (lambda ()
              (edit (selection-set (at 0 1 0 1) (at 0 2 4)))
              (edit (how))
              (check= (body) (car res))))
          (with-buffer-body '(document (concat "some " (strong "bold") " t")
                                       "next")
            (lambda ()
              (edit (selection-set (at 0 1 0 1) (at 1 2)))
              (edit (how))
              (check= (body) (cadr res))))))
      (list (list (lambda () (clipboard-paste "editing-test"))
                  '(document "some P" "Qt after")
                  '(document "some P" "Qxt"))
            (list (lambda () (insert '(quotation (document "P" "Q"))))
                  '(document "some " (quotation (document "P" "Q")) "t after")
                  '(document "some " (quotation (document "P" "Q")) "xt"))))
    (clipboard-clear "editing-test")
    ;; a block after text, before a tag which holds paragraphs: not in the tag
    (let* ((block '(quotation (document "P" "Q"))))
      (for-each
        (lambda (tag)
          (check-at (lambda () (insert block))
                    `(document (concat "w" ,tag)) '(0 0 1)
                    `(document "w" ,block ,tag)))
        '((hidden (document "b" "c")) (ignore (document "b" "c"))
          (freeze (document "b" "c")) (shown (document "b" "c"))
          (hidden "b") (strong "b")))
      (for-each
        (lambda (tag)
          (check-at (lambda () (insert block))
                    `(document (concat "w" ,tag "z")) '(0 0 1)
                    `(document "w" ,block (concat ,tag "z"))))
        '((hidden (document "b" "c")) (freeze (document "b" "c")) (strong "b"))))
    ;; before the table format of a tabular, a position of scripts only:
    ;; nothing there, and no stray concat
    (check-at (lambda () (insert by))
              '(document (tabular (tformat (table (row (cell "a")))))) '(0 0 0)
              '(document (tabular (tformat (table (row (cell "a")))))))
    (check-at kbd-return
              '(document (tabular (tformat (table (row (cell "a")))))) '(0 0 0)
              '(document (tabular (tformat (table (row (cell "a")))))))
    ;; can-insert-at?
    (with-buffer-body '(document (concat "t " (strong "ab"))
                                 (tabular (tformat (table (row (cell "c"))))))
      (lambda ()
        (check-true (can-insert-at? (stree->tree by) (at 0 0 1)))
        (check-false (can-insert-at? (stree->tree by) (at 0 1 0 1)))
        (check-false (can-insert-at? (stree->tree by) (at 1 0 0 0 0 0 1)))
        (check-true (can-insert-at? (stree->tree "x") (at 0 1 0 1)))
        ;; (a path which is not in the document)
        (check-false (can-insert-at? (stree->tree by) (at 7 7)))))))

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

;; Each view has its own author, and undo takes back the last step of the
;; author of the view. After an edit in another buffer, the cursor which
;; archive-state records before start-editing still belongs to the author of
;; that buffer; the step which follows belongs to the author of the view all
;; the same. Before, it kept the owner of the cursor: undo then took back the
;; step before it (or, in a buffer whose body a script had set, everything).
(define (test-undo-authors)
  (check-group "undo after editing another buffer")
  (with-buffer-body '(document "")
    (lambda ()
      (let ((a (current-buffer)))
        (edit (insert "a"))
        ;; the body of the new buffer is set under the author of a
        (with-buffer-body '(document "x")
          (lambda ()
            (let ((b (current-buffer)))
              (edit (go-end) (insert '(strong "S")))
              (edit (undo 0))
              (check= (body) '(document "x"))
              (edit (go-end) (insert '(strong "S")))
              (with s (body)
                (switch-to-buffer* a)
                (edit (insert "a"))
                (switch-to-buffer* b)
                (edit (go-end) (insert '(em "W")))
                (check-true (!= (body) s))
                (edit (undo 0))
                (check= (body) s)
                (edit (undo 0))
                (check= (body) '(document "x"))))))))))

;; Clearing the undo history concerns the current buffer only, and a buffer
;; which was saved remains unmodified.
(define (test-undo-clear)
  (check-group "clear undo history")
  (with-buffer-body '(document "")
    (lambda ()
      (let* ((b1 (current-buffer))
             (b2 #f))
        (edit (insert "one"))
        (buffer-pretend-saved b1)
        (check= (undo-possibilities) 1)
        (set! b2 (new-buffer))
        (switch-to-buffer* b2)
        (edit (insert "two"))
        (check-true (buffer-modified? b2))
        (edit (clear-undo-history))
        (check= (undo-possibilities) 0)
        (check= (body) '(document "two"))
        ;; not saved before, not saved after
        (check-true (buffer-modified? b2))
        (check-false (buffer-modified? b1))
        ;; saved before, saved after, and modified by the next change
        (edit (insert "!"))
        (buffer-pretend-saved b2)
        (edit (clear-undo-history))
        (check-false (buffer-modified? b2))
        (edit (insert "?"))
        (check-true (buffer-modified? b2))
        (edit (undo 0))
        (check= (body) '(document "two!"))
        (check-false (buffer-modified? b2))
        ;; the history of the other buffer is still there
        (switch-to-buffer* b1)
        (check= (undo-possibilities) 1)
        (edit (undo 0))
        (check= (body) '(document ""))
        (check-true (buffer-modified? b1))
        (buffer-pretend-saved b1)))))

;; Closing a buffer empties its place in the root tree, and that change
;; belongs to no history: a cancel-editing in another buffer (as after an
;; error in a key press or a menu action) leaves that place as it is,
;; whether it is still free or already holds the body of a newer buffer,
;; here one without a view. It used to put the closed body back there.
(define (test-cancel-closed)
  (check-group "cancel after closing a buffer")
  (with-buffer-body '(document "here")
    (lambda ()
      (let* ((b (current-buffer))
             (a (new-buffer))
             (x (string->url "tmfs://aux/editing-test-cancel-closed"))
             (i #f))
        (switch-to-buffer* a)
        (buffer-set-body a (stree->tree '(document "closed")))
        (update-forced)
        (clear-undo-history)
        (set! i (car (buffer-path)))
        (switch-to-buffer* b)
        (buffer-close a)
        (check= (tree-label (tree-ref (root-tree) i)) 'uninit)
        (buffer-set-body x (stree->tree '(document "newer")))
        (with before (tree->stree (tree-ref (root-tree) i))
          (start-editing)
          (cancel-editing)
          (end-editing)
          (check= (tree->stree (tree-ref (root-tree) i)) before))
        (check= (tree->stree (buffer-get-body x)) '(document "newer"))
        (check= (body) '(document "here"))
        (buffer-close x)))))

;; The changes between mark-start and mark-end are one step of the history;
;; mark-cancel undoes those made since mark-start. A marker which is not in
;; the history, or which cannot be removed, leaves the history and the
;; document as they are.
(define (test-undo-markers)
  (check-group "undo markers")
  ;; several changes between the markers, after a save
  (with-buffer-body '(document "")
    (lambda ()
      (let ((b (current-buffer))
            (m (mark-new)))
        (edit (insert "one"))
        (buffer-pretend-saved b)
        (mark-start m)
        (edit (insert "a"))
        (edit (insert-return))
        (edit (insert "b"))
        (mark-end m)
        (check= (body) '(document "onea" "b"))
        (check= (undo-possibilities) 1)
        (check-true (buffer-modified? b))
        (edit (undo 0))
        (check= (body) '(document "one"))
        (check-false (buffer-modified? b))
        (edit (redo 0))
        (check= (body) '(document "onea" "b"))
        (check-true (buffer-modified? b))
        (edit (undo 0))
        (check-false (buffer-modified? b))
        (edit (undo 0))
        (check= (body) '(document ""))
        (check-true (buffer-modified? b))
        (buffer-pretend-saved b))))
  ;; mark-cancel
  (with-buffer-body '(document "")
    (lambda ()
      (let ((b (current-buffer))
            (m (mark-new)))
        (edit (insert "one"))
        (buffer-pretend-saved b)
        (mark-start m)
        (edit (insert "a"))
        (edit (insert-return))
        (mark-cancel m)
        (check= (body) '(document "one"))
        (check= (undo-possibilities) 1)
        (check-false (buffer-modified? b)))))
  ;; markers which are not in the history
  (for (end (list mark-end mark-cancel))
    (with-buffer-body '(document "")
      (lambda ()
        (let ((b (current-buffer)))
          (edit (insert "one"))
          (edit (insert-return))
          (edit (insert "two"))
          (buffer-pretend-saved b)
          (with n (undo-possibilities)
            (check-true (> n 0))
            (end (mark-new))
            (check= (undo-possibilities) n)
            (check= (body) '(document "one" "two"))
            (check-false (buffer-modified? b)))
          (edit (undo 0))
          (check-true (buffer-modified? b))
          (buffer-pretend-saved b)))))
  ;; a marker under a change which was undone cannot be removed
  (with-buffer-body '(document "")
    (lambda ()
      (let ((b (current-buffer))
            (m (mark-new)))
        (edit (insert "one"))
        (mark-start m)
        (edit (insert "a"))
        (edit (undo 0))
        (check= (body) '(document "one"))
        (check= (redo-possibilities) 1)
        (mark-end m)
        (check= (redo-possibilities) 1)
        (edit (redo 0))
        (check= (body) '(document "onea"))
        (edit (undo 0))
        (check= (body) '(document "one"))
        (check-true (> (undo-possibilities) 0))
        (buffer-pretend-saved b)))))

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
          ;; (none for English: the style is then "generic" alone)
          (with st (cadr (doc-part doc 'style))
            (check= (if (pair? st) (cadr st) st) "generic"))
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

;; Save as tells the window of the buffer its new url (its file in the
;; title bar, the proxy icon of macOS), also when the file keeps its name in
;; another folder and the title of the buffer does not change: the url was
;; only sent with a new title (set_title_buffer, new_buffer.cpp).
(define (test-save-as-window)
  (check-group "save as: the url of the window")
  (with-buffer-body '(document "a")
    (lambda ()
      (let* ((sub (string-append editing-dir "/sub2"))
             (f (tmp-file* "d.tm"))
             (g (tmp-file* "sub2/d.tm"))
             (h (tmp-file* "sub2/e.tm")))
        (system-mkdir (system->url sub))
        (edit (save-buffer-as f :overwrite))
        (check= (current-buffer) f)
        (check= (window->file (current-window)) f)
        (with title (buffer-get-title f)
          ;; the same file name in another folder: the same title
          (edit (save-buffer-as g :overwrite))
          (check= (current-buffer) g)
          (check= (buffer-get-title g) title)
          (check= (window->file (current-window)) g))
        ;; another file name: another title
        (edit (save-buffer-as h :overwrite))
        (check= (current-buffer) h)
        (check= (window->file (current-window)) h)
        (for-each (lambda (u) (when (url-exists? u) (system-remove u)))
                  (list g h))
        (system-rmdir (system->url sub))))))

;; The initial environment belongs to the buffer (#528): a change made in
;; one view of it (init-env, init-default) is in its other views, in
;; buffer-get and in the saved file, and in a view made after it; the
;; buffer stays modified until it is saved. The variables of the window
;; (zoom-factor, page-screen-width) stay those of each view.
(define (initial-value doc var)
  ;; the value of @var in the initial environment of @doc (a document read
  ;; from a file, as an stree), or #f
  (with init (doc-part doc 'initial)
    (and init (pair? (cdr init))
         (with a (list-find (cdadr init)
                            (lambda (x) (and (pair? x) (pair? (cdr x))
                                             (== (cadr x) var))))
           (and a (caddr a))))))

(define (buffer-init-value u var)
  (initial-value (tree->stree (buffer-get u)) var))

(define (file-init-value u var)
  (initial-value (tree->stree (tree-import u "texmacs")) var))

(define (test-views-init)
  (check-group "initial environment in the views of a buffer")
  (let ((old (current-buffer))
        (before (buffer-list))
        (f (tmp-file* "views-init.tm")))
    (tree-export (stree->tree
                  '(document (TeXmacs "2.1") (style (tuple "generic"))
                             (body (document "one"))
                             (initial (collection
                                       (associate "font-base-size" "12")))))
                 f "texmacs")
    (with r (check-run
             (lambda ()
               (load-buffer f)
               (update-forced)
               (let ((v1 (current-view-url))
                     (z1 (get-init "zoom-factor"))
                     (w1 (get-init "page-screen-width")))
                 ;; switching to the buffer shown makes a second view, and
                 ;; switching again goes back to the first one
                 (switch-to-buffer f)
                 (with v2 (current-view-url)
                   (check= (length (buffer->views f)) 2)
                   (check-false (== v1 v2))
                   ;; the variables of the window stay in their view, and
                   ;; do not modify the buffer
                   (edit (init-env "zoom-factor" "1.7"))
                   (edit (init-env "page-screen-width" "123tmpt"))
                   (check= (get-init "zoom-factor") "1.7")
                   (check-false (buffer-modified? f))
                   (switch-to-buffer f)
                   (check= (current-view-url) v1)
                   (check= (get-init "zoom-factor") z1)
                   (check= (get-init "page-screen-width") w1)
                   ;; init-env in the first view reaches the second one
                   (edit (init-env "par-indent" "4fn"))
                   (check-true (buffer-modified? f))
                   (switch-to-buffer f)
                   (check= (current-view-url) v2)
                   (check= (get-init "par-indent") "4fn")
                   (check= (buffer-init-value f "par-indent") "4fn")
                   (check-true (buffer-modified? f))
                   ;; and init-default in the second view the first one
                   (edit (init-default "font-base-size"))
                   (check= (get-init "font-base-size") "10")
                   (switch-to-buffer f)
                   (check= (current-view-url) v1)
                   (check= (get-init "font-base-size") "10")
                   (check= (get-init "par-indent") "4fn")
                   (check-false (buffer-init-value f "font-base-size"))
                   (check= (buffer-init-value f "par-indent") "4fn")
                   (check-true (buffer-modified? f))
                   ;; a view made after the changes has them
                   (with v3 (view-new f)
                     (window-set-view (current-window) v3 #t)
                     (check= (current-view-url) v3)
                     (check= (get-init "par-indent") "4fn")
                     (check= (get-init "font-base-size") "10"))
                   ;; the file saved has them, and not the variables of
                   ;; the window
                   (check-true (buffer-modified? f))
                   (buffer-save f)
                   (check-false (buffer-modified? f))
                   (check= (file-init-value f "par-indent") "4fn")
                   (check-false (file-init-value f "font-base-size"))
                   (check-false (file-init-value f "zoom-factor"))
                   (check-false (file-init-value f "page-screen-width"))
                   ;; a change made after the save, in another view, is
                   ;; saved from this one
                   (switch-to-buffer f)
                   (edit (init-env "par-indent" "2fn"))
                   (switch-to-buffer f)
                   (check= (get-init "par-indent") "2fn")
                   (check-true (buffer-modified? f))
                   (buffer-save f)
                   (check-false (buffer-modified? f))
                   (check= (file-init-value f "par-indent") "2fn")
                   ;; the source mode (preamble) is a setting of the document
                   (edit (init-env "preamble" "true"))
                   (switch-to-buffer f)
                   (check= (get-init "preamble") "true")
                   (edit (init-default "preamble"))
                   (switch-to-buffer f)
                   (check= (get-init "preamble") "false")))))
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))
    (for (b (buffer-list))
      (when (nin? b before)
        (buffer-pretend-saved b)
        (buffer-close b)))
    (when (buffer-exists? old) (switch-to-buffer old))))

;; The style belongs to the buffer too (#535): a change of the style made
;; in one view (set-style-tree, add-style-package, remove-style-package) is
;; in its other views, in buffer-get and in the saved file, and in a view
;; made after it; the buffer stays modified until it is saved.
(define (style-of doc)
  ;; the style of @doc (a document as an stree), or #f
  (with st (doc-part doc 'style)
    (and st (pair? (cdr st)) (cadr st))))

(define (test-views-style)
  (check-group "style in the views of a buffer")
  (let ((old (current-buffer))
        (before (buffer-list))
        (f (tmp-file* "views-style.tm")))
    (tree-export (stree->tree
                  '(document (TeXmacs "2.1") (style (tuple "generic"))
                             (body (document "one"))))
                 f "texmacs")
    (with r (check-run
             (lambda ()
               (load-buffer f)
               (update-forced)
               (with v1 (current-view-url)
                 (switch-to-buffer f)
                 (with v2 (current-view-url)
                   (check= (length (buffer->views f)) 2)
                   (check-false (== v1 v2))
                   ;; a style set in the second view reaches the first one
                   (edit (set-style-tree (stree->tree
                                          '(tuple "article" "number-us"))))
                   (check-true (buffer-modified? f))
                   (switch-to-buffer f)
                   (check= (current-view-url) v1)
                   (check= (tree->stree (get-style-tree))
                           '(tuple "article" "number-us"))
                   (check= (style-of (tree->stree (buffer-get f)))
                           '(tuple "article" "number-us"))
                   (check= (tree->stree (buffer-get-style f))
                           '(tuple "article" "number-us"))
                   ;; and the file saved from the first view has it
                   (buffer-save f)
                   (check-false (buffer-modified? f))
                   (check= (style-of (tree->stree (tree-import f "texmacs")))
                           '(tuple "article" "number-us"))
                   ;; a package added in the first view, after the save, is
                   ;; in the second view and saved from it
                   (edit (add-style-package "comment"))
                   (check-true (buffer-modified? f))
                   (switch-to-buffer f)
                   (check= (current-view-url) v2)
                   (check= (get-style-list)
                           '("article" "number-us" "comment"))
                   (check-true (buffer-modified? f))
                   (buffer-save f)
                   (check-false (buffer-modified? f))
                   (check= (style-of (tree->stree (tree-import f "texmacs")))
                           '(tuple "article" "number-us"
                                   "comment"))
                   ;; a package removed in the second view
                   (edit (remove-style-package "number-us"))
                   (switch-to-buffer f)
                   (check= (current-view-url) v1)
                   (check= (get-style-list)
                           '("article" "comment"))
                   ;; a view made after the changes has them
                   (with v3 (view-new f)
                     (window-set-view (current-window) v3 #t)
                     (check= (current-view-url) v3)
                     (check= (get-style-list)
                             '("article" "comment")))
                   (buffer-save f)
                   (check= (style-of (tree->stree (tree-import f "texmacs")))
                           '(tuple "article" "comment"))))))
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))
    (for (b (buffer-list))
      (when (nin? b before)
        (buffer-pretend-saved b)
        (buffer-close b)))
    (when (buffer-exists? old) (switch-to-buffer old))))

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
  (test-rename-relative)
  (test-delete-view)
  (test-tree-pointers)
  (test-embedded)
  (test-embedded-close)
  (test-load)
  (test-insert)
  (test-insert-paragraphs)
  (test-cursor)
  (test-bad-cursor)
  (test-mouse-long)
  (test-selection)
  (test-remove)
  (test-structure)
  (test-environment)
  (test-undo)
  (test-undo-save)
  (test-undo-authors)
  (test-undo-clear)
  (test-undo-markers)
  (test-cancel-closed)
  (test-save)
  (test-save-as-window)
  (test-views-init)
  (test-views-style)
  (test-export)
  (test-copy-as-text)
  (remove-tmp-files)
  (check-end))
