;; A window closed by a key typed in ANOTHER window: a key of the main
;; window deletes the window of the editor of a comment (alt-window-delete)
;; while the main window is laid out. In single-window mode both are
;; virtual windows of one host, and the loop over them in process_layout
;; read the freed window (a use after free, which shows under ASan; see
;; comment-close for a key in the window itself). Its buffer is closed by
;; the time the key is done (#566), the main window goes on.
;;   TEXMACS_VUE_SINGLE_WINDOW=1 sh tests/Plugins/Vue/run.sh comment-close-other 50
(use-modules (check gui-driver) (tools comment comment-widgets))

(define (comment-buffers)
  (list-filter (map url->unix (buffer-list))
               (cut string-starts? <> "tmfs://aux/edit-comment")))

(tm-define (test-close-comment-window)
  (with l (alt-window-search (string->url "tmfs://aux/edit-comment"))
    (when (pair? l) (alt-window-delete (car l)))
    (gt-log "closed in the key: " (null? (comment-buffers)))))

(kbd-map ("C-F7" (test-close-comment-window)))

(gui-test-run
 (list (gt-wait 4000)
       (gt-eval (lambda () (insert "text ") (make-folded-comment "comment")))
       (gt-window "Comment editor")
       (gt-idle)
       (gt-check "the editor of the comment is open"
                 (lambda () (== (length (comment-buffers)) 1)))
       (gt-window "main")
       (gt-raw "focus")
       (gt-idle)
       (gt-raw "key C-F7")
       (gt-wait 1500)
       (gt-check "closed by a key of the main window, its buffer too"
                 (lambda () (null? (comment-buffers))))
       (gt-eval (lambda () (insert "after")))
       (gt-idle)
       (gt-check "the main window goes on"
                 (lambda () (string-contains?
                                 (object->string (tree->stree (buffer-tree)))
                                 "after")))
       (gt-eval (lambda () (buffer-pretend-saved (current-buffer))))
       (gt-exit)))
