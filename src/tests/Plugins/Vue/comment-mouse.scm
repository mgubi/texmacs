;; A window closed by a click in its own editor (as an action of the
;; document would): the mouse handler of the editor of a comment closes it
;; while the click is handled. TeXmacs crashed in handle_mouse (see
;; comment-close for the key). Its buffer is closed by the time the click
;; is done (#566), the main window goes on.
;;   sh tests/Plugins/Vue/run.sh comment-mouse 50
(use-modules (check gui-driver) (tools comment comment-widgets))

(define (comment-buffers)
  (list-filter (map url->unix (buffer-list))
               (cut string-starts? <> "tmfs://aux/edit-comment")))

(define close-on-click? #f)

(tm-define (mouse-event key x y mods time data)
  (former key x y mods time data)
  (when (and close-on-click? (== key "release-left")
             (== (url->unix (current-buffer)) "tmfs://aux/edit-comment"))
    (set! close-on-click? #f)
    (with l (tree-search (buffer-tree)
                         (lambda (t) (tree-is? t 'carbon-comment)))
      (with t (car l)
        (tree-go-to t (- (tree-arity t) 1) :end)))
    (kbd-control-return)
    (gt-log "closed in the handler: " (null? (comment-buffers)))))

(gui-test-run
 (list (gt-wait 4000)
       (gt-eval (lambda () (insert "text ") (make-folded-comment "comment")))
       (gt-window "Comment editor")
       (gt-raw "focus")
       (gt-idle)
       (gt-check "the editor of the comment is open"
                 (lambda () (== (length (comment-buffers)) 1)))
       (gt-eval (lambda () (set! close-on-click? #t)))
       (gt-raw "click 200 100")
       (gt-wait 1500)
       (gt-check "the click was handled" (lambda () (not close-on-click?)))
       (gt-check "closed by the click, its buffer too"
                 (lambda () (null? (comment-buffers))))
       (gt-window "main")
       (gt-eval (lambda () (insert "after")))
       (gt-idle)
       (gt-check "the main window goes on"
                 (lambda () (string-contains?
                                 (object->string (tree->stree (buffer-tree)))
                                 "after")))
       (gt-eval (lambda () (buffer-pretend-saved (current-buffer))))
       (gt-exit)))
