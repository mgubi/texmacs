;; A window closed by a key typed in its own editor: C-Return in the editor
;; of a comment (kbd-control-return -> comment-quit-command -> the quit of
;; dialogue-window -> alt-window-delete) closes the window while the key is
;; handled. TeXmacs crashed: the editor read its buffer after its view was
;; gone (end_editing), and the window was freed while it was laid out
;; (layout_window_passes). Its buffer is closed by the time the key is
;; done (#566), the main window goes on.
;;   sh tests/Plugins/Vue/run.sh comment-close 50
(use-modules (check gui-driver) (tools comment comment-widgets))

(define (comment-buffers)
  (list-filter (map url->unix (buffer-list))
               (cut string-starts? <> "tmfs://aux/edit-comment")))

;; the cursor at the end of the text of the comment
(define (go-into-comment)
  (with l (tree-search (buffer-tree)
                       (lambda (t) (tree-is? t 'carbon-comment)))
    (with t (car l)
      (tree-go-to t (- (tree-arity t) 1) :end))))

(gui-test-run
 (list (gt-wait 4000)
       (gt-eval (lambda () (insert "text ") (make-folded-comment "comment")))
       (gt-window "Comment editor")
       (gt-raw "focus")
       (gt-idle)
       (gt-check "the editor of the comment is open"
                 (lambda () (== (length (comment-buffers)) 1)))
       (gt-raw "click 200 100")
       (gt-idle)
       (gt-eval (lambda ()
                  (with-buffer (string->url "tmfs://aux/edit-comment")
                    (go-into-comment))))
       (gt-idle)
       (gt-raw "key C-Return")
       (gt-wait 1500)
       (gt-check "closed by C-Return, its buffer too"
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
