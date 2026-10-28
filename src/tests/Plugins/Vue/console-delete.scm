;; The window of the error messages closed by a program (alt-window-delete,
;; as a dialog closed by one of its buttons) and not by its close button
;; (console-edit): the buffers of its editors are closed at once, as with
;; Qt, and stay closed (#566: they stayed open, the window passed the
;; deletion on to its contents only after a close request).
;;   SCM=$PWD/tests/Plugins/Vue/console-delete.scm sh tests/Plugins/Vue/run.sh console-delete 40
(use-modules (check gui-driver))

(define (console-buffers)
  (list-filter (map url->unix (buffer-list))
               (cut string-starts? <> "tmfs://aux/console")))

(define (console-window)
  (with l (alt-window-search
           (string->url "tmfs://aux/console-error-messages-all"))
    (and (pair? l) (car l))))

(gui-test-run
 (list (gt-wait 4000)
       (gt-eval (lambda () (debug-message "std-warning" "console-delete\n")))
       (gt-wait 2000)
       (gt-check "the console opened, a buffer per tab"
                 (lambda () (== (length (console-buffers)) 2)))
       (gt-check "its window is known" (lambda () (and (console-window) #t)))
       (gt-eval (lambda () (alt-window-delete (console-window))))
       (gt-check "deleted: its buffers are closed at once"
                 (lambda () (null? (console-buffers))))
       (gt-wait 1500)
       (gt-check "and stay closed" (lambda () (null? (console-buffers))))
       (gt-window "main")
       (gt-exit)))
