;; The window of the error messages shows them in read-only editors: they
;; can be selected and copied, nothing typed, pasted, deleted or undone
;; changes them, new messages are added without losing the selection, and
;; the window leaves no buffer behind when it is closed. (That the window
;; opened by itself does not take the keys of the document is checked on a
;; real screen only: the dummy video driver gives every new window the
;; focus.)
;;   SCM=$PWD/tests/Plugins/Vue/console-edit.scm sh tests/Plugins/Vue/run.sh console-edit 60
(use-modules (check gui-driver))

(define (console-buffers)
  (list-filter (map url->unix (buffer-list))
               (cut string-starts? <> "tmfs://aux/console")))

(define (all-buffer)
  (string->url "tmfs://aux/console-error-messages-all"))

(define (shown-messages)
  ;; the number of messages in the tab of all of them
  (with body (buffer-get-body (all-buffer))
    (tree-arity (tree-ref body 0 6))))

(define (shows? s)
  ;; (message by message: s7 prints a long list with an ellipsis)
  (with d (tree-ref (buffer-get-body (all-buffer)) 0 6)
    (list-or (map (lambda (m) (string-contains? (object->string (tm->stree m)) s))
                  (tree-children d)))))

(define (warn s)
  (debug-message "std-warning" (string-append s "\n")))

(define buffers-before #f)

(gui-test-run
 (list (gt-wait 6000)
       (gt-eval (lambda () (set! buffers-before (length (buffer-list)))))
       (gt-eval (lambda ()
                  (for (i (.. 0 12))
                    (warn (string-append "warning " (number->string i) ": "
                                         (make-string 300 #\x))))))
       (gt-wait 3000)
       (gt-check "the console opened by itself, a buffer per tab"
                 (lambda () (== (length (console-buffers)) 2)))
       (gt-window "messages")
       (gt-raw "focus")
       (gt-idle)
       (gt-raw "click 400 300")
       (gt-idle)
       (gt-check "the click is in the console"
                 (lambda () (== (current-buffer) (all-buffer))))
       (gt-raw "key S-Up")
       (gt-raw "key S-Up")
       (gt-idle)
       (gt-check "a selection" (lambda () (selection-active-any?)))
       (gt-eval (lambda () (warn "a new message")))
       (gt-wait 1000)
       (gt-check "the new message is shown" (lambda () (shows? "a new message")))
       (gt-check "the selection stays" (lambda () (selection-active-any?)))
       (gt-check "no buffer more"
                 (lambda () (== (length (buffer-list)) (+ buffers-before 2))))
       (gt-raw "key M-c")
       (gt-idle)
       (gt-check "the selection is copied"
                 (lambda ()
                   (string-contains?
                     (object->string (tm->stree (clipboard-get "primary")))
                     "Warning: warning")))
       (gt-raw "text zzz")
       (gt-raw "key Backspace")
       (gt-raw "key Backspace")
       (gt-raw "key Backspace")
       (gt-raw "key Backspace")
       (gt-raw "key M-v")
       (gt-raw "key M-x")
       (gt-raw "key M-z")
       (gt-idle)
       (gt-check "read only: the messages are unchanged"
                 (lambda () (and (== (shown-messages) 13) (not (shows? "zzz")))))
       (gt-check "read only: nothing to save"
                 (lambda () (not (buffer-modified? (all-buffer)))))
       (gt-eval (lambda ()
                  (for (i (.. 0 120))
                    (warn (string-append "message " (number->string i))))))
       (gt-wait 2000)
       (gt-check "the last messages, as many as the console size"
                 (lambda () (and (== (shown-messages) 100) (shows? "message 119"))))
       (gt-check "still no buffer more"
                 (lambda () (== (length (buffer-list)) (+ buffers-before 2))))
       (gt-raw "close")
       (gt-wait 1500)
       (gt-window "main")
       (gt-check "the window leaves no buffer"
                 (lambda () (and (null? (console-buffers))
                                 (== (length (buffer-list)) buffers-before))))
       (gt-exit)))
