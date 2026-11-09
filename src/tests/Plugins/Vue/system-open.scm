;; files given by the system (the Finder, the Dock, open -a; on macOS a drop
;; with no window from SDL): each is opened, by load-buffer-from-system
;; (#768). The report says which buffer is current, whether a file is shown
;; in a window, and whether a window was added since the last report
(define system-open-windows 0)
(tm-define (system-open-report tag)
  (let* ((b (current-buffer))
         (n (length (window-list)))
         (new? (> n system-open-windows)))
    (set! system-open-windows n)
    (display* "OPENED " tag " " (url->string (url-tail b))
              (if (buffer->window b) " shown" " not shown")
              (if new? " new window" " same window") "\n")))
