;; A window whose close was refused ("The document has not been saved.
;; Really close it?" -> No) closes at a later request: the request asks
;; again, and the window goes on a yes (#970: the window latched its first
;; close request and never let another through, also after a save). Two
;; requests before the first ran ask once. The question is answered by a
;; stand-in for user-confirm. It needs no drawing, and runs headless (no
;; window on the screen):
;;   TM_ARGS="-headless -X" sh tests/Plugins/Vue/run.sh close-refused 40
(use-modules (check gui-driver))

(define cr-asked 0)
(define cr-answer #f)
(define cr-old-confirm user-confirm)
(define cr-file (url-append (url-temp-dir) "close-refused.tm"))
(define cr-windows 0)

(define (cr-confirm prompt default cont)
  (set! cr-asked (+ cr-asked 1))
  (display* "got: asked " cr-asked ", answer " cr-answer "\n")
  (cont cr-answer))

(define (cr-open)
  (string-save "<TeXmacs|2.1.4>\n\n<style|generic>\n\n<\\body>\n  close-refused\n</body>\n"
               (url->system cr-file))
  (set! cr-windows (length (window-list)))
  (load-buffer-in-new-window cr-file))

(define (cr-close)
  (gui-test-raw "window close-refused")
  (gui-test-raw "close"))

(define (cr-shown?) ; its buffer is open
  (in? cr-file (buffer-list)))

(gui-test-run
 (list (gt-wait 1000)
       (gt-eval (lambda () (set! user-confirm cr-confirm)))
       (gt-eval cr-open)
       (gt-idle)
       ;; (as Document > Project > Expand inclusions leaves it)
       (gt-eval (lambda () (buffer-pretend-modified cr-file)))
       (gt-check "a second window, on the modified document"
                 (lambda () (and (== (length (window-list)) (+ cr-windows 1))
                                 (buffer-modified? cr-file))))
       (gt-window "close-refused")
       ;; two requests before the first ran: one question
       (gt-eval (lambda () (cr-close) (cr-close)))
       (gt-idle)
       (gt-check "asked once" (lambda () (== cr-asked 1)))
       (gt-check "no: the window stays"
                 (lambda () (and (== (length (window-list)) (+ cr-windows 1))
                                 (cr-shown?))))
       ;; a request after the refusal asks again (it did nothing: #970)
       (gt-eval cr-close)
       (gt-idle)
       (gt-check "asked again" (lambda () (== cr-asked 2)))
       (gt-check "no again: the window stays"
                 (lambda () (== (length (window-list)) (+ cr-windows 1))))
       (gt-eval (lambda () (set! cr-answer #t) (cr-close)))
       (gt-idle)
       (gt-check "asked a third time" (lambda () (== cr-asked 3)))
       (gt-check "yes: the window is closed"
                 (lambda () (== (length (window-list)) cr-windows)))
       (gt-eval (lambda ()
                  (set! user-confirm cr-old-confirm)
                  (when (cr-shown?)
                    (buffer-pretend-saved cr-file)
                    (buffer-close cr-file))
                  (when (url-exists? cr-file) (system-remove cr-file))))
       (gt-exit)))
