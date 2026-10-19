
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-driver.scm
;; DESCRIPTION : scripted tests of the graphical interface
;; COPYRIGHT   : (C) 2026  The TeXmacs Upsilon developers
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A test of the interface is a list of steps, which the port runs one at a
;; time from its event loop: gui-test-run asks it (gui-test-set-pump) to
;; call gui-test-pump at each turn, which runs the next step when no event
;; of the port is waiting (gui-test-busy?) and the last wait is over. A
;; step is made by one of the gt- procedures below; the checks count their
;; failures as those of check-lib, and with gt-exit TeXmacs ends with the
;; status 1 when one failed. The line language of the scripts is in
;; gui-script.scm; docs/gui-scripting.md has the whole of it.
;;
;;   (gui-test-run
;;     (list (gt-wait 12000)
;;           (gt-raw "window #2")
;;           (gt-activate "equation*")
;;           (gt-activate "Inline formula")
;;           (gt-idle)
;;           (gt-check "inline" (lambda () (tree-is? (cursor-tree) :up 'math)))
;;           (gt-exit)))
;;
;; The steps of a port which has no hooks (gui-test-port is "") do nothing.

(texmacs-module (check gui-driver)
  (:use (check check-lib)))

;; A step is a pair (echo . thunk): echo, a string or #f, is written to the
;; log before the step runs; the thunk returns
;;   'next      done, the next step comes at the next turn of the loop
;;   'continue  done, the next step may follow in the same turn
;;   'retry     not yet: the step runs again at the next turn
(define gt-steps '())
(define gt-running? #f)
(define gt-next-time 0)          ; no step before this time (gt-wait)
(define gt-retrying #f)          ; the time at which the retried step began
(define gt-quiet 0)              ; the turns in a row without waiting events
(define gt-exit? #f)             ; exit at the end, with the status
(define gt-on-end #f)

(tm-define (gt-log . l)
  (:synopsis "Write @l to the log, as a line of the script")
  (display* (gui-test-port) " script: ")
  (apply display* l)
  (display "\n")
  (force-output))

(tm-define (gt-step echo thunk)
  (:synopsis "The step which writes @echo to the log, then runs @thunk")
  (cons echo thunk))

(tm-define (gui-test-run steps . opt-on-end)
  (:synopsis "Run the steps @steps from the event loop of the port")
  (set! gt-steps steps)
  (set! gt-on-end (and (nnull? opt-on-end) (car opt-on-end)))
  (set! gt-running? #t)
  (set! gt-next-time 0)
  (set! gt-retrying #f)
  (set! gt-quiet 0)
  (gui-test-set-pump #t))

(define (gt-run-step step)
  (when (and (not gt-retrying) (car step))
    (gt-log (car step)))
  (catch #t
    (cdr step)
    (lambda (key . args)
      (check-report #f (or (car step) "step")
                    (string-append "error " (object->string (cons key args))))
      'next)))

(define (gt-finish)
  (set! gt-running? #f)
  (gui-test-set-pump #f)
  (gt-log "done")
  (when gt-on-end (gt-on-end))
  (when gt-exit?
    (with n (check-end)
      (exit (if (> n 0) 1 0)))))

(tm-define (gui-test-pump)
  (:synopsis "Run the next step, if it is time (the port calls it)")
  (cond ((not gt-running?) (noop))
        ((gui-test-busy?) (set! gt-quiet 0))
        ((< (texmacs-time) gt-next-time) (set! gt-quiet (+ gt-quiet 1)))
        (else
          (set! gt-quiet (+ gt-quiet 1))
          (let loop ()
            (if (null? gt-steps) (gt-finish)
                (with r (gt-run-step (car gt-steps))
                  (if (== r 'retry)
                      (when (not gt-retrying) (set! gt-retrying (texmacs-time)))
                      (begin
                        (set! gt-steps (cdr gt-steps))
                        (set! gt-retrying #f)
                        (when (== r 'continue) (loop))))))))))

(define (gt-retried-for)
  ;; how long the step being retried has been tried, in ms
  (if gt-retrying (- (texmacs-time) gt-retrying) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The steps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gt-wait ms . opt-echo)
  (:synopsis "Wait @ms milliseconds")
  (gt-step (if (nnull? opt-echo) (car opt-echo)
               (string-append "wait " (number->string ms)))
           (lambda ()
             (set! gt-next-time (+ (texmacs-time) ms))
             'next)))

(tm-define (gt-raw line)
  (:synopsis "The command @line of the port (click, key, window...)")
  (gt-step line
           (lambda ()
             (if (== (gui-test-raw line) 1) 'continue 'next))))

(tm-define (gt-eval-later cmd . opt-echo)
  (:synopsis "Run the Scheme command @cmd (a string) at the next turn")
  (gt-step (if (nnull? opt-echo) (car opt-echo)
               (string-append "scheme " cmd))
           (lambda ()
             (gui-test-eval-later cmd)
             'next)))

(tm-define (gt-eval thunk)
  (:synopsis "Run @thunk now")
  (gt-step #f (lambda () (thunk) 'next)))

(tm-define (gt-check what thunk)
  (:synopsis "Check that @thunk returns true")
  (gt-step (string-append "check " what)
           (lambda ()
             (check-predicate what thunk (lambda (x) x) "true")
             'next)))

(tm-define (gt-idle . opt)
  (:synopsis "Wait until the port has had no event for a few turns")
  ;; at most opt (ms, 10 s by default)
  (with limit (if (nnull? opt) (car opt) 10000)
    (gt-step "idle"
             (lambda ()
               (cond ((>= gt-quiet 5) 'next)
                     ((> (gt-retried-for) limit)
                      (check-report #f "idle" "the port stayed busy")
                      'next)
                     (else 'retry))))))

(define (gt-text-entry s)
  ;; "x\ty\tw\th\ttext" -> (x y w h text)
  (with l (string-tokenize-by-char s #\tab)
    (and (>= (length l) 5)
         (append (map string->number (sublist l 0 4))
                 (list (apply string-append
                              (list-intersperse (list-tail l 4) "\t")))))))

(tm-define (gt-find-text label)
  (:synopsis "The box (x y w h) of the text @label drawn last, or #f")
  ;; the last one drawn, the topmost (a menu is drawn over the window); a
  ;; text equal to the label, else one which starts with it
  (let* ((l (list-filter (map gt-text-entry (gui-test-texts)) identity))
         (same (list-filter l (lambda (e) (== (list-ref e 4) label))))
         (starts (list-filter l (lambda (e)
                                  (string-starts? (list-ref e 4) label))))
         (found (if (nnull? same) same starts)))
    (and (nnull? found) (sublist (cAr found) 0 4))))

(define (gt-texts-summary)
  ;; the texts of the window, for the log of a failure
  (with l (list-filter (map gt-text-entry (gui-test-texts)) identity)
    (if (null? l) "none"
        (apply string-append
               (list-intersperse (map (lambda (e) (object->string (list-ref e 4)))
                                      l)
                                 " ")))))

(tm-define (gt-activate label . opt)
  (:synopsis "Click the text @label of the window of the script")
  ;; when it is drawn, within opt ms (3 s by default)
  (with limit (if (nnull? opt) (car opt) 3000)
    (gt-step (string-append "activate " label)
             (lambda ()
               (with box (gt-find-text label)
                 (cond (box
                        (with (x y w h) box
                          (gui-test-raw
                           (string-append "click "
                                          (number->string (+ x (quotient w 2)))
                                          " "
                                          (number->string (+ y (quotient h 2))))))
                        'next)
                       ((> (gt-retried-for) limit)
                        (check-report #f (string-append "activate " label)
                                      (string-append
                                       "no such text in the window, which has "
                                       (gt-texts-summary)))
                        'next)
                       (else 'retry)))))))

(tm-define (gt-port name)
  (:synopsis "The steps which follow are for the port @name only")
  (gt-step #f
           (lambda ()
             (when (!= (gui-test-port) name)
               (gt-log "skipped: for the port " name)
               (set! gt-steps (list (car gt-steps))))
             'continue)))

(tm-define (gt-exit)
  (:synopsis "At the end, quit TeXmacs with the status 1 if a check failed")
  (gt-step #f (lambda () (set! gt-exit? #t) 'continue)))
