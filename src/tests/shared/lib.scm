;; Library of the tests of the shared files (tests/shared/check.sh).
;;
;; Each scenario is run by two TeXmacs processes at once, ROLE A and ROLE B
;; (the environment variables SH_ROLE, SH_SYNC, SH_FILE, SH_OUT), which do
;; their steps one after the other and meet through marker files in the folder
;; SH_SYNC. A step is a thunk which gives #t when it is done and #f when it
;; must be called again (a wait); the steps are run by the delayed commands of
;; the process, so that it serves the other one in between, as TeXmacs does.

(define SYNC (getenv "SH_SYNC"))
(define ROLE (getenv "SH_ROLE"))
(define FILE (getenv "SH_FILE"))
(define OUT  (getenv "SH_OUT"))
(define STEP-TIMEOUT 20000)

(define results '())

(define (check name ok)
  (display* "CHECK " ROLE " " name " " (if ok "PASS" "FAIL") "\n")
  (set! results (cons (cons name ok) results)))

(define (sync-file n) (string->url (string-append SYNC "/" n)))
(define (sync-set! n) (string-save "1" (sync-file n)))
(define (sync-has? n) (url-exists? (sync-file n)))
(define (sync-write! n s) (string-save s (sync-file n)))
(define (sync-read n) (string-load (sync-file n)))

(define (finish!)
  (display* "RESULT " ROLE " " (length (list-filter results (lambda (r) (not (cdr r)))))
            " failures of " (length results) "\n")
  (string-save (if (list-and (map cdr results)) "ok" "fail")
               (string->url OUT))
  (quit-TeXmacs))

(define (run-steps steps)
  (let ((cur steps) (start (texmacs-time)))
    (exec-delayed-pause
     (lambda ()
       (if (null? cur)
           (begin (finish!) #t)
           (let ((r (catch #t
                      (lambda () ((cdar cur)))
                      (lambda args
                        (display* "ERROR in step " (caar cur) ": " args "\n")
                        'error))))
             (cond ((eq? r 'error)
                    (check (string-append (caar cur) " (error)") #f)
                    (finish!) #t)
                   (r (set! cur (cdr cur)) (set! start (texmacs-time)) 5)
                   ((> (- (texmacs-time) start) STEP-TIMEOUT)
                    (check (string-append (caar cur) " (timeout)") #f)
                    (finish!) #t)
                   (else 10))))))))

(define-macro (step name . body)
  `(cons ,name (lambda () ,@body)))

(define-macro (act name . body)
  ;; a step which does something once
  `(cons ,name (lambda () ,@body #t)))

(define-macro (wait-sync n)
  `(cons (string-append "wait " ,n) (lambda () (sync-has? ,n))))

(define (the-name) (string->url FILE))
(define (body) (buffer-get-body (the-name)))
(define (body-stree) (tm->stree (body)))
(define (para i) (tree-ref (body) i))
(define (role-of) (and-with i (shared-file-info (the-name)) (car i)))
(define (state-of) (and-with i (shared-file-info (the-name)) (cadr i)))

;; Edits, as an editor makes them
(define (insert-text! i pos s) (tree-insert (para i) pos s))
(define (para-text i) (tm->stree (para i)))
(define (para-count) (tree-arity (body)))


(define (publish-body!) (sync-write! (string-append ROLE "-body") (object->string (body-stree))))
(define (text-of i) (para-text i))
(define (pending-count) (and-with i (shared-file-info (the-name)) (list-ref i 5)))
(define (peers-count) (and-with i (shared-file-info (the-name)) (list-ref i 2)))
(define (die!) (system (string-append "kill -9 " (number->string (shared-process-id)))))

(define (timed-wait ms)
  ;; a step which waits @ms milliseconds (the times of files have seconds)
  (let ((t0 #f))
    (cons (string-append "wait " (number->string ms) " ms")
          (lambda ()
            (if (not t0) (set! t0 (texmacs-time)))
            (> (- (texmacs-time) t0) ms)))))

(define (rname n s) (string-append "r" (number->string n) "-" s))

;; One round of concurrent edits: A (the host) and B edit while neither hears
;; of the other; B's patch reaches the host first, then the host works.
(define (round-steps n a-edit! b-edit!)
  (if (== ROLE "A")
      (list (act (rname n "A edits") (shared-pause! #t) (a-edit!) (sync-set! (rname n "a-ready")))
            (wait-sync (rname n "b-submitted"))
            (act (rname n "A resumes") (shared-pause! #f))
            (timed-wait 800)
            (act (rname n "A done") (sync-set! (rname n "a-done")))
            (wait-sync (rname n "b-done")))
      (list (wait-sync (rname n "a-ready"))
            (act (rname n "B edits") (shared-pause! #t) (b-edit!))
            (act (rname n "B resumes") (shared-pause! #f))
            (step (rname n "B submitted") (>= (shared-stat 'submitted) n))
            (act (rname n "B signals") (sync-set! (rname n "b-submitted")))
            (wait-sync (rname n "a-done"))
            (timed-wait 300)
            (act (rname n "B done") (sync-set! (rname n "b-done"))))))

(define (recovered-files)
  (let* ((dir (string-append (getenv "TEXMACS_HOME_PATH") "/system/recovered"))
         (l (shared-list-dir dir)))
    (map (lambda (n) (string-append dir "/" (if (symbol? n) (symbol->string n) n)))
         (if (list? l) l '()))))
