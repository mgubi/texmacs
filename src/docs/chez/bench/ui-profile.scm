;;; ui-profile.scm -- profiling companion to docs/chez/bench/ui.scm
;;;
;;; Same five tasks, same "once cold, then three warm" protocol, but every
;;; measurement also reports where the time and the heap went.
;;;
;;; Why this file exists: ui.scm prints one number per task (milliseconds) and
;;; that number cannot be attributed.  In this benchmark a keystroke is five
;;; distinct calls, and only one of them (key-press) is Scheme; the rest are
;;; editing state and C++ typesetting.  Without the split, "typing is 2.2 ms per
;;; key" is not actionable.
;;;
;;; Chez facts this file had to establish by measurement (10.4.1, this tree):
;;;   * There is no zero-argument `sstats' procedure.  `(statistics)' IS the
;;;     snapshot: it returns an sstats record and prints nothing (~70 ns/call).
;;;   * The result of `sstats-difference' is a time-duration whose
;;;     `time-second' is an integer, so a 1.7 ms delta reads as 0.  The
;;;     fraction is in `time-nanosecond'.
;;;   * `sstats-bytes' is GROSS bytes allocated (it matches the `time' macro's
;;;     "N bytes allocated"), so it measures heap churn and is what you want.
;;;     `(bytes-allocated)' is NET heap growth: after 29 MB of garbage it
;;;     reports 3.9 MB, and it can go negative.  Do not use it for churn.
;;;   * `sstats-gc-real' / `sstats-gc-cpu' are only non-zero when a collection
;;;     actually happened in the window.
;;;   * `current-time' is NOT Chez's inside TeXmacs -- a 0-argument glue
;;;     procedure is installed under that name, so (current-time 'time-monotonic)
;;;     raises "incorrect number of arguments".  Wall time is taken from the
;;;     sstats snapshot instead.
;;;   * `top-level-bound?' is #f for syntax, so `time' looks unbound; test with
;;;     (procedure? (top-level-syntax 'time)).
;;;
;;; Usage, from the repository root, with the usual environment:
;;;   ... -x '(load "docs/chez/bench/ui-profile.scm")'
;;;
;;; Output (greppable):
;;;   PROF <task> cold_ms <n> warm_med_ms <n> warm_mean_ms <n>
;;;   PROF <task> warm cpu_ms <n> gc_cpu_ms <n> gc_count <n> gc_ms <n> bytes <n>
;;;   PROF <task> warm per_key_us <n> bytes_per_key <n>     (typing tasks only)
;;;   PHASE <task> <phase> ms <n> pct <n> per_key_us <n> bytes <n> bytes_net <n>
;;;
;;; Knobs:
;;;   TEXMACS_BENCH_PHASES=1     per-phase split of the key path (and of
;;;                              load-buffer vs update-forced for open-change-log)
;;;   TEXMACS_BENCH_COLLECT=1    (collect 2) before each warm run
;;;   TEXMACS_BENCH_GC_NOTIFY=1  collect-notify: one line per collection
;;;   TEXMACS_BENCH_ONLY=<substr> run only tasks whose name contains substr
;;;   TEXMACS_BENCH_BIG=1        also the 273-page manual task (very slow)
;;;
;;; docs/chez/bench/ui.scm is untouched, so the published numbers stay
;;; comparable.

;;; --- sstats ---------------------------------------------------------------

(define (pf-snap) (statistics))

(define (pf-ssec t)
  (+ (time-second t) (/ (time-nanosecond t) 1000000000.0)))

;; sstats-difference s1 s0 = s1 - s0.  Each time field is a time-duration.
(define (pf-delta s0 s1)
  (let ((d (sstats-difference s1 s0)))
    (vector (pf-ssec (sstats-real d))     ; 0 wall seconds
            (pf-ssec (sstats-cpu d))      ; 1 cpu seconds
            (pf-ssec (sstats-gc-real d))  ; 2 gc wall seconds
            (pf-ssec (sstats-gc-cpu d))   ; 3 gc cpu seconds
            (sstats-bytes d)              ; 4 GROSS bytes allocated
            (sstats-gc-count d))))        ; 5 collections

(define (pf-wall d)   (vector-ref d 0))
(define (pf-cpu d)    (vector-ref d 1))
(define (pf-gc d)     (vector-ref d 2))
(define (pf-gccpu d)  (vector-ref d 3))
(define (pf-bytes d)  (vector-ref d 4))
(define (pf-gcn d)    (vector-ref d 5))

(define (pf-round x) (inexact->exact (round x)))
(define (pf-ms sec)  (pf-round (* 1000.0 sec)))

;; "n.n" with one decimal, without relying on `format' (TeXmacs may shadow it)
(define (pf-dec1 x)
  (let* ((tenths (pf-round (* 10.0 x)))
         (whole  (quotient tenths 10))
         (frac   (modulo tenths 10)))
    (string-append (number->string whole) "." (number->string frac))))

;; Bytes allocated by one (statistics) call, so the phase table can subtract
;; its own instrumentation.  Two snapshots bracket every phase, so the floor is
;; 2 * this per phase call; it is ~192 B and would otherwise dominate the
;; phases that allocate nothing (typesetting allocates on the C++ heap).
(define pf-snap-bytes
  (let* ((s0 (pf-snap))
         (n  (do ((i 0 (+ i 1)) (k 0 (+ k 0))) ((= i 500) k) (pf-snap)))
         (s1 (pf-snap))
         (b  (sstats-bytes (sstats-difference s1 s0))))
    (max 0 (pf-round (/ b 500.0)))))

;;; --- per-phase accumulator ------------------------------------------------

(define pf-phase-order '(archive start keypress end update load typeset))
(define pf-phase-ms    (make-ahash-table))
(define pf-phase-bytes (make-ahash-table))
(define pf-phase-calls (make-ahash-table))
(define pf-phase-rounds 1)

(define (pf-phase-reset! rounds)
  (set! pf-phase-rounds rounds)
  (for-each (lambda (p)
              (ahash-set! pf-phase-ms p 0.0)
              (ahash-set! pf-phase-bytes p 0)
              (ahash-set! pf-phase-calls p 0))
            pf-phase-order))

(define (pf-phase-add! p ms bytes)
  (ahash-set! pf-phase-ms p (+ (ahash-ref pf-phase-ms p) ms))
  (ahash-set! pf-phase-bytes p (+ (ahash-ref pf-phase-bytes p) bytes))
  (ahash-set! pf-phase-calls p (+ 1 (ahash-ref pf-phase-calls p))))

(pf-phase-reset! 1)

;; Run thunk, charging its wall time and gross allocated bytes to phase `p'.
(define (pf-phase p thunk)
  (let* ((s0 (pf-snap))
         (r  (thunk))
         (s1 (pf-snap))
         (d  (pf-delta s0 s1)))
    (pf-phase-add! p (* 1000.0 (pf-wall d)) (pf-bytes d))
    r))

(define (pf-phase-report! task keys)
  (let ((total (let loop ((ps pf-phase-order) (acc 0.0))
                 (if (null? ps) acc (loop (cdr ps) (+ acc (ahash-ref pf-phase-ms (car ps))))))))
    (for-each
     (lambda (p)
       (let* ((ms    (ahash-ref pf-phase-ms p))
              (bytes (ahash-ref pf-phase-bytes p))
              (calls (ahash-ref pf-phase-calls p))
              (net   (max 0 (- bytes (* calls 2 pf-snap-bytes)))))
         (if (> calls 0)
             (display* "PHASE " task " " p
                       " ms " (pf-round ms)
                       " pct " (pf-dec1 (if (> total 0.0) (* 100.0 (/ ms total)) 0.0))
                       " per_key_us " (if keys
                                          (pf-round (/ (* 1000.0 ms)
                                                       (* keys pf-phase-rounds)))
                                          0)
                       " bytes " bytes
                       " bytes_net " net
                       "\n"))))
     pf-phase-order)
    (pf-phase-reset! 1)))

;;; --- task harness ---------------------------------------------------------

(define (pf-substring? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))

(define (pf-wanted? name)
  (let ((only (getenv "TEXMACS_BENCH_ONLY")))
    (if (and only (not (string=? only "")))
        (pf-substring? name only)
        #t)))

(define (pf-median3 l)
  (let ((a (car l)) (b (cadr l)) (c (caddr l)))
    (cond ((and (<= a b) (<= b c)) b)
          ((and (<= b a) (<= a c)) a)
          ((and (<= a c) (<= c b)) c)
          ((and (<= c a) (<= a b)) a)
          ((and (<= b c) (<= c a)) c)
          (else b))))

;; One warm run: wall ms + resource delta.
(define (pf-warm-run thunk)
  (if (getenv "TEXMACS_BENCH_COLLECT") (guard (e (else #f)) (collect 2)))
  (let* ((s0 (pf-snap))
         (r  (thunk))
         (s1 (pf-snap))
         (d  (pf-delta s0 s1)))
    (cons (* 1000.0 (pf-wall d)) d)))

;; keys = keystrokes in one round, or #f for non-typing tasks.
(define (prof-bench name keys thunk)
  (if (pf-wanted? name)
      (let* ((s0 (pf-snap))
             (n  (thunk))
             (s1 (pf-snap))
             (cold-d  (pf-delta s0 s1))
             (cold-ms (* 1000.0 (pf-wall cold-d))))
        (pf-phase-reset! 3)
        (let* ((runs (list (pf-warm-run thunk) (pf-warm-run thunk) (pf-warm-run thunk)))
               (walls (map car runs))
               (ds    (map cdr runs))
               (mean-wall (/ (apply + walls) 3.0))
               ;; means per warm run
               (cpu   (/ (apply + (map pf-cpu   ds)) 3.0))
               (gccpu (/ (apply + (map pf-gccpu ds)) 3.0))
               (gcn   (/ (apply + (map pf-gcn   ds)) 3.0))
               (gc    (/ (apply + (map pf-gc    ds)) 3.0))
               (byts  (/ (apply + (map pf-bytes ds)) 3.0)))
          (display* "PROF " name
                    " cold_ms " (pf-round cold-ms)
                    " warm_med_ms " (pf-round (pf-median3 walls))
                    " warm_mean_ms " (pf-round mean-wall)
                    "\n")
          (display* "PROF " name
                    " warm cpu_ms " (pf-round (* 1000.0 cpu))
                    " gc_cpu_ms " (pf-round (* 1000.0 gccpu))
                    " gc_count " (pf-dec1 gcn)
                    " gc_ms " (pf-round (* 1000.0 gc))
                    " bytes " (pf-round byts)
                    "\n")
          (if keys
              (display* "PROF " name
                        " warm per_key_us " (pf-round (/ (* 1000.0 mean-wall) keys))
                        " bytes_per_key " (pf-round (/ byts keys))
                        "\n"))
          (display* "PROF " name " cold_bytes " (pf-round (pf-bytes cold-d)) "\n")
          (if (getenv "TEXMACS_BENCH_PHASES") (pf-phase-report! name keys))
          (if (number? n) (display* "PROF " name " items " n "\n"))
          #t))))

;;; --- tasks (verbatim from ui.scm, except `key' and `load-and-typeset') -----

(define pf-ui-roots
  '((vertical (link texmacs-menu))
    (horizontal (link texmacs-main-icons))
    (horizontal (link texmacs-mode-icons))
    (horizontal (link texmacs-extra-icons))
    (horizontal (dynamic (texmacs-focus-icons)))
    (vertical (link texmacs-popup-menu))))

(define (pf-ui-links x acc)
  (cond ((not (pair? x)) acc)
        ((and (eq? (car x) 'link) (pair? (cdr x)) (symbol? (cadr x)))
         (cons (cadr x) acc))
        (else (pf-ui-links (cdr x) (pf-ui-links (car x) acc)))))

(define (pf-expand-all-menus)
  (let ((seen (make-ahash-table)) (count 0))
    (define (expand m)
      (set! count (+ count 1))
      (let ((r (guard (e (else '())) (menu-expand m))))
        (for-each (lambda (name)
                    (if (not (ahash-ref seen name))
                        (begin
                          (ahash-set! seen name #t)
                          (expand (list 'vertical (list 'link name))))))
                  (pf-ui-links r '()))))
    (for-each expand pf-ui-roots)
    count))

;; One key press exactly as ui.scm drives it -- or, with TEXMACS_BENCH_PHASES,
;; the same five calls with a snapshot around each.
(define (pf-key k)
  (if (getenv "TEXMACS_BENCH_PHASES")
      (begin
        (pf-phase 'archive  (lambda () (archive-state)))
        (pf-phase 'start    (lambda () (start-editing)))
        (pf-phase 'keypress (lambda () (key-press k)))
        (pf-phase 'end      (lambda () (end-editing)))
        (pf-phase 'update   (lambda () (update-forced))))
      (begin
        (archive-state)
        (start-editing)
        (key-press k)
        (end-editing)
        (update-forced))))

(define pf-text-keys
  (map char->string
       (string->list "The quick brown fox jumps over the lazy dog, again. ")))

(define pf-math-keys
  '("a" "^" "2" "right" "+" "b" "_" "1" "right" "=" "\\" "f" "r" "a" "c"
    "return" "x" "down" "y" "right" "space" "s" "i" "n" "space" "x" "space"))

(define (pf-ui-buffer doc)
  (let ((u (new-buffer)))
    (buffer-set-body u (stree->tree doc))
    (update-forced)
    u))

(define (pf-close-ui-buffer u)
  (buffer-pretend-saved u)
  (buffer-close u))

(define (pf-in-text thunk)
  (let ((u (pf-ui-buffer '(document "Some text."))))
    (go-end)
    (let ((r (thunk)))
      (pf-close-ui-buffer u)
      r)))

(define (pf-in-math thunk)
  (let ((u (pf-ui-buffer '(document (concat "Text " (math "x+y"))))))
    (go-to (append (buffer-path) '(0 1 0 1)))
    (let ((r (thunk)))
      (pf-close-ui-buffer u)
      r)))

(define (pf-type keys n)
  (do ((i 0 (+ i 1))) ((= i n)) (for-each pf-key keys)))

(define (pf-load-and-typeset)
  (let ((u (url-unix "$TEXMACS_PATH" "doc/about/changes/change-log.en.tm")))
    (if (getenv "TEXMACS_BENCH_PHASES")
        (begin
          (pf-phase 'load    (lambda () (load-buffer u)))
          (pf-phase 'typeset (lambda () (update-forced))))
        (begin
          (load-buffer u)
          (update-forced)))
    (pf-close-ui-buffer u)))

(define (pf-in-user-manual thunk)
  (let ((root (url-resolve (url-unix "$TEXMACS_DOC_PATH"
                                     "main/man-user-manual.en.tm") "r")))
    (tmdoc-expand-help root "book")
    (update-forced)
    (go-end)
    (let ((r (thunk)))
      (pf-close-ui-buffer (current-buffer))
      r)))

;;; --- driver ---------------------------------------------------------------

(exec-delayed
 (lambda ()
   (if (getenv "TEXMACS_BENCH_GC_NOTIFY") (collect-notify #t))
   (display* "PROF-BEGIN phases " (if (getenv "TEXMACS_BENCH_PHASES") "on" "off")
             " collect " (if (getenv "TEXMACS_BENCH_COLLECT") "on" "off")
             " snap_bytes " pf-snap-bytes
             "\n")
   (prof-bench "menus-text" #f (lambda () (pf-in-text pf-expand-all-menus)))
   (prof-bench "menus-math" #f (lambda () (pf-in-math pf-expand-all-menus)))
   (prof-bench "typing-text-520-keys" 520
               (lambda () (pf-in-text (lambda () (pf-type pf-text-keys 10)))))
   (prof-bench "typing-math-270-keys" 270
               (lambda () (pf-in-math (lambda () (pf-type pf-math-keys 10)))))
   (prof-bench "open-change-log" #f pf-load-and-typeset)
   (if (getenv "TEXMACS_BENCH_BIG")
       (prof-bench "typing-user-manual-106-keys" 106
                   (lambda () (pf-in-user-manual (lambda () (pf-type pf-text-keys 2))))))
   (display* "PROF-END\n")
   (quit-TeXmacs)))
