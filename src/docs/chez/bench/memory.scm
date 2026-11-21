;;; memory.scm -- memory footprint benchmark across typical TeXmacs usage states
;;;
;;; Measures Chez Scheme heap metrics (current-memory-bytes, maximum-memory-bytes,
;;; bytes-allocated, sstats-bytes) and can be run per-state under `/usr/bin/time -l`
;;; to capture OS Max RSS and macOS Peak Memory Footprint.
;;;
;;; Usage:
;;;   TEXMACS_MEM_STATE=startup-50ms   texmacs -x '(load "docs/chez/bench/memory.scm")'
;;;   TEXMACS_MEM_STATE=startup-300ms  texmacs -x '(load "docs/chez/bench/memory.scm")'
;;;   TEXMACS_MEM_STATE=manual         texmacs -x '(load "docs/chez/bench/memory.scm")'
;;;   TEXMACS_MEM_STATE=small-edit-50  texmacs -x '(load "docs/chez/bench/memory.scm")'
;;;   TEXMACS_MEM_STATE=big-edit-50    texmacs -x '(load "docs/chez/bench/memory.scm")'
;;;   (omit TEXMACS_MEM_STATE to run all states sequentially in one process)

(define (mem-report label)
  (let ((s (statistics)))
    (display* "MEM " label
              " chez_cur=" (current-memory-bytes)
              " chez_max=" (maximum-memory-bytes)
              " chez_alloc=" (bytes-allocated)
              " chez_gross=" (sstats-bytes s)
              " gc_count=" (sstats-gc-count s)
              "\n")))

(define (mem-new-small-buffer)
  (let ((u (new-buffer)))
    (buffer-set-body u (stree->tree '(document "Some text.")))
    (update-forced)
    (go-end)
    u))

(define (mem-open-user-manual)
  (let ((root (url-resolve (url-unix "$TEXMACS_DOC_PATH"
                                     "main/man-user-manual.en.tm") "r")))
    (tmdoc-expand-help root "book")
    (update-forced)
    (go-end)
    (current-buffer)))

(define (mem-edit-50-undo-redo)
  ;; Perform 50 distinct archived edits, then undo 25, branch with 5 more edits,
  ;; and undo/redo to exercise a 50-step branching undo/redo tree.
  (do ((i 0 (+ i 1))) ((= i 50))
    (archive-state)
    (start-editing)
    (insert (string-append " edit-" (number->string i)))
    (end-editing)
    (update-forced))
  (do ((i 0 (+ i 1))) ((= i 25))
    (undo 0))
  (update-forced)
  (do ((i 0 (+ i 1))) ((= i 5))
    (archive-state)
    (start-editing)
    (insert (string-append " branch-" (number->string i)))
    (end-editing)
    (update-forced))
  (do ((i 0 (+ i 1))) ((= i 5))
    (undo 0))
  (do ((i 0 (+ i 1))) ((= i 5))
    (redo 0))
  (update-forced))

(define (run-mem-state state)
  (cond
    ((string=? state "startup-50ms")
     (mem-report "startup-50ms"))
    ((string=? state "startup-300ms")
     (mem-report "startup-300ms"))
    ((string=? state "manual")
     (let ((u (mem-open-user-manual)))
       (display* "MANUAL pages=" (get-page-count) "\n")
       (mem-report "manual-273p")
       (buffer-pretend-saved u)
       (buffer-close u)))
    ((string=? state "small-edit-50")
     (let ((u (mem-new-small-buffer)))
       (mem-report "small-before-edit")
       (mem-edit-50-undo-redo)
       (display* "UNDO_REDO small undo=" (undo-possibilities)
                 " redo=" (redo-possibilities) "\n")
       (mem-report "small-after-edit-50")
       (buffer-pretend-saved u)
       (buffer-close u)))
    ((string=? state "big-edit-50")
     (let ((u (mem-open-user-manual)))
       (mem-report "big-before-edit")
       (mem-edit-50-undo-redo)
       (display* "UNDO_REDO big undo=" (undo-possibilities)
                 " redo=" (redo-possibilities) "\n")
       (mem-report "big-after-edit-50")
       (buffer-pretend-saved u)
       (buffer-close u)))
    (else
     (display* "Unknown TEXMACS_MEM_STATE: " state "\n"))))

(let ((state (getenv "TEXMACS_MEM_STATE")))
  (if (and state (string=? state "startup-300ms"))
      (delayed (:idle 300)
        (guard (e (else (display "ERR: ") (display-condition e) (newline)))
          (run-mem-state state))
        (quit-TeXmacs))
      (delayed (:idle 50)
        (guard (e (else (display "ERR: ") (display-condition e) (newline)))
          (if state
              (run-mem-state state)
              (begin
                (run-mem-state "startup-50ms")
                (run-mem-state "small-edit-50")
                (run-mem-state "big-edit-50"))))
        (quit-TeXmacs))))
