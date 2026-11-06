;; Random patches with small and hostile integers (negative, over 2^31, over
;; 2^63) through the functions that a host or a participant calls on what
;; another process sends: nothing may kill the process. Process A fuzzes (a
;; crash shows as a missing result), B only takes part.
;; The functions are called as the shared files call them: patch-apply,
;; patch-invert, patch-pull... only on what patch-applicable? accepted.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define seed (string->number (or (getenv "FZ_SEED") "12345")))
(define (rnd n) (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648)) (modulo (quotient seed 4096) n))
(define (pick l) (list-ref l (rnd (length l))))
(define D '(shared (meta "style" (collection)) (document "abc" (concat "x" (frac "a" "b") "y") "def" (document "q"))))
(define dt (stree->tree D))
(define kinds '(assign insert remove split join assign-node insert-node remove-node set-cursor))
(define smallv '(0 0 1 1 1 2 2 3 4))
(define hugev '(-1 -5 100 999999 1000000 1000001 2147483646 2147483647 2147483648 4294967296 9223372036854775807 99999999999))
(define args (list "" "ab" "x" '(frac "a" "b") '(document "x") '(concat) '(tuple) '(document) '(concat "u" "v") '(frac) 'foo '(document (concat "z" (document)))))
(define (gen-path hostile)
  (let* ((n (+ 1 (rnd 6)))
         (first (pick '(0 1 1 1 0)))
         (rest (map (lambda (i) (if (and hostile (= (rnd 3) 0)) (pick hugev) (pick smallv))) (.. 0 (- n 1)))))
    (cons first rest)))
(define (gen-mod hostile) (list (pick kinds) (gen-path hostile) (pick args)))
(define (gen-psexp hostile)
  (let ((l (map (lambda (i) (list 'pair (gen-mod hostile) (gen-mod hostile))) (.. 0 (+ 1 (rnd 3))))))
    (if (= (rnd 2) 0) (car l) (cons 'compound l))))

(define nshape 0) (define napp 0) (define npull 0) (define nsan 0)
(define (try-one)
  (let* ((hostile (= (rnd 3) 0))
         (ps (gen-psexp hostile)))
    ;; what any participant may send, whatever its shape: applicability must be decided, not crash
    (catch #t
      (lambda ()
        (let ((p (scheme->patch ps)))
          (set! nshape (+ nshape 1))
          (patch-applicable? p dt)))
      (lambda args #f))
    ;; what the host does with a patch of a participant (shape checked, rebuilt
    ;; with the inverses of the document), then with the patches it has
    (let ((p (shared-sanitize-patch ps dt)))
      (when p
        (set! nsan (+ nsan 1))
        (patch-applicable? p dt)
        (set! napp (+ napp 1))
        (patch-invert p dt)
        (patch-apply dt p)
        (patch-cursor-hint p dt)
        (patch-compactify p)
        (let ((q (shared-sanitize-patch (gen-psexp #f) dt)))
          (when (and q (patch-can-pull? p q))
            (set! npull (+ npull 1))
            (patch-pull p q) (patch-co-pull p q) (patch-commute? p q)))))))

(define (A-steps)
  (list
   (act "fuzz" (let loop ((i 0)) (when (< i (string->number (or (getenv "FZ_N") "4000"))) (try-one) (loop (+ i 1))))
        (display* "FUZZ " nshape " " napp " " npull " " nsan "\n")
        (check "the process survived the random patches" #t)
        (check "and some of them applied" (and (> napp 50) (> nsan 50))))
   (act "go" (sync-set! "a-done"))
   (wait-sync "b-done")))

(define (B-steps)
  (list (wait-sync "a-done")
        (act "done" (sync-set! "b-done"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
