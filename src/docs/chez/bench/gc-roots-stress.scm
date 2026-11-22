;;; gc-roots-stress.scm -- Aggressive GC stress test for ChezRoot C++ -> Scheme converters
;;;
;;; Verifies GC safety, deep nesting growth, exception unwind safety, Cork byte
;;; fidelity, and memory stability across C++ -> Scheme conversion paths under
;;; forced full copying collections every 4 KB of allocation.
;;;
;;; Usage:
;;;   HOME=/tmp/tm-stress-home TEXMACS_HOME_PATH=/tmp/tm-stress-home/.TeXmacs-chez \
;;;     TEXMACS_PATH="$PWD/texmacs/TeXmacs" TEXMACS_BOOT_PATH="$PWD/build-texmacos/boot" \
;;;     TEXMACS_DOC_PATH="$PWD/texmacs/TeXmacs/doc" QT_QPA_PLATFORM=offscreen \
;;;     ./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs \
;;;     -x '(load "texmacs/docs/chez/bench/gc-roots-stress.scm")'

(define chez-root-depth
  (foreign-procedure "chez_root_stack_depth" () iptr))

(define chez-root-cap
  (foreign-procedure "chez_root_stack_capacity" () iptr))

(define chez-call-scheme-1
  (foreign-procedure "chez_call_scheme_1" (scheme-object scheme-object) scheme-object))

(define *stress-pass* 0)
(define *stress-fail* 0)
(define *gc-fires* 0)

(define (stress-assert name ok? detail)
  (if ok?
      (begin
        (set! *stress-pass* (+ *stress-pass* 1))
        (display* "PASS " name " " detail "\n"))
      (begin
        (set! *stress-fail* (+ *stress-fail* 1))
        (display* "FAIL " name " " detail "\n"))))

(define (iota-list n)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0) acc (loop (- i 1) (cons i acc)))))

(define (int-list? x)
  (and (list? x)
       (let loop ((l x))
         (or (null? l)
             (and (integer? (car l)) (exact? (car l)) (loop (cdr l)))))))

(define (run-gc-roots-stress)
  (let ((orig-crh (collect-request-handler))
        (orig-trip (collect-trip-bytes))
        (bench-docs
          (map (lambda (f) (tree-import (url-unix "$TEXMACS_PATH" f) "texmacs"))
               '("doc/about/changes/change-log.en.tm"
                 "doc/devel/format/environment/env-page.en.tm"
                 "examples/texts/bigtable-test.tm"
                 "examples/texts/superscript-test-bis.tm"))))
    (dynamic-wind
      (lambda ()
        (collect-request-handler
          (lambda ()
            (set! *gc-fires* (+ *gc-fires* 1))
            (collect (collect-maximum-generation))
            (reap-blackboxes!)))
        (collect-trip-bytes 4096))
      (lambda ()
        ;; Test 1: array_tree_to_tmscm (tree-children) across edge sizes and 1500 children
        (guard (e (else (stress-assert "test1-array-tree" #f (list 'exception (tm-error-message e)))))
          (let* ((make-children
                   (lambda (n)
                     (map (lambda (i) (string-append "node-" (number->string i)))
                          (iota-list n))))
                 (check-n
                   (lambda (n)
                     (let* ((expected (make-children n))
                            (t (stree->tree (cons 'document expected)))
                            (kids (tree-children t))
                            (got (map tree->stree kids)))
                       (and (= (length kids) n)
                            (equal? got expected))))))
            (let ((ok0 (check-n 0))
                  (ok1 (check-n 1))
                  (ok2 (check-n 2))
                  (ok16 (check-n 16))
                  (big-expected (make-children 1500))
                  (big-tree (stree->tree (cons 'document (make-children 1500))))
                  (gc0 *gc-fires*))
              (let* ((kids (tree-children big-tree))
                     (gc1 *gc-fires*)
                     (got (map tree->stree kids))
                     (ok1500 (and (= (length kids) 1500)
                                  (equal? got big-expected))))
                (stress-assert "test1-array-tree"
                  (and ok0 ok1 ok2 ok16 ok1500
                       (> (- gc1 gc0) 0)
                       (= (chez-root-depth) 0))
                  (list 'gcs-during-1500 (- gc1 gc0) 'depth (chez-root-depth)))))))

        ;; Test 2: scheme_tree_to_tmscm (tree->stree / stree->tree) on all 4 benchmark docs
        (guard (e (else (stress-assert "test2-scheme-tree-docs" #f (list 'exception (tm-error-message e)))))
          (let ((expected-strees (map tree->stree bench-docs))
                (gc0 *gc-fires*)
                (all-ok #t))
            (do ((r 0 (+ r 1))) ((= r 3))
              (for-each
                (lambda (t exp-s)
                  (let ((rt (tree->stree (stree->tree (tree->stree t)))))
                    (unless (equal? rt exp-s)
                      (set! all-ok #f))))
                bench-docs expected-strees))
            (let ((gc1 *gc-fires*))
              (stress-assert "test2-scheme-tree-docs"
                (and all-ok (> (- gc1 gc0) 0) (= (chez-root-depth) 0))
                (list 'gcs (- gc1 gc0) 'depth (chez-root-depth))))))

        ;; Test 3: deep nesting (> initial 512 capacity -> triggers chez_root_stack_grow)
        (guard (e (else (stress-assert "test3-deep-grow" #f (list 'exception (tm-error-message e)))))
          (let* ((depth 1200)
                 (deep-s (let loop ((i 0) (acc "leaf"))
                           (if (= i depth) acc (loop (+ i 1) (list 'concat acc)))))
                 (gc0 *gc-fires*)
                 (deep-t (stree->tree deep-s))
                 (rt (tree->stree deep-t))
                 (gc1 *gc-fires*)
                 (cap (chez-root-cap))
                 (d (chez-root-depth)))
            (stress-assert "test3-deep-grow"
              (and (equal? rt deep-s)
                   (>= cap depth)
                   (> (- gc1 gc0) 0)
                   (= d 0))
              (list 'cap cap 'gcs (- gc1 gc0) 'depth d))))

        ;; Test 4: array_int, array_double, array_string, array_url, path, array_path, list_tree, list_string
        (guard (e (else (stress-assert "test4-other-converters" #f (list 'exception (tm-error-message e)))))
          ;; array_int
          (let* ((rgba-red (named-color->rgba "red"))
                 (rgba-blk (named-color->rgba "black"))
                 (ok-int (and (= (length rgba-red) 4)
                              (int-list? rgba-red)
                              (equal? rgba-red '(255 0 0 255))
                              (equal? rgba-blk '(0 0 0 255)))))
            ;; array_double
            (let* ((d0 (anim-control-times (stree->tree '(document "x"))))
                   (d1 (anim-control-times (stree->tree '(morph (tuple "0.0" "a")))))
                   (d3 (anim-control-times
                         (stree->tree '(morph (tuple "0.0" "a")
                                              (tuple "0.5" "b")
                                              (tuple "1.0" "c")))))
                   (ok-dbl (and (null? d0)
                                (equal? d1 '(0.0))
                                (equal? d3 '(0.0 0.5 1.0)))))
              ;; array_string
              (let* ((fams (font-database-families))
                     (ok-str (and (list? fams)
                                  (> (length fams) 0)
                                  (let loop ((l fams))
                                    (or (null? l) (and (string? (car l)) (loop (cdr l))))))))
                ;; array_url + path + array_path + list_tree + list_string via scratch buffers
                (let* ((u1 (new-buffer))
                       (u2 (new-buffer))
                       (u3 (new-buffer))
                       (_ (begin
                            (switch-to-buffer u1)
                            (buffer-set-body u1
                              (stree->tree
                                '(document
                                   (locus (id "gc-stress-id")
                                          (link "stress-link"
                                                (id "gc-stress-id")
                                                (id "gc-stress-other"))
                                          "alpha")
                                   (locus (id "gc-stress-id") "beta")
                                   (concat "gamma" "alpha"))))
                            (update-forced)))
                       (bufs (buffer-list))
                       (ok-url (and (>= (length bufs) 3)
                                    (let loop ((l bufs))
                                      (or (null? l) (and (url? (car l)) (string? (url->string (car l))) (loop (cdr l)))))))
                       (bt (buffer-tree))
                       (ip0 (tree-ip bt))
                       (ip1 (tree-ip (tree-child-ref bt 0)))
                       (hits (tree-search-tree bt "alpha" '() 100))
                       (ok-path (and (int-list? ip0)
                                     (pair? ip0)
                                     (equal? ip1 (cons 0 ip0))
                                     (list? hits)
                                     (>= (length hits) 2)
                                     (let loop ((l hits))
                                       (or (null? l) (and (int-list? (car l)) (loop (cdr l)))))))
                       (id-empty (id->trees "gc-stress-nonexistent-id"))
                       (id-trees (id->trees "gc-stress-id"))
                       (ids-empty (tree->ids (stree->tree '(document "plain"))))
                       (ids-hit (if (pair? id-trees) (tree->ids (car id-trees)) '()))
                       (ltypes (current-link-types))
                       (ok-list (and (null? id-empty)
                                     (null? ids-empty)
                                     (= (length id-trees) 2)
                                     (equal? (map tree->stree id-trees) '("alpha" "beta"))
                                     (equal? ids-hit '("gc-stress-id"))
                                     (list? ltypes)
                                     (member "stress-link" ltypes))))
                  (for-each (lambda (u) (buffer-pretend-saved u) (buffer-close u))
                            (list u1 u2 u3))
                  (stress-assert "test4-other-converters"
                    (and ok-int ok-dbl ok-str ok-url ok-path ok-list (= (chez-root-depth) 0))
                    (list 'int ok-int 'dbl ok-dbl 'str ok-str 'url ok-url
                          'path ok-path 'list ok-list 'depth (chez-root-depth))))))))

        ;; Test 5: Cork transcoder exact byte fidelity & >255 replacement semantics
        (guard (e (else (stress-assert "test5-cork-transcoders" #f (list 'exception (tm-error-message e)))))
          (let* ((bv256 (make-bytevector 256)))
            (do ((i 0 (+ i 1))) ((= i 256))
              (bytevector-u8-set! bv256 i i))
            (let* ((s256 (cork-bytes->string bv256))
                   (rt256 (string->cork-bytes s256))
                   (ctrl-bv #vu8(13 10 0 128 255))
                   (ctrl-s (cork-bytes->string ctrl-bv))
                   (ctrl-rt (string->cork-bytes ctrl-s))
                   (hi-s (string (integer->char #x03BC) #\A (integer->char #x1F600)))
                   (hi-bv (string->cork-bytes hi-s)))
              (stress-assert "test5-cork-transcoders"
                (and (equal? rt256 bv256)
                     (equal? (cork-bytes->string #vu8()) "")
                     (equal? (string->cork-bytes "") #vu8())
                     (= (string-length ctrl-s) 5)
                     (equal? ctrl-rt ctrl-bv)
                     (equal? hi-bv #vu8(63 65 63)))
                (list 'len (string-length s256) 'hi hi-bv)))))

        ;; Test 6: Exception unwind safety
        (guard (e (else (stress-assert "test6-exception-unwind" #f (list 'exception (tm-error-message e)))))
          (let ((r1 (guard (e (else 'caught)) (tree-children 123)))
                (r2 (guard (e (else 'caught)) (named-color->rgba 456)))
                (r3 (guard (e (else 'caught)) (tree-ip "not-a-tree")))
                (ok-after (equal? (tree->stree (stree->tree '(document "ok"))) '(document "ok"))))
            (stress-assert "test6-exception-unwind"
              (and (or (eq? r1 'caught) (and (pair? r1) (eq? (car r1) #t)))
                   (or (eq? r2 'caught) (and (pair? r2) (eq? (car r2) #t)))
                   (or (eq? r3 'caught) (and (pair? r3) (eq? (car r3) #t)))
                   ok-after
                   (= (chez-root-depth) 0))
              (list 'depth (chez-root-depth) 'ok-after ok-after))))

        ;; Test 7: Re-entrancy & memory stability across 20 full-GC rounds
        (guard (e (else (stress-assert "test7-memory-stability" #f (list 'exception (tm-error-message e)))))
          (let ((mem5 #f)
                (mem20 #f)
                (depths-ok #t))
            (do ((round 1 (+ round 1))) ((> round 20))
              (for-each
                (lambda (t)
                  (let* ((s (tree->stree t))
                         (kids (tree-children t))
                         (rt (stree->tree s)))
                    (unless (and (list? kids) (tree? rt))
                      (set! depths-ok #f))))
                bench-docs)
              (collect (collect-maximum-generation))
              (reap-blackboxes!)
              (unless (= (chez-root-depth) 0)
                (set! depths-ok #f))
              (when (= round 5) (set! mem5 (current-memory-bytes)))
              (when (= round 20) (set! mem20 (current-memory-bytes))))
            (stress-assert "test7-memory-stability"
              (and depths-ok mem5 mem20 (<= mem20 (* 2 mem5)) (= (chez-root-depth) 0))
              (list 'mem5 mem5 'mem20 mem20 'total-gcs *gc-fires* 'depth (chez-root-depth)))))

        ;; Test 8: to_scheme_arg / from_scheme_result / call_scheme UTF-8 & ChezRoot stress
        (guard (e (else (stress-assert "test8-call-scheme-utf8-gc" #f (list 'exception (tm-error-message e)))))
          (let* ((s-empty "")
                 (s-ascii "ascii-hello-123")
                 (s-2b "café-μ-αβγ")
                 (s-3b "中文-日本語-한국어")
                 (s-4b (string #\x1F600 #\x1F389 #\x1F680))
                 (unmod (list 'a 1 #t (vector 2 (cons 3 4))))
                 (unmod-res (chez-call-scheme-1 (lambda (x) x) unmod))
                 (ok-share (eq? unmod-res unmod))
                 (make-bv-tree
                   (lambda (tag)
                     (list 'prefix 42 #f
                           (string->utf8 s-empty)
                           (string->utf8 s-ascii)
                           (vector 10 (string->utf8 s-2b) (cons 20 (string->utf8 s-4b)))
                           (cons 1 (cons 2 (string->utf8 s-3b)))
                           (cons 3 (cons 4 (vector 5 (string->utf8 tag)))))))
                 (make-str-tree
                   (lambda (tag)
                     (list 'prefix 42 #f
                           s-empty
                           s-ascii
                           (vector 10 s-2b (cons 20 s-4b))
                           (cons 1 (cons 2 s-3b))
                           (cons 3 (cons 4 (vector 5 tag))))))
                 (expected-str (make-str-tree "tag-ok"))
                 (expected-bv (make-bv-tree "tag-ok"))
                 (in-bv (make-bv-tree "tag-ok"))
                 (seen-ok #f)
                 (out-bv
                   (chez-call-scheme-1
                     (lambda (x)
                       (set! seen-ok (equal? x expected-str))
                       (collect (collect-maximum-generation))
                       x)
                     in-bv))
                 (ok-roundtrip (and seen-ok (equal? out-bv expected-bv)))
                 (apply-seen #f)
                 (apply-args
                   (list 1
                         'sym
                         (string->utf8 s-2b)
                         (list 'a (string->utf8 s-3b) (vector (string->utf8 s-4b)))
                         (cons 9 (string->utf8 s-ascii))
                         (vector (string->utf8 s-empty) (string->utf8 s-2b))))
                 (apply-expected
                   (list 1
                         'sym
                         s-2b
                         (list 'a s-3b (vector s-4b))
                         (cons 9 s-ascii)
                         (vector s-empty s-2b)))
                 (_ (command-apply
                      (object->command
                        (lambda args
                          (set! apply-seen (equal? args apply-expected))
                          (collect (collect-maximum-generation))
                          args))
                      apply-args))
                 (gc0 *gc-fires*)
                 (bulk-ok #t))
            (do ((r 0 (+ r 1))) ((= r 40))
              (let* ((items (map (lambda (i)
                                   (list i 'sym
                                         (vector (string->utf8 s-2b)
                                                 (cons (string->utf8 s-3b)
                                                       (string->utf8 s-4b)))))
                                 (iota-list 60)))
                     (exp-items (map (lambda (i)
                                       (list i 'sym
                                             (vector s-2b (cons s-3b s-4b))))
                                     (iota-list 60)))
                     (got (chez-call-scheme-1
                            (lambda (x)
                              (unless (equal? x exp-items)
                                (set! bulk-ok #f))
                              x)
                            items)))
                (unless (equal? got items)
                  (set! bulk-ok #f))))
            (let ((gc1 *gc-fires*))
              (stress-assert "test8-call-scheme-utf8-gc"
                (and ok-share ok-roundtrip apply-seen bulk-ok
                     (> (- gc1 gc0) 0)
                     (= (chez-root-depth) 0))
                (list 'share ok-share 'rt ok-roundtrip 'apply apply-seen
                      'bulk bulk-ok 'gcs (- gc1 gc0) 'depth (chez-root-depth)))))))
      (lambda ()
        (collect-trip-bytes orig-trip)
        (collect-request-handler orig-crh)))
    (display* "STRESS SUMMARY pass=" *stress-pass* " fail=" *stress-fail*
              " total-gcs=" *gc-fires* " root-depth=" (chez-root-depth)
              " root-cap=" (chez-root-cap) "\n")))

(exec-delayed
  (lambda ()
    (guard (e (else
                (set! *stress-fail* (+ *stress-fail* 1))
                (display* "FAIL top-level " (tm-error-message e) "\n")))
      (run-gc-roots-stress))
    (if (> *stress-fail* 0)
        (exit 1)
        (quit-TeXmacs))))
