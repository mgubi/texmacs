;;; TODO: UNDER REVIEWED AI GENERATED CODE.
;;;
;;; compiler-scan-test.scm -- Tests for compiler-scan.scm.
;;;
;;; Run from the repository root or from src/Scheme/Chez:
;;;   scheme -q compiler-scan-test.scm
;;;
;;; Exits with status 0 when every check passes, 1 otherwise.

(define (first-existing paths)
  (cond ((null? paths) #f)
        ((file-exists? (car paths)) (car paths))
        (else (first-existing (cdr paths)))))

(define compiler-db-path
  (or (first-existing '("compiler-db.scm"
                        "texmacs/src/Scheme/Chez/compiler-db.scm"))
      (error 'compiler-scan-test "cannot find compiler-db.scm")))

(define compiler-scan-path
  (or (first-existing '("compiler-scan.scm"
                        "texmacs/src/Scheme/Chez/compiler-scan.scm"))
      (error 'compiler-scan-test "cannot find compiler-scan.scm")))

(load compiler-db-path)
(import (compiler-db))
(load compiler-scan-path)
(import (compiler-scan))

(define checks 0)
(define failures 0)

(define (check label expected actual)
  (set! checks (+ checks 1))
  (if (equal? expected actual)
      (begin
        (display "PASS ") (display label) (newline))
      (begin
        (set! failures (+ failures 1))
        (display "FAIL ") (display label)
        (display " expected ") (write expected)
        (display " got ") (write actual)
        (newline))))

(define (check-true label actual)
  (check label #t (and actual #t)))

(check "public scanner imported" #t (top-level-bound? 'scan-source-files))
(check "public file-list scanner imported" #t (top-level-bound? 'scan-source-files->db))
(check "internal collector not leaked" #f (top-level-bound? 'scan-collect))
(check "internal walker not leaked" #f (top-level-bound? 'scan-walk))

;;; Temporary source tree -----------------------------------------------------

(define (delete-tree path)
  (cond ((file-directory? path)
         (for-each
          (lambda (name)
            (delete-tree (string-append path "/" name)))
          (directory-list path))
         (delete-directory path))
        ((file-exists? path)
         (delete-file path))))

(define root
  (string-append "/tmp/compiler-scan-test-"
                 (number->string (get-process-id))))

(delete-tree root)
(mkdir root)
(mkdir (string-append root "/test"))

(define (write-source rel text)
  (let ((path (string-append root "/" rel)))
    (with-output-to-file path
      (lambda () (display text)))))

(write-source
 "test/a.scm"
 (string-append
  "(texmacs-module (test a) (:use (test b)))\n"
  "(define-public (foo x) x)\n"
  "(define-public-macro (pm x) `(+ ,x 1))\n"
  "(tm-define (bar) 1)\n"
  "(tm-define (bar) (former))\n"
  "(tm-define-macro (mac x) `(+ ,x 1))\n"))

(write-source
 "test/b.scm"
 (string-append
  "(texmacs-module (test b))\n"
  "(define-public baz 1)\n"
  "(provide-public qux 2)\n"))

(write-source "test/no-header.scm" "(display \"plain\")\n")
(write-source "test/unreadable.scm" "(")
(write-source "test/bad-public.scm"
              "(texmacs-module (test bad))\n(define-public)\n")
(write-source "test/nested.scm"
              (string-append
               "(texmacs-module (test nested))\n"
               "(tm-define ((nested-call a . rest) . args) args)\n"))

;;; Scan ----------------------------------------------------------------------

(define db (scan-source-files->db root '("test/a.scm" "test/b.scm")))
(define modules (compiler-db-modules db))

(check "file-list db count" 2 (length modules))
(check "file-list db a" '(foo pm)
       (module-info-exports (compiler-db-ref db '(test a))))

(check "malformed declaration rejected" #t
       (guard (e (else #t))
         (scan-source-file root "test/bad-public.scm")
         #f))

(check "nested tm-define head" '(nested-call)
       (module-info-global-values
        (scan-source-file root "test/nested.scm")))

(check "module count" 2 (length modules))
(check "first module" '(test a) (module-info-name (car modules)))
(check "second module" '(test b) (module-info-name (cadr modules)))

(define a (compiler-db-ref db '(test a)))
(define b (compiler-db-ref db '(test b)))

(check "a path" "test/a.scm" (module-info-path a))
(check "a exports" '(foo pm) (module-info-exports a))
(check "a global-values" '(bar bar) (module-info-global-values a))
(check "a global-macros" '(mac) (module-info-global-macros a))
(check "a macro-impls" '(mac$impl) (module-info-macro-impls a))
(check "a deps" '((test b)) (module-info-deps a))
(check "a compile-uses" '() (module-info-compile-uses a))
(check "a runtime-uses" '() (module-info-runtime-uses a))
(check-true "a interface-hash" (integer? (module-info-interface-hash a)))
(check-true "a source-hash" (integer? (module-info-source-hash a)))
(check "a artifact-hash" #f (module-info-artifact-hash a))

(check "b exports" '(baz qux) (module-info-exports b))
(check "b deps" '() (module-info-deps b))
(check "b global-values" '() (module-info-global-values b))

;;; Persistence ---------------------------------------------------------------

(define db-path (string-append root "/compiler-db.fasl"))
(write-compiler-db db db-path)
(check "db exists" #t (file-exists? db-path))

(define db2 (read-compiler-db db-path))
(check "roundtrip count" 2 (length (compiler-db-modules db2)))
(check "roundtrip a exports" '(foo pm)
       (module-info-exports (compiler-db-ref db2 '(test a))))
(check "roundtrip a globals" '(bar bar)
       (module-info-global-values (compiler-db-ref db2 '(test a))))

;;; Cleanup and summary -------------------------------------------------------

(delete-tree root)

(display "------------------------------------------------------------") (newline)
(display checks) (display " checks, ")
(display failures) (display " failures") (newline)
(exit (if (= failures 0) 0 1))
