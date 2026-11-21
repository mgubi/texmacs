;;; TODO: UNDER REVIEWED AI GENERATED CODE.
;;;
;;; compiler-db-test.scm -- Tests for compiler-db.scm.
;;;
;;; Run from this directory:
;;;   scheme -q compiler-db-test.scm
;;;
;;; Exits with status 0 when every check passes, 1 otherwise.

(define (first-existing paths)
  (cond ((null? paths) #f)
        ((file-exists? (car paths)) (car paths))
        (else (first-existing (cdr paths)))))

(define compiler-db-path
  (or (first-existing '("compiler-db.scm"
                        "texmacs/src/Scheme/Chez/compiler-db.scm"))
      (error 'compiler-db-test
             "cannot find compiler-db.scm; run from the repository root or from src/Scheme/Chez")))

(load compiler-db-path)
(import (compiler-db))

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

(define (expect-error label thunk)
  (set! checks (+ checks 1))
  (guard (e (else
             (display "PASS ") (display label) (newline)))
    (thunk)
    (set! failures (+ failures 1))
    (display "FAIL ") (display label)
    (display " (no error)") (newline)))

(check "public constructor imported" #t (top-level-bound? 'make-module-info))
(check "public updater imported" #t (top-level-bound? 'compiler-db-set!))
(check "internal validator not leaked" #f (top-level-bound? 'validate-module-info))
(check "internal sorter not leaked" #f (top-level-bound? 'compiler-db-sorted))
(check "old add! not leaked" #f (top-level-bound? 'compiler-db-add!))
(check "module fields not mutable" #f (top-level-bound? 'set-module-info-exports!))

(define m1
  (make-module-info
   '(database db-base)
   "database/db-base.scm"
   '(db-search db-get-entry)
   '(global-database current-database)
   '(with-database with-time)
   '(with-database$impl with-time$impl)
   '((utils library tree))
   '(with-database$impl)
   '(db-search)
   #x1111
   #x2222
   #x3333
   1700000000))

(define m2
  (make-module-info
   '(utils library tree)
   "utils/library/tree.scm"
   '(tree-get tree-set)
   '()
   '()
   '()
   '()
   '()
   '(tree-get)
   #x4444
   #x5555
   #f
   1700000001))

;;; Field accessors -----------------------------------------------------------

(check "record predicate" #t (module-info? m1))
(check "name" '(database db-base) (module-info-name m1))
(check "path" "database/db-base.scm" (module-info-path m1))
(check "exports" '(db-search db-get-entry) (module-info-exports m1))
(check "global-values" '(global-database current-database) (module-info-global-values m1))
(check "global-macros" '(with-database with-time) (module-info-global-macros m1))
(check "macro-impls" '(with-database$impl with-time$impl) (module-info-macro-impls m1))
(check "deps" '((utils library tree)) (module-info-deps m1))
(check "compile-uses" '(with-database$impl) (module-info-compile-uses m1))
(check "runtime-uses" '(db-search) (module-info-runtime-uses m1))
(check "interface-hash" #x1111 (module-info-interface-hash m1))
(check "source-hash" #x2222 (module-info-source-hash m1))
(check "artifact-hash" #x3333 (module-info-artifact-hash m1))
(check "built-at" 1700000000 (module-info-built-at m1))

;;; Duplicate policy ----------------------------------------------------------

(expect-error "duplicate exports"
  (lambda ()
    (make-module-info '(a) "a.scm" '(x x) '() '() '() '() '() '() #f #f #f #f)))

;; tm-define supports overloads and conditional definitions, so duplicates are
;; legitimate and must be preserved.
(define duplicate-globals
  (make-module-info '(a) "a.scm" '() '(x x) '() '() '() '() '() #f #f #f #f))
(check "duplicate global-values allowed" '(x x)
       (module-info-global-values duplicate-globals))

(expect-error "duplicate global-macros"
  (lambda ()
    (make-module-info '(a) "a.scm" '() '() '(x x) '() '() '() '() #f #f #f #f)))

(expect-error "duplicate macro-impls"
  (lambda ()
    (make-module-info '(a) "a.scm" '() '() '() '(x x) '() '() '() #f #f #f #f)))

(expect-error "duplicate compile-uses"
  (lambda ()
    (make-module-info '(a) "a.scm" '() '() '() '() '() '(x x) '() #f #f #f #f)))

(expect-error "duplicate runtime-uses"
  (lambda ()
    (make-module-info '(a) "a.scm" '() '() '() '() '() '() '(x x) #f #f #f #f)))

(expect-error "duplicate deps"
  (lambda ()
    (make-module-info '(a) "a.scm" '() '() '() '() '((b) (b)) '() '() #f #f #f #f)))

(expect-error "bad name"
  (lambda ()
    (make-module-info '() "a.scm" '() '() '() '() '() '() '() #f #f #f #f)))

(expect-error "bad global-values type"
  (lambda ()
    (make-module-info '(a) "a.scm" '() '(1 2) '() '() '() '() '() #f #f #f #f)))

;;; Database queries ----------------------------------------------------------

(define db
  (make-compiler-db (scheme-version)
                    1700000002
                    (list m2 m1)))

(check "db modules" 2 (length (compiler-db-modules db)))
(check "db ref m1" m1 (compiler-db-ref db '(database db-base)))
(check "db ref missing" #f (compiler-db-ref db '(no such)))
(check "db compatible" #t (compiler-db-compatible? db))

(expect-error "duplicate module names"
  (lambda ()
    (make-compiler-db (scheme-version)
                      1700000002
                      (list m1 m1))))

;;; Fasl persistence ----------------------------------------------------------

(define test-path
  (string-append "/tmp/compiler-db-test-"
                 (number->string (get-process-id))
                 ".fasl"))

(when (file-exists? test-path)
  (delete-file test-path))

(write-compiler-db db test-path)
(check "file exists" #t (file-exists? test-path))
(check "temp removed" #f (file-exists? (string-append test-path ".tmp")))

(define db2 (read-compiler-db test-path))
(check "roundtrip modules" 2 (length (compiler-db-modules db2)))
(check "roundtrip first name" '(database db-base)
       (module-info-name (car (compiler-db-modules db2))))
(check "roundtrip exports" '(db-search db-get-entry)
       (module-info-exports (compiler-db-ref db2 '(database db-base))))
(check "roundtrip artifact" #x3333
       (module-info-artifact-hash (compiler-db-ref db2 '(database db-base))))
(check "roundtrip #f artifact" #f
       (module-info-artifact-hash (compiler-db-ref db2 '(utils library tree))))

(check "missing file" #f
       (read-compiler-db "/tmp/no-such-compiler-db.fasl"))
(check "missing or-empty modules" 0
       (length (compiler-db-modules
                (read-compiler-db-or-empty "/tmp/no-such-compiler-db.fasl"))))

;;; Mutable module list -------------------------------------------------------

(define db3 (make-empty-compiler-db))
(compiler-db-set! db3 m1)
(check "set inserts" 1 (length (compiler-db-modules db3)))
(check "set ref" m1 (compiler-db-ref db3 '(database db-base)))

(define replacement
  (make-module-info '(database db-base) "database/db-base.scm"
                    '(db-search) '(global-database) '() '() '() '() '()
                    #x9999 #x8888 #f #f))

(compiler-db-set! db3 replacement)
(check "set replaces" 1 (length (compiler-db-modules db3)))
(check "set exports" '(db-search)
       (module-info-exports (compiler-db-ref db3 '(database db-base))))

(define new-module
  (make-module-info '(new module) "new/module.scm" '(n) '() '() '() '() '() '()
                    #f #f #f #f))
(compiler-db-set! db3 new-module)
(check "set appends" 2 (length (compiler-db-modules db3)))
(check "set append ref" new-module (compiler-db-ref db3 '(new module)))

(define corrupt-path (string-append test-path ".corrupt"))
(when (file-exists? corrupt-path)
  (delete-file corrupt-path))
(with-output-to-file corrupt-path
  (lambda () (display "this is not a fasl file")))
(check "corrupt or-empty modules" 0
       (length (compiler-db-modules (read-compiler-db-or-empty corrupt-path))))

;;; Cleanup and summary -------------------------------------------------------

(when (file-exists? test-path)
  (delete-file test-path))
(when (file-exists? corrupt-path)
  (delete-file corrupt-path))

(display "------------------------------------------------------------") (newline)
(display checks) (display " checks, ")
(display failures) (display " failures") (newline)
(exit (if (= failures 0) 0 1))
