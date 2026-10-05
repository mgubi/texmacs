
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : check-lib.scm
;; DESCRIPTION : checks which count their failures, for test suites
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A test suite written with these checks runs all of them, reports each
;; failure with the expression which failed, and returns the number of
;; failures, where regression-test-group stops at the first one:
;;
;;   (tm-define (lists-test-failures)
;;     (check-suite "lists")
;;     (check-group "sublists")
;;     (check= (sublist '(a b c d) 1 3) '(b c))
;;     (check-true (list-find '(1 2 3) even?))
;;     (check-error (car '()) #t)
;;     (check-end))
;;
;; By convention the suite of foo-test.scm is foo-test-failures, which
;; tests/scheme/check.sh can run from the file itself or, once the suite is
;; listed in check-master.scm, by its name.

(texmacs-module (check check-lib))

(define check-suite-name "")
(define check-group-name "")
(define check-count 0)
(define check-failure-count 0)

(tm-define (check-suite name)
  (:synopsis "Start the test suite @name")
  (set! check-suite-name name)
  (set! check-group-name "")
  (set! check-count 0)
  (set! check-failure-count 0)
  (display* "Test suite of " name "\n")
  (force-output))

(tm-define (check-root)
  (:synopsis "The root of the absolute file names of the checks")
  ;; a name which starts with / is relative on Windows, whose absolute
  ;; names start with a drive
  (if (or (os-mingw?) (os-win32?)) "c:/" "/"))

(tm-define (check-abs s)
  (:synopsis "The absolute file name @s (without its first /) of the checks")
  (string-append (check-root) s))

(tm-define (check-unix-abs s)
  (:synopsis "The absolute name @s (without its first /) in the syntax of urls")
  ;; the syntax of string->url, that of Unix: /c/a is c:/a on Windows (as
  ;; in MSYS), where c:/a would be the alternatives c and /a
  (string-append (if (or (os-mingw?) (os-win32?)) "/c/" "/") s))

(tm-define (check-unix s)
  (:synopsis "The name @s given back by TeXmacs, written as on Unix")
  ;; with / between the parts of a name and : between alternatives, which
  ;; are \ and ; on Windows
  (if (or (os-mingw?) (os-win32?))
      (string-replace (string-replace s "\\" "/") ";" ":")
      s))

(tm-define (check-group name)
  (:synopsis "Start the group @name of checks")
  ;; the output is flushed, so that a crash shows the group it was in
  (set! check-group-name name)
  (display* "  group " name "\n")
  (force-output))

(tm-define (check-report ok? what detail)
  (:synopsis "Count a check of @what, a failure with @detail unless @ok?")
  (set! check-count (+ check-count 1))
  (when (not ok?)
    (set! check-failure-count (+ check-failure-count 1))
    (display* "  FAILED [" check-group-name "] " what ": " detail "\n")
    (force-output))
  ok?)

(tm-define (check-run thunk)
  (:synopsis "The value of @thunk, or (error key) when it raises an error")
  (catch #t
    (lambda () (thunk))
    (lambda (key . args) (list 'error key))))

(tm-define (check-equal what thunk expected)
  (let ((result (check-run thunk)))
    (check-report (equal? result expected) what
                  (string-append "expected " (object->string expected)
                                 ", got " (object->string result)))))

(tm-define (check-predicate what thunk pred? pred-name)
  (let ((result (check-run thunk)))
    (check-report (and (not (and (pair? result) (== (car result) 'error)))
                       (pred? result))
                  what
                  (string-append "got " (object->string result)
                                 ", which is not " pred-name))))

(tm-define (check-raises what thunk key)
  ;; key #t accepts any error
  (let ((result (check-run thunk)))
    (check-report (and (pair? result) (== (car result) 'error)
                       (or (== key #t) (== (cadr result) key)))
                  what
                  (string-append "expected the error "
                                 (object->string key) ", got "
                                 (object->string result)))))

(define-public-macro (check= expr expected)
  `(check-equal ,(object->string expr) (lambda () ,expr) ,expected))

(define-public-macro (check-true expr)
  `(check-predicate ,(object->string expr) (lambda () ,expr)
                    (lambda (x) x) "true"))

(define-public-macro (check-false expr)
  `(check-equal ,(object->string expr) (lambda () ,expr) #f))

(define-public-macro (check-error expr key)
  `(check-raises ,(object->string expr) (lambda () ,expr) ,key))

(tm-define (check-isolate-git)
  (:synopsis "Ignore the global and system configurations of Git")
  ;; NOTE: tests/scheme/check.sh sets them for all suites already; they
  ;; cannot be unset afterwards (the empty value also means no global
  ;; configuration), so that they stay for the rest of the process.
  ;; GIT_CONFIG_GLOBAL needs Git 2.32 or newer.
  (system-setenv "GIT_CONFIG_GLOBAL" "/dev/null")
  (system-setenv "GIT_CONFIG_NOSYSTEM" "1")
  (with v (string->list (eval-system "git --version 2>/dev/null"))
    (with n (map string->number
                 (string-tokenize-by-char
                  (list->string (list-filter v (lambda (c)
                                                 (or (char-numeric? c)
                                                     (== c #\.)))))
                  #\.))
      (when (and (>= (length n) 2) (car n) (cadr n)
                 (or (< (car n) 2) (and (== (car n) 2) (< (cadr n) 32))))
        (display* "  warning: Git older than 2.32 reads the global "
                  "configuration of the user\n")))))

(tm-define (check-end)
  (:synopsis "End the test suite and return its number of failures")
  (display* "Total: " (number->string check-count) " checks, "
            (number->string check-failure-count) " failed\n")
  (force-output)
  check-failure-count)
