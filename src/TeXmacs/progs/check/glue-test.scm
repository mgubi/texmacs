
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : glue-test.scm
;; DESCRIPTION : tests of the glue between C++ and Scheme
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The glue (src/Scheme/Glue) makes the C++ functions listed in the files
;; build-glue-*.scm available in Scheme, each with the types of its result
;; and arguments. The tests read these lists from the source tree and check
;;
;;   - that each function is bound to a procedure with the declared number
;;     of arguments;
;;   - that a function refuses a first argument of the wrong type with a
;;     wrong-type-arg error, before any C++ code runs (every argument is
;;     checked before the call, so this is safe on all of them);
;;   - that values cross the glue unchanged: trees and Scheme trees,
;;     strings with any byte, integers up to the limits of a C int, paths,
;;     urls, lists of strings, booleans and doubles.
;;
;; The checks are those of check-lib: every check runs, and
;; glue-test-failures returns the number of failures.

(texmacs-module (check glue-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checks (check-lib), named by group as the tables of this file need
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (glue-group name) (check-group name))
(define (glue-try thunk) (check-run thunk))

(define (glue-check group what ok? detail)
  (check-report ok? what detail))

(define (glue-check-equal group what thunk expected)
  (check-equal what thunk expected))

(define (glue-check-error group what thunk key)
  (check-raises what thunk key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The declarations of the glue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define glue-spec-files
  '("build-glue-basic.scm" "build-glue-editor.scm" "build-glue-server.scm"))

(define (glue-spec-dir)
  (url-append (url-head (system->url "$TEXMACS_PATH"))
              "src/Scheme/Glue"))

(define (glue-read-forms file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((acc '()))
        (with form (read port)
          (if (eof-object? form) (reverse acc) (loop (cons form acc))))))))

;; the entries (scheme-name cpp-name (result-type argument-type ...)) of the
;; build forms of the glue files, or #f outside of a source tree
(define (glue-declarations)
  (let ((dir (glue-spec-dir)))
    (and (url-exists? (url-append dir (car glue-spec-files)))
         (append-map
          (lambda (name)
            (let ((forms (glue-read-forms
                          (url->system (url-append dir name)))))
              (append-map (lambda (form)
                            (if (and (pair? form) (== (car form) 'build))
                                (filter (lambda (e)
                                          (and (list? e) (= (length e) 3)
                                               (symbol? (car e))
                                               (list? (caddr e))))
                                        (cdddr form))
                                '()))
                          forms)))
          glue-spec-files))))

(define (glue-argument-types e) (cdr (caddr e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings and number of arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define glue-redefined '())

;; the procedure bound to a glue name, as the glue installed it: a name
;; which Scheme code has redefined (tm-define) is a closure, not the glue
;; primitive, and is left out of the checks of the primitive
(define (glue-primitive name)
  (and (defined? name)
       (with f (eval name)
         (and (procedure? f) (not (closure? f)) f))))

(define (test-glue-bindings decls)
  (for (e decls)
    (let* ((name (car e))
           (nr (length (glue-argument-types e))))
      (cond ((not (defined? name))
             (glue-check "bindings" (symbol->string name) #f "not bound"))
            ((not (procedure? (eval name)))
             (glue-check "bindings" (symbol->string name) #f
                         "not a procedure"))
            ((closure? (eval name))
             (set! glue-redefined (cons name glue-redefined)))
            (else
             (with arity (procedure-property (eval name) 'arity)
               (glue-check "bindings" (symbol->string name)
                           (equal? arity (list nr 0 #f))
                           (string-append "declared with "
                                          (number->string nr)
                                          " arguments, arity "
                                          (object->string arity)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments of the wrong type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for each type whose argument is checked, a value which the check refuses
;; FIXME: a uint is not checked for its sign (TMSCM_ASSERT_UINT, #72)
(define glue-wrong-values
  '((string . 42)
    (int . "x")
    (uint . "x")
    (double . "x")
    (bool . "x")
    (url . 42)
    (path . 42)
    (tree . "x")
    (content . 42)
    (tree_label . "x")
    (array_string . 42)
    (array_url . 42)
    (array_path . 42)
    (array_tree . 42)
    (array_int . 42)
    (array_double . 42)
    (modification . 42)
    (patch . 42)))

(define (test-glue-wrong-types decls)
  (for (e decls)
    (let* ((name (car e))
           (types (glue-argument-types e))
           (f (glue-primitive name))
           (wrong (and (nnull? types) (assoc (car types) glue-wrong-values))))
      (when (and f wrong)
        (let ((args (cons (cdr wrong) (map (lambda (t) #f) (cdr types)))))
          (glue-check-error "types"
                            (string-append (symbol->string name) " with a "
                                           (symbol->string (car types))
                                           " of the wrong type")
                            (lambda () (apply f args))
                            'wrong-type-arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Values across the glue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scheme trees become C++ trees and back without change
(define (test-glue-trees)
  (for (st (list "" "abc" "a b\tc\n" "<alpha>" "\xe9t\xe9"
                 '(concat) '(concat "a") '(concat "a" (frac "x" "y"))
                 '(document (section "A") (concat "x" (rsup "2")))
                 '(with "color" "red" "text")
                 '(tuple (tuple (tuple "deep")))
                 '(foo "an" "unknown" "tag")))
    (glue-check-equal "trees" (object->string st)
                      (lambda () (tree->stree (stree->tree st))) st))
  (glue-check-equal "trees" "label" (lambda () (tree-label (stree->tree
                                                    '(frac "a" "b"))))
                    'frac)
  (glue-check-equal "trees" "arity" (lambda () (tree-arity (stree->tree
                                                    '(concat "a" "b" "c"))))
                    3)
  ;; content may be given as a string, a tree or a Scheme tree
  (for (c (list "abc" (stree->tree '(concat "a" "b")) '(concat "a" "b")))
    (glue-check-equal "trees" (string-append "content " (object->string c))
                      (lambda () (tree->stree (tm->tree c)))
                      (if (string? c) c '(concat "a" "b")))))

;; strings cross the glue byte for byte, NUL included
(define (test-glue-strings)
  (let ((all (list->string (map integer->char (.. 0 256)))))
    (glue-check-equal "strings" "every byte"
                      (lambda () (tree->string (string->tree all))) all)
    (glue-check-equal "strings" "every byte, search"
                      (lambda () (string-search-forwards
                                  (string (integer->char 0)) 0 all))
                      0))
  (glue-check-equal "strings" "empty" (lambda () (tree->string
                                                  (string->tree "")))
                    "")
  (glue-check-equal "strings" "utf8 and cork"
                    (lambda () (cork->utf8 (utf8->cork "caf\xc3\xa9 \xce\xb1")))
                    "caf\xc3\xa9 \xce\xb1")
  (glue-check-equal "strings" "list of strings"
                    (lambda () (cpp-string-tokenize "a,b,,c" ","))
                    '("a" "b" "" "c"))
  (glue-check-equal "strings" "empty list of strings"
                    (lambda () (cpp-string-tokenize "" ",")) '("")))

;; integers up to the limits of a C int, and an error beyond them
(define (test-glue-integers)
  (for (n (list 0 1 -1 255 65536 2147483647 -2147483647 -2147483648))
    (glue-check-equal "integers" (number->string n)
                      (lambda () (hexadecimal->integer
                                  (integer->hexadecimal n)))
                      n))
  (glue-check-equal "integers" "hexadecimal" (lambda () (integer->hexadecimal
                                                         255))
                    "FF")
  (glue-check-error "integers" "2^31" (lambda () (integer->hexadecimal
                                                  2147483648))
                    'out-of-range)
  (glue-check-error "integers" "-2^31-1" (lambda () (integer->hexadecimal
                                                     -2147483649))
                    'out-of-range)
  (glue-check-error "integers" "a double for an int"
                    (lambda () (integer->hexadecimal 1.5)) 'wrong-type-arg))

;; booleans come back as #t and #f, doubles as numbers
(define (test-glue-booleans-doubles)
  (glue-check-equal "booleans" "true" (lambda () (version-before? "1.0" "2.0"))
                    #t)
  (glue-check-equal "booleans" "false" (lambda () (version-before? "2.0"
                                                                   "1.0"))
                    #f)
  (glue-check-error "booleans" "a string for a bool"
                    (lambda () (version-before? "1.0" 2)) 'wrong-type-arg)
  (glue-check "doubles" "a double result"
              (real? (glue-try (lambda () (font-master-guessed-distance
                                            "roman" "roman")))) "not a number"))

;; paths and urls
(define (test-glue-paths-urls)
  (glue-check-equal "paths" "strip" (lambda () (path-strip '(1 2 3) '(1)))
                    '(2 3))
  (glue-check-equal "paths" "empty" (lambda () (path-strip '() '())) '())
  (glue-check-error "paths" "a list of strings for a path"
                    (lambda () (path-strip '("a") '())) 'wrong-type-arg)
  ;; a number and an improper list used to crash TeXmacs
  (glue-check-error "paths" "a number for a path"
                    (lambda () (path-strip 5 '())) 'wrong-type-arg)
  (glue-check-error "paths" "an improper list for a path"
                    (lambda () (path-strip '(1 . 2) '())) 'wrong-type-arg)
  (glue-check-error "paths" "a string for a path"
                    (lambda () (path-strip "x" '())) 'wrong-type-arg)
  ;; (an absolute name: /abs/path.tm, or /c/abs/path.tm which is c:/abs/path.tm
  ;; on Windows; check-unix writes the name given back with /)
  (for (p (list (cons "a.tm" "a.tm") (cons "a/b/c.tm" "a/b/c.tm")
                (cons (check-unix-abs "abs/path.tm") (check-abs "abs/path.tm"))
                (cons "a b.tm" "a b.tm") (cons "" "")))
    (glue-check-equal "urls" (car p)
                      (lambda () (check-unix (url->string (string->url (car p)))))
                      (cdr p)))
  (glue-check-equal "urls" "a string for an url"
                    (lambda () (check-unix (url->string "x/y.tm"))) "x/y.tm")
  ;; system-mkdir of no url, and so run-test-suite with a name which is not
  ;; a directory, used to crash TeXmacs (#163)
  (glue-check-equal "urls" "system-mkdir of no url"
                    (lambda () (system-mkdir (url-none)) #t) #t)
  (glue-check-error "urls" "run-test-suite of a name which is not a directory"
                    (lambda () (run-test-suite "no-such-test-suite-dir"))
                    'texmacs-error))

;; processes: evaluate-system sends its inputs to the file descriptors of
;; the process and reads its outputs. An empty input used to be left open,
;; so that a process which reads it to its end never stopped, nor TeXmacs,
;; which waits for it (openssl passwd -stdin of an empty password). A long
;; input is sent in pieces of 4096 bytes on Unix.
;; On Windows too (with the cat of MSYS2): the process gets its pipes as its
;; standard handles, so it sees the end of its input.
(define (test-glue-processes)
  (if (not (url-exists-in-path? "cat"))
      (display "  no cat command, skipped\n")
      ;; (on Unix, cat is stopped after 20 s when perl is at hand: the check
      ;; then fails, instead of TeXmacs waiting for cat forever)
      (let* ((guard (if (and (not (or (os-mingw?) (os-win32?)))
                             (url-exists-in-path? "perl"))
                        '("perl" "-e" "alarm 20; exec @ARGV") '()))
             (cat (lambda (in) (evaluate-system (append guard '("cat"))
                                                '(0) (list in) '(1 2))))
             (long (make-string 100000 #\a)))
        (glue-check-equal "processes" "an input"
                          (lambda () (cat "abc")) '("0" "abc" ""))
        (glue-check-equal "processes" "an empty input"
                          (lambda () (cat "")) '("0" "" ""))
        (glue-check-equal "processes" "a long input"
                          (lambda () (cat long)) (list "0" long "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (glue-test-failures)
  (:synopsis "Run the tests of the glue and return the number of failures")
  (set! glue-redefined '())
  (check-suite "the glue")
  (with decls (glue-declarations)
    (if (not decls)
        (display "  the glue declarations are not at hand, skipped\n")
        (begin
          (display* "  " (number->string (length decls)) " declarations\n")
          (glue-group "bindings")
          (test-glue-bindings decls)
          (glue-group "types")
          (test-glue-wrong-types decls))))
  (glue-group "trees")
  (test-glue-trees)
  (glue-group "strings")
  (test-glue-strings)
  (glue-group "integers")
  (test-glue-integers)
  (glue-group "booleans and doubles")
  (test-glue-booleans-doubles)
  (glue-group "paths and urls")
  (test-glue-paths-urls)
  (glue-group "processes")
  (test-glue-processes)
  (when (nnull? glue-redefined)
    (display* "  redefined in Scheme, not checked: "
              (object->string (reverse glue-redefined)) "\n"))
  (check-end))

(tm-define (regtest-glue)
  (with n (glue-test-failures)
    (if (> n 0)
        (error "Regression failure:" "glue" n)
        (display "Test suite of the glue: ok\n"))))
