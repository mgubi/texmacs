
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : r5rs-femtolisp.scm
;; DESCRIPTION : standard Scheme (R5RS and the Guile basics) on femtolisp
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loaded first by init-femtolisp.scm, with the load of femtolisp, before the
;; module system: the definitions are global. Femtolisp is a Scheme-like Lisp
;; with its own names (aref, string.sub, table...); this file gives it the
;; names and the behavior of R5RS and Guile 1.8 which TeXmacs relies on.
;; Strings are strings of bytes and characters are bytes, as in Guile 1.8
;; (the byte string functions are in Scheme/Femtolisp/fl_core.c).

;; write as Guile: on one line, and labels only for cycles
(set! *print-pretty* #f)
(set! *print-shared* #f)
(set! *print-closures* #f)

;; the compiled functions keep their source (procedure-source), which TeXmacs
;; inspects (the actions of the menus, for instance)
(set! *keep-source* (not (os.getenv "TM_NOSRC")))

;; with TEXMACS_FL_EAGER (function bodies expanded at load, see
;; boot-femtolisp.scm), the errors of the macro expansions are raised when the
;; code runs, as with Guile, which expands the macros when it first evaluates
;; them; lazy function bodies are expanded at their first call anyway
(set! *defer-macro-errors* (if (os.getenv "TEXMACS_FL_EAGER") #t #f))

;; femtolisp ignores set! of its builtins (constants): (define-override ...)
;; makes the name redefinable first, when it is expanded (the compiler
;; compiles the set! of a constant as nothing)
(define-macro (define-override head . body)
  (let ((name (if (pair? head) (car head) head)))
    (%unconstant! name)
    `(define ,head ,@body)))

;; TEXMACS_FL_TRACE=1: the errors which reach the C++ code are reported with
;; their stack; TEXMACS_FL_TRACE=catch: also the errors caught by catch
(define %trace-errors? (os.getenv "TEXMACS_FL_TRACE"))

;; the femtolisp versions of builtins which are redefined below
(define %fl-string string)
(define %fl-symbol? symbol?)
(define %fl-integer? integer?)
(define %fl-truncate truncate)
(define %fl-atan atan)
(define %fl-write write)
(define %fl-read read)
(define %fl-eval eval)
(define %fl-load load)
(define %fl-iota iota)
(define %fl-keyword? keyword?)
(define %fl-error error)
(define %fl-hash hash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols and keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; as in Guile with (read-set! keywords 'prefix), :foo is a keyword and not a
;; symbol (femtolisp also takes foo: for a keyword)
(define-override keyword? %keyword?) ;; in C: no allocation

;; (the unspecified value of femtolisp is the symbol #<unspecified>)
(define-override symbol? %symbol?) ;; in C
(define (unspecified? x) (eq? x (if #f #f)))

(define (symbol->string s) (%fl-string s))
(define (string->symbol s) (symbol s))
(define (symbol-append . l) (symbol (apply %fl-string l)))
(define (keyword->symbol k) (symbol (substring (%fl-string k) 1)))
(define (symbol->keyword s) (symbol (%fl-string ":" s)))
(define (symbol<? a b) (< (%string-compare (%fl-string a) (%fl-string b)) 0))

(define %gensym-counter 0)
(define-override (gensym . prefix)
  (set! %gensym-counter (+ %gensym-counter 1))
  (symbol (%fl-string (if (pair? prefix) (car prefix) " g")
                      %gensym-counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; femtolisp has exact integers (fixnums, then 64 bits) and doubles

(define (exact? x) (%fl-integer? x))
(define (inexact? x) (and (number? x) (not (%fl-integer? x))))
(define (exact-integer? x) (%fl-integer? x))
(define (real? x) (number? x))
(define (rational? x) (and (number? x) (not (nan? x))
                           (or (%fl-integer? x) (< (abs x) +inf.0))))
(define-override (integer? x)
  (and (number? x) (or (%fl-integer? x) (integer-valued? x))))
(define (exact-nonnegative-integer? x) (and (%fl-integer? x) (>= x 0)))
(define (inf? x) (and (number? x) (or (= x +inf.0) (= x -inf.0))))

(define (exact->inexact x) (double x))
(define (inexact->exact x)
  (cond ((%fl-integer? x) x)
        ((integer-valued? x) (%fl-truncate x))
        (else x))) ;; no rationals: the closest is the number itself
(define exact inexact->exact)
(define inexact exact->inexact)

(define (%integral x) ;; x is a double
  (or (nan? x) (inf? x) (>= (abs x) 4.5e15)))

(define-override (truncate x)
  (cond ((%fl-integer? x) x)
        ((%integral x) x)
        (else (double (%fl-truncate x)))))
(define (floor x)
  (if (%fl-integer? x) x
      (let ((t (truncate x))) (if (< x t) (- t 1.0) t))))
(define (ceiling x)
  (if (%fl-integer? x) x
      (let ((t (truncate x))) (if (> x t) (+ t 1.0) t))))
(define (round x)
  (if (%fl-integer? x) x
      (let* ((f (floor x)) (d (- x f)))
        (cond ((< d 0.5) f)
              ((> d 0.5) (+ f 1.0))
              ((even? (%fl-truncate f)) f)
              (else (+ f 1.0))))))

(define (quotient a b)
  (if (and (%fl-integer? a) (%fl-integer? b))
      (div0 a b)
      (truncate (/ a b))))
(define (remainder a b)
  (if (and (%fl-integer? a) (%fl-integer? b))
      (mod0 a b)
      (- a (* b (quotient a b)))))
(define (modulo a b)
  (let ((r (remainder a b)))
    (if (and (not (zero? r)) (not (eq? (negative? r) (negative? b))))
        (+ r b)
        r)))

(define (gcd . l)
  (define (gcd2 a b) (if (zero? b) (abs a) (gcd2 b (remainder a b))))
  (if (null? l) 0 (foldl (lambda (x acc) (gcd2 acc x)) (car l) (cdr l))))
(define (lcm . l)
  (define (lcm2 a b)
    (if (or (zero? a) (zero? b)) 0 (abs (quotient (* a b) (gcd a b)))))
  (if (null? l) 1 (foldl (lambda (x acc) (lcm2 acc x)) (car l) (cdr l))))

(define pi (* 4 (%fl-atan 1.0)))

(define-override (atan y . opt)
  (if (null? opt) (%fl-atan y)
      (let ((x (car opt)))
        (cond ((> x 0) (%fl-atan (/ y x)))
              ((< x 0) (if (>= y 0)
                           (+ (%fl-atan (/ y x)) pi)
                           (- (%fl-atan (/ y x)) pi)))
              ((> y 0) (/ pi 2))
              ((< y 0) (- (/ pi 2)))
              (else 0.0)))))

(define (expt b e)
  (cond ((and (%fl-integer? e) (>= e 0))
         (let loop ((b b) (e e) (r 1))
           (cond ((= e 0) r)
                 ((odd? e) (loop (* b b) (div0 e 2) (* r b)))
                 (else (loop (* b b) (div0 e 2) r)))))
        ((%fl-integer? e) (/ 1 (expt b (- e))))
        ((zero? b) (if (zero? e) 1.0 0.0))
        (else (exp (* e (log b))))))

(define (exact-integer-sqrt n)
  (let ((r (%fl-truncate (sqrt n)))) (values r (- n (* r r)))))
(define (square x) (* x x))

;; as Guile: an exact integer in [0, n) for an exact n, else a double
(define-override (random n . state)
  (if (%fl-integer? n)
      (if (<= n 0) (error "random: bad range" n) (mod (rand) n))
      (* (rand.double) n)))

;; complex numbers, #(%complex re im), which + - * / = handle through the
;; hook *arith-fallback* of femtolisp (patch 0020); a complex number with a
;; zero imaginary part is a real number
(define (%complex? x)
  (and (vector? x) (= (length x) 3) (eq? (aref x 0) '%complex)))
(define (make-rectangular re im)
  (if (and (number? im) (= im 0)) re (vector '%complex re im)))
(define (make-polar r a) (make-rectangular (* r (cos a)) (* r (sin a))))
(define (real-part z) (if (%complex? z) (aref z 1) z))
(define (imag-part z) (if (%complex? z) (aref z 2) 0))
(define (magnitude z)
  (if (%complex? z)
      (sqrt (+ (* (aref z 1) (aref z 1)) (* (aref z 2) (aref z 2))))
      (abs z)))
(define (angle z)
  (if (%complex? z)
      (atan (aref z 2) (aref z 1))
      (if (< z 0) pi 0)))
(define (complex? x) (or (number? x) (%complex? x)))

(define (%complex-mul x y)
  (let ((a (real-part x)) (b (imag-part x)) (c (real-part y)) (d (imag-part y)))
    (make-rectangular (- (* a c) (* b d)) (+ (* a d) (* b c)))))
(define (%complex-div x y)
  (let* ((a (real-part x)) (b (imag-part x)) (c (real-part y)) (d (imag-part y))
         (n (+ (* c c) (* d d))))
    (if (= n 0) (error "/: division by zero"))
    (make-rectangular (/ (+ (* a c) (* b d)) n) (/ (- (* b c) (* a d)) n))))

(set! *arith-fallback*
  (lambda (op args)
    (if (not (every complex? args))
        (raise (list 'type-error op 'number
                     (car (filter (lambda (x) (not (complex? x))) args)))))
    (case op
      ((+) (make-rectangular (apply + (map real-part args))
                             (apply + (map imag-part args))))
      ((-) (make-rectangular (- (real-part (car args)))
                             (- (imag-part (car args)))))
      ((*) (foldl (lambda (y acc) (%complex-mul acc y)) 1 args))
      ((/) (%complex-div (car args) (cadr args)))
      ((=) (and (= (real-part (car args)) (real-part (cadr args)))
                (= (imag-part (car args)) (imag-part (cadr args)))))
      (else (raise (list 'type-error op 'number args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Characters (bytes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char->integer c) (fixnum c))
(define (integer->char i) (wchar i))

(define (%char-compare cmp)
  (lambda (a b . rest)
    (let loop ((a a) (b b) (rest rest))
      (and (cmp (fixnum a) (fixnum b))
           (or (null? rest) (loop b (car rest) (cdr rest)))))))
;; (the usual case, two characters, without the loop)
(define (char=? a b . rest)
  (if (null? rest)
      (eqv? a b)
      (and (eqv? a b) (apply char=? b rest))))
(define char<? (%char-compare <))
(define char>? (%char-compare >))
(define char<=? (%char-compare <=))
(define char>=? (%char-compare >=))

(define (char-upcase c)
  (let ((n (fixnum c))) (if (and (>= n 97) (<= n 122)) (wchar (- n 32)) c)))
(define (char-downcase c)
  (let ((n (fixnum c))) (if (and (>= n 65) (<= n 90)) (wchar (+ n 32)) c)))
(define (%char-ci-compare cmp)
  (lambda (a b . rest)
    (apply cmp (char-downcase a) (char-downcase b) (map char-downcase rest))))
(define char-ci=? (%char-ci-compare char=?))
(define char-ci<? (%char-ci-compare char<?))
(define char-ci>? (%char-ci-compare char>?))
(define char-ci<=? (%char-ci-compare char<=?))
(define char-ci>=? (%char-ci-compare char>=?))

(define (char-upper-case? c) (let ((n (fixnum c))) (and (>= n 65) (<= n 90))))
(define (char-lower-case? c) (let ((n (fixnum c))) (and (>= n 97) (<= n 122))))
(define-override (char-alphabetic? c)
  (or (char-upper-case? c) (char-lower-case? c)))
(define (char-numeric? c) (let ((n (fixnum c))) (and (>= n 48) (<= n 57))))
(define (char-whitespace? c)
  (let ((n (fixnum c))) (or (= n 32) (and (>= n 9) (<= n 13)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings (of bytes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; built into fl_core.c: list->string make-string string-ref string-set!
;; substring string->list, and (%string char ...)
(define-override (string . chars) (apply %string chars))

(define (string-append . l) (%string-copy (apply %fl-string l)))
(define (string-copy s . range)
  (if (null? range) (%string-copy s) (apply substring s range)))
(define (string-null? s) (= (length s) 0))

(define (%string-cmp test)
  (lambda (a b . rest)
    (let loop ((a a) (b b) (rest rest))
      (and (test (%string-compare a b))
           (or (null? rest) (loop b (car rest) (cdr rest)))))))
(define (string=? a b . rest)
  (if (null? rest)
      (= (%string-compare a b) 0)
      (and (= (%string-compare a b) 0) (apply string=? b rest))))
(define string<? (%string-cmp (lambda (c) (< c 0))))
(define string>? (%string-cmp (lambda (c) (> c 0))))
(define string<=? (%string-cmp (lambda (c) (<= c 0))))
(define string>=? (%string-cmp (lambda (c) (>= c 0))))

(define (string-map-bytes f s)
  (list->string (map f (string->list s))))
(define (string-upcase s) (string-map-bytes char-upcase s))
(define (string-downcase s) (string-map-bytes char-downcase s))
(define (%string-ci-cmp cmp)
  (lambda (a b . rest)
    (apply cmp (string-downcase a) (string-downcase b)
           (map string-downcase rest))))
(define string-ci=? (%string-ci-cmp string=?))
(define string-ci<? (%string-ci-cmp string<?))
(define string-ci>? (%string-ci-cmp string>?))
(define string-ci<=? (%string-ci-cmp string<=?))
(define string-ci>=? (%string-ci-cmp string>=?))

(define (string-fill! s c)
  (do ((i 0 (+ i 1))) ((= i (length s))) (string-set! s i c)))

(define (string-for-each f s . range)
  (for-each f (apply string->list s range)))

;; > <= >= with any number of arguments, as < and = (patch 0017)
(define-override (> . l) (apply nary< (reverse l)))
(define-override (<= . l) (nary-compare (lambda (a b) (not (< b a))) l))
(define-override (>= . l) (nary-compare (lambda (a b) (not (< a b))) l))

;; femtolisp reads no fractions: "a/b" is read as the inexact a/b
(define %fl-string->number string->number)
(define-override (string->number s . radix)
  (or (apply %fl-string->number s radix)
      (let ((i (%string-index-of s #\/ 0 (length s))))
        (and i (> i 0) (< i (- (length s) 1)) (null? radix)
             (let ((a (%fl-string->number (substring s 0 i)))
                   (b (%fl-string->number (substring s (+ i 1) (length s)))))
               (and (%fl-integer? a) (%fl-integer? b) (not (= b 0))
                    (not (memv (string-ref s (+ i 1)) '(#\+ #\-)))
                    (/ (double a) b)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; as Guile's: an error when k is negative or larger than the length, and
;; iterative (a recursion as deep as a long list)
(define-override (list-tail l k)
  (if (or (not (%fl-integer? k)) (< k 0))
      (error "list-tail: bad index" k))
  (let loop ((l l) (k k))
    (if (= k 0) l (loop (cdr l) (- k 1)))))
(define-override (list-head l k)
  (if (or (not (%fl-integer? k)) (< k 0))
      (error "list-head: bad index" k))
  (let loop ((l l) (k k) (acc '()))
    (if (= k 0) (reverse! acc) (loop (cdr l) (- k 1) (cons (car l) acc)))))

(define-override (iota n . opt)
  (let ((start (if (pair? opt) (car opt) 0))
        (step (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) 1)))
    (let loop ((i (- n 1)) (r '()))
      (if (< i 0) r (loop (- i 1) (cons (+ start (* i step)) r))))))

(define (list-set! l k x) (set-car! (list-tail l k) x))

(define (make-list n . fill)
  (let ((x (if (pair? fill) (car fill) #f)))
    (let loop ((i 0) (r '()))
      (if (>= i n) r (loop (+ i 1) (cons x r))))))

(define (list-copy l) (if (pair? l) (copy-list l) l))
(define (last l) (car (last-pair l)))
(define (list-index pred l)
  (let loop ((l l) (i 0))
    (cond ((null? l) #f) ((pred (car l)) i) (else (loop (cdr l) (+ i 1))))))

(define (delete x l . eq)
  (let ((same? (if (pair? eq) (car eq) equal?)))
    (filter (lambda (y) (not (same? x y))) l)))
(define (delete! x l . eq) (apply delete x l eq))
(define (delq x l) (filter (lambda (y) (not (eq? x y))) l))
(define (delv x l) (filter (lambda (y) (not (eqv? x y))) l))
(define delq! delq)
(define delv! delv)
(define (remove pred l) (filter (lambda (x) (not (pred x))) l))

(define (acons key datum alist) (cons (cons key datum) alist))

(define (vector-ref v i) (aref v i))
(define (vector-set! v i x) (aset! v i x))
(define (vector-length v) (length v))
(define (make-vector n . fill)
  (if (pair? fill) (vector.alloc n (car fill)) (vector.alloc n #f)))
(define (vector-fill! v x)
  (do ((i 0 (+ i 1))) ((= i (length v))) (aset! v i x)))
(define (vector-map f v) (vector.map f v))
(define (vector-for-each f v)
  (do ((i 0 (+ i 1))) ((= i (length v))) (f (aref v i))))
(define (vector-copy v) (list->vector (vector->list v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (noop . args) (and (pair? args) (car args)))

;; promises, as in R5RS
(define (make-promise thunk)
  (let ((done #f) (value #f))
    (lambda ()
      (if (not done)
          (let ((v (thunk)))
            (if (not done) (begin (set! done #t) (set! value v)))))
      value)))
(define-macro (delay expr) `(make-promise (lambda () ,expr)))
(define (force p) (if (procedure? p) (p) p))

;; non-local exits, through the error mechanism: escape-only continuations
(define (call-with-exit f)
  (let ((tag (list 'call-with-exit)))
    (trycatch
     (f (lambda vals (raise (list tag vals))))
     (lambda (e)
       (if (and (pair? e) (eq? (car e) tag))
           (apply values (cadr e))
           (raise e))))))
(define call-with-current-continuation call-with-exit)
(define call/cc call-with-exit)

(define (dynamic-wind before thunk after)
  (before)
  (let ((r (trycatch (thunk) (lambda (e) (after) (raise e)))))
    (after)
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Errors, as in Guile: (key . args), usually (key subr message args rest)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the errors of femtolisp, made Guile errors
(define (%error-message msg args)
  (apply %fl-string msg (map (lambda (x) (%fl-string " " (print-to-string x)))
                             args)))
(define (%guile-error e)
  (if (not (pair? e))
      (list 'misc-error #f "~S" (list e) #f)
      (let ((key (car e)) (args (cdr e)))
        (case key
          ((type-error)
           (list 'wrong-type-arg
                 (and (pair? args) (symbol? (car args))
                      (symbol->string (car args)))
                 "Wrong type argument (expected ~A): ~S"
                 (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args)))
                     (list (cadr args) (caddr args))
                     (list "?" args))
                 #f))
          ((unbound-error)
           (list 'unbound-variable #f "Unbound variable: ~S" args #f))
          ((bounds-error)
           (list 'out-of-range (and (pair? args) (symbol? (car args))
                                    (symbol->string (car args)))
                 "Argument out of range: ~S" (if (pair? args) (cdr args) args)
                 #f))
          ((arg-error)
           (list 'wrong-number-of-args #f "~A" args #f))
          ((divide-error)
           (list 'numerical-overflow #f "~A" args #f))
          ((io-error)
           (list 'system-error #f "~A" args #f))
          ((parse-error)
           (list 'read-error #f "~A" args #f))
          ((key-error)
           (list 'misc-error #f "Key error: ~S" args #f))
          ((assert-failed)
           (list 'misc-error #f "Assertion failed: ~S" args #f))
          ((load-error)
           ;; (load-error file e): the error e during the load of file
           (if (and (pair? args) (pair? (cdr args)))
               (%guile-error (cadr args))
               (list 'misc-error #f "~S" args #f)))
          ((error)
           (list 'misc-error #f "~A" (list (%error-message (car args) (cdr args))) #f))
          (else e)))))

(define (error message . args)
  (raise (list 'misc-error #f "~A"
               (list (if (string? message)
                         (%error-message message args)
                         (%error-message (print-to-string message) args)))
               #f)))
(define (scm-error key subr message args rest)
  (raise (list key subr message args rest)))
(define-override (throw key . args)
  (raise (cons key args)))

(define-override (catch key thunk handler . pre)
  (trycatch
   (thunk)
   (lambda (e)
     (if (equal? %trace-errors? "catch")
         (begin
           (%write-string *stderr* "caught: ")
           (%fl-write e *stderr*)
           (%write-string *stderr* "\n")
           (%print-stack-trace (stacktrace) *stderr*)))
     (let ((g (%guile-error e)))
       (if (or (eq? key #t) (eq? key (car g)))
           (apply handler g)
           (raise e))))))
(define lazy-catch catch)

(define-macro (false-if-exception expr)
  `(catch #t (lambda () ,expr) (lambda args #f)))

(define (with-exception-handler handler thunk)
  (trycatch (thunk) (lambda (e) (handler (%guile-error e)))))

;; a simple format, for the messages (~A ~S ~a ~s ~% ~~)
(define (%simple-format port msg args)
  (let ((n (length msg)))
    (let loop ((i 0) (args args))
      (if (< i n)
          (let ((c (string-ref msg i)))
            (if (and (char=? c #\~) (< (+ i 1) n))
                (let ((d (char-downcase (string-ref msg (+ i 1)))))
                  (cond ((char=? d #\a)
                         (display (if (pair? args) (car args) "") port)
                         (loop (+ i 2) (if (pair? args) (cdr args) args)))
                        ((char=? d #\s)
                         (write (if (pair? args) (car args) "") port)
                         (loop (+ i 2) (if (pair? args) (cdr args) args)))
                        ((char=? d #\%) (newline port) (loop (+ i 2) args))
                        ((char=? d #\~) (%write-byte port #\~) (loop (+ i 2) args))
                        (else (%write-byte port c) (loop (+ i 1) args))))
                (begin (%write-byte port c) (loop (+ i 1) args))))))))

(define (%error-text g)
  ;; the text of a Guile error (key subr message args rest)
  (call-with-output-string
   (lambda (port)
     (if (and (pair? g) (pair? (cdr g)) (pair? (cddr g))
              (string? (caddr g)))
         (begin
           (if (cadr g) (begin (display (cadr g) port) (display ": " port)))
           (%simple-format port (caddr g)
                           (if (and (pair? (cdddr g)) (list? (cadddr g)))
                               (cadddr g) '())))
         (write g port)))))

;; the frames of (stacktrace), innermost first: the name of the function (as
;; its definition named it) and the arguments, abbreviated
(define (%print-stack-trace st port)
  (define (short x)
    (let ((s (with-bindings ((*print-length* 4) (*print-level* 2))
               (object->string x))))
      (if (> (length s) 60) (string-append (substring s 0 57) "...") s)))
  (let loop ((st (reverse st)) (n 0))
    (if (and (pair? st) (< n 20))
        (let* ((f (car st))
               (fun (aref f 0))
               (name (if (function? fun) (function:name fun) fun)))
          (%write-string port
            (apply %fl-string "  #" n " (" (%fl-string name)
                   (append (map (lambda (a) (%fl-string " " (short a)))
                                (cdr (vector->list f)))
                           (list ")\n"))))
          (loop (cdr st) (+ n 1))))))

;; called by the C++ code (fl_core.c) for the errors it catches
;; (with TEXMACS_FL_TRACE set, also the error of femtolisp and its stack)
(define (%report-error e)
  (let ((g (%guile-error e)))
    (%write-string *stderr*
                   (string-append "Error: " (%error-text g) "\n"))
    (let loop ((e e))
      (if (and (pair? e) (eq? (car e) 'load-error) (pair? (cdr e))
               (pair? (cddr e)))
          (begin
            (%write-string *stderr*
                           (string-append "  while loading " (cadr e) "\n"))
            (loop (caddr e)))))
    (if %trace-errors?
        (begin
          (%write-string *stderr* "femtolisp error: ")
          (%fl-write e *stderr*)
          (%write-string *stderr* "\n")
          (%print-stack-trace (stacktrace) *stderr*)))
    (io.flush *stderr*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the ports are the iostreams of femtolisp; strings are written as bytes
(define (%write-string port s) (io.write port s))

(define (port? x) (iostream? x))
(define (input-port? x) (iostream? x))
(define (output-port? x) (iostream? x))
(define (current-output-port) *output-stream*)
(define (current-error-port) *stderr*)
(define (current-input-port) *input-stream*)
(define (set-current-output-port p) (set! *output-stream* p))
(define (set-current-error-port p) (noop))
(define (close-port p) (io.close p))
(define close-input-port close-port)
(define close-output-port close-port)
(define (force-output . port)
  (io.flush (if (pair? port) (car port) *output-stream*)))
(define (flush-all-ports) (io.flush *output-stream*) (io.flush *stderr*))

(define (display x . port)
  (let ((p (if (pair? port) (car port) *output-stream*)))
    (cond ((string? x) (io.write p x))
          ((char? x) (%write-byte p x))
          (else (with-bindings ((*print-readably* #f)) (%fl-write x p))))
    #t))

(define-override (write x . port)
  (let ((p (if (pair? port) (car port) *output-stream*)))
    (with-bindings ((*print-readably* #t)) (%fl-write x p))
    #t))

(define-override (newline . port)
  (io.write (if (pair? port) (car port) *output-stream*) "\n")
  #t)

(define (write-char c . port)
  (%write-byte (if (pair? port) (car port) *output-stream*) c))
(define (read-char . port)
  (%read-byte (if (pair? port) (car port) *input-stream*)))
(define (peek-char . port)
  (%peek-byte (if (pair? port) (car port) *input-stream*)))
(define (char-ready? . port) #t)

(define-override (read . port)
  (%fl-read (if (pair? port) (car port) *input-stream*)))

(define (read-line . args)
  (let* ((p (if (pair? args) (car args) *input-stream*))
         (l (io.readline p)))
    (cond ((eof-object? l) l)
          ((and (> (length l) 0) (char=? (string-ref l (- (length l) 1)) #\newline))
           (substring l 0 (- (length l) 1)))
          (else l))))

(define (open-input-string s)
  (let ((b (buffer)))
    (io.write b s)
    (io.seek b 0)
    b))
(define (open-output-string) (buffer))
(define (get-output-string b)
  (let ((p (io.pos b)))
    (io.seek b 0)
    (let ((s (io.readall b)))
      (io.seek b p)
      (if (eof-object? s) "" s))))

(define (call-with-output-string proc)
  (let ((b (buffer)))
    (proc b)
    (io.tostring! b)))
(define (with-output-to-string thunk)
  (let ((b (buffer)))
    (with-bindings ((*output-stream* b)) (thunk))
    (io.tostring! b)))
(define (with-input-from-string s thunk)
  (with-bindings ((*input-stream* (open-input-string s))) (thunk)))
(define (call-with-input-string s proc) (proc (open-input-string s)))

(define (open-input-file name) (file name :read))
(define (open-output-file name) (file name :write :create :truncate))
(define (call-with-input-file name proc)
  (let* ((f (open-input-file name))
         (r (trycatch (proc f) (lambda (e) (io.close f) (raise e)))))
    (io.close f)
    r))
(define (call-with-output-file name proc)
  (let* ((f (open-output-file name))
         (r (trycatch (proc f) (lambda (e) (io.close f) (raise e)))))
    (io.close f)
    r))
(define (with-output-to-file name thunk)
  (call-with-output-file name
    (lambda (f) (with-bindings ((*output-stream* f)) (thunk)))))
(define (with-input-from-file name thunk)
  (call-with-input-file name
    (lambda (f) (with-bindings ((*input-stream* f)) (thunk)))))
(define (file-exists? name) (path.exists? name))

(define (object->string x . opt)
  (call-with-output-string (lambda (p) (write x p))))
(define (display-to-string x)
  (call-with-output-string (lambda (p) (display x p))))

(define (getenv name) (os.getenv name))
(define (setenv name value) (os.setenv name value))
(define (primitive-exit . code) (exit (if (pair? code) (car code) 0)))
