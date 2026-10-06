;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : base-test.scm
;; DESCRIPTION : tests of the base library and of hash tables
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The routines of kernel/library/base.scm (booleans, numbers, strings,
;; symbols, functions, objects, urls) and the adaptive hash tables of
;; kernel/boot/ahash-table.scm. The routines on buffers, windows, views and
;; positions need an open editor and are left to other suites.
;;
;; Strings are byte strings: a character above 127 is written as an escape
;; such as "\xe9" and counts as one character.

(texmacs-module (check base-test)
  (:use (check check-lib)))

(define e-acute (integer->char 233))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Booleans and numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; xor is true when an odd number of its arguments are true values
(define (test-booleans)
  (check-group "booleans")
  (check= (xor) #f)
  (check= (xor #t) #t)
  (check= (xor #f) #f)
  (check= (xor #t #t) #f)
  (check= (xor #t #f) #t)
  (check= (xor #f #f) #f)
  (check= (xor #t #t #t) #t)
  (check= (xor #f 1) #t)
  (check= (xor 'a "b") #f))

;; string-number? asks the C++ side whether a string reads as a double
(define (test-numbers)
  (check-group "numbers")
  (check= (float->string 2.5) "2.5")
  (check= (float->string 3) "3")
  (check= (float->string -0.5) "-0.5")
  (check= (string->float "3") 3.0)
  (check= (string->float "2.5") 2.5)
  (check= (string->float "-1/2") -0.5)
  (check-true (inexact? (string->float "7")))
  (check-error (string->float "abc") #t)
  (check= (string-number? "12") #t)
  (check= (string-number? "-1.5") #t)
  (check= (string-number? "1e3") #t)
  (check= (string-number? "abc") #f)
  (check= (string-number? "") #f)
  (check= (string-number? " 1") #f)
  (check= (string-number? "1a") #f)
  (check= (string-number? 12) #f)
  (check= (string-number? 'a) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tm-char-whitespace? only knows the four ASCII spaces: the byte A0 is a
;; character of the Cork encoding, not a non-breaking space
(define (test-characters)
  (check-group "characters")
  (check= (char->string #\a) "a")
  (check= (char->string #\space) " ")
  (check= (char->string e-acute) "\xe9")
  (check= (tm-char-whitespace? #\space) #t)
  (check= (tm-char-whitespace? (integer->char 9)) #t)
  (check= (tm-char-whitespace? #\newline) #t)
  (check= (tm-char-whitespace? #\return) #t)
  (check= (tm-char-whitespace? #\a) #f)
  (check= (tm-char-whitespace? (integer->char 160)) #f)
  (check= (tm-char-whitespace? (integer->char 0)) #f)
  (check= (char-in-string? #\a "abc") #t)
  (check= (char-in-string? #\c "abc") #t)
  (check= (char-in-string? #\d "abc") #f)
  (check= (char-in-string? #\a "") #f)
  (check= (char-in-string? e-acute "caf\xe9") #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefixes, suffixes and substrings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-affixes)
  (check-group "prefixes and suffixes")
  (check= (string-starts? "abc" "ab") #t)
  (check= (string-starts? "abc" "abc") #t)
  (check= (string-starts? "abc" "") #t)
  (check= (string-starts? "" "") #t)
  (check= (string-starts? "ab" "abc") #f)
  (check= (string-starts? "abc" "b") #f)
  (check= (string-starts? "" "a") #f)
  (check= (string-starts? "\xe9t\xe9" "\xe9") #t)
  (check= (string-ends? "abc" "bc") #t)
  (check= (string-ends? "abc" "abc") #t)
  (check= (string-ends? "abc" "") #t)
  (check= (string-ends? "bc" "abc") #f)
  (check= (string-ends? "abc" "b") #f)
  (check= (string-ends? "" "a") #f)
  (check= (string-ends? "caf\xe9" "f\xe9") #t)
  (check= (string-contains? "abcd" "bc") #t)
  (check= (string-contains? "abcd" "abcd") #t)
  (check= (string-contains? "abc" "") #t)
  (check= (string-contains? "" "") #t)
  (check= (string-contains? "" "a") #f)
  (check= (string-contains? "abc" "ac") #f)
  (check= (string-contains? "abc" "abcd") #f)
  (check= (string-contains? "a\xe9z" "\xe9") #t))

;; string-take and friends count characters; out of range is an error
(define (test-substrings)
  (check-group "substrings")
  (check= (string-tail "abc" 1) "bc")
  (check= (string-tail "abc" 0) "abc")
  (check= (string-tail "abc" 3) "")
  (check-error (string-tail "abc" 4) #t)
  (check= (string-drop "abc" 2) "c")
  (check= (string-drop-right "abc" 1) "ab")
  (check= (string-drop-right "abc" 3) "")
  (check= (string-drop-right "abc" 0) "abc")
  (check-error (string-drop-right "abc" 4) #t)
  (check= (string-take "abc" 2) "ab")
  (check= (string-take "abc" 0) "")
  (check= (string-take "abc" 3) "abc")
  (check-error (string-take "abc" 4) #t)
  (check= (string-take-right "abc" 2) "bc")
  (check= (string-take-right "abc" 0) "")
  (check= (string-take-right "abc" 3) "abc")
  (check-error (string-take-right "abc" 4) #t)
  (check= (string-take "\xe9t\xe9" 1) "\xe9")
  (check= (string-tail "\xe9t\xe9" 2) "\xe9")
  (check= (force-string "abc") "abc")
  (check= (force-string "") "")
  (check= (force-string 'abc) "")
  (check= (force-string #f) "")
  (check= (force-string '("a")) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trimming and iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the trims remove the whitespace of tm-char-whitespace?, and keep the
;; whitespace inside the string
(define (test-trimming)
  (check-group "trimming")
  (check= (tm-string-trim "  ab c ") "ab c ")
  (check= (tm-string-trim "\t\n\rab") "ab")
  (check= (tm-string-trim "ab") "ab")
  (check= (tm-string-trim "") "")
  (check= (tm-string-trim "   ") "")
  (check= (tm-string-trim-right "  ab c ") "  ab c")
  (check= (tm-string-trim-right "ab\n\r\t ") "ab")
  (check= (tm-string-trim-right "") "")
  (check= (tm-string-trim-right "   ") "")
  (check= (tm-string-trim-both "\t a b \n") "a b")
  (check= (tm-string-trim-both "ab") "ab")
  (check= (tm-string-trim-both "") "")
  (check= (tm-string-trim-both " \n ") "")
  (check= (tm-string-trim-both " \xa0 ") "\xa0")
  (check= (list-drop-right-while '(1 2 3 4) even?) '(1 2 3))
  (check= (list-drop-right-while '(1 2 4 6) even?) '(1))
  (check= (list-drop-right-while '(2 4) even?) '())
  (check= (list-drop-right-while '() even?) '())
  (check= (list-drop-right-while '(1 3) even?) '(1 3)))

(define (test-string-iteration)
  (check-group "string iteration")
  (check= (reverse-list->string '(#\a #\b #\c)) "cba")
  (check= (reverse-list->string '()) "")
  (check= (string-concatenate '("ab" "" "c")) "abc")
  (check= (string-concatenate '()) "")
  (check= (string-join '("a" "b" "c")) "a b c")
  (check= (string-join '("a" "b") "-") "a-b")
  (check= (string-join '("a" "b") "") "ab")
  (check= (string-join '("a") ", ") "a")
  (check= (string-join '()) "")
  (check= (string-join '("" "") ",") ",")
  (check= (string-map char-upcase "abc") "ABC")
  (check= (string-map char-upcase "") "")
  (check= (string-map (lambda (c) (if (== c #\a) e-acute c)) "aza")
          "\xe9z\xe9")
  (check= (string-fold cons '() "abc") '(#\c #\b #\a))
  (check= (string-fold cons '() "") '())
  (check= (string-fold-right cons '() "abc") '(#\a #\b #\c))
  (check= (string-fold-right cons '() "") '())
  (check= (string-fold (lambda (c n) (+ n 1)) 0 "a\xe9z") 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Splitting and joining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a separator at an end or a repeated separator gives empty pieces, so that
;; string-recompose undoes string-tokenize-by-char and string-decompose
(define (test-splitting)
  (check-group "splitting")
  (check= (string-split-lines "a\nb") '("a" "b"))
  (check= (string-split-lines "a\nb\n") '("a" "b" ""))
  (check= (string-split-lines "\na") '("" "a"))
  (check= (string-split-lines "a\n\nb") '("a" "" "b"))
  (check= (string-split-lines "") '(""))
  (check= (string-split-lines "abc") '("abc"))
  (check= (string-tokenize-by-char "a,b,c" #\,) '("a" "b" "c"))
  (check= (string-tokenize-by-char "abc" #\,) '("abc"))
  (check= (string-tokenize-by-char "" #\,) '(""))
  (check= (string-tokenize-by-char "," #\,) '("" ""))
  (check= (string-tokenize-by-char ",a,,b," #\,) '("" "a" "" "b" ""))
  (check= (string-tokenize-by-char "a\xe9z\xe9" e-acute) '("a" "z" ""))
  (check= (string-tokenize-by-char-n "a:b:c:d" #\: 2) '("a" "b" "c:d"))
  (check= (string-tokenize-by-char-n "a:b:c:d" #\: 1) '("a" "b:c:d"))
  (check= (string-tokenize-by-char-n "a:b" #\: 0) '("a:b"))
  (check= (string-tokenize-by-char-n "a:b" #\: 5) '("a" "b"))
  (check= (string-tokenize-by-char-n "" #\: 1) '(""))
  (check= (string-tokenize-by-char-n "::" #\: 1) '("" ":"))
  (check= (string-decompose "a--b--c" "--") '("a" "b" "c"))
  (check= (string-decompose "a--b----c" "--") '("a" "b" "" "c"))
  (check= (string-decompose "--a--" "--") '("" "a" ""))
  (check= (string-decompose "abc" "--") '("abc"))
  (check= (string-decompose "" "-") '(""))
  (check= (string-decompose "a-b" "-") '("a" "b"))
  (check= (string-decompose "x\xe9\xe9y" "\xe9\xe9") '("x" "y"))
  ;; a long string: the pieces are found in linear time, without a recursion
  ;; as deep as their number (with copies of the rest of the string at each
  ;; piece, 20000 lines took about 400 MB with S7)
  (with s (apply string-append (make-list 20000 "ab\n"))
    (check= (length (string-decompose s "\n")) 20001)
    (check= (length (string-tokenize-by-char s #\newline)) 20001)
    (check= (car (string-decompose s "b\na")) "a"))
  (check= (string-tokenize-comma "a, b ,c") '("a" "b" "c"))
  (check= (string-tokenize-comma " a ") '("a"))
  (check= (string-tokenize-comma "") '(""))
  (check= (string-tokenize-comma "a,,b") '("a" "" "b")))

(define (test-joining)
  (check-group "joining")
  (check= (string-recompose '("a" "b" "c") ",") "a,b,c")
  (check= (string-recompose '("a" "b") #\/) "a/b")
  (check= (string-recompose '("a") ",") "a")
  (check= (string-recompose '() ",") "")
  (check= (string-recompose '("" "") ",") ",")
  (check= (string-recompose '("a" "b") "") "ab")
  (check= (string-recompose (string-tokenize-by-char ",a,,b," #\,) ",")
          ",a,,b,")
  (check= (string-recompose (string-decompose "--a----b" "--") "--")
          "--a----b")
  (check= (string-recompose-comma '("a" "b" "c")) "a, b, c")
  (check= (string-recompose-comma '("a")) "a")
  (check= (string-recompose-comma '()) "")
  (check= (raw-quote "x") "\"x\"")
  (check= (raw-quote "") "\"\"")
  (check= (raw-quote "a\"b") "\"a\"b\""))

;; a component without "=" has the value "true"; only the first "=" cuts
(define (test-alists)
  (check-group "string alists")
  (check= (string->alist "a=1/b=2") '(("a" . "1") ("b" . "2")))
  (check= (string->alist "a=1/b/c=x=y")
          '(("a" . "1") ("b" . "true") ("c" . "x=y")))
  (check= (string->alist "a=") '(("a" . "")))
  (check= (string->alist "=v") '(("" . "v")))
  (check= (string->alist "") '(("" . "true")))
  (check= (alist->string '(("a" . "1") ("b" . "2"))) "a=1/b=2")
  (check= (alist->string '(("a" . "1"))) "a=1")
  (check= (alist->string '()) "")
  (check= (string->alist (alist->string '(("x" . "1") ("y" . "z"))))
          '(("x" . "1") ("y" . "z"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols, functions and objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-symbols)
  (check-group "symbols")
  (check= (symbol<=? 'a 'b) #t)
  (check= (symbol<=? 'a 'a) #t)
  (check= (symbol<=? 'b 'a) #f)
  (check= (symbol<=? 'a 'ab) #t)
  (check= (symbol-starts? 'tm-define 'tm-) #t)
  (check= (symbol-starts? 'tm 'tm-) #f)
  (check= (symbol-ends? 'list->string '->string) #t)
  (check= (symbol-ends? 'string 'list) #f)
  (check= (symbol-drop 'abc 1) 'bc)
  (check= (symbol-drop 'abc 0) 'abc)
  (check= (symbol-drop-right 'abc 1) 'ab)
  ;; s7 has no empty symbol (see docs/s7/04-progs-changes.md)
  (when (not (s7-scheme?))
    (check= (symbol-drop-right 'abc 3) (string->symbol "")))
  (check-error (symbol-drop 'abc 4) #t))

(define (test-functions)
  (check-group "functions")
  (check= ((compose car cdr) '(1 2 3)) 2)
  (check= ((compose (lambda (x) (* 2 x)) +) 1 2 3) 12)
  (check= ((compose string->symbol symbol->string) 'a) 'a)
  (check= ((non even?) 3) #t)
  (check= ((non even?) 2) #f)
  (check= ((non <) 1 2) #f)
  (check= ((non <) 2 1) #t)
  (check= ((non (lambda () #f))) #t))

;; Guile's curried define, which the TeXmacs code uses (the vendored s7 is
;; patched for it)
(define ((base-test-adder a) b) (+ a b))
(define (((base-test-adder3 a) b) . l) (apply + a b l))

(define (base-test-curried-twice k l)
  (define ((scale k) x) (* k x))
  (define (walk l) (if (null? l) '() (cons ((scale k) (car l)) (walk (cdr l)))))
  (walk (cdr l)))

(define (test-curried-define)
  (check-group "curried define")
  (check= ((base-test-adder 1) 2) 3)
  (check= (map (base-test-adder 10) '(1 2)) '(11 12))
  (check= (((base-test-adder3 1) 2) 3 4) 10)
  (check= (((base-test-adder3 1) 2)) 3)
  (check= (let () (define ((mul a) b) (* a b)) ((mul 6) 7)) 42)
  (check= (base-test-curried-twice 2 '(0 1 2)) '(2 4))
  (check= (base-test-curried-twice 3 '(0 1 2)) '(3 6)))

;; object->string* writes the kinds of data it knows and #f for others
(define (test-objects)
  (check-group "objects")
  (check= (string->object "(a 1 \"b\")") '(a 1 "b"))
  (check= (string->object "42") 42)
  (check= (string->object "sym") 'sym)
  (check-true (eof-object? (string->object "")))
  (check= (object->string* '()) "()")
  (check= (object->string* '(a 1)) "(a 1)")
  (check= (object->string* 12) "12")
  (check= (object->string* "s") "\"s\"")
  (check= (object->string* 'a) "a")
  (check= (object->string* (string->tree "x")) "\"x\"")
  (check= (object->string* (stree->tree '(concat "a" (b)))) "(concat \"a\" (b))")
  (check= (object->string* #t) "#f")
  (check= (object->string* #\a) "#f")
  (check= (string->object (object->string* '(f "x" 2))) '(f "x" 2))
  (check= (func? '(a b) 'a) #t)
  (check= (func? '(a) 'a) #t)
  (check= (func? '(a b) 'b) #f)
  (check= (func? '() 'a) #f)
  (check= (func? 'a 'a) #f)
  (check= (func? '(a b) 'a 1) #t)
  (check= (func? '(a b) 'a 2) #f)
  (check= (func? '(a) 'a 0) #t)
  (check-error (func? '(a b) 'a 1 2) #t)
  (check= (tuple? '()) #t)
  (check= (tuple? '(1 2)) #t)
  (check= (tuple? 3) #f)
  (check= (tuple? '(1 . 2)) #f)
  (check= (tuple? '(a b) 'a) #t)
  (check= (tuple? '(a b) 'a 2) #f)
  (check= (tuple? '() 'a) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Urls and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; url->list and list->url convert between a url-or and the list of its
;; alternatives; the buffer predicates only look at the name of a url
(define (test-urls)
  (check-group "urls")
  (let ((a (string->url "a"))
        (b (string->url "b"))
        (c (string->url "c")))
    (check= (url->list (url-none)) '())
    (check= (map url->string (url->list a)) '("a"))
    (check= (map url->string (url->list (url-or a (url-or b c))))
            '("a" "b" "c"))
    (check= (url-none? (list->url '())) #t)
    (check= (url->string (list->url (list a))) "a")
    (check= (map url->string (url->list (list->url (list a b c))))
            '("a" "b" "c")))
  (check= (url-wrap (string->url "a")) #f)
  (check= (url->string (url-autosave (string->url "/tmp/a.tm") "~"))
          "/tmp/a.tm~")
  (check= (url-autosave (string->url "http://www.texmacs.org/a.tm") "~") #f)
  (check= (url-autosave (string->url "tmfs://help/a") "~") #f)
  (check= (first-in-path) "")
  (check= (first-in-path "no-such-program-for-base-test" "sh") "sh")
  (check= (first-in-path "no-such-program-for-base-test") "")
  (check= (buffer-in-recent-menu? (string->url "/tmp/a.tm")) #t)
  (check= (buffer-in-recent-menu? (string->url "tmfs://part/a")) #t)
  (check= (buffer-in-recent-menu? (string->url "tmfs://help/a")) #f)
  (check= (buffer-in-menu? (string->url "tmfs://help/a")) #t)
  (check= (buffer-in-menu? (string->url "tmfs://apidoc/a")) #t)
  (check= (buffer-in-menu? (string->url "tmfs://remote-file/a")) #t)
  (check= (buffer-in-menu? (string->url "tmfs://grep/a")) #f)
  (check= (buffer-exists? "/no/such/buffer/for-base-test.tm") #f)
  ;; the directory of the check suites lists this very file
  (check-true
   (in? "base-test.scm"
        (map (lambda (u) (url->string (url-tail u)))
             (url-read-directory
              (url-append (system->url "$TEXMACS_PATH") "progs/check")
              "*.scm")))))

(define (test-output)
  (check-group "output")
  (check= (tm-with-output-to-string (lambda () (display "hi"))) "hi")
  (check= (tm-with-output-to-string (lambda () #t)) "")
  (check= (tm-with-output-to-string (lambda () (write "a") (display 1)))
          "\"a\"1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ahash-table->list has no order, so the lists are sorted before comparing
(define (sorted-alist t)
  (sort (ahash-table->list t)
        (lambda (x y) (string<=? (object->string (car x))
                                 (object->string (car y))))))

(define (make-abc)
  (list->ahash-table '((a . 1) (b . 2) (c . 3))))

;; keys are compared with equal?, so strings and lists are good keys; a key
;; bound to #f is present, although ahash-ref cannot tell it from a missing one
(define (test-hash-basics)
  (check-group "hash tables")
  (let ((h (make-ahash-table)))
    (check= (ahash-size h) 0)
    (check= (ahash-table->list h) '())
    (check= (ahash-ref h 'a) #f)
    (check= (ahash-get-handle h 'a) #f)
    (ahash-set! h 'a 1)
    (check= (ahash-ref h 'a) 1)
    (check= (ahash-get-handle h 'a) '(a . 1))
    (check= (ahash-size h) 1)
    (ahash-set! h 'a 2)
    (check= (ahash-ref h 'a) 2)
    (check= (ahash-size h) 1)
    (ahash-set! h "k" 'string)
    (ahash-set! h (list 1 2) 'list)
    (ahash-set! h 3 'number)
    (check= (ahash-ref h (string-append "k" "")) 'string)
    (check= (ahash-ref h (list 1 2)) 'list)
    (check= (ahash-ref h 3) 'number)
    (check= (ahash-ref h "\xe9") #f)
    (ahash-set! h "\xe9" 'accent)
    (check= (ahash-ref h "\xe9") 'accent)
    (check= (ahash-size h) 5)
    (ahash-set! h 'f #f)
    (check= (ahash-ref h 'f) #f)
    ;; storing #f in an s7 hash table removes the key, or does not add it
    ;; (see docs/s7/03-compat-layer.md)
    (if (s7-scheme?)
        (begin
          (check= (ahash-get-handle h 'f) #f)
          (check= (ahash-size h) 5))
        (begin
          (check= (ahash-get-handle h 'f) '(f . #f))
          (check= (ahash-size h) 6))))
  (let ((h (make-abc)))
    (check-true (ahash-remove! h 'b))
    (check= (ahash-ref h 'b) #f)
    (check= (ahash-size h) 2)
    (check-false (ahash-remove! h 'b))
    (check-false (ahash-remove! h 'zz))
    (check= (ahash-size h) 2)
    (ahash-remove! h 'a)
    (ahash-remove! h 'c)
    (check= (ahash-size h) 0)
    (check= (ahash-table->list h) '())
    (check-false (ahash-remove! h 'a)))
  ;; many keys, to go through the growing of the table
  (let ((h (make-ahash-table)))
    (for-each (lambda (i) (ahash-set! h i (* i i))) (iota 200))
    (check= (ahash-size h) 200)
    (check= (ahash-ref h 199) (* 199 199))
    (for-each (lambda (i) (ahash-remove! h i)) (iota 190))
    (check= (ahash-size h) 10)
    (check= (ahash-ref h 195) (* 195 195))
    (check= (ahash-ref h 5) #f)))

(define (test-hash-lists)
  (check-group "hash tables and lists")
  (check= (sorted-alist (make-abc)) '((a . 1) (b . 2) (c . 3)))
  (check= (ahash-size (list->ahash-table '())) 0)
  (check= (ahash-table->list (list->ahash-table '((a . 1) (a . 2))))
          '((a . 2)))
  (check= (ahash-fold (lambda (k v s) (+ v s)) 0 (make-abc)) 6)
  (check= (ahash-fold (lambda (k v s) (+ v s)) 0 (make-ahash-table)) 0)
  (check= (sort (ahash-fold (lambda (k v s) (cons k s)) '() (make-abc))
                symbol<=?)
          '(a b c))
  (check= (sorted-alist (list->frequencies '(a b a c a b)))
          '((a . 3) (b . 2) (c . 1)))
  (check= (ahash-size (list->frequencies '())) 0)
  (check= (ahash-ref (list->frequencies '("x" "x")) "x") 2)
  (check= (ahash-ref* (make-abc) 'a 0) 1)
  (check= (ahash-ref* (make-abc) 'z 0) 0)
  (check= (ahash-ref* (list->ahash-table '((f . #f))) 'f 'dflt) 'dflt))

(define (test-hash-operations)
  (check-group "operations on hash tables")
  (check= (sorted-alist (ahash-table-invert (make-abc)))
          '((1 . a) (2 . b) (3 . c)))
  (check= (ahash-size (ahash-table-invert (make-ahash-table))) 0)
  ;; two keys with one value: one of them wins
  (check-true (in? (ahash-ref (ahash-table-invert
                               (list->ahash-table '((a . 1) (b . 1))))
                              1)
                   '(a b)))
  (check= (sorted-alist (ahash-table-append (make-abc)
                                            (list->ahash-table
                                             '((c . 30) (d . 4)))))
          '((a . 1) (b . 2) (c . 30) (d . 4)))
  (check= (ahash-size (ahash-table-append)) 0)
  (check= (sorted-alist (ahash-table-append (make-abc))) (sorted-alist (make-abc)))
  (check= (sorted-alist (ahash-table-difference
                         (make-abc) (list->ahash-table '((b . 0) (z . 1)))))
          '((a . 1) (c . 3)))
  (check= (sorted-alist (ahash-table-difference (make-abc) (make-ahash-table)))
          '((a . 1) (b . 2) (c . 3)))
  (check= (ahash-size (ahash-table-difference (make-abc) (make-abc))) 0)
  (check= (sorted-alist (ahash-table-map (lambda (v) (* 10 v)) (make-abc)))
          '((a . 10) (b . 20) (c . 30)))
  (check= (ahash-size (ahash-table-map (lambda (v) v) (make-ahash-table))) 0)
  (check= (sorted-alist (ahash-table-select (make-abc) '(a c z)))
          '((a . 1) (c . 3)))
  (check= (ahash-size (ahash-table-select (make-abc) '())) 0)
  ;; the operations make new tables and leave their arguments alone
  (let ((h (make-abc)))
    (ahash-table-map (lambda (v) 0) h)
    (ahash-table-difference h (make-abc))
    (ahash-table-invert h)
    (check= (sorted-alist h) '((a . 1) (b . 2) (c . 3)))))

;; ahash-with binds a key during its body and puts the old value back; a key
;; which was missing comes back bound to #f
(define (test-hash-with)
  (check-group "ahash-with")
  (let ((h (make-abc)))
    (check= (ahash-with h 'a 10 (ahash-ref h 'a)) 10)
    (check= (ahash-ref h 'a) 1)
    (check= (ahash-with h 'z 5 (+ (ahash-ref h 'z) (ahash-ref h 'b))) 7)
    (check= (ahash-ref h 'z) #f)
    (check= (ahash-with h 'a 2 (ahash-with h 'a 3 (ahash-ref h 'a))) 3)
    (check= (ahash-ref h 'a) 1)))

(define-table base-test-table
  (a . 1)
  (b . ,(+ 1 1)))

(extend-table base-test-table
  (c . 3)
  (a . 10))

(define-collection base-test-collection
  x y)

(extend-collection base-test-collection
  z)

;; fill-dictionary reads entries (key1 ... keyn value)
(define (test-dictionaries)
  (check-group "dictionaries and declared tables")
  (let ((d (make-ahash-table)))
    (fill-dictionary d '((a b 1) (c 2)))
    (check= (sorted-alist d) '((a . 1) (b . 1) (c . 2))))
  (let ((d (make-ahash-table)))
    (fill-dictionary d '())
    (check= (ahash-size d) 0))
  (let ((d (make-ahash-table)))
    (fill-dictionary d '((1)))
    (check= (ahash-size d) 0))
  (let ((d (make-ahash-table)))
    (define-table-decls d '((p . 1) (q . 2)))
    (check= (sorted-alist d) '((p . 1) (q . 2))))
  (let ((d (make-ahash-table)))
    (define-collection-decls d '(p q))
    (check= (sorted-alist d) '((p . #t) (q . #t))))
  (check= (sorted-alist base-test-table) '((a . 10) (b . 2) (c . 3)))
  (check= (sorted-alist base-test-collection) '((x . #t) (y . #t) (z . #t)))
  (check= (ahash-ref base-test-collection 'w) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (base-test-failures)
  (check-suite "base")
  (test-booleans)
  (test-numbers)
  (test-characters)
  (test-affixes)
  (test-substrings)
  (test-trimming)
  (test-string-iteration)
  (test-splitting)
  (test-joining)
  (test-alists)
  (test-symbols)
  (test-functions)
  (test-curried-define)
  (test-objects)
  (test-urls)
  (test-output)
  (test-hash-basics)
  (test-hash-lists)
  (test-hash-operations)
  (test-hash-with)
  (test-dictionaries)
  (check-end))
