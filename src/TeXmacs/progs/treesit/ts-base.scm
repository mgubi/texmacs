
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-base.scm
;; DESCRIPTION : data structures of the tree-sitter parser generator in Scheme
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a port of the parser generator of tree-sitter (crates/generate of
;; tree-sitter 0.27): a grammar.json is turned into parse and lex tables which
;; are plain Scheme data, run by the parser of ts-parse.scm.
;;
;; The data shared by the stages:
;;
;;   - symbols are integers, kind * ts-kind-base + index, so that the order of
;;     the integers is the order of tree-sitter's symbols (kind first);
;;   - token sets are sorted lists of symbols;
;;   - character sets are sorted lists of disjoint half-open ranges
;;     (start . end) of code points;
;;   - growable vectors (gvec) hold the tables while they are built.

(texmacs-module (treesit ts-base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public ts-kind-base 1048576)
(define-public ts-external 0)
(define-public ts-end-kind 1)
(define-public ts-eont-kind 2)
(define-public ts-terminal 3)
(define-public ts-non-terminal 4)

(define-public (sym-make kind index) (+ (* kind ts-kind-base) index))
(define-public (sym-kind s) (quotient s ts-kind-base))
(define-public (sym-index s) (remainder s ts-kind-base))
(define-public (terminal i) (sym-make ts-terminal i))
(define-public (non-terminal i) (sym-make ts-non-terminal i))
(define-public (external i) (sym-make ts-external i))
(define-public sym-end (sym-make ts-end-kind 0))
(define-public sym-eont (sym-make ts-eont-kind 0))
(define-public (terminal? s) (= (sym-kind s) ts-terminal))
(define-public (non-terminal? s) (= (sym-kind s) ts-non-terminal))
(define-public (external? s) (= (sym-kind s) ts-external))
(define-public (sym-end? s) (= s sym-end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Growable vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gvec . l)
  (let ((v (make-gvec)))
    (for-each (lambda (x) (gvec-push! v x)) l)
    v))

(define-public (make-gvec) (vector (make-vector 8 #f) 0))
(define-public (gvec-length v) (vector-ref v 1))
(define-public (gvec-ref v i) (vector-ref (vector-ref v 0) i))
(define-public (gvec-set! v i x) (vector-set! (vector-ref v 0) i x))
(define-public (gvec-last v) (gvec-ref v (- (gvec-length v) 1)))

(define-public (gvec-push! v x)
  (let ((data (vector-ref v 0))
        (n (vector-ref v 1)))
    (when (= n (vector-length data))
      (let ((new (make-vector (* 2 n) #f)))
        (do ((i 0 (+ i 1))) ((= i n))
          (vector-set! new i (vector-ref data i)))
        (vector-set! v 0 new)
        (set! data new)))
    (vector-set! data n x)
    (vector-set! v 1 (+ n 1))
    n))

(define-public (gvec-pop! v)
  (let* ((n (- (gvec-length v) 1))
         (x (gvec-ref v n)))
    (gvec-set! v n #f)
    (vector-set! v 1 n)
    x))

(define-public (gvec->list v)
  (let loop ((i (- (gvec-length v) 1)) (acc '()))
    (if (< i 0) acc (loop (- i 1) (cons (gvec-ref v i) acc)))))

(define-public (gvec->vector v)
  (let* ((n (gvec-length v)) (r (make-vector n #f)))
    (do ((i 0 (+ i 1))) ((= i n) r)
      (vector-set! r i (gvec-ref v i)))))

(define-public (list->gvec l)
  (let ((v (make-gvec)))
    (for-each (lambda (x) (gvec-push! v x)) l)
    v))

(define-public (gvec-for-each fun v)
  (do ((i 0 (+ i 1))) ((= i (gvec-length v)))
    (fun (gvec-ref v i))))

(define-public (gvec-index v x)
  ;; first index of x (equal?) or #f
  (let loop ((i 0))
    (cond ((= i (gvec-length v)) #f)
          ((equal? (gvec-ref v i) x) i)
          (else (loop (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (ts-sort l less?)
  ;; a stable merge sort, the same on every Scheme
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((less? (car b) (car a)) (cons (car b) (merge a (cdr b))))
          (else (cons (car a) (merge (cdr a) b)))))
  (define (msort l n)
    (if (< n 2)
        (if (= n 0) '() (list (car l)))
        (let ((h (quotient n 2)))
          (merge (msort l h) (msort (list-tail l h) (- n h))))))
  (msort l (length l)))

(define-public (ts-filter pred l)
  (cond ((null? l) '())
        ((pred (car l)) (cons (car l) (ts-filter pred (cdr l))))
        (else (ts-filter pred (cdr l)))))

(define-public (ts-any pred l)
  (and (pair? l) (or (pred (car l)) (ts-any pred (cdr l)))))

(define-public (ts-every pred l)
  (or (null? l) (and (pred (car l)) (ts-every pred (cdr l)))))

(define-public (ts-iota n)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0) acc (loop (- i 1) (cons i acc)))))

(define-public (ts-position x l)
  (let loop ((l l) (i 0))
    (cond ((null? l) #f)
          ((equal? (car l) x) i)
          (else (loop (cdr l) (+ i 1))))))

(define-public (ts-adjoin x l)
  ;; add x at the end of l unless already there
  (if (member x l) l (append l (list x))))

(define-public (ts-dedup l)
  ;; remove repeated elements, keeping the first occurrences
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc))
          ((member (car l) acc) (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))

(define-public (ts-last l) (if (null? (cdr l)) (car l) (ts-last (cdr l))))

(define-public (ts-take l n)
  (if (or (= n 0) (null? l)) '() (cons (car l) (ts-take (cdr l) (- n 1)))))

(define-public (ts-list-index pred l)
  (let loop ((l l) (i 0))
    (cond ((null? l) #f)
          ((pred (car l)) i)
          (else (loop (cdr l) (+ i 1))))))

(define-public (ts-compare-lists a b cmp)
  ;; lexicographic comparison of two lists, cmp returns -1, 0 or 1
  (cond ((and (null? a) (null? b)) 0)
        ((null? a) -1)
        ((null? b) 1)
        (else (let ((c (cmp (car a) (car b))))
                (if (= c 0) (ts-compare-lists (cdr a) (cdr b) cmp) c)))))

(define-public (ts-cmp a b) (cond ((< a b) -1) ((> a b) 1) (else 0)))

(define-public (ts-string-cmp a b)
  (cond ((string<? a b) -1) ((string<? b a) 1) (else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Token sets: sorted lists of symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (ts-insert s set)
  (cond ((null? set) (list s))
        ((< s (car set)) (cons s set))
        ((= s (car set)) set)
        (else (let ((r (ts-insert s (cdr set))))
                (if (eq? r (cdr set)) set (cons (car set) r))))))

(define-public (ts-remove s set)
  (cond ((null? set) set)
        ((< s (car set)) set)
        ((= s (car set)) (cdr set))
        (else (cons (car set) (ts-remove s (cdr set))))))

(define-public (ts-contains? set s)
  (cond ((null? set) #f)
        ((< s (car set)) #f)
        ((= s (car set)) #t)
        (else (ts-contains? (cdr set) s))))

(define-public (ts-union a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((< (car a) (car b)) (cons (car a) (ts-union (cdr a) b)))
        ((> (car a) (car b)) (cons (car b) (ts-union a (cdr b))))
        (else (cons (car a) (ts-union (cdr a) (cdr b))))))

(define-public (ts-subset? a b)
  (cond ((null? a) #t)
        ((null? b) #f)
        ((< (car a) (car b)) #f)
        ((= (car a) (car b)) (ts-subset? (cdr a) (cdr b)))
        (else (ts-subset? a (cdr b)))))

(define-public (ts-terminals set) (ts-filter terminal? set))

(define-public (list->ts l)
  (let loop ((l l) (acc '()))
    (if (null? l) acc (loop (cdr l) (ts-insert (car l) acc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character sets: sorted disjoint half-open ranges (start . end)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public cs-end #x110000)

(define-public (cs-char c) (list (cons c (+ c 1))))

(define-public (cs-add-range cs start end)
  ;; add [start, end), joining the ranges which touch it
  (cond ((null? cs) (list (cons start end)))
        ((> (caar cs) end) (cons (cons start end) cs))
        ((>= (cdar cs) start)
         (let loop ((s (min start (caar cs)))
                    (e (max end (cdar cs)))
                    (rest (cdr cs)))
           (if (and (pair? rest) (<= (caar rest) e))
               (loop s (max e (cdar rest)) (cdr rest))
               (cons (cons s e) rest))))
        (else (cons (car cs) (cs-add-range (cdr cs) start end)))))

(define-public (cs-add a b)
  (let loop ((a a) (b b))
    (if (null? b) a (loop (cs-add-range a (caar b) (cdar b)) (cdr b)))))

(define-public (cs-negate cs)
  (let loop ((cs cs) (prev 0) (acc '()))
    (if (null? cs)
        (reverse (if (< prev cs-end) (cons (cons prev cs-end) acc) acc))
        (loop (cdr cs) (cdar cs)
              (if (< prev (caar cs)) (cons (cons prev (caar cs)) acc) acc)))))

(define-public (cs-intersect a b)
  (cond ((or (null? a) (null? b)) '())
        ((<= (cdar a) (caar b)) (cs-intersect (cdr a) b))
        ((<= (cdar b) (caar a)) (cs-intersect a (cdr b)))
        (else
         (let ((s (max (caar a) (caar b)))
               (e (min (cdar a) (cdar b))))
           (cons (cons s e)
                 (if (< (cdar a) (cdar b))
                     (cs-intersect (cdr a) b)
                     (cs-intersect a (cdr b))))))))

(define-public (cs-difference a b)
  (cs-intersect a (cs-negate b)))

(define-public (cs-does-intersect? a b)
  (cond ((or (null? a) (null? b)) #f)
        ((<= (cdar a) (caar b)) (cs-does-intersect? (cdr a) b))
        ((<= (cdar b) (caar a)) (cs-does-intersect? a (cdr b)))
        (else #t)))

(define-public (cs-contains-range? cs start end)
  (cond ((null? cs) #f)
        ((<= (cdar cs) start) (cs-contains-range? (cdr cs) start end))
        (else (and (<= (caar cs) start) (>= (cdar cs) end)))))

(define-public (cs-contains? cs c) (cs-contains-range? cs c (+ c 1)))

(define-public (cs-count cs)
  (let loop ((cs cs) (n 0))
    (if (null? cs) n (loop (cdr cs) (+ n (- (cdar cs) (caar cs)))))))

(define-public (cs-compare a b)
  ;; the order of tree-sitter's CharacterSet: number of characters, then
  ;; range by range, by length and then by the characters
  (let ((c (ts-cmp (cs-count a) (cs-count b))))
    (if (not (= c 0)) c
        (let loop ((a a) (b b))
          (if (or (null? a) (null? b)) 0
              (let* ((la (- (cdar a) (caar a)))
                     (lb (- (cdar b) (caar b)))
                     (c (ts-cmp la lb)))
                (cond ((not (= c 0)) c)
                      ((not (= (caar a) (caar b))) (ts-cmp (caar a) (caar b)))
                      (else (loop (cdr a) (cdr b))))))))))

(define-public (cs-simplify-ignoring cs ruled-out)
  ;; fewer ranges, assuming the characters of ruled-out never occur
  (let loop ((l cs) (prev #f) (acc '()))
    (cond ((null? l) (reverse (if prev (cons prev acc) acc)))
          ((cs-contains-range? ruled-out (caar l) (cdar l)) (loop (cdr l) prev acc))
          ((and prev (cs-contains-range? ruled-out (cdr prev) (caar l)))
           (loop (cdr l) (cons (car prev) (cdar l)) acc))
          (else (loop (cdr l) (car l) (if prev (cons prev acc) acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTF-8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (utf8->codes s)
  ;; the code points of a string of UTF-8 bytes
  (let ((n (string-length s)))
    (define (byte i) (char->integer (string-ref s i)))
    (define (cont i k acc)
      (if (= k 0) acc
          (cont (+ i 1) (- k 1)
                (+ (* acc 64) (- (byte i) 128)))))
    (let loop ((i 0) (acc '()))
      (if (>= i n) (reverse acc)
          (let ((b (byte i)))
            (cond ((< b #x80) (loop (+ i 1) (cons b acc)))
                  ((< b #xe0) (loop (+ i 2) (cons (cont (+ i 1) 1 (- b #xc0)) acc)))
                  ((< b #xf0) (loop (+ i 3) (cons (cont (+ i 1) 2 (- b #xe0)) acc)))
                  (else (loop (+ i 4) (cons (cont (+ i 1) 3 (- b #xf0)) acc)))))))))

(define-public (code->utf8 c)
  (define (ch x) (string (integer->char x)))
  (cond ((< c #x80) (ch c))
        ((< c #x800)
         (string-append (ch (+ #xc0 (quotient c 64)))
                        (ch (+ #x80 (remainder c 64)))))
        ((< c #x10000)
         (string-append (ch (+ #xe0 (quotient c 4096)))
                        (ch (+ #x80 (remainder (quotient c 64) 64)))
                        (ch (+ #x80 (remainder c 64)))))
        (else
         (string-append (ch (+ #xf0 (quotient c 262144)))
                        (ch (+ #x80 (remainder (quotient c 4096) 64)))
                        (ch (+ #x80 (remainder (quotient c 64) 64)))
                        (ch (+ #x80 (remainder c 64)))))))

(define-public (codes->utf8 l)
  (apply string-append (map code->utf8 l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Objects are read as (obj (key . value) ...) in the order of the text,
;; arrays as (arr value ...), strings stay UTF-8, true, false and null are
;; #t, #f and 'null.

(define-public (json-get obj key . default)
  (let ((h (assoc key (cdr obj))))
    (cond (h (cdr h))
          ((pair? default) (car default))
          (else #f))))

(define-public (json-arr-items x) (if (and (pair? x) (eq? (car x) 'arr)) (cdr x) '()))

(define-public (string->json s)
  (let ((n (string-length s)) (i 0))
    (define (peek) (if (< i n) (string-ref s i) #\nul))
    (define (next!) (let ((c (peek))) (set! i (+ i 1)) c))
    (define (skip-ws!)
      (when (and (< i n) (memv (peek) '(#\space #\tab #\newline #\return)))
        (set! i (+ i 1))
        (skip-ws!)))
    (define (expect! c)
      (skip-ws!)
      (if (char=? (peek) c) (next!)
          (error "json: expected" c 'at i)))
    (define (hex4)
      (let loop ((k 0) (v 0))
        (if (= k 4) v
            (loop (+ k 1)
                  (+ (* v 16)
                     (let ((c (char->integer (next!))))
                       (cond ((<= 48 c 57) (- c 48))
                             ((<= 97 c 102) (- c 87))
                             (else (- c 55)))))))))
    (define (read-string)
      (next!)
      (let loop ((acc '()))
        (let ((c (next!)))
          (cond ((char=? c #\") (apply string-append (reverse acc)))
                ((char=? c #\\)
                 (let ((e (next!)))
                   (case e
                     ((#\n) (loop (cons (string #\newline) acc)))
                     ((#\t) (loop (cons (string #\tab) acc)))
                     ((#\r) (loop (cons (string #\return) acc)))
                     ((#\b) (loop (cons (string (integer->char 8)) acc)))
                     ((#\f) (loop (cons (string (integer->char 12)) acc)))
                     ((#\u)
                      (let ((c (hex4)))
                        (if (and (>= c #xd800) (< c #xdc00)
                                 (char=? (peek) #\\))
                            (begin (next!) (next!)
                                   (let ((lo (hex4)))
                                     (loop (cons (code->utf8
                                                  (+ #x10000
                                                     (* (- c #xd800) 1024)
                                                     (- lo #xdc00)))
                                                 acc))))
                            (loop (cons (code->utf8 c) acc)))))
                     (else (loop (cons (string e) acc))))))
                (else (loop (cons (string c) acc)))))))
    (define (read-number)
      (let ((start i))
        (let loop ()
          (when (memv (peek) '(#\- #\+ #\. #\e #\E #\0 #\1 #\2 #\3 #\4 #\5
                               #\6 #\7 #\8 #\9))
            (next!) (loop)))
        (string->number (substring s start i))))
    (define (read-word w val)
      (set! i (+ i (string-length w)))
      val)
    (define (read-value)
      (skip-ws!)
      (let ((c (peek)))
        (cond ((char=? c #\{)
               (next!) (skip-ws!)
               (if (char=? (peek) #\}) (begin (next!) (list 'obj))
                   (let loop ((acc '()))
                     (skip-ws!)
                     (let ((k (read-string)))
                       (expect! #\:)
                       (let* ((v (read-value))
                              (acc (cons (cons k v) acc)))
                         (skip-ws!)
                         (if (char=? (next!) #\,) (loop acc)
                             (cons 'obj (reverse acc))))))))
              ((char=? c #\[)
               (next!) (skip-ws!)
               (if (char=? (peek) #\]) (begin (next!) (list 'arr))
                   (let loop ((acc '()))
                     (let ((v (read-value)))
                       (skip-ws!)
                       (if (char=? (next!) #\,) (loop (cons v acc))
                           (cons 'arr (reverse (cons v acc))))))))
              ((char=? c #\") (read-string))
              ((char=? c #\t) (read-word "true" #t))
              ((char=? c #\f) (read-word "false" #f))
              ((char=? c #\n) (read-word "null" 'null))
              (else (read-number)))))
    (read-value)))

(define-public (ts-read-file name)
  (call-with-input-file name
    (lambda (port)
      (let loop ((acc '()))
        (let ((c (read-char port)))
          (if (eof-object? c)
              (list->string (reverse acc))
              (loop (cons c acc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define profile-start #f)
(define profile-last 0)
(define-public (ts-profile what)
  ;; with TS_PROFILE=1, the time since the last step, in ms
  (when (and (defined? 'getenv) (getenv "TS_PROFILE"))
    (let ((now (texmacs-time)))
      (when profile-start
        (display* "ts-profile: " what " " (- now profile-last) " ms\n"))
      (set! profile-start #t)
      (set! profile-last now))))
