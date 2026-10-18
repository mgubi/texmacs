;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-yaml.scm
;; DESCRIPTION : the external scanner of tree-sitter-yaml
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c and src/schema.core.c of
;; tree-sitter-grammars/tree-sitter-yaml (MIT): YAML is scanned almost
;; entirely here (directives, block and flow structures with their
;; indentation, strings, plain scalars and their type in the core schema,
;; tags, anchors, aliases, comments). The C code is followed closely: the
;; rows and columns are 16-bit integers, the scalar types come from the
;; state machine of schema.core.c (made into the table below), and two flow
;; ends give the symbol of another context, as in C. The protocol of the
;; scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-yaml)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-scan)))

(define t-end-of-file 0)
(define t-s-dir-yml-bgn 1)
(define t-r-dir-yml-ver 2)
(define t-s-dir-tag-bgn 3)
(define t-r-dir-tag-hdl 4)
(define t-r-dir-tag-pfx 5)
(define t-s-dir-rsv-bgn 6)
(define t-r-dir-rsv-prm 7)
(define t-s-drs-end 8)
(define t-s-doc-end 9)
(define t-r-blk-seq-bgn 10)
(define t-br-blk-seq-bgn 11)
(define t-b-blk-seq-bgn 12)
(define t-r-blk-key-bgn 13)
(define t-br-blk-key-bgn 14)
(define t-b-blk-key-bgn 15)
(define t-r-blk-val-bgn 16)
(define t-br-blk-val-bgn 17)
(define t-b-blk-val-bgn 18)
(define t-r-blk-imp-bgn 19)
(define t-r-blk-lit-bgn 20)
(define t-br-blk-lit-bgn 21)
(define t-r-blk-fld-bgn 22)
(define t-br-blk-fld-bgn 23)
(define t-br-blk-str-ctn 24)
(define t-r-flw-seq-bgn 25)
(define t-br-flw-seq-bgn 26)
(define t-b-flw-seq-bgn 27)
(define t-r-flw-seq-end 28)
(define t-br-flw-seq-end 29)
(define t-b-flw-seq-end 30)
(define t-r-flw-map-bgn 31)
(define t-br-flw-map-bgn 32)
(define t-b-flw-map-bgn 33)
(define t-r-flw-map-end 34)
(define t-br-flw-map-end 35)
(define t-b-flw-map-end 36)
(define t-r-flw-sep-bgn 37)
(define t-br-flw-sep-bgn 38)
(define t-r-flw-key-bgn 39)
(define t-br-flw-key-bgn 40)
(define t-r-flw-jsv-bgn 41)
(define t-br-flw-jsv-bgn 42)
(define t-r-flw-njv-bgn 43)
(define t-br-flw-njv-bgn 44)
(define t-r-dqt-str-bgn 45)
(define t-br-dqt-str-bgn 46)
(define t-b-dqt-str-bgn 47)
(define t-r-dqt-str-ctn 48)
(define t-br-dqt-str-ctn 49)
(define t-r-dqt-esc-nwl 50)
(define t-br-dqt-esc-nwl 51)
(define t-r-dqt-esc-seq 52)
(define t-br-dqt-esc-seq 53)
(define t-r-dqt-str-end 54)
(define t-br-dqt-str-end 55)
(define t-r-sqt-str-bgn 56)
(define t-br-sqt-str-bgn 57)
(define t-b-sqt-str-bgn 58)
(define t-r-sqt-str-ctn 59)
(define t-br-sqt-str-ctn 60)
(define t-r-sqt-esc-sqt 61)
(define t-br-sqt-esc-sqt 62)
(define t-r-sqt-str-end 63)
(define t-br-sqt-str-end 64)
(define t-r-sgl-pln-nul-blk 65)
(define t-br-sgl-pln-nul-blk 66)
(define t-b-sgl-pln-nul-blk 67)
(define t-r-sgl-pln-nul-flw 68)
(define t-br-sgl-pln-nul-flw 69)
(define t-r-sgl-pln-bol-blk 70)
(define t-br-sgl-pln-bol-blk 71)
(define t-b-sgl-pln-bol-blk 72)
(define t-r-sgl-pln-bol-flw 73)
(define t-br-sgl-pln-bol-flw 74)
(define t-r-sgl-pln-int-blk 75)
(define t-br-sgl-pln-int-blk 76)
(define t-b-sgl-pln-int-blk 77)
(define t-r-sgl-pln-int-flw 78)
(define t-br-sgl-pln-int-flw 79)
(define t-r-sgl-pln-flt-blk 80)
(define t-br-sgl-pln-flt-blk 81)
(define t-b-sgl-pln-flt-blk 82)
(define t-r-sgl-pln-flt-flw 83)
(define t-br-sgl-pln-flt-flw 84)
(define t-r-sgl-pln-tms-blk 85)
(define t-br-sgl-pln-tms-blk 86)
(define t-b-sgl-pln-tms-blk 87)
(define t-r-sgl-pln-tms-flw 88)
(define t-br-sgl-pln-tms-flw 89)
(define t-r-sgl-pln-str-blk 90)
(define t-br-sgl-pln-str-blk 91)
(define t-b-sgl-pln-str-blk 92)
(define t-r-sgl-pln-str-flw 93)
(define t-br-sgl-pln-str-flw 94)
(define t-r-mtl-pln-str-blk 95)
(define t-br-mtl-pln-str-blk 96)
(define t-r-mtl-pln-str-flw 97)
(define t-br-mtl-pln-str-flw 98)
(define t-r-tag 99)
(define t-br-tag 100)
(define t-b-tag 101)
(define t-r-acr-bgn 102)
(define t-br-acr-bgn 103)
(define t-b-acr-bgn 104)
(define t-r-acr-ctn 105)
(define t-r-als-bgn 106)
(define t-br-als-bgn 107)
(define t-b-als-bgn 108)
(define t-r-als-ctn 109)
(define t-bl 110)
(define t-comment 111)
(define t-err-rec 112)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The core schema (schema.core.c)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rs-str 0)
(define rs-int 1)
(define rs-null 2)
(define rs-bool 3)
(define rs-float 4)
(define sch-stt-frz -1)

;; for each state: (result-set-first rules), a rule (ranges result next)
(define schema
  (list->vector
   '(
    (#f ((((46 . 46)) 0 6) (((48 . 48)) 1 37) (((70 . 70)) 0 2) (((78 . 78)) 0 16) (((84 . 84)) 0 13) (((102 . 102)) 0 17) (((110 . 110)) 0 29) (((116 . 116)) 0 26) (((126 . 126)) 2 35) (((43 . 43)) 0 1) (((45 . 45)) 0 1) (((49 . 57)) 1 38)))
    (#f ((((46 . 46)) 0 7) (((48 . 57)) 1 38)))
    (#f ((((65 . 65)) 0 9) (((97 . 97)) 0 22)))
    (#f ((((65 . 65)) 0 12) (((97 . 97)) 0 12)))
    (#f ((((69 . 69)) 3 36)))
    (#f ((((70 . 70)) 4 41)))
    (#f ((((73 . 73)) 0 11) (((78 . 78)) 0 3) (((105 . 105)) 0 24) (((110 . 110)) 0 18) (((48 . 57)) 4 42)))
    (#f ((((73 . 73)) 0 11) (((105 . 105)) 0 24) (((48 . 57)) 4 42)))
    (#f ((((76 . 76)) 2 35)))
    (#f ((((76 . 76)) 0 14)))
    (#f ((((76 . 76)) 0 8)))
    (#f ((((78 . 78)) 0 5) (((110 . 110)) 0 20)))
    (#f ((((78 . 78)) 4 41)))
    (#f ((((82 . 82)) 0 15) (((114 . 114)) 0 28)))
    (#f ((((83 . 83)) 0 4)))
    (#f ((((85 . 85)) 0 4)))
    (#f ((((85 . 85)) 0 10) (((117 . 117)) 0 23)))
    (#f ((((97 . 97)) 0 22)))
    (#f ((((97 . 97)) 0 25)))
    (#f ((((101 . 101)) 3 36)))
    (#f ((((102 . 102)) 4 41)))
    (#f ((((108 . 108)) 2 35)))
    (#f ((((108 . 108)) 0 27)))
    (#f ((((108 . 108)) 0 21)))
    (#f ((((110 . 110)) 0 20)))
    (#f ((((110 . 110)) 4 41)))
    (#f ((((114 . 114)) 0 28)))
    (#f ((((115 . 115)) 0 19)))
    (#f ((((117 . 117)) 0 19)))
    (#f ((((117 . 117)) 0 23)))
    (#f ((((43 . 43) (45 . 45)) 0 32) (((48 . 57)) 4 43)))
    (#f ((((48 . 55)) 1 39)))
    (#f ((((48 . 57)) 4 43)))
    (#f ((((48 . 57) (65 . 70) (97 . 102)) 1 40)))
    (#f ())
    (2 ())
    (3 ())
    (1 ((((46 . 46)) 4 42) (((111 . 111)) 0 31) (((120 . 120)) 0 33) (((69 . 69) (101 . 101)) 0 30) (((48 . 57)) 1 38)))
    (1 ((((46 . 46)) 4 42) (((69 . 69) (101 . 101)) 0 30) (((48 . 57)) 1 38)))
    (1 ((((48 . 55)) 1 39)))
    (1 ((((48 . 57) (65 . 70) (97 . 102)) 1 40)))
    (4 ())
    (4 ((((69 . 69) (101 . 101)) 0 30) (((48 . 57)) 4 42)))
    (4 ((((48 . 57)) 4 43))))))

(define (in-ranges? c l)
  (and (pair? l)
       (or (and (<= (caar l) c) (<= c (cdar l))) (in-ranges? c (cdr l)))))

(define (adv-sch-stt p stt c)
  ;; the next state of the schema after the character c; sets rlt-sch
  (call-with-exit
   (lambda (out)
     (when (and (>= stt 0) (< stt (vector-length schema)))
       (let ((e (vector-ref schema stt)))
         (when (car e) (vector-set! p 13 (car e)))
         (for-each (lambda (rule)
                     (when (in-ranges? c (car rule))
                       (vector-set! p 13 (cadr rule))
                       (out (caddr rule))))
                   (cadr e))))
     (when (>= stt (vector-length schema))   ; the default case of C
       (vector-set! p 13 rs-str)
       (out sch-stt-frz))
     (unless (memv c '(13 10 32 0)) (vector-set! p 13 rs-str))
     sch-stt-frz)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the payload is #(row col blk-imp-row blk-imp-col blk-imp-tab typs lens
;; end-row end-col cur-row cur-col cur-chr sch-stt rlt-sch): the stacks of
;; the indentations (their types and lengths, the innermost first, the root
;; at the bottom), then the temporary fields of a scan

(define ind-rot 114)   ; r
(define ind-map 109)   ; m
(define ind-seq 113)   ; q
(define ind-str 115)   ; s

(define (i16 x) (- (modulo (+ x 32768) 65536) 32768))

(define (yaml-reset! p)
  (vector-set! p 0 0) (vector-set! p 1 0)
  (vector-set! p 2 -1) (vector-set! p 3 -1) (vector-set! p 4 0)
  (vector-set! p 5 (list ind-rot))
  (vector-set! p 6 (list -1)))

(define (yaml-create)
  (let ((p (make-vector 14 0))) (yaml-reset! p) p))

(define (yaml-serialize p)
  ;; never #f: as in C, the state is never empty
  (list (vector-ref p 0) (vector-ref p 1) (vector-ref p 2) (vector-ref p 3)
        (vector-ref p 4) (vector-ref p 5) (vector-ref p 6)))

(define (yaml-deserialize p state)
  (yaml-reset! p)
  (when state
    (vector-set! p 0 (list-ref state 0)) (vector-set! p 1 (list-ref state 1))
    (vector-set! p 2 (list-ref state 2)) (vector-set! p 3 (list-ref state 3))
    (vector-set! p 4 (list-ref state 4))
    (vector-set! p 5 (list-ref state 5)) (vector-set! p 6 (list-ref state 6))))

(define (la lx) (lexer-lookahead lx))

(define (adv p lx)
  (vector-set! p 10 (i16 (+ (vector-ref p 10) 1)))
  (vector-set! p 11 (la lx))
  (lexer-advance! lx #f))

(define (adv-nwl p lx)
  (vector-set! p 9 (i16 (+ (vector-ref p 9) 1)))
  (vector-set! p 10 0)
  (vector-set! p 11 (la lx))
  (lexer-advance! lx #f))

(define (skp p lx)
  (vector-set! p 10 (i16 (+ (vector-ref p 10) 1)))
  (vector-set! p 11 (la lx))
  (lexer-advance! lx #t))

(define (skp-nwl p lx)
  (vector-set! p 9 (i16 (+ (vector-ref p 9) 1)))
  (vector-set! p 10 0)
  (vector-set! p 11 (la lx))
  (lexer-advance! lx #t))

(define (mrk-end p lx)
  (vector-set! p 7 (vector-ref p 9))
  (vector-set! p 8 (vector-ref p 10))
  (lexer-mark-end! lx))

(define (ret p lx sym)
  ;; RET_SYM: flush, the result, #t
  (vector-set! p 0 (vector-ref p 7))
  (vector-set! p 1 (vector-ref p 8))
  (lexer-set-result! lx sym)
  #t)

(define (cur-col p) (vector-ref p 10))
(define (cur-row p) (vector-ref p 9))
(define (cur-chr p) (vector-ref p 11))

(define (pop-ind! p)
  (vector-set! p 5 (cdr (vector-ref p 5)))
  (vector-set! p 6 (cdr (vector-ref p 6))))

(define (push-ind! p typ len)
  (vector-set! p 5 (cons typ (vector-ref p 5)))
  (vector-set! p 6 (cons (i16 len) (vector-ref p 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wsp? c) (or (= c 32) (= c 9)))
(define (nwl? c) (or (= c 13) (= c 10)))
(define (wht? c) (or (wsp? c) (nwl? c) (= c 0)))
(define (dec-digit? c) (and (>= c 48) (<= c 57)))
(define (hex-digit? c)
  (or (dec-digit? c) (and (>= c 97) (<= c 102)) (and (>= c 65) (<= c 70))))
(define (word-char? c)
  (or (= c 45) (dec-digit? c) (and (>= c 97) (<= c 122)) (and (>= c 65) (<= c 90))))
(define (nb-json? c) (or (= c 9) (and (>= c 32) (<= c #x10ffff))))
(define (nb-double-char? c) (and (nb-json? c) (not (= c 92)) (not (= c 34))))
(define (nb-single-char? c) (and (nb-json? c) (not (= c 39))))
(define (ns-char? c)
  (or (and (>= c #x21) (<= c #x7e)) (= c #x85) (and (>= c #xa0) (<= c #xd7ff))
      (and (>= c #xe000) (<= c #xfefe)) (and (>= c #xff00) (<= c #xfffd))
      (and (>= c #x10000) (<= c #x10ffff))))
(define (c-indicator? c)
  (memv c '(45 63 58 44 91 93 123 125 35 38 42 33 124 62 39 34 37 64 96)))
(define (c-flow-indicator? c) (memv c '(44 91 93 123 125)))
(define (plain-safe-in-block? c) (ns-char? c))
(define (plain-safe-in-flow? c) (and (ns-char? c) (not (c-flow-indicator? c))))
(define (uri-char? c)
  (or (word-char? c)
      (memv c '(35 59 47 63 58 64 38 61 43 36 44 95 46 33 126 42 39 40 41 91 93))))
(define (tag-char? c)
  (or (word-char? c)
      (memv c '(35 59 47 63 58 64 38 61 43 36 95 46 126 42 39 40 41))))
(define (anchor-char? c) (and (ns-char? c) (not (c-flow-indicator? c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The pieces (status: 'fail, 'stop or 'succ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scn-uri-esc p lx)
  (if (not (= (la lx) 37)) 'stop
      (begin
        (mrk-end p lx)
        (adv p lx)
        (if (not (hex-digit? (la lx))) 'fail
            (begin
              (adv p lx)
              (if (not (hex-digit? (la lx))) 'fail
                  (begin (adv p lx) 'succ)))))))

(define (scn-ns-uri-char p lx)
  (if (uri-char? (la lx)) (begin (adv p lx) 'succ) (scn-uri-esc p lx)))

(define (scn-ns-tag-char p lx)
  (if (tag-char? (la lx)) (begin (adv p lx) 'succ) (scn-uri-esc p lx)))

(define (scn-word p lx s)
  ;; advance over the characters of s while they match; whether they all did
  (let loop ((i 0))
    (cond ((= i (string-length s)) #t)
          ((= (la lx) (char->integer (string-ref s i))) (adv p lx) (loop (+ i 1)))
          (else #f))))

(define (scn-dir-bgn p lx)
  (adv p lx)
  (call-with-exit
   (lambda (out)
     (cond ((= (la lx) 89)   ; Y
            (when (and (scn-word p lx "YAML") (wht? (la lx)))
              (mrk-end p lx)
              (out (ret p lx t-s-dir-yml-bgn))))
           ((= (la lx) 84)   ; T
            (when (and (scn-word p lx "TAG") (wht? (la lx)))
              (mrk-end p lx)
              (out (ret p lx t-s-dir-tag-bgn)))))
     (let loop () (when (ns-char? (la lx)) (adv p lx) (loop)))
     (and (> (cur-col p) 1) (wht? (la lx))
          (begin (mrk-end p lx) (ret p lx t-s-dir-rsv-bgn))))))

(define (scn-dir-yml-ver p lx sym)
  (let ((n1 (let loop ((n 0)) (if (dec-digit? (la lx)) (begin (adv p lx) (loop (+ n 1))) n))))
    (and (= (la lx) 46)
         (begin
           (adv p lx)
           (let ((n2 (let loop ((n 0)) (if (dec-digit? (la lx)) (begin (adv p lx) (loop (+ n 1))) n))))
             (and (> n1 0) (> n2 0)
                  (begin (mrk-end p lx) (ret p lx sym))))))))

(define (scn-tag-hdl-tal p lx)
  (if (= (la lx) 33) (begin (adv p lx) #t)
      (let ((n (let loop ((n 0)) (if (word-char? (la lx)) (begin (adv p lx) (loop (+ n 1))) n))))
        (cond ((= n 0) #t)
              ((= (la lx) 33) (adv p lx) #t)
              (else #f)))))

(define (scn-dir-tag-hdl p lx sym)
  (and (= (la lx) 33)
       (begin (adv p lx) (scn-tag-hdl-tal p lx))
       (begin (mrk-end p lx) (ret p lx sym))))

(define (scn-dir-tag-pfx p lx sym)
  (and (cond ((= (la lx) 33) (adv p lx) #t)
             ((eq? (scn-ns-tag-char p lx) 'succ) #t)
             (else #f))
       (let loop ()
         (let ((r (scn-ns-uri-char p lx)))
           (cond ((eq? r 'stop) (mrk-end p lx) (ret p lx sym))
                 ((eq? r 'fail) (ret p lx sym))
                 (else (loop)))))))

(define (scn-dir-rsv-prm p lx sym)
  (and (ns-char? (la lx))
       (begin
         (adv p lx)
         (let loop () (when (ns-char? (la lx)) (adv p lx) (loop)))
         (mrk-end p lx)
         (ret p lx sym))))

(define (scn-tag p lx sym)
  (and (= (la lx) 33)
       (begin
         (adv p lx)
         (cond ((wht? (la lx)) (mrk-end p lx) (ret p lx sym))
               ((= (la lx) 60)   ; <
                (adv p lx)
                (and (eq? (scn-ns-uri-char p lx) 'succ)
                     (let loop ()
                       (let ((r (scn-ns-uri-char p lx)))
                         (cond ((eq? r 'stop)
                                (and (= (la lx) 62)
                                     (begin (adv p lx) (mrk-end p lx) (ret p lx sym))))
                               ((eq? r 'fail) #f)
                               (else (loop)))))))
               (else
                (and (not (and (scn-tag-hdl-tal p lx)
                               (not (eq? (scn-ns-tag-char p lx) 'succ))))
                     (let loop ()
                       (let ((r (scn-ns-tag-char p lx)))
                         (cond ((eq? r 'stop) (mrk-end p lx) (ret p lx sym))
                               ((eq? r 'fail) (ret p lx sym))
                               (else (loop)))))))))))

(define (scn-anchor-bgn p lx c sym)
  ;; & or * followed by an anchor character
  (and (= (la lx) c)
       (begin (adv p lx) (anchor-char? (la lx)))
       (begin (mrk-end p lx) (ret p lx sym))))

(define (scn-anchor-ctn p lx sym)
  (let loop () (when (anchor-char? (la lx)) (adv p lx) (loop)))
  (mrk-end p lx)
  (ret p lx sym))

(define (scn-dqt-esc-seq p lx sym)
  (let ((c (la lx)))
    (define (hex! n)
      (let loop ((i 0))
        (cond ((= i n) #t)
              ((hex-digit? (la lx)) (adv p lx) (loop (+ i 1)))
              (else #f))))
    (and (cond ((memv c '(48 97 98 116 9 110 118 114 101 102 32 34 47 92 78 95 76 80))
                (adv p lx) #t)
               ((= c 85) (adv p lx) (hex! 8))    ; U
               ((= c 117) (adv p lx) (hex! 4))   ; u
               ((= c 120) (adv p lx) (hex! 2))   ; x
               (else #f))
         (begin (mrk-end p lx) (ret p lx sym)))))

(define (scn-drs-doc-end p lx)
  ;; --- or ... followed by white space
  (let ((d (la lx)))
    (and (or (= d 45) (= d 46))
         (begin
           (adv p lx)
           (or (and (= (la lx) d)
                    (begin (adv p lx) (= (la lx) d))
                    (begin (adv p lx) (wht? (la lx))))
               (begin (mrk-end p lx) #f))))))

(define (scn-qt-str-cnt p lx ok? sym)
  ;; the content of a double (ok? nb-double-char?) or single quoted string
  (and (ok? (la lx))
       (if (and (= (cur-col p) 0) (scn-drs-doc-end p lx))
           (begin
             (mrk-end p lx)
             (ret p lx (if (= (cur-chr p) 45) t-s-drs-end t-s-doc-end)))
           (begin
             (adv p lx)
             (let loop () (when (ok? (la lx)) (adv p lx) (loop)))
             (mrk-end p lx)
             (ret p lx sym)))))

(define (scn-blk-str-bgn p lx sym)
  (and (or (= (la lx) 124) (= (la lx) 62))
       (begin
         (adv p lx)
         (let ((cur-ind (car (vector-ref p 6))) (ind -1))
           (cond ((and (>= (la lx) 49) (<= (la lx) 57))
                  (set! ind (- (la lx) 49))
                  (adv p lx)
                  (when (or (= (la lx) 43) (= (la lx) 45)) (adv p lx)))
                 ((or (= (la lx) 43) (= (la lx) 45))
                  (adv p lx)
                  (when (and (>= (la lx) 49) (<= (la lx) 57))
                    (set! ind (- (la lx) 49))
                    (adv p lx))))
           (and (wht? (la lx))
                (begin
                  (mrk-end p lx)
                  (if (not (= ind -1))
                      (set! ind (+ ind cur-ind))
                      (begin
                        (set! ind cur-ind)
                        (let loop () (when (wsp? (la lx)) (adv p lx) (loop)))
                        (when (= (la lx) 35)
                          (adv p lx)
                          (let loop ()
                            (unless (or (nwl? (la lx)) (= (la lx) 0)) (adv p lx) (loop))))
                        (when (nwl? (la lx)) (adv-nwl p lx))
                        (let loop ()
                          (unless (= (la lx) 0)
                            (cond ((= (la lx) 32) (adv p lx) (loop))
                                  ((nwl? (la lx))
                                   (unless (< (- (cur-col p) 1) ind)
                                     (set! ind (- (cur-col p) 1))
                                     (adv-nwl p lx)
                                     (loop)))
                                  (else
                                   (when (> (- (cur-col p) 1) ind)
                                     (set! ind (- (cur-col p) 1)))))))))
                  (push-ind! p ind-str ind)
                  (ret p lx sym)))))))

(define (scn-blk-str-cnt p lx sym)
  (and (ns-char? (la lx))
       (call-with-exit
        (lambda (out)
          (if (and (= (cur-col p) 0) (scn-drs-doc-end p lx))
              (begin
                (when (null? (cdr (vector-ref p 5))) (out #f))
                (pop-ind! p)
                (out (ret p lx t-bl)))
              (adv p lx))
          (mrk-end p lx)
          (let loop ()
            (when (ns-char? (la lx))
              (adv p lx)
              (let more () (when (ns-char? (la lx)) (adv p lx) (more)))
              (mrk-end p lx))
            (when (wsp? (la lx))
              (adv p lx)
              (let more () (when (wsp? (la lx)) (adv p lx) (more)))
              (loop)))
          (ret p lx sym)))))

(define (scn-pln-cnt p lx safe?)
  ;; the rest of a plain scalar on a line: 'succ, 'stop or 'fail
  (let ((cur-wsp (wsp? (cur-chr p))) (cur-saf (safe? (cur-chr p)))
        (lka-wsp (wsp? (la lx))) (lka-saf (safe? (la lx))))
    (define (sch!) (vector-set! p 12 (adv-sch-stt p (vector-ref p 12) (cur-chr p))))
    (if (not (or lka-saf lka-wsp)) 'stop
        (call-with-exit
         (lambda (out)
           (let loop ()
             (cond ((and lka-saf (not (= (la lx) 35)) (not (= (la lx) 58)))
                    (adv p lx) (mrk-end p lx) (sch!))
                   ((and cur-saf (= (la lx) 35))
                    (adv p lx) (mrk-end p lx) (sch!))
                   (lka-wsp (adv p lx) (sch!))
                   ((= (la lx) 58) (adv p lx))
                   (else (out 'succ)))
             (set! cur-wsp lka-wsp)
             (set! cur-saf lka-saf)
             (set! lka-wsp (wsp? (la lx)))
             (set! lka-saf (safe? (la lx)))
             (when (= (cur-chr p) 58)
               (if lka-saf
                   (begin (mrk-end p lx) (sch!))
                   (out 'fail)))
             (loop)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sgl-pln-sym p pos ctx)
  ;; the symbol of a single line plain scalar of the type found by the schema
  (let* ((r (vector-ref p 13))
         (k (cond ((= r rs-null) 0) ((= r rs-bool) 1) ((= r rs-int) 2)
                  ((= r rs-float) 3) (else 5)))
         (base (cond ((and (eq? pos 'r) (eq? ctx 'blk)) t-r-sgl-pln-nul-blk)
                     ((and (eq? pos 'br) (eq? ctx 'blk)) t-br-sgl-pln-nul-blk)
                     ((eq? pos 'b) t-b-sgl-pln-nul-blk)
                     ((eq? pos 'r) t-r-sgl-pln-nul-flw)
                     (else t-br-sgl-pln-nul-flw))))
    ;; the five symbols of a type follow each other, NUL BOL INT FLT TMS STR
    (+ base (* k 5))))

(define (yaml-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  ;; init
  (vector-set! p 9 (vector-ref p 0))
  (vector-set! p 10 (vector-ref p 1))
  (vector-set! p 11 0)
  (vector-set! p 12 0)
  (vector-set! p 13 rs-str)
  (mrk-end p lx)
  (call-with-exit
   (lambda (out)
     (define (pop-ind-or-fail!)
       ;; POP_IND: the root stays (an error recovery may get here)
       (when (null? (cdr (vector-ref p 5))) (out #f))
       (pop-ind! p))
     (define (rt sym) (out (ret p lx sym)))
     (let* ((allow-comment (not (or (valid? t-r-dqt-str-ctn) (valid? t-br-dqt-str-ctn)
                                    (valid? t-r-sqt-str-ctn) (valid? t-br-sqt-str-ctn))))
            (lens (vector-ref p 6))
            (cur-ind (car lens))
            (prt-ind (if (pair? (cdr lens)) (cadr lens) -1))
            (cur-ind-typ (car (vector-ref p 5)))
            (has-tab-ind #f)
            (leading-spaces 0))
       ;; spaces, newlines and comments
       (let loop ()
         (let ((c (la lx)))
           (cond ((= c 32)
                  (unless has-tab-ind (set! leading-spaces (+ leading-spaces 1)))
                  (skp p lx)
                  (loop))
                 ((= c 9) (set! has-tab-ind #t) (skp p lx) (loop))
                 ((nwl? c)
                  (set! has-tab-ind #f)
                  (set! leading-spaces 0)
                  (skp-nwl p lx)
                  (loop))
                 ((and allow-comment (= c 35))
                  (when (and (valid? t-br-blk-str-ctn) (valid? t-bl) (<= (cur-col p) cur-ind))
                    (pop-ind-or-fail!)
                    (rt t-bl))
                  (when (if (valid? t-br-blk-str-ctn)
                            (= (cur-row p) (vector-ref p 0))
                            (or (= (cur-col p) 0) (not (= (cur-row p) (vector-ref p 0)))
                                (> (cur-col p) (vector-ref p 1))))
                    (adv p lx)
                    (let more () (unless (or (nwl? (la lx)) (= (la lx) 0)) (adv p lx) (more)))
                    (mrk-end p lx)
                    (rt t-comment))))))
       (when (= (la lx) 0)
         (when (valid? t-bl)
           (mrk-end p lx)
           (pop-ind-or-fail!)
           (rt t-bl))
         (when (valid? t-end-of-file)
           (mrk-end p lx)
           (rt t-end-of-file))
         (out #f))
       (let* ((bgn-row (cur-row p)) (bgn-col (cur-col p)) (bgn-chr (la lx)))
         (define (push-bgn-ind! typ)
           (when has-tab-ind (out #f))
           (push-ind! p typ bgn-col))
         (define (may-upd-imp-col!)
           (unless (= (vector-ref p 2) bgn-row)
             (vector-set! p 2 bgn-row)
             (vector-set! p 3 bgn-col)
             (vector-set! p 4 (if has-tab-ind 1 0))))
         (when (and (valid? t-bl) (<= bgn-col cur-ind) (not has-tab-ind))
           (when (if (and (= cur-ind prt-ind) (= cur-ind-typ ind-seq))
                     (or (< bgn-col cur-ind) (not (= (la lx) 45)))
                     (or (<= bgn-col prt-ind) (= cur-ind-typ ind-str)))
             (pop-ind-or-fail!)
             (rt t-bl)))
         (let* ((has-nwl (> (cur-row p) (vector-ref p 0)))
                (is-r (not has-nwl))
                (is-br (and has-nwl (> leading-spaces cur-ind)))
                (is-b (and has-nwl (= leading-spaces cur-ind) (not has-tab-ind)))
                (is-s (= bgn-col 0))
                (c (la lx)))
           (when (and (valid? t-r-dir-yml-ver) is-r) (out (scn-dir-yml-ver p lx t-r-dir-yml-ver)))
           (when (and (valid? t-r-dir-tag-hdl) is-r) (out (scn-dir-tag-hdl p lx t-r-dir-tag-hdl)))
           (when (and (valid? t-r-dir-tag-pfx) is-r) (out (scn-dir-tag-pfx p lx t-r-dir-tag-pfx)))
           (when (and (valid? t-r-dir-rsv-prm) is-r) (out (scn-dir-rsv-prm p lx t-r-dir-rsv-prm)))
           (when (and (valid? t-br-blk-str-ctn) is-br (scn-blk-str-cnt p lx t-br-blk-str-ctn))
             (out #t))
           (when (or (and (valid? t-r-dqt-str-ctn) is-r
                          (scn-qt-str-cnt p lx nb-double-char? t-r-dqt-str-ctn))
                     (and (valid? t-br-dqt-str-ctn) (or is-br has-nwl)
                          (scn-qt-str-cnt p lx nb-double-char? t-br-dqt-str-ctn)))
             (out #t))
           (when (or (and (valid? t-r-sqt-str-ctn) is-r
                          (scn-qt-str-cnt p lx nb-single-char? t-r-sqt-str-ctn))
                     (and (valid? t-br-sqt-str-ctn) is-br
                          (scn-qt-str-cnt p lx nb-single-char? t-br-sqt-str-ctn)))
             (out #t))
           (when (and (valid? t-r-acr-ctn) is-r) (out (scn-anchor-ctn p lx t-r-acr-ctn)))
           (when (and (valid? t-r-als-ctn) is-r) (out (scn-anchor-ctn p lx t-r-als-ctn)))
           (let ((c (la lx)))
             (define (one-char sym) (adv p lx) (mrk-end p lx) (rt sym))
             (define (by-context r br b scan)
               ;; the first valid of the R, BR and B variants
               (cond ((and r (valid? r) is-r) (scan r))
                     ((and br (valid? br) is-br) (scan br))
                     ((and b (valid? b) is-b) (scan b))))
             (cond
              ((= c 37)   ; %
               (when (and (valid? t-s-dir-yml-bgn) is-s) (out (scn-dir-bgn p lx))))
              ((= c 42)   ; *
               (by-context t-r-als-bgn t-br-als-bgn t-b-als-bgn
                           (lambda (s) (may-upd-imp-col!) (out (scn-anchor-bgn p lx 42 s)))))
              ((= c 38)   ; &
               (by-context t-r-acr-bgn t-br-acr-bgn t-b-acr-bgn
                           (lambda (s) (may-upd-imp-col!) (out (scn-anchor-bgn p lx 38 s)))))
              ((= c 33)   ; !
               (by-context t-r-tag t-br-tag t-b-tag
                           (lambda (s) (may-upd-imp-col!) (out (scn-tag p lx s)))))
              ((= c 91)   ; [
               (by-context t-r-flw-seq-bgn t-br-flw-seq-bgn t-b-flw-seq-bgn
                           (lambda (s) (may-upd-imp-col!) (one-char s))))
              ((= c 93)   ; ]  (B gives the symbol of BR, as in C)
               (by-context t-r-flw-seq-end t-br-flw-seq-end t-b-flw-seq-end
                           (lambda (s) (one-char (if (= s t-b-flw-seq-end) t-br-flw-seq-end s)))))
              ((= c 123)   ; {
               (by-context t-r-flw-map-bgn t-br-flw-map-bgn t-b-flw-map-bgn
                           (lambda (s) (may-upd-imp-col!) (one-char s))))
              ((= c 125)   ; }
               (by-context t-r-flw-map-end t-br-flw-map-end t-b-flw-map-end
                           (lambda (s) (one-char (if (= s t-b-flw-map-end) t-br-flw-map-end s)))))
              ((= c 44)   ; ,
               (by-context t-r-flw-sep-bgn t-br-flw-sep-bgn #f (lambda (s) (one-char s))))
              ((= c 34)   ; "
               (by-context t-r-dqt-str-bgn t-br-dqt-str-bgn t-b-dqt-str-bgn
                           (lambda (s) (may-upd-imp-col!) (one-char s)))
               (by-context t-r-dqt-str-end t-br-dqt-str-end #f (lambda (s) (one-char s))))
              ((= c 39)   ; '
               (by-context t-r-sqt-str-bgn t-br-sqt-str-bgn t-b-sqt-str-bgn
                           (lambda (s) (may-upd-imp-col!) (one-char s)))
               (by-context t-r-sqt-str-end t-br-sqt-str-end #f
                           (lambda (s)
                             (adv p lx)
                             (if (= (la lx) 39)
                                 (begin (adv p lx) (mrk-end p lx)
                                        (rt (if (= s t-r-sqt-str-end) t-r-sqt-esc-sqt t-br-sqt-esc-sqt)))
                                 (begin (mrk-end p lx) (rt s))))))
              ((= c 63)   ; ?
               (let ((r-blk (and (valid? t-r-blk-key-bgn) is-r))
                     (br-blk (and (valid? t-br-blk-key-bgn) is-br))
                     (b-blk (and (valid? t-b-blk-key-bgn) is-b))
                     (r-flw (and (valid? t-r-flw-key-bgn) is-r))
                     (br-flw (and (valid? t-br-flw-key-bgn) is-br)))
                 (when (or r-blk br-blk b-blk r-flw br-flw)
                   (adv p lx)
                   (when (wht? (la lx))
                     (mrk-end p lx)
                     (when r-blk (push-bgn-ind! ind-map) (rt t-r-blk-key-bgn))
                     (when br-blk (push-bgn-ind! ind-map) (rt t-br-blk-key-bgn))
                     (when b-blk (rt t-b-blk-key-bgn))
                     (when r-flw (rt t-r-flw-key-bgn))
                     (when br-flw (rt t-br-flw-key-bgn))))))
              ((= c 58)   ; :
               (when (and (valid? t-r-flw-jsv-bgn) is-r) (one-char t-r-flw-jsv-bgn))
               (when (and (valid? t-br-flw-jsv-bgn) is-br) (one-char t-br-flw-jsv-bgn))
               (let ((r-val (and (valid? t-r-blk-val-bgn) is-r))
                     (br-val (and (valid? t-br-blk-val-bgn) is-br))
                     (b-val (and (valid? t-b-blk-val-bgn) is-b))
                     (r-imp (and (valid? t-r-blk-imp-bgn) is-r))
                     (r-njv (and (valid? t-r-flw-njv-bgn) is-r))
                     (br-njv (and (valid? t-br-flw-njv-bgn) is-br)))
                 (when (or r-val br-val b-val r-imp r-njv br-njv)
                   (adv p lx)
                   (let ((lka-wht (wht? (la lx))))
                     (when lka-wht
                       (when r-val (push-bgn-ind! ind-map) (mrk-end p lx) (rt t-r-blk-val-bgn))
                       (when br-val (push-bgn-ind! ind-map) (mrk-end p lx) (rt t-br-blk-val-bgn))
                       (when b-val (mrk-end p lx) (rt t-b-blk-val-bgn))
                       (when r-imp
                         ;; MAY_PUSH_IMP_IND
                         (unless (= cur-ind (vector-ref p 3))
                           (unless (= (vector-ref p 4) 0) (out #f))
                           (push-ind! p ind-map (vector-ref p 3)))
                         (mrk-end p lx)
                         (rt t-r-blk-imp-bgn)))
                     (when (or lka-wht (memv (la lx) '(44 93 125)))
                       (when r-njv (mrk-end p lx) (rt t-r-flw-njv-bgn))
                       (when br-njv (mrk-end p lx) (rt t-br-flw-njv-bgn)))))))
              ((= c 45)   ; -
               (let ((r-seq (and (valid? t-r-blk-seq-bgn) is-r))
                     (br-seq (and (valid? t-br-blk-seq-bgn) is-br))
                     (b-seq (and (valid? t-b-blk-seq-bgn) is-b)))
                 (when (or r-seq br-seq b-seq is-s)
                   (adv p lx)
                   (cond ((wht? (la lx))
                          (when r-seq (push-bgn-ind! ind-seq) (mrk-end p lx) (rt t-r-blk-seq-bgn))
                          (when br-seq (push-bgn-ind! ind-seq) (mrk-end p lx) (rt t-br-blk-seq-bgn))
                          (when b-seq
                            ;; MAY_PUSH_SPC_SEQ_IND
                            (when (= cur-ind-typ ind-map) (push-ind! p ind-seq bgn-col))
                            (mrk-end p lx)
                            (rt t-b-blk-seq-bgn)))
                         ((and (= (la lx) 45) is-s)
                          (adv p lx)
                          (when (= (la lx) 45)
                            (adv p lx)
                            (when (wht? (la lx))
                              (when (valid? t-bl) (pop-ind-or-fail!) (rt t-bl))
                              (mrk-end p lx)
                              (rt t-s-drs-end))))))))
              ((= c 46)   ; .
               (when is-s
                 (adv p lx)
                 (when (= (la lx) 46)
                   (adv p lx)
                   (when (= (la lx) 46)
                     (adv p lx)
                     (when (wht? (la lx))
                       (when (valid? t-bl) (pop-ind-or-fail!) (rt t-bl))
                       (mrk-end p lx)
                       (rt t-s-doc-end))))))
              ((= c 92)   ; \
               (let ((r-nwl (and (valid? t-r-dqt-esc-nwl) is-r))
                     (br-nwl (and (valid? t-br-dqt-esc-nwl) is-br))
                     (r-seq (and (valid? t-r-dqt-esc-seq) is-r))
                     (br-seq (and (valid? t-br-dqt-esc-seq) is-br)))
                 (when (or r-nwl br-nwl r-seq br-seq)
                   (adv p lx)
                   (when (nwl? (la lx))
                     (when r-nwl (mrk-end p lx) (rt t-r-dqt-esc-nwl))
                     (when br-nwl (mrk-end p lx) (rt t-br-dqt-esc-nwl)))
                   (when r-seq (out (scn-dqt-esc-seq p lx t-r-dqt-esc-seq)))
                   (when br-seq (out (scn-dqt-esc-seq p lx t-br-dqt-esc-seq)))
                   (out #f))))
              ((= c 124)   ; |
               (when (and (valid? t-r-blk-lit-bgn) is-r) (out (scn-blk-str-bgn p lx t-r-blk-lit-bgn)))
               (when (and (valid? t-br-blk-lit-bgn) is-br) (out (scn-blk-str-bgn p lx t-br-blk-lit-bgn))))
              ((= c 62)   ; >
               (when (and (valid? t-r-blk-fld-bgn) is-r) (out (scn-blk-str-bgn p lx t-r-blk-fld-bgn)))
               (when (and (valid? t-br-blk-fld-bgn) is-br) (out (scn-blk-str-bgn p lx t-br-blk-fld-bgn))))))
           ;; plain scalars
           (let* ((sgl-blk (or (and (valid? t-r-sgl-pln-str-blk) is-r)
                               (and (valid? t-br-sgl-pln-str-blk) is-br)
                               (and (valid? t-b-sgl-pln-str-blk) is-b)))
                  (sgl-flw (or (and (valid? t-r-sgl-pln-str-flw) is-r)
                               (and (valid? t-br-sgl-pln-str-flw) is-br)))
                  (mtl-blk (or (and (valid? t-r-mtl-pln-str-blk) is-r)
                               (and (valid? t-br-mtl-pln-str-blk) is-br)))
                  (mtl-flw (or (and (valid? t-r-mtl-pln-str-flw) is-r)
                               (and (valid? t-br-mtl-pln-str-flw) is-br))))
             (when (or sgl-blk sgl-flw mtl-blk mtl-flw)
               (let ((safe? (if (or sgl-blk mtl-blk) plain-safe-in-block? plain-safe-in-flow?)))
                 (when (= (- (cur-col p) bgn-col) 0) (adv p lx))
                 (if (= (- (cur-col p) bgn-col) 1)
                     (let ((plain-first (or (and (ns-char? bgn-chr) (not (c-indicator? bgn-chr)))
                                            (and (memv bgn-chr '(45 63 58)) (safe? (la lx))))))
                       (unless plain-first (out #f))
                       (vector-set! p 12 (adv-sch-stt p (vector-ref p 12) (cur-chr p))))
                     ;; ..X ...X --X ---X: a string
                     (vector-set! p 12 sch-stt-frz))
                 (mrk-end p lx)
                 (let loop ()
                   (when (or (nwl? (la lx)) (eq? (scn-pln-cnt p lx safe?) 'succ))
                     (when (and (not (= (la lx) 0)) (nwl? (la lx)))
                       (let blank ()
                         (cond ((nwl? (la lx)) (adv-nwl p lx) (blank))
                               ((wsp? (la lx)) (adv p lx) (blank))))
                       (unless (or (= (la lx) 0) (<= (cur-col p) cur-ind)
                                   (and (= (cur-col p) 0) (scn-drs-doc-end p lx)))
                         (loop)))))
                 (if (= (vector-ref p 7) bgn-row)
                     (begin
                       (when sgl-blk
                         (may-upd-imp-col!)
                         (rt (sgl-pln-sym p (cond (is-r 'r) (is-br 'br) (else 'b)) 'blk)))
                       (when sgl-flw (rt (sgl-pln-sym p (if is-r 'r 'br) 'flw))))
                     (begin
                       (when mtl-blk
                         (may-upd-imp-col!)
                         (rt (if is-r t-r-mtl-pln-str-blk t-br-mtl-pln-str-blk)))
                       (when mtl-flw (rt (if is-r t-r-mtl-pln-str-flw t-br-mtl-pln-str-flw)))))
                 (out #f))))
           (not (valid? t-err-rec))))))))

(define-public (ts-yaml-scanner)
  (vector yaml-create yaml-scan yaml-deserialize yaml-serialize))
