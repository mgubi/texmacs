
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmconcat.scm
;; DESCRIPTION : manipulation of concatenations
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools tmconcat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor for concatenations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmconcat* . l)
  (:synopsis "Non-correcting constructor of horizontal concatenations.")
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'concat l))))

(tm-define (tmconcat-simplify l)
  (cond ((null? l) l)
        ((tm-atomic? (car l))
         (let* ((head (tm->string (car l)))
                (tail (tmconcat-simplify (cdr l))))
           (cond ((== head "") tail)
                 ((and (nnull? tail) (string? (car tail)))
                  (cons (string-append head (car tail)) (cdr tail)))
                 (else (cons head tail)))))
	((tm-func? (car l) 'concat)
         (tmconcat-simplify (append (tm-cdr (car l)) (cdr l))))
        (else (cons (car l) (tmconcat-simplify (cdr l))))))

;; (tm-define (tmconcat . in)
;;   (:synopsis "Constructor of horizontal concatenations with corrections.")
;;   (let* ((l (tmconcat-simplify in))
;; 	 (o (length (list-filter l (lambda (x) (tm-func? x 'left)))))
;; 	 (c (length (list-filter l (lambda (x) (tm-func? x 'right))))))
;;     (if (> o c) (set! l (append l (make-list (- o c) '(right ".")))))
;;     (if (< o c) (set! l (append l '(right ".") (make-list (- o c)))))
;;     (apply tmconcat* l)))

(tm-define (tmconcat . in)
  (:synopsis "Constructor of horizontal concatenations with corrections.")
  (apply tmconcat* (tmconcat-simplify in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replacing mathematical string by list of tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmconcat-eat s pos plus pred?)
  (let eat ((end pos))
    (cond ((>= end (string-length s))
	   (cons (substring s pos end)
		 (tmconcat-math-sub s end)))
	  ((pred? (string-ref s end))
	   (cons (substring s pos (+ end plus))
		 (tmconcat-math-sub s (+ end plus))))
	  (else (eat (+ end 1))))))

(define (tmconcat-number s pos)
  ;; A number, with at most one decimal point between digits
  (define (digit? i) (and (< i (string-length s)) (char-numeric? (string-ref s i))))
  (let eat ((end pos) (point? #f))
    (cond ((digit? end) (eat (+ end 1) point?))
	  ((and (not point?) (< end (string-length s))
		(== (string-ref s end) #\.) (digit? (+ end 1)))
	   (eat (+ end 1) #t))
	  (else (cons (substring s pos end) (tmconcat-math-sub s end))))))

(define (tmconcat-math-sub s pos)
  (if (>= pos (string-length s)) '()
      (with c (string-ref s pos)
	(cond ((== c #\<)
	       (tmconcat-eat s pos 1 (lambda (c) (== c #\>))))
	      ((char-numeric? c)
	       (tmconcat-number s pos))
	      ((and (char-alphabetic? c)
		    (< (+ pos 1) (string-length s))
		    (char-alphabetic? (string-ref s (+ pos 1))))
	       (tmconcat-eat s pos 0 (lambda (c) (not (char-alphabetic? c)))))
	      (else (cons (substring s pos (+ pos 1))
			  (tmconcat-math-sub s (+ pos 1))))))))

(tm-define (tmconcat-tokenize-math s)
  (:type (-> string (list string)))
  (:synopsis "Decompose mathematical string @s into list of tokens")
  (tmconcat-math-sub s 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplification of weak tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tabs of a line are typeset as follows (make_unit, lazy_paragraph.cpp;
;; tab.cpp): each is a space of its length, and the rest of the line goes
;; to the tabs with a weight (an ordinary tab has the weight 1, <htab|len|w>
;; the weight w), in proportion to it. When no tab has a weight, it goes in
;; equal parts to the first "first" tab, the last "last" tab (the weak tabs)
;; and the tabs of weight 0; the others get nothing. In HTML, the tabs which
;; get a part of the line cut it (tmconcat-structure-tabs), as ordinary
;; tabs; the others are removed. What stays different: a tab which gets
;; nothing loses its length too; the parts of the line are equal whatever
;; the weights; the typesetter applies this to the last line of a
;; paragraph only (the other lines are justified), and the HTML to the
;; whole paragraph; and a tab in a <with> cuts that <with> only

(define (tab? tab) (func? tab 'htab))
(define (tab-left? tab) (and (func? tab 'htab 2) (== (caddr tab) "last")))
(define (tab-right? tab) (and (func? tab 'htab 2) (== (caddr tab) "first")))
(define (tab-weak? tab) (or (tab-left? tab) (tab-right? tab)))

(define (tab-weight tab)
  ;; the weight of a tab (0 for a weak tab)
  (cond ((tab-weak? tab) 0)
        ((or (not (func? tab 'htab 2)) (not (string? (caddr tab)))) 1)
        (else (or (string->number (caddr tab)) 0))))

;; The tabs of a concatenation include those of its <with> and <concat>
;; descendants, down to tabs-depth levels: a deeper tab is simplified with
;; the tabs of its own concatenation, when the converter gets there. A
;; converter simplifies the tabs of each concatenation of a nesting (as
;; tmhtml-concat): without the bound, each level looked at all the levels
;; below it, and the HTML export of a nesting 1000 deep took minutes. The
;; depth is that of @x below the concatenation which is simplified
(define tabs-depth 64)

(define (line-tabs x depth acc)
  ;; the tabs of @x and of its <with> and <concat> descendants down to
  ;; @depth levels, the last one first, on @acc
  (cond ((tab? x) (cons x acc))
        ((<= depth 0) acc)
        ((func? x 'with) (line-tabs (cAr x) (- depth 1) acc))
        ((func? x 'concat)
         (let loop ((l (cdr x)) (acc acc))
           (if (null? l) acc
               (loop (cdr l) (line-tabs (car l) (- depth 1) acc)))))
        (else acc)))

(define (tabs-replacements tabs)
  ;; what each of the tabs @tabs of a line becomes: itself or an ordinary
  ;; tab when it gets a part of the line, else ""
  (let* ((weighted? (not (= (apply + (map tab-weight tabs)) 0)))
         (lasts (length (list-filter tabs tab-left?))))
    (let loop ((l tabs) (nf 0) (nl 0) (r '()))
      (if (null? l) (reverse r)
          (let* ((t (car l))
                 (nf2 (if (tab-right? t) (+ nf 1) nf))
                 (nl2 (if (tab-left? t) (+ nl 1) nl))
                 (part? (cond (weighted? (not (= (tab-weight t) 0)))
                              ((tab-right? t) (= nf2 1))
                              ((tab-left? t) (= nl2 lasts))
                              (else #t))))
            (loop (cdr l) nf2 nl2
                  (cons (cond ((not part?) "")
                              ((tab-weak? t) (list 'htab (cadr t)))
                              (else t))
                        r)))))))

(define (replace-tabs x depth next)
  ;; @x with each of its tabs (as line-tabs) replaced by (next)
  (cond ((tab? x) (next))
        ((<= depth 0) x)
        ((func? x 'with)
         (rcons (cDr x) (replace-tabs (cAr x) (- depth 1) next)))
        ((func? x 'concat)
         (cons 'concat
               (let loop ((l (cdr x)) (r '()))
                 (if (null? l) (reverse r)
                     (loop (cdr l)
                           (cons (replace-tabs (car l) (- depth 1) next)
                                 r))))))
        (else x)))

(tm-define (tmconcat-simplify-tabs l)
  (:type (forall T (-> (list T) (list T))))
  (:synopsis "Rewrite weak left and right tabs in concatenation @l")
  (let* ((c (cons 'concat l))
         (tabs (reverse (line-tabs c tabs-depth '())))
         (new (tabs-replacements tabs)))
    (if (list-and (map eq? tabs new)) l
        (cdr (replace-tabs c tabs-depth
                           (lambda ()
                             (with t (car new)
                               (set! new (cdr new))
                               t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replacing tab information by alignment information
;; The alignment is expressed using the !left, !middle and !right tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmconcat-tabs-make head tail where next)
  (with (first . others) (tmconcat-tabs-sub (cdar tail) (cdr tail) next)
    (if (== where next)
	`((,where ,@head ,@(cdr first)) ,@others)
	`((,where ,@head) ,first ,@others))))

(define (tmconcat-tabs-sub head tail where)
  (cond ((null? tail) `((,where ,@head)))
	((and (== where '!left) (> (length tail) 1))
	 (tmconcat-tabs-make head tail where '!middle))
	(else (tmconcat-tabs-make head tail where '!right))))

(tm-define (tmconcat-structure-tabs l)
  (:type (forall T (-> (list T) (list T))))
  (:synopsis "Structure tabs in concatenation @l")
  ;;(display* "**** l << " l "\n")
  (set! l (tmconcat-simplify-tabs l))
  ;;(display* "**** l << " l "\n")
  (with r (list-scatter l tab? #t)
    (if (null? (cdr r)) l
        (tmconcat-tabs-sub (car r) (cdr r) '!left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grouping brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmconcat-opening? x)
  (or (func? x 'left)
      (and (string? x) (== (math-symbol-type x) "opening-bracket"))))

(define (tmconcat-closing? x)
  (or (func? x 'right)
      (and (string? x) (== (math-symbol-type x) "closing-bracket"))))

(define (tmconcat-brackets-sub l)
  ;; used for instance in MathML generation
  (cond ((null? l) (values l l))
	((tmconcat-opening? (car l))
	 (receive (r tail) (tmconcat-brackets-sub (cdr l))
	   (receive (r2 tail2) (tmconcat-brackets-sub tail)
	     (values (cons (cons* 'concat! (car l) r) r2) tail2))))
	((tmconcat-closing? (car l))
	 (values (list (car l)) (cdr l)))
	(else
	 (receive (r tail) (tmconcat-brackets-sub (cdr l))
	   (values (cons (car l) r) tail)))))

(tm-define (tmconcat-structure-brackets l)
  (:type (forall T (-> (list T) (list T))))
  (:synopsis "Recursively group matching brackets in concatenation @l")
  (receive (r tail) (tmconcat-brackets-sub l)
    (if (null? tail) r
	(append r (tmconcat-structure-brackets tail)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grouping scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmconcat-structure-scripts-sub l)
  (cond ((null? l) l)
	((func? (car l) 'rsub)
	 (cons (list 'rsub! "" (cadar l))
	       (tmconcat-structure-scripts-sub (cdr l))))
	((func? (car l) 'rsup)
	 (cons (list 'rsup! "" (cadar l))
	       (tmconcat-structure-scripts-sub (cdr l))))
	((match? l '(:%1 (rsub :%1) (rsup :%1) :*))
	 (cons (list 'rsubsup! (car l) (cadadr l) (cadr (caddr l)))
	       (tmconcat-structure-scripts-sub (cdddr l))))
	((match? l '(:%1 (rsup :%1) (rsub :%1) :*))
	 (cons (list 'rsubsup! (car l) (cadr (caddr l)) (cadadr l))
	       (tmconcat-structure-scripts-sub (cdddr l))))
	((match? l '(:%1 (rsub :%1) :*))
	 (cons (list 'rsub! (car l) (cadadr l))
	       (tmconcat-structure-scripts-sub (cddr l))))
	((match? l '(:%1 (rsup :%1) :*))
	 (cons (list 'rsup! (car l) (cadadr l))
	       (tmconcat-structure-scripts-sub (cddr l))))
	((or (func? (car l) 'lsub) (func? (car l) 'lsup))
	 (let* ((r (tmconcat-structure-scripts-sub (cdr l)))
		(s (if (== (caar l) 'lsub) 'lsub! 'lsup!)))
	   (cond ((null? r) (list (list s "" (cadar l))))
		 ((and (== s 'lsub!) (func? (car r) 'lsup!))
		  (cons (list 'lsubsup! (cadar r) (cadar l) (caddar r))
			(cdr r)))
		 ((and (== s 'lsup!) (func? (car r) 'lsub!))
		  (cons (list 'lsubsup! (cadar r) (caddar r) (cadar l))
			(cdr r)))
		 (else (cons (list s (car r) (cadar l)) (cdr r))))))
	(else (cons (car l) (tmconcat-structure-scripts-sub (cdr l))))))

(define (tmconcat-simplify-scripts l)
  (cond ((null? l) l)
	((func? (car l) 'lprime)
	 (tmconcat-simplify-scripts (cons (cons 'lsup (cdar l)) (cdr l))))
	((func? (car l) 'rprime)
	 (tmconcat-simplify-scripts (cons (cons 'rsup (cdar l)) (cdr l))))
	((null? (cdr l)) l)
	((and (pair? (car l)) (pair? (cadr l))
	      (in? (caar l) '(lsub lsup rsub rsup))
	      (== (caar l) (caadr l)))
	 (tmconcat-simplify-scripts
	  `((,(caar l) (concat ,(cadar l) ,(cadadr l))) ,@(cddr l))))
	(else (cons (car l) (tmconcat-simplify-scripts (cdr l))))))

(tm-define (tmconcat-structure-scripts l)
  ;; used for instance in MathML generation
  (:type (forall T (-> (list T) (list T))))
  (:synopsis "Group scripts in @l")
  (tmconcat-structure-scripts-sub (tmconcat-simplify-scripts l)))
