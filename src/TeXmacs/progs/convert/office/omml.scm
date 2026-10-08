
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : omml.scm
;; DESCRIPTION : the formulas of Word (OMML) as MathML
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A formula of Word is a tree of its own format, Office Math Markup
;; (m:oMath), whose constructions are those of MathML with other names: a
;; fraction is (m:f (m:num ...) (m:den ...)) where MathML has (mfrac ...
;; ...). The text of a formula is in runs (m:r (m:t "text")) which are not
;; cut into identifiers, numbers and operators as in MathML: this is done
;; here. The result is a tree of MathML as parse-xml would give it, for the
;; converter of MathML (convert/mathml/mathtm.scm).

(texmacs-module (convert office omml)
  (:use (convert office office-tools)))

(define (omml-val x)
  (and x (ox-attr x 'm:val)))

(define (omml-property x pr name)
  ;; the value of the property name of the construction x, in its child pr
  (with p (ox-child x pr)
    (and p (omml-val (ox-child p name)))))

(define (omml-on? x pr name)
  ;; a property which is set: <m:degHide m:val="1"/>, or without a value
  (let* ((p (ox-child x pr))
         (y (and p (ox-child p name))))
    (and y (not (in? (omml-val y) '("0" "false" "off"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The text of a run: identifiers, numbers and operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (omml-characters s)
  ;; the characters of a string in UTF-8, each as a string
  (let loop ((i 0) (acc '()))
    (if (>= i (string-length s)) (reverse acc)
        (let* ((c (char->integer (string-ref s i)))
               (n (cond ((< c #x80) 1) ((< c #xe0) 2) ((< c #xf0) 3) (else 4)))
               (j (min (string-length s) (+ i n))))
          (loop j (cons (substring s i j) acc))))))

(define (omml-code c)
  ;; the code of a character in UTF-8
  (let* ((l (map char->integer (string->list c)))
         (b (car l)))
    (cond ((< b #x80) b)
          ((and (< b #xe0) (>= (length l) 2))
           (+ (* (- b #xc0) 64) (- (cadr l) #x80)))
          ((and (< b #xf0) (>= (length l) 3))
           (+ (* (- b #xe0) 4096) (* (- (cadr l) #x80) 64) (- (caddr l) #x80)))
          ((>= (length l) 4)
           (+ (* (- b #xf0) 262144) (* (- (cadr l) #x80) 4096)
              (* (- (caddr l) #x80) 64) (- (cadddr l) #x80)))
          (else 0))))

(define (omml-digit? c)
  (and (== (string-length c) 1) (char-numeric? (string-ref c 0))))

(define (omml-letter? c)
  ;; a letter: of the Latin and Greek alphabets, a letter-like symbol, or
  ;; one of the mathematical alphabets
  (with n (omml-code c)
    (or (and (>= n 65) (<= n 90)) (and (>= n 97) (<= n 122))
        (and (>= n #xc0) (<= n #x24f) (!= n #xd7) (!= n #xf7))
        (and (>= n #x370) (<= n #x3ff))
        (and (>= n #x400) (<= n #x4ff))
        (and (>= n #x2100) (<= n #x214f))
        (and (>= n #x1d400) (<= n #x1d7ff)))))

(define (omml-tokens s upright?)
  ;; the tokens of MathML for the text of a run. In a run which is not
  ;; upright each letter is an identifier; an upright word is one (sin).
  (let loop ((l (omml-characters s)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((in? (car l) '(" " "\t" "\n")) (loop (cdr l) acc))
          ((omml-digit? (car l))
           ;; a number, with its decimal point
           (let sub ((r (cdr l)) (n (car l)))
             (cond ((and (pair? r) (omml-digit? (car r)))
                    (sub (cdr r) (string-append n (car r))))
                   ((and (pair? r) (in? (car r) '("." ",")) (pair? (cdr r))
                         (omml-digit? (cadr r)))
                    (sub (cddr r) (string-append n (car r) (cadr r))))
                   (else (loop r (cons `(m:mn ,n) acc))))))
          ((and upright? (omml-letter? (car l)))
           (let sub ((r (cdr l)) (w (car l)))
             (if (and (pair? r) (omml-letter? (car r)))
                 (sub (cdr r) (string-append w (car r)))
                 (loop r (cons (if (== (length (omml-characters w)) 1)
                                   `(m:mi (@ (mathvariant "normal")) ,w)
                                   `(m:mi ,w))
                               acc)))))
          ((omml-letter? (car l)) (loop (cdr l) (cons `(m:mi ,(car l)) acc)))
          (else (loop (cdr l) (cons `(m:mo ,(car l)) acc))))))

(define (omml-run x)
  ;; a run: its text as tokens, or as text when it is marked as such
  (let* ((pr (ox-child x 'm:rPr))
         (text (apply string-append (map ox-text (ox-childs x 'm:t))))
         (normal? (and pr (ox-child pr 'm:nor)
                       (not (in? (omml-val (ox-child pr 'm:nor))
                                 '("0" "false" "off")))))
         (sty (and pr (omml-val (ox-child pr 'm:sty)))))
    (cond ((== text "") '())
          (normal? (list `(m:mtext ,text)))
          ((in? sty '("b" "bi"))
           (map (lambda (t)
                  (if (func? t 'm:mi)
                      `(m:mi (@ (mathvariant ,(if (== sty "b") "bold" "bold-italic")))
                             ,(cAr t))
                      t))
                (omml-tokens text #f)))
          (else (omml-tokens text (== sty "p"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The constructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (omml-row l)
  ;; a list of nodes of MathML as one node
  (if (list-1? l) (car l) (cons 'm:mrow l)))

(define (omml-arg x tag)
  ;; the argument tag of the construction x, as one node
  (with y (ox-child x tag)
    (omml-row (if y (omml-list (ox-children y)) '()))))

(define (omml-empty? x tag)
  (with y (ox-child x tag)
    (or (not y) (null? (omml-list (ox-children y))))))

;; the accents, which Word writes as combining characters
(define (omml-utf8 n)
  ;; the character of code n, in UTF-8
  (list->string
    (map integer->char
         (cond ((< n #x80) (list n))
               ((< n #x800) (list (+ #xc0 (quotient n 64)) (+ #x80 (modulo n 64))))
               ((< n #x10000)
                (list (+ #xe0 (quotient n 4096))
                      (+ #x80 (modulo (quotient n 64) 64))
                      (+ #x80 (modulo n 64))))
               (else
                 (list (+ #xf0 (quotient n 262144))
                       (+ #x80 (modulo (quotient n 4096) 64))
                       (+ #x80 (modulo (quotient n 64) 64))
                       (+ #x80 (modulo n 64))))))))

;; the code of the combining character, and the one of the accent by itself
(define omml-accents
  '((#x302 . #x5e) (#x303 . #x7e) (#x304 . #xaf) (#x305 . #xaf)
    (#x307 . #x2d9) (#x308 . #xa8) (#x30c . #x2c7) (#x306 . #x2d8)
    (#x301 . #xb4) (#x300 . #x60) (#x20d7 . #x2192) (#x20d6 . #x2190)
    (#x20e1 . #x2194)))

(define (omml-accent c)
  (with n (assoc-ref omml-accents (omml-code c))
    (if n (omml-utf8 n) c)))

(define (omml-nary x)
  ;; a big operator with its limits, and what it applies to
  (let* ((chr (or (omml-property x 'm:naryPr 'm:chr) (omml-utf8 #x222b)))
         (loc (omml-property x 'm:naryPr 'm:limLoc))
         (sub? (not (or (omml-on? x 'm:naryPr 'm:subHide) (omml-empty? x 'm:sub))))
         (sup? (not (or (omml-on? x 'm:naryPr 'm:supHide) (omml-empty? x 'm:sup))))
         (under? (== loc "undOvr"))
         (op `(m:mo ,chr))
         (head (cond ((and sub? sup?)
                      `(,(if under? 'm:munderover 'm:msubsup)
                        ,op ,(omml-arg x 'm:sub) ,(omml-arg x 'm:sup)))
                     (sub? `(,(if under? 'm:munder 'm:msub) ,op ,(omml-arg x 'm:sub)))
                     (sup? `(,(if under? 'm:mover 'm:msup) ,op ,(omml-arg x 'm:sup)))
                     (else op))))
    (list `(m:mrow ,head ,(omml-arg x 'm:e)))))

(define (omml-delimiters x)
  ;; brackets around arguments which a separator sets apart
  (let* ((pr (ox-child x 'm:dPr))
         (get (lambda (name default)
                (with y (and pr (ox-child pr name))
                  (if y (or (omml-val y) "") default))))
         (open (get 'm:begChr "("))
         (close (get 'm:endChr ")"))
         (sep (get 'm:sepChr "|"))
         (args (map (lambda (e) (omml-row (omml-list (ox-children e))))
                    (ox-childs x 'm:e))))
    (list `(m:mrow ,@(if (== open "") '() `((m:mo (@ (fence "true")) ,open)))
                   ,@(list-intersperse args `(m:mo ,sep))
                   ,@(if (== close "") '() `((m:mo (@ (fence "true")) ,close)))))))

(define (omml-matrix x)
  (list `(m:mtable
           ,@(map (lambda (r)
                    `(m:mtr ,@(map (lambda (e)
                                     `(m:mtd ,(omml-row (omml-list (ox-children e)))))
                                   (ox-childs r 'm:e))))
                  (ox-childs x 'm:mr)))))

(define (omml-array x)
  ;; equations one above the other: the & of Word marks where they align
  (list `(m:mtable
           ,@(map (lambda (e)
                    `(m:mtr (m:mtd ,(omml-row
                                      (list-filter
                                        (omml-list (ox-children e))
                                        (lambda (t) (!= t '(m:mo "&"))))))))
                  (ox-childs x 'm:e)))))

(define (omml-node x)
  ;; the nodes of MathML for a node of OMML
  (cond ((not (pair? x)) '())
        (else
          (case (car x)
            ((m:r) (omml-run x))
            ((m:f)
             (with type (omml-property x 'm:fPr 'm:type)
               (cond ((== type "lin")
                      (list `(m:mrow ,(omml-arg x 'm:num) (m:mo "/")
                                     ,(omml-arg x 'm:den))))
                     ((== type "noBar")
                      (list `(m:mfrac (@ (linethickness "0"))
                                      ,(omml-arg x 'm:num) ,(omml-arg x 'm:den))))
                     (else (list `(m:mfrac ,(omml-arg x 'm:num)
                                           ,(omml-arg x 'm:den)))))))
            ((m:sSup) (list `(m:msup ,(omml-arg x 'm:e) ,(omml-arg x 'm:sup))))
            ((m:sSub) (list `(m:msub ,(omml-arg x 'm:e) ,(omml-arg x 'm:sub))))
            ((m:sSubSup)
             (list `(m:msubsup ,(omml-arg x 'm:e) ,(omml-arg x 'm:sub)
                               ,(omml-arg x 'm:sup))))
            ((m:sPre)
             (list `(m:mmultiscripts ,(omml-arg x 'm:e) (m:mprescripts)
                                     ,(omml-arg x 'm:sub) ,(omml-arg x 'm:sup))))
            ((m:rad)
             (if (or (omml-on? x 'm:radPr 'm:degHide) (omml-empty? x 'm:deg))
                 (list `(m:msqrt ,(omml-arg x 'm:e)))
                 (list `(m:mroot ,(omml-arg x 'm:e) ,(omml-arg x 'm:deg)))))
            ((m:nary) (omml-nary x))
            ((m:d) (omml-delimiters x))
            ((m:func)
             ;; a function and its argument
             (list `(m:mrow ,(omml-arg x 'm:fName) (m:mo ,(omml-utf8 #x2061))
                            ,(omml-arg x 'm:e))))
            ((m:acc)
             (list `(m:mover (@ (accent "true")) ,(omml-arg x 'm:e)
                             (m:mo ,(omml-accent
                                      (or (omml-property x 'm:accPr 'm:chr)
                                          (omml-utf8 #x302)))))))
            ((m:bar)
             (if (== (omml-property x 'm:barPr 'm:pos) "bot")
                 (list `(m:munder ,(omml-arg x 'm:e) (m:mo "_")))
                 (list `(m:mover (@ (accent "true")) ,(omml-arg x 'm:e) (m:mo ,(omml-utf8 #xaf))))))
            ((m:limLow) (list `(m:munder ,(omml-arg x 'm:e) ,(omml-arg x 'm:lim))))
            ((m:limUpp) (list `(m:mover ,(omml-arg x 'm:e) ,(omml-arg x 'm:lim))))
            ((m:groupChr)
             (let ((chr (or (omml-property x 'm:groupChrPr 'm:chr)
                            (omml-utf8 #x23df)))
                   (pos (omml-property x 'm:groupChrPr 'm:pos)))
               (list `(,(if (== pos "top") 'm:mover 'm:munder)
                       ,(omml-arg x 'm:e) (m:mo ,chr)))))
            ((m:m) (omml-matrix x))
            ((m:eqArr) (omml-array x))
            ;; what only holds a formula
            ((m:box m:borderBox m:phant)
             (with y (ox-child x 'm:e)
               (if y (omml-list (ox-children y)) '())))
            ((m:oMath m:e m:num m:den m:sub m:sup m:deg m:lim m:fName)
             (omml-list (ox-children x)))
            ;; a run of the text of Word inside a formula
            ((w:r)
             (with s (ox-text x)
               (if (== s "") '() (list `(m:mtext ,s)))))
            (else '())))))

(define (omml-list l)
  (append-map omml-node l))

(tm-define (omml->mathml x)
  (:synopsis "The formula of Word @x, an element m:oMath, as a tree of MathML")
  `(m:math ,(omml-row (omml-list (ox-children x)))))
