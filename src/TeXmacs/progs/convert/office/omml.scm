
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MathML as a formula of Word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The other way: the elements of MathML have the prefix m: (as the
;; converter of MathML writes them), or another one, or none.

(define (mmlo-name x)
  ;; the name of an element of MathML without its prefix, as a symbol
  (let* ((s (symbol->string (car x)))
         (i (string-search-forwards ":" 0 s)))
    (string->symbol (if (>= i 0) (substring s (+ i 1) (string-length s)) s))))

(define (mmlo-is? x name)
  (and (pair? x) (symbol? (car x)) (== (mmlo-name x) name)))

(define (mmlo-text x)
  (apply string-append (list-filter (ox-children x) string?)))

(define (mmlo-run text . props)
  (if (== text "") '()
      (list `(m:r ,@(if (null? props) '() `((m:rPr ,@props)))
                  (m:t ,text)))))

(define (mmlo-arg tag x)
  ;; the argument tag of a construction, with the formula x
  (cons tag (mmlo-node x)))

;; the big operators, by their codes
(define mmlo-big-operators
  '(#x2211 #x220f #x2210 #x222b #x222c #x222d #x222e #x22c0 #x22c1 #x22c2
    #x22c3 #x2a00 #x2a01 #x2a02 #x2a04 #x2a05 #x2a06))

(define (mmlo-big-operator x)
  ;; the character of the element x when it is a big operator, or #f
  (and (mmlo-is? x 'mo)
       (with s (mmlo-text x)
         (and (!= s "") (in? (omml-code s) mmlo-big-operators) s))))

(define (mmlo-nary-head x)
  ;; (character under-and-over? sub sup) when x is a big operator, with
  ;; its limits or without, or #f
  (let ((c (ox-elements x)))
    (cond ((mmlo-big-operator x) (list (mmlo-big-operator x) #f #f #f))
          ((null? c) #f)
          ((not (mmlo-big-operator (car c))) #f)
          ((and (mmlo-is? x 'munderover) (== (length c) 3))
           (list (mmlo-big-operator (car c)) #t (cadr c) (caddr c)))
          ((and (mmlo-is? x 'msubsup) (== (length c) 3))
           (list (mmlo-big-operator (car c)) #f (cadr c) (caddr c)))
          ((and (mmlo-is? x 'munder) (== (length c) 2))
           (list (mmlo-big-operator (car c)) #t (cadr c) #f))
          ((and (mmlo-is? x 'mover) (== (length c) 2))
           (list (mmlo-big-operator (car c)) #t #f (cadr c)))
          ((and (mmlo-is? x 'msub) (== (length c) 2))
           (list (mmlo-big-operator (car c)) #f (cadr c) #f))
          ((and (mmlo-is? x 'msup) (== (length c) 2))
           (list (mmlo-big-operator (car c)) #f #f (cadr c)))
          (else #f))))

(define (mmlo-ends-operand? x)
  ;; a relation or a separator: where what a big operator applies to ends
  (and (mmlo-is? x 'mo)
       (in? (mmlo-text x)
            (list "=" "<" ">" "," ";" (omml-utf8 #x2264) (omml-utf8 #x2265)
                  (omml-utf8 #x2260) (omml-utf8 #x2248) (omml-utf8 #x2261)
                  (omml-utf8 #x2192) (omml-utf8 #x21d2)))))

(define (mmlo-fence? x form)
  ;; a bracket which opens (form "prefix") or closes (form "postfix")
  (and (mmlo-is? x 'mo)
       (or (== (ox-attr x 'form) form)
           (and (== (ox-attr x 'fence) "true")
                (in? (mmlo-text x)
                     (if (== form "prefix") '("(" "[" "{") '(")" "]" "}")))))))

(define (mmlo-row l)
  ;; the nodes of OMML for the children l of a row. A big operator takes
  ;; what follows it, up to a relation; brackets take what they enclose.
  (let loop ((l (list-filter l pair?)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((mmlo-nary-head (car l))
           => (lambda (h)
                (let sub ((r (cdr l)) (operand '()))
                  (if (or (null? r) (mmlo-ends-operand? (car r)))
                      (loop r
                            (cons `(m:nary
                                     (m:naryPr (m:chr (@ (m:val ,(car h))))
                                               (m:limLoc (@ (m:val ,(if (cadr h) "undOvr" "subSup"))))
                                               ,@(if (caddr h) '() '((m:subHide (@ (m:val "1")))))
                                               ,@(if (cadddr h) '() '((m:supHide (@ (m:val "1"))))))
                                     (m:sub ,@(if (caddr h) (mmlo-node (caddr h)) '()))
                                     (m:sup ,@(if (cadddr h) (mmlo-node (cadddr h)) '()))
                                     (m:e ,@(mmlo-row (reverse operand))))
                                  acc))
                      (sub (cdr r) (cons (car r) operand))))))
          ((mmlo-fence? (car l) "prefix")
           ;; up to the bracket which closes this one
           (let sub ((r (cdr l)) (depth 0) (inner '()))
             (cond ((null? r)
                    ;; none: the bracket is a character
                    (loop (cdr l) (append (reverse (mmlo-run (mmlo-text (car l)))) acc)))
                   ((and (mmlo-fence? (car r) "postfix") (== depth 0))
                    (loop (cdr r)
                          (cons `(m:d (m:dPr (m:begChr (@ (m:val ,(mmlo-text (car l)))))
                                             (m:endChr (@ (m:val ,(mmlo-text (car r))))))
                                      (m:e ,@(mmlo-row (reverse inner))))
                                acc)))
                   (else
                     (sub (cdr r)
                          (cond ((mmlo-fence? (car r) "prefix") (+ depth 1))
                                ((mmlo-fence? (car r) "postfix") (- depth 1))
                                (else depth))
                          (cons (car r) inner))))))
          (else (loop (cdr l) (append (reverse (mmlo-node (car l))) acc))))))

;; the accents by themselves, and their combining characters
(define mmlo-accents
  '((#x5e . #x302) (#x7e . #x303) (#xaf . #x304) (#x2d9 . #x307)
    (#xa8 . #x308) (#x2c7 . #x30c) (#x2d8 . #x306) (#xb4 . #x301)
    (#x60 . #x300) (#x2192 . #x20d7) (#x2190 . #x20d6) (#x203e . #x304)
    (#x2c6 . #x302) (#x2dc . #x303)))

(define (mmlo-accent x)
  ;; the combining character of the element x when it is an accent, or #f
  (and (mmlo-is? x 'mo)
       (with s (mmlo-text x)
         (and (!= s "")
              (with n (assoc-ref mmlo-accents (omml-code s))
                (and n (omml-utf8 n)))))))

(define (mmlo-node x)
  ;; the nodes of OMML for a node of MathML
  (cond ((not (pair? x)) '())
        ((not (symbol? (car x))) '())
        (else
          (let ((c (ox-elements x)))
            (case (mmlo-name x)
              ((math mrow mstyle semantics mpadded mphantom merror)
               (mmlo-row (ox-children x)))
              ((mi)
               (with s (mmlo-text x)
                 (if (or (> (length (omml-characters s)) 1)
                         (== (ox-attr x 'mathvariant) "normal"))
                     (mmlo-run s '(m:sty (@ (m:val "p"))))
                     (cond ((== (ox-attr x 'mathvariant) "bold")
                            (mmlo-run s '(m:sty (@ (m:val "b")))))
                           ((== (ox-attr x 'mathvariant) "bold-italic")
                            (mmlo-run s '(m:sty (@ (m:val "bi")))))
                           (else (mmlo-run s))))))
              ((mn mo) (mmlo-run (mmlo-text x)))
              ((mtext ms) (mmlo-run (mmlo-text x) '(m:nor)))
              ((mspace) '())
              ((mfrac)
               (if (!= (length c) 2) (mmlo-row c)
                   (list `(m:f ,@(if (in? (ox-attr x 'linethickness) '("0" "0pt" "0px"))
                                     '((m:fPr (m:type (@ (m:val "noBar")))))
                                     '())
                               ,(mmlo-arg 'm:num (car c))
                               ,(mmlo-arg 'm:den (cadr c))))))
              ((msup)
               (if (!= (length c) 2) (mmlo-row c)
                   (list `(m:sSup ,(mmlo-arg 'm:e (car c)) ,(mmlo-arg 'm:sup (cadr c))))))
              ((msub)
               (if (!= (length c) 2) (mmlo-row c)
                   (list `(m:sSub ,(mmlo-arg 'm:e (car c)) ,(mmlo-arg 'm:sub (cadr c))))))
              ((msubsup)
               (if (!= (length c) 3) (mmlo-row c)
                   (list `(m:sSubSup ,(mmlo-arg 'm:e (car c)) ,(mmlo-arg 'm:sub (cadr c))
                                     ,(mmlo-arg 'm:sup (caddr c))))))
              ((msqrt)
               (list `(m:rad (m:radPr (m:degHide (@ (m:val "1")))) (m:deg)
                             (m:e ,@(mmlo-row c)))))
              ((mroot)
               (if (!= (length c) 2) (mmlo-row c)
                   (list `(m:rad ,(mmlo-arg 'm:deg (cadr c)) ,(mmlo-arg 'm:e (car c))))))
              ((mover)
               (cond ((!= (length c) 2) (mmlo-row c))
                     ((mmlo-accent (cadr c))
                      (list `(m:acc (m:accPr (m:chr (@ (m:val ,(mmlo-accent (cadr c))))))
                                    ,(mmlo-arg 'm:e (car c)))))
                     (else (list `(m:limUpp ,(mmlo-arg 'm:e (car c))
                                            ,(mmlo-arg 'm:lim (cadr c)))))))
              ((munder)
               (if (!= (length c) 2) (mmlo-row c)
                   (list `(m:limLow ,(mmlo-arg 'm:e (car c)) ,(mmlo-arg 'm:lim (cadr c))))))
              ((munderover)
               (if (!= (length c) 3) (mmlo-row c)
                   (list `(m:limUpp (m:e (m:limLow ,(mmlo-arg 'm:e (car c))
                                                   ,(mmlo-arg 'm:lim (cadr c))))
                                    ,(mmlo-arg 'm:lim (caddr c))))))
              ((mfenced)
               (list `(m:d (m:dPr (m:begChr (@ (m:val ,(or (ox-attr x 'open) "("))))
                                  (m:endChr (@ (m:val ,(or (ox-attr x 'close) ")")))))
                           ,@(map (lambda (y) (mmlo-arg 'm:e y)) c))))
              ((mtable)
               (list `(m:m ,@(map (lambda (r)
                                    `(m:mr ,@(map (lambda (d)
                                                    `(m:e ,@(mmlo-row (ox-children d))))
                                                  (list-filter (ox-elements r)
                                                               (lambda (d) (mmlo-is? d 'mtd))))))
                                  (list-filter c (lambda (r) (or (mmlo-is? r 'mtr)
                                                                 (mmlo-is? r 'mlabeledtr))))))))
              ((mmultiscripts)
               ;; the scripts before the base, when there are some
               (let* ((i (list-find-index c (lambda (y) (mmlo-is? y 'mprescripts)))))
                 (cond ((and i (>= (length c) (+ i 3)))
                        (list `(m:sPre ,(mmlo-arg 'm:sub (list-ref c (+ i 1)))
                                       ,(mmlo-arg 'm:sup (list-ref c (+ i 2)))
                                       ,(mmlo-arg 'm:e (car c)))))
                       ((>= (length c) 3)
                        (list `(m:sSubSup ,(mmlo-arg 'm:e (car c)) ,(mmlo-arg 'm:sub (cadr c))
                                          ,(mmlo-arg 'm:sup (caddr c)))))
                       (else (mmlo-row c)))))
              ((menclose)
               (list `(m:borderBox (m:e ,@(mmlo-row c)))))
              ((none mprescripts annotation annotation-xml) '())
              (else (mmlo-row (ox-children x))))))))

(tm-define (mathml->omml x)
  (:synopsis "The tree of MathML @x as a formula of Word, an element m:oMath")
  (cons 'm:oMath (mmlo-node x)))
