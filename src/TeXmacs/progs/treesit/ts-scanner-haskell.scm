
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-haskell.scm
;; DESCRIPTION : the external scanner of tree-sitter-haskell
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-haskell (MIT), which explains the
;; design at length: the layouts (a stack of contexts with their
;; indentation), the newline lookahead and processing over several runs,
;; the symbolic operators and their variants (tight, prefix, sections), the
;; qualified names, comments, pragmas, CPP and quasiquotes, and the
;; lookahead of constraints and infix declarations. The functions follow
;; those of C, in the same order. The environment of C (the lexer, the
;; valid tokens, the state) is in variables of the module during a scan
;; (scans do not nest), as is the lookahead buffer, which C empties at
;; each deserialize and which is emptied here at the start of each scan
;; (the trampolines skip the deserialize which restores the same state).
;; FAIL is #f, so that SEQ is or; the lexed tokens are symbols, #f being
;; LNothing. The classes of characters are in ts-haskell-chars.scm.

(texmacs-module (treesit ts-scanner-haskell)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-scan)
        (treesit ts-haskell-chars)))

(define t-fail 0)
(define t-semicolon 1)
(define t-start 2)
(define t-start-do 3)
(define t-start-case 4)
(define t-start-if 5)
(define t-start-let 6)
(define t-start-quote 7)
(define t-start-explicit 8)
(define t-end 9)
(define t-end-explicit 10)
(define t-start-brace 11)
(define t-end-brace 12)
(define t-start-texp 13)
(define t-end-texp 14)
(define t-where 15)
(define t-in 16)
(define t-arrow 17)
(define t-bar 18)
(define t-deriving 19)
(define t-comment 20)
(define t-haddock 21)
(define t-cpp 22)
(define t-pragma 23)
(define t-qq-start 24)
(define t-qq-body 25)
(define t-splice 26)
(define t-qual-dot 27)
(define t-tight-dot 28)
(define t-prefix-dot 29)
(define t-dotdot 30)
(define t-tight-at 31)
(define t-prefix-at 32)
(define t-tight-bang 33)
(define t-prefix-bang 34)
(define t-tight-tilde 35)
(define t-prefix-tilde 36)
(define t-prefix-percent 37)
(define t-qualified-op 38)
(define t-left-section-op 39)
(define t-no-section-op 40)
(define t-minus 41)
(define t-context 42)
(define t-infix 43)
(define t-data-infix 44)
(define t-type-instance 45)
(define t-varsym 46)
(define t-consym 47)
(define t-update 48)

;; the sorts of contexts, in the order of C (they are compared)
(define decl-layout 0)
(define do-layout 1)
(define case-layout 2)
(define let-layout 3)
(define quote-layout 4)
(define multi-way-if-layout 5)
(define braces 6)
(define texp 7)
(define module-header 8)
(define no-context 9)

;; the states of the newline
(define n-inactive 0)
(define n-init 1)
(define n-process 2)
(define n-resume 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The payload is #(contexts newline): the contexts are a list of (sort .
;; indent), the innermost first, whose pairs are not modified; the newline
;; is #(state end indent eof no-semi skip-semi unsafe).

(define (make-newline) (vector n-inactive #f 0 #f #f #f #f))
(define (nl-state nl) (vector-ref nl 0))
(define (nl-end nl) (vector-ref nl 1))
(define (nl-indent nl) (vector-ref nl 2))
(define (nl-eof nl) (vector-ref nl 3))
(define (nl-no-semi nl) (vector-ref nl 4))
(define (nl-skip-semi nl) (vector-ref nl 5))
(define (nl-unsafe nl) (vector-ref nl 6))

(define (haskell-create) (vector '() (make-newline)))

(define (haskell-serialize p)
  ;; never #f: as in C, a state is never empty (an empty one means resume)
  (list (vector-ref p 0) (vector->list (vector-ref p 1))))

(define (haskell-deserialize p state)
  (if state
      (begin
        (vector-set! p 0 (car state))
        (vector-set! p 1 (list->vector (cadr state))))
      (let ((nl (make-newline)))
        (vector-set! nl 0 n-resume)
        (vector-set! p 0 '())
        (vector-set! p 1 nl))))

;; the environment of a scan
(define L #f)          ; the lexer
(define V #f)          ; the valid tokens
(define P #f)          ; the payload
(define NL #f)         ; its newline
(define la 0)          ; the lookahead of the lexer
(define buf (make-vector 64 0))   ; the lookahead buffer
(define size 0)
(define offset 0)
(define symop 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexer interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eof?) (lexer-eof? L))

(define (column) (if (eof?) 0 (lexer-column L)))

(define (adv!)
  ;; advance, appending the character to the lookahead buffer
  (unless (eof?)
    (when (= size (vector-length buf))
      (let ((b (make-vector (* 2 size) 0)))
        (do ((i 0 (+ i 1))) ((= i size)) (vector-set! b i (vector-ref buf i)))
        (set! buf b)))
    (vector-set! buf size la)
    (set! size (+ size 1))
    (lexer-advance! L #f)
    (set! la (lexer-lookahead L))))

(define (skp!)
  (lexer-advance! L #t)
  (set! la (lexer-lookahead L)))

(define (mark!) (lexer-mark-end! L))

(define (finish-if-valid s) (and (vector-ref V s) s))

(define (finish-marked s) (mark!) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookahead
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (advance-over-abs abs)
  (do ((i size (+ i 1))) ((> i abs)) (adv!)))

(define (advance-over rel) (advance-over-abs (+ offset rel)))

(define (skip-over rel)
  (when (> offset size) (advance-over-abs (- offset 1)))
  (let ((abs (+ offset rel)))
    (do ((i size (+ i 1))) ((> i abs)) (skp!))))

(define (advance-before rel)
  (let ((abs (+ offset rel)))
    (when (> abs 0) (advance-over-abs (- abs 1)))))

(define (unsafe-peek rel)
  (let ((abs (+ offset rel)))
    (if (< abs size) (vector-ref buf abs) 0)))

(define (peek rel)
  (if (< (+ offset rel) size)
      (vector-ref buf (+ offset rel))
      (begin (advance-before rel) la)))

(define (char-at? n c) (= (peek n) c))

(define (reset-lookahead-abs abs)
  (set! offset abs)
  (set! symop 0))

(define (reset-lookahead-to rel) (reset-lookahead-abs (+ offset rel)))

(define (reset-lookahead) (reset-lookahead-abs size))

(define (no-lookahead?) (= size 0))

(define (start-column) (- (column) size))

(define (advance-while i pred)
  (let loop ((i i)) (if (pred (peek i)) (loop (+ i 1)) i)))

(define (advance-until-char i c)
  (let loop ((i i))
    (if (and (not (eof?)) (not (char-at? i c))) (loop (+ i 1)) i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (contexts) (vector-ref P 0))
(define (has-contexts?) (pair? (contexts)))

(define (push-context sort indent)
  (vector-set! P 0 (cons (cons sort indent) (contexts))))

(define (pop!)
  (when (has-contexts?) (vector-set! P 0 (cdr (contexts)))))

(define (current-context)
  (if (has-contexts?) (caar (contexts)) no-context))

(define (layout-context?) (< (current-context) braces))
(define (semicolon-context?) (< (current-context) multi-way-if-layout))

(define (current-indent)
  (let loop ((l (contexts)))
    (cond ((null? l) 0)
          ((< (caar l) braces) (cdar l))
          (else (loop (cdr l))))))

(define (indent-less? indent)
  (and (layout-context?) (< indent (current-indent))))

(define (indent-lesseq? indent)
  (and (layout-context?) (<= indent (current-indent))))

(define (top-layout?)
  (let ((l (contexts))) (and (pair? l) (null? (cdr l)))))

(define (in-module-header?) (= (current-context) module-header))

(define (context-end-sym s)
  (cond ((= s texp) t-end-texp)
        ((= s braces) t-end-brace)
        ((< s braces) t-end)
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ascii-table ranges)
  ;; the class for the first 128 code points, as a vector of booleans
  (let ((v (make-vector 128 #f)))
    (do ((c 0 (+ c 1))) ((= c 128) v)
      (vector-set! v c (char-in-ranges? ranges c)))))

(define identifier-ascii (ascii-table haskell-identifier-chars))
(define varid-start-ascii (ascii-table haskell-varid-start-chars))
(define conid-start-ascii (ascii-table haskell-conid-start-chars))
(define symop-ascii (ascii-table haskell-symop-chars))
(define space-ascii (ascii-table haskell-space-chars))

(define (identifier-char? c)
  (if (< c 128) (vector-ref identifier-ascii c)
      (char-in-ranges? haskell-identifier-chars c)))
(define (varid-start-char0? c)
  (if (< c 128) (vector-ref varid-start-ascii c)
      (char-in-ranges? haskell-varid-start-chars c)))
(define (conid-start-char? c)
  (if (< c 128) (vector-ref conid-start-ascii c)
      (char-in-ranges? haskell-conid-start-chars c)))
(define (symop-char0? c)
  (if (< c 128) (vector-ref symop-ascii c)
      (char-in-ranges? haskell-symop-chars c)))
(define (space-char? c)
  (if (< c 128) (vector-ref space-ascii c)
      (char-in-ranges? haskell-space-chars c)))

(define (newline? c) (or (= c 10) (= c 13) (= c 12)))
(define (varid-start-char? c) (or (= c 95) (varid-start-char0? c)))
(define (id-char? c) (or (= c 95) (= c 39) (identifier-char? c)))
(define (inner-id-char? c) (or (id-char? c) (= c 35)))
(define (quoter-char? c) (or (id-char? c) (= c 46)))
(define (reserved-symbolic? c) (memv c '(40 41 44 59 91 93 96 123 125 34 39 95)))
(define (symop-char? c) (and (symop-char0? c) (not (reserved-symbolic? c))))

(define (symop-lookahead)
  (when (= symop 0) (set! symop (advance-while 0 symop-char?)))
  symop)

(define (symop?) (> (symop-lookahead) 0))

(define (after-error?) (vector-ref V t-fail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (seq-from s start)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((= i n) (peek (+ start n)) #t)
            ((= (char->integer (string-ref s i)) (peek (+ start i))) (loop (+ i 1)))
            (else #f)))))

(define (seq s) (seq-from s 0))

(define (take-line)
  (let loop ()
    (when (and (not (eof?)) (not (newline? la))) (adv!) (loop))))

(define (space-or-tab? c) (or (= c 32) (= c 9)))

(define (take-line-escaped-newline)
  (let loop ()
    (let more ()
      (when (and (not (eof?)) (not (newline? la)) (not (= la 92))) (adv!) (more)))
    (when (= la 92)
      (adv!)
      (if (space-or-tab? la)
          (begin
            (let sp () (when (space-or-tab? la) (adv!) (sp)))
            (when (newline? la) (adv!)))
          (adv!))
      (loop))))

(define (skip-space)
  (and (space-char? la)
       (begin
         (skp!)
         (let loop () (when (space-char? la) (skp!) (loop)))
         #t)))

(define (skip-newlines)
  (and (newline? la)
       (begin
         (skp!)
         (let loop () (when (newline? la) (skp!) (loop)))
         #t)))

(define (skip-whitespace)
  ;; 'no, 'indented or 'bol
  (let loop ((space 'no))
    (cond ((skip-space) (loop 'indented))
          ((skip-newlines) (loop 'bol))
          (else space))))

(define (take-space-from start) (advance-while start space-char?))

(define (token-end? c) (not (inner-id-char? c)))

(define (token-from s start)
  (and (seq-from s start) (token-end? (peek (+ start (string-length s))))))

(define (token s) (token-from s 0))

(define (any-token-from l start)
  (and (pair? l) (or (token-from (car l) start) (any-token-from (cdr l) start))))

(define (match-symop target)
  (and (= (symop-lookahead) (string-length target)) (seq target)))

(define (uninitialized?) (not (has-contexts?)))

(define (conid)
  (if (not (conid-start-char? (peek 0))) 0
      (advance-while 1 inner-id-char?)))

(define (qualified-name name)
  ;; 'none, 'target or 'conid
  (let loop ((qualified #f))
    (let ((end (conid)))
      (cond ((= end 0) 'none)
            ((not (char-at? end 46)) (if qualified 'conid 'none))
            (else
             (reset-lookahead-to (+ end 1))
             (if (name) 'target (loop #t)))))))

(define (odd-backslashes-before index)
  (let loop ((index index) (odd #f))
    (if (and (>= index 0) (= (peek index) 92))
        (loop (- index 1) (not odd))
        odd)))

(define (take-string-literal)
  (let loop ((end 1))
    (let ((end (+ (advance-until-char end 34) 1)))
      (if (or (eof?) (not (odd-backslashes-before (- end 2)))) end (loop end)))))

(define (take-char-literal)
  (cond ((char-at? 1 92) (+ (advance-until-char 2 39) 2))
        ((char-at? 2 39) 3)
        (else 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cpp-tokens-start '("if" "ifdef" "ifndef"))
(define cpp-tokens-else '("else" "elif" "elifdef" "elifndef"))
(define cpp-tokens-other '("define" "undef" "include" "pragma" "error" "warning" "line"))

(define (cpp-directive)
  ;; 'nothing, 'start, 'else, 'end or 'other
  (if (not (char-at? 0 35)) 'nothing
      (let ((start (take-space-from 1)))
        (cond ((any-token-from cpp-tokens-start start) 'start)
              ((any-token-from cpp-tokens-else start) 'else)
              ((token-from "endif" start) 'end)
              ((or (any-token-from cpp-tokens-other start)
                   (newline? (peek start))
                   (and (char-at? 1 33) (uninitialized?)))
               'other)
              (else 'nothing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (start-brace)
  (and (vector-ref V t-start-brace)
       (begin (push-context braces 0) t-start-brace)))

(define (end-brace)
  (and (vector-ref V t-end-brace) (= (current-context) braces)
       (begin (pop!) t-end-brace)))

(define (valid-layout-start-sym)
  (let loop ((i t-start))
    (cond ((>= i t-end) #f)
          ((vector-ref V i) i)
          (else (loop (+ i 1))))))

(define (layout-sort s)
  (cond ((eqv? s t-start-do) do-layout)
        ((eqv? s t-start-case) case-layout)
        ((eqv? s t-start-if) multi-way-if-layout)
        ((eqv? s t-start-let) let-layout)
        ((eqv? s t-start-quote) quote-layout)
        (else decl-layout)))

(define (valid-layout-start next)
  ;; (sym . sort), the sort being no-context when no layout starts
  (let ((sym (valid-layout-start-sym)))
    (if (or (uninitialized?) (not sym)) (cons sym no-context)
        (let ((sort (layout-sort sym)))
          (cond ((eq? next 'bar) (cons sym sort))
                ((eq? next 'brace-open)
                 (if (newline-active?) (cons sym no-context)
                     (cons t-start-explicit braces)))
                ((= sort multi-way-if-layout) (cons sym no-context))
                (else (cons sym sort)))))))

(define (indent-can-start-layout? sort indent)
  (or (= (current-context) braces)
      (let ((cur (current-indent)))
        (or (> indent cur) (and (= indent cur) (= sort do-layout))))))

(define (start-layout start indent)
  (and (cond ((in-module-header?) (pop!) #t)
             ((= (cdr start) braces) (mark!) #t)
             (else (indent-can-start-layout? (cdr start) indent)))
       (begin
         (push-context (cdr start) indent)
         (car start))))

(define (start-layout-interior next)
  (let ((start (valid-layout-start next)))
    (and (not (= (cdr start) no-context))
         (start-layout start (start-column)))))

(define (start-layout-newline)
  (let ((start (valid-layout-start (nl-end NL))))
    (and (not (= (cdr start) no-context))
         (let ((result (start-layout start (nl-indent NL))))
           (when result (vector-set! NL 4 #t))
           result))))

(define (texp-context)
  (cond ((vector-ref V t-start-texp) (push-context texp 0) t-start-texp)
        ((and (vector-ref V t-end-texp) (= (current-context) texp)) (pop!) t-end-texp)
        (else #f)))

(define (end-layout-unchecked) (pop!) t-end)

(define (end-layout) (and (vector-ref V t-end) (end-layout-unchecked)))

(define (end-layout-brace)
  (and (vector-ref V t-end-explicit) (= (current-context) braces)
       (begin
         (advance-over 0)
         (mark!)
         (pop!)
         t-end-explicit)))

(define (end-layout-indent)
  (and (vector-ref V t-end) (indent-less? (nl-indent NL))
       (if (top-layout?)
           (begin
             ;; the indentation of the top layout becomes that of the line
             (vector-set! P 0 (list (cons (caar (contexts)) (nl-indent NL))))
             t-update)
           (begin
             (vector-set! NL 5 #f)
             (end-layout-unchecked)))))

(define (end-layout-infix)
  (and (not (vector-ref V t-varsym)) (not (vector-ref V t-consym)) (end-layout)))

(define (end-layout-where)
  (and (vector-ref V t-end) (not (vector-ref V t-where)) (layout-context?) (end-layout)))

(define (end-layout-in)
  (and (vector-ref V t-end) (or (not (vector-ref V t-in)) (= (current-context) let-layout))
       (end-layout)))

(define (end-layout-deriving)
  (and (vector-ref V t-end) (not (vector-ref V t-deriving)) (not (top-layout?))
       (= (current-context) decl-layout) (end-layout)))

(define (layouts-in-texp?)
  (and (layout-context?) (pair? (contexts)) (pair? (cdr (contexts)))
       (let loop ((l (cdr (contexts))))
         (cond ((null? l) #f)
               ((or (= (caar l) texp) (= (caar l) braces)) #t)
               ((> (caar l) braces) #f)
               (else (loop (cdr l)))))))

(define (token-end-layout-texp)
  (and (vector-ref V t-end) (layouts-in-texp?) (end-layout)))

(define (force-end-context)
  (let loop ()
    (and (has-contexts?)
         (let ((s (context-end-sym (current-context))))
           (pop!)
           (if (and s (vector-ref V s)) s (loop))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (opening-token? i)
  (let ((c (peek i)))
    (cond ((memv c '(#x27e6 #x2987 40 91 34)) #t)
          ((= c 123) (not (= (peek (+ i 1)) 45)))
          (else (id-char? c)))))

(define (valid-symop-two-chars? c1 c2)
  (cond ((= c1 61) (not (= c2 62)))   ; =>
        ((= c1 60) (not (= c2 45)))   ; <-
        ((= c1 58) (not (= c2 58)))   ; ::
        (else #t)))

(define (lex-prefix t) (if (opening-token? 1) t 'symop))

(define (lex-splice c) (if (or (varid-start-char? c) (= c 40)) 'dollar 'symop))

(define (lex-symop)
  (let ((len (symop-lookahead)))
    (if (= len 0) #f
        (let ((c1 (unsafe-peek 0)))
          (cond
           ((= len 1)
            (cond ((= c1 63) (if (varid-start-char? (peek 1)) #f 'symop))   ; ?
                  ((= c1 35) (if (char-at? 1 41) 'unboxed-close 'hash))     ; #
                  ((= c1 124) (if (char-at? 1 93) 'quote-close 'bar))       ; |
                  ((= c1 33) (lex-prefix 'bang))
                  ((= c1 126) (lex-prefix 'tilde))
                  ((= c1 64) (lex-prefix 'at))
                  ((= c1 37) (lex-prefix 'percent))
                  ((= c1 36) (lex-splice (peek 1)))
                  ((= c1 46)
                   (cond ((id-char? (peek 1)) 'dot-id)
                         ((opening-token? 1) 'dot-open)
                         (else 'symop)))
                  ((memv c1 '(#x2192 #x22b8)) 'arrow)
                  ((= c1 #x21d2) 'carrow)
                  ((memv c1 '(61 #x27e7 #x2988)) 'texp-closer)
                  ((memv c1 '(42 45)) 'symop-special)
                  ((memv c1 '(92 #x2190 #x2200 #x2237 #x2605 #x27e6 #x2919
                              #x291a #x291b #x291c #x2987))
                   #f)
                  (else 'symop)))
           ((= len 2)
            (cond ((seq "->") 'arrow)
                  ((seq "=>") 'carrow)
                  (else
                   (let ((c2 (unsafe-peek 1)))
                     (cond ((and (= c1 36) (= c2 36)) (lex-splice (peek 2)))
                           ((and (= c1 124) (= c2 124) (char-at? 2 93)) 'quote-close)
                           ((= c1 46) (if (= c2 46) 'dot-dot 'dot-symop))
                           ((and (= c1 35) (or (= c2 35) (= c2 124))) 'symop-special)
                           ((memv c1 '(36 124 35)) 'symop)
                           ((not (valid-symop-two-chars? c1 c2)) #f)
                           (else 'symop))))))
           ((and (= c1 45) (seq "->.")) 'arrow)
           ((= c1 46) 'dot-symop)
           (else 'symop))))))

(define (left-section-op start)
  (and (vector-ref V t-left-section-op)
       (begin
         (advance-before start)
         (let ((space (skip-whitespace)))
           (cond ((char-at? start 41) t-left-section-op)
                 ((not (eq? space 'no)) (finish-if-valid t-no-section-op))
                 (else #f))))))

(define (left-section-ticked)
  (and (vector-ref V t-left-section-op)
       (let ((end-tick (advance-until-char 1 96)))
         (and (char-at? end-tick 96) (left-section-op (+ end-tick 1))))))

(define (finish-symop s)
  (and (or (vector-ref V s) (vector-ref V t-left-section-op))
       (let ((after (symop-lookahead)))
         (or (left-section-op after)
             (begin (mark!) s)))))

(define (tight-op ws s) (and (not ws) (finish-if-valid s)))

(define (prefix-or-varsym ws s)
  (or (and ws (finish-if-valid s)) (finish-symop t-varsym)))

(define (tight-or-varsym ws s)
  (or (tight-op ws s) (finish-symop t-varsym)))

(define (infix-or-varsym ws prefix tight)
  (or (finish-if-valid (if ws prefix tight)) (finish-symop t-varsym)))

(define (qualified-op)
  (and (eq? (qualified-name symop?) 'target)
       (or (left-section-op (symop-lookahead)) t-qualified-op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qq-start?)
  (char-at? (advance-while 1 quoter-char?) 124))

(define (try-end-token target match) (and (token target) match))

(define (only-minus?)
  (let loop ((i 2))
    (if (= (peek i) 45) (loop (+ i 1)) (not (symop-char? (peek i))))))

(define (line-comment-herald?) (and (seq "--") (only-minus?)))

(define (lex-cpp)
  (let ((d (cpp-directive)))
    (cond ((eq? d 'else) 'cpp-else)
          ((eq? d 'nothing) #f)
          (else 'cpp))))

(define (lex-extras bol)
  (let ((c (peek 0)))
    (cond ((= c 123)   ; {
           (and (char-at? 1 45) (if (char-at? 2 35) 'pragma 'block-comment)))
          ((= c 35) (and bol (lex-cpp)))
          ((= c 45) (and (line-comment-herald?) 'line-comment))
          (else #f))))

(define (lex bol)
  (or (lex-extras bol)
      (let ((c (peek 0)))
        (if (symop-char? c) (lex-symop)
            (cond ((= c 119) (try-end-token "where" 'where))      ; w
                  ((= c 105) (try-end-token "in" 'in))            ; i
                  ((= c 116) (try-end-token "then" 'then))        ; t
                  ((= c 101) (try-end-token "else" 'else))        ; e
                  ((= c 100) (try-end-token "deriving" 'deriving)) ; d
                  ((= c 109)                                      ; m
                   (and (or (uninitialized?) (in-module-header?)) (token "module")
                        'module))
                  ((= c 123) 'brace-open)
                  ((= c 125) 'brace-close)
                  ((= c 59) 'semi)
                  ((= c 96) 'tick)
                  ((= c 91) (and (vector-ref V t-qq-start) (qq-start?) 'bracket-open))
                  ((memv c '(93 41 44)) 'texp-closer)
                  ((conid-start-char? c) 'upper)
                  (else #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extras
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cpp-else emit)
  (let loop ((nesting 1))
    (take-line-escaped-newline)
    (when emit (mark!))
    (adv!)
    (reset-lookahead)
    (let* ((d (cpp-directive))
           (nesting (cond ((eq? d 'start) (+ nesting 1))
                          ((eq? d 'end) (- nesting 1))
                          (else nesting))))
      (if (and (not (eof?)) (> nesting 0)) (loop nesting)
          (and emit t-cpp)))))

(define (cpp-line)
  (take-line-escaped-newline)
  (finish-marked t-cpp))

(define (comment-type)
  (let loop ((i 2))
    (if (= (peek i) 45) (loop (+ i 1))
        (let more ((i i))
          (if (eof?) t-comment
              (let ((c (peek i)))
                (cond ((or (= c 124) (= c 94)) t-haddock)
                      ((not (space-char? c)) t-comment)
                      (else (more (+ i 1))))))))))

(define (inline-comment)
  (let ((sym (comment-type)))
    (let loop ()
      (take-line)
      (mark!)
      (adv!)
      (reset-lookahead)
      (when (line-comment-herald?) (loop)))
    sym))

(define (consume-block-comment col)
  (let loop ((col col) (level 0))
    (if (eof?) col
        (let ((col (+ col 1)))
          (cond ((= la 123)   ; {
                 (adv!)
                 (if (= la 45) (begin (adv!) (loop (+ col 1) (+ level 1)))
                     (loop col level)))
                ((= la 45)    ; -
                 (adv!)
                 (if (= la 125)
                     (begin
                       (adv!)
                       (if (= level 0) (+ col 1) (loop (+ col 1) (- level 1))))
                     (loop col level)))
                ((newline? la) (adv!) (loop 0 level))
                ((= la 9) (adv!) (loop (+ col 7) level))
                (else (adv!) (loop col level)))))))

(define (block-comment)
  (let ((sym (comment-type)))
    (consume-block-comment size)
    (finish-marked sym)))

(define (consume-pragma)
  (and (seq "{-#")
       (begin
         (let loop ()
           (when (and (not (seq "#-}")) (not (eof?)))
             (reset-lookahead)
             (advance-over 0)
             (loop)))
         #t)))

(define (pragma)
  (and (consume-pragma)
       (begin
         (mark!)
         (unless (= (nl-state NL) n-inactive) (vector-set! NL 0 n-resume))
         t-pragma)))

(define (qq-body)
  (let loop ()
    (cond ((eof?) t-qq-body)
          ((= la #x27e7) (finish-marked t-qq-body))
          ((= la 124)
           (mark!)
           (adv!)
           (if (= la 93) t-qq-body (loop)))
          (else (adv!) (loop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semicolons and the processing of tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (explicit-semicolon)
  (and (vector-ref V t-semicolon) (not (nl-skip-semi NL))
       (begin (vector-set! NL 5 #t) t-update)))

(define (resolve-semicolon next)
  (and (nl-skip-semi NL)
       (not (memq next '(line-comment block-comment pragma semi)))
       (begin (vector-set! NL 5 #f) t-update)))

(define (semicolon)
  (and (semicolon-context?)
       (not (or (nl-no-semi NL) (nl-skip-semi NL)))
       (indent-lesseq? (nl-indent NL))
       (begin (vector-set! NL 4 #t) t-semicolon)))

(define (process-token-safe next)
  (case next
    ((where) (end-layout-where))
    ((in) (end-layout-in))
    ((then else) (end-layout))
    ((deriving) (end-layout-deriving))
    ((bar) (and (not (vector-ref V t-bar)) (end-layout)))
    ((pragma) (pragma))
    ((block-comment) (block-comment))
    ((line-comment) (inline-comment))
    ((cpp-else) (cpp-else #t))
    ((cpp) (cpp-line))
    ((symop tick hash) (end-layout-infix))
    ((unboxed-close) (or (token-end-layout-texp) (end-layout-infix)))
    ((arrow) (and (not (vector-ref V t-arrow)) (token-end-layout-texp)))
    ((texp-closer) (token-end-layout-texp))
    ((quote-close) (end-layout))
    (else #f)))

(define (process-token-symop ws next)
  (case next
    ((dot-dot) (or (finish-if-valid t-dotdot) (tight-op ws t-qual-dot)))
    ((dot-id) (or (finish-if-valid (if ws t-prefix-dot t-tight-dot))
                  (tight-op ws t-qual-dot)))
    ((dot-symop) (tight-or-varsym ws t-qual-dot))
    ((dot-open) (prefix-or-varsym ws t-prefix-dot))
    ((bang) (infix-or-varsym ws t-prefix-bang t-tight-bang))
    ((tilde) (infix-or-varsym ws t-prefix-tilde t-tight-tilde))
    ((at) (infix-or-varsym ws t-prefix-at t-tight-at))
    ((percent) (prefix-or-varsym ws t-prefix-percent))
    ((symop) (if (char-at? 0 58) (finish-symop t-consym) (finish-symop t-varsym)))
    ((symop-special)
     (or (left-section-op (symop-lookahead))
         (and (vector-ref V t-minus) (match-symop "-") t-minus)))
    ((unboxed-close hash) (left-section-op (symop-lookahead)))
    ((tick) (left-section-ticked))
    ((upper) (and (or (vector-ref V t-qualified-op) (vector-ref V t-left-section-op))
                  (qualified-op)))
    (else #f)))

(define (process-token-splice next)
  (and (eq? next 'dollar) (finish-if-valid t-splice)))

(define (process-token-interior next)
  (case next
    ((brace-close) (or (end-layout-brace) (token-end-layout-texp)))
    ((module) #f)
    ((semi) (explicit-semicolon))
    ((bracket-open) t-qq-start)
    (else (or (process-token-safe next) (start-layout-interior next)))))

(define (process-token-init indent next)
  (case next
    ((module) (push-context module-header 0) t-update)
    ((brace-open)
     (advance-over 0)
     (mark!)
     (push-context braces indent)
     t-start-explicit)
    (else (push-context decl-layout indent) t-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Newlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (newline-active?)
  (let ((s (nl-state NL))) (or (= s n-init) (= s n-process))))

(define (newline-init?) (= (nl-state NL) n-init))

(define (reset-newline!)
  (vector-set! P 1 (make-newline))
  (set! NL (vector-ref P 1)))

(define (newline-extras space)
  (let ((bol (or (eq? space 'bol) (and (eq? space 'no) (newline-init?)))))
    (process-token-safe (lex-extras bol))))

(define (newline-process)
  (let ((indent (nl-indent NL)) (end (nl-end NL)))
    (or (end-layout-indent)
        (process-token-safe end)
        (let ((space (skip-whitespace)))
          (mark!)
          (or (and (nl-unsafe NL) (newline-extras space))
              (and (not (nl-eof NL)) (start-layout-newline))
              (semicolon)
              (begin
                (reset-newline!)
                (if (uninitialized?)
                    (process-token-init indent end)
                    (or (process-token-symop #t end)
                        (process-token-splice end))))
              t-update)))))

(define (newline-post)
  (let ((res (newline-process)))
    (when (newline-init?) (vector-set! NL 0 n-process))
    res))

(define (newline-lookahead nl)
  (let loop ()
    (let ((c (peek 0)))
      (cond ((newline? c) (skip-over 0) (vector-set! nl 2 0) (reset-lookahead) (loop))
            ((= c 9) (skip-over 0) (vector-set! nl 2 (+ (nl-indent nl) 8))
             (reset-lookahead) (loop))
            ((space-char? c) (skip-over 0) (vector-set! nl 2 (+ (nl-indent nl) 1))
             (reset-lookahead) (loop))
            (else
             (let ((end (lex (= (nl-indent nl) 0))))
               (vector-set! nl 1 end)
               (when (not (no-lookahead?)) (vector-set! nl 6 #t))
               (case end
                 ((then else semi) (vector-set! nl 4 #t))
                 ((block-comment)
                  (vector-set! nl 2 (consume-block-comment (+ (nl-indent nl) 2)))
                  (reset-lookahead) (loop))
                 ((line-comment)
                  (vector-set! nl 2 0)
                  (take-line)
                  (reset-lookahead) (loop))
                 ((cpp-else)
                  (cpp-else #f)
                  (take-line-escaped-newline)
                  (reset-lookahead) (loop))
                 ((cpp)
                  (take-line-escaped-newline)
                  (reset-lookahead) (loop))
                 (else #f))))))))

(define (newline-start)
  (vector-set! NL 0 n-init)
  (newline-lookahead NL)
  (if (nl-unsafe NL) t-update (newline-post)))

(define (newline-resume)
  (let ((indent (nl-indent NL)))
    (skip-space)
    (reset-newline!)
    (vector-set! NL 2 indent)
    (newline-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraints, infix declarations and type instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the results are 'undecided, 'impossible, 'arrow, 'infix, 'equals and
;; 'bar; the state is #(reset brackets context infix data-infix
;; type-instance)

(define (ctr-bracket-open st)
  (vector-set! st 1 (+ (vector-ref st 1) 1))
  (vector-set! st 0 1)
  'undecided)

(define (ctr-bracket-close st)
  (if (= (vector-ref st 1) 0) 'impossible
      (begin
        (vector-set! st 1 (- (vector-ref st 1) 1))
        (vector-set! st 0 1)
        'undecided)))

(define (ctr-stop-on-token target) (if (token target) 'impossible 'undecided))

(define (ctr-top next)
  (case next
    ((carrow) 'arrow)
    ((symop symop-special tilde tick) 'infix)
    ((bar) 'bar)
    ((arrow where dot-dot semi) 'impossible)
    ((texp-closer) (if (char-at? 0 61) 'equals 'impossible))
    (else
     (let ((c (peek 0)))
       (cond ((= c 61) 'equals)
             ((= c #x2200) 'impossible)
             ((= c 58) (if (char-at? 1 58) 'impossible 'undecided))
             ((= c 102)
              (let ((r (ctr-stop-on-token "forall")))
                (if (eq? r 'undecided) (ctr-stop-on-token "family") r)))
             ((= c 105) (ctr-stop-on-token "instance"))
             (else 'undecided))))))

(define (ctr-lookahead-step st next)
  (vector-set! st 0 1)
  (let ((r (case next
             ((brace-close) (ctr-bracket-close st))
             ((unboxed-close)
              (let ((r (ctr-bracket-close st)))
                (if (eq? r 'undecided) (begin (vector-set! st 0 2) 'undecided) r)))
             ((brace-open) (ctr-bracket-open st))
             ((symop-special symop) (vector-set! st 0 (symop-lookahead)) #f)
             ((upper) (vector-set! st 0 (conid)) 'undecided)
             ((dot-id) 'undecided)
             ((pragma) (when (consume-pragma) (vector-set! st 0 3)) 'undecided)
             ((texp-closer #f)
              (let ((c (peek 0)))
                (cond ((or (= c 41) (= c 93)) (ctr-bracket-close st))
                      ((or (= c 40) (= c 91)) (ctr-bracket-open st))
                      ((= c 34) (vector-set! st 0 (take-string-literal)) 'undecided)
                      ((= c 39) (vector-set! st 0 (take-char-literal)) 'undecided)
                      (else
                       (when (varid-start-char? c)
                         (vector-set! st 0 (advance-while 1 id-char?)))
                       #f))))
             (else #f))))
    (cond (r r)
          ((not (= (vector-ref st 1) 0)) 'undecided)
          (else (ctr-top next)))))

(define (constraint-lookahead)
  (let ((st (vector 0 0 #f #f #f #f)))
    (let loop ()
      (when (not (eof?))
        (let ((nl (make-newline)))
          (vector-set! nl 2 99999)
          (newline-lookahead nl)
          (unless (and (<= (nl-indent nl) (current-indent))
                       (not (= (current-context) braces)))
            (let* ((result (ctr-lookahead-step st (nl-end nl)))
                   (done (case result
                           ((arrow) (vector-set! st 2 #t) #t)
                           ((infix)
                            (when (or (char-at? 0 58) (char-at? 0 96)) (vector-set! st 4 #t))
                            (vector-set! st 3 #t)
                            (not (vector-ref V t-context)))
                           ((equals) (vector-set! st 5 #t) (not (vector-ref V t-type-instance)))
                           ((bar) (vector-set! st 5 #f) #t)
                           ((impossible) #t)
                           (else #f))))
              (reset-lookahead-to (vector-ref st 0))
              (vector-set! st 0 0)
              (unless done (loop)))))))
    (or (and (vector-ref st 2) (finish-if-valid t-context))
        (and (vector-ref st 3) (finish-if-valid t-infix))
        (and (vector-ref st 4) (finish-if-valid t-data-infix))
        (and (vector-ref st 5) (finish-if-valid t-type-instance)))))

(define (process-token-constraint)
  (and (or (vector-ref V t-context) (vector-ref V t-infix) (vector-ref V t-data-infix)
           (vector-ref V t-type-instance))
       (constraint-lookahead)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interior ws)
  (let ((next (lex #f)))
    (or (resolve-semicolon next)
        (process-token-interior next)
        (process-token-symop ws next)
        (process-token-constraint)
        (process-token-splice next))))

(define (pre-ws-commands)
  (or (texp-context)
      (start-brace)
      (end-brace)
      (if (vector-ref V t-qq-body) (qq-body)
          (cond ((newline-active?) (newline-post))
                ((= (nl-state NL) n-resume) (newline-resume))
                (else #f)))))

(define (scan-main)
  (mark!)
  (or (pre-ws-commands)
      (let ((ws (skip-space)))
        (cond ((newline? la) (newline-start))
              ((not (eof?)) (interior ws))
              (else #f)))))

(define (process-result result)
  (let ((result
         (if (and (not result) (eof?) (no-lookahead?))
             (begin
               (mark!)
               (cond ((vector-ref V t-end) (end-layout-unchecked))
                     ((vector-ref V t-semicolon) t-semicolon)
                     (else (force-end-context))))
             result)))
    (and result (begin (lexer-set-result! L result) #t))))

(define (haskell-scan p lx valid)
  (set! L lx)
  (set! V valid)
  (set! P p)
  (set! NL (vector-ref p 1))
  (set! la (lexer-lookahead lx))
  (set! size 0)
  (set! offset 0)
  (set! symop 0)
  (and (not (after-error?))
       (process-result (scan-main))))

(define-public (ts-haskell-scanner)
  (vector haskell-create haskell-scan haskell-deserialize haskell-serialize))
