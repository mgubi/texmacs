;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : parse-test.scm
;; DESCRIPTION : tests of the packrat parser and of syntax highlighting
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite has two parts.
;;
;; The packrat parser (System/Language/packrat_*.cpp) with its Scheme front
;; end define-language (kernel/texmacs/tm-language.scm). Small grammars
;; named parse-test-* are defined below; a packrat grammar lives in a table
;; of C++ instances for the whole session, and the names are not used by
;; anything else. They are checked through packrat-correct? (does a rule
;; parse the whole input), packrat-parse (where the longest parse of a rule
;; from the start ends, as a path in the input tree, (-1) when it fails at
;; once) and packrat-context (the selectable structures around a position).
;; The input is a tree, serialized for the parser: strings as they are, a
;; tag as <\tag>arg<|>arg</>, the lines of a document followed by a
;; newline, and the presentation tags (with, concat...) transparently. The
;; grammar of formulas, std-math (language/std-math.scm, std-symbols.scm),
;; is checked on many correct and incorrect formulas, rule by rule for its
;; precedence levels, and through the classes of symbols that the
;; typesetter takes from it (math-symbol-group, math-symbol-type).
;;
;; Syntax highlighting of programs, by the C++ languages of
;; System/Language: cpp, scheme, mathemagix, fortran, r and scilab have
;; their own highlighters; python, java, scala, julia, json and csv are
;; prog_language_rep, configured by the tables of their *-lang.scm module
;; (parser-feature); the others are verb_language_rep, which only colors
;; through the :highlight properties of a packrat grammar of that name.
;; Nothing exposes the color of a token to Scheme, so the suite typesets the
;; code in prog mode and prints it with print-snippet into an EPS file: the
;; PostScript printer selects a color (/Cn {r g b setrgbcolor}) before the
;; glyphs drawn with it, and the code font (ectt) draws each ASCII character
;; as its own code. The glyphs are matched back to the characters of the
;; source, and a line is read as a list of runs (text color), a new run
;; starting at a space or where the color changes. The code is typeset in a
;; buffer, since the highlighters look at the neighbouring lines through the
;; tree of the buffer (multi-line comments, preprocessor lines).

(texmacs-module (check parse-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Arithmetic with left recursive rules, which the C++ side rewrites into
;; Sum-head (Sum-tail)*.
(define-language parse-test-arith
  (:synopsis "arithmetic, for the tests")
  (define Main (Spc Sum Spc))
  (define Spc (* " "))
  (define Sum (Sum Plus Prod) (Sum "-" Prod) Prod)
  (define Plus (:operator associative) "+")
  (define Prod (Prod "*" Atom) Atom)
  (define Atom ("(" Sum ")") Num Var (:<frac Sum :/ Sum :>))
  (define Num (:type symbol) (+ (- "0" "9")))
  (define Var (:highlight variable_identifier) (+ (- "a" "z"))))

;; The same, right recursive, for the contexts (see test-context)
(define-language parse-test-right
  (define Main Sum)
  (define Sum (Prod "+" Sum) Prod)
  (define Prod (Atom "*" Prod) Atom)
  (define Atom (+ (- "a" "z")) ("(" Sum ")")))

;; The constructs of the rules
(define-language parse-test-peg
  (define First "a" "ab")
  (define Second "ab" "a")
  (define Stars (* "x"))
  (define Pluses (+ "x"))
  (define NotX ((not "x") :char))
  (define Except (except :char "q"))
  (define Empty "")
  (define Word "abc")
  (define Sym "<alpha>")
  (define Frac (:<frac :any :/ :any :>))
  (define AnyTag (:< :args :>))
  (define Leaf :leaf)
  ;; FIXME: a rule without alternatives cannot be defined
  ;; (tm-language.scm:57 passes the list '(or) where packrat-define wants a
  ;; tree): (define-language l (define Never)) raises wrong-type-arg in
  ;; packrat-define, expected a rule which never matches, as (or) gives.
  (define Never (or))
  (define Cur ("a" :cursor "b")))

;; Inheritance: parse-test-derived takes the rules of parse-test-base and
;; adds and overrides some
(define-language parse-test-base
  (define Main (+ Letter))
  (define Letter (- "a" "c")))

(define-language parse-test-derived
  (inherit parse-test-base)
  (define Letter (- "a" "e"))
  (define Pair (Letter Letter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for the parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ok? lan rule x)
  (packrat-correct? lan rule (stree->tree x)))

(define (end lan rule x)
  (packrat-parse lan rule (stree->tree x)))

(define (arith? x) (ok? "parse-test-arith" "Main" x))
(define (arith-end x) (end "parse-test-arith" "Main" x))
(define (peg? rule x) (ok? "parse-test-peg" rule x))
(define (peg-end rule x) (end "parse-test-peg" rule x))
(define (math? x) (ok? "std-math" "Main" x))
(define (math-rule? rule x) (ok? "std-math" rule x))

(define (context x p)
  (packrat-context "parse-test-right" "Main" (stree->tree x) p))

(define (run-group name thunk)
  ;; an error in a group counts as one failure and does not stop the suite
  (check-group name)
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define-language keeps the rules of each symbol, without the properties
;; (:operator, :type, :highlight...), which go to the C++ grammar.
(define (test-definitions)
  (check= (get-packrat-definition "parse-test-arith" "Sum")
          '((Sum Plus Prod) (Sum "-" Prod) Prod))
  (check= (get-packrat-definition "parse-test-arith" "Plus") '("+"))
  (check= (get-packrat-definition "parse-test-arith" "Num") '((+ (- "0" "9"))))
  (check= (get-packrat-definition "parse-test-arith" "Var") '((+ (- "a" "z"))))
  (check= (get-packrat-definition "parse-test-arith" "Atom")
          '(("(" Sum ")") Num Var (:<frac Sum :/ Sum :>)))
  (check-false (get-packrat-definition "parse-test-arith" "Nope"))
  (check-false (get-packrat-definition "parse-test-nolang" "Main"))
  ;; inherited rules are not copied on the Scheme side
  (check-false (get-packrat-definition "parse-test-derived" "Main"))
  (check= (get-packrat-definition "parse-test-derived" "Letter") '((- "a" "e")))
  ;; the definitions of std-math
  (check= (get-packrat-definition "std-math" "Main") #f)
  (check= (get-packrat-definition "std-math-grammar" "Identifier")
          '((+ (or (- "a" "z") (- "A" "Z")))))
  (check= (get-packrat-definition "std-symbols" "Not-symbol") '("<neg>"))
  (check= (get-packrat-definition "std-symbols" "Quantifier-symbol")
          '("<forall>" "<exists>" "<nexists>" "<Exists>" "<mathlambda>"))
  ;; an invalid rule is refused
  (check-error (define-language-impl "parse-test-bad" '((foo))) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-strings)
  (check-true (arith? "1+2"))
  (check-true (arith? "1+2+3"))
  (check-true (arith? "1-2-3"))
  (check-true (arith? "12*x+y*3"))
  (check-true (arith? "(1+2)*x"))
  (check-true (arith? "((a))"))
  (check-true (arith? " 1"))
  (check-true (arith? "1 "))
  (check-false (arith? ""))
  (check-false (arith? "+"))
  (check-false (arith? "1+"))
  (check-false (arith? "1+2*"))
  (check-false (arith? "12 + 3"))
  (check-false (arith? "(1+2"))
  (check-false (arith? "1+2)"))
  (check-false (arith? "A"))
  ;; where the longest parse ends
  (check= (arith-end "1+2") '(3))
  (check= (arith-end "1+2*") '(3))
  (check= (arith-end "12 + 3") '(3))
  (check= (arith-end "(1+2)*x") '(7))
  (check= (arith-end " 1") '(2))
  (check= (arith-end "1+2)") '(3))
  (check= (arith-end "") '(-1))
  (check= (arith-end "+") '(-1))
  ;; an inner rule, alone
  (check-true (ok? "parse-test-arith" "Prod" "2*x"))
  (check-false (ok? "parse-test-arith" "Prod" "2+x"))
  (check= (end "parse-test-arith" "Prod" "2*x+1") '(3))
  (check-true (ok? "parse-test-arith" "Num" "007"))
  (check-false (ok? "parse-test-arith" "Num" "x"))
  ;; the left recursive rules are rewritten by the C++ side
  (check-true (ok? "parse-test-arith" "Sum-head" "1"))
  (check-true (ok? "parse-test-arith" "Sum-tail" "+1"))
  (check-true (ok? "parse-test-arith" "Sum-tail" "-1"))
  (check-false (ok? "parse-test-arith" "Sum-tail" "1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The constructs of the rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Packrat parsing is ordered choice without backtracking into a choice
;; once it succeeded: "a" | "ab" never parses "ab" as a whole.
(define (test-peg)
  (check-true (peg? "First" "a"))
  (check-false (peg? "First" "ab"))
  (check= (peg-end "First" "ab") '(1))
  (check-true (peg? "Second" "ab"))
  (check-true (peg? "Second" "a"))
  ;; repetitions
  (check-true (peg? "Stars" ""))
  (check-true (peg? "Stars" "xx"))
  (check-false (peg? "Stars" "xy"))
  (check= (peg-end "Stars" "xxy") '(2))
  (check= (peg-end "Stars" "y") '(0))
  (check-false (peg? "Pluses" ""))
  (check-true (peg? "Pluses" "x"))
  (check-true (peg? "Pluses" "xx"))
  (check= (peg-end "Pluses" "y") '(-1))
  ;; negative lookahead and exceptions; a character is a symbol <...> too
  (check-true (peg? "NotX" "a"))
  (check-false (peg? "NotX" "x"))
  (check-false (peg? "NotX" ""))
  (check-true (peg? "NotX" "<alpha>"))
  (check= (peg-end "NotX" "<alpha>") '(7))
  (check-true (peg? "Except" "x"))
  (check-false (peg? "Except" "q"))
  (check-false (peg? "Except" "ab"))
  ;; the empty word, words and symbols
  (check-true (peg? "Empty" ""))
  (check-false (peg? "Empty" "a"))
  (check= (peg-end "Empty" "a") '(0))
  (check-true (peg? "Word" "abc"))
  (check-false (peg? "Word" "ab"))
  (check= (peg-end "Word" "ab") '(-1))
  (check-true (peg? "Sym" "<alpha>"))
  (check-false (peg? "Sym" "<beta>"))
  (check-false (peg? "Sym" "alpha"))
  ;; markup: a given tag, any tag, the text up to the next tag
  (check-true (peg? "Frac" '(frac "1" "2")))
  (check-true (peg? "Frac" '(frac (sqrt "x") "")))
  (check-false (peg? "Frac" '(frac "1")))
  (check-false (peg? "Frac" '(sqrt "x")))
  (check-false (peg? "Frac" "frac"))
  (check-true (peg? "AnyTag" '(frac "1" "2")))
  (check-true (peg? "AnyTag" '(sqrt "x")))
  (check-true (peg? "AnyTag" '(frac "1")))
  (check-false (peg? "AnyTag" "x"))
  (check-true (peg? "Leaf" ""))
  (check-true (peg? "Leaf" "a b<alpha>"))
  (check-false (peg? "Leaf" '(concat "a" (frac "1" "2"))))
  (check= (peg-end "Leaf" '(concat "a" (frac "1" "2"))) '(0 1))
  ;; a rule which never matches, an undefined rule
  (check-false (peg? "Never" ""))
  (check-false (peg? "Never" "a"))
  (check= (peg-end "Never" "a") '(-1))
  (check-false (peg? "Undefined" ""))
  (check= (peg-end "Undefined" "a") '(-1))
  ;; there is no cursor in the input of packrat-correct?
  (check-false (peg? "Cur" "ab"))
  (check= (peg-end "Cur" "ab") '(-1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees and positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-trees)
  ;; concat and with are transparent
  (check-true (arith? '(concat "1+" "2")))
  (check-true (arith? '(concat "1" (with "color" "red" "+2"))))
  (check= (arith-end '(concat "1" (with "color" "red" "+2"))) '(1 2 2))
  (check-true (arith? '(with "font-series" "bold" "x*y")))
  ;; a with in text mode is left out
  (check-true (arith? '(concat "1+2" (with "mode" "text" "hello"))))
  ;; a tag of the grammar
  (check-true (arith? '(concat "1+" (frac "2" "x"))))
  (check= (arith-end '(concat "1+" (frac "2" "x"))) '(1 1))
  (check-true (arith? '(frac (frac "1" "2") "3*x")))
  (check-false (arith? '(frac "1" "")))
  (check-false (arith? '(concat "1+" (big "sum"))))
  (check= (arith-end '(concat "1+" (big "sum"))) '(0 1))
  (check-false (arith? '(concat "a" (rsub "i"))))
  (check= (arith-end '(concat "a" (rsub "i"))) '(0 1))
  ;; the end of a string inside a tag
  (check= (arith-end '(frac "1+" "2")) '(-1))
  (check= (arith-end '(concat "1+2" (frac "1+" "2"))) '(0 3))
  ;; spaces, labels and other invisible markup are left out
  (check-true (arith? '(concat "1" (hspace "1em") "+2")))
  (check-true (arith? '(concat "1" (label "x") "+2")))
  (check-true (arith? '(concat "1" (hidden "zz") "+2")))
  ;; each line of a document ends with a newline
  (check-false (arith? '(document "1" "2")))
  (check= (arith-end '(document "1" "2")) '(0 1))
  (check-true (ok? "std-math" "Main" '(document "a+b")))
  (check-false (ok? "std-math" "Main" '(document "a+")))
  (check-true (ok? "std-math" "Strict" '(document "a,b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; packrat-context lists the selectable structures around a position, from
;; the innermost one, as (rule start end) with paths in the input. A
;; structure is kept when it is the only one with its extent, or the
;; innermost one. Only the right recursive grammar is used here:
;; FIXME: a context inside a left recursive rule has a garbage name
;; (packrat_parser.cpp:792 takes the label of the compound (symbol "Sum")
;; inside (partial (symbol "Sum"))): (packrat-context "std-math" "Main"
;; (stree->tree "a+b*c") '(2)) gives a first entry named by random bytes,
;; expected Sum (or a name for the partial sum), with (1) (5).
(define (test-context)
  (check= (context "a+b*c" '(0)) '())
  (check= (context "a+b*c" '(1)) '((Sum (0) (5))))
  (check= (context "a+b*c" '(2)) '((Sum (0) (5))))
  (check= (context "a+b*c" '(3)) '((Prod (2) (5)) (Sum (0) (5))))
  (check= (context "a+b*c" '(4)) '((Prod (2) (5)) (Sum (0) (5))))
  (check= (context "a+b*c" '(5)) '())
  (check= (context "(ab)" '(2)) '((Atom (1) (3)) (Atom (0) (4))))
  (check= (context '(concat "a+" (with "color" "red" "b")) '(1 2 0))
          '((Sum (0 0) (1 2 1))))
  ;; incorrect inputs and invalid positions
  (check= (context "a+" '(1)) '())
  (check= (context "ab" '(7)) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inheritance and fallbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-inherit)
  (check-true (ok? "parse-test-base" "Main" "abc"))
  (check-false (ok? "parse-test-base" "Main" "abd"))
  ;; the derived grammar has the rules of the base and its own ones
  (check-true (ok? "parse-test-derived" "Main" "abd"))
  (check-true (ok? "parse-test-derived" "Main" "edcba"))
  (check-true (ok? "parse-test-derived" "Pair" "ae"))
  (check-false (ok? "parse-test-derived" "Pair" "aef"))
  (check-false (ok? "parse-test-base" "Pair" "ab"))
  ;; std-math inherits the symbols and the operators
  (check-true (ok? "std-math" "Relation-symbol" "<leq>"))
  (check-true (ok? "std-symbols" "Relation-symbol" "<leq>"))
  (check-false (ok? "std-symbols" "Main" "a")))

;; An unknown language is an empty grammar (find_packrat_grammar), where
;; every rule fails.
(define (test-fallbacks)
  (check-false (ok? "parse-test-empty" "Main" ""))
  (check-false (ok? "parse-test-empty" "Main" "a"))
  (check= (end "parse-test-empty" "Main" "a") '(-1))
  (check= (packrat-context "parse-test-empty" "Main" (stree->tree "a") '(0))
          '())
  (check-false (ok? "std-math" "No-such-rule" "a")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The grammar of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-math-correct)
  (for (x (list "a" "12" "1.5" "1." ".5" "ab" "<alpha>+<beta>" "<infty>"
                "a+b" "a-b-c" "+a" "-a" "--a" "a+b*c" "a*b" "a/b/c" "a^b"
                "a<cdot>b<cdot>c" "a<times>b" "a!" "a!!" "a=b" "a=b=c"
                "a<less>b" "a<leq>b<less>c" "a+b=c+d" "a:=b" "a:=b:=c"
                "a,b" "a;b" "a,b;c" "f(x)" "sin x" "a b"
                "<forall>x,x=x" "<exists>x:x<gtr>0" "<forall>x<exists>y,x=y"
                "<neg>a" "<neg><neg>a" "a<wedge>b<vee>c" "a<Rightarrow>b"
                "a<Rightarrow>b<Rightarrow>c" "x<in>A<cup>B" "a<cup>b<cap>c"
                "a." "a," "="
                '(concat "x" (rsup "2"))
                '(concat "x" (rsub "i") (rsup "2"))
                '(concat "x" (rprime "'"))
                '(sqrt "x") '(sqrt "x" "3")
                '(frac "a" "b") '(frac "a" "")
                '(concat (big "sum") (rsub "i") "a")
                '(around "(" "a,b" ")") '(around "[" "" ")")
                '(around* "(" "a" ")")
                '(concat "f" (around "(" "x" ")"))
                '(table (row (cell "a") (cell "b")))
                '(concat "a" (wide "b" "<bar>"))
                '(concat "a" (mid "|") (rsub "x"))
                '(with "color" "red" "a+b")))
    (check-true (math? x))))

(define (test-math-incorrect)
  (for (x (list "" "a+" "a-+b" "(a" "a)" "a  b" "2x" "x'" "a.b"
                "a<nosymbol>b" "a^b^c"
                '(concat "x" (rsup ""))
                '(frac "" "b")
                '(concat "a" (lsub "i"))
                '(around "(" "a+" ")")
                '(concat "a+" (text "foo"))
                '(concat "a+" (with "mode" "text" "b"))))
    (check-false (math? x)))
  ;; where the parse stops
  (check= (end "std-math" "Main" "a+") '(1))
  (check= (end "std-math" "Main" "a-+b") '(1))
  (check= (end "std-math" "Main" "(a") '(-1))
  (check= (end "std-math" "Main" "a)") '(1))
  (check= (end "std-math" "Main" "2x") '(1))
  (check= (end "std-math" "Main" "a<less>b") '(8))
  (check= (end "std-math" "Main" '(concat "x" (rsup ""))) '(0 1)))

;; The rules of std-math are its precedence levels: each one parses the
;; operators of its level and above, not those below.
(define (test-math-precedence)
  (check-true (math-rule? "Identifier" "ab"))
  (check-false (math-rule? "Identifier" "a1"))
  (check-true (math-rule? "Number" "1.5"))
  (check-false (math-rule? "Number" "a"))
  (check-true (math-rule? "Radical" "a"))
  (check-false (math-rule? "Radical" "-a"))
  (check-true (math-rule? "Prefixed" "ab"))
  (check-false (math-rule? "Prefixed" "a^b"))
  (check-true (math-rule? "Power" "a^b"))
  (check-false (math-rule? "Power" "a*b"))
  (check-true (math-rule? "Product" "a*b"))
  (check-true (math-rule? "Product" "a^b"))
  (check-false (math-rule? "Product" "a+b"))
  (check-false (math-rule? "Product" "-a"))
  (check-true (math-rule? "Sum" "a+b"))
  (check-true (math-rule? "Sum" "-a"))
  (check-false (math-rule? "Sum" "a=b"))
  (check-true (math-rule? "Relation" "a=b"))
  (check-true (math-rule? "Relation" "a+b"))
  (check-false (math-rule? "Relation" "a,b"))
  (check-true (math-rule? "Expression" "a=b"))
  (check-false (math-rule? "Expression" "a,b"))
  (check-true (math-rule? "Strict" "a,b"))
  (check-false (math-rule? "Strict" "a."))
  (check-true (math-rule? "Main" "a."))
  ;; the power is not associative: a^b^c needs a script
  (check-false (math-rule? "Power" "a^b^c"))
  (check-true (math? '(concat "a" (rsup (concat "b" (rsup "c")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols and their classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The typesetter takes the class of a mathematical symbol, and with it the
;; spacing and the breaking penalties, from the rules of std-symbols with a
;; :type (math_language.cpp).
(define (test-symbols)
  (for (x '(("+" "Plus-visible-symbol" "prefix-infix")
            ("-" "Minus-symbol" "prefix-infix")
            ("<noplus>" "Plus-invisible-symbol" "prefix-infix")
            ("<upl>" "Plus-prefix-symbol" "prefix")
            ("<um>" "Minus-prefix-symbol" "prefix")
            ("<cup>" "Union-symbol" "infix")
            ("<cap>" "Intersection-symbol" "infix")
            ("<setminus>" "Exclude-symbol" "infix")
            ("<times>" "Times-visible-symbol" "infix")
            ("*" "Times-invisible-symbol" "infix")
            ("/" "Over-condensed-symbol" "infix")
            ("^" "Power-symbol" "infix")
            ("_" "Index-symbol" "infix")
            ("<leq>" "Relation-nolim-symbol" "infix")
            ("<Rightarrow>" "Imply-nolim-symbol" "infix")
            ("<rightarrow>" "Arrow-nolim-symbol" "infix")
            ("<assign>" "Assign-symbol" "infix")
            ("<models>" "Models-symbol" "prefix-infix")
            ("<vee>" "Or-symbol" "infix")
            ("<wedge>" "And-symbol" "infix")
            ("<neg>" "Not-symbol" "prefix")
            ("<forall>" "Quantifier-symbol" "prefix")
            ("<exists>" "Quantifier-symbol" "prefix")
            ("<big-sum>" "Big-lim-symbol" "prefix")
            ("<big-int>" "Big-nolim-symbol" "prefix")
            ("#" "Other-prefix-symbol" "prefix")
            ("!" "Other-postfix-symbol" "postfix")
            ("'" "Prime-symbol" "symbol")
            ("," "Ponctuation-visible-symbol" "separator")
            (";" "Ponctuation-visible-symbol" "separator")
            ("<nocomma>" "Ponctuation-invisible-symbol" "separator")
            ("(" "Open-symbol" "opening-bracket")
            ("[" "Open-symbol" "opening-bracket")
            ("]" "Close-symbol" "closing-bracket")
            (")" "Close-symbol" "closing-bracket")
            ("|" "Middle-bracket-symbol" "middle-bracket")
            ("<suchthat>" "Middle-separator-symbol" "middle-bracket")
            ("<ldots>" "Suspension-nolim-symbol" "symbol")
            ("<partial>" "Unary-operator-glyph-symbol" "unary")
            ("<sum>" "Unary-operator-glyph-symbol" "unary")
            ("<infty>" "Miscellaneous-symbol" "symbol")
            ("<alpha>" "Letter-symbol" "symbol")
            ("<space>" "Spacing-wide-symbol" "infix")
            ("<nospace>" "Spacing-invisible-symbol" "infix")))
    (check= (list (car x) (math-symbol-group (car x)) (math-symbol-type (car x)))
            x))
  ;; a letter, a number, an unknown word or symbol is a plain symbol
  (check= (math-symbol-group "a") "symbol")
  (check= (math-symbol-type "a") "symbol")
  (check= (math-symbol-group "1") "symbol")
  (check= (math-symbol-group "foo") "symbol")
  (check= (math-symbol-type "<nosuch>") "symbol")
  ;; the members of a class
  (check= (sort (math-group-members "Ponctuation-visible-symbol") string<?)
          '("," ":" ";" "<mid>" "<point>"))
  (check= (math-group-members "Plus-prefix-symbol") '("<upl>"))
  (check= (math-group-members "Not-symbol") '("<neg>"))
  (check= (sort (math-group-members "Quantifier-symbol") string<?)
          '("<Exists>" "<exists>" "<forall>" "<nexists>"))
  (check= (math-group-members "Nope") '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading the colors of typeset code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-dir (url-append (url-temp-dir) "parse-test"))
(define eps-file (url-append parse-dir "code.eps"))

(define (hex2 x)
  ;; the color component @x, printed with 3 decimals, as two hex digits
  (let* ((n (inexact->exact (round (* x 255))))
         (s (number->string n 16)))
    (if (< n 16) (string-append "0" s) s)))

(define (eps-colors s)
  ;; the table Cn -> "#rrggbb" of the colors defined in the PostScript @s
  (let ((t (make-ahash-table)))
    (for (l (string-decompose s "\n"))
      (when (and (string-starts? l "/C") (string-contains? l "setrgbcolor"))
        (let* ((sp (string-search-forwards " {" 0 l))
               (nums (string-decompose
                      (substring l (+ sp 2) (string-length l)) " ")))
          (ahash-set! t (substring l 1 sp)
                      (string-append "#" (hex2 (string->number (car nums)))
                                     (hex2 (string->number (cadr nums)))
                                     (hex2 (string->number (caddr nums))))))))
    t))

(define (ps-string s i)
  ;; the PostScript string which starts after the ( at @i of @s, and the
  ;; index after its )
  (let loop ((i i) (acc '()))
    (let ((c (string-ref s i)))
      (cond ((== c #\)) (cons (list->string (reverse acc)) (+ i 1)))
            ((== c #\\)
             (let ((d (string-ref s (+ i 1))))
               (if (char-numeric? d)
                   (loop (+ i 4)
                         (cons (integer->char
                                (string->number (substring s (+ i 1) (+ i 4)) 8))
                               acc))
                   (loop (+ i 2) (cons d acc)))))
            (else (loop (+ i 1) (cons c acc)))))))

(define (eps-glyphs s)
  ;; the characters drawn on the page of the PostScript @s, as a list of
  ;; (char . color)
  (let* ((cols (eps-colors s))
         (b (string-search-forwards "%%Page: 1 1" 0 s))
         (e (string-search-forwards "%%Trailer" b s))
         (col "#000000"))
    (let loop ((i b) (acc '()))
      (if (>= i e) (reverse acc)
          (let ((c (string-ref s i)))
            (cond ((== c #\()
                   (let ((r (ps-string s (+ i 1))))
                     (loop (cdr r)
                           (append (reverse (map (lambda (ch) (cons ch col))
                                                 (string->list (car r))))
                                   acc))))
                  ((char-whitespace? c) (loop (+ i 1) acc))
                  (else
                   (let tok ((j i))
                     (if (and (< j e)
                              (not (char-whitespace? (string-ref s j)))
                              (not (== (string-ref s j) #\()))
                         (tok (+ j 1))
                         (let ((w (substring s i j)))
                           (when (ahash-ref cols w)
                             (set! col (ahash-ref cols w)))
                           (loop j acc)))))))))))

(define (line-runs l glyphs)
  ;; the runs of the line @l, taking its glyphs from the front of @glyphs;
  ;; returns the runs and the remaining glyphs. A glyph which is not the
  ;; character of the source is shown as ?
  (let ((acc '()) (cur #f) (col #f))
    (define (flush)
      (when cur (set! acc (cons (list (list->string (reverse cur)) col) acc)))
      (set! cur #f))
    (for (c (string->list l))
      (if (== c #\space) (flush)
          (let ((g (if (null? glyphs) (cons #\? "none") (car glyphs))))
            (when (pair? glyphs) (set! glyphs (cdr glyphs)))
            (when (and cur (!= (cdr g) col)) (flush))
            (when (not cur) (set! cur '()) (set! col (cdr g)))
            (set! cur (cons (if (== (car g) c) c #\?) cur)))))
    (flush)
    (cons (reverse acc) glyphs)))

(define (tm-line s)
  ;; the TeXmacs string of the ASCII line @s
  (apply string-append
         (map (lambda (c)
                (cond ((== c #\<) "<less>")
                      ((== c #\>) "<gtr>")
                      (else (string c))))
              (string->list s))))

(define (highlight lan lines)
  ;; the runs of each of the @lines of code in the language @lan
  (let* ((old (current-buffer))
         (before (buffer-list))
         (u (new-buffer)))
    (switch-to-buffer u)
    (buffer-set-body u (stree->tree
                        `(document (with "mode" "prog" "prog-language" ,lan
                                     (document ,@(map tm-line lines))))))
    (update-forced)
    (with r (check-run
             (lambda ()
               (when (url-exists? eps-file) (system-remove eps-file))
               (print-snippet eps-file (tree-ref (buffer-tree) 0) #f)
               (let loop ((ls lines)
                          ;; a token with a space (as "abstract type"
                          ;; in julia) has a glyph for it
                          (gs (list-filter
                               (eps-glyphs (string-load eps-file))
                               (lambda (g) (!= (car g) #\space))))
                          (acc '()))
                 (if (null? ls) (reverse acc)
                     (with p (line-runs (car ls) gs)
                       (loop (cdr ls) (cdr p) (cons (car p) acc)))))))
      (for (b (buffer-list))
        (when (nin? b before) (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer old))
      r)))

(define-macro (check-lines lan . l)
  ;; @l is a list of lines and their expected runs
  `(let ((r (highlight ,lan (list ,@(map car l)))))
     (for-each (lambda (line expected got)
                 (check-equal (string-append ,lan ": " line)
                              (lambda () got) expected))
               (list ,@(map car l))
               (list ,@(map cadr l))
               (if (and (list? r) (== (length r) ,(length l))) r
                   (map (lambda (x) r) (list ,@(map car l)))))))

;; the colors of the environment (Typeset/Env/env_default.cpp), which the
;; highlighters of cpp, scheme, mathemagix and r use
(define black "#000000")
(define env-keyword "#8020c0")
(define env-constant "#2060c0")
(define env-string "#a06040")
(define env-comment "#802000")
(define env-preprocessor "#400040")
(define env-declaration "#0000e0")
(define env-type "#008000")
(define env-defined "#204080")
(define env-alt-keyword "#309090")
(define env-alt-constant "#800080")

;; the colors of the classes of prog_language_rep (language.cpp,
;; initialize_color_decodings, and the preferences of the *-lang.scm)
(define c-constant "#4040c0")
(define c-number "#3030b0")
(define c-string "#707070")
(define c-char "#333333")
(define c-declare "#0000c0")
(define c-operator "#8b008b")
(define c-openclose "#b02020")
(define c-field "#888888")
(define c-special "#ff8000")
(define c-keyword "#309090")
(define c-control "#000080")
(define c-comment env-comment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting by the C++ highlighters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cpp_language.cpp: keywords, constants, basic types, numbers, strings,
;; comments and preprocessor lines; operators and identifiers keep the
;; color of the text. A multi-line comment ends where it is closed, and a
;; later comment of the document does not reach back into the line.
(define (test-cpp)
  (check-lines "cpp"
    ("/* a"
     `(("/*" ,env-comment) ("a" ,env-comment)))
    ("b */ x"
     `(("b" ,env-comment) ("*/" ,env-comment) ("x" ,black))))
  (check-lines "cpp"
    ("a /* b */ c"
     `(("a" ,black) ("/*" ,env-comment) ("b" ,env-comment)
       ("*/" ,env-comment) ("c" ,black)))
    ("d"
     `(("d" ,black)))
    ("/* e */"
     `(("/*" ,env-comment) ("e" ,env-comment) ("*/" ,env-comment))))
  (check-lines "cpp"
    ("#include <stdio.h>"
     `(("#include" ,env-preprocessor) ("<stdio.h>" ,env-preprocessor)))
    ("int main (void) {"
     `(("int" ,env-defined) ("main" ,black) ("(" ,black) ("void" ,env-defined)
       (")" ,black) ("{" ,black)))
    ("  return 0; // done"
     `(("return" ,env-keyword) ("0" ,env-constant) (";" ,black)
       ("//" ,env-comment) ("done" ,env-comment)))
    ("  if (a <= 1.5e3) cout << true;"
     `(("if" ,env-keyword) ("(a" ,black) ("<=" ,black) ("1.5e3" ,env-constant)
       (")" ,black) ("cout" ,env-constant) ("<<" ,black) ("true" ,env-constant)
       (";" ,black)))
    ("  x = \"str\"; unsigned y;"
     `(("x" ,black) ("=" ,black) ("\"str\"" ,env-string) (";" ,black)
       ("unsigned" ,env-defined) ("y;" ,black)))
    ("  /* a"
     `(("/*" ,env-comment) ("a" ,env-comment)))
    ("  b */"
     `(("b" ,env-comment) ("*/" ,env-comment)))
    ("}"
     `(("}" ,black)))))

;; scheme_language.cpp: the keywords of tm-keywords (highlight-any) in the
;; alternative keyword color, defined symbols, numbers, strings and
;; characters, keywords :x, comments.
(define (test-scheme)
  (check-lines "scheme"
    ("(define (f x) ; c"
     `(("(" ,black) ("define" ,env-alt-keyword) ("(f" ,black) ("x)" ,black)
       (";" ,env-comment) ("c" ,env-comment)))
    ("  (if (> x 1) \"s\" #t :key 'q 12))"
     `(("(" ,black) ("if" ,env-alt-keyword) ("(" ,black) (">" ,env-defined)
       ("x" ,black) ("1" ,env-constant) (")" ,black) ("\"s\"" ,env-string)
       ("#t" ,env-string) (":key" ,env-alt-constant) ("'q" ,black)
       ("12" ,env-constant) ("))" ,black)))
    ("(f \"a;b\" c) ; d"
     `(("(f" ,black) ("\"a;b\"" ,env-string) ("c)" ,black)
       (";" ,env-comment) ("d" ,env-comment)))
    ("(g #\\( x)"
     `(("(g" ,black) ("#\\(" ,env-string) ("x)" ,black)))))

;; mathemagix_language.cpp: declarations, types, keywords, numbers,
;; strings and comments.
(define (test-mathemagix)
  (check-lines "mathemagix"
    ("f (x: Int): Int == { if x = 0 then return 1; } // c"
     `(("f" ,env-declaration) ("(x:" ,black) ("Int" ,env-type) ("):" ,black)
       ("Int" ,env-type) ("==" ,black) ("{" ,black) ("if" ,env-keyword)
       ("x" ,black) ("=" ,black) ("0" ,env-constant) ("then" ,env-keyword)
       ("return" ,env-keyword) ("1" ,env-constant) (";" ,black) ("}" ,black)
       ("//" ,env-comment) ("c" ,env-comment)))
    ("  forall (T: Type) class C == { mutable s: String := \"a\"; }"
     `(("forall" ,env-keyword) ("(T:" ,black) ("Type" ,env-type) (")" ,black)
       ("class" ,env-keyword) ("C" ,env-declaration) ("==" ,black) ("{" ,black)
       ("mutable" ,env-keyword) ("s:" ,black) ("String" ,env-type)
       (":=" ,black) ("\"a\"" ,env-string) (";" ,black) ("}" ,black)))))

;; fortran_language.cpp, colored by classes as prog_language_rep
(define (test-fortran)
  (check-lines "fortran"
    ("program p ! c"
     `(("program" ,c-declare) ("p" ,black) ("!" ,c-comment) ("c" ,c-comment)))
    ("  integer :: i = 10"
     `(("integer" "#00c000") ("::" ,c-special) ("i" ,black) ("=" ,c-operator)
       ("10" ,c-number)))
    ("  if (i .eq. 1) then"
     `(("if" ,c-keyword) ("(" ,c-openclose) ("i" ,black) (".eq." ,black)
       ("1" ,c-number) (")" ,c-openclose) ("then" ,c-keyword)))
    ("    print *, 'hi', 1.5e3"
     `(("print" ,c-constant) ("*" ,c-operator) ("," ,black) ("'hi'" ,c-string)
       ("," ,black) ("1.5e3" ,c-number)))
    ("  end if"
     `(("end" ,c-declare) ("if" ,c-keyword)))
    ("end program"
     `(("end" ,c-declare) ("program" ,c-declare)))))

;; r_language.cpp: keywords, constants, numbers and strings in the colors
;; of the environment, operators in red, assignments in dark green, index
;; brackets in dark blue, comments in brown.
(define (test-r)
  (check-lines "r"
    ("f = function(x) { if (x > 1) TRUE else NULL }"
     `(("f" ,black) ("=" ,env-type) ("function" ,env-keyword) ("(" "#000080")
       ("x" ,black) (")" "#000080") ("{" ,black) ("if" ,env-keyword)
       ("(" "#000080") ("x" ,black) (">" "#ff0000") ("1" ,env-constant)
       (")" "#000080") ("TRUE" ,env-constant) ("else" ,env-keyword)
       ("NULL" ,env-constant) ("}" ,black)))
    ("x[1] == \"s\""
     `(("x" ,black) ("[" "#000080") ("1" ,env-constant) ("]" "#000080")
       ("==" "#ff0000") ("\"s\"" ,env-string)))
    ("x = 1 # c"
     `(("x" ,black) ("=" ,env-type) ("1" ,env-constant) ("#" "#802000")
       ("c" "#802000")))
    ("x <- 1"
     `(("x" ,black) ("<-" ,env-type) ("1" ,env-constant)))
    ("y <- \"#\" -> z"
     `(("y" ,black) ("<-" ,env-type) ("\"#\"" ,env-string) ("->" ,env-type)
       ("z" ,black)))))

;; scilab_language.cpp
(define (test-scilab)
  (check-lines "scilab"
    ("function y = f(x) // c"
     `(("function" ,c-declare) ("y" ,black) ("=" ,c-operator) ("f" ,black)
       ("(" ,c-openclose) ("x" ,black) (")" ,c-openclose) ("//" ,c-comment)
       ("c" ,c-comment)))
    ("  if x == 1 then y = %pi; end"
     `(("if" ,c-keyword) ("x" ,black) ("==" ,c-operator) ("1" ,c-number)
       ("then" ,c-keyword) ("y" ,black) ("=" ,c-operator) ("%pi" ,c-constant)
       (";" ,black) ("end" ,c-keyword)))
    ("  disp(\"s\")"
     `(("disp" ,black) ("(" ,c-openclose) ("\"s\"" ,c-string)
       (")" ,c-openclose)))
    ("endfunction"
     `(("endfunction" ,c-declare)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting configured by the *-lang.scm tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only the languages whose comment feature has multi_line, as java, color
;; /* ... */ as a comment (python has no such comments).
;; FIXME: the class operator_decoration of the tables (@ in python, java,
;; scala, julia, $ in julia) has no color (language.cpp:221, the encoding
;; of the classes, has no operator_decoration), so that it gets the color
;; of syntax:<lan>:none: in the python line "@deco", @ is red, expected a
;; color of the decorations.
;; FIXME: syntax:python:operator_field is "#88888" (python-lang.scm:119,
;; five digits), which is no color: in "a.b", the dot has the color of the
;; text, expected #888888 as in java and scala.
;; FIXME: the keyword parser reads a word of letters only (read_word,
;; analyze.cpp:1056, used by keyword_parser.cpp:25): in python, __debug__,
;; __import__ and raw_input have the color of the text, expected the color
;; of constants; and if_x shows if as a keyword.
;; FIXME: the operators which are words ("and" "not" "or" of python) are
;; parsed before the keywords and identifiers, also inside a word
;; (prog_language.cpp:211): in python, ord is or (operator) then d,
;; expected ord as a constant.
(define (test-python)
  (check-lines "python"
    ("import os"
     `(("import" ,c-declare) ("os" ,black)))
    ("def f(x, y=1):"
     `(("def" ,c-declare) ("f" ,black) ("(" ,c-openclose) ("x," ,black)
       ("y" ,black) ("=" ,c-operator) ("1" ,c-constant) (")" ,c-openclose)
       (":" ,c-special)))
    ("    if x is None: return 0x1F + 2j  # c"
     `(("if" ,c-keyword) ("x" ,black) ("is" ,c-keyword) ("None" ,c-constant)
       (":" ,c-special) ("return" ,c-keyword) ("0x1F" ,c-constant)
       ("+" ,c-operator) ("2j" ,c-constant) ("#" ,c-comment) ("c" ,c-comment)))
    ("    s = 'a\\nb' + \"x\""
     `(("s" ,black) ("=" ,c-operator) ("'a" ,c-string) ("\\n" ,c-char)
       ("b'" ,c-string) ("+" ,c-operator) ("\"x\"" ,c-string)))
    ("    return [a for a in xs]"
     `(("return" ,c-keyword) ("[" ,c-openclose) ("a" ,black) ("for" ,c-keyword)
       ("a" ,black) ("in" ,c-keyword) ("xs" ,black) ("]" ,c-openclose)))
    ("n = 1_000 + 0b101 + 1e-3"
     `(("n" ,black) ("=" ,c-operator) ("1_000" ,c-constant) ("+" ,c-operator)
       ("0b101" ,c-constant) ("+" ,c-operator) ("1e-3" ,c-constant)))
    ("\"\"\"doc\"\"\""
     `(("\"\"\"doc\"\"\"" ,c-string)))
    ("a /* b */ c"
     `(("a" ,black) ("/*" ,c-operator) ("b" ,black) ("*/" ,c-operator)
       ("c" ,black)))))

(define (test-java)
  (check-lines "java"
    ("package p;"
     `(("package" ,c-declare) ("p" ,black) (";" ,c-operator)))
    ("public class A extends B {"
     `(("public" ,c-keyword) ("class" ,c-declare) ("A" ,black)
       ("extends" ,c-keyword) ("B" ,black) ("{" ,c-openclose)))
    ("  int f() { return 0x1F + 1L; } // c"
     `(("int" ,c-constant) ("f" ,black) ("()" ,c-openclose) ("{" ,c-openclose)
       ("return" ,c-control) ("0x1F" ,c-number) ("+" ,c-operator)
       ("1L" ,c-number) (";" ,c-operator) ("}" ,c-openclose) ("//" ,c-comment)
       ("c" ,c-comment)))
    ("  String s = \"a\\n\"; char c = 'x';"
     `(("String" ,black) ("s" ,black) ("=" ,c-operator) ("\"a" ,c-string)
       ("\\n" ,c-char) ("\"" ,c-string) (";" ,c-operator) ("char" ,c-constant)
       ("c" ,black) ("=" ,c-operator) ("'x'" ,c-string) (";" ,c-operator)))
    ("  if (x != null) throw new E(); a.b"
     `(("if" ,c-keyword) ("(" ,c-openclose) ("x" ,black) ("!=" ,c-operator)
       ("null" ,c-constant) (")" ,c-openclose) ("throw" ,c-control)
       ("new" ,c-keyword) ("E" ,black) ("()" ,c-openclose) (";" ,c-operator)
       ("a" ,black) ("." ,c-field) ("b" ,black)))
    ("  x /* c */ y"
     `(("x" ,black) ("/*" ,c-comment) ("c" ,c-comment) ("*/" ,c-comment)
       ("y" ,black)))))

(define (test-scala)
  (check-lines "scala"
    ("object A extends App {"
     `(("object" ,c-declare) ("A" ,black) ("extends" ,c-keyword) ("App" ,black)
       ("{" ,c-openclose)))
    ("  val x: Int = 1 :: Nil"
     `(("val" ,c-declare) ("x" ,black) (":" ,c-special) ("Int" ,c-constant)
       ("=" ,c-operator) ("1" ,c-number) ("::" ,c-special) ("Nil" ,black)))
    ("  def f(y: Int) = y map (_ + 1) // c"
     `(("def" ,c-declare) ("f" ,black) ("(" ,c-openclose) ("y" ,black)
       (":" ,c-special) ("Int" ,c-constant) (")" ,c-openclose) ("=" ,c-operator)
       ("y" ,black) ("map" ,c-constant) ("(" ,c-openclose) ("_" ,black)
       ("+" ,c-operator) ("1" ,c-number) (")" ,c-openclose) ("//" ,c-comment)
       ("c" ,c-comment)))
    ("  case class B(s: String = \"q\")"
     `(("case" ,c-keyword) ("class" ,c-declare) ("B" ,black) ("(" ,c-openclose)
       ("s" ,black) (":" ,c-special) ("String" ,c-constant) ("=" ,c-operator)
       ("\"q\"" ,c-string) (")" ,c-openclose)))
    ("}"
     `(("}" ,c-openclose)))))

;; FIXME: syntax:julia:declare_module and declare_type are "0000c0"
;; (julia-lang.scm:131-132, without #), which is no color: in julia,
;; import and struct have the color of the text, expected #0000c0.
(define (test-julia)
  (check-lines "julia"
    ("function f(x)"
     `(("function" ,c-declare) ("f" ,black) ("(" ,c-openclose) ("x" ,black)
       (")" ,c-openclose)))
    ("  if x > 0 return 1im end  # c"
     `(("if" ,c-keyword) ("x" ,black) (">" ,c-operator) ("0" ,c-constant)
       ("return" ,c-keyword) ("1im" ,c-constant) ("end" ,c-keyword)
       ("#" ,c-comment) ("c" ,c-comment)))
    ("  s = \"s\" ; true"
     `(("s" ,black) ("=" ,black) ("\"s\"" ,c-string) (";" ,black)
       ("true" ,c-constant)))
    ("end"
     `(("end" ,c-keyword)))))

;; The declarations of several words of julia ("abstract type", "mutable
;; struct", "primitive type") are keywords, but not their words alone. They
;; have the color of declare_type, that of struct (see the FIXME above).
(define (test-julia-declarations)
  (let* ((r (highlight "julia" '("struct" "abstract type T end"
                                 "mutable  struct S; mutable = 1; types")))
         (line (lambda (i) (and (list? r) (> (length r) i) (list-ref r i))))
         (c (and (pair? (line 0)) (cadr (car (line 0))))))
    (check= (line 1)
            `(("abstract" ,c) ("type" ,c) ("T" ,black) ("end" ,c-keyword)))
    (check= (line 2)
            `(("mutable" ,black) ("struct" ,c) ("S;" ,black) ("mutable" ,black)
              ("=" ,black) ("1" ,c-constant) (";" ,black) ("types" ,black)))))

;; FIXME: the number format of json is defined for javascript
;; (json-lang.scm:29 requires (== lan "javascript")): json numbers have no
;; exponent, "1e3" shows 1 as a number and e3 in the color of the text,
;; expected 1e3 as a number; and once json-lang is loaded, (parser-feature
;; "javascript" "number") gives (number (bool_features "sci_notation")),
;; expected the prefixes 0x 0b 0o of javascript-lang.scm as well.
(define (test-json-csv)
  (check-lines "json"
    ("{\"a\": [1, -2.5, true, null], \"b\": \"x\\\"y\"}"
     `(("{" ,c-openclose) ("\"a\"" ,c-string) (":" ,c-operator)
       ("[" ,c-openclose) ("1" ,c-number) ("," ,c-operator) ("-" ,c-operator)
       ("2.5" ,c-number) ("," ,c-operator) ("true" ,c-constant)
       ("," ,c-operator) ("null" ,c-constant) ("]" ,c-openclose)
       ("," ,c-operator) ("\"b\"" ,c-string) (":" ,c-operator)
       ("\"x" ,c-string) ("\\\"" ,c-char) ("y\"" ,c-string)
       ("}" ,c-openclose))))
  (check-lines "csv"
    ("a,b,\"c,d\",1"
     `(("a" ,black) ("," ,c-operator) ("b" ,black) ("," ,c-operator)
       ("\"c,d\"" ,c-string) ("," ,c-operator) ("1" ,c-number)))))

;; Languages without a highlighter keep the color of the text.
;; FIXME: javascript, dot and octave have complete tables
;; (javascript-lang.scm, dot-lang.scm, octave-lang.scm) and code
;; environments (javascript-code, dot-code, octave-code in
;; env-program.ts), but no format, and prog_language (prog_language.cpp:304)
;; only builds a prog_language_rep for a language with a format: their code
;; is not highlighted at all, "let a = null;" in javascript is all in the
;; color of the text, expected let #0000c0, = #8b008b, null #4040c0.
;; FIXME: packrat highlighting does not reach the document: the
;; highlighting of a verb_language (minimal, or any define-language with
;; :highlight properties) is attached to the copy of the input that
;; make_packrat_parser keeps (packrat_parser.cpp:46, last_in= copy (in)),
;; not to the typeset trees: the minimal line "x == 1;" is all in the color
;; of the text, expected x in the color of declarations (Lhs-radical has
;; :highlight declare in language/minimal.scm).
(define (test-no-highlighting)
  (check-lines "verbatim"
    ("int x = 1; // c"
     `(("int" ,black) ("x" ,black) ("=" ,black) ("1;" ,black) ("//" ,black)
       ("c" ,black))))
  (check-lines "shell"
    ("echo $HOME # c"
     `(("echo" ,black) ("$HOME" ,black) ("#" ,black) ("c" ,black)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The tables of the languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (feature lan key)
  (parser-feature lan key))

(define (group lan key g)
  (with r (assq g (cdr (feature lan key)))
    (and r (cdr r))))

(define (test-tables)
  ;; every table module loads, and its features are lists headed by the key
  (for (m '((python-lang) (java-lang) (scala-lang) (julia-lang) (json-lang)
            (csv-lang) (cpp-lang) (dot-lang) (javascript-lang) (octave-lang)))
    (check= (begin (module-provide m) (module-available? m)) #t))
  (for (lan '("python" "java" "scala" "julia" "json" "csv" "cpp" "dot"
              "javascript" "octave"))
    (for (key '("keyword" "operator" "number" "string" "comment"))
      (check= (car (feature lan key)) (string->symbol key))))
  ;; the default comments are // and /* */, python and julia use #,
  ;; octave # and %
  (check= (feature "java" "comment")
          '(comment (inline "//") (multi_line "/*" "*/")))
  (check= (feature "python" "comment") '(comment (inline "#")))
  (check= (feature "julia" "comment") '(comment (inline "#")))
  (check= (feature "octave" "comment") '(comment (inline "#" "%")))
  ;; some classes
  (check= (group "python" "keyword" 'declare_function) '("def" "lambda"))
  (check= (group "python" "keyword" 'declare_type) '("class"))
  (check-true (in? "yield" (group "python" "keyword" 'keyword_control)))
  (check-true (in? "elif" (group "python" "keyword" 'keyword_conditional)))
  (check= (group "java" "keyword" 'declare_module) '("package" "import"))
  (check-true (in? "null" (group "java" "keyword" 'constant)))
  (check= (group "scala" "keyword" 'declare_identifier) '("val" "var"))
  (check= (group "json" "keyword" 'constant) '("false" "true" "null"))
  (check= (group "csv" "operator" 'operator) '(","))
  (check= (group "dot" "keyword" 'declare_type)
          '("graph" "node" "edge" "digraph" "subgraph"))
  (check= (feature "cpp" "preprocessor")
          '(preprocessor
            (directives "define" "undef" "include" "if" "ifdef" "ifndef"
                        "else" "elif" "endif" "line" "error" "pragma")))
  (check= (feature "python" "number")
          '(number (bool_features "prefix_0x" "prefix_0b" "prefix_0o"
                                  "no_suffix_with_box" "sci_notation")
                   (suffix (imaginary "j" "J"))
                   (separator "_")))
  ;; a language without a table has the empty features of default-lang
  (check= (feature "parse-test-nolang" "keyword") '(keyword))
  (check= (feature "parse-test-nolang" "comment")
          '(comment (inline "//") (multi_line "/*" "*/"))))

;; Each word of the tables of a prog_language_rep has the color of its
;; class: all the words of a language are typeset, one per line, and the
;; words of each class which do not get its color are listed.
(define color-defaults
  ;; the C++ defaults (language.cpp, initialize_color_decodings)
  '(("constant" . "#4040c0") ("constant_type" . "#4040c0")
    ("declare_function" . "#0000c0") ("declare_type" . "#0000c0")
    ("declare_module" . "#0000c0") ("declare_identifier" . "#0000c0")
    ("operator" . "#8b008b") ("operator_openclose" . "#B02020")
    ("operator_field" . "#888888") ("operator_special" . "orange")
    ("keyword" . "#309090") ("keyword_conditional" . "#309090")
    ("keyword_control" . "#000080")))

(define (class-color lan g)
  (let ((p (get-preference (string-append "syntax:" lan ":" g))))
    (string-downcase
     (get-hex-color (if (!= p "default") p
                        (or (assoc-ref color-defaults g) "red"))))))

(define (ascii-word? w)
  (and (not (string-index w #\space))
       (list-and (map (lambda (c) (< (char->integer c) 128))
                      (string->list w)))))

(define (misclassified lan key)
  ;; the list of (class word...) of the words of the table @key of @lan
  ;; which do not get the color of their class; the words of several words
  ;; and those which are not ASCII are left out
  (let* ((tab (cdr (feature lan key)))
         (words (append-map
                 (lambda (g)
                   (map (lambda (w) (cons w (symbol->string (car g))))
                        (list-filter (cdr g) ascii-word?)))
                 tab))
         (r (highlight lan (map car words))))
    (if (not (list? r)) r
        (list-filter
         (map (lambda (g)
                (let* ((gs (symbol->string (car g)))
                       (col (class-color lan gs)))
                  (cons (car g)
                        (map car
                             (list-filter
                              (map cons words r)
                              (lambda (p)
                                (and (== (cdar p) gs)
                                     (!= (cdr p)
                                         (list (list (caar p) col))))))))))
              tab)
         (lambda (x) (pair? (cdr x)))))))

(define (without l . classes)
  ;; @l without the entries of the @classes left out by a FIXME
  (list-filter l (lambda (x) (nin? (car x) classes))))

(define (test-classes)
  ;; python: see the FIXMEs before test-python
  (check= (misclassified "python" "keyword")
          '((constant ("__debug__" . "constant") ("__import__" . "constant")
                      ("ord" . "constant") ("raw_input" . "constant"))))
  (check= (without (misclassified "python" "operator")
                   'operator_decoration 'operator_field)
          '())
  (check= (misclassified "java" "keyword") '())
  (check= (without (misclassified "java" "operator") 'operator_decoration) '())
  (check= (misclassified "scala" "keyword") '())
  (check= (without (misclassified "scala" "operator") 'operator_decoration)
          '())
  ;; julia: see the FIXMEs before test-julia
  (check= (without (misclassified "julia" "keyword")
                   'declare_module 'declare_type)
          '())
  (check= (without (misclassified "julia" "operator") 'operator_decoration)
          '())
  (check= (misclassified "json" "keyword") '())
  (check= (misclassified "json" "operator") '())
  (check= (misclassified "csv" "operator") '())
  ;; the colors of the classes which work
  (check= (class-color "java" "keyword_control") "#000080")
  (check= (class-color "python" "keyword_control") "#309090")
  (check= (class-color "python" "constant_string") "#707070")
  (check= (class-color "java" "operator_special") "#ff8000"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parse-test-failures)
  (:synopsis "Run the tests of the packrat parser and of highlighting")
  (check-suite "parse")
  (when (url-exists? parse-dir) (system-rmdir-recursive parse-dir))
  (system-mkdir parse-dir)
  (run-group "definitions" test-definitions)
  (run-group "strings" test-strings)
  (run-group "rules" test-peg)
  (run-group "trees" test-trees)
  (run-group "contexts" test-context)
  (run-group "inheritance" test-inherit)
  (run-group "fallbacks" test-fallbacks)
  (run-group "correct formulas" test-math-correct)
  (run-group "incorrect formulas" test-math-incorrect)
  (run-group "precedence" test-math-precedence)
  (run-group "symbols" test-symbols)
  (run-group "highlight cpp" test-cpp)
  (run-group "highlight scheme" test-scheme)
  (run-group "highlight mathemagix" test-mathemagix)
  (run-group "highlight fortran" test-fortran)
  (run-group "highlight r" test-r)
  (run-group "highlight scilab" test-scilab)
  (run-group "highlight python" test-python)
  (run-group "highlight java" test-java)
  (run-group "highlight scala" test-scala)
  (run-group "highlight julia" test-julia)
  (run-group "highlight julia declarations" test-julia-declarations)
  (run-group "highlight json csv" test-json-csv)
  (run-group "no highlighting" test-no-highlighting)
  (run-group "language tables" test-tables)
  (run-group "classes" test-classes)
  (when (url-exists? parse-dir) (system-rmdir-recursive parse-dir))
  (check-end))
