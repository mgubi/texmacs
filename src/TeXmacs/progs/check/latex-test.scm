;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-test.scm
;; DESCRIPTION : tests of the conversion to and from LaTeX
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The LaTeX converters of convert/latex, through their registered entry
;; points (init-latex.scm):
;;
;;   - export: texmacs-stree -> latex-stree (texmacs->latex in tmtex.scm)
;;     -> latex-snippet (serialize-latex in texout.scm), and
;;     texmacs->latex-document for whole documents;
;;   - import: latex-snippet / latex-document -> texmacs-stree, through
;;     parse-latex / parse-latex-document and latex->texmacs (C++);
;;   - round trips TeXmacs -> LaTeX -> TeXmacs.
;;
;; The export options are passed explicitly (convert gives the options of
;; its caller precedence over the preferences), so that the checks do not
;; depend on the preferences of the user; no preference is set.
;;
;; The texmacs-stree -> latex-document converter is not used: it expands
;; the document in the view of its buffer (latex_expand in new_buffer.cpp)
;; and needs one; texmacs->latex-document is used instead.

(texmacs-module (check latex-test)
  (:use (check check-lib)
        (convert latex init-latex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the default values of the export options (init-latex.scm)
(define std-opts
  '(("texmacs->latex:encoding" . "ascii")
    ("texmacs->latex:use-macros" . "on")
    ("texmacs->latex:replace-style" . "on")
    ("texmacs->latex:expand-macros" . "on")
    ("texmacs->latex:indirect-bib" . "off")))

;; TeXmacs stree to a LaTeX snippet, with extra options before std-opts
(define (tl x . opts)
  (apply convert (cons* x "texmacs-stree" "latex-snippet"
                        (append opts std-opts))))

;; TeXmacs stree to the LaTeX stree of tmtex, before serialization
(define (ts x)
  (apply convert (cons* x "texmacs-stree" "latex-stree" std-opts)))

;; LaTeX snippet or document to a TeXmacs stree
(define (lt s) (convert s "latex-snippet" "texmacs-stree"))
(define (ld s) (convert s "latex-document" "texmacs-stree"))

;; a whole TeXmacs document to LaTeX
(define (td style body . init)
  (texmacs->latex-document
   `(document (TeXmacs "2.1") (style (tuple ,@style))
              ,@(if (null? init) '() `((initial (collection ,@init))))
              (body ,body))
   std-opts))

;; the body of an imported document
(define (body-of doc)
  (and (pair? doc)
       (with b (assoc 'body (cdr doc))
         (and b (cadr b)))))

;; the import of a block (an environment, a list) is a document with one
;; paragraph, where the export was given the block itself
(define (unblock t)
  (if (and (pair? t) (== (car t) 'document) (= (length t) 2)) (cadr t) t))

;; TeXmacs -> LaTeX -> TeXmacs
(define (round-trip x) (unblock (lt (tl x))))

;; the layout of the LaTeX output (indentation, line breaks) does not
;; matter in the larger cases: runs of white space become one space
(define (squash s)
  (let loop ((l (string->list s)) (acc '()) (space? #t))
    (cond ((null? l)
           (list->string (reverse (if (and (pair? acc) (== (car acc) #\space))
                                      (cdr acc) acc))))
          ((char-whitespace? (car l))
           (loop (cdr l) (if space? acc (cons #\space acc)) #t))
          (else (loop (cdr l) (cons (car l) acc) #f)))))

(define (tls x) (squash (tl x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export: text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the characters which are special in LaTeX are escaped; < and > are
;; the TeXmacs symbols <less> and <gtr>
(define (test-export-special)
  (check-group "export: special characters")
  (check= (tl "a%b&c_d#e$f{g}h~i^j\\k")
          "a\\%b\\&c\\_d\\#e\\$f\\{g\\}h\\~{}i\\^{}j\\textbackslash k")
  (check= (tl "100%") "100\\%")
  (check= (tl "<less>a<gtr>") "<a>")
  (check= (tl "a b") "a b")
  (check= (tl '(nbsp)) "~")
  ;; Cork quotes and dashes
  (check= (tl "\x10q\x11") "``q''")
  (check= (tl "\x15 \x16") "-- ---")
  (check= (tl "...") "..."))

;; accented letters are Cork bytes in TeXmacs strings; with the ascii
;; encoding they become accent commands, with utf-8 UTF-8 bytes and with
;; cork the Cork bytes themselves
(define (test-export-accents)
  (check-group "export: accented letters")
  (check= (tl "caf\xe9") "caf{\\'e}")
  (check= (tl "\xe0\xe8\xf6") "{\\`a}{\\`e}{\\\"o}")
  (check= (tl "\xc9") "{\\'E}")
  (check= (tl "\xff") "{\\ss}")
  (check= (tl "\xc0") "{\\`A}")
  (check= (tl "caf\xe9" '("texmacs->latex:encoding" . "utf-8"))
          "caf\xc3\xa9")
  (check= (tl "caf\xe9" '("texmacs->latex:encoding" . "cork"))
          "caf\xe9"))

;; content tags become TeXmacs macros (\tmem, defined in the preamble of
;; a document); font changes become \tmtext.. macros
(define (test-export-styles)
  (check-group "export: emphasis and fonts")
  (check= (tl '(em "x")) "{\\tmem{x}}")
  (check= (tl '(strong "x")) "{\\tmstrong{x}}")
  (check= (tl '(concat "a" (em "b") "c")) "a{\\tmem{b}}c")
  (check= (tl '(strong (em "x"))) "{\\tmstrong{{\\tmem{x}}}}")
  (check= (tl '(with "font-series" "bold" "x")) "\\tmtextbf{x}")
  (check= (tl '(with "font-shape" "italic" "x")) "\\tmtextit{x}")
  (check= (tl '(with "font-shape" "small-caps" "x")) "\\tmtextsc{x}")
  (check= (tl '(with "font-family" "tt" "x")) "\\tmtexttt{x}")
  (check= (tl '(with "font-family" "ss" "x")) "\\tmtextsf{x}")
  (check= (tl '(tt "x")) "{\\tmtt{x}}")
  (check= (tl '(with "color" "red" "x")) "\\tmcolor{red}{x}")
  (check= (tl '(small "x")) "{\\small{x}}")
  (check= (tl '(with "font-size" "1.41" "x")) "{\\Large x}")
  (check= (tl '(LaTeX)) "{\\LaTeX}")
  (check= (tl '(TeX)) "{\\TeX}")
  ;; without macros, the content tags are written in plain LaTeX
  (check= (tl '(em "x") '("texmacs->latex:use-macros" . "off"))
          "{{\\em x\\/}}")
  (check= (tl '(strong "x") '("texmacs->latex:use-macros" . "off"))
          "{\\textbf{x}}"))

(define (test-export-sections)
  (check-group "export: sections and paragraphs")
  (check= (tl '(section "Intro")) "\\section{Intro}")
  (check= (tl '(section* "Intro")) "\\section*{Intro}")
  (check= (tl '(subsection "S")) "\\subsection{S}")
  (check= (tl '(chapter "C")) "\\chapter{C}")
  (check= (tl '(paragraph "P")) "\\paragraph{P}")
  ;; paragraphs are separated by an empty line
  (check= (tl '(document "a" "b")) "a\n\nb")
  (check= (tl '(document (section "A") "text")) "\\section{A}\n\ntext")
  (check= (tl '(concat "a" (next-line) "b")) "a\\\\\nb")
  (check= (tl '(new-page)) "{\\newpage}")
  (check= (tl '(space "1em")) "\\quad"))

(define (test-export-lists)
  (check-group "export: lists")
  (check= (tl '(itemize (document (concat (item) "a") (concat (item) "b"))))
          "\\begin{itemize}\n  \\item a\n  \n  \\item b\n\\end{itemize}")
  (check= (tl '(enumerate (document (concat (item) "a"))))
          "\\begin{enumerate}\n  \\item a\n\\end{enumerate}")
  (check= (tl '(description (document (concat (item* "x") "a"))))
          "\\begin{description}\n  \\item[x] a\n\\end{description}"))

(define (test-export-references)
  (check-group "export: labels, references, notes")
  (check= (tl '(label "l")) "\\label{l}")
  (check= (tl '(concat "a" (label "x") "b")) "a\\label{x}b")
  (check= (tl '(reference "l")) "\\ref{l}")
  (check= (tl '(pageref "l")) "\\pageref{l}")
  (check= (tl '(eqref "l")) "\\eqref{l}")
  (check= (tl '(concat "a" (footnote "f"))) "a\\footnote{f}")
  (check= (tl '(cite "k")) "{\\cite{k}}")
  (check= (tl '(cite "k" "l")) "{\\cite{k,l}}")
  (check= (tl '(hlink "t" "http://x")) "\\href{http://x}{t}")
  (check= (tl '(url "http://x")) "{\\url{http://x}}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export: mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-export-math)
  (check-group "export: formulas")
  (check= (tl '(math "<alpha>+<infty>")) "$\\alpha + \\infty$")
  (check= (tl '(math "a<leq>b")) "$a \\leq b$")
  (check= (tl '(math "a<cdot>b<times>c")) "$a \\cdot b \\times c$")
  (check= (tl '(math "<varepsilon><Gamma>")) "$\\varepsilon \\Gamma$")
  (check= (tl '(math "<partial>f")) "$\\partial f$")
  (check= (tl '(math "sin x")) "$\\sin x$")
  (check= (tl '(math "<bbb-R>")) "$\\mathbb{R}$")
  (check= (tl '(math "<cal-A>")) "$\\mathcal{A}$")
  (check= (tl '(math "<b-x>")) "$\\tmmathbf{x}$")
  ;; an underscore in a formula is a character, not a subscript
  (check= (tl '(math "a_b")) "$a\\_b$")
  (check= (tl '(math (frac "a" "b"))) "$\\frac{a}{b}$")
  (check= (tl '(math (frac "1" (concat "1" "+" (frac "1" "x")))))
          "$\\frac{1}{1 + \\frac{1}{x}}$")
  (check= (tl '(math (sqrt "x"))) "$\\sqrt{x}$")
  (check= (tl '(math (sqrt "x" "3"))) "$\\sqrt[3]{x}$")
  (check= (tl '(math (concat "x" (rsub "i") (rsup "2")))) "$x_i^2$")
  (check= (tl '(math (concat "x" (rsub "i+1")))) "$x_{i + 1}$")
  ;; several letters in a row are an operator name
  (check= (tl '(math (concat "x" (rsub "ij")))) "$x_{\\tmop{ij}}$")
  (check= (tl '(math (concat "x" (rprime "'")))) "$x'$")
  (check= (tl '(math (concat "a" (lsub "b")))) "$a {}_b$")
  (check= (tl '(math (binom "n" "k"))) "$\\binom{n}{k}$")
  (check= (tl '(math (concat "a" (text "if") "b"))) "$a \\text{if} b$"))

;; LaTeX's \boxed is the boxed macro; a frame is \fbox in text, but \fbox
;; typesets its argument as text, so a frame in a formula is \boxed
(define (test-export-boxes)
  (check-group "export: boxes")
  (check= (tl '(math (boxed (concat "x" (rsup "2") "+" (frac "1" "2")))))
          "$\\boxed{x^2 + \\frac{1}{2}}$")
  (check= (tl '(equation* (document (boxed (concat "y=" (frac "1" "3"))))))
          "\\[ \\boxed{y = \\frac{1}{3}} \\]")
  (check= (tl '(boxed "x")) "$\\boxed{x}$")
  (check= (tl '(frame (concat "text " (math "a")))) "\\fbox{text $a$}")
  (check= (tl '(frame (concat "x" (rsup "2")))) "\\fbox{x\\tmrsup{2}}")
  (check= (tl '(math (frame (concat "x" (rsup "2"))))) "$\\boxed{x^2}$")
  (check= (tl '(fcolorbox "red" "yellow" "warn"))
          "\\fcolorbox{red}{yellow}{warn}")
  (check= (tl '(colored-frame "yellow" "hi")) "\\colorbox{yellow}{hi}")
  (check-true (string-contains?
               (td '("generic") '(document (concat "a " (math (boxed "x")))))
               "\\usepackage{amsmath}")))

(define (test-export-big)
  (check-group "export: big operators and delimiters")
  (check= (tl '(math (concat (big "sum") (rsub "i") "x"))) "$\\sum_i x$")
  (check= (tl '(math (concat (big "int") (rsub "0") (rsup "1") "f")))
          "$\\int_0^1 f$")
  (check= (tl '(math (concat (big "prod") (rsub "i") "a"))) "$\\prod_i a$")
  (check= (tl '(math (concat (big "cup") "A"))) "$\\bigcup A$")
  ;; delimiters which do not stretch in the source are written as such
  (check= (tl '(math (around* "(" "x" ")"))) "$(x)$")
  (check= (tl '(math (around* "[" "x" ")"))) "$[x)$")
  (check= (tl '(math (around* "<langle>" "x" "<rangle>")))
          "$\\langle x \\rangle$")
  (check= (tl '(math (around* "<lfloor>" "x" "<rfloor>")))
          "$\\lfloor x \\rfloor$")
  (check= (tl '(math (concat "f" (around* "(" "x" ")")))) "$f (x)$"))

(define (test-export-accents-math)
  (check-group "export: math accents")
  (check= (tl '(math (wide "x" "^"))) "$\\hat{x}$")
  (check= (tl '(math (wide "x" "<hat>"))) "$\\hat{x}$")
  (check= (tl '(math (wide "x" "~"))) "$\\tilde{x}$")
  (check= (tl '(math (wide "x" "<bar>"))) "$\\bar{x}$")
  (check= (tl '(math (wide "x" "<vect>"))) "$\\vec{x}$")
  (check= (tl '(math (wide "x" "<dot>"))) "$\\dot{x}$")
  (check= (tl '(math (wide "x" "<ddot>"))) "$\\ddot{x}$")
  (check= (tl '(math (wide "x" "<check>"))) "$\\check{x}$")
  (check= (tl '(math (wide "x" "<breve>"))) "$\\breve{x}$")
  (check= (tl '(math (wide* "x" "<bar>"))) "$\\underline{x}$"))

;; matrices are arrays between stretched delimiters
(define (test-export-matrices)
  (check-group "export: matrices")
  (check= (tl '(math (matrix (tformat (table (row (cell "a") (cell "b"))
                                             (row (cell "c") (cell "d")))))))
          (string-append "$\\left(\\begin{array}{cc}\n  a & b\\\\\n"
                         "  c & d\n\\end{array}\\right)$"))
  (check= (tls '(math (det (tformat (table (row (cell "a") (cell "b")))))))
          "$\\left|\\begin{array}{cc} a & b \\end{array}\\right|$")
  (check= (tls '(math (choice (tformat (table (row (cell "a") (cell "b")))))))
          "$\\left\\{\\begin{array}{ll} a & b \\end{array}\\right.$"))

;; numbered rows of eqnarray and align carry \nonumber unless they have an
;; eq-number
(define (test-export-equations)
  (check-group "export: equations")
  (check= (tl '(equation* (document "x=1"))) "\\[ x = 1 \\]")
  (check= (tl '(equation (document "x=1")))
          "\\begin{equation}\n  x = 1\n\\end{equation}")
  (check= (tls '(equation (document (concat "x=1" (label "e")))))
          "\\begin{equation} x = 1 \\label{e} \\end{equation}")
  (check= (tl '(eqnarray* (document (tformat (table
                                              (row (cell "a") (cell "=") (cell "b"))
                                              (row (cell "c") (cell "=") (cell "d")))))))
          "\\begin{eqnarray*}\n  a & = & b\\\\\n  c & = & d\n\\end{eqnarray*}")
  (check= (tls '(eqnarray (document (tformat (table
                                              (row (cell "a") (cell "=") (cell "b")))))))
          "\\begin{eqnarray} a & = & b \\nonumber \\end{eqnarray}")
  (check= (tls '(align (document (tformat (table (row (cell "a") (cell "=b")))))))
          "\\begin{align} a & = b \\nonumber \\end{align}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export: tables and environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tables at the level of the LaTeX stree, as in the (commented out) table
;; tests of convert/latex/test-tmtex.scm, whose expectations still hold:
;; tabular is left aligned, tabular* centered, block has rules everywhere
(define (test-export-tables)
  (check-group "export: tables")
  (with t '(table (row (cell "a")))
    (check= (ts t) '((!begin "tabular" "l") (!table (!row "a"))))
    (check= (ts `(tformat ,t)) '((!begin "tabular" "l") (!table (!row "a"))))
    (check= (ts `(tabular (tformat ,t)))
            '((!begin "tabular" "l") (!table (!row "a"))))
    (check= (ts `(tabular* (tformat ,t)))
            '((!begin "tabular" "c") (!table (!row "a"))))
    (check= (ts `(block (tformat ,t)))
            '((!begin "tabular" "|l|") (!table (hline) (!row "a") (hline))))
    (check= (ts `(block* (tformat ,t)))
            '((!begin "tabular" "|c|") (!table (hline) (!row "a") (hline)))))
  (with t '(table (row (cell "a") (cell "b")) (row (cell "c") (cell "d")))
    (check= (ts `(tabular* (tformat (cwith "1" "-1" "1" "1" "cell-halign" "r")
                                    ,t)))
            '((!begin "tabular" "rc") (!table (!row "a" "b") (!row "c" "d"))))
    (check= (ts `(tabular* (tformat (cwith "1" "-1" "1" "-1" "cell-halign" "r")
                                    ,t)))
            '((!begin "tabular" "rr") (!table (!row "a" "b") (!row "c" "d"))))
    (check= (ts `(tabular* (tformat (cwith "1" "1" "1" "-1" "cell-bborder" "1ln")
                                    ,t)))
            '((!begin "tabular" "cc")
              (!table (!row "a" "b") (hline) (!row "c" "d"))))
    (check= (ts `(tabular* (tformat (cwith "1" "-1" "1" "1" "cell-rborder" "1ln")
                                    ,t)))
            '((!begin "tabular" "c|c") (!table (!row "a" "b") (!row "c" "d"))))
    (check= (tl `(tabular (tformat ,t)))
            "\\begin{tabular}{ll}\n  a & b\\\\\n  c & d\n\\end{tabular}"))
  (check= (tls '(block (tformat (table (row (cell "a") (cell "b"))))))
          "\\begin{tabular}{|l|l|} \\hline a & b\\\\ \\hline \\end{tabular}"))

(define (test-export-environments)
  (check-group "export: theorems and environments")
  (check= (tl '(theorem (document "T")))
          "\\begin{theorem}\n  T\n\\end{theorem}")
  (check= (tls '(lemma (document "T"))) "\\begin{lemma} T \\end{lemma}")
  (check= (tls '(theorem* (document "T"))) "\\begin{theorem*} T \\end{theorem*}")
  (check= (tl '(proof (document "a" "b")))
          "\\begin{proof}\n  a\n  \n  b\n\\end{proof}")
  (check= (tls '(abstract (document "a"))) "\\begin{abstract} a \\end{abstract}")
  (check= (tls '(quote-env (document "q"))) "\\begin{quoteenv} q \\end{quoteenv}")
  (check= (tl '(center (document "c"))) "{\\center{c}}"))

;; inline verbatim escapes as text does; a verbatim block is alltt
(define (test-export-verbatim)
  (check-group "export: verbatim")
  (check= (tl '(verbatim "a%b")) "\\tmverbatim{a\\%b}")
  (check= (tl '(verbatim "a\\b{c}")) "\\tmverbatim{a\\textbackslash b\\{c\\}}")
  (check= (tl '(verbatim "$x$")) "\\tmverbatim{\\$x\\$}")
  (check= (tl '(verbatim (document "a" "b")))
          "\\begin{alltt}\na\nb\n\\end{alltt}")
  (check= (tl '(code (document "x")))
          "\\begin{tmcode}\nx\n\\end{tmcode}"))

;; user macros are exported as \newcommand
(define (test-export-macros)
  (check-group "export: macros")
  (check= (tl '(assign "foo" (macro "bar"))) "\\newcommand{\\foo}{bar}")
  (check= (tl '(concat (assign "foo" (macro "x" (concat "[" (arg "x") "]")))
                       (foo "y")))
          "\\newcommand{\\foo}[1]{[#1]}{\\foo{y}}"))

;; whole documents: class, babel, the preamble with the TeXmacs macros
;; and theorem environments which the body uses, the title
(define (test-export-documents)
  (check-group "export: documents")
  (with s (td '("article")
              '(document (doc-data (doc-title "T")
                                   (doc-author (author-data (author-name "A"))))
                         (section "Intro")
                         (concat "Hello " (em "w") " " (math (frac "a" "b")))
                         (theorem (document "x"))))
    (check-true (string-starts? s "\\documentclass{article}\n"))
    (check-true (string-contains? s "\\usepackage[english]{babel}"))
    (check-true (string-contains? s "\\newcommand{\\tmem}[1]{{\\em #1\\/}}"))
    (check-true (string-contains? s "\\newtheorem{theorem}{Theorem}"))
    (check-true (string-contains? s "\\title{T}"))
    (check-true (string-contains? s "\\author{A}"))
    (check-true (string-contains? s "\\maketitle"))
    (check-true (string-contains? s "\\section{Intro}"))
    (check-true (string-contains? s "Hello {\\tmem{w}} $\\frac{a}{b}$"))
    (check-true (string-contains? s "\\begin{theorem}\n  x\n\\end{theorem}"))
    (check-true (string-ends? s "\\end{document}\n")))
  ;; no macro is defined when none is used
  (check= (squash (td '("book") '(document (chapter "C") "x")))
          (string-append "\\documentclass{book} \\usepackage[english]{babel} "
                         "\\begin{document} \\chapter{C} x \\end{document}"))
  (check= (squash (td '("generic") '(document "x")
                      '(associate "language" "french")))
          (string-append "\\documentclass{article} "
                         "\\usepackage[french]{babel} "
                         "\\begin{document} x \\end{document}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import: text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-import-special)
  (check-group "import: special characters")
  (check= (lt "a\\%b\\&c\\_d\\#e\\$f\\{g\\}h\\~{}i\\^{}j\\textbackslash k")
          "a%b&c_d#e$f{g}h~i^j\\k")
  (check= (lt "<") "<less>")
  (check= (lt ">") "<gtr>")
  (check= (lt "~") '(nbsp))
  (check= (lt "``q''") "\x10q\x11")
  (check= (lt "--") "\x15")
  (check= (lt "---") "\x16")
  (check= (lt "\\dots") "...")
  ;; the space after a control word belongs to it
  (check= (lt "x \\ldots y") "x ...y")
  ;; accents become Cork bytes
  (check= (lt "caf\\'e") "caf\xe9")
  (check= (lt "caf{\\'e}") "caf\xe9")
  (check= (lt "\\`a\\\"o") "\xe0\xf6")
  (check= (lt "\\ss{} \\o{} \\AA") "\xff \xf8 \xc5"))

(define (test-import-styles)
  (check-group "import: emphasis and fonts")
  (check= (lt "\\emph{x}") '(em "x"))
  (check= (lt "\\textbf{x}") '(with "font-series" "bold" "x"))
  (check= (lt "{\\bf x}") '(with "font-series" "bold" "x"))
  (check= (lt "\\textit{x}") '(with "font-shape" "italic" "x"))
  (check= (lt "\\texttt{x}") '(with "font-family" "tt" "x"))
  (check= (lt "\\textsuperscript{a}") '(rsup "a"))
  ;; a space which begins the argument of a command of text is typeset
  (check= (lt "a\\textbf{ b}") '(concat "a" (with "font-series" "bold" " b")))
  (check= (lt "a\\emph{ b}") '(concat "a" (em " b")))
  (check= (lt "a\\mbox{ b }c") "a b c")
  (check= (lt "$a,\\text{ if }b$") '(math (concat "a," (text " if ") "b")))
  (check= (lt "\\section{ Intro}") '(section "Intro")))

(define (test-import-sections)
  (check-group "import: sections, paragraphs, spaces")
  (check= (lt "\\section{Intro}") '(section "Intro"))
  (check= (lt "\\section*{Intro}") '(section* "Intro"))
  (check= (lt "\\subsection{S}") '(subsection "S"))
  (check= (lt "\\section{A}\n\ntext") '(document (section "A") "text"))
  (check= (lt "a\n\nb") '(document "a" "b"))
  (check= (lt "a\\\\b") '(concat "a" (next-line) "b"))
  (check= (lt "\\noindent a") '(concat (no-indent) "a"))
  (check= (lt "\\newpage") '(new-page))
  (check= (lt "\\quad") '(space "1em"))
  (check= (lt "\\hspace{2em}") '(space "2em"))
  (check= (lt "\\vspace{1cm}") '(vspace "1cm"))
  (check= (lt "\\bigskip") '(vspace "2fn")))

(define (test-import-lists)
  (check-group "import: lists")
  (check= (lt "\\begin{itemize}\n\\item a\n\\item b\n\\end{itemize}")
          '(document (itemize (document (concat (item) "a")
                                        (concat (item) "b")))))
  (check= (lt "\\begin{enumerate}\\item a\\end{enumerate}")
          '(document (enumerate (document (concat (item) "a")))))
  (check= (lt "\\begin{description}\\item[x] a\\end{description}")
          '(document (description (document (concat (item* "x") "a")))))
  (check= (lt "\\begin{itemize}\\item[*] a\\end{itemize}")
          '(document (itemize (document (concat (item* "*") "a")))))
  ;; the options of enumitem are dropped, the lists kept
  (check= (lt "\\begin{itemize}[nosep]\n\\item a\n\\item b\n\\end{itemize}")
          '(document (itemize (document (concat (item) "a")
                                        (concat (item) "b")))))
  (check= (lt "\\begin{description}[style=nextline]\\item[x] a\\end{description}")
          '(document (description (document (concat (item* "x") "a")))))
  (check= (lt "\\begin{compactitem}[nosep]\\item a\\end{compactitem}")
          '(document (itemize (document (concat (item) "a")))))
  (check= (lt "\\begin{enumerate}[label=(\\alph*)]\\item \\href{http://x.org}{x}\\end{enumerate}")
          '(document (enumerate (document (concat (item) (hlink "x" "http://x.org")))))))

(define (test-import-references)
  (check-group "import: labels, references, notes")
  (check= (lt "\\label{l}") '(label "l"))
  (check= (lt "\\ref{l}") '(reference "l"))
  (check= (lt "\\pageref{l}") '(pageref "l"))
  (check= (lt "a\\footnote{f}") '(concat "a" (footnote "f")))
  (check= (lt "\\cite{k}") '(cite "k"))
  (check= (lt "\\cite{ab,cd}") '(cite "ab" "cd"))
  (check= (lt "\\cite{a, bc}") '(cite "a" "bc"))
  ;; keys of one character, also the last one
  (check= (lt "\\cite{ab,c}") '(cite "ab" "c"))
  (check= (lt "\\cite{a,b,c}") '(cite "a" "b" "c"))
  (check= (lt "\\cite[p. 3]{k}") '(cite-detail "k" "p. 3"))
  (check= (lt "\\href{http://x}{t}") '(hlink "t" "http://x")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import: mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the importer adds the invisible multiplications and the stretched
;; delimiters (around*) of TeXmacs formulas
(define (test-import-math)
  (check-group "import: formulas")
  (check= (lt "$\\alpha+\\infty$") '(math "<alpha>+<infty>"))
  (check= (lt "$a \\leq b$") '(math "a<leq>b"))
  (check= (lt "$<$") '(math "<less>"))
  (check= (lt "$\\mathbb{R}$") '(math "<bbb-R>"))
  (check= (lt "$\\mathcal{A}$") '(math "<cal-A>"))
  (check= (lt "$\\mathbf{x}$") '(math "<b-up-x>"))
  (check= (lt "$\\operatorname{rank} A$")
          '(math (concat (math-up "rank") "A")))
  ;; a text separates the factors around it: no multiplication across it
  (check= (lt "$a\\text{if}b$") '(math (concat "a" (text "if") "b")))
  ;; (the space which begins the argument of \text is kept, as in LaTeX)
  (check= (lt "$x\\text{ for all }y$")
          '(math (concat "x" (text " for all ") "y")))
  (check= (lt "$ab$") '(math "a*b"))
  (check= (lt "$\\frac{a}{b}$") '(math (frac "a" "b")))
  (check= (lt "$a\\over b$") '(math (frac "a" "b")))
  (check= (lt "$\\sqrt{x}$") '(math (sqrt "x")))
  (check= (lt "$\\sqrt[3]{x}$") '(math (sqrt "x" "3")))
  (check= (lt "$x_i^2$") '(math (concat "x" (rsub "i") (rsup "2"))))
  (check= (lt "$\\sum_i x$") '(math (concat (big "sum") (rsub "i") "x")))
  (check= (lt "$\\left(x\\right)$") '(math (around* "(" "x" ")")))
  (check= (lt "$\\left[x\\right)$") '(math (around* "[" "x" ")")))
  (check= (lt "$x\\,y\\;z$")
          '(math (concat "x*" (space "0.17em") "y*" (space "0.27em") "z")))
  (check= (lt "$\\hat{x}$") '(math (wide "x" "^")))
  (check= (lt "$\\tilde x$") '(math (wide "x" "~")))
  (check= (lt "$\\overline{x}$") '(math (wide "x" "<bar>")))
  (check= (lt "$\\dot x \\ddot y$")
          '(math (concat (wide "x" "<dot>") (wide "y" "<ddot>"))))
  (check= (lt "$\\underline{u}$") '(math (wide* "u" "<bar>"))))

;; the optional width and position of \framebox and \makebox are dropped
(define (test-import-boxes)
  (check-group "import: boxes")
  (check= (lt "$\\boxed{x^2+\\frac{1}{2}}$")
          '(math (boxed (concat "x" (rsup "2") "+" (frac "1" "2")))))
  (check= (lt "\\[\\boxed{y=\\frac13}\\]")
          '(document (equation* (document (boxed (concat "y=" (frac "1" "3")))))))
  (check= (lt "\\begin{equation*}\\boxed{a=b}\\end{equation*}")
          '(document (equation* (document (boxed "a=b")))))
  (check= (lt "\\fbox{text $a$}") '(frame (concat "text " (math "a"))))
  (check= (lt "\\framebox{plain}") '(frame "plain"))
  (check= (lt "\\framebox[3cm]{w}") '(frame "w"))
  (check= (lt "\\framebox[3cm][c]{centered}") '(frame "centered"))
  (check= (lt "a \\framebox[2cm][r]{$x$} b")
          '(concat "a " (frame (math "x")) " b"))
  (check= (lt "\\makebox{mb}") "mb")
  (check= (lt "\\makebox[2cm]{mb}") "mb")
  (check= (lt "\\makebox[3cm][r]{right}") "right")
  ;; (a text ends the implicit product: no * before it, #271)
  (check= (lt "$a \\makebox[1cm][l]{t u} b$")
          '(math (concat "a" (text "t u") "b")))
  (check= (lt "\\fcolorbox{red}{yellow}{warn}")
          '(fcolorbox "red" "yellow" "warn"))
  (check= (lt "\\colorbox{yellow}{hi}") '(colored-frame "yellow" "hi")))

(define (test-import-matrices)
  (check-group "import: matrices")
  (check= (lt "$\\begin{pmatrix}a&b\\\\c&d\\end{pmatrix}$")
          '(math (matrix (tformat (table (row (cell "a") (cell "b"))
                                         (row (cell "c") (cell "d")))))))
  (check= (lt "$\\begin{matrix}a\\end{matrix}$")
          '(math (tabular* (tformat (table (row (cell "a")))))))
  (check= (lt "$\\begin{cases}a & b\\end{cases}$")
          '(math (choice (tformat (table (row (cell "a") (cell "b"))))))))

(define (test-import-equations)
  (check-group "import: equations")
  (check= (lt "\\[ x = 1 \\]") '(document (equation* (document "x=1"))))
  (check= (lt "\\begin{equation}x=1\\end{equation}")
          '(document (equation (document "x=1"))))
  (check= (lt "\\begin{equation}x=1\\label{e}\\end{equation}")
          '(document (equation (document (concat "x=1" (label "e"))))))
  (check= (lt "\\begin{eqnarray*}a&=&b\\end{eqnarray*}")
          '(document (eqnarray* (document (tformat (table
                                  (row (cell "a") (cell "=") (cell "b"))))))))
  ;; numbered align rows get an eq-number
  (check= (lt "\\begin{align}a&=b\\end{align}")
          '(document (align (document (tformat (table
                       (row (cell "a") (cell (concat "=b" (eq-number))))))))))
  (check= (lt "\\begin{align*}a&=b\\\\c&=d\\end{align*}")
          '(document (align* (document (tformat (table
                       (row (cell "a") (cell "=b"))
                       (row (cell "c") (cell "=d")))))))))

;; the column specification becomes explicit cell formats
(define (test-import-tables)
  (check-group "import: tables")
  (check= (lt "\\begin{tabular}{ll}a&b\\\\c&d\\end{tabular}")
          '(tabular* (tformat (cwith "1" "-1" "1" "1" "cell-halign" "l")
                              (cwith "1" "-1" "1" "1" "cell-lborder" "0ln")
                              (cwith "1" "-1" "2" "2" "cell-halign" "l")
                              (cwith "1" "-1" "2" "2" "cell-rborder" "0ln")
                              (cwith "1" "-1" "1" "-1" "cell-valign" "c")
                              (table (row (cell "a") (cell "b"))
                                     (row (cell "c") (cell "d"))))))
  (check= (lt "\\begin{tabular}{|c|}\\hline a\\\\\\hline\\end{tabular}")
          '(tabular* (tformat (cwith "1" "-1" "1" "1" "cell-lborder" "1ln")
                              (cwith "1" "-1" "1" "1" "cell-halign" "c")
                              (cwith "1" "-1" "1" "1" "cell-rborder" "1ln")
                              (cwith "1" "-1" "1" "-1" "cell-valign" "c")
                              (cwith "1" "1" "1" "-1" "cell-tborder" "1ln")
                              (cwith "1" "1" "1" "-1" "cell-bborder" "1ln")
                              (table (row (cell "a")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import: environments, macros, unknown commands, comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-import-environments)
  (check-group "import: environments")
  (check= (lt "\\begin{theorem}T\\end{theorem}")
          '(document (theorem (document "T"))))
  (check= (lt "\\begin{proof}P\\end{proof}") '(document (proof (document "P"))))
  (check= (lt "\\begin{quote}q\\end{quote}") '(quote-env "q"))
  (check= (lt "\\begin{figure}x\\caption{c}\\end{figure}")
          '(document (big-figure "x" "c")))
  (check= (lt "\\begin{verbatim}a%b\\end{verbatim}") '(verbatim "a%b"))
  (check= (lt "\\verb|a%b|") '(verbatim "a%b"))
  (check= (lt "\\begin{alltt}\na\nb\n\\end{alltt}")
          '(document (verbatim-code (document "a" "b"))))
  ;; as alltt, the line breaks after \begin{tmcode} and before \end{tmcode}
  ;; do not become empty lines, and empty lines inside are kept
  (check= (lt "\\begin{tmcode}\nx\n\\end{tmcode}") '(code (document "x")))
  (check= (lt "\\begin{tmcode}[cpp]\na\n\nb\n\\end{tmcode}")
          '(document (cpp-code (document "a" "" "b"))))
  ;; an unknown environment becomes a tag of the same name
  (check= (lt "\\begin{unknownenv}x\\end{unknownenv}") '(unknownenv "x"))
  ;; the text of a \parbox is text, also in a formula
  (check= (lt "\\parbox{3cm}{a $x$}")
          '(mini-paragraph "3cm" (concat "a " (math "x"))))
  (check= (lt "$\\parbox{3cm}{a b}$") '(math (mini-paragraph "3cm" (text "a b"))))
  (check= (lt "\\[\\parbox{3cm}{Given $x$, \\[a=b\\] for all $s$.}\\]")
          '(document
             (equation*
               (document
                 (mini-paragraph "3cm"
                   (document
                     (text (document (concat "Given " (math "x") ",")
                                     (equation* (document "a=b"))
                                     (concat "for all " (math "s") "."))))))))))

;; definitions become assign/macro and their uses macro applications
(define (test-import-macros)
  (check-group "import: macros")
  (check= (lt "\\newcommand{\\foo}{bar}\\foo")
          '(concat (assign "foo" (macro "bar")) (foo)))
  (check= (lt "\\newcommand{\\foo}[1]{<#1>}\\foo{x}")
          '(concat (assign "foo" (macro "1" (concat "<less>" (arg "1") "<gtr>")))
                   (foo "x")))
  (check= (lt "\\def\\foo{bar}\\foo") '(concat (assign "foo" (macro "bar")) (foo)))
  (check= (lt "\\renewcommand{\\emph}[1]{X#1}")
          '(assign "emph" (macro "1" (concat "X" (arg "1")))))
  (check= (lt "\\newenvironment{myenv}{[}{]}\\begin{myenv}x\\end{myenv}")
          '(concat (assign "myenv" (macro "body" (surround "[" "]"
                                                          (document (arg "body")))))
                   (myenv "x"))))

(define (test-import-unknown)
  (check-group "import: unknown commands and comments")
  ;; an unknown command is kept as a tag, its optional argument makes
  ;; the starred variant
  (check= (lt "\\unknowncmd{x}") '(unknowncmd "x"))
  (check= (lt "\\foo[opt]{x}") '(foo* "opt" "x"))
  ;; comments disappear, with the end of their line
  (check= (lt "% only a comment") "")
  (check= (lt "a % comment\nb") "a b")
  (check= (lt "a%\nb") "ab"))

;; parse-latex gives the LaTeX tree, latex->texmacs converts it
(define (test-import-parse)
  (check-group "import: parse-latex and latex->texmacs")
  (check-true (tree? (parse-latex "\\emph{x}")))
  (check= (tree->stree (latex->texmacs (parse-latex "\\emph{x} $a_1$")))
          '(concat (em "x") " " (math (concat "a" (rsub "1")))))
  (check= (tree->stree (parse-latex "\\emph{x}")) '(concat (tuple "\\emph" "x")))
  (check= (tree->stree (parse-latex-document "x")) '(!file (concat "x")))
  ;; \parbox[pos][height][inner]{width}{text}: the options after the first
  ;; are dropped, not read as text
  (check= (lt "\\parbox[t][3cm][c]{2cm}{a b} c")
          '(concat (mini-paragraph "2cm" "a b") " c"))
  (check= (lt "\\parbox[t][3cm]{2cm}{a b} c")
          '(concat (mini-paragraph "2cm" "a b") " c"))
  (check= (lt "\\parbox{2cm}{a b} [x] c")
          '(concat (mini-paragraph "2cm" "a b") " [x] c")))

;; \begin{document}: in a snippet, the class and the body; in a document,
;; the style, the preamble definitions (hidden) and the title
(define (test-import-documents)
  (check-group "import: documents")
  (check= (lt "\\documentclass{article}\\begin{document}Hello\\end{document}")
          '(document (documentclass "article") "Hello"))
  (check= (lt "\\begin{document}x\\end{document}") '(document "x"))
  (with d (ld "\\documentclass{article}\n\\begin{document}\nHello\n\\end{document}\n")
    (check= (cadr (assoc 'style (cdr d))) '(tuple "article" "std-latex"))
    (check= (body-of d) '(document "Hello")))
  (check= (body-of (ld (string-append "\\documentclass{book}\\begin{document}"
                                     "x\n\n\\chapter{C}\n\ny\\end{document}")))
          '(document "x" (chapter "C") "y"))
  ;; a section which starts the body is a paragraph of its own, as the
  ;; later ones
  (check= (body-of (ld (string-append "\\documentclass{article}\\begin{document}"
                                     "\\section{C}\n\nx\\end{document}")))
          '(document (section "C") "x"))
  (check= (body-of (ld (string-append "\\documentclass{article}\\begin{document}"
                                     "\\section{C}\n\nx\n\n\\section{D}\n\ny"
                                     "\\end{document}")))
          '(document (section "C") "x" (section "D") "y"))
  (check= (body-of (ld (string-append
                        "\\documentclass{article}\n\\usepackage{amsmath}\n"
                        "\\newcommand{\\R}{\\mathbb{R}}\n"
                        "\\begin{document}\n$x \\in \\R$\n\\end{document}\n")))
          '(document (hide-preamble (document (assign "R" (macro "<bbb-R>"))))
                     (math (concat "x<in>" (R)))))
  ;; \maketitle without \date dates the document today
  (check= (body-of (ld (string-append
                        "\\documentclass{article}\\title{T}\\author{Ann}"
                        "\\begin{document}\\maketitle\n\nx\\end{document}")))
          '(document (doc-data (doc-title "T")
                               (doc-author (author-data (author-name "Ann")))
                               (doc-date (date "")))
                     "x"))
  ;; an author of one character is kept
  (check= (body-of (ld (string-append
                        "\\documentclass{article}\\begin{document}"
                        "\\title{T}\\author{A}\\date{D}\\maketitle x"
                        "\\end{document}")))
          '(document (doc-data (doc-title "T")
                               (doc-author (author-data (author-name "A")))
                               (doc-date "D"))
                     "x"))
  (check= (body-of (ld (string-append
                        "\\documentclass{article}\\begin{document}"
                        "\\title{T}\\author{Ann}\\date{D}\\maketitle x"
                        "\\end{document}")))
          '(document (doc-data (doc-title "T")
                               (doc-author (author-data (author-name "Ann")))
                               (doc-date "D"))
                     "x"))
  ;; the abstract goes to the front matter
  (check= (body-of (ld (string-append
                        "\\documentclass{article}\\begin{document}a % c\nb\n\n"
                        "\\begin{abstract}A\\end{abstract}\\end{document}")))
          '(document (abstract-data (abstract (document "A"))) "a b"))
  (check= (cadr (assoc 'style (cdr (ld "\\documentclass{amsart}\\begin{document}x\\end{document}"))))
          '(tuple "amsart")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Round trips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; constructs which LaTeX expresses exactly come back unchanged (a block
;; comes back as the only paragraph of a document, which unblock removes)
(define round-trip-exact
  '("a%b&c_d#e$f{g}h~i^j\\k"
    "caf\xe9"
    "\xe0\xe8\xf6\xff\xc9"
    (em "x") (strong "x") (concat "a" (em "b") "c")
    (with "font-series" "bold" "x") (with "font-shape" "italic" "x")
    (with "font-shape" "small-caps" "x")
    (with "font-family" "tt" "x") (with "font-family" "ss" "x")
    (with "color" "red" "x")
    (section "Intro") (section* "Intro") (chapter "C") (paragraph "P")
    (document (section "A") "text") (document "a" "b")
    (itemize (document (concat (item) "a") (concat (item) "b")))
    (enumerate (document (concat (item) "a")))
    (description (document (concat (item* "x") "a")))
    (label "l") (reference "l") (pageref "l") (eqref "l")
    (concat "a" (footnote "f")) (cite "k") (cite "ab" "cd") (cite "k" "l")
    (hlink "t" "http://x") (new-page) (space "1em") (nbsp)
    (verbatim "a%b")
    (math "<alpha>+<infty>") (math "a<leq>b") (math "sin x")
    (math (frac "a" "b")) (math (sqrt "x")) (math (sqrt "x" "3"))
    (math (concat "x" (rsub "i") (rsup "2")))
    (math (concat (big "sum") (rsub "i") "x"))
    (math (concat (big "int") (rsub "0") (rsup "1") "f"))
    (math (binom "n" "k")) (math (op "lim"))
    (math (concat "a" (text "if") "b"))
    (math (wide "x" "^")) (math (wide "x" "~")) (math (wide "x" "<bar>"))
    (math (wide "x" "<vect>")) (math (wide* "x" "<bar>"))
    (math "<bbb-R>") (math "<cal-A>")
    (math (boxed (concat "x" (rsup "2") "+" (frac "1" "2"))))
    (equation* (document (boxed (concat "y=" (frac "1" "3")))))
    (frame (concat "text " (math "a"))) (frame "x")
    (fcolorbox "red" "yellow" "warn") (colored-frame "yellow" "hi")
    (equation* (document "x=1")) (equation (document "x=1"))
    (equation (document (concat "x=1" (label "e"))))
    (eqnarray* (document (tformat (table (row (cell "a") (cell "=") (cell "b"))
                                         (row (cell "c") (cell "=") (cell "d"))))))
    (theorem (document "T")) (lemma (document "T"))
    (definition (document "T")) (proof (document "P")) (remark (document "R"))
    (abstract (document "a")) (center (document "c"))))

;; constructs which come back changed, with what they come back as; these
;; are losses of information of the conversion, not bugs:
;;   - a non stretched delimiter is written as a plain one and read back as
;;     the plain TeXmacs around; left/right become around too;
;;   - matrices are arrays between \left( and \right), read back as such;
;;   - <hat> and ^ are both \hat;
;;   - a tabular gets its default format written out;
;;   - a left subscript is written {}_b, which is read back on the left
;;     atom;
;;   - text underline and math under-bar are both \underline;
;;   - math bold is \tmmathbf, read back as a bold letter;
;;   - a frame in a formula is \boxed, read back as boxed, and \boxed is
;;     only allowed in formulas.
(define round-trip-lossy
  '(((math (around* "(" "x" ")")) (math (around "(" "x" ")")))
    ((math (concat (left "(") "x" (right ")"))) (math (around "(" "x" ")")))
    ((math (wide "x" "<hat>")) (math (wide "x" "^")))
    ((math (concat "a" (lsub "b"))) (math (concat "a" (rsub "b"))))
    ((underline "u") (wide* "u" "<bar>"))
    ((math (with "math-font-series" "bold" "x")) (math "<b-x>"))
    ((math (frame "x")) (math (boxed "x")))
    ((boxed "x") (math (boxed "x")))
    ((verbatim (document "a" "b")) (verbatim-code (document "a" "b")))
    ((math (det (tformat (table (row (cell "a") (cell "b"))))))
     (math (around* "|" (tabular* (tformat
       (cwith "1" "-1" "1" "1" "cell-halign" "c")
       (cwith "1" "-1" "1" "1" "cell-lborder" "0ln")
       (cwith "1" "-1" "2" "2" "cell-halign" "c")
       (cwith "1" "-1" "2" "2" "cell-rborder" "0ln")
       (table (row (cell "a") (cell "b"))))) "|")))
    ((tabular (tformat (table (row (cell "a") (cell "b")))))
     (tabular* (tformat (cwith "1" "-1" "1" "1" "cell-halign" "l")
                        (cwith "1" "-1" "1" "1" "cell-lborder" "0ln")
                        (cwith "1" "-1" "2" "2" "cell-halign" "l")
                        (cwith "1" "-1" "2" "2" "cell-rborder" "0ln")
                        (cwith "1" "-1" "1" "-1" "cell-valign" "c")
                        (table (row (cell "a") (cell "b"))))))))

(define (test-round-trips)
  (check-group "round trips")
  (for-each (lambda (x)
              (check-equal (object->string x) (lambda () (round-trip x)) x))
            round-trip-exact)
  (for-each (lambda (p)
              (check-equal (object->string (car p))
                           (lambda () (round-trip (car p))) (cadr p)))
            round-trip-lossy)
  ;; LaTeX -> TeXmacs -> LaTeX on LaTeX written by the exporter
  (for-each (lambda (s)
              (check-equal s (lambda () (tl (lt s))) s))
            '("\\section{A}\n\ntext" "$\\frac{a}{b}$" "$x_i^2$" "a\\footnote{f}"
              "\\begin{theorem}\n  T\n\\end{theorem}"
              "\\begin{equation}\n  x = 1\n\\end{equation}"
              "$\\boxed{x^2 + \\frac{1}{2}}$" "\\[ \\boxed{y = \\frac{1}{3}} \\]"
              "\\fbox{text $a$}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a whole document converted from Scheme has no view, whose editor would
;; expand its macros; it is converted without that expansion (it used to
;; crash TeXmacs in latex_expand)
(define (test-export-without-view)
  (check-group "export: documents without a view")
  (with s (convert '(document (TeXmacs "2.1.5") (style (tuple "article"))
                              (body (document (section "A") "text")))
                   "texmacs-stree" "latex-document")
    ;; NOTE: string-contains is Guile only
    (with pos (lambda (x) (string-search-forwards x 0 s))
      (check-true (string? s))
      (check-true (>= (pos "\\documentclass{article}") 0))
      (check-true (>= (pos "\\section{A}") 0))
      (check-true (>= (pos "text") 0))
      (check-true (< -1 (pos "\\begin{document}") (pos "\\section{A}")
                     (pos "\\end{document}"))))))

(tm-define (latex-test-failures)
  (check-suite "latex")
  (test-export-special)
  (test-export-accents)
  (test-export-styles)
  (test-export-sections)
  (test-export-lists)
  (test-export-references)
  (test-export-math)
  (test-export-boxes)
  (test-export-big)
  (test-export-accents-math)
  (test-export-matrices)
  (test-export-equations)
  (test-export-tables)
  (test-export-environments)
  (test-export-verbatim)
  (test-export-macros)
  (test-export-documents)
  (test-import-special)
  (test-import-styles)
  (test-import-sections)
  (test-import-lists)
  (test-import-references)
  (test-import-math)
  (test-import-boxes)
  (test-import-matrices)
  (test-import-equations)
  (test-import-tables)
  (test-import-environments)
  (test-import-macros)
  (test-import-unknown)
  (test-import-parse)
  (test-import-documents)
  (test-round-trips)
  (test-export-without-view)
  (check-end))
