
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmhtml-expand.scm
;; DESCRIPTION : environment patch for expanding the document before conversion
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html tmhtml-expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "identity" macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-env-macro name)
  `(associate ,(symbol->string name)
	      (xmacro "x" (eval-args "x"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Displayed formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Their tags stay, for the HTML export, under names of their own (one with
;; tmhtml- at its start would redefine the tag itself). What is in them is
;; evaluated in display style, so that tables are not a level smaller, and as
;; a formula for the evaluation, which records its environment (html-formula,
;; see exec_with). A style may color them (math-colored of alt-colors.ts), but
;; not the number of equation-lab.

(define (tmhtml-display-body x)
  (with body `(with "html-formula" "display" "math-display" "true" ,x)
    `(if (provides "math-colored") (math-colored ,body) ,body)))

(define tmhtml-display-macros
  `((associate "equation*"
               (macro "x" (html-display ,(tmhtml-display-body '(arg "x")))))
    (associate "equations-base"
               (macro "x" (html-equations-base
                           ,(tmhtml-display-body '(arg "x")))))
    (associate "equation-lab"
               (macro "x" "n" (html-equation-lab
                               ,(tmhtml-display-body '(arg "x")) (arg "n"))))
    ,(tmhtml-env-macro 'html-display)
    ,(tmhtml-env-macro 'html-equations-base)
    ,(tmhtml-env-macro 'html-equation-lab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmhtml-env-patch)
  ;; FIXME: we should use the DRD here
  `(collection
    ,@(map tmhtml-env-macro
	   '(TeXmacs TeX LaTeX shown hrule item
	     chapter-title section-title subsection-title subsubsection-title
	     paragraph-title subparagraph-title
	     itemize itemize-minus itemize-dot itemize-arrow
	     enumerate enumerate-numeric enumerate-roman
	     enumerate-Roman enumerate-alpha enumerate-Alpha
	     description description-compact description-dash
	     description-aligned description-long description-paragraphs item*
	     strong em dfn code* samp kbd var abbr acronym
	     verbatim code tt underline overline strike-through
	     deleted marked fill-out
	     hidden-title doc-title-block
	     ;; arrays of LaTeX which TeXmacs does not define: kept for
	     ;; the LaTeX of MathJax, as they would otherwise vanish (#611)
	     gathered subarray
	     ;; these expand to nothing here, and only take space when typeset
	     phantom hphantom vphantom
             wide-float draw-over draw-under
             html-tag html-attr
	     html-div-style html-div-class html-style html-class
             html-javascript html-javascript-src html-video
	     web-title tmdoc-title tmdoc-flag tmdoc-license
	     tmdoc-title* tmdoc-title** tmdoc-copyright
	     hlink action hyper-link
             mouse-over-balloon mouse-over-balloon*
             hover-balloon hover-balloon*
             popup-balloon popup-balloon*))
    ,@tmhtml-display-macros
    ;; FIXME: should apply 'filter_style' to the environment
    ;; in an appropriate way to avoid adding the primitives below
    ,@(map tmhtml-env-macro
           '(shrink-inline
             binom tbinom dbinom choose ontop
             tfrac dfrac cfrac
             bmod pmod pod))))
