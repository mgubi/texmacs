
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmarkdown-expand.scm
;; DESCRIPTION : the macros which are kept when a document is expanded for
;;               its conversion to Markdown
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A document which is exported is first expanded by the typesetter
;; (exec_markdown in src/Edit/Editor/edit_typeset.cpp), which gives the
;; numbers of the sections, of the theorems and of the figures, the text of
;; the references and of the citations, and the macros of the user. The
;; macros below are not expanded (their arguments are): the converter
;; (tmmarkdown.scm) knows them, and their expansion is layout which Markdown
;; cannot express. The same is done for Html, see tmhtml-expand.scm.

(texmacs-module (convert markdown tmmarkdown-expand)
  (:use (convert markdown tmmarkdown)))

(define (tmmarkdown-env-macro name)
  `(associate ,(symbol->string name)
              (xmacro "x" (eval-args "x"))))

;; the title of the document, as it is written: its expansion is a block of
;; layout, from which the authors and the date could not be told apart
(tm-define (tmmarkdown-doc-data t)
  (:secure #t)
  ;; (quoted: the typesetter evaluates what an extern returns)
  `(quote (markdown-doc-data ,@(tm-children (tm->stree t)))))

;; a formula or HTML of a file (the macros md-math and md-html of the style
;; markdown): its source, when its body is still the copy which was made
;; when it was read, and its body else. This is told here, before the
;; expansion, which would change the body and the copy
(define (tmmarkdown-md-source kind t)
  (with l (tm-children (tm->stree t))
    (cond ((null? l) "")
          ((and (== (length l) 3) (== (car l) (caddr l)))
           ;; (quoted: the typesetter evaluates what an extern returns)
           `(quote (md-kept ,(if (and (== kind "block")
                                      (tmmarkdown-table? (car l)))
                                 "table" kind)
                            ,(cadr l))))
          (else (car l)))))

(tm-define (tmmarkdown-md-inline-source t)
  (:secure #t)
  (tmmarkdown-md-source "inline" t))

(tm-define (tmmarkdown-md-block-source t)
  (:secure #t)
  (tmmarkdown-md-source "block" t))

(tm-define (tmmarkdown-env-patch)
  `(collection
    ,@(map tmmarkdown-env-macro
           '(TeXmacs TeX LaTeX hrule item
             ;; the titles of the sections, with their numbers
             part-title chapter-title section-title subsection-title
             subsubsection-title paragraph-title subparagraph-title
             appendix-title
             ;; lists
             itemize itemize-minus itemize-dot itemize-arrow
             enumerate enumerate-numeric enumerate-roman
             enumerate-Roman enumerate-alpha enumerate-Alpha
             description description-compact description-dash
             description-aligned description-long description-paragraphs
             item*
             ;; text markup
             strong em dfn code* samp kbd var abbr acronym
             verbatim code tt underline overline strike-through
             deleted marked
             hlink hlink* action hyper-link render-key
             ;; environments, with their names and numbers
             render-theorem render-remark render-exercise render-proof
             render-solution render-enunciation
             render-big-figure render-small-figure
             render-big-algorithm render-small-algorithm
             footnote render-bibitem
             quotation quote-env verse
             ;; code
             verbatim-code cpp-code python-code scm-code shell-code
             java-code javascript-code json-code julia-code r-code
             scala-code fortran-code octave-code scilab-code dot-code
             mmx-code pseudo-code render-code
             ;; mathematics, which is converted to LaTeX
             equation* equation-lab equations-base
             eqnarray eqnarray* align align* gather gather* multline
             multline* eqsplit eqsplit*
             shrink-inline binom tbinom dbinom choose ontop
             tfrac dfrac cfrac bmod pmod pod
             ;; the documentation of TeXmacs
             tmdoc-title tmdoc-title* tmdoc-title** tmdoc-copyright
             tmdoc-license tmdoc-flag
             html-tag html-attr html-div-style html-div-class html-style
             html-class
             ;; the macros of the style markdown
             md-loose md-fence md-image md-footnote md-footnote-text
             md-metadata md-meta md-h6))
    (associate "md-source"
               (xmacro "x" (extern "tmmarkdown-md-inline-source"
                                   (quote-arg "x"))))
    (associate "md-block-source"
               (xmacro "x" (extern "tmmarkdown-md-block-source"
                                   (quote-arg "x"))))
    (associate "doc-data"
               (xmacro "x" (extern "tmmarkdown-doc-data" (quote-arg "x"))))))
