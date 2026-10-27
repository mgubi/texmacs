;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fonts-test.scm
;; DESCRIPTION : tests of the font fallback, the font database, math fonts
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The characters which the font of the text does not have are taken from
;; other fonts (resolve and adjusted_dpi in Graphics/Fonts/smart_font.cpp).
;; The boxes are measured with the box-info primitive (see typeset-test.scm):
;; a CJK character fills the em square of its font, so its box is about 1fn
;; wide, while a character which no font has is drawn as its name in red,
;; "<#30AB>", several times wider.
;;
;; The math fonts which go with a text font (math-stix, math-termes...) are
;; compared with the formulas set when that text font is the text font too.
;;
;; The CJK fonts and STIX Two are those of the system: their checks are made
;; for each character when one of the fonts of the database with CJK
;; characters or Hangul draws it, and when the files of STIX Two are
;; installed. STIX and TeX Gyre come with TeXmacs.

(texmacs-module (check fonts-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (width t)
  ;; the logical width of the box of @t, in tmpt
  (with r (tree->stree (texmacs-expand `(box-info ,t "w")))
    (if (and (pair? r) (== (car r) 'tuple))
        (string->number (cadr r))
        0)))

(define (extents t)
  ;; the width and the height of the box of @t, in tmpt
  (with r (tree->stree (texmacs-expand `(box-info ,t "wh")))
    (if (and (pair? r) (== (car r) 'tuple))
        (map string->number (cdr r))
        r)))

(define (em) (width '(space "1fn")))

(define (em-wide? t tol)
  ;; whether @t is one em wide, up to the fraction @tol
  (let ((w (width t)) (e (em)))
    (and (> e 0) (<= (abs (- w e)) (* tol e)))))

(define cjk-families-cache #f)

(define (cjk-families)
  ;; the families of the database with CJK characters or Hangul
  (when (not cjk-families-cache)
    (set! cjk-families-cache
          (list-filter
           (font-database-families)
           (lambda (fam)
             (list-or
              (map (lambda (sty)
                     (with chs (font-database-characteristics fam sty)
                       (or (in? "CJK" chs) (in? "Hangul" chs))))
                   (font-database-styles fam)))))))
  cjk-families-cache)

(define (covered? c)
  ;; whether a font of the system has the character @c: as the text font,
  ;; one of the CJK fonts draws it, instead of its name in red
  (let loop ((l (cjk-families)))
    (cond ((null? l) #f)
          ((< (width `(with "font" ,(car l) ,c)) (* 1.5 (em))) #t)
          (else (loop (cdr l))))))

(define (check-em c tol . opt-t)
  ;; whether the full-width character @c (or the tree @opt-t holding it) is
  ;; one em wide in the default font, when a font of the system has it
  (with t (if (null? opt-t) c (car opt-t))
    (if (covered? c)
        (check-report (em-wide? t tol)
                      (string-append "one em wide: " (object->string t))
                      (string-append "width " (number->string (width t))
                                     ", em " (number->string (em))))
        (display* "  (no font has " c ": skipped)\n"))))

(define (in-buffer thunk)
  ;; @thunk in a new generic buffer, closed afterwards
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree '(document "")))
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Japanese and Korean through the font fallback (#196, items 1 and 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-kana)
  (check-group "kana in the default font")
  ;; hiragana (U+3040-309F) and katakana (U+30A0-30FF) were drawn as
  ;; their names, katakana having no range in get_unicode_range
  (check-em "<#3053>" 0.1)
  (check-em "<#3093>" 0.1)
  (check-em "<#30AB>" 0.1)
  (check-em "<#30CA>" 0.1)
  (check-em "<#30BF>" 0.1 '(with "language" "japanese" "<#30BF>"))
  (check-em "<#306F>" 0.1 '(with "font-series" "bold" "<#306F>"))
  (when (and (covered? "<#3053>") (covered? "<#30AB>"))
    (check-true (< (width "<#3053><#30AB>") (* 2.2 (em)))))
  ;; katakana phonetic extensions (U+31F0-31FF)
  (check-em "<#31F0>" 0.05))

(define (test-ideographs)
  (check-group "ideographs outside the basic block")
  ;; extension A (U+3400-4DBF), the compatibility ideographs (U+F900-FAFF)
  ;; and extension B (U+20000-2A6DF) were scaled by the x-heights as Latin
  ;; fallbacks
  (check-em "<#3400>" 0.05)
  (check-em "<#4DB5>" 0.05)
  (check-em "<#F900>" 0.05)
  (check-em "<#20B9F>" 0.05))

(define (test-hangul)
  (check-group "hangul in the default font")
  ;; the fallback font was scaled by the ratio of the x-heights of the
  ;; Latin letters, which made the syllables 20 to 30% smaller
  (check-em "<#D55C>" 0.05)
  (check-em "<#AD6D>" 0.05)
  (check-em "<#D55C>" 0.05 '(with "font" "pagella" "<#D55C>"))
  ;; the jamo (U+1100-11FF) and the compatibility jamo (U+3130-318F)
  (check-em "<#1100>" 0.05)
  (check-em "<#3131>" 0.05)
  ;; the Latin letters are not concerned
  (check-true (< (width "x") (* 0.7 (em)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STIX Two in the font database (#196, item 7)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stix-two-installed?)
  (or (font-exists-in-tt? "STIXTwoMath")
      (font-exists-in-tt? "STIXTwoMath-Regular")))

(define (test-stix-two)
  (check-group "STIX Two in the font database")
  (if (not (stix-two-installed?))
      (display "  (STIX Two is not installed: skipped)\n")
      (with fams (font-database-families)
        (check-true (in? "Stix Two Math" fams))
        (check-true (in? "Stix Two Text" fams))
        (check-true (nnull? (font-database-styles "Stix Two Text")))
        ;; the italic letters of the math font come from Stix Two Text, not
        ;; from the closest font to a family "Stix Two" which does not exist
        (check= (width '(with "math-font" "Stix Two Math" "mode" "math" "x"))
                (width '(with "font" "Stix Two Text" "font-shape" "italic"
                          "x"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STIX and TeX Gyre as math fonts of another text font (#196, items 3-5)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (math-only text t)
  ;; @t as a formula in the math font of the text font @text, with the
  ;; default text font
  `(with "math-font" ,(string-append "math-" text) "mode" "math" ,t))

(define (math-and-text text t)
  ;; @t as a formula in the math font of the text font @text, with that
  ;; text font
  `(with "font" ,text "math-font" ,(string-append "math-" text)
     "mode" "math" ,t))

(define (in-display t) `(with "math-display" "true" ,t))

(define (test-math-fonts)
  (check-group "math fonts of another text font")
  ;; With math-font=math-stix (or math-termes...) and the default text font,
  ;; the math font was named math-stix-..., which the hand-tuned code of
  ;; these fonts did not recognize: the wide accents of STIX were drawn as
  ;; red <rubber-hat-0>, its big operators were heavy, the big operators of
  ;; TeX Gyre stayed small in display style and their radicals lost their
  ;; overbar. These formulas are now as with the text font of the same name.
  (for-each
   (lambda (text)
     (for-each
      (lambda (t)
        (check= (extents (math-only text t)) (extents (math-and-text text t))))
      (list '(wide "x+y" "<wide-hat>")
            '(wide "x+y" "<wide-check>")
            '(big "oint")
            (in-display '(big "sum"))
            (in-display '(big "cup"))
            '(sqrt "x" "3")
            '(sqrt (frac "a" "b"))
            '(sqrt (concat "1+" (sqrt (concat "1+" (sqrt "1+x"))))))))
   (list "stix" "termes" "pagella" "bonum" "schola"))
  (check-true (< (width (math-only "stix" '(wide "x+y" "<wide-hat>")))
                 (* 1.2 (width (math-only "stix" "x+y"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fonts-test-failures)
  (:synopsis "Run the tests of the font fallback and return the failures")
  (check-suite "fonts")
  (in-buffer
   (lambda ()
     (test-kana)
     (test-ideographs)
     (test-hangul)
     (test-stix-two)
     (test-math-fonts)))
  (check-end))
