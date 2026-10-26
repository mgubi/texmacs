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
;; when TeXmacs knows a Japanese or a Korean font of the system, and when the
;; files of STIX Two are installed. STIX and TeX Gyre come with TeXmacs.

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
  (if (== (default-japanese-font) "roman")
      (display "  (no Japanese font on this system: skipped)\n")
      (begin
        ;; hiragana (U+3040-309F) and katakana (U+30A0-30FF) were drawn as
        ;; their names, katakana having no range in get_unicode_range
        (check-true (em-wide? "<#3053>" 0.1))
        (check-true (em-wide? "<#3093>" 0.1))
        (check-true (em-wide? "<#30AB>" 0.1))
        (check-true (em-wide? "<#30CA>" 0.1))
        (check-true (em-wide? '(with "language" "japanese" "<#30BF>") 0.1))
        (check-true (em-wide? '(with "font-series" "bold" "<#306F>") 0.1))
        (check-true (< (width "<#3053><#30AB>") (* 2.2 (em)))))))

(define (test-hangul)
  (check-group "hangul in the default font")
  (if (== (default-korean-font) "roman")
      (display "  (no Korean font on this system: skipped)\n")
      (begin
        ;; the fallback font was scaled by the ratio of the x-heights of the
        ;; Latin letters, which made the syllables 20 to 30% smaller
        (check-true (em-wide? "<#D55C>" 0.05))
        (check-true (em-wide? "<#AD6D>" 0.05))
        (check-true (em-wide? '(with "font" "pagella" "<#D55C>") 0.05))
        ;; the Latin letters are not concerned
        (check-true (< (width "x") (* 0.7 (em)))))))

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
     (test-hangul)
     (test-stix-two)
     (test-math-fonts)))
  (check-end))
