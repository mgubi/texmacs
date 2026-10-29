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
  (check-em "<#20B9F>" 0.05)
  ;; the CJK fallback of the default font is substituted by SimSun (or
  ;; FandolSong) when it is installed, as on Windows: the substitution lost
  ;; the CJK characters asked for, so that the fonts tried after SimSun were
  ;; Courier New and other fonts without CJK, and U+31F0 or U+F900, which
  ;; SimSun lacks, were drawn as their names (apply_substitutions in
  ;; font_select.cpp); the rules themselves are checked on any system by
  ;; tests/Graphics/Fonts/font_test.cpp
  (with v (logical-font-substitute '("roman" "cjk"))
    (if (== (car v) "roman")
        (display "  (no substitution of (roman cjk) is active: skipped)\n")
        (check-true (in? "cjk" v)))))

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
;; The Han characters of a Japanese or Korean text (#196, items 1 and 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ink t)
  ;; the ink box of @t: it tells which font drew a glyph
  (with r (tree->stree (texmacs-expand `(box-info ,t "LBRT")))
    (if (and (pair? r) (== (car r) 'tuple))
        (map string->number (cdr r))
        r)))

(define (check-language-font lan fam c)
  ;; whether the Han character @c of a text in the language @lan comes from
  ;; the font @fam of that language, as the kana or the Hangul next to it
  (if (or (== fam "roman") (not (covered? c)))
      (display* "  (no " lan " font for " c ": skipped)\n")
      (check-report (== (ink `(with "language" ,lan ,c))
                        (ink `(with "font" ,fam ,c)))
                    (string-append c " in a " lan " text from " fam)
                    (object->string (ink `(with "language" ,lan ,c))))))

(define (test-language-han)
  (check-group "the Han characters of a Japanese or Korean text")
  ;; the kanji of a Japanese text came from the Chinese font of the
  ;; fallback (Songti on macOS), next to kana from the Japanese font
  (check-language-font "japanese" (default-japanese-font) "<#6F22>")
  (check-language-font "japanese" (default-japanese-font) "<#9AA8>")
  (check-language-font "korean" (default-korean-font) "<#97D3>")
  ;; the language does not override a text font which has them
  (when (covered? "<#6F22>")
    (with fam (car (list-filter (cjk-families)
                                (lambda (f)
                                  (< (width `(with "font" ,f "<#6F22>"))
                                     (* 1.5 (em))))))
      (check= (ink `(with "font" ,fam "language" "japanese" "<#6F22>"))
              (ink `(with "font" ,fam "<#6F22>")))))
  ;; the fonts sys-japanese and sys-korean (cjk=<font>,roman, set by the
  ;; menus for a Japanese or Korean document) scaled their CJK characters
  ;; by the x-heights of the Latin letters, as Latin fallbacks: they are
  ;; now as large as in the font itself
  (for-each
   (lambda (x)
     (let ((sys (car x)) (fam (cadr x)) (c (caddr x)))
       (when (and (!= fam "roman") (covered? c))
         (check= (extents `(with "font" ,sys ,c))
                 (extents `(with "font" ,fam ,c))))))
   (list (list "sys-japanese" (default-japanese-font) "<#6F22>")
         (list "sys-japanese" (default-japanese-font) "<#3053>")
         (list "sys-korean" (default-korean-font) "<#D55C>")
         (list "sys-korean" (default-korean-font) "<#97D3>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The CJK fonts of Windows in the shipped font database (#538)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; On the first start, the local database is the shipped one, keeping the
;; entries whose files exist (font_database_filter in font_database.cpp).
;; Entries with the Mac file names only (MS Gothic.ttf) were dropped on
;; Windows, whose files are msgothic.ttc and others. These checks read the
;; shipped files, so that they are made on any system.

(define shipped-cache (list))

(define (shipped-lines name)
  ;; the lines of the file @name of TeXmacs/fonts
  (or (assoc-ref shipped-cache name)
      (with l (string-tokenize-by-char
               (string-load (system->url (string-append "$TEXMACS_PATH/fonts/"
                                                        name)))
               #\newline)
        (set! shipped-cache (cons (cons name l) shipped-cache))
        l)))

(define (shipped-key fam)
  ;; the start of the entries of the family @fam
  (string-append "((" (string-replace fam " " "\\ ") " "))

(define (shipped-file? fam file)
  ;; whether the shipped database gives the file @file to the family @fam:
  ;; the name as Windows lists it (font_database_collect compares the names
  ;; exactly), or without its suffix, @file.* in any case
  (let* ((k (shipped-key fam))
         (exact? (string-occurs? "." file))
         (f (string-append "(" (if exact? file (locase-all file))
                           (if exact? " " "."))))
    (list-or (map (lambda (l) (and (string-starts? l k)
                                   (string-occurs? f (if exact? l
                                                         (locase-all l)))))
                  (shipped-lines "font-database.scm")))))

(define (shipped-characteristics? fam)
  ;; whether the shipped characteristics describe the family @fam (for the
  ;; fallback, which looks for fonts with CJK characters)
  (with k (shipped-key fam)
    (list-or (map (lambda (l) (string-starts? l k))
                  (shipped-lines "font-characteristics.scm")))))

(define (check-shipped fam file)
  (check-report (shipped-file? fam file)
                (string-append "shipped database: " fam " in " file)
                "no entry")
  (check-report (shipped-characteristics? fam)
                (string-append "shipped characteristics: " fam)
                "no entry"))

(define (list-tail-from pred? l)
  ;; the tail of @l which starts with the first element satisfying @pred?
  (cond ((null? l) l)
        ((pred? (car l)) l)
        (else (list-tail-from pred? (cdr l)))))

(define (test-windows-fonts)
  (check-group "the CJK fonts of Windows in the shipped database")
  ;; the CJK fonts of every Windows 10 or later
  (for-each (lambda (p) (check-shipped (car p) (cadr p)))
            '(("MS Gothic" "msgothic.ttc") ("MS PGothic" "msgothic.ttc")
              ("Yu Gothic" "YuGothR.ttc") ("Yu Gothic UI" "YuGothM.ttc")
              ("Gulim" "gulim.ttc") ("MingLiU" "mingliu.ttc")
              ("PMingLiU" "mingliu.ttc") ("SimSun" "simsun.ttc")
              ("Microsoft YaHei" "msyh.ttc") ("Microsoft JhengHei" "msjh.ttc")
              ("Malgun Gothic" "malgun.ttf")))
  ;; the Japanese fonts of Windows, by preference (default_japanese_font_name
  ;; on Windows): each must be a family of the database, else the "Japanese
  ;; font first" step of the fallback of kana finds no font
  (with l (windows-cjk-font-candidates "japanese")
    (check-true (>= (length l) 10))
    (check= (and (pair? l) (cadr l)) "MS PMincho")
    (let loop ((l l))
      (when (and (pair? l) (pair? (cdr l)))
        (check-shipped (cadr l) (car l))
        (loop (cddr l)))))
  ;; on Windows, the Japanese font is one of them (Yu Gothic and MS Gothic
  ;; come with every Windows 10 or later) instead of roman
  ;; and its Regular style survives the filter of the database with the file
  ;; of the candidate as its first file (Yu Gothic Regular was YuGothM.ttc,
  ;; the Medium weight, when both files were given to it)
  (when (os-mingw?)
    (let* ((l (windows-cjk-font-candidates "japanese"))
           (fam (default-japanese-font))
           (tail (list-tail-from (lambda (x) (== x fam)) l)))
      (check-report (in? fam l) "the Japanese font of Windows" fam)
      (when (and (pair? tail) (< (length tail) (length l)))
        ;; the file of a candidate comes just before its family
        (let* ((file (list-ref l (- (length l) (length tail) 1)))
               (files (font-database-search fam "Regular")))
          (check-report (and (pair? files)
                             (string-starts? (locase-all (car files))
                                             (string-append (locase-all file)
                                                            ".")))
                        (string-append "the Regular file of " fam)
                        (object->string files)))))))

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
     (test-language-han)
     (test-windows-fonts)
     (test-stix-two)
     (test-math-fonts)))
  (check-end))
