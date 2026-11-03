;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-features.scm
;; DESCRIPTION : the OpenType features of the current font
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-features)
  (:use (generic format-edit)
        (generic document-edit)))

;; A feature of an OpenType font replaces a glyph by another one: the old
;; style figures of onum, the small capitals of smcp, the stylistic sets.
;; The value of the font-features environment variable names the ones a
;; document wants, see apply_features in Graphics/Fonts/feature_font.cpp.
;; Only the features which replace one glyph by one other are applied, so
;; the ligatures of liga, the fractions of frac and the contextual forms of
;; calt are not proposed here.

(define font-feature-table
  '(("onum" "Old style figures")
    ("lnum" "Lining figures")
    ("tnum" "Tabular figures")
    ("pnum" "Proportional figures")
    ("zero" "Slashed zero")
    ("smcp" "Small capitals")
    ("c2sc" "Caps to small caps")
    ("case" "Capital forms")
    ("hist" "Historical forms")
    ("swsh" "Swashes")
    ("salt" "Stylistic alternates")
    ("ss01" "Stylistic set 1")
    ("ss02" "Stylistic set 2")
    ("ss03" "Stylistic set 3")
    ("ss04" "Stylistic set 4")
    ("ss05" "Stylistic set 5")))

(tm-define (font-feature-name tag)
  (:synopsis "The name of an OpenType feature, or the tag itself")
  (with l (assoc tag font-feature-table)
    (if l (cadr l) tag)))

(tm-define (font-features-proposed)
  (:synopsis "The features this menu proposes, in the order of the table")
  (map car font-feature-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What the font at the cursor is able to do
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (font-features-of-file file)
  (:synopsis "The features of one font file, among the ones we propose")
  (with l (font-available-features file)
    (list-filter (font-features-proposed) (lambda (tag) (in? tag l)))))

;; The font which draws the character before the cursor is the one the
;; typesetter chose for it: the rules of the `font' variable and the
;; companions of the profiles decide, which a search of the font database
;; by family and variant does not know. The editor names it as
;; "unicode:STIXTwoText-Regular10@600", with the size of the smart font
;; "Stix Two Text-rm-medium-right-10-600-smart" after the name of the file.
(define (debug-info-ref info key)
  (with l (list-find info (lambda (x) (and (pair? x) (== (car x) 'tuple)
                                           (pair? (cdr x)) (== (cadr x) key))))
    (if (and l (string? (caddr l))) (caddr l) "")))

(define (smart-font-size name)
  ;; the size, as a string, in the name of a smart font
  (with l (reverse (string-tokenize-by-char name #\-))
    (if (and (>= (length l) 3) (== (car l) "smart")) (caddr l) "")))

(tm-define (font-file-of-subfont sub size)
  (:synopsis "The font file in the name @sub of a font of size @size, or #f")
  (let* ((s (car (string-tokenize-by-char sub #\#)))
         (i (string-search-forwards "@" 0 s)))
    (and (string-starts? s "unicode:") (>= i 8)
         (with name (substring s 8 i)
           (if (and (!= size "") (string-ends? name size))
               (string-drop-right name (string-length size))
               name)))))

(tm-define (font-file-here)
  (:synopsis "The file of the font at the cursor, without its suffix")
  ;; "" for a font without features (the fonts of TeX), #f when the editor
  ;; cannot tell
  (with info (tree->stree (font-debug-info #f))
    (and (pair? info) (== (car info) 'tuple)
         (with sub (debug-info-ref (cdr info) "subfont-name")
           (and (!= sub "")
                (or (font-file-of-subfont
                     sub (smart-font-size (debug-info-ref (cdr info) "font")))
                    ""))))))

(tm-define (font-features-here)
  (:synopsis "The features of the font at the cursor, among the proposed ones")
  (with file (font-file-here)
    (if file
        (if (== file "") (list) (font-features-of-file file))
        ;; no character there: the font the database has for the variables
        (with l (font-logical-search (get-env "font") (get-env "font-family")
                                     (get-env "font-series")
                                     (get-env "font-shape"))
          (if (null? l) (list) (font-features-of-file (car l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading and setting the variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (features->list val)
  (list-filter (string-tokenize-comma val) (lambda (s) (!= s ""))))

(define (list->features l)
  (string-recompose l ","))

(tm-define (font-features-get)
  (:synopsis "The features asked for at the cursor, as a list of tags")
  (features->list (get-env "font-features")))

(tm-define (font-feature-on? tag)
  (:synopsis "Is this feature asked for at the cursor?")
  (in? tag (font-features-get)))

(define (font-features-without l tag)
  ;; a figure style excludes the other one, and so do the two cases
  (let* ((excl '(("onum" "lnum") ("lnum" "onum")
                 ("tnum" "pnum") ("pnum" "tnum")))
         (out (assoc tag excl))
         (l2 (if out (list-difference l (cdr out)) l)))
    (list-difference l2 (list tag))))

(tm-define (font-features-toggle tag)
  (:synopsis "Switch one feature on or off for the selection")
  (:check-mark "v" font-feature-on?)
  (with l (font-features-without (font-features-get) tag)
    (make-with "font-features"
               (list->features (if (font-feature-on? tag) l (rcons l tag))))))

(tm-define (font-features-get-global)
  (:synopsis "The features asked for by the document, as a list of tags")
  (features->list (get-init "font-features")))

(tm-define (font-feature-on-global? tag)
  (:synopsis "Is this feature asked for by the document?")
  (in? tag (font-features-get-global)))

(tm-define (font-features-toggle-global tag)
  (:synopsis "Switch one feature on or off for the whole document")
  (:check-mark "v" font-feature-on-global?)
  ;; from what the document asks for, not from what holds at the cursor,
  ;; which may be inside a text with features of its own
  (let* ((on? (font-feature-on-global? tag))
         (l (font-features-without (font-features-get-global) tag)))
    (if (and on? (null? l))
        (init-default "font-features")
        (init-env "font-features"
                  (list->features (if on? l (rcons l tag)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The menus, which propose what the font really has
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (font-features-menu setter on?)
  (with l (font-features-here)
    (if (null? l)
        (group "This font declares no feature"))
    (for (tag l)
      ((check (eval (font-feature-name tag)) "v" (on? tag))
       (setter tag)))))

(menu-bind text-font-features-menu
  (dynamic (font-features-menu font-features-toggle font-feature-on?)))

(menu-bind document-font-features-menu
  (dynamic (font-features-menu font-features-toggle-global
                               font-feature-on-global?))
  ---
  ("Default" (init-default "font-features")))
