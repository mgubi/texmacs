
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-weight.scm
;; DESCRIPTION : the weights of the font at the cursor
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-weight)
  ;; (font-short-menu declares the variants of the fonts without mathematics)
  (:use (fonts fonts-opentype) (fonts font-short-menu) (fonts font-features)
        (generic format-edit)))

;; The weight of a text is its font-series: thin, extralight, light, medium,
;; semibold, bold and heavy are the values the font selection knows. A font
;; has some of them, and the menu of the weights lists the ones which the
;; font at the cursor really has, from the lightest to the heaviest.
;;
;; A face is named here as the font selection names it, by the list
;; (master variant series shape) of the values of font, font-family,
;; font-series and font-shape which select it (logical-font-public).
;;
;; A document may be set in a variant of a design (fonts-opentype.scm): IBM
;; Plex Light, Iwona Condensed. A variant is a master with a regular and a
;; bold weight only, and its regular face is one of the faces of the design:
;; the light one, the condensed one. The menu then still offers the weights
;; of the design, in the width of the variant: Bold is the bold of the
;; variant, the other weights are faces of the design, and take a local
;; change of font. The variants which are weights the design has no
;; font-series for (Text and Medium of IBM Plex) are offered as weights too.

(define weight-names
  '(("thin" . "Thin") ("extralight" . "Extra light") ("light" . "Light")
    ("medium" . "Regular") ("semibold" . "Semibold") ("bold" . "Bold")
    ("heavy" . "Heavy")))

(define (weight-name w)
  (with p (assoc w weight-names) (if p (cdr p) (upcase-first w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (face-style face)
  ;; the (family style) of the font database which the face selects
  (with r (logical-font-search (apply logical-font-private face))
    (and (pair? r) (pair? (cdr r)) (string? (car r)) r)))

(define (style-face style)
  (with lf (logical-font-public (car style) (cadr style))
    (list (logical-font-family lf) (logical-font-variant lf)
          (logical-font-series lf) (logical-font-shape lf))))

(define (style-file style)
  (with l (font-database-search (car style) (cadr style))
    (and (pair? l) (string? (car l)) (car l))))

(define (face-file face)
  (and-with style (face-style face) (style-file style)))

(define (shape-as-italic shape)
  ;; an oblique face is the italic of a font which has no other
  (string-replace shape "slanted" "italic"))

(define (face-exact? face)
  ;; is there a font for the face itself, and not only a font close to it?
  (and-with style (face-style face)
    (with f (style-face style)
      (and (== (car f) (car face)) (== (caddr f) (caddr face))
           (== (shape-as-italic (cadddr f)) (shape-as-italic (cadddr face)))))))

(define (master-styles master)
  (append-map (lambda (fam)
                (map (lambda (sty) (list fam sty)) (font-database-styles fam)))
              (font-master->families master)))

(define (face-in-master face master)
  ;; the face of @master which is the same font file as @face, or #f
  ;; (with the font family of @face when it selects the file as well: the
  ;; style "Light 10 Regular" of a typewriter font has the family tt-10)
  (and-with file (face-file face)
    (and-with style (list-find (master-styles master)
                               (lambda (s) (== (style-file s) file)))
      (let* ((f (style-face style))
             (g (cons (car f) (cons (cadr face) (cddr f)))))
        (if (== (face-file g) file) g f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The face at the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (env-main-family)
  ;; the main family of the value of `font', without its rules
  (with l (list-filter (string-tokenize-by-char (get-env "font") #\,)
                       (lambda (s) (not (string-occurs? "=" s))))
    (if (null? l) (get-env "font") (cAr l))))

(define (env-font-with master)
  ;; the value of `font' with @master for main family
  (with l (string-tokenize-by-char (get-env "font") #\,)
    (string-recompose
     (rcons (list-filter l (lambda (s) (string-occurs? "=" s))) master) ",")))

(define (env-face)
  (list (env-main-family) (get-env "font-family")
        (get-env "font-series") (get-env "font-shape")))

;; The font which draws the text at the cursor may not be the one the
;; variables name: the sans serif text of a document in Pagella is set in
;; Heros, the companion of its profile, and a rule of `font' gives a part
;; another font. The editor knows the file of the font (font-file-here),
;; and the font database the faces of a file.
(define file-styles #f)

(define (file-base file)
  (with i (string-search-backwards "." (string-length file) file)
    (if (> i 0) (substring file 0 i) file)))

(define (styles-of-file file)
  (when (not file-styles)
    (set! file-styles (make-ahash-table))
    (for (fam (font-database-families))
      (for (sty (font-database-styles fam))
        (and-with f (style-file (list fam sty))
          (ahash-set! file-styles (file-base f)
                      (rcons (or (ahash-ref file-styles (file-base f)) (list))
                             (list fam sty)))))))
  (or (ahash-ref file-styles file) (list)))

(define (cursor-face)
  ;; the face of the font at the cursor; of the faces of a file, the one of
  ;; the main family of `font', or else the first one
  (let* ((here (env-face))
         (file (font-file-here))
         (same (and-with f (face-file here) (file-base f))))
    (if (or (not file) (== file "") (== file same)) here
        (let* ((faces (map style-face (styles-of-file file)))
               (own (list-find faces (lambda (f) (== (car f) (car here))))))
          (cond (own own)
                ((nnull? faces) (car faces))
                (else here))))))

(define (face-with-series face w)
  (list (car face) (cadr face) w (cadddr face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The weights
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (weight-variant? master face)
  ;; is the variant @master a weight which the design has no series for?
  (not (face-in-master (cons master (cdr face)) (font-variant-base master))))

(tm-define (font-weights-of face)
  (:synopsis "The weights to offer for the text set in @face")
  ;; a list of (label face), from the lightest to the heaviest; @face is
  ;; the list (master variant series shape)
  (let* ((master (car face))
         (base (font-variant-base master))
         (regular (face-with-series face "medium"))
         ;; the regular face of a variant, as a face of its design
         (coords (if (== master base) regular
                     (face-in-master regular base)))
         (in-base (or coords (cons base (cdr regular))))
         (own (and coords (caddr coords)))
         (target
          (lambda (w)
            (cond ((== master base)
                   (and (face-exact? (face-with-series face w))
                        (face-with-series face w)))
                  ((== w own) regular)
                  ((and (== w "bold")
                        (face-exact? (face-with-series face w)))
                   (face-with-series face w))
                  ((face-exact? (face-with-series in-base w))
                   (face-with-series in-base w))
                  (else #f))))
         ;; the variants which are weights of their own, between the
         ;; regular and the semibold ones, in the order of their declaration
         (extra
          (append-map
           (lambda (m)
             (with f (cons m (cdr (face-with-series in-base "medium")))
               (if (and (weight-variant? m f) (face-exact? f))
                   (list (list (font-variant-label m) f))
                   (list))))
           (font-variants-of base)))
         (plain
          (lambda (l)
            (append-map (lambda (w)
                          (with f (target w)
                            (if f (list (list (weight-name w) f)) (list))))
                        l))))
    ;; A weight keeps the font family of the text. The widths of Antykwa
    ;; Poltawskiego (Cond, Expd) are no shapes for the font selection but
    ;; values of the font family: their light weights, which are faces of
    ;; the design in such a family, are left out.
    (list-filter (append (plain '("thin" "extralight" "light" "medium"))
                         extra
                         (plain '("semibold" "bold" "heavy")))
                 (lambda (x) (== (cadr (cadr x)) (cadr face))))))

(tm-define (font-weights-here)
  (:synopsis "The weights to offer at the cursor")
  (font-weights-of (cursor-face)))

(define (face-current? face)
  (== (face-file face) (face-file (cursor-face))))

(tm-define (make-font-face face)
  (:synopsis "Set the selection, or the text to come, in @face")
  ;; only the variables which change are set
  (let* ((here (cursor-face))
         (l (append
             (if (== (car face) (car here)) (list)
                 (list "font" (if (== (car here) (env-main-family))
                                  (env-font-with (car face))
                                  (car face))))
             (if (== (cadr face) (cadr here)) (list)
                 (list "font-family" (cadr face)))
             (if (== (caddr face) (get-env "font-series")) (list)
                 (list "font-series" (caddr face)))
             (if (== (cadddr face) (cadddr here)) (list)
                 (list "font-shape" (cadddr face))))))
    (when (nnull? l) (make-multi-with l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (text-font-weight-menu)
  (with l (font-weights-here)
    (assuming (<= (length l) 1)
      ;; a font with one face in the font database, or which is not there
      ("Light" (make-with "font-series" "light"))
      ("Medium" (make-with "font-series" "medium"))
      ("Bold" (make-with "font-series" "bold")))
    (assuming (> (length l) 1)
      (for (x l)
        ((check (eval (car x)) "*" (face-current? (cadr x)))
         (make-font-face (cadr x)))))))
