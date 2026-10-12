
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-custom.scm
;; DESCRIPTION : the fonts of a document as a named choice, kept across runs
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-custom))

;; The page of the design of the fonts (font-design.scm) gives a document
;; the fonts of its parts one by one: a text font with the mathematics of
;; another and the typewriter of a third. Such a choice is the value of
;; `font' with the fonts of its parts ("math=Stix Two Math,typewriter=Fira,
;; Gentium Plus") and is no font of the menus. Here it gets a name: the
;; button of the font in the focus bar says "Custom" for it, the menu of the
;; fonts has an entry for it, and it may be saved under a name of its own,
;; which the menu then offers for every document. The saved choices are in
;; the preference "font designs", a list of (name font family).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The saved choices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (valid-design? d)
  (and (list? d) (== (length d) 3)
       (string? (car d)) (string? (cadr d)) (string? (caddr d))))

(tm-define (font-designs)
  (:synopsis "The saved choices of fonts, as a list of (name font family)")
  (with s (get-preference "font designs")
    (if (or (not (string? s)) (== s "") (== s "default")) (list)
        (with l (catch #t (lambda () (string->object s)) (lambda args (list)))
          (if (list? l) (list-filter l valid-design?) (list))))))

(define (set-font-designs l)
  (set-preference "font designs" (object->string l)))

(define (design<=? a b)
  (string<=? (locase-all (car a)) (locase-all (car b))))

(tm-define (font-design-store name font family)
  (:synopsis "Save the fonts @font, with the family @family, as @name")
  (with l (list-filter (font-designs) (lambda (d) (!= (car d) name)))
    (set-font-designs (list-sort (cons (list name font family) l)
                                 design<=?))))

(tm-define (font-design-forget name)
  (:synopsis "Forget the saved fonts @name")
  (set-font-designs (list-filter (font-designs)
                                 (lambda (d) (!= (car d) name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The fonts of the current document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (document-family)
  (if (== (get-init "font-family") "ss") "ss" "rm"))

(tm-define (font-design-custom?)
  (:synopsis "Does the document have fonts of its own for some of its parts?")
  ;; "math=Euler Math,TeX Gyre Pagella" is the entry Euler of the menus
  (with font (get-init "font")
    (and (string-occurs? "=" font)
         (not (list-or (map (lambda (p)
                              (and (== (get-init "font") (opentype-font-value* p))
                                   #t))
                            (opentype-math-font-list)))))))

;; the value of `font' of a pair of the menus (opentype-font-value in
;; fonts-opentype.scm, which is private)
(define (opentype-font-value* p)
  (let* ((math (cadr p))
         (text (caddr p)))
    (if (== (math-family-for-text text) math) text
        (string-append "math=" math "," text))))

(tm-define (font-design-current)
  (:synopsis "The saved fonts which the document has, or #f")
  (let* ((font (get-init "font"))
         (fam (document-family)))
    (list-find (font-designs)
               (lambda (d) (and (== (cadr d) font) (== (caddr d) fam))))))

(tm-define (font-design-label)
  (:synopsis "The name of the fonts of the document, for the focus bar")
  (cond ((font-design-current) => car)
        ((font-design-custom?) "Custom")
        (else (upcase-first (font-family-main (get-init "font"))))))

(tm-define (font-design-use name)
  (:synopsis "Give the document the saved fonts @name")
  (and-with d (list-find (font-designs) (lambda (d) (== (car d) name)))
    (font-design-apply (font-design-parse (cadr d) (caddr d)))))

(tm-define (font-design-save name)
  (:synopsis "Save the fonts of the document under @name")
  (:argument name "Name of these fonts")
  (when (and (string? name) (!= name ""))
    (font-design-store name (get-init "font") (document-family))
    (set-message (string-append "The fonts are saved as " name) "Fonts")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The entries of the menu of the fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (design-current? name)
  (with d (font-design-current) (and d (== (car d) name))))

(tm-menu (font-design-forget-menu)
  (for (d (font-designs))
    ((eval (car d)) (font-design-forget (car d)))))

(tm-menu (document-custom-font-menu)
  (assuming (nnull? (font-designs))
    (group "Saved fonts")
    (for (d (font-designs))
      ((check (eval (car d)) "*" (design-current? (car d)))
       (font-design-use (car d)))))
  (assuming (and (font-design-custom?) (not (font-design-current)))
    ((check "Custom" "*" #t) (open-font-design))
    ("Save these fonts" (interactive font-design-save)))
  (assuming (nnull? (font-designs))
    (-> "Forget saved fonts" (link font-design-forget-menu)))
  (assuming (or (nnull? (font-designs)) (font-design-custom?))
    ---))
