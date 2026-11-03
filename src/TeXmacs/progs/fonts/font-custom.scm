
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

(define (read-font-designs)
  ;; the list of the preference, or #f when it cannot be read
  (with s (get-preference "font designs")
    (if (or (not (string? s)) (== s "") (== s "default")) (list)
        (with l (catch #t (lambda () (string->object s)) (lambda args #f))
          (and (list? l) l)))))

(tm-define (font-designs)
  (:synopsis "The saved choices of fonts, as a list of (name font family)")
  (list-filter (or (read-font-designs) (list)) valid-design?))

;; A preference which cannot be read is not written over: its value is put
;; aside, in the preference "font designs unreadable", where it can be
;; repaired by hand
(define (set-font-designs l)
  (when (not (read-font-designs))
    (set-preference "font designs unreadable"
                    (get-preference "font designs")))
  (set-preference "font designs" (object->string l)))

(define (design<=? a b)
  (string<=? (locase-all (car a)) (locase-all (car b))))

;; The preferences are read back with the double quotes and the backslashes
;; of a value unescaped once more than they were written: a name with one
;; of them would make the list unreadable at the next start
(tm-define (font-design-name? name)
  (:synopsis "May the saved fonts have the name @name?")
  (and (string? name) (!= name "")
       (not (string-occurs? "\"" name)) (not (string-occurs? "\\" name))))

(tm-define (font-design-store name font family)
  (:synopsis "Save the fonts @font, with the family @family, as @name")
  ;; #f, with a message, for a name which cannot be kept
  (if (font-design-name? name)
      (with l (list-filter (font-designs) (lambda (d) (!= (car d) name)))
        (set-font-designs (list-sort (cons (list name font family) l)
                                     design<=?))
        #t)
      (begin
        (when (and (string? name) (!= name ""))
          (set-message "A name of fonts cannot have \" or \\ in it"
                       "Fonts"))
        #f)))

(tm-define (font-design-forget name)
  (:synopsis "Forget the saved fonts @name")
  (set-font-designs (list-filter (font-designs)
                                 (lambda (d) (!= (car d) name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The fonts of the current document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (document-family)
  (get-init "font-family"))

(tm-define (font-design-custom?)
  (:synopsis "Does the document have fonts of its own for some of its parts?")
  ;; "math=Euler Math,TeX Gyre Pagella" is the entry Euler of the menus
  (with font (get-init "font")
    (and (string-occurs? "=" font)
         (nin? font (opentype-font-values)))))

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

;; The fonts of a value of `font' which the font database does not know.
;; The fonts of TeX (roman, stix, pagella), in lower case, are not in it.
(define (font-missing? name)
  ;; (the master of an unknown name is the name, with itself for family)
  (and (!= name "") (!= name (locase-all name))
       (not (list-or (map (lambda (fam) (nnull? (font-database-styles fam)))
                          (font-master->families name))))))

(tm-define (font-design-missing font)
  (:synopsis "The fonts of the value @font which are not installed")
  (list-remove-duplicates
   (list-filter (map (lambda (s) (cAr (string-tokenize-by-char s #\=)))
                     (string-tokenize-by-char font #\,))
                font-missing?)))

(tm-define (font-design-menu-label)
  (:synopsis "The label of a menu for the name of the fonts of the document")
  ;; the name of saved fonts is shown as it is, not translated nor read
  ;; as markup
  (if (font-design-current) `(verbatim ,(font-design-label))
      (font-design-label)))

(tm-define (font-design-use name)
  (:synopsis "Give the document the saved fonts @name")
  (and-with d (list-find (font-designs) (lambda (d) (== (car d) name)))
    (font-design-apply (font-design-parse (cadr d) (caddr d)))
    (with l (font-design-missing (cadr d))
      (when (nnull? l)
        (set-message (string-append "Not installed: "
                                    (string-recompose l ", "))
                     "Fonts")))))

(tm-define (font-design-save name)
  (:synopsis "Save the fonts of the document under @name")
  (:argument name "Name of these fonts")
  (when (font-design-store name (get-init "font") (document-family))
    (set-message (string-append "The fonts are saved as " name) "Fonts")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The entries of the menu of the fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (design-current? name)
  (with d (font-design-current) (and d (== (car d) name))))

(tm-menu (font-design-forget-menu)
  (for (d (font-designs))
    ((eval `(verbatim ,(car d))) (font-design-forget (car d)))))

(tm-menu (document-custom-font-menu)
  (assuming (nnull? (font-designs))
    (group "Saved fonts")
    (for (d (font-designs))
      ((check (eval `(verbatim ,(car d))) "*" (design-current? (car d)))
       (font-design-use (car d)))))
  (assuming (and (font-design-custom?) (not (font-design-current)))
    ((check "Custom" "*" #t) (open-font-design))
    ("Save these fonts" (interactive font-design-save)))
  (assuming (nnull? (font-designs))
    (-> "Forget saved fonts" (link font-design-forget-menu)))
  (assuming (or (nnull? (font-designs)) (font-design-custom?))
    ---))
