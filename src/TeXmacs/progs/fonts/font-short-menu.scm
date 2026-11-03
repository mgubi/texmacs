
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-short-menu.scm
;; DESCRIPTION : the text fonts which the focus bar offers for quick selection
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-short-menu)
  (:use (fonts fonts-opentype)))

;; The fonts of this table carry no mathematics of their own: choosing one
;; changes the text of the document and leaves formulas to the mathematical
;; font.  The fonts which do bring mathematics along are those profiled in
;; fonts-opentype.scm, and the menu lists them apart.
;;
;; A font is named by its MASTER, the second field of an entry of
;; TeXmacs/fonts/font-features.scm, since that is what the `font' environment
;; variable holds; `Fira Sans' belongs to the master `Fira' and `KpRoman' to
;; `Kepler'.  The keys are: menu (label, the name by default), family (a
;; family of the master, to ask the font database whether it is installed,
;; the name by default), file (a font file to look for instead, for the
;; fonts which TeXmacs ships and the database may ignore), kind (serif,
;; sans, mono or other, which decides the submenu), master (the master, when
;; the name is not it: two entries may share a master), face (the font
;; family, tt, the document is set in: the typewriter face of a master
;; which has a serif one) and variants (the variants of the design, as a
;; list of (label master) which starts with the design itself, see
;; fonts-opentype.scm).

(define text-font-table (make-ahash-table))
(define text-font-names (list))

(tm-define (text-font-declare name props)
  (when (not (ahash-ref text-font-table name))
    (set! text-font-names (rcons text-font-names name)))
  (ahash-set! text-font-table name props)
  (with v (assq 'variants props)
    (when v (font-variants-declare (cdr v)))))

(define-public-macro (define-text-font name . props)
  `(text-font-declare ,name ',props))

(define (text-font-attr name key default)
  (with p (assq key (or (ahash-ref text-font-table name) (list)))
    (if (and p (list-2? p)) (cadr p) default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-text-font "Alegreya")
(define-text-font "Antykwa Poltawskiego"
  (variants ("Regular" "Antykwa Poltawskiego")
            ("Light" "Antykwa Poltawskiego Light")
            ("Condensed" "Antykwa Poltawskiego Condensed")
            ("Expanded" "Antykwa Poltawskiego Expanded")))
(define-text-font "Antykwa Torunska"
  (variants ("Regular" "Antykwa Torunska")
            ("Light" "Antykwa Torunska Light")
            ("Condensed" "Antykwa Torunska Condensed")
            ("Light condensed" "Antykwa Torunska Condensed Light")))
(define-text-font "Baskerville")
(define-text-font "Cardo")
(define-text-font "Charter")
(define-text-font "Cochin")
(define-text-font "Crimson")
(define-text-font "DejaVu" (family "DejaVu Serif"))
(define-text-font "Didot")
(define-text-font "EB Garamond")
(define-text-font "Erewhon")
(define-text-font "Essays1743" (file "Essays1743"))
(define-text-font "Garamond")
(define-text-font "Gentium Plus")
(define-text-font "Georgia")
(define-text-font "Hoefler Text")
(define-text-font "IBM Plex" (family "IBM Plex Serif")
  (variants ("Regular" "IBM Plex") ("Text" "IBM Plex Text")
            ("Light" "IBM Plex Light") ("Medium" "IBM Plex Medium")))
(define-text-font "Iowan Old Style")
(define-text-font "Junicode")
(define-text-font "Kepler" (family "KpRoman") (menu "Kp Fonts"))
(define-text-font "Latin Modern Roman" (menu "Latin Modern"))
(define-text-font "Liberation" (family "Liberation Serif"))
(define-text-font "Libertinus" (family "Libertinus Serif"))
(define-text-font "Linux Libertine")
(define-text-font "Merriweather")
(define-text-font "NewComputerModern10" (menu "New Computer Modern"))
(define-text-font "Noto" (family "Noto Serif") (menu "Noto Serif"))
(define-text-font "Old Standard")
(define-text-font "Palatino")
(define-text-font "PT" (family "PT Serif") (menu "PT Serif"))
(define-text-font "Stix Two Text" (menu "STIX Two"))
(define-text-font "TeX Gyre Bonum" (menu "Bonum"))
(define-text-font "TeX Gyre Pagella" (menu "Pagella"))
(define-text-font "TeX Gyre Schola" (menu "Schola"))
(define-text-font "TeX Gyre Termes" (menu "Termes"))
(define-text-font "Times New Roman")
(define-text-font "Xits" (menu "XITS"))
(define-text-font "XCharter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sans serif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-text-font "Arial" (kind sans))
(define-text-font "Avenir" (kind sans))
(define-text-font "Cuprum" (kind sans))
(define-text-font "Fira" (family "Fira Sans") (kind sans))
(define-text-font "Futura" (kind sans))
(define-text-font "Gill" (family "Gill Sans") (menu "Gill Sans") (kind sans))
(define-text-font "Helvetica" (kind sans))
(define-text-font "Inter" (kind sans))
(define-text-font "Iwona" (kind sans)
  (variants ("Regular" "Iwona") ("Light" "Iwona Light")
            ("Condensed" "Iwona Condensed")
            ("Light condensed" "Iwona Condensed Light")))
(define-text-font "Kurier" (kind sans)
  (variants ("Regular" "Kurier") ("Light" "Kurier Light")
            ("Condensed" "Kurier Condensed")
            ("Light condensed" "Kurier Condensed Light")))
(define-text-font "Lato" (kind sans))
(define-text-font "Linux Biolinum" (kind sans))
(define-text-font "Lucida Grande" (kind sans))
(define-text-font "Open" (family "Open Sans") (menu "Open Sans") (kind sans))
(define-text-font "Optima" (kind sans))
(define-text-font "Roboto" (kind sans))
(define-text-font "Source" (family "Source Sans") (menu "Source Sans")
  (kind sans))
(define-text-font "TeX Gyre Adventor" (menu "Adventor") (kind sans))
(define-text-font "TeX Gyre Heros" (menu "Heros") (kind sans))
(define-text-font "Verdana" (kind sans))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typewriter and the rest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-text-font "Andale" (family "Andale Mono") (menu "Andale Mono")
  (kind mono))
(define-text-font "Courier New" (kind mono))
(define-text-font "IBM Plex Mono" (master "IBM Plex") (face "tt")
  (menu "IBM Plex") (kind mono)
  (variants ("Regular" "IBM Plex") ("Text" "IBM Plex Text")
            ("Light" "IBM Plex Light") ("Medium" "IBM Plex Medium")))
(define-text-font "Inconsolatazi4" (menu "Inconsolata") (kind mono))
(define-text-font "Latin Modern Mono" (face "tt")
  (menu "Latin Modern") (kind mono)
  (variants ("Regular" "Latin Modern Mono")
            ("Proportional" "Latin Modern Mono Proportional")
            ("Proportional light" "Latin Modern Mono Light")))
(define-text-font "Menlo" (kind mono))
(define-text-font "Monaco" (kind mono))
(define-text-font "TeX Gyre Cursor" (menu "Cursor") (kind mono))

(define-text-font "American Typewriter" (kind other))
(define-text-font "Chalkboard" (kind other))
(define-text-font "Chalkduster" (kind other))
(define-text-font "Marker Felt" (kind other))
(define-text-font "Meyne Textur" (file "meyne_textur") (kind other))
(define-text-font "Papyrus" (kind other))
(define-text-font "TeX Gyre Chorus" (menu "Chorus") (kind other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The menus, which only show what is installed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (text-font-installed? name)
  (with file (text-font-attr name 'file "")
    (if (!= file "")
        (font-exists-in-tt? file)
        (nnull? (font-database-styles (text-font-attr name 'family name))))))

(tm-define (text-font-list kind)
  (:synopsis "Installed text fonts of a given @kind, as (label master name)")
  ;; a font which an installed OpenType math font brings along is left out:
  ;; the menu offers it above, in the company of its mathematics
  (let* ((skip (opentype-math-companions))
         (keep? (lambda (name)
                  (and (== (text-font-attr name 'kind 'serif) kind)
                       (not (in? name skip))
                       (text-font-installed? name))))
         (l (list-filter text-font-names keep?))
         (labeled (map (lambda (name)
                         (list (text-font-attr name 'menu name)
                               (text-font-attr name 'master name) name))
                       l)))
    (list-sort labeled
               (lambda (a b) (string<=? (locase-all (car a))
                                        (locase-all (car b)))))))

(tm-define (text-font-variants name)
  (:synopsis "The variants of the text font @name which the menus offer")
  (with v (assq 'variants (or (ahash-ref text-font-table name) (list)))
    (font-variants-installed (if v (cdr v) (list)))))

(tm-define (text-font-face name)
  (:synopsis "The font family which the entry @name sets the document in")
  (text-font-attr name 'face "rm"))

;; The entries of the typewriter face of a master and of its serif face
;; differ by the font family of the document, and so does the entry of the
;; sans serif face of a master which a mathematical font brings along
(define (test-text-font? name master)
  (with fam (get-init "font-family")
    (and (test-init-font? master)
         (== (== fam "tt") (== (text-font-face name) "tt"))
         (not (and (== fam "ss")
                   (in? (get-init "font") (opentype-font-values)))))))

(tm-define (init-text-font name master)
  (:synopsis "Set the document in @master, a variant of the text font @name")
  (init-font (font-variant-value master))
  (when (!= (text-font-face name) "rm")
    (init-env "font-family" (text-font-face name))))

;; The entries of the fonts of a kind: a font with variants is a submenu.
;; (A submenu is expanded when it is opened, and a menu with arguments has
;; lost them by then ("widget expected"): one menu per kind.)
(tm-menu (text-font-entries l)
  (for (p l)
    (with vs (text-font-variants (caddr p))
      (assuming (null? vs)
        ((check (eval (car p)) "*" (test-text-font? (caddr p) (cadr p)))
         (init-text-font (caddr p) (cadr p))))
      (assuming (nnull? vs)
        (-> (eval (font-variants-label
                   (car p) (list-find vs (lambda (v) (test-text-font?
                                                      (caddr p) (cadr v))))))
            (for (v vs)
              ((check (eval (car v)) "*" (test-text-font? (caddr p) (cadr v)))
               (init-text-font (caddr p) (cadr v)))))))))

(tm-menu (text-font-serif-menu)
  (dynamic (text-font-entries (text-font-list 'serif))))

(tm-menu (text-font-sans-menu)
  (dynamic (text-font-entries (text-font-list 'sans))))

(tm-menu (text-font-mono-menu)
  (dynamic (text-font-entries (text-font-list 'mono))))

(tm-menu (text-font-other-menu)
  (dynamic (text-font-entries (text-font-list 'other))))

;; The fonts by design: in each submenu the fonts which bring their
;; mathematics along (the profiles of fonts-opentype.scm, with the
;; traditional Roman and Stix among the serif ones), then those which
;; change the text only
(tm-menu (document-serif-font-menu)
  (group "With mathematics")
  ("Roman" (init-font "roman" "roman"))
  (if (font-exists-in-tt? "STIX-Regular")
      ("Stix" (init-font "stix" "math-stix")))
  (link opentype-serif-font-menu)
  (assuming (nnull? (text-font-list 'serif))
    ---
    (group "Text only")
    (link text-font-serif-menu)))

(tm-menu (document-sans-font-menu)
  (assuming (nnull? (opentype-math-font-group-list "Sans serif"))
    (group "With mathematics")
    (link opentype-sans-font-menu))
  (assuming (and (nnull? (opentype-math-font-group-list "Sans serif"))
                 (nnull? (text-font-list 'sans)))
    ---)
  (assuming (nnull? (text-font-list 'sans))
    (group "Text only")
    (link text-font-sans-menu)))

(tm-menu (document-short-design-font-menu)
  (-> "Serif" (link document-serif-font-menu))
  (assuming (or (nnull? (opentype-math-font-group-list "Sans serif"))
                (nnull? (text-font-list 'sans)))
    (-> "Sans serif" (link document-sans-font-menu)))
  (assuming (nnull? (text-font-list 'mono))
    (-> "Typewriter" (link text-font-mono-menu)))
  (assuming (nnull? (text-font-list 'other))
    (-> "Decorative" (link text-font-other-menu)))
  (assuming (nnull? (opentype-math-font-group-list "Other"))
    (-> "Other OpenType math fonts" (link opentype-other-font-menu))))
