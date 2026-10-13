;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fonts-opentype.scm
;; DESCRIPTION : profiles of OpenType math fonts and their text companions
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-opentype))

;; A profile records what the MATH table of a font cannot tell: the text
;; companions of the same design, whether math letters should come from
;; the math font itself or from the text italic, a bold math font, the
;; menu label. Family names are those of the TeXmacs font database. See
;; doc/opentype-math-fonts-survey.md.
;;
;; Keys: text, sans, mono (companion families; a list of alternatives
;; separated by commas, the first installed one is used), file (file name of the
;; math font without suffix, to test for its presence), letters (math or
;; text), bold-math (family of a bold math font), text-file (a file of the
;; text companion, for a companion which the font database may not know:
;; its directory is then added), family (the font family, rm or ss, the
;; text is set in, rm when absent), menu (label) and group
;; (Serif, Sans serif or Other: the section of the font menus, where the
;; labels follow the names LaTeX users know, Times for Termes and so on)
;; and variants (the variants of the text companion, see below; the key
;; stays in Scheme).
;;
;; A companion is named the way the `font' environment variable names one,
;; that is by its MASTER, the second field of an entry of
;; TeXmacs/fonts/font-features.scm, not by the family. The master of
;; `Fira Sans' is `Fira' and the master of `KpRoman' is `Kepler'; naming
;; the family instead makes the font selection fall back to the feature
;; distance and print "missing 'Fira Sans' master". The variant (roman,
;; sans serif, typewriter) picks the family inside the master, which is
;; why the three keys often repeat the same name.
;;
;; When two math fonts name the same text companion (Asana Math and TeX Gyre
;; Pagella Math are both Palladio designs), the first profile below is the
;; one that companion pulls in for formulas, so canonical pairings come
;; first.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The variants of a design
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A design may come in variants: the weights Light, Text and Medium of IBM
;; Plex, the condensed width of DejaVu. A variant is a master of its own
;; in TeXmacs/fonts/font-features.scm, with its italic and its bold, so that
;; a document set in it finds its emphasis there. The variants of a design
;; are a list of (label master), in the order of the menus; the first one,
;; "Regular", is the design itself, the base of the others.
;;
;; The families of a variant are those of TeXmacs/fonts/font-database.scm.
;; The condensed and expanded widths, the light weights of Antykwa
;; Poltawskiego and of Latin Modern Mono Prop come with a bold of their
;; own. The other weights have none, and their families list one which is
;; paired with them by hand, as the style Bold: IBM Plex Light with IBM
;; Plex SemiBold, IBM Plex Text and Medium with IBM Plex Bold, the light
;; weights of Iwona, Kurier and Antykwa Torunska with the bold of the
;; regular weight, and their light condensed ones with the condensed bold.
;; A face may so be listed in two families: the variables of a font hold
;; one series, and a style "Light Bold" could not be selected.

(define font-variant-bases (make-ahash-table))
(define font-variant-labels (make-ahash-table))
(define font-variant-masters (list))

(tm-define (font-variants-declare l)
  (:synopsis "Declare the masters of the list @l of (label master) as variants")
  (when (nnull? l)
    (with base (cadr (car l))
      (for (v (cdr l))
        (when (not (ahash-ref font-variant-bases (cadr v)))
          (set! font-variant-masters (rcons font-variant-masters (cadr v))))
        (ahash-set! font-variant-bases (cadr v) base)
        (ahash-set! font-variant-labels (cadr v) (car v))))))

(tm-define (font-variant-base master)
  (:synopsis "The design which @master is a variant of, or @master")
  (or (ahash-ref font-variant-bases master) master))

(tm-define (font-variants-of base)
  (:synopsis "The masters which are variants of the design @base")
  (list-filter font-variant-masters
               (lambda (m) (== (ahash-ref font-variant-bases m) base))))

(tm-define (font-variant-label master)
  (:synopsis "The label of the variant @master of its design, or #f")
  (ahash-ref font-variant-labels master))

(define (master-installed? master)
  (list-or (map (lambda (fam) (nnull? (font-database-styles fam)))
                (font-master->families master))))

(tm-define (font-variants-installed l)
  (:synopsis "The variants of @l to offer: none unless one besides the first is installed")
  (if (null? l) l
      (with r (list-filter (cdr l) (lambda (v) (master-installed? (cadr v))))
        (if (null? r) r (cons (car l) r)))))

(tm-define (font-variant-value master)
  (:synopsis "The value of `font' for a document set in @master")
  ;; formulas follow the math companion of the text font, and a variant is
  ;; a master without one: the companion of its design is named by a rule
  (let* ((base (font-variant-base master))
         (math (math-family-for-text base)))
    (if (or (== master base) (== math "")) master
        (string-append "math=" math "," master))))

(tm-define (font-variants-label label v)
  (:synopsis "The label of the submenu of the design @label, set in the variant @v")
  ;; a submenu has no check mark: the variant in use, or #f, is named
  (if v `(concat ,label " (" ,(car v) ")") label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define math-font-variants (make-ahash-table))

(tm-define (math-font-profile-declare name props)
  (with v (assq 'variants props)
    (when v
      (ahash-set! math-font-variants name (cdr v))
      (font-variants-declare (cdr v)))
    (math-font-profile-set
     name (list-filter props (lambda (x) (!= (car x) 'variants))))))

(define-public-macro (define-math-font-profile name . props)
  `(math-font-profile-declare ,name ',props))

(define-math-font-profile "Latin Modern Math"
  (file "latinmodern-math") (text "Latin Modern Roman")
  (sans "Latin Modern Sans") (mono "Latin Modern Mono")
  (letters "math") (menu "Latin Modern") (group "Serif"))

(define-math-font-profile "NewComputerModernMath"
  (file "NewCMMath-Regular") (text "NewComputerModern10")
  (sans "NewComputerModernSans10") (mono "NewComputerModernMono10")
  (letters "math") (bold-math "NewComputerModernMath")
  (menu "New Computer Modern") (group "Serif"))

(define-math-font-profile "TeX Gyre Pagella Math"
  (file "texgyrepagella-math") (text "TeX Gyre Pagella")
  (sans "TeX Gyre Heros") (mono "Inconsolatazi4, TeX Gyre Cursor")
  (letters "text") (menu "Palatino") (group "Serif"))

(define-math-font-profile "TeX Gyre Termes Math"
  (file "texgyretermes-math") (text "TeX Gyre Termes")
  (sans "TeX Gyre Heros") (mono "Inconsolatazi4, TeX Gyre Cursor")
  (letters "text") (menu "Times") (group "Serif"))

(define-math-font-profile "TeX Gyre Bonum Math"
  (file "texgyrebonum-math") (text "TeX Gyre Bonum")
  (sans "TeX Gyre Adventor") (mono "Inconsolatazi4, TeX Gyre Cursor")
  (letters "text") (menu "Bookman") (group "Serif"))

(define-math-font-profile "TeX Gyre Schola Math"
  (file "texgyreschola-math") (text "TeX Gyre Schola")
  (sans "TeX Gyre Heros") (mono "Inconsolatazi4, TeX Gyre Cursor")
  (letters "text") (menu "Schoolbook") (group "Serif"))

(define-math-font-profile "TeX Gyre DejaVu Math"
  (file "texgyredejavu-math") (text "DejaVu")
  (sans "DejaVu") (mono "DejaVu")
  (letters "math") (menu "DejaVu") (group "Serif")
  (variants ("Regular" "DejaVu") ("Condensed" "DejaVu Condensed")))

(define-math-font-profile "Stix Two Math"
  (file "STIXTwoMath-Regular") (text "Stix Two Text")
  (letters "math") (menu "STIX Two") (group "Serif"))

(define-math-font-profile "Libertinus Math"
  (file "LibertinusMath-Regular") (text "Libertinus")
  (sans "Libertinus") (mono "Libertinus")
  (letters "math") (menu "Libertinus") (group "Serif"))

(define-math-font-profile "KpMath"
  (file "KpMath-Regular") (text "Kepler")
  (sans "Kepler") (mono "KpMono")
  (letters "math") (bold-math "Kepler Math")
  (menu "Kp Fonts") (group "Serif"))

(define-math-font-profile "Erewhon Math"
  (file "Erewhon-Math") (text "Erewhon")
  (letters "math") (menu "Utopia") (group "Serif"))

(define-math-font-profile "XCharter Math"
  (file "XCharter-Math") (text "XCharter")
  (letters "math") (menu "Charter") (group "Serif"))

(define-math-font-profile "Euler Math"
  (file "Euler-Math") (text "TeX Gyre Pagella")
  (sans "TeX Gyre Heros") (mono "Inconsolatazi4, TeX Gyre Cursor")
  (letters "math") (menu "Euler") (group "Serif"))

(define-math-font-profile "Concrete Math"
  (file "Concrete-Math") (text "CMU Concrete")
  (letters "math") (menu "Concrete") (group "Serif"))

(define-math-font-profile "Fira Math"
  (file "FiraMath-Regular") (text "Fira")
  (sans "Fira") (mono "Fira")
  (letters "math") (menu "Fira") (group "Sans serif"))

;; KpMath-Sans calls its family KpMath, with the style Sans; the shipped
;; database lists it as the family KpMathSans, a master of its own, so that
;; a sans serif document (family ss) finds it rather than the KpSans text
;; faces. Hence no sans companion either.
(define-math-font-profile "KpMathSans"
  (file "KpMath-Sans") (text "Kepler") (family "ss") (mono "KpMono")
  (letters "math") (bold-math "KpMathSans")
  (menu "Kp Sans") (group "Sans serif"))

(define-math-font-profile "NewComputerModernSansMath"
  (file "NewCMSansMath-Regular") (text "NewComputerModernSans10")
  (text-file "NewCMSans10-Regular")
  (sans "NewComputerModernSans10") (mono "NewComputerModernMono10")
  (letters "math") (menu "Computer Modern Sans") (group "Sans serif"))

(define-math-font-profile "Lete Sans Math"
  (file "LeteSansMath") (text "Lete Sans Math")
  (letters "math") (menu "Lete Sans") (group "Sans serif"))

;; Noto Sans Math is a master of its own, as Fira Math is; its text is the
;; sans serif face of the master Noto, which also has Noto Serif
(define-math-font-profile "Noto Sans Math"
  (file "NotoSansMath-Regular") (text "Noto") (family "ss")
  (sans "Noto") (mono "Noto")
  (letters "math") (menu "Noto Sans") (group "Sans serif"))

(define-math-font-profile "XITS Math"
  (file "XITSMath-Regular") (text "Xits")
  (letters "math") (bold-math "XITS Math")
  (menu "XITS") (group "Other"))

(define-math-font-profile "Asana Math"
  (file "Asana-Math") (text "TeX Gyre Pagella")
  (letters "math") (menu "Asana") (group "Other"))

(define-math-font-profile "IBM Plex Math"
  (file "IBMPlexMath-Regular") (text "IBM Plex") (family "ss")
  (sans "IBM Plex") (mono "IBM Plex")
  (letters "math") (menu "IBM Plex") (group "Sans serif")
  (variants ("Regular" "IBM Plex") ("Text" "IBM Plex Text")
            ("Light" "IBM Plex Light") ("Medium" "IBM Plex Medium")
            ("Condensed" "IBM Plex Condensed")))

(define-math-font-profile "Garamond-Math"
  (file "Garamond-Math") (text "EB Garamond")
  (mono "Inconsolatazi4, TeX Gyre Cursor")
  (letters "math") (menu "Garamond") (group "Serif"))

(define-math-font-profile "OldStandard-Math"
  (file "OldStandard-Math") (text "Old Standard")
  (sans "TeX Gyre Heros") (mono "Inconsolatazi4, TeX Gyre Cursor")
  (letters "math") (menu "Old Standard") (group "Serif"))

(define-math-font-profile "GFS Neohellenic Math"
  (file "GFSNeohellenicMath") (text "GFS Neohellenic")
  (mono "Inconsolatazi4, TeX Gyre Cursor")
  (letters "math") (menu "GFS Neohellenic") (group "Sans serif"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus: the profiled math fonts which are installed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (opentype-math-font-installed? name)
  (let* ((file (math-font-profile-attr name "file"))
         (tfile (math-font-profile-attr name "text-file")))
    (and (!= file "") (font-exists-in-tt? file)
         (or (== tfile "") (font-exists-in-tt? tfile)))))

(tm-define (opentype-math-font-list)
  (:synopsis "Installed profiled math fonts as (label math-family text-family)")
  (with l (list-filter (math-font-profile-families) opentype-math-font-installed?)
    (list-sort (map (lambda (name)
                      (list (math-font-profile-attr name "menu") name
                            (math-font-profile-attr name "text")))
                    l)
               (lambda (a b) (string<=? (locase-all (car a))
                                        (locase-all (car b)))))))

(tm-define (opentype-math-font-group-list group)
  (:synopsis "Installed profiled math fonts of the menu section @group")
  (list-filter (opentype-math-font-list)
               (lambda (p) (== (math-font-profile-attr (cadr p) "group")
                               group))))

(tm-define (opentype-math-companions)
  (:synopsis "The text fonts which the installed math fonts bring along")
  ;; only those set as roman text: Noto Sans Math sets its text in Noto
  ;; Sans, and Noto Serif stays in the menu of text fonts
  (map caddr (list-filter (opentype-math-font-list)
                          (lambda (p) (== (opentype-font-family (cadr p))
                                          "rm")))))

(define (opentype-font-family math)
  (with fam (math-font-profile-attr math "family")
    (if (== fam "") "rm" fam)))

;; Formulas are set in the math companion of the text font, whatever the
;; math-font variable says, unless the text font is roman. A math font
;; which is not the companion of its text font (Euler Math and Asana Math
;; with Pagella, KpMath Sans with Kepler) is therefore given by a rule, and
;; so is the one of a variant of the text font, which is a master without
;; a profile: the optional argument is the master of a variant.
(tm-define (opentype-font-value math . opt)
  (with text (if (null? opt) (math-font-profile-attr math "text") (car opt))
    (if (== (math-family-for-text text) math) text
        (string-append "math=" math "," text))))

(tm-define (opentype-font-variants math)
  (:synopsis "The variants of the text of @math which the menus offer")
  (font-variants-installed (or (ahash-ref math-font-variants math) (list))))

(tm-define (opentype-font-values)
  (:synopsis "The values of `font' of the entries of the menus with mathematics")
  (append-map (lambda (p)
                (cons (opentype-font-value (cadr p))
                      (map (lambda (v) (opentype-font-value (cadr p) (cadr v)))
                           (opentype-font-variants (cadr p)))))
              (opentype-math-font-list)))

(define (tex-gyre-package math)
  (with text (math-font-profile-attr math "text")
    (and (string-starts? text "TeX Gyre ")
         (string-starts? math text)
         (string-append (locase-all (string-drop text 9)) "-font"))))

;; The menus below are built in loops, so their entries say which one is
;; checked with (check ...): the check mark a command declares is applied
;; to the arguments as written in the menu, (cadr p), not to their values
(define (test-opentype-font? math . opt)
  (with pack (and (null? opt) (tex-gyre-package math))
    (if pack
        ;; the TeX Gyre fonts go through the package of their mathematics
        (has-style-package? pack)
        (and (== (get-init "font") (apply opentype-font-value (cons math opt)))
             (== (get-init "font-family") (opentype-font-family math))))))

(tm-define (init-opentype-font math . opt)
  (:synopsis "Set the text and mathematics of the document in @math")
  ;; the optional argument is the master of a variant of the text
  (init-font (apply opentype-font-value (cons math opt)) math)
  (with fam (opentype-font-family math)
    (when (!= fam "rm") (init-env "font-family" fam))))

;; The entry of a font of the list @l: a submenu when its text has variants
(tm-menu (opentype-font-entries l)
  (for (p l)
    (with vs (opentype-font-variants (cadr p))
      (assuming (null? vs)
        ((check (eval (car p)) "*" (test-opentype-font? (cadr p)))
         (init-opentype-font (cadr p))))
      (assuming (nnull? vs)
        (-> (eval (font-variants-label
                   (car p) (list-find vs (lambda (v) (test-opentype-font?
                                                      (cadr p) (cadr v))))))
            (for (v vs)
              ((check (eval (car v)) "*"
                      (test-opentype-font? (cadr p) (cadr v)))
               (init-opentype-font (cadr p) (cadr v)))))))))

(tm-menu (opentype-math-font-menu)
  (for (p (opentype-math-font-list))
    ((check (eval (car p)) "*" (== (get-init "math-font") (cadr p)))
     (init-env "math-font" (cadr p)))))

;; A submenu is expanded when it is opened, after the menu which holds it,
;; and a menu with arguments has lost them by then ("widget expected"):
;; hence one menu without arguments for each section
(tm-menu (opentype-serif-font-menu)
  (dynamic (opentype-font-entries (opentype-math-font-group-list "Serif"))))

(tm-menu (opentype-sans-font-menu)
  (dynamic (opentype-font-entries (opentype-math-font-group-list "Sans serif"))))

(tm-menu (opentype-other-font-menu)
  (dynamic (opentype-font-entries (opentype-math-font-group-list "Other"))))

;; Text fonts without mathematics: a profile with companions only (no file,
;; so that it is not offered in the menus of math fonts), for the typewriter
;; text of a font which has no monospaced face of its own; otherwise the
;; closest monospaced font is taken, Linux Libertine Mono for Palatino.
;; The names are those of the menu of text fonts (font-short-menu.scm); the
;; families with a monospaced face of their own (DejaVu, IBM Plex, Noto, PT,
;; Liberation, Fira, Libertinus, Roboto, Source) find it without help
(for (name '("Palatino" "Baskerville" "Charter" "Cochin" "Didot" "Garamond"
             "Georgia" "Hoefler Text" "Iowan Old Style" "Times New Roman"
             "Alegreya" "Cardo" "Crimson" "Essays1743" "Junicode"
             "Merriweather" "Arial" "Avenir" "Futura" "Gill" "Helvetica"
             "Lucida Grande" "Optima" "Verdana" "Inter" "Lato" "Cuprum"
             "Antykwa Poltawskiego" "Antykwa Torunska" "Gentium Plus"
             "Iwona" "Kurier"))
  (math-font-profile-set name '((mono "Inconsolatazi4, TeX Gyre Cursor"))))

;; The variants of the same fonts take the same typewriter text, and the
;; condensed variants of IBM Plex and DejaVu, which have no typewriter face,
;; the one of their design (named by its family: a companion counts as
;; installed when the font database has styles for its name)
(for (name '("Antykwa Poltawskiego Light" "Antykwa Poltawskiego Condensed"
             "Antykwa Poltawskiego Expanded" "Antykwa Torunska Light"
             "Antykwa Torunska Condensed" "Antykwa Torunska Condensed Light"
             "Iwona Light" "Iwona Condensed" "Iwona Condensed Light"
             "Kurier Light" "Kurier Condensed" "Kurier Condensed Light"))
  (math-font-profile-set name '((mono "Inconsolatazi4, TeX Gyre Cursor"))))
(math-font-profile-set "IBM Plex Condensed" '((mono "IBM Plex Mono")))
(math-font-profile-set "DejaVu Condensed" '((mono "DejaVu Sans Mono")))
