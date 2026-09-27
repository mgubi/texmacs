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
;; Keys: text, sans, mono (companion families), file (file name of the
;; math font without suffix, to test for its presence), letters (math or
;; text), bold-math (family of a bold math font), text-file (a file of the
;; text companion, for a companion which the font database may not know:
;; its directory is then added), family (the font family, rm or ss, the
;; text is set in, rm when absent), menu (label) and group
;; (Serif, Sans serif or Other: the section of the font menus, where the
;; labels follow the names LaTeX users know, Times for Termes and so on).
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

(define-public-macro (define-math-font-profile name . props)
  `(math-font-profile-set ,name ',props))

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
  (sans "TeX Gyre Heros") (mono "TeX Gyre Cursor")
  (letters "text") (menu "Palatino") (group "Serif"))

(define-math-font-profile "TeX Gyre Termes Math"
  (file "texgyretermes-math") (text "TeX Gyre Termes")
  (sans "TeX Gyre Heros") (mono "TeX Gyre Cursor")
  (letters "text") (menu "Times") (group "Serif"))

(define-math-font-profile "TeX Gyre Bonum Math"
  (file "texgyrebonum-math") (text "TeX Gyre Bonum")
  (sans "TeX Gyre Adventor") (mono "TeX Gyre Cursor")
  (letters "text") (menu "Bookman") (group "Serif"))

(define-math-font-profile "TeX Gyre Schola Math"
  (file "texgyreschola-math") (text "TeX Gyre Schola")
  (sans "TeX Gyre Heros") (mono "TeX Gyre Cursor")
  (letters "text") (menu "Schoolbook") (group "Serif"))

(define-math-font-profile "TeX Gyre DejaVu Math"
  (file "texgyredejavu-math") (text "DejaVu")
  (sans "DejaVu") (mono "DejaVu")
  (letters "math") (menu "DejaVu") (group "Serif"))

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
  (sans "TeX Gyre Heros") (mono "TeX Gyre Cursor")
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

(define-math-font-profile "XITS Math"
  (file "XITSMath-Regular") (text "Xits")
  (letters "math") (bold-math "XITS Math")
  (menu "XITS") (group "Other"))

(define-math-font-profile "Asana Math"
  (file "Asana-Math") (text "TeX Gyre Pagella")
  (letters "math") (menu "Asana") (group "Other"))

(define-math-font-profile "IBM Plex Math"
  (file "IBMPlexMath-Regular") (text "IBM Plex")
  (sans "IBM Plex") (mono "IBM Plex")
  (letters "math") (menu "IBM Plex") (group "Other"))

(define-math-font-profile "Garamond-Math"
  (file "Garamond-Math") (text "EB Garamond")
  (letters "math") (menu "Garamond") (group "Other"))

(define-math-font-profile "OldStandard-Math"
  (file "OldStandard-Math") (text "Old Standard")
  (letters "math") (menu "Old Standard") (group "Other"))

(define-math-font-profile "GFS Neohellenic Math"
  (file "GFSNeohellenicMath") (text "GFS Neohellenic")
  (letters "math") (menu "GFS Neohellenic") (group "Other"))

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
  (map caddr (opentype-math-font-list)))

(define (opentype-font-family math)
  (with fam (math-font-profile-attr math "family")
    (if (== fam "") "rm" fam)))

;; Formulas are set in the math companion of the text font, whatever the
;; math-font variable says, unless the text font is roman. A math font
;; which is not the companion of its text font (Euler Math and Asana Math
;; with Pagella, KpMath Sans with Kepler) is therefore given by a rule.
(define (opentype-font-value math)
  (with text (math-font-profile-attr math "text")
    (if (== (math-family-for-text text) math) text
        (string-append "math=" math "," text))))

(define (tex-gyre-package math)
  (with text (math-font-profile-attr math "text")
    (and (string-starts? text "TeX Gyre ")
         (string-starts? math text)
         (string-append (locase-all (string-drop text 9)) "-font"))))

(define (test-opentype-font? math)
  (with pack (tex-gyre-package math)
    (if pack
        ;; the TeX Gyre fonts go through the package of their mathematics
        (has-style-package? pack)
        (and (== (get-init "font") (opentype-font-value math))
             (== (get-init "font-family") (opentype-font-family math))))))

(tm-define (init-opentype-font math)
  (:synopsis "Set the text and mathematics of the document in @math")
  (:check-mark "*" test-opentype-font?)
  (init-font (opentype-font-value math) math)
  (with fam (opentype-font-family math)
    (when (!= fam "rm") (init-env "font-family" fam))))

(tm-menu (opentype-math-font-menu)
  (for (p (opentype-math-font-list))
    ((eval (car p)) (init-env "math-font" (cadr p)))))

(tm-menu (opentype-font-group-menu group)
  (for (p (opentype-math-font-group-list group))
    ((eval (car p)) (init-opentype-font (cadr p)))))

(tm-menu (opentype-font-menu)
  (assuming (nnull? (opentype-math-font-group-list "Serif"))
    (group "Serif text and mathematics")
    (dynamic (opentype-font-group-menu "Serif")))
  (assuming (nnull? (opentype-math-font-group-list "Sans serif"))
    (group "Sans serif text and mathematics")
    (dynamic (opentype-font-group-menu "Sans serif")))
  (assuming (nnull? (opentype-math-font-group-list "Other"))
    (-> "Other OpenType math fonts"
        (dynamic (opentype-font-group-menu "Other")))))
