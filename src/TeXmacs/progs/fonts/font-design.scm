
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-design.scm
;; DESCRIPTION : a page with samples to choose the fonts of a document
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-design)
  (:use (fonts fonts-opentype) (fonts font-short-menu) (fonts font-custom)
        (generic document-edit)))

;; The fonts of a document are a design decision, and the menus only name
;; the fonts.  "Font design" opens a page (tmfs://font-design/<document>)
;; with a tab for each part of a document: the text and the mathematics
;; together, the text, the mathematics, the sans serif and typewriter text,
;; and the blackboard bold, calligraphic and fraktur letters of the
;; formulas.  A tab lists the fonts its part may have, each with a sample
;; of that part, a few words and a button to choose it.  The choice is
;; shown at the top of the page, in the fonts themselves, and is given to
;; the document with "Use for the document".
;;
;; The samples are pictures (TeXmacs/misc/font-samples, made by
;; font-design-make-samples): the page of some eighty fonts would otherwise
;; load every one of them, each a file to fetch in the browser.  Only the
;; fonts of the choice are loaded.  A font which does not come with TeXmacs
;; has no picture: its sample is made on request, in the cache of the home
;; directory.
;;
;; The choice is the value of the `font' variable, a main font preceded by
;; the fonts of its parts, "cal=TeX Gyre Termes,math=Stix Two Math,
;; typewriter=Fira,Gentium Plus" (see smart_font_rep::resolve and
;; logical-font-family* in font-new-widgets.scm).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The descriptions, by the name of the menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define font-design-descriptions (make-ahash-table))

(define-public-macro (font-design-describe name text)
  `(ahash-set! font-design-descriptions ,name ,text))

(font-design-describe "Roman"
  "The Computer Modern of TeX, in the fonts TeXmacs has always used: the look of mathematical papers since the 1980s.")
(font-design-describe "Latin Modern"
  "Computer Modern as an OpenType family, light and wide, with a mathematical font of the same drawing.")
(font-design-describe "New Computer Modern"
  "Computer Modern once more, extended to every mathematical symbol of Unicode, with a real bold mathematical font.")
(font-design-describe "Times"
  "TeX Gyre Termes, a Times design: narrow and economical, the font of many journals.")
(font-design-describe "Palatino"
  "TeX Gyre Pagella, a Palatino design: calligraphic and open, wide enough for long lines.")
(font-design-describe "Bookman"
  "TeX Gyre Bonum, a Bookman design: sturdy and wide, with a large x-height.")
(font-design-describe "Schoolbook"
  "TeX Gyre Schola, a Century Schoolbook design made for legibility.")
(font-design-describe "DejaVu"
  "Wide and very legible on the screen, with sans serif, typewriter and condensed faces of its own.")
(font-design-describe "STIX Two"
  "The font of the scientific publishers, a Times-like design redrawn in 2016 with the most complete set of mathematical symbols.")
(font-design-describe "Libertinus"
  "The successor of Linux Libertine: a warm, slightly condensed serif, with a sans serif and a typewriter face.")
(font-design-describe "Kp Fonts"
  "A complete family inspired by the typefaces of the Imprimerie nationale, light in colour, with many alternates.")
(font-design-describe "Utopia"
  "Erewhon, an extended Utopia: a crisp transitional design.")
(font-design-describe "Charter"
  "XCharter, an extended Bitstream Charter: sturdy and open, good at small sizes and on the screen.")
(font-design-describe "Euler"
  "Hermann Zapf's upright mathematics, in the hand of a mathematician at the blackboard, with Palatino for the text.")
(font-design-describe "Concrete"
  "Knuth's slab serif of Concrete Mathematics, dark and even, with Euler-like mathematics.")
(font-design-describe "Garamond"
  "EB Garamond, the free revival of the sixteenth century design, with a mathematical font to go with it.")
(font-design-describe "Old Standard"
  "A Modern face of the kind of the scientific books of the nineteenth century.")
(font-design-describe "Fira"
  "Mozilla's humanist sans serif and its mathematical companion, the usual choice for slides.")
(font-design-describe "Kp Sans"
  "The sans serif of the Kp fonts, with sans serif mathematics.")
(font-design-describe "Computer Modern Sans"
  "The sans serif of Computer Modern, with a sans serif mathematical font which has all the symbols.")
(font-design-describe "Lete Sans"
  "A sans serif mathematical font designed to go with Lato, with a bold weight.")
(font-design-describe "Noto Sans"
  "Google's family for every script of Unicode, with a sans serif mathematical font.")
(font-design-describe "IBM Plex"
  "IBM's corporate family, with serif, sans serif and typewriter faces, eight weights and a complete mathematical font.")
(font-design-describe "GFS Neohellenic"
  "The Greek sans serif of the Greek Font Society, round and light, with its mathematics.")
(font-design-describe "XITS"
  "A Times design derived from the first STIX fonts, with a bold mathematical font.")
(font-design-describe "Asana"
  "A Palatino design for mathematics with a larger set of symbols than Pagella.")
(font-design-describe "Antykwa Poltawskiego"
  "Adam Poltawski's Polish roman of the 1920s, lively, with distinctive g, w and y.")
(font-design-describe "Antykwa Torunska"
  "Zygfryd Gardzielewski's roman from Torun, with wavy serifs and flared strokes.")
(font-design-describe "Gentium Plus"
  "SIL's elegant roman for the Latin, Greek and Cyrillic scripts with all their diacritics.")
(font-design-describe "Linux Libertine"
  "A warm, slightly condensed serif for books, the predecessor of Libertinus.")
(font-design-describe "Noto Serif"
  "The serif of Google's Noto family.")
(font-design-describe "Iwona"
  "Malgorzata Budyta's Polish humanist sans serif, slightly condensed, in four weights.")
(font-design-describe "Kurier"
  "The wider sister of Iwona, drawn for newspapers, with ink traps.")
(font-design-describe "Heros"
  "TeX Gyre Heros, a Helvetica design.")
(font-design-describe "Adventor"
  "TeX Gyre Adventor, an Avant Garde design: geometric, with round letters.")
(font-design-describe "Linux Biolinum"
  "The sans serif companion of Linux Libertine.")
(font-design-describe "Cursor"
  "TeX Gyre Cursor, a Courier design: the typewriter of the typewriters, thin and wide.")
(font-design-describe "Inconsolata"
  "A typewriter font drawn for program listings, narrow and clear.")
(font-design-describe "Chorus"
  "TeX Gyre Chorus, a Zapf Chancery design: a calligraphic italic for invitations and titles.")

(define (font-design-description name)
  (or (ahash-ref font-design-descriptions name) ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The catalogue: for each part of a document, the fonts it may have
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The page shows the fonts for one part at a time, its "tab": "text",
;; "math", "sansserif", "typewriter", "bbb", "cal" or "frak". The pairs of
;; the menus (text and mathematics together) are the catalogue "all",
;; which has no tab: a pair is its text font with the mathematics "as the
;; text font". An entry of a tab is an association list with
;;   tab     the part
;;   name    the name of the menus
;;   text    the master of the text font (the value of `font')
;;   math    the family of the mathematical font, or #f
;;   family  ss when the text is the sans serif of its master
;;   id      the name of the picture of the sample, unique in the tab

(tm-define font-design-tabs
  '("text" "math" "sansserif" "typewriter" "bbb" "cal" "frak"))

(define tab-names
  '(("all" . "Text and mathematics") ("text" . "Text")
    ("math" . "Mathematics") ("sansserif" . "Sans serif")
    ("typewriter" . "Typewriter") ("bbb" . "Blackboard bold")
    ("cal" . "Calligraphic") ("frak" . "Fraktur")))

(define (tab-name tab)
  (with p (assoc tab tab-names) (if p (cdr p) tab)))

(define (entry-ref e key)
  (with p (assq key e) (and p (cdr p))))

(define (font-design-slug s)
  (with safe (lambda (c)
               (cond ((char-alphabetic? c) (char-downcase c))
                     ((char-numeric? c) c)
                     (else #\-)))
    (list->string (map safe (string->list s)))))

(define (make-entry tab name text math family)
  `((tab . ,tab) (name . ,name) (text . ,text) (math . ,math)
    (family . ,family)
    (id . ,(string-append tab "-" (font-design-slug name)))))

;; the value which names the font of an entry for its part
(define (entry-value e)
  (if (in? (entry-ref e 'tab) '("math" "bbb" "cal" "frak"))
      (entry-ref e 'math)
      (entry-ref e 'text)))

(define (master-has-feature? master feature)
  (list-or (map (lambda (fam) (in? feature (font-family-features fam)))
                (font-master->families master))))

;; the pairs of the menus, as (name text math family), by group
(define (pairs group)
  (map (lambda (p)
         (with fam (math-font-profile-attr (cadr p) "family")
           (list (car p) (caddr p) (cadr p) (if (== fam "") "rm" fam))))
       (opentype-math-font-group-list group)))

(define (serif-pairs)
  (cons (list "Roman" "roman" "roman" "rm") (pairs "Serif")))

;; the text fonts of the menus, as (name text #f family); a font with the
;; name of a pair says what it is: the serif faces of IBM Plex next to the
;; pair IBM Plex, which is sans serif; the Palatino of macOS next to the
;; pair Palatino, which is TeX Gyre Pagella
(define (pair-names)
  (map car (append (serif-pairs) (pairs "Sans serif") (pairs "Other"))))

(define (texts kind)
  (with taken (pair-names)
    (map (lambda (p)
           (list (cond ((nin? (car p) taken) (car p))
                       ;; the serif of a master whose pair is sans serif
                       ((master-shipped? (cadr p))
                        (string-append (car p) " Serif"))
                       (else (string-append (car p) " (system)")))
                 (cadr p) #f (if (== kind 'sans) "ss" "rm")))
         (text-font-list kind))))

(define (as-entries tab l)
  (map (lambda (x) (apply make-entry (cons tab x))) l))

;; the first of the fonts with the same value of @key (a text font which
;; two pairs share, Pagella for Palatino and Euler, is listed once, under
;; the name of the pair whose mathematics it brings along)
(define (unique l key)
  (let loop ((l l) (seen (list)) (r (list)))
    (cond ((null? l) (reverse r))
          ((in? (key (car l)) seen) (loop (cdr l) seen r))
          (else (loop (cdr l) (cons (key (car l)) seen) (cons (car l) r))))))

(define (own-math-first l)
  (append (list-filter l (lambda (x) (== (math-family-for-text (cadr x))
                                         (caddr x))))
          (list-filter l (lambda (x) (!= (math-family-for-text (cadr x))
                                         (caddr x))))))

(define (by-name l)
  (list-sort l (lambda (a b) (string<=? (locase-all (car a))
                                        (locase-all (car b))))))

(define (text-key x) (list (cadr x) (cadddr x)))

(define (text-fonts)
  ;; (title . fonts): the text fonts of the pairs, then those of text only
  (list (cons "Serif"
              (by-name (unique (append (own-math-first (serif-pairs))
                                       (own-math-first (pairs "Other"))
                                       (texts 'serif))
                               text-key)))
        (cons "Sans serif"
              (by-name (unique (append (own-math-first (pairs "Sans serif"))
                                       (texts 'sans))
                               text-key)))
        (cons "Decorative" (texts 'other))))

(define (math-fonts roman?)
  (list (cons "Serif" (unique (if roman? (serif-pairs) (pairs "Serif"))
                              caddr))
        (cons "Sans serif" (unique (pairs "Sans serif") caddr))
        (cons "Other" (unique (pairs "Other") caddr))))

(define (all-text-fonts)
  ;; one for each master (not the decorative ones); the sans serif ones
  ;; first: IBM Plex stands for its master rather than IBM Plex Serif
  (unique (append (cdr (cadr (text-fonts))) (cdr (car (text-fonts)))) cadr))

(define (variant-fonts feature kind)
  ;; the masters with a sans serif or a typewriter face
  (by-name
   (unique (append (texts kind)
                   (list-filter (all-text-fonts)
                                (lambda (x)
                                  (and (!= (cadr x) "roman")
                                       (master-has-feature? (cadr x)
                                                            feature)))))
           cadr)))

(define (sections tab l)
  (list-filter (map (lambda (s) (cons (car s) (as-entries tab (cdr s)))) l)
               (lambda (s) (nnull? (cdr s)))))

(tm-define (font-design-sections tab)
  (:synopsis "The fonts of the tab @tab of the page, by section")
  (cond ((== tab "all")
         (sections tab (list (cons "Serif" (serif-pairs))
                             (cons "Sans serif" (pairs "Sans serif"))
                             (cons "Other" (pairs "Other")))))
        ((== tab "text") (sections tab (text-fonts)))
        ((== tab "math") (sections tab (math-fonts #t)))
        ((== tab "sansserif")
         (sections tab (list (cons "" (variant-fonts "sansserif" 'sans)))))
        ((== tab "typewriter")
         (sections tab (list (cons "" (variant-fonts "mono" 'mono)))))
        ((in? tab '("bbb" "cal" "frak")) (sections tab (math-fonts #f)))
        (else (list))))

(tm-define (font-design-entries tab)
  (append-map cdr (font-design-sections tab)))

(tm-define (font-design-find-entry tab id)
  (list-find (font-design-entries tab) (lambda (e) (== (entry-ref e 'id) id))))

;; the name of the menus for the value of a part
(define (font-design-value-name part val)
  (with e (list-find (font-design-entries part)
                     (lambda (e) (== (entry-value e) val)))
    (if e (entry-ref e 'name) val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The choice: the fonts of the parts, and the value of `font' for them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A choice is an association list from the parts "text", "math",
;; "sansserif", "typewriter", "bbb", "cal" and "frak" to their fonts, and
;; from "family" to rm or ss; a part which is not there follows the text.

(define extra-parts '("frak" "cal" "bbb" "math" "typewriter" "sansserif"))

(define (choice-ref c part)
  (with p (assoc part c) (and p (cdr p))))

(define (choice-set c part val)
  (with r (list-filter c (lambda (p) (!= (car p) part)))
    (if val (rcons r (cons part val)) r)))

(tm-define (font-design-parse font family)
  (:synopsis "The choice which the values @font and @family of a document are")
  (let* ((items (map (lambda (s) (string-tokenize-by-char s #\=))
                     (string-tokenize-by-char font #\,)))
         (c (list (cons "family" (if (== family "ss") "ss" "rm")))))
    (for (it items)
      (cond ((and (list-2? it) (in? (car it) extra-parts))
             (set! c (choice-set c (car it) (cadr it))))
            ((list-1? it)
             (set! c (choice-set c "text" (car it))))))
    (if (choice-ref c "text") c (choice-set c "text" "roman"))))

(tm-define (font-design-font c)
  (:synopsis "The value of the font variable for the choice @c")
  (let* ((text (or (choice-ref c "text") "roman"))
         (math (choice-ref c "math"))
         ;; the mathematics which the text font brings along anyway
         (own (math-family-for-text text))
         (math* (and math (!= math own) (!= math text) math))
         (r text))
    (for (part (reverse extra-parts))
      (with val (if (== part "math") math* (choice-ref c part))
        (when val
          (set! r (string-append part "=" val "," r)))))
    r))

(define (choice-pair c)
  ;; the pair of the menus which the choice is, when no other part is set
  (let* ((text (or (choice-ref c "text") "roman"))
         (fam (or (choice-ref c "family") "rm"))
         (math (or (choice-ref c "math") (math-family-for-text text))))
    (and (not (list-or (map (cut choice-ref c <>)
                            '("frak" "cal" "bbb" "typewriter" "sansserif"))))
         (list-find (font-design-entries "all")
                    (lambda (e) (and (== (entry-ref e 'text) text)
                                     (== (entry-ref e 'math) math)
                                     (== (entry-ref e 'family) fam)))))))

(define (choice-plain? c)
  ;; a text font of the menus, without parts of its own
  (== (font-design-font c) (or (choice-ref c "text") "roman")))

;; the choices in the making, by document
(define font-design-choices (make-ahash-table))

(define (document-choice u)
  (or (ahash-ref font-design-choices (url->system u))
      (with c (with-buffer u
                (font-design-parse (get-init "font") (get-init "font-family")))
        (ahash-set! font-design-choices (url->system u) c)
        c)))

(define (set-document-choice u c)
  (ahash-set! font-design-choices (url->system u) c))

(tm-define (font-design-choose c e)
  (:synopsis "The choice @c with the entry @e for the part of its tab")
  (let* ((tab (entry-ref e 'tab))
         (fam (or (entry-ref e 'family) "rm")))
    (cond ((== tab "all")
           ;; a pair, as its entry of the menus: the other parts follow it
           (list (cons "family" fam) (cons "text" (entry-ref e 'text))
                 (cons "math" (entry-ref e 'math))))
          ((== tab "text")
           (choice-set (choice-set c "text" (entry-ref e 'text))
                       "family" fam))
          (else (choice-set c tab (entry-value e))))))

(tm-define (font-design-apply c)
  (:synopsis "Give the current document the fonts of the choice @c")
  (let* ((text (or (choice-ref c "text") "roman"))
         (fam (or (choice-ref c "family") "rm"))
         (pair (choice-pair c)))
    (cond ((and pair (== text "roman"))
           (init-font "roman" "roman"))
          ;; as the entry of the menus of the pair does
          (pair (init-opentype-font (entry-ref pair 'math)))
          ((choice-plain? c)
           (init-font text)
           (if (== fam "ss") (init-env "font-family" "ss")
               (init-default "font-family")))
          (else
            (remove-font-packages)
            (init-default "math-font")
            (init-env "font" (font-design-font c))
            (if (== fam "ss") (init-env "font-family" "ss")
                (init-default "font-family"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The samples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each tab shows its part: the text of a text font, the formulas of a
;; mathematical font, the alphabet of a calligraphic one. The sample of an
;; entry is set as the document would be with that entry chosen for its
;; part and the default font for the rest.

(define sample-formula
  '(concat (big "int") (rsub "0") (rsup "<infty>")
           "e" (rsup (concat "-x" (rsup "2"))) "*<mathd>x="
           (frac (sqrt "<pi>") "2") "," (space "1em")
           (big "sum") (rsub "n=1") (rsup "<infty>")
           (frac "1" (concat "n" (rsup "2"))) "="
           (frac (concat "<pi>" (rsup "2")) "6") "," (space "1em")
           (around* "(" (frac "<partial>f" "<partial>x") ")") (rsup "2")
           "<leqslant>" (around* "<||>" "f" "<||>") (rsub "<infty>")
           (rsup "2")))

(define sample-symbols
  '(concat "<alpha><beta><gamma><delta><varepsilon><theta><lambda><mu><pi>"
           "<Gamma><Delta><Omega>," (space "0.8em")
           "a*x" (rsup "2") "+b*x+c=0," (space "0.8em")
           "<forall>x<in>A<cap>B<Rightarrow>f(x)<neq><emptyset>,"
           (space "0.8em") "<nabla><times>E=-<partial>" (rsub "t") "B"))

(define sample-letters
  '(concat "<alpha><beta><gamma><delta><varepsilon><theta><lambda><Gamma>"
           "<Omega>," (space "0.8em") "<bbb-N><bbb-Z><bbb-Q><bbb-R><bbb-C>,"
           (space "0.8em") "<cal-A><cal-B><cal-F><cal-L>," (space "0.8em")
           "<frak-g><frak-h><frak-S>," (space "0.8em")
           (math-bf "v") "+" (math-ss "A") "+" (math-tt "x")))

;; The lines are short: the samples keep their size, to compare the fonts
;; at the same size, and have to fit a narrow window.
(define (sample-text-lines)
  (list "The quick brown fox jumps over the lazy dog, 0123456789."
        '(concat (with "font-shape" "italic" "Italic") ", "
                 (with "font-series" "bold" "bold") ", "
                 (with "font-series" "bold" "font-shape" "italic"
                   "bold italic")
                 ", " (with "font-shape" "small-caps" "Small Capitals")
                 "; wavy AV fjord, caf<#E9>, <#152>uvre, Stra<#DF>e.")))

(define (sample-companions-line)
  '(concat (with "font-family" "ss" "Sans serif: jinxed gnomes.")
           (space "1em")
           (with "font-family" "tt" "Typewriter: f(x) = x + 1;")))

(define (sample-mono-lines)
  (list "for (i = 0; i < n; i++) { s += a[i] * b[i]; }"
        '(concat "0O 1lI |!  "
                 (with "font-series" "bold" "bold") "  "
                 (with "font-shape" "italic" "italic") "  "
                 "<less>tag<gtr> [x] {y} #$%&@")))

(define (alphabet prefix letters)
  (apply string-append
         (map (lambda (c) (string-append "<" prefix "-" (string c) ">"))
              (string->list letters))))

(define upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define lower "abcdefghijklmnopqrstuvwxyz")

(define (sample-alphabet-lines tab)
  (cond ((== tab "bbb")
         (list `(math ,(alphabet "bbb" upper))
               `(math ,(string-append (alphabet "bbb" "abcdefghijk") ","
                                      (alphabet "bbb" "0123456789")))))
        ((== tab "cal")
         (list `(math ,(alphabet "cal" upper))))
        (else
          (list `(math ,(alphabet "frak" upper))
                `(math ,(alphabet "frak" lower))))))

;; the choice which an entry alone makes
(define (entry-choice e)
  (let* ((tab (entry-ref e 'tab))
         (fam (or (entry-ref e 'family) "rm")))
    (cond ((== tab "all")
           (list (cons "family" fam) (cons "text" (entry-ref e 'text))
                 (cons "math" (entry-ref e 'math))))
          ((== tab "text")
           (list (cons "family" fam) (cons "text" (entry-ref e 'text))))
          (else (list (cons "family" "rm") (cons "text" "roman")
                      (cons tab (entry-value e)))))))

(tm-define (font-design-sample-tree e)
  (:synopsis "The sample of the entry @e of the catalogue")
  (let* ((tab (entry-ref e 'tab))
         (c (entry-choice e))
         (fam (cond ((== tab "sansserif") "ss")
                    ((== tab "typewriter") "tt")
                    (else (or (choice-ref c "family") "rm"))))
         (lines (cond ((== tab "all")
                       (append (sample-text-lines)
                               (list (sample-companions-line)
                                     `(math (with "math-display" "true"
                                              ,sample-formula))
                                     `(math ,sample-letters))))
                      ((== tab "math")
                       (list `(math (with "math-display" "true"
                                      ,sample-formula))
                             `(math ,sample-symbols)
                             `(math ,sample-letters)))
                      ((== tab "typewriter") (sample-mono-lines))
                      ((in? tab '("bbb" "cal" "frak"))
                       (sample-alphabet-lines tab))
                      (else (sample-text-lines)))))
    `(with "font" ,(font-design-font c) "math-font" "roman"
       "font-family" ,fam "font-base-size" "10" "par-sep" "0.45fn"
       (document ,@lines))))

(define (shipped-samples)
  (url-append (system->url "$TEXMACS_PATH") "misc/font-samples"))

(define (cached-samples)
  (url-append (system->url "$TEXMACS_HOME_PATH") "system/cache/font-samples"))

(define (sample-file dir e)
  (url-append dir (string-append (entry-ref e 'id) ".pdf")))

(tm-define (font-design-sample e)
  (:synopsis "The picture of the sample of the entry @e, or #f")
  (cond ((url-exists? (sample-file (shipped-samples) e))
         (sample-file (shipped-samples) e))
        ((url-exists? (sample-file (cached-samples) e))
         (sample-file (cached-samples) e))
        (else #f)))

(tm-define (font-design-make-sample e dir)
  (:synopsis "Make the picture of the sample of the entry @e in @dir")
  (when (not (url-exists? dir)) (system-mkdir dir))
  (with u (sample-file dir e)
    (print-snippet u (stree->tree (font-design-sample-tree e)) #f)
    (url-exists? u)))

;; The fonts which come with TeXmacs: those whose files are in its tree
(define (shipped-file? name)
  (with u (url-append (url-append (system->url "$TEXMACS_PATH/fonts/truetype")
                                  (url-wildcard "*"))
                      name)
    (not (url-none? (url-complete u "fr")))))

(define (family-shipped? family)
  (list-or (map (lambda (style)
                  (list-or (map shipped-file?
                                (font-database-search family style))))
                (font-database-styles family))))

(define (master-shipped? master)
  (or (== master "roman")
      (list-or (map family-shipped? (font-master->families master)))))

(define (math-shipped? math)
  (or (== math "roman")
      (shipped-file?
       (string-append (math-font-profile-attr math "file") ".otf"))))

(define (entry-shipped? e)
  (let* ((tab (entry-ref e 'tab))
         (text (entry-ref e 'text))
         (math (entry-ref e 'math)))
    (cond ((== tab "all") (and (master-shipped? text) (math-shipped? math)))
          ((in? tab '("math" "bbb" "cal" "frak")) (math-shipped? math))
          (else (master-shipped? text)))))

(tm-define (font-design-make-samples dir)
  (:synopsis "Make in @dir the samples of the fonts which come with TeXmacs")
  ;; run in a TeXmacs with a document open; TeXmacs/misc/font-samples is
  ;; made with (font-design-make-samples "$TEXMACS_PATH/misc/font-samples")
  (with dir* (if (string? dir) (system->url dir) dir)
    (for (tab font-design-tabs)
      (for (e (font-design-entries tab))
        (when (entry-shipped? e)
          (display* "font sample: " (entry-ref e 'id)
                    (if (font-design-make-sample e dir*) "" " FAILED")
                    "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The paragraphs of the page: a line with its title, the choice (its
;; parts on the left, its sample on the right) and the list of the fonts
;; for one part, which scrolls. The names of the parts in the choice are
;; the buttons which show their fonts: the "tabs" of the list. A choice or
;; another part puts the paragraphs which change again in place.

(tm-define (tmfs-url-font-design u)
  (string-append "tmfs://font-design/" (url->tmfs-string u)))

(define (page-document name)
  (tmfs-string->url name))

;; the tab which each page shows
(define font-design-page-tabs (make-ahash-table))

(define (document-tab u)
  (or (ahash-ref font-design-page-tabs (url->system u)) "text"))

(define (design-action text cmd . args)
  `(action (font-design-button ,text)
           ,(string-append "(" cmd " "
                           (string-recompose (map object->string args) " ")
                           ")")))

;; A row of the choice: the name of the part is the button which shows
;; the fonts for it in the list (pressed for the part which is shown)
(define (choice-row doc c part)
  (let* ((d (url->system doc))
         (val (and-with v (choice-ref c part)
                (font-design-value-name part v)))
         (name (or val "as the text font")))
    `(row (cell ,(if (== part (document-tab doc))
                     `(font-design-tab-on ,(tab-name part))
                     `(action (font-design-tab-off ,(tab-name part))
                              ,(string-append "(font-design-page-tab "
                                              (object->string d) " "
                                              (object->string part) ")"))))
          (cell ,(if val `(strong ,name) `(font-design-muted ,name))))))

;; the choice, in its own fonts: the only fonts which the page loads
(define (choice-preview c)
  ;; (the mathematical font of the page does not show through: formulas
  ;; follow the text font unless it is roman)
  `(with "font" ,(font-design-font c) "math-font" "roman"
       "font-family" ,(or (choice-ref c "family") "rm")
     (document
       ,@(sample-text-lines)
       ,(sample-companions-line)
       (equation* (document ,sample-formula))
       (math ,sample-letters))))

(define (choice-block doc c)
  ;; the parts and the buttons on the left, the sample on the right
  (with d (url->system doc)
    `(tabular
      (tformat
       (twith "table-width" "1par") (twith "table-hmode" "exact")
       (cwith "1" "1" "1" "-1" "cell-hyphen" "t")
       (cwith "1" "1" "1" "-1" "cell-valign" "t")
       ;; a height of its own, whatever the sample takes in a narrow
       ;; window: the list has the rest of the window (font-design.ts)
       (cwith "1" "1" "1" "-1" "cell-vmode" "exact")
       (cwith "1" "1" "1" "-1" "cell-height" "134pt")
       (cwith "1" "1" "1" "1" "cell-width" "0.42par")
       (cwith "1" "1" "1" "1" "cell-hmode" "exact")
       (cwith "1" "1" "1" "-1" "cell-lsep" "0spc")
       (table
        (row
         (cell
          (document
            (tabular
             (tformat (cwith "1" "-1" "1" "-1" "cell-lsep" "0spc")
                      (cwith "1" "-1" "1" "2" "cell-rsep" "1em")
                      (table ,@(map (cut choice-row doc c <>)
                                    font-design-tabs))))
            (concat ,(design-action "Use for the document"
                                    "font-design-page-apply" d)
                    " "
                    ,(design-action "Save as" "font-design-page-save" d)
                    " "
                    ,(design-action "Start again"
                                    "font-design-page-restart" d))))
         (cell ,(choice-preview c))))))))

;; is the entry what the choice has for its part?
(define (entry-chosen? c e)
  (let* ((tab (entry-ref e 'tab))
         (fam (or (entry-ref e 'family) "rm"))
         (text (or (choice-ref c "text") "roman")))
    (cond ((== tab "all")
           (with pair (choice-pair c)
             (and pair (== (entry-ref pair 'id) (entry-ref e 'id)))))
          ((== tab "text")
           (and (== text (entry-ref e 'text))
                (== (or (choice-ref c "family") "rm") fam)))
          (else (== (choice-ref c tab) (entry-value e))))))

(define (entry-block doc c e)
  (let* ((d (url->system doc))
         (tab (entry-ref e 'tab))
         (id (entry-ref e 'id))
         (pic (font-design-sample e))
         (descr (font-design-description (entry-ref e 'name))))
    `((concat ,(if (entry-chosen? c e)
                   '(font-design-chosen "Chosen")
                   (design-action "Choose" "font-design-page-choose" d tab id))
              (space "1em") (strong ,(entry-ref e 'name))
              ,@(if (== descr "") (list)
                    (list '(space "1em") `(font-design-muted ,descr))))
      ,(if pic
           `(font-design-sample (image ,(url->system pic) "" "" "" ""))
           (design-action "Show a sample" "font-design-page-sample"
                          d tab id)))))

(define (default-block doc c tab)
  ;; the parts which follow the text font unless they have one of their own
  (with d (url->system doc)
    `((concat ,(if (choice-ref c tab)
                   (design-action "Choose" "font-design-page-reset" d tab)
                   '(font-design-chosen "Chosen"))
              (space "1em") (strong "As the text font")
              (space "1em")
              (font-design-muted
               "What the text font, or its mathematics, brings along.")))))

(define (section-blocks doc c s)
  ;; a light rule between the fonts
  (append (if (== (car s) "") (list) (list `(font-design-heading ,(car s))))
          (entry-block doc c (cadr s))
          (append-map (lambda (e)
                        (cons '(font-design-rule) (entry-block doc c e)))
                      (cddr s))))

(define (list-block doc c scroll)
  (let* ((tab (document-tab doc))
         (ss (font-design-sections tab)))
    `(font-design-list
      ,scroll
      (document
        ,@(if (== tab "text") (list)
              (append (default-block doc c tab) (list '(font-design-rule))))
        ,@(append-map (cut section-blocks doc c <>) ss)))))

(define (font-design-content doc)
  (with c (document-choice doc)
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "generic" "font-design"))
       (body
        (document
          (concat (strong ,(string-append "Fonts of "
                                          (url->system (url-tail doc))))
                  (space "1em")
                  (font-design-muted
                   (concat "A part shows its fonts in the list; the "
                           "document changes only with "
                           (em "Use for the document") ".")))
          ,(choice-block doc c)
          ,(list-block doc c "100%"))))))

(tmfs-title-handler (font-design name doc)
  (string-append "Font design - "
                 (url->system (url-tail (page-document name)))))

(tmfs-load-handler (font-design name)
  (font-design-content (page-document name)))

(define (page-list)
  (with t (buffer-tree)
    (and (tm-func? t 'document) (> (tm-arity t) 2)
         (tree-is? (tree-ref t 2) 'font-design-list)
         (tree-ref t 2))))

;; The paragraphs which change are put again in place, which keeps the
;; page where the reader is: the list keeps its position unless the tab
;; changes.
(define (page-update doc top?)
  (let* ((t (buffer-tree))
         (l (page-list))
         (c (document-choice doc)))
    (if l
        (with scroll (if top? "100%" (tree->string (tree-ref l 0)))
          (tree-set (tree-ref t 1) (stree->tree (choice-block doc c)))
          (tree-set (tree-ref t 2) (stree->tree (list-block doc c scroll))))
        (revert-buffer-revert (tmfs-url-font-design doc)))))

(define (page-context? d)
  ;; the actions only work from the page of the document they name
  (with ok? (== (url->system (current-buffer))
                (url->system (string->url (tmfs-url-font-design
                                           (system->url d)))))
    (when (not ok?)
      (set-message "This only works from the page of the fonts" "Font design"))
    ok?))

;; The wheel scrolls the list of the fonts, wherever the pointer is: the
;; page itself fits the window (font-design.ts) and has nothing to scroll.
;; The ports hand the wheel to an editor which captures it (wheel-capture?,
;; as the graphics do), with the displacement in points.
(define (font-design-page?)
  (string-starts? (url->system (current-buffer)) "tmfs://font-design/"))

;; The position of the list is a percentage (100% at the top), which the
;; canvas turns into a displacement and keeps within the list. Points are
;; turned into a percentage with an estimate of the height of the list: the
;; extents of what a canvas holds are not known here, and an estimate
;; which is off by a fifth only makes the wheel that much faster or slower.
(define (list-height-estimate tab)
  (with entry-height
      (lambda (e)
        (cond ((not (font-design-sample e)) 55)
              ((== tab "all") 135)
              ((== tab "math") 110)
              (else 75)))
    (apply + (map (lambda (s) (+ 30 (apply + (map entry-height (cdr s)))))
                  (font-design-sections tab)))))

(tm-define (font-design-scroll dy)
  (:synopsis "Scroll the list of the fonts by @dy points")
  (and-with l (page-list)
    (let* ((doc (page-document
                 (string-drop (url->system (current-buffer))
                              (string-length "tmfs://font-design/"))))
           (h (max 200 (- (list-height-estimate (document-tab doc)) 300)))
           (s (tree->string (tree-ref l 0)))
           (old (or (and (string-ends? s "%")
                         (string->number (string-drop-right s 1)))
                    100))
           (new (max 0 (min 100 (+ old (/ (* 100.0 dy) h))))))
      (when (!= new old)
        (tree-set (tree-ref l 0) (string-append (number->string new) "%"))))))

(tm-define (wheel-capture?)
  (:require (font-design-page?))
  #t)

(tm-define (wheel-event dx dy)
  (:require (font-design-page?))
  (font-design-scroll dy))

(tm-define (font-design-page-tab d tab)
  (:secure #t)
  (when (and (page-context? d) (in? tab font-design-tabs))
    (ahash-set! font-design-page-tabs d tab)
    (page-update (system->url d) #t)))

(tm-define (font-design-page-choose d tab id)
  (:secure #t)
  (when (page-context? d)
    (let* ((doc (system->url d))
           (e (font-design-find-entry tab id)))
      (when e
        (set-document-choice doc (font-design-choose (document-choice doc) e))
        (page-update doc #f)
        (set-message (string-append (tab-name tab) ": " (entry-ref e 'name))
                     "Font design")))))

(tm-define (font-design-page-reset d part)
  (:secure #t)
  (when (page-context? d)
    (with doc (system->url d)
      (set-document-choice doc (choice-set (document-choice doc) part #f))
      (page-update doc #f))))

(tm-define (font-design-page-restart d)
  (:secure #t)
  (when (page-context? d)
    (with doc (system->url d)
      (ahash-remove! font-design-choices d)
      (page-update doc #f))))

(tm-define (font-design-page-sample d tab id)
  (:secure #t)
  (when (page-context? d)
    (and-with e (font-design-find-entry tab id)
      (font-design-make-sample e (cached-samples))
      (page-update (system->url d) #f))))

(tm-define (font-design-page-save d)
  (:secure #t)
  ;; the choice under a name, for the menu of the fonts of every document
  (when (page-context? d)
    (with c (document-choice (system->url d))
      (interactive
          (lambda (name)
            (when (!= name "")
              (font-design-store name (font-design-font c)
                                 (or (choice-ref c "family") "rm"))
              (set-message (string-append "The fonts are saved as " name)
                           "Font design")))
        (list "Name of these fonts" "string" '())))))

(tm-define (font-design-page-apply d)
  (:secure #t)
  (when (page-context? d)
    (let* ((doc (system->url d))
           (c (document-choice doc)))
      (switch-to-buffer doc)
      (font-design-apply c)
      (set-message "The document has the fonts of the choice" "Font design"))))

(tm-define (open-font-design)
  (:synopsis "Open the page of the design of the fonts of the document")
  (with doc (current-buffer)
    (ahash-remove! font-design-choices (url->system doc))
    (document-choice doc)
    (cursor-history-add (cursor-path))
    (load-document (tmfs-url-font-design doc))))
