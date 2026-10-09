
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
;; which shows every font TeXmacs offers with a sample and a few words, and
;; lets the fonts of the parts of a document be chosen one by one: the
;; text, the mathematics, the sans serif and typewriter text, and the
;; blackboard bold, calligraphic and fraktur letters of the formulas.  The
;; choice is shown at the top of the page, in the fonts themselves, and is
;; given to the document with "Use for the document".
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
;; The catalogue: the fonts of the menus, with what each may be used for
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An entry is an association list with the keys
;;   name    the name of the menus
;;   kind    pair (text with mathematics), serif, sans, mono or other
;;   text    the master of the text font (the value of `font')
;;   math    the family of the mathematical font, for a pair
;;   family  ss when the text of a pair is sans serif
;;   id      the name of the picture of the sample

(define (entry-ref e key)
  (with p (assq key e) (and p (cdr p))))

(define (font-design-slug kind name)
  (with safe (lambda (c)
               (cond ((char-alphabetic? c) (char-downcase c))
                     ((char-numeric? c) c)
                     (else #\-)))
    (string-append (symbol->string kind) "-"
                   (list->string (map safe (string->list name))))))

(define (make-entry kind name text math family)
  `((name . ,name) (kind . ,kind) (text . ,text) (math . ,math)
    (family . ,family) (id . ,(font-design-slug kind name))))

(define (master-has-feature? master feature)
  (list-or (map (lambda (fam) (in? feature (font-family-features fam)))
                (font-master->families master))))

(define (pair-entries group)
  (map (lambda (p)
         (with fam (math-font-profile-attr (cadr p) "family")
           (make-entry 'pair (car p) (caddr p) (cadr p)
                       (if (== fam "") "rm" fam))))
       (opentype-math-font-group-list group)))

(define (text-entries kind)
  (map (lambda (p) (make-entry kind (car p) (cadr p) #f
                               (if (== kind 'sans) "ss" "rm")))
       (text-font-list kind)))

(tm-define (font-design-sections)
  (:synopsis "The fonts of the page of the design of the fonts, by section")
  (list-filter
   (list (cons "Serif text with mathematics"
               (cons (make-entry 'pair "Roman" "roman" "roman" "rm")
                     (pair-entries "Serif")))
         (cons "Sans serif text with mathematics" (pair-entries "Sans serif"))
         (cons "Other mathematical fonts" (pair-entries "Other"))
         (cons "Serif text" (text-entries 'serif))
         (cons "Sans serif text" (text-entries 'sans))
         (cons "Typewriter text" (text-entries 'mono))
         (cons "Decorative text" (text-entries 'other)))
   (lambda (s) (nnull? (cdr s)))))

(define (font-design-entries)
  (append-map cdr (font-design-sections)))

;; what an entry may be used for: the parts of the choice
(define (entry-parts e)
  (let* ((kind (entry-ref e 'kind))
         (text (entry-ref e 'text))
         (roman? (== text "roman")))
    (append
     (if (== kind 'pair) (list "all") (list))
     (if (== kind 'mono) (list) (list "text"))
     (if (== kind 'pair) (list "math") (list))
     (if (and (not roman?)
              (or (== kind 'sans) (== (entry-ref e 'family) "ss")
                  (master-has-feature? text "sansserif")))
         (list "sansserif") (list))
     (if (and (not roman?)
              (or (== kind 'mono) (master-has-feature? text "mono")))
         (list "typewriter") (list))
     (if (and (== kind 'pair) (not roman?)) (list "bbb" "cal" "frak")
         (list)))))

(define part-names
  '(("all" . "Text and mathematics") ("text" . "Text")
    ("math" . "Mathematics") ("sansserif" . "Sans serif")
    ("typewriter" . "Typewriter") ("bbb" . "Blackboard bold")
    ("cal" . "Calligraphic") ("frak" . "Fraktur")))

(define (part-name part)
  (with p (assoc part part-names) (if p (cdr p) part)))

;; the value which names the font of an entry for a part
(define (entry-part-value e part)
  (if (in? part '("math" "bbb" "cal" "frak"))
      (entry-ref e 'math)
      (entry-ref e 'text)))

(define (font-design-find-entry id)
  (list-find (font-design-entries) (lambda (e) (== (entry-ref e 'id) id))))

;; the name of the menus for the value of a part
(define (font-design-value-name part val)
  (with math? (in? part '("math" "bbb" "cal" "frak"))
    (with e (list-find (font-design-entries)
                       (lambda (e) (== (entry-part-value e part) val)))
      (cond (e (entry-ref e 'name))
            ;; a mathematical font named by its text font
            ((and math? (list-find (font-design-entries)
                                   (lambda (e) (== (entry-ref e 'text) val))))
             => (lambda (e) (entry-ref e 'name)))
            (else val)))))

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
         (list-find (font-design-entries)
                    (lambda (e) (and (== (entry-ref e 'kind) 'pair)
                                     (== (entry-ref e 'text) text)
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

(tm-define (font-design-choose c e part)
  (:synopsis "The choice @c with the entry @e for @part")
  (let* ((val (entry-part-value e part))
         (fam (or (entry-ref e 'family) "rm")))
    (cond ((== part "all")
           ;; a pair, as its entry of the menus: the other parts follow it
           (list (cons "family" fam) (cons "text" (entry-ref e 'text))
                 (cons "math" (entry-ref e 'math))))
          ((== part "text")
           (choice-set (choice-set c "text" val) "family" fam))
          (else (choice-set c part val)))))

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

(define (sample-mono-line)
  '(concat "for (i = 0; i < n; i++) { s += a[i] * b[i]; }  0O 1lI "
           (with "font-series" "bold" "bold") " "
           (with "font-shape" "italic" "italic")))

(tm-define (font-design-sample-tree e)
  (:synopsis "The sample of the entry @e of the catalogue")
  (let* ((kind (entry-ref e 'kind))
         (text (entry-ref e 'text))
         (fam (if (== kind 'mono) "tt" (or (entry-ref e 'family) "rm")))
         (lines (cond ((== kind 'pair)
                       (list (car (sample-text-lines))
                             (cadr (sample-text-lines))
                             (sample-companions-line)
                             `(math (with "math-display" "true"
                                      ,sample-formula))
                             `(math ,sample-letters)))
                      ((== kind 'mono)
                       (list (sample-mono-line)))
                      (else
                        (sample-text-lines)))))
    `(with "font" ,(font-design-font
                    `(("text" . ,text)
                      ,@(if (== kind 'pair)
                            (list (cons "math" (entry-ref e 'math)))
                            (list))))
       "font-family" ,fam "font-base-size" "10"
       "par-sep" "0.45fn"
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

(define (entry-shipped? e)
  (let* ((kind (entry-ref e 'kind))
         (text (entry-ref e 'text))
         (math (entry-ref e 'math)))
    (cond ((== text "roman") #t)
          ((== kind 'pair)
           (shipped-file?
            (string-append (math-font-profile-attr math "file") ".otf")))
          (else (list-or (map family-shipped?
                              (font-master->families text)))))))

(tm-define (font-design-make-samples dir)
  (:synopsis "Make in @dir the samples of the fonts which come with TeXmacs")
  ;; run in a TeXmacs with a document open; TeXmacs/misc/font-samples is
  ;; made with (font-design-make-samples "$TEXMACS_PATH/misc/font-samples")
  (with dir* (if (string? dir) (system->url dir) dir)
    (for (e (font-design-entries))
      (when (entry-shipped? e)
        (display* "font sample: " (entry-ref e 'id)
                  (if (font-design-make-sample e dir*) "" " FAILED") "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmfs-url-font-design u)
  (string-append "tmfs://font-design/" (url->tmfs-string u)))

(define (page-document name)
  (tmfs-string->url name))

(define (design-action text cmd . args)
  `(action (font-design-button ,text)
           ,(string-append "(" cmd " "
                           (string-recompose (map object->string args) " ")
                           ")")))

(define (choice-row doc c part)
  (let* ((val (choice-ref c part))
         (name (if val (font-design-value-name part val)
                   "as the text font")))
    `(row (cell ,(part-name part))
          (cell ,(if val `(strong ,name) `(font-design-muted ,name)))
          (cell ,(if (and val (!= part "text"))
                     (design-action "reset" "font-design-page-reset"
                                    (url->system doc) part)
                     "")))))

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
                                    '("text" "math" "sansserif" "typewriter"
                                      "bbb" "cal" "frak")))))
            (concat ,(design-action "Use for the document"
                                    "font-design-page-apply" d)
                    " "
                    ,(design-action "Save as" "font-design-page-save" d)
                    " "
                    ,(design-action "Start again"
                                    "font-design-page-restart" d))))
         (cell ,(choice-preview c))))))))

(define (entry-block doc e)
  (let* ((d (url->system doc))
         (id (entry-ref e 'id))
         (pic (font-design-sample e))
         (descr (font-design-description (entry-ref e 'name)))
         (acts (map (lambda (part)
                      (design-action (part-name part) "font-design-page-choose"
                                     d id part))
                    (entry-parts e))))
    `((paragraph ,(entry-ref e 'name))
      ,@(if (== descr "") (list) (list descr))
      ,(if pic
           `(font-design-sample (image ,(url->system pic) "" "" "" ""))
           (design-action "Show a sample" "font-design-page-sample" d id))
      (concat (font-design-muted "Use for: ")
              ,@(list-intersperse acts " ")))))

(define (section-blocks doc s)
  ;; a light rule between the fonts of a section
  (cons `(section ,(car s))
        (append (entry-block doc (cadr s))
                (append-map (lambda (e)
                              (cons '(font-design-rule) (entry-block doc e)))
                            (cddr s)))))

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
                   (concat "Choose the fonts of the parts in the list; the "
                           "document changes only with "
                           (em "Use for the document") ".")))
          ,(choice-block doc c)
          ;; the fonts scroll in a frame of their own, under the choice
          (font-design-list
           "100%"
           (document
             ,@(append-map (cut section-blocks doc <>)
                           (font-design-sections)))))))))

(tmfs-title-handler (font-design name doc)
  (string-append "Font design - "
                 (url->system (url-tail (page-document name)))))

(tmfs-load-handler (font-design name)
  (font-design-content (page-document name)))

;; The block of the choice is the second paragraph of the page: it is put
;; again in place, which keeps the page where the reader is; a page whose
;; pictures change is loaded again.
(define (page-update doc)
  (with t (buffer-tree)
    (if (and (tm-func? t 'document) (> (tm-arity t) 1))
        (tree-set (tree-ref t 1)
                  (stree->tree (choice-block doc (document-choice doc))))
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

(define (page-list)
  (with t (buffer-tree)
    (and (tm-func? t 'document) (> (tm-arity t) 2)
         (tree-is? (tree-ref t 2) 'font-design-list)
         (tree-ref t 2))))

;; The position of the list is a percentage (100% at the top), which the
;; canvas turns into a displacement and keeps within the list. Points are
;; turned into a percentage with an estimate of the height of the list: the
;; extents of what a canvas holds are not known here, and an estimate
;; which is off by a fifth only makes the wheel that much faster or slower.
(define (list-height-estimate)
  (with entry-height
      (lambda (e)
        (cond ((not (font-design-sample e)) 75)
              ((== (entry-ref e 'kind) 'pair) 190)
              ((== (entry-ref e 'kind) 'mono) 105)
              (else 125)))
    (apply + (map (lambda (s) (+ 45 (apply + (map entry-height (cdr s)))))
                  (font-design-sections)))))

(tm-define (font-design-scroll dy)
  (:synopsis "Scroll the list of the fonts by @dy points")
  (and-with l (page-list)
    (let* ((h (max 400 (- (list-height-estimate) 300)))
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

(tm-define (font-design-page-choose d id part)
  (:secure #t)
  (when (page-context? d)
    (let* ((doc (system->url d))
           (e (font-design-find-entry id)))
      (when e
        (set-document-choice doc (font-design-choose (document-choice doc)
                                                     e part))
        (page-update doc)
        (set-message (string-append (part-name part) ": " (entry-ref e 'name))
                     "Font design")))))

(tm-define (font-design-page-reset d part)
  (:secure #t)
  (when (page-context? d)
    (with doc (system->url d)
      (set-document-choice doc (choice-set (document-choice doc) part #f))
      (page-update doc))))

(tm-define (font-design-page-restart d)
  (:secure #t)
  (when (page-context? d)
    (with doc (system->url d)
      (ahash-remove! font-design-choices d)
      (page-update doc))))

(tm-define (font-design-page-sample d id)
  (:secure #t)
  (when (page-context? d)
    (and-with e (font-design-find-entry id)
      (font-design-make-sample e (cached-samples))
      (revert-buffer-revert (tmfs-url-font-design (system->url d))))))

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
