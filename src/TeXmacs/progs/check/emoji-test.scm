
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : emoji-test.scm
;; DESCRIPTION : tests of the typesetting of emoji
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The emoji are drawn with the color font which comes with TeXmacs,
;; Twemoji Mozilla (TeXmacs/fonts/truetype/twemoji, chosen in smart_font.cpp):
;; every emoji of it is one em wide, so that its width tells whether it was
;; found (a character which no font has is written out as its code, several
;; em wide). A sequence (a family joined by U+200D, a flag of two regional
;; indicators, a skin tone, a keycap, a symbol followed by U+FE0F) is one
;; piece of text for the typesetter (emoji_sequence_end, language.cpp) and
;; one ligature of the font (its ccmp feature, unicode_font.cpp), one em
;; wide; what is left of a sequence takes no room. The colors of the layers
;; (COLR) cannot be seen from here: see the samples in docs/color-fonts.md.

(texmacs-module (check emoji-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (width t)
  ;; the logical width of @t typeset as a line, in tmpt
  (with r (tree->stree (texmacs-expand `(box-info ,t "w")))
    (string->number (cadr r))))

(define (near? x y tol) (<= (abs (- x y)) tol))

(define (em) (length-decode "1fn"))

(define (one-em? t)
  ;; @t is typeset as one emoji
  (near? (width t) (em) (/ (em) 20)))

(define smile "<#1F600>")
(define rocket "<#1F680>")
(define man "<#1F468>")
(define woman "<#1F469>")
(define girl "<#1F467>")
(define zwj "<#200D>")
(define vs16 "<#FE0F>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single emoji
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-single)
  (check-group "single emoji")
  (check-true (one-em? smile))
  (check-true (one-em? rocket))
  ;; in every style of the text, and in formulas
  (check-true (one-em? `(with "font-series" "bold" ,smile)))
  (check-true (one-em? `(with "font-shape" "italic" ,smile)))
  (check-true (one-em? `(with "font-family" "ss" ,smile)))
  (check-true (one-em? `(with "font-family" "tt" ,smile)))
  (check-true (one-em? `(with "mode" "math" ,smile)))
  ;; side by side: one em each
  (check-true (near? (width (string-append smile rocket smile))
                     (* 3 (em)) (/ (em) 10)))
  ;; at another size, in proportion
  (check-true (near? (width `(with "font-base-size" "20" ,smile))
                     (* 2 (em)) (/ (em) 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-sequences)
  (check-group "sequences")
  ;; a family: three people joined by U+200D
  (check-true (one-em? (string-append man zwj woman zwj girl)))
  ;; a flag: two regional indicators, and two flags side by side
  (check-true (one-em? "<#1F1EE><#1F1F9>"))
  (check-true (near? (width "<#1F1EE><#1F1F9><#1F1EB><#1F1F7>")
                     (* 2 (em)) (/ (em) 10)))
  ;; a skin tone
  (check-true (one-em? "<#1F44D><#1F3FD>"))
  ;; a keycap, from a digit or a sign of the text
  (check-true (one-em? (string-append "#" vs16 "<#20E3>")))
  (check-true (one-em? (string-append "1" vs16 "<#20E3>")))
  ;; a digit followed by U+FE0F alone stays a digit of the text
  (check-true (near? (width (string-append "1" vs16)) (width "1") 2))
  (check-true (near? (width (string-append "a1" vs16 "b")) (width "a1b") 2))
  ;; a heart, which is a symbol unless followed by U+FE0F
  (check-true (one-em? (string-append "<#2764>" vs16)))
  ;; in a sentence
  (check-true (near? (width (string-append "a" man zwj woman zwj girl "b"))
                     (+ (width "ab") (em)) (/ (em) 10)))
  ;; a man and a woman joined are no sequence of the font: two emoji
  (check-true (near? (width (string-append man zwj woman))
                     (* 2 (em)) (/ (em) 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is left of a sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-marks)
  (check-group "marks alone")
  (check-true (near? (width (string-append "x" vs16 "y")) (width "xy") 2))
  (check-true (near? (width (string-append "x" zwj "y")) (width "xy") 2))
  (check-true (near? (width (string-append "x<#FE0E>y")) (width "xy") 2))
  ;; a joiner at the end, or between emoji which make no ligature
  (check-true (one-em? (string-append smile zwj)))
  (check-true (near? (width (string-append smile zwj rocket))
                     (* 2 (em)) (/ (em) 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The emoji font of the system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With the preference "emoji font" at "system" (or at the name of a font),
;; the emoji come from a font of the system: Apple Color Emoji, made of
;; bitmaps (sbix) and joined by its AAT morx table, or Noto Color Emoji
;; (bitmaps, CBDT, and GSUB). Its emoji need not be one em wide: the
;; sequences are compared with one emoji of it. The group checks nothing
;; with the shipped font (the default); run the suite with a home whose
;; preferences say "system" to check it.

(define (test-system-font)
  (check-group "system font")
  (when (!= (get-preference "emoji font") "default")
    (let* ((one (width smile))
           (same? (lambda (t) (near? (width t) one (/ one 20)))))
      (check-true (> one 0))
      (check-true (same? rocket))
      (check-true (same? (string-append man zwj woman zwj girl)))
      (check-true (same? "<#1F1EE><#1F1F9>"))
      (check-true (same? (string-append "<#2764>" vs16)))
      (check-true (near? (width "<#1F44D><#1F3FD>") (width "<#1F44D>")
                         (/ one 20)))
      (check-true (same? (string-append "#" vs16 "<#20E3>")))
      (check-true (near? (width (string-append "x" vs16 "y")) (width "xy") 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deleting a sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A sequence is one character for the editor (tm_grapheme_end and
;; tm_grapheme_start, analyze.cpp): Delete and Backspace remove it whole.
;; (The cursor keys step over it whole too, out_of_emoji in edit_cursor.cpp,
;; but they move through the boxes on the screen, which need a window.)

(define (edit-step thunk)
  (archive-state)
  (start-editing)
  (thunk)
  (end-editing)
  (update-forced))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (with-buffer-text s thunk)
  (let* ((old (current-buffer))
         (before (buffer-list))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree `(document ,s)))
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (for (b (buffer-list))
        (when (nin? b before) (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (after-key s pos key)
  ;; the text @s once @key was pressed with the cursor at @pos
  (let ((r #f))
    (with-buffer-text s
      (lambda ()
        (edit-step (lambda () (go-to (append (buffer-path) (list 0 pos)))))
        (edit-step key)
        (set! r (body))))
    r))

(define (test-deletion)
  (check-group "deletion")
  (let* ((fam (string-append man zwj woman zwj girl))
         (s (string-append "a" fam "b<#1F1EE><#1F1F9>c#" vs16 "<#20E3>d"))
         (n (string-length s))
         (a (string-length (string-append "a" fam)))
         (b (+ a (string-length "b<#1F1EE><#1F1F9>"))))
    ;; Delete after the a, Backspace after the family: the family goes
    (check= (after-key s 1 kbd-delete)
            `(document ,(string-append "ab<#1F1EE><#1F1F9>c#" vs16 "<#20E3>d")))
    (check= (after-key s a kbd-backspace)
            `(document ,(string-append "ab<#1F1EE><#1F1F9>c#" vs16 "<#20E3>d")))
    ;; the flag, the keycap
    (check= (after-key s b kbd-backspace)
            `(document ,(string-append "a" fam "bc#" vs16 "<#20E3>d")))
    (check= (after-key s (- n 1) kbd-backspace)
            `(document ,(string-append "a" fam "b<#1F1EE><#1F1F9>cd")))
    ;; a letter stays a letter
    (check= (after-key s n kbd-backspace)
            `(document ,(string-append "a" fam "b<#1F1EE><#1F1F9>c#" vs16
                                       "<#20E3>")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The names of the emoji
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The emoji have names in the universal encoding, those of the LaTeX
;; package emoji: <emoji-grinning-face> for U+1F600, one name for a whole
;; sequence (TeXmacs/langs/encoding/emoji-names.scm, made from the
;; emoji-test.txt of Unicode). A text in UTF-8 gets the names of its
;; fully-qualified emoji (utf8->cork); a name is typeset as its code points,
;; which stay valid (the documents written before the names).

(define (named s) (utf8->cork (cork->utf8 s)))

(define (test-name-conversions)
  (check-group "names: conversions")
  (check= (named smile) "<emoji-grinning-face>")
  (check= (named (string-append man zwj woman zwj girl))
          "<emoji-family-man-woman-girl>")
  (check= (named (string-append "<#2764>" vs16)) "<emoji-red-heart>")
  (check= (named "<#1F1EE><#1F1F9>") "<emoji-flag-italy>")
  (check= (named "<#1F3F4><#E0067><#E0062><#E0065><#E006E><#E0067><#E007F>")
          "<emoji-flag-england>")
  (check= (named "<#1F44D><#1F3FD>") "<emoji-thumbs-up-medium-skin-tone>")
  (check= (named "<#1F3FD>") "<emoji-medium-skin-tone>")
  (check= (named (string-append "1" vs16 "<#20E3>")) "<emoji-keycap-1>")
  (check= (named (string-append "#" vs16 "<#20E3>")) "<emoji-keycap-hash>")
  ;; in a text, and what is not a fully-qualified emoji
  (check= (named (string-append "a" smile "b")) "a<emoji-grinning-face>b")
  (check= (named "a12#*") "a12#*")
  (check= (named "<#2764>") "<#2764>")
  (check= (named (string-append "1" vs16)) (string-append "1" vs16))
  (check= (named (string-append smile zwj rocket))
          "<emoji-grinning-face><#200D><emoji-rocket>")
  (check= (named "<alpha>") "<alpha>")
  ;; back to UTF-8: the same characters
  (check= (cork->utf8 "<emoji-family-man-woman-girl>")
          (cork->utf8 (string-append man zwj woman zwj girl)))
  (check= (cork->utf8 "<emoji-keycap-1>")
          (cork->utf8 (string-append "1" vs16 "<#20E3>")))
  (check= (cork->utf8 "<emoji-no-such-emoji>") "<emoji-no-such-emoji>")
  ;; the name of an emoji, by its name or by its code points
  (check= (emoji-name smile) "<emoji-grinning-face>")
  (check= (emoji-name "<emoji-rocket>") "<emoji-rocket>")
  (check= (emoji-name "<#1F1EE><#1F1F9>") "<emoji-flag-italy>")
  (check= (emoji-name "<#2764>") "")
  (check= (emoji-name "a") "")
  (check= (emoji-name (string-append smile rocket)) "")
  ;; the character before a position: a sequence whole
  (with s (string-append "a" man zwj woman zwj girl)
    (check= (string-grapheme-start s (string-length s)) 1))
  ;; a sequence partly by names is one emoji too, a selector after a name
  ;; goes with it
  (check= (emoji-name "<emoji-man><#200D><emoji-woman><#200D><emoji-girl>")
          "<emoji-family-man-woman-girl>")
  (check= (emoji-name (string-append "<emoji-rocket>" vs16)) "<emoji-rocket>")
  (with s (string-append "a<emoji-man>" zwj "<emoji-woman>" zwj girl)
    (check= (string-grapheme-start s (string-length s)) 1))
  (with s (string-append "a<emoji-rocket>" vs16)
    (check= (string-grapheme-start s (string-length s)) 1))
  (with s "a<emoji-thumbs-up><emoji-medium-skin-tone>"
    (check= (string-grapheme-start s (string-length s)) 1))
  ;; a flag by its name takes no regional indicator after it
  (with s "<emoji-flag-italy><#1F1EB>"
    (check= (string-grapheme-start s (string-length s))
            (string-length "<emoji-flag-italy>")))
  ;; one symbol only is a name
  (check= (emoji-name "<emoji-rocket><alpha>") "")
  (check= (emoji-name "<emoji-no-such-emoji>") "")
  (check= (cork->utf8 "<emoji-rocket><alpha>")
          (string-append (cork->utf8 rocket) (cork->utf8 "<alpha>")))
  ;; the other conversions of the encoding know the names
  (check= (t2a->utf8 "<emoji-rocket>") (cork->utf8 rocket)))

(define (same-width? s t)
  (near? (width s) (width t) 2))

(define (test-name-widths)
  (check-group "names: typesetting")
  ;; a name is typeset as its code points, in every font of emoji
  (check-true (same-width? "<emoji-grinning-face>" smile))
  (check-true (same-width? "<emoji-family-man-woman-girl>"
                           (string-append man zwj woman zwj girl)))
  (check-true (same-width? "<emoji-flag-italy>" "<#1F1EE><#1F1F9>"))
  (check-true (same-width? "<emoji-thumbs-up-medium-skin-tone>"
                           "<#1F44D><#1F3FD>"))
  (check-true (same-width? "<emoji-keycap-1>" (string-append "1" vs16 "<#20E3>")))
  (check-true (same-width? "<emoji-red-heart>" (string-append "<#2764>" vs16)))
  (check-true (same-width? "a<emoji-grinning-face><emoji-rocket>b"
                           (string-append "a" smile rocket "b")))
  (check-true (same-width? `(with "font-series" "bold" "<emoji-grinning-face>")
                           `(with "font-series" "bold" ,smile)))
  (check-true (same-width? `(with "mode" "math" "<emoji-grinning-face>")
                           `(with "mode" "math" ,smile)))
  ;; names mixed with code points, and next to each other: the sequences
  ;; of their code points (an input method which sends a family code point
  ;; after code point gives names and joiners)
  (check-true (same-width? (string-append "<emoji-man>" zwj "<emoji-woman>"
                                          zwj "<emoji-girl>")
                           (string-append man zwj woman zwj girl)))
  (check-true (same-width? (string-append man zwj "<emoji-woman>" zwj girl)
                           (string-append man zwj woman zwj girl)))
  (check-true (same-width? "<emoji-thumbs-up><emoji-medium-skin-tone>"
                           "<#1F44D><#1F3FD>"))
  (check-true (same-width? (string-append "<emoji-rocket>" vs16) rocket))
  ;; a name which the emoji font has not is typeset as its code points are
  ;; (by the fallback of the fonts)
  (check-true (same-width? "<emoji-face-with-bags-under-eyes>" "<#1FAE9>"))
  ;; one em in the shipped font
  (when (== (get-preference "emoji font") "default")
    (check-true (one-em? "<emoji-grinning-face>"))
    (check-true (one-em? "<emoji-family-man-woman-girl>"))
    (check-true (one-em? (string-append "<emoji-man>" zwj "<emoji-woman>"
                                        zwj "<emoji-girl>")))
    (check-true (one-em? "<emoji-thumbs-up><emoji-medium-skin-tone>"))))

(define (after-inserts s pos l)
  ;; the text @s once the strings @l were inserted one after the other at
  ;; @pos, as keys are
  (let ((r #f))
    (with-buffer-text s
      (lambda ()
        (edit-step (lambda () (go-to (append (buffer-path) (list 0 pos)))))
        (for (x l) (edit-step (lambda () (insert x))))
        (set! r (list (body) (cAr (cursor-path))))))
    r))

(define (test-name-editing)
  (check-group "names: editing")
  ;; a name is one character: Backspace removes it whole
  (with s "a<emoji-family-man-woman-girl>"
    (check= (after-key (string-append s "b") (string-length s) kbd-backspace)
            '(document "ab")))
  ;; and so is a sequence partly by names
  (with s (string-append "a<emoji-man>" zwj "<emoji-woman>")
    (check= (after-key (string-append s "b") (string-length s) kbd-backspace)
            '(document "ab")))
  ;; a family typed code point after code point (as an input method sends
  ;; it) is stored by the name of the family, the cursor after it
  (check= (after-inserts "ab" 1 (list man zwj woman zwj girl))
          '((document "a<emoji-family-man-woman-girl>b") 30))
  (check= (after-inserts "" 0 (list "<emoji-man>" zwj "<emoji-woman>" zwj
                                    "<emoji-girl>"))
          '((document "<emoji-family-man-woman-girl>") 29))
  ;; a skin tone after a hand, the second half of a flag, a keycap
  (check= (after-inserts "" 0 (list "<#1F44D>" "<#1F3FD>"))
          '((document "<emoji-thumbs-up-medium-skin-tone>") 34))
  (check= (after-inserts "" 0 (list "<#1F1EE>" "<#1F1F9>"))
          '((document "<emoji-flag-italy>") 18))
  (check= (after-inserts "" 0 (list "1" vs16 "<#20E3>"))
          '((document "<emoji-keycap-1>") 16))
  ;; next to the code points of an older document
  (check= (after-inserts "x<#1F44D>" 9 (list "<#1F3FD>"))
          '((document "x<emoji-thumbs-up-medium-skin-tone>") 35))
  ;; only what is before the cursor is renamed, and the cursor stays after
  ;; what was typed: a joiner between two emoji, a regional indicator
  ;; before a flag
  (check= (after-inserts (string-append smile rocket "z")
                         (string-length smile) (list zwj))
          `((document ,(string-append "<emoji-grinning-face>" zwj rocket "z"))
            28))
  (check= (after-inserts "<emoji-flag-italy>" 0 (list "<#1F1EB>"))
          '((document "<#1F1EB><emoji-flag-italy>") 8))
  ;; what makes no emoji stays as it is
  (check= (after-inserts "" 0 (list "<#1F600>" zwj "<#1F680>"))
          '((document "<emoji-grinning-face><#200D><emoji-rocket>") 42))
  (check= (after-inserts "" 0 (list "<#2764>" "a"))
          '((document "<#2764>a") 8)))

(define (search-hits doc what)
  (tree-search-tree (stree->tree doc) what '() 1000))

(define (test-name-search)
  (check-group "names: search")
  ;; an emoji is found by its name and by its code points, whichever form
  ;; the text has (the documents written before the names have code points)
  (let ((doc `(document ,(string-append "a<emoji-grinning-face>b" smile "c"))))
    (check= (length (search-hits doc "<emoji-grinning-face>")) 4)
    (check= (search-hits doc smile) (search-hits doc "<emoji-grinning-face>")))
  (let ((doc `(document ,(string-append "x" man zwj woman zwj girl "y"))))
    (check= (length (search-hits doc "<emoji-family-man-woman-girl>")) 2)
    (check= (search-hits doc "<emoji-family-man-woman-girl>")
            (search-hits doc (string-append man zwj woman zwj girl))))
  ;; an emoji alone is found as a whole emoji, not inside a sequence,
  ;; whatever form the sequence has
  (let ((doc `(document ,(string-append "x" man zwj woman zwj girl
                                        "y<emoji-family-man-woman-girl>z"
                                        "<emoji-man>"))))
    (check= (length (search-hits doc "<emoji-man>")) 2)
    (check= (search-hits doc man) (search-hits doc "<emoji-man>")))
  ;; the other texts are searched as before
  (check= (length (search-hits '(document "a<alpha>b") "<alpha>")) 2))

(define (test-name-export)
  (check-group "names: export and import")
  (let ((ex (lambda (s fmt) (convert `(document ,s) "texmacs-stree" fmt))))
    ;; LaTeX: \emoji of the package emoji, by its name there, for an emoji
    ;; by its name and for one in code points (an older document)
    (check= (ex "a<emoji-grinning-face>b" "latex-snippet")
            "a{\\emoji{grinning-face}}b")
    (check= (ex (string-append "a" smile "b") "latex-snippet")
            "a{\\emoji{grinning-face}}b")
    (check= (ex "a<emoji-family-man-woman-girl>b" "latex-snippet")
            "a{\\emoji{family-man-woman-girl}}b")
    (check= (ex (string-append "a" man zwj woman zwj girl "b") "latex-snippet")
            "a{\\emoji{family-man-woman-girl}}b")
    (check= (ex "<emoji-flag-congo---kinshasa>" "latex-snippet")
            "{\\emoji{flag-congo---kinshasa}}")
    ;; by the name of the package where CLDR changed it since
    (check= (ex "<emoji-smiling-face-with-open-hands>" "latex-snippet")
            "{\\emoji{hugging-face}}")
    ;; in a formula, in a box
    (check= (convert '(document (math "x<emoji-rocket>")) "texmacs-stree"
                     "latex-snippet")
            "$x\\mbox{\\emoji{rocket}}$")
    ;; an emoji which the package has not: its characters, as before
    (check= (ex "<emoji-face-with-bags-under-eyes>" "latex-snippet")
            (cork->utf8 "<#1FAE9>"))
    (check= (ex "<#1FAE9>" "latex-snippet") (cork->utf8 "<#1FAE9>"))
    ;; the preamble loads the package (with LuaLaTeX; another engine shows
    ;; the names) only when a document has an emoji
    (let ((doc (lambda (s)
                 (convert `(document (TeXmacs "2.1.5") (style "generic")
                                     (body (document ,s)))
                          "texmacs-stree" "latex-document")))
          (has? (lambda (s what) (>= (string-search-forwards what 0 s) 0))))
      (with d (doc "a<emoji-grinning-face>b")
        (check-true (has? d "\\ifLuaTeX"))
        (check-true (has? d "\\usepackage{emoji}"))
        (check-true (has? d "\\providecommand{\\emoji}"))
        ;; and the import gives the emoji back, by its name
        (check= (convert d "latex-document" "texmacs-stree")
                '(document (TeXmacs "2.1.5") (style (tuple "article" "std-latex"))
                           (body (document "a<emoji-grinning-face>b")))))
      (check-false (has? (doc "ab") "emoji")))
    (check= (convert (ex "a<emoji-smiling-face-with-open-hands>b" "latex-snippet")
                     "latex-snippet" "texmacs-stree")
            "a<emoji-smiling-face-with-open-hands>b")
    (check= (ex "a<emoji-grinning-face>b" "html-snippet")
            (ex (string-append "a" smile "b") "html-snippet")))
  ;; \emoji of the LaTeX package emoji, by its names and its other ones
  (check= (convert "\\emoji{grinning-face}" "latex-snippet" "texmacs-stree")
          "<emoji-grinning-face>")
  (check= (convert "\\emoji{smile}" "latex-snippet" "texmacs-stree")
          "<emoji-grinning-face-with-smiling-eyes>")
  (check= (convert "\\emoji{+1}" "latex-snippet" "texmacs-stree")
          "<emoji-thumbs-up>")
  (check= (convert "\\emoji{tada}" "latex-snippet" "texmacs-stree")
          "<emoji-party-popper>")
  (check= (convert "\\emoji{hugging-face}" "latex-snippet" "texmacs-stree")
          "<emoji-smiling-face-with-open-hands>")
  (check= (convert "\\emoji{flag-turkey}" "latex-snippet" "texmacs-stree")
          "<emoji-flag-turkiye>")
  (check= (convert "\\emoji{flag-congo---kinshasa}" "latex-snippet"
                   "texmacs-stree")
          "<emoji-flag-congo---kinshasa>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-group thunk)
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (emoji-test-failures)
  (:synopsis "Run the tests of the emoji and return the failures")
  (check-suite "emoji")
  ;; the widths of the shipped font, whose emoji are one em
  (when (== (get-preference "emoji font") "default")
    (run-group test-single)
    (run-group test-sequences)
    (run-group test-marks))
  (run-group test-system-font)
  (run-group test-deletion)
  (run-group test-name-conversions)
  (run-group test-name-widths)
  (run-group test-name-editing)
  (run-group test-name-search)
  (run-group test-name-export)
  (check-end))
