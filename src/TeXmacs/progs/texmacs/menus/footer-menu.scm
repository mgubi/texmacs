
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : footer-menu.scm
;; DESCRIPTION : an interactive status bar
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The status bar, when the preference "interactive footer" is on, shows as
;; menus what it otherwise shows as text (the GUI draws them: Vue and
;; Cocoa). On the left, a menu of the document (its style, paper, fonts,
;; language: what the focus bar shows only at the top of the document),
;; then the properties of the text at the cursor, each a menu to change it
;; (the language, the font, its size, its series and shape, the colour); on
;; the right, the tags around the cursor, from the outermost to the
;; innermost, each a menu which selects it or changes it where it is,
;; without moving the cursor (its Focus menu).
;; While the editor shows a message on the status bar ((footer-environment?)
;; is false), the GUI shows the text of the message instead.

(texmacs-module (texmacs menus footer-menu)
  (:use (generic format-edit)
        (generic format-menu)
        (fonts font-old-menu)
        (fonts font-short-menu)
        (fonts fonts-opentype)
        (text text-menu)
        (texmacs menus main-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The properties of the text at the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (footer-capitalize s)
  (if (== s "") s
      (string-append (string-upcase (substring s 0 1)) (substring s 1))))

(define (footer-first-family s)
  ;; "pagella,roman" -> "pagella"
  (with l (string-tokenize-by-char s #\,)
    (if (null? l) s (car l))))

(tm-define (footer-font-label)
  (with f (cond ((in-math?) (get-env "math-font"))
                ((in-prog?) (get-env "prog-font"))
                (else (get-env "font")))
    (footer-capitalize (footer-first-family f))))

(tm-define (footer-size-label)
  (let* ((base (or (string->number (get-env "font-base-size")) 10))
         (k (or (string->number (get-env "font-size")) 1))
         (sz (inexact->exact (round (* base k)))))
    (string-append (number->string sz) "pt")))

(tm-define (footer-language-label)
  (footer-capitalize (get-env "language")))

(define (footer-default? var)
  (== (get-env var) (tree->stree (get-init-tree var))))

(tm-define (footer-effects-label)
  ;; the series and the shape, when they are not those of the document
  (let* ((series (get-env "font-series"))
         (shape (get-env "font-shape"))
         (l (append (if (footer-default? "font-series") '() (list series))
                    (if (footer-default? "font-shape") '() (list shape)))))
    (string-recompose l " ")))

(tm-define (footer-color-label)
  (get-env "color"))

;; The fonts as in the menu of the font of the document in the focus bar
;; (document-short-font-menu: the fonts by design, in each submenu those
;; with mathematics and those of text only, the selector for the others),
;; set at the cursor. A submenu is expanded when it is opened and loses the
;; arguments of its menu: one menu without arguments for each list.
(define (footer-local-font? f) (== (get-env "font") f))

(tm-menu (footer-math-text-serif-menu)
  (for (p (opentype-math-font-group-list "Serif"))
    ((check (eval (car p)) "*" (opentype-font-local? (cadr p)))
     (make-multi-with (opentype-font-local-vars (cadr p))))))

(tm-menu (footer-math-text-sans-menu)
  (for (p (opentype-math-font-group-list "Sans serif"))
    ((check (eval (car p)) "*" (opentype-font-local? (cadr p)))
     (make-multi-with (opentype-font-local-vars (cadr p))))))

(tm-menu (footer-math-text-other-menu)
  (for (p (opentype-math-font-group-list "Other"))
    ((check (eval (car p)) "*" (opentype-font-local? (cadr p)))
     (make-multi-with (opentype-font-local-vars (cadr p))))))

(tm-menu (footer-text-serif-menu)
  (for (p (text-font-list 'serif))
    ((check (eval (car p)) "*" (footer-local-font? (cadr p)))
     (make-with "font" (cadr p)))))

(tm-menu (footer-text-sans-menu)
  (for (p (text-font-list 'sans))
    ((check (eval (car p)) "*" (footer-local-font? (cadr p)))
     (make-with "font" (cadr p)))))

(tm-menu (footer-text-mono-menu)
  (for (p (text-font-list 'mono))
    ((check (eval (car p)) "*" (footer-local-font? (cadr p)))
     (make-with "font" (cadr p)))))

(tm-menu (footer-text-other-menu)
  (for (p (text-font-list 'other))
    ((check (eval (car p)) "*" (footer-local-font? (cadr p)))
     (make-with "font" (cadr p)))))

(tm-menu (footer-serif-font-menu)
  (group "With mathematics")
  ((check "Roman" "*" (footer-local-font? "roman"))
   (make-with "font" "roman"))
  (if (font-exists-in-tt? "STIX-Regular")
      ((check "Stix" "*" (footer-local-font? "stix"))
       (make-with "font" "stix")))
  (link footer-math-text-serif-menu)
  (assuming (nnull? (text-font-list 'serif))
    ---
    (group "Text only")
    (link footer-text-serif-menu)))

(tm-menu (footer-sans-font-menu)
  (assuming (nnull? (opentype-math-font-group-list "Sans serif"))
    (group "With mathematics")
    (link footer-math-text-sans-menu))
  (assuming (and (nnull? (opentype-math-font-group-list "Sans serif"))
                 (nnull? (text-font-list 'sans)))
    ---)
  (assuming (nnull? (text-font-list 'sans))
    (group "Text only")
    (link footer-text-sans-menu)))

(tm-menu (footer-text-font-menu)
  ((check "Default" "*" (footer-local-font? (get-init "font")))
   (make-with "font" (get-init "font")))
  ---
  (-> "Serif" (link footer-serif-font-menu))
  (assuming (or (nnull? (opentype-math-font-group-list "Sans serif"))
                (nnull? (text-font-list 'sans)))
    (-> "Sans serif" (link footer-sans-font-menu)))
  (assuming (nnull? (text-font-list 'mono))
    (-> "Typewriter" (link footer-text-mono-menu)))
  (assuming (nnull? (text-font-list 'other))
    (-> "Decorative" (link footer-text-other-menu)))
  (assuming (nnull? (opentype-math-font-group-list "Other"))
    (-> "Other OpenType math fonts" (link footer-math-text-other-menu)))
  ---
  ("Other" (open-font-selector)))

;; in a formula, its font
(tm-menu (footer-math-font-menu)
  ((check "Default" "*" (== (get-env "math-font") (get-init "math-font")))
   (make-with "math-font" (get-init "math-font")))
  ---
  ((check "Roman" "*" (== (get-env "math-font") "roman"))
   (make-with "math-font" "roman"))
  (for (p (opentype-math-font-list))
    ((check (eval (car p)) "*" (== (get-env "math-font") (cadr p)))
     (make-with "math-font" (cadr p))))
  ---
  ("Other" (open-font-selector)))

(menu-bind footer-font-menu
  (if (in-math?) (dynamic (footer-math-font-menu)))
  (if (not (in-math?)) (dynamic (footer-text-font-menu))))

(tm-define (footer-prog-label)
  ;; the language of the program at the cursor
  (with lan (get-env "prog-language")
    (if (in? lan '("" "verbatim")) "Program" (format-get-name lan))))

;; in a source file, its language only: the fonts and the colours of code
;; are those of its language, and its font is a preference (code in a
;; document keeps the tools of the document)
(tm-define (footer-in-source?)
  (and (in-prog?) (== (get-init "mode") "prog")))

(tm-menu (footer-document-menu)
  ;; the focus menu of the document (as at its top level), wherever the
  ;; cursor is
  (with t (buffer-tree)
    (dynamic (focus-style-menu t))
    ---
    (dynamic (focus-document-menu t))))

(menu-bind texmacs-footer-environment
  ;; (a promise: made when it is opened, not at each key)
  (=> (balloon "Document" "The style, paper, fonts and language of the document")
      (promise (menu-dynamic (dynamic (footer-document-menu)))))
  (text "|")
  (if (in-text?)
      (=> (balloon (eval (footer-language-label)) "Language")
          (link text-language-menu)))
  (if (in-math?) (text "Math"))
  (if (in-prog?) (text (footer-prog-label)))
  (if (not (footer-in-source?))
      (=> (balloon (eval (footer-font-label)) "Font")
          (link footer-font-menu))
      (=> (balloon (eval (footer-size-label)) "Font size")
          (link font-size-menu))
      (if (!= (footer-effects-label) "")
        (=> (balloon (eval (footer-effects-label)) "Font effects")
            (link text-font-effects-menu)))
      ;; the colour: a swatch, and its name as the menu
      (color (get-env "color") #f #f 10 10)
      (=> (balloon (eval (footer-color-label)) "Colour")
          (link color-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The tags around the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A click on a tag opens its menu: Select, then its Focus menu (the one
;; of the focus bar and of the Focus menu when the cursor is in it), whose
;; commands change that tag where it is, without moving the cursor. When the
;; tags do not fit in
;; footer-path-budget characters, the outer ones fold into a menu (the
;; innermost tags, the ones near the cursor, stay in view). The GUI sets
;; the budget from the room the tags have (footer-set-budget).

(define footer-path-budget 64)

(tm-define (footer-set-budget n)
  (set! footer-path-budget n))

(define (footer-hidden-tag? t)
  ;; the containers which say nothing: the document of the buffer, the
  ;; concatenations of lines and of words
  (in? (tree-label t) '(document concat)))

(tm-define (footer-context-trees)
  (list-filter (reverse (upward-context-trees (cursor-tree)))
               (lambda (t) (not (footer-hidden-tag? t)))))

(tm-define (footer-tag-name t)
  (with l (tree-label t)
    (if (symbol? l) (symbol->string l) "?")))

(define (footer-split-path ts)
  ;; (folded . shown): the innermost tags which fit in the budget are shown
  ;; (at least one), the others are folded; the character after the path
  ;; and the menu of the folded tags take some of it
  (let loop ((l (reverse ts))
             (n (+ (string-length (footer-char-info)) 6))
             (shown '()))
    (if (null? l) (cons '() shown)
        (let ((m (+ n (string-length (footer-tag-name (car l))) 3)))
          (if (and (nnull? shown) (> m footer-path-budget))
              (cons (reverse l) shown)
              (loop (cdr l) m (cons (car l) shown)))))))

(tm-define (footer-folded-trees)
  (car (footer-split-path (footer-context-trees))))

(tm-define (footer-shown-trees)
  (cdr (footer-split-path (footer-context-trees))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is just before the cursor, as the text footer says it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (footer-char-start s i)
  ;; the start of the character which ends at i: a symbol <...> or a byte,
  ;; an emoji sequence whole (a family, a flag, a skin tone, a keycap)
  (if (<= i 0) 0 (string-grapheme-start s i)))

(tm-define (footer-char-info)
  ;; as edit_interface_rep::compute_text_footer: the character before the
  ;; cursor, "start", "space" ("apply" in a formula), a symbol and its name
  (let* ((p (cursor-path))
         (t (path->tree (cDr p))))
    (if (not (tree-atomic? t)) ""
        (let* ((s (tree->string t))
               (i (min (cAr p) (string-length s)))
               (c (substring s (footer-char-start s i) i)))
          (cond ((== c "") "start")
                ((!= (emoji-name c) "")
                 ;; an emoji with its name (also one in code points)
                 (with e (emoji-name c)
                   (string-append c " (" (substring e 1 (- (string-length e) 1))
                                  ")")))
                ((== c " ") (if (in-math?) "apply" "space"))
                ((== c "<#9>") "tab")   ; (a tab of a source file)
                ((and (string-starts? c "<") (not (string-starts? c "<#"))
                      (> (string-length c) 2))
                 (string-append c " (" (substring c 1 (- (string-length c) 1))
                                ")"))
                (else c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The right side of the footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (footer-tag-menu t)
  ;; a tag around the cursor: select it, or change it in place
  ("Select" (tree-select t))
  ---
  (dynamic (standard-focus-menu t)))

(define (footer-run-in t cmd)
  ;; many commands of the Focus menu act on the innermost tag of their kind
  ;; around the cursor, whatever tag they are given (variant-formula on an
  ;; equation with an inline formula inside a text of it, for instance):
  ;; they run with the cursor at the start of the first child of t, which goes back where it
  ;; was afterwards, unless the command moved it or selected something
  (with p (and (> (tree-arity t) 0) (tree->path t 0 :start))
    (if (not p) (cmd)
        (let ((back (position-new))
              (here (position-new)))
          (position-set back (cursor-path))
          (go-to p)
          (position-set here (cursor-path))
          (cmd)
          (when (and (== (cursor-path) (position-get here))
                     (not (selection-active-any?)))
            (go-to (position-get back)))
          (position-delete here)
          (position-delete back)))))

(tm-define (footer-tag-items t)
  ;; the menu of the tag t, whose commands act on t (footer-run-in)
  (menu-wrap-commands (lambda (cmd) (footer-run-in t cmd)))
  (menu-dynamic (dynamic (footer-tag-menu t))))

(menu-bind footer-folded-menu
  (for (t (footer-folded-trees))
    (-> (eval (footer-tag-name t))
        (promise (footer-tag-items t)))))

(tm-define (footer-menu-key)
  ;; the menus of the tags hold the trees, which their expansion does not
  ;; show (a procedure is expanded to its source): the GUI makes the menus
  ;; again when this changes too, the buffer and the places of the tags
  (cons (current-buffer-url) (map tree->path (footer-context-trees))))

(menu-bind texmacs-footer-path
  (with ts (footer-shown-trees)
    (if (nnull? (footer-folded-trees))
        (=> (balloon "<#2026>" "Outer tags") (link footer-folded-menu))
        (text "<#203A>"))
    (for (t ts)
      (if (!= t (car ts)) (text "<#203A>"))  ; a single right angle quote
      ;; the innermost tag, the one of the cursor, in bold
      (if (!= t (cAr ts))
          (=> (balloon (eval (footer-tag-name t))
                       "Select this tag, or change it where it is")
              (promise (footer-tag-items t))))
      (if (== t (cAr ts))
          (bold (=> (balloon (eval (footer-tag-name t))
                             "Select this tag, or change it where it is")
                    (promise (footer-tag-items t))))))
    (if (!= (footer-char-info) "")
        (glue #f #f 12 0)
        (text (footer-char-info)))))
