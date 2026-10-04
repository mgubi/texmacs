
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
;; menus what it otherwise shows as text (the GUI draws them: Vue only for
;; now). On the left, the properties of the text at the cursor, each a menu
;; to change it (the language, the font, its size, its series and shape,
;; the colour); on the right, the tags around the cursor, from the outermost
;; to the innermost, each a button which selects it (as the context tool).
;; While the editor shows a message on the status bar ((footer-environment?)
;; is false), the GUI shows the text of the message instead.

(texmacs-module (texmacs menus footer-menu)
  (:use (generic format-menu)
        (fonts font-old-menu)
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

(menu-bind footer-font-menu
  (if (in-math?) (link math-font-menu))
  (if (not (in-math?)) (link text-font-menu)))

(menu-bind texmacs-footer-environment
  (if (in-text?)
      (=> (balloon (eval (footer-language-label)) "Language")
          (link text-language-menu)))
  (if (in-math?) (text "Math"))
  (if (in-prog?) (text "Program"))
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
      (link color-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The tags around the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A click on a tag selects it; a right click selects it and opens the
;; context menu of the editor on it, its Focus menu (the GUI does that:
;; vue_widget.cpp, menu_button in the footer). When the tags do not fit in
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
  ;; the start of the character which ends at i: a symbol <...> or a byte
  (if (and (> i 0) (== (string-ref s (- i 1)) #\>))
      (let loop ((j (- i 1)))
        (cond ((< j 0) (- i 1))
              ((== (string-ref s j) #\<) j)
              (else (loop (- j 1)))))
      (max 0 (- i 1))))

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
                ((== c " ") (if (in-math?) "apply" "space"))
                ((and (string-starts? c "<") (not (string-starts? c "<#"))
                      (> (string-length c) 2))
                 (string-append c " (" (substring c 1 (- (string-length c) 1))
                                ")"))
                (else c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The right side of the footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind footer-folded-menu
  (for (t (footer-folded-trees))
    ((eval (footer-tag-name t)) (tree-select t))))

(menu-bind texmacs-footer-path
  (with ts (footer-shown-trees)
    (if (nnull? (footer-folded-trees))
        (=> (balloon "<#2026>" "Outer tags") (link footer-folded-menu))
        (text "<#203A>"))
    (for (t ts)
      (if (!= t (car ts)) (text "<#203A>"))  ; a single right angle quote
      ;; the innermost tag, the one of the cursor, in bold
      (if (!= t (cAr ts))
          ((balloon (eval (footer-tag-name t))
                    "Select this tag (right click: its menu)")
           (tree-select t)))
      (if (== t (cAr ts))
          (bold ((balloon (eval (footer-tag-name t))
                          "Select this tag (right click: its menu)")
                 (tree-select t)))))
    (if (!= (footer-char-info) "")
        (glue #f #f 12 0)
        (text (footer-char-info)))))
