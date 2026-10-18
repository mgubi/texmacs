
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prog-edit.scm
;; DESCRIPTION : editing verbatim programs
;; COPYRIGHT   : (C) 2008  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog prog-edit)
  (:use (utils library tree)
        (utils library cursor)
        (treesit ts-edit)
        (code code-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic routines for textual programs
;; WARNING: most of these fail for non-verbatim content!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (inside-program?)
  (:synopsis "are we inside the line of a textual document?")
  (let* ((ct (cursor-tree))
         (dt (tree-ref ct :up)))
    (and (tree-atomic? ct) (tree-is? dt 'document))))

(tm-define (program-tree)
  (:synopsis "get the entire program tree")
  (let* ((ct (cursor-tree))
         (dt (tree-ref ct :up)))
    (and (tree-atomic? ct) (tree-is? dt 'document) dt)))

(tm-define (program-row row)
  (:synopsis "get the string at a given @row")
  (and-with doc (program-tree)
    (and-with par (tree-ref doc row)
      (and (tree-atomic? par) (tree->string par)))))

(tm-define (program-row-number)
  (:synopsis "get the vertical position on the current line")
  (and (inside-program?) (cADr (cursor-path))))

(tm-define (program-column-number)
  (:synopsis "get the horizontal position on the current line")
  (and (inside-program?) (cAr (cursor-path))))

(tm-define (program-go-to row col)
  (:synopsis "go to the character at a given @row and @col")
  (and-with doc (program-tree)
    (tree-go-to doc row col)))

(tm-define (program-character path)
  (let ((s (tree->string (path->tree (cDr path))))
        (pos (cAr path)))
    (if (or (string-null? s) (>= pos (string-length s)) (< pos 0)) #\nul
        (string-ref s pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for bracket handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define prog-auto-close-brackets? #f)
(tm-define prog-balanced-brackets? #t)   ; in code with a syntax tree
(tm-define prog-highlight-brackets? #f)
(tm-define prog-select-brackets? #f)

(define (notify-auto-close-brackets var val)
  (set! prog-auto-close-brackets? (== val "on")))
(define (notify-balanced-brackets var val)
  (set! prog-balanced-brackets? (== val "on")))
(define (notify-highlight-brackets var val)
  (set! prog-highlight-brackets? (== val "on")))
(define (notify-select-brackets var val)
  (set! prog-select-brackets? (== val "on")))

(define-preferences
  ("prog:automatic brackets" "off" notify-auto-close-brackets)
  ("prog:balanced brackets" "on" notify-balanced-brackets)
  ("prog:highlight brackets" "off" notify-highlight-brackets)
  ("prog:select brackets" "off" notify-select-brackets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracket handling for strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-bracket-find* s pos inc br ibr level)
  ;(display* "find: pos= " pos ", level= " level "\n")
  (cond ((or (< pos 0) (>= pos (string-length s))) (- level 1000))
        ((and (== level 0) (== (string-ref s pos) br))
         ;(display* "returning at " pos "\n")
         pos)
        ((== (string-ref s pos) br)
         ;(display* "found at " pos "\n")
         (string-bracket-find* s (+ pos inc) inc br ibr (- level 1)))
        ((== (string-ref s pos) ibr)
         (string-bracket-find* s (+ pos inc) inc br ibr (+ level 1)))
        (else (string-bracket-find* s (+ pos inc) inc br ibr level))))

(define (string-bracket-find s pos inc br ibr level)
  (with r (string-bracket-find* s pos inc br ibr level)
    (and (>= r 0) r)))

(tm-define (string-bracket-level s pos inc br ibr)
  (with ret (string-bracket-find* s pos inc br ibr 0)
    (if (< ret 0) (+ ret 1000)
        (string-bracket-level s (+ ret inc) inc br ibr))))

(tm-define (string-bracket-forward s pos br ibr)
  (:synopsis "find next bracket @br with inverse @ibr in @s at @pos")
  (string-bracket-find s pos 1 br ibr 0))

(tm-define (string-bracket-backward s pos br ibr)
  (:synopsis "find previous bracket @br with inverse @ibr in @s at @pos")
  (string-bracket-find s pos -1 br ibr 0))

(define (program-bracket-find row col inc br ibr level)
  (and-with s (program-row row)
    (with ret (string-bracket-find* s col inc br ibr level)   
      (if (>= ret 0) (cons row ret)
	  (with level* (+ ret 1000)
	    (and-with s* (program-row (+ row inc))
	      (with col* (if (> inc 0) 0 (- (string-length s*) 1))
		(program-bracket-find (+ row inc) col* inc
				      br ibr level*))))))))

(tm-define (program-previous-match row br ibr)
  (:synopsis "find matching opening row for @row and bracket @br")
  (let* ((s (program-row row))
         (last (- (string-length s) 1)))
    (if (not s) row
        (with ret (string-bracket-level s last -1 br ibr)
          (if (== ret 0) row
              (with pos (program-bracket-find row last -1 br ibr -1)
                (if (not pos) row
                    (car pos))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracket handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (path++ p)
  (rcons (cDr p) (+ 1 (cAr p))))

(define (path-- p)
  (rcons (cDr p) (- (cAr p) 1)))

(tm-define (select-brackets path lb rb)
  (:synopsis "Highlights innermost matching brackets around given @path")
  (let ((prev (find-left-bracket path lb rb))
        (next (find-right-bracket path lb rb)))
    (if (or (null? prev) (null? next))
      (if (nnull? (get-alt-selection "brackets"))
          (cancel-alt-selection "brackets"))
      (set-alt-selection "brackets" 
                         (list prev (path++ prev) next (path++ next))))))

(define (string-ref* s i)
  (char->string (string-ref s i)))

(tm-define (select-brackets-after-movement lbs rbs esc)
  (:synopsis "Highlight any of @lbs (matching @rbs) after a cursor movement")
  ;; by the syntax tree when there is one (see treesit/ts-edit.scm)
  (or (syntax-select-brackets)
      (select-brackets-by-characters lbs rbs esc)))

(define (select-brackets-by-characters lbs rbs esc)
  (let* ((p (cursor-path))
         (p* (path-- p))
         (ch (program-character p))
         (lch (program-character p*))
         (i1 (string-index lbs ch))
         (i2 (string-index rbs ch))
         (i3 (string-index rbs lch)))
    (cond (i1 (select-brackets p (string-ref* lbs i1) (string-ref* rbs i1)))
          (i2 (select-brackets p* (string-ref* lbs i2) (string-ref* rbs i2)))
          (i3 (select-brackets p* (string-ref* lbs i3) (string-ref* rbs i3)))
          ((nnull? (get-alt-selection "brackets"))
           (cancel-alt-selection "brackets")))))

(tm-define (bracket-open lb rb esc)
  (if prog-auto-close-brackets?
      (if (selection-active-normal?)
          (begin
            (clipboard-cut "temp")
            (insert-go-to (string-append lb rb) '(1))
            (clipboard-paste "temp"))
          (with ch (or (before-cursor) "")
            ; Don't create right bracket if prev char is escape char
            (if (== ch esc)
                (insert lb)
                (insert-go-to (string-append lb rb) '(1)))))
      (insert lb))
  (if prog-highlight-brackets? (select-brackets (cursor-path) lb rb)))

; TODO: warn if unmatched
(tm-define (bracket-close lb rb esc)
  (with p (cursor-path)
    (insert rb)
    (if prog-highlight-brackets? (select-brackets p lb rb))))

;; in code with a syntax tree, the brackets stay balanced (see "Balanced
;; brackets" in treesit/ts-edit.scm), unless the preference "prog:balanced
;; brackets" is off; "prog:automatic brackets" is for the other code
(tm-define (bracket-open lb rb esc)
  (:require (and prog-balanced-brackets? (syntax-cursor-language)))
  (syntax-bracket-open lb rb)
  (if prog-highlight-brackets? (syntax-select-brackets)))

(tm-define (bracket-close lb rb esc)
  (:require (and prog-balanced-brackets? (syntax-cursor-language)))
  (syntax-bracket-close lb rb)
  (if prog-highlight-brackets? (syntax-select-brackets)))

(tm-define (kbd-backspace)
  (:mode in-prog?)
  (:require (and prog-balanced-brackets? (syntax-cursor-language)))
  (when (not (syntax-delete-bracket #f)) (former)))

(tm-define (kbd-delete)
  (:mode in-prog?)
  (:require (and prog-balanced-brackets? (syntax-cursor-language)))
  (when (not (syntax-delete-bracket #t)) (former)))

;; the brackets of the program languages without keys of their own
(tm-define (program-bracket-open lb rb) (insert lb))
(tm-define (program-bracket-close lb rb) (insert rb))

(tm-define (program-bracket-open lb rb)
  (:require (syntax-cursor-language))
  (bracket-open lb rb "\\"))

(tm-define (program-bracket-close lb rb)
  (:require (syntax-cursor-language))
  (bracket-close lb rb "\\"))

; HACK! I'd like to use selection-active-enlarging? But current C++ code
; doesn't mix well with this selection mechanism. Also: if mouse selections
; ever use program-select-enlarge, this has to be disabled.
(define kbd-select-enlarging 0) ; 0=no, 1=started, 2=selecting

(tm-define (program-select-enlarge lb rb)
  ;; by the syntax tree when there is one (see treesit/ts-edit.scm)
  (or (syntax-select-enlarge)
      (program-select-enlarge-by-brackets lb rb)))

(tm-define (kbd-select-enlarge)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-select-enlarge))

;; the structured moves, by the syntax tree (see treesit/ts-edit.scm)
(tm-define (traverse-right)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-move-node 1))

(tm-define (traverse-left)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-move-node -1))

(tm-define (traverse-up)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-move-node 2))

(tm-define (traverse-down)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-move-node -2))

(tm-define (traverse-next)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-move-definition 1))

(tm-define (traverse-previous)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-move-definition -1))

;; the structured removals, by the syntax tree: the brackets around the
;; cursor (keeping what is between them), the node at the cursor or before
(tm-define (program-remove-structure)
  (remove-structure-upwards))

(tm-define (program-remove-structure)
  (:require (syntax-cursor-language))
  (syntax-splice-brackets))

;; the end tag of HTML after the > of a start tag
(tm-define (program-greater)
  (insert "<gtr>")
  (when (and prog-balanced-brackets? (syntax-cursor-language))
    (syntax-close-tag)))

;; folds (std ( and std )): those of the features of program code, which
;; ask tree-sitter or the language servers for the blocks (code-fold.scm)
(tm-define (program-fold) (code-fold))
(tm-define (program-unfold) (code-unfold))

(tm-define (structured-remove-right)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-delete-node #t))

(tm-define (structured-remove-left)
  (:mode in-prog?)
  (:require (syntax-cursor-language))
  (syntax-delete-node #f))

(define (program-select-enlarge-by-brackets lb rb)
  (if (== 0 kbd-select-enlarging)
      (set! kbd-select-enlarging 1)
      (if (and (not (selection-active-any?)) (== 2 kbd-select-enlarging))
          (set! kbd-select-enlarging 1)
          (let* ((start (selection-get-start))
                 (end (selection-get-end))
                 (start* (if (== start end) start (path-- start)))
                 (prev (find-left-bracket start* lb rb))
                 (next (find-right-bracket end lb rb)))
            (if (or (and (== start prev) (== end next))
                    (null? prev) (null? next))
                (begin
                  (set! kbd-select-enlarging 0)
                  (selection-cancel))
                (begin
                  (set! kbd-select-enlarging 2)
                  (selection-set prev (path++ next))))))))

; Cancel any active selection when we leave a code fragment
(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:require (not (in-prog?)))
  ;; (the other hooks run too: language servers)
  (former status)
  (if (nnull? (get-alt-selection "brackets"))
      (cancel-alt-selection "brackets")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("editor:verbatim:tabstop" 4 (lambda (pref val) (noop))))

(define (char-space? c)
  (== c #\space))

(define (char-non-space? c)
  (!= c #\space))

(tm-define (string-space? s)
  (:synopsis "does @s only contain spaces?")
  (list-and (map char-space? (string->list s))))
      
(tm-define (get-tabstop)
  (with tabstop* (get-preference "editor:verbatim:tabstop")
    (cond ((and (string? tabstop*) (string->number tabstop*))
           (string->number tabstop*))
          ((and (number? tabstop*) (> tabstop* 0)) tabstop*)
          (else (set-message
                 `(replace "Wrong tabstop: %1" ,tabstop*) "User preferences")
                8))))

(tm-define (program-tab-width)
  (:synopsis "the columns between the tab stops of the program")
  (with w (string->number (get-env "prog-tab-width"))
    (if (and w (> w 0)) w 8)))

(tm-define (program-indent-tabs?)
  (:synopsis "are the lines of the program indented by tabs?")
  ;; (as its lines are, else as a language wants, else as preferred)
  (with st (get-init "source-indent")
    (cond ((== st "tabs") #t)
          ((== st "spaces") #f)
          ((in? (get-env "prog-language") '("go" "makefile")) #t)
          (else (== (get-preference "editor:code indent") "tabs")))))

(tm-define (program-indent-step)
  (:synopsis "the columns of one level of indentation")
  ;; (a tab where the lines are indented by tabs)
  (if (program-indent-tabs?) (program-tab-width) (get-tabstop)))

(tm-define (string-column s offset)
  (:synopsis "the column of the place at @offset of @s (tabs at their stops)")
  ;; (the columns of the core, System/Language/code_lines.cpp)
  (code-column s offset (program-tab-width)))

(tm-define (insert-tabstop)
  ;; (a tab where the lines are indented by tabs, else spaces up to the
  ;; next tab stop, from the column of the cursor)
  (if (program-indent-tabs?) (insert "<#9>")
      (let* ((w (get-tabstop))
             (t (cursor-tree))
             (col (if (tree-atomic? t)
                      (string-column (tree->string t) (cAr (cursor-path)))
                      (cAr (cursor-path))))
             (fill (- w (remainder col w))))
        (if (> fill 0) (insert (make-string fill #\space))))))

;;HACK: should rewrite program-indent-line to accept unindent
(tm-define (remove-tabstop)
  (with w (program-indent-step)
    (with c (program-get-indent)
      (if (>= c w) (program-set-indent (- c w))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string-indent-end s)
  (:synopsis "the offset in @s of its first character which is not blank")
  (code-indent-end s))

(tm-define (string-get-indent s)
  (:synopsis "get the indentation of @s (a column: tabs at their stops)")
  (string-column s (string-indent-end s)))

(tm-define (string-set-indent s i)
  (:synopsis "set the indentation of @s to the column @i")
  ;; (by tabs then spaces, or by spaces, as the program is indented)
  (let* ((tw (program-tab-width))
         (l (if (program-indent-tabs?)
                (string-append (apply string-append
                                      (make-list (quotient i tw) "<#9>"))
                               (make-string (remainder i tw) #\space))
                (make-string i #\space)))
         (r (substring s (string-indent-end s) (string-length s))))
    (string-append l r)))

(tm-define (program-get-indent)
  (:synopsis "get the indentation of the current line")
  (and (inside-program?)
       (string-get-indent (program-row (program-row-number)))))

(tm-define (program-set-indent i)
  (:synopsis "set the indentation of the current line to @i spaces")
  (when (inside-program?)
    (with t (cursor-tree)
      (tree-set t (string-set-indent (tree->string t) i)))))

; Redefine this for specific languages (by default, by the syntax tree
; when there is one, see treesit/ts-edit.scm)
(tm-define (program-compute-indentation doc row col)
  (or (syntax-compute-indentation doc row (get-tabstop)) 0))

(tm-define (program-indent-line doc row unindent?)
  ; TODO: implement unindent for general languages
  ;; (the offset of the end of the indentation, where the cursor goes)
  (let* ((i (program-compute-indentation doc row -1))
         (t (tree-ref doc row)))
    ; HACK: I should change program-set-indent to accept line numbers
    (tree-set t (string-set-indent (tree->string t) i))
    (string-indent-end (tree->string (tree-ref doc row)))))

(tm-define (program-indent-all unindent?)
  (:synopsis "indent a whole program")
  (and-with doc (program-tree)
    (for-each (lambda (r) (program-indent-line doc r unindent?))
              (iota (tree-arity doc)))))

(tm-define (program-indent unindent?)
  (and-with doc (program-tree)
    (let* ((r (program-row-number))
           (c (program-indent-line doc r unindent?)))
      (program-go-to r c))))

(tm-define (insert-return)
  (:mode in-prog?)
  (insert-raw-return)
  (program-indent #f)
  ;; a block which Return opens gets its closing keyword (end, fi...)
  (when (and prog-balanced-brackets? (syntax-cursor-language))
    (syntax-close-block)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic copy and paste for programming mode 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (textual? x)
  (or (tm-atomic? x)
    (and (tm-in? x '(concat document))
      (forall? textual? (tm-children x)))))

(define (can-convert from to)
  (converter-search (string-append from "-snippet") 
                    (string-append to "-snippet")))

(tm-define (kbd-cut)
  (:mode in-prog?)
  (:require (textual? (selection-tree)))
  (let* ((lan (get-env "prog-language") )
         (fmt (if (can-convert lan "texmacs") lan "verbatim")))
  (clipboard-cut-export fmt "primary")))

(tm-define (kbd-paste)
  (:mode in-prog?)
  (:require (textual? (tm-ref (clipboard-get "primary") 1)))
  (let* ((lan (get-env "prog-language") )
         (fmt (if (can-convert "texmacs" lan) lan "verbatim")))
    (clipboard-paste-import fmt "primary")))
  

