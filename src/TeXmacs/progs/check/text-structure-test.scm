;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-structure-test.scm
;; DESCRIPTION : tests of text structures and dynamic markup, without a window
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite checks the structured editing of texts (progs/text): sections
;; and their numbering, lists, enunciations and proofs, equations, figures
;; and algorithms, footnotes, textual markup, document titles and
;; abstracts, automatic sections such as the table of contents, the
;; variants of environments and the navigation between sections; and the
;; dynamic markup (progs/dynamic): folding, switches and their global
;; operations, and the fields of sessions, edited as document structures
;; without starting a plugin.
;;
;; As in editing-test, each user action is wrapped in edit (archive-state,
;; start-editing, end-editing, update-forced). Two more things matter here:
;;
;;   - make chooses the shape of a new environment (a block with a document,
;;     or an inline tag) from the macro in the typesetting environment,
;;     which only exists once the buffer has been typeset with some text
;;     in it: a document holding only an empty paragraph is never typeset.
;;     The documents of the suite therefore start with a paragraph of text,
;;     and with-doc does a first edit step to typeset the buffer;
;;   - the focus is set by the event loop after each key press, so a
;;     keyboard command which depends on the focus (kbd-return, ...) is
;;     run in its own edit step, after the cursor movement.
;;
;; Paths are relative to the body of the current buffer (see at and cursor).

(texmacs-module (check text-structure-test)
  (:use (check check-lib)
        (utils edit variants)
        (text text-drd)
        (text text-edit)
        (text text-structure)
        (dynamic dynamic-drd)
        (dynamic fold-edit)
        (dynamic session-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-step thunk)
  ;; one user action, as the event loop wraps a key press or a menu action
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define-macro (edit . body)
  `(edit-step (lambda () ,@body)))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (at . l)
  ;; the absolute path of @l in the current buffer
  (append (buffer-path) l))

(define (cursor)
  (list-tail (cursor-path) (length (buffer-path))))

(define (node . l)
  ;; the subtree at @l of the body
  (if (null? l) (buffer-tree) (apply tree-ref (cons (buffer-tree) l))))

(define (snode . l)
  (tree->stree (apply node l)))

(define (with-doc doc thunk)
  ;; run @thunk in a new buffer holding @doc, then close the buffer; an
  ;; error counts as one failure and does not stop the suite
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (update-forced)
    ;; the first edit step typesets the buffer, which defines the macros
    (edit (go-start))
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (stree x)
  ;; a tree, or a list holding trees, as an stree
  (cond ((tree? x) (tree->stree x))
        ((pair? x) (cons (stree (car x)) (stree (cdr x))))
        (else x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-section on an empty paragraph, inside a paragraph (which is split
;; first) and on a selection (which becomes the title); the numbered and
;; unnumbered variants; the sectional variants; return and labels in a
;; section title; unnamed sections (prologue, epilogue).
(define (test-sections)
  (check-group "sections")
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-section 'section))
      (check= (body) '(document "Intro" (section "")))
      (check= (cursor) '(1 0 0))
      (edit (insert "Title"))
      (check= (body) '(document "Intro" (section "Title")))
      (check-true (section-context? (node 1)))
      (check-false (section-context? (node 0)))
      (check-true (numbered-context? (node 1)))
      (check-true (numbered-numbered? (node 1)))
      (check-false (numbered-unnumbered? (node 1)))
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) '(section* "Title"))
      (check-true (section-context? (node 1)))
      (check-true (numbered-unnumbered? (node 1)))
      (check-false (numbered-numbered? (node 1)))
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) '(section "Title"))
      ;; the numbering toggle of an inner tree goes to the section
      (edit (numbered-toggle (node 1 0)))
      (check= (snode 1) '(section* "Title"))
      (edit (numbered-toggle (node 1 0)))
      (check= (snode 1) '(section "Title"))
      ;; variants: from part to subparagraph, in a circle
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(subsection "Title"))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(subsubsection "Title"))
      (edit (variant-circulate (node 1) #f))
      (edit (variant-circulate (node 1) #f))
      (edit (variant-circulate (node 1) #f))
      (check= (snode 1) '(appendix "Title"))
      (edit (variant-set (node 1) 'subparagraph))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(part "Title"))
      (edit (variant-circulate (node 1) #f))
      (check= (snode 1) '(subparagraph "Title"))
      ;; an unnumbered section circulates among unnumbered sections
      (edit (variant-set (node 1) 'section*))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(subsection* "Title"))
      (check= (variants-of 'section)
              '(part chapter appendix section subsection subsubsection
                paragraph subparagraph))
      (check= (variants-of 'subsection*)
              '(part* chapter* appendix* section* subsection*
                subsubsection* paragraph* subparagraph*))
      (check-true (in? 'section* (similar-to 'section)))
      (check-true (in? 'paragraph (similar-to 'section*)))))
  (with-doc '(document "Intro" "Para")
    (lambda ()
      ;; inside a paragraph, the paragraph is split and the section starts
      ;; the second part
      (edit (go-to (at 1 2)) (make-section 'section))
      (check= (body) '(document "Intro" "Pa" (concat (section "") "ra")))
      (check= (cursor) '(2 0 0 0))
      ;; at the start of a paragraph, nothing is split
      (edit (go-to (at 0 0)) (make-section 'chapter))
      (check= (snode 0) '(concat (chapter "") "Intro"))
      (check= (cursor) '(0 0 0 0))))
  (with-doc '(document "Intro" "Para")
    (lambda ()
      ;; a selected paragraph becomes the title; the cursor stays where it
      ;; was, outside the selection
      (edit (selection-set (at 1 0) (at 1 4)) (make-section 'section))
      (check= (body) '(document "Intro" (section "Para")))
      (check= (cursor) '(0 0))))
  (with-doc '(document "Intro" (section "S") "After")
    (lambda ()
      ;; return in a title starts a new paragraph after the section
      (edit (tree-go-to (node 1) 0 :start))
      (check= (stree (focus-tree)) '(section "S"))
      (edit (kbd-return))
      (check= (body) '(document "Intro" (section "S") "" "After"))
      (check= (cursor) '(2 0))))
  (with-doc '(document "Intro" (section "S") "After")
    (lambda ()
      ;; a label goes after the title, and return activates it
      (edit (tree-go-to (node 1) 0 :end) (label-insert (node 1)))
      (check= (body) '(document "Intro"
                                (concat (section "S") (inactive (label "")))
                                "After"))
      (check= (cursor) '(1 1 0 0 0))
      (edit (insert "sec"))
      (edit (kbd-return))
      (check= (body) '(document "Intro" (concat (section "S") (label "sec"))
                                "After"))
      (check= (cursor) '(1 1 1))))
  (with-doc '(document "Intro" "")
    (lambda ()
      ;; an unnamed section is followed by a new paragraph
      ;; FIXME: whether the new paragraph is made depends on the index of
      ;; the buffer in the root tree: make_return_before compares the
      ;; length of the paragraph's document with q->item+1, the first item
      ;; of the path (the buffer), instead of last_item (q)
      ;; (src/Edit/Modify/edit_dynamic.cpp:646): with other buffers open
      ;; before this one, make-unnamed-section 'prologue at the end of
      ;; (document "Intro" "") gives (document "Intro" (prologue)),
      ;; expected (document "Intro" (prologue) "").
      (edit (go-end) (make-unnamed-section 'prologue))
      (check= (snode 0) "Intro")
      (check= (snode 1) '(prologue))
      (check= (tm/section-get-title-string (node 1) #f) "Prologue"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section titles and document parts (text-structure)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define structured-doc
  '(document (chapter "Begin") "p1"
             (section "Alpha") "p2"
             (concat (subsection "Beta") "p3")
             (section* "Gamma") "p4"
             (table-of-contents "toc" (document ""))))

;; The titles of sections, as shown in the menus of document parts and of
;; the section navigation, the principal sections (sections and above in
;; the short sectional style of generic) and the split of a document into
;; parts.
(define (test-section-titles)
  (check-group "section titles")
  (with-doc structured-doc
    (lambda ()
      (check= (tm/section-get-title-string (node 0) #f) "Begin")
      (check= (tm/section-get-title-string (node 1) #f) "no title")
      (check= (tm/section-get-title-string (node 2) #f) "Alpha")
      ;; a section at the start of a paragraph
      (check= (tm/section-get-title-string (node 4) #f) "Beta")
      (check= (tm/section-get-title-string (node 5) #f) "Gamma")
      (check= (tm/section-get-title-string (node 7) #f) "Table of contents")
      ;; indented titles (the short style drops the indentation of sections)
      (check= (tm/section-get-title-string (node 0) #t) "Begin")
      (check= (tm/section-get-title-string (node 2) #t) "Alpha")
      (check= (tm/section-get-title-string (node 4) #t) "   Beta")
      (check= (tm/section-get-title-string
               (stree->tree '(epilogue)) #f) "Epilogue")
      (check= (tm/section-get-title-string
               (stree->tree '(list-of-figures "figure" (document ""))) #f)
              "List of figures")
      (check= (tm/section-get-title-string "plain" #f) "no title")
      ;; the index and the glossary have titles of their own
      (check= (tm/section-get-title-string
               (stree->tree '(the-index "idx" (document ""))) #f)
              "Index")
      (check= (tm/section-get-title-string
               (stree->tree '(the-glossary "gly" (document ""))) #f)
              "Glossary")
      (check= (texmacs->string (stree->tree '(concat "a" (em "b"))))
              "ab")
      (check-true (principal-section? (node 0)))
      (check-false (principal-section? (node 1)))
      (check-true (principal-section? (node 2)))
      (check-false (principal-section? (node 4)))
      (check-true (principal-section? (node 5)))
      (check-true (principal-section? (node 7)))
      (check= (principal-section-title (buffer-tree)) "Begin")
      (check= (principal-section-title
               (stree->tree '(document "x" (section "S") "y")))
              "S")
      (check= (principal-section-title (stree->tree '(document "x" "y")))
              "no title")
      (check= (stree (principal-sections-to-document-parts
                      (tree-children (buffer-tree))))
              '((show-part "1" (document (chapter "Begin") "p1")
                           (document (chapter "Begin")))
                (show-part "2" (document (section "Alpha") "p2"
                                         (concat (subsection "Beta") "p3"))
                           (document (section "Alpha")))
                (show-part "3" (document (section* "Gamma") "p4")
                           (document (section* "Gamma")))
                (show-part "4" (document (table-of-contents
                                          "toc" (document "")))
                           (document (table-of-contents
                                      "toc" (document "")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation between sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; previous-section (the section the cursor is in, used by the focus bar),
;; go-to-section-title, and the moves to the previous and next section
;; titles (structured:cmd section, traverse-next/previous on a title).
(define (test-section-navigation)
  (check-group "section navigation")
  (with-doc structured-doc
    (lambda ()
      (edit (go-to (at 3 1)))
      (check= (stree (previous-section)) '(section "Alpha"))
      (edit (go-to-section-title))
      (check= (cursor) '(2 0 0))
      (edit (go-to (at 4 1 1)))
      (check= (stree (previous-section)) '(subsection "Beta"))
      (edit (go-to-section-title))
      (check= (cursor) '(4 0 0 0))
      (edit (go-to (at 6 2)))
      (check= (stree (previous-section)) '(section* "Gamma"))
      (edit (go-to (at 1 1)))
      (check= (stree (previous-section)) '(chapter "Begin"))
      ;; to the previous section titles, one by one
      (edit (go-to (at 6 2)) (traverse-previous-section-title))
      (check= (cursor) '(5 0 5))
      (edit (traverse-previous-section-title))
      (check= (cursor) '(4 0 0 4))
      (edit (traverse-previous-section-title))
      (check= (cursor) '(2 0 5))
      ;; to the next section titles
      (edit (go-to (at 0 0 0)) (go-to-next-tag (similar-to 'section)))
      (check= (cursor) '(2 0 0))
      (edit (go-to-next-tag (similar-to 'section)))
      (check= (cursor) '(4 0 0 0))
      (edit (go-to-next-tag (similar-to 'section)))
      (check= (cursor) '(5 0 0))
      ;; traversal from a title goes to the similar titles
      (edit (go-to (at 2 0 1)))
      (edit (traverse-next))
      (check= (cursor) '(4 0 0 0))
      (edit (traverse-next))
      (check= (cursor) '(5 0 0))
      (edit (traverse-previous))
      (check= (cursor) '(4 0 0 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Making lists (make-tmlist), new items by return, a new paragraph inside
;; an item by shift-return, nested lists, descriptions (item*), and the
;; list contexts.
(define (test-lists)
  (check-group "lists")
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-tmlist 'itemize))
      (check= (body) '(document "Intro" (itemize (document (item)))))
      (check= (cursor) '(1 0 0 1))
      (edit (insert "one"))
      (edit (kbd-return))
      (check= (body) '(document "Intro"
                                (itemize (document (concat (item) "one")
                                                   (item)))))
      (check= (cursor) '(1 0 1 1))
      (edit (insert "two"))
      ;; shift-return: a new paragraph without an item
      (edit (kbd-shift-return))
      (check= (snode 1) '(itemize (document (concat (item) "one")
                                            (concat (item) "two") "")))
      (check= (cursor) '(1 0 2 0))
      ;; return on an empty paragraph of the list makes an item there
      (edit (kbd-return))
      (check= (snode 1) '(itemize (document (concat (item) "one")
                                            (concat (item) "two") (item))))
      (check= (cursor) '(1 0 2 1))
      (check-true (list-context? (node 1)))
      (check-true (itemize-context? (node 1)))
      (check-false (enumerate-context? (node 1)))
      (check-true (itemize-enumerate-context? (node 1)))
      (check-false (list-context? (node 0)))
      ;; a nested list
      (edit (make-tmlist 'enumerate))
      (check= (snode 1) '(itemize (document (concat (item) "one")
                                            (concat (item) "two") (item)
                                            (enumerate (document (item))))))
      (check= (cursor) '(1 0 3 0 0 1))
      (check-true (enumerate-context? (node 1 0 3)))
      (check-true (itemize-enumerate-context? (node 1 0 3)))
      ;; make-item outside a list makes a plain item
      (edit (insert "sub"))
      (edit (make-item))
      (check= (snode 1 0 3) '(enumerate (document (concat (item) "sub")
                                                  (item))))))
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-tmlist 'description))
      (check= (body) '(document "Intro" (description (document (item* "")))))
      (check= (cursor) '(1 0 0 0 0))
      (check-true (list-context? (node 1)))
      (check-false (itemize-enumerate-context? (node 1)))
      (edit (insert "a"))
      ;; return in the described item goes after it
      (edit (kbd-return))
      (check= (snode 1) '(description (document (item* "a"))))
      (check= (cursor) '(1 0 0 1))
      (edit (insert "b"))
      ;; return after it makes a new described item
      (edit (kbd-return))
      (check= (snode 1) '(description (document (concat (item* "a") "b")
                                                (item* ""))))
      (check= (cursor) '(1 0 1 0 0))))
  (with-doc '(document "Intro" "first" "second")
    (lambda ()
      ;; make-tmlist on a selection of paragraphs
      (edit (selection-set (at 1 0) (at 2 6)) (make-tmlist 'enumerate))
      (check= (tree-label (node 1)) 'enumerate)
      (check-true (tm-find (node 1) (lambda (x) (tm-equal? x "first"))))
      (check-true (tm-find (node 1) (lambda (x) (tm-equal? x "second"))))
      (check-true (tm-find (node 1) (lambda (x) (tm-is? x 'item)))))))

;; Changing the type of a list: numbered-toggle between itemize and
;; enumerate, variant-circulate among the itemize or enumerate variants,
;; variant-set to a description.
(define (test-list-variants)
  (check-group "list variants")
  (with-doc '(document "Intro" (itemize (document (concat (item) "one")
                                                  (concat (item) "two"))))
    (lambda ()
      (define items '(document (concat (item) "one") (concat (item) "two")))
      (check-true (numbered-context? (node 1)))
      (check-false (numbered-numbered? (node 1)))
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) `(enumerate ,items))
      (check-true (numbered-numbered? (node 1)))
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) `(itemize ,items))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) `(itemize-minus ,items))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) `(itemize-dot ,items))
      (edit (variant-circulate (node 1) #f))
      (edit (variant-circulate (node 1) #f))
      (edit (variant-circulate (node 1) #f))
      (check= (snode 1) `(itemize-arrow ,items))
      ;; all itemize variants toggle to enumerate
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) `(enumerate ,items))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) `(enumerate-numeric ,items))
      (edit (variant-circulate (node 1) #f))
      (edit (variant-circulate (node 1) #f))
      (check= (snode 1) `(enumerate-Alpha ,items))
      (check-true (numbered-numbered? (node 1)))
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) `(itemize ,items))
      (edit (variant-set (node 1) 'description))
      (check= (snode 1) `(description ,items))
      (check-false (numbered-context? (node 1)))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) `(description-compact ,items))
      (check= (variants-of 'itemize)
              '(itemize itemize-minus itemize-dot itemize-arrow))
      (check= (variants-of 'enumerate-roman)
              '(enumerate enumerate-numeric enumerate-roman
                enumerate-Roman enumerate-alpha enumerate-Alpha))
      (check= (list-tag-list)
              (append (itemize-tag-list) (enumerate-tag-list)
                      (description-tag-list) (new-list-tag-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enunciations and proofs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Theorem-like environments: making them (a block), a proof inside, the
;; numbered toggle, the variants (theorem, proposition, lemma...), the
;; "due to" annotation and the change between named and unnamed forms.
(define (test-enunciations)
  (check-group "enunciations")
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make 'theorem))
      (check= (body) '(document "Intro" (theorem (document ""))))
      (check= (cursor) '(1 0 0 0))
      (edit (insert "S") (insert-return) (make 'proof))
      (check= (snode 1) '(theorem (document "S" (proof (document "")))))
      (check= (cursor) '(1 0 1 0 0 0))
      (check-true (enunciation-context? (node 1)))
      (check-false (enunciation-context? (node 1 0 1)))
      (check-true (titled-context? (node 1)))
      (check-true (titled-context? (node 1 0 1)))
      (check-false (titled-named? (node 1)))
      (check-true (dueto-supporting-context? (node 1)))
      (check-true (dueto-supporting-context? (node 1 0 1)))
      (check-false (dueto-added? (node 1)))
      (edit (dueto-add (node 1)))
      (check= (snode 1) '(theorem (document (concat (dueto "") "S")
                                            (proof (document "")))))
      (check= (cursor) '(1 0 0 0 0 0))
      (edit (insert "Euler"))
      (check-true (dueto-added? (node 1)))
      ;; a named theorem, and back
      (edit (titled-toggle-name (node 1)))
      (check= (snode 1) '(render-theorem "" (document
                                             (concat (dueto "Euler") "S")
                                             (proof (document "")))))
      (check-true (titled-named? (node 1)))
      (edit (titled-toggle-name (node 1)))
      (check= (snode 1) '(theorem (document (concat (dueto "Euler") "S")
                                            (proof (document "")))))
      ;; a named proof, and back
      (edit (titled-toggle-name (node 1 0 1)))
      (check= (snode 1 0 1) '(render-proof "" (document "")))
      (edit (titled-toggle-name (node 1 0 1)))
      (check= (snode 1 0 1) '(proof (document "")))))
  (with-doc '(document "Intro" (theorem* (document "t"))
                       (remark (document "r")) (exercise (document "e")))
    (lambda ()
      (check-true (numbered-context? (node 1)))
      (check-false (numbered-numbered? (node 1)))
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) '(theorem (document "t")))
      (check-true (numbered-numbered? (node 1)))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(proposition (document "t")))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(lemma (document "t")))
      (edit (variant-circulate (node 1) #f))
      (edit (variant-circulate (node 1) #f))
      (edit (variant-circulate (node 1) #f))
      (check= (snode 1) '(conjecture (document "t")))
      ;; the numbering is kept by the variants
      (edit (numbered-toggle (node 1)))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(theorem* (document "t")))
      (check= (variants-of 'lemma)
              '(theorem proposition lemma corollary conjecture))
      (check= (variants-of 'remark)
              '(remark note example convention warning acknowledgments))
      (edit (variant-circulate (node 2) #t))
      (check= (snode 2) '(note (document "r")))
      (edit (titled-toggle-name (node 2)))
      (check= (snode 2) '(render-remark "" (document "r")))
      (edit (titled-toggle-name (node 2)))
      (check= (snode 2) '(remark (document "r")))
      (edit (titled-toggle-name (node 3)))
      (check= (snode 3) '(render-exercise "" (document "e")))
      (edit (titled-toggle-name (node 3)))
      (check= (snode 3) '(exercise (document "e")))
      (check-true (enunciation-context? (node 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equations, figures, algorithms, frames, notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-other-environments)
  (check-group "other environments")
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-equation))
      (check= (body) '(document "Intro" (equation (document ""))))
      (check= (cursor) '(1 0 0 0))
      (check-true (numbered-numbered? (node 1)))
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) '(equation* (document "")))
      (edit (go-end) (insert-return) (make-equation*))
      (check= (snode 2) '(equation* (document "")))
      (check= (cursor) '(2 0 0 0))))
  (with-doc '(document "Intro" (big-figure "pic" "cap")
                       (small-table "tab" "cap"))
    (lambda ()
      (check-true (figure-context? (node 1)))
      (check-true (floatable-context? (node 1)))
      (check-false (floatable-context? (node 2)))
      (edit (numbered-toggle (node 1)))
      (check= (snode 1) '(big-figure* "pic" "cap"))
      (edit (numbered-toggle (node 1)))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(big-table "pic" "cap"))
      ;; the variants of a big figure are the big figures and tables
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(big-figure "pic" "cap"))
      (edit (variant-circulate (node 2) #t))
      (check= (snode 2) '(small-figure "tab" "cap"))
      (edit (variant-circulate (node 2) #t))
      (check= (snode 2) '(small-table "tab" "cap"))
      ;; the named form keeps the type of the figure or table
      (edit (titled-toggle-name (node 2)))
      (check= (snode 2) '(render-small-figure "table" "" "tab" "cap"))
      (edit (titled-toggle-name (node 2)))
      (check= (snode 2) '(small-table "tab" "cap"))
      (edit (variant-circulate (node 1) #t))
      (edit (titled-toggle-name (node 1)))
      (check= (snode 1) '(render-big-figure "table" "" "pic" "cap"))
      (edit (titled-toggle-name (node 1)))
      (check= (snode 1) '(big-table "pic" "cap"))
      (edit (variant-circulate (node 1) #t))
      (edit (variant-circulate (node 2) #t))
      (check= (snode 2) '(small-figure "tab" "cap"))
      (edit (titled-toggle-name (node 1)))
      (check= (snode 1) '(render-big-figure "figure" "" "pic" "cap"))
      (edit (titled-toggle-name (node 1)))
      (check= (snode 1) '(big-figure "pic" "cap"))
      (edit (titled-toggle-name (node 2)))
      (check= (snode 2) '(render-small-figure "figure" "" "tab" "cap"))
      (edit (titled-toggle-name (node 2)))
      (check= (snode 2) '(small-figure "tab" "cap"))
      ;; an unnamed figure with no type is a figure
      (edit (tree-assign (node 2) '(render-small-figure "" "" "tab" "cap")))
      (edit (titled-toggle-name (node 2)))
      (check= (snode 2) '(small-figure "tab" "cap"))
      ;; a floating figure, anchored at the end of the previous paragraph,
      ;; and back
      (edit (turn-floating (node 1)))
      (check= (body) '(document (concat "Intro" (float "float" "thb"
                                                       (big-figure "pic" "cap")))
                                (small-figure "tab" "cap")))
      (check= (cursor) '(0 1 1))
      (check-true (float-context? (node 0 1)))
      (check-true (float-or-footnote-context? (node 0 1)))
      (check-false (floatable-context? (node 0 1 2)))
      (check-true (rich-float-context? (node 0 1 2)))
      (check-false (float-wide? (node 0 1 2)))
      (edit (float-toggle-wide (node 0 1 2)))
      (check= (snode 0 1) '(wide-float "float" "thb" (big-figure "pic" "cap")))
      (check-true (float-wide? (node 0 1 2)))
      (edit (float-toggle-wide (node 0 1 2)))
      (check= (snode 0 1) '(float "float" "thb" (big-figure "pic" "cap")))
      (edit (turn-non-floating (node 0 1)))
      (check-false (tm-find (buffer-tree) (lambda (x) (tm-is? x 'float))))
      (check-true (tm-find (buffer-tree)
                           (lambda (x) (tm-equal? x '(big-figure "pic" "cap")))))))
  (with-doc '(document "Intro" (algorithm (document "x")))
    (lambda ()
      (check-true (algorithm-context? (node 1)))
      (check-true (algorithm-numbered? (node 1)))
      (check-false (algorithm-named? (node 1)))
      (check-false (algorithm-specified? (node 1)))
      (check= (algorithm-root 'named-specified-algorithm*) 'algorithm)
      (edit (algorithm-toggle-number (node 1)))
      (check= (snode 1) '(algorithm* (document "x")))
      (check-false (algorithm-numbered? (node 1)))
      (edit (algorithm-toggle-number (node 1)))
      (check= (snode 1) '(algorithm (document "x")))
      (edit (algorithm-toggle-name (node 1)))
      (check= (snode 1) '(named-algorithm "" (document "x")))
      (check= (cursor) '(1 0 0))
      (check-true (algorithm-named? (node 1)))
      (edit (insert "Euclid"))
      (edit (algorithm-toggle-specification (node 1)))
      (check= (snode 1) '(named-specified-algorithm "Euclid" (document "")
                                                    (document "x")))
      (check= (cursor) '(1 1 0 0))
      (check-true (algorithm-specified? (node 1)))
      (edit (algorithm-toggle-name (node 1)))
      (check= (snode 1) '(specified-algorithm (document "") (document "x")))
      (edit (algorithm-toggle-specification (node 1)))
      (check= (snode 1) '(algorithm (document "x")))))
  (with-doc '(document "Intro" (framed (document "f"))
                       (note-footnote "n") (note-footnote* "m" "*"))
    (lambda ()
      (check-true (frame-context? (node 1)))
      (check-false (frame-titled? (node 1)))
      (edit (frame-toggle-title (node 1)))
      (check= (snode 1) '(framed-titled (document "f") ""))
      (check= (cursor) '(1 1 0))
      (check-true (frame-titled? (node 1)))
      (check-true (frame-titled-context? (node 1)))
      (edit (insert "Box") (frame-toggle-title (node 1)))
      (check= (snode 1) '(framed (document "f")))
      (check= (cursor) '(1 0 0 1))
      (edit (variant-circulate (node 1) #f))
      (check= (snode 1) '(verticallined (document "f")))
      (check-true (detached-note-context? (node 2)))
      (check-true (auto-note-context? (node 2)))
      (check-true (custom-note-context? (node 3)))
      (edit (note-toggle-custom (node 2)))
      (check= (snode 2) '(note-footnote* "n" "<dag>"))
      (edit (note-toggle-custom (node 2)))
      (check= (snode 2) '(note-footnote "n"))
      (edit (note-toggle-custom (node 3)))
      (check= (snode 3) '(note-footnote "m")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Footnotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A footnote holds a document; it can be made wide; the cursor goes from
;; the footnote to its anchor and back.
(define (test-footnotes)
  (check-group "footnotes")
  (with-doc '(document "Hello")
    (lambda ()
      (edit (go-end) (make 'footnote))
      (check= (body) '(document (concat "Hello" (footnote (document "")))))
      (check= (cursor) '(0 1 0 0 0))
      (edit (insert "note"))
      (check= (body) '(document (concat "Hello" (footnote (document "note")))))
      (check-true (footnote-context? (node 0 1)))
      (check-false (float-context? (node 0 1)))
      (check-true (float-or-footnote-context? (node 0 1)))
      (check-false (float-wide? (node 0 1 0)))
      (edit (float-toggle-wide (node 0 1 0)))
      (check= (snode 0 1) '(wide-footnote (document "note")))
      (check-true (float-wide? (node 0 1 0)))
      (check-true (footnote-context? (node 0 1)))
      (check= (cursor) '(0 1 0 0 4))
      (check-false (cursor-at-anchor?))
      (edit (go-to-anchor))
      (check= (cursor) '(0 1 1))
      (check-true (cursor-at-anchor?))
      (edit (go-to-float))
      (check= (cursor) '(0 1 0 0 0))
      (edit (cursor-toggle-anchor))
      (check= (cursor) '(0 1 1))
      (edit (cursor-toggle-anchor))
      (check= (cursor) '(0 1 0 0 0))
      (edit (float-toggle-wide (node 0 1 0)))
      (check= (snode 0 1) '(footnote (document "note"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textual markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emphasis and strong text on a selection, the toggles of bold, italic and
;; underlined text (Format menu, shortcuts), and the textual variants.
(define (test-markup)
  (check-group "markup")
  (with-doc '(document "Hello world")
    (lambda ()
      (edit (selection-set (at 0 0) (at 0 5)) (make 'em))
      (check= (body) '(document (concat (em "Hello") " world")))
      (check= (cursor) '(0 0 0 5))
      (edit (variant-circulate (node 0 0) #t))
      (check= (snode 0 0) '(dfn "Hello"))
      (edit (variant-circulate (node 0 0) #f))
      (edit (variant-circulate (node 0 0) #f))
      (check= (snode 0 0) '(strong "Hello"))
      (check= (variants-of 'em) '(strong em dfn underline))
      ;; toggling bold at the end: a bold part is started, and toggling
      ;; again at its end leaves it
      (edit (go-to (at 0 1 6)) (toggle-bold) (insert "B"))
      (check= (body) '(document (concat (strong "Hello") " world"
                                        (with "font-series" "bold" "B"))))
      (check= (cursor) '(0 2 2 1))
      (edit (toggle-bold) (insert "C"))
      (check= (body) '(document (concat (strong "Hello") " world"
                                        (with "font-series" "bold" "B")
                                        "C")))
      (check= (cursor) '(0 3 1))
      (edit (toggle-italic) (insert "I"))
      (check= (snode 0 4) '(with "font-shape" "italic" "I"))
      (edit (toggle-italic) (insert "J"))
      (check= (snode 0 5) "J")
      ;; underlined has no opposite: toggling removes the underline
      (edit (toggle-underlined) (insert "U"))
      (check= (snode 0 6) '(underline "U"))
      (check= (cursor) '(0 6 0 1))
      (edit (toggle-underlined))
      (check= (snode 0) '(concat (strong "Hello") " world"
                                 (with "font-series" "bold" "B") "C"
                                 (with "font-shape" "italic" "I") "JU"))
      ;; on a selection, the selection is made bold
      (edit (selection-set (at 0 1 1) (at 0 1 6)) (toggle-bold))
      (check= (snode 0 2) '(with "font-series" "bold" "world"))
      (check= (snode 0 1) " ")))
  (with-doc '(document "Some text")
    (lambda ()
      (edit (go-end) (make 'strong) (insert "bold"))
      (check= (body) '(document (concat "Some text" (strong "bold"))))
      (check= (cursor) '(0 1 0 4))
      (edit (go-end) (make 'name) (insert "Knuth"))
      (check= (snode 0 2) '(name "Knuth"))
      (check= (variants-of 'name) '(name person cite*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Titles, authors and abstracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-doc-data)
  (check-group "title and abstract")
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end))
      (check-true (document-propose-title?))
      (check-false (document-propose-abstract?))
      (edit (make-doc-data))
      (check= (body) '(document "Intro" (doc-data (doc-title ""))))
      (check= (cursor) '(1 0 0 0))
      (check-true (doc-title-context? (node 1 0)))
      (edit (insert "T"))
      ;; return in the title adds an author
      (edit (kbd-return))
      (check= (snode 1) '(doc-data (doc-title "T")
                                   (doc-author (author-data
                                                (author-name "")))))
      (check= (cursor) '(1 1 0 0 0 0))
      (check-true (doc-author-context? (node 1 1 0 0)))
      (check-false (doc-author-context? (node 1 0)))
      (edit (insert "Me"))
      ;; return in the name of the author adds an affiliation
      (edit (kbd-return))
      (check= (snode 1 1) '(doc-author (author-data
                                        (author-name "Me")
                                        (author-affiliation (document "")))))
      (check= (cursor) '(1 1 0 1 0 0 0))
      (edit (make-author-data-element 'author-email))
      (check= (snode 1 1) '(doc-author (author-data
                                        (author-name "Me")
                                        (author-affiliation (document ""))
                                        (author-email ""))))
      (check= (cursor) '(1 1 0 2 0 0))
      (edit (make-doc-data-element 'doc-date))
      (check= (snode 1) '(doc-data (doc-title "T")
                                   (doc-author
                                    (author-data
                                     (author-name "Me")
                                     (author-affiliation (document ""))
                                     (author-email "")))
                                   (doc-date "")))
      (check= (cursor) '(1 2 0 0))
      (check-false (doc-data-has-hidden?))
      ;; a running title is made inactive (hidden)
      (edit (make-doc-data-element 'doc-running-title))
      (check= (snode 1 3) '(doc-inactive (doc-running-title "")))
      (check= (cursor) '(1 3 0 0 0))
      (check-true (doc-data-has-hidden?))
      (check-true (doc-data-deactivated?))
      (edit (doc-data-activate-all))
      (check= (snode 1 3) '(doc-running-title ""))
      (check-false (doc-data-deactivated?))
      (check-true (doc-data-has-hidden?))
      (edit (doc-data-activate-toggle))
      (check= (snode 1 3) '(doc-inactive (doc-running-title "")))
      (edit (doc-data-activate-toggle))
      (check= (snode 1 3) '(doc-running-title ""))
      ;; title options
      (edit (set-doc-title-clustering "cluster-all"))
      (check= (get-doc-title-options) '("cluster-all"))
      (check-true (test-doc-title-clustering? "cluster-all"))
      (check-false (test-doc-title-clustering? #f))
      (check= (snode 1 3) '(doc-title-options "cluster-all"))
      (edit (set-doc-title-clustering #f))
      (check= (get-doc-title-options) '())
      (check-true (test-doc-title-clustering? #f))
      (check= (snode 1) '(doc-data (doc-title "T")
                                   (doc-author
                                    (author-data
                                     (author-name "Me")
                                     (author-affiliation (document ""))
                                     (author-email "")))
                                   (doc-date "")
                                   (doc-running-title "")))))
  (with-doc '(document (doc-data (doc-title "T")
                                 (doc-inactive (doc-running-title "R"))
                                 (doc-author (author-data
                                              (author-name "A"))))
                       "Intro")
    (lambda ()
      ;; return in an inactive element activates it
      (edit (tree-go-to (node 0) 1 0 0 :end))
      (edit (kbd-return))
      (check= (snode 0 1) '(doc-running-title "R"))
      (check= (cursor) '(0 0 0 1))
      ;; a subtitle, removed again when empty
      (edit (make-doc-data-element 'doc-subtitle))
      (check= (snode 0 1) '(doc-subtitle ""))
      (check= (cursor) '(0 1 0 0))
      (edit (kbd-remove (focus-tree) #f))
      (check= (snode 0) '(doc-data (doc-title "T") (doc-running-title "R")
                                   (doc-author (author-data
                                                (author-name "A")))))
      (check= (cursor) '(0 0 0 1))
      (check-false (document-propose-title?))))
  (with-doc '(document (doc-data (doc-title "T")) "")
    (lambda ()
      (edit (go-end))
      (check-false (document-propose-title?))
      (check-true (document-propose-abstract?))
      (edit (make-abstract-data))
      (check= (snode 1) '(abstract-data (abstract "")))
      (check= (cursor) '(1 0 0 0))
      (check-true (abstract-data-context? (node 1 0)))
      (edit (insert "A") (make-abstract-data-element 'abstract-keywords))
      (check= (snode 1) '(abstract-data (abstract "A") (abstract-keywords "")))
      (check= (cursor) '(1 1 0 0))
      (edit (insert "k1"))
      ;; return in the keywords adds a keyword
      (edit (kbd-return))
      (check= (snode 1 1) '(abstract-keywords "k1" (concat "")))
      (check= (cursor) '(1 1 1 0 0))
      (check-false (document-propose-abstract?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The table of contents and the bibliography, as the Insert menu makes
;; them, and the renaming of an automatic section.
(define (test-automatic-sections)
  (check-group "automatic sections")
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-aux "table-of-contents" "toc-prefix" "toc"))
      (check= (body) '(document "Intro" (table-of-contents "toc" (document ""))))
      (check= (cursor) '(1 1))
      (check-true (automatic-section-context? (node 1)))
      (check-false (section-context? (node 1)))
      (check-true (principal-section? (node 1)))
      (edit (insert-return) (make-aux "bibliography" "bib-prefix" "bib"))
      (check= (snode 2) '(bibliography "bib" (document "")))
      (edit (insert-return) (make-bib "refs.bib"))
      (check= (snode 3) '(bibliography "bib" "tm-plain" "refs.bib" (document "")))
      (check= (cursor) '(3 3 0 0))
      (edit (tree-go-to (node 3) :end) (insert-return)
            (make-aux* "the-index" "index-prefix" "idx" "Index"))
      (check= (snode 4) '(the-index "idx" "Index" (document "")))
      (edit (tree-go-to (node 1) 1 :start)
            (automatic-section-rename "Contents"))
      (check= (snode 1) '(table-of-contents* "toc" "Contents" (document "")))
      ;; a renamed automatic section is still a principal section, with
      ;; its new name as title
      (check= (tm/section-get-title-string (node 1) #f) "Contents")
      (check-true (principal-section? (node 1)))
      (check= (tm/section-get-title-string
               (stree->tree '(bibliography* "bib" "tm-plain" "refs.bib"
                                            "Sources" (document ""))) #f)
              "Sources")
      (check-false (automatic-section-context? (node 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fold tags (folded/unfolded and their variants), summarized/detailed
;; tags: making them, fold and unfold at the cursor, alternate-toggle, the
;; dynamic moves (first, previous, next, last), the variants, maximizing
;; and minimizing, and the mouse.
(define (test-folding)
  (check-group "folding")
  (with-doc '(document "Intro" "")
    (lambda ()
      (define folded '(folded (document "sum") (document "det")))
      (define unfolded '(unfolded (document "sum") (document "det")))
      (edit (go-end) (make-toggle 'folded))
      (check= (body) '(document "Intro" (folded (document "") (document ""))))
      (check= (cursor) '(1 0 0 0))
      (edit (insert "sum") (tree-go-to (node 1) 1 :end) (insert "det"))
      (check= (snode 1) folded)
      (check-true (toggle-context? (node 1)))
      (check-true (toggle-first-context? (node 1)))
      (check-false (toggle-second-context? (node 1)))
      (check-true (fold-context? (node 1)))
      (check-true (dynamic-context? (node 1)))
      (check-false (switch-context? (node 1)))
      (check-true (alternate-first? (node 1)))
      (check-false (alternate-second? (node 1)))
      (edit (go-to (at 1 0 0 1)))
      (edit (unfold))
      (check= (snode 1) unfolded)
      (check= (cursor) '(1 1 0 0))
      (check-true (toggle-second-context? (node 1)))
      (check-true (alternate-second? (node 1)))
      ;; unfolding an unfolded tag does nothing
      (edit (unfold))
      (check= (snode 1) unfolded)
      (edit (fold))
      (check= (snode 1) folded)
      (check= (cursor) '(1 0 0 0))
      (edit (fold))
      (check= (snode 1) folded)
      (edit (alternate-toggle (node 1)))
      (check= (snode 1) unfolded)
      (edit (alternate-toggle (node 1)))
      (check= (snode 1) folded)
      ;; the dynamic moves unfold and fold
      (edit (dynamic-next))
      (check= (snode 1) unfolded)
      (check= (cursor) '(1 1 0 0))
      (edit (dynamic-previous))
      (check= (snode 1) folded)
      (check= (cursor) '(1 0 0 0))
      (edit (dynamic-last))
      (check= (snode 1) unfolded)
      (edit (dynamic-first))
      (check= (snode 1) folded)
      (edit (structured-maximize (node 1)))
      (check= (snode 1) unfolded)
      (edit (structured-minimize (node 1)))
      (check= (snode 1) folded)
      (edit (tree-show-hidden (node 1)))
      (check= (snode 1) unfolded)
      (edit (tree-show-hidden (node 1)))
      (check= (snode 1) folded)
      (edit (mouse-unfold (node 1 0)))
      (check= (snode 1) unfolded)
      (edit (mouse-fold (node 1 0)))
      (check= (snode 1) folded)
      ;; variants
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(folded-plain (document "sum") (document "det")))
      (edit (alternate-toggle (node 1)))
      (check= (snode 1) '(unfolded-plain (document "sum") (document "det")))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(unfolded-std (document "sum") (document "det")))
      (edit (variant-circulate (node 1) #f))
      (edit (variant-circulate (node 1) #f))
      (check= (snode 1) unfolded)
      (check= (variants-of 'folded)
              '(folded folded-plain folded-std folded-explain folded-env
                folded-documentation folded-grouped))
      (check= (symbol-toggle-alternate 'folded-env) 'unfolded-env)
      (check= (symbol-toggle-alternate 'unfolded-env) 'folded-env)))
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-toggle 'summarized))
      (check= (snode 1) '(summarized (document "") (document "")))
      (check-true (toggle-first-context? (node 1)))
      (check-false (fold-context? (node 1)))
      (edit (unfold))
      (check= (snode 1) '(detailed (document "") (document "")))
      (check= (cursor) '(1 1 0 0))
      (edit (fold))
      (check= (snode 1) '(summarized (document "") (document "")))
      (check= (symbol-toggle-alternate 'summarized-tiny) 'detailed-tiny))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Switches (one shown child), unrolls (the children up to the current one
;; are shown): inserting and removing children, the moves between them,
;; rotation, showing hidden children, variants.
(define (test-switches)
  (check-group "switches")
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-switch 'switch))
      (check= (body) '(document "Intro" (switch (shown (document "")))))
      (check= (cursor) '(1 0 0 0 0))
      (edit (insert "A"))
      (check-true (switch-context? (node 1)))
      (check-true (alternative-context? (node 1)))
      (check-true (dynamic-context? (node 1)))
      (check-false (toggle-context? (node 1)))
      (check= (switch-index (node 1)) 0)
      (check= (switch-index (node 1) :visible) 0)
      ;; a new child after the current one, which is shown
      (edit (structured-insert-horizontal (node 1) #t))
      (check= (snode 1) '(switch (hidden (document "A")) (shown (document ""))))
      (check= (cursor) '(1 1 0 0 0))
      (edit (insert "B"))
      (edit (structured-insert-horizontal (node 1) #t) (insert "C"))
      (check= (snode 1) '(switch (hidden (document "A")) (hidden (document "B"))
                                 (shown (document "C"))))
      (check= (cursor) '(1 2 0 0 1))
      (check= (switch-index (node 1)) 2)
      (check= (switch-index (node 1) :previous) 1)
      (check= (switch-index (node 1) :next) 2)
      (check= (switch-index (node 1) :rotate-forward) 0)
      (check= (switch-index (node 1) :last) 2)
      (check-true (hidden-context? (node 1 0)))
      (check-false (hidden-context? (node 1 2)))
      ;; previous: to the end of the previous child
      (edit (dynamic-previous))
      (check= (snode 1) '(switch (hidden (document "A")) (shown (document "B"))
                                 (hidden (document "C"))))
      (check= (cursor) '(1 1 0 0 1))
      (edit (dynamic-previous))
      (check= (snode 1) '(switch (shown (document "A")) (hidden (document "B"))
                                 (hidden (document "C"))))
      (check= (cursor) '(1 0 0 0 1))
      ;; there is nothing before the first child
      (edit (dynamic-previous))
      (check= (snode 1) '(switch (shown (document "A")) (hidden (document "B"))
                                 (hidden (document "C"))))
      ;; next: to the start of the next child
      (edit (dynamic-next))
      (check= (snode 1) '(switch (hidden (document "A")) (shown (document "B"))
                                 (hidden (document "C"))))
      (check= (cursor) '(1 1 0 0 0))
      (edit (dynamic-last))
      (check= (snode 1) '(switch (hidden (document "A")) (hidden (document "B"))
                                 (shown (document "C"))))
      (check= (cursor) '(1 2 0 0 1))
      (edit (dynamic-first))
      (check= (snode 1) '(switch (shown (document "A")) (hidden (document "B"))
                                 (hidden (document "C"))))
      (check= (cursor) '(1 0 0 0 0))
      ;; rotation
      (edit (alternate-toggle (node 1)))
      (check= (snode 1) '(switch (hidden (document "A")) (shown (document "B"))
                                 (hidden (document "C"))))
      (edit (switch-to (node 1) :last))
      (check= (snode 1) '(switch (hidden (document "A")) (hidden (document "B"))
                                 (shown (document "C"))))
      (edit (alternate-toggle (node 1)))
      (check= (snode 1) '(switch (shown (document "A")) (hidden (document "B"))
                                 (hidden (document "C"))))
      (check= (cursor) '(1 0 0 0 0))
      ;; removal of the current child
      (edit (structured-remove-horizontal (node 1) #t))
      (check= (snode 1) '(switch (shown (document "B")) (hidden (document "C"))))
      (check= (cursor) '(1 0 0 0 0))
      (edit (switch-remove-at (node 1) :first))
      (check= (snode 1) '(switch (shown (document "C"))))
      ;; the last child is never removed
      (edit (switch-remove-at (node 1) :first))
      (check= (snode 1) '(switch (shown (document "C"))))
      ;; an insertion before the current child
      (edit (structured-insert-horizontal (node 1) #f) (insert "Z"))
      (check= (snode 1) '(switch (shown (document "Z")) (hidden (document "C"))))))
  (with-doc '(document "Intro" (switch (shown (document "x"))
                                       (hidden (document "y"))))
    (lambda ()
      (edit (tree-go-to (node 1) 1 0 0 :start))
      (edit (cursor-show-hidden))
      (check= (snode 1) '(switch (hidden (document "x")) (shown (document "y"))))
      (edit (tree-go-to (node 1) 0 0 0 :start))
      (edit (tree-show-hidden (node 1)))
      (check= (snode 1) '(switch (shown (document "x")) (hidden (document "y"))))
      ;; variants of big switches
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(screens (shown (document "x")) (hidden (document "y"))))
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(unroll (shown (document "x")) (hidden* (document "y"))))
      (edit (variant-circulate (node 1) #f))
      (check= (snode 1) '(screens (shown (document "x")) (hidden (document "y"))))))
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-switch 'unroll))
      (check= (snode 1) '(unroll (shown (document ""))))
      (check-false (alternative-context? (node 1)))
      (check-true (switch-context? (node 1)))
      (edit (insert "A") (structured-insert-horizontal (node 1) #t) (insert "B"))
      (check= (snode 1) '(unroll (shown (document "A")) (shown (document "B"))))
      (edit (structured-insert-horizontal (node 1) #t) (insert "C"))
      (check= (snode 1) '(unroll (shown (document "A")) (shown (document "B"))
                                 (shown (document "C"))))
      (edit (dynamic-first))
      (check= (snode 1) '(unroll (shown (document "A")) (hidden* (document "B"))
                                 (hidden* (document "C"))))
      (check= (cursor) '(1 0 0 0 0))
      (edit (dynamic-next))
      (check= (snode 1) '(unroll (shown (document "A")) (shown (document "B"))
                                 (hidden* (document "C"))))
      (check= (switch-index (node 1) :visible) 1)
      (edit (variant-circulate (node 1) #t))
      (check= (snode 1) '(unroll-compressed (shown (document "A"))
                                            (shown (document "B"))
                                            (hidden* (document "C"))))
      (edit (switch-select (node 1) 2))
      (check= (snode 1) '(unroll-compressed (shown (document "A"))
                                            (shown (document "B"))
                                            (shown (document "C"))))))
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-switch 'tiny-switch))
      (check= (snode 1) '(tiny-switch (shown "")))
      (check= (cursor) '(1 0 0 0))
      (edit (insert "a") (structured-insert-horizontal (node 1) #t) (insert "b"))
      (check= (snode 1) '(tiny-switch (hidden "a") (shown "b")))
      (edit (go-end) (insert-return) (make-switch 'expanded))
      (check= (snode 2) '(expanded (shown (document ""))))
      (edit (insert "p") (structured-insert-horizontal (node 2) #t) (insert "q"))
      (check= (snode 2) '(expanded (shown (document "p")) (shown (document "q")))))))

;; Switches whose children are list items (Insert, Fold, Unroll, Itemize).
(define (test-switch-lists)
  (check-group "switch lists")
  (with-doc '(document "Intro" "")
    (lambda ()
      (edit (go-end) (make-switch-list 'unroll 'itemize))
      (check= (snode 1) '(itemize (document (unroll (shown (document (item)))))))
      (check= (cursor) '(1 0 0 0 0 0 1))
      (check-true (switch-list-context? (node 1 0 0)))
      (edit (insert "one"))
      (edit (kbd-return))
      (check= (snode 1) '(itemize (document
                                   (unroll (shown (document
                                                   (concat (item) "one")))
                                           (shown (document (item)))))))
      (check= (cursor) '(1 0 0 1 0 0 1))))
  (with-doc '(document "Intro" (itemize (document (concat (item) "a")
                                                  (concat (item) "b"))))
    (lambda ()
      (edit (tree-select (node 1)))
      (edit (make-unroll 'unroll))
      (check= (snode 1) '(itemize (document
                                   (unroll (shown (document (concat (item) "a")))
                                           (shown (document
                                                   (concat (item) "b")))))))
      (check= (cursor) '(1 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global dynamic operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dynamic-doc
  '(document "Intro"
             (folded (document "a") (document "b"))
             (unfolded (document "c") (document (theorem (document "d"))))
             (switch (shown (document "x")) (hidden (document "y")))
             (unroll (shown (document "u")) (hidden* (document "v")))))

;; Fold, unfold, expand and compress everything (the Fold menu in the
;; Document menu), the first and last states, folding by environment,
;; filtering of the buffer, and the traversal of presentations.
(define (test-dynamic-global)
  (check-group "dynamic global")
  (with-doc dynamic-doc
    (lambda ()
      (edit (dynamic-operate-on-buffer :unfold))
      (check= (body) '(document "Intro"
                                (unfolded (document "a") (document "b"))
                                (unfolded (document "c")
                                          (document (theorem (document "d"))))
                                (switch (shown (document "x"))
                                        (hidden (document "y")))
                                (unroll (shown (document "u"))
                                        (hidden* (document "v")))))
      (edit (dynamic-operate-on-buffer :fold))
      (check= (snode 1) '(folded (document "a") (document "b")))
      (check= (snode 2) '(folded (document "c")
                                 (document (theorem (document "d")))))
      (edit (dynamic-operate-on-buffer :expand))
      (check= (body) '(document "Intro"
                                (unfolded (document "a") (document "b"))
                                (unfolded (document "c")
                                          (document (theorem (document "d"))))
                                (switch (shown (document "x"))
                                        (shown (document "y")))
                                (unroll (shown (document "u"))
                                        (shown (document "v")))))
      (edit (dynamic-operate-on-buffer :compress))
      (check= (body) dynamic-doc*)
      (edit (dynamic-operate-on-buffer :last))
      (check= (body) '(document "Intro"
                                (unfolded (document "a") (document "b"))
                                (unfolded (document "c")
                                          (document (theorem (document "d"))))
                                (switch (hidden (document "x"))
                                        (shown (document "y")))
                                (unroll (shown (document "u"))
                                        (shown (document "v")))))
      (check= (cursor) '(4 1))
      (edit (dynamic-operate-on-buffer :first))
      (check= (body) dynamic-doc*)
      (check= (cursor) '(0 0))
      ;; folding and unfolding by environment
      (call-with-values fold-get-environments-in-buffer
        (lambda (l first second)
          (check= l '("text" "theorem"))
          ;; everything is folded: the theorem is in a folded body
          (check-true (ahash-ref first 'theorem))
          (check-true (ahash-ref first 'text))
          (check-false (ahash-ref second 'theorem))))
      (edit (dynamic-operate-on-buffer '(:unfold theorem)))
      (check= (snode 1) '(folded (document "a") (document "b")))
      (check= (snode 2) '(unfolded (document "c")
                                   (document (theorem (document "d")))))
      (edit (dynamic-operate-on-buffer '(:fold text)))
      (check= (snode 2) '(unfolded (document "c")
                                   (document (theorem (document "d")))))
      (edit (dynamic-operate-on-buffer '(:fold theorem)))
      (check= (snode 2) '(folded (document "c")
                                 (document (theorem (document "d")))))))
  (with-doc dynamic-doc
    (lambda ()
      ;; removing the folded parts: the folded tag and the switches which
      ;; show their first child
      (edit (dynamic-filter-buffer :remove-folded))
      (check= (body) '(document "Intro"
                                (unfolded (document "c")
                                          (document (theorem (document "d"))))))))
  (with-doc dynamic-doc
    (lambda ()
      (edit (dynamic-filter-buffer :remove-unfolded))
      (check= (body) '(document "Intro"
                                (folded (document "a") (document "b"))
                                (switch (shown (document "x"))
                                        (hidden (document "y")))
                                (unroll (shown (document "u"))
                                        (hidden* (document "v")))))))
  (with-doc dynamic-doc
    (lambda ()
      ;; keeping the unfolded parts removes everything else
      (edit (dynamic-filter-buffer :keep-unfolded))
      (check= (body) '(document (unfolded (document "c")
                                          (document
                                           (theorem (document "d"))))))))
  (with-doc '(document "Intro" (folded (document "a") (document "b"))
                       (switch (shown (document "x")) (hidden (document "y"))))
    (lambda ()
      ;; the traversal of a presentation: one step at a time
      (edit (dynamic-traverse-buffer :next))
      (check= (snode 1) '(unfolded (document "a") (document "b")))
      (check= (cursor) '(1 1 0 1))
      (edit (dynamic-traverse-buffer :next))
      (check= (snode 2) '(switch (hidden (document "x")) (shown (document "y"))))
      (check= (cursor) '(2 1 0 0 0))
      ;; at the end, nothing changes
      (edit (dynamic-traverse-buffer :next))
      (check= (snode 1) '(unfolded (document "a") (document "b")))
      (check= (snode 2) '(switch (hidden (document "x")) (shown (document "y"))))
      (edit (dynamic-traverse-buffer :previous))
      (check= (snode 2) '(switch (shown (document "x")) (hidden (document "y"))))
      (check= (cursor) '(2 0 0 0 0))
      (edit (dynamic-traverse-buffer :previous))
      (check= (snode 1) '(folded (document "a") (document "b")))
      (check= (cursor) '(1 0 0 1)))))

(define dynamic-doc*
  ;; dynamic-doc with everything folded and compressed
  '(document "Intro"
             (folded (document "a") (document "b"))
             (folded (document "c") (document (theorem (document "d"))))
             (switch (shown (document "x")) (hidden (document "y")))
             (unroll (shown (document "u")) (hidden* (document "v")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define session-doc
  '(document "Intro"
             (session "scheme" "default"
                      (document (output (document "Welcome"))
                                (unfolded-io "> " (document "a") (document "1"))
                                (folded-io "> " (document "b") (document "2"))
                                (input "> " (document "c"))))
             "End"))

(define (ses . l)
  ;; the subtree at @l of the document of the session
  (apply node (cons* 1 2 l)))

(define (sses . l)
  (tree->stree (apply ses l)))

;; The fields of a session, edited as a document structure: no plugin is
;; started (nothing is evaluated). Contexts, folding of input/output
;; fields, inserting and removing fields, text fields, the banner,
;; subsessions, clearing and splitting a session.
(define (test-sessions)
  (check-group "sessions")
  (with-doc session-doc
    (lambda ()
      (check-true (session-document-context? (ses)))
      (check-true (subsession-document-context? (ses)))
      (check-false (session-document-context? (node)))
      (check-false (field-context? (ses 0)))
      (check-true (field-or-output-context? (ses 0)))
      (check-true (field-context? (ses 1)))
      (check-true (field-unfolded-context? (ses 1)))
      (check-false (field-folded-context? (ses 1)))
      (check-true (field-folded-context? (ses 2)))
      (check-true (field-prog-context? (ses 3)))
      (check-false (field-text-context? (ses 3)))
      (check-false (field-math-context? (ses 3)))
      (edit (tree-go-to (ses 3) 1 :end))
      (check= (cursor) '(1 2 3 1 0 1))
      (check-true (field-input-context? (ses 3)))
      (check-false (field-input-context? (ses 2)))
      (check= (get-env "prog-language") "scheme")
      (check= (get-env "prog-session") "default")
      (check-true (inside? 'session))
      ;; folding and unfolding of input/output fields
      (edit (alternate-toggle (ses 1)))
      (check= (sses 1) '(folded-io "> " (document "a") (document "1")))
      (edit (alternate-toggle (ses 1)))
      (check= (sses 1) '(unfolded-io "> " (document "a") (document "1")))
      (edit (field-fold (ses 1)))
      (check= (sses 1) '(folded-io "> " (document "a") (document "1")))
      (edit (field-unfold (ses 1)))
      (check= (sses 1) '(unfolded-io "> " (document "a") (document "1")))
      ;; an input field has nothing to fold
      (edit (field-fold (ses 3)))
      (check= (sses 3) '(input "> " (document "c")))
      (edit (session-fold-all))
      (check= (sses 1) '(folded-io "> " (document "a") (document "1")))
      (check= (sses 2) '(folded-io "> " (document "b") (document "2")))
      (edit (session-unfold-all))
      (check= (sses 1) '(unfolded-io "> " (document "a") (document "1")))
      (check= (sses 2) '(unfolded-io "> " (document "b") (document "2")))
      ;; new input fields, after and before, with the prompt of the plugin
      (edit (field-insert (ses 3) #t))
      (check= (sses 4) '(input "Scheme] " (document "")))
      (check= (cursor) '(1 2 4 1 0 0))
      (edit (field-insert (cursor-tree) #f))
      (check= (tree-arity (ses)) 6)
      (check= (cursor) '(1 2 4 1 0 0))
      (edit (field-insert-text (cursor-tree) #t))
      (check= (sses 5) '(textput (document "")))
      (check= (cursor) '(1 2 5 0 0 0))
      (check= (tree-arity (ses)) 7)))
  (with-doc session-doc
    (lambda ()
      (edit (tree-go-to (ses 3) 1 :end))
      ;; backwards: the previous field is removed
      (edit (field-remove (cursor-tree) #f))
      (check= (sses) '(document (output (document "Welcome"))
                                (unfolded-io "> " (document "a") (document "1"))
                                (input "> " (document "c"))))
      (check= (cursor) '(1 2 2 1 0 1))
      ;; forwards on the last field: it is removed, and the cursor goes to
      ;; the previous one
      (edit (field-remove (cursor-tree) #t))
      (check= (sses) '(document (output (document "Welcome"))
                                (unfolded-io "> " (document "a")
                                             (document "1"))))
      (check= (cursor) '(1 2 1 1 0 1))
      (edit (field-remove-banner (cursor-tree)))
      (check= (sses) '(document (unfolded-io "> " (document "a")
                                             (document "1"))))
      (edit (session-clear-all))
      (check= (sses) '(document (input "> " (document "a"))))
      (check= (body) '(document "Intro"
                                (session "scheme" "default"
                                         (document (input "> " (document "a"))))
                                "End"))))
  (with-doc session-doc
    (lambda ()
      (edit (tree-go-to (ses 3) 1 :start))
      (edit (structured-insert-down))
      (check= (sses 4) '(input "Scheme] " (document "")))
      (check= (cursor) '(1 2 4 1 0 0))
      (edit (structured-remove-up))
      (check= (sses 3) '(input "Scheme] " (document "")))
      (check= (tree-arity (ses)) 4)))
  (with-doc session-doc
    (lambda ()
      ;; a subsession around a field
      (edit (tree-go-to (ses 2) 1 :end))
      (edit (field-insert-fold (cursor-tree)))
      (check= (sses 2) '(unfolded-subsession
                         (document "")
                         (document (folded-io "> " (document "b")
                                              (document "2")))))
      (check= (cursor) '(1 2 2 0 0 0))
      (check-true (subsession-document-context? (ses 2 1)))
      (check-true (toggle-second-context? (ses 2)))
      (edit (alternate-toggle (ses 2)))
      (check= (tree-label (ses 2)) 'folded-subsession)))
  (with-doc session-doc
    (lambda ()
      ;; splitting a session after the current field
      (edit (tree-go-to (ses 1) 1 :end))
      (edit (session-split))
      (check= (snode 1) '(session "scheme" "default"
                                  (document
                                   (output (document "Welcome"))
                                   (unfolded-io "> " (document "a")
                                                (document "1")))))
      ;; an empty paragraph between the two parts, with the cursor
      (check= (snode 2) "")
      (check= (cursor) '(2 0))
      (check= (snode 3) '(session "scheme" "default"
                                  (document
                                   (folded-io "> " (document "b")
                                              (document "2"))
                                   (input "> " (document "c")))))
      (check= (snode 4) "End"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo of structured changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The structured changes are ordinary edits, undone one step at a time.
(define (test-undo)
  (check-group "undo")
  (with-doc '(document "Intro" (section "S") (folded (document "a")
                                                     (document "b")))
    (lambda ()
      ;; the setting of the body is not an edit to undo
      (clear-undo-history)
      (check= (undo-possibilities) 0)
      (edit (numbered-toggle (node 1)))
      (edit (unfold* (node 2)))
      (check= (body) '(document "Intro" (section* "S")
                                (unfolded (document "a") (document "b"))))
      (edit (undo 0))
      (check= (body) '(document "Intro" (section* "S")
                                (folded (document "a") (document "b"))))
      (edit (undo 0))
      (check= (body) '(document "Intro" (section "S")
                                (folded (document "a") (document "b"))))
      (edit (redo 0))
      (check= (snode 1) '(section* "S")))))

(define (unfold* t)
  (alternate-unfold t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (text-structure-test-failures)
  (:synopsis "Run the tests of text structures and dynamic markup")
  (check-suite "text-structure")
  (test-sections)
  (test-section-titles)
  (test-section-navigation)
  (test-lists)
  (test-list-variants)
  (test-enunciations)
  (test-other-environments)
  (test-footnotes)
  (test-markup)
  (test-doc-data)
  (test-automatic-sections)
  (test-folding)
  (test-switches)
  (test-switch-lists)
  (test-dynamic-global)
  (test-sessions)
  (test-undo)
  (check-end))
