;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : links-test.scm
;; DESCRIPTION : tests of references, links and multi-file documents
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; References, tables of contents, indexes, hyperlinks, loci and multi-file
;; documents, all without a window.
;;
;; The documents are written as .tm files to links-test in the temporary
;; directory and loaded, so that they have a style and an initial
;; environment of their own. A loaded file is typeset on papyrus unless it
;; says otherwise, and then has a single page whose number is "?": the
;; documents here set page-medium to paper.
;;
;; Typesetting stores the value of each label in the buffer as a tuple
;; (number page) (Typeset/Env/env_exec.cpp, exec_set_binding), which
;; get-reference reads; the generated parts of a document (the table of
;; contents, the index, the glossary, the lists of figures) are produced by
;; generate-all-aux from the auxiliary data which typesetting collects
;; (get-auxiliary), and appear after the next typesetting. Document > Update
;; > All does this in idle time, which never comes without a window: the
;; suite takes the steps of tests/documents/export.scm itself (update-all).
;;
;; texmacs-expand evaluates a TeXmacs expression at the start of the
;; buffer: (get-binding l) gives the number of a label as a string and
;; (get-binding l "1") its page, and the reference macros come back as
;; hlinks to "#label" (Edit/Editor/edit_typeset.cpp, expand_references).
;;
;; Links (Data/Observers/link.cpp) are registered when a locus is
;; typeset: a label is a locus with an "anchor" link to the vertex
;; (id "#label"), a reference and an hlink are loci with a "hyperlink"
;; link from a hard identifier to (url "#label") or (url destination), and
;; the typesetter adds the attributes (attr "secure" ...) after the type.
;; The identifiers of the loci are found on their bodies (id->trees,
;; tree->ids). The link editing of link/link-edit.scm works from the
;; cursor, which the suite moves with go-to; following a link
;; (link-follow-ids) moves the cursor too.
;;
;; A multi-file document is a master with include tags, whose typesetting
;; collects the labels of all the included files (the third element of a
;; reference tuple is the file); a chapter which names the master as its
;; project (<project|master.tm>, or project-attach) sees the references of
;; the master as global references. The part tmfs (part/part-tmfs.scm)
;; opens an included file as a document with the inherited environment.
;;
;; Nothing here writes preferences: the navigation toggles which would
;; (bidirectional and external navigation, link pages) are left out, the
;; link types are blocked and allowed through the table of
;; navigation-toggle-type, which is not saved, and the link mode is a
;; variable of link-edit.scm.

(texmacs-module (check links-test)
  (:use (check check-lib)
        (link locus-edit)
        (link link-edit)
        (link link-navigate)
        (link link-extern)
        (link ref-edit)
        (link ref-markup)
        (generic document-part)
        (part part-tmfs)
        (part part-shared)
        (part part-menu)
        (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define links-dir
  (string-append (url->system (url-temp-dir)) "/links-test"))

(define (tmp name)
  (system->url (string-append links-dir "/" name)))

(define (clear-dir)
  (with d (system->url links-dir)
    (when (url-exists? d)
      (for (u (url-read-directory d "*"))
        (system-remove u))
      (system-rmdir d))))

(define (make-dir)
  (clear-dir)
  (system-mkdir (system->url links-dir)))

(define (write-doc name style init body)
  ;; write the document with @style (a list), the extra initial
  ;; environment @init (a list of (var val)) and the paragraphs @body
  (with doc `(document
              (TeXmacs "2.1")
              (style (tuple ,@style))
              (body (document ,@body))
              (initial (collection
                        (associate "page-medium" "paper")
                        ,@(map (lambda (x) `(associate ,@x)) init))))
    (tree-export (stree->tree doc) (tmp name) "texmacs")))

(define (run-group thunk)
  ;; run a group; an error is a failure, and the buffers which it opened
  ;; are closed
  (let ((old (current-buffer))
        (before (buffer-list)))
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (for (b (buffer-list))
        (when (nin? b before)
          (buffer-pretend-saved b)
          ;; FIXME: a buffer closed before a wait message was shown with
          ;; it current can crash TeXmacs at the next wait message: the
          ;; wait window lets Qt deliver a focus event, and
          ;; QTMWidget::focusInEvent (Plugins/Qt/QTMWidget.cpp:857) calls
          ;; is_embedded_widget (Edit/Interface/edit_interface.cpp:1205) on
          ;; the editor of the closed view: load a file holding
          ;; x<index|a> and <the-index|idx|>, update-forced, buffer-close
          ;; it, switch-to-buffer back, load a copy of it, update-forced and
          ;; generate-all-aux gives a segmentation fault, expected the
          ;; index. A wait message with the buffer current avoids it.
          (when (!= (current-buffer) b) (switch-to-buffer b))
          (system-wait "Closing" "")
          (system-wait "" "")
          (buffer-close b)))
      (when (and (buffer-exists? old) (!= (current-buffer) old))
        (switch-to-buffer old)))))

(define (open name)
  ;; load the file @name of the temporary directory and typeset it
  (with u (tmp name)
    (load-buffer u)
    (update-forced)
    u))

(define (update-all)
  ;; what Document > Update > All does, twice so that the page numbers
  ;; settle (see tests/documents/export.scm)
  (update-forced)
  (generate-all-aux)
  (update-current-buffer)
  (update-forced)
  (generate-all-aux)
  (update-current-buffer)
  (update-forced))

(define (with-new-body doc thunk)
  ;; run @thunk in a new buffer holding @doc (generic style), typeset
  (with u (new-buffer)
    (buffer-set-body u (stree->tree doc))
    (update-forced)
    (thunk)))

(define (edit-step thunk)
  ;; one user action, as the event loop wraps a key press or a menu action
  ;; (see editing-test.scm)
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define-macro (edit . body)
  `(edit-step (lambda () ,@body)))

(define (ev t) (tree->stree (texmacs-expand t)))
(define (num l) (ev `(get-binding ,l)))
(define (page l) (ev `(get-binding ,l "1")))
(define (ref l) (tree->stree (get-reference l)))

(define (at . l) (append (buffer-path) l))
(define (rel p) (list-tail p (length (buffer-path))))
(define (cursor) (rel (cursor-path)))
(define (bt . l) (apply tree-ref (cons (buffer-tree) l)))
(define (st . l) (tree->stree (apply bt l)))

(define (user-labels l)
  ;; the labels of @l which the user wrote
  (list-filter l (lambda (s) (not (or (string-starts? s "auto-")
                                      (string-starts? s "footn")
                                      (string-starts? s "part:"))))))

(define (sorted l) (sort l string<?))

(define (resolve t)
  ;; @t with each (pageref l) replaced by the page of l
  (cond ((func? t 'pageref 1) (page (cadr t)))
        ((pair? t) (cons (car t) (map resolve (cdr t))))
        (else t)))

(define (find-sub t pred?)
  ;; the first subtree of the scheme tree @t which satisfies @pred?
  (cond ((pred? t) t)
        ((pair? t) (list-any (lambda (x) (find-sub x pred?)) (cdr t)))
        (else #f)))

(define (list-any f l)
  (and (pair? l) (or (f (car l)) (list-any f (cdr l)))))

(define (contains? t x) (and (find-sub t (lambda (y) (== y x))) #t))

(define (toc-title e)
  ;; the number and title of an entry of a table of contents, or its title
  (or (and-with c (find-sub e (lambda (x) (and (func? x 'concat)
                                               (>= (length x) 4)
                                               (== (caddr x) '(space "2spc")))))
        (list (cadr c) (cadddr c)))
      (and-with w (find-sub e (lambda (x) (and (func? x 'with)
                                               (string? (cAr x)))))
        (list (cAr w)))))

(define (toc-page e)
  (and-with p (find-sub e (lambda (x) (func? x 'pageref 1)))
    (page (cadr p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An article with labels of all kinds, references, an index, a glossary,
;; a list of figures and a table of contents; the second section starts on
;; the second page.
(define article-body
  '((table-of-contents "toc" (document ""))
    (section (concat "Intro" (label "sec:intro")))
    (concat "Text" (index "zeta") (index "alpha") (subindex "alpha" "beta")
            (index-complex (tuple "gamma") "strong" "" (tuple "Gamma"))
            (glossary "foo") (glossary-explain "bar" "explained"))
    (equation (document (concat "x=1" (label "eq:one"))))
    (theorem (document (concat "T" (label "thm:one"))))
    (subsection (concat "Detail" (label "sec:detail")))
    (equation* (document "z"))
    (equation (document (concat "y" (label "eq:two"))))
    (lemma (document (concat "L" (label "lem:one"))))
    (new-page)
    (section (concat "Second" (label "sec:second")))
    (concat "See " (reference "sec:intro") ", " (pageref "sec:second") ", "
            (eqref "eq:two") ", " (reference "missing")
            (index "beta") (glossary-dup "foo"))
    (big-figure "F" (concat "Cap" (label "fig:one")))
    (enumerate (document (concat (item) "a" (label "it:a"))
                         (concat (item) "b" (label "it:b"))))
    (concat "x" (footnote (concat "note" (label "fn:one"))))
    (concat (label "dup") (label "dup"))
    (the-index "idx" (document ""))
    (the-glossary "gly" (document ""))
    (list-of-figures "figure" (document ""))))

(define article-count 0)

(define (open-article)
  ;; a new file each time
  (set! article-count (+ article-count 1))
  (with name (string-append "article-" (number->string article-count) ".tm")
    (write-doc name '("article" "smart-ref") '() article-body)
    (open name)))

;; A book: the numbers of sections, equations, theorems, figures and
;; tables are prefixed by the chapter.
(define book-body
  '((chapter (concat "One" (label "c1")))
    (section (concat "S" (label "s11")))
    (equation (document (concat "x" (label "e11"))))
    (theorem (document (concat "T" (label "t11"))))
    (lemma (document (concat "L" (label "l12"))))
    (big-figure "F" (concat "cap" (label "f11")))
    (big-table "T" (concat "cap" (label "tab11")))
    (eqnarray* (table (row (cell "a") (cell "=")
                           (cell (concat "b" (eq-number) (label "ea1"))))
                      (row (cell "c") (cell "=")
                           (cell (concat "d" (eq-number) (label "ea2"))))))
    (enumerate (document (concat (item) "a" (label "i1"))
                         (concat (item) "b" (label "i2"))))
    (chapter (concat "Two" (label "c2")))
    (section (concat "S" (label "s21")))
    (subsection (concat "SS" (label "ss211")))
    (equation (document (concat "y" (label "e21"))))
    (theorem (document (concat "T" (label "t21"))))
    (table-of-contents "toc" (document ""))))

;; A multi-file book: the master includes two chapters, which name it as
;; their project.
(define (write-project)
  (write-doc "ch1.tm" '("book") '()
             '((chapter (concat "One" (label "c1")))
               (section (concat "Alpha" (label "a1")))
               (equation (document (concat "x" (label "eq1"))))))
  (write-doc "ch2.tm" '("book") '()
             '((chapter (concat "Two" (label "c2")))
               (concat "See " (reference "a1") ", " (reference "eq1")
                       " and " (reference "c2") ".")))
  (write-doc "master.tm" '("book") '(("project-flag" "true")
                                     ("font-base-size" "12"))
             '((include "ch1.tm")
               (include "ch2.tm")))
  (write-doc "loose.tm" '("book") '()
             '((concat "See " (reference "a1") "."))))

(define (add-project name)
  ;; name the master as the project of the file @name, as saving a file
  ;; after Document > Project > Attach master does
  (with s (string-load (url->system (tmp name)))
    (string-save (string-replace s "<style|" "<project|master.tm>\n\n<style|")
                 (url->system (tmp name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Labels and references
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The labels get the number of what they label and the page; in an
;; article, equations, theorems and lemmas are numbered through the
;; document (theorems and lemmas share a counter), subsections within
;; their section.
(define (test-labels)
  (check-group "labels")
  (open-article)
  (update-all)
  (check= (get-page-count) 2)
  (check= (user-labels (list-references))
          '("dup" "eq:one" "eq:two" "fig:one" "fn:one" "it:a" "it:b"
            "lem:one" "sec:detail" "sec:intro" "sec:second" "thm:one"))
  (check= (ref "sec:intro") '(tuple "1" "1"))
  (check= (ref "sec:second") '(tuple "2" "2"))
  (check= (ref "eq:one") '(tuple "1" "1"))
  (check= (ref "eq:two") '(tuple "2" "1"))
  (check= (ref "thm:one") '(tuple "1" "1"))
  (check= (ref "lem:one") '(tuple "2" "1"))
  (check= (ref "missing") '(uninit))
  (check= (num "sec:detail") "1.1")
  (check= (page "sec:detail") "1")
  (check= (num "fig:one") "1")
  (check= (page "fig:one") "2")
  (check= (num "it:a") "1")
  (check= (num "it:b") "2")
  (check= (num "fn:one") "1")
  (check= (page "fn:one") "2")
  (check= (ev '(has-binding "sec:intro")) "true")
  (check= (ev '(has-binding "missing")) "false")
  ;; the reference macros
  (check= (ev '(reference "sec:detail")) '(hlink "1.1" "#sec:detail"))
  (check= (ev '(pageref "sec:second")) '(hlink "2" "#sec:second"))
  (check= (ev '(reference "missing")) '(hlink "?" "#missing"))
  (check= (ev '(eqref "eq:two"))
          '(with "font-shape" "right" (concat "(" (hlink "2" "#eq:two") ")")))
  ;; from numbers back to labels
  (check= (sorted (number->labels "2"))
          '("eq:two" "it:b" "lem:one" "sec:second"))
  (check= (number->labels "1.1") '("sec:detail"))
  (check= (number->labels "7") '())
  (check-true (in? "sec:detail" (find-references "1.1")))
  ;; where the labels are
  (with p (rel (label->path "sec:intro"))
    (check= p '(1 0 1 1))
    (check= (st 1 0 1) '(label "sec:intro")))
  (check= (rel (label->path "eq:two")) '(7 0 0 1 1))
  (go-to (at 2 0 0))
  (go-to-label "eq:two")
  (check= (cursor) '(7 0 0 1 1))
  (go-to-label "sec:second")
  (check= (cursor) '(10 0 1 1)))

;; The table of references can be changed by hand; the next typesetting
;; recomputes it.
(define (test-set-reference)
  (check-group "set-reference")
  (open-article)
  (set-reference "custom" (stree->tree '(tuple "X" "9")))
  (check= (ref "custom") '(tuple "X" "9"))
  (check-true (in? "custom" (list-references)))
  (check= (num "custom") "X")
  (check= (page "custom") "9")
  (reset-reference "custom")
  (check= (ref "custom") '(uninit))
  (check-false (in? "custom" (list-references)))
  (set-reference "sec:intro" (stree->tree '(tuple "Z" "1")))
  (check= (num "sec:intro") "Z")
  (update-current-buffer)
  (update-forced)
  (check= (num "sec:intro") "1"))

;; Labels and references are found in the tree: the broken references
;; (to labels which do not exist), the duplicate labels, the ties of a key
;; (its labels and references); a number in a reference which is not a
;; label is turned into the label which has that number, using the prefix
;; or the word before it to choose between labels with the same number.
(define (test-ref-search)
  (check-group "ref-search")
  (open-article)
  (update-all)
  (let ((t (buffer-tree)))
    (check= (length (search-labels t)) 13)
    (check= (map tree->stree (search-label t "sec:intro"))
            '((label "sec:intro")))
    ;; the generated parts hold page references to auto- labels
    (check= (list-filter (map tree->stree (search-references t))
                         (lambda (r) (not (string-starts? (cadr r) "auto-"))))
            '((reference "sec:intro") (pageref "sec:second")
              (eqref "eq:two") (reference "missing")))
    (check= (length (search-references t)) 19)
    (check= (map tree->stree (search-reference t "eq:two"))
            '((eqref "eq:two")))
    (check= (map tree->stree (search-tie t "sec:intro"))
            '((label "sec:intro") (reference "sec:intro")))
    (check= (map tree->stree (search-broken-references t))
            '((reference "missing")))
    (check= (map tree->stree (search-duplicate-labels t))
            '((label "dup") (label "dup")))
    (check= (map tree->stree (search-citations t)) '()))
  (check= (length (broken-references)) 1)
  (check= (length (duplicate-labels)) 2)
  (check= (number->label (stree->tree "eq:2")) "eq:two")
  (check= (number->label (stree->tree "s:2")) "sec:second")
  (check= (number->label (stree->tree "lem:2")) "lem:one")
  (check= (number->label (stree->tree "1.1")) "sec:detail")
  (check= (number->label (stree->tree "99")) #f)
  ;; the cursor in a tie: same-ties are the label and its references
  (go-to (at 11 1 0 0))
  (check= (tie-id) "sec:intro")
  (check= (length (same-ties)) 2)
  (go-to-same-tie :first)
  (check= (cursor) '(1 0 1 1))
  (go-to-same-tie :last)
  (check= (cursor) '(11 1 1)))

;; Typed references (ref-markup.scm): the type is put before the
;; references, in the plural for several; smart references group the keys
;; by the prefix of their names.
(define (test-smart-refs)
  (check-group "smart refs")
  (open-article)
  (update-all)
  (check= (tm->stree (ext-typed-ref "theorem" (stree->tree '(tuple "a"))))
          '(concat (localize "theorem") (nbsp) (reference "a")))
  (check= (tm->stree (ext-typed-ref "theorem"
                                    (stree->tree '(tuple "a" "b"))))
          '(concat (localize "theorems") (nbsp)
                   (reference "a") " " (localize "and") (nbsp)
                   (reference "b")))
  (check= (tm->stree (ext-typed-ref "theorem"
                                    (stree->tree '(tuple "a" "b" "c"))))
          '(concat (localize "theorems") (nbsp)
                   (reference "a") ", " (reference "b") ", "
                   (localize "and") (nbsp) (reference "c")))
  (check= (tm->stree (ext-typed-ref "property"
                                    (stree->tree '(tuple "a" "b"))))
          '(concat (localize "properties") (nbsp)
                   (reference "a") " " (localize "and") (nbsp)
                   (reference "b")))
  (check= (tm->stree (ext-typed-ref "" (stree->tree '(tuple "a"))))
          '(reference "a"))
  (check= (tm->stree (ext-typed-ref "x" (stree->tree '(tuple)))) "")
  (check= (tm->stree (ext-typed-ref* "equation" "eqref"
                                     (stree->tree '(tuple "a"))))
          '(concat (localize "equation") (nbsp) (eqref (reference "a"))))
  (check= (tm->stree (ext-smart-ref (stree->tree '(tuple "thm:one"))))
          '(thm-ref "thm:one"))
  (check= (tm->stree (ext-smart-ref
                      (stree->tree '(tuple "thm:one" "eq:one" "eq:two"))))
          '(concat (thm-ref "thm:one") " " (localize "and") (nbsp)
                   (eq-ref "eq:one" "eq:two")))
  (check= (tm->stree (ext-smart-ref (stree->tree '(tuple "sec:intro"))))
          '(sec-ref "sec:intro"))
  (check= (tm->stree (ext-smart-ref (stree->tree '(tuple "dup"))))
          '(unknown-ref "dup"))
  ;; typeset in the document
  (let ((r (ev '(smart-ref "thm:one" "lem:one"))))
    (check= (car r) 'concat)
    (check= (cadr r) "Theorem ")
    (check-true (contains? r " and "))
    (check-true (contains? r "Lemma "))
    (check-true (contains? r '(hlink "1" "#thm:one")))
    (check-true (contains? r '(hlink "2" "#lem:one")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In a book, the numbers start again in each chapter and are prefixed by
;; its number; the numbered rows of an eqnarray* count as equations;
;; chapters start a new page.
(define (test-book-numbering)
  (check-group "book numbering")
  (write-doc "book.tm" '("book") '() book-body)
  (open "book.tm")
  (update-all)
  (check= (num "c1") "1")
  (check= (num "c2") "2")
  (check= (num "s11") "1.1")
  (check= (num "s21") "2.1")
  (check= (num "ss211") "2.1.1")
  (check= (num "e11") "1.1")
  (check= (num "ea1") "1.2")
  (check= (num "ea2") "1.3")
  (check= (num "e21") "2.1")
  (check= (num "t11") "1.1")
  (check= (num "l12") "1.2")
  (check= (num "t21") "2.1")
  (check= (num "f11") "1.1")
  (check= (num "tab11") "1.1")
  (check= (num "i1") "1")
  (check= (num "i2") "2")
  (check= (page "c1") "1")
  (check= (page "e11") "1")
  ;; a chapter starts on a right-hand page
  (check= (page "c2") "3")
  (check= (page "ss211") "3")
  (check= (ev '(eqref "ea2"))
          '(with "font-shape" "right" (concat "(" (hlink "1.3" "#ea2") ")")))
  ;; the table of contents at the end lists the chapters, sections and
  ;; subsections with their numbers (the space before the dots of a
  ;; section joins its title)
  (with toc (cdr (st 14 1))
    (check= (map toc-title toc)
            '(("1" "One") ("1.1" "S ") ("2" "Two") ("2.1" "S ") ("2.1.1" "SS ")))
    (check= (toc-page (car toc)) "1")
    (check= (toc-page (caddr toc)) (page "c2"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generated parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The table of contents: typesetting collects the auxiliary data, which
;; generate-all-aux turns into the body of the table-of-contents tag; the
;; sections are listed with their numbers and pages, the subsections
;; indented, the index, glossary and list of figures at the end; a second
;; update gives the same result.
(define (test-toc)
  (check-group "table of contents")
  (open-article)
  (check= (st 0) '(table-of-contents "toc" (document "")))
  (check= (sorted (list-auxiliaries)) '("figure" "gly" "idx" "toc"))
  (check= (tree->stree (get-auxiliary "nothing")) '(uninit))
  (update-forced)
  (generate-all-aux)
  (update-current-buffer)
  (update-forced)
  (let ((toc1 (st 0)))
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (check= (st 0) toc1))
  (with toc (cdr (st 0 1))
    (check= (length toc) 6)
    (check= (map toc-title toc)
            '(("1" "Intro") ("1.1" "Detail ") ("2" "Second")
              ("Index") ("Glossary") ("List of figures")))
    (check= (map toc-page toc) '("1" "1" "2" "2" "2" "2"))
    ;; a section entry is bold with space above; a subsection indented
    (check= (car (car toc)) 'concat)
    (check= (cadr (car toc)) '(vspace* "1fn"))
    (check= (list-head (cadr toc) 3) '(with "par-left" "1tab"))
    (check-true (contains? (car toc) '(vspace "0.5fn"))))
  ;; the auxiliary data of the table of contents is a document with the
  ;; same number of entries
  (with aux (tree->stree (get-auxiliary "toc"))
    (check= (car aux) 'document)
    (check= (length (cdr aux)) 6)))

;; The index is sorted without regard to case, a subentry follows its
;; entry, the pages of each entry are given, an index-complex entry has
;; its own text and font; the auxiliary data keeps the order of the text.
(define (test-index)
  (check-group "index")
  (open-article)
  (update-all)
  (check= (resolve (st 16 1))
          '(document (index+1 "alpha" "1")
                     (index+2 "alpha" "beta" "1")
                     (index+1 "beta" "2")
                     (index+1 "Gamma" (strong "1"))
                     (index+1 "zeta" "1")))
  (check= (resolve (tree->stree (get-auxiliary "idx")))
          '(document (tuple (tuple "zeta") "1")
                     (tuple (tuple "alpha") "1")
                     (tuple (tuple "alpha" "beta") "1")
                     (tuple (tuple "gamma") "strong" "" (tuple "Gamma") "1")
                     (tuple (tuple "beta") "2"))))

;; The glossary keeps the order of the text, glossary-dup adds a page to an
;; entry, glossary-explain has an explanation; the list of figures is a
;; glossary of the figures with their numbers and captions.
(define (test-glossary)
  (check-group "glossary")
  (open-article)
  (update-all)
  (check= (resolve (st 17 1))
          '(document (glossary-1 "foo" (concat "1" ", " "2"))
                     (glossary-2 "bar" "explained" "1")))
  (check= (resolve (tree->stree (get-auxiliary "gly")))
          '(document (tuple "normal" "foo" "1")
                     (tuple "normal" "bar" "explained" "1")
                     (tuple "dup" "foo" "2")))
  (check= (resolve (st 18 1))
          '(document (glossary-1 (surround (hidden-binding (tuple) "1") ""
                                           "Cap")
                                 "2"))))

;; An index entry in a section title or in a caption: the table of
;; contents and the list of figures get the title or the caption with its
;; page, but not the assignment of the counter of the automatic labels
;; and the index entry which writing the title evaluates (these made the
;; auto-N labels move at each update, so that all the pages of the table of
;; contents and of the index were "?"); the index has the entry once.
(define (test-index-in-title)
  (check-group "index entry in a title")
  (write-doc "title-index.tm" '("generic") '()
             '((table-of-contents "toc" (document ""))
               (section (concat "Foo" (index "foo")))
               "Text"
               (big-figure "F" (concat "Cap" (index "cap")))
               (new-page)
               (section "Bar")
               (concat "x" (index "bar"))
               (the-index "idx" (document ""))
               (list-of-figures "figure" (document ""))))
  (open "title-index.tm")
  (update-all)
  (let ((toc1 (st 0))
        (idx1 (st 7)))
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (check= (st 0) toc1)
    (check= (st 7) idx1))
  (with toc (cdr (st 0 1))
    (check= (length toc) 4)
    (check= (map toc-page toc) '("1" "2" "2" "2"))
    (check-true (contains? (car toc) "Foo")))
  (check= (resolve (st 7 1))
          '(document (index+1 "bar" "2")
                     (index+1 "cap" "1")
                     (index+1 "foo" "1")))
  (with lof (cdr (st 8 1))
    (check= (length lof) 1)
    (check= (toc-page (car lof)) "1"))
  (for-each (lambda (aux)
              (with t (tree->stree (get-auxiliary aux))
                (check-false (find-sub t (lambda (x) (func? x 'assign))))
                (check-false (find-sub t (lambda (x) (func? x 'write))))))
            '("toc" "figure")))

;; Document > Update > Table of contents, Index or Glossary regenerate
;; one generated part (generic/document-edit.scm, update-document, which
;; calls generate-aux with "table-of-contents", "index" or "glossary").
(define (test-generate-one)
  (check-group "generate one part")
  (open-article)
  (update-all)
  (tree-assign (bt 0 1) (stree->tree '(document "")))
  (generate-aux "table-of-contents")
  (update-current-buffer)
  (update-forced)
  (check= (length (cdr (st 0 1))) 6)
  ;; FIXME: generating one part empties the others: the body of every
  ;; generated tag is reset before the kind is compared with the one asked
  ;; for (Edit/Process/edit_process.cpp:589-591). After update-all,
  ;; (generate-aux "table-of-contents") leaves the index (the-index "idx"
  ;; (document "")), expected the index as it was.
  ;; FIXME: (generate-aux "index") and (generate-aux "glossary"), which
  ;; Document > Update > Index and Glossary call, empty the index and the
  ;; glossary and do not generate them, since generate_aux compares the
  ;; kind with "the-index" and "the-glossary" (edit_process.cpp:612,617):
  ;; after update-all, (generate-aux "index") gives (the-index "idx"
  ;; (document "")), expected the five entries of the index.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hyperlinks and loci
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define links-doc
  '(document
    (section (concat "Intro" (label "s1")))
    (concat "See " (reference "s1") " and "
            (hlink "web" "http://example.org/x") " and " (hlink "lab" "#s1"))
    (locus (id "src1") (link "mytype" (id "src1") (id "tgt1")) "source")
    (locus (id "tgt1") "target")
    "plain"))

;; hlink is a locus with a hyperlink from its body to the destination; a
;; label is a locus with an anchor, a reference a hyperlink to "#label".
(define (test-hyperlinks)
  (check-group "hyperlinks")
  (with-new-body links-doc
    (lambda ()
      (with h (ev '(hlink "text" "http://a.org"))
        (check= (car h) 'locus)
        (check= (car (cadr h)) 'id)
        (check= (caddr h) `(link "hyperlink" ,(cadr h) (url "http://a.org")))
        (check= (cadddr h) "text"))
      (check-true (list-and (map (cut in? <> (current-link-types))
                                 '("anchor" "hyperlink" "mytype"))))
      (with l (vertex->links '(url "http://example.org/x"))
        (check= (length l) 1)
        (check= (link-type (car l)) "hyperlink")
        (check= (link-attributes (car l)) '(("secure" . "true")))
        (check= (car (car (link-vertices (car l)))) 'id)
        (check= (cadr (link-vertices (car l))) '(url "http://example.org/x")))
      ;; the reference to s1 and the hlink to #s1
      (check= (length (vertex->links '(url "#s1"))) 2)
      (check= (map tree->stree (vertex->links '(id "#s1")))
              '((link "anchor" (attr "secure" "true") (id "#s1"))))
      (check= (vertex->links '(url "http://nowhere.org")) '())
      ;; the attributes of a link are not vertices: (attr "secure" ...),
      ;; which every hyperlink has, held all the links of all the buffers,
      ;; and each link which went was looked for in it
      (check= (vertex->links '(attr "secure" "true")) '())
      (check= (vertex->links '(attr "secure" "false")) '())
      ;; following them moves the cursor to the label
      (go-to (at 4 0))
      (go-to-label "s1")
      (check= (cursor) '(0 0 1 1))
      (go-to (at 4 0))
      (go-to-url "#s1")
      (check= (cursor) '(0 0 1 1)))))

;; A link to a label of another file loads it and goes to the label.
(define (test-hyperlink-files)
  (check-group "hyperlinks to files")
  (write-doc "other.tm" '("article") '()
             '((section (concat "Target" (label "tgt"))) "text"))
  (write-doc "from.tm" '("article") '()
             '((concat (hlink "go" "other.tm#tgt"))))
  (open "from.tm")
  (go-to-url (string-append (url->system (tmp "other.tm")) "#tgt"))
  (check= (current-buffer) (tmp "other.tm"))
  (check-true (buffer-exists? (tmp "other.tm")))
  (check= (cursor) '(0 0 1 1)))

;; The loci of a typeset document: their identifiers are on their bodies,
;; and the links between them are found from either end.
(define (test-loci)
  (check-group "loci")
  (with-new-body links-doc
    (lambda ()
      (check= (map tree->stree (id->trees "src1")) '("source"))
      (check= (map tree->stree (id->trees "tgt1")) '("target"))
      (check= (id->trees "nothing") '())
      (check= (map tree->stree (id->loci "tgt1")) '((locus (id "tgt1") "target")))
      (check= (tree->ids (bt 2 2)) '("src1"))
      (check= (tree->ids (bt 3 1)) '("tgt1"))
      (check= (tree->ids (bt 4)) '())
      (check= (locus-id (bt 2)) "src1")
      (check= (locus-id (bt 4)) #f)
      (check= (locus-id (stree->tree '(locus "x"))) #f)
      (with l '(link "mytype" (attr "secure" "true") (id "src1") (id "tgt1"))
        (check= (map tree->stree (vertex->links '(id "src1"))) (list l))
        (check= (map tree->stree (vertex->links '(id "tgt1"))) (list l))
        (check= (link-flatten (stree->tree l))
                '(link "mytype" (("secure" . "true")) (id "src1") (id "tgt1")))
        (check= (link-type (stree->tree l)) "mytype")
        (check= (link-vertices (stree->tree l)) '((id "src1") (id "tgt1"))))
      ;; FIXME: link-source and link-target raise wrong-type-arg: the
      ;; vertices of the link are read by link-vertices, which a define of
      ;; link-edit.scm:146 (the vertices of the link being built, by number)
      ;; shadows inside the module after the tm-define of link-edit.scm:58:
      ;; (link-source '(link "t" (("secure" . "true")) (id "a") (id "b")))
      ;; gives an error, expected (id "a").
      (check= (vertex->id '(id "a")) "a")
      (check= (vertex->id '(url "a")) #f)
      (check= (vertex->url '(url "u")) "u")
      (check= (vertex->url '(id "u")) #f)
      (check= (vertex->script '(script "f" "x")) "f")
      ;; the locus-set changes the bodies of all the loci with an id
      (locus-set "tgt1" (stree->tree "changed"))
      (check= (st 3) '(locus (id "tgt1") "changed"))
      ;; the loci of this buffer are not extern to it
      (check= (tree->stree (get-link-locations (current-buffer) (buffer-tree)))
              '(collection)))))

;; Link lists and navigation lists: a link is followed from its source to
;; its target, not back (bidirectional navigation is off by default); a
;; blocked type is not followed.
(define (test-navigation)
  (check-group "navigation")
  (with-new-body links-doc
    (lambda ()
      (with attrs '(("secure" . "true"))
        (check= (exact-link-list (bt 2 2) #f)
                `(("src1" "mytype" ,attrs (id "src1") (id "tgt1"))))
        (check= (exact-link-list (bt 4) #f) '())
        (check= (ids->link-list '("tgt1"))
                `(("tgt1" "mytype" ,attrs (id "src1") (id "tgt1"))))
        (check= (upward-navigation-list (bt 2 2))
                `(("mytype" ,attrs 1 "src1" (id "tgt1"))))
        (check= (upward-navigation-list (bt 3 1)) '())
        (with nl (link-list->navigation-list (ids->link-list '("tgt1")))
          (check= nl `(("mytype" ,attrs 0 "tgt1" (id "src1"))))
          (check= (navigation-list-xtypes nl) '("mytype*")))
        (with nl (upward-navigation-list (bt 2 2))
          (check= (navigation-list-xtypes nl) '("mytype"))
          (check= (navigation-list-types nl) '("mytype"))
          (check= (navigation-type (car nl)) "mytype")
          (check= (navigation-pos (car nl)) 1)
          (check= (navigation-source (car nl)) "src1")
          (check= (navigation-target (car nl)) '(id "tgt1"))
          (check= (navigation-list-filter nl "mytype" 1 #t) nl)
          (check= (navigation-list-filter nl "other" #t #t) '())
          (check= (navigation-list-filter nl #t 0 #t) '())))
      (check-true (link-may-follow? (bt 2 2)))
      (check-false (link-may-follow? (bt 3 1)))
      (check-false (link-may-follow? (bt 4)))
      (check= (link-active-ids '("src1" "tgt1")) '("src1"))
      (check= (link-mouse-ids '("src1")) '("src1"))
      (check= (map tree->stree (link-active-upwards (bt 2 2))) '("source"))
      ;; a blocked type
      (navigation-toggle-type "mytype")
      (check-false (link-may-follow? (bt 2 2)))
      (navigation-allow-all-types)
      (check-true (link-may-follow? (bt 2 2)))
      ;; following
      (check-false (has-been-visited? "id:src1"))
      (go-to (at 4 0))
      (link-follow-ids '("src1") "click")
      (check= (cursor) '(3 1 6))
      (check-true (has-been-visited? "id:src1"))
      (check-true (has-been-visited? "id:tgt1"))
      (check-false (has-been-visited? "id:other"))
      (go-to (at 4 0))
      (link-follow-ids '("tgt1") "click")
      (check= (cursor) '(4 0))
      (go-to (at 4 0))
      (go-to-id "tgt1")
      (check= (cursor) '(3 1 6))
      (go-to (at 2 2 1))
      (locus-link-follow)
      (check= (cursor) '(3 1 6)))))

;; The documents which navigation builds: automatic links to a vertex, and
;; enumerations of them.
(define (test-link-pages)
  (check-group "link pages")
  (with-new-body links-doc
    (lambda ()
      (with l (automatic-link '(url "http://a.org"))
        (check= (car l) 'locus)
        (check-true (string-starts? (cadr (cadr l)) "+"))
        (check= (caddr l) `(link "automatic" ,(cadr l) (url "http://a.org")))
        (check= (cadddr l) '(verbatim "http://a.org")))
      (with l (automatic-link '(id "tgt1"))
        (check= (cadddr l) "target"))
      (check= (automatic-link '(id "nothing"))
              '(with "color" "red" "Broken"))
      (check= (automatic-link '(id "nothing") "Gone")
              '(with "color" "red" "Gone"))
      (check= (build-enumeration '()) '())
      (check= (build-enumeration '("a")) '("a"))
      (check= (build-enumeration '("a" "b"))
              '((enumerate (document (surround (item) "" "a")
                                     (surround (item) "" "b")))))
      (let ((a (create-unique-id))
            (b (create-unique-id)))
        (check-true (string-starts? a "+"))
        (check-false (== a b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Insert > Link: a new locus at the cursor, then the loci of the link
;; are chosen with the cursor inside them (link-set-locus), and make-link
;; puts the link into both loci (bidirectional mode, the default), into
;; the source only (simple mode); the target can also be an url or a
;; script.
(define (test-make-link)
  (check-group "make link")
  (with-new-body '(document (locus (id "tgt1") "target") "aaa bbb")
    (lambda ()
      (go-to (at 1 0))
      (edit (make-locus) (insert "A"))
      (with t (st 1)
        (check= (car t) 'concat)
        (check= (car (cadr t)) 'locus)
        (check= (length (cadr t)) 3))
      (check= (cursor) '(1 0 1 1))
      (with id (locus-id (bt 1 0))
        (check-true (string-starts? id "+"))
        (check= (st 1 0) `(locus (id ,id) "A"))
        (check= (map tree->stree (id->trees id)) '("A"))
        (check-false (link-completed?))
        (edit (link-set-locus 0))
        (check-false (link-completed?))
        (go-to (at 0 1 3))
        (edit (link-set-locus 1))
        (check-true (link-completed?))
        (edit (make-link "foo"))
        (check-false (link-completed?))
        (with ln `(link "foo" (id ,id) (id "tgt1"))
          (check= (st 0) `(locus (id "tgt1") ,ln "target"))
          (check= (st 1 0) `(locus (id ,id) ,ln "A")))
        (check= (locus-link-types #t) '("foo"))
        (check= (locus-link-types #f) '("foo"))
        (check= (length (vertex->links '(id "tgt1"))) 2)
        (check-true (link-may-follow? (bt 1 0 2)))
        (go-to (at 1 1 0))
        (check= (locus-link-types #t) '())
        ;; removing the link of a type at the cursor
        (go-to (at 0 2 3))
        (edit (remove-link-of-types "foo"))
        (check= (st 0) '(locus (id "tgt1") "target"))
        ;; FIXME: in bidirectional mode the link should go from the other
        ;; locus as well, but remove-link reads the ids of the vertices with
        ;; the link-vertices of link-edit.scm:146 (see the loci group), which
        ;; finds none (link-edit.scm:221): after removing "foo" at tgt1, the
        ;; other locus still is (locus (id ID) (link "foo" (id ID) (id
        ;; "tgt1")) "A"), expected (locus (id ID) "A").
        (tree-assign (bt 1 0) (stree->tree `(locus (id ,id) "A")))
        (update-forced)
        ;; several types at once
        (go-to (at 1 0 1 0))
        (edit (link-set-locus 0))
        (go-to (at 0 1 3))
        (edit (link-set-locus 1))
        (edit (make-link "p,q"))
        (check= (st 0) `(locus (id "tgt1")
                               (link "p" (id ,id) (id "tgt1"))
                               (link "q" (id ,id) (id "tgt1"))
                               "target"))
        (check= (locus-link-types #t) '("p" "q"))
        (tree-assign (bt 0) (stree->tree '(locus (id "tgt1") "target")))
        (tree-assign (bt 1 0) (stree->tree `(locus (id ,id) "A")))
        (update-forced)
        ;; simple mode: in the source only
        (set-link-mode "simple")
        (go-to (at 1 0 1 0))
        (edit (link-set-locus 0))
        (go-to (at 0 1 3))
        (edit (link-set-locus 1))
        (edit (make-link "bar"))
        (set-link-mode "bidirectional")
        (check= (st 0) '(locus (id "tgt1") "target"))
        (check= (st 1 0) `(locus (id ,id) (link "bar" (id ,id) (id "tgt1"))
                                 "A"))
        ;; FIXME: in simple mode locus-link-types with filtering raises
        ;; wrong-type-arg (link-edit.scm:199, link-source; see the loci
        ;; group): (set-link-mode "simple"), then (locus-link-types #t) in a
        ;; locus with a link gives an error, expected ("bar").
        (go-to (at 1 0 2 0))
        (check= (locus-link-types #f) '("bar"))
        (tree-assign (bt 1 0) (stree->tree `(locus (id ,id) "A")))
        (update-forced)
        ;; a link to an url, and to a script
        (go-to (at 1 0 1 0))
        (edit (link-set-locus 0))
        (link-set-target-url "http://example.org")
        (check-true (link-target-is-url?))
        (check-false (link-target-is-script?))
        (check-true (link-completed?))
        (edit (make-link "web"))
        (check= (st 1 0) `(locus (id ,id)
                                 (link "web" (id ,id) (url "http://example.org"))
                                 "A"))
        (check= (length (vertex->links '(url "http://example.org"))) 1)
        (edit (link-set-locus 0))
        (link-set-target-script "noop")
        (check-true (link-target-is-script?))
        (check-false (link-target-is-url?))
        (link-set-url 1 "http://b.org")
        (check-true (link-target-is-url?))
        (link-set-script 1 "noop")
        (check-true (link-target-is-script?))
        (edit (make-link "act"))
        (check= (st 1 0 2) `(link "act" (id ,id) (script "noop")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-file documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The master: the labels of the included files are numbered in sequence
;; and remembered with the file they come from.
(define (test-master)
  (check-group "master")
  (write-project)
  (with m (open "master.tm")
    (update-all)
    (check-true (project-attached?))
    (check= (project-get) m)
    (check= (get-init "project-flag") "true")
    (check= (buffer-get-includes) '("ch1.tm" "ch2.tm"))
    (check-true (buffer-contains-includes?))
    (check= (user-labels (list-references)) '("a1" "c1" "c2" "eq1"))
    (check-true (in? "part:ch1.tm" (list-references)))
    (check-true (in? "part:ch2.tm" (list-references)))
    (check= (num "c1") "1")
    (check= (num "a1") "1.1")
    (check= (num "eq1") "1.1")
    (check= (num "c2") "2")
    (check= (list-tail (ref "a1") 3) '("ch1.tm"))
    (check= (list-tail (ref "c2") 3) '("ch2.tm"))
    (check= (page "c1") "1")
    (check-true (> (string->number (page "c2")) 1))
    (check= (ev '(reference "eq1")) '(hlink "1.1" "#eq1"))
    (check= (tm-get-includes '(document (with "a" "b" (document (include "x.tm")))
                                        (include "y.tm") (include (arg "z")) "w"))
            '("x.tm" "y.tm"))
    (check= (tree->stree (tree-load-inclusion (tmp "ch1.tm")))
            '(document (chapter (concat "One" (label "c1")))
                       (section (concat "Alpha" (label "a1")))
                       (equation (document (concat "x" (label "eq1"))))))
    ;; the inclusions are expanded into the master
    (buffer-expand-includes)
    (check= (tree->stree (buffer-tree))
            '(document (chapter (concat "One" (label "c1")))
                       (section (concat "Alpha" (label "a1")))
                       (equation (document (concat "x" (label "eq1"))))
                       (chapter (concat "Two" (label "c2")))
                       (concat "See " (reference "a1") ", " (reference "eq1")
                               " and " (reference "c2") ".")))
    (check-false (buffer-contains-includes?))))

;; A chapter with the master as its project: the references to the other
;; chapters come from the master, which is loaded with it.
(define (test-chapter)
  (check-group "chapter")
  (write-project)
  (add-project "ch2.tm")
  (add-project "ch1.tm")
  (with m (open "master.tm")
    (update-all)
    (with c (open "ch2.tm")
      (check= (current-buffer) c)
      (check-true (project-attached?))
      (check= (project-get) m)
      (check= (user-labels (list-references)) '("c2"))
      (check= (user-labels (list-references* #t)) '("a1" "c1" "c2" "eq1"))
      (check= (num "a1") "1.1")
      (check= (num "eq1") "1.1")
      (check= (num "c2") "2")
      (check= (ref "a1") '(uninit))
      ;; the references to labels of the project are not broken
      (check= (search-broken-references (buffer-tree)) '())
      ;; texmacs-expand, which exports use, finds the references to the
      ;; other files of the project
      (check= (ev '(reference "a1")) '(hlink "1.1" "#a1"))
      (check= (ev '(reference "eq1")) '(hlink "1.1" "#eq1"))
      (check= (ev '(pageref "c1")) '(hlink "1" "#c1"))
      (check= (ev '(reference "no-such-label")) '(hlink "?" "#no-such-label"))
      (check= (ev '(reference "c2")) '(hlink "2" "#c2")))))

;; Attaching a master by hand (Document > Project > Attach master): the
;; file is modified, its project is the master, which is loaded; detaching
;; forgets it. Use as master makes a file a project of its own.
(define (test-attach)
  (check-group "attach master")
  (write-project)
  (with m (open "master.tm")
    (update-all)
    (with l (open "loose.tm")
      (check-false (project-attached?))
      (check-true (url-none? (project-get)))
      (check= (num "a1") '(uninit))
      (check-false (buffer-modified? l))
      (project-attach "master.tm")
      (update-forced)
      (check-true (project-attached?))
      (check= (project-get) m)
      (check-true (buffer-modified? l))
      (check= (user-labels (list-references* #t)) '("a1" "c1" "c2" "eq1"))
      (check= (user-labels (list-references* #f)) '())
      ;; the view which the file had before sees the references of the
      ;; master, until the file is detached
      (check= (num "a1") "1.1")
      (check= (num "c2") "2")
      (project-detach)
      (update-forced)
      (check-false (project-attached?))
      (check-true (url-none? (project-get)))
      (check= (num "a1") '(uninit))
      ;; Use as master
      (buffer-toggle-master)
      (check= (get-init "project-flag") "true")
      (check-true (project-attached?))
      (check= (project-get) l)
      (buffer-toggle-master)
      (check= (get-init "project-flag") "false")
      (check-false (project-attached?)))))

;; Saving a part view (#77, item 2): the view shows each included file in
;; a shared copy; saving writes the part file and the included files whose
;; copies changed, and only those; an included file open as a buffer is
;; saved through the buffer, which keeps its own unsaved changes; a file
;; which changed on disk since it was loaded or which cannot be written is
;; refused and the view stays modified. Document > Part stars the files
;; with unsaved changes, and the question when closing the view names them.
(define (write-part-project)
  (write-doc "pc1.tm" '("book") '() '("one"))
  (write-doc "pc2.tm" '("book") '() '("two"))
  (write-doc "pc3.tm" '("book") '() '("three"))
  (write-doc "pm.tm" '("book") '()
             '((include "pc1.tm") (include "pc2.tm") (include "pc3.tm"))))

(define (file-body name)
  (tmfile-extract (tree-import (tmp name) "texmacs") 'body))

(define (buffer-body u)
  (tree->stree (buffer-get-body u)))

(define (part-labels m)
  (map car (part-menu-entries m)))

(define (test-part-save)
  (check-group "part view save")
  (write-part-project)
  (let* ((m (tmp "pm.tm"))
         (c2 (tmp "pc2.tm"))
         (c3 (tmp "pc3.tm"))
         (p (system->url (part-url m m)))
         (edit-copy (lambda (i s)
                      (switch-to-buffer p)
                      (edit (tree-set (bt i 2 0) s)))))
    (load-buffer p)
    (update-forced)
    (check= (car (st 1)) 'shared)
    (check= (part-labels m) '("pm.tm" "pc1.tm" "pc2.tm" "pc3.tm"))
    ;; an edit in the copy of pc1.tm is written into pc1.tm, and only there
    (edit-copy 0 "one edited")
    (check= (part-labels m) '("pm.tm" "pc1.tm *" "pc2.tm" "pc3.tm"))
    (check= (buffer-close-question p)
            "The document has not been saved (changes in pc1.tm). Really close it?")
    (save-buffer p)
    (check= (file-body "pc1.tm") '(document "one edited"))
    (check= (file-body "pc2.tm") '(document "two"))
    (check= (file-body "pc3.tm") '(document "three"))
    (check= (file-body "pm.tm")
            '(document (include "pc1.tm") (include "pc2.tm")
                       (include "pc3.tm")))
    (check= (part-save-message) "Saved pm.tm, pc1.tm")
    (check-false (buffer-modified? p))
    (check= (part-labels m) '("pm.tm" "pc1.tm" "pc2.tm" "pc3.tm"))
    (check= (buffer-close-question p)
            "The document has not been saved. Really close it?")
    ;; pc2.tm is open: it is saved through its buffer
    (load-buffer c2)
    (edit-copy 1 "two edited")
    (save-buffer p)
    (check= (file-body "pc2.tm") '(document "two edited"))
    (check= (buffer-body c2) '(document "two edited"))
    (check-false (buffer-modified? c2))
    (check= (part-save-message) "Saved pm.tm, pc2.tm")
    (check= (file-body "pc1.tm") '(document "one edited"))
    ;; pc3.tm is open with unsaved changes of its own when its copy is
    ;; changed: they are kept, and pc3.tm is not saved
    (load-buffer c3)
    (edit (tree-set (bt 0) "three mine"))
    (edit-copy 2 "three view")
    (buffer-shared-flush)
    (check= (buffer-body c3) '(document "three mine"))
    (check-true (buffer-modified? c3))
    (save-buffer p)
    (check= (buffer-body c3) '(document "three mine"))
    (check= (file-body "pc3.tm") '(document "three"))
    (check= (part-save-message)
            (string-append "Saved pm.tm; not saved: pc3.tm "
                           "(it is open with other unsaved changes)"))
    (check-true (buffer-modified? p))
    (check= (part-labels m) '("pm.tm" "pc1.tm" "pc2.tm" "pc3.tm *"))
    (check= (buffer-close-question p)
            "The document has not been saved (changes in pc3.tm). Really close it?")
    ;; pc1.tm changed on disk since it was loaded: the save asks first, and
    ;; without the answer pc1.tm is not written
    (buffer-pretend-saved c3)
    (buffer-close c3)
    (write-doc "pc1.tm" '("book") '() '("one external"))
    (edit-copy 0 "one again")
    (check= (map cdr (part-save-plan p))
            '((write #t) (same #f) (write #f)))
    (buffer-save p)
    (check= (file-body "pc1.tm") '(document "one external"))
    (check= (file-body "pc3.tm") '(document "three view"))
    (check= (part-save-message)
            (string-append "Saved pm.tm, pc3.tm; not saved: pc1.tm "
                           "(the file changed on disk)"))
    ;; a file which cannot be written
    (buffer-pretend-saved c2)
    (buffer-close c2)
    (system (string-append "chmod a-w " (url->system c2)))
    (edit-copy 1 "two again")
    (buffer-save p)
    (system (string-append "chmod u+w " (url->system c2)))
    (check= (file-body "pc2.tm") '(document "two edited"))
    (check-true (in? (cons c2 "the file is not writable") part-save-refused))
    (buffer-pretend-saved p)))

;; The identifiers of the parts (#77, item 8): a part is known by its
;; number everywhere; parts with the same title are told apart, in a
;; document and in the part view of a master, whose menu labels name the
;; file with which a part starts. A part view of a master keeps the parts
;; of the master when it is saved, with inclusions in place of the copies.
(define (part-shown)
  ;; the identifiers of the parts shown, read from the tree
  (map cadr (list-filter (cdr (tree->stree (buffer-tree)))
                         (lambda (x) (func? x 'show-part)))))

(define (test-part-ids)
  (check-group "part identifiers")
  (write-doc "same.tm" '("article") '()
             '("intro" (section "Same") "a" (section "Same") "b"))
  (open "same.tm")
  (check= (buffer-parts-list #t) '("1" "2" "3"))
  (check= (map buffer-part-label (buffer-parts-list #t))
          '("front matter" "Same" "Same"))
  (buffer-set-part-mode :one)
  (buffer-show-part "3")
  (check= (buffer-parts-list #f) '("3"))
  (check= (part-shown) '("3"))
  (check= (st 2 1) '(document (section "Same") "b"))
  (check-true (show-hidden-part "2"))
  (check= (part-shown) '("2"))
  (buffer-set-part-mode :all)
  ;; an include written ./pa.tm is named pa.tm, relative to the master
  (write-doc "pa.tm" '("book") '() '((chapter "Same") "a"))
  (write-doc "dot.tm" '("article") '()
             '((include "./pa.tm") (section "S") "x"))
  (open "dot.tm")
  (check= (map buffer-part-label (buffer-parts-list #t)) '("pa.tm" "S"))
  ;; the part view of a master whose chapters have the same title
  (write-doc "pa.tm" '("book") '() '((chapter "Same") "a"))
  (write-doc "pb.tm" '("book") '() '((chapter "same") "b"))
  (write-doc "pi.tm" '("book") '() '((include "pa.tm") (include "pb.tm")))
  (let* ((m (tmp "pi.tm"))
         (p (system->url (part-url m m))))
    (load-buffer p)
    (update-forced)
    (check= (buffer-parts-list #t) '("1" "2"))
    (buffer-set-part-mode :one)
    (check= (buffer-parts-list #t) '("1" "2"))
    ;; (the title is capitalized, not the file)
    (check= (map buffer-part-label (buffer-parts-list #t))
            '("Same (pa.tm)" "Same (pb.tm)"))
    (check= (part-shown) '("1"))
    (buffer-show-part "2")
    (check= (part-shown) '("2"))
    (check= (buffer-parts-list #f) '("2"))
    (check= (car (st 1 1 0)) 'shared)
    (check= (caddr (st 1 1 0)) (url->unix (tmp "pb.tm")))
    ;; a hidden part evaluates the chapter of its file, not a second copy
    (check= (st 0 2) '(document (chapter "Same")))
    (check-true (show-hidden-part "1"))
    (check= (part-shown) '("1"))
    ;; an edit in a shown part; the master keeps its parts and inclusions
    (edit (tree-set (bt 0 1 0 2 1) "a edited"))
    (save-buffer p)
    (check= (file-body "pa.tm") '(document (chapter "Same") "a edited"))
    (check= (file-body "pb.tm") '(document (chapter "same") "b"))
    (check= (file-body "pi.tm")
            '(document (show-part "1" (document (include "pa.tm"))
                                  (document (chapter "Same")))
                       (hide-part "2" (document (include "pb.tm"))
                                  (document (chapter "same")))))
    (check-false (buffer-modified? p))
    (buffer-pretend-saved p)
    (buffer-close p))
  ;; a master saved in parts: its part view shows them, and saving it
  ;; keeps them (also with a copy in the alternative of a part, as older
  ;; versions saved)
  (write-doc "pp.tm" '("book") '()
             '((show-part "1" (document (chapter "I") "x" (include "pa.tm"))
                          (document (chapter "I")))
               (hide-part "2" (document (chapter "II") "y")
                          (document (shared "u1" "pb.tm" (document "old"))))))
  (let* ((m (tmp "pp.tm"))
         (p (system->url (part-url m m))))
    (load-buffer p)
    (update-forced)
    (check= (buffer-get-part-mode) :one)
    (check= (car (st 0 1 2)) 'shared)
    (edit (tree-set (bt 0 1 1) "x edited"))
    (save-buffer p)
    (check= (file-body "pp.tm")
            '(document (show-part "1" (document (chapter "I") "x edited"
                                                (include "pa.tm"))
                                  (document (chapter "I")))
                       (hide-part "2" (document (chapter "II") "y")
                                  (document (include "pb.tm")))))
    (buffer-pretend-saved p)))

;; Jumping to a label of a hidden part shows the part: its reference names
;; the part by its number (go_to_label in Edit/Interface/edit_cursor.cpp
;; keeps what comes before the first "." of auto.2-1).
(define (test-part-label-jump)
  (check-group "part label jump")
  (write-doc "jump.tm" '("article") '()
             '((section "One") "a" (section "Two") (concat "b" (label "lb"))))
  (open "jump.tm")
  (buffer-set-part-mode :one)
  (update-all)
  (check= (part-shown) '("1"))
  (with r (ref "lb")
    (check= (and (pair? r) (list-tail r 3)) '("#2"))
    (check= (and (pair? r) (cadr r)) "2"))
  ;; a label in the body of a hidden part is found in the tree
  (go-to-label "lb")
  (check= (part-shown) '("2"))
  (check= (cursor) '(1 1 1 1 1))
  ;; the automatic label of the section of a hidden part is found through
  ;; its reference, auto.1-1 -> #1 -> (show-hidden-part "1")
  (check= (ref "auto.1-1") '(tuple "1" "?" "#1"))
  (go-to-label "auto.1-1")
  (check= (part-shown) '("1"))
  (check= (list-head (cursor) 2) '(0 1))
  (buffer-set-part-mode :all))

;; The style of a part view (#77, item 11) is the style of its master: a
;; change of style in the view gives the master the new style when the view
;; is saved, through the buffer of the master when it is open (saved, or
;; left modified when it has other unsaved changes of its own). The
;; references and the auxiliary data of the view are not saved.
(define (file-style name)
  (tmfile-extract (tree-import (tmp name) "texmacs") 'style))

(define (file-part name what)
  (tmfile-extract (tree-import (tmp name) "texmacs") what))

(define (set-view-style p style)
  (switch-to-buffer p)
  (edit (set-style-tree (stree->tree style))))

(define (test-part-style)
  (check-group "part view style")
  (write-doc "sc1.tm" '("book") '() '((chapter (concat "One" (label "x1"))) "a"))
  (write-doc "sc2.tm" '("book") '() '((chapter "Two") "b"))
  (write-doc "sm.tm" '("book") '() '((include "sc1.tm") (include "sc2.tm")))
  (let* ((m (tmp "sm.tm"))
         (p (system->url (part-url m (tmp "sc1.tm"))))
         (q (system->url (part-url m m)))
         (eu '(tuple "book" "number-europe"))
         (us '(tuple "book" "number-us"))
         (la '(tuple "book" "number-long-article"))
         (incl '(document (include "sc1.tm") (include "sc2.tm"))))
    ;; the master is not open: its file gets the style
    (load-buffer p)
    (update-forced)
    (check-false (buffer-modified? p))
    (check= (part-view-unsaved-files p) '())
    (set-view-style p eu)
    (check-true (buffer-modified? p))
    (check= (part-view-unsaved-files p) (list m))
    (check= (part-labels m) '("sm.tm *" "sc1.tm" "sc2.tm"))
    (check= (part-master-style-plan p) (list m 'write #f))
    (save-buffer p)
    (check= (file-style "sm.tm") eu)
    (check= (file-style "sc1.tm") "book")
    (check= (file-body "sm.tm") incl)
    (check= (part-save-message) "Saved sc1.tm, sm.tm")
    (check-false (buffer-modified? p))
    (check= (part-view-unsaved-files p) '())
    ;; the references of the view are not saved: neither into the part
    ;; file nor into the master
    (check-false (file-part "sm.tm" 'references))
    (check= (file-part "sc1.tm" 'references) '(collection))
    ;; the master changed on disk since the view was loaded: refused
    (write-doc "sm.tm" '("book" "number-long-article") '() (cdr incl))
    (set-view-style p us)
    (check= (part-master-style-plan p) (list m 'write #t))
    (buffer-save p)
    (check= (file-style "sm.tm") la)
    (check= (part-save-message)
            "Saved sc1.tm; not saved: sm.tm (style: the file changed on disk)")
    (buffer-pretend-saved p)
    (buffer-close p)
    ;; the master is open without changes: its buffer gets the style and
    ;; is saved
    (load-buffer m)
    (load-buffer p)
    (update-forced)
    (check= (tmfile-extract (buffer-get p) 'style) la)
    (set-view-style p eu)
    (check= (part-master-style-plan p) (list m 'buffer #f))
    (save-buffer p)
    (check= (file-style "sm.tm") eu)
    (check= (tmfile-extract (buffer-get m) 'style) eu)
    (check-false (buffer-modified? m))
    (check= (part-save-message) "Saved sc1.tm, sm.tm")
    ;; the master is open with changes of its own: its buffer gets the
    ;; style, and is not saved
    (buffer-pretend-modified m)
    (set-view-style p us)
    (check= (part-master-style-plan p) (list m 'pending #f))
    (save-buffer p)
    (check= (tmfile-extract (buffer-get m) 'style) us)
    (check-true (buffer-modified? m))
    (check= (file-style "sm.tm") eu)
    (check= (part-save-message)
            (string-append "Saved sc1.tm; new style in sm.tm "
                           "(open with other unsaved changes, not saved)"))
    (check-false (buffer-modified? p))
    (check= (part-view-unsaved-files p) (list m))
    (buffer-pretend-saved m)
    (buffer-close m)
    (buffer-pretend-saved p)
    (buffer-close p)
    ;; the part view of the master itself
    (load-buffer q)
    (update-forced)
    (set-view-style q us)
    (check= (part-view-unsaved-files q) (list m))
    (save-buffer q)
    (check= (file-style "sm.tm") us)
    (check= (file-body "sm.tm") incl)
    (check= (part-save-message) "Saved sm.tm")
    (check-false (buffer-modified? q))
    (buffer-close q)
    ;; the master is open without a view (as when it is loaded for the
    ;; references of a chapter): its data gets the style, and it is saved
    (buffer-load m)
    (check= (buffer->views m) '())
    (load-buffer p)
    (update-forced)
    (set-view-style p eu)
    (check= (part-master-style-plan p) (list m 'buffer #f))
    (save-buffer p)
    (check= (file-style "sm.tm") eu)
    (check= (tmfile-extract (buffer-get m) 'style) eu)
    (check= (file-body "sm.tm") incl)
    (check= (part-save-message) "Saved sc1.tm, sm.tm")
    (buffer-close m)
    ;; the file of the view could not be written: the master does not get
    ;; the style
    (set-view-style p us)
    (system (string-append "chmod a-w " (url->system (tmp "sc1.tm"))))
    (buffer-save p)
    (system (string-append "chmod u+w " (url->system (tmp "sc1.tm"))))
    (check= (file-style "sm.tm") eu)
    (check= (part-save-message)
            "Not saved: sc1.tm (the file could not be written)")
    (buffer-pretend-saved p)
    (buffer-close p)
    ;; the view of the master itself, whose style changed on disk: the file
    ;; keeps the style on disk
    (load-buffer q)
    (update-forced)
    (write-doc "sm.tm" '("book" "number-long-article") '() (cdr incl))
    (set-view-style q us)
    (check= (part-master-style-plan q) (list m 'write #t))
    (buffer-save q)
    (check= (file-style "sm.tm") la)
    (check= (file-body "sm.tm") incl)
    (check= (part-save-message)
            "Saved sm.tm; not saved: sm.tm (style: the file changed on disk)")
    (buffer-pretend-saved q)))

;; Saving a part view whose own file is also open as an ordinary buffer:
;; the view of the master is not shared with the buffer of the master, and
;; the buffer of an included file diverges from the view of that file when
;; both have changes (part-shared.scm). When that buffer has unsaved
;; changes of its own, the file is not written (as an included file, #390)
;; and the buffer keeps its changes and stays modified; without changes
;; of its own, the buffer gets what was written and is saved.
(define (file-init name)
  (sort (collection->assoc (file-part name 'initial))
        (lambda (x y) (string<? (car x) (car y)))))

(define (part-buffer-style-of u)
  (tm->stree (buffer-get-style u)))

(define (test-part-open-own-file)
  (check-group "part view of an open file")
  (write-doc "oa.tm" '("book") '() '((chapter "A") "a"))
  (write-doc "om.tm" '("book") '() '("intro" (include "oa.tm")))
  (let* ((m (tmp "om.tm"))
         (a (tmp "oa.tm"))
         (q (system->url (part-url m m)))
         (p (system->url (part-url m a)))
         (eu '(tuple "book" "number-europe"))
         (us '(tuple "book" "number-us"))
         (la '(tuple "book" "number-long-article")))
    ;; the master is open with unsaved changes of its own
    (load-buffer m)
    (edit (tree-set (bt 0) "intro mine"))
    (check-true (buffer-modified? m))
    (load-buffer q)
    (update-forced)
    (check= (st 0) "intro")
    (edit (tree-set (bt 0) "intro view"))
    (save-buffer q)
    (check= (file-body "om.tm") '(document "intro" (include "oa.tm")))
    (check= (buffer-body m) '(document "intro mine" (include "oa.tm")))
    (check-true (buffer-modified? m))
    (check= (part-save-message)
            "Not saved: om.tm (it is open with other unsaved changes)")
    (check-true (buffer-modified? q))
    (check= (part-view-unsaved-files q) (list m))
    ;; the master is open without changes of its own: it gets the body
    ;; written and is saved
    (switch-to-buffer m)
    (edit (tree-set (bt 0) "intro"))
    (buffer-pretend-saved m)
    (save-buffer q)
    (check= (file-body "om.tm") '(document "intro view" (include "oa.tm")))
    (check= (buffer-body m) '(document "intro view" (include "oa.tm")))
    (check-false (buffer-modified? m))
    (check= (part-save-message) "Saved om.tm")
    (check-false (buffer-modified? q))
    (buffer-close q)
    (buffer-close m)
    ;; an included file opened, without changes, after its copy was
    ;; changed: its buffer gets the copy and is saved (part-copy-action,
    ;; sync)
    (load-buffer q)
    (update-forced)
    (edit (tree-set (bt 1 2 1) "a copy"))
    (load-buffer a)
    (save-buffer q)
    (check= (file-body "oa.tm") '(document (chapter "A") "a copy"))
    (check= (buffer-body a) '(document (chapter "A") "a copy"))
    (check-false (buffer-modified? a))
    (check= (part-save-message) "Saved om.tm, oa.tm")
    (buffer-close a)
    (buffer-close q)
    (write-doc "oa.tm" '("book") '() '((chapter "A") "a"))
    ;; the view of an included file open with unsaved changes of its own
    (load-buffer a)
    (edit (tree-set (bt 1) "a mine"))
    (load-buffer p)
    (update-forced)
    (check= (car (st 0)) 'shared)
    (check= (st 0 2) '(document (chapter "A") "a"))
    (edit (tree-set (bt 0 2 1) "a view"))
    (save-buffer p)
    (check= (file-body "oa.tm") '(document (chapter "A") "a"))
    (check= (buffer-body a) '(document (chapter "A") "a mine"))
    (check-true (buffer-modified? a))
    (check= (part-save-message)
            "Not saved: oa.tm (it is open with other unsaved changes)")
    (check-true (buffer-modified? p))
    (buffer-pretend-saved p)
    (buffer-close p)
    (buffer-pretend-saved a)
    (buffer-close a)
    ;; the master is open with an unsaved change of its style, and the view
    ;; does not change the body: the buffer is saved itself, with its style
    (write-doc "om.tm" '("book") '() '("intro" (include "oa.tm")))
    (load-buffer m)
    (set-view-style m eu)
    (load-buffer q)
    (update-forced)
    (edit (init-env "font-base-size" "12"))
    (check-true (buffer-modified? q))
    (check= (part-own-plan q) (list m 'buffer #f))
    (save-buffer q)
    (check= (part-save-message) "Saved om.tm")
    (check= (file-style "om.tm") eu)
    (check= (file-body "om.tm") '(document "intro" (include "oa.tm")))
    (check-false (buffer-modified? m))
    (check-false (buffer-modified? q))
    (buffer-close q)
    ;; ... and when the view changes the style too, the master gets the
    ;; style of the view (as an open master gets the style of the view of
    ;; an included file, part-save-master-style)
    (set-view-style m la)
    (load-buffer q)
    (update-forced)
    (set-view-style q us)
    (check= (part-own-plan q) (list m 'buffer #f))
    (save-buffer q)
    (check= (part-save-message) "Saved om.tm")
    (check= (file-style "om.tm") us)
    (check= (part-buffer-style-of m) us)
    (check-false (buffer-modified? m))
    (check-false (buffer-modified? q))
    (buffer-close q)
    (buffer-close m)
    ;; the view of an included file which is not open
    (load-buffer p)
    (update-forced)
    (edit (tree-set (bt 0 2 1) "a alone"))
    (edit (init-env "font-base-size" "11"))
    (check= (part-own-plan p) (list a 'none #f))
    (save-buffer p)
    (check= (part-save-message) "Saved oa.tm")
    (check= (file-body "oa.tm") '(document (chapter "A") "a alone"))
    (check= (file-init "oa.tm")
            '(("font-base-size" . "11") ("page-medium" . "paper")))
    (check-false (buffer-modified? p))
    (buffer-close p)
    ;; an included file open without changes is kept in sync with the view
    ;; of it, and saved itself
    (load-buffer a)
    (load-buffer p)
    (update-forced)
    (edit (tree-set (bt 0 2 1) "a view"))
    (buffer-shared-flush)
    (check= (buffer-body a) '(document (chapter "A") "a view"))
    (check-true (buffer-modified? a))
    (check= (part-own-plan p) (list a 'buffer #f))
    (save-buffer p)
    (check= (part-save-message) "Saved oa.tm")
    (check= (file-body "oa.tm") '(document (chapter "A") "a view"))
    (check-false (buffer-modified? a))
    (check-false (buffer-modified? p))
    ;; ... also with an unsaved change of its style, which it keeps
    (set-view-style a eu)
    (switch-to-buffer p)
    (edit (tree-set (bt 0 2 1) "a again"))
    (buffer-shared-flush)
    (save-buffer p)
    (check= (part-save-message) "Saved oa.tm")
    (check= (file-body "oa.tm") '(document (chapter "A") "a again"))
    (check= (file-style "oa.tm") eu)
    (check-false (buffer-modified? a))
    (check-false (buffer-modified? p))
    ;; ... and a change of the initial environment in the view, which the
    ;; buffer gets before it is saved (the file has what it gets without
    ;; an open buffer, above)
    (set-view-style a la)
    (switch-to-buffer p)
    (edit (tree-set (bt 0 2 1) "a third"))
    (edit (init-env "font-base-size" "12"))
    (buffer-shared-flush)
    (check= (part-own-plan p) (list a 'buffer #f))
    (save-buffer p)
    (check= (part-save-message) "Saved oa.tm")
    (check= (file-body "oa.tm") '(document (chapter "A") "a third"))
    (check= (file-style "oa.tm") la)
    (check= (file-init "oa.tm")
            '(("font-base-size" . "12") ("page-medium" . "paper")))
    (check-false (buffer-modified? a))
    (check-false (buffer-modified? p))
    (buffer-close p)
    (buffer-close a)
    ;; a file included by the included file: the open buffer of the
    ;; included file gets copies of it from the view (buffer-restore in
    ;; part-shared.scm), which are inclusions again when it is saved
    (write-doc "ob.tm" '("book") '() '((section "B") "b"))
    (write-doc "oa.tm" '("book") '() '((chapter "A") "a" (include "ob.tm")))
    (load-buffer a)
    (load-buffer p)
    (update-forced)
    (edit (tree-set (bt 0 2 2 2 1) "b view"))
    (buffer-shared-flush)
    (save-buffer p)
    (check= (part-save-message) "Saved oa.tm, ob.tm")
    (check= (file-body "ob.tm") '(document (section "B") "b view"))
    (check= (file-body "oa.tm") '(document (chapter "A") "a" (include "ob.tm")))
    (check= (buffer-body a) '(document (chapter "A") "a" (include "ob.tm")))
    (check-false (buffer-modified? a))
    (check-false (buffer-modified? p))
    (switch-to-buffer p)
    (edit (tree-set (bt 0 2 2 2 1) "b again"))
    (buffer-shared-flush)
    (save-buffer p)
    (check= (part-save-message) "Saved oa.tm, ob.tm")
    (check= (file-body "ob.tm") '(document (section "B") "b again"))
    (check-false (buffer-modified? a))
    (check-false (buffer-modified? p))
    (buffer-pretend-saved p)
    (buffer-close p)
    (buffer-pretend-saved a)
    (buffer-close a)
    ;; the view of the master, whose file (body and style) changed on disk:
    ;; the question names it once
    (write-doc "om.tm" '("book") '() '("intro" (include "oa.tm")))
    (load-buffer m)
    (load-buffer q)
    (update-forced)
    (edit (tree-set (bt 0) "intro again"))
    (set-view-style q us)
    (write-doc "om.tm" '("book" "number-long-article") '()
               '("intro disk" (include "oa.tm")))
    (check= (part-own-plan q) (list m 'sync #t))
    (check= (part-master-style-plan q) (list m 'write #t))
    (check= (part-save-changed q) (list m))
    (buffer-pretend-saved q)
    (buffer-close q)
    (buffer-close m)))

;; The initial environment of the view of the master (#421): the view
;; saves the changes of its initial environment into the master, as the
;; master buffer itself would, and writes nothing which it inherits from
;; the master when it is loaded (part-flag, the extra initial environment
;; of the parts). An open master gets what was written.
(define (master-buffer-init u var)
  (collection-ref (tm->stree (tmfile-extract (buffer-get u) 'initial)) var))

(define (write-master-init)
  (write-doc "im.tm" '("book") '(("par-first" "0fn"))
             '("intro" (include "ia.tm"))))

(define (test-part-master-init)
  (check-group "part view of the master: initial environment")
  (write-doc "ia.tm" '("book") '() '((chapter "A") "a"))
  (write-master-init)
  (let* ((m (tmp "im.tm"))
         (q (system->url (part-url m m)))
         (eu '(tuple "book" "number-europe")))
    ;; a save without a change of the initial environment keeps it
    (load-buffer q)
    (update-forced)
    (edit (tree-set (bt 0) "intro view"))
    (save-buffer q)
    (check= (part-save-message) "Saved im.tm")
    (check= (file-body "im.tm") '(document "intro view" (include "ia.tm")))
    (check= (file-init "im.tm")
            '(("page-medium" . "paper") ("par-first" . "0fn")))
    ;; a new entry and a changed entry are written into the master
    (edit (init-env "font-base-size" "12"))
    (edit (init-env "par-first" "2fn"))
    (check-true (buffer-modified? q))
    (save-buffer q)
    (check= (part-save-message) "Saved im.tm")
    (check= (file-init "im.tm")
            '(("font-base-size" . "12") ("page-medium" . "paper")
              ("par-first" . "2fn")))
    (check-false (buffer-modified? q))
    (buffer-close q)
    ;; a master saved with its parts gives its view part-flag and the
    ;; extra initial environment of the parts, which are not written
    (write-master-init)
    (with doc (tree->stree (tree-import m "texmacs"))
      (tree-export
       (stree->tree
        (tmfile-assign doc 'auxiliary
                       '(collection
                         (associate "parts"
                                    (document
                                     (tuple "im.tm" "chapter-nr" "0")
                                     (tuple "ia.tm" "chapter-nr" "1"))))))
       m "texmacs"))
    (load-buffer q)
    (update-forced)
    (edit (init-env "font-base-size" "11"))
    (save-buffer q)
    (check= (part-save-message) "Saved im.tm")
    (check= (file-init "im.tm")
            '(("font-base-size" . "11") ("page-medium" . "paper")
              ("par-first" . "0fn")))
    (buffer-close q)
    ;; the master is open without changes: its file is written, and its
    ;; buffer gets the initial environment written before it is marked
    ;; saved
    (write-master-init)
    (load-buffer m)
    (load-buffer q)
    (update-forced)
    (edit (init-env "font-base-size" "12"))
    (check= (part-own-plan q) (list m 'same #f))
    (save-buffer q)
    (check= (part-save-message) "Saved im.tm")
    (check= (file-init "im.tm")
            '(("font-base-size" . "12") ("page-medium" . "paper")
              ("par-first" . "0fn")))
    (check= (master-buffer-init m "font-base-size") "12")
    (check-false (buffer-modified? m))
    (check-false (buffer-modified? q))
    (buffer-close q)
    (buffer-close m)
    ;; the master is open with an unsaved change of its style: it is saved
    ;; itself, with its style and the initial environment of the view
    (write-master-init)
    (load-buffer m)
    (set-view-style m eu)
    (load-buffer q)
    (update-forced)
    (edit (init-env "font-base-size" "12"))
    (check= (part-own-plan q) (list m 'buffer #f))
    (save-buffer q)
    (check= (part-save-message) "Saved im.tm")
    (check= (file-style "im.tm") eu)
    (check= (file-init "im.tm")
            '(("font-base-size" . "12") ("page-medium" . "paper")
              ("par-first" . "0fn")))
    (check= (master-buffer-init m "font-base-size") "12")
    (check-false (buffer-modified? m))
    (check-false (buffer-modified? q))
    (buffer-close q)
    (buffer-close m)
    ;; an entry removed in the view is removed from the file and from an
    ;; open buffer without changes, which has what was written
    (write-master-init)
    (load-buffer m)
    (load-buffer q)
    (update-forced)
    (edit (init-default "par-first"))
    (edit (init-env "font-base-size" "12"))
    (check= (part-own-plan q) (list m 'same #f))
    (save-buffer q)
    (check= (part-save-message) "Saved im.tm")
    (check= (file-init "im.tm")
            '(("font-base-size" . "12") ("page-medium" . "paper")))
    (check= (master-buffer-init m "par-first") #f)
    (check= (master-buffer-init m "font-base-size") "12")
    (check-false (buffer-modified? m))
    (buffer-close q)
    (buffer-close m)
    ;; ... also from the open buffer of an included file
    (write-doc "ia.tm" '("book") '(("par-first" "1fn")) '((chapter "A") "a"))
    (write-master-init)
    (with a (tmp "ia.tm")
      (with p (system->url (part-url m a))
        (load-buffer a)
        (load-buffer p)
        (update-forced)
        (edit (init-default "par-first"))
        (edit (init-env "font-base-size" "12"))
        (check= (part-own-plan p) (list a 'same #f))
        (save-buffer p)
        (check= (part-save-message) "Saved ia.tm")
        (check= (file-init "ia.tm")
                '(("font-base-size" . "12") ("page-medium" . "paper")))
        (check= (master-buffer-init a "par-first") #f)
        (check= (master-buffer-init a "font-base-size") "12")
        (check-false (buffer-modified? a))
        (buffer-close p)
        (buffer-close a)))
    (write-doc "ia.tm" '("book") '() '((chapter "A") "a"))
    ;; the initial environment of the master changed on disk after the view
    ;; was loaded: the save asks first
    (write-master-init)
    (load-buffer q)
    (update-forced)
    (with doc (tree->stree (tree-import m "texmacs"))
      (tree-export
       (stree->tree
        (tmfile-assign doc 'initial
                       '(collection (associate "page-medium" "paper")
                                    (associate "par-first" "3fn")
                                    (associate "font" "pagella"))))
       m "texmacs"))
    (edit (init-env "font-base-size" "12"))
    (check= (part-save-changed q) (list m))
    (buffer-pretend-saved q)
    (buffer-close q)
    ;; the master lost the extra initial environment of its parts after the
    ;; view was loaded: the view does not write what it inherited then
    (write-master-init)
    (with doc (tree->stree (tree-import m "texmacs"))
      (tree-export
       (stree->tree
        (tmfile-assign doc 'auxiliary
                       '(collection
                         (associate "parts"
                                    (document
                                     (tuple "im.tm" "chapter-nr" "0"))))))
       m "texmacs"))
    (load-buffer q)
    (update-forced)
    (write-master-init)
    (edit (init-env "font-base-size" "12"))
    (check= (part-save-changed q) (list))
    (save-buffer q)
    (check= (part-save-message) "Saved im.tm")
    (check= (file-init "im.tm")
            '(("font-base-size" . "12") ("page-medium" . "paper")
              ("par-first" . "0fn")))
    (buffer-close q)
    ;; a master whose initial environment is not sorted (written by hand),
    ;; saved by its buffer, which sorts it: it did not change on disk
    (write-unsorted-master)
    (load-buffer q)
    (update-forced)
    (load-buffer m)
    (update-forced)
    (buffer-pretend-modified m)
    (buffer-save m)
    (switch-to-buffer q)
    (edit (tree-set (bt 0) "intro view"))
    (check= (part-save-changed q) (list))
    (buffer-pretend-saved q)
    (buffer-close q)
    (buffer-close m)
    ;; ... also when the buffer of the master is saved by a new style in
    ;; the view of an included file
    (write-unsorted-master)
    (with p (system->url (part-url m (tmp "ia.tm")))
      (load-buffer q)
      (update-forced)
      (load-buffer m)
      (update-forced)
      (load-buffer p)
      (update-forced)
      (set-view-style p eu)
      (save-buffer p)
      (check= (file-style "im.tm") eu)
      (switch-to-buffer q)
      (edit (tree-set (bt 0) "intro view"))
      (check= (part-save-changed q) (list))
      (buffer-pretend-saved q)
      (buffer-pretend-saved p)
      (buffer-close p)
      (buffer-close q)
      (buffer-close m))
    ;; the view of an included file keeps the entries of the file which
    ;; the master has too, also when the file is open and saved itself
    (write-master-init)
    (write-doc "ia.tm" '("book") '(("par-first" "0fn")) '((chapter "A") "a"))
    (with a (tmp "ia.tm")
      (with p (system->url (part-url m a))
        (load-buffer a)
        (update-forced)
        (set-view-style a eu)
        (load-buffer p)
        (update-forced)
        (edit (init-env "font-base-size" "12"))
        (check= (part-own-plan p) (list a 'buffer #f))
        (save-buffer p)
        (check= (part-save-message) "Saved ia.tm")
        (check= (file-init "ia.tm")
                '(("font-base-size" . "12") ("page-medium" . "paper")
                  ("par-first" . "0fn")))
        (check= (master-buffer-init a "par-first") "0fn")
        (check-false (buffer-modified? a))
        (buffer-close p)
        (buffer-close a)
        ;; ... and when it is not open
        (write-doc "ia.tm" '("book") '(("par-first" "0fn"))
                   '((chapter "A") "a"))
        (load-buffer p)
        (update-forced)
        (edit (init-env "font-base-size" "12"))
        (check= (part-own-plan p) (list a 'none #f))
        (save-buffer p)
        (check= (file-init "ia.tm")
                '(("font-base-size" . "12") ("page-medium" . "paper")
                  ("par-first" . "0fn")))
        (buffer-close p)
        ;; the view of an included file does not write what it inherited
        ;; from the parts of the master when it was loaded, after the
        ;; master changed them
        (write-doc "ia.tm" '("book") '() '((chapter "A") "a"))
        (with doc (tree->stree (tree-import m "texmacs"))
          (tree-export
           (stree->tree
            (tmfile-assign doc 'auxiliary
                           '(collection
                             (associate "parts"
                                        (document
                                         (tuple "ia.tm" "chapter-nr" "1"))))))
           m "texmacs"))
        (load-buffer p)
        (update-forced)
        (write-master-init)
        (edit (init-env "font-base-size" "12"))
        (save-buffer p)
        (check= (part-save-message) "Saved ia.tm")
        (check= (file-init "ia.tm")
                '(("font-base-size" . "12") ("page-medium" . "paper")))
        (buffer-close p)))
    (write-doc "ia.tm" '("book") '() '((chapter "A") "a"))))

(define (write-unsorted-master)
  ;; the master im.tm with an initial environment in no order
  (tree-export
   (stree->tree
    '(document (TeXmacs "2.1") (style (tuple "book"))
               (body (document "intro" (include "ia.tm")))
               (initial (collection (associate "zz" "1")
                                    (associate "page-medium" "paper")
                                    (associate "par-first" "0fn")
                                    (associate "aa" "2")))))
   (tmp "im.tm") "texmacs"))

;; The part tmfs opens a file of a project as a document of its own: its
;; url names the master and the file, its body is shared with the file and
;; its environment marks it as a part.
;; Document > Part in a part view lists the files of the master, then the
;; part modes of the view itself and, in the modes :one and :several, its
;; parts, as in an ordinary document
(define (menu-labels r)
  ;; the strings of the expanded menu @r: the labels of its items, also
  ;; those of the check items, (check label "v" checked?)
  (cond ((string? r) (list r))
        ((pair? r) (append (menu-labels (car r)) (menu-labels (cdr r))))
        (else (list))))

(define (part-menu-labels)
  (menu-labels (menu-expand '(vertical (link document-part-menu)))))

(define (test-part-view-menu)
  (check-group "part view menu")
  (write-doc "va.tm" '("book") '() '((chapter "Alpha") "a"))
  (write-doc "vb.tm" '("book") '() '((chapter "Beta") "b"))
  (write-doc "vm.tm" '("book") '() '((include "va.tm") (include "vb.tm")))
  (let* ((m (tmp "vm.tm"))
         (p (system->url (part-url m m))))
    (load-buffer p)
    (update-forced)
    (buffer-set-part-mode :all)
    (with l (part-menu-labels)
      (check-true (in? "vm.tm" l))
      (check-true (in? "va.tm" l))
      (check-true (in? "Show one part" l))
      (check-true (in? "Show several parts" l))
      (check-true (in? "Show all parts" l))
      ;; (no list of the parts when they are all shown)
      (check-false (in? "Alpha (va.tm)" l)))
    (buffer-set-part-mode :one)
    (with l (part-menu-labels)
      (check-true (in? "Alpha (va.tm)" l))
      (check-true (in? "Beta (vb.tm)" l)))
    (buffer-set-part-mode :all)
    (buffer-pretend-saved p)
    (buffer-close p)))

(define (test-parts-tmfs)
  (check-group "part tmfs")
  (write-project)
  (let* ((m (tmp "master.tm"))
         (c (tmp "ch1.tm"))
         (s (part-url m c))
         (p (system->url s)))
    (check-true (string-starts? s "tmfs://part/"))
    (check= (part-url m m) (string-append "tmfs://part/" (url->tmfs-string m)))
    (check= (part-open-name p) (string-drop s (string-length "tmfs://part/")))
    (check= (part-master (part-open-name p)) m)
    (check= (part-file (part-open-name p)) c)
    (check= (part-master (url->tmfs-string m)) m)
    (check= (part-file (url->tmfs-string m)) m)
    (load-buffer p)
    (update-forced)
    (check= (current-buffer) p)
    (with t (tree->stree (buffer-tree))
      (check= (car t) 'document)
      (check= (length t) 2)
      (check= (car (cadr t)) 'shared)
      (check= (caddr (cadr t)) (url->unix c))
      (check= (cadddr (cadr t))
              '(document (chapter (concat "One" (label "c1")))
                         (section (concat "Alpha" (label "a1")))
                         (equation (document (concat "x" (label "eq1")))))))
    ;; the environment of the master is inherited, but for the page
    (check= (get-init "font-base-size") "12")
    (check= (get-init "page-medium") "paper")
    (check= (user-labels (list-references)) '("a1" "c1" "eq1"))
    (check= (num "a1") "1.1")
    (check= (buffer-get-title p) "master - ch1")))

;; The parts of a document: one or several of its principal sections are
;; shown, or all of them; the preamble is shown instead of the body. The
;; parts are known by their numbers (#77, item 8: a part which starts with
;; an included file is known by its file, see test-part-ids); the menus show
;; their titles, and their titles are still accepted.
(define (test-document-parts)
  (check-group "document parts")
  (write-doc "parts.tm" '("article") '()
             '((section "One") "a" (section "Two") "b" (section "Three") "c"))
  (open "parts.tm")
  (check= (buffer-get-part-mode) :all)
  (check= (buffer-parts-list #t) '("1" "2" "3"))
  (check= (map buffer-part-label (buffer-parts-list #t)) '("One" "Two" "Three"))
  (check-false (buffer-has-preamble?))
  (buffer-set-part-mode :one)
  (check= (buffer-get-part-mode) :one)
  (check= (buffer-parts-list #f) '("1"))
  (check= (buffer-parts-list #t) '("1" "2" "3"))
  (check= (map buffer-part-label (buffer-parts-list #t)) '("One" "Two" "Three"))
  (check= (st 1)
          '(hide-part "2" (document (section "Two") "b")
                      (document (section "Two"))))
  (buffer-show-part "2")
  (check= (buffer-parts-list #f) '("2"))
  (check= (car (st 0)) 'hide-part)
  (check= (car (st 1)) 'show-part)
  (check= (cursor) '(1 1 0 0))
  (check-true (show-hidden-part "3"))
  (check= (buffer-parts-list #f) '("3"))
  ;; a title is still accepted
  (buffer-show-part "Two")
  (check= (buffer-parts-list #f) '("2"))
  (buffer-set-part-mode :several)
  (buffer-toggle-part "1")
  (check= (buffer-parts-list #f) '("1" "2"))
  (buffer-toggle-part "Two")
  (check= (buffer-parts-list #f) '("1"))
  ;; the last part shown stays
  (buffer-toggle-part "1")
  (check= (buffer-parts-list #f) '("1"))
  (buffer-set-part-mode :all)
  (check= (tree->stree (buffer-tree))
          '(document (section "One") "a" (section "Two") "b"
                     (section "Three") "c"))
  ;; the preamble
  (buffer-make-preamble)
  (check-true (buffer-has-preamble?))
  (check-true (in-preamble-mode?))
  (check= (buffer-get-part-mode) :preamble)
  (check= (tree->stree (buffer-tree))
          '(document (show-preamble (document ""))
                     (ignore (document (section "One") "a" (section "Two") "b"
                                       (section "Three") "c"))))
  (toggle-preamble-mode)
  (check-false (in-preamble-mode?))
  (check= (buffer-get-part-mode) :all)
  (check= (st 0) '(hide-preamble (document "")))
  (check= (tree->stree (buffer-get-preamble)) '(document ""))
  (check= (length (buffer-parts-list #t)) 3))

;; Showing and hiding the preamble and changing the mode of the parts move
;; the paragraphs with node operations; each change is one undo step, and
;; undo and redo give back exactly the trees before and after it, also
;; with typing in between and after a redo.
(define (check-undo-redo states)
  ;; @states are the trees from the oldest to the current one, one undo
  ;; step apart: undo back to the first, redo to the last, undo again
  (with back (cdr (reverse states))
    (for (s back)
      (edit (undo 0))
      (check= (tree->stree (buffer-tree)) s)
      (check-visible))
    (for (s (cdr states))
      (edit (redo 0))
      (check= (tree->stree (buffer-tree)) s)
      (check-visible))
    (for (s back)
      (edit (undo 0))
      (check= (tree->stree (buffer-tree)) s)
      (check-visible))))

(define (body-now) (tree->stree (buffer-tree)))

(define (cursor-visible?)
  ;; is the cursor outside the hidden preamble, the hidden body, the
  ;; hidden parts, and the identifiers and alternatives of the parts?
  (let loop ((t (buffer-tree)) (p (rel (cursor-path))))
    (cond ((tree-in? t '(hide-preamble ignore hide-part)) #f)
          ((or (tree-atomic? t) (null? (cdr p))) #t)
          ((and (tree-is? t 'show-part) (!= (car p) 1)) #f)
          (else (loop (tree-ref t (car p)) (cdr p))))))

(define (check-visible)
  ;; after undo and redo, the cursor is where typing is seen
  (check-true (cursor-visible?))
  (check-true (cursor-accessible?)))

(define (open-for-undo name)
  ;; without a window, the first edit step after a buffer is opened is
  ;; still recorded with the author of the view which was current before,
  ;; and undo goes past the changes of other authors: an empty step first
  (open name)
  (edit (noop)))

(define (test-document-parts-undo)
  (check-group "document parts: undo")
  (write-doc "parts-undo.tm" '("article") '()
             '("intro" (section "One") "a" (section "Two") "b"))
  (open-for-undo "parts-undo.tm")
  (let* ((pars '("intro" (section "One") "a" (section "Two") "b"))
         (xpars (cons "xintro" (cdr pars)))
         (s0 (body-now))
         (s1 (begin (edit (buffer-make-preamble)) (body-now)))
         (s2 (begin (edit (insert "p")) (body-now)))
         (s3 (begin (edit (toggle-preamble-mode)) (body-now)))
         (c3 (cursor))
         (s4 (begin (edit (insert "x")) (body-now)))
         (s5 (begin (edit (buffer-set-part-mode :one)) (body-now)))
         (c5 (cursor))
         (s6 (begin (edit (toggle-preamble-mode)) (body-now)))
         (c6 (cursor))
         (s7 (begin (edit (insert "q")) (body-now)))
         (s8 (begin (edit (toggle-preamble-mode)) (body-now)))
         (s9 (begin (edit (toggle-preamble-mode)) (body-now)))
         (s10 (begin (edit (toggle-preamble-mode)) (body-now))))
    (check= s0 `(document ,@pars))
    (check= s1 `(document (show-preamble (document ""))
                          (ignore (document ,@pars))))
    (check= s2 `(document (show-preamble (document "p"))
                          (ignore (document ,@pars))))
    (check= s3 `(document (hide-preamble (document "p")) ,@pars))
    (check= c3 '(1 0))
    (check= s4 `(document (hide-preamble (document "p")) ,@xpars))
    (check= s5 '(document (hide-preamble (document "p"))
                          (show-part "1" (document "xintro") "")
                          (hide-part "2" (document (section "One") "a")
                                     (document (section "One")))
                          (hide-part "3" (document (section "Two") "b")
                                     (document (section "Two")))))
    (check= c5 '(1 1 0 0))
    (check= s6 `(document (show-preamble (document "p"))
                          (ignore (document ,@xpars))))
    (check= c6 '(0 0 0 0))
    (check= s7 `(document (show-preamble (document "qp"))
                          (ignore (document ,@xpars))))
    (check= s8 `(document (hide-preamble (document "qp")) ,@xpars))
    (check= s9 s7)
    (check= s10 s8)
    (check-undo-redo (list s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10))
    ;; removing the empty preamble
    (edit (buffer-set-part-mode :preamble))
    (check= (body-now) s1)
    (edit (kbd-remove (bt 0) #t))
    (check= (body-now) s0)
    (check= (cursor) '(0 0))
    (check-undo-redo (list s0 s1 s0))
    ;; after undo and redo of a toggle, the cursor is in a visible place,
    ;; also when the change was in the hidden preamble or body
    (edit (buffer-make-preamble))
    (edit (insert "p"))
    (edit (toggle-preamble-mode))
    (edit (go-to (at 2 0)))
    (edit (toggle-preamble-mode))
    (edit (undo 0))
    (check= (body-now) `(document (hide-preamble (document "p")) ,@pars))
    (check-visible)
    (edit (toggle-preamble-mode))
    (edit (toggle-preamble-mode))
    (edit (undo 0))
    (check= (body-now) `(document (show-preamble (document "p"))
                                  (ignore (document ,@pars))))
    (check-visible)
    (edit (redo 0))
    (check= (body-now) `(document (hide-preamble (document "p")) ,@pars))
    (check-visible)
    (edit (undo 0))
    (edit (toggle-preamble-mode))
    ;; the same with parts
    (edit (buffer-set-part-mode :one))
    (edit (undo 0))
    (check= (body-now) `(document (hide-preamble (document "p")) ,@pars))
    (check-visible)
    (edit (redo 0))
    (check= (st 1) '(show-part "1" (document "intro") ""))
    (check-visible)
    (edit (buffer-set-part-mode :all))
    (edit (undo 0))
    (check-visible)
    (edit (buffer-show-part "Two"))
    (check= (st 3 0) "3")
    (check= (st 3) '(show-part "3" (document (section "Two") "b")
                               (document (section "Two"))))
    (edit (undo 0))
    (check-visible)
    (edit (redo 0))
    (check-visible)
    (edit (buffer-show-part "Two"))
    (edit (undo 0))
    (check-visible))
  ;; parts with subparts
  (write-doc "parts-undo-book.tm" '("book") '()
             '((chapter "A") "a" (section "A1") "a1" (chapter "B") "b"))
  (open-for-undo "parts-undo-book.tm")
  (let* ((pars '((chapter "A") "a" (section "A1") "a1" (chapter "B") "b"))
         (parts '((show-part "1" (document (chapter "A")
                                           (show-part "1" (document "a") "")
                                           (show-part "2" (document (section "A1") "a1")
                                                      (document (section "A1"))))
                             (document (chapter "A")))
                  (hide-part "2" (document (chapter "B")
                                           (show-part "1" (document "b") ""))
                             (document (chapter "B")))))
         (s0 (body-now))
         (s1 (begin (edit (buffer-set-part-mode :one)) (body-now)))
         (s2 (begin (edit (buffer-set-part-mode :all)) (body-now)))
         (s3 (begin (edit (buffer-set-part-mode :preamble)) (body-now)))
         (s4 (begin (edit (buffer-set-part-mode :one)) (body-now)))
         (s5 (begin (edit (buffer-set-part-mode :preamble)) (body-now))))
    (check= s0 `(document ,@pars))
    (check= s1 `(document ,@parts))
    (check= s2 s0)
    (check= s3 `(document (show-preamble (document ""))
                          (ignore (document ,@pars))))
    (check= s4 `(document (hide-preamble (document "")) ,@parts))
    (check= s5 s3)
    (check-undo-redo (list s0 s1 s2 s3 s4 s5))
    ;; the redo puts the cursor at the end of the shown chapter, in its
    ;; last subpart
    (edit (redo 0))
    (check= (body-now) s1)
    (check-visible))
  ;; typing in the identifier of a hidden part, between mode changes:
  ;; undo and redo must not change the archived trees
  (write-doc "parts-undo-ids.tm" '("article") '()
             '("intro" (section "One") "a" (section "Two") "b"))
  (open-for-undo "parts-undo-ids.tm")
  (let* ((type-id (lambda (s) (tree-insert! (bt 2 0) 0 s)))
         (s0 (body-now))
         (s1 (begin (edit (buffer-set-part-mode :one)) (body-now)))
         (s2 (begin (edit (type-id "q")) (body-now)))
         (s3 (begin (edit (buffer-set-part-mode :all)) (body-now)))
         (s4 (begin (edit (buffer-set-part-mode :one)) (body-now)))
         (s5 (begin (edit (type-id "r")) (body-now))))
    (check= (st 2 0) "r3")
    (check= s3 s0)
    (check-undo-redo (list s0 s1 s2 s3 s4 s5))
    (for (s (cdr (list s0 s1 s2 s3 s4 s5)))
      (edit (redo 0))
      (check= (body-now) s)))
  (test-document-parts-undo-cursor))

(define part-layouts
  (let ((b '(hide-part "9" (document (chapter "B") "b") "")))
    `(("parts-nested.tm"
       (show-part "1" (document (chapter "A") "a"
                                (show-part "2" (document (section "A1") "a1"
                                                         (hide-part "3" (document (subsection "A11") "x") ""))
                                           ""))
                  "")
       ,b)
      ("parts-strings.tm"
       (show-part "1" (document "a" "b"
                                (hide-part "2" (document (section "S") "s") ""))
                  "")
       ,b)
      ("parts-middle.tm"
       (show-part "1" (document (chapter "A")
                                (hide-part "2" (document (section "A1") "a1") "")
                                "a")
                  "")
       ,b)
      ("parts-string.tm" (show-part "1" "" "") ,b)
      ("parts-preamble.tm"
       (hide-preamble (document "p"))
       (show-part "1" (document (chapter "A") "a"
                                (hide-part "2" (document (section "A1") "a1") ""))
                  "")
       ,b))))

(define (check-layout-undo name body)
  (write-doc name '("book") '() body)
  (open-for-undo name)
  ;; showing a part does nothing unless the mode is :one
  (check= (buffer-get-part-mode) :one)
  (with states (list (body-now))
    (for (id (buffer-parts-list #t))
      (edit (buffer-show-part id))
      (when (!= (body-now) (car states))
        (set! states (cons (body-now) states))))
    (edit (toggle-preamble-mode))
    (set! states (cons (body-now) states))
    (check-undo-redo (reverse states))))

(define (check-typing-undo name body p)
  ;; type at the hidden place @p of a document with @body, then undo and
  ;; redo: the cursor goes to a visible place
  (write-doc name '("article") '() body)
  (open-for-undo name)
  (edit (go-to (apply at p)))
  (with s0 (body-now)
    (edit (insert "Z"))
    (with s1 (body-now)
      (check-true (!= s1 s0))
      (check-undo-redo (list s0 s1)))))

(define (test-document-parts-undo-cursor)
  ;; undo and redo of a change of the shown parts leave the cursor in a
  ;; visible place, whatever the layout of the parts; showing a hidden
  ;; part puts the cursor in it
  ;; a shown part whose body is a string
  (write-doc "parts-undo-atom.tm" '("article") '()
             '((show-part "1" "" "")
               (hide-part "2" (document (section "B") "b") "")))
  (open-for-undo "parts-undo-atom.tm")
  (check= (buffer-get-part-mode) :one)
  (let* ((s0 (body-now))
         (s1 (begin (edit (buffer-show-part "B")) (body-now))))
    (check= s1 '(document (hide-part "1" "" "")
                          (show-part "2" (document (section "B") "b") "")))
    (edit (undo 0))
    (check= (body-now) s0)
    (check-visible)
    (edit (redo 0))
    (check= (body-now) s1)
    (check-visible))
  ;; a shown chapter which ends with a hidden section
  (write-doc "parts-undo-nest.tm" '("book") '()
             '((show-part "1" (document (chapter "A") "a"
                                        (show-part "2" (document (section "A1") "a1") "")
                                        (hide-part "3" (document (section "A2") "a2") ""))
                          "")
               (hide-part "4" (document (chapter "B") "b") "")))
  (open-for-undo "parts-undo-nest.tm")
  (check= (buffer-get-part-mode) :one)
  (with s0 (body-now)
    (edit (buffer-show-part "B"))
    (edit (undo 0))
    (check= (body-now) s0)
    (check-visible))
  ;; shown parts which end with a hidden subpart, deeper down or after
  ;; paragraphs which are strings, with a hidden subpart in the middle,
  ;; with a body which is a string, and with a preamble: each part shown
  ;; in turn, then the preamble
  (for (layout part-layouts)
    (check-layout-undo (car layout) (cdr layout)))
  ;; in source mode the hidden parts are shown: the cursor stays where
  ;; the undone change was
  (write-doc "parts-undo-src.tm" '("article") '()
             '("intro" (section "One") "a"))
  (open-for-undo "parts-undo-src.tm")
  (edit (buffer-set-part-mode :one))
  (check= (tree-label (bt 1)) 'hide-part)
  (edit (init-env "preamble" "true"))
  (edit (go-to (at 1 1 1 1)))
  (edit (insert "x"))
  (check= (st 1 1 1) "ax")
  (edit (undo 0))
  (check= (st 1 1 1) "a")
  (check= (cursor) '(1 1 1 1))
  (edit (redo 0))
  (check= (st 1 1 1) "ax")
  (check= (cursor) '(1 1 1 2))
  (edit (init-default "preamble"))
  ;; typing in the alternative of a shown part, and in a shown part which
  ;; has only an identifier (both only by hand or by a script)
  (check-typing-undo "parts-undo-alt.tm"
                     '((show-part "1" (document (section "A") "a") "alt")
                       (hide-part "2" (document (section "B") "b") ""))
                     '(0 2 1))
  (check-typing-undo "parts-undo-short.tm"
                     '((show-part "1")
                       (show-part "2" (document (section "B") "b") "")
                       (hide-part "3" (document (section "C") "c") ""))
                     '(0 0 0))
  ;; :one: showing a hidden part moves the cursor out of the part which
  ;; is hidden now
  (write-doc "parts-undo-sel.tm" '("article") '()
             '("intro" (section "One") "a" (section "Two") "b"))
  (open-for-undo "parts-undo-sel.tm")
  (edit (buffer-set-part-mode :one))
  (check= (cursor) '(0 1 0 0))
  (edit (show-hidden-part "3"))
  (check= (st 2 0) "3")
  (check= (tree-label (bt 2)) 'show-part)
  (check= (tree-label (bt 0)) 'hide-part)
  (check= (cursor) '(2 1 0 0))
  ;; :several: toggling a part, and showing it as a hidden part
  (edit (buffer-set-part-mode :several))
  (edit (buffer-set-part-mode :all))
  (edit (buffer-set-part-mode :several))
  (let* ((s0 (body-now))
         (s1 (begin (edit (buffer-toggle-part "Two")) (body-now))))
    (check= (tree-label (bt 2)) 'hide-part)
    (edit (undo 0))
    (check= (body-now) s0)
    (check-visible)
    (edit (redo 0))
    (check= (body-now) s1)
    (check-visible)
    ;; showing it again, by a toggle or as a hidden part
    (edit (buffer-toggle-part "Two"))
    (check= (body-now) s0)
    (edit (undo 0))
    (check-visible)
    (edit (redo 0))
    (check-visible)
    (edit (undo 0))
    (check= (body-now) s1)
    (edit (show-hidden-part "3"))
    (check= (body-now) s0)
    (edit (undo 0))
    (check= (body-now) s1)
    (check-visible)
    (edit (redo 0))
    (check-visible)))

;; The files to which a document is linked: its bibliography files, with
;; the .bib added, relative to the document.
(define (test-linked-files)
  (check-group "linked files")
  (write-doc "linked.tm" '("article") '() '("text"))
  (with u (open "linked.tm")
    (check= (linked-file-list) '())
    (buffer-set-body u (stree->tree
                        '(document
                          (bibliography "bib" "plain" "refs" (document ""))
                          (with "color" "red"
                            (document (bibliography* "bib2" "alpha" "more.bib"
                                                     "" (document ""))))
                          (bibliography "x" "plain" "" (document ""))
                          (concat (bibliography "y" "plain" "inner" (document ""))))))
    (check= (linked-file-list) (list (tmp "refs.bib") (tmp "more.bib")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (links-test-failures)
  (check-suite "links")
  (make-dir)
  (run-group test-labels)
  (run-group test-set-reference)
  (run-group test-ref-search)
  (run-group test-smart-refs)
  (run-group test-book-numbering)
  (run-group test-toc)
  (run-group test-index)
  (run-group test-glossary)
  (run-group test-index-in-title)
  (run-group test-generate-one)
  (run-group test-hyperlinks)
  (run-group test-hyperlink-files)
  (run-group test-loci)
  (run-group test-navigation)
  (run-group test-link-pages)
  (run-group test-make-link)
  (run-group test-master)
  (run-group test-chapter)
  (run-group test-attach)
  (run-group test-part-save)
  (run-group test-part-ids)
  (run-group test-part-label-jump)
  (run-group test-part-style)
  (run-group test-part-open-own-file)
  (run-group test-part-master-init)
  (run-group test-part-view-menu)
  (run-group test-parts-tmfs)
  (run-group test-document-parts)
  (run-group test-document-parts-undo)
  (run-group test-linked-files)
  (clear-dir)
  (check-end))
