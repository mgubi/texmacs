;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : misc-modules-test.scm
;; DESCRIPTION : tests of the help, fonts, language, education and tools modules
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The smaller modules under progs, without a window:
;;
;;   - doc: the resolution of help files for a topic in the output language
;;     (help-funcs.scm), the files of the Help menu, a parse of every .tm
;;     file of TeXmacs/doc/main, the expansion of a small manual of
;;     traverse and branch tags (tmdoc.scm) through the help file system
;;     (tmfs://help/...), the searches in the documentation (tmdoc-search.scm
;;     and docgrep.scm, which grep the local files) and the insertion of
;;     the meta data of a manual page (tmdoc-edit.scm);
;;   - fonts: the font database for the fonts shipped in TeXmacs/fonts
;;     (families, styles, files, characteristics, masters and features),
;;     the logical font descriptors and their search, the substitutions,
;;     the typesetting with them and init-font, which picks the text and
;;     math fonts and the style package of a font (generic/document-edit);
;;   - language: the language names and locales (System/Language/locale.cpp),
;;     the translations of the dictionaries in langs/natural/dic, the
;;     hyphenation patterns of each language (a long word is broken at a
;;     narrow width) and spell checking, which needs an external speller
;;     (hunspell or aspell) and is skipped without one;
;;   - education: the context predicates and groups of the quiz markup
;;     (edu-drd.scm, edu-edit.scm), the switch between questions, answers
;;     and both (edu-operate), multiple choice lists and the macros of
;;     edu-markup.scm;
;;   - tools: comments (comment-edit.scm), poster blocks and themes
;;     (poster-edit.scm, theme-edit.scm), the end of a spell check
;;     (spell-edit.scm) and the serialization for AI requests (ai-batch.scm).
;;     There is no word counting or statistics tool in progs/tools.
;;
;; Without a window the initial environment of a buffer is not updated
;; after a change of its style (see editing-test.scm), so that the modes
;; which depend on the style (in-manual?, in-poster?, in-edu-text?) are
;; false: the overloads under these modes (the poster page sizes, the
;; propositions of tmdoc-edit) are left out, and the font which a style
;; package such as pagella-font sets is not seen by get-init. The editor's
;; change-time does not advance either, and the comment functions cache
;; their results under it: the suite clears that cache after each change.
;; The tooltips, the scripts attached to fields and the idle work
;; (delayed) never run.

(texmacs-module (check misc-modules-test)
  (:use (check check-lib)
        (doc help-funcs)
        (doc tmdoc)
        (doc tmdoc-search)
        (doc tmdoc-markup)
        (doc tmdoc-edit)
        (doc docgrep)
        (language natural)
        (generic document-edit)
        (generic document-style)
        (education edu-edit)
        (education edu-markup)
        (tools comment comment-edit)
        (tools poster poster-edit)
        (tools theme theme-edit)
        (tools spell spell-edit)
        (tools ai ai-batch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (priv module name)
  ;; a definition of @module which is not exported
  (eval name (resolve-module module)))

(define T stree->tree)

;; the temporary files go to misc-modules in the temporary directory
(define misc-dir
  (string-append (url->system (url-temp-dir)) "/misc-modules"))

(define (tmp-file name)
  (system->url (string-append misc-dir "/" name)))

(define (clear-misc-dir)
  (with d (system->url misc-dir)
    (when (url-exists? d)
      (for-each (lambda (u)
                  (when (url-regular? u) (system-remove u)))
                (url-read-directory d "*"))
      (system-rmdir d))))

(define (save-tmdoc name body)
  ;; a manual page @name with the body @body, in the temporary directory
  (tree-export
   (T `(document (TeXmacs "2.1.4")
                 (style (tuple "tmdoc" "english"))
                 (body ,body)))
   (tmp-file name) "texmacs"))

(define (edit-step thunk)
  ;; one user action, as the event loop wraps a key press (editing-test)
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define-macro (edit . body)
  `(edit-step (lambda () ,@body)))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (rel p)
  ;; the path @p relative to the current buffer
  (list-tail p (length (buffer-path))))

(define (bt . l) (apply tree-ref (cons (buffer-tree) l)))

(define (with-buffer-doc doc style thunk)
  ;; run @thunk in a new buffer holding @doc in the @style, then close it;
  ;; an error counts as one failure
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (T doc))
    (when style (set-style-list style))
    (go-start)
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (run-group thunk)
  ;; an error in a group counts as one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(define (ev t)
  ;; the value of the TeXmacs expression @t, as a Scheme tree
  (tree->stree (texmacs-expand t)))

(define (width t)
  ;; the width of the box of @t in tmpt
  (with r (ev `(box-info ,t "w"))
    (string->number (cadr r))))

(define (height t)
  (with r (ev `(box-info ,t "h"))
    (string->number (cadr r))))

(define (lines w t)
  ;; the number of lines of the paragraph @t at the width @w
  (let ((h (height `(with "par-width" ,w (par-block (document ,t)))))
        (h1 (height `(with "par-width" ,w (par-block (document "x"))))))
    (inexact->exact (round (/ h h1)))))

(define (stree-select x pred?)
  ;; the subtrees of the Scheme tree @x which satisfy @pred?
  (cond ((pred? x) (list x))
        ((pair? x) (append-map (cut stree-select <> pred?) (cdr x)))
        (else '())))

(define (hlinks x)
  ;; the destinations of the hyperlinks in the Scheme tree @x
  (map caddr (stree-select x (lambda (y) (and (pair? y) (== (car y) 'hlink)
                                              (= (length y) 3))))))

(define (ends? s suffix)
  (and (string? s) (string-ends? s suffix)))

(define (doc-main-files)
  ;; all .tm files below TeXmacs/doc/main
  (let* ((u (url-append (unix->url "$TEXMACS_PATH/doc/main") (url-any)))
         (v (url-expand (url-complete (url-append u (url-wildcard "*.tm"))
                                      "fr"))))
    (url->list v)))

(define (reset-comment-cache)
  ;; the comment functions cache their results under (change-time), which
  ;; does not advance without a window
  (eval '(set! volatile-cache-stamp #f)
        (resolve-module '(tools comment comment-edit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The articles which the Help menu loads (help-menu.scm)
(define help-menu-topics
  '("about/welcome/new-welcome" "about/welcome/start"
    "main/config/man-configuration" "main/config/man-preferences"
    "main/config/man-config-keyboard" "main/config/man-russian"
    "main/config/man-oriental" "main/man-manual"
    "main/start/man-getting-started" "main/text/man-text"
    "main/math/man-math" "main/table/man-table" "main/links/man-links"
    "main/graphics/man-graphics" "main/layout/man-layout"
    "main/editing/man-editing-tools" "main/beamer/man-beamer"
    "main/interface/man-itf" "main/remote/man-collaborative"
    "devel/style/style" "main/scheme/man-scheme" "devel/plugin/plugins"
    "main/man-reference" "devel/format/basics/basics"
    "devel/format/environment/environment" "devel/format/regular/regular"
    "devel/format/stylesheet/stylesheet" "main/styles/styles"
    "main/convert/man-convert" "about/about" "about/about-summary"
    "about/philosophy/philosophy" "about/authors/authors"))

;; A topic is resolved to the file in the output language, or else to the
;; English one; a file name with its suffix is taken as it is.
(define (test-help-files)
  (check-group "help files")
  (let* ((resolve (priv '(doc help-funcs) 'url-resolve-help))
         (lan (string-take (language-to-locale (get-output-language)) 2))
         (en? (== lan "en")))
    (check-true (url-exists-in-help? "main/man-manual.en.tm"))
    (check-true (url-exists-in-help? "about/about.en.tm"))
    (check-false (url-exists-in-help? "main/no-such-file.en.tm"))
    ;; the answer is cached
    (check-true (url-exists-in-help? "main/man-manual.en.tm"))
    (check-true (url-none? (resolve "main/no-such-topic")))
    (check-true (ends? (url->system (resolve "main/man-manual"))
                       (if en? "/doc/main/man-manual.en.tm"
                           ".tm")))
    (when en?
      (check-true (ends? (url->system (resolve "about/about"))
                         "/doc/about/about.en.tm"))
      (check-true (ends? (url->system (resolve "devel/style/style"))
                         "/doc/devel/style/style.en.tm")))
    ;; every article of the Help menu exists
    (check= (list-filter help-menu-topics
                         (lambda (s) (url-none? (resolve s))))
            '())
    ;; the title of a help file
    (check= (tree->stree
             (help-file-title
              (url-concretize "$TEXMACS_PATH/doc/main/man-manual.en.tm")))
            '(concat "The GNU " (TeXmacs) " manual"))
    (check= (tree->stree
             (help-file-title
              (url-concretize "$TEXMACS_PATH/doc/about/about.en.tm")))
            '(concat "About GNU " (TeXmacs) "-" (TeXmacs-version)))))

;; Every .tm file of the manual parses to a document with a TeXmacs version
;; and a body.
(define (test-doc-parse)
  (check-group "doc/main parses")
  (let* ((l (doc-main-files))
         (en (list-filter l (lambda (u) (ends? (url->unix u) ".en.tm"))))
         (bad (list-filter
               l (lambda (u)
                   (with t (check-run
                            (lambda ()
                              (tree->stree (tree-import u "texmacs"))))
                     (not (and (pair? t) (== (car t) 'document)
                               (assoc 'TeXmacs (cdr t))
                               (assoc 'body (cdr t)))))))))
    (display* "  " (length l) " files in doc/main, " (length en)
              " in English, " (length bad) " which do not parse\n")
    (check-true (>= (length l) 800))
    (check-true (>= (length en) 190))
    (check-true (in? (url->system
                      (url-concretize "$TEXMACS_PATH/doc/main/man-manual.en.tm"))
                     (map url->system l)))
    (check= (map url->unix bad) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expansion of manuals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A small manual: master.en.tm traverses one (a branch), two (continued,
;; so that its title is dropped), three (an extra branch, an appendix) and
;; an optional branch, which is left out; two branches to one again, which
;; is only expanded once.
(define (make-small-manual)
  (save-tmdoc "master.en.tm"
              '(document
                 (tmdoc-title "Master")
                 (traverse (document
                             (branch "One" "one.en.tm")
                             (continue "Two" "two.en.tm")
                             (extra-branch "Three" "three.en.tm")
                             (optional-branch "Opt" "opt.en.tm")))
                 (tmdoc-copyright "2026" "Me")
                 (tmdoc-license "lic")))
  (save-tmdoc "one.en.tm"
              '(document (tmdoc-title "One") "first"
                         (hlink "back" "master.en.tm")
                         (hlink "two" "two.en.tm#lab")))
  (save-tmdoc "two.en.tm"
              '(document (tmdoc-title "Two") "second" (label "lab")
                         (branch "Again" "one.en.tm")))
  (save-tmdoc "three.en.tm"
              '(document (tmdoc-title "Three") "third")))

(define small-manual-rest
  ;; the expansion of the branches of the master
  '((concat (section "One") (label "sec-one"))
    "first" (hlink "back" "master.en.tm") (hlink "two" "two.en.tm#lab")
    "second" (label "lab") ""
    (concat (appendix "Three") (label "sec-three"))
    "third"))

(define (test-tmdoc-expand)
  (check-group "tmdoc expansion")
  (make-small-manual)
  (let ((expand (priv '(doc tmdoc) 'tmdoc-expand))
        (m (tmp-file "master.en.tm")))
    (check-true (url-exists? m))
    ;; as an article, the title stays and the branches are sections
    (check= (expand m m 'tmdoc-title)
            `(document (concat (tmdoc-title "Master") (label "sec-master"))
                       ,@small-manual-rest))
    ;; as a book, the title is the title of the book and the branches
    ;; are chapters
    (check= (expand m m 'title)
            '(document (title "Master")
                       (concat (chapter "One") (label "sec-one"))
                       "first" (hlink "back" "master.en.tm")
                       (hlink "two" "two.en.tm#lab")
                       "second" (label "lab") ""
                       (concat (appendix "Three") (label "sec-three"))
                       "third"))
    (check= (expand m m 'chapter)
            `(document (concat (chapter "Master") (label "sec-master"))
                       ,@small-manual-rest))
    ;; a file which does not exist expands to nothing
    (check= (expand (tmp-file "none.en.tm") (tmp-file "none.en.tm") 'title)
            '(document ""))
    (check= ((priv '(doc tmdoc) 'tmdoc-language) m) "english")
    ;; a branch is looked for with the suffix of the language
    (check= (url->system ((priv '(doc tmdoc) 'tmdoc-relative) m "one.en.tm"))
            (url->system (tmp-file "one.en.tm")))
    ;; tmdoc-include expands without the chapters
    (check= (tree->stree (tmdoc-include (T (url->system m))))
            `(document ,@small-manual-rest))
    ;; the help file system
    (let ((load (ahash-ref tmfs-handler-table (cons "help" 'load)))
          (title (ahash-ref tmfs-handler-table (cons "help" 'title)))
          (perm (ahash-ref tmfs-handler-table (cons "help" 'permission?))))
      (check= (load (string-append "article/" (url->tmfs-string m)))
              `(document (TeXmacs ,(texmacs-version))
                         (style (tuple "tmdoc" "english"))
                         (body (document
                                 (concat (tmdoc-title "Master")
                                         (label "sec-master"))
                                 ,@small-manual-rest))))
      (check= (load (string-append "normal/" (url->tmfs-string
                                              (tmp-file "three.en.tm"))))
              '(document (TeXmacs "2.1.4")
                         (style (tuple "tmdoc" "english"))
                         (body (document (tmdoc-title "Three") "third"))))
      (with r (load "book/file/no/such/file.en.tm")
        (check= (car r) 'document)
        (check= (cadr (assoc 'style (cdr r))) "tmdoc")
        (check= (cadr (cadr (assoc 'body (cdr r)))) "Broken link."))
      (with r (load (string-append "book/" (url->tmfs-string m)))
        (check= (cadr (assoc 'style (cdr r)))
                `(tuple ,(get-preference "manual style") "english"))
        (check= (cadr (assoc 'body (cdr r)))
                '(document (title "Master")
                           (table-of-contents "toc" (document ""))
                           (concat (chapter "One") (label "sec-one"))
                           "first" "back" (hlink "two" "#lab")
                           "second" (label "lab") ""
                           (concat (appendix "Three") (label "sec-three"))
                           "third"
                           (the-index "idx" (document "")))))
      (check= (title "normal/x" (T '(document (tmdoc-title "Hello") "x")))
              "Help - Hello")
      (check= (title "normal/x" (T '(document "x"))) "tmfs://help/normal/x")
      (check-true (perm "normal/file/no/such/file.en.tm" "read"))
      (check-false (perm "normal/file/no/such/file.en.tm" "write")))))

;; The rewriting functions behind the expansion
(define (test-tmdoc-rewrite)
  (check-group "tmdoc rewriting")
  (let ((down (priv '(doc tmdoc) 'tmdoc-down))
        (lab (priv '(doc tmdoc) 'tmdoc-internal-label))
        (internalize (priv '(doc tmdoc) 'tmdoc-internalize))
        (add-aux (priv '(doc tmdoc) 'tmdoc-add-aux))
        (unlink (priv '(doc tmdoc) 'tmdoc-remove-hyper-links)))
    (check= (down 'title) 'chapter)
    (check= (down 'part) 'chapter)
    (check= (down 'tmdoc-title) 'section)
    (check= (down 'chapter) 'section)
    (check= (down 'appendix) 'section)
    (check= (down 'section) 'subsection)
    (check= (down 'subsection) 'subsubsection)
    (check= (down 'subsubsection) 'paragraph)
    (check= (down 'paragraph) 'subparagraph)
    (check= (lab "foo/bar.en.tm#here") "here")
    (check= (lab "foo/bar.en.tm") "sec-bar")
    (check= (lab 3) #f)
    (check= (internalize '(document (label "sec-x") (hlink "a" "x.en.tm")
                                    (hlink "b" "y.en.tm")))
            '(document (label "sec-x") (hlink "a" "#sec-x") "b"))
    (check= (add-aux '(document (title "T") "a" (cite "x")))
            '(document (title "T") (table-of-contents "toc" (document ""))
                       "a" (cite "x")
                       (bibliography "bib" "tm-plain" "" (document ""))
                       (the-index "idx" (document ""))))
    (check= (add-aux '(document "a" (chapter "Preface")))
            '(document (table-of-contents "toc" (document ""))
                       "a" (chapter* "Preface")
                       (the-index "idx" (document ""))))
    (check= (unlink '(document (hyper-link "a") "c"))
            '(document "a" "c"))
    (check= (tmdoc-find-title (T '(document "x" (tmdoc-title "Hi"))))
            "Help - Hi")
    (check= (tmdoc-find-title (T '(document "x"))) #f)
    (check= (tmdoc-render-keys "C-x C-s")
            '(concat (render-key "C-x") (render-key "C-s")))
    (check= (tmdoc-render-keys "a") '(render-key "a"))
    (check= (tmdoc-render-keys "") '(render-key ""))
    ;; tmdoc-key, tmdoc-key* and tmdoc-shortcut are left out: they call
    ;; (lazy-keyboard-force #t), which loads the keyboard modules of all
    ;; modes for good and changes the bindings which later suites see
    ;; (after it, typing "$" in kbd-menu-test no longer enters math mode,
    ;; and database-test sees the fields of imported entries reordered)
    (check= (tmdoc-render-keys (T "b")) '(render-key "b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching the documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-doc-search)
  (check-group "doc search")
  ;; the explanations of tags, styles, parameters and functions
  ;; (a document of the explanations which are found)
  (with t (tree->stree (tmdoc-search-tag "strong"))
    (check= (car t) 'document)
    (check= (car (cadr t)) 'explain)
    (check= (cadr (cadr t)) '(explain-macro "strong" "content")))
  (with t (tree->stree (tmdoc-search-tag 'strong))
    (check= (cadr (cadr t)) '(explain-macro "strong" "content")))
  (with t (tree->stree (tmdoc-search-style "article"))
    (check= (car (cadr t)) 'explain)
    (check= (cadr (cadr t)) '(tmstyle "article")))
  (with t (tree->stree (tmdoc-search-parameter "font-base-size"))
    (check= (car (cadr t)) 'explain)
    (check-true (pair? (stree-select
                        t (lambda (x) (== x '(var-val "font-base-size"
                                                      "10"))))))
    (check-true (pair? (stree-select
                        t (lambda (x) (== x '(label "font-base-size")))))))
  (check= (tmdoc-search-tag "no-such-tag-zzz") #f)
  (check= (tmdoc-search-style "no-such-style-zzz") #f)
  ;; the scores of files for keywords
  (check= (system-search-score (tmp-file "one.en.tm") '("zzz")) 0)
  (check-true (> (system-search-score (tmp-file "one.en.tm") '("first")) 0))
  (check-true (> (system-search-score (tmp-file "two.en.tm")
                                      '("second" "Two"))
                 (system-search-score (tmp-file "one.en.tm") '("first"))))
  ;; a search in a directory gives a page of links, the best one first
  (let* ((docgrep (priv '(doc docgrep) 'docgrep))
         (r (docgrep "second" misc-dir "*.en.tm"))
         (none (docgrep "zzzqqq" misc-dir "*.en.tm")))
    (check= (car r) 'document)
    (check= (map url->system (hlinks r))
            (list (url->system (tmp-file "two.en.tm"))))
    (check-true (pair? (stree-select
                        r (lambda (x) (== x '(concat "100" "%"))))))
    (check= (hlinks none) '())
    (check-true (pair? (stree-select
                        none (lambda (x) (and (pair? x)
                                              (== (car x) 'tmdoc-title)))))))
  ;; in the documentation of TeXmacs
  (let* ((docgrep (priv '(doc docgrep) 'docgrep))
         (r (docgrep "hyphenation" "$TEXMACS_DOC_PATH" "*.en.tm"))
         (l (hlinks r)))
    (check-true (>= (length l) 3))
    (check-true (list-and (map (cut ends? <> ".en.tm") l)))
    (check-true (in? "env-par.en.tm" (map (lambda (s) (url->string
                                                       (url-tail s)))
                                          l))))
  ;; the queries of tmfs://grep
  (with q (list->query (list (cons "type" "doc") (cons "what" "a b")))
    (check= (query-ref q "type") "doc")
    (check= (query-ref q "what") "a b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meta data of manual pages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnu-fdl
  (string-append
   "Permission is granted to copy, distribute and/or modify this document "
   "under the terms of the GNU Free Documentation License, Version 1.1 or "
   "any later version published by the Free Software Foundation; with no "
   "Invariant Sections, with no Front-Cover Texts, and with no Back-Cover "
   "Texts. A copy of the license is included in the section entitled "
   "\"GNU Free Documentation License\"."))

(define (test-tmdoc-edit)
  (check-group "tmdoc meta data")
  (with-buffer-doc '(document "x") '("tmdoc")
    (lambda ()
      (edit (tmdoc-insert-title))
      (check= (body) '(document (tmdoc-title "") "x"))
      (check= (rel (cursor-path)) '(0 0 0))
      (edit (tmdoc-insert-copyright-and-license))
      (check= (body) `(document (tmdoc-title "") "x"
                                (tmdoc-copyright "" "")
                                (tmdoc-license ,gnu-fdl)))
      ;; the cursor is in the copyright
      (check= (rel (cursor-path)) '(2 0 0))
      ;; a second copyright goes before the license
      (edit (tmdoc-insert-copyright))
      (check= (bt 3) (T '(tmdoc-copyright "" "")))
      (check= (tree-label (bt 4)) 'tmdoc-license)))
  (with-buffer-doc '(document "x") '("tmweb")
    (lambda ()
      (edit (tmweb-insert-title))
      (check= (body) '(document (tmweb-title "" "") "x"))
      (edit (tmweb-insert-copyright-and-license))
      (check= (body) '(document (tmweb-title "" "") "x"
                                (tmdoc-copyright "" "") (tmweb-license))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The font database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The families of the fonts in TeXmacs/fonts/truetype
(define shipped-families
  '("Fira Mono" "Fira Sans" "Linux Biolinum" "Linux Libertine"
    "OpenDyslexic" "Stix" "Stix Math"
    "TeX Gyre Adventor" "TeX Gyre Bonum" "TeX Gyre Bonum Math"
    "TeX Gyre Cursor" "TeX Gyre Heros" "TeX Gyre Pagella"
    "TeX Gyre Pagella Math" "TeX Gyre Schola" "TeX Gyre Schola Math"
    "TeX Gyre Termes" "TeX Gyre Termes Math"))

;; NOTE: other fonts may be installed, for instance by TeX Live, with more
;; styles of the same families (Fira Sans Book, Fira Mono Oblique) and
;; files of the same names: the checks require the shipped styles and files
;; to be found, not to be the only ones.
(define (includes? l required)
  (and (list? l) (list-and (map (cut in? <> l) required))))

(define (test-font-database)
  (check-group "font database")
  (let ((fams (font-database-families)))
    (check= (list-filter shipped-families (lambda (f) (nin? f fams))) '()))
  (check-true (includes? (font-database-styles "TeX Gyre Pagella")
                         '("Bold" "Bold Italic" "Italic" "Regular")))
  (check-true (includes? (font-database-styles "Stix")
                         '("Bold" "Bold Italic" "Italic" "Regular")))
  (check-true (includes? (font-database-styles "Fira Sans")
                         '("Bold" "Bold Italic" "Italic" "Regular")))
  (check-true (includes? (font-database-styles "Fira Mono")
                         '("Bold" "Regular")))
  (check-true (includes? (font-database-styles "Linux Biolinum")
                         '("Bold" "Italic" "Regular")))
  (check-true (includes? (font-database-styles "Stix Math")
                         '("Regular")))
  (check-true (includes? (font-database-styles "TeX Gyre Pagella Math")
                         '("Regular")))
  (check= (font-database-styles "No Such Font Zzz") '())
  ;; the files of a style
  (check-true (in? "texgyrepagella-regular.otf"
                  (font-database-search "TeX Gyre Pagella" "Regular")))
  (check-true (in? "texgyrepagella-math.otf"
                  (font-database-search "TeX Gyre Pagella Math" "Regular")))
  (check-true (in? "LinLibertine_RBI.otf"
                  (font-database-search "Linux Libertine" "Bold Italic")))
  (check-true (in? "FiraMono-Bold.otf"
                  (font-database-search "Fira Mono" "Bold")))
  (check-true (in? "STIX-Bold.otf"
                  (font-database-search "Stix" "Bold")))
  (check-true (tt-exists? "texgyrepagella-regular"))
  (check-true (tt-exists? "LinLibertine_R"))
  (check-true (font-exists-in-tt? "FiraSans-Regular"))
  (check-false (tt-exists? "no-such-font-file-zzz"))
  (check= (map (lambda (x) (list (symbol->string (car x)) (cadr x)))
               (tt-font-name (url-concretize "$TEXMACS_PATH/fonts/truetype/fira/FiraSans-Bold.otf")))
          '(("Fira Sans" Bold)))
  ;; the characteristics, as the font selector filters them
  (with c (font-database-characteristics "TeX Gyre Pagella" "Regular")
    (check-true (in? "Latin" c))
    (check-true (in? "Greek" c))
    (check-true (in? "mono=no" c))
    (check-true (in? "sans=no" c))
    (check-true (in? "italic=no" c))
    (check-true (in? "slant=0" c)))
  (with c (font-database-characteristics "TeX Gyre Pagella" "Italic")
    (check-true (in? "italic=yes" c))
    (check-true (in? "slant=15" c)))
  (with c (font-database-characteristics "Fira Mono" "Regular")
    (check-true (in? "mono=yes" c))
    (check-true (in? "sans=yes" c))
    (check-true (in? "Cyrillic" c)))
  (with c (font-database-characteristics "Stix Math" "Regular")
    (check-true (in? "MathSymbols" c))
    (check-true (in? "MathExtra" c)))
  ;; masters group the families of a font
  (check= (font-family->master "Fira Sans") "Fira")
  (check= (font-family->master "Fira Mono") "Fira")
  (check= (font-master->families "Fira") '("Fira Mono" "Fira Sans"))
  (check= (font-family->master "TeX Gyre Pagella") "TeX Gyre Pagella")
  (check= (font-family->master "TeX Gyre Pagella Math")
          "TeX Gyre Pagella Math")
  (check= (font-master->families "TeX Gyre Pagella") '("TeX Gyre Pagella"))
  (check= (font-family-main "Fira Sans") "Fira Sans")
  (check= (font-family-main "roman") "roman")
  ;; features
  (check= (font-master-features "Fira") '("sansserif"))
  (check= (font-family-features "Fira Sans") '("sansserif"))
  (check= (font-family-features "Fira Mono") '("mono" "sansserif"))
  (check= (font-family-strict-features "Fira Mono") '("mono"))
  (check= (font-family-features "Linux Biolinum") '("sansserif"))
  (check= (font-family-features "TeX Gyre Heros") '("sansserif"))
  (check= (font-family-features "TeX Gyre Cursor") '("mono"))
  (check= (font-family-features "TeX Gyre Pagella") '())
  (check= (font-family-guessed-features "TeX Gyre Cursor" #t)
          '("TeX Gyre Cursor" "mono"))
  (check= (font-family-guessed-features "Fira Sans" #t)
          '("Fira" "sansserif"))
  (check= (font-style-features "Bold Italic") '("bold" "italic"))
  (check= (font-style-features "Regular") '())
  (check= (font-style-features "Light Condensed") '("light" "condensed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logical fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A logical font is a family followed by features; the public form comes
;; from a family and a style, the private one from the family, variant,
;; series and shape of the environment; a search finds the closest family
;; and style in the database.
(define (test-logical-fonts)
  (check-group "logical fonts")
  (let ((pbi (logical-font-public "TeX Gyre Pagella" "Bold Italic")))
    (check= pbi '("TeX Gyre Pagella" "bold" "italic"))
    (check= (logical-font-family pbi) "TeX Gyre Pagella")
    (check= (logical-font-variant pbi) "rm")
    (check= (logical-font-series pbi) "bold")
    (check= (logical-font-shape pbi) "italic")
    (check= (logical-font-search pbi) '("TeX Gyre Pagella" "Bold Italic"))
    (check= (logical-font-search-exact pbi)
            '("TeX Gyre Pagella" "Bold Italic")))
  (check= (logical-font-exact "TeX Gyre Pagella" "Bold Italic")
          '("TeX Gyre Pagella" "bold" "italic" "ascii" "latin" "greek"))
  (check= (logical-font-exact "Fira Mono" "Regular")
          '("Fira" "mono" "sansserif" "ascii" "latin" "greek" "cyrillic"))
  (check= (logical-font-public "Fira Sans" "Regular") '("Fira"))
  (check= (logical-font-public "Linux Libertine" "Italic")
          '("Linux Libertine" "italic"))
  (check= (logical-font-private "TeX Gyre Pagella" "rm" "bold" "italic")
          '("TeX Gyre Pagella" "bold" "italic"))
  (check= (logical-font-private "roman" "ss" "medium" "right")
          '("roman" "sansserif"))
  (check= (logical-font-private "roman" "tt" "medium" "small-caps")
          '("roman" "typewriter" "smallcaps"))
  (check= (logical-font-private "Fira" "ss" "bold" "italic")
          '("Fira" "sansserif" "bold" "italic"))
  (with fbi (logical-font-private "Fira" "ss" "bold" "italic")
    (check= (logical-font-variant fbi) "ss")
    (check= (logical-font-series fbi) "bold")
    (check= (logical-font-shape fbi) "italic"))
  (check= (logical-font-search (logical-font-public "Fira Sans" "Bold"))
          '("Fira Sans" "Bold"))
  (check= (logical-font-search (logical-font-private "Fira" "ss" "medium"
                                                     "italic"))
          '("Fira Sans" "Italic"))
  (check= (logical-font-search (logical-font-private "Stix" "rm" "medium"
                                                     "italic"))
          '("Stix" "Italic"))
  (check= (logical-font-search (logical-font-private "Linux Libertine" "rm"
                                                     "bold" "italic"))
          '("Linux Libertine" "Bold Italic"))
  (check= (logical-font-search (logical-font-private "TeX Gyre Termes" "rm"
                                                     "bold" "right"))
          '("TeX Gyre Termes" "Bold"))
  ;; there are no small capitals in Bonum: the closest is the bold style
  (check= (logical-font-search (logical-font-private "TeX Gyre Bonum" "rm"
                                                     "bold" "small-caps"))
          '("TeX Gyre Bonum" "Bold"))
  ;; the styles with some features
  (check= (search-font-styles "TeX Gyre Pagella" '("italic"))
          '("Bold Italic" "Italic"))
  (check= (search-font-styles "TeX Gyre Pagella" '("bold" "italic"))
          '("Bold Italic"))
  (check= (search-font-styles "TeX Gyre Pagella" '())
          '("Bold" "Bold Italic" "Italic" "Regular"))
  (check-true (includes? (search-font-styles "Fira Sans" '("bold"))
                         '("Bold" "Bold Italic")))
  (check-true (in? "Fira Sans" (search-font-families '("sansserif"))))
  (check-true (in? "TeX Gyre Heros" (search-font-families '("sansserif"))))
  (check-false (in? "TeX Gyre Pagella" (search-font-families '("sansserif"))))
  ;; patches add features
  (check= (logical-font-patch (logical-font-public "TeX Gyre Pagella" "Regular")
                              '("italic"))
          '("TeX Gyre Pagella" "italic"))
  (check= (logical-font-patch (logical-font-public "TeX Gyre Pagella" "Bold")
                              '("italic"))
          '("TeX Gyre Pagella" "bold" "italic"))
  (check= (logical-font-patch (logical-font-public "Fira Sans" "Regular")
                              '("bold"))
          '("Fira" "bold"))
  ;; substitutions (fonts/font-substitutions.scm) only apply to the fonts
  ;; which are installed
  (check= (logical-font-substitute (logical-font-private "roman" "rm"
                                                         "medium" "right"))
          '("roman"))
  (check= (logical-font-substitute (logical-font-public "TeX Gyre Pagella"
                                                        "Bold"))
          '("TeX Gyre Pagella" "bold")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typesetting with the fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-font-typesetting)
  (check-group "fonts in the typesetter")
  (let ((plain (width "Hello world")))
    (check-true (> plain 0))
    ;; a font name and the name of its family give the same font
    (check= (width '(with "font" "pagella" "Hello world"))
            (width '(with "font" "TeX Gyre Pagella" "Hello world")))
    (check-false (= (width '(with "font" "pagella" "Hello world")) plain))
    (check-false (= (width '(with "font" "termes" "Hello world"))
                    (width '(with "font" "pagella" "Hello world"))))
    ;; an unknown font falls back to another one
    (check-true (> (width '(with "font" "no-such-font-zzz" "Hello world")) 0))
    (check-true (> (width '(with "font-series" "bold" "Hello world")) plain)))
  ;; the typewriter variant is monospaced, the roman one is not
  (check= (width '(with "font-family" "tt" "iiiii"))
          (width '(with "font-family" "tt" "mmmmm")))
  (check-true (< (width "iiiii") (width "mmmmm")))
  ;; the math fonts
  (let ((cm (width '(with "mode" "math" "x+y")))
        (pagella (width '(with "mode" "math" "math-font" "math-pagella"
                               "x+y")))
        (stix (width '(with "mode" "math" "math-font" "math-stix" "x+y"))))
    (check-true (> cm 0))
    (check-false (= pagella cm))
    (check-false (= stix cm))
    (check-false (= stix pagella))))

;; init-font sets the text font, the math font and the style package of a
;; font (Document > Font)
(define (test-init-font)
  (check-group "init-font")
  (with-buffer-doc '(document "x") '("generic")
    (lambda ()
      (check= (get-init "font") "roman")
      (edit (init-font "TeX Gyre Pagella"))
      (check= (get-style-list) '("generic" "pagella-font"))
      ;; the font itself is set by the package
      (check= (get-init "font") "roman")
      (check= (get-init "math-font") "math-pagella")
      (check= (get-init "font-family") "rm")
      (edit (init-font "TeX Gyre Bonum"))
      (check= (get-style-list) '("generic" "bonum-font"))
      (check= (get-init "math-font") "math-bonum")
      (edit (init-font "TeX Gyre Schola"))
      (check= (get-style-list) '("generic" "schola-font"))
      (check= (get-init "math-font") "math-schola")
      (edit (init-font "TeX Gyre Termes"))
      (check= (get-style-list) '("generic" "termes-font"))
      (check= (get-init "math-font") "math-termes")
      ;; Fira has a package, which replaces the previous one
      (edit (init-font "Fira"))
      (check= (get-style-list) '("generic" "fira-font"))
      (edit (init-font "Linux Libertine"))
      (check= (get-style-list) '("generic" "libertine-font"))
      (edit (init-font "Linux Biolinum"))
      (check= (get-style-list) '("generic" "biolinum-font"))
      ;; Stix has no package
      (edit (init-font "Stix"))
      (check= (get-style-list) '("generic"))
      (check= (get-init "font") "stix")
      (check= (get-init "math-font") "math-stix")
      ;; an explicit math font
      (edit (init-font "Fira Sans" "math-dejavu"))
      (check= (get-init "font") "Fira Sans")
      (check= (get-init "math-font") "math-dejavu")
      (edit (init-font "TeXmacs Computer Modern"))
      (check= (get-style-list) '("generic"))
      (check= (get-init "font") "roman")
      (check= (get-init "math-font") "roman")
      (check-true (test-init-font? "roman"))
      (check-false (test-init-font? "Stix"))
      (edit (remove-font-packages))
      (check= (get-style-list) '("generic")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define language-locales
  '(("british" "en_GB") ("bulgarian" "bg_BG") ("chinese" "zh_CN")
    ("croatian" "hr_HR") ("czech" "cs_CZ") ("danish" "da_DK")
    ("dutch" "nl_NL") ("english" "en_US") ("esperanto" "eo_EO")
    ("finnish" "fi_FI") ("french" "fr_FR") ("german" "de_DE")
    ("greek" "gr_GR") ("hungarian" "hu_HU") ("italian" "it_IT")
    ("japanese" "ja_JP") ("korean" "ko_KR") ("polish" "pl_PL")
    ("portuguese" "pt_PT") ("romanian" "ro_RO") ("russian" "ru_RU")
    ("slovak" "sk_SK") ("slovene" "sl_SI") ("spanish" "es_ES")
    ("swedish" "sv_SV") ("taiwanese" "zh_TW") ("ukrainian" "uk_UA")))

(define (test-language-names)
  (check-group "language names and locales")
  (check= (length supported-languages) 27)
  (check= supported-languages (map car language-locales))
  (check= (map language-to-locale supported-languages)
          (map cadr language-locales))
  ;; a locale gives back its language
  (check= (map (lambda (l) (locale-to-language (language-to-locale l)))
               supported-languages)
          supported-languages)
  (check= (language-to-locale "american") "en_US")
  (check= (language-to-locale "klingon") "en_US")
  (check= (locale-to-language "en_GB.UTF-8") "british")
  (check= (locale-to-language "en_US.UTF-8") "english")
  (check= (locale-to-language "en") "english")
  (check= (locale-to-language "de") "german")
  (check= (locale-to-language "de_AT") "german")
  (check= (locale-to-language "fr_CA.UTF-8") "french")
  (check= (locale-to-language "pt_BR") "portuguese")
  (check= (locale-to-language "zh_TW") "taiwanese")
  (check= (locale-to-language "zh_CN") "chinese")
  (check= (locale-to-language "zh_HK") "chinese")
  (check= (locale-to-language "xx_YY") "english")
  (check= (locale-to-language "") "english")
  ;; FIXME: Greek has the ISO 639 code el, not gr, and Swedish is spoken in
  ;; SE (System/Language/locale.cpp:118,150,162): (locale-to-language "el_GR")
  ;; gives "english", expected "greek"; (language-to-locale "swedish")
  ;; gives "sv_SV", expected "sv_SE".
  (check-true (string? (get-locale-language)))
  (check-true (in? (get-output-language)
                   (cons "american" supported-languages))))

;; Translations from the dictionaries langs/natural/dic/english-*.scm,
;; which keep the capitalization of the word
(define (test-translations)
  (check-group "translations")
  (let ((e-acute (string (integer->char 233)))
        (e-grave (string (integer->char 232))))
    (check= (translate-from-to "File" "english" "german") "Datei")
    (check= (translate-from-to "file" "english" "german") "Datei")
    (check= (translate-from-to "Theorem" "english" "german") "Satz")
    (check= (translate-from-to "Help" "english" "german") "Hilfe")
    (check= (translate-from-to "Theorem" "english" "french")
            (string-append "Th" e-acute "or" e-grave "me"))
    (check= (translate-from-to "theorem" "english" "french")
            (string-append "th" e-acute "or" e-grave "me"))
    (check= (translate-from-to "Chapter" "english" "italian") "Capitolo")
    (check= (translate-from-to "Table" "english" "dutch") "Tabel")
    (check= (translate-from-to "help" "english" "spanish") "ayuda")
    (check= (translate-from-to "Theorem" "english" "english") "Theorem")
    ;; a string which is not in the dictionary stays as it is
    (check= (translate-from-to "Nothing to translate xyz" "english" "german")
            "Nothing to translate xyz")
    (check= (tree->stree (tree-translate-from-to
                          '(concat "File" " " "Edit") "english" "german"))
            '(concat "Datei" " " "Bearbeiten"))
    (check= (replace "Read %1 items from %2" 3 "x") "Read 3 items from x")
    (check= (replace "Wrote %1" 'f) "Wrote f")
    (check= (url->system (tr-file "german"))
            (url->system (url-concretize
                          "$TEXMACS_PATH/langs/natural/dic/english-german.scm")))
    ;; every language but English has a dictionary
    (check= (list-filter (cdr supported-languages)
                         (lambda (l) (and (!= l "english")
                                          (not (url-exists? (tr-file l))))))
            '())))

;; The hyphenation patterns of each language (System/Language/
;; text_language.cpp), and a long word which is broken at a narrow width
(define hyphenation-files
  '(("american" "us") ("british" "ukenglish") ("bulgarian" "bulgarian")
    ("croatian" "croatian") ("czech" "czech") ("danish" "danish")
    ("dutch" "dutch") ("english" "us") ("esperanto" "esperanto")
    ("finnish" "finnish") ("french" "french") ("german" "german")
    ("greek" "greek") ("hungarian" "hungarian") ("italian" "italian")
    ("polish" "polish") ("portuguese" "portuguese")
    ("romanian" "romanian") ("russian" "russian") ("slovak" "slovak")
    ("slovene" "slovene") ("spanish" "spanish") ("swedish" "swedish")
    ("ukrainian" "ukrainian")))

(define long-words
  '(("english" "incomprehensibilities")
    ("american" "incomprehensibilities")
    ("british" "incomprehensibilities")
    ("german" "Donaudampfschifffahrtsgesellschaft")
    ("french" "anticonstitutionnellement")
    ("dutch" "arbeidsongeschiktheidsverzekering")
    ("italian" "precipitevolissimevolmente")
    ("spanish" "electroencefalografista")
    ("portuguese" "inconstitucionalissimamente")
    ("danish" "speciallaegepraksisplanlaegning")
    ("swedish" "realisationsvinstbeskattning")
    ("finnish" "lentokonesuihkuturbiinimoottori")
    ("polish" "konstantynopolitanczykowianeczka")
    ("czech" "nejneobhospodarovavatelnejsimi")
    ("hungarian" "megszentsegtelenithetetlensegeskedeseitekert")
    ("romanian" "pneumonoultramicroscopicsilicovulcanoconioza")
    ("slovak" "najneobhospodarovavatelnejsimi")
    ("slovene" "dezoksiribonukleinska")
    ("croatian" "prijestolonasljednikovica")
    ("esperanto" "malsanulejestrinoj")))

(define (test-hyphenation)
  (check-group "hyphenation patterns")
  (check= (list-filter
           hyphenation-files
           (lambda (p)
             (not (url-exists?
                   (url-append "$TEXMACS_PATH/langs/natural/hyphen"
                               (string-append "hyphen." (cadr p)))))))
          '())
  ;; the languages whose words are broken (none is left out)
  (check= (list-filter
           long-words
           (lambda (p)
             (<= (lines "1.5cm" `(with "language" ,(car p) ,(cadr p))) 1)))
          '())
  (check= (lines "15cm" '(with "language" "german"
                           "Donaudampfschifffahrtsgesellschaft"))
          1)
  ;; no patterns: the word sticks out of the line
  (check= (lines "1.5cm" '(with "language" "verbatim"
                            "incomprehensibilities"))
          1)
  (check= (lines "1.5cm" '(with "language" "chinese"
                            "incomprehensibilities"))
          1))

;; Spell checking goes through an external speller (hunspell or aspell);
;; without one the checks are skipped and reported in the log.
(define (test-spelling)
  (check-group "spell checking")
  (let ((r (single-spell-start "english")))
    (if (!= r "ok")
        (begin
          (display* "  no speller for English (" r "), checks skipped\n")
          (check-true (string? r)))
        (begin
          (check-true (spell-check? "english" "hello"))
          (check-true (spell-check? "english" "language"))
          (check-false (spell-check? "english" "helo"))
          (check-false (spell-check? "english" "lnaguage"))
          (with s (tree->stree (spell-check "english" "helo"))
            (check= (car s) 'tuple)
            (check-true (in? "hello" (cddr s))))
          (single-spell-done "english")
          (check-true (string-starts? (single-spell-start "klingon")
                                      "Error"))
          ;; the misspelled words of a buffer, as pairs of paths
          (with-buffer-doc '(document "Thsi is a tset") '("generic")
            (lambda ()
              (check= (map rel (tree-spell "english" (buffer-tree)
                                           (tree->path (buffer-tree)) 100))
                      '((0 0) (0 4) (0 10) (0 14)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Education: markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-edu-contexts)
  (check-group "education contexts")
  (check-true (problem-context? (T '(exercise (document "x")))))
  (check-true (problem-context? (T '(problem* (document "x")))))
  (check-false (problem-context? (T '(theorem (document "x")))))
  (check-true (solution-context? (T '(solution (document "x")))))
  (check-true (solution-context? (T '(answer* (document "x")))))
  (check-true (question-context? (T '(question (document "x")))))
  (check-true (question-context? (T '(question-arabic "x"))))
  (check-false (question-context? (T '(theorem (document "x")))))
  (check-true (short-question-context? (T '(question-item "x"))))
  (check-false (short-question-context? (T '(question (document "x")))))
  (check-true (short-answer-context? (T '(answer-Roman "x"))))
  (check-true (answer-context? (T '(answer-alpha "x"))))
  (check-true (question-or-answer-context? (T '(answer-alpha "x"))))
  (check-true (question-context*? (T '(document (exercise (document "x"))))))
  (check-false (question-context*? (T '(document "a" (exercise
                                                      (document "x"))))))
  (check-true (answer-context*? (T '(document (solution (document "x"))))))
  (check-true (question-answer-context?
               (T '(folded (exercise (document "x")) (solution (document "y"))))))
  (check-true (question-answer-context?
               (T '(unfolded-reverse (question-item "x") (answer-item "y")))))
  (check-false (question-answer-context? (T '(folded "a" "b"))))
  (check-true (mc-context? (T '(mcs-vertical (mc-field "false" "x")))))
  (check-true (mc-context? (T '(mc-popup (mc-field "false" "x")))))
  (check-false (mc-context? (T '(itemize (document "x")))))
  (check-true (mc-exclusive-context? (T '(mc-horizontal (mc-field "false" "x")))))
  (check-true (mc-exclusive-context? (T '(mc-popup (mc-field "false" "x")))))
  (check-false (mc-exclusive-context? (T '(mcs (mc-field "false" "x")))))
  (check-true (mc-plural-context? (T '(mcs (mc-field "false" "x")))))
  (check-true (mc-popup-context? (T '(mc-popup (mc-field "false" "x")))))
  (check-true (mc-exposed-context? (T '(mc (mc-field "false" "x")))))
  (check-false (mc-exposed-context? (T '(mc-popup (mc-field "false" "x")))))
  (check-true (mc-field-context? (T '(mc-field "false" "x"))))
  (check-true (gap-context? (T '(gap-box-wide "x"))))
  (check-true (gap-non-long-context? (T '(gap-box-wide "x"))))
  (check-true (gap-non-long-context? (T '(gap "x"))))
  (check-false (gap-non-long-context? (T '(gap-long "x"))))
  (check-true (gap-long-context? (T '(gap-underlined-long "x"))))
  (check-false (gap-long-context? (T '(gap "x"))))
  (check-true (with-button-context? (T '(with-button-Alpha "x"))))
  (check-false (with-button-context? (T '(button-Alpha "x"))))
  (check-true (mc-field-active? '(mc-field "true" "x")))
  (check-true (mc-field-active? '(mc-field (hide-simple "true" "false") "x")))
  (check-false (mc-field-active? '(mc-field "false" "x")))
  (check-false (mc-field-active? '(mc-field (hide-simple "false" "true") "x")))
  ;; the groups of edu-drd.scm
  (check= (mc-tag-list)
          '(mc mc-monospaced mc-horizontal mc-vertical mc-popup
            mcs mcs-monospaced mcs-horizontal mcs-vertical))
  (check= (gap-tag-list)
          '(gap gap-dots gap-underlined gap-box
            gap-wide gap-dots-wide gap-underlined-wide gap-box-wide
            gap-long gap-dots-long gap-underlined-long gap-box-long))
  (check= (button-tag-list)
          '(button-box button-box* button-circle button-circle*
            button-arabic button-alpha button-Alpha button-roman button-Roman))
  (check= (short-question-tag-list)
          '(question-arabic question-alpha question-Alpha
            question-roman question-Roman question-item))
  (check= (short-answer-tag-list)
          '(answer-arabic answer-alpha answer-Alpha
            answer-roman answer-Roman answer-item)))

(define (test-edu-macros)
  (check-group "education macros")
  ;; a tiling of items in columns
  (with t (ext-tiled-items (T '(tuple "a" "b" "c")) (T "2"))
    (check= (car t) 'tformat)
    (check-true (in? '(cwith "1" "-1" "1" "-1" "cell-width" "0.4999995par")
                     t))
    (check= (map (lambda (row) (map (lambda (c) (tm->stree (cadr (cadr c))))
                                    (cdr row)))
                 (cdr (cAr t)))
            '(("a" "b") ("c" ""))))
  ;; five columns by default
  (with t (ext-tiled-items (T '(tuple "a")) (T "x"))
    (check= (length (cdr (cadr (cAr t)))) 5)
    (check-true (in? '(cwith "1" "-1" "1" "-1" "cell-width" "0.1999998par")
                     t)))
  (with t (ext-vertical-items (T '(tuple "a" "b")) (T "1ln") (T "0.5ln"))
    (check= (car t) 'tformat)
    (check= (length (cdr (cAr t))) 2)
    (check= (tm->stree (caddr (assoc-ref-list t "table-lborder"))) "1ln"))
  ;; the popup shows the selected field, else the first one
  (with t (ext-mc-popup (T '(mc-popup (mc-field "false" "x")
                                      (mc-field "true" "y"))))
    (check= (car t) 'button-popup)
    (check= (tm->stree (cadr t)) '(mc-selected-field "y"))
    (check= (cdddr (cdr t)) '("left" "Bottom" "default")))
  (with t (ext-mc-popup (T '(mc-popup (mc-field "false" "x"))))
    (check= (tm->stree (cadr t)) '(mc-selected-none "x")))
  (with t (ext-mc-popup (T '(mc-popup)))
    (check= (cadr t) '(mc-selected-none "---")))
  (check= (customizable-parameters (T '(mc-popup)))
          '(("button-popup-activate" "Activate")))
  (check= (parameter-choice-list "button-popup-activate")
          '("click" "mouse-over" "focus")))

(define (assoc-ref-list t var)
  ;; the twith of @var in the tformat @t
  (list-find (cdr t) (lambda (x) (and (pair? x) (== (car x) 'twith)
                                      (== (cadr x) var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Education: questions and answers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (operated doc mode)
  ;; the body of a buffer with @doc after (edu-operate (buffer-tree) mode)
  (with r #f
    (with-buffer-doc doc '("generic")
      (lambda ()
        (edit (edu-operate (buffer-tree) mode))
        (set! r (body))))
    r))

(define qa '(document (exercise (document "Q")) (solution (document "A"))))

;; Question mode folds the answers, answer mode folds the questions and
;; mixed mode shows both; the gaps and fields hide their answers in question
;; mode and show them otherwise.
(define (test-edu-modes)
  (check-group "questions and answers")
  (check= (operated qa :question)
          '(document (folded (exercise (document "Q"))
                             (solution (document "A")))))
  (check= (operated qa :answer)
          '(document (folded-reverse (exercise (document "Q"))
                                     (solution (document "A")))))
  (check= (operated qa :mixed)
          '(document (unfolded (exercise (document "Q"))
                               (solution (document "A")))))
  (check= (operated '(document (question (document "Q"))
                               (answer (document "A")) "x")
                    :question)
          '(document (folded (question (document "Q"))
                             (answer (document "A")))
                     "x"))
  ;; a question without an answer stays
  (check= (operated '(document (exercise (document "Q")) "x") :question)
          '(document (exercise (document "Q")) "x"))
  ;; existing foldings change
  (check= (operated '(document (folded (exercise (document "Q"))
                                       (solution (document "A"))))
                    :answer)
          '(document (folded-reverse (exercise (document "Q"))
                                     (solution (document "A")))))
  (check= (operated '(document (unfolded (exercise (document "Q"))
                                         (solution (document "A"))))
                    :question)
          '(document (folded (exercise (document "Q"))
                             (solution (document "A")))))
  (check= (operated '(document (folded (exercise (document "Q"))
                                       (solution (document "A"))))
                    :mixed)
          '(document (unfolded (exercise (document "Q"))
                               (solution (document "A")))))
  ;; gaps
  (check= (operated '(document (concat "a " (gap "b") " c")) :question)
          '(document (concat "a " (gap (hide-reply "" "b")) " c")))
  (check= (operated '(document (concat "a " (gap (hide-reply "" "b")) " c"))
                    :answer)
          '(document (concat "a " (gap "b") " c")))
  (check= (operated '(document (concat "a " (gap-dots "b") " c")) :mixed)
          '(document (concat "a " (gap-dots "b") " c")))
  (check= (operated '(document (gap-long (document "a" "b"))) :question)
          '(document (gap-long (hide-simple (document "" "")
                                            (document "a" "b")))))
  (check= (operated '(document (gap-long (hide-simple (document "" "")
                                                      (document "a" "b"))))
                    :mixed)
          '(document (gap-long (document "a" "b"))))
  ;; multiple choice fields
  (check= (operated '(document (mc (mc-field "true" "x")
                                   (mc-field "false" "y")))
                    :question)
          '(document (mc (mc-field (hide-simple "false" "true") "x")
                         (mc-field (hide-simple "false" "false") "y"))))
  (check= (operated '(document (mc (mc-field (hide-simple "false" "true") "x")
                                   (mc-field (hide-simple "false" "false")
                                             "y")))
                    :answer)
          '(document (mc (mc-field "true" "x") (mc-field "false" "y"))))
  (with-buffer-doc qa '("generic")
    (lambda ()
      (edit (edu-set-mode :question))
      (check= (tree-label (bt 0)) 'folded)
      (edit (edu-set-mode :mixed))
      (check= (tree-label (bt 0)) 'unfolded)))
  (with-buffer-doc '(document (exercise (document "Q"))) '("generic")
    (lambda ()
      (check-true (unanswered-question-context? (bt 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Education: multiple choice lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define abc '(document (mc (mc-field "false" "a") (mc-field "true" "b")
                           (mc-field "false" "c"))))

(define (test-mc)
  (check-group "multiple choice")
  (with-buffer-doc '(document "") '("generic")
    (lambda ()
      ;; make on an mc tag makes a list with one field
      (edit (make 'mc))
      (check= (body) '(document (mc (mc-field "false" ""))))
      (check= (rel (cursor-path)) '(0 0 1 0))
      (edit (insert "one"))
      (check= (body) '(document (mc (mc-field "false" "one"))))))
  (with-buffer-doc abc '("generic")
    (lambda ()
      (edit (tree-go-to (buffer-tree) 0 0 1 0))
      (check= (mc-get-button-theme) #f)
      (check= (mc-get-pretty-button-theme) "Default")
      ;; switching selects one field only
      (edit (mc-switch (bt 0) :last))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "b")
                                    (mc-field "true" "c"))))
      (edit (mc-switch (bt 0) :first))
      (check= (body) '(document (mc (mc-field "true" "a")
                                    (mc-field "false" "b")
                                    (mc-field "false" "c"))))
      (edit (mc-switch (bt 0) 1))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "true" "b")
                                    (mc-field "false" "c"))))
      (check= (rel (cursor-path)) '(0 1 1 1))
      ;; button themes
      (edit (mc-set-button-theme 'with-button-Alpha))
      (check= (tree-label (bt 0)) 'with-button-Alpha)
      (check= (mc-get-pretty-button-theme) "A, B, C")
      (edit (mc-set-button-theme 'with-button-circle))
      (check= (tree-label (bt 0)) 'with-button-circle)
      (check= (mc-get-pretty-button-theme) "Plain circles")
      (edit (mc-set-button-theme #f))
      (check= (tree-label (bt 0)) 'mc)
      (check= (mc-get-pretty-button-theme) "Default")
      ;; several answers, then a single one again (which clears them)
      (edit (mc-select #t))
      (check= (tree-label (bt 0)) 'mcs)
      (edit (mc-select #f))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "b")
                                    (mc-field "false" "c"))))))
  ;; inserting and removing fields
  (with-buffer-doc '(document (mc (mc-field "false" "a")
                                  (mc-field "false" "b")))
      '("generic")
    (lambda ()
      (edit (tree-go-to (buffer-tree) 0 0 1 0))
      (edit (structured-insert-horizontal (bt 0) #t))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "")
                                    (mc-field "false" "b"))))
      (check= (rel (cursor-path)) '(0 1 1 0))
      (edit (structured-insert-horizontal (bt 0) #f))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "")
                                    (mc-field "false" "")
                                    (mc-field "false" "b"))))
      (edit (kbd-backspace))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "")
                                    (mc-field "false" "b"))))
      ;; a structured removal backwards removes the previous field
      (edit (tree-go-to (buffer-tree) 0 2 1 1))
      (edit (structured-remove-horizontal (bt 0) #f))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "b"))))
      (edit (tree-go-to (buffer-tree) 0 0 1 0))
      (edit (structured-remove-horizontal (bt 0) #t))
      (check= (body) '(document (mc (mc-field "false" "b"))))))
  ;; deleting forwards in the last field removes it, the cursor goes to the
  ;; previous one
  (with-buffer-doc '(document (mc (mc-field "false" "a")
                                  (mc-field "false" "b")))
      '("generic")
    (lambda ()
      (edit (tree-go-to (buffer-tree) 0 1 1 0))
      (edit (structured-remove-horizontal (bt 0) #t))
      (check= (body) '(document (mc (mc-field "false" "a"))))
      (check= (list-head (rel (cursor-path)) 2) '(0 0))))
  (with-buffer-doc '(document (mc (mc-field "false" "a")
                                  (mc-field "false" "")))
      '("generic")
    (lambda ()
      (edit (tree-go-to (buffer-tree) 0 1 1 0))
      (edit (kbd-delete))
      (check= (body) '(document (mc (mc-field "false" "a"))))
      (check= (list-head (rel (cursor-path)) 2) '(0 0))))
  ;; vertical insertion and removal, downwards and upwards
  (with-buffer-doc '(document (mc (mc-field "false" "a")
                                  (mc-field "false" "b")))
      '("generic")
    (lambda ()
      (edit (tree-go-to (buffer-tree) 0 0 1 0))
      (edit (structured-insert-vertical (bt 0) #t))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "")
                                    (mc-field "false" "b"))))
      (check= (rel (cursor-path)) '(0 1 1 0))
      (edit (structured-insert-vertical (bt 0) #f))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "")
                                    (mc-field "false" "")
                                    (mc-field "false" "b"))))
      (check= (rel (cursor-path)) '(0 1 1 0))
      (edit (structured-remove-vertical (bt 0) #t))
      (check= (body) '(document (mc (mc-field "false" "a")
                                    (mc-field "false" "")
                                    (mc-field "false" "b"))))
      (check= (list-head (rel (cursor-path)) 2) '(0 1))
      (edit (structured-remove-vertical (bt 0) #f))
      (check= (body) '(document (mc (mc-field "false" "")
                                    (mc-field "false" "b"))))))
  ;; popups: one field is selected, the new one
  (with-buffer-doc '(document (mc-popup (mc-field "true" "a")
                                        (mc-field "false" "b")))
      '("generic")
    (lambda ()
      (edit (tree-go-to (buffer-tree) 0 0 1 1))
      (edit (structured-insert-horizontal (bt 0) #t))
      (check= (body) '(document (mc-popup (mc-field "false" "a")
                                          (mc-field "true" "")
                                          (mc-field "false" "b"))))
      (edit (kbd-incremental (bt 0) #t))
      (check= (body) '(document (mc-popup (mc-field "false" "a")
                                          (mc-field "false" "")
                                          (mc-field "true" "b"))))
      (edit (kbd-extremal (bt 0) #f))
      (check= (body) '(document (mc-popup (mc-field "true" "a")
                                          (mc-field "false" "")
                                          (mc-field "false" "b")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools: comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (comment lab id type by body)
  `(,lab ,id ,(string-append "m" id) ,type ,by "1" "" ,body))

(define (test-comments)
  (check-group "comments")
  (let ((c (T (comment 'folded-comment "2" "remark" "Bob" "c2"))))
    (check-true (comment-context? (T (comment 'unfolded-comment "1" "comment"
                                              "Ann" "x"))))
    (check-false (comment-context? (T (comment 'hidden-unfolded-comment "1"
                                               "comment" "Ann" "x"))))
    (check-true (any-comment-context? (T (comment 'hidden-unfolded-comment
                                                  "1" "comment" "Ann" "x"))))
    (check-true (any-comment-context? (T (comment 'mirror-comment "1"
                                                  "comment" "Ann" "x"))))
    (check-false (any-comment-context? (T '(folded-comment "x"))))
    (check-true (folded-comment-context? c))
    (check-false (folded-comment-context?
                  (T (comment 'unfolded-comment "1" "comment" "Ann" "x"))))
    ;; the arguments: unique id, mirror id, type, author, time, src, body
    (check= (comment-id c) "m2")
    (check= (comment-type c) "remark")
    (check= (comment-by c) "Bob")
    (check= (comment-type (T '(strong "x"))) "?")
    (check= (comment-by (T '(strong "x"))) "?")
    (check= (comment-id (T '(strong "x"))) #f)
    (check-true (hidden-child? c 2))
    (check-true (hidden-child? c 3))
    (check-false (hidden-child? c 6))
    (check= (ext-contains-shown-comments? (T `(document ,(tree->stree c))))
            "true")
    (check= (ext-contains-shown-comments?
             (T `(document ,(comment 'hidden-folded-comment "3" "remark"
                                     "Bob" "x"))))
            "false")
    (check= (ext-abbreviate-name (T "Joris van der Hoeven")) "Joris")
    (check= (ext-abbreviate-name (T "Joris")) "Joris")
    (check= (ext-abbreviate-name "not a tree") "not a tree"))
  ;; colors, which can be changed in the preferences
  (check= (get-comment-color "reminder" "Somebody Else") "#844")
  (check= (get-comment-color "comment" "Somebody Else") "#727")
  (check-true (default-comment-color? "comment" "Somebody Else"))
  (check= (ext-comment-color (T "reminder") (T "Somebody Else")) "#844")
  ;; the comments of a buffer
  (with-buffer-doc `(document ,(comment 'unfolded-comment "1" "comment"
                                        "Ann" "c1")
                              ,(comment 'folded-comment "2" "remark"
                                        "Bob" "c2")
                              (concat "a" ,(comment 'unfolded-comment "3"
                                                    "comment" "Bob" "c3")))
      '("generic")
    (lambda ()
      (reset-comment-cache)
      (check= (length (search-comments (buffer-tree))) 3)
      (check= (comment-type-list :all) '("comment" "remark"))
      (check= (comment-by-list :all) '("Ann" "Bob"))
      (check= (comment-type-list :hide) '())
      (check-true (comment-test-type? "remark"))
      (check-true (pair? (comments-in-buffer)))
      (edit (operate-on-comments :fold))
      (check= (map tree-label (list (bt 0) (bt 1) (bt 2 1)))
              '(folded-comment folded-comment folded-comment))
      (reset-comment-cache)
      ;; hiding a type of comments
      (edit (comment-toggle-type "remark"))
      (check= (map tree-label (list (bt 0) (bt 1) (bt 2 1)))
              '(folded-comment hidden-folded-comment folded-comment))
      (reset-comment-cache)
      (check= (comment-type-list :hide) '("remark"))
      (check= (comment-type-list :show) '("comment"))
      (check-false (comment-test-type? "remark"))
      (check= (length (search-comments (buffer-tree))) 2)
      (edit (operate-on-comments :unfold))
      (check= (map tree-label (list (bt 0) (bt 1) (bt 2 1)))
              '(unfolded-comment hidden-folded-comment unfolded-comment))
      (reset-comment-cache)
      (edit (comment-toggle-type "remark"))
      (check= (tree-label (bt 1)) 'folded-comment)
      (reset-comment-cache)
      ;; hiding the comments of an author
      (edit (comment-toggle-by "Bob"))
      (check= (map tree-label (list (bt 0) (bt 1) (bt 2 1)))
              '(unfolded-comment hidden-folded-comment
                                 hidden-unfolded-comment))
      (reset-comment-cache)
      (check= (comment-by-list :show) '("Ann"))
      (check-false (comment-test-by? "Bob"))
      (check-true (comment-test-by? "Ann"))
      (edit (operate-on-comments :show))
      (reset-comment-cache)
      (check= (map tree-label (list (bt 0) (bt 1) (bt 2 1)))
              '(unfolded-comment hidden-folded-comment
                                 hidden-unfolded-comment))
      (edit (operate-on-comments :cut))
      (reset-comment-cache)
      (check= (tree->stree (bt 0)) "")
      (check= (length (tree-search (buffer-tree) any-comment-context?)) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools: posters, themes, spelling, AI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define themes
  '("blackboard" "bluish" "boring-white" "dark-vador" "granite"
    "ice" "manila-paper" "metal" "pale-blue" "parchment"
    "pine" "reddish" "ridged-paper" "rough-paper" "xperiment"))

;; the files of the package of the theme @th, in a subdirectory of themes
;; FIXME: completing a url-any followed by a plain file name aborts TeXmacs
;; (System/Classes/url.cpp:909, complete, "invalid base url"):
;; (url-complete (url-append (url-append "$TEXMACS_PATH/packages/themes"
;; (url-any)) "pine.ts") "fr") throws a C++ exception which is not caught,
;; expected the url of themes/pine/pine.ts; a wildcard works.
(define (theme-files th)
  (url->list
   (url-expand
    (url-complete
     (url-append (url-append "$TEXMACS_PATH/packages/themes" (url-any))
                 (url-wildcard (string-append th ".ts")))
     "fr"))))

(define (test-posters-themes)
  (check-group "posters and themes")
  (check= (poster-themes) themes)
  (check= (basic-themes) themes)
  (check= (poster-title-styles)
          '("plain-poster-title" "framed-poster-title"
            "topless-poster-title"))
  ;; every theme has a package
  (check= (list-filter themes
                       (lambda (th)
                         (null? (theme-files th))))
          '())
  (check= (style-category "plain-poster-title") :poster-title-style)
  (check-true (style-includes? "poster" "boring-white"))
  (check-true (style-includes? "poster" "framed-poster-title"))
  (check-true (style-category-precedes? :basic-theme :theorem-decorations))
  (check-true (poster-block-context? (T '(framed-block (document "x")))))
  (check-true (titled-block-context? (T '(framed-titled-block "t"
                                           (document "x")))))
  (check-true (untitled-block-context? (T '(alternate-block (document "x")))))
  (check-false (titled-block-context? (T '(alternate-block (document "x")))))
  (check= (parameter-choice-list "framed-shape")
          (parameter-choice-list "ornament-shape"))
  ;; themes of a document
  (with-buffer-doc '(document "x") '("generic")
    (lambda ()
      (check= (current-basic-theme) "plain")
      (check-true (default-basic-theme?))
      (edit (add-style-package "pine"))
      (check= (get-style-list) '("generic" "pine"))
      (check= (current-basic-theme) "pine")
      (check-false (default-basic-theme?))
      (check= (style-category "pine") :basic-theme)
      (edit (select-default-basic-theme))
      (check= (get-style-list) '("generic"))
      (check= (current-basic-theme) "plain")))
  (with-buffer-doc '(document "x") '("poster")
    (lambda ()
      (check= (current-poster-theme) "boring-white")
      (check= (current-poster-title-style) "framed-poster-title")
      (edit (add-style-package "reddish"))
      (check= (current-poster-theme) "reddish")
      (edit (add-style-package "topless-poster-title"))
      (check= (current-poster-title-style) "topless-poster-title")))
  ;; blocks
  (with-buffer-doc '(document (framed-block (document "x"))
                              (plain-titled-block "T" (document "y")))
      '("poster")
    (lambda ()
      (edit (block-toggle-titled (bt 0)))
      (check= (tree->stree (bt 0)) '(framed-titled-block "" (document "x")))
      (check= (rel (cursor-path)) '(0 0 0))
      (edit (block-toggle-titled (bt 1)))
      (check= (tree->stree (bt 1)) '(plain-block (document "y")))
      (check-false (block-wide? (bt 0)))
      (edit (block-toggle-wide (bt 0)))
      (check= (tree->stree (bt 0))
              '(with "par-columns" "1"
                 (framed-titled-block "" (document "x"))))
      (check-true (block-wide? (bt 0 2)))
      (edit (block-toggle-wide (bt 0 2)))
      (check= (tree->stree (bt 0)) '(framed-titled-block "" (document "x")))
      (edit (insert-same-block (bt 1) #t))
      (check= (tree->stree (bt 2)) '(plain-block (document "")))
      (edit (insert-same-block (bt 0) #f))
      (check= (tree->stree (bt 0)) '(framed-titled-block "" (document "")))
      (check= (tree-arity (buffer-tree)) 4))))

(define (test-spell-edit)
  (check-group "spell markup")
  (check-true (spell-context? (T '(spell-error "teh" "the"))))
  (check-false (spell-context? (T '(strong "x"))))
  ;; the end of a spell check removes the marks of the errors
  (with-buffer-doc '(document (concat "a " (spell-error "teh" "the") " b")
                              (spell-error "x" "y"))
      '("generic")
    (lambda ()
      (check-false (inside-spell?))
      (edit (spell-initiate))
      (check= (body) '(document (concat "a teh b") "x"))))
  (with-buffer-doc '(document (with "language" "french" (document "x")))
      '("generic")
    (lambda ()
      (check= (tree-get-env (bt 0 2 0) "language") "french")
      (check= (tree-get-env (T "detached") "language")
              (get-init "language")))))

(define (test-ai-serialize)
  (check-group "AI serialization")
  (check= (ai-serialize "english" "hello") "hello")
  (check= (ai-serialize "english" '(document "hello")) "hello")
  (check= (ai-serialize "english" '(strong "x")) "{\\tmstrong{x}}")
  (check= (ai-serialize "english" '(with "mode" "math" (frac "a" "b")))
          "$\\frac{a}{b}$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (misc-modules-test-failures)
  (:synopsis "Run the tests of the doc, fonts, language, education and tools modules")
  (check-suite "misc-modules")
  (clear-misc-dir)
  (system-mkdir (system->url misc-dir))
  (for-each run-group
            (list test-help-files test-doc-parse test-tmdoc-expand
                  test-tmdoc-rewrite test-doc-search test-tmdoc-edit
                  test-font-database test-logical-fonts
                  test-font-typesetting test-init-font
                  test-language-names test-translations test-hyphenation
                  test-spelling
                  test-edu-contexts test-edu-macros test-edu-modes test-mc
                  test-comments test-posters-themes test-spell-edit
                  test-ai-serialize))
  (clear-misc-dir)
  (check-end))
