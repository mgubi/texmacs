
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : zotero-widgets.scm
;; DESCRIPTION : searching the Zotero library for citations
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: (bibtex zotero-db) is only loaded when the database is used (it
;; loads the modules of the database), through the lazy definitions
(texmacs-module (bibtex zotero-widgets)
  (:use (bibtex zotero)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (citation-around)
  ;; The citation tag which contains the cursor, if any
  (tree-innermost '(cite nocite cite-raw cite-raw* cite-textual
                    cite-textual* cite-parenthesized cite-parenthesized*)))

(tm-define (zotero-insert-citation keys)
  (:synopsis "Cite the items with the citation @keys")
  ;; In a citation, the keys are added to it; otherwise a new one is made
  (when (nnull? keys)
    (with t (citation-around)
      (if t
          (begin
            ;; an empty key (of a new citation) is replaced
            (when (and (== (tree-arity t) 1)
                       (== (tree->stree (tree-ref t 0)) ""))
              (tree-remove! t 0 1))
            (tree-insert! t (tree-arity t) keys)
            (tree-go-to t :end))
          (insert `(cite ,@keys))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The search dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The dialog searches Zotero and the other sources of the document (see
;; zotero-search-sources): each line is marked with its sources

(define search-query "")
(define search-results '())
(define search-selected '())
(define search-message "")
(define search-file #f)

(define (shorten s n)
  (if (<= (string-length s) n) s
      (string-append (substring s 0 (- n 3)) "...")))

(tm-define (zotero-entry-label e)
  ;; One line: creators (year): title, then the citation key
  ;; NOTE: the key makes the labels of different items different
  (string-append (zotero-entry-creators e)
                 (if (== (zotero-entry-year e) "") ""
                     (string-append " (" (zotero-entry-year e) ")"))
                 ": " (shorten (zotero-entry-title e) 60)
                 "  [" (zotero-entry-key e) "]"))

(tm-define (zotero-source-name mark)
  (cond ((== mark "L") "the document")
        ((== mark "F") (if search-file (url->system (url-tail search-file))
                           "the BibTeX file"))
        ((== mark "D") "the database")
        (else "Zotero")))

(tm-define (zotero-line-label l)
  ;; [sources] creators (year): title  [key], with (!) for a collision
  (let* ((sum (zotero-line-summary l))
         (e (zotero-line-entry l))
         (lib (and e (!= (zotero-entry-library e) (zotero-user-library))
                   (zotero-library-name (zotero-entry-library e)))))
    (string-append (if (zotero-line-collision? l) "(!) " "")
                   "[" (string-recompose (zotero-line-marks l) " ")
                   (if lib (string-append ": " lib) "") "] "
                   (third sum)
                   (if (== (fourth sum) "") ""
                       (string-append " (" (fourth sum) ")"))
                   ": " (shorten (second sum) 60)
                   "  [" (first sum) "]")))

(define (search-labels)
  (map zotero-line-label search-results))

(define (selected-lines)
  (list-filter search-results
               (lambda (l) (in? (zotero-line-label l) search-selected))))

(define (selected-keys)
  (list-remove-duplicates (map zotero-line-key (selected-lines))))

(define (selected-zotero-entries . opt-new)
  ;; the Zotero items of the selected lines (for Show in Zotero), or only
  ;; those which the database does not have yet (for an import)
  (with new? (and (nnull? opt-new) (car opt-new))
    (list-filter (map zotero-line-entry
                      (list-filter (selected-lines)
                                   (lambda (l)
                                     (not (and new?
                                               (in? "D" (zotero-line-marks
                                                         l)))))))
                 identity)))

(define (count-message n)
  (cond ((== n 0) "Nothing found")
        ((== n 1) "1 reference")
        (else (string-append (number->string n) " references"))))

(define (search-now q)
  (set! search-query q)
  (set! search-selected '())
  (set! search-file (zotero-own-bib-file))
  (let* ((empty? (== (tm-string-trim-both q) ""))
         (st (zotero-status)))
    (set! search-results
          (if empty? '() (zotero-combine (zotero-search-sources q))))
    (set! search-message
          (cond (empty? "Type authors, words of the title or a year")
                (else
                  (string-append
                   (count-message (length search-results))
                   (if (== st 'ready) ""
                       (string-append " (" (zotero-status-message st) ")"))
                   (with c (zotero-collision-message search-results
                                                     zotero-source-name)
                     (if c (string-append ". " c) "")))))))
  (refresh-now "zotero-results"))

(define (search-sources-title)
  ;; the sources which the dialog searches
  (string-append "Search references ("
                 (string-recompose
                  (append (list "Zotero")
                          (if search-file
                              (list (url->system (url-tail search-file))) '())
                          (if (supports-db?) (list "database") '()))
                  ", ")
                 ")"))

(define (show-selected)
  (with l (selected-zotero-entries)
    (if (null? l)
        (set-message "Select a reference from Zotero" "Zotero")
        (zotero-show-item (car l)))))

(tm-widget ((zotero-search-widget) cmd)
  (padded
    (hlist
      (text "Search:") // //
      (input (when answer (search-now answer))
             "string" (list search-query) "40em"))
    ===
    (refreshable "zotero-results"
      (hlist (text search-message) >>)
      ===
      (resize "650px" "300px"
        (scrollable
          (choices (set! search-selected answer)
                   (search-labels) search-selected))))
    ===
    (hlist
      (text "L: the document, F: the BibTeX file, D: the database, Z: Zotero")
      >>)
    ===
    (bottom-buttons
      ("Show in Zotero" (show-selected)) >>
      ("Cancel" (cmd '())) // //
      (assuming (supports-db?)
        ("Import into database"
         (cmd (list :import (selected-zotero-entries #t))))
        // //)
      ("Cite" (cmd (selected-keys))))))

(define (search-done r)
  (if (and (pair? r) (== (car r) :import))
      (with n (zotero-import-items (cadr r))
        (set-message (string-append "Imported " (number->string n)
                                    " references into the database")
                     "Zotero"))
      (zotero-insert-citation r)))

(tm-define (open-zotero-search)
  (:synopsis "Search Zotero and the sources of the document for citations")
  (:interactive #t)
  (search-now "")
  (dialogue-window (zotero-search-widget) search-done (search-sources-title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keeping the database in sync, entries changed on both sides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (field-text v)
  ;; The value @v of a field, as one line of text (in cork)
  (cond ((not v) "(none)")
        ((string? v) v)
        (else (with s (convert v "texmacs-stree" "verbatim-snippet")
                (string-replace (if (string? s) (utf8->cork s) "?")
                                "\n" " ")))))

(define conflict-choices '())

(tm-widget ((zotero-conflict-widget name rows) cmd)
  (padded
    (bold (text (string-append name ": changed in TeXmacs and in Zotero")))
    ===
    (text "Choose the value to keep for each field")
    ===
    (resize "650px" "320px"
      (scrollable
        ;; NOTE: no for inside aligned
        (vlist
          (for (row rows)
            (hlist (bold (text (car row))) >>)
            (hlist // (text "TeXmacs: ") (text (field-text (cadr row))) >>)
            (hlist // (text "Zotero: ") (text (field-text (caddr row))) >>)
            (hlist
              // (text "Keep: ") //
              (enum (set! conflict-choices
                          (assoc-set! conflict-choices (car row)
                                      (if (== answer "Zotero")
                                          'zotero 'texmacs)))
                    '("TeXmacs" "Zotero")
                    (if (== (list-ref row 4) 'zotero) "Zotero" "TeXmacs")
                    "8em")
              >>)
            ===))))
    ===
    (bottom-buttons >>
      ("Later" (cmd #f)) // //
      ("Save" (cmd #t)))))

(define (resolve-conflicts l)
  ;; One dialog per entry changed on both sides
  (when (nnull? l)
    (let* ((old (car (car l)))
           (new (cdr (car l)))
           (rows (zotero-conflict-fields old new)))
      (set! conflict-choices
            (map (lambda (row) (cons (car row) (list-ref row 4))) rows))
      (dialogue-window
       (zotero-conflict-widget (list-ref old 3) rows)
       (lambda (save?)
         (when save?
           (zotero-merge-entries old new conflict-choices))
         (resolve-conflicts (cdr l)))
       "Reference changed on both sides"))))

(tm-define (zotero-synchronize)
  (:synopsis "Update the references of the database which come from Zotero")
  (:interactive #t)
  (zotero-forget-state)
  (if (not (zotero-ready?))
      (set-message (zotero-status-message (zotero-status)) "Zotero")
      (with r (zotero-sync-database #t)
        (set-message (or (zotero-sync-message r)
                         "The references from Zotero are up to date")
                     "Zotero")
        (resolve-conflicts (cadddr r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking the citations against Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keys-text l)
  (string-recompose l ", "))

(tm-define (zotero-check-lines r)
  ;; The lines of the report @r of zotero-check-document
  (let* ((get (lambda (cat) (or (assoc-ref r cat) '())))
         (count (lambda (n what)
                  (string-append (number->string n) " " what))))
    (append
     (list (count (length (get 'zotero)) "citations found in Zotero")
           (count (length (get 'elsewhere))
                  "citations found in the other sources"))
     (map (lambda (p) (string-append (car p) " is now " (cdr p)
                                     " in Zotero"))
          (get 'renamed))
     (if (null? (get 'deleted)) '()
         (list (string-append "No longer in Zotero (the exported copy is "
                              "kept): " (keys-text (get 'deleted)))))
     (if (null? (get 'missing)) '()
         (list (string-append "Not found: " (keys-text (get 'missing)))))
     (map (lambda (k) (string-append k " is a different work in Zotero "
                                     "and in the other source, which wins"))
          (get 'collisions))
     (if (null? (get 'copies)) '()
         (list (string-append "Copies of Zotero items in the database, "
                              "not kept in sync: "
                              (keys-text (get 'copies))))))))

(tm-widget ((zotero-check-widget r) cmd)
  (padded
    (resize "600px" "250px"
      (scrollable
        (vlist
          (for (l (zotero-check-lines r))
            (hlist (text l) >>)))))
    ===
    (bottom-buttons >>
      ("Close" (cmd #f))
      (assuming (nnull? (assoc-ref r 'renamed))
        // // ("Update the citations" (cmd 'rename)))
      (assuming (nnull? (assoc-ref r 'copies))
        // // ("Keep the copies in sync" (cmd 'adopt))))))

(tm-define (open-zotero-check)
  (:synopsis "Check the citation keys of the document against Zotero")
  (:interactive #t)
  (zotero-forget-state)
  (if (not (zotero-ready?))
      (set-message (zotero-status-message (zotero-status)) "Zotero")
      (with r (zotero-check-document)
        (dialogue-window
         (zotero-check-widget r)
         (lambda (what)
           (cond ((== what 'rename) (zotero-update-citations))
                 ((== what 'adopt)
                  (with conflicts (zotero-adopt-entries (assoc-ref r 'copies))
                    (set-message
                     (string-append "The copies are kept in sync with Zotero"
                                    (if (null? conflicts) ""
                                        "; choose the fields of those which differ"))
                     "Zotero")
                    (resolve-conflicts conflicts)))))
         "Check against Zotero"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define settings-status "")

(define (settings-test)
  (zotero-forget-state)
  (zotero-forget-keys)
  (set! settings-status (zotero-status-message (zotero-status)))
  (refresh-now "zotero-settings-status"))

(define (set-zotero-preference which val)
  (set-preference which val)
  (zotero-forget-state)
  (zotero-forget-keys))

(tm-widget ((zotero-settings-widget) cmd)
  (padded
    (aligned
      (item (text "Zotero server:")
        (input (when answer (set-zotero-preference "zotero server" answer))
               "string" (list (get-preference "zotero server")) "20em"))
      (item (text "Libraries:")
        (enum (set-zotero-preference "zotero libraries"
                                     (if (== answer "My Library and groups")
                                         "all" "user"))
              '("My Library" "My Library and groups")
              (if (== (get-preference "zotero libraries") "all")
                  "My Library and groups" "My Library")
              "20em"))
      (item (text "Export format:")
        (enum (set-zotero-preference "zotero export format" answer)
              '("bibtex" "biblatex")
              (get-preference "zotero export format") "20em"))
      (item (text "Complete keys from Zotero:")
        (toggle (set-preference "zotero completion" (if answer "on" "off"))
                (== (get-preference "zotero completion") "on"))))
    ===
    (refreshable "zotero-settings-status"
      (hlist (text settings-status) >>))
    ===
    (bottom-buttons >>
      ("Test the connection" (settings-test)) // //
      ("Close" (cmd #f)))))

(tm-define (open-zotero-settings)
  (:synopsis "The settings of the citations from Zotero")
  (:interactive #t)
  (set! settings-status
        (string-append "Zotero (local API) at " (get-preference "zotero server")))
  (dialogue-window (zotero-settings-widget) noop "Zotero settings"))
