
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : zotero-db.scm
;; DESCRIPTION : Zotero as a source of the bibliographic database
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With the database tool, Zotero is one of the sources of the references
;; of a bibliography (bib-retrieve-entries, in database/bib-manage.scm),
;; after the database of the user, which wins (see doc/zotero-design.md).
;; The search window of references (open-db-chooser) lists the references
;; of Zotero after those of the database, or, without the database tool,
;; after those of the BibTeX file of the bibliography.
;;
;; The entries which come from Zotero carry the meta attributes
;;   zotero-item     the key of the Zotero item
;;   zotero-library  the library (users/0, or groups/<id>)
;;   zotero-version  the version of the item when it was last exported
;;   zotero-synced   the fields as they were then (to tell which side
;;                   changed a field)
;;   zotero-key      the citation key in Zotero, when it was renamed there
;;   zotero-deleted  "yes" when the item is no longer in Zotero
;; and the contributor "Zotero". When they enter the database (by "auto bib
;; import", zotero-import-items, or the adoption of a copy made by hand),
;; they are kept in sync with Zotero, from Zotero to TeXmacs only:
;; zotero-sync-database.

(texmacs-module (bibtex zotero-db)
  (:use (bibtex zotero)
        (database db-base)
        (database db-convert)
        (database bib-db)
        (database bib-manage)
        (database db-widgets)))

(tm-define (zotero-in-database? key)
  (:synopsis "Does the database of the user have the reference @key?")
  (with-database (bib-database)
    (nnull? (db-search (list (list "name" key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entries from Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An entry is (db-entry id type name (document meta...) (document fields...))

(define (meta-set e key val)
  ;; The entry @e with the meta attribute @key set to @val
  (let* ((meta (cdr (list-ref e 4))))
    (list (car e) (cadr e) (caddr e) (cadddr e)
          `(document ,@(list-filter meta
                                    (lambda (f)
                                      (not (and (tm-func? f 'db-field 2)
                                                (== (cadr f) key)))))
                     (db-field ,key ,val))
          (list-ref e 5))))

(define (meta-set* e l)
  ;; The entry @e with the meta attributes of the pairs @l set
  (if (null? l) e
      (meta-set* (meta-set e (caar l) (cdar l)) (cdr l))))

(tm-define (zotero-entry-meta e key)
  (:synopsis "The meta attribute @key of the database entry @e, or #f")
  (with f (list-find (cdr (list-ref e 4))
                     (lambda (f) (and (tm-func? f 'db-field 2)
                                      (== (cadr f) key))))
    (and f (caddr f))))

(define (entry-fields e) (cdr (list-ref e 5)))
(define (entry-name e) (list-ref e 3))

(define (fields->string l)
  (object->string `(document ,@l)))

(define (string->fields s)
  (with t (and (string? s) (!= s "") (string->object s))
    (if (tm-func? t 'document) (cdr t) '())))

(define (mark e z)
  ;; The database entry @e of the Zotero entry @z, marked as such
  (meta-set* e
        (list (cons "contributor" "Zotero")
              (cons "modus" "imported")
              (cons "zotero-item" (zotero-entry-item z))
              (cons "zotero-library" (zotero-entry-library z))
              (cons "zotero-version"
                    (number->string (zotero-entry-version z)))
              (cons "zotero-synced" (fields->string (entry-fields e))))))

(define (convert-now zs)
  ;; The database entries of the Zotero entries @zs, as (key . entry)
  (if (null? zs) '()
      (let* ((bib (zotero-export zs))
             (t (tm->stree (zealous-bib-import bib)))
             (es (if (tm-func? t 'document)
                     (list-filter (cdr t) db-entry-any?) '())))
        (list-filter
         (map (lambda (e)
                (with z (list-find zs (lambda (z) (== (zotero-entry-key z)
                                                      (entry-name e))))
                  (and z (cons (zotero-entry-key z) (mark e z)))))
              es)
         identity))))

;; The entries converted before, per item: (version key . entry). An item is
;; exported again only when its version or its key changed; so a
;; bibliography made after its references were asked (in a web browser,
;; where the answers come later) needs no request
(define converted (make-ahash-table))

(define (converted-entry z)
  (with c (ahash-ref converted (list (zotero-entry-library z)
                                     (zotero-entry-item z)))
    (and c (== (car c) (zotero-entry-version z))
         (== (cadr c) (zotero-entry-key z))
         (cons (cadr c) (cddr c)))))

(define (convert-entries zs)
  ;; The database entries of the Zotero entries @zs, as (key . entry)
  (let* ((todo (list-filter zs (negate converted-entry)))
         (new (convert-now todo)))
    (for (x new)
      (with z (list-find todo (lambda (z) (== (zotero-entry-key z) (car x))))
        (ahash-set! converted (list (zotero-entry-library z)
                                    (zotero-entry-item z))
                    (cons (zotero-entry-version z) x))))
    (list-filter (map converted-entry zs) identity)))

(define (rename-entry e name)
  (list (car e) (cadr e) (caddr e) name (list-ref e 4) (list-ref e 5)))

(tm-define (zotero-db-entries names)
  (:synopsis "The (name . entry) for the @names which Zotero has")
  ;; The source :zotero of bib-retrieve-entries. The items are recorded
  ;; with the document; a key renamed in Zotero still gives its item,
  ;; under the old name, until the citations are updated
  (if (not (zotero-ready?)) '()
      (let* ((found (zotero-resolve names))
             (missing (list-difference names (map car found)))
             (renamed (car (zotero-check-missing missing))))
        (zotero-record-items (map cdr found))
        (append (convert-entries (map cdr found))
                (append-map
                 (lambda (p)
                   (map (lambda (x)
                          (cons (car p)
                                (meta-set (rename-entry (cdr x) (car p))
                                          "zotero-key" (car x))))
                        (convert-entries (list (cdr p)))))
                 renamed)))))

(tm-define (zotero-import-items zs)
  (:synopsis "Import the Zotero entries @zs into the database")
  ;; Returns the number of imported entries; they are synced from then on
  (with l (convert-entries zs)
    (with-database (bib-database)
      (bib-save `(document ,@(map cdr l))))
    (length l)))

(tm-define (zotero-import-citations)
  (:synopsis "Import into the database the references of the citations")
  ;; those of Zotero which the database does not have yet
  (:interactive #t)
  (zotero-forget-state)
  (zotero-command import-citations-again import-citations))

(define (import-citations-again)
  (zotero-command import-citations-again import-citations))

(define (import-citations)
  (if (not (zotero-ready?))
      (set-message (zotero-status-message (zotero-status)) "Zotero")
      (let* ((keys (list-filter (zotero-project-citations)
                                (negate zotero-in-database?)))
             (found (zotero-resolve keys)))
        ;; NOTE: in a web browser, once all the answers have come
        (when (not (zotero-asking?))
          (with n (zotero-import-items (map cdr found))
            (set-message
             (if (== n 0) "The database has all the references of the citations"
                 (zotero-tr (if (== n 1)
                                "Imported %1 reference from Zotero into the database"
                                "Imported %1 references from Zotero into the database")
                            (number->string n)))
             "Zotero"))))))

(tm-define (zotero-import-entry e)
  (:synopsis "Import into the database the reference of the Zotero entry @e")
  (:interactive #t)
  (zotero-command (lambda () (zotero-import-entry e))
                  (lambda () (import-entry e))))

(define (import-entry e)
  (with n (zotero-import-items (list e))
    (when (not (zotero-asking?))
      (set-message (if (== n 1)
                       (zotero-tr "Imported %1 from Zotero into the database"
                                  (zotero-entry-key e))
                       (zotero-tr "%1 could not be imported"
                                  (zotero-entry-key e)))
                   "Zotero"))))

(tm-define (zotero-can-import? e)
  (:synopsis "Can the reference of the Zotero entry @e enter the database?")
  (and e (supports-db?) (not (zotero-in-database? (zotero-entry-key e)))))

(tm-define (zotero-search-entries query exclude)
  (:synopsis "The database entries of the Zotero items matching @query")
  ;; For the search window of the database: at most 10 items, except those
  ;; with the names @exclude and those which the database has
  (if (or (< (string-length query) 2) (not (zotero-ready?))) '()
      (with zs (list-filter (zotero-search query 10 #t)
                            (lambda (z)
                              (let ((k (zotero-entry-key z)))
                                (and (nin? k exclude)
                                     (not (and (supports-db?)
                                               (zotero-in-database? k)))))))
        (map cdr (convert-entries zs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The search window of the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("zotero in database search" "on" noop))

(tm-define (zotero-in-database-search?)
  (:synopsis "Does the search window of the database also search Zotero?")
  (== (get-preference "zotero in database search") "on"))

(tm-define (zotero-source-text e zotero?)
  (:synopsis "The source of the entry @e, for the search window")
  ;; @zotero? is #t when it comes from Zotero, #f when it is in the
  ;; database, or the name of its source (a BibTeX file)
  (with lib (zotero-entry-meta e "zotero-library")
    (cond ((string? zotero?) zotero?)
          ((not zotero?) (zotero-tr (if lib "Database, from Zotero" "Database")))
          ((and lib (!= (zotero-normalize-library lib) (zotero-user-library)))
           (string-append "Zotero, " (zotero-library-name lib)))
          (else "Zotero"))))

(define (with-source val src)
  ;; The pretty reference @val, preceded by its source @src
  (with mark `(with "color" "dark green" "font-shape" "small-caps"
                (concat "[" ,src "] "))
    (cond ((tm-func? val 'concat) `(concat ,mark ,@(cdr val)))
          ((tm-func? val 'document)
           (if (null? (cdr val)) `(document ,mark)
               `(document (concat ,mark ,(cadr val)) ,@(cddr val))))
          (else `(concat ,mark ,val)))))

(tm-define (zotero-mark-results results entries zotero?)
  (:synopsis "The search @results with the sources of their @entries")
  (map (lambda (res)
         (if (not (and (tm-func? res 'db-result 2) (string? (cadr res)))) res
             (with e (list-find entries
                                (lambda (e) (== (entry-name e) (cadr res))))
               (if (not e) res
                   `(db-result ,(cadr res)
                               ,(with-source (caddr res)
                                             (zotero-source-text e
                                                                 zotero?)))))))
       results))

;; Without the database tool, the same window searches the BibTeX file of
;; the bibliography and Zotero

(define bib-file-cache (make-ahash-table))

(define (bib-file-entries f)
  ;; The database entries of the BibTeX file @f, while it does not change
  (let* ((name (url->system f))
         (date (url-last-modified f))
         (cached (ahash-ref bib-file-cache name)))
    (if (and cached (== (car cached) date)) (cdr cached)
        (let* ((t (tm->stree (zealous-bib-import (string-load f))))
               (l (if (tm-func? t 'document)
                      (list-filter (cdr t) db-entry-any?) '())))
          (ahash-set! bib-file-cache name (cons date l))
          l))))

(tm-define (zotero-file-search-results query)
  (:synopsis "The results of the search of @query, without the database")
  ;; At most 20 references of the BibTeX file of the bibliography (unless
  ;; it is managed by Zotero: its items are those of Zotero), then those of
  ;; Zotero, each with its source
  (let* ((f (zotero-own-bib-file))
         (fl (if (not f) '()
                 (list-filter (bib-file-entries f)
                              (lambda (e)
                                (zotero-summary-matches?
                                 query (db-entry-summary e))))))
         (fl* (if (> (length fl) 20) (sublist fl 0 20) fl))
         (zl (if (zotero-in-database-search?)
                 (zotero-search-entries query (map entry-name fl*))
                 '()))
         (r (if (null? fl*) '()
                (zotero-mark-results (db-pretty fl* "bib" :pretty) fl*
                                     (url->system (url-tail f)))))
         (z (if (null? zl) '()
                (zotero-mark-results (db-pretty zl "bib" :pretty) zl #t))))
    (cond ((nnull? (append r z))
           (append r (if (> (length fl) 20) (list "More items follow") '())
                   z (zotero-searching-results)))
          ((zotero-asking?) (zotero-searching-results))
          ((and (not f) (not (zotero-ready?)))
           (list (zotero-tr "No bibliography file, and Zotero is not available")))
          (else (list (zotero-tr "No matching items"))))))

(tm-define (zotero-searching-results)
  (:synopsis "A line of the search results, while zotero.org is asked")
  (if (and (zotero-in-database-search?) (zotero-asking?))
      (list (zotero-tr "Searching zotero.org..."))
      '()))

(define (zotero-source-state)
  ;; Zotero, as a source of the search window
  (let ((where (if (zotero-web?) "zotero.org" "Zotero")))
    (cond ((not (zotero-in-database-search?))
           (zotero-tr "Zotero is left out (see the Zotero settings)"))
          ((zotero-ready?)
           (with libs (if (== (get-preference "zotero libraries") "all")
                          (with n (length (zotero-groups))
                            (zotero-tr (if (== n 1)
                                           "%1 (My Library and %2 group)"
                                           "%1 (My Library and %2 groups)")
                                       where (number->string n)))
                          (zotero-tr "%1 (My Library)" where))
             (if (zotero-asking?) (zotero-tr "%1, searching..." libs) libs)))
          ((== (zotero-status) 'disabled)
           (zotero-tr "Zotero refuses the requests (enable its local API)"))
          ((and (== (zotero-status) 'not-running) (not (zotero-web?)))
           (zotero-tr "Zotero is not running"))
          ((== (zotero-status) 'no-key)
           (zotero-tr "Zotero (give the API key of zotero.org)"))
          (else (zotero-status-message (zotero-status))))))

(tm-define (zotero-search-sources-text db)
  (:synopsis "The sources of the search window of references, for @db")
  ;; @db is :bib-file without the database tool; in a web browser, the
  ;; state of Zotero may come later, and shows the line again
  (zotero-with-retry (lambda () (refresh-now "db-search-sources"))
                     (lambda () (search-sources-text db))))

(define (search-sources-text db)
  (zotero-tr "Sources: %1; %2"
             (if (== db :bib-file)
                 (with f (zotero-own-bib-file)
                   (cond (f (url->system (url-tail f)))
                         ((with m (zotero-master-bibliography-file)
                            (and m (url-exists? m)))
                          (zotero-tr "the BibTeX file exported from Zotero"))
                         (else (zotero-tr "no BibTeX file in the bibliography"))))
                 (zotero-tr "your database"))
             (zotero-source-state)))

(tm-define (zotero-open-search-tool t)
  (:synopsis "Search a reference for the citation @t, without the database")
  (zotero-forget-state)
  (zotero-search-opened)
  (and-with u (if (tree-func? t 'cite-detail) (tree-ref t 0) (tree-down t))
    (open-db-chooser
     :bib-file "bib" "Search bibliographic reference"
     (lambda (key)
       (when (and key
                  (tree->path u)
                  (tree-in? (tree-up u)
                            '(cite nocite cite-detail cite-TeXmacs)))
         (tree-set! u key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summaries of the entries of the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-entry-summary e)
  (zotero-summary (entry-name e)
                  (map (lambda (f) (cons (cadr f) (caddr f)))
                       (list-filter (entry-fields e)
                                    (cut tm-func? <> 'db-field 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keeping the imported entries in sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("zotero sync version" "" noop))

(define (imported-entries)
  ;; The current entries of the database which come from Zotero
  ;; NOTE: the libraries of the groups are those of the user now, even if
  ;; TeXmacs does not look for citations in them
  (with libs (append (list "user" (zotero-user-library))
                     (map car (zotero-groups)))
    (with-database (bib-database)
      (map db-load-entry (db-search (list (cons "zotero-library" libs)))))))

(tm-define (zotero-imported?)
  (:synopsis "Has the database entries which come from Zotero?")
  ;; NOTE: without asking Zotero (the groups known so far)
  (with-database (bib-database)
    (nnull? (db-search (list (cons "zotero-library"
                                   (cons* "user" (zotero-user-library)
                                          (zotero-known-groups))))))))

(define (entry-library e)
  (zotero-normalize-library (zotero-entry-meta e "zotero-library")))

(define (entry-item e)
  (zotero-entry-meta e "zotero-item"))

(define (update-entry! e new)
  ;; Make @new the current version of the database entry @e
  ;; NOTE: db-update-entry ignores the meta attributes: when only they
  ;; differ (a renamed key, a new version of the item), they are set
  (with-database (bib-database)
    (with id (db-update-entry (cadr e) (entry->assoc-list new #t))
      (when (== id (cadr e))
        (for (key '("zotero-key" "zotero-version" "zotero-synced"))
          (and-with v (zotero-entry-meta new key)
            (db-set-field id key (list v))))))))

(define (set-meta! e key val)
  (with-database (bib-database)
    (db-set-field (cadr e) key (list val))))

(define (newer? e versions)
  ;; Has the item of the entry @e a newer version in Zotero?
  (with x (assoc (entry-item e) versions)
    (and x (> (cdr x)
              (or (string->number (or (zotero-entry-meta e "zotero-version")
                                      "0"))
                  0)))))

(tm-define (zotero-sync-database . opt-force)
  (:synopsis "Update the entries of the database which come from Zotero")
  ;; Returns (updated renamed deleted conflicts), lists of names, the
  ;; conflicts being (old-entry . new-entry); #f when Zotero is unavailable
  ;; or nothing changed in Zotero since the last sync
  (let* ((force? (and (nnull? opt-force) (car opt-force)))
         (old (if (zotero-ready?) (imported-entries) '()))
         (libs (list-remove-duplicates
                (cons (zotero-user-library) (map entry-library old))))
         (v (zotero-libraries-versions libs))
         (last (get-preference "zotero sync version")))
    (cond ((not (zotero-ready?)) #f)
          ((and (not force?) v (== v last)) #f)
          (else
            (let* ((updated '()) (renamed '()) (deleted '()) (conflicts '())
                   (report
                    (lambda (e new)
                      (lambda (kind)
                        (cond ((== kind 'updated)
                               (set! updated (cons (entry-name e) updated)))
                              ((== kind 'renamed)
                               (set! renamed (cons (entry-name e) renamed))
                               (set! updated (cons (entry-name e) updated)))
                              ((== kind 'conflict)
                               (set! conflicts
                                     (cons (cons e new) conflicts))))))))
              ;; first what Zotero says of each library, then the changes:
              ;; NOTE: in a web browser an awaited answer would look like
              ;; deleted items, so that nothing is changed then
              (let* ((gathered
                      (map (lambda (lib)
                             (let* ((here (list-filter
                                           old (lambda (e) (== (entry-library e)
                                                               lib))))
                                    (versions (zotero-item-versions
                                               (map entry-item here) lib))
                                    ;; items changed in Zotero
                                    (changed (list-filter
                                              here (cut newer? <> versions)))
                                    (zs (zotero-items-entries
                                         (map entry-item changed) lib))
                                    (new (convert-entries zs)))
                               (list here versions changed zs new)))
                           libs)))
                (when (zotero-asking?) (set! v #f) (set! gathered '()))
                (for (g gathered)
                  (with (here versions changed zs new) g
                    ;; items no longer in Zotero
                    (for (e here)
                      (when (not (assoc (entry-item e) versions))
                        (when (!= (zotero-entry-meta e "zotero-deleted") "yes")
                          (set-meta! e "zotero-deleted" "yes"))
                        (set! deleted (cons (entry-name e) deleted))))
                    (for (e changed)
                      (let* ((z (list-find zs (lambda (z)
                                                (== (zotero-entry-item z)
                                                    (entry-item e)))))
                             (x (and z (assoc (zotero-entry-key z) new))))
                        (when x
                          (sync-entry e (cdr x) z (report e (cdr x)))))))))
              (when v (set-preference "zotero sync version" v))
              (list (reverse updated) (reverse renamed) (reverse deleted)
                    (reverse conflicts)))))))

(define (sync-entry e new z report)
  ;; Bring the database entry @e up to date with the entry @new from Zotero
  (let* ((manual? (== (zotero-entry-meta e "modus") "manual"))
         (synced (zotero-entry-meta e "zotero-synced"))
         (key (zotero-entry-key z))
         (same? (and (== (fields->string (entry-fields new)) synced)
                     (== key (entry-name e))))
         ;; the entry keeps its name, so that the citations still work
         (new* (if (== key (entry-name e)) new
                   (meta-set (list (car new) (cadr new) (caddr new)
                                   (entry-name e) (list-ref new 4)
                                   (list-ref new 5))
                             "zotero-key" key))))
    (cond (same?
           ;; only the version changed (e.g. an attachment was added)
           (set-meta! e "zotero-version"
                      (number->string (zotero-entry-version z))))
          (manual?
           ;; edited in TeXmacs too: the user decides, field by field
           (report 'conflict))
          (else
            (update-entry! e new*)
            (report (if (== key (entry-name e)) 'updated 'renamed))))))

(tm-define (zotero-sync-message r)
  (:synopsis "A message describing the result @r of a sync")
  (with (updated renamed deleted conflicts) r
    (with l (append
             (if (null? updated) '()
                 (list (zotero-tr "%1 updated"
                                  (number->string (length updated)))))
             (if (null? renamed) '()
                 (list (zotero-tr "%1 renamed in Zotero"
                                  (number->string (length renamed)))))
             (if (null? deleted) '()
                 (list (zotero-tr "%1 no longer in Zotero"
                                  (number->string (length deleted)))))
             (if (null? conflicts) '()
                 (list (zotero-tr "%1 changed on both sides"
                                  (number->string (length conflicts))))))
      (and (nnull? l)
           (zotero-tr "Zotero references: %1" (string-recompose l ", "))))))

(tm-define (zotero-database-renames keys)
  (:synopsis "The (old . new) of the @keys renamed in Zotero, by the sync")
  ;; the entries of the database for @keys, which Zotero now calls otherwise
  (with-database (bib-database)
    (list-filter
     (map (lambda (k)
            (with ids (db-search (list (list "name" k)))
              (and (pair? ids)
                   (with e (db-load-entry (car ids))
                     (and-with z (zotero-entry-meta e "zotero-key")
                       (and (!= z k) (cons k z)))))))
          keys)
     identity)))

(tm-define (zotero-rename-database-entries renames)
  (:synopsis "Give the entries of the database the keys of Zotero")
  ;; @renames are (old . new); the entries renamed in Zotero (zotero-key)
  ;; get a new version with the new name
  (with-database (bib-database)
    (for (p renames)
      (with ids (db-search (list (list "name" (car p))))
        (when (pair? ids)
          (let* ((e (db-load-entry (car ids)))
                 (l (entry->assoc-list (rename-entry e (cdr p)) #t))
                 (l* (list-filter l (lambda (f)
                                      (!= (car f) "zotero-key")))))
            (when (== (zotero-entry-meta e "zotero-key") (cdr p))
              (db-update-entry (car ids) l*))))))))

(tm-define (zotero-database-entry-info key)
  (:synopsis "(summary . from-zotero?) of the entry @key of the database")
  ;; #f when the database has no entry @key
  (with-database (bib-database)
    (with ids (db-search (list (list "name" key)))
      (and (pair? ids)
           (with e (db-load-entry (car ids))
             (cons (db-entry-summary e)
                   (and (zotero-entry-meta e "zotero-item") #t)))))))

(tm-define (zotero-adopt-entries keys)
  (:synopsis "Mark the copies of Zotero items with @keys in the database")
  ;; They are synced from then on. The copies whose fields differ from
  ;; those of Zotero become manual, and are returned as conflicts
  ;; (old . new), for the user to choose field by field
  (let* ((zs (list-filter (map zotero-find-key keys) identity))
         (new (convert-entries zs))
         (conflicts '()))
    (with-database (bib-database)
      (for (x new)
        (with ids (db-search (list (list "name" (car x))))
          (when (pair? ids)
            (let* ((id (car ids))
                   (e (db-load-entry id))
                   (n (cdr x)))
              (when (not (zotero-entry-meta e "zotero-item"))
                (for (key '("zotero-item" "zotero-library" "zotero-version"
                            "zotero-synced"))
                  (db-set-field id key (list (zotero-entry-meta n key))))
                (when (!= (fields->string (entry-fields e))
                          (fields->string (entry-fields n)))
                  (db-set-field id "modus" (list "manual"))
                  (set! conflicts
                        (cons (cons (db-load-entry id) n) conflicts)))))))))
    (reverse conflicts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entries changed on both sides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (field-value l name)
  (with f (list-find l (lambda (f) (and (tm-func? f 'db-field 2)
                                        (== (cadr f) name))))
    (and f (caddr f))))

(define (field-names . ls)
  (list-remove-duplicates
   (append-map (lambda (l)
                 (map cadr (list-filter l (cut tm-func? <> 'db-field 2))))
               ls)))

(tm-define (zotero-conflict-fields old new)
  (:synopsis "The fields of the entries @old and @new which differ")
  ;; Each is (name texmacs-value zotero-value synced-value default), the
  ;; values being #f for an absent field; by default, a field changed only
  ;; in Zotero is taken from Zotero, otherwise the TeXmacs value is kept
  (let* ((lo (entry-fields old))
         (ln (entry-fields new))
         (ls (string->fields (zotero-entry-meta old "zotero-synced"))))
    (list-filter
     (map (lambda (name)
            (let ((vo (field-value lo name))
                  (vn (field-value ln name))
                  (vs (field-value ls name)))
              (and (!= vo vn)
                   (list name vo vn vs
                         (if (== vo vs) 'zotero 'texmacs)))))
          (field-names lo ln))
     identity)))

(tm-define (zotero-merge-entries old new choices)
  (:synopsis "Save the entry @old with the fields of @new chosen in @choices")
  ;; @choices associates the names of the differing fields to texmacs or
  ;; zotero; the result is a manual version of the entry, synced with the
  ;; current version of the Zotero item, which supersedes @old
  (let* ((lo (entry-fields old))
         (ln (entry-fields new))
         (names (field-names lo ln))
         (pick (lambda (name)
                 (with v (if (== (assoc-ref choices name) 'zotero)
                             (field-value ln name) (field-value lo name))
                   (and v `(db-field ,name ,v)))))
         (fields (list-filter (map pick names) identity))
         (merged (list (car old) (cadr old) (caddr old) (cadddr old)
                       (list-ref old 4) `(document ,@fields)))
         (merged* (meta-set* merged
                        (list (cons "modus" "manual")
                              (cons "zotero-version"
                                    (zotero-entry-meta new "zotero-version"))
                              (cons "zotero-synced"
                                    (fields->string ln))))))
    (with-database (bib-database)
      (db-update-entry (cadr old) (entry->assoc-list merged* #t)))))
