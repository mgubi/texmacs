
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
;; import" or by an explicit import), they are kept in sync with Zotero,
;; from Zotero to TeXmacs only: zotero-sync-database.

(texmacs-module (bibtex zotero-db)
  (:use (bibtex zotero)
        (database db-base)
        (database db-convert)
        (database bib-db)))

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

(define (convert-entries zs)
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

(tm-define (zotero-db-entries names)
  (:synopsis "The (name . entry) for the @names which Zotero has")
  ;; The source :zotero of bib-retrieve-entries
  (if (not (zotero-ready?)) '()
      (convert-entries (map cdr (zotero-resolve names)))))

(tm-define (zotero-import-items zs)
  (:synopsis "Import the Zotero entries @zs into the database")
  ;; Returns the number of imported entries; they are synced from then on
  (with l (convert-entries zs)
    (with-database (bib-database)
      (bib-save `(document ,@(map cdr l))))
    (length l)))

(tm-define (zotero-search-entries query exclude)
  (:synopsis "The database entries of the Zotero items matching @query")
  ;; For the search window of the database: at most 10 items, except those
  ;; with the names @exclude and those which the database has
  (if (or (< (string-length query) 2) (not (zotero-ready?))) '()
      (with zs (list-filter (zotero-search query 10 #t)
                            (lambda (z)
                              (let ((k (zotero-entry-key z)))
                                (and (nin? k exclude)
                                     (not (zotero-in-database? k))))))
        (map cdr (convert-entries zs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The other sources of the combined search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-entry-summary e)
  (zotero-summary (entry-name e)
                  (map (lambda (f) (cons (cadr f) (caddr f)))
                       (list-filter (entry-fields e)
                                    (cut tm-func? <> 'db-field 2)))))

(define (local-entries)
  ;; the entries of the document (attachments *-biblio)
  (append-map (lambda (name)
                (with t (tm->stree (get-attachment name))
                  (if (pair? t) (list-filter (cdr t) db-entry-any?) '())))
              (list-filter (list-attachments)
                           (cut string-ends? <> "-biblio"))))

(tm-define (zotero-database-sources q)
  (:synopsis "The (mark summary ...) of the document and the database")
  ;; L for the entries of the document, D for the database of the user,
  ;; for the combined search of @q; at most 20 entries of the database
  (let* ((local (list-filter (map db-entry-summary (local-entries))
                             (cut zotero-summary-matches? q <>)))
         (types (smart-ref db-kind-table "bib"))
         (db (if (== (tm-string-trim-both q) "") '()
                 (with-database (bib-database)
                   (with-limit 20
                     (map db-load-entry
                          (db-search (list (list :completes q)
                                           (cons "type" types)))))))))
    (list (cons "L" local)
          (cons "D" (map db-entry-summary db)))))

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
              (for (lib libs)
                (let* ((here (list-filter old (lambda (e) (== (entry-library e)
                                                              lib))))
                       (versions (zotero-item-versions (map entry-item here)
                                                       lib))
                       ;; items changed in Zotero
                       (changed (list-filter here (cut newer? <> versions)))
                       (zs (zotero-items-entries (map entry-item changed) lib))
                       (new (convert-entries zs)))
                  ;; items no longer in Zotero
                  (for (e here)
                    (when (not (assoc (entry-item e) versions))
                      (when (!= (zotero-entry-meta e "zotero-deleted") "yes")
                        (set-meta! e "zotero-deleted" "yes"))
                      (set! deleted (cons (entry-name e) deleted))))
                  (for (e changed)
                    (let* ((z (list-find zs (lambda (z) (== (zotero-entry-item z)
                                                            (entry-item e)))))
                           (x (and z (assoc (zotero-entry-key z) new))))
                      (when x
                        (sync-entry e (cdr x) z (report e (cdr x))))))))
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
                 (list (string-append (number->string (length updated))
                                      " updated")))
             (if (null? renamed) '()
                 (list (string-append (number->string (length renamed))
                                      " renamed in Zotero")))
             (if (null? deleted) '()
                 (list (string-append (number->string (length deleted))
                                      " no longer in Zotero")))
             (if (null? conflicts) '()
                 (list (string-append (number->string (length conflicts))
                                      " changed on both sides"))))
      (and (nnull? l)
           (string-append "Zotero references: " (string-recompose l ", "))))))

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
