
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : zotero-test.scm
;; DESCRIPTION : tests of the citations from Zotero
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite checks bibtex/zotero.scm, bibtex/zotero-db.scm and the
;; insertion of citations of bibtex/zotero-widgets.scm without Zotero:
;; zotero-request is replaced by a fake server, which answers from a small
;; library (with the versions of its items and of the library) and records
;; the requests. The database groups use a temporary database, and load
;; bibtex/zotero-db.scm (and with it the modules of the database) only
;; when they run, since other suites expect them not to be loaded.

(texmacs-module (check zotero-test)
  (:use (check check-lib)
        (bibtex zotero)
        (bibtex zotero-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A fake Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fake? #f)
(define fake-status 200)
(define fake-requests '())
(define fake-version 7)

;; (item-key citation-key title creators date version type), in utf8;
;; an empty citation key means none
(define (default-library)
  `(("AAAA1111" "smith2020" "On gravity" "Smith" "2020-03" 3 "journalArticle")
    ("BBBB2222" "smith2020a" "On gravity, again" "Smith and Jones" "2020" 4
     "journalArticle")
    ("CCCC3333" "muller2019" ,(cork->utf8 "Caf<#E9> physics")
     ,(cork->utf8 "M<#FC>ller et al.") "2019" 5 "book")
    ("DDDD4444" "" "A note" "" "" 1 "note")
    ("EEEE5555" "" "Without key" "Doe" "2018" 2 "journalArticle")))

(define fake-library (default-library))

(define (fake-item key) (assoc key fake-library))

(define (json-item x)
  (with (key ck title creators date version type) x
    (string-append
     "{\"key\": \"" key "\", \"version\": " (number->string version) ","
     " \"meta\": {\"creatorSummary\": \"" creators "\","
     " \"parsedDate\": \"" date "\"},"
     " \"data\": {\"key\": \"" key "\", \"itemType\": \"" type "\","
     " \"title\": \"" title "\""
     (if (== ck "") "" (string-append ", \"citationKey\": \"" ck "\""))
     "}}")))

(define (json-items l)
  (string-append "[" (string-recompose (map json-item l) ", ") "]"))

(define (bibtex-item x)
  ;; Zotero gives its own key to an item without citation key
  (with (key ck title creators date version type) x
    (string-append "\n@article{" (if (== ck "") "zotero_own_key" ck) ",\n"
                   "\ttitle = {" title "},\n"
                   "\tyear = {" (if (>= (string-length date) 4)
                                    (substring date 0 4) "") "},\n}\n")))

(define (query-ref path name)
  ;; the value of the parameter @name of the query of @path, decoded
  (with l (with q (string-decompose path "?")
            (if (pair? (cdr q)) (string-decompose (cadr q) "&") '()))
    (and-with p (list-find l (cut string-starts? <> (string-append name "=")))
      (url-decode (string-drop p (+ (string-length name) 1))))))

(define (url-decode s)
  (let loop ((l (string->list s)) (acc '()))
    (cond ((null? l) (list->string (reverse acc)))
          ((and (== (car l) #\%) (>= (length l) 3))
           (loop (cdddr l)
                 (cons (integer->char
                        (string->number (list->string (list (cadr l) (caddr l)))
                                        16))
                       acc)))
          (else (loop (cdr l) (cons (car l) acc))))))

(define (item-keys path)
  (string-decompose (or (query-ref path "itemKey") "") ","))

(define (fake-answer path)
  (cond ((string-starts? path "items/top?limit=1&format=keys")
         "AAAA1111\n")
        ((string-starts? path "items/top?format=json")
         ;; the search matches the citation keys, titles and creators
         (with q (locase-all (or (query-ref path "q") ""))
           (json-items
            (list-filter fake-library
                         (lambda (x)
                           (or (string-contains? (locase-all (cadr x)) q)
                               (string-contains? (locase-all (caddr x)) q)
                               (string-contains? (locase-all (cadddr x))
                                                 q)))))))
        ((string-starts? path "items?format=json")
         (json-items (list-filter (map fake-item (item-keys path)) identity)))
        ((string-starts? path "items?format=versions")
         (string-append
          "{"
          (string-recompose
           (map (lambda (x) (string-append "\"" (car x) "\": "
                                           (number->string (list-ref x 5))))
                (list-filter (map fake-item (item-keys path)) identity))
           ", ")
          "}"))
        ((string-starts? path "items?format=")
         (apply string-append
                (map bibtex-item
                     (list-filter (map fake-item (item-keys path)) identity))))
        ((string-starts? path "items/")
         (with x (fake-item (car (string-decompose
                                  (string-drop path 6) "?")))
           (if x (json-item x) "")))
        (else "")))

(tm-define (zotero-request path interactive?)
  (:require fake?)
  (set! fake-requests (cons path fake-requests))
  (if (== fake-status 200)
      (list 200 (fake-answer path) fake-version)
      (list fake-status "" #f)))

;; NOTE: update-document schedules work on the document for later, which
;; would run once the test has closed it
(tm-define (update-document what)
  (:require fake?)
  (noop))

(define (with-fake thunk)
  (set! fake? #t)
  (set! fake-status 200)
  (set! fake-requests '())
  (set! fake-version 7)
  (set! fake-library (default-library))
  (zotero-forget-state)
  (with r (check-run thunk)
    (set! fake? #f)
    (zotero-forget-state)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(define (set-status! st)
  (set! fake-status st)
  (zotero-forget-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requests and state of Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-requests)
  (check-group "requests")
  (check= (zotero-url-encode "abc-_.~") "abc-_.~")
  (check= (zotero-url-encode "a b&c=d") "a%20b%26c%3Dd")
  (check= (zotero-url-encode (cork->utf8 "M<#FC>ller")) "M%C3%BCller")
  (with-fake
    (lambda ()
      (check= (zotero-status) 'ready)
      (check= (zotero-library-version) 7)
      (set-status! 403)
      (check= (zotero-status) 'disabled)
      (check-true (string-contains? (zotero-status-message 'disabled)
                                    "Allow other applications"))
      (set-status! 0)
      (check= (zotero-status) 'not-running)
      (set-status! 500)
      (check= (zotero-status) 'error)
      (check= (zotero-search "gravity") '())
      ;; a failure is remembered: Zotero is not asked again for a while
      (set-status! 0)
      (check-false (zotero-ready?))
      (set! fake-status 200)
      (set! fake-requests '())
      (check-false (zotero-ready?))
      (check= (zotero-search "gravity") '())
      (check= fake-requests '())
      ;; until the state is forgotten (e.g. by an explicit command)
      (zotero-forget-state)
      (check-true (zotero-ready?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search, keys, completion and export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-search)
  (check-group "search")
  (with-fake
    (lambda ()
      (with l (zotero-search "gravity")
        (check= (map zotero-entry-key l) '("smith2020" "smith2020a"))
        (check= (map zotero-entry-item l) '("AAAA1111" "BBBB2222"))
        (check= (map zotero-entry-year l) '("2020" "2020"))
        (check= (map zotero-entry-version l) '(3 4))
        (check= (zotero-entry-creators (cadr l)) "Smith and Jones")
        (check= (zotero-entry-label (car l))
                "Smith (2020): On gravity  [smith2020]"))
      ;; the query is sent in utf8, the texts come back in cork
      (with l (zotero-search (utf8->cork (cork->utf8 "M<#FC>ller")))
        (check= (map zotero-entry-key l) '("muller2019"))
        (check= (zotero-entry-title (car l))
                (utf8->cork (cork->utf8 "Caf<#E9> physics"))))
      (check-true (string-contains? (car fake-requests) "q=M%C3%BCller"))
      ;; notes are not offered; an item without key is zotero:<item key>
      (check= (zotero-search "note") '())
      (check= (map zotero-entry-key (zotero-search "without"))
              '("zotero:EEEE5555"))
      ;; a key is found exactly, not as the start of a longer one
      (check= (zotero-entry-item (zotero-find-key "smith2020")) "AAAA1111")
      (check= (zotero-entry-item (zotero-find-key "smith2020a")) "BBBB2222")
      (check-false (zotero-find-key "smith"))
      (check-false (zotero-find-key "nobody2000"))
      (check= (zotero-entry-item (zotero-find-key "zotero:EEEE5555"))
              "EEEE5555")
      (check-false (zotero-find-key "zotero:NOSUCH99"))
      ;; the keys found are remembered while the library does not change
      (set! fake-requests '())
      (zotero-find-key "smith2020")
      (check= fake-requests '())
      (check= (map car (zotero-resolve '("muller2019" "nobody" "smith2020")))
              '("muller2019" "smith2020")))))

(define (test-completion)
  (check-group "completion")
  (with-fake
    (lambda ()
      (check= (zotero-complete "smith") '("smith2020" "smith2020a"))
      (check= (zotero-completion-suffixes "smith2020") '("" "a"))
      ;; one letter is not enough
      (check= (zotero-complete "s") '())
      (set-preference "zotero completion" "off")
      (check= (zotero-completion-suffixes "smith") '())
      (set-preference "zotero completion" "on")
      (set-status! 0)
      (check= (zotero-completion-suffixes "smith") '()))))

(define (test-export)
  (check-group "export")
  (with-fake
    (lambda ()
      (check= (zotero-export '()) "")
      (with s (zotero-export (map zotero-find-key '("smith2020" "muller2019")))
        (check-true (string-contains? s "@article{muller2019,"))
        (check-true (string-contains? s "@article{smith2020,")))
      (check-true (string-contains? (car fake-requests)
                                    "itemKey=AAAA1111,CCCC3333"))
      ;; an item without key is exported with its zotero:<item key>
      (with s (zotero-export (list (zotero-find-key "zotero:EEEE5555")))
        (check-true (string-contains? s "@article{zotero:EEEE5555,"))
        (check-false (string-contains? s "zotero_own_key")))
      ;; at most 50 items per request
      (set! fake-requests '())
      (zotero-export (map (lambda (i) (zotero-find-key "smith2020"))
                          (.. 0 120)))
      (check= (length fake-requests) 3)
      (check= (zotero-item-versions '("AAAA1111" "NOSUCH99" "BBBB2222"))
              '(("AAAA1111" . 3) ("BBBB2222" . 4))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citations and bibliography of a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define zotero-dir
  (string-append (url->system (url-temp-dir)) "/zotero-test"))

(define (tmp name) (system->url (string-append zotero-dir "/" name)))

(define (test-document)
  (check-group "document")
  (let ((doc '(document
               (concat "See " (cite "smith2020" "muller2019") ".")
               (cite-detail "smith2020a" "p. 3")
               (with "font-shape" "italic" (nocite "smith2020"))
               (cite-textual "")
               (bibliography "bib" "tm-plain" "refs" (document ""))))
        (u (tmp "paper.tm")))
    (check= (zotero-citations doc) '("smith2020" "muller2019" "smith2020a"))
    (check= (zotero-citations '(document "no citation")) '())
    (check= (url->system (zotero-bibliography-file u doc))
            (string-append zotero-dir "/refs.bib"))
    (check= (url->system (zotero-bibliography-file
                          u '(bibliography "bib" "tm-plain" "sub/x.bib"
                                           (document ""))))
            (string-append zotero-dir "/sub/x.bib"))
    (check-false (zotero-bibliography-file u '(document "x")))
    (with-fake
      (lambda ()
        (eval-system (string-append "mkdir -p '" zotero-dir "'"))
        (with f (tmp "refs.bib")
          (check= (zotero-write-bibliography
                   '("smith2020" "nobody2000" "muller2019") f)
                  '("nobody2000"))
          (check-true (zotero-managed-file? f))
          (check-true (string? (zotero-managed-date f)))
          (with s (string-load f)
            (check-true (string-contains? s "@article{smith2020,"))
            (check-true (string-contains? s "@article{muller2019,"))
            ;; only the items asked for
            (check-false (string-contains? s "smith2020a")))
          (system-remove f))
        ;; a file of the user is not managed
        (with f (tmp "mine.bib")
          (string-save "@article{x, title={X}}\n" f)
          (check-false (zotero-managed-file? f))
          (system-remove f))
        (check-false (zotero-managed-file? (tmp "none.bib")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating the bibliography of a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (doc-tm bib)
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                 "  See <cite|smith2020> and <cite|zotero:EEEE5555>.\n\n"
                 bib "</body>\n"))

(define (with-document name text thunk)
  (let* ((u (tmp name))
         (old (current-buffer)))
    (string-save text u)
    (load-buffer u)
    (switch-to-buffer* u)
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (update-forced)
      (buffer-pretend-saved u)
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (test-update)
  (check-group "update")
  (with-fake
    (lambda ()
      (eval-system (string-append "mkdir -p '" zotero-dir "'"))
      ;; a managed file is refreshed by Document -> Update
      (with-document "a.tm"
          (doc-tm "  <\\bibliography|bib|tm-plain|a-refs>\n  </bibliography>\n")
        (lambda ()
          (with f (tmp "a-refs.bib")
            (zotero-update-bibliography)
            (check-true (zotero-managed-file? f))
            (check-true (string-contains? (string-load f)
                                          "@article{zotero:EEEE5555,"))
            (check-true (with-zotero-bibliography?))
            (string-save (string-append (car (string-decompose
                                              (string-load f) "\n"))
                                        "\n")
                         f)
            (zotero-before-update "bibliography")
            (check-true (string-contains? (string-load f)
                                          "@article{smith2020,"))
            ;; without Zotero, the file is kept as it is
            (set-status! 0)
            (with s (string-load f)
              (zotero-before-update "bibliography")
              (check= (string-load f) s))
            ;; the Zotero items of the citations are remembered
            (check-true (string-contains?
                         (object->string (tm->stree
                                          (get-attachment "zotero-items")))
                         "EEEE5555"))
            (set-status! 200)
            (system-remove f))))
      ;; a file of the user is never replaced
      (with f (tmp "own.bib")
        (string-save "@article{smith2020, title={Mine}}\n" f)
        (with-document "b.tm"
            (doc-tm "  <\\bibliography|bib|tm-plain|own>\n  </bibliography>\n")
          (lambda ()
            (check-false (with-zotero-bibliography?))
            (zotero-update-bibliography)
            (check= (string-load f) "@article{smith2020, title={Mine}}\n")))
        (system-remove f))
      ;; a document without bibliography gets one, with a managed file
      (with-document "c.tm" (doc-tm "")
        (lambda ()
          (zotero-update-bibliography)
          (with f (tmp "c-zotero.bib")
            (check-true (zotero-managed-file? f))
            (system-remove f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-db? #f)
(define test-db-file #f)
(define test-db-defined? #f)

(define (in-module expr)
  ;; NOTE: the macros of the database (with-database) are only defined
  ;; once its modules are loaded, so that the forms using them are
  ;; evaluated then
  (eval expr (resolve-module '(check zotero-test))))

(define (with-test-database thunk)
  ;; Run @thunk with the database tool, and a temporary bibliographic
  ;; database as the database of the user
  (module-provide '(bibtex zotero-db))
  (when (not test-db-defined?)
    (set! test-db-defined? #t)
    (in-module '(tm-define (bib-database)
                  (:require test-db?)
                  test-db-file)))
  (let ((old (get-preference "database tool")))
    (set! test-db-file (tmp "test.tmdb"))
    (system-remove test-db-file)
    (set! test-db? #t)
    (set-preference "database tool" "on")
    (set-preference "zotero sync version" "")
    (with r (check-run thunk)
      (set! test-db? #f)
      (set-preference "database tool" old)
      (reset-preference "zotero sync version")
      (system-remove test-db-file)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))))

(define (db-ids name)
  (in-module `(with-database (bib-database)
                (db-search (list (list "name" ,name))))))

(define (db-field name attr)
  (with ids (db-ids name)
    (and (pair? ids)
         (in-module `(with-database (bib-database)
                       (db-get-field ,(car ids) ,attr))))))

(define (db-set! name attr val)
  (with id (car (db-ids name))
    (in-module `(with-database (bib-database)
                  (db-set-field ,id ,attr (list ,val))))))

(define (test-database)
  (check-group "database")
  (with-fake
    (lambda ()
      (with-test-database
        (lambda ()
          ;; the entries of Zotero carry their origin
          (with l (zotero-db-entries '("smith2020" "nobody2000"))
            (check= (map car l) '("smith2020"))
            (with e (cdar l)
              (check= (zotero-entry-meta e "zotero-item") "AAAA1111")
              (check= (zotero-entry-meta e "zotero-version") "3")
              (check= (zotero-entry-meta e "contributor") "Zotero")
              (check-true (string? (zotero-entry-meta e "zotero-synced")))))
          ;; the database comes before Zotero
          (check-false (zotero-resolved-elsewhere? "smith2020"))
          (check= (zotero-import-items
                   (map zotero-find-key '("smith2020" "muller2019"
                                          "zotero:EEEE5555")))
                  3)
          (check= (length (db-ids "smith2020")) 1)
          (check= (db-field "smith2020" "zotero-item") '("AAAA1111"))
          (check-true (zotero-resolved-elsewhere? "smith2020"))
          (check-false (zotero-resolved-elsewhere? "smith2020a"))
          ;; the search window of the database shows the other items
          (check= (map (lambda (e) (list-ref e 3))
                       (zotero-search-entries "gravity" '()))
                  '("smith2020a"))
          (check= (zotero-search-entries "gravity" '("smith2020a")) '())))))
  (check-group "sync")
  (with-fake
    (lambda ()
      (with-test-database
        (lambda ()
          (zotero-import-items
           (map zotero-find-key '("smith2020" "smith2020a" "muller2019"
                                  "zotero:EEEE5555")))
          ;; nothing changed in Zotero
          (check= (zotero-sync-database) '(() () () ()))
          (check-false (zotero-sync-database))
          ;; changed in Zotero: the title of an item, the key of another;
          ;; an item deleted; an item edited in TeXmacs and in Zotero
          (set! fake-library
                (map (lambda (x)
                       (cond ((== (car x) "AAAA1111")
                              (list "AAAA1111" "smith2020" "On gravity (2nd)"
                                    "Smith" "2020-03" 9 "journalArticle"))
                             ((== (car x) "BBBB2222")
                              (list "BBBB2222" "smith2020again"
                                    "On gravity, again" "Smith and Jones"
                                    "2020" 9 "journalArticle"))
                             ((== (car x) "CCCC3333")
                              (list "CCCC3333" "muller2019" "Physics"
                                    "M" "2019" 9 "book"))
                             (else x)))
                     (list-filter fake-library
                                  (lambda (x) (!= (car x) "EEEE5555")))))
          (set! fake-version 8)
          (db-set! "muller2019" "modus" "manual")
          (db-set! "muller2019" "year" "1999")
          (with r (zotero-sync-database)
            (with (updated renamed deleted conflicts) r
              (check= updated '("smith2020" "smith2020a"))
              (check= renamed '("smith2020a"))
              (check= deleted '("zotero:EEEE5555"))
              (check= (map (lambda (c) (list-ref (car c) 3)) conflicts)
                      '("muller2019"))
              (check-true (string-contains? (zotero-sync-message r)
                                            "2 updated"))
              ;; the new versions
              (check= (db-field "smith2020" "title") '("On gravity (2nd)"))
              (check= (db-field "smith2020" "zotero-version") '("9"))
              ;; a renamed key keeps its name, so that citations work
              (check= (db-field "smith2020a" "zotero-key")
                      '("smith2020again"))
              (check= (db-ids "smith2020again") '())
              ;; a deleted item is kept, and marked
              (check= (db-field "zotero:EEEE5555" "zotero-deleted") '("yes"))
              ;; changed on both sides: the user chooses, field by field
              (with c (car conflicts)
                (with rows (zotero-conflict-fields (car c) (cdr c))
                  (check= (map car rows) '("title" "year"))
                  ;; changed only in Zotero: Zotero's value by default
                  (check= (list-ref (assoc "title" rows) 4) 'zotero)
                  ;; changed in TeXmacs: kept by default
                  (check= (list-ref (assoc "year" rows) 4) 'texmacs)
                  (zotero-merge-entries
                   (car c) (cdr c)
                   (map (lambda (row) (cons (car row) (list-ref row 4)))
                        rows))))
              (check= (db-field "muller2019" "title") '("Physics"))
              (check= (db-field "muller2019" "year") '("1999"))
              (check= (db-field "muller2019" "modus") '("manual"))
              (check= (db-field "muller2019" "zotero-version") '("9"))
              (check= (length (db-ids "muller2019")) 1)))
          ;; once synced, nothing more to do
          (check= (zotero-sync-database #t) '(() () ("zotero:EEEE5555") ()))
          (check-false (zotero-sync-database)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-buffer-body doc thunk)
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (go-end)
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (update-forced)
      (buffer-pretend-saved u)
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (test-insert)
  (check-group "insert")
  (with-buffer-body '(document "Text ")
    (lambda ()
      (zotero-insert-citation '("smith2020" "muller2019"))
      (check= (body) '(document (concat "Text " (cite "smith2020"
                                                       "muller2019"))))
      (zotero-insert-citation '())
      (check= (body) '(document (concat "Text " (cite "smith2020"
                                                       "muller2019"))))))
  ;; inside a citation, the keys are added to it
  (with-buffer-body '(document (cite "smith2020"))
    (lambda ()
      (go-to (append (buffer-path) '(0 0 0)))
      (zotero-insert-citation '("muller2019"))
      (check= (body) '(document (cite "smith2020" "muller2019")))))
  ;; the empty key of a new citation is replaced
  (with-buffer-body '(document (cite ""))
    (lambda ()
      (go-to (append (buffer-path) '(0 0 0)))
      (zotero-insert-citation '("smith2020a"))
      (check= (body) '(document (cite "smith2020a"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (zotero-test-failures)
  (check-suite "zotero")
  (eval-system (string-append "mkdir -p '" zotero-dir "'"))
  (test-requests)
  (test-search)
  (test-completion)
  (test-export)
  (test-document)
  (test-update)
  (test-database)
  (test-insert)
  (eval-system (string-append "rm -rf '" zotero-dir "'"))
  (check-end))
