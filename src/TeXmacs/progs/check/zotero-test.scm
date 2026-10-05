
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

;; a group library, with a key which the library of the user has too
(define (default-group)
  `(("GGGG1111" "group2021" "Group work" "Group" "2021" 8 "journalArticle")
    ("GGGG2222" "smith2020" "On gravity (group copy)" "Smith" "2020" 9
     "journalArticle")
    ("GGGG3333" "" "Group item without key" "Roe" "2017" 10 "book")))

(define fake-group (default-group))
(define fake-groups-json
  "[{\"id\": 42, \"version\": 1, \"data\": {\"id\": 42, \"name\": \"Physics group\"}}]")

;; the library of the current request
(define fake-current fake-library)

(define (fake-item key) (assoc key fake-current))

(define (fake-extra x)
  ;; the field extra of an item, when the list gives one
  (if (> (length x) 7) (list-ref x 7) ""))

(define (json-item x)
  (let ((key (list-ref x 0)) (ck (list-ref x 1)) (title (list-ref x 2))
        (creators (list-ref x 3)) (date (list-ref x 4))
        (version (list-ref x 5)) (type (list-ref x 6)))
    (string-append
     "{\"key\": \"" key "\", \"version\": " (number->string version) ","
     " \"meta\": {\"creatorSummary\": \"" creators "\","
     " \"parsedDate\": \"" date "\"},"
     " \"data\": {\"key\": \"" key "\", \"itemType\": \"" type "\","
     " \"title\": \"" title "\""
     (if (== ck "") "" (string-append ", \"citationKey\": \"" ck "\""))
     (if (== (fake-extra x) "") ""
         (string-append ", \"extra\": \"" (fake-extra x) "\""))
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

(define (fake-answer path*)
  ;; the answer for the request path* (after /api/)
  (cond ((string-starts? path* "users/0/")
         (set! fake-current fake-library)
         (fake-library-answer (string-drop path* 8)))
        ((string-starts? path* "groups/42/")
         (set! fake-current fake-group)
         (fake-library-answer (string-drop path* 10)))
        (else "")))

(define (fake-library-answer path)
  (cond ((string-starts? path "groups?format=json") fake-groups-json)
        ((string-starts? path "items/top?limit=1&format=keys")
         "AAAA1111\n")
        ((string-starts? path "items/top?format=json")
         ;; the search matches the citation keys, titles and creators
         (let ((q (locase-all (or (query-ref path "q") "")))
               (all? (string-contains? path "qmode=everything")))
           (json-items
            (list-filter fake-current
                         (lambda (x)
                           (or (string-contains? (locase-all (cadr x)) q)
                               (and all? (string-contains?
                                          (locase-all (fake-extra x)) q))
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
  (set! fake-group (default-group))
  (zotero-forget-state)
  (zotero-forget-keys)
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
        (check= (zotero-entry-creators (cadr l)) "Smith and Jones"))
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
      (check= (zotero-completion-suffixes "smith") '())))
  (check-group "completion cache")
  (with-fake
    (lambda ()
      ;; a prefix asked again, or a longer one, needs no request while the
      ;; answer for the shorter one was complete
      (check= (zotero-complete "smi") '("smith2020" "smith2020a"))
      (set! fake-requests '())
      (check= (zotero-complete "smi") '("smith2020" "smith2020a"))
      (check= (zotero-complete "smith2020a") '("smith2020a"))
      (check= (list-filter fake-requests (cut string-contains? <> "q="))
              '())
      ;; a change of the library forgets them
      (rename-in-zotero! "BBBB2222" "smith2021")
      (check= (zotero-complete "smi") '("smith2020" "smith2021"))
      (check-true (nnull? (list-filter fake-requests
                                       (cut string-contains? <> "q=")))))))

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
      ;; a bibliography without file gets one exported from Zotero, when
      ;; Zotero has references of the citations
      (with-document "nofile.tm"
          (doc-tm "  <\\bibliography|bib|tm-plain|>\n  </bibliography>\n")
        (lambda ()
          (with f (tmp "nofile-zotero.bib")
            (zotero-before-update "all")
            (check= (zotero-master-bibliography-file) f)
            (check-true (zotero-managed-file? f))
            (check-true (string-contains? (string-load f)
                                          "@article{smith2020,"))
            (system-remove f))))
      ;; not when it has none
      (with-document "nozotero.tm"
          (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                         "  See <cite|nowhere2000>.\n\n"
                         "  <\\bibliography|bib|tm-plain|>\n"
                         "  </bibliography>\n</body>\n")
        (lambda ()
          (zotero-before-update "all")
          (check-false (zotero-master-bibliography-file))))
      ;; a file of the user is never replaced: the references of Zotero
      ;; which it lacks are added at its end, after a comment naming their
      ;; item
      (with f (tmp "own.bib")
        (string-save "@article{smith2020, title={Mine}}\n" f)
        (with-document "b.tm"
            (doc-tm "  <\\bibliography|bib|tm-plain|own>\n  </bibliography>\n")
          (lambda ()
            (check-false (with-zotero-bibliography?))
            (zotero-update-bibliography)
            (with t (string-load f)
              (check-true (string-starts? t "@article{smith2020, title={Mine}}\n"))
              (check-true (string-contains?
                           t (string-append "% Added from Zotero by TeXmacs on ")))
              (check-true (string-contains?
                           t ": zotero://select/library/items/EEEE5555\n@article{zotero:EEEE5555,"))
              (check-false (string-contains? t "zotero_own_key")))
            (check= (zotero-bib-file-items f)
                    '(("zotero:EEEE5555" "EEEE5555" "users/0")))
            ;; once there, it is not added again
            (zotero-update-bibliography)
            (zotero-before-update "bibliography")
            (check= (length (zotero-bib-chunks-of (string-load f))) 2)))
        (system-remove f))
      ;; Document -> Update adds them as well, unless the preference says no
      (with f (tmp "own2.bib")
        (string-save "@article{other, title={Other}}" f)
        (with-document "b2.tm"
            (doc-tm "  <\\bibliography|bib|tm-plain|own2>\n  </bibliography>\n")
          (lambda ()
            (with old (get-preference "zotero add to bib file")
              (set-preference "zotero add to bib file" "off")
              (zotero-before-update "bibliography")
              (check= (string-load f) "@article{other, title={Other}}")
              (set-preference "zotero add to bib file" old))
            (zotero-before-update "bibliography")
            (check= (map car (zotero-bib-chunks-of (string-load f)))
                    '("other" "smith2020" "zotero:EEEE5555"))
            ;; a newline before the first reference added
            (check-true (string-starts? (string-load f)
                                        "@article{other, title={Other}}\n"))))
        (system-remove f))
      ;; a key renamed in Zotero is found from the comments of the file,
      ;; even when the document does not remember its item
      (with f (tmp "own3.bib")
        (string-save "" f)
        (with-document "b3.tm"
            (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                           "  See <cite|smith2020a>.\n\n"
                           "  <\\bibliography|bib|tm-plain|own3>\n"
                           "  </bibliography>\n</body>\n")
          (lambda ()
            (zotero-update-bibliography)
            (check= (map car (zotero-bib-file-items f)) '("smith2020a"))
            (set-attachment "zotero-items" (stree->tree '(tuple)))
            (rename-in-zotero! "BBBB2222" "smith2020again")
            (check= (zotero-citation-renames)
                    '(("smith2020a" . "smith2020again")))))
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
(define test-db-count 0)

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
    ;; NOTE: a new file each time, since a database stays in memory
    (set! test-db-count (+ test-db-count 1))
    (set! test-db-file (tmp (string-append "test-" (number->string
                                                    test-db-count)
                                           ".tmdb")))
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
;; Group libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-libraries val thunk)
  ;; Run @thunk with the preference "zotero libraries" set to @val
  (with old (get-preference "zotero libraries")
    (set-preference "zotero libraries" val)
    (with r (check-run thunk)
      (set-preference "zotero libraries" old)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))))

(define (test-groups)
  (check-group "groups")
  (with-fake
    (lambda ()
      ;; by default, only the library of the user
      (check= (zotero-libraries) '("users/0"))
      (check= (zotero-search "group") '())
      (check-false (zotero-find-key "group2021"))
      (check-true (list-find fake-requests
                             (cut string-starts? <> "users/0/items/top?")))
      (with-libraries "all"
        (lambda ()
          (check= (zotero-libraries) '("users/0" "groups/42"))
          (check= (zotero-groups) '(("groups/42" . "Physics group")))
          (check= (zotero-library-name "groups/42") "Physics group")
          (check= (zotero-library-name "users/0") "My Library")
          (with l (zotero-search "gravity")
            (check= (map zotero-entry-key l)
                    '("smith2020" "smith2020a" "smith2020"))
            (check= (map zotero-entry-library l)
                    '("users/0" "users/0" "groups/42")))
          ;; the library of the user wins; the key is ambiguous
          (check= (zotero-entry-item (zotero-find-key "smith2020"))
                  "AAAA1111")
          (check= (zotero-key-libraries "smith2020")
                  '("users/0" "groups/42"))
          (with e (zotero-find-key "group2021")
            (check= (zotero-entry-item e) "GGGG1111")
            (check= (zotero-entry-library e) "groups/42"))
          ;; an item without key in a group: zotero:g<id>:<item key>
          (check= (map zotero-entry-key (zotero-search "Roe"))
                  '("zotero:g42:GGGG3333"))
          (check= (zotero-derived-item "zotero:g42:GGGG3333")
                  '("groups/42" . "GGGG3333"))
          (check= (zotero-derived-item "zotero:EEEE5555")
                  '("users/0" . "EEEE5555"))
          (check= (zotero-entry-title (zotero-find-key "zotero:g42:GGGG3333"))
                  "Group item without key")
          (check= (zotero-complete "smith") '("smith2020" "smith2020a"))
          (check= (zotero-complete "gro") '("group2021"))
          ;; one request per library
          (set! fake-requests '())
          (with s (zotero-export (map zotero-find-key
                                      '("group2021" "smith2020"
                                        "zotero:g42:GGGG3333")))
            (check-true (string-contains? s "@article{group2021,"))
            (check-true (string-contains? s "@article{smith2020,"))
            (check-true (string-contains? s "@article{zotero:g42:GGGG3333,")))
          (check-true (list-find fake-requests
                                 (cut string-starts? <>
                                      "groups/42/items?format=bibtex&itemKey=GGGG1111")))
          (check= (zotero-item-versions '("GGGG1111" "AAAA1111") "groups/42")
                  '(("GGGG1111" . 8)))
          (check= (map zotero-entry-key
                       (zotero-items-entries '("GGGG1111") "groups/42"))
                  '("group2021"))
          (check-true (string? (zotero-libraries-versions
                                '("users/0" "groups/42"))))))
      (check= (zotero-libraries) '("users/0"))))
  (check-group "groups database")
  (with-fake
    (lambda ()
      (with-libraries "all"
        (lambda ()
          (with-test-database
            (lambda ()
              (check= (zotero-import-items
                       (map zotero-find-key '("group2021" "smith2020")))
                      2)
              (check= (db-field "group2021" "zotero-library") '("groups/42"))
              (check= (db-field "smith2020" "zotero-library") '("users/0"))
              (check= (zotero-sync-database) '(() () () ()))
              (check-false (zotero-sync-database))
              ;; changed in the group
              (set! fake-group
                    (map (lambda (x)
                           (if (== (car x) "GGGG1111")
                               (list "GGGG1111" "group2021" "Group work (2nd)"
                                     "Group" "2021" 12 "journalArticle")
                               x))
                         fake-group))
              (set! fake-version 12)
              (check= (zotero-sync-database) '(("group2021") () () ()))
              (check= (db-field "group2021" "title") '("Group work (2nd)"))
              (check= (db-field "group2021" "zotero-version") '("12"))))))
      ;; the entries of a group are synced even when TeXmacs no longer
      ;; looks for citations in it
      (with-test-database
        (lambda ()
          (with-libraries "all"
            (lambda ()
              (zotero-import-items (list (zotero-find-key "group2021")))))
          (set! fake-group
                (map (lambda (x)
                       (if (== (car x) "GGGG1111")
                           (list "GGGG1111" "group2021" "Group work (3rd)"
                                 "Group" "2021" 13 "journalArticle")
                           x))
                     fake-group))
          (set! fake-version 13)
          (check= (zotero-sync-database) '(("group2021") () () ()))
          (check= (db-field "group2021" "title") '("Group work (3rd)")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The combined search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define own-bib
  (string-append
   "@article{smith2020,\n author = {Smith, John},\n"
   " title = {On {G}ravity},\n year = {2020},\n}\n"
   "@article{jones2021,\n author = {Jones, Ann and Brown, Bob and Gray, C.},\n"
   " title = {Dark matter},\n year = {2021},\n}\n"
   "@book{muller2019,\n editor = {M{\\\"u}ller, Hans},\n"
   " title = {Another book},\n year = {2019},\n}\n"))

(define (test-summaries)
  (check-group "summaries")
  (check= (zotero-creators-summary
           '(bib-names (bib-name "John" "" "Smith" "")))
          "Smith")
  (check= (zotero-creators-summary
           '(concat "John " (name "Smith") (name-sep) "Ann " (name "Jones")))
          "Smith and Jones")
  (check= (zotero-creators-summary
           '(bib-names (bib-name "A" "" "X" "") (bib-name "B" "" "Y" "")
                       (bib-name "C" "" "Z" "")))
          "X et al.")
  (check= (zotero-summary "k" '(("title" . (concat "On " (keepcase "Gravity")))
                                ("year" . "2020")))
          '("k" "On Gravity" "" "2020" #f ""))
  ;; the same work: the same DOI when both have one, otherwise the same
  ;; title and year
  (let ((a '("k" "On gravity" "Smith" "2020" #f "10.1000/ABC"))
        (b '("k" "On Gravity!" "Smith" "2020" #f "https://doi.org/10.1000/abc"))
        (c '("k" "On gravity" "Smith" "2020" #f "10.1000/other"))
        (d '("k" "Another title" "Smith" "2021" #f "10.1000/abc"))
        (e '("k" "On gravity" "Smith" "2020" #f "")))
    (check-true (zotero-same-work? a b))
    (check-false (zotero-same-work? a c))
    (check-true (zotero-same-work? a d))
    (check-true (zotero-same-work? c e))
    (check= (zotero-normalized-doi " DOI:10.1/X ") "10.1/x"))
  ;; the formulas of Zotero's BibTeX are LaTeX again
  (check= (zotero-unescape-math
           "\ttitle = {\\${\\textbackslash}{Phi}{\\textasciicircum}4\\_3\\$ is orthogonal to {GFF}},")
          "\ttitle = {$\\Phi^4_3$ is orthogonal to {GFF}},")
  (check= (zotero-unescape-math
           "\ttitle = {on \\${\\textbackslash}mathbf\\{{R}\\}{\\textasciicircum}2\\$ and \\${\\textbackslash}mathrm\\{{SU}\\}(2)\\$},")
          "\ttitle = {on $\\mathbf{{R}}^2$ and $\\mathrm{{SU}}(2)$},")
  (check= (zotero-unescape-math
           "title = {Phase {Transitions} for \\$\\${\\textbackslash}phi {\\textasciicircum}4\\_3\\$\\$},")
          "title = {Phase {Transitions} for $\\phi ^4_3$},")
  (check= (zotero-unescape-math "title = {the {Yukawa}\\$\\_2\\$ theory, \\${\\textbackslash}delta{\\textgreater}3\\$}")
          "title = {the {Yukawa}$_2$ theory, $\\delta>3$}")
  ;; a literal brace of the formula, and a dollar alone
  (check= (zotero-unescape-math
           "t = {\\${\\textbackslash}\\{x{\\textbackslash}\\}\\$}")
          "t = {$\\{x\\}$}")
  (check= (zotero-unescape-math "@article{k,\n\tfile = {Full Text:/Users/me/x.pdf:application/pdf},\n\tyear = {2020},\n}")
          "@article{k,\n\tyear = {2020},\n}")
  (check= (zotero-unescape-math "note = {costs \\$10, {\\textbackslash}o}")
          "note = {costs \\$10, {\\textbackslash}o}")
  (check= (zotero-iso-date 0) "1970-01-01")
  (check= (zotero-iso-date 951782400) "2000-02-29")
  (check= (zotero-iso-date 1791158400) "2026-10-05")
  (check-true (zotero-summary-matches? "gravity 2020"
                                       '("k" "On Gravity" "Smith" "2020" #f)))
  (check-false (zotero-summary-matches? "gravity 2021"
                                        '("k" "On Gravity" "Smith" "2020" #f)))
  (with-fake
    (lambda ()
      (eval-system (string-append "mkdir -p '" zotero-dir "'"))
      (with f (tmp "own.bib")
        (string-save own-bib f)
        (with l (zotero-bib-file-summaries f)
          (check= (map car l) '("smith2020" "jones2021" "muller2019"))
          (check= (third (cadr l)) "Jones et al.")
          (check= (cadr (car l)) "On Gravity")))
      (check= (zotero-select-url (zotero-find-key "smith2020"))
              "zotero://select/library/items/AAAA1111")
      (with-libraries "all"
        (lambda ()
          (check= (zotero-select-url (zotero-find-key "group2021"))
                  "zotero://select/groups/42/items/GGGG1111")))
      ;; in a document whose bibliography is a file of the user
      (with-document "c.tm"
          (doc-tm "  <\\bibliography|bib|tm-plain|own>\n  </bibliography>\n")
        (lambda ()
          (check= (url->system (zotero-own-bib-file))
                  (string-append zotero-dir "/own.bib")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys renamed and items deleted in Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rename-in-zotero! item new-key)
  ;; a new key for the @item of the library of the user, a new version
  ;; NOTE: Zotero is asked again, as after a few seconds
  (set! fake-version (+ fake-version 1))
  (set! fake-library
        (map (lambda (x)
               (if (== (car x) item)
                   (list (car x) new-key (third x) (fourth x) (fifth x)
                         fake-version (list-ref x 6))
                   x))
             fake-library))
  (zotero-forget-state))

(define (delete-in-zotero! item)
  (set! fake-library (list-filter fake-library (lambda (x) (!= (car x) item))))
  (set! fake-version (+ fake-version 1))
  (zotero-forget-state))

(define rename-doc
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                 "  See <cite|smith2020|smith2020a> and "
                 "<cite-detail|smith2020a|p. 2> and <cite|zotero:EEEE5555>."
                 "\n\n  <\\bibliography|bib|tm-plain|r-refs>\n"
                 "  </bibliography>\n</body>\n"))

(define (edit-step thunk)
  ;; one user action, as the event loop wraps a menu action (see the
  ;; editing suite): the changes can then be undone
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define (body-citations)
  (zotero-citations (tree->stree (buffer-tree))))

(define (with-preferences l thunk)
  ;; Run @thunk with the preferences of the pairs @l, set back afterwards
  (let ((old (map (lambda (p) (cons (car p) (get-preference (car p)))) l)))
    (for (p l) (set-preference (car p) (cdr p)))
    (with r (check-run thunk)
      (for (p old) (set-preference (car p) (cdr p)))
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))))

(define (zotero-private name)
  (eval name (resolve-module '(bibtex zotero))))

(define (test-web)
  (check-group "web")
  ;; where the library is read
  (with-preferences '(("zotero source" . "local"))
    (lambda () (check-false (zotero-web?))))
  (with-preferences '(("zotero source" . "web"))
    (lambda () (check-true (zotero-web?))))
  (with-preferences '(("zotero source" . "auto"))
    (lambda () (check= (zotero-web?) (zotero-in-browser?))))
  ;; the library of the user is users/<id> on zotero.org
  (with-preferences '(("zotero user" . "12345 alice"))
    (lambda ()
      (check= ((zotero-private 'web-path) "users/0/items?format=keys")
              "users/12345/items?format=keys")
      (check= ((zotero-private 'web-path) "groups/42/items")
              "groups/42/items")
      (check= (zotero-web-user-name) "alice")))
  (check= ((zotero-private 'js-string) "a'b\\c") "'a\\'b\\\\c'")
  ;; the citation keys of Better BibTeX in the field extra
  (check= ((zotero-private 'extra-citation-key)
           "arXiv: 1234\nCitation Key: smith2020x\nother")
          "smith2020x")
  (check-false ((zotero-private 'extra-citation-key) "nothing"))
  (with-fake
    (lambda ()
      (with-preferences '(("zotero source" . "web")
                          ("zotero user" . "12345 alice"))
        (lambda ()
          ;; a search of keys searches all the fields on zotero.org
          (set! fake-requests '())
          (zotero-find-key "smith2020a")
          (check-true (list-find fake-requests
                                 (cut string-contains? <> "qmode=everything")))
          (set! fake-requests '())
          (zotero-search "gravity")
          (check-false (list-find fake-requests
                                  (cut string-contains? <> "qmode")))
          (check= (zotero-web-url (zotero-find-key "smith2020"))
                  "https://www.zotero.org/alice/items/AAAA1111")
          (with-libraries "all"
            (lambda ()
              (check= (zotero-web-url (zotero-find-key "group2021"))
                      "https://www.zotero.org/groups/42/items/GGGG1111")))
          (check-true (string-contains? (zotero-status-message 'ready)
                                        "library of alice"))
          (check-true (string-contains? (zotero-status-message 'no-key)
                                        "API key"))
          (check= (zotero-status-message 'not-running)
                  "zotero.org cannot be reached")))
      ;; the application does not search keys in all the fields
      (set! fake-requests '())
      (zotero-find-key "smith2020a")
      (check-false (list-find fake-requests (cut string-contains? <> "qmode")))))
  ;; a key of Better BibTeX in extra is the citation key of the item
  (with-fake
    (lambda ()
      (set! fake-library
            (cons '("XXXX9999" "" "Old style" "Roe" "2001" 3 "book"
                    "Citation Key: roe2001old")
                  fake-library))
      (with-preferences '(("zotero source" . "web")
                          ("zotero user" . "12345 alice"))
        (lambda ()
          (check= (zotero-entry-item (zotero-find-key "roe2001old"))
                  "XXXX9999"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asynchronous requests (in a web browser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The browser is simulated: the requests are recorded, and answered by the
;; test with zotero-async-answer
(define sim-browser? #f)
(define sim-requests '())

(tm-define (zotero-in-browser?)
  (:require sim-browser?)
  #t)

(tm-define (zotero-start-request id url headers)
  (:require sim-browser?)
  (set! sim-requests (append sim-requests (list (list id url headers)))))

(define (sim-answer! url-part status version body)
  ;; answer the first recorded request whose url contains @url-part
  (with r (list-find sim-requests (lambda (r) (string-contains? (cadr r)
                                                                 url-part)))
    (set! sim-requests (list-filter sim-requests (lambda (x) (not (eq? x r)))))
    (zotero-async-answer (car r) status version (encode-base64 body))))

(define (with-sim-browser thunk)
  (with-preferences '(("zotero source" . "web")
                      ("zotero api key" . "secret")
                      ("zotero user" . "123 alice"))
    (lambda ()
      (set! sim-browser? #t)
      (set! sim-requests '())
      (set! fake-library (default-library))
      (set! fake-current fake-library)
      (zotero-forget-state)
      (zotero-forget-keys)
      (with r (check-run thunk)
        (set! sim-browser? #f)
        (zotero-forget-state)
        (zotero-forget-keys)
        (when (and (pair? r) (== (car r) 'error))
          (check-report #f "the group" (object->string r)))))))

(define (test-async)
  (check-group "async")
  (with-sim-browser
    (lambda ()
      (let* ((runs 0)
             (again (lambda () (set! runs (+ runs 1)))))
        ;; the state is asked asynchronously: awaited first
        (check= (zotero-with-retry again zotero-status) 'pending)
        (check-true (zotero-pending?))
        (check= (map cadr sim-requests)
                '("https://api.zotero.org/users/123/items/top?limit=1&format=keys"))
        ;; the key goes in a header
        (check-true (in? "Zotero-API-Key: secret" (caddr (car sim-requests))))
        (check= (zotero-status-message 'pending) "Asking zotero.org...")
        ;; the footer says what is asked
        (check= (zotero-progress-message)
                "Asking zotero.org: checking the library...")
        ;; the operation which waited runs again once answered
        ;; the results of the search window wait for the state too
        (check= (zotero-with-retry again
                                   (lambda () (zotero-file-search-results "")))
                '("Searching zotero.org..."))
        (sim-answer! "format=keys" 200 7 "AAAA1111\n")
        (check= runs 1)
        (check-false (zotero-pending?))
        (check-false (zotero-progress-message))
        (check= (zotero-with-retry again zotero-status) 'ready)
        ;; a search: nothing at first, the items once answered
        (check= (zotero-with-retry again (lambda () (zotero-search "gravity")))
                '())
        (check= (length sim-requests) 1)
        (check= (zotero-progress-message)
                "Asking zotero.org: searching ``gravity''...")
        ;; the search window says so, for the results which wait (and run
        ;; again), not for others
        (check= (zotero-searching-results) '())
        (check= (zotero-with-retry (lambda () (noop)) zotero-searching-results)
                '())
        (check= (zotero-with-retry again
                                   (lambda ()
                                     (zotero-file-search-results "gravity")))
                '("Searching zotero.org..."))
        ;; (its own request, with fewer items)
        (sim-answer! "limit=10" 200 7 "[]")
        ;; its sources line says what is asked
        (check= (zotero-search-sources-text :bib-file)
                (string-append "Sources: no BibTeX file in the bibliography; "
                               "Asking zotero.org: searching ``gravity''..."))
        ;; the same request is not asked twice while it is awaited
        (zotero-with-retry again (lambda () (zotero-search "gravity")))
        (check= (length sim-requests) 1)
        (set! fake-current fake-library)
        (sim-answer! "q=gravity" 200 7
                     ((eval 'fake-library-answer
                            (resolve-module '(check zotero-test)))
                      "items/top?format=json&q=gravity"))
        (check= runs 2)
        (check= (map zotero-entry-key
                     (zotero-with-retry again (lambda ()
                                                (zotero-search "gravity"))))
                '("smith2020" "smith2020a"))
        (check= sim-requests '())
        ;; an awaited answer is no deleted item
        (with-buffer-body '(document (cite "smith2020"))
          (lambda ()
            (zotero-record-items
             (list '("smith2020" "AAAA1111" "" "" "" 3 "users/0")))
            (zotero-with-retry again
                               (lambda ()
                                 (check= (zotero-check-missing '("smith2020"))
                                         '(() ()))))
            (check-true (zotero-pending?))
            (zotero-forget-keys)
            (set! sim-requests '())))
        ;; a failure is an answer too: no request again for a while
        (zotero-forget-keys)
        (zotero-forget-state)
        (zotero-with-retry again zotero-status)
        (sim-answer! "format=keys" 0 0 "")
        (check= (zotero-with-retry again zotero-status) 'not-running)
        ;; which is said once all is answered
        (check= (zotero-answered-message) "zotero.org cannot be reached")
        (check= sim-requests '())
        ;; without retry, the request is synchronous (not recorded here)
        (check-false (zotero-pending?)))))
  (check-group "async messages")
  (check= (zotero-request-label
           "https://api.zotero.org/users/1/items/top?format=json&limit=50&q=caf%C3%A9%20au")
          (string-append "searching ``caf" (string (integer->char 233))
                         " au''"))
  (check= (zotero-request-label
           "https://api.zotero.org/users/1/items/top?format=json&limit=9&qmode=everything&q=smith")
          "searching ``smith''")
  (check= (zotero-request-label
           "https://api.zotero.org/users/1/items?format=bibtex&itemKey=A,B,C")
          "exporting 3 references")
  (check= (zotero-request-label
           "https://api.zotero.org/users/1/items?format=json&itemKey=A")
          "fetching 1 reference")
  (check= (zotero-request-label
           "https://api.zotero.org/users/1/items?format=versions&itemKey=A,B")
          "looking for changes of 2 references")
  (check= (zotero-request-label "https://api.zotero.org/keys/current")
          "checking the API key")
  (check= (zotero-request-label
           "https://api.zotero.org/users/1/groups?format=json&limit=100")
          "listing your groups")
  (with-sim-browser
    (lambda ()
      (let* ((again (lambda () (noop))))
        (zotero-with-retry again zotero-status)
        (sim-answer! "format=keys" 200 7 "AAAA1111\n")
        (zotero-with-retry again (lambda () (zotero-search "gravity")))
        (zotero-with-retry again (lambda () (zotero-search "smith")))
        (check= (zotero-progress-message)
                "Asking zotero.org: searching ``smith'', and 1 more...")
        (set! sim-requests '())
        (zotero-forget-keys))))
  (check-group "async update")
  (with-sim-browser
    (lambda ()
      (eval-system (string-append "mkdir -p '" zotero-dir "'"))
      (with f (tmp "async.bib")
        (string-save "@article{other, title={Other}}\n" f)
        (with-document "as.tm"
            (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                           "  See <cite|smith2020>.\n\n"
                           "  <\\bibliography|bib|tm-plain|async>\n"
                           "  </bibliography>\n</body>\n")
          (lambda ()
            ;; an update waits for the answers, and is made again then
            (check= (zotero-before-update "bibliography") 'wait)
            (sim-answer! "format=keys" 200 7 "AAAA1111\n")
            (check= (zotero-before-update "bibliography") 'wait)
            (set! fake-current fake-library)
            (sim-answer! "q=smith2020" 200 7
                         ((eval 'fake-library-answer
                                (resolve-module '(check zotero-test)))
                          "items/top?format=json&q=smith2020"))
            (check= (zotero-before-update "bibliography") 'wait)
            (sim-answer! "format=bibtex" 200 7
                         ((eval 'fake-library-answer
                                (resolve-module '(check zotero-test)))
                          "items?format=bibtex&itemKey=AAAA1111"))
            ;; all is known: the reference is added to the file
            (check= (zotero-before-update "bibliography") 'done)
            (check= (map car (zotero-bib-chunks-of (string-load f)))
                    '("other" "smith2020"))))
        (system-remove f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The key of zotero.org, asked when it is needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (asking-key?) (zotero-private 'asking-key?))

(define (stop-asking!)
  ;; the question is asked when idle (a dialog): the test forgets it
  (eval '(set! asking-key? #f) (resolve-module '(bibtex zotero))))

(define (test-key)
  (check-group "key")
  (with-preferences '(("zotero source" . "web")
                      ("zotero api key" . "")
                      ("zotero user" . ""))
    (lambda ()
      (zotero-forget-state)
      (check-true (zotero-key-missing?))
      (check= (zotero-status) 'no-key)
      ;; an operation which needed zotero.org without a key asks for it
      (stop-asking!)
      (zotero-forget-key-wanted)
      (zotero-ready?)
      (zotero-key-wanted)
      (check-true (asking-key?))
      (stop-asking!)
      ;; a command asks first, and runs once the key is given
      (let* ((ran 0) (again (lambda () (set! ran (+ ran 1)))))
        (zotero-command again again)
        (check= ran 0)
        (check-true (asking-key?))
        (stop-asking!)
        (zotero-key-given "  abc123  " again)
        (check= ran 1)
        (check= (zotero-api-key) "abc123")
        (check-false (zotero-key-missing?))
        ;; an empty answer: no key, and the updates do not ask again
        (zotero-set-api-key "")
        (zotero-key-given "" again)
        (check= ran 1)
        (zotero-ready?)
        (zotero-key-wanted)
        (check-false (asking-key?))
        (zotero-key-given "abc123" again)
        (check= ran 2))
      (zotero-set-api-key "")))
  ;; the application needs no key
  (with-preferences '(("zotero source" . "local"))
    (lambda () (check-false (zotero-key-missing?))))
  ;; an update which needs nothing from Zotero does not ask for the key
  (with-preferences '(("zotero source" . "web")
                      ("zotero api key" . ""))
    (lambda ()
      (eval-system (string-append "mkdir -p '" zotero-dir "'"))
      (with f (tmp "nokey.bib")
        (string-save "@article{smith2020, title={Mine}}\n" f)
        (with-document "nk.tm"
            (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                           "  See <cite|smith2020>.\n\n"
                           "  <\\bibliography|bib|tm-plain|nokey>\n"
                           "  </bibliography>\n</body>\n")
          (lambda ()
            (stop-asking!)
            (zotero-before-update "bibliography")
            (check-false (asking-key?))))
        (system-remove f)))))

(define (test-renamed)
  (check-group "renamed")
  (check= (map car (zotero-bib-chunks-of
                    "% x\n@article{a1,\n title={A},\n}\n@book{ b2 ,\n}\n"))
          '("a1" "b2"))
  (with-fake
    (lambda ()
      (eval-system (string-append "mkdir -p '" zotero-dir "'"))
      (with-document "r.tm" rename-doc
        (lambda ()
          (with f (tmp "r-refs.bib")
            (zotero-update-bibliography)
            (check= (map car (zotero-recorded-items))
                    '("smith2020" "smith2020a" "zotero:EEEE5555"))
            (check= (zotero-citation-renames) '())
            (rename-in-zotero! "BBBB2222" "smith2020again")
            (delete-in-zotero! "EEEE5555")
            (check= (zotero-write-bibliography (body-citations) f) '())
            ;; the renamed item under its old key, the deleted one kept
            (with s (string-load f)
              (check-true (string-contains? s "@article{smith2020a,"))
              (check-false (string-contains? s "smith2020again"))
              (check-true (string-contains? s "@article{zotero:EEEE5555,")))
            (with (renamed deleted) (zotero-last-check)
              (check= (map car renamed) '("smith2020a"))
              (check= (zotero-entry-key (cdar renamed)) "smith2020again")
              (check= deleted '("zotero:EEEE5555"))
              (check= (zotero-rename-message renamed deleted)
                      (string-append
                       "smith2020a is now smith2020again in Zotero; "
                       "zotero:EEEE5555 is no longer in Zotero: "
                       "Document -> Bibliography -> Update the citations")))
            ;; the deleted entry stays while the file is refreshed again
            (zotero-write-bibliography (body-citations) f)
            (check-true (string-contains? (string-load f)
                                          "@article{zotero:EEEE5555,"))
            (with r (zotero-check-document)
              (check= (assoc-ref r 'renamed)
                      '(("smith2020a" . "smith2020again")))
              (check= (assoc-ref r 'zotero) '("smith2020"))
              (check= (assoc-ref r 'deleted) '("zotero:EEEE5555"))
              (check= (assoc-ref r 'missing) '())
              (check-true (in? "smith2020a is now smith2020again in Zotero"
                               (zotero-check-lines r))))
            (check= (zotero-citation-renames)
                    '(("smith2020a" . "smith2020again")))
            ;; Update the citations
            (edit-step zotero-update-citations)
            (check= (body-citations)
                    '("smith2020" "smith2020again" "zotero:EEEE5555"))
            (check= (zotero-citation-renames) '())
            ;; in one step, which can be undone
            (edit-step (lambda () (undo 0)))
            (check= (body-citations)
                    '("smith2020" "smith2020a" "zotero:EEEE5555"))))))))

(define (result-sources l)
  ;; the (name source) of the results of the search window of the database
  (map (lambda (res)
         (list (cadr res)
               (let find ((t (caddr res)))
                 (cond ((and (tm-func? t 'concat 3) (== (cadr t) "["))
                        (caddr t))
                       ((pair? t) (list-or (map find (cdr t))))
                       (else #f)))))
       (list-filter l (cut tm-func? <> 'db-result 2))))

(define (test-database-search)
  (check-group "database search")
  (with-fake
    (lambda ()
      (with-test-database
        (lambda ()
          (zotero-import-items (list (zotero-find-key "smith2020")))
          (save-in-database!
           "@article{mine2020,\n\ttitle = {Gravity of mine},\n\tyear = {2020},\n}\n")
          ;; NOTE: the pretty results come from the bibliographic database
          (module-provide '(database bib-manage))
          (with search (lambda (q)
                         (in-module `((eval 'db-search-results
                                            (resolve-module
                                             '(database db-widgets)))
                                      (bib-database) "bib" ,q)))
            (check= (zotero-search-sources-text (bib-database))
                    "Sources: your database; Zotero (My Library)")
            ;; the source of each reference
            (check= (result-sources (search "gravity"))
                    '(("mine2020" "Database")
                      ("smith2020" "Database, from Zotero")
                      ("smith2020a" "Zotero")))
            ;; Zotero is searched only when the preference says so
            (with old (get-preference "zotero in database search")
              (set-preference "zotero in database search" "off")
              (check= (map car (result-sources (search "gravity")))
                      '("mine2020" "smith2020"))
              (set-preference "zotero in database search" old)))
          (with-libraries "all"
            (lambda ()
              (with e (cdar (zotero-db-entries '("group2021")))
                (check= (zotero-source-text e #t) "Zotero, Physics group")
                (check= (zotero-source-text e #f)
                        "Database, from Zotero")))))))))

(define (test-file-search)
  (check-group "file search")
  ;; without the database tool, the search window has the BibTeX file of
  ;; the bibliography and Zotero
  (module-provide '(bibtex zotero-db))
  (with-fake
    (lambda ()
      (eval-system (string-append "mkdir -p '" zotero-dir "'"))
      (string-save own-bib (tmp "own.bib"))
      (with-document "fs.tm"
          (doc-tm "  <\\bibliography|bib|tm-plain|own>\n  </bibliography>\n")
        (lambda ()
          (check-false (supports-db?))
          (go-to (append (buffer-path) '(0 1 0 0)))
          (check-true (focus-can-search? (tree-innermost 'cite)))
          (check= (result-sources (zotero-file-search-results "gravity"))
                  '(("smith2020" "own.bib") ("smith2020a" "Zotero")))
          (check= (zotero-search-sources-text :bib-file)
                  "Sources: own.bib; Zotero (My Library)")
          (with-libraries "all"
            (lambda ()
              (check= (zotero-search-sources-text :bib-file)
                      "Sources: own.bib; Zotero (My Library and 1 group)")))
          (with old (get-preference "zotero in database search")
            (set-preference "zotero in database search" "off")
            (check= (zotero-search-sources-text :bib-file)
                    (string-append "Sources: own.bib; Zotero is left out "
                                   "(see the Zotero settings)"))
            (set-preference "zotero in database search" old))
          (check= (result-sources (zotero-file-search-results "dark"))
                  '(("jones2021" "own.bib")))
          (check= (zotero-file-search-results "nothing like this")
                  '("No matching items"))
          (set-status! 0)
          (check= (result-sources (zotero-file-search-results "gravity"))
                  '(("smith2020" "own.bib")))
          (set-status! 200)))
      (with-document "fn.tm" (doc-tm "")
        (lambda ()
          (check= (result-sources (zotero-file-search-results "gravity"))
                  '(("smith2020" "Zotero") ("smith2020a" "Zotero")))
          (set-status! 0)
          (check= (zotero-file-search-results "gravity")
                  '("No bibliography file, and Zotero is not available"))
          (check= (zotero-search-sources-text :bib-file)
                  (string-append "Sources: no BibTeX file in the "
                                 "bibliography; Zotero is not running"))
          (set-status! 200))))))

(define (test-import)
  (check-group "import")
  ;; by hand, into the database: the citations, or one reference
  (with-fake
    (lambda ()
      (with-test-database
        (lambda ()
          (with-document "im.tm" rename-doc
            (lambda ()
              (check-true (zotero-can-import? (zotero-find-key "smith2020")))
              (zotero-import-entry (zotero-find-key "smith2020"))
              (check= (db-field "smith2020" "zotero-item") '("AAAA1111"))
              (check-false (zotero-can-import? (zotero-find-key "smith2020")))
              (zotero-import-citations)
              (check= (db-field "smith2020a" "zotero-item") '("BBBB2222"))
              (check= (db-field "zotero:EEEE5555" "zotero-item")
                      '("EEEE5555"))
              (check= (length (db-ids "smith2020")) 1))))))))

(define (test-update-database)
  (check-group "update database")
  ;; with the database, Update from Zotero adds a bibliography without file
  (with-fake
    (lambda ()
      (with-test-database
        (lambda ()
          (with-document "nb.tm" (doc-tm "")
            (lambda ()
              (zotero-update-bibliography)
              (check= (select (tree->stree (buffer-tree))
                              '(:* bibliography))
                      '((bibliography "bib" "tm-plain" "" (document ""))))
              (check-false (url-exists? (tmp "nb-zotero.bib")))
              ;; once there, it is kept as it is
              (zotero-update-bibliography)
              (check= (length (select (tree->stree (buffer-tree))
                                      '(:* bibliography)))
                      1))))))))

(define (test-renamed-database)
  (check-group "renamed database")
  (with-fake
    (lambda ()
      (with-test-database
        (lambda ()
          (zotero-import-items (list (zotero-find-key "smith2020a")))
          (rename-in-zotero! "BBBB2222" "smith2020again")
          (zotero-sync-database)
          (check= (db-field "smith2020a" "zotero-key") '("smith2020again"))
          (with-document "rd.tm" rename-doc
            (lambda ()
              (check= (zotero-citation-renames)
                      '(("smith2020a" . "smith2020again")))
              (zotero-update-citations)
              (check= (body-citations)
                      '("smith2020" "smith2020again" "zotero:EEEE5555"))
              ;; the entry of the database follows
              (check= (db-ids "smith2020a") '())
              (check= (length (db-ids "smith2020again")) 1)
              (check= (db-field "smith2020again" "zotero-key") '())
              (check= (db-field "smith2020again" "zotero-item") '("BBBB2222"))
              (check= (zotero-citation-renames) '())))))))
  (check-group "renamed source")
  (with-fake
    (lambda ()
      (with-test-database
        (lambda ()
          (with-document "rs.tm" rename-doc
            (lambda ()
              ;; the source :zotero records the items with the document
              (check= (map car (zotero-db-entries '("smith2020a"))) '("smith2020a"))
              (check= (map car (zotero-recorded-items)) '("smith2020a"))
              (rename-in-zotero! "BBBB2222" "smith2020again")
              ;; and gives a renamed item under its old key
              (with l (zotero-db-entries '("smith2020a"))
                (check= (map car l) '("smith2020a"))
                (check= (zotero-entry-meta (cdar l) "zotero-key")
                        "smith2020again")
                (check= (list-ref (cdar l) 3) "smith2020a")))))))))

(define master-doc
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                 "  See <cite|smith2020>.\n\n"
                 "  <include|chap.tm>\n\n"
                 "  <\\bibliography|bib|tm-plain|m-refs>\n"
                 "  </bibliography>\n</body>\n"))

(define chapter-doc
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                 "  In the chapter <cite|smith2020a|muller2019>.\n</body>\n"))

(define (test-project)
  (check-group "project")
  (with-fake
    (lambda ()
      (eval-system (string-append "mkdir -p '" zotero-dir "'"))
      (string-save chapter-doc (tmp "chap.tm"))
      (with-document "m.tm" master-doc
        (lambda ()
          (check= (map url->system (zotero-project-files))
                  (list (string-append zotero-dir "/m.tm")
                        (string-append zotero-dir "/chap.tm")))
          (check= (zotero-project-citations)
                  '("smith2020" "smith2020a" "muller2019"))
          ;; the managed file of the master has the citations of the chapter
          (zotero-update-bibliography)
          (with s (string-load (tmp "m-refs.bib"))
            (check-true (string-contains? s "@article{smith2020a,"))
            (check-true (string-contains? s "@article{muller2019,")))
          ;; the citations of the chapter are renamed too
          (rename-in-zotero! "BBBB2222" "smith2020again")
          (check= (zotero-citation-renames)
                  '(("smith2020a" . "smith2020again")))
          (with r (zotero-rename-citations
                   '(("smith2020a" . "smith2020again")))
            (check= (car r) 1)
            (check= (map url->system (cadr r))
                    (list (string-append zotero-dir "/chap.tm")))
            ;; the chapter was not open: it is saved
            (check= (map url->system (caddr r))
                    (list (string-append zotero-dir "/chap.tm"))))
          (with u (tmp "chap.tm")
            (check-false (buffer-exists? u))
            (check= (zotero-citations (zotero-file-stree u))
                    '("smith2020again" "muller2019")))
          ;; an open chapter is changed, and left to be saved
          (with u (tmp "chap.tm")
            (buffer-load u)
            (with r (zotero-rename-citations
                     '(("smith2020again" . "smith2020a")))
              (check= (car r) 1)
              (check= (caddr r) '()))
            (check= (zotero-citations (tree->stree (buffer-get u)))
                    '("smith2020a" "muller2019"))
            (check= (zotero-citations (tree->stree (tree-import u "texmacs")))
                    '("smith2020again" "muller2019"))
            (buffer-close u))))
      (system-remove (tmp "chap.tm")))))

(define (save-in-database! bib)
  (in-module `(with-database (bib-database)
                (bib-save (tm->stree (zealous-bib-import ,bib))))))

(define (test-copies)
  (check-group "copies")
  (with-fake
    (lambda ()
      (with-test-database
        (lambda ()
          ;; copies of Zotero items made by hand, without the marks
          (save-in-database!
           (string-append "@article{smith2020,\n\ttitle = {On gravity},\n"
                          "\tyear = {2020},\n}\n"
                          "@article{smith2020a,\n\ttitle = {On gravity, again},\n"
                          "\tyear = {2020},\n\tvolume = {3},\n}\n"
                          "@article{muller2019,\n\ttitle = {Other},\n"
                          "\tyear = {2019},\n}\n"))
          (check= (db-field "smith2020" "zotero-item") '())
          (with-document "cp.tm" rename-doc
            (lambda ()
              (with r (zotero-check-document)
                (check= (assoc-ref r 'copies) '("smith2020" "smith2020a"))
                (check= (assoc-ref r 'zotero) '("zotero:EEEE5555"))
                (check= (assoc-ref r 'collisions) '()))
              (with conflicts (zotero-adopt-entries '("smith2020" "smith2020a"))
                ;; the same fields: adopted as it is
                (check= (db-field "smith2020" "zotero-item") '("AAAA1111"))
                (check= (db-field "smith2020" "modus") '("imported"))
                ;; other fields: adopted, and the user chooses
                (check= (map (lambda (c) (list-ref (car c) 3)) conflicts)
                        '("smith2020a"))
                (check= (db-field "smith2020a" "modus") '("manual"))
                (with c (car conflicts)
                  (with rows (zotero-conflict-fields (car c) (cdr c))
                    (check= (map car rows) '("volume"))
                    (check= (list-ref (car rows) 4) 'texmacs))))
              (with r (zotero-check-document)
                (check= (assoc-ref r 'copies) '())
                (check= (assoc-ref r 'zotero)
                        '("smith2020" "smith2020a" "zotero:EEEE5555"))))))))))

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

(define (citation-at-cursor)
  (tree-innermost '(cite nocite cite-detail)))

(define (test-focus)
  (check-group "focus")
  ;; Show in Zotero, for a key which TeXmacs knows without asking Zotero
  (with-fake
    (lambda ()
      (with-buffer-body '(document (cite "smith2020" "unknown2000"))
        (lambda ()
          (go-to (append (buffer-path) '(0 0 0)))
          (check= (zotero-citation-entry (citation-at-cursor)) #f)
          ;; once found, it is known
          (zotero-find-key "smith2020")
          (set! fake-requests '())
          (with e (zotero-citation-entry (citation-at-cursor))
            (check= (zotero-entry-item e) "AAAA1111"))
          (go-to (append (buffer-path) '(0 1 0)))
          (check= (zotero-citation-entry (citation-at-cursor)) #f)
          (check= fake-requests '())))
      ;; an item recorded with the document
      (with-buffer-body '(document (cite-detail "group2021" "p. 2"))
        (lambda ()
          (zotero-record-items (list '("group2021" "GGGG1111" "" "" "" 8
                                       "groups/42")))
          (check= (zotero-recorded-items)
                  '(("group2021" "GGGG1111" "groups/42")))
          (go-to (append (buffer-path) '(0 0 0)))
          (with e (zotero-citation-entry (citation-at-cursor))
            (check= (zotero-select-url e)
                    "zotero://select/groups/42/items/GGGG1111"))
          ;; not in the details
          (go-to (append (buffer-path) '(0 1 0)))
          (check= (zotero-citation-entry (citation-at-cursor)) #f))))))

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
  (test-groups)
  (test-summaries)
  (test-web)
  (test-async)
  (test-key)
  (test-renamed)
  (test-database-search)
  (test-file-search)
  (test-import)
  (test-update-database)
  (test-renamed-database)
  (test-copies)
  (test-project)
  (test-focus)
  (eval-system (string-append "rm -rf '" zotero-dir "'"))
  (check-end))
