
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : zotero.scm
;; DESCRIPTION : citations from the Zotero desktop application
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Zotero desktop application (version 7 or later) serves the library of
;; its user at http://localhost:23119/api/, with the same requests as the
;; Zotero web API (version 3), once "Allow other applications on this
;; computer to communicate with Zotero" is enabled in its advanced settings.
;; Reading needs no key; TeXmacs only reads.
;;
;; The citations use the citation keys of Zotero (the "citationKey" field,
;; which Zotero fills since version 7, or Better BibTeX); an item without
;; one is cited as zotero:<item key>. See doc/zotero-design.md for the
;; precedence of the sources of references and the other situations.

(texmacs-module (bibtex zotero)
  (:use (convert bibtex bibtextm)))

(define-preferences
  ("zotero server" "http://localhost:23119" noop)
  ("zotero export format" "bibtex" noop)
  ;; "user" for the library of the user, "all" for the groups too
  ("zotero libraries" "user" noop))

;; A library is the start of the paths of its requests
(define user-library "users/0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unreserved? c)
  (or (char-alphabetic? c) (char-numeric? c) (in? c '(#\- #\_ #\. #\~))))

(define (hex-digit n)
  (string-ref "0123456789ABCDEF" n))

(tm-define (zotero-url-encode s)
  (:synopsis "Percent-encode the (utf8) string @s for a query")
  (apply string-append
         (map (lambda (c)
                (if (and (< (char->integer c) 128) (unreserved? c))
                    (string c)
                    (with n (char->integer c)
                      (string #\% (hex-digit (quotient n 16))
                              (hex-digit (remainder n 16))))))
              (string->list s))))

;; Interactive requests (completion, search as you type) wait less
(define interactive-timeout "1.5")
(define batch-timeout "20")

(tm-define (zotero-request path interactive?)
  (:synopsis "Ask Zotero for @path (after /api/); return (status body version)")
  ;; The status is the HTTP status, or 0 when Zotero cannot be reached or
  ;; does not answer in time; the body is the answer, in utf8; the version
  ;; is the version of the library (Last-Modified-Version), or #f
  ;; NOTE: %header needs curl 7.84; with an older one, the version is #f
  (let* ((url (string-append (get-preference "zotero server")
                             "/api/" path))
         (cmd (list "curl" "--silent"
                    "--max-time" (if interactive? interactive-timeout
                                     batch-timeout)
                    "--header" "Zotero-API-Version: 3"
                    "--write-out"
                    "\n%{http_code} %header{last-modified-version}" url))
         (ret (evaluate-system cmd '() '() '(1 2)))
         (out (cadr ret))
         (pos (string-search-backwards "\n" (string-length out) out)))
    (if (< pos 0)
        (list 0 "" #f)
        (with l (string-tokenize-by-char
                 (substring out (+ pos 1) (string-length out)) #\space)
          (list (or (and (pair? l) (string->number (car l))) 0)
                (substring out 0 pos)
                (and (pair? l) (pair? (cdr l)) (string->number (cadr l))))))))

;; The state of Zotero is remembered for a while, so that menus and typing
;; do not wait for it: a failure is not retried at once (circuit breaker)

(define last-state #f)
(define last-state-time 0)
(define last-version #f)

(define (state-delay st)
  (cond ((== st 'ready) 5000)
        ((== st 'disabled) 60000)
        (else 30000)))

(define (status->state st)
  (cond ((== st 200) 'ready)
        ((== st 403) 'disabled)
        ((== st 0) 'not-running)
        (else 'error)))

(define (remember-state! st)
  (set! last-state st)
  (set! last-state-time (texmacs-time)))

(tm-define (zotero-forget-state)
  (:synopsis "Ask Zotero again at the next request")
  (set! last-state #f))

(tm-define (zotero-status)
  (:synopsis "One of ready, disabled (local API not enabled), not-running")
  (if (and last-state
           (< (- (texmacs-time) last-state-time) (state-delay last-state)))
      last-state
      (with (st body version) (zotero-request
                               (string-append user-library
                                              "/items/top?limit=1&format=keys")
                               #t)
        (when version (set! last-version version))
        (remember-state! (status->state st))
        last-state)))

(tm-define (zotero-ready?)
  (== (zotero-status) 'ready))

(tm-define (zotero-library-version)
  (:synopsis "The version of the Zotero library, or #f")
  ;; NOTE: it changes with any change of the library
  (zotero-forget-state)
  (and (zotero-ready?) last-version))

(tm-define (zotero-status-message st)
  (cond ((== st 'disabled)
         (string-append "Zotero refuses the request: enable \"Allow other "
                        "applications on this computer to communicate with "
                        "Zotero\" in Settings -> Advanced"))
        ((== st 'not-running) "Zotero is not running")
        ((== st 'ready) "Zotero is ready")
        (else "Zotero answered with an error")))

(define (zotero-get lib path . opt-interactive)
  ;; The body of the answer to @path in the library @lib, or #f; nothing is
  ;; asked while Zotero is known not to answer
  (with interactive? (and (nnull? opt-interactive) (car opt-interactive))
    (and (zotero-ready?)
         (with (st body version) (zotero-request (string-append lib "/" path)
                                                 interactive?)
           (when version (note-version! lib version))
           (if (== st 200) body
               (begin
                 (remember-state! (status->state st))
                 #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Answers of Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; json->tree gives objects as (attr key value ...) and arrays as (tuple ...),
;; with the strings in utf8

(tm-define (zotero-attr-ref t key)
  (and (tm-func? t 'attr)
       (let loop ((l (cdr t)))
         (cond ((or (null? l) (null? (cdr l))) #f)
               ((== (car l) key) (cadr l))
               (else (loop (cddr l)))))))

(tm-define (zotero-json s)
  (:synopsis "The answer @s of Zotero (json), as an stree, or #f")
  (and s (!= s "") (tree->stree (json->tree s))))

(define (json-items s)
  ;; The items in the answer @s of Zotero (an array, or a single item)
  (with t (zotero-json s)
    (cond ((tm-func? t 'tuple) (cdr t))
          ((tm-func? t 'attr) (list t))
          (else '()))))

(define (string-or-empty x)
  ;; the utf8 string @x, in cork
  (if (string? x) (utf8->cork x) ""))

(define (json-string x)
  ;; a number or a string of json, as a string
  (cond ((string? x) x)
        ((number? x) (number->string x))
        (else "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The library of the user is users/0, a group library groups/<id>

(tm-define (zotero-user-library) user-library)

(define groups-cache #f)
(define groups-time 0)

(tm-define (zotero-groups)
  (:synopsis "The (library . name) of the group libraries of the user")
  ;; NOTE: remembered for a minute
  (if (and groups-cache (< (- (texmacs-time) groups-time) 60000))
      groups-cache
      (with l (list-filter
               (map (lambda (g)
                      (let* ((id (json-string (zotero-attr-ref g "id")))
                             (data (zotero-attr-ref g "data"))
                             (name (and data (zotero-attr-ref data "name"))))
                        (and (!= id "")
                             (cons (string-append "groups/" id)
                                   (if (string? name) (utf8->cork name)
                                       id)))))
                    (json-items (zotero-get user-library
                                            "groups?format=json")))
               identity)
        (when (zotero-ready?)
          (set! groups-cache l)
          (set! groups-time (texmacs-time)))
        l)))

(tm-define (zotero-libraries)
  (:synopsis "The libraries in which TeXmacs looks for citations")
  ;; the library of the user first: its keys win over those of the groups
  (cons user-library
        (if (== (get-preference "zotero libraries") "all")
            (map car (zotero-groups))
            '())))

(tm-define (zotero-library-name lib)
  (:synopsis "The name of the library @lib, for the user")
  (cond ((== lib user-library) "My Library")
        ((assoc lib (or groups-cache '())) => cdr)
        (else lib)))

(tm-define (zotero-normalize-library lib)
  ;; NOTE: the first entries imported into the database had "user"
  (if (in? lib '(#f "" "user")) user-library lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An item without citation key is cited as zotero:<item key> in the
;; library of the user, as zotero:g<id>:<item key> in a group library
(define derived-prefix "zotero:")

(tm-define (zotero-derived-key? key)
  (string-starts? key derived-prefix))

(define (derived-key lib item)
  (if (== lib user-library)
      (string-append derived-prefix item)
      (string-append derived-prefix "g" (string-drop lib 7) ":" item)))

(tm-define (zotero-derived-item key)
  (:synopsis "The (library . item) of the derived key @key")
  (let* ((s (string-drop key (string-length derived-prefix)))
         (pos (string-search-forwards ":" 0 s)))
    (if (and (string-starts? s "g") (> pos 1))
        (cons (string-append "groups/" (substring s 1 pos))
              (substring s (+ pos 1) (string-length s)))
        (cons user-library s))))

(define (item-entry it lib)
  ;; (citation-key item-key title creators year version library), in cork,
  ;; or #f for a note, an attachment or an annotation
  (let* ((data (zotero-attr-ref it "data"))
         (meta (zotero-attr-ref it "meta"))
         (key (string-or-empty (zotero-attr-ref it "key")))
         (type (and data (zotero-attr-ref data "itemType")))
         (ck (and data (zotero-attr-ref data "citationKey"))))
    (and (string? type)
         (nin? type '("note" "attachment" "annotation"))
         (!= key "")
         (list (if (and (string? ck) (!= ck "")) (utf8->cork ck)
                   (derived-key lib key))
               key
               (string-or-empty (zotero-attr-ref data "title"))
               (string-or-empty (zotero-attr-ref meta "creatorSummary"))
               (with d (string-or-empty (zotero-attr-ref meta "parsedDate"))
                 (if (>= (string-length d) 4) (substring d 0 4) d))
               (or (string->number
                    (string-or-empty (zotero-attr-ref it "version")))
                   0)
               lib))))

(define (items-entries lib s)
  ;; The entries of the items in the answer @s for the library @lib
  (list-filter (map (cut item-entry <> lib) (json-items s)) identity))

(tm-define (zotero-entry-key e) (first e))
(tm-define (zotero-entry-item e) (second e))
(tm-define (zotero-entry-title e) (third e))
(tm-define (zotero-entry-creators e) (fourth e))
(tm-define (zotero-entry-year e) (fifth e))
(tm-define (zotero-entry-version e) (sixth e))
(tm-define (zotero-entry-library e) (list-ref e 6))

(define (search-library lib q n interactive?)
  (items-entries lib
                 (zotero-get lib (string-append
                                  "items/top?format=json&limit="
                                  (number->string n) "&q="
                                  (zotero-url-encode (cork->utf8 q)))
                             interactive?)))

(tm-define (zotero-search q . opt)
  (:synopsis "The items of the libraries matching @q (author, title, year)")
  ;; @q is in cork, as typed in TeXmacs; the options are the maximal number
  ;; of items (50 by default) and whether the request is interactive
  (let* ((n (if (null? opt) 50 (car opt)))
         (interactive? (and (pair? opt) (pair? (cdr opt)) (cadr opt)))
         (l (append-map (cut search-library <> q n interactive?)
                        (zotero-libraries))))
    (if (> (length l) n) (sublist l 0 n) l)))

(define-preferences
  ("zotero completion" "on" noop))

(tm-define (zotero-completion-suffixes prefix)
  (:synopsis "The completions of the citation key @prefix from Zotero")
  ;; As suffixes, for custom-complete; nothing when Zotero is unavailable
  (if (!= (get-preference "zotero completion") "on") '()
      (map (cut string-drop <> (string-length prefix))
           (zotero-complete prefix))))

(tm-define (zotero-complete prefix)
  (:synopsis "The citation keys of Zotero which start with @prefix")
  ;; NOTE: the search of Zotero also matches the prefixes of citation keys
  (if (< (string-length prefix) 2) '()
      (list-remove-duplicates
       (list-filter (map zotero-entry-key (zotero-search prefix 50 #t))
                    (cut string-starts? <> prefix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The combined search: Zotero and the other sources of the document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A summary of a reference is (key title creators year zotero-entry), in
;; cork, the Zotero entry being #f for the other sources. The sources are
;; marked L (the entries of the document), F (a BibTeX file), D (the
;; database) and Z (Zotero), and come in this order of precedence

(tm-define (zotero-flat-text t)
  (:synopsis "The text of the stree @t, without its markup")
  (cond ((string? t) t)
        ((tm-func? t 'name-sep) ", ")
        ((pair? t) (apply string-append (map zotero-flat-text (cdr t))))
        (else "")))

(define (last-names t)
  ;; the last names in an author field, as BibTeX (bib-names) or as the
  ;; database (name) gives it
  (cond ((tm-func? t 'bib-name 4) (list (zotero-flat-text (list-ref t 3))))
        ((tm-func? t 'name) (list (zotero-flat-text t)))
        ((pair? t) (append-map last-names (cdr t)))
        (else '())))

(tm-define (zotero-creators-summary t)
  (:synopsis "The authors of the field @t, as Zotero summarizes them")
  (with l (list-filter (last-names t) (lambda (x) (!= x "")))
    (cond ((null? l) "")
          ((null? (cdr l)) (car l))
          ((null? (cddr l)) (string-append (car l) " and " (cadr l)))
          (else (string-append (car l) " et al.")))))

(tm-define (zotero-summary key fields)
  (:synopsis "The summary of the reference @key with the @fields")
  ;; @fields are (name . value), the values being strees
  (let* ((get (lambda (name) (assoc-ref fields name)))
         (who (or (get "author") (get "editor"))))
    (list key
          (zotero-flat-text (or (get "title") ""))
          (if who (zotero-creators-summary who) "")
          (zotero-flat-text (or (get "year") ""))
          #f)))

(tm-define (zotero-entry-summary e)
  (:synopsis "The summary of the Zotero entry @e")
  (list (zotero-entry-key e) (zotero-entry-title e) (zotero-entry-creators e)
        (zotero-entry-year e) e))

(define bib-file-cache (make-ahash-table))

(tm-define (zotero-bib-file-summaries f)
  (:synopsis "The summaries of the references of the BibTeX file @f")
  ;; NOTE: remembered while the file does not change
  (let* ((name (url->system f))
         (date (url-last-modified f))
         (cached (ahash-ref bib-file-cache name)))
    (if (and cached (== (car cached) date)) (cdr cached)
        (let* ((t (bibtex->texmacs (parse-bibtex-document (string-load f))))
               (l (let walk ((t t))
                    (cond ((tm-func? t 'bib-entry 3)
                           (list (zotero-summary
                                  (cadr (cdr t))
                                  (map (lambda (x) (cons (symbol->string*
                                                          (cadr x))
                                                         (caddr x)))
                                       (list-filter (cdr (cadddr t))
                                                    (cut tm-func? <>
                                                         'bib-field 2))))))
                          ((pair? t) (append-map walk (cdr t)))
                          (else '())))))
          (ahash-set! bib-file-cache name (cons date l))
          l))))

(define (symbol->string* x)
  (if (symbol? x) (symbol->string x) x))

(tm-define (zotero-summary-matches? q sum)
  (:synopsis "Does the summary @sum match all the words of the query @q?")
  (with text (locase-all (string-append (first sum) " " (second sum) " "
                                        (third sum) " " (fourth sum)))
    (list-and (map (lambda (w) (string-contains? text (locase-all w)))
                   (list-filter (string-tokenize-by-char q #\space)
                                (lambda (w) (!= w "")))))))

(define (normalized-title s)
  (list->string (list-filter (string->list (locase-all s))
                             (lambda (c) (or (char-alphabetic? c)
                                             (char-numeric? c))))))

(define (same-work? a b)
  ;; NOTE: without the DOI, the same title and year
  (and (== (normalized-title (second a)) (normalized-title (second b)))
       (or (== (fourth a) (fourth b)) (== (fourth a) "") (== (fourth b) ""))))

;; A line of the combined search is (summary marks collision?)
(tm-define (zotero-line-summary l) (car l))
(tm-define (zotero-line-key l) (first (car l)))
(tm-define (zotero-line-marks l) (cadr l))
(tm-define (zotero-line-collision? l) (caddr l))
(tm-define (zotero-line-entry l) (fifth (car l)))

(tm-define (zotero-combine sources)
  (:synopsis "The lines of the combined search of the @sources")
  ;; @sources are (mark summary ...), in their order of precedence. The
  ;; same work with the same key is one line, with the marks of all its
  ;; sources; different works with the same key are a collision
  (let ((lines '()))
    (for (src sources)
      (for (sum (cdr src))
        (with old (list-find lines (lambda (l)
                                     (and (== (zotero-line-key l) (car sum))
                                          (same-work? (car l) sum))))
          (if old
              (set! lines
                    (map (lambda (l)
                           (if (not (eq? l old)) l
                               (list (if (zotero-line-entry l) (car l)
                                         ;; the Zotero entry, for its item
                                         (rcons (sublist (car l) 0 4)
                                                (fifth sum)))
                                     (list-remove-duplicates
                                      (rcons (cadr l) (car src)))
                                     #f)))
                         lines))
              (set! lines (rcons lines (list sum (list (car src)) #f)))))))
    (map (lambda (l)
           (list (car l) (cadr l)
                 (> (length (list-filter lines
                                         (lambda (x) (== (zotero-line-key x)
                                                         (zotero-line-key l)))))
                    1)))
         lines)))

(tm-define (zotero-collision-message lines source-name)
  (:synopsis "The warning for the keys of the @lines in several works")
  ;; @source-name gives the name of a source from its mark
  (let* ((keys (list-remove-duplicates
                (map zotero-line-key
                     (list-filter lines zotero-line-collision?))))
         (describe
          (lambda (k)
            (with marks (list-remove-duplicates
                         (append-map zotero-line-marks
                                     (list-filter lines
                                                  (lambda (l)
                                                    (== (zotero-line-key l)
                                                        k)))))
              (string-append k " is defined by "
                             (string-recompose (map source-name marks)
                                               " and by "))))))
    (and (nnull? keys)
         (string-recompose (map describe keys) "; "))))

;; NOTE: needs the bibliography of the document, defined below
(tm-define (zotero-own-bib-file)
  (:synopsis "The BibTeX file of the user in the bibliography, or #f")
  ;; the file of the bibliography of the current document, unless it is
  ;; managed by Zotero (its items are then those of Zotero)
  (and-with u (current-buffer)
    (and-with f (zotero-bibliography-file u (tree->stree (buffer-tree)))
      (and (url-exists? f) (not (zotero-managed-file? f)) f))))

(tm-define (zotero-search-sources q)
  (:synopsis "The (mark summary ...) of the sources of the current document")
  ;; for the combined search of @q, in their order of precedence; Zotero
  ;; is left out when it is unavailable
  (let* ((db (if (supports-db?) (zotero-database-sources q) '()))
         (f (zotero-own-bib-file))
         (empty? (== (tm-string-trim-both q) "")))
    (if empty? '()
        (list-filter
         (list (assoc "L" db)
               (and f (cons "F" (list-filter (zotero-bib-file-summaries f)
                                             (cut zotero-summary-matches?
                                                  q <>))))
               (assoc "D" db)
               (and (zotero-ready?)
                    (cons "Z" (map zotero-entry-summary (zotero-search q)))))
         identity))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing an item in Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (zotero-select-url e)
  (:synopsis "The url which shows the item of the Zotero entry @e in Zotero")
  (with lib (zotero-entry-library e)
    (string-append "zotero://select/"
                   (if (== lib user-library) "library"
                       lib)
                   "/items/" (zotero-entry-item e))))

(tm-define (zotero-show-item e)
  (:synopsis "Show the item of the Zotero entry @e in Zotero")
  (with url (zotero-select-url e)
    ;; NOTE: the url only has letters, digits, / and :
    (system (string-append (default-open) " " url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resolving citation keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The entries found for keys, as long as no library changes
(define resolved (make-ahash-table))
(define library-versions (make-ahash-table))

(define (note-version! lib v)
  (when (== lib user-library) (set! last-version v))
  (when (!= v (ahash-ref library-versions lib))
    (set! resolved (make-ahash-table))
    (ahash-set! library-versions lib v)))

(tm-define (zotero-forget-keys)
  (:synopsis "Forget the citation keys found in Zotero")
  (set! resolved (make-ahash-table))
  (set! library-versions (make-ahash-table)))

(define (find-in-library lib key)
  ;; NOTE: the search also finds longer keys containing key
  (list-find (search-library lib key 100 #f)
             (lambda (e) (== (zotero-entry-key e) key))))

(tm-define (zotero-find-key key)
  (:synopsis "The entry of the item with the citation key @key, or #f")
  ;; the first library which has it wins
  (with cached (ahash-ref resolved key)
    (if cached (and (pair? cached) cached)
        (with e (if (zotero-derived-key? key)
                    (let* ((p (zotero-derived-item key))
                           (lib (car p))
                           (item (cdr p)))
                      (and-with s (zotero-get lib (string-append
                                                   "items/" item
                                                   "?format=json"))
                        (and-with e (list-find (items-entries lib s) identity)
                          (and (== (zotero-entry-key e) key) e))))
                    (list-or (map (cut find-in-library <> key)
                                  (zotero-libraries))))
          (when (zotero-ready?)
            (ahash-set! resolved key (or e 'none)))
          e))))

(tm-define (zotero-key-libraries key)
  (:synopsis "The libraries which have an item with the citation key @key")
  ;; more than one when the key is ambiguous
  (list-filter (zotero-libraries) (cut find-in-library <> key)))

(tm-define (zotero-known-entry key)
  (:synopsis "The Zotero entry of @key which TeXmacs knows, or #f")
  ;; without asking Zotero (for menus): the key found before, or the item
  ;; recorded with the document
  (with c (ahash-ref resolved key)
    (if (pair? c) c
        (with x (assoc key (zotero-recorded-items))
          (and x (list key (cadr x) "" "" "" 0 (caddr x)))))))

(define cite-tags* '(cite nocite cite-detail))

(tm-define (zotero-citation-entry t)
  (:synopsis "The Zotero entry of the key at the cursor in the citation @t")
  (and (tree-in? t cite-tags*)
       (cursor-inside? t)
       (let* ((p (cursor-path))
              (tp (tree->path t))
              (i (and (> (length p) (length tp))
                      (list-ref p (length tp))))
              (k (and i (< i (tree-arity t)) (tree-ref t i))))
         (and k (tree-atomic? k)
              (not (and (tree-is? t 'cite-detail) (!= i 0)))
              (zotero-known-entry (tree->string k))))))

(tm-define (zotero-items-entries items . opt-lib)
  (:synopsis "The entries of the Zotero @items (item keys) which still exist")
  ;; in the library of the user, or the library given as option
  (with lib (if (null? opt-lib) user-library (car opt-lib))
    (append-map
     (lambda (l)
       (items-entries lib (zotero-get lib (string-append
                                           "items?format=json&itemKey="
                                           (string-recompose l ",")))))
     (if (null? items) '() (chunks items 50)))))

(tm-define (zotero-resolve keys)
  (:synopsis "The (key . entry) for the @keys which Zotero has")
  (list-filter (map (lambda (k) (and-with e (zotero-find-key k) (cons k e)))
                    keys)
               identity))

(define (chunks l n)
  (if (<= (length l) n) (list l)
      (cons (sublist l 0 n) (chunks (sublist l n (length l)) n))))

(define (rekey bib key)
  ;; The BibTeX entry @bib with the key @key
  (let* ((open (string-search-forwards "{" 0 bib))
         (comma (and (>= open 0) (string-search-forwards "," open bib))))
    (if (and comma (>= comma 0))
        (string-append (substring bib 0 (+ open 1)) (cork->utf8 key)
                       (substring bib comma (string-length bib)))
        bib)))

(tm-define (zotero-export entries)
  (:synopsis "The BibTeX of the Zotero @entries, in utf8")
  ;; At most 50 items per request, as for the web API, and one library per
  ;; request; the items cited as zotero:<item key> are exported one by one,
  ;; since Zotero gives them keys of its own
  (let* ((format (get-preference "zotero export format"))
         (export (lambda (lib items)
                   (or (zotero-get lib (string-append
                                        "items?format=" format "&itemKey="
                                        (string-recompose items ",")))
                       "")))
         (derived? (lambda (e) (zotero-derived-key? (zotero-entry-key e))))
         (plain (list-filter entries (negate derived?)))
         (libs (list-remove-duplicates (map zotero-entry-library plain))))
    (apply string-append
           (append
            (append-map
             (lambda (lib)
               (with items (map zotero-entry-item
                                (list-filter plain
                                             (lambda (e)
                                               (== (zotero-entry-library e)
                                                   lib))))
                 (map (cut export lib <>) (chunks items 50))))
             libs)
            (map (lambda (e)
                   (rekey (export (zotero-entry-library e)
                                  (list (zotero-entry-item e)))
                          (zotero-entry-key e)))
                 (list-filter entries derived?))))))

(tm-define (zotero-item-versions items . opt-lib)
  (:synopsis "The (item . version) of the @items which Zotero still has")
  ;; in the library of the user, or the library given as option
  ;; NOTE: the local API has no list of deleted items: an item which is
  ;; not returned has been deleted (or moved to the trash). It also returns
  ;; the children (attachments, notes) of the items, which are not asked for
  (with lib (if (null? opt-lib) user-library (car opt-lib))
    (append-map
     (lambda (l)
       (with t (zotero-json (zotero-get lib (string-append
                                             "items?format=versions&itemKey="
                                             (string-recompose l ","))))
         (if (not (tm-func? t 'attr)) '()
             (let loop ((r (cdr t)) (acc '()))
               (if (or (null? r) (null? (cdr r))) (reverse acc)
                   (loop (cddr r)
                         (cons (cons (car r)
                                     (or (string->number (json-string (cadr r)))
                                         0))
                               acc)))))))
     (if (null? items) '() (chunks items 50)))))

(tm-define (zotero-libraries-versions libs)
  (:synopsis "The versions of the libraries @libs, as a string")
  ;; NOTE: it changes with any change of one of them
  (zotero-forget-state)
  (and (zotero-ready?)
       (string-recompose
        (map (lambda (lib)
               (with (st body version)
                   (zotero-request (string-append
                                    lib "/items/top?limit=1&format=keys") #f)
                 (string-append lib "=" (if version
                                            (number->string version) "?"))))
             libs)
        " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citations of a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tags whose arguments are citation keys; cite-detail has one key
;; followed by the details
(define citation-tags
  '(cite nocite cite-raw cite-raw* cite-textual cite-textual*
    cite-parenthesized cite-parenthesized* cite-author-link
    cite-author*-link cite-year-link))

(tm-define (zotero-citations doc)
  (:synopsis "The citation keys in the stree @doc, without repetitions")
  (let ((keys '()))
    (let walk ((t doc))
      (when (pair? t)
        (cond ((in? (car t) citation-tags)
               (for (k (cdr t))
                 (when (string? k) (set! keys (cons k keys)))))
              ((and (== (car t) 'cite-detail) (pair? (cdr t))
                    (string? (cadr t)))
               (set! keys (cons (cadr t) keys)))
              (else (for-each walk (cdr t))))))
    (list-remove-duplicates
     (reverse (list-filter keys (lambda (k) (!= k "")))))))

(define (bibliography-tag doc)
  (let walk ((t doc))
    (and (pair? t)
         (if (and (== (car t) 'bibliography) (== (length t) 5)) t
             (list-or (map walk (cdr t)))))))

(tm-define (zotero-bibliography-file u doc)
  (:synopsis "The BibTeX file of the bibliography of @doc, in the buffer @u")
  ;; As for the bibliography tag, the file is relative to the document, and
  ;; ".bib" is implicit; #f when the document has no bibliography
  (and-with t (bibliography-tag doc)
    (with name (fourth t)
      (and (string? name) (!= name "")
           (with f (url-relative u (unix->url name))
             (if (== (url-suffix f) "bib") f (url-glue f ".bib")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managed BibTeX files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A BibTeX file whose first line starts with the marker is written by
;; TeXmacs from Zotero, and may be replaced; any other one is the user's
(define managed-marker "% Exported from Zotero by TeXmacs")

(tm-define (zotero-managed-file? f)
  (:synopsis "Is the BibTeX file @f written by TeXmacs from Zotero?")
  (and (url-exists? f)
       (string-starts? (string-load f) managed-marker)))

(tm-define (zotero-managed-date f)
  (:synopsis "The date of the export of the managed file @f, or #f")
  (with s (string-load f)
    (with pos (string-search-forwards " on " 0 s)
      (and (>= pos 0)
           (with end (string-search-forwards ";" pos s)
             (and (> end pos) (substring s (+ pos 4) end)))))))

(tm-define (zotero-resolved-elsewhere? key)
  (:synopsis "Does a source before Zotero provide the reference @key?")
  ;; In a BibTeX file, only the managed file is consulted; with the
  ;; database, it comes first
  (and (supports-db?) (zotero-in-database? key)))

(tm-define (zotero-write-bibliography keys file)
  (:synopsis "Write the BibTeX of the Zotero items with @keys to @file")
  ;; Only the items which TeXmacs asks Zotero for go into the file: the
  ;; @keys which no source before Zotero provides. Returns the keys which
  ;; are not in the library of Zotero
  (let* ((asked (list-filter keys (negate zotero-resolved-elsewhere?)))
         (found (zotero-resolve asked))
         (missing (list-difference asked (map car found)))
         (bib (zotero-export (map cdr found))))
    (string-save (string-append
                  managed-marker " on "
                  (pretty-date (current-time) "iso8601")
                  "; replaced by Document -> Update -> Bibliography\n"
                  bib)
                 file)
    (zotero-record-items (map cdr found))
    missing))

(tm-define (zotero-recorded-items)
  (:synopsis "The (key item library) of the Zotero items of the citations")
  ;; as remembered with the current document
  (with t (tm->stree (get-attachment "zotero-items"))
    (if (not (tm-func? t 'tuple)) '()
        (list-filter
         (map (lambda (p)
                (cond ((tm-func? p 'tuple 2)
                       (list (cadr p) (caddr p) user-library))
                      ((tm-func? p 'tuple 3) (cdr p))
                      (else #f)))
              (cdr t))
         (lambda (x) (and x (string? (car x)) (string? (cadr x))
                          (string? (caddr x))))))))

(tm-define (zotero-record-items entries)
  (:synopsis "Remember with the document the Zotero items of its citations")
  ;; The (key item library) are kept in an attachment of the document, so
  ;; that a key renamed in Zotero can be found again
  (when (nnull? entries)
    (let* ((h (make-ahash-table)))
      (for (x (zotero-recorded-items))
        (ahash-set! h (car x) (cdr x)))
      (for (e entries)
        (ahash-set! h (zotero-entry-key e)
                    (list (zotero-entry-item e) (zotero-entry-library e))))
      (set-attachment "zotero-items"
                      (stree->tree
                       `(tuple ,@(map (lambda (x) `(tuple ,(car x) ,@(cdr x)))
                                      (sort (ahash-table->list h)
                                            (lambda (x y)
                                              (string<? (car x)
                                                        (car y)))))))))))

(tm-define (zotero-refresh-bibliography . opt-quiet)
  (:synopsis "Refresh the managed BibTeX file of the current document")
  ;; Returns #t when the file was refreshed; without Zotero, the existing
  ;; file is kept, with a message saying of when it is
  (let* ((quiet? (and (nnull? opt-quiet) (car opt-quiet)))
         (u (current-buffer))
         (doc (tree->stree (buffer-tree)))
         (file (zotero-bibliography-file u doc)))
    (cond ((or (not file) (url-rooted-tmfs? file)) #f)
          ((and (url-exists? file) (not (zotero-managed-file? file))) #f)
          ((not (zotero-ready?))
           (when (url-exists? file)
             (set-message
              (string-append (zotero-status-message (zotero-status))
                             ": the bibliography uses the references "
                             "exported on "
                             (or (zotero-managed-date file) "?"))
              "Zotero"))
           #f)
          (else
            (with missing (zotero-write-bibliography (zotero-citations doc)
                                                     file)
              (when (and (nnull? missing) (not quiet?))
                (set-message (string-append "Not found: "
                                            (string-recompose missing ", "))
                             "Zotero"))
              #t)))))

(define (insert-managed-bibliography)
  ;; A bibliography with a managed file named after the document
  (with name (string-append (url-basename (current-buffer)) "-zotero")
    (with body (buffer-get-body (current-buffer))
      (tree-insert! body (tree-arity body)
                    (list (stree->tree
                           `(bibliography "bib" "tm-plain" ,name
                                          (document ""))))))))

(tm-define (zotero-update-bibliography)
  (:synopsis "Export the cited items from Zotero and update the bibliography")
  (let* ((u (current-buffer))
         (doc (tree->stree (buffer-tree)))
         (file (zotero-bibliography-file u doc)))
    (zotero-forget-state)
    (cond ((url-rooted-tmfs? u)
           (set-message "Save the document first" "Zotero"))
          ((not (zotero-ready?))
           (set-message (zotero-status-message (zotero-status)) "Zotero"))
          ((not file)
           (insert-managed-bibliography)
           (zotero-update-bibliography))
          ((and (url-exists? file) (not (zotero-managed-file? file)))
           (set-message (string-append (url->system (url-tail file))
                                       " is not managed by Zotero: it is "
                                       "left as it is")
                        "Zotero"))
          (else
            (let* ((keys (zotero-citations doc))
                   (missing (zotero-write-bibliography keys file)))
              (update-document "bibliography")
              (set-message
               (if (null? missing)
                   (string-append "Exported the citations to "
                                  (url->system (url-tail file)))
                   (string-append "Not found: "
                                  (string-recompose missing ", ")))
               "Zotero"))))))

(tm-define (zotero-before-update what)
  (:synopsis "Refresh the managed BibTeX file before updating @what")
  ;; NOTE: called by update-document (Document -> Update)
  (when (in? what '("all" "bibliography"))
    (when (with-zotero-bibliography?)
      (zotero-refresh-bibliography #t))
    ;; the entries imported from Zotero into the database follow it
    (when (and (supports-db?) (zotero-ready?))
      (and-with r (zotero-sync-database)
        (and-with msg (zotero-sync-message r)
          (set-message msg "Zotero"))))))

(tm-define (with-zotero-bibliography?)
  (and-with u (current-buffer)
    (and (not (url-rooted-tmfs? u))
         (and-with f (zotero-bibliography-file u (tree->stree (buffer-tree)))
           (zotero-managed-file? f)))))
