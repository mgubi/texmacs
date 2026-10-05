
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
  ("zotero libraries" "user" noop)
  ;; the references of Zotero which the BibTeX file of the user lacks are
  ;; added to it (without the database tool)
  ("zotero add to bib file" "on" noop))

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
        ;; NOTE: the keys found before are forgotten when the library of
        ;; the user changed (those of the groups, at their next request)
        (when version (note-version! user-library version))
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
  ;; (citation-key item-key title creators year version library doi), in
  ;; cork, or #f for a note, an attachment or an annotation
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
               lib
               (string-or-empty (zotero-attr-ref data "DOI"))))))

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
(tm-define (zotero-entry-doi e) (if (> (length e) 7) (list-ref e 7) ""))

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

;; The completions of the prefixes asked before, as long as no library
;; changes: (keys . complete?), complete? when Zotero gave all its matches
(define completions (make-ahash-table))
(define completion-limit 50)

(define (cached-completions prefix)
  ;; the keys for @prefix, from the answer for @prefix or for a shorter
  ;; prefix which was complete, or #f
  (let loop ((p prefix))
    (and (>= (string-length p) 2)
         (with c (ahash-ref completions p)
           (cond ((and c (== p prefix)) (car c))
                 ((and c (cdr c))
                  (list-filter (car c) (cut string-starts? <> prefix)))
                 (else (loop (substring p 0 (- (string-length p) 1)))))))))

(tm-define (zotero-complete prefix)
  (:synopsis "The citation keys of Zotero which start with @prefix")
  ;; NOTE: the search of Zotero also matches the prefixes of citation keys;
  ;; one request per new prefix while typing, the state of Zotero (which
  ;; notices a change of the library) being checked first
  (cond ((< (string-length prefix) 2) '())
        ((and (zotero-ready?) (cached-completions prefix)) => identity)
        (else
          (let* ((l (zotero-search prefix completion-limit #t))
                 (keys (list-remove-duplicates
                        (list-filter (map zotero-entry-key l)
                                     (cut string-starts? <> prefix)))))
            (when (zotero-ready?)
              (ahash-set! completions prefix
                          (cons keys (< (length l) completion-limit))))
            keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summaries of references (for the check and the search of citations)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A summary of a reference is (key title creators year zotero-entry doi),
;; in cork, the Zotero entry being #f for the other sources and the DOI ""
;; when it is not known

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
          #f
          (zotero-flat-text (or (get "doi") "")))))

(tm-define (zotero-entry-summary e)
  (:synopsis "The summary of the Zotero entry @e")
  (list (zotero-entry-key e) (zotero-entry-title e) (zotero-entry-creators e)
        (zotero-entry-year e) e (zotero-entry-doi e)))

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

(tm-define (zotero-normalized-doi s)
  (:synopsis "The DOI @s without its prefixes, in lowercase")
  (let loop ((s (locase-all (tm-string-trim-both s)))
             (l '("https://doi.org/" "http://doi.org/" "https://dx.doi.org/"
                  "http://dx.doi.org/" "doi:")))
    (cond ((null? l) s)
          ((string-starts? s (car l))
           (tm-string-trim-both (string-drop s (string-length (car l)))))
          (else (loop s (cdr l))))))

(define (summary-doi x)
  (zotero-normalized-doi (if (> (length x) 5) (sixth x) "")))

(tm-define (zotero-same-work? a b)
  (:synopsis "Are the summaries @a and @b the same work?")
  ;; the same DOI when both have one, otherwise the same title and year
  (let ((da (summary-doi a)) (db (summary-doi b)))
    (if (and (!= da "") (!= db "")) (== da db)
        (and (== (normalized-title (second a)) (normalized-title (second b)))
             (or (== (fourth a) (fourth b))
                 (== (fourth a) "") (== (fourth b) ""))))))

(define (same-work? a b) (zotero-same-work? a b))

;; NOTE: needs the bibliography of the document, defined below
(tm-define (zotero-own-bib-file)
  (:synopsis "The BibTeX file of the user in the bibliography, or #f")
  ;; the file of the bibliography of the current document, unless it is
  ;; managed by Zotero (its items are then those of Zotero)
  (and (current-buffer)
       (and-with f (zotero-master-bibliography-file)
         (and (url-exists? f) (not (zotero-managed-file? f)) f))))

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
    (set! completions (make-ahash-table))
    (ahash-set! library-versions lib v)))

(tm-define (zotero-forget-keys)
  (:synopsis "Forget the citation keys found in Zotero")
  (set! resolved (make-ahash-table))
  (set! completions (make-ahash-table))
  (set! library-versions (make-ahash-table)))

(define (find-in-library lib key)
  ;; NOTE: the search also finds longer keys containing key
  (list-find (search-library lib key 100 #f)
             (lambda (e) (== (zotero-entry-key e) key))))

(tm-define (zotero-find-key key)
  (:synopsis "The entry of the item with the citation key @key, or #f")
  ;; the first library which has it wins
  ;; NOTE: the state of Zotero is checked first, since it forgets the keys
  ;; found before when the library changed
  (with cached (and (zotero-ready?) (ahash-ref resolved key))
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

(define (export-items lib items)
  ;; The BibTeX of the @items (at most 50) of the library @lib
  (or (zotero-get lib (string-append
                       "items?format=" (get-preference "zotero export format")
                       "&itemKey=" (string-recompose items ",")))
      ""))

(tm-define (zotero-export-as e key)
  (:synopsis "The BibTeX of the Zotero entry @e, with the citation key @key")
  (rekey (export-items (zotero-entry-library e) (list (zotero-entry-item e)))
         key))

(tm-define (zotero-export entries)
  (:synopsis "The BibTeX of the Zotero @entries, in utf8")
  ;; At most 50 items per request, as for the web API, and one library per
  ;; request; the items cited as zotero:<item key> are exported one by one,
  ;; since Zotero gives them keys of its own
  (let* ((export export-items)
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
;; Keys renamed and items deleted in Zotero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (zotero-check-missing keys)
  (:synopsis "The renamed and deleted items of the @keys not in Zotero")
  ;; Returns (renamed deleted): renamed are (key . entry), the entry of
  ;; the item under its new key, deleted are the keys whose item is gone.
  ;; Only the keys recorded with the document (zotero-recorded-items) are
  ;; checked, by their item; the others are just missing
  (let* ((rec (list-filter (append (zotero-recorded-items)
                                   (with f (zotero-own-bib-file)
                                     (if f (zotero-bib-file-items f) '())))
                           (lambda (x) (in? (car x) keys))))
         (libs (list-remove-duplicates (map caddr rec)))
         (entries (append-map
                   (lambda (lib)
                     (zotero-items-entries
                      (map cadr (list-filter rec (lambda (x)
                                                   (== (caddr x) lib))))
                      lib))
                   libs))
         (entry-of (lambda (x)
                     (list-find entries
                                (lambda (e)
                                  (and (== (zotero-entry-item e) (cadr x))
                                       (== (zotero-entry-library e)
                                           (caddr x))))))))
    (if (not (zotero-ready?)) (list '() '())
        (list (list-filter
               (map (lambda (x)
                      (with e (entry-of x)
                        (and e (!= (zotero-entry-key e) (car x))
                             (cons (car x) e))))
                    rec)
               identity)
              (map car (list-filter rec (negate entry-of)))))))

(tm-define (zotero-rename-message renamed deleted)
  (:synopsis "The message for the @renamed keys and the @deleted items")
  (with l (append
           (map (lambda (p)
                  (string-append (car p) " is now "
                                 (if (string? (cdr p)) (cdr p)
                                     (zotero-entry-key (cdr p)))
                                 " in Zotero"))
                renamed)
           (map (lambda (k) (string-append k " is no longer in Zotero"))
                deleted))
    (and (nnull? l)
         (string-append (string-recompose l "; ")
                        (if (null? renamed) ""
                            ": Document -> Bibliography -> Update the citations")))))

(define (rename-in! t renames)
  ;; Rename the keys of the citations in the tree @t
  (cond ((tree-atomic? t) 0)
        ((tree-in? t citation-tags)
         (apply + (map (lambda (i)
                         (with c (tree-ref t i)
                           (rename-key! c renames)))
                       (.. 0 (tree-arity t)))))
        ((tree-is? t 'cite-detail)
         (if (> (tree-arity t) 0) (rename-key! (tree-ref t 0) renames) 0))
        (else (apply + (map (cut rename-in! <> renames)
                            (tree-children t))))))

(define (rename-key! c renames)
  (with x (and (tree-atomic? c) (assoc (tree->string c) renames))
    (if (not x) 0
        (begin (tree-set! c (cdr x)) 1))))

(tm-define (zotero-rename-citations renames)
  (:synopsis "Rename the keys of the citations of the document or project")
  ;; @renames are (old . new); returns (count changed saved): the number of
  ;; renamed citations, the files changed, and those among them which were
  ;; not open, and were saved
  ;; NOTE: the changes of the open documents can be undone
  (if (null? renames) (list 0 '() '())
      (let ((n 0) (changed '()) (saved '()))
        (for (u (zotero-project-files))
          (let* ((open? (buffer-exists? u))
                 ;; NOTE: buffer-load returns #t when it fails
                 (ok? (or open? (not (buffer-load u)))))
            (when ok?
              (with k (rename-in! (buffer-get u) renames)
                (when (> k 0)
                  (set! n (+ n k))
                  (set! changed (cons u changed))
                  (cond ((not open?)
                         (buffer-save u)
                         (set! saved (cons u saved)))
                        ;; NOTE: only the changes to the current buffer pass
                        ;; through its undo history, which marks it modified
                        ((!= (url->system u) (url->system (current-buffer)))
                         (buffer-pretend-modified u))))
                (when (not open?) (buffer-close u))))))
        (list n (reverse changed) (reverse saved)))))

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
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In a project, the citations are those of the master document and of
;; the files it includes, and the bibliography is that of the master

(tm-define (zotero-master)
  (:synopsis "The master document of the current document")
  (if (project-attached?) (project-get) (current-buffer)))

(tm-define (zotero-file-stree u)
  (:synopsis "The document @u, as an stree")
  ;; the buffer, when it is open, otherwise the file
  (cond ((buffer-exists? u) (tree->stree (buffer-get u)))
        ((url-exists? u) (tree->stree (tree-import u "texmacs")))
        (else '(document ""))))

(define (includes doc)
  ;; the files included by the stree @doc
  (let walk ((t doc))
    (cond ((and (tm-func? t 'include 1) (string? (cadr t))) (list (cadr t)))
          ((pair? t) (append-map walk (cdr t)))
          (else '()))))

(tm-define (zotero-project-files)
  (:synopsis "The master document and the files it includes, recursively")
  (let loop ((todo (list (zotero-master))) (done '()))
    (cond ((null? todo) (reverse done))
          ((in? (url->system (car todo)) (map url->system done))
           (loop (cdr todo) done))
          (else
            (let* ((u (car todo))
                   (sub (map (lambda (name)
                               (url-relative u (unix->url name)))
                             (includes (zotero-file-stree u)))))
              (loop (append (cdr todo) sub) (cons u done)))))))

(tm-define (zotero-project-citations)
  (:synopsis "The citation keys of the current document or of its project")
  (list-remove-duplicates
   (append-map (lambda (u) (zotero-citations (zotero-file-stree u)))
               (zotero-project-files))))

(tm-define (zotero-master-bibliography-file)
  (:synopsis "The BibTeX file of the bibliography of the master document")
  (with m (zotero-master)
    (zotero-bibliography-file m (zotero-file-stree m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managed BibTeX files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (zotero-iso-date t)
  (:synopsis "The date of the time @t (seconds since 1970), as YYYY-MM-DD")
  ;; in UTC; NOTE: pretty-date formats with the patterns of Qt (and runs
  ;; date without Qt), it knows no ISO format. The civil date of a day
  ;; number, after Howard Hinnant
  (define (two n) (string-append (if (< n 10) "0" "") (number->string n)))
  (let* ((z (+ (quotient t 86400) 719468))
         (era (quotient z 146097))
         (doe (- z (* era 146097)))
         (yoe (quotient (- doe (quotient doe 1460) (- (quotient doe 36524))
                           (quotient doe 146096))
                        365))
         (doy (- doe (- (+ (* 365 yoe) (quotient yoe 4)) (quotient yoe 100))))
         (mp (quotient (+ (* 5 doy) 2) 153))
         (d (+ (- doy (quotient (+ (* 153 mp) 2) 5)) 1))
         (m (if (< mp 10) (+ mp 3) (- mp 9)))
         (y (+ (* era 400) yoe (if (<= m 2) 1 0))))
    (string-append (number->string y) "-" (two m) "-" (two d))))

(define (zotero-today) (zotero-iso-date (current-time)))

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

(tm-define (zotero-bib-chunks-of s)
  (:synopsis "The (key . text) of the entries of the BibTeX @s")
  (bib-chunks s))

(define (bib-chunks s)
  ;; The (key . text) of the entries of the BibTeX @s, in utf8
  (let loop ((pos (string-search-forwards "@" 0 s)) (acc '()))
    (if (< pos 0) (reverse acc)
        (let* ((next (string-search-forwards "\n@" pos s))
               (end (if (< next 0) (string-length s) (+ next 1)))
               (text (substring s pos end))
               (open (string-search-forwards "{" 0 text))
               (comma (if (< open 0) -1
                          (string-search-forwards "," open text)))
               (key (and (>= comma 0)
                         (tm-string-trim-both
                          (substring text (+ open 1) comma)))))
          (loop (if (< next 0) -1 (+ next 1))
                (if key (cons (cons (utf8->cork key) text) acc) acc))))))

;; What the last export found about the keys not in Zotero
(define last-check (list '() '()))

(tm-define (zotero-last-check)
  (:synopsis "The (renamed deleted) of the last export to a managed file")
  last-check)

(tm-define (zotero-write-bibliography keys file)
  (:synopsis "Write the BibTeX of the Zotero items with @keys to @file")
  ;; Only the items which TeXmacs asks Zotero for go into the file: the
  ;; @keys which no source before Zotero provides. A key renamed in Zotero
  ;; is exported under its old key, and the entry of a deleted item is
  ;; kept, until the citations are updated. Returns the keys which are
  ;; not in the library of Zotero
  (let* ((asked (list-filter keys (negate zotero-resolved-elsewhere?)))
         (found (zotero-resolve asked))
         (missing (list-difference asked (map car found)))
         (check (zotero-check-missing missing))
         (renamed (car check))
         (deleted (cadr check))
         (old (if (url-exists? file) (bib-chunks (string-load file)) '()))
         (kept (list-filter old (lambda (x) (in? (car x) deleted))))
         (bib (string-append
               (zotero-export (map cdr found))
               (apply string-append
                      (map (lambda (p) (zotero-export-as (cdr p) (car p)))
                           renamed))
               (apply string-append (map cdr kept)))))
    (string-save (string-append
                  managed-marker " on "
                  (zotero-today)
                  "; replaced by Document -> Update -> Bibliography\n"
                  bib)
                 file)
    (zotero-record-items (map cdr found))
    (set! last-check (list renamed deleted))
    (list-difference missing (append (map car renamed) (map car kept)))))

;; References of Zotero added to the BibTeX file of the user: each one is
;; preceded by a comment which names its item, as a link which shows it in
;; Zotero; nothing else of the file is changed
(define added-marker "% Added from Zotero by TeXmacs on ")

(tm-define (zotero-bib-file-items f)
  (:synopsis "The (key item library) of the references added to @f")
  (let* ((s (string-load f))
         (n (string-length s)))
    (let loop ((pos (string-search-forwards added-marker 0 s)) (acc '()))
      (if (< pos 0) (reverse acc)
          (let* ((eol (with e (string-search-forwards "\n" pos s)
                        (if (< e 0) n e)))
                 (line (substring s pos eol))
                 (u (string-search-forwards "zotero://select/" 0 line))
                 (url (and (>= u 0) (substring line u (string-length line))))
                 (at (string-search-forwards "@" eol s))
                 (chunk (and (>= at 0) (bib-chunks (substring s at n))))
                 (key (and (pair? chunk) (caar chunk)))
                 (item (and url (select-url->item url)))
                 (next (string-search-forwards added-marker (+ pos 1) s)))
            (loop next
                  (if (and key item)
                      (cons (list key (cdr item) (car item)) acc)
                      acc)))))))

(define (select-url->item url)
  ;; (library . item) of a url zotero://select/...
  (let* ((rest (string-drop url (string-length "zotero://select/")))
         (l (string-decompose (tm-string-trim-both rest) "/")))
    (cond ((and (== (length l) 3) (== (car l) "library") (== (cadr l) "items"))
           (cons user-library (caddr l)))
          ((and (== (length l) 4) (== (car l) "groups") (== (caddr l) "items"))
           (cons (string-append "groups/" (cadr l)) (cadddr l)))
          (else #f))))

(tm-define (zotero-add-to-bib-file f keys)
  (:synopsis "Add to the BibTeX file @f the references of Zotero it lacks")
  ;; Of the @keys, those which no source before Zotero provides and which
  ;; Zotero has; returns (added missing), lists of keys
  (let* ((s (string-load f))
         (have (map car (bib-chunks s)))
         (asked (list-filter keys
                             (lambda (k)
                               (not (or (in? k have)
                                        (zotero-resolved-elsewhere? k))))))
         (found (zotero-resolve asked))
         (missing (list-difference asked (map car found))))
    (when (nnull? found)
      (let* ((date (zotero-today))
             (texts (map (lambda (p)
                           (string-append
                            "\n" added-marker date ": "
                            (zotero-select-url (cdr p)) "\n"
                            (tm-string-trim-both
                             (zotero-export-as (cdr p) (car p)))
                            "\n"))
                         found))
             (sep (if (or (== s "") (string-ends? s "\n")) "" "\n")))
        (string-save (apply string-append s sep texts) f)
        (zotero-record-items (map cdr found))))
    (list (map car found) missing)))

(tm-define (zotero-add-message added missing f)
  (:synopsis "The message after the references @added to the file @f")
  (with l (append
           (if (null? added) '()
               (list (string-append
                      "Added " (number->string (length added))
                      (if (== (length added) 1) " reference" " references")
                      " from Zotero to " (url->system (url-tail f)))))
           (if (null? missing) '()
               (list (string-append "Not found: "
                                    (string-recompose missing ", ")))))
    (and (nnull? l) (string-recompose l ". "))))

(define (add-to-own-file? f)
  (and f (url-exists? f) (not (zotero-managed-file? f))
       (not (supports-db?))
       (== (get-preference "zotero add to bib file") "on")))

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
         (file (zotero-master-bibliography-file)))
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
            (with missing (zotero-write-bibliography
                           (zotero-project-citations) file)
              ;; NOTE: the generation reports the missing keys; the keys
              ;; renamed in Zotero are always reported
              (and-with msg (export-message (if quiet? '() missing))
                (set-message msg "Zotero"))
              #t)))))

(define (export-message missing)
  ;; The message after an export, for the keys @missing in Zotero
  (with l (append (with r (apply zotero-rename-message (zotero-last-check))
                    (if r (list r) '()))
                  (if (null? missing) '()
                      (list (string-append "Not found: "
                                           (string-recompose missing ", ")))))
    (and (nnull? l) (string-recompose l ". "))))

(define (insert-managed-bibliography)
  ;; A bibliography with a managed file named after the document; with the
  ;; database, a bibliography without file, whose references are kept in
  ;; the document
  (with name (if (supports-db?) ""
                 (string-append (url-basename (current-buffer)) "-zotero"))
    (with body (buffer-get-body (current-buffer))
      (tree-insert! body (tree-arity body)
                    (list (stree->tree
                           `(bibliography "bib" "tm-plain" ,name
                                          (document ""))))))))

(tm-define (zotero-update-bibliography)
  (:synopsis "Export the cited items from Zotero and update the bibliography")
  (let* ((u (zotero-master))
         (file (zotero-master-bibliography-file)))
    (zotero-forget-state)
    (cond ((and (supports-db?)
                (not (and file (url-exists? file)
                          (zotero-managed-file? file))))
           ;; Zotero is a source of the database: no file is needed
           (when (not (bibliography-tag (zotero-file-stree u)))
             (insert-managed-bibliography))
           (update-document "bibliography")
           (set-message
            (if (zotero-ready?)
                "Updated the bibliography from the database and Zotero"
                (string-append (zotero-status-message (zotero-status))
                               ": the bibliography uses the references "
                               "kept in the document"))
            "Zotero"))
          ((url-rooted-tmfs? u)
           (set-message "Save the document first" "Zotero"))
          ((not (zotero-ready?))
           (set-message (zotero-status-message (zotero-status)) "Zotero"))
          ((not file)
           (insert-managed-bibliography)
           (zotero-update-bibliography))
          ((and (url-exists? file) (not (zotero-managed-file? file)))
           (if (!= (get-preference "zotero add to bib file") "on")
               (set-message (string-append (url->system (url-tail file))
                                           " is yours: the references of "
                                           "Zotero are not added to it")
                            "Zotero")
               (with (added missing) (zotero-add-to-bib-file
                                      file (zotero-project-citations))
                 (update-document "bibliography")
                 (set-message
                  (or (zotero-add-message added missing file)
                      (string-append (url->system (url-tail file))
                                     " has the references of the citations"))
                  "Zotero"))))
          (else
            (let* ((keys (zotero-project-citations))
                   (missing (zotero-write-bibliography keys file)))
              (update-document "bibliography")
              (set-message
               (or (export-message missing)
                   (string-append "Exported the citations to "
                                  (url->system (url-tail file))))
               "Zotero"))))))

(tm-define (zotero-citation-renames)
  (:synopsis "The (old . new) citation keys of the document renamed in Zotero")
  ;; the keys of the database renamed by the sync, and the keys recorded
  ;; with the document whose item now has another key
  (let* ((keys (zotero-project-citations))
         (db (if (supports-db?) (zotero-database-renames keys) '()))
         (rest (list-filter keys
                            (lambda (k)
                              (not (or (assoc k db)
                                       (zotero-resolved-elsewhere? k)
                                       (zotero-find-key k))))))
         (renamed (car (zotero-check-missing rest))))
    (append db (map (lambda (p) (cons (car p) (zotero-entry-key (cdr p))))
                    renamed))))

(tm-define (zotero-update-citations)
  (:synopsis "Give the citations the keys which Zotero now has")
  (:interactive #t)
  (zotero-forget-state)
  (if (not (zotero-ready?))
      (set-message (zotero-status-message (zotero-status)) "Zotero")
      (with renames (zotero-citation-renames)
        (if (null? renames)
            (set-message "The citation keys agree with Zotero" "Zotero")
            (let* ((r (zotero-rename-citations renames))
                   (n (car r))
                   (saved (caddr r)))
              (when (supports-db?) (zotero-rename-database-entries renames))
              (update-document "bibliography")
              (set-message
               (string-append "Renamed " (number->string n)
                              (if (== n 1) " citation: " " citations: ")
                              (string-recompose
                               (map (lambda (p) (string-append (car p) " -> "
                                                               (cdr p)))
                                    renames)
                               ", ")
                              (if (null? saved) ""
                                  (string-append
                                   "; saved "
                                   (string-recompose
                                    (map (lambda (u) (url->system (url-tail u)))
                                         saved)
                                    ", "))))
               "Zotero"))))))

(tm-define (zotero-check-document)
  (:synopsis "How the citation keys of the document relate to Zotero")
  ;; An association list, of lists of keys: zotero (found in Zotero),
  ;; elsewhere (found in another source only), renamed ((old . new)),
  ;; deleted (recorded, but the item is gone), missing (nowhere), collisions
  ;; (another source has the key for another work), copies (unmarked
  ;; copies of the Zotero items in the database, which may be adopted)
  (let* ((keys (zotero-project-citations))
         (f (zotero-own-bib-file))
         (own (if f (zotero-bib-file-summaries f) '()))
         (db? (supports-db?))
         (renamed (zotero-citation-renames))
         (r (make-ahash-table))
         (add! (lambda (cat x)
                 (ahash-set! r cat (cons x (or (ahash-ref r cat) '())))))
         (neither '()))
    (for (k keys)
      (if (assoc k renamed) (noop)
          (let* ((info (or (with x (assoc k own) (and x (cons x #f)))
                           (and db? (zotero-database-entry-info k))))
                 (z (zotero-find-key k)))
            (cond ((and info z (cdr info)) (add! 'zotero k))
                  ((and info z (same-work? (car info)
                                           (zotero-entry-summary z)))
                   (add! (if (assoc k own) 'elsewhere 'copies) k))
                  ((and info z) (add! 'collisions k))
                  (info (add! 'elsewhere k))
                  (z (add! 'zotero k))
                  (else (set! neither (cons k neither)))))))
    (with deleted (cadr (zotero-check-missing neither))
      (for (k (reverse neither))
        (add! (if (in? k deleted) 'deleted 'missing) k)))
    (cons (cons 'renamed renamed)
          (map (lambda (cat) (cons cat (reverse (or (ahash-ref r cat) '()))))
               '(zotero elsewhere deleted missing collisions copies)))))

(tm-define (zotero-before-update what)
  (:synopsis "Refresh the managed BibTeX file before updating @what")
  ;; NOTE: called by update-document (Document -> Update)
  (when (in? what '("all" "bibliography"))
    (when (with-zotero-bibliography?)
      (zotero-refresh-bibliography #t))
    ;; the references of Zotero which the file of the user lacks
    (with f (and (current-buffer) (not (url-rooted-tmfs? (current-buffer)))
                 (zotero-master-bibliography-file))
      (when (and (add-to-own-file? f) (zotero-ready?))
        (with (added missing) (zotero-add-to-bib-file
                               f (zotero-project-citations))
          (when (nnull? added)
            (set-message (zotero-add-message added '() f) "Zotero")))))
    ;; the entries imported from Zotero into the database follow it
    (when (and (supports-db?) (zotero-ready?))
      (and-with r (zotero-sync-database)
        (and-with msg (zotero-sync-message r)
          (set-message msg "Zotero"))))))

(tm-define (with-zotero-bibliography?)
  (and-with u (current-buffer)
    (and (not (url-rooted-tmfs? u))
         (and-with f (zotero-master-bibliography-file)
           (zotero-managed-file? f)))))
