
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

(texmacs-module (bibtex zotero))

(define-preferences
  ("zotero server" "http://localhost:23119" noop)
  ("zotero export format" "bibtex" noop))

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
  (:synopsis "Ask Zotero for @path; return (status body version)")
  ;; The status is the HTTP status, or 0 when Zotero cannot be reached or
  ;; does not answer in time; the body is the answer, in utf8; the version
  ;; is the version of the library (Last-Modified-Version), or #f
  ;; NOTE: %header needs curl 7.84; with an older one, the version is #f
  (let* ((url (string-append (get-preference "zotero server")
                             "/api/users/0/" path))
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
      (with (st body version) (zotero-request "items/top?limit=1&format=keys"
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

(define (zotero-get path . opt-interactive)
  ;; The body of the answer to @path, or #f; nothing is asked while Zotero
  ;; is known not to answer
  (with interactive? (and (nnull? opt-interactive) (car opt-interactive))
    (and (zotero-ready?)
         (with (st body version) (zotero-request path interactive?)
           (when version (note-version! version))
           (if (== st 200) body
               (begin
                 (remember-state! (status->state st))
                 #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items
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

(define derived-prefix "zotero:")

(tm-define (zotero-derived-key? key)
  (string-starts? key derived-prefix))

(define (item-entry it)
  ;; (citation-key item-key title creators year version), in cork, or #f for
  ;; a note, an attachment or an annotation; an item without citation key
  ;; is cited as zotero:<item key>
  (let* ((data (zotero-attr-ref it "data"))
         (meta (zotero-attr-ref it "meta"))
         (key (string-or-empty (zotero-attr-ref it "key")))
         (type (and data (zotero-attr-ref data "itemType")))
         (ck (and data (zotero-attr-ref data "citationKey"))))
    (and (string? type)
         (nin? type '("note" "attachment" "annotation"))
         (!= key "")
         (list (if (and (string? ck) (!= ck "")) (utf8->cork ck)
                   (string-append derived-prefix key))
               key
               (string-or-empty (zotero-attr-ref data "title"))
               (string-or-empty (zotero-attr-ref meta "creatorSummary"))
               (with d (string-or-empty (zotero-attr-ref meta "parsedDate"))
                 (if (>= (string-length d) 4) (substring d 0 4) d))
               (or (string->number
                    (string-or-empty (zotero-attr-ref it "version")))
                   0)))))

(tm-define (zotero-entry-key e) (first e))
(tm-define (zotero-entry-item e) (second e))
(tm-define (zotero-entry-title e) (third e))
(tm-define (zotero-entry-creators e) (fourth e))
(tm-define (zotero-entry-year e) (fifth e))
(tm-define (zotero-entry-version e) (sixth e))

(tm-define (zotero-search q . opt)
  (:synopsis "The items of the library matching @q (author, title, year)")
  ;; @q is in cork, as typed in TeXmacs; the options are the maximal number
  ;; of items (50 by default) and whether the request is interactive
  (let ((n (if (null? opt) 50 (car opt)))
        (interactive? (and (pair? opt) (pair? (cdr opt)) (cadr opt))))
    (list-filter
     (map item-entry
          (json-items
           (zotero-get (string-append "items/top?format=json&limit="
                                      (number->string n) "&q="
                                      (zotero-url-encode (cork->utf8 q)))
                       interactive?)))
     identity)))

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
      (list-filter (map zotero-entry-key (zotero-search prefix 50 #t))
                   (cut string-starts? <> prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resolving citation keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The entries found for keys, as long as the library does not change
(define resolved (make-ahash-table))
(define resolved-version #f)

(define (note-version! v)
  (set! last-version v)
  (when (!= v resolved-version)
    (set! resolved (make-ahash-table))
    (set! resolved-version v)))

(tm-define (zotero-find-key key)
  (:synopsis "The entry of the item with the citation key @key, or #f")
  (with cached (ahash-ref resolved key)
    (if cached (and (pair? cached) cached)
        (with e (if (zotero-derived-key? key)
                    (with item (string-drop key (string-length derived-prefix))
                      (and-with s (zotero-get (string-append "items/" item
                                                             "?format=json"))
                        (and-with e (list-find (map item-entry (json-items s))
                                               identity)
                          (and (== (zotero-entry-key e) key) e))))
                    ;; NOTE: the search also finds longer keys containing key
                    (list-find (zotero-search key 100)
                               (lambda (e) (== (zotero-entry-key e) key))))
          (when (zotero-ready?)
            (ahash-set! resolved key (or e 'none)))
          e))))

(tm-define (zotero-items-entries items)
  (:synopsis "The entries of the Zotero @items (item keys) which still exist")
  (append-map
   (lambda (l)
     (list-filter
      (map item-entry
           (json-items (zotero-get (string-append
                                    "items?format=json&itemKey="
                                    (string-recompose l ",")))))
      identity))
   (if (null? items) '() (chunks items 50))))

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
  ;; At most 50 items per request, as for the web API; the items cited as
  ;; zotero:<item key> are exported one by one, since Zotero gives them
  ;; keys of its own
  (let* ((format (get-preference "zotero export format"))
         (export (lambda (items)
                   (or (zotero-get (string-append
                                    "items?format=" format "&itemKey="
                                    (string-recompose items ",")))
                       "")))
         (plain (list-filter entries
                             (lambda (e) (not (zotero-derived-key?
                                               (zotero-entry-key e))))))
         (derived (list-filter entries
                               (lambda (e) (zotero-derived-key?
                                            (zotero-entry-key e))))))
    (apply string-append
           (append
            (map export
                 (if (null? plain) '()
                     (chunks (map zotero-entry-item plain) 50)))
            (map (lambda (e)
                   (rekey (export (list (zotero-entry-item e)))
                          (zotero-entry-key e)))
                 derived)))))

(tm-define (zotero-item-versions items)
  (:synopsis "The (item . version) of the @items which Zotero still has")
  ;; NOTE: the local API has no list of deleted items: an item which is
  ;; not returned has been deleted (or moved to the trash)
  (append-map
   (lambda (l)
     (with t (zotero-json (zotero-get (string-append
                                       "items?format=versions&itemKey="
                                       (string-recompose l ","))))
       (if (not (tm-func? t 'attr)) '()
           (let loop ((r (cdr t)) (acc '()))
             (if (or (null? r) (null? (cdr r))) (reverse acc)
                 (loop (cddr r)
                       (cons (cons (car r) (or (string->number (cadr r)) 0))
                             acc)))))))
   (if (null? items) '() (chunks items 50))))

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

(tm-define (zotero-record-items entries)
  (:synopsis "Remember with the document the Zotero items of its citations")
  ;; The pairs (key item) are kept in an attachment of the document, so that
  ;; a key renamed in Zotero can be found again
  (when (nnull? entries)
    (let* ((old (with t (get-attachment "zotero-items")
                  (if (tm-func? (tm->stree t) 'tuple)
                      (cdr (tm->stree t)) '())))
           (h (make-ahash-table)))
      (for (p old)
        (when (tm-func? p 'tuple 2) (ahash-set! h (cadr p) (caddr p))))
      (for (e entries)
        (ahash-set! h (zotero-entry-key e) (zotero-entry-item e)))
      (set-attachment "zotero-items"
                      (stree->tree
                       `(tuple ,@(map (lambda (x) `(tuple ,(car x) ,(cdr x)))
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
