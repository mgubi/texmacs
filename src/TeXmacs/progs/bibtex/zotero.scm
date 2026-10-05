
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
;; Reading needs no key.
;;
;; The citations use the citation keys of Zotero (the "citationKey" field,
;; which Zotero fills since version 7, or Better BibTeX), and the
;; bibliography of a document is a BibTeX file which Zotero exports with the
;; same keys, so that the usual bibliography tools of TeXmacs apply.

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

(tm-define (zotero-request path)
  (:synopsis "Ask Zotero for @path; return (status body)")
  ;; The status is the HTTP status, or 0 when Zotero cannot be reached;
  ;; the body is the answer of Zotero, in utf8
  (let* ((url (string-append (get-preference "zotero server")
                             "/api/users/0/" path))
         (cmd (list "curl" "--silent" "--max-time" "20"
                    "--header" "Zotero-API-Version: 3"
                    "--write-out" "\n%{http_code}" url))
         (ret (evaluate-system cmd '() '() '(1 2)))
         (out (cadr ret))
         (pos (string-search-backwards "\n" (string-length out) out)))
    (if (< pos 0)
        (list 0 "")
        (list (or (string->number (substring out (+ pos 1)
                                             (string-length out)))
                  0)
              (substring out 0 pos)))))

(tm-define (zotero-status)
  (:synopsis "One of ready, disabled (local API not enabled), not-running")
  (with st (car (zotero-request "items/top?limit=1&format=keys"))
    (cond ((== st 200) 'ready)
          ((== st 403) 'disabled)
          ((== st 0) 'not-running)
          (else 'error))))

(tm-define (zotero-status-message st)
  (cond ((== st 'disabled)
         (string-append "Zotero refuses the request: enable \"Allow other "
                        "applications on this computer to communicate with "
                        "Zotero\" in Settings -> Advanced"))
        ((== st 'not-running) "Zotero is not running")
        ((== st 'ready) "Zotero is ready")
        (else "Zotero answered with an error")))

(define (zotero-get path)
  ;; The body of the answer to @path, or #f (with a message)
  (with (st body) (zotero-request path)
    (if (== st 200) body
        (begin
          (set-message (zotero-status-message
                        (cond ((== st 403) 'disabled)
                              ((== st 0) 'not-running)
                              (else 'error)))
                       "Zotero")
          #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; json->tree gives objects as (attr key value ...) and arrays as (tuple ...),
;; with the strings in utf8

(define (attr-ref t key)
  (and (tm-func? t 'attr)
       (let loop ((l (cdr t)))
         (cond ((or (null? l) (null? (cdr l))) #f)
               ((== (car l) key) (cadr l))
               (else (loop (cddr l)))))))

(define (json-items s)
  ;; The items in the answer @s of Zotero (a json array), as strees
  (with t (and s (tree->stree (json->tree s)))
    (if (tm-func? t 'tuple) (cdr t) '())))

(define (string-or-empty x)
  ;; the utf8 string @x, in cork
  (if (string? x) (utf8->cork x) ""))

(define (item-entry it)
  ;; (citation-key item-key title creators year), in cork, or #f for an
  ;; item without citation key (a note or an attachment)
  (let* ((data (attr-ref it "data"))
         (meta (attr-ref it "meta"))
         (ck (and data (attr-ref data "citationKey"))))
    (and (string? ck) (!= ck "")
         (list (utf8->cork ck)
               (string-or-empty (attr-ref it "key"))
               (string-or-empty (attr-ref data "title"))
               (string-or-empty (attr-ref meta "creatorSummary"))
               (with d (string-or-empty (attr-ref meta "parsedDate"))
                 (if (>= (string-length d) 4) (substring d 0 4) d))))))

(tm-define (zotero-entry-key e) (first e))
(tm-define (zotero-entry-item e) (second e))
(tm-define (zotero-entry-title e) (third e))
(tm-define (zotero-entry-creators e) (fourth e))
(tm-define (zotero-entry-year e) (fifth e))

(tm-define (zotero-search q . opt-limit)
  (:synopsis "The items of the library matching @q (author, title, year)")
  ;; @q is in cork, as typed in TeXmacs
  (with n (if (null? opt-limit) 50 (car opt-limit))
    (list-filter
     (map item-entry
          (json-items
           (zotero-get (string-append "items/top?format=json&limit="
                                      (number->string n) "&q="
                                      (zotero-url-encode (cork->utf8 q))))))
     identity)))

(tm-define (zotero-find-key key)
  (:synopsis "The entry of the item with the citation key @key, or #f")
  ;; NOTE: the search also finds longer keys which contain @key
  (list-find (zotero-search key 100)
             (lambda (e) (== (zotero-entry-key e) key))))

(define (chunks l n)
  (if (<= (length l) n) (list l)
      (cons (sublist l 0 n) (chunks (sublist l n (length l)) n))))

(tm-define (zotero-export items)
  (:synopsis "The BibTeX of the Zotero @items (keys of items), in utf8")
  ;; At most 50 items per request, as for the web API
  (with format (get-preference "zotero export format")
    (apply string-append
           (map (lambda (l)
                  (or (zotero-get (string-append
                                   "items?format=" format "&itemKey="
                                   (string-recompose l ",")))
                      ""))
                (if (null? items) '() (chunks items 50))))))

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

(tm-define (zotero-write-bibliography keys file)
  (:synopsis "Write the BibTeX of the Zotero items with @keys to @file")
  ;; Returns the keys which are not in the library of Zotero
  (let* ((entries (map (lambda (k) (cons k (zotero-find-key k))) keys))
         (found (list-filter entries cdr))
         (missing (map car (list-filter entries (lambda (x) (not (cdr x))))))
         (bib (zotero-export (map (lambda (x) (zotero-entry-item (cdr x)))
                                  found))))
    (string-save (string-append
                  "% Exported from Zotero by TeXmacs; this file is "
                  "replaced by Document -> Bibliography -> Update from Zotero\n"
                  bib)
                 file)
    missing))

(tm-define (zotero-update-bibliography)
  (:synopsis "Export the cited items from Zotero and update the bibliography")
  (let* ((u (current-buffer))
         (doc (tree->stree (buffer-tree)))
         (file (zotero-bibliography-file u doc))
         (st (zotero-status)))
    (cond ((not file)
           (set-message "Insert a bibliography first (Insert -> Automatic)"
                        "Zotero"))
          ((url-rooted-tmfs? file)
           (set-message "Save the document first" "Zotero"))
          ((!= st 'ready)
           (set-message (zotero-status-message st) "Zotero"))
          (else
            (let* ((keys (zotero-citations doc))
                   (missing (zotero-write-bibliography keys file)))
              (update-document "bibliography")
              (set-message
               (if (null? missing)
                   (string-append "Exported " (number->string (length keys))
                                  " citations to "
                                  (url->system (url-tail file)))
                   (string-append "Not in Zotero: "
                                  (string-recompose missing ", ")))
               "Zotero"))))))
