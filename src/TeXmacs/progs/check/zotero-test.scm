
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

;; The suite checks bibtex/zotero.scm and the insertion of citations of
;; bibtex/zotero-widgets.scm without Zotero: zotero-request is replaced by
;; a fake server, which answers from a small library and records the
;; requests. The real local API is used by the same requests (see the
;; comments of zotero.scm).

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

;; (item-key citation-key title creators date)
;; NOTE: the strings are in utf8, as Zotero sends them
(define fake-library
  `(("AAAA1111" "smith2020" "On gravity" "Smith" "2020-03")
    ("BBBB2222" "smith2020a" "On gravity, again" "Smith and Jones" "2020")
    ("CCCC3333" "muller2019" ,(cork->utf8 "Caf<#E9> physics")
     ,(cork->utf8 "M<#FC>ller et al.") "2019")
    ("DDDD4444" "" "A note" "" "")))

(define (json-item x)
  (with (key ck title creators date) x
    (string-append
     "{\"key\": \"" key "\", \"version\": 1,"
     " \"meta\": {\"creatorSummary\": \"" creators "\","
     " \"parsedDate\": \"" date "\"},"
     " \"data\": {\"key\": \"" key "\", \"itemType\": \"journalArticle\","
     " \"title\": \"" title "\""
     (if (== ck "") "" (string-append ", \"citationKey\": \"" ck "\""))
     "}}")))

(define (json-items l)
  (string-append "[" (string-recompose (map json-item l) ", ") "]"))

(define (query-ref path name)
  ;; the value of the parameter @name of the query of @path, decoded
  (with l (string-decompose (cadr (string-decompose path "?")) "&")
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
                               (string-contains? (locase-all (cadddr x)) q)))))))
        ((string-starts? path "items?format=")
         (apply string-append
                (map (lambda (k)
                       (with x (assoc k fake-library)
                         (string-append "\n@article{" (cadr x) ",\n"
                                        "\ttitle = {" (caddr x) "},\n}\n")))
                     (string-decompose (query-ref path "itemKey") ","))))
        (else "")))

(tm-define (zotero-request path)
  (:require fake?)
  (set! fake-requests (cons path fake-requests))
  (if (== fake-status 200)
      (list 200 (fake-answer path))
      (list fake-status "")))

(define (with-fake thunk)
  (set! fake? #t)
  (set! fake-status 200)
  (set! fake-requests '())
  (with r (check-run thunk)
    (set! fake? #f)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requests and status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-requests)
  (check-group "requests")
  (check= (zotero-url-encode "abc-_.~") "abc-_.~")
  (check= (zotero-url-encode "a b&c=d") "a%20b%26c%3Dd")
  (check= (zotero-url-encode (cork->utf8 "M<#FC>ller")) "M%C3%BCller")
  (with-fake
    (lambda ()
      (check= (zotero-status) 'ready)
      (set! fake-status 403)
      (check= (zotero-status) 'disabled)
      (check-true (string-contains? (zotero-status-message 'disabled)
                                    "Allow other applications"))
      (set! fake-status 0)
      (check= (zotero-status) 'not-running)
      (set! fake-status 500)
      (check= (zotero-status) 'error)
      (check= (zotero-search "gravity") '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-search)
  (check-group "search")
  (with-fake
    (lambda ()
      (with l (zotero-search "gravity")
        (check= (map zotero-entry-key l) '("smith2020" "smith2020a"))
        (check= (map zotero-entry-item l) '("AAAA1111" "BBBB2222"))
        (check= (map zotero-entry-year l) '("2020" "2020"))
        (check= (zotero-entry-creators (cadr l)) "Smith and Jones")
        (check= (zotero-entry-label (car l))
                "Smith (2020): On gravity  [smith2020]"))
      ;; the query is sent in utf8, the texts come back in cork
      (with l (zotero-search "M<#FC>ller")
        (check= (map zotero-entry-key l) '("muller2019"))
        (check= (zotero-entry-title (car l))
                (utf8->cork (cork->utf8 "Caf<#E9> physics")))
        (check= (zotero-entry-creators (car l))
                (utf8->cork (cork->utf8 "M<#FC>ller et al."))))
      (check-true (string-contains? (car fake-requests) "q=M%C3%BCller"))
      ;; items without citation key (notes) are not offered
      (check= (zotero-search "note") '())
      ;; a key is found exactly, not as the start of a longer one
      (check= (zotero-entry-item (zotero-find-key "smith2020")) "AAAA1111")
      (check= (zotero-entry-item (zotero-find-key "smith2020a")) "BBBB2222")
      (check-false (zotero-find-key "smith"))
      (check-false (zotero-find-key "nobody2000")))))

(define (test-export)
  (check-group "export")
  (with-fake
    (lambda ()
      (check= (zotero-export '()) "")
      (check-true (string-contains? (zotero-export '("AAAA1111" "CCCC3333"))
                                    "@article{muller2019,"))
      (check-true (string-contains? (car fake-requests)
                                    "itemKey=AAAA1111,CCCC3333"))
      ;; at most 50 items per request
      (set! fake-requests '())
      (zotero-export (map (lambda (i) "AAAA1111") (.. 0 120)))
      (check= (length fake-requests) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citations and bibliography of a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define zotero-dir
  (string-append (url->system (url-temp-dir)) "/zotero-test"))

(define (test-document)
  (check-group "document")
  (let ((doc '(document
               (concat "See " (cite "smith2020" "muller2019") ".")
               (cite-detail "smith2020a" "p. 3")
               (with "font-shape" "italic" (nocite "smith2020"))
               (cite-textual "")
               (bibliography "bib" "tm-plain" "refs" (document ""))))
        (u (system->url (string-append zotero-dir "/paper.tm"))))
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
        (with f (system->url (string-append zotero-dir "/refs.bib"))
          (check= (zotero-write-bibliography
                   '("smith2020" "nobody2000" "muller2019") f)
                  '("nobody2000"))
          (with s (string-load f)
            (check-true (string-starts? s "% Exported from Zotero"))
            (check-true (string-contains? s "@article{smith2020,"))
            (check-true (string-contains? s "@article{muller2019,"))
            (check-false (string-contains? s "smith2020a")))
          (system-remove f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-buffer-body doc thunk)
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (go-end)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
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
      (go-start)
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
  (test-requests)
  (test-search)
  (test-export)
  (test-document)
  (test-insert)
  (eval-system (string-append "rm -rf '" zotero-dir "'"))
  (check-end))
