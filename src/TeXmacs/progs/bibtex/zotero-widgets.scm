
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

(define search-query "")
(define search-results '())
(define search-selected '())
(define search-message "")

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

(define (search-labels)
  (map zotero-entry-label search-results))

(define (selected-keys)
  (map zotero-entry-key
       (list-filter search-results
                    (lambda (e) (in? (zotero-entry-label e) search-selected)))))

(define (search-now q)
  (set! search-query q)
  (set! search-selected '())
  (with st (zotero-status)
    (if (!= st 'ready)
        (begin
          (set! search-results '())
          (set! search-message (zotero-status-message st)))
        (begin
          (set! search-results
                (if (== (tm-string-trim-both q) "") '() (zotero-search q)))
          (set! search-message
                (cond ((== (tm-string-trim-both q) "")
                       "Type authors, words of the title or a year")
                      ((null? search-results) "Nothing found")
                      ((== (length search-results) 1) "1 item")
                      (else (string-append (number->string
                                            (length search-results))
                                           " items")))))))
  (refresh-now "zotero-results"))

(tm-widget ((zotero-search-widget) cmd)
  (padded
    (hlist
      (text "Search Zotero:") // //
      (input (when answer (search-now answer))
             "string" (list search-query) "40em"))
    ===
    (refreshable "zotero-results"
      (hlist (text search-message) >>)
      ===
      (resize "600px" "300px"
        (scrollable
          (choices (set! search-selected answer)
                   (search-labels) search-selected))))
    ===
    (bottom-buttons >>
      ("Cancel" (cmd '())) // //
      ("Cite" (cmd (selected-keys))))))

(tm-define (open-zotero-search)
  (:synopsis "Search the Zotero library and cite the chosen items")
  (:interactive #t)
  (search-now "")
  (dialogue-window (zotero-search-widget) zotero-insert-citation
                   "Cite from Zotero"))
