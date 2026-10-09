
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-bib.scm
;; DESCRIPTION : bibliographies of TeXmacs documents in CSL styles
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A bibliography whose style is called csl-NAME is formatted by the CSL
;; style NAME.csl, which is sought next to the document, in
;; $TEXMACS_HOME_PATH/csl/styles and in $TEXMACS_PATH/misc/csl/styles.
;;
;; The result has the shape of the bibliographies of the other styles,
;;
;;   (bib-list largest (document (concat (bibitem* text) (label key) ...)))
;;
;; so that the citations and the converters work as before. The text of
;; bibitem* is what a citation of the entry shows. The macros
;; transform-bibitem and render-bibitem are redefined around the list so
;; that the labels look as the style wants them, or do not show.

(texmacs-module (csl csl-bib)
  (:use (csl csl-utils) (csl csl-style) (csl csl-data) (csl csl-process)
        (csl csl-cite) (csl csl-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (csl-style-name? style)
  (:synopsis "Is @style the name of a CSL style, csl-NAME?")
  (and (string? style) (string-starts? style "csl-")
       (> (string-length style) 4)))

(tm-define (csl-available-styles)
  (:synopsis "The names csl-NAME of the CSL styles which are installed")
  (let* ((files (append-map
                 (lambda (d)
                   (if (url-exists? d) (url-read-directory d "*.csl") '()))
                 (csl-directories "styles")))
         (names (map (lambda (u) (url->string (url-basename u))) files)))
    (map (cut string-append "csl-" <>)
         (list-remove-duplicates (csl-sort names string<?)))))

(define (document-style name)
  ;; a style next to the current document comes first
  (let* ((dir (and (url-rooted? (current-buffer))
                   (url-head (current-buffer))))
         (u (and dir (url-append dir (string-append name ".csl")))))
    (if (and u (url-exists? u))
        (csl-style-from-string name (string-load u))
        (csl-load-style name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The citations of the document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When a bibliography in a CSL style is present, the citation macros
;; (cite-csl in std-automatic.ts) write each citation to the auxiliary data
;; PREFIX-cites, as (tuple mode (tuple entry ...)) where an entry is a key
;; or (tuple key details) and the mode is
;;
;;   p   the citation as the style prints it: (Knuth, 1984)
;;   t   as part of the sentence: Knuth (1984)
;;   a   the authors only: Knuth
;;   y   without the authors and without the brackets: 1984
;;
;; The citations are rendered with the bibliography, which carries them as
;; bindings PREFIX-cite-MODE:SIGNATURE; the macros show these bindings.

(define locator-labels
  '(("p." . "page") ("pp." . "page") ("page" . "page") ("pages" . "page")
    ("ch." . "chapter") ("chap." . "chapter") ("chapter" . "chapter")
    ("sec." . "section") ("section" . "section") ("vol." . "volume")
    ("volume" . "volume") ("fig." . "figure") ("figure" . "figure")
    ("no." . "issue") ("l." . "line") ("ll." . "line") ("line" . "line")
    ("n." . "note") ("note" . "note") ("para." . "paragraph")
    ("pt." . "part") ("part" . "part") ("col." . "column")
    ("bk." . "book") ("v." . "verse") ("vv." . "verse")))

(define (parse-details s)
  ;; "pp. 3-5" -> ((locator . "3-5") (label . "page")); other text is a
  ;; suffix of the cite
  (let* ((t (string-trim-both s))
         (i (string-index t #\space))
         (head (and i (csl-locase (substring t 0 i))))
         (p (and head (assoc head locator-labels)))
         (rest (and i (string-trim-both (string-drop t i)))))
    (cond ((== t "") '())
          ((and p (!= rest ""))
           (list (cons 'locator rest) (cons 'label (cdr p))))
          ((csl-numeric? t) (list (cons 'locator t) (cons 'label "page")))
          (else (list (cons 'suffix (string-append ", " t)))))))

(define (entry->cite e)
  (cond ((string? e) (list (cons 'id e)))
        ((and (func? e 'tuple 2) (string? (cadr e)))
         (cons (cons 'id (cadr e))
               (if (string? (caddr e)) (parse-details (caddr e))
                   (list (cons 'suffix (list 'cat ", "
                                             (list 'raw (caddr e))))))))
        (else #f)))

(define (entry-signature e)
  ;; as the macros cite-csl-key and cite-detail make it
  (cond ((string? e) (string-append e ","))
        ((and (func? e 'tuple 2) (string? (cadr e)) (string? (caddr e)))
         (string-append (cadr e) "@" (caddr e) ","))
        (else #f)))

(define (document-citations prefix)
  ;; the citations of the document, in order: ((mode signature cites) ...)
  ;; or #f for those which cannot be understood
  (let* ((aux (tree->stree (get-auxiliary (string-append prefix "-cites"))))
         (l (if (func? aux 'document) (cdr aux) '())))
    (map (lambda (c)
           (let* ((ok? (and (func? c 'tuple 2) (string? (cadr c))
                            (func? (caddr c) 'tuple)))
                  (entries (if ok? (cdr (caddr c)) '()))
                  (sigs (map entry-signature entries))
                  (cites (map entry->cite entries)))
             (and ok? (nnull? entries) (list-and sigs) (list-and cites)
                  (list (cadr c)
                        (string-append (cadr c) ":"
                                       (apply string-append sigs))
                        cites))))
         l)))

(define (render-citation proc mode cites)
  (cond ((== mode "t") (csl-textual-citation proc cites))
        ((== mode "a")
         (csl-citation proc (map (lambda (c) (cons '(author-only . #t) c))
                                 cites)))
        ((== mode "y")
         (csl-citation proc (map (lambda (c) (cons '(suppress-author . #t) c))
                                 cites)
                       #t))
        (else (csl-citation proc cites))))

(define (cite-position cite before seen)
  ;; @before is the cite which precedes, or #f; @seen tells whether the
  ;; item was cited already
  (let* ((loc (csl-cite-ref cite 'locator))
         (same? (and before (== (csl-cite-ref before 'id)
                                (csl-cite-ref cite 'id))))
         (old (and same? (csl-cite-ref before 'locator))))
    (cond ((and same? (not loc) (not old)) 'ibid)
          ((and same? loc (== loc old)) 'ibid)
          ((and same? loc) 'ibid-with-locator)
          (seen 'subsequent)
          (else 'first))))

(define (with-positions proc cites last nr first-notes last-notes)
  ;; the cites of the citation number @nr with their positions; @last
  ;; holds the cites of the citation before
  (let* ((area (csl-style-ref (csl-processor-ref proc 'style) 'citation))
         (near (or (string->number (csl-attr area 'near-note-distance "5"))
                   5)))
    (let loop ((l cites) (before (and (== (length last) 1) (car last)))
               (r '()))
      (if (null? l) (reverse r)
          (let* ((c (car l))
                 (id (csl-cite-ref c 'id))
                 (first (ahash-ref first-notes id))
                 (recent (ahash-ref last-notes id))
                 (pos (cite-position c before first)))
            (when (not first) (ahash-set! first-notes id nr))
            (ahash-set! last-notes id nr)
            (loop (cdr l) c
                  (cons (append
                         (list (cons 'position pos))
                         (if first
                             (list (cons 'first-note (number->string first)))
                             '())
                         (if (and recent (<= (- nr recent) near))
                             '((near-note . #t)) '())
                         c)
                        r)))))))

(define (citation-bindings prefix proc note?)
  ;; the bindings which the citation macros look for: one for each
  ;; different citation, and one more for a citation which reads
  ;; otherwise where it stands (ibid., a short form). In a note style, a
  ;; citation in the text has two bindings: the names with the mode t and
  ;; the rest, which goes to a footnote, with the mode n.
  (let* ((locale (csl-processor-locale proc))
         (first-notes (make-ahash-table))
         (last-notes (make-ahash-table))
         (done (make-ahash-table))
         (tm (lambda (x) (csl->texmacs x locale)))
         (render
          ;; a list of (mode . tree)
          (lambda (mode cites)
            (if (and note? (== mode "t"))
                (with parts (csl-textual-parts proc cites)
                  (list (cons "t" (tm (rt-join (map car parts) ", ")))
                        (cons "n" (tm (rt-join (map cdr parts) "; ")))))
                (list (cons mode (tm (render-citation proc mode cites)))))))
         (bind
          ;; the values are quoted: the macros inside them are expanded
          ;; where the citation stands
          (lambda (sig suffix l)
            (map (lambda (p)
                   `(set-binding ,(string-append prefix "-cite-" (car p)
                                                 (string-drop sig 1) suffix)
                                 (quote ,(cdr p))))
                 l))))
    (let loop ((l (document-citations prefix)) (nr 1) (last '())
               (r (list `(set-binding ,(string-append prefix "-csl")
                                      ,(if note? "note" "true")))))
      (cond ((null? l) r)
            ((not (car l)) (loop (cdr l) (+ nr 1) '() r))
            (else
              (let* ((mode (car (car l)))
                     (sig (cadr (car l)))
                     (cites (caddr (car l)))
                     (new? (not (ahash-ref done sig)))
                     (generic (or (ahash-ref done sig) (render mode cites)))
                     (placed? (in? mode '("p" "t")))
                     (cites* (if placed?
                                 (with-positions proc cites last nr
                                                 first-notes last-notes)
                                 cites))
                     (moved? (and placed?
                                  (list-or (map (lambda (c)
                                                  (!= (csl-cite-ref
                                                       c 'position)
                                                      'first))
                                                cites*))))
                     (own (if moved? (render mode cites*) generic)))
                (ahash-set! done sig generic)
                (loop (cdr l) (+ nr 1) (if placed? cites last)
                      (append
                       r
                       (if new? (bind sig "" generic) '())
                       (if (!= own generic)
                           (bind sig (string-append "#" (number->string nr))
                                 own)
                           '())))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibliographies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-order items keys)
  ;; the items in the order of the citations @keys, the other ones after
  (let* ((cited (list-filter
                 (map (lambda (k)
                        (list-find items (lambda (i) (== (csl-item-id i) k))))
                      (list-remove-duplicates keys))
                 identity))
         (ids (map csl-item-id cited)))
    (append cited
            (list-filter items (lambda (i) (nin? (csl-item-id i) ids))))))

(define (split-around s what)
  ;; (prefix . suffix) of the first occurrence of @what in @s, or #f
  (with i (string-search-forwards what 0 s)
    (and (>= i 0) (!= what "")
         (cons (substring s 0 i)
               (substring s (+ i (string-length what)) (string-length s))))))

(define (hidden-labels hanging? body)
  `(with "transform-bibitem" (macro "body" ,(if hanging? '(space "1.5em") ""))
         "render-bibitem"
         (macro "text" (with "par-first"
                           (minus "1tmpt" (value "bibitem-width"))
                         (yes-indent)))
     ,body))

(define (shown-labels affixes body)
  `(with "transform-bibitem"
         (macro "body" (concat ,(car affixes) (arg "body") ,(cdr affixes)
                               " "))
     ,body))

(define (longest l)
  (let loop ((l l) (r ""))
    (cond ((null? l) r)
          ((> (string-length (car l)) (string-length r)) (loop (cdr l) (car l)))
          (else (loop (cdr l) r)))))

(define (bib-entry prefix locale text label bindings body)
  `(concat (bibitem* ,text)
           (label ,(string-append prefix "-" label))
           ,@bindings
           ,(csl->texmacs body locale)))

(tm-define (csl-bib-process prefix style t . opt)
  (:synopsis "The bibliography of the BibTeX entries @t in the CSL style")
  ;; the optional arguments are the list of the keys in the order of
  ;; citation, and the code of a locale to use instead of the language of
  ;; the document
  (let* ((name (string-drop style 4))
         (st (document-style name))
         (keys (if (and (nnull? opt) (list? (car opt)))
                   (list-filter (car opt) string?) '()))
         (forced (and (> (length opt) 1) (cadr opt))))
    (cond ((not st)
           (string-append "Error: CSL style " name " not found"))
          ((not (csl-style-ref st 'bibliography))
           (string-append "Error: CSL style " name " has no bibliography"))
          (else
            (let* ((lang (csl-style-ref st 'default-locale))
                   (doc-lang (or forced (csl-language->locale
                                         (get-document-language))))
                   (items (in-order (csl-bib->items t) keys))
                   (proc (csl-make-processor st (or lang doc-lang) items))
                   (locale (csl-processor-locale proc))
                   (entries (csl-bibliography proc))
                   (numeric? (csl-processor-ref proc 'numeric?))
                   (cites (map (lambda (e) (csl-cite-text proc (car e)))
                               entries))
                   (plain (map (lambda (e c)
                                 (if numeric? (cadr e) (rt->string c)))
                               entries cites))
                   (labels (map (lambda (e) (rt->string (caddr e))) entries))
                   ;; the labels are shown when they are the text of the
                   ;; citations between a prefix and a suffix
                   (affixes (and (nnull? entries)
                                 (list-and (map (lambda (e) (caddr e))
                                                entries))
                                 (list-and (map split-around labels plain))
                                 (split-around (car labels) (car plain))))
                   (texts (if (or numeric? affixes) plain
                              (map (cut csl->texmacs <> locale) cites)))
                   (bindings (citation-bindings
                              prefix proc
                              (== (csl-style-ref st 'class) "note")))
                   (hanging? (== (csl-bibliography-option
                                  proc 'hanging-indent "false")
                                 "true"))
                   (body
                    `(bib-list
                      ,(if affixes (longest plain) "")
                      (document
                        ,@(map (lambda (e text i)
                                 (bib-entry
                                  prefix locale text (car e)
                                  ;; the first entry carries the citations
                                  (if (== i 0) bindings '())
                                  (if (or affixes (not (caddr e)))
                                      (cadddr e)
                                      (rt-cat (caddr e) " " (cadddr e)))))
                               entries texts (iota (length entries)))))))
              (if affixes (shown-labels affixes body)
                  (hidden-labels hanging? body)))))))
