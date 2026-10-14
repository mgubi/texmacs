
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
  (:use (csl csl-utils) (csl csl-files) (csl csl-style) (csl csl-data)
        (csl csl-process) (csl csl-cite) (csl csl-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; csl-style-name? and csl-available-styles are in (csl csl-files)

(define (document-style name)
  ;; a style next to the current document comes first; returns the style,
  ;; #f when there is no such style and the symbol invalid when its file
  ;; is no style which can be used
  (let* ((dir (and (url-rooted? (current-buffer))
                   (url-head (current-buffer))))
         (u (and dir (url-append dir (string-append name ".csl"))))
         (own? (and u (url-exists? u))))
    (or (if own? (csl-style-from-string name (string-load u))
            (csl-load-style name))
        (and (or own? (csl-style-file name)) 'invalid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The citations of the document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When a bibliography in a CSL style is present, the citation macros
;; (cite-csl in std-automatic.ts) write each citation to the auxiliary data
;; PREFIX-cites, as (tuple mode (tuple entry ...) nr note) where an entry
;; is a key or (tuple key details), nr is the number of the citation in the
;; document, note the number of the footnote which holds the citation (or
;; would hold it in a style of the class note) and the mode is
;;
;;   p   the citation as the style prints it: (Knuth, 1984)
;;   t   as part of the sentence: Knuth (1984)
;;   a   the authors only: Knuth
;;   y   without the authors and without the brackets: 1984
;;   p*  as p, with all the names (no et al.), for cite* of natbib
;;   t*  as t, with all the names, for cite-textual*
;;
;; The citations are rendered with the bibliography, which carries them as
;; bindings PREFIX-cite-MODE:SIGNATURE; the macros show these bindings. A
;; citation which reads otherwise where it stands has the binding
;; PREFIX-cite-MODE:SIGNATURE#NR.

(define locator-labels
  '(("p." . "page") ("pp." . "page") ("page" . "page") ("pages" . "page")
    ("ch." . "chapter") ("chap." . "chapter") ("chapter" . "chapter")
    ("sec." . "section") ("section" . "section") ("vol." . "volume")
    ("volume" . "volume") ("fig." . "figure") ("figure" . "figure")
    ("no." . "issue") ("l." . "line") ("ll." . "line") ("line" . "line")
    ("n." . "note") ("note" . "note") ("para." . "paragraph")
    ("pt." . "part") ("part" . "part") ("col." . "column")
    ("bk." . "book") ("v." . "verse") ("vv." . "verse")))

(define (page-token? s)
  ;; "12", "12a", "A12": at most one letter before the first digit
  (with l (csl-chars s)
    (and (nnull? l)
         (or (csl-digit? (car l))
             (and (nnull? (cdr l)) (csl-digit? (cadr l)))))))

(define (page-numbers? s)
  ;; are the details @s numbers of pages? "Lemma2" is not
  (with toks (csl-number-tokens s)
    (and toks (list-and (map (lambda (t) (or (in? t '("-" "," "&"))
                                             (page-token? t)))
                             toks)))))

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
          ((page-numbers? t) (list (cons 'locator t) (cons 'label "page")))
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
  ;; as the macros cite-csl-key and cite-detail make it: no key holds a
  ;; brace, and details which are not a plain text are written "*"
  (cond ((string? e) (string-append e ","))
        ((and (func? e 'tuple 2) (string? (cadr e)))
         (string-append (cadr e) "{"
                        (if (string? (caddr e)) (caddr e) "*") ","))
        (else #f)))

(define (rich-details? e)
  ;; the signature of such an entry does not tell its details
  (and (func? e 'tuple 2) (not (string? (caddr e)))))

(define (number-at c i)
  ;; the number in the field @i of the tuple @c, or #f
  (and (> (length c) i) (string? (list-ref c i))
       (string->number (list-ref c i))))

(define (document-citations prefix)
  ;; the citations of the document, in order:
  ;; ((mode signature cites nr own? note) ...), or #f for those which
  ;; cannot be understood; nr is the number which the macros gave to the
  ;; citation, own? tells that it needs a binding of its own and note is
  ;; the number of its footnote
  (let* ((aux (tree->stree (get-auxiliary (string-append prefix "-cites"))))
         (l (if (func? aux 'document) (cdr aux) '())))
    (map (lambda (c i)
           (let* ((ok? (and (func? c 'tuple) (<= 2 (length (cdr c)) 4)
                            (string? (cadr c))
                            (func? (caddr c) 'tuple)))
                  (entries (if ok? (cdr (caddr c)) '()))
                  (sigs (map entry-signature entries))
                  (cites (map entry->cite entries))
                  (nr (and ok? (number-at c 3))))
             (and ok? (nnull? entries) (list-and sigs) (list-and cites)
                  (list (cadr c)
                        (string-append (cadr c) ":"
                                       (apply string-append sigs))
                        cites
                        (or nr (+ i 1))
                        (list-or (map rich-details? entries))
                        (or (number-at c 4) nr (+ i 1))))))
         l (iota (length l)))))

(define (all-names cites)
  (map (lambda (c) (cons '(all-names . #t) c)) cites))

(define (render-citation proc mode cites)
  (cond ((== mode "t") (csl-textual-citation proc cites))
        ((== mode "t*") (csl-textual-citation proc (all-names cites)))
        ((== mode "p*") (csl-citation proc (all-names cites)))
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
  ;; the cites of a citation in the note number @nr with their positions;
  ;; @last holds the cites of the citation before
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
  ;; the rest, which goes to a footnote, with the mode n (t* and n* for
  ;; the mode t*). The positions count the footnotes in a note style, and
  ;; the citations in the other ones.
  (let* ((locale (csl-processor-locale proc))
         (first-notes (make-ahash-table))
         (last-notes (make-ahash-table))
         (done (make-ahash-table))
         (tm (lambda (x) (csl->texmacs x locale)))
         (render
          ;; a list of (mode . tree)
          (lambda (mode cites)
            (if (and note? (in? mode '("t" "t*")))
                (let* ((star? (== mode "t*"))
                       (parts (csl-textual-parts
                               proc (if star? (all-names cites) cites))))
                  (list (cons mode (tm (rt-join (map car parts) ", ")))
                        (cons (if star? "n*" "n")
                              (tm (rt-join (map cdr parts) "; ")))))
                (list (cons mode (tm (render-citation proc mode cites)))))))
         (bind
          ;; the values are quoted: the macros inside them are expanded
          ;; where the citation stands
          (lambda (sig suffix l)
            (with keys (string-drop sig (string-index sig #\:))
              (map (lambda (p)
                     `(set-binding ,(string-append prefix "-cite-" (car p)
                                                   keys suffix)
                                   (quote ,(cdr p))))
                   l)))))
    ;; the bindings are gathered in reverse order
    (let loop ((l (document-citations prefix)) (last '())
               (r (list `(set-binding ,(string-append prefix "-csl")
                                      ,(if note? "note" "true")))))
      (cond ((null? l) (reverse r))
            ((not (car l)) (loop (cdr l) '() r))
            (else
              (let* ((mode (car (car l)))
                     (sig (cadr (car l)))
                     (cites (caddr (car l)))
                     (nr (cadddr (car l)))
                     (own? (list-ref (car l) 4))
                     (note (if note? (list-ref (car l) 5) nr))
                     (old (and (not own?) (ahash-ref done sig)))
                     (new? (not (or own? old)))
                     (generic (or old (render mode cites)))
                     (placed? (in? mode '("p" "t" "p*" "t*")))
                     (cites* (if placed?
                                 (with-positions proc cites last note
                                                 first-notes last-notes)
                                 cites))
                     (moved? (and placed?
                                  (list-or (map (lambda (c)
                                                  (!= (csl-cite-ref
                                                       c 'position)
                                                      'first))
                                                cites*))))
                     (own (if moved? (render mode cites*) generic)))
                (when (not own?) (ahash-set! done sig generic))
                (loop (cdr l) (if placed? cites last)
                      (append
                       (reverse
                        (if (or own? (!= own generic))
                            (bind sig (string-append "#" (number->string nr))
                                  own)
                            '()))
                       (reverse (if new? (bind sig "" generic) '()))
                       r))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibliographies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-order items keys)
  ;; the items in the order of the citations @keys, the other ones after
  (let* ((by-id (make-ahash-table))
         (seen (make-ahash-table)))
    (for (i (reverse items)) (ahash-set! by-id (csl-item-id i) i))
    (let loop ((l keys) (r '()))
      (if (nnull? l)
          (with i (and (not (ahash-ref seen (car l)))
                       (ahash-ref by-id (car l)))
            (when i (ahash-set! seen (car l) #t))
            (loop (cdr l) (if i (cons i r) r)))
          (append (reverse r)
                  (list-filter items
                               (lambda (i)
                                 (not (ahash-ref seen (csl-item-id i))))))))))

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
  ;; citation, the code of a locale to use instead of the language of the
  ;; document, and #f for the entries alone, without the citations of the
  ;; document (a preview). A style which fails gives a message instead of
  ;; the bibliography, and the citations show as without a CSL style.
  (catch #t
    (lambda () (apply process-bibliography (cons* prefix style t opt)))
    (lambda (key . args)
      (string-append "Error: CSL style " (string-drop style 4) ": "
                     (if (symbol? key) (symbol->string key) "failed")))))

(define (process-bibliography prefix style t . opt)
  (let* ((name (string-drop style 4))
         (st (document-style name))
         (keys (if (and (nnull? opt) (list? (car opt)))
                   (list-filter (car opt) string?) '()))
         (forced (and (> (length opt) 1) (cadr opt)))
         (cites? (or (< (length opt) 3) (caddr opt))))
    (cond ((not st)
           (string-append "Error: CSL style " name " not found"))
          ((== st 'invalid)
           (string-append "Error: CSL style " name " is not valid"))
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
                   (bindings (if cites?
                                 (citation-bindings
                                  prefix proc
                                  (== (csl-style-ref st 'class) "note"))
                                 '()))
                   (hanging? (== (csl-bibliography-option
                                  proc 'hanging-indent "false")
                                 "true"))
                   (body
                    `(bib-list
                      ,(if affixes (longest plain) "")
                      (document
                        ;; the citations are kept when no entry is found
                        ,@(if (null? entries) `((concat ,@bindings)) '())
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
