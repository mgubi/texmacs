
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
        (csl csl-output)))

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

(define (bib-entry prefix locale text label body)
  `(concat (bibitem* ,text)
           (label ,(string-append prefix "-" label))
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
                   (hanging? (== (csl-bibliography-option
                                  proc 'hanging-indent "false")
                                 "true"))
                   (body
                    `(bib-list
                      ,(if affixes (longest plain) "")
                      (document
                        ,@(map (lambda (e text)
                                 (bib-entry
                                  prefix locale text (car e)
                                  (if (or affixes (not (caddr e)))
                                      (cadddr e)
                                      (rt-cat (caddr e) " " (cadddr e)))))
                               entries texts)))))
              (if affixes (shown-labels affixes body)
                  (hidden-labels hanging? body)))))))
