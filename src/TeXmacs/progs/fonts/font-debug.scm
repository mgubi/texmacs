
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-debug.scm
;; DESCRIPTION : tools to see how the font system renders the text
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The font inspector is the entry point of these tools: its window turns
;; the colouring on and off and opens the font report of the document being
;; edited.
;;
;; A font is resolved character by character: a glyph may come from the
;; requested font, from a family a font rule names, from another family
;; found by feature distance, from an emulation (a virtual font or a derived
;; font), or from the error font. These tools show which, without slowing
;; the typesetter down: the colouring of glyphs by origin acts on drawing
;; only, under the debug switch "fonts", and the inspector asks the editor
;; about one glyph at a time, only while its window is open.

(texmacs-module (fonts font-debug)
  (:use (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colouring the glyphs by origin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define origin-colours
  '(("font" "black" "the requested font")
    ("rule" "#005ac8" "a family named by a font rule")
    ("fallback" "#e16e00" "another family, found by feature distance")
    ("emulated" "#00963c" "an emulation: a virtual or derived font")
    ("error" "red" "no font: the name of the character")))

(tm-define (font-colours-by-origin?)
  (debug-get "fonts"))

(tm-define (toggle-font-colours-by-origin)
  (:synopsis "Draw each glyph in the colour of the font it comes from")
  (:check-mark "v" font-colours-by-origin?)
  (debug-set "fonts" (not (debug-get "fonts")))
  (refresh-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The glyph inspector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define inspector-open? #f)
(define inspector-follows-mouse? #f)
(define inspector-frozen? #f)
(define inspector-pending? #f)
(define inspector-info '())
(define inspector-master #f)  ; the document edited when last looked at

(define (editor-document? u)
  ;; a document of the user, not a buffer of a widget nor a report
  (with s (url->string u)
    (not (or (string-starts? s "tmfs://aux/")
             (string-starts? s "tmfs://fontdbg/")))))

(define (inspector-note-document)
  (with u (current-buffer)
    (when (and u (editor-document? u))
      (set! inspector-master u))))

(define (inspector-active?)
  (and inspector-open? (not inspector-frozen?)))

(define (info-ref info key)
  (with l (assoc key info)
    (if l (cadr l) "")))

(define (info-value v)
  (cond ((string? v) v)
        ((and (pair? v) (== (car v) 'tuple))
         (string-append "(" (string-recompose (map info-value (cdr v)) " ")
                        ")"))
        (else (object->string v))))

(define (inspector-query)
  ;; the glyph at the cursor of the edited document: in a font report or
  ;; another auxiliary buffer, the one of the document it was opened from
  (cond ((editor-document? (current-buffer))
         (font-debug-info inspector-follows-mouse?))
        ((and inspector-master (buffer-exists? inspector-master))
         (font-debug-info-of inspector-master inspector-follows-mouse?))
        (else (tree 'tuple))))

(define (inspector-read)
  (with t (tree->stree (inspector-query))
    (if (and (pair? t) (== (car t) 'tuple))
        (map (lambda (x) (if (and (pair? x) (== (car x) 'tuple)) (cdr x) x))
             (cdr t))
        '())))

(define (origin-colour origin)
  (with l (assoc origin origin-colours)
    (if l (cadr l) "black")))

(define (inspector-row key label)
  (with v (info-ref inspector-info key)
    (if (== v "") '()
        (list `(row (cell ,label) (cell (verbatim ,(info-value v))))))))

;; The inspector is a debugging aid which stays open next to the document:
;; it is set small, so as to take as little room as it can
(define (inspector-small doc)
  `(with "font-base-size" "8" "par-sep" "0.1fn" ,doc))

(define (inspector-document)
  (inspector-small (inspector-document*)))

(define (inspector-document*)
  (if (null? inspector-info)
      `(document
         ,(if inspector-follows-mouse?
              "Move the mouse over a character."
              "Put the cursor after a character."))
      (let* ((origin (info-ref inspector-info "origin"))
             (rows (append
                     (inspector-row "char" "character")
                     (inspector-row "origin" "comes from")
                     (inspector-row "subfont-name" "font")
                     (inspector-row "spec" "route")
                     (inspector-row "rewritten" "drawn as")
                     (inspector-row "opentype-math" "OpenType math")
                     (inspector-row "math-type" "math type")
                     (inspector-row "family" "family")
                     (inspector-row "variant" "variant")
                     (inspector-row "series" "series")
                     (inspector-row "shape" "shape")
                     (inspector-row "font" "smart font")
                     (inspector-row "box" "box")
                     (inspector-row "box-type" "box type"))))
        `(document
           (with "color" ,(origin-colour origin)
             (strong ,(string-append "Origin: " origin)))
           (tabular
             (tformat (twith "table-width" "1par")
                      (cwith "1" "-1" "1" "-1" "cell-lsep" "0.2em")
                      (cwith "1" "-1" "1" "-1" "cell-rsep" "0.2em")
                      (cwith "1" "-1" "1" "-1" "cell-tsep" "0.05em")
                      (cwith "1" "-1" "1" "-1" "cell-bsep" "0.05em")
                      (cwith "1" "-1" "1" "1" "cell-width" "6.5em")
                      (cwith "1" "-1" "1" "1" "cell-hmode" "exact")
                      (table ,@rows)))))))

(define (inspector-legend)
  ;; one paragraph rather than a line for each origin
  (inspector-small
    `(document
       (concat
         ,@(list-intersperse
             (map (lambda (l)
                    `(concat (with "color" ,(cadr l) (strong ,(car l))) ": "
                             ,(caddr l)))
                  origin-colours)
             ";  ")))))

(define (inspector-update)
  (set! inspector-pending? #f)
  (inspector-note-document)
  (when (inspector-active?)
    (set! inspector-info (inspector-read))
    (refresh-now "font-inspector")))

(define (inspector-schedule)
  (when (and (inspector-active?) (not inspector-pending?))
    (set! inspector-pending? #t)
    (delayed
      (:idle 100)
      (inspector-update))))

(tm-define (notify-cursor-moved status)
  (:require (and inspector-open? (not inspector-follows-mouse?)))
  (former status)
  (when (editor-document? (current-buffer))
    (inspector-note-document)
    (inspector-schedule)))

(tm-define (mouse-event key x y mods time data)
  (:require (and inspector-open? inspector-follows-mouse?))
  (former key x y mods time data)
  (when (and (== key "move") (editor-document? (current-buffer)))
    (inspector-note-document)
    (inspector-schedule)))

(tm-widget (font-inspector-widget)
  (padded
    (hlist
      (toggle (begin (set! inspector-follows-mouse? answer)
                     (inspector-update))
              inspector-follows-mouse?)
      // (text "Follow mouse") >>>
      (toggle (begin (set! inspector-frozen? answer)
                     (inspector-update))
              inspector-frozen?)
      // (text "Freeze") >>>
      (toggle (toggle-font-colours-by-origin)
              (font-colours-by-origin?))
      // (text "Colour by origin"))
    ===
    (explicit-buttons
      ("Font report" (open-font-report-of inspector-master)) >>)
    ===
    (refreshable "font-inspector"
      (resize "360px" "200px"
        (texmacs-output (inspector-document) '(style "generic"))))
    ===
    (resize "360px" "72px"
      (texmacs-output (inspector-legend) '(style "generic")))))

(tm-define (open-font-inspector)
  (:synopsis "Open a window which says where the glyph at the cursor comes from")
  (when (not inspector-open?)
    (set! inspector-open? #t)
    (inspector-note-document)
    (set! inspector-info (inspector-read))
    ;; as top-window does, but the window stays above the editor windows
    (let* ((win (alt-window-handle))
           (quit (object->command
                   (lambda ()
                     (set! inspector-open? #f)
                     ;; the colouring belongs to the inspector: it stops
                     ;; with it
                     (when (font-colours-by-origin?)
                       (toggle-font-colours-by-origin))
                     (alt-window-delete win))))
           (wid (make-menu-widget* (list 'vertical (font-inspector-widget))
                                   0)))
      (alt-window-create-quit win wid (translate "Font inspector") quit)
      (alt-window-set-on-top win #t)
      (alt-window-show win))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The font report of a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An entry of font-debug-report is
;;   (origin char family subfont pdf variant series shape count)
;; for every distinct character of the typeset document, read from the boxes
;; the typesetter made; the report is a document of its own, attached to its
;; master like the bibliography viewer, and regenerated when it is reopened.

(define (report-entries master)
  (with t (with-buffer master (tree->stree (font-debug-report)))
    (if (and (pair? t) (== (car t) 'tuple))
        (map (lambda (e) (if (pair? e) (cdr e) e)) (cdr t))
        '())))

(define (entry-origin e) (list-ref e 0))
(define (entry-char e) (list-ref e 1))
(define (entry-family e) (list-ref e 2))
(define (entry-subfont e) (list-ref e 3))
(define (entry-pdf e) (list-ref e 4))
(define (entry-shape e) (list-ref e 7))
(define (entry-count e) (string->number (list-ref e 8)))

(define (strip-size s)
  ;; "FiraMath-Regular10@600" -> "FiraMath-Regular"
  (with i (string-search-forwards "@" 0 s)
    (if (< i 0) s
        (let loop ((j i))
          (if (and (> j 0) (char-numeric? (string-ref s (- j 1))))
              (loop (- j 1))
              (substring s 0 j))))))

(define (short-font-name s)
  ;; "unicode:FiraMath-Regular10@600#enhance-emu-fundamental10@600#..."
  ;; -> "FiraMath-Regular + emu-fundamental + ..."
  (let* ((parts (string-tokenize-by-char s #\#))
         (clean (lambda (p)
                  (let* ((p (if (string-starts? p "unicode:")
                                (substring p 8 (string-length p)) p))
                         (p (if (string-starts? p "enhance-")
                                (substring p 8 (string-length p)) p))
                         (p (if (string-starts? p "virtual-")
                                (substring p 8 (string-length p)) p)))
                    (strip-size p)))))
    (string-recompose (map clean parts) " + ")))

(define (escaped-name c)
  ;; the internal name of a character, shown as text rather than drawn
  (string-replace (string-replace c "<" "<less>") ">" "<gtr>"))

(define (entry-glyph e)
  ;; the character as the document shows it, in its family
  (let* ((c (entry-char e))
         (sh (entry-shape e)))
    (if (string-starts? sh "math")
        `(with "font" ,(entry-family e) (math ,c))
        `(with "font" ,(entry-family e) "font-shape"
               ,(if (== sh "") "right" sh) ,c))))

(define (sum l) (apply + l))

(define (report-summary entries)
  (let* ((families (list-filter
                     (list-remove-duplicates (map entry-family entries))
                     (lambda (f) (!= f ""))))
         (origins (map car origin-colours))
         (count (lambda (fam o)
                  (sum (map entry-count
                            (list-filter entries
                                         (lambda (e)
                                           (and (== (entry-family e) fam)
                                                (== (entry-origin e) o)))))))))
    `(tabular
       (tformat (cwith "1" "1" "1" "-1" "cell-bborder" "1ln")
                (table
                  (row (cell (em "family"))
                       ,@(map (lambda (o) `(cell (em ,o))) origins))
                  ,@(map (lambda (fam)
                           `(row (cell ,fam)
                                 ,@(map (lambda (o)
                                          `(cell ,(number->string
                                                   (count fam o))))
                                        origins)))
                         families))))))

(define (report-section entries origin title)
  (let* ((l (list-filter entries (lambda (e) (== (entry-origin e) origin))))
         (s (sort l (lambda (a b) (> (entry-count a) (entry-count b))))))
    (if (null? s) '()
        `((section ,title)
          (tabular
            (tformat (cwith "1" "1" "1" "-1" "cell-bborder" "1ln")
                     (table
                       (row (cell (em "glyph")) (cell (em "character"))
                            (cell (em "family")) (cell (em "drawn with"))
                            (cell (em "times"))
                            ,@(if (== origin "emulated")
                                  '((cell (em "in PDF"))) '()))
                       ,@(map (lambda (e)
                                `(row (cell (with "color"
                                                  ,(origin-colour origin)
                                              ,(entry-glyph e)))
                                      (cell (verbatim ,(escaped-name
                                                        (entry-char e))))
                                      (cell ,(entry-family e))
                                      (cell ,(short-font-name
                                               (entry-subfont e)))
                                      (cell ,(number->string (entry-count e)))
                                      ,@(if (== origin "emulated")
                                            `((cell ,(entry-pdf e))) '())))
                              s))))))))

(define (report-document master)
  (let* ((entries (report-entries master))
         (total (sum (map entry-count entries))))
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "generic"))
       (body
         (document
           (doc-data (doc-title "Font report"))
           ,(string-append "The characters of "
                           (url->system (url-tail master))
                           ", as typeset now, by the route their font took. "
                           (number->string total) " characters in all.")
           ,(inspector-legend)
           (section "Summary")
           ,(report-summary entries)
           ,@(report-section entries "emulated" "Emulated characters")
           ,@(report-section entries "fallback" "Characters from another family")
           ,@(report-section entries "rule" "Characters from a family of a font rule")
           ,@(report-section entries "error" "Characters no font has")
           ,@(report-section entries "unresolved" "Characters not yet routed"))))))

(tmfs-load-handler (fontdbg name)
  (report-document (tmfs-string->url name)))

(tmfs-master-handler (fontdbg name)
  (tmfs-string->url name))

(tmfs-title-handler (fontdbg name doc)
  (string-append (url->system (url-tail (tmfs-string->url name)))
                 " - Font report"))

(tmfs-permission-handler (fontdbg name type)
  (== type "read"))

(define (font-report-url master)
  (string-append "tmfs://fontdbg/" (url->tmfs-string master)))

(define (open-font-report-of master)
  (when master
    (with u (font-report-url master)
      (if (buffer->window u)
          (with-buffer u (revert-buffer-revert))
          (load-buffer-in-new-window u)))))

(tm-define (open-font-report)
  (:synopsis "Open a document which reports the route of every character")
  (with master (current-buffer)
    (open-font-report-of
      (if (string-starts? (url->string master) "tmfs://fontdbg/")
          (buffer-get-master master)
          master))))
