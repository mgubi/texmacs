
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-tools.scm
;; DESCRIPTION : Widgets for text, paragraph and page properties
;; COPYRIGHT   : (C) 2013-2021  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-tools)
  (:use (generic format-widgets)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (place-get-env win var mode)
  (or (with-place win
        (cond ((== mode :here)
               (get-env var))
              ((== mode :paragraph)
               (get-env var))
              ((== mode :global)
               (get-init var))
              (else "?")))
      "?"))

(tm-define (place-set-env win var val mode)
  (with-place win
    (cond ((== mode :here)
           (make-with (list var val)))
          ((== mode :paragraph)
           (make-multi-line-with (list var val)))
          ((== mode :global)
           (with-place win (init-env var val))
           (update-menus)))))

(tm-define (place-get-init win var)
  (place-get-env win var :global))

(tm-define (place-set-init win var val)
  (place-set-env win var val :global))

(tm-define (place-defined-init? win var)
  (with-place win
    (style-has? var)))

(tm-define (place-reset-init win . vars)
  (with-place win
    (apply init-default vars)
    (update-menus)))

(tm-define (place-id win)
  (url->string (url-tail win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (paragraph-basic-tool win mode)
  (refreshable "paragraph tool"
    (aligned
      (item (text "Alignment:")
        (enum (place-set-env win "par-mode" answer mode)
              '("left" "center" "right" "justify")
              (place-get-env win "par-mode" mode) "10em"))
      (assuming (== mode :paragraph)
        (item ====== ======)
        (item (text "Left margin:")
          (enum (place-set-env win "par-left" answer mode)
                (cons-new (place-get-env win "par-left" mode)
                          '("0tab" "1tab" "2tab" ""))
                (place-get-env win "par-left" mode) "10em"))
        (item (text "Right margin:")
          (enum (place-set-env win "par-right" answer mode)
                (cons-new (place-get-env win "par-right" mode)
                          '("0tab" "1tab" "2tab" ""))
                (place-get-env win "par-right" mode) "10em")))
      (item (text "First indentation:")
        (enum (place-set-env win "par-first" answer mode)
              (cons-new (place-get-env win "par-first" mode)
                        '("0tab" "1tab" "-1tab" ""))
              (place-get-env win "par-first" mode) "10em"))
      (item ====== ======)
      (item (text "Interline space:")
        (enum (place-set-env win "par-sep" answer mode)
              (cons-new (place-get-env win "par-sep" mode)
                        '("0fn" "0.2fn" "0.5fn" "1fn" ""))
              (place-get-env win "par-sep" mode) "10em"))
      (item (text "Interparagraph space:")
        (enum (place-set-env win "par-par-sep" answer mode)
              (cons-new (place-get-env win "par-par-sep" mode)
                        '("0fn" "0.3333fn" "0.5fn" "0.6666fn"
                          "1fn" "0.5fns" ""))
              (place-get-env win "par-par-sep" mode) "10em"))
      (item ====== ======)
      (item (text "Number of columns:")
        (enum (begin
                (place-set-env win "par-columns" answer mode)
                (refresh-now "paragraph-tool-columns"))
              '("1" "2" "3" "4" "5" "6")
              (place-get-env win "par-columns" mode) "10em"))
      (item (when (!= (place-get-env win "par-columns" mode) "1")
              (text "Column separation:"))
        (refreshable "paragraph-formatter-columns-sep"
          (when (!= (place-get-env win "par-columns" mode) "1")
            (enum (place-set-env win "par-columns-sep" answer mode)
                  (cons-new (place-get-env win "par-columns-sep" mode)
                            '("1fn" "2fn" "3fn" ""))
                  (place-get-env win "par-columns-sep" mode) "10em"))))
      (assuming (global-ref win mode :advanced)
        (item ====== ======)
        (item === ===)
        (item (text "Line breaking:")
          (enum (place-set-env win "par-hyphen" answer mode)
                '("normal" "professional")
                (place-get-env win "par-hyphen" mode) "10em"))
        (item ====== ======)
        (item (text "Extra interline space:")
          (enum (place-set-env win "par-line-sep" answer mode)
                (cons-new (place-get-env win "par-line-sep" mode)
                          '("0fn" "0.025fns" "0.05fns" "0.1fns"
                            "0.2fns" "0.5fns" "1fns" ""))
                (place-get-env win "par-line-sep" mode) "10em"))
        (item (text "Minimal line separation:")
          (enum (place-set-env win "par-ver-sep" answer mode)
                (cons-new (place-get-env win "par-ver-sep" mode)
                          '("0fn" "0.1fn" "0.2fn" "0.5fn" "1fn" ""))
                (place-get-env win "par-ver-sep" mode) "10em"))
        (item (text "Horizontal collapse distance:")
          (enum (place-set-env win "par-hor-sep" answer mode)
                (cons-new (place-get-env win "par-hor-sep" mode)
                          '("0.1fn" "0.2fn" "0.5fn" "1fn"
                            "2fn" "5fn" "10fn" "100fn" ""))
                (place-get-env win "par-hor-sep" mode) "10em"))
        (item ====== ======)
        (item (text "Space stretchability:")
          (enum (place-set-env win "par-flexibility" answer mode)
                (cons-new (place-get-env win "par-flexibility" mode)
                          '("1" "2" "4" "1000" ""))
                (place-get-env win "par-flexibility" mode) "10em"))
        (item (text "CJK spacing:")
          (enum (place-set-env win "par-spacing" answer mode)
                '("plain" "quanjiao" "banjiao" "hangmobanjiao" "kaiming")
                (place-get-env win "par-spacing" mode) "10em"))
        (item ====== ======)
        (item (text "Intercharacter stretching:")
          (enum (place-set-env win "par-kerning-stretch" answer mode)
                (cons-new (place-get-env win "par-kerning-stretch" mode)
                          '("auto" "tolerant"
                            "0" "0.02" "0.05" "0.1" "0.2" "0.5" "1" ""))
                (place-get-env win "par-kerning-stretch" mode) "10em"))
        (item (text "Intercharacter compression:")
          (enum (place-set-env win "par-kerning-reduce" answer mode)
                (cons-new (place-get-env win "par-kerning-reduce" mode)
                          '("auto"
                            "0" "0.01" "0.02" "0.03" "0.05" "0.1" "0.2" ""))
                (place-get-env win "par-kerning-reduce" mode) "10em"))
        (item (text "Character expansion:")
          (enum (place-set-env win "par-expansion" answer mode)
                (cons-new (place-get-env win "par-expansion" mode)
                          '("auto" "tolerant"
                            "0" "0.01" "0.02" "0.05" "0.1" "0.2" ""))
                (place-get-env win "par-expansion" mode) "10em"))
        (item (text "Character contraction:")
          (enum (place-set-env win "par-contraction" answer mode)
                (cons-new (place-get-env win "par-contraction" mode)
                          '("auto" "tolerant"
                            "0" "0.01" "0.02" "0.05" "0.1" "0.2" ""))
                (place-get-env win "par-contraction" mode) "10em"))
        (item ====== ======)
        (item (text "Use margin kerning:")
          (toggle (place-set-env win "par-kerning-margin"
                                  (if answer "true" "false") mode)
                  (== (place-get-env win "par-kerning-margin" mode)
                      "true")))))
    ======
    (division "discrete"
      (hlist
        (assuming (== mode :global)
          ("Restore defaults"
           (apply place-reset-init (cons win paragraph-parameters))))
        >>
        (assuming (not (global-ref win mode :advanced))
          ("Show advanced settings"
           (begin
             (global-set win mode :advanced #t)
             (refresh-now "paragraph tool")
             (update-menus))))
        (assuming (global-ref win mode :advanced)
          ("Hide advanced settings"
           (begin
             (global-set win mode :advanced #f)
             (refresh-now "paragraph tool")
             (update-menus))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This page format tool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool* (format-page-tool win u st t)
  (:name "This page format")
  (dynamic ((page-formatter u st t #f)
            (tool-quit 'format-page-tool #f win))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Page format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (place-get-page-rendering win)
  (or (with-place win
        (get-init-page-rendering))
      "?"))

(define (place-set-page-rendering win s)
  (with-place win
    (init-page-rendering s)))

(define (place-page-size-list win)
  (if (place-defined-init? win "beamer-style")
      (list "16:9" "8:5" "4:3" "5:4" "user")
      (list "a3" "a4" "a5" "b4" "b5" "letter" "legal" "executive" "user")))

(define (place-user-page-size? win)
  (== (place-get-init win "page-type") "user"))

(define (encode-rendering s)
  (cond ((== s "screen") "automatic")
        (else s)))

(define (decode-rendering s)
  (cond ((== s "automatic") "screen")
        (else s)))

(define (encode-crop-marks s)
  (cond ((== s "none") "")
        (else s)))

(define (decode-crop-marks s)
  (cond ((== s "") "none")
        (else s)))

(tm-widget (page-format-tool win)
  (refreshable "page format tool"
    ===
    (aligned
      (item (text "Page rendering:")
        (enum (place-set-page-rendering win (encode-rendering answer))
              '("paper" "papyrus" "screen" "beamer" "book"
                "panorama" "slideshow")
              (decode-rendering (place-get-page-rendering win)) "10em"))
      (item (text "Page type:")
        (enum (begin
                (place-set-init win "page-type" answer)
                (when (!= answer "user")
                  (place-set-init win "page-width" "auto")
                  (place-set-init win "page-height" "auto"))
                (refresh-now "page format tool"))
              (cons-new (place-get-init win "page-type")
                        (place-page-size-list win))
              (place-get-init win "page-type") "10em"))
      (item (when (place-user-page-size? win) (text "Page width:"))
        (when (place-user-page-size? win)
          (enum (place-set-init win "page-width" answer)
                (list (place-get-init win "page-width") "")
                (place-get-init win "page-width") "10em")))
      (item (when (place-user-page-size? win) (text "Page height:"))
        (when (place-user-page-size? win)
          (enum (place-set-init win "page-height" answer)
                (list (place-get-init win "page-height") "")
                (place-get-init win "page-height") "10em")))
      (item (text "Orientation:")
        (enum (place-set-init win "page-orientation" answer)
              '("portrait" "landscape")
              (place-get-init win "page-orientation") "10em"))
      (item (text "First page:")
        (enum (place-set-init win "page-first" answer)
              (list (place-get-init win "page-first") "")
              (place-get-init win "page-first") "10em"))
      (item (text "Crop marks:")
        (enum (place-set-init win "page-crop-marks"
                               (encode-crop-marks answer))
              '("none" "a3" "a4" "letter")
              (decode-crop-marks (place-get-init win "page-crop-marks"))
              "10em")))
    ===
    (division "discrete"
      (hlist
        >>
        ("Restore defaults"
         (place-reset-init win "page-medium" "page-type" "page-orientation"
                            "page-border" "page-packet" "page-offset"
                            "page-width" "page-height" "page-crop-marks"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global page margins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-margins-tool win)
  (padded
    (aligned
      (meti (hlist // (text "Determine margins from text width"))
        (toggle (begin
                  (place-set-init win "page-width-margin"
                                   (if answer "true" "false"))
                  (refresh-now "page-margin-settings"))
                (== (place-get-init win "page-width-margin") "true")))
      (meti (hlist // (text "Same screen margins as on paper"))
        (toggle (begin
                  (place-set-init win "page-screen-margin"
                                   (if answer "false" "true"))
                  (refresh-now "page-screen-margin-settings"))
                (!= (place-get-init win "page-screen-margin") "true")))))
  (refreshable "page-margin-settings"
    ===
    (division "subtitle"
      (text "Margins on paper"))
    (padded
      (if (!= (place-get-init win "page-width-margin") "true")
          (aligned
            (item (text "Left:")
              (hlist
                (input (place-set-init win "page-odd" answer) "string"
                       (list (place-get-init win "page-odd")) "6em")
                // // (text "(odd pages)") // >>>))
            (item (text "")
              (hlist
                (input (place-set-init win "page-even" answer) "string"
                       (list (place-get-init win "page-odd")) "6em")
                // // (text "(even pages)") >>>))
            (item (text "Right:")
              (hlist
                (input (place-set-init win "page-right" answer) "string"
                       (list (place-get-init win "page-right")) "6em")
                // // (text "(odd pages)") // >>>))
            (item (text "Top:")
              (input (place-set-init win "page-top" answer) "string"
                     (list (place-get-init win "page-top")) "6em"))
            (item (text "Bottom:")
              (input (place-set-init win "page-bot" answer) "string"
                     (list (place-get-init win "page-bot")) "6em"))))
      (if (== (place-get-init win "page-width-margin") "true")
          (aligned
            (item (text "Text width:")
              (input (place-set-init win "par-width" answer) "string"
                     (list (place-get-init win "par-width")) "6em"))
            (item (text "Odd page shift:")
              (input (place-set-init win "page-odd-shift" answer) "string"
                     (list (place-get-init win "page-odd-shift")) "6em"))
            (item (text "Even page shift:")
              (input (place-set-init win "page-even-shift" answer) "string"
                     (list (place-get-init win "page-even-shift")) "6em"))
            (item (text "Top:")
              (input (place-set-init win "page-top" answer) "string"
                     (list (place-get-init win "page-top")) "6em"))
            (item (text "Bottom:")
              (input (place-set-init win "page-bot" answer) "string"
                     (list (place-get-init win "page-bot")) "6em"))))))
  (refreshable "page-screen-margin-settings"
    (assuming (== (place-get-init win "page-screen-margin") "true")
      ===
      (division "subtitle"
        (text "Margins on screen"))
      (padded
        (aligned
          (item (text "Left:")
            (input (place-set-init win "page-screen-left" answer) "string"
                   (list (place-get-init win "page-screen-left")) "6em"))
          (item (text "Right:")
            (input (place-set-init win "page-screen-right" answer) "string"
                   (list (place-get-init win "page-screen-right")) "6em"))
          (item (text "Top:")
            (input (place-set-init win "page-screen-top" answer) "string"
                   (list (place-get-init win "page-screen-top")) "6em"))
          (item (text "Bottom:")
            (input (place-set-init win "page-screen-bot" answer) "string"
                   (list (place-get-init win "page-screen-bot")) "6em"))))))
  (division "discrete"
    (hlist
      >>
      ("Restore defaults"
       (place-reset-init win "page-odd" "page-even" "page-right"
                          "page-top" "page-bot" "par-width"
                          "page-odd-shift" "page-even-shift"
                          "page-screen-left" "page-screen-right"
                          "page-screen-top" "page-screen-bot"
                          "page-width-margin" "page-screen-margin")
       (refresh-now "page-margin-toggles")
       (refresh-now "page-margin-settings")
       (refresh-now "page-screen-margin-settings")))))

(tm-widget (page-margins-tool win)
  (:require (style-has? "std-latex-dtd"))
  (padded
    (text "This style specifies page margins in the TeX way"))
  (refreshable "page-margin-toggles"
    (padded
      (aligned
        (meti (hlist // (text "Same screen margins as on paper"))
          (toggle (begin
                    (place-set-init win "page-screen-margin"
                                     (if answer "false" "true"))
                    (refresh-now "page-screen-margin-settings"))
                  (!= (place-get-init win "page-screen-margin") "true"))))))
  (refreshable "page-tex-hor-margins"
    ===
    (division "subtitle"
      (text "Horizontal margins"))
    (padded
      (aligned
        (item (text "oddsidemargin:")
          (input (place-set-init win "tex-odd-side-margin" answer) "string"
                 (list (place-get-init win "tex-odd-side-margin")) "6em"))
        (item (text "evensidemargin:")
          (input (place-set-init win "tex-even-side-margin" answer) "string"
                 (list (place-get-init win "tex-even-side-margin")) "6em"))
        (item (text "textwidth:")
          (input (place-set-init win "tex-text-width" answer) "string"
                 (list (place-get-init win "tex-text-width")) "6em"))
        (item (text "linewidth:")
          (input (place-set-init win "tex-line-width" answer) "string"
                 (list (place-get-init win "tex-line-width")) "6em"))
        (item (text "columnwidth:")
          (input (place-set-init win "tex-column-width" answer) "string"
                 (list (place-get-init win "tex-column-width")) "6em")))))
  (refreshable "page-tex-ver-margins"
    ===
    (division "subtitle"
      (text "Vertical margins"))
    (padded
      (aligned
        (item (text "topmargin:")
          (input (place-set-init win "tex-top-margin" answer) "string"
                 (list (place-get-init win "tex-top-margin")) "6em"))
        (item (text "headheight:")
          (input (place-set-init win "tex-head-height" answer) "string"
                 (list (place-get-init win "tex-head-height")) "6em"))
        (item (text "headsep:")
          (input (place-set-init win "tex-head-sep" answer) "string"
                 (list (place-get-init win "tex-head-sep")) "6em"))
        (item (text "textheight:")
          (input (place-set-init win "tex-text-height" answer) "string"
                 (list (place-get-init win "tex-text-height")) "6em"))
        (item (text "footskip:")
          (input (place-set-init win "tex-foot-skip" answer) "string"
                 (list (place-get-init win "tex-foot-skip")) "6em")))))
  (refreshable "page-screen-margin-settings"
    (assuming (== (place-get-init win "page-screen-margin") "true")
      ===
      (division "subtitle"
        (text "Margins on screen"))
      (padded
        (aligned
          (item (text "Left:")
            (input (place-set-init win "page-screen-left" answer) "string"
                   (list (place-get-init win "page-screen-left")) "6em"))
          (item (text "Right:")
            (input (place-set-init win "page-screen-right" answer) "string"
                   (list (place-get-init win "page-screen-right")) "6em"))
          (item (text "Top:")
            (input (place-set-init win "page-screen-top" answer) "string"
                   (list (place-get-init win "page-screen-top")) "6em"))
          (item (text "Bottom:")
            (input (place-set-init win "page-screen-bot" answer) "string"
                   (list (place-get-init win "page-screen-bot")) "6em"))))))
  (division "discrete"
    (hlist
      >>
      ("Restore defaults"
       (place-reset-init win "tex-odd-side-margin" "tex-even-side-margin"
                          "tex-text-width" "tex-line-width"
                          "tex-column-width" "tex-top-margin"
                          "tex-head-height" "tex-head-sep"
                          "tex-text-height" "tex-foot-skip"
                          "page-screen-left" "page-screen-right"
                          "page-screen-top" "page-screen-bot"
                          "page-width-margin" "page-screen-margin")
       (refresh-now "page-margin-toggles")
       (refresh-now "page-tex-hor-margins")
       (refresh-now "page-tex-ver-margins")
       (refresh-now "page-screen-margin-settings")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-breaking-tool win)
  (refreshable "page-breaking-settings"
    ===
    (aligned
      (item (text "Page breaking algorithm:")
        (enum (place-set-init win "page-breaking" answer)
              '("sloppy" "professional")
              (place-get-init win "page-breaking") "10em"))
      (item (text "Allowed page height reduction:")
        (enum (place-set-init win "page-shrink" answer)
              (cons-new (place-get-init win "page-shrink")
                        '("0cm" "0.5cm" "1cm" ""))
              (place-get-init win "page-shrink") "10em"))
      (item (text "Allowed page height extension:")
        (enum (place-set-init win "page-extend" answer)
              (cons-new (place-get-init win "page-extend")
                        '("0cm" "0.5cm" "1cm" ""))
              (place-get-init win "page-extend") "10em"))
      (item (text "Vertical space stretchability:")
        (enum (place-set-init win "page-flexibility" answer)
              (cons-new (place-get-init win "page-flexibility")
                        '("0" "0.25" "0.5" "0.75" "1" ""))
              (place-get-init win "page-flexibility") "10em")))
    ===
    (division "discrete"
      (hlist
        >>
        ("Restore defaults"
         (place-reset-init win "page-breaking" "page-shrink"
                            "page-extend" "page-flexibility")
         (refresh-now "page-breaking-settings"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Header & Footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define header-parameters
  (list "page-odd-header" "page-even-header"
        "page-odd-footer" "page-even-footer"))

(define (header-buffer win var)
  (string->url (string-append "tmfs://aux/" var "-" (place-id win))))

(define (get-field-contents u)
  (and-with t (tm->stree (buffer-get-body u))
    (when (tm-func? t 'document 1)
      (set! t (tm-ref t 0)))
    t))

(define (apply-headers-settings win u)
  (with l (list)
    (for (var header-parameters)
      (and-with doc (get-field-contents (header-buffer win var))
        (set! l (cons `(,var ,doc) l))))
    (when (nnull? l)
      (delayed
        (:idle 10)
        (for (x l) (initial-set-tree u (car x) (cadr x)))
        (refresh-view)))))

(define (editing-headers? win)
  (in? (current-buffer)
       (map (lambda (var) (header-buffer win var))
            header-parameters)))

(tm-define synchronize-table (make-ahash-table))

(define (synchronize win var)
  (let* ((key (list "header" win var))
         (val (tm->stree (initial-get-tree (place->buffer win) var)))
         (old (ahash-ref synchronize-table key)))
    (when (and old (!= val old))
      (delayed
        (:idle 1)
        (buffer-set-body (header-buffer win var) `(document ,val))))
    (when (!= val old)
      (ahash-set! synchronize-table key val))
    #t))

(tm-widget (page-headers-tool win)
  (let* ((u (place->buffer win))
         (style (embedded-style-list "macro-editor")))
    (for (var header-parameters)
      ======
      (text (eval (parameter-name var)))
      ===
      (cached (string-append "edit-" var) (synchronize win var)
        (resize "330px" "60px"
          (texmacs-input `(document ,(initial-get-tree u var))
                         `(style (tuple ,@style "gui-base"))
                         (header-buffer win var))))))
  ====== ===
  (division "plain"
    (hlist
      ("Tab" (when (editing-headers? win) (make-htab "5mm")))
      ("Page number" (when (editing-headers? win) (make 'page-the-page)))
      >>>
      ("Restore" (apply place-reset-init (cons win header-parameters)))
      ("Apply"
       (apply-headers-settings win (place->buffer win))
       (with-place win (update-menus))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool (format-paragraph-tool win)
  (:name "This paragraph format")
  (dynamic (paragraph-basic-tool win :paragraph)))

(tm-tool (document-paragraph-tool win)
  (:name "Global paragraph format")
  (dynamic (paragraph-basic-tool win :global)))

(tm-tool* (document-page-tool win)
  (:name "Global page format")
  (section-tabs "document-page-tabs" win
    (section-tab "Format" ===
      (centered (dynamic (page-format-tool win))))
    (section-tab "Breaking" ===
      (centered (dynamic (page-breaking-tool win))))
    (section-tab "Margins" ===
      (centered (dynamic (page-margins-tool win))))
    (section-tab "Headers" ===
      (centered (dynamic (page-headers-tool win))))))

(tm-widget (texmacs-side-tool win tool . opts)
  (:require (== (car tool) 'sections-tool))
  (division "sections"
    (hlist
      ("File" (noop))
      ("Edit" (noop))
      ("Insert" (noop))
      ("Format" (noop))
      (class "active-section" ("Document" (noop)))
      >>>))
  (division "title"
    (text "Page headers and footers"))
  (centered
    (dynamic (page-headers-tool win))
    ======))

(tm-widget (texmacs-side-tool win tool . opts)
  (:require (== (car tool) 'subsections-tool))
  (division "sections"
    (hlist
      ("File" (noop))
      ("Edit" (noop))
      ("Insert" (noop))
      ("Format" (noop))
      (class "active-section" ("Document" (noop)))
      >>>))
  (division "section-tabs"
    (hlist
      ("Style" (noop))
      ("View" (noop))
      (class "section-active-tab" ("Page" (noop)))
      ("Paragraph" (noop))
      ("Font" (noop))
      ("More" (noop))
      >>>))
  (centered
    (dynamic (page-format-tool win))
    ======))
