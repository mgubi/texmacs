
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-variations.scm
;; DESCRIPTION : a panel for the axes of variable fonts
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-variations)
  (:use (utils library cursor)
        (generic format-edit)
        (generic document-edit)))

;; The value of the font-variations environment variable gives axes of the
;; variable fonts and their values, as in "wght=550,opsz=auto"; see
;; tt_variation_name in Plugins/Freetype/tt_tools.cpp. The panel shows the
;; axes of the font at the cursor of the document it was opened from, with
;; the value in force, and changes them for the selection or for the whole
;; document.

(define fv-buffer #f)
(define fv-global? #f)
(define fv-open? #f)

(define-macro (in-document . body)
  `(if (and fv-buffer (buffer-exists? fv-buffer))
       (with-buffer fv-buffer ,@body)
       (begin ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The value of the variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spec->alist s)
  (with l (list-filter (string-tokenize-comma s) (lambda (x) (!= x "")))
    (list-filter
      (map (lambda (x)
             (with p (string-tokenize-by-char x #\=)
               (and (== (length p) 2)
                    (cons (tm-string-trim-both (car p))
                          (tm-string-trim-both (cadr p))))))
           l)
      identity)))

(define (alist->spec l)
  (string-recompose (map (lambda (p) (string-append (car p) "=" (cdr p))) l)
                    ","))

(define (fv-env var)
  (in-document
    (if fv-global? (get-init var) (get-env var))))

(define (fv-spec) (spec->alist (fv-env "font-variations")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The axes of the font at the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (series-name ser)
  ;; as series_name in Typeset/Env/env_semantics.cpp
  (with w (and (string-number? ser) (string->number ser))
    (cond ((not w) ser)
          ((< w 150) "thin")
          ((< w 250) "extralight")
          ((< w 350) "light")
          ((< w 550) "medium")
          ((< w 650) "semibold")
          ((< w 750) "bold")
          ((< w 850) "extrabold")
          (else "black"))))

(define (fv-font-file)
  (with l (font-logical-search (fv-env "font") (fv-env "font-family")
                               (series-name (fv-env "font-series"))
                               (fv-env "font-shape"))
    (and (nnull? l) (car l))))

(define (as-text x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else "")))

(define (as-num x)
  (cond ((number? x) x)
        ((string? x) (or (string->number x) 0))
        ((symbol? x) (or (string->number (symbol->string x)) 0))
        (else 0)))

;; An axis: (tag name minimum default maximum value-of-the-font)
(define (fv-axes)
  (with f (fv-font-file)
    (if (not f) (list)
        (map (lambda (a)
               (list (as-text (list-ref a 0)) (as-text (list-ref a 1))
                     (as-num (list-ref a 2)) (as-num (list-ref a 3))
                     (as-num (list-ref a 4)) (as-num (list-ref a 5))))
             (list-filter (font-variation-axes f) pair?)))))

(define (axis-tag a) (list-ref a 0))
(define (axis-name a) (list-ref a 1))
(define (axis-min a) (list-ref a 2))
(define (axis-max a) (list-ref a 4))
(define (axis-font-value a) (list-ref a 5))

(define (number->text x)
  (let* ((r (/ (round (* x 10)) 10))
         (i (inexact->exact (round r))))
    (if (= r i) (number->string i) (number->string (exact->inexact r)))))

(define (axis-value a)
  ;; the value in force: font-variations, then a numeric series for the
  ;; weight, then the style the font series and shape select
  (let* ((tag (axis-tag a))
         (p (assoc tag (fv-spec)))
         (ser (fv-env "font-series")))
    (cond ((and p (== (cdr p) "auto")) "auto")
          ((and p (string-number? (cdr p))) (cdr p))
          ((and (== tag "wght") (string-number? ser)) ser)
          (else (number->text (axis-font-value a))))))

(define (axis-choices a)
  ;; a few values along the axis, and the one in force
  (let* ((lo (axis-min a)) (hi (axis-max a))
         (l (map (lambda (i) (number->text (+ lo (* i (/ (- hi lo) 8)))))
                 (.. 0 9)))
         (l2 (if (== (axis-tag a) "opsz") (cons "auto" l) l))
         (cur (axis-value a)))
    (if (in? cur l2) l2 (cons cur l2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-at-cursor var)
  ;; the innermost with around the cursor which sets var, if any
  (let loop ((t (cursor-tree)))
    (cond ((not (tree? t)) #f)
          ((and (tree-is? t 'with)
                (in? var (map tree->string
                              (list-filter (tree-children t)
                                           (lambda (c) (tree-atomic? c))))))
           t)
          ((tree-up t) (loop (tree-up t)))
          (else #f))))

(define (set-variable var val)
  (in-document
    (cond (fv-global?
           (if (== val "") (init-default var) (init-env var val)))
          ((selection-active-any?) (make-with var val))
          ((and (== val "") (with-at-cursor var))
           => (lambda (t) (tree-with-reset t var)))
          ((with-at-cursor var)
           => (lambda (t)
                (for (i (.. 0 (quotient (- (tree-arity t) 1) 2)))
                  (when (== (tree->string (tree-ref t (* 2 i))) var)
                    (tree-set! t (+ (* 2 i) 1) val)))))
          ((== val "") #f)
          (else (make-with var val)))))

(define (fv-refresh)
  (refresh-now "font-variations-axes"))

(tm-define (font-variation-set tag val)
  (:synopsis "Set one axis of the variable font at the cursor")
  (let* ((a (list-find (fv-axes) (lambda (a) (== (axis-tag a) tag))))
         (val* (cond ((not a) val)
                     ((== val "auto") val)
                     ((string-number? val)
                      (number->text (max (axis-min a)
                                         (min (axis-max a)
                                              (string->number val)))))
                     (else #f)))
         (l (list-filter (fv-spec) (lambda (p) (!= (car p) tag))))
         (l2 (if (and val* a (!= val* "auto")
                      (== val* (number->text (axis-font-value a)))
                      (not (and (== tag "wght")
                                (string-number? (fv-env "font-series")))))
                 l
                 (if val* (rcons l (cons tag val*)) l))))
    (set-variable "font-variations" (alist->spec l2))
    (fv-refresh)))

(define (font-variation-step a dir)
  (let* ((cur (axis-value a))
         (x (if (string-number? cur) (string->number cur)
                (axis-font-value a)))
         (d (/ (- (axis-max a) (axis-min a)) 20)))
    (font-variation-set (axis-tag a) (number->text (+ x (* dir d))))))

(tm-define (font-variations-reset)
  (:synopsis "Remove all the variations at the cursor")
  (set-variable "font-variations" "")
  (fv-refresh))

(define (fv-set-mode m)
  (set! fv-global? (== m "Whole document"))
  (fv-refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (axis-range a)
  (string-append (number->text (axis-min a)) " to "
                 (number->text (axis-max a))))

(tm-widget (font-variations-widget)
  (padded
    (hlist
      (text "Apply to") // //
      (enum (fv-set-mode answer) '("Selection" "Whole document")
            (if fv-global? "Whole document" "Selection") "12em")
      >>>
      (explicit-buttons ("Update" (fv-refresh))))
    ===
    (refreshable "font-variations-axes"
      (with axes (fv-axes)
        (if (null? axes)
            (text "The font at the cursor is not a variable font"))
        (if (nnull? axes)
            (vlist
              (for (a axes)
                (hlist
                  (resize "9em" "1.5em" (text (axis-name a)))
                  (explicit-buttons
                    ("-" (font-variation-step a -1)))
                  (enum (font-variation-set (axis-tag a) answer)
                        (axis-choices a) (axis-value a) "6em")
                  (explicit-buttons
                    ("+" (font-variation-step a 1)))
                  // //
                  (text (axis-range a))
                  >>>))))))
    ===
    (hlist
      >>>
      (explicit-buttons ("Reset" (font-variations-reset))))))

(tm-define (open-font-variations)
  (:synopsis "Open a panel for the axes of the variable font at the cursor")
  (set! fv-buffer (current-buffer))
  (when (not fv-open?)
    (set! fv-open? #t)
    (let* ((win (alt-window-handle))
           (quit (object->command
                   (lambda ()
                     (set! fv-open? #f)
                     (alt-window-delete win))))
           (wid (make-menu-widget* (list 'vertical (font-variations-widget))
                                   0)))
      (alt-window-create-quit win wid (translate "Font variations") quit)
      (alt-window-set-on-top win #t)
      (alt-window-show win)))
  (fv-refresh))

(tm-define (open-document-font-variations)
  (:synopsis "The variations panel, for the whole document")
  (set! fv-global? #t)
  (open-font-variations))

(tm-define (open-text-font-variations)
  (:synopsis "The variations panel, for the selection")
  (set! fv-global? #f)
  (open-font-variations))

;; Keep the panel on the font at the cursor

(define (editor-document? u)
  ;; a document of the user, not a buffer of a widget nor a report
  (with s (url->string u)
    (not (or (string-starts? s "tmfs://aux/")
             (string-starts? s "tmfs://fontdbg/")))))

(tm-define (notify-cursor-moved status)
  (:require fv-open?)
  (former status)
  (when (editor-document? (current-buffer))
    (set! fv-buffer (current-buffer))
    (delayed
      (:idle 150)
      (when fv-open? (fv-refresh)))))
