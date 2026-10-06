
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : asymptote-edit.scm
;; DESCRIPTION : The labels of Asymptote pictures made in a browser
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A picture of the Asymptote plugin in a browser (web/tm-asy.mjs) is an
;; image of its drawing with its labels set by TeXmacs over it, in a graphics
;; (docs/wasm/asymptote.md):
;;
;;   (asy-picture source
;;     (superpose (asy-drawing (image ...))
;;                (graphics ... (text-at (asy-label n orig body) point) ...)))
;;
;; The worker sends the text of a label as (asy-latex "LaTeX"), converted
;; here when the output comes. A label can be edited; in an executable fold,
;; when the source is shown again, an edited label goes back into it: its
;; string "orig", when the source has it once, becomes the LaTeX of the label.

(texmacs-module (asymptote-edit)
  (:use (utils plugins plugin-eval)
        (dynamic scripts-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX of the labels <-> TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (asy-latex->texmacs s)
  (with t (convert s "latex-snippet" "texmacs-stree")
    (if (func? t 'document 1) (cadr t) t)))

(define (asy-texmacs->latex t)
  (with s (convert t "texmacs-stree" "latex-snippet")
    (if (string? s) (tm-string-trim-both s) "")))

(define (asy-convert t)
  (cond ((func? t 'asy-latex 1) (asy-latex->texmacs (cadr t)))
        ((pair? t) (cons (car t) (map asy-convert (cdr t))))
        (else t)))

;; the output of the plugin: the text of the labels as TeXmacs
(tm-define (connection-notify lan ses ch t)
  (:require (and (== lan "asymptote") (== ch "output")))
  (former lan ses ch (stree->tree (asy-convert (tree->stree t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The source of a picture with its edited labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (asy-find t tag)
  (cond ((tree-is? t tag) t)
        ((tree-compound? t)
         (let loop ((l (tree-children t)))
           (and (nnull? l)
                (or (asy-find (car l) tag) (loop (cdr l))))))
        (else #f)))

(define (asy-collect t tag)
  (cond ((tree-is? t tag) (list t))
        ((tree-compound? t) (append-map (cut asy-collect <> tag) (tree-children t)))
        (else '())))

;; the number of times a string occurs in another
(define (asy-count s what)
  (let loop ((i 0) (n 0))
    (with j (string-search-forwards what i s)
      (if (< j 0) n (loop (+ j (string-length what)) (+ n 1))))))

(define (asy-replace s what by)
  (with j (string-search-forwards what 0 s)
    (string-append (substring s 0 j) by
                   (substring s (+ j (string-length what)) (string-length s)))))

;; the source with the edited labels whose string it has once, or #f
(define (asy-edited-source src pic)
  (with edited? #f
    (for (lab (asy-collect pic 'asy-label))
      (when (!= (tree->stree (tree-ref lab 1)) (tree->stree (tree-ref lab 2)))
        (let* ((orig (string-append "\"" (tree->string (tree-ref lab 1)) "\""))
               (new (string-append "\"" (asy-texmacs->latex (tree->stree (tree-ref lab 2))) "\"")))
          (when (== (asy-count src orig) 1)
            (set! src (asy-replace src orig new))
            (set! edited? #t)))))
    (and edited? src)))

;; the input of a fold as a string, and back
(define (asy-input->string t)
  (if (tree-is? t 'document)
      (string-recompose (map tree->string (tree-children t)) "\n")
      (tree->string t)))

;; Return in the output of an executable fold of Asymptote: its source with
;; the edited labels, before it is shown
(tm-define (alternate-toggle t)
  (:require (and (tree-is? t 'script-output)
                 (== (tree->string (tree-ref t 0)) "asymptote")))
  (and-with pic (asy-find (tree-ref t 3) 'asy-picture)
    (and-with src (asy-edited-source (asy-input->string (tree-ref t 2)) pic)
      ;; one line as a string (Return evaluates it), several as a document
      (with lines (string-split-lines src)
        (tree-set (tree-ref t 2)
                  (if (== (length lines) 1) (car lines) (cons 'document lines))))))
  (former t))
