
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tikz-edit.scm
;; DESCRIPTION : The text of the nodes of TikZ pictures made in a browser
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A picture of the TikZ plugin in a browser (web/tm-tikz.js) is an image of
;; its drawing with the text of its nodes over it, typeset by TeXmacs from
;; their LaTeX (src/docs/wasm/tikzjax.md):
;;
;;   (tikz-picture (tuple text1 node1 text2 node2 ... textn)
;;     (superpose (image ...) ... (move (tikz-label n orig body) x y) ...))
;;
;; The worker sends the text of a node as (tikz-latex "LaTeX"), converted
;; here when the output comes. The text of a node can be edited; in an
;; executable fold, the source of the picture is made again from the labels
;; when the fold shows it, an edited label as LaTeX in place of the text of
;; its node, so that the picture made again from it has the new text.

(texmacs-module (tikz-edit)
  (:use (utils plugins plugin-eval)
        (dynamic scripts-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX of the nodes <-> TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tikz-latex->texmacs s)
  (with t (convert s "latex-snippet" "texmacs-stree")
    (if (func? t 'document 1) (cadr t) t)))

(define (tikz-texmacs->latex t)
  (with s (convert t "texmacs-stree" "latex-snippet")
    (if (string? s) (tm-string-trim-both s) "")))

(define (tikz-convert t)
  (cond ((func? t 'tikz-latex 1) (tikz-latex->texmacs (cadr t)))
        ((pair? t) (cons (car t) (map tikz-convert (cdr t))))
        (else t)))

;; the output of the plugin: the text of the nodes as TeXmacs
(tm-define (connection-notify lan ses ch t)
  (:require (and (== lan "tikz") (== ch "output")))
  (former lan ses ch (stree->tree (tikz-convert (tree->stree t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The source of a picture from its labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tikz-find t tag)
  (cond ((tree-is? t tag) t)
        ((tree-compound? t)
         (let loop ((l (tree-children t)))
           (and (nnull? l)
                (or (tikz-find (car l) tag) (loop (cdr l))))))
        (else #f)))

(define (tikz-collect t tag)
  (cond ((tree-is? t tag) (list t))
        ((tree-compound? t) (append-map (cut tikz-collect <> tag) (tree-children t)))
        (else '())))

;; the source of the picture with its edited labels, #f if none was edited
;; (or if the picture does not know the nodes of its source)
(define (tikz-edited-source pic)
  (let* ((segs (tree-ref pic 0))
         (labels (tikz-collect (tree-ref pic 1) 'tikz-label))
         (edited? #f))
    (and (tree-is? segs 'tuple)
         (let* ((parts (map tree->string (tree-children segs)))
                (r (let loop ((l parts) (k 0) (acc '()))
                     (cond ((null? l) (reverse acc))
                           ((even? k) (loop (cdr l) (+ k 1) (cons (car l) acc)))
                           (else
                             (let* ((n (number->string (quotient (+ k 1) 2)))
                                    (lab (list-find labels
                                                    (lambda (x) (== (tree->string (tree-ref x 0)) n))))
                                    (s (if (and lab (!= (tree->stree (tree-ref lab 1))
                                                       (tree->stree (tree-ref lab 2))))
                                           (begin
                                             (set! edited? #t)
                                             (tikz-texmacs->latex (tree->stree (tree-ref lab 2))))
                                           (car l))))
                               (loop (cdr l) (+ k 1) (cons s acc))))))))
           (and edited? (apply string-append r))))))

;; Return in the output of an executable fold of TikZ: its source with the
;; edited labels, before it is shown
(tm-define (alternate-toggle t)
  (:require (and (tree-is? t 'script-output)
                 (== (tree->string (tree-ref t 0)) "tikz")))
  (and-with pic (tikz-find (tree-ref t 3) 'tikz-picture)
    (and-with src (tikz-edited-source pic)
      ;; one line as a string, as typed (Return evaluates it), several as a
      ;; document (Return makes a new line, Shift+Return evaluates)
      (with lines (string-split-lines src)
        (tree-set (tree-ref t 2)
                  (if (== (length lines) 1) (car lines) (cons 'document lines))))))
  (former t))
