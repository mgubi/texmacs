;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-fold.scm
;; DESCRIPTION : folding the blocks of source files (folding ranges)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The blocks of a source file which its server gives
;; (textDocument/foldingRange), for the folds of program code, which are
;; those of the core (progs/code/code-fold.scm): this module is their
;; provider (lsp-code.scm), and keeps the names of its former commands. A
;; block (start end) of the server is folded as its first line shown and
;; the lines after it up to end hidden.

(texmacs-module (lsp lsp-fold)
  (:use (lsp lsp-base)
        (lsp lsp-client)
        (lsp lsp-documents)
        (lsp lsp-edit)
        (code code-fold)))

(tm-define (lsp-folding-ranges-of r)
  (:synopsis "The FoldingRanges @r as (start end), the blocks of lines")
  ;; (those of one line, which hide nothing, are left out)
  (list-filter
   (map (lambda (x)
          (list (or (lsp-number-value (lsp-ref x "startLine")) 0)
                (or (lsp-number-value (lsp-ref x "endLine")) 0)))
        (lsp-items r))
   (lambda (x) (> (cadr x) (car x)))))

(define (folding-ranges)
  ;; the blocks of the current buffer, or #f (with a message)
  (let ((c (lsp-buffer-client*))
        (uri (lsp-buffer-uri)))
    (cond ((not (and c uri))
           (set-message "No language server for this buffer" "") #f)
          ((not (lsp-client-capable? c "foldingRangeProvider"))
           (set-message "The language server cannot fold" "") #f)
          (else
           ;; (the server must have the text: the changes which wait)
           (lsp-sync-pending)
           (with a (lsp-request-wait c "textDocument/foldingRange"
                                     (lsp-object "textDocument"
                                                 (lsp-object "uri" uri))
                                     5000)
             (if (== (car a) 'result) (lsp-folding-ranges-of (cadr a))
                 (begin (set-message "The language server does not answer" "")
                        #f)))))))

(tm-define (lsp-innermost-range ranges line)
  (:synopsis "The innermost of the blocks @ranges which contain @line")
  (with in (list-filter ranges (lambda (r) (and (<= (car r) line)
                                                (<= line (cadr r)))))
    (and (nnull? in)
         (car (sort in (lambda (x y)
                         (or (> (car x) (car y))
                             (and (== (car x) (car y))
                                  (< (cadr x) (cadr y))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (distinct-ranges ranges)
  (let ((t (make-ahash-table)))
    (for (r ranges)
      (with o (ahash-ref t (car r))
        (when (or (not o) (> (cadr r) (cadr o)))
          (ahash-set! t (car r) r))))
    (sort (map cdr (ahash-table->list t))
          (lambda (x y) (< (car x) (car y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The provider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-can-fold?)
  (:synopsis "Does a server which can fold follow the current buffer?")
  (and (lsp-on?) (lsp-buffer-followed?)
       (with c (lsp-buffer-client*)
         (and c (lsp-client-running? c)
              (lsp-client-capable? c "foldingRangeProvider")))))

(define (in-source-file?)
  ;; (the cursor is in the lines of the buffer, which its server sees)
  (with c (code-cursor-lines)
    (and c (tree-eq? (car c) (buffer-get-body (current-buffer))))))

(tm-define (lsp-fold-ranges)
  (:synopsis "The blocks of the server around the cursor, as (start end)")
  (let ((c (code-cursor-lines))
        (ranges (and (in-source-file?) (folding-ranges))))
    (and c ranges
         (list-filter ranges (lambda (r) (and (<= (car r) (cadr c))
                                               (<= (cadr c) (cadr r))))))))

(tm-define (lsp-all-fold-ranges)
  (:synopsis "All the blocks of the server, as (start end)")
  (with ranges (and (in-source-file?) (folding-ranges))
    (and ranges (distinct-ranges ranges))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The former commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (body) (buffer-get-body (current-buffer)))

(tm-define (lsp-fold-block start end)
  (:synopsis "Folds the lines @start to @end: @start stays shown")
  (code-fold-lines (body) start end))

(tm-define (lsp-fold) (code-fold))
(tm-define (lsp-unfold) (code-unfold))
(tm-define (lsp-fold-all) (code-fold-all))
(tm-define (lsp-unfold-all) (code-unfold-all))

(tm-define (lsp-folds)
  (:synopsis "The folds of the current buffer, as (start end)")
  (code-folds (body)))

(tm-define (lsp-line-folded? t)
  (:synopsis "Is the line @t hidden by a fold?")
  (> (code-line-fold-state t) 0))
