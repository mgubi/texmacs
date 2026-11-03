
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-folds.scm
;; DESCRIPTION : the executable folds of the AI engines
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An executable fold of a chatbot asks its question alone, with its model
;; and its settings (around it, as for a session: init-ai.scm), and keeps its
;; answer: an answer costs, and changes each time. Unfolding the fold again
;; (or all the folds of the document) shows the answer which it has; Return
;; in its question, or Ask again in its focus bar, asks again.
;;
;; (A module of its own, loaded once by the plug-in, whose file is loaded
;; again when a key is given: its definitions would else be made again. It
;; uses the modules whose definitions it overloads, which are so loaded
;; before it.)

(texmacs-module (tools ai ai-folds)
  (:use (dynamic scripts-edit)
        (dynamic scripts-menu)
        (generic generic-menu)))

(tm-define (ai-fold? t)
  (and (tree? t) (tree-in? t '(script-input script-output))
       (tree-atomic? (tree-ref t 0))
       (in? (tree->string (tree-ref t 0)) (ai-models))))

(define (ai-fold-answered? t)
  (not (in? (tree->stree (tree-ref t 3))
            '("" (document) (document "") (script-busy)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The answer which is kept, and the question asked again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the question of the answer which a fold keeps: a short sign of it, kept
;; around the fold (ai-asked), which tells when it changed since
(define (ai-question-sign t)
  (let* ((s (object->string (tree->stree (tree-ref t 2))))
         (n (string-length s)))
    (let loop ((i 0) (h 0))
      (if (>= i n) (number->string h 16)
          (loop (+ i 1) (modulo (+ (* h 31) (char->integer (string-ref s i)))
                                1000000007))))))

(tm-define (ai-fold-stale? t)
  (and (ai-fold? t) (ai-fold-answered? t)
       (with sign (ai-tree-var t "ai-asked")
         (and sign (!= sign (ai-question-sign t))))))

(define ai-fold-asked? #f)

(tm-define (ai-fold-ask-again t)
  (when (ai-fold? t)
    (if (tree-is? t 'script-output) (tree-assign-node! t 'script-input))
    (with-global ai-fold-asked? #t
      (alternate-toggle t))))

(tm-define (alternate-toggle t)
  (:require (and (tree-is? t 'script-input) (ai-fold? t)
                 (not ai-fold-asked?) (ai-fold-answered? t)))
  (when (ai-fold-stale? t)
    (set-message "The question changed since this answer: Return in it, or Ask again, asks it"
                 (session-name (tree->string (tree-ref t 0)))))
  (tree-assign-node! t 'script-output)
  (tree-go-to t 3 :end))

(tm-define (kbd-enter t forwards?)
  (:require (and (tree-is? t 'script-input) (ai-fold? t)
                 (not (tree-is? t :up 'inactive))
                 (xor (not forwards?) (tree-is? t 2 'document))))
  (ai-fold-ask-again t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The fold of a request: the evaluation of a fold gives it to the request
;; among its options (:ai-fold and a pointer to the fold), which the
;; plug-in keeps with its pending request (silent-feed, plugin-eval.scm),
;; so that the request which is made, and the answer which comes, find
;; their fold there: the pending requests are those of the engine and the
;; session, and an interrupted one is removed with them. (Queues of the
;; folds of an engine got out of step when a request was interrupted, or
;; when two sessions of an engine asked at once: a fold was then asked with
;; the model and the files of another, and given its answer.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (script-feed lan ses in out opts)
  (:require (in? lan (ai-models)))
  (with f (and (tree? out) (tree-up out))
    (if (ai-fold? f)
        (begin
          (ai-tree-set-var! f "ai-asked" (ai-question-sign f))
          (former lan ses in out
                  (cons (list :ai-fold (tree->tree-pointer f)) opts)))
        (former lan ses in out opts))))

;; the entry :ai-fold of the options of the request of the engine and the
;; session which is made (the first pending one), #f if it is not a fold's
(define (ai-pending-fold-entry name chat)
  (let* ((l (pending-ref name chat))
         (item (and (pair? l) (car l)))
         ;; (cb in out err return opts, see silent-encode)
         (opts (and (list? item) (>= (length item) 6) (list-ref item 5))))
    (and (list? opts)
         (list-find opts (lambda (o) (and (pair? o) (== (car o) :ai-fold)))))))

(define (ai-entry-fold e)
  (and e
       (with f (catch #t (lambda () (tree-pointer->tree (cadr e)))
                 (lambda args #f))
         (and (ai-fold? f) f))))

;; the fold of the request which is made, #f if it is not in a fold
(tm-define (ai-fold-pop name chat)
  (ai-entry-fold (ai-pending-fold-entry name chat)))

;; the fold whose answer comes, where the answer is put with its tokens
;; (ai-result-filter, init-ai.scm): the pointer is let go then
(tm-define (ai-fold-result-pop name chat)
  (let* ((e (ai-pending-fold-entry name chat))
         (f (ai-entry-fold e)))
    (when e (tree-pointer-detach (cadr e)))
    f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The focus bar of a fold: its model, the reasoning, Ask again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-extra-icons t)
  (:require (ai-fold? t))
  (dynamic (ai-fold-icons (tree->string (tree-ref t 0)) t)))
