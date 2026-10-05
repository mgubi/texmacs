
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
;; The folds which ask: queued until their request is made (ai-request,
;; init-ai.scm), in the order of the requests of the engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ai-fold-queue (make-ahash-table)) ; engine -> pointers to the folds

(tm-define (script-feed lan ses in out opts)
  (:require (in? lan (ai-models)))
  (with f (and (tree? out) (tree-up out))
    (when (ai-fold? f)
      (ai-tree-set-var! f "ai-asked" (ai-question-sign f)))
    (ahash-set! ai-fold-queue lan
                (append (or (ahash-ref ai-fold-queue lan) '())
                        (list (and (ai-fold? f) (tree->tree-pointer f))))))
  (former lan ses in out opts))

;; the fold of the next request of the engine, #f if it is not in a fold
(tm-define (ai-fold-pop name)
  (with l (or (ahash-ref ai-fold-queue name) '())
    (and (nnull? l)
         (begin
           (ahash-set! ai-fold-queue name (cdr l))
           (and (car l)
                (with f (catch #t (lambda () (tree-pointer->tree (car l)))
                          (lambda args #f))
                  (tree-pointer-detach (car l))
                  (and (ai-fold? f) f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The focus bar of a fold: its model, the reasoning, Ask again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-extra-icons t)
  (:require (ai-fold? t))
  (dynamic (ai-fold-icons (tree->string (tree-ref t 0)) t)))
