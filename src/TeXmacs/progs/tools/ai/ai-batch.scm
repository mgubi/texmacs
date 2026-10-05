
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-batch.scm
;; DESCRIPTION : AI tools using blocking batch calls of the AI engines
;; COPYRIGHT   : (C) 2025  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tools ai ai-batch)
  (version version-compare))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pre- and post-processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ai-serialize lan t)
  (when (tm-func? t 'document 1)
    (set! t (tm-ref t 0)))
  (if (tm-atomic? t)
      (with s (tm->string t)
        (cork->utf8 s))
      (with s (convert (tm->stree t) "texmacs-stree" "latex-snippet"
                       (cons "texmacs->latex:encoding" "utf-8"))
        ;;(display* "s = " s "\n")
        s)))

(tm-define (ai-cmdline name chat cmd)
  (cpp-ai-latex-command cmd name chat))

(tm-define (ai-request name chat cmd)
  (cpp-ai-latex-request cmd name chat))

(tm-define (ai-result name chat res)
  (with t (cpp-ai-latex-output res name chat)
    (when (string-contains? (object->string (tm->stree t)) "ai-tikz")
      (delayed
        (:idle 100)
        (ai-run-pending-folds)))
    t))

;; The TikZ pictures of an answer are executable folds of the TikZ plug-in
;; (ai.cpp), marked as pending: once in the document they are evaluated
(tm-define (ai-run-pending-folds)
  (with l (select (buffer-tree) '(:* with))
    (with pending (list-filter l (lambda (w)
                                   (and (== (tree-arity w) 3)
                                        (tm-equal? (tree-ref w 0) "ai-tikz")
                                        (tm-equal? (tree-ref w 1) "pending"))))
      (when (nnull? pending)
        (when (not (in? "tikz" (get-style-list)))
          (add-style-package "tikz"))
        (for (w pending)
          (with t (tree-ref w 2)
            (tree-remove-node! w 2)
            (when (tree-is? t 'script-input)
              (script-eval-at (tree-ref t 3)
                              (tree->string (tree-ref t 0))
                              (tree->string (tree-ref t 1))
                              (tree->stree (tree-ref t 2))
                              :math-input :simplify-output)
              (tree-assign-node! t 'script-output))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic correction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (open-comments c)
  (let* ((doc
	  `(document
	     (style "generic")
	     (body (document
		     (strong ,(pretty-time (current-time)))
		     ,@(map tree->stree c)))))
	 (aux "Comments from AI about corrections")
	 (name (aux-name aux)))
    (aux-set-document aux doc)
    (if (not (buffer->window name))
	(load-buffer-main name :new-window))))

(tm-define (ai-correct model)
  (when (selection-active-any?)
    (with lan (get-env "language")
      (with t (selection-tree)
        (clipboard-cut "primary")
        (with r (cpp-ai-correct t lan model)
          (when (and (tree-func? r 'tuple) (>= (tree-arity r) 1))
            (let* ((l (tree-children r))
                   (s (car l))
                   (c (cdr l)))
	      (if (get-boolean-preference "ai-correct show differences")
		  (with d (compare-versions (tree->stree t) (tree->stree s))
		    (insert (stree->tree d)))
		  (insert s))
	      (when (and (> (length c) 0)
			 (get-boolean-preference "ai-correct explain"))
		(open-comments c)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ai-translate into model)
  (when (selection-active-any?)
    (with from (get-env "language")
      (with t (selection-tree)
        (clipboard-cut "primary")
        (with r (cpp-ai-translate t from into model)
          (insert r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy and paste while compressing non natural language text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ai-copy)
  (when (selection-active-any?)
    (clipboard-set "primary" (compress-html (selection-tree) 1))))

(tm-define (ai-cut)
  (when (selection-active-any?)
    (ai-copy)
    (clipboard-cut "dummy")))

(define (clipboard-get* key)
  (with t (clipboard-get key)
    (cond ((not (tm-func? t 'tuple)) t)
          ((< (tm-arity t) 2) t)
          ((tm-equal? (tm-ref t 0) "texmacs")
           (tm->string (tm-ref t 1)))
          ((tm-equal? (tm-ref t 0) "extern")
           (tm->string (tm-ref t 1)))
          (else t))))

(tm-define (ai-paste)
  (with t (decompress-html (clipboard-get* "extern") 1)
    (insert t)))
