
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
        (cork->sourcecode s))
      (with s (convert (tm->stree t) "texmacs-stree" "latex-snippet"
                       (cons "texmacs->latex:encoding" "utf-8"))
        ;;(display* "s = " s "\n")
        s)))

(tm-define (ai-cmdline name chat cmd)
  (cpp-ai-latex-command cmd name chat))

(tm-define (ai-request name chat cmd)
  (with missing (and (defined? (quote ai-key-missing)) (ai-key-missing name))
    (if missing
        (object->string `(error ,missing)) ; no request (request_link.cpp)
        (begin
          ;; (the model of the session which asks, init-ai.scm)
          (when (defined? 'ai-request-prepare) (ai-request-prepare name chat))
          (with r (cpp-ai-latex-request cmd name chat)
            (when (defined? 'ai-request-done) (ai-request-done))
            r)))))

(tm-define (ai-result name chat res)
  (with t (cpp-ai-latex-output res name chat)
    (when (string-contains? (object->string (tm->stree t)) "ai-tikz")
      (delayed
        (:idle 100)
        (ai-run-pending-folds)))
    t))

;; The TikZ pictures of an answer are folds of the TikZ plug-in (ai.cpp).
;; Each is made once, as soon as it is complete, while the answer still
;; comes: it is evaluated apart (a silent evaluation, not in a fold of the
;; document, which the answer so far replaces at each piece), and its
;; picture kept by its code (ai-picture, asked by ai.cpp). The folds whose
;; picture is not there yet are pending: once in the document they are
;; filled when it comes.

(define ai-pictures (make-ahash-table))   ; code -> output of the plug-in
(define ai-pictures-busy (make-ahash-table)) ; code -> folds which wait

;; (cork->sourcecode as the code of a fold is sent: cork->utf8 would make the
;; ... of \foreach an ellipsis)
(define (ai-picture-code doc)
  (if (tm-func? doc 'document)
      (string-recompose (map (lambda (l)
                               (if (string? l) (cork->sourcecode l) ""))
                             (cdr doc))
                        "\n")
      (if (string? doc) (cork->sourcecode doc) "")))

(define (ai-picture-input code)
  `(document ,@(map utf8->cork (string-decompose code "\n"))))

;; the picture of the code (in UTF-8), if it was made
(tm-define (ai-picture code)
  (with r (ahash-ref ai-pictures code)
    (and r (stree->tree r))))

;; the picture of the code is made, unless it is or is being made
(tm-define (ai-picture-request code)
  (when (and (not (ahash-ref ai-pictures code))
             (not (ahash-ref ai-pictures-busy code)))
    (ahash-set! ai-pictures-busy code (list))
    (when (not (in? "tikz" (get-style-list)))
      (add-style-package "tikz"))
    (silent-feed* "tikz" "default" (ai-picture-input code)
                  (lambda (r) (ai-picture-made code r))
                  '(:math-input :simplify-output)))
  "")

(define (ai-fill-fold p r)
  (with t (tree-pointer->tree p)
    (tree-pointer-detach p)
    (when (and (tree? t) (tree-is? t 'script-output) (== (tree-arity t) 4))
      (tree-set! t 3 r))))

(define (ai-picture-made code r)
  (ahash-set! ai-pictures code r)
  (with waiting (or (ahash-ref ai-pictures-busy code) (list))
    (ahash-remove! ai-pictures-busy code)
    (for (p waiting) (ai-fill-fold p r))))

;; the pending folds of the document (not those of an answer which is
;; still coming, which the next piece replaces): filled now, or when their
;; picture comes
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
            (when (tree-is? t 'script-output)
              (let* ((code (ai-picture-code (tree->stree (tree-ref t 2))))
                     (r (ahash-ref ai-pictures code)))
                (if r (tree-set! t 3 r)
                    (begin
                      (ai-picture-request code)
                      (ahash-set! ai-pictures-busy code
                                  (cons (tree->tree-pointer t)
                                        (or (ahash-ref ai-pictures-busy code)
                                            (list))))))))))))))

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
