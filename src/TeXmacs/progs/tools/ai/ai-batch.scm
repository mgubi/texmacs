
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
  ;; (the model of the session or the fold which asks, init-ai.scm)
  (when (defined? 'ai-request-prepare) (ai-request-prepare name chat))
  (with missing (and (defined? (quote ai-key-missing)) (ai-key-missing name))
    (with r (if missing
                (object->string `(error ,missing)) ; no request (request_link)
                (cpp-ai-latex-request cmd name chat))
      (when (defined? 'ai-request-done) (ai-request-done))
      r)))

(tm-define (ai-result name chat res)
  (with t (cpp-ai-latex-output res name chat)
    ;; (in a fold: the answer alone, init-ai.scm)
    (when (defined? 'ai-result-filter)
      (set! t (ai-result-filter name chat t)))
    (when (string-contains? (object->string (tm->stree t)) "ai-tikz")
      (delayed
        (:idle 100)
        (ai-run-pending-folds)))
    (when (and (== (ai-tikz-policy) "ask") (nnull? (ai-pictures-awaiting))
               (not ai-asking-pictures?))
      (set! ai-asking-pictures? #t)
      (delayed
        (:idle 100)
        (ai-ask-compile-pictures)))
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

;; The code of a picture is written by a remote model, and LaTeX runs it:
;; TeX can read the files of the user (\input), and show them in the
;; picture. The preference "ai tikz compile" says whether the pictures of
;; an answer are compiled: "ask" (the default) asks it once the answer is
;; there, "always" compiles them at once, "never" leaves their folds
;; uncompiled. An uncompiled picture is a fold of the TikZ plug-in, which its
;; usual evaluation compiles later. In a web browser the pictures are made by
;; TikZJax, in the page, which reads no files: they are always made.
(tm-define (ai-tikz-policy)
  (with p (get-preference "ai tikz compile")
    (cond ((defined? 'web-javascript) "always")
          ((in? p '("always" "never")) p)
          (else "ask"))))

(define ai-pictures-waiting (make-ahash-table)) ; code -> #t: to be asked
(define ai-asking-pictures? #f)

;; the codes of the pictures which are not compiled yet, the user to be asked
(tm-define (ai-pictures-awaiting)
  (map car (ahash-table->list ai-pictures-waiting)))

(tm-define (ai-pictures-awaiting-clear)
  (set! ai-pictures-waiting (make-ahash-table)))

(define (ai-tikz-fold-code t)
  (and (tree-is? t 'script-input) (== (tree-arity t) 4)
       (tm-equal? (tree-ref t 0) "tikz")
       (ai-picture-code (tree->stree (tree-ref t 2)))))

;; the uncompiled folds of the pictures of codes, in the document: compiled
(define (ai-compile-pictures codes)
  (with l (select (buffer-tree) '(:* script-input))
    (for (t l)
      (with code (ai-tikz-fold-code t)
        (when (and code (in? code codes))
          (tree-set! t `(with "ai-tikz" "pending"
                          (script-output ,(tree->stree (tree-ref t 0))
                                         ,(tree->stree (tree-ref t 1))
                                         ,(tree->stree (tree-ref t 2))
                                         (script-busy)))))))
    (ai-run-pending-folds)))

(tm-define (ai-ask-compile-pictures)
  (with codes (ai-pictures-awaiting)
    (ai-pictures-awaiting-clear)
    (set! ai-asking-pictures? #f)
    (when (nnull? codes)
      (user-confirm
       (string-append "Compile the TikZ pictures of the answer with LaTeX? "
                      "Their code comes from the model, and could read "
                      "your files.")
       #f
       (lambda (answ)
         (when answ (ai-compile-pictures codes)))))))

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
(define (ai-picture-make code)
  (when (and (not (ahash-ref ai-pictures code))
             (not (ahash-ref ai-pictures-busy code)))
    (ahash-set! ai-pictures-busy code (list))
    (when (not (in? "tikz" (get-style-list)))
      (add-style-package "tikz"))
    (silent-feed* "tikz" "default" (ai-picture-input code)
                  (lambda (r) (ai-picture-made code r))
                  '(:math-input :simplify-output))))

;; asked by ai.cpp for a complete picture of an answer: #t if it is made,
;; or being made (its fold waits for it), #f if it is not compiled (its fold
;; stays uncompiled; with "ask", the user is asked once the answer is there)
(tm-define (ai-picture-request code)
  (with policy (ai-tikz-policy)
    (cond ((or (ahash-ref ai-pictures code) (ahash-ref ai-pictures-busy code))
           #t)
          ((== policy "always") (ai-picture-make code) #t)
          (else
            (when (== policy "ask") (ahash-set! ai-pictures-waiting code #t))
            #f))))

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
                      (ai-picture-make code)
                      (ahash-set! ai-pictures-busy code
                                  (cons (tree->tree-pointer t)
                                        (or (ahash-ref ai-pictures-busy code)
                                            (list))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic correction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-empty? t)
  (or (tm-equal? t "")
      (and (tm-func? t 'document)
           (list-and (map (lambda (x) (tm-equal? x "")) (tm-children t))))))

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

;; The correction and the translation of the selection: the selection is
;; replaced by the answer only when one came; without a key the key is asked
;; (ai-key-missing in init-ai.scm), and an error is said on the status bar
;; rather than put in the document.
(define (ai-error-text t)
  (with s (cond ((tm-atomic? t) (tm->string t))
                ((and (tm-func? t 'document 1) (tm-atomic? (tm-ref t 0)))
                 (tm->string (tm-ref t 0)))
                (else ""))
    (and (string-starts? s "Error:") s)))

(define (ai-key-checked? model what)
  (with missing (and (defined? 'ai-key-missing) (ai-key-missing model))
    (when missing (set-message missing what))
    (not missing)))

(define (ai-answer-ok? t what)
  (cond ((ai-error-text t)
         (set-message (ai-error-text t) what) #f)
        ((ai-empty? t)
         (set-message "No answer" what) #f)
        (else #t)))

(tm-define (ai-correct model)
  (when (and (selection-active-any?) (ai-key-checked? model "Correct"))
    (with lan (get-env "language")
      (with t (selection-tree)
        (with r (cpp-ai-correct t lan model)
          (when (and (tree-func? r 'tuple) (>= (tree-arity r) 1)
                     (ai-answer-ok? (tree-ref r 0) "Correct"))
            (let* ((l (tree-children r))
                   (s (car l))
                   (c (cdr l)))
              (clipboard-cut "primary")
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
  (when (and (selection-active-any?) (ai-key-checked? model "Translate"))
    (with from (get-env "language")
      (with t (selection-tree)
        (with r (cpp-ai-translate t from into model)
          (when (ai-answer-ok? r "Translate")
            (clipboard-cut "primary")
            (insert r)))))))

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
