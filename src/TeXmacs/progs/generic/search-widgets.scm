
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : search-widgets.scm
;; DESCRIPTION : widgets for general purpose editing
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic search-widgets)
  (:use (generic generic-edit)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic search and replace buffers management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-window #f)

(tm-define (search-buffer)
  (string->url "tmfs://aux/search"))

(tm-define (replace-buffer)
  (string->url "tmfs://aux/replace"))

(tm-define (master-buffer)
  (and (buffer-exists? (search-buffer))
       (with mas (buffer-get-master (search-buffer))
         (cond ((nnull? (buffer->windows mas))
                mas)
               ((in? search-window (window-list))
                (buffer-set-master (search-buffer)
                                   (window->buffer search-window))
                (with-buffer (buffer-get-master (search-buffer))
                  (set-search-reference (cursor-path))
                  (set-search-filter))
                (master-buffer))
               ((nnull? (window-list))
                (set! search-window (car (window-list)))
                (master-buffer))
               (else #f)))))

(tm-define (inside-search-buffer?)
  (== (current-buffer) (search-buffer)))

(tm-define (inside-replace-buffer?)
  (== (current-buffer) (replace-buffer)))

(tm-define (inside-search-or-replace-buffer?)
  (in? (current-buffer) (list (search-buffer) (replace-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filtered searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-filter-table (make-ahash-table))

(define (mode-language mode)
  (cond ((== mode "text") "language")
        ((== mode "math") "math-language")
        ((== mode "prog") "prog-language")
        (else "language")))

(define (get-main-attrs getter)
  (list "mode" (getter "mode")
        "language" (getter "language")
        "math-language" (getter "math-language")
        "prog-language" (getter "prog-language")))

(define (set-search-filter)
  (let* ((vars (list "mode" (mode-language (get-env "mode"))))
         (vals (map get-env-tree vars))
         (attrs (append-map list vars vals))
         (env `(attr ,@attrs)))
    (ahash-set! search-filter-table (current-buffer) env)))

(define (get-search-filter)
  (ahash-ref search-filter-table (current-buffer)))

(define (check-same-sub? env var val)
  (cond ((or (null? env) (null? (cdr env))) #f)
        ((tm-equal? (car env) var) (tm-equal? (cadr env) val))
        (else (check-same-sub? (cddr env) var val))))

(define (check-same? new-env old-env)
  ;;(display* "Check " new-env ", " old-env "\n")
  (if (null? old-env) #t
      (and (check-same-sub? new-env (car old-env) (cadr old-env))
           (check-same? new-env (cddr old-env)))))

(define (accept-search-result? p)
  (or (== (get-init "mode") "src")
      (let* ((buf (buffer-tree))
             (rel (path-strip (cDr p) (tree->path buf)))
             (initial (cons 'attr (get-main-attrs get-init)))
             (old-env (get-search-filter))
             (new-env (tree-descendant-env* buf rel initial)))
        ;;(display* p " ~> " new-env "\n")
        (check-same? (tm-children new-env) (tm-children old-env)))))

(define (filter-search-results sels)
  (if (or (null? sels) (null? (cdr sels))) (list)
      (with r (filter-search-results (cddr sels))
        (if (accept-search-result? (car sels))
            (cons* (car sels) (cadr sels) r)
            r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-serial 0)
(define search-filter-out? #f)

(define (tree-perform-search t what p limit)
  (let* ((source-mode 2)
         (old-mode (get-access-mode))
         (new-mode (if (== (get-init "mode") "src") source-mode old-mode)))
    (set-access-mode new-mode)
    (let* ((cp (cDr (cursor-path)))
           (pos (if (list-starts? cp p) (list-tail cp (length p)) (list)))
           (r (tree-search-tree-at t what p pos limit)))
      (set-access-mode old-mode)
      r)))

(define (go-to* p)
  (go-to p)
  (when (and (not (cursor-accessible?)) (not (in-source?)))
    (cursor-show-hidden)
    (delayed
      (:pause 50)
      (set! search-serial (+ search-serial 1))
      (perform-search-sub 100 #f))))

(define (perform-search-sub limit top?)
  (let* ((what (buffer-get-body (search-buffer)))
         (ok? #t)
	 (too-many-matches? #f)
	 (serial search-serial)
         (go-to** (if top? go-to* go-to)))
    (when (tm-func? what 'document 1)
      (set! what (tm-ref what 0)))
    (when (tm-func? what 'inactive 1)
      (set! what (tm-ref what 0)))
    (when (tm-func? what 'inactive* 1)
      (set! what (tm-ref what 0)))
    (with-buffer (master-buffer)
      (if (tree-empty? what)
          (begin
            (selection-cancel)
            (cancel-alt-selection "alternate")
            (go-to** (get-search-reference #t)))
          (let* ((t (buffer-tree))
                 (sels* (tree-perform-search t what (tree->path t) limit))
                 (sels (filter-search-results sels*)))
	    ;;(display* "sels= " sels "\n")
	    (cond ((>= (length sels*) limit)
		   (set-alt-selection "alternate" sels)
		   (set! too-many-matches? #t)
		   (next-search-result #t #f))
		  ((null? sels)
		   (selection-cancel)
		   (cancel-alt-selection "alternate")
		   (go-to** (get-search-reference #t))
		   (set! ok? #f))
		  (else
		   (set-alt-selection "alternate" sels)
		   (with after? (next-search-result #t #f)
		     (when (not after?)
		       (selection-cancel))))))))
    (with-buffer (search-buffer)
      (if ok?
          (init-default "bg-color")
          (init-env "bg-color" "#fff0f0")))
    (when too-many-matches?
      ;;(display* "Extend limit to " (* 2 limit) "\n")
      (delayed
	(:pause limit)
	(when (== serial search-serial)
	  (perform-search-sub (* 2 limit) top?))))))

(define (perform-search*)
  (set! search-serial (+ search-serial 1))
  (perform-search-sub 100 #t))

(define (perform-search)
  (search-show-only)
  (perform-search*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting a particular next or previous search result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-search-reference cur)
  (set-alt-selection "search-reference" (list cur cur)))

(define (get-search-reference forward?)
  (with sel (get-alt-selection "search-reference")
    (if (nnull? sel) (car sel)
        (if forward? (cursor-path) (cursor-path*)))))

(define (search-next sels cur strict?)
  (with sel (next-search-hit sels cur strict?)
    (and (nnull? sel) sel)))

(define (search-previous sels cur strict?)
  (with sel (previous-search-hit sels cur strict?)
    (and (nnull? sel) sel)))

(define (next-search-result forward? strict?)
  (let* ((cur (get-search-reference forward?))
         (sel (navigate-search-hit cur forward? #f strict?)))
    (and (nnull? sel)
         (begin
           (selection-set-range-set sel)
           (go-to* (car sel))
           (when strict? (set-search-reference (car sel)))
           #t))))

(define (extreme-search-result last?)
  (let* ((cur (get-search-reference last?))
         (sel (navigate-search-hit cur last? #t #f)))
    (and (nnull? sel)
         (begin
           (selection-set-range-set sel)
           (go-to* (car sel))
           (set-search-reference (car sel))))))

(tm-define (search-next-match forward?)
  (with-buffer (master-buffer)
    (next-search-result forward? #t)))

(tm-define (search-extreme-match last?)
  (with-buffer (master-buffer)
    (extreme-search-result last?)))

(tm-define (search-rotate-match)
  (with ok? (search-next-match #t)
    (when (not ok?)
      (search-extreme-match #f))))

(tm-define ((search-cancel u) . args)
  (search-show-all)
  (set! search-serial (+ search-serial 1))
  (with-buffer (master-buffer)
    (cancel-alt-selection "alternate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace occurrences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-tree)
  (and (buffer-exists? (search-buffer))
       (with what (buffer-get-body (search-buffer))
         (when (tm-func? what 'document 1)
           (set! what (tm-ref what 0)))
         what)))

(define (by-tree)
  (and (buffer-exists? (replace-buffer))
       (with by (buffer-get-body (replace-buffer))
         (when (tm-func? by 'document 1)
           (set! by (tm-ref by 0)))
         by)))

(define (get-positions-in-string-rec what match b)
  ;; Return the list of positions of the strings in what,
  ;; from position b in the string match.
  (if (null? what) (list)
      (with w (car what)
	(with pos (string-search-forwards w b match)
	  (if (< pos b) (list)
	      (with pos1 (+ pos (string-length w))
		(cons (list pos pos1)
		      (get-positions-in-string-rec
		       (cdr what) match pos1))))))))

(define (get-positions-in-string what match)
  ;; Return the list of positions of the strings
  ;; in what in the string match.
  (with l (get-positions-in-string-rec what match 0)
    (if (!= (length l) (length what)) (list) l)))

(define (get-wildcards-in-string-rec what l match b)
  (if (null? what) (list)
      (with w (car what)
	(cond ((tm-func? w `wildcard 1)
	       (if (null? l) `((:wildcard ,(tm->stree (tm-ref w 0))
					  ,(string-tail match b)))
		   (with pos (car l)
		     (cons `(:wildcard ,(tm->stree (tm-ref w 0))
				       ,(substring match b (first pos)))
			   (get-wildcards-in-string-rec
			    (cdr what) l match (first pos))))))
	      ((tm-atomic? w)
	       (if (null? l) (list)
		   (with pos (car l)
		     (get-wildcards-in-string-rec
		      (cdr what) (cdr l) match (second pos)))))
	      (else (list))))))

(define (get-wildcards-in-string what match)
  (with l (get-positions-in-string
	   (map tm->string (list-filter what tm-atomic?)) match)
    (if (null? l) (list)
	(get-wildcards-in-string-rec what l match 0))))

(define (aligned-runs t p1 p2 what)
  ;; the run of the hit from p1 to p2 in t (all of t if both are empty)
  ;; which each child of the formula pattern what takes, as the search
  ;; matches it (#520), or #f
  (with runs (tree-search-align t p1 p2 what)
    (when (null? runs)
      (with why (tree-search-align-reason)
        (when (!= why "") (set! replace-skip-reason why))))
    (and (nnull? runs) runs)))

(define replace-skip-reason #f)   ;; why the last hit was left, if known
(define replace-skip-message #f)  ;; the message of the last hit left

(define (get-wildcards-aligned what runs)
  ;; the wildcards of the formula pattern what, whose children took runs
  (append-map get-wildcards (tm-children what) runs))

(define (get-wildcards-var what match)
  (cond ((tm-equal? what match) (list))
        ((tm-func? what 'wildcard 1)
         (list `(:wildcard ,(tm->stree (tm-ref what 0)) ,(tm->stree match))))
        ((tm-equal? what "")
         (list `(:empty ,(tm->stree match))))
        ;; a formula pattern against a formula, a string or a tag: its
        ;; children are aligned with the text of the match as the search
        ;; does, a wildcard taking a part of a string, or several children
        ;; (#520); they were matched child by child
        ((and (tm-func? what 'concat) (not (tm-func? match 'document))
              (aligned-runs match '() '() what))
         => (lambda (runs) (get-wildcards-aligned what runs)))
        ;; the pattern matches, but its values would lose text: none (the
        ;; hit is left), not those of the rules below
        ((and (tm-func? what 'concat) replace-skip-reason)
         (list))
	((and (tm-atomic? match)
	      (tm-func? what 'concat))
	 (get-wildcards-in-string (tm-children what) (tm->string match)))
        ((and (tm-compound? what) (tm-compound? match)
              (== (tm-label what) (tm-label match))
              (== (tm-arity what) (tm-arity match)))
         (append-map get-wildcards (tm-children what) (tm-children match)))
        (else (list))))

(define (get-wildcards what match)
  ;;(display* "-> get-wildcards " what " " match "\n")
  (with ret (get-wildcards-var what match)
    ;;(display* "<- " ret "\n")
    ret))

(define (find-wildcard wc which)
  ;;(display* "find-wildcard " wc " " which "\n")
  (cond ((null? wc) "")
        ((and (tm-func? (car wc) :wildcard 2)
              (== (cadr (car wc)) which))
         (caddr (car wc)))
        (else (find-wildcard (cdr wc) which))))

(define (find-empty wc)
  (cond ((null? wc)
         (cons "" wc))
        ((tm-func? (car wc) :empty 1)
         (cons (cadar wc) (cdr wc)))
        (else (with r (find-empty (cdr wc))
                (cons (car r) (cons (car wc) (cdr r)))))))

(define (replace-wildcards-list l wc)
  ;;(display* "replace-wildcards-list " l " " wc "\n")
  (if (null? l) (cons l wc)
      (let* ((h (replace-wildcards (car l) wc))
             (t (replace-wildcards-list (cdr l) (cdr h))))
        (cons (cons (car h) (car t)) (cdr t)))))

(define (spliced-value v)
  ;; the children which the value v of a wildcard gives in a formula
  (cond ((tm-func? v 'concat) (tm-children v))
        ((tm-equal? v "") (list))
        (else (list v))))

(define (replace-wildcards-concat l wc)
  ;; the children l of a formula of the replacement, each marked #t if it
  ;; comes from the value of a wildcard, which is spliced into the formula
  (if (null? l) (cons l wc)
      (let* ((c (car l))
             (h (if (tm-func? c 'wildcard 1)
                    (with v (find-wildcard wc (tm->stree (tm-ref c 0)))
                      (cons (map (lambda (x) (cons #t x)) (spliced-value v))
                            wc))
                    (with r (replace-wildcards c wc)
                      (cons (list (cons #f (car r))) (cdr r)))))
             (t (replace-wildcards-concat (cdr l) (cdr h))))
        (cons (append (car h) (car t)) (cdr t)))))

(define (merge-inserted l)
  ;; two strings in a row, one of them from a value, are one string: the
  ;; parts of the replacement which the user wrote are kept as they are
  (cond ((or (null? l) (null? (cdr l))) l)
        ((and (tm-atomic? (cdar l)) (tm-atomic? (cdadr l))
              (or (caar l) (caadr l)))
         (merge-inserted
           (cons (cons #t (string-append (tm->string (cdar l))
                                         (tm->string (cdadr l))))
                 (cddr l))))
        (else (cons (car l) (merge-inserted (cdr l))))))

(define (replace-wildcards by wc)
  ;;(display* "replace-wildcards " by " " wc "\n")
  (cond ((null? wc)
         (cons by wc))
        ((tm-func? by 'wildcard 1)
         (cons (find-wildcard wc (tm->stree (tm-ref by 0))) wc))
        ((tm-equal? by "")
         (find-empty wc))
        ((tm-atomic? by)
         (cons by wc))
        ((and (tm-func? by 'concat)
              (nnull? (list-filter (tm-children by)
                                   (lambda (c) (tm-func? c 'wildcard 1)))))
         ;; a value of several children is spliced into the formula, and
         ;; merged with the strings around it (#520)
         (let* ((r (replace-wildcards-concat (tm-children by) wc))
                (l (map cdr (merge-inserted (car r)))))
           (cons (cond ((null? l) "")
                       ((null? (cdr l)) (car l))
                       (else (cons 'concat l)))
                 (cdr r))))
        (else (with r (replace-wildcards-list (tm-children by) wc)
                (cons (cons (tm-label by) (car r)) (cdr r))))))

(define (get-wildcards-of-hit what match hit)
  ;; the wildcards of what in its match; hit is () or the tree in which the
  ;; match was found and the paths of the match in it, by which a formula
  ;; pattern is aligned: also a match which a wildcard carried out of a tag
  ;; (#515, #520), which its subtree does not show
  (with runs (and (tm-func? what 'concat) (nnull? hit)
                  (aligned-runs (car hit) (cadr hit) (caddr hit) what))
    (cond (runs (get-wildcards-aligned what runs))
          ((and (tm-func? what 'concat) replace-skip-reason) (list))
          (else (get-wildcards what match)))))

(define (adjust-by by what match . hit)
  ;; the replacement of match, a match of what, by by: its wildcards take
  ;; the values of those of what; a value of several children is spliced
  ;; into the formula around the wildcard (#520)
  (let* ((swhat (and (tm-atomic? what) (tm->string what)))
         (smatch (and (tm-atomic? match) (tm->string match)))
         (sby (and (tm-atomic? by) (tm->string by)))
         (lwhat (and (tm-compound? what) (tm->list what)))
         (lmatch (and (tm-compound? match) (tm->list match))))
    (cond ((tm-equal? what match) by)
          ((and (tm-func? what 'concat)
                (tm-func? match 'concat)
                (< (length lwhat) (length lmatch))
                (tm-equal? what (sublist lmatch 0 (length lwhat))))
           (let* ((lby (if (tm-func? by 'concat) (tm->list by) `(concat ,by)))
                  (xby (sublist lmatch (length lwhat) (length lmatch))))
             (append lby xby)))
          ((and swhat smatch sby (== (locase-all swhat) (locase-all smatch)))
           (cond ((== (locase-all swhat) smatch) (locase-all sby))
                 ((== (upcase-all swhat) smatch) (upcase-all sby))
                 ((== (locase-first swhat) smatch) (locase-first sby))
                 ((== (upcase-first swhat) smatch) (upcase-first sby))
                 (else sby)))
          ((nnull? (list-intersection (wildcard-names by)
                                      (wildcard-names what)))
           ;; the replacement takes values of the wildcards of the pattern:
           ;; #f if a value is not known (the pattern could not be aligned
           ;; with the match), so that the hit is not replaced: inserting
           ;; the (wildcard ...) tags, or nothing in their place, would lose
           ;; the text which they took (#520)
           (set! replace-skip-reason #f)
           (let* ((wc (get-wildcards-of-hit what match hit))
                  (known (map cadr (list-filter wc (cut tm-func? <> :wildcard 2))))
                  ;; the wildcards whose values take the other content of
                  ;; a tag of the hit, which the replacement must take once
                  (carriers (if (or (null? hit) (not (tm-func? what 'concat)))
                                (list)
                                (map tm->stree
                                     (tree-search-align-carriers
                                       (car hit) (cadr hit) (caddr hit) what))))
                  (uses (wildcard-uses by)))
             (cond ((nlist-and (map (cut in? <> known)
                                    (list-intersection (wildcard-names by)
                                                       (wildcard-names what))))
                    #f)
                   ((nlist-and (map (lambda (c) (== (count-in c uses) 1))
                                    carriers))
                    (set! replace-skip-reason
                          "the replacement would drop or repeat the other content of a tag")
                    #f)
                   (else (car (replace-wildcards by wc))))))
          (else
            (with wc (get-wildcards-of-hit what match hit)
              (if (null? wc) by (car (replace-wildcards by wc))))))))

(define (nlist-and l) (not (list-and l)))

(define (count-in x l)
  (length (list-filter l (cut == <> x))))

(define (wildcard-uses t)
  ;; the names of the wildcards in t, once for each occurrence
  (cond ((tm-func? t 'wildcard 1) (list (tm->stree (tm-ref t 0))))
        ((tm-compound? t) (append-map wildcard-uses (tm-children t)))
        (else (list))))

(define (wildcard-names t)
  ;; the names of the wildcards in t
  (cond ((tm-func? t 'wildcard 1) (list (tm->stree (tm-ref t 0))))
        ((tm-compound? t) (list-remove-duplicates
                            (append-map wildcard-names (tm-children t))))
        (else (list))))

(define (search-hit sel)
  ;; the buffer and the paths in it of the hit sel, for adjust-by
  (let* ((t (buffer-tree))
         (p (tree->path t)))
    (if (and (list-starts? (car sel) p) (list-starts? (cadr sel) p))
        (list t (list-tail (car sel) (length p))
              (list-tail (cadr sel) (length p)))
        (list))))

(define (inner-hit sel what)
  ;; the part of the hit sel which Replace replaces, if the formula pattern
  ;; what matches inside an argument of a tag of the hit, and nothing else
  ;; (a lone tag, a cascaded match): the replacement goes inside that
  ;; argument, as when the tag is alone (#520); else sel
  (with hit (search-hit sel)
    (if (or (null? hit) (not (tm-func? what 'concat))) sel
        (with in (tree-search-inner-hit (car hit) (cadr hit) (caddr hit) what)
          (if (null? in) sel
              (with p (tree->path (car hit))
                (list (append p (car in)) (append p (cadr in)))))))))

(define replace-last #f)

(define (replace-state cur)
  ;; the place where Replace goes on, for this buffer and this pattern
  (list (url->string (current-buffer)) (tm->stree (search-tree)) cur))

(define (replace-from cur)
  ;; Replace goes on from cur, after its last replacement (or a hit which it
  ;; left): the next hit starts there, also when the replacement is inside
  ;; a hit (a lone tag, whose replaced argument is inside it, #520). Only
  ;; while the buffer, the pattern and the reference are the same
  (set! replace-last (replace-state cur))
  (set-search-reference cur))

(define (search-next-after sels cur)
  ;; the first hit of sels which starts at cur or after it
  (cond ((or (null? sels) (null? (cdr sels))) #f)
        ((path-less? (car sels) cur) (search-next-after (cddr sels) cur))
        (else (list (car sels) (cadr sels)))))

(define (replace-next* by)
  ;;(display* "replace-next* " by "\n")
  (let* ((sels (get-alt-selection "alternate"))
         (cur (get-search-reference #t)))
    (and (nnull? sels)
         (and-with sel0 (if (== (replace-state cur) replace-last)
                            (search-next-after sels cur)
                            (search-next sels cur #f))
           (let* ((what (search-tree))
                  (sel (inner-hit sel0 what)))
             (go-to* (car sel))
             (selection-set-range-set sel)
             (let* ((hit (search-hit sel))
                    (by* (apply adjust-by (cons* by what (selection-tree) hit)))
                    ;; a hit carried out of a tag (or into it) by a wildcard
                    ;; of a formula pattern; not one of several paragraphs
                    (parts (if (or (not by*) (null? hit)
                                   (tm-func? what 'document))
                               (list)
                               (tree-search-replace-parts
                                 (car hit) (cadr hit) (caddr hit) what))))
               (cond ((not by*)
                      ;; the values of the wildcards are not known: the hit
                      ;; is left as it is, and Replace goes on after it
                      ;; (#520)
                      (selection-cancel)
                      (go-to (cadr sel0))
                      (replace-from (cursor-path))
                      (set! replace-skip-message
                            (string-append
                              "a match was left as it is: "
                              (or replace-skip-reason
                                  "the pattern could not be aligned with it")))
                      (set-message replace-skip-message "Replace")
                      (!= (car sel0) (cadr sel0)))
                     ((null? parts)
                      (clipboard-cut "dummy")
                      (with by* (tm->tree by*)
                        (insert-go-to (tree-copy by*) (path-end by* '())))
                      ;; Replace goes on after the replacement, never inside
                      ;; it (a lone tag became a formula, whose start was
                      ;; before the cursor)
                      (replace-from (cursor-path))
                      #t)
                     (else
                      (replace-hit-parts (tm->tree by*) parts hit
                                         (tree->path (buffer-tree)))
                      (replace-from (cursor-path))
                      #t))))))))

(define (inside-argument? p q i)
  ;; is the position p inside the argument i of the tree at q?
  (and (> (length p) (+ (length q) 1))
       (list-starts? p (rcons q i))))

(define (crossed-tags t p1 p2 . ends)
  ;; the paths in t of the tags which the hit from p1 to p2 goes out of or
  ;; into (not both of its ends inside the same argument), the innermost
  ;; first; also those on the way to the ends of its parts (a hit which its
  ;; outer wildcards carry around a tag in which the pattern matches)
  (let* ((tags (lambda (p)
                 (let loop ((u t) (q (list)) (l p) (r (list)))
                   (if (or (null? l) (tree-atomic? u)) r
                       (let* ((r2 (if (and (nnull? q)
                                           (not (tree-is? u 'concat))
                                           (not (tree-is? u 'document))
                                           (not (and (inside-argument? p1 q (car l))
                                                     (inside-argument? p2 q (car l)))))
                                      (cons q r) r)))
                         (if (or (null? (cdr l)) (>= (car l) (tree-arity u)))
                             r2
                             (loop (tree-ref u (car l)) (rcons q (car l))
                                   (cdr l) r2)))))))
         (all (list-remove-duplicates
                (append (tags p1) (tags p2)
                        (append-map tags (if (null? ends) ends (car ends)))))))
    (sort all (lambda (a b) (> (length a) (length b))))))

(define (removable? u crossed)
  ;; can the tag u, which the hit crossed, be removed after the cut? Only if
  ;; it has nothing left: its children are empty strings, attributes which
  ;; the cursor cannot reach (the color of a with), or crossed tags which
  ;; can be removed; an image, a label... is content. The parts of a table
  ;; are never removed: their content is empty
  (and (tree-compound? u)
       (not (tree-in? u '(concat document table row cell subtable tformat)))
       (list-and (map (lambda (i)
                        (nothing-left? (tree-ref u i)
                                       (content-child? u i) crossed))
                      (iota (tree-arity u))))))

(define (content-child? u i)
  ;; does the argument i of u hold content: one which the cursor reaches, or
  ;; a hidden one (the body of a folded), not an attribute (of a with)?
  (or (tree-accessible-child? u i)
      (and (not (and (tree-is? u 'with) (< i (- (tree-arity u) 1))))
           (in? (tree-child-type u i) '("regular" "adhoc" "unknown")))))

(define (nothing-left? c accessible? crossed)
  (cond ((tree-atomic? c) (or (== (tree->string c) "") (not accessible?)))
        ((tree-is? c 'concat)
         (list-and (map (lambda (x) (nothing-left? x #t crossed))
                        (tree-children c))))
        (else (and (list-or (map (lambda (x) (tree-eq? x c)) crossed))
                   (removable? c crossed)))))

(define (normalize-formula f)
  ;; the formula f without empty strings and without two strings in a row,
  ;; a formula of one child being that child (with editing operations,
  ;; which keep the positions)
  (when (and f (tree->path f) (tree-is? f 'concat))
    (let loop ((i 0))
      (when (< i (tree-arity f))
        (cond ((and (> (tree-arity f) 1) (tree-atomic? (tree-ref f i))
                    (== (tree->string (tree-ref f i)) ""))
               (tree-remove f i 1)
               (loop i))
              ((and (> i 0) (tree-atomic? (tree-ref f (- i 1)))
                    (tree-atomic? (tree-ref f i)))
               (tree-join f (- i 1))
               (loop i))
              (else (loop (+ i 1))))))
    (cond ((== (tree-arity f) 1) (tree-remove-node f 0))
          ((== (tree-arity f) 0) (tree-assign f "")))))

(define (range-formula a b)
  ;; the formula in which the text from a to b lies, or #f
  (let* ((n (list-common-left a b))
         (f (path->tree (list-head a n)))
         (g (and f (> n 0) (tree-up f))))
    (cond ((and f (tree-is? f 'concat)) f)
          ((and g (tree-is? g 'concat)) g)
          (else #f))))

(define (cut-range a b)
  ;; remove the text from a to b, in one string or in one formula, with
  ;; editing operations (a selection is extended to the tags around it,
  ;; and to the base of a script: it removed text outside the hit, #520).
  ;; The strings which are left empty are removed afterwards
  (let* ((n0 (list-common-left a b))
         (t0 (path->tree (list-head a n0)))
         ;; a whole tag, from before it to after it: in its formula
         (n (if (and t0 (tree-compound? t0) (> n0 0)
                     (== (length a) (+ n0 1)) (== (length b) (+ n0 1)))
                (- n0 1) n0))
         (q (list-head a n))
         (f (path->tree q)))
    (cond ((not f) (noop))
          ((tree-atomic? f)
           (when (and (== n (- (length a) 1)) (== n (- (length b) 1))
                      (< (cAr a) (cAr b)))
             (tree-remove f (cAr a) (- (cAr b) (cAr a)))))
          ((and (tree-is? f 'concat) (> (length a) (+ n 1))
                (> (length b) (+ n 1)))
           (let* ((k1 (list-ref a n)) (ra (list-tail a (+ n 1)))
                  (k2 (list-ref b n)) (rb (list-tail b (+ n 1)))
                  (c1 (tree-ref f k1)) (c2 (tree-ref f k2))
                  (s1? (and (tree-atomic? c1) (null? (cdr ra))))
                  (s2? (and (tree-atomic? c2) (null? (cdr rb))))
                  (lo (if (and (tree-compound? c1) (== ra '(0))) k1 (+ k1 1)))
                  (hi (if (and (tree-compound? c2) (== rb '(1))) k2 (- k2 1))))
             (when (< k1 k2)
               (when (and s2? (> (car rb) 0)) (tree-remove c2 0 (car rb)))
               (when (<= lo hi) (tree-remove f lo (+ (- hi lo) 1)))
               (when (and s1? (< (car ra) (string-length (tree->string c1))))
                 (tree-remove c1 (car ra)
                              (- (string-length (tree->string c1)) (car ra)))))
             (when (and (== k1 k2) (tree-compound? c1)
                        (== ra '(0)) (== rb '(1)))
               (tree-remove f k1 1))))
          (else (noop)))))

(define (replace-hit-parts by* parts hit p)
  ;; a hit which a wildcard carried out of a tag or into one: only its
  ;; parts are removed, in the tag and outside (a selection would take the
  ;; whole tag, and the replacement drops the rest of it), by* goes after
  ;; the tag which it leaves, or before the one which it enters, and a tag
  ;; which is left without text is removed (#520). The parts are removed
  ;; from the last one, so that the paths of the others stay valid, and the
  ;; formulas are normalized at the end
  (let* ((abs (map (lambda (q) (append p q)) parts))
         (ins (cAr abs))
         (tag (path->tree (cDr ins)))
         (ranges (let loop ((l (cDr abs)))
                   (if (or (null? l) (null? (cdr l))) (list)
                       (cons (list (car l) (cadr l)) (loop (cddr l))))))
         (forms (list-filter (map (lambda (r) (range-formula (car r) (cadr r)))
                                  ranges)
                             identity))
         (tags (map (lambda (q) (path->tree (append p q)))
                    (crossed-tags (car hit) (cadr hit) (caddr hit)
                                  (cDr parts)))))
    (for (r (reverse ranges))
      (cut-range (car r) (cadr r)))
    (go-to (rcons (tree->path tag) (cAr ins)))
    (insert-go-to (tree-copy by*) (path-end by* '()))
    (with cur (position-new-path (cursor-path))
      (for (u tags)
        (when (and u (tree->path u) (removable? u tags))
          (with up (tree-up u)
            (if (and up (tree-is? up 'concat))
                (begin
                  (tree-remove up (tree-index u) 1)
                  (set! forms (cons up forms)))
                (tree-assign u "")))))
      (for (f (sort (list-filter forms (lambda (f) (tree->path f)))
                    (lambda (f g) (> (length (tree->path f))
                                     (length (tree->path g))))))
        (normalize-formula f))
      (go-to (position-get cur))
      (position-delete cur))))

(define (replace-next by)
  ;; TODO: replacing all occurrences of 'a' by 'axa' may result in
  ;; a segmentation fault.  We may wish to protect against that
  (with old-p (cursor-path)
    (and (replace-next* by)
         (with mid-p (cursor-path)
           (perform-search*)
           (with new-p (cursor-path)
             (or (and (path-less? old-p new-p)
                      ;; (path-less? mid-p new-p)
                      ;; Commenting fixes bug #62534
                      ;; Check also bug #59508
                      )
                 (search-next-match #t)))))))

(tm-define (replace-one)
  (and-with by (by-tree)
    (with-buffer (master-buffer)
      (start-editing)
      (replace-next by)
      (end-editing))
    (perform-search*)))

(tm-define (replace-all)
  (and-with by (by-tree)
    (with-buffer (master-buffer)
      (start-editing)
      (while (replace-next by)
        (perform-search*))
      (end-editing))
    (perform-search*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized keyboard shortcuts in search and replace modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-kbd-intercepted? #f)

(tm-define (keyboard-press key time)
  (:require (inside-search-buffer?))
  (set! search-kbd-intercepted? #f)
  (former key time)
  (when (not search-kbd-intercepted?)
    (perform-search)))

(tm-define (kbd-enter t shift?)
  (:require (inside-search-buffer?))
  (if (or shift? (inside? 'inactive) (inside? 'inactive*))
      (former t shift?)
      (begin
        (set! search-kbd-intercepted? #t)
        (search-rotate-match))))

(tm-define (kbd-enter t shift?)
  (:require (inside-replace-buffer?))
  (if (or shift? (inside? 'inactive) (inside? 'inactive*))
      (former t shift?)
      (replace-one)))

(tm-define (kbd-incremental t forwards?)
  (:require (inside-search-or-replace-buffer?))
  (set! search-kbd-intercepted? #t)
  (search-next-match forwards?))

(tm-define (traverse-incremental t forwards?)
  (:require (inside-search-or-replace-buffer?))
  (set! search-kbd-intercepted? #t)
  (search-next-match forwards?))

(tm-define (traverse-extremal t forwards?)
  (:require (inside-search-or-replace-buffer?))
  (set! search-kbd-intercepted? #t)
  (search-extreme-match forwards?))

(kbd-map
  (:require (inside-search-or-replace-buffer?))
  ("std ?" (make 'select-region))
  ("std 1" (insert '(wildcard "x")))
  ("std 2" (insert '(wildcard "y")))
  ("std 3" (insert '(wildcard "z"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hacks for keyboard events between the moments that
;; a search was triggered and that the search bar actually shows up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define waiting-for-toolbar? #f)
(define pending-key-strokes "")
(define current-search #f)
(define current-replace #f)

(define (wait-for-toolbar)
  (set! waiting-for-toolbar? #t)
  (set! pending-key-strokes "")
  (delayed
    (:pause 3000)
    (when waiting-for-toolbar?
      (stop-waiting-for-toolbar))))

(define (stop-waiting-for-toolbar)
  (set! waiting-for-toolbar? #f))

(tm-define (keyboard-press key time)
  (:require waiting-for-toolbar?)
  (when (== (tmstring-length key) 1)
    (set! pending-key-strokes (string-append pending-key-strokes key))))

(define (notify-bar-change)
  ;; FIXME: not clear what is the most appriate setting here
  ;; The value 127 is safest, but causes the whole document to be re-typeset 
  (if (style-has? "beamer-style")
      (notify-change 127)
      (notify-change 68)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("allow-blank-match" "on" noop)
  ("allow-initial-match" "on" noop)
  ("allow-partial-match" "on" noop)
  ("allow-injective-match" "on" noop)
  ("allow-cascaded-match" "on" noop)
  ("case-insensitive-match" "off" noop))

(tm-define (toggle-search-preference which)
  (:synopsis "Toggle the search preference @which")
  (:check-mark "v" preference-on?)
  (toggle-preference which)
  (perform-search*))

(menu-bind search-preferences-menu
  ("Allow blank matches" (toggle-search-preference "allow-blank-match"))
  ("Allow initial matches" (toggle-search-preference "allow-initial-match"))
  ("Allow partial matches" (toggle-search-preference "allow-partial-match"))
  ("Allow injective matches" (toggle-search-preference "allow-injective-match"))
  ("Allow cascaded matches" (toggle-search-preference "allow-cascaded-match"))
  ("Disable case sensitivity"
   (toggle-search-preference "case-insensitive-match")))

(tm-define (search-document)
  (if (buffer-exists? (search-buffer))
      (buffer->tree (search-buffer))
      `(document "")))

(tm-widget ((search-widget u style init aux) quit)
  (padded
    (resize "600px" "100px"
      (texmacs-input `(with ,@init ,(search-document))
                     `(style (tuple ,@style)) aux))
    ===
    (hlist
      ((balloon (icon "tm_search_first.xpm") "First occurrence")
       (search-extreme-match #f))
      ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
       (search-next-match #f))
      ((balloon (icon "tm_search_next.xpm") "Next occurrence")
       (search-next-match #t))
      ((balloon (icon "tm_search_last.xpm") "Last occurrence")
       (search-extreme-match #t))
      >>>
      (=> (balloon (icon "tm_preferences.xpm") "Search preferences")
          (link search-preferences-menu))
      ((check (balloon (icon "tm_filter.xpm") "Only show paragraphs with hits")
              "v" (search-filter-enabled?))
       (search-toggle-filter))
      ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
       (set-boolean-preference "toolbar search" #t)
       (quit)
       (toolbar-search-start))
      ((balloon (icon "tm_close_tool.xpm") "Close search tool")
       (quit)))))

(tm-tool* (search-tool win u style init aux)
  (:name "Search")
  (:quit ((search-cancel u)))
  ===
  (horizontal
    //
    (vertical
      (resize "360px" "60px"
        (texmacs-input `(with ,@init ,(search-document))
                       `(style (tuple ,@style)) aux))
      ===
      (hlist
        ((balloon (icon "tm_search_first.xpm") "First occurrence")
         (search-extreme-match #f))
        ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
         (search-next-match #f))
        ((balloon (icon "tm_search_next.xpm") "Next occurrence")
         (search-next-match #t))
        ((balloon (icon "tm_search_last.xpm") "Last occurrence")
         (search-extreme-matche #t))
        >>>
        (=> (balloon (icon "tm_preferences.xpm") "Search preferences")
            (link search-preferences-menu))
        ((check (balloon (icon "tm_filter.xpm")
                         "Only show paragraphs with hits")
                "v" (search-filter-enabled?))
         (search-toggle-filter)))
      ===)
    //))

(tm-define (open-search)
  (:interactive #t)
  (when (not (inside-search-buffer?))
    (let* ((u (current-buffer))
           (st (embedded-style-list "macro-editor"))
           (init (get-main-attrs get-env))
           (aux (search-buffer))
           (tool (list 'search-tool u st init aux)))
      (buffer-set-master aux u)
      (set! search-window (current-window))
      (set-search-reference (cursor-path))
      (set-search-filter)
      (set! search-filter-out? #f)
      (if (side-tools?)
          (tool-focus :bottom-right tool aux)
          (begin
            (gui-set-next-window-as-popup)
            (dialogue-window (search-widget u st init aux)
                             (search-cancel u)
                             "Search" aux))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and replace widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("allow-blank-replace" "off" noop)
  ("allow-initial-replace" "off" noop)
  ("allow-partial-replace" "off" noop)
  ("allow-injective-replace" "off" noop))

(menu-bind replace-preferences-menu
  ("Allow blank matches" (toggle-search-preference "allow-blank-replace"))
  ("Allow cascaded matches" (toggle-search-preference "allow-cascaded-match"))
  ("Disable case sensitivity"
   (toggle-search-preference "case-insensitive-match")))

(tm-define (replace-document)
  (if (buffer-exists? (replace-buffer))
      (buffer->tree (replace-buffer))
      `(document "")))

(tm-widget ((replace-widget u style init saux raux) quit)
  (padded
    (resize "600px" "100px"
      (texmacs-input `(with ,@init ,(search-document))
                     `(style (tuple ,@style)) saux))
    === ===
    (resize "600px" "100px"
      (texmacs-input `(with ,@init ,(replace-document))
                     `(style (tuple ,@style)) raux))
    === ===
    (hlist
      ((balloon (icon "tm_search_first.xpm") "First occurrence")
       (search-extreme-match #f))
      ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
       (search-next-match #f))
      ((balloon (icon "tm_search_next.xpm") "Next occurrence")
       (search-next-match #t))
      ((balloon (icon "tm_search_last.xpm") "Last occurrence")
       (search-extreme-match #t))
      // // //
      ((balloon (icon "tm_replace_one.xpm") "Replace one occurrence")
       (replace-one))
      ((balloon (icon "tm_replace_all.xpm") "Replace all further occurrences")
       (replace-all))
      >>>
      (=> (balloon (icon "tm_preferences.xpm")
                   "Search and replace preferences")
          (link replace-preferences-menu))
      ((check (balloon (icon "tm_filter.xpm") "Only show paragraphs with hits")
              "v" (search-filter-enabled?))
       (search-toggle-filter))
      ((balloon (icon "tm_compress_tool.xpm") "Compress into toolbar")
       (set-boolean-preference "toolbar replace" #t)
       (quit)
       (toolbar-replace-start))
      ((balloon (icon "tm_close_tool.xpm") "Close replace tool")
       (quit)))))

(tm-tool* (replace-tool win u style init saux raux)
  (:name "Search and replace")
  (:quit ((search-cancel u)))
  ===
  (horizontal
    //
    (vertical
      (resize "360px" "60px"
        (texmacs-input `(with ,@init ,(search-document))
                       `(style (tuple ,@style)) saux))
      === ===
      (resize "360px" "60px"
        (texmacs-input `(with ,@init ,(replace-document))
                       `(style (tuple ,@style)) raux))
      === ===
      (hlist
        ((balloon (icon "tm_search_first.xpm") "First occurrence")
         (search-extreme-match #f))
        ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
         (search-next-match #f))
        ((balloon (icon "tm_search_next.xpm") "Next occurrence")
         (search-next-match #t))
        ((balloon (icon "tm_search_last.xpm") "Last occurrence")
         (search-extreme-match #t))
        // // //
        ((balloon (icon "tm_replace_one.xpm") "Replace one occurrence")
         (replace-one))
        ((balloon (icon "tm_replace_all.xpm") "Replace all further occurrences")
         (replace-all))
        >>>
        (=> (balloon (icon "tm_preferences.xpm")
                     "Search and replace preferences")
            (link replace-preferences-menu))
        ((check (balloon (icon "tm_filter.xpm")
                         "Only show paragraphs with hits")
                "v" (search-filter-enabled?))
         (search-toggle-filter)))
      ===)
    //))

(tm-define (open-replace)
  (:interactive #t)
  (when (not (inside-search-buffer?))
    (let* ((u (current-buffer))
           (st (embedded-style-list "macro-editor"))
           (init (get-main-attrs get-env))
           (saux (search-buffer))
           (raux (replace-buffer))
           (tool (list 'replace-tool u st init saux raux)))
      (buffer-set-master saux u)
      (buffer-set-master raux u)
      (set! search-window (current-window))
      (set-search-reference (cursor-path))
      (set-search-filter)
      (set! search-filter-out? #f)
      (if (side-tools?)
          (tool-focus :bottom-right tool saux)
          (dialogue-window (replace-widget u st init saux raux)
                           (search-cancel u)
                           "Search and replace" saux raux)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-toolbar-search what)
  (let* ((u (current-buffer))
         (aux (search-buffer))
         (what-case (if (get-boolean-preference "case-insensitive-match")
                        (string-downcase what) what)))
    (set-search-reference (cursor-path))
    (set-search-filter)
    (buffer-set-body aux `(document ,what-case))
    (buffer-set-master aux u)
    (set! search-window (current-window))
    (perform-search)))

(tm-define (search-toolbar-keypress what r?)
  (with key (and (pair? what) (cadr what))
    (if (pair? what) (set! what (car what)))
    (set! current-search what)
    (cond ((== key "home") (search-extreme-match #f))
          ((== key "end") (search-extreme-match #t))
          ((== key "up") (search-next-match #f))
          ((== key "down") (search-next-match #t))
          ((== key "pageup") (search-next-match #f))
          ((== key "pagedown") (search-next-match #t))
          ((== key "S-F3") (search-next-match #f))
          ((== key "F3") (search-next-match #t))
          ((in? key '("C-F" "A-F" "M-F" "C-G" "A-G" "M-G" "C-r" "A-r" "M-r"))
           (search-next-match #f))
          ((in? key '("C-f" "A-f" "M-f" "C-g" "A-g" "M-g" "C-s" "A-s" "M-s"))
           (search-next-match #t))
          ((and r? (in? key (list "tab" "S-tab" "return")))
           (search-toolbar-search what)
           (keyboard-focus-on "replace-by"))
          ((== key "return") (search-rotate-match))
          ((== key "escape") (toolbar-search-end))
          ((string? what) (search-toolbar-search what))
          (else (cancel-alt-selection "alternate")))))

(tm-widget (search-toolbar)
  ===
  (hlist
    //
    (text "Search: ") //
    ;;(resize "0.5w" "24px"
    ;;  (texmacs-input `(document "")
    ;;                 `(style (tuple "generic"))
    ;;                 (search-buffer)))
    (input (search-toolbar-keypress answer #f) "search"
           (list (or current-search pending-key-strokes)) "25em")
    // //
    ((balloon (icon "tm_search_first.xpm") "First occurrence")
     (search-extreme-match #f))
    ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
     (search-next-match #f))
    ((balloon (icon "tm_search_next.xpm") "Next occurrence")
     (search-next-match #t))
    ((balloon (icon "tm_search_last.xpm") "Last occurrence")
     (search-extreme-match #t))
    >>>
    ((check (balloon (icon "tm_filter.xpm") "Only show paragraphs with hits")
            "v" (search-filter-enabled?))
     (search-toggle-filter))
    ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
     (set-boolean-preference "toolbar search" #f)
     (toolbar-search-end)
     (open-search))
    ((balloon (icon "tm_close_tool.xpm") "Close search tool")
     (toolbar-search-end))
    //)
  ===)

(tm-define (toolbar-search-start)
  (:interactive #t)
  (search-show-all)
  (set! search-filter-out? #f)
  (set! toolbar-search-active? #t)
  (set! toolbar-replace-active? #f)
  (update-bottom-tools)
  (search-toolbar-search "")
  (wait-for-toolbar)
  (notify-bar-change)
  (delayed
    (:idle 250)
    (keyboard-focus-on "search")
    (search-toolbar-search (or current-search pending-key-strokes))
    (notify-bar-change)
    (stop-waiting-for-toolbar)))

(tm-define (toolbar-search-end)
  (cancel-alt-selection "alternate")
  (search-show-all)
  (set! search-filter-out? #f)
  (set! toolbar-search-active? #f)
  (set! toolbar-replace-active? #f)
  (update-bottom-tools)
  (set! search-serial (+ search-serial 1))
  (set! pending-key-strokes "")
  (set! current-search #f)
  (set! current-replace #f)
  (when toolbar-db-active?
    (db-show-toolbar))
  (when (and (not (cursor-accessible?)) (not (in-source?)))
    (cursor-show-hidden)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replace-toolbar-replace by)
  (let* ((u (current-buffer))
         (aux (replace-buffer)))
    (buffer-set-body aux `(document ,by))
    (buffer-set-master aux u)
    (set! search-window (current-window))
    (replace-one)))

(tm-define (replace-toolbar-keypress by)
  (with key (and (pair? by) (cadr by))
    (if (pair? by) (set! by (car by)))
    (set! current-replace by)
    (cond ((not (string? by)) (cancel-alt-selection "alternate"))
          ((== key "home") (search-extreme-match #f))
          ((== key "end") (search-extreme-match #t))
          ((== key "up") (search-next-match #f))
          ((== key "down") (search-next-match #t))
          ((== key "pageup") (search-next-match #f))
          ((== key "pagedown") (search-next-match #t))
          ((== key "tab") (keyboard-focus-on "replace-what"))
          ((== key "S-tab") (keyboard-focus-on "replace-what"))
          ((== key "return") (replace-toolbar-replace by))
          ((== key "S-return") (undo 0) (perform-search*))
          ((== key "C-return") (replace-all))
          ((== key "escape") (toolbar-search-end))
          (else (perform-search*)))))

(tm-widget (replace-toolbar)
  ===
  (hlist
    //
    (text "Replace: ") //
    (input (search-toolbar-keypress answer #t) "replace-what"
           (list (or current-search pending-key-strokes)) "15em")
    //
    (text " by: ")
    (input (replace-toolbar-keypress answer) "replace-by"
           (list (or current-replace "")) "15em")
    // //
    ;;(if (nnull? (get-alt-selection "alternate"))
    ((balloon (icon "tm_search_first.xpm") "First occurrence")
     (search-extreme-match #f))
    ((balloon (icon "tm_search_previous.xpm") "Previous occurrence")
     (search-next-match #f))
    ((balloon (icon "tm_search_next.xpm") "Next occurrence")
     (search-next-match #t))
    ((balloon (icon "tm_search_last.xpm") "Last occurrence")
     (search-extreme-match #t))
    //
    ((balloon (icon "tm_replace_one.xpm") "Replace one occurrence")
     (replace-one))
    ((balloon (icon "tm_replace_all.xpm") "Replace all further occurrences")
     (replace-all))
    >>>
    ((check (balloon (icon "tm_filter.xpm") "Only show paragraphs with hits")
            "v" (search-filter-enabled?))
     (search-toggle-filter))
    ((balloon (icon "tm_expand_tool.xpm") "Open tool in separate window")
     (set-boolean-preference "toolbar replace" #f)
     (toolbar-search-end)
     (open-replace))
    ((balloon (icon "tm_close_tool.xpm") "Close replace tool")
     (toolbar-search-end))
    //)
  ===)

(tm-define (toolbar-replace-start)
  (:interactive #t)
  (search-show-all)
  (set! search-filter-out? #f)
  (set! toolbar-search-active? #f)
  (set! toolbar-replace-active? #t)
  (update-bottom-tools)
  (search-toolbar-search "")
  (wait-for-toolbar)
  (notify-bar-change)
  (delayed
    (:idle 250)
    (keyboard-focus-on "replace-what")
    (search-toolbar-search pending-key-strokes)
    (notify-bar-change)
    (stop-waiting-for-toolbar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hiding paragraphs that do not match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-only-sub u what)
  (with t (if (tree-is? u 'hide-para) (tree-ref u 0) u)
    (let* ((sels* (tree-search-tree t what (tree->path t) 100))
           (sels (filter-search-results sels*)))
      (if (null? sels)
          (when (not (tree-is? u 'hide-para))
            (tree-insert-node u 0 '(hide-para)))
          (begin
            (when (tree-is? u 'hide-para)
              (tree-remove-node! u 0))
            (show-only u what))))))

(define (show-only-document t what)
  (if (tree-is? t 'document) (show-only t what)))

(define (show-only t what)
  (cond ((== what (string->tree ""))
         (show-all t))
        ((tree-is? t 'document)
         (for-each (cut show-only-sub <> what) (tree-children t)))
        ((tree-is? t 'hide-para)
         (show-only-sub t what))
        ((tree-compound? t)
         (for-each (cut show-only-document <> what)
                   (tree-accessible-children t)))
        (else (noop))))

(define (show-all-document t)
  (if (tree-is? t 'document) (show-all t)))

(define (show-all t)
  (cond ((tree-is? t 'document)
         (for-each show-all (tree-children t)))
        ((tree-is? t 'hide-para)
         (tree-remove-node t 0))
        ((tree-compound? t)
         (for-each show-all-document (tree-accessible-children t)))
        (else (noop))))

(define (search-show-only)
  (when search-filter-out?
    (with-buffer (master-buffer)
      (let* ((doc (buffer-get-body (master-buffer)))
             (what (buffer-get-body (search-buffer))))
        (when (tree-func? what 'document 1)
          (set! what (tree-ref what 0)))
        (when (and (tree-func? doc 'document)
                   (tree-func? (tree-ref doc :last) 'blank-line))
          (tree-remove! doc (- (tree-arity doc) 1) 1))
        (show-only doc what)
        (when (and (tree-func? doc 'document)
                   (list-and (map (lambda (x) (tree-is? x 'hide-para))
                                  (tree-children doc)))
                   (not (tree-func? (tree-ref doc :last) 'blank-line)))
          (tree-insert! doc (tree-arity doc) '((blank-line))))))))

(define (search-show-all)
  (when search-filter-out?
    (with-buffer (master-buffer)
      (with doc (buffer-get-body (master-buffer))
        (show-all doc)
        (when (and (tree-func? doc 'document)
                   (tree-func? (tree-ref doc :last) 'blank-line))
          (tree-remove! doc (- (tree-arity doc) 1) 1))))))

(define (search-filter-enabled?)
  search-filter-out?)

(define (search-toggle-filter)
  (search-show-all)
  (set! search-filter-out? (not search-filter-out?))
  (perform-search))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("toolbar search" "on" noop)
  ("toolbar replace" "on" noop))

(tm-define (interactive-search)
  (:interactive #t)
  (set-boolean-preference "search-and-replace" #f)
  (if (and (get-boolean-preference "toolbar search")
           (not (buffer-aux? (current-buffer))))
      (toolbar-search-start)
      (open-search)))

(tm-define (interactive-replace)
  (:interactive #t)
  (set-boolean-preference "search-and-replace" #t)
  (if (and (get-boolean-preference "toolbar replace")
           (not (buffer-aux? (current-buffer))))
      (toolbar-replace-start)
      (open-replace)))
