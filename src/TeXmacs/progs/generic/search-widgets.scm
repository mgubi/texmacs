
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

(define (search-filter-accepts-all?)
  ;; does the filter of the search accept every hit (see
  ;; accept-search-result?)? Then Replace need not find the start of the
  ;; hit which it judges (see next-hit, #618)
  (or (== (get-init "mode") "src")
      (with filter (get-search-filter)
        (or (not filter) (null? (tm-children filter))))))

(define (filter-search-results sels)
  ;; (iterative: a document may have tens of thousands of hits, #596)
  (let loop ((l sels) (r (list)))
    (if (or (null? l) (null? (cdr l))) (reverse r)
        (loop (cddr l)
              (if (accept-search-result? (car l))
                  (cons* (cadr l) (car l) r)
                  r)))))

(define (first-accepted-hit sels cur)
  ;; the first hit of sels (unfiltered) which the filter of the search
  ;; accepts, after cur, as search-next on the filtered hits gives it, but
  ;; judging the hits only until it (#596)
  (let* ((accepted (lambda (l)
                     ;; the hits from the first accepted one in l on
                     (let loop ((l l))
                       (cond ((or (null? l) (null? (cdr l))) (list))
                             ((accept-search-result? (car l)) l)
                             (else (loop (cddr l)))))))
         (after (let loop ((l sels))
                  (if (and (pair? l) (pair? (cdr l))
                           (not (path-less-eq? cur (cadr l))))
                      (loop (cddr l))
                      l))))
    (let loop ((l (accepted after)))
      (and (pair? l)
           (with next (accepted (cddr l))
             ;; (as next-search-hit: an empty hit at cur after a hit which
             ;; ends there)
             (if (and (pair? next) (== (cadr l) (car next)) (== (cadr l) cur))
                 (loop next)
                 (list (car l) (cadr l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define search-serial 0)
(define search-filter-out? #f)

(define (with-search-access-mode thunk)
  ;; thunk with the access mode of the search: in a source document, also
  ;; the hidden content
  (let* ((source-mode 2)
         (old-mode (get-access-mode))
         (new-mode (if (== (get-init "mode") "src") source-mode old-mode)))
    (dynamic-wind
      (lambda () (set-access-mode new-mode))
      thunk
      (lambda () (set-access-mode old-mode)))))

(define (tree-perform-search t what p limit)
  (with-search-access-mode
    (lambda ()
      (let* ((cp (cDr (cursor-path)))
             (pos (if (list-starts? cp p) (list-tail cp (length p)) (list))))
        (tree-search-tree-at t what p pos limit)))))

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

;; The values of the wildcards are the runs which they take in the match, as
;; the search aligns the pattern with its text (lazily: a part of a string,
;; several children, or nothing; in the region of a pattern with one, as
;; its content); they were matched child by child, and in a string by a
;; case-sensitive search of its literal parts (#520)

(define (has-region? t)
  (or (tm-func? t 'select-region)
      (and (tm-compound? t) (list-or (map has-region? (tm-children t))))))

(define (regions t)
  (cond ((tm-func? t 'select-region 1) (list t))
        ((tm-compound? t) (append-map regions (tm-children t)))
        (else (list))))

(define (region-content what)
  ;; the pattern which the hit of what matches: the content of its region,
  ;; if it has one which is not empty, else #f
  (with l (regions what)
    (and (== (length l) 1) (not (tm-equal? (tm-ref (car l) 0) ""))
         (tm-ref (car l) 0))))

(define (has-wildcard? t)
  (or (tm-func? t 'wildcard 1)
      (and (tm-compound? t) (list-or (map has-wildcard? (tm-children t))))))

(define (tuple->path x)
  (map (lambda (c) (string->number (tree->string c))) (tree-children x)))

(define (replace-match t what p1 p2)
  ;; the match which Replace replaces in the hit from p1 to p2 in t (all of
  ;; t if both are empty): its start, its end, the match, and the value of
  ;; each child of a formula pattern, or #f (see tree-search-replace-match)
  (with l (with-search-access-mode
            (lambda () (tree-search-replace-match t what p1 p2)))
    (and (nnull? l)
         (list (tuple->path (car l)) (tuple->path (cadr l)) (caddr l)
               (cdddr l)))))

(define (get-wildcards-var what match)
  (cond ((tm-equal? what match) (list))
        ((tm-func? what 'wildcard 1)
         (list `(:wildcard ,(tm->stree (tm-ref what 0)) ,(tm->stree match))))
        ((tm-equal? what "")
         (list `(:empty ,(tm->stree match))))
        ((has-region? what)
         (with c (region-content what)
           (if c (get-wildcards c match) (list))))
        ((and (tm-func? what 'concat) (not (tm-func? match 'document))
              (has-wildcard? what))
         (with r (replace-match (tm->tree match) what '() '())
           (if r (append-map get-wildcards (tm-children what) (cadddr r))
               (list))))
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

(define (replace-wildcards-concat l wc)
  ;; the children l of a formula of the replacement, each marked #t if it
  ;; comes from the value of a wildcard, which is spliced into the formula
  (if (null? l) (cons l wc)
      (let* ((c (car l))
             (h (if (tm-func? c 'wildcard 1)
                    (let* ((v (find-wildcard wc (tm->stree (tm-ref c 0))))
                           (l (cond ((tm-func? v 'concat) (tm-children v))
                                    ((tm-equal? v "") (list))
                                    (else (list v)))))
                      (cons (map (lambda (x) (cons #t x)) l) wc))
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
              (list-or (map (cut tm-func? <> 'wildcard 1) (tm-children by))))
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

(define (wildcard-names t)
  ;; the names of the wildcards in t
  (cond ((tm-func? t 'wildcard 1) (list (tm->stree (tm-ref t 0))))
        ((tm-compound? t) (list-remove-duplicates
                            (append-map wildcard-names (tm-children t))))
        (else (list))))

(define (adjust-by by what match)
  (adjust-by* by what match (list)))

(define (adjust-by* by what match runs)
  ;; the replacement of match, a match of what, by by: its wildcards take
  ;; the values of those of what (runs, if not empty: the value of each
  ;; child of a formula pattern, aligned with the match). #f if the values
  ;; of the wildcards of what which by takes are not known: inserting the
  ;; (wildcard ...) tags, or nothing in their place, would lose the text
  ;; which they took (#520)
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
          (else
            (let* ((wc (if (and (tm-func? what 'concat) (nnull? runs))
                           (append-map get-wildcards (tm-children what) runs)
                           (get-wildcards what match)))
                   (need (list-intersection (wildcard-names by)
                                            (wildcard-names what))))
              (and (or (null? need)
                       (with known (map cadr (list-filter
                                               wc (cut tm-func? <> :wildcard 2)))
                         (list-and (map (cut in? <> known) need))))
                   (car (replace-wildcards by wc))))))))

;; Replace one replaces the current hit (the match in it, see replace-hit),
;; and goes on from the end of the replacement: the next match is the first
;; hit which the search finds in the text after it, in its paragraph, else
;; the next hit of the document. Replace All is this, again and again, in
;; one step of undo (#520)

(define replace-last #f)    ;; the state where Replace goes on, and whether
                            ;; its last match was empty
(define replace-trace #f)   ;; for the tests, if a list: the document and the
                            ;; range of each replacement (all of the
                            ;; document at each step: only for small ones)
(define replace-skip-message #f)

(define (replace-state cur)
  ;; the place where Replace goes on, with the buffer and the pattern
  (list (url->string (current-buffer)) (tm->stree (search-tree)) cur))

(define (replace-from cur empty?)
  ;; Replace goes on from cur, after its last replacement (or a hit which it
  ;; left; empty? if its match was empty), as long as the buffer, the
  ;; pattern and the reference are the same
  (set! replace-last (list (replace-state cur) empty?))
  (set-search-reference cur))

(define (replace-continues? cur)
  (and replace-last (== (car replace-last) (replace-state cur))))

(define (next-hit cur what)
  ;; the hit from which Replace goes on, from cur: the first one which the
  ;; search finds in the text of the document after cur, now (nothing found
  ;; before a change is kept across it), which the filter of the search
  ;; accepts: the search goes on after the hits which it rejects. The filter
  ;; judges the start of the whole hit, as for the hits which the search
  ;; shows, not that of the match in it (#596); without a filter, nothing
  ;; judges it, and it is not looked for (#618)
  (let* ((t (buffer-tree))
         (p (tree->path t))
         (filtered? (not (search-filter-accepts-all?))))
    (and (list-starts? cur p)
         ;; (after an empty match, the next one starts at least one token
         ;; later: Replace, which goes on after its replacement, ends)
         (let loop ((from cur)
                    (empty? (and (replace-continues? cur)
                                 (cadr replace-last))))
           ;; (one hit: its match, and the start of the whole hit)
           (with l (with-search-access-mode
                     (lambda ()
                       (tree-search-from t what p from 1 empty? filtered?)))
             (cond ((< (length l) 3) #f)
                   ((accept-search-result? (caddr l)) (list (car l) (cadr l)))
                   ;; (no empty hit at from: tree-search-from leaves it out)
                   ((path-less? from (cadr l)) (loop (cadr l) #f))
                   (else #f)))))))

(define (replace-current-range sel what)
  ;; the match which Replace replaces in the hit sel of the buffer (see
  ;; replace-match), its range in the buffer, or #f
  (let* ((t (buffer-tree))
         (p (tree->path t))
         (n (length p)))
    (and (list-starts? (car sel) p) (list-starts? (cadr sel) p)
         (and-with r (replace-match t what (list-tail (car sel) n)
                                    (list-tail (cadr sel) n))
           (cons* (append p (car r)) (append p (cadr r)) (cddr r))))))

(define (replace-hit sel by what)
  ;; replace the hit sel of what by by: the match in it which Replace
  ;; replaces (see replace-match), exactly (the selection of the editor
  ;; moves the ends of a range next to a script over its base, which was
  ;; lost), or leave it with a message: the range replaced, or #f if left
  (let* ((rm (replace-current-range sel what))
         (by* (and rm (adjust-by* by what (caddr rm) (cadddr rm)))))
    (if by*
        (let* ((a (car rm))
               (b (cadr rm))
               (by* (tm->tree by*)))
          (when replace-trace
            (let* ((t (buffer-tree))
                   (n (length (tree->path t))))
              (set! replace-trace (cons (list (tm->stree t) (list-tail a n)
                                              (list-tail b n))
                                        replace-trace))))
          (go-to a)
          ;; (an empty match: nothing to remove)
          (when (path-less? a b) (clipboard-cut-between a b))
          (insert-go-to (tree-copy by*) (path-end by* '()))
          (list a b))
        (begin
          (go-to (cadr sel))
          ;; (the message, also for the tests)
          (set! replace-skip-message
            (cond ((not rm) "Match left: not one, or it crosses a tag")
                  ((not (tm-func? what 'concat))
                   "Match left: it would lose the rest of a tag")
                  (else "Match left: its wildcards have no values")))
          (set-message replace-skip-message "Replace")
          #f))))

(define (current-hit cur what)
  ;; the hit which Replace replaces from cur: after a replacement, the next
  ;; one in the text after it (see next-hit), else the next hit of the search
  (if (replace-continues? cur)
      (next-hit cur what)
      (search-next (get-alt-selection "alternate") cur #f)))

(define (replace-step sel by what)
  ;; replace the hit sel (or leave it), and go on from the end of the
  ;; replacement; #t if replaced
  (let* ((r (replace-hit sel by what))
         (empty? (if r (== (car r) (cadr r)) (== (car sel) (cadr sel)))))
    (replace-from (cursor-path) empty?)
    (and r #t)))

(define (with-editing thunk)
  ;; thunk as one step of undo; if it fails, the document as it was before
  ;; it, and the error raised again (in a read-only document, end-editing
  ;; cancels what it changed: there are no read-only parts). cancel-editing
  ;; is global_cancel: it cancels all that is pending since the last
  ;; confirm, as the handlers of the keyboard and the mouse of the kernel do
  ;; after an error (edit_keyboard.cpp, edit_mouse.cpp)
  (start-editing)
  (with r (catch #t thunk
            (lambda args
              (cancel-editing)
              (end-editing)
              (apply throw args)))
    (end-editing)
    r))

(tm-define (replace-one)
  (and-with by (by-tree)
    (with-buffer (master-buffer)
      (with what (search-tree)
        (and-with sel (current-hit (get-search-reference #t) what)
          (with-editing
            (lambda ()
              (replace-step sel by what)
              (go-to* (cursor-path)))))))
    (perform-search*)
    ;; the match which the next Replace replaces, not the whole hit (none:
    ;; nothing left to replace)
    (with-buffer (master-buffer)
      (let* ((cur (get-search-reference #t))
             (what (search-tree)))
        (when (replace-continues? cur)
          (with r (and-with h (current-hit cur what)
                    (replace-current-range h what))
            (if r (selection-set-range-set (list (car r) (cadr r)))
                (selection-cancel))))))))

(define (replace-all-hits by)
  ;; Replace one again and again, from the reference: each step goes on in
  ;; the text after its match, which a match which is not empty shortens,
  ;; and after an empty one the next starts at least one character later
  ;; (see next-hit), so it ends. The first hit: after a replacement, the
  ;; next one in the text after it, as Replace one goes on (the hit which
  ;; ends there would replace in the replacement, #596), else the first from
  ;; a search of the document now (the hits of the widget can be partial)
  (let* ((what (search-tree))
         (t (buffer-tree))
         (cur (get-search-reference #t)))
    (let loop ((sel (if (replace-continues? cur)
                        (next-hit cur what)
                        (first-accepted-hit
                          (tree-perform-search t what (tree->path t) 1000000)
                          cur))))
      (when sel
        (replace-step sel by what)
        (loop (next-hit (cursor-path) what))))
    ;; (the place where it ended shown, if hidden)
    (go-to* (cursor-path))))

(tm-define (replace-all)
  (and-with by (by-tree)
    (with-buffer (master-buffer)
      (with-editing (lambda () (replace-all-hits by))))
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

(define (master-in-code?)
  ;; is the buffer which is searched program code (a source file)?
  (with mas (and (buffer-exists? (search-buffer))
                 (buffer-get-master (search-buffer)))
    (and mas (buffer-exists? mas)
         (with-buffer mas (== (get-init "mode") "prog")))))

(tm-define (kbd-variant t forwards?)
  ;; (tab searches and replaces a tab, the character <#9> of the lines of a
  ;; source file, there)
  (:require (and forwards? (inside-search-or-replace-buffer?)
                 (master-in-code?)))
  (insert "<#9>"))

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
