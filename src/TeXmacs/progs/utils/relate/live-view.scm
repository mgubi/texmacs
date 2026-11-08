
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : live-view.scm
;; DESCRIPTION : Views on live shared documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils relate live-view)
  (:use (utils plugins plugin-eval)
        (utils relate live-document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-context? t)
  (and (or (tree-func? t 'live-io 3)
           (tree-func? t 'live-io* 3))
       (tree-atomic? (tree-ref t 0))
       (tree-atomic? (tree-ref t 1))))

(tm-define (live-view-context? t)
  (and (tree->path t)
       (live-context? (tree-up t))
       (== (tree-index t) 2)))  

(tm-define (live-view-id t)
  (or (and (live-context? t) (tree->string (tree-ref t 0)))
      (and (live-view-context? t) (live-view-id (tree-up t)))))

(tm-define (live-id t)
  (or (and (live-context? t) (tree->string (tree-ref t 1)))
      (and (live-view-context? t) (live-id (tree-up t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-views (make-ahash-table))

(define (live-view-table lid)
  (when (not (ahash-ref live-views lid))
    (ahash-set! live-views lid (make-ahash-table)))
  (ahash-ref live-views lid))

;; A view is typeset by each editor of its buffer: a buffer with two views
;; (one of them hidden, as load-buffer makes it) has two observers on the
;; same tree, which id->trees then gives twice, and which are both told of
;; each change (#796). The views of an identifier are its trees, each once.

(tm-define (live-view-trees vid)
  (:synopsis "The trees of the view @vid of a live document, each once")
  (with r (list)
    (for (t (id->trees vid))
      (when (not (list-find r (lambda (u) (tree-eq? t u))))
        (set! r (cons t r))))
    (reverse r)))

(tm-define (live-exists? lid)
  (with t (ahash-ref live-views lid)
    (and t (!= (ahash-size t) 0))))

(tm-define (live-view-ids lid)
  (:synopsis "The identifiers of the views of the live document @lid")
  (map car (ahash-table->list (live-view-table lid))))

(tm-define (live-view-set-state lid vid state)
  (with t (live-view-table lid)
    (ahash-set! t vid state)))

(tm-define (live-view-get-state lid vid)
  (with t (live-view-table lid)
    (ahash-ref t vid)))

(tm-define (live-view-remove lid vid)
  (with t (live-view-table lid)
    (ahash-remove! t vid)
    (when (== (ahash-size t) 0)
      (ahash-remove! live-views lid))))

(tm-define (live-view-clean lid)
  (with t (live-view-table lid)
    (for (vid (map car (ahash-table->list t)))
      (when (null? (live-view-trees vid))
        (live-view-remove lid vid)))))

(tm-define (live-states-in-use lid)
  (live-view-clean lid)
  (let* ((t (live-view-table lid))
         (l1 (former lid))
         (l2 (if (live-exists? lid) (map cdr (ahash-table->list t)) (list))))
    (append l1 l2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating views to match the current live document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-author (new-author))
(define live-updating? #f)

(tm-define (live-view-update lid vid)
  (with-author live-author
    (with-global live-updating? #t
      (let* ((old-state (live-view-get-state lid vid))
             (new-state (live-current-state lid))
             (p (live-get-inverse-patch lid old-state))
             (cur (live-current-document lid))
             (vts (live-view-trees vid)))
        (when (!= new-state old-state)
          (for (vt vts)
            (if (and p (patch-applicable? p vt))
		(begin
                  (when (debug-get "live")
                    (display* "]] Apply " (patch->scheme p) "\n"))
		  (patch-apply! vt p))
                (begin
                  (when (debug-get "live")
                    (display* "]] Reset view " (tm->stree cur) "\n"))
		  (tree-set! vt cur))))
          (live-view-set-state lid vid new-state))
        (when (null? vts)
          (live-view-remove lid vid))))))

(tm-define (live-update-views lid)
  (with vids (map car (ahash-table->list (live-view-table lid)))
    (for (vid vids)
      (live-view-update lid vid)))
  (live-forget-obsolete lid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restoring views to match the current live document in case of panic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-view-restore lid vid)
  ;;(display* "live-view-restore " lid ", " vid "\n")
  (with-author live-author
    (with-global live-updating? #t
      (let* ((cur (live-current-document lid))
             (vts (live-view-trees vid)))
        (for (vt vts)
          (when (debug-get "live")
            (display* "]] Restore view " (tm->stree cur) "\n"))
          (tree-set! vt cur))
        (live-view-set-state lid vid (live-current-state lid))
        (when (null? vts)
          (live-view-remove lid vid))))))

(tm-define (live-restore-views lid)
  (with vids (map car (ahash-table->list (live-view-table lid)))
    (for (vid vids)
      (live-view-restore lid vid)))
  (live-forget-obsolete lid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retracting recent changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (live-view-retract lid vid new-state p t)
  (with-author live-author
    (with-global live-updating? #t
      (let* ((old-state (live-view-get-state lid vid))
             (vts (live-view-trees vid)))
        (when (!= new-state old-state)
          (for (vt vts)
            (if (and p (patch-applicable? p vt))
		(begin
                  (when (debug-get "live")
                    (display* "]] Apply " (patch->scheme p) "\n"))
		  (patch-apply! vt p))
		(begin
                  (when (debug-get "live")
                    (display* "]] Reset view " (tm->stree t) "\n"))
		  (tree-set! vt t))))
          (live-view-set-state lid vid new-state))
        (when (null? vts)
          (live-view-remove lid vid))))))

(tm-define (live-retract lid state)
  (let* ((vids (map car (ahash-table->list (live-view-table lid))))
         (p (live-get-patch lid state))
         (t (patch-apply (live-current-document lid) p)))
    (for (vid vids)
      (live-view-retract lid vid state p t)))
  (former lid state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initializing views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-retrieve t)
  ;; to be redefined for particular tags
  (let* ((lid (live-id t))
         (vid (live-view-id t))
         (vt (tree-ref t :last))
         (initial-state (live-create lid (tm->stree vt))))
    ;;(display* "live-retrieve " lid ", " vid "\n")
    (live-view-set-state lid vid initial-state)))

(tm-define (live-view-separate lid vid)
  (with-author live-author
    (with-global live-updating? #t
      (with vts (live-view-trees vid)
        (when (>= (length vts) 2)
          (for (vt (cdr vts))
            (tree-set (tree-ref vt :up 0) (create-unique-id))))))))

(tm-define (live-initialize t)
  (:secure #t)
  (when (live-view-context? t)
    (let* ((lid (live-id t))
           (vid (live-view-id t)))
    (when (not (live-view-get-state lid vid))
      ;;(display* "live-initialize " lid ", " vid "\n")
      (if (live-exists? lid)
          (delayed (:idle 1) (live-view-restore lid vid))
          (live-retrieve (tree-up t))))
    (when (>= (length (live-view-trees vid)) 2)
      (delayed (:idle 1) (live-view-separate lid vid))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monitor changes in views and update live document accordingly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-pending (make-ahash-table))

(define live-announced (make-ahash-table)) ;; vid -> #t, until done

(tm-define (live-treat-pending lid)
  (and-with pend (ahash-ref live-pending lid)
    ;; the changes are taken first: an error below must not leave them to
    ;; be applied once more with the next change
    (ahash-remove! live-pending lid)
    ;; (and a change whose end was not seen does not hide the next ones)
    (for (vid (map car (ahash-table->list pend)))
      (ahash-remove! live-announced vid))
    (let* ((vids (map car (ahash-table->list pend)))
	   (vid (car vids))
	   (l (ahash-ref pend vid))
	   (p (patch-compound (reverse l)))
	   (new-state (live-apply-patch lid p))
	   (ts (live-view-trees vid)))
      (if (and new-state (== (length ts) 1))
	  (live-view-set-state lid vid new-state)
	  (live-view-restore lid vid))
      (for (vid* (cdr vids))
	(live-view-restore lid vid*)))
    (live-update-views lid)))

(define (live-notify-patch lid vid p)
  (when (not (ahash-ref live-pending lid))
    (delayed (:idle 1) (live-treat-pending lid))
    (ahash-set! live-pending lid (make-ahash-table)))
  (with t (ahash-ref live-pending lid)
    (with old (or (ahash-ref t vid) (list))
      (ahash-set! t vid (cons p old)))))

(tm-define (live-notify event t mod)
  (:secure #t)
  ;; A change of a view is announced to each observer of its tree, all
  ;; before the change is made, and then done to each: when the view has
  ;; several observers (one for each editor of its buffer, #796), only the
  ;; first announcement is kept, until the change is done.
  (when (and (== event 'done) (live-view-context? t))
    (ahash-remove! live-announced (live-view-id t)))
  (when (and (not live-updating?)
             (== event 'announce)
             (!= (modification-type mod) 'set-cursor)
             (live-view-context? t))
    (let* ((lid (live-id t))
           (vid (live-view-id t)))
      (when (and (live-view-get-state lid vid)
                 (not (ahash-ref live-announced vid)))
        ;;(display* event ", " (tree->path t) ", "
        ;;(modification->scheme mod) "\n")
        (ahash-set! live-announced vid #t)
        (set! mod (modification-copy mod))
        (with p (patch-pair mod (modification-invert mod t))
          (live-notify-patch lid vid p))))))
