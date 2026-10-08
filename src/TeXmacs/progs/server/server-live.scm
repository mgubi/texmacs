
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-live.scm
;; DESCRIPTION : Live shared documents (server side)
;; COPYRIGHT   : (C) 2015-2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-live)
  (:use (utils relate live-connection)
        (server server-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storing live documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (live-name lid)
  (let* ((s1 (url->unix (url-unroot lid)))
         (s2 (if (== (tmfs-car s1) "live") (tmfs-cdr s1) s1))
         (s3 (tmfs-cdr s2))
         (s4 (if (== (tmfs-car s3) "live") (tmfs-cdr s3) s3)))
    s4))

(define (live-new uid lid)
  (with-time-stamp #t
    (with rid (db-create-entry `(("type" "live")
                                 ("name" ,(live-name lid))
                                 ("owner" ,uid)
                                 ("readable" "all")
                                 ("writable" "all")))
      (repository-add rid "tm")
      rid)))

(define (live-find lid)
  (with l (db-search `(("type" "live")
                       ("name" ,(live-name lid))))
    (and (nnull? l) (car l))))

(tm-define (search-remote-identifier u)
  (:require (string-starts? (url->string u) "tmfs://live/"))
  (live-find (url->string u)))

(define (live-load lid)
  (and-let* ((rid (live-find lid))
             (fname (repository-get rid))
             (doc (if (url-exists? fname) (string-load fname) "")))
    (if (== doc "")
        `(document "")
        (convert doc "texmacs-snippet" "texmacs-stree"))))

(define (live-save lid t)
  (and-let* ((rid (live-find lid))
             (fname (repository-get rid))
             (doc (convert t "texmacs-stree" "texmacs-snippet")))
    (string-save doc fname)))

(tm-service (remote-list-live)
  ;; Return list of live documents owned by the user as (name date) pairs
  ;;(display* "remote-list-live\n")
  (with (client msg-id) envelope
    (let* ((uid (server-get-user envelope))
           (l (db-search `(("type" "live")
                           ("owner" ,uid))))
           (get-entry (lambda (id)
                        (let* ((name (db-get-field-first id "name" #f))
                               (date (db-get-field-first id "date" "")))
                          (list name date))))
           (r (map get-entry l)))
      (server-return envelope r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-waiting (make-ahash-table))

(define (live-applicable? lid client p old-state)
  (when (and (list? p) (== (live-current-state lid) old-state))
    (set! p (modlist->patch p (live-current-document lid))))
  (when (debug-get "live")
    (cond ((!= (live-get-remote-state lid client) old-state)
           (display* "  ** Bad remote state " (live-get-remote-state lid client)
                     " instead of " old-state "\n"))
          ((!= (live-current-state lid) old-state)
           (display* "  ** Bad state " (live-current-state lid)
                     " instead of " old-state "\n"))
          ((not (with doc (live-current-document lid)
                  (patch-applicable? p doc)))
           (display* "  ** Non applicable patch " (patch->scheme p) "\n"))))
  (and (== (live-get-remote-state lid client) old-state)
       (== (live-current-state lid) old-state)
       (with doc (live-current-document lid)
         (patch-applicable? p doc))))

(define (live-apply lid client p old-state new-state)
  (when (and (list? p) (== (live-current-state lid) old-state))
    (set! p (modlist->patch p (live-current-document lid))))
  (and (== (live-current-state lid) old-state)
       (live-apply-patch lid p new-state)
       (begin
         (when (debug-get "live")
           (display* "Confirm " client ": " new-state "\n"))
         (live-set-remote-state lid client new-state)
	 (live-forget-obsolete lid)
         (live-broadcast lid)
         new-state)))

(define (live-update lid client state)
  (with key (list lid client)
    (when (not (ahash-ref live-waiting key))
      (ahash-set! live-waiting key #t)
      (let* ((p (live-get-inverse-patch lid state))
             (mods (patch->modlist p))
             (new-state (live-current-state lid)))
        (when (debug-get "live")
          (display* "Send " client ": "
                    `(live-modify ,lid ,mods ,state ,new-state) "\n"))
        (server-remote-eval client `(live-modify ,lid ,mods ,state ,new-state)
          (lambda (ok?)
            (when (debug-get "live")
              (display* "Confirm " client ": " new-state ", " ok? "\n"))
            (ahash-remove! live-waiting key)
            (when ok?
	      (live-set-remote-state lid client new-state)
	      (live-forget-obsolete lid))
            (live-broadcast-one lid client)))))))

(define (live-broadcast-one lid client)
  (with state (live-get-remote-state lid client)
    (when (and (!= state (live-current-state lid))
               (active-client? client))
      (live-update lid client state))))

(define (live-broadcast lid)
  (for (client (live-get-connections lid))
    (live-broadcast-one lid client)))

;; The cursors of the users of a live document: each client says where its
;; cursor is in the document (live-cursor, a path in it, or #f when it left
;; the document), and the server tells the other clients of the document,
;; with the pseudo and the name of the user. The client is named by the
;; number of its connection: a user with two clients has two cursors.

(define live-cursor-table (make-ahash-table)) ;; (lid client) -> (uid pos)

(define (live-cursor-tell other lid client uid pos)
  (let* ((info (and uid (server-get-user-info uid)))
         (pseudo (if info (first info) "?"))
         (name (if info (second info) pseudo)))
    ;; (a client which does not know of cursors answers with an error)
    (server-remote-eval other `(live-cursor ,lid ,client ,pseudo ,name ,pos)
                        ignore ignore)))

(define (live-cursor-broadcast lid client uid pos)
  (if pos
      (ahash-set! live-cursor-table (list lid client) (list uid pos))
      (ahash-remove! live-cursor-table (list lid client)))
  (for (other (live-get-connections lid))
    (when (and (!= other client) (active-client? other))
      (live-cursor-tell other lid client uid pos))))

(define (live-cursor-welcome lid client)
  ;; a client which opens the document is told where the others are
  (for (x (ahash-table->list live-cursor-table))
    (let* ((lid* (caar x))
           (other (cadar x))
           (uid (cadr x))
           (pos (caddr x)))
      (when (and (== lid* lid) (!= other client) (active-client? other))
        (live-cursor-tell client lid other uid pos)))))

(tm-service (live-cursor lid pos)
  (with (client msg-id) envelope
    (let* ((uid (server-get-user envelope))
           (rid (live-find lid)))
      (if (and uid rid (db-allow? rid uid "readable")
               (or (not pos) (and (list? pos) (list-and (map integer? pos)))))
          (begin
            (live-cursor-broadcast lid client uid pos)
            (server-return envelope #t))
          (server-error envelope "Error: read access denied")))))

(tm-define (server-remove client)
  ;; the others no longer see the cursor of a client which leaves
  (for (lid (live-remote-connections client))
    (live-cursor-broadcast lid client #f #f))
  (former client)
  (for (key (map car (ahash-table->list live-waiting)))
    (when (== (cadr key) client)
      (ahash-remove! live-waiting key)))
  (for (lid (live-remote-connections client))
    (with-database (server-database)
      (live-save lid (tm->stree (live-current-document lid))))
    (live-hang-up lid client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public services
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (live-exists? lid)
  (server-return envelope (nnot (live-find lid))))

(tm-service (live-open lid)
  ;; Connect client to the live channel lid
  ;;(display* "live-open " lid "\n")
  (with uid (server-get-user envelope)
    (when (not (live-current-document lid))
      (when (not (live-find lid))
        (let* ((rid (live-new uid lid))
               (doc '(document "")))
          (live-save lid doc)))
      (with doc (live-load lid)
        (live-create lid doc)))
    (if (and-with rid (live-find lid)
          (db-allow? rid uid "readable"))
        (with (client msg-id) envelope
          (live-connect lid client)
          (live-cursor-welcome lid client)
          (let* ((doc (live-current-document lid))
                 (state (live-get-remote-state lid client)))
            (server-return envelope (list state (tm->stree doc)))))
        (server-error envelope "Error: read access denied"))))

(tm-service (live-modify lid mods old-state new-state)
  ;; States that the 'new-state' of the client is obtained
  ;; from 'old-state' by applying the list of modifications 'mods'
  (if (and-let* ((rid (live-find lid))
                 (uid (server-get-user envelope)))
        (db-allow? rid uid "writable"))
      (with (client msg-id) envelope
        (when (debug-get "live")
          (display* "Receive " client
                    ": " mods ", " old-state ", " new-state "\n"))
        (with ok? (live-applicable? lid client mods old-state)
          (when (debug-get "live")
            (when (not ok?)
              (display* ">> refuse " client ", " mods
                        ", state= " (live-current-state lid) "\n")))
          (when ok?
            (live-apply lid client mods old-state new-state))
          (server-return envelope ok?)))
      (server-error envelope "Error: write access denied")))
