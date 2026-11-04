;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : shared-files.scm
;; DESCRIPTION : Files open in several TeXmacs processes of the same user
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See src/docs/shared-files.md for the design. In short: when a file is
;; loaded and another process of the user has it open, the new process joins
;; the one which hosts the file (the entry of the file in a per-user folder
;; tells where to connect, with a token) and gets its document instead of
;; reading the file. Every participant sends its modifications to the host as
;; patches against a state; the host rebases them when it must (patch-pull,
;; only after patch-can-pull?, and checked), numbers the result as the next
;; state and sends it to all. Only the host writes the file.

(texmacs-module (utils relate shared-files)
  (:use (utils relate live-document)
        (utils relate live-connection)
        (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables, records, switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sf-table (make-ahash-table))      ; key -> record of a shared file
(define sf-by-buffer (make-ahash-table))  ; (url->string buffer) -> key
(define sf-applying? #f)                  ; applying a remote change
(define sf-tick-running? #f)
(define sf-incoming '())                  ; (link . time) not yet identified
(define sf-interactive 'auto)             ; #t, #f (tests) or auto
(define sf-serial 0)
(define sf-max-history 100)
(define sf-max-rebase 200)

(define sf-paused? #f)                    ; tests: do not serve the links
(define sf-stats (make-ahash-table))      ; tests: counts of what happened

(define (stat! name)
  (ahash-set! sf-stats name (+ 1 (or (ahash-ref sf-stats name) 0))))

(tm-define (shared-stat name)
  (:synopsis "For the tests: how many times @name (a symbol) happened")
  (or (ahash-ref sf-stats name) 0))

(tm-define (shared-pause! flag)
  (:synopsis "For the tests: stop (or resume) serving the links of the shared files")
  (set! sf-paused? flag))

(define (sf-get sf name) (ahash-ref sf name))

;; Patches are numbered by their author: the id is <author>/<n>. Every process
;; keeps the highest n it knows to have been applied, for each author (from
;; the patches the host sends, so that a participant that becomes host has it),
;; and a patch whose n is not above is not applied again.
(define (id-split id)
  (let loop ((i (- (string-length id) 1)))
    (cond ((< i 0) (cons id 0))
          ((char=? (string-ref id i) #\/)
           (cons (substring id 0 i)
                 (or (string->number (substring id (+ i 1) (string-length id))) 0)))
          (else (loop (- i 1))))))
(define (id-author id) (car (id-split id)))
(define (id-seq id) (cdr (id-split id)))
(define (seen-seq sf author) (or (ahash-ref (sf-get sf 'seen) author) 0))
(define (seen! sf id)
  (when (string? id)
    (ahash-set! (sf-get sf 'seen) (id-author id)
                (max (id-seq id) (seen-seq sf (id-author id))))))
(define (sf-put! sf name val) (ahash-set! sf name val))

(define sf-debugging? (and (getenv "TEXMACS_SHARED_DEBUG") #t))

(define (sf-debug . l)
  (when sf-debugging?
    (apply display* (cons "shared: " l))))

(tm-define (shared-interactive?)
  (:synopsis "Does this TeXmacs have a person at the keyboard?")
  (if (== sf-interactive 'auto) (not (headless?)) sf-interactive))

(tm-define (shared-set-interactive! flag)
  (:synopsis "For the tests: tell if this process counts as interactive")
  (set! sf-interactive flag))

(define sf-run-id (substring (shared-random-token) 0 8))   ; (a pid is reused)

(define (sf-new-id)
  (set! sf-serial (+ sf-serial 1))
  (string-append (number->string (shared-process-id)) "." sf-run-id "."
                 (number->string sf-serial)))

(define (sf-by-name name)
  (and-with key (ahash-ref sf-by-buffer (url->string name))
    (ahash-ref sf-table key)))

(define (sf-buffer sf) (sf-get sf 'buffer))
(define (sf-key sf) (sf-get sf 'key))
(define (sf-host? sf) (== (sf-get sf 'role) 'host))

(tm-define (shared-file? name)
  (:synopsis "Is the buffer @name shared with other processes?")
  (and (sf-by-name name) #t))

(tm-define (shared-file-participant? name)
  (:synopsis "Is the buffer @name shared, and hosted by another process?")
  (and-with sf (sf-by-name name)
    (== (sf-get sf 'role) 'participant)))

(tm-define (shared-file-info name)
  (:synopsis "The role, state and number of participants of the shared @name")
  (and-with sf (sf-by-name name)
    (list (sf-get sf 'role)
          (live-current-state (sf-key sf))
          (length (sf-get sf 'peers))
          (sf-get sf 'host-id)
          (sf-get sf 'id)
          (length (sf-get sf 'pending)))))

(tm-define (shared-files-list)
  (map (lambda (e) (cdr e)) (ahash-table->list sf-by-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The entries of the per-user folder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WHO HOSTS A FILE is decided by the system, not by messages or timeouts: a
;; process is the host of a file exactly while it holds an exclusive lock
;; (flock) on the file f-<hash of the canonical path>.lock of the folder. The
;; system frees the lock when the process dies, however it dies (kill -9
;; too), and never while it only stalls. Becoming a host is acquiring the
;; lock without waiting; failing means that another process hosts the file.
;;
;; The entry f-<hash>.entry only tells how to reach the host: (entry key port
;; token pid interactive host-id npeers), written (whole, replaced atomically)
;; by the holder of the lock after it has got it. It is not trusted for anything
;; else: a stale entry (the host is gone) is found by the lock being free.

(define (token=? a b)
  ;; without stopping at the first difference
  (and (string? a) (string? b) (== (string-length a) (string-length b))
       (let loop ((i 0) (acc 0))
         (if (>= i (string-length a))
             (== acc 0)
             (loop (+ i 1)
                   (+ acc (if (== (string-ref a i) (string-ref b i)) 0 1)))))))

(define (entry-path key)
  (with dir (shared-user-dir)
    (and (!= dir "")
         (string-append dir "/f-" (tree-hash (tm->tree key)) ".entry"))))

(define (lock-path key)
  (with dir (shared-user-dir)
    (and (!= dir "")
         (string-append dir "/f-" (tree-hash (tm->tree key)) ".lock"))))

(tm-define (shared-entry name)
  (:synopsis "The entry of the file @name in the per-user folder, or #f")
  (and-with key (shared-real-path (url->system name))
    (read-entry key)))

(tm-define (shared-entry-path name)
  (:synopsis "The file of the entry of @name in the per-user folder")
  (and-with key (shared-real-path (url->system name))
    (entry-path key)))

(tm-define (shared-lock-path name)
  (:synopsis "The file whose lock makes a process the host of @name")
  (and-with key (shared-real-path (url->system name))
    (lock-path key)))

(define (read-entry key)
  (and-with path (entry-path key)
    (with u (string->url path)
      (and (url-exists? u)
           (with e (catch #t
                     (lambda () (string->object (string-load u)))
                     (lambda args #f))
             (and (func? e 'entry) (== (length e) 8) (== (cadr e) key) e))))))

(define (write-entry key port token interactive host-id npeers)
  ;; (only by the holder of the lock) @npeers is the number of participants:
  ;; a process that finds the entry of a host which is gone knows by it how
  ;; long the participants take to seek the lock
  (and-with path (entry-path key)
    (shared-replace-file
     path (object->string (list 'entry key port token (shared-process-id)
                                interactive host-id npeers)))))

(define (host-write-entry! sf)
  (write-entry (sf-key sf) (local-listen) (sf-get sf 'token)
               (sf-get sf 'interactive) (sf-get sf 'id)
               (length (sf-get sf 'peers))))

(define (remove-entry key)
  ;; (only by the holder of the lock)
  (and-with path (entry-path key)
    (system-remove (string->url path))))

(define (lock-acquire key)
  ;; a handle, #f if another process holds the lock (or it can not be made)
  (and-with path (lock-path key)
    (with h (shared-lock-acquire path)
      (and (>= h 0) h))))

(define (lock-free? key)
  ;; (the lock is taken to be tested, and given back at once)
  (with h (lock-acquire key)
    (when h (shared-lock-release h))
    (and h #t)))

(define (list-dir-names dir)
  ;; (a tuple of strings comes as the list of the strings)
  (with l (shared-list-dir dir)
    (if (list? l)
        (map (lambda (n) (if (symbol? n) (symbol->string n) n)) l)
        '())))

(tm-define (shared-files-in-use)
  (:synopsis "The files which other processes of the user host: ((path interactive pid) ...)")
  ;; (for `texmacs --mcp': what the person has open; a file of this process
  ;; is not listed, nor an entry whose host is gone, which is found by its
  ;; lock being free)
  (let ((dir (shared-user-dir)))
    (if (== dir "")
        '()
        (let loop ((names (list-dir-names dir)) (acc '()))
          (cond ((null? names) (reverse acc))
                ((not (string-ends? (car names) ".entry")) (loop (cdr names) acc))
                (else
                  (let* ((u (string->url (string-append dir "/" (car names))))
                         (e (catch #t
                              (lambda () (string->object (string-load u)))
                              (lambda args #f))))
                    (if (and (func? e 'entry) (== (length e) 8)
                             (string? (cadr e))
                             (read-entry (cadr e))
                             (not (ahash-ref sf-table (cadr e)))
                             (not (lock-free? (cadr e))))
                        (loop (cdr names)
                              (cons (list (cadr e) (list-ref e 5) (list-ref e 4))
                                    acc))
                        (loop (cdr names) acc)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (msg-send link msg)
  (local-send link (object->string msg)))

(define (msg-receive link)
  ;; the next message of a link, or #f
  (with s (local-receive link)
    (and (!= s "")
         (catch #t
           (lambda () (string->object s))
           (lambda args #f)))))

(define (msg-wait link timeout)
  ;; the next message of a link, waiting for it (the other process answers
  ;; at its next tick); #f after the timeout or when the link is dead
  (let ((end (+ (texmacs-time) timeout)))
    (let loop ()
      (let ((m (msg-receive link)))
        (cond (m m)
              ((not (local-alive? link)) #f)
              ((> (texmacs-time) end) #f)
              (else (loop)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The document of a shared file and its modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What is shared is the body of the buffer and its style and initial
;; environment, which the editor keeps in its own copies, not in the body.
;; One tree holds them for the states and the patches: (shared META BODY),
;; where META is (meta STYLE INITIAL) as in the file: STYLE is the tuple of
;; the style and INITIAL a collection of (associate var value). Patches are
;; on this tree, with the paths of the body starting with 1 and those of the
;; meta tree with 0; there is one stream of states, one ordering, one rule
;; of rebase for both. In a process the parts are: the body of the buffer (a
;; tree observer makes the patches), and a shadow tree of the meta, which
;; mirrors the editor: each tick the editor's style and initial environment
;; are compared with the shadow (this takes in every path by which they change,
;; Document > Style, the page setup, the language, set-style-list, init-env,
;; the buffer-set of a whole document) and the differences become patches of
;; the shadow; a patch of the meta that comes from another process is applied
;; to the shadow and then installed in the editor (buffer-set-style,
;; buffer-set-init, buffer-reset-init), so that the next comparison finds
;; nothing to send back. The references and the auxiliary data are not
;; shared: each process typesets them.
;; The states are kept as for the live documents (live-create,
;; live-apply-patch: the document of the last state, the list of states, and
;; the inverse of each change). A participant keeps them too, from the
;; patches it is sent, so that it can take over.

(define (sf-body sf) (buffer-get-body (sf-buffer sf)))
(define (sf-meta sf) (sf-get sf 'meta))

(define (sf-composite-stree sf)
  `(shared ,(tm->stree (sf-meta sf)) ,(tm->stree (sf-body sf))))

(define (sf-composite sf)
  ;; (the live documents must not be the trees of the buffers)
  (stree->tree (sf-composite-stree sf)))

;; The initial environment is split in 16 collections, by a hash of the name
;; of the variable: (initial (collection ...) ...). Two settings added at the
;; same time would otherwise both be inserted at the end of one collection,
;; which is two inserts at the same place, and the second is refused (as for
;; two texts typed at the same place); in different collections they merge.
(define sf-buckets 16)

(define (var-bucket var)
  (modulo (apply + (map char->integer (string->list var))) sf-buckets))

(define (bucketize m)
  ;; (meta STYLE (collection (associate ...) ...)) as the editor gives it
  ;; into the form of the shared tree
  `(meta ,(cadr m)
         (initial ,@(map (lambda (b)
                           `(collection
                             ,@(list-filter (cdr (caddr m))
                                            (lambda (a) (== (var-bucket (cadr a)) b)))))
                         (.. 0 sf-buckets)))))

(define (flatten-meta m)
  `(meta ,(cadr m) (collection ,@(append-map cdr (cdr (caddr m))))))

(define (read-editor-meta name)
  (stree->tree (bucketize (tm->stree (buffer-get-meta name)))))

(define (prefix-mod i m)
  (let ((l (modification->scheme m)))
    (scheme->modification (list (car l) (cons i (cadr l)) (caddr l)))))

(define (strip-mod m)
  (let ((l (modification->scheme m)))
    (scheme->modification (list (car l) (cdr (cadr l)) (caddr l)))))

(define (split-patch p part)
  ;; the modifications of @p which are under child @part of the shared tree,
  ;; with their paths relative to it; #f for a kind of patch which a patch
  ;; made here never is
  (cond ((patch-pair? p)
         (let ((path (modification-path (patch-direct p))))
           (cond ((not (pair? path)) #f)
                 ((== (car path) part)
                  (patch-pair (strip-mod (patch-direct p))
                              (strip-mod (patch-inverse p))))
                 (else (patch-compound '())))))
        ((patch-compound? p)
         (let ((l (map (lambda (c) (split-patch c part)) (patch-children p))))
           (and (list-and (map (lambda (x) (and x #t)) l))
                (patch-compound l))))
        (else #f)))

(define (meta-install! sf)
  ;; make the editor have the style and initial environment of the shadow
  (let* ((name (sf-buffer sf))
         (cur (tm->stree (buffer-get-meta name)))
         (want (flatten-meta (tm->stree (sf-meta sf)))))
    (when (!= cur want)
      (when (!= (cadr cur) (cadr want))
        (buffer-set-style name (stree->tree (cadr want))))
      (let ((cur-init (map (lambda (a) (cons (cadr a) (caddr a))) (cdr (caddr cur))))
            (want-init (map (lambda (a) (cons (cadr a) (caddr a))) (cdr (caddr want)))))
        (for (a cur-init)
          (when (not (assoc (car a) want-init))
            (buffer-reset-init name (car a))))
        (for (a want-init)
          (let ((c (assoc (car a) cur-init)))
            (when (or (not c) (!= (cdr c) (cdr a)))
              (buffer-set-init name (car a) (stree->tree (cdr a)))))))
      (sf-put! sf 'meta-hash (buffer-meta-hash name)))))

(define (meta-diff! shadow new)
  ;; Change the tree @shadow into the stree @new, by modifications; gives
  ;; their patches (paths with the 0 of the meta part), the first first.
  (let ((out '()))
    (define (do! m)
      (let ((inv (modification-invert m shadow)))
        (modification-inplace-apply shadow m)
        (set! out (cons (patch-pair (prefix-mod 0 m) (prefix-mod 0 inv)) out))))
    (define (index-of var l)
      (let loop ((l l) (i 0))
        (cond ((null? l) #f)
              ((== (cadr (car l)) var) i)
              (else (loop (cdr l) (+ i 1))))))
    (define (diff-bucket! b want)
      (define (cur-list) (cdr (tm->stree (tree-ref shadow 1 b))))
      ;; variables which are no longer there
      (let loop ()
        (let find ((l (cur-list)) (i 0))
          (when (nnull? l)
            (if (index-of (cadr (car l)) want)
                (find (cdr l) (+ i 1))
                (begin (do! (modification-remove (list 1 b) i 1))
                       (loop))))))
      ;; new values, new variables
      (for (a want)
        (let ((i (index-of (cadr a) (cur-list))))
          (cond ((not i)
                 ;; (the content inserted is a node whose children are inserted)
                 (do! (modification-insert (list 1 b) (length (cur-list))
                                           (stree->tree `(collection ,a)))))
                ((!= (caddr (list-ref (cur-list) i)) (caddr a))
                 (do! (modification-assign (list 1 b i 1) (stree->tree (caddr a)))))))))
    (when (!= (cadr new) (tm->stree (tree-ref shadow 0)))
      (do! (modification-assign (list 0) (stree->tree (cadr new)))))
    (for (b (.. 0 sf-buckets))
      (let ((want (cdr (list-ref (cdr (caddr new)) b))))
        (when (!= (sort-assoc want)
                  (sort-assoc (cdr (tm->stree (tree-ref shadow 1 b)))))
          (diff-bucket! b want))))
    (reverse out)))

(define (sort-assoc l)
  (sort l (lambda (a b) (string<? (cadr a) (cadr b)))))

(define (meta-same? a b)
  ;; two bucketed metas (strees), the order within a collection not mattering
  (and (== (cadr a) (cadr b))
       (let loop ((x (cdr (caddr a))) (y (cdr (caddr b))))
         (cond ((null? x) (null? y))
               ((null? y) #f)
               (else (and (== (sort-assoc (cdr (car x))) (sort-assoc (cdr (car y))))
                          (loop (cdr x) (cdr y))))))))

(define (meta-poll! sf)
  ;; the changes of the style and of the initial environment in the editor,
  ;; since the last time, become local changes. A number from the editor
  ;; (buffer-meta-hash) is compared first: the trees are made and compared
  ;; only when it changed.
  (when (and (not sf-applying?) (buffer-exists? (sf-buffer sf)))
    (let ((h (buffer-meta-hash (sf-buffer sf))))
      (when (!= h (sf-get sf 'meta-hash))
        (sf-put! sf 'meta-hash h)
        (let ((new (bucketize (tm->stree (buffer-get-meta (sf-buffer sf))))))
          (when (not (meta-same? new (tm->stree (sf-meta sf))))
            (for (pp (meta-diff! (sf-meta sf) new))
              (sf-put! sf 'collect (cons pp (sf-get sf 'collect))))))))))

(define (author-of sf peer-id)
  (with t (sf-get sf 'authors)
    (or (ahash-ref t peer-id)
        (with a (new-author)
          (ahash-set! t peer-id a)
          a))))

(define (apply-in-place! sf p author)
  ;; apply the patch @p to the body of the buffer and to the meta tree (then
  ;; to the editor), as a change of @author
  (let ((body (sf-body sf)) (meta (sf-meta sf))
        (mp (split-patch p 0)) (bp (split-patch p 1)))
    (and mp bp (patch-applicable? bp body) (patch-applicable? mp meta)
         (with-author author
           (with-global sf-applying? #t
             (patch-inplace-apply body bp)
             (when (patch-modifies? mp)
               (patch-inplace-apply meta mp)
               (meta-install! sf))))
         #t)))

(define (sf-notify-modified! sf)
  (sf-put! sf 'dirty #t))

(tm-define (shared-notify event t mod)
  (:secure #t)
  (when (and (not sf-applying?) (== event 'announce)
             (!= (modification-type mod) 'set-cursor))
    (and-with buf (path->buffer (tree->path t))
      (and-with key (ahash-ref sf-by-buffer (url->string buf))
        (and-with sf (ahash-ref sf-table key)
          (let ((m (modification-copy mod)))
            (sf-put! sf 'collect
                     (cons (patch-pair (prefix-mod 1 m)
                                       (prefix-mod 1 (modification-invert m t)))
                           (sf-get sf 'collect)))))))))

(define (take-local-patch sf)
  ;; the local changes since the last time: a patch, or #f
  (meta-poll! sf)
  (let ((l (sf-get sf 'collect)))
    (sf-put! sf 'collect '())
    (and (nnull? l)
         (with p (patch-remove-set-cursor (patch-compound (reverse l)))
           (and (patch-modifies? p) p)))))

(define (sf-trim! key)
  (let ((states (live-get-history key)))
    (when (and states (> (length states) (+ sf-max-history 50)))
      (live-forget-prior key (list-ref states sf-max-history)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rebasing a patch made against an older state (section 4.4 of the design)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rebase-over-history key base p)
  ;; The patch @p, made against the state @base, as a patch against the
  ;; current state of the live document @key, or #f when it can not be: patch
  ;; -pull is called only after patch-can-pull?, and the result is checked
  ;; (it applies, and what it is co-pulled to leads back to the base with p).
  (let ((chs (live-get-patch-list key base)))
    (and chs (<= (length chs) sf-max-rebase)
         (let loop ((l (reverse chs)) (cur p) (revs '()))
           (if (null? l)
               (let* ((now (live-current-document key))
                      (d0 (live-get-document key base)))
                 (and d0 (patch-applicable? p d0) (patch-applicable? cur now)
                      (let ((ta (patch-apply d0 p)))
                        (let back ((t (patch-apply now cur)) (rs revs))
                          (cond ((null? rs) (and (== t ta) cur))
                                ((patch-applicable? (car rs) t)
                                 (back (patch-apply t (car rs)) (cdr rs)))
                                (else #f))))))
               (let ((q (car l)))
                 (and (patch-can-pull? cur q)
                      (loop (cdr l) (patch-pull cur q)
                            (cons (patch-co-pull cur q) revs)))))))))

(define (transform-pending pending rev)
  ;; The patches @pending, made one after the other on top of a document, as
  ;; patches on top of that document changed by a patch r, of which @rev is
  ;; the inverse: each is pulled over @rev, which becomes the inverse of r
  ;; changed by the patch before (patch-co-pull). Gives those which can be
  ;; (the first ones; the others depend on a patch which can not).
  (let loop ((l pending) (rev rev) (kept '()))
    (if (null? l)
        (reverse kept)
        (if (patch-can-pull? (car l) rev)
            (loop (cdr l) (patch-co-pull (car l) rev)
                  (cons (patch-pull (car l) rev) kept))
            (reverse kept)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making a shared file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sf-make name key role)
  (with sf (make-ahash-table)
    (sf-put! sf 'buffer name)
    (sf-put! sf 'key key)
    (sf-put! sf 'role role)
    (sf-put! sf 'id (sf-new-id))
    (sf-put! sf 'interactive (shared-interactive?))
    (sf-put! sf 'authors (make-ahash-table))
    (sf-put! sf 'seen (make-ahash-table))
    (sf-put! sf 'collect '())
    (sf-put! sf 'pending '())     ; local patches not yet in the host's states
    (sf-put! sf 'inflight #f)     ; id of the first of them, sent
    (sf-put! sf 'peers '())       ; host: (id link interactive seq pid)
    (sf-put! sf 'roster '())      ; participant: (id interactive seq pid)
    (sf-put! sf 'seq-counter 0)
    (sf-put! sf 'dirty #f)
    (sf-put! sf 'conn #f)         ; participant: the link to the host
    (sf-put! sf 'host-id #f)
    (sf-put! sf 'seeking #f)
    (sf-put! sf 'host-token #f)
    (sf-put! sf 'msg-id 0)
    (sf-put! sf 'meta (read-editor-meta name))
    (sf-put! sf 'meta-hash (buffer-meta-hash name))
    (ahash-set! sf-table key sf)
    (ahash-set! sf-by-buffer (url->string name) key)
    (sf-put! sf 'observer (tree-observe (buffer-get-body name) "shared-notify"))
    (set-shared-files-in-use! #t)
    (shared-ensure-tick!)
    sf))

(define (sf-destroy! sf)
  (let ((key (sf-key sf)))
    (when (sf-get sf 'lock)
      (shared-lock-release (sf-get sf 'lock))
      (sf-put! sf 'lock #f))
    (and-with obs (sf-get sf 'observer)
      (catch #t (lambda () (tree-pointer-detach obs)) (lambda args #f)))
    (ahash-remove! sf-table key)
    (ahash-remove! sf-by-buffer (url->string (sf-buffer sf)))
    (live-forget-all key)))

;; (the live documents have no way to forget one: its document is replaced
;; by a small one)
(define (live-forget-all key)
  (catch #t
    (lambda () (live-create key "" "x") (noop))
    (lambda args (noop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which files are shared
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (shared-files-on?)
  (:synopsis "Is the sharing of open files switched on (preference)?")
  (== (get-preference "share-open-files") "on"))

(define (shareable-key name)
  ;; the canonical path of the file @name when it can be shared, else #f:
  ;; a plain file of the system, in TeXmacs format, which exists
  (and (url? name)
       (not (url-scratch? name))
       (not (url-rooted-web? name))
       (not (url-rooted-tmfs? name))
       (url-test? name "f")
       (== (url-format name) "texmacs")
       (with key (shared-real-path (url->system name))
         (and (!= key "") (!= (shared-user-dir) "") key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (the hello: token of the entry, file, my id, my process, am I interactive,
;; the state I have, or #f if I have none)
(define (do-hello key entry id interactive base)
  ;; the link, or a negative number: -2 refused, -1 another error
  (let ((c (local-connect (caddr entry))))
    (when (>= c 0)
      (msg-send c (list 'hello (cadddr entry) key id (shared-process-id)
                        interactive base #f)))
    c))

(define sf-join-wait 500)   ; ms: the most a join makes the interface wait
(define sf-unshared (make-ahash-table))  ; names opened without sharing

;; A host that is gone but whose participants may be seeking the lock (its
;; entry says how many): a process which opens the file then must not host it
;; from the disk, which has less than the participants have. It opens the file
;; read-only (the footer says that it is connecting) and polls, on the timer,
;; for the new host, for as long as the participants take (300 ms for each, and
;; one second); it hosts from the disk only after that, if the lock is still
;; free. Nothing blocks.
(define sf-pending (make-ahash-table))   ; (url->string name) -> record

(define (pending-join! name key wait old-token)
  (ahash-set! sf-pending (url->string name)
              (list name key (+ (texmacs-time) wait) old-token 0))
  (shared-ensure-tick!))

(define sf-was-read-only (make-ahash-table))

(define (pending-start! name)
  ;; the buffer of a file whose host is awaited
  ;; (the state before the first time only: later calls find it read-only)
  (when (not (ahash-ref sf-was-read-only (url->string name)))
    (ahash-set! sf-was-read-only (url->string name)
                (if (buffer-read-only? name) 'yes 'no)))
  (buffer-set-read-only name #t)
  (set-message "Shared file: connecting to the process which had this file open (read-only for a moment)"
               "Shared file"))

(define (pending-done! name)
  (ahash-remove! sf-pending (url->string name))
  (when (buffer-exists? name)
    (buffer-set-read-only name (== (ahash-ref sf-was-read-only (url->string name)) 'yes)))
  (ahash-remove! sf-was-read-only (url->string name)))

(define (pending-poll!)
  (let ((now (texmacs-time)))
    (for (rec (map cdr (ahash-table->list sf-pending)))
      (with (name key deadline old-token next) rec
        (when (>= now next)
          (ahash-set! sf-pending (url->string name)
                      (list name key deadline old-token (+ now 100)))
          (cond
            ((not (buffer-exists? name))
             (ahash-remove! sf-pending (url->string name))
             (ahash-remove! sf-was-read-only (url->string name)))
            ((not (lock-free? key))
             ;; someone hosts: join it when its entry is new
             (let ((e (read-entry key)))
               (cond ((and e (not (and old-token (token=? (cadddr e) old-token))))
                      (pending-done! name)
                      (with r (join-host name key e)
                        (cond ((== r #t) #t)
                              ((== r 'busy) (tell-unshared name))
                              (else (host-start name key 0)))))
                     ((> now deadline)
                      (pending-done! name)
                      (tell-unshared name)))))
            ((> now deadline)
             ;; nobody took the file: from the disk
             (pending-done! name)
             (host-start name key 0))))))))

(define (tell-unshared name)
  (set-message "Shared file: the process which has this file open does not answer; it is opened from the file, without sharing"
               "Shared file"))

(define (note-unshared! name)
  ;; another process holds the lock of the file (it hosts it) but does not
  ;; answer: the file is read as usual, not shared. It is never hosted
  ;; meanwhile: the lock is what makes a host, and a stalled host keeps it.
  (ahash-set! sf-unshared (url->string name) (texmacs-time))
  (tell-unshared name))

(define (abandoned! name key entry)
  ;; the lock of the file is free but its entry is there: the host is gone
  (when (> (list-ref entry 7) 0)
    (pending-join! name key (+ 1000 (* 300 (list-ref entry 7))) (cadddr entry)))
  #f)

(tm-define (shared-file-try-join name)
  (:synopsis "Join the process which has the file @name open, if there is one")
  ;; Called when @name is to be loaded: if another process hosts it, the
  ;; buffer is made from the document of the host (and the file is not read);
  ;; gives #t then, #f when the file is to be loaded as usual.
  (and (shared-files-on?)
       (not (buffer-exists? name))
       (with key (shareable-key name)
         (and key
              (not (ahash-ref sf-table key))
              (with entry (read-entry key)
                (and entry
                     (if (lock-free? key)
                         (abandoned! name key entry)
                         (with r (join-host name key entry)
                           (cond ((== r #t) #t)
                                 ((== r 'busy) (note-unshared! name) #f)
                                 (else (abandoned! name key entry)))))))))))

;; (join-host with a buffer which exists, read from the file, replaces it)
(define (join-host name key entry)
  ;; gives #t (joined), 'busy (the lock is held, but the host does not
  ;; answer: it may be busy or stopped) or #f (nobody holds the lock: the
  ;; entry is not of a host)
  (let* ((id (sf-new-id)) (inter (shared-interactive?))
         (c (do-hello key entry id inter #f))
         (m (and (>= c 0) (msg-wait c sf-join-wait))))
    (cond ((and m (func? m 'welcome) (== (length m) 7))
           (with (tag state doc modified roster host-id meta) m
             (buffer-set name (stree->tree doc))
             (let ((sf (sf-make name key 'participant)))
               (sf-put! sf 'meta (stree->tree meta))
               (meta-install! sf)
               (sf-put! sf 'id id)
               (sf-put! sf 'interactive inter)
               (sf-put! sf 'conn c)
               (sf-put! sf 'host-id host-id)
               (sf-put! sf 'host-token (cadddr entry))
               (sf-put! sf 'roster roster)
               (live-create key (sf-composite sf) state)
               (if modified
                   (buffer-pretend-modified name)
                   (buffer-pretend-saved name))
               (sf-debug "joined " key " state " state "\n")
               #t)))
          (else
            (when (>= c 0) (local-close c))
            (if (lock-free? key) #f 'busy)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hosting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (shared-file-opened name)
  (:synopsis "Called when the buffer @name has been loaded from its file")
  ;; The process hosts the file if it can get the lock; else a host appeared
  ;; meanwhile and the process joins it, replacing what it has read.
  (cond ((ahash-ref sf-pending (url->string name))
         (pending-start! name))
        ((let ((t (ahash-ref sf-unshared (url->string name))))
           (ahash-remove! sf-unshared (url->string name))
           (and t (< (- (texmacs-time) t) 10000)))
         (noop))
        ((and (shared-files-on?) (not (sf-by-name name)))
         (and-with key (shareable-key name)
           (when (not (ahash-ref sf-table key))
             (host-start name key 0))))))

(define (host-start name key tries)
  (let ((port (local-listen)) (h (lock-acquire key)))
    (cond ((not (> port 0)) (when h (shared-lock-release h)))
          (h
            (let* ((token (shared-random-token))
                   (sf (sf-make name key 'host)))
              (sf-put! sf 'lock h)
              (sf-put! sf 'token token)
              (sf-put! sf 'host-id (sf-get sf 'id))
              (live-create key (sf-composite sf))
              (host-write-entry! sf)
              (sf-put! sf 'touch-at (+ (texmacs-time) 3600000))
              (sf-debug "hosting " key " on " port "\n")))
          (else
            ;; someone holds the lock: it hosts the file
            (let ((e (read-entry key)))
              (if (not e)
                  (begin                         ; (it is starting: wait)
                    (pending-join! name key 2000 #f)
                    (pending-start! name))
                  (with r (join-host name key e)
                    (cond ((== r #t) #t)
                          ((== r 'busy) (note-unshared! name))
                          ((< tries 3) (host-start name key (+ tries 1)))
                          (else (note-unshared! name))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Peers, roster, broadcasting (host)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (peer-id p) (list-ref p 0))
(define (peer-link p) (list-ref p 1))
(define (peer-interactive? p) (list-ref p 2))
(define (peer-seq p) (list-ref p 3))
(define (peer-pid p) (list-ref p 4))

(define (find-peer sf id)
  (with l (list-filter (sf-get sf 'peers) (lambda (p) (== (peer-id p) id)))
    (and (nnull? l) (car l))))

(define (roster-of sf)
  ;; (id interactive seq pid) of all the participants, host included
  (cons (list (sf-get sf 'id) (sf-get sf 'interactive) 0 (shared-process-id))
        (map (lambda (p)
               (list (peer-id p) (peer-interactive? p) (peer-seq p) (peer-pid p)))
             (sf-get sf 'peers))))

(define (send-peers sf msg)
  (for (p (sf-get sf 'peers))
    (msg-send (peer-link p) msg)))

(define (send-roster sf)
  (with r (roster-of sf)
    (sf-put! sf 'roster r)
    (host-write-entry! sf)
    (send-peers sf (list 'roster r))))

(define (drop-peer! sf p)
  (local-close (peer-link p))
  (sf-put! sf 'peers (list-filter (sf-get sf 'peers)
                                  (lambda (q) (!= (peer-id q) (peer-id p)))))
  (send-roster sf))

(define (priority-less? a b)
  ;; entries (id interactive seq pid): interactive first, then the earlier
  (cond ((and (cadr a) (not (cadr b))) #t)
        ((and (cadr b) (not (cadr a))) #f)
        (else (< (caddr a) (caddr b)))))

(define (by-priority l)
  (sort l priority-less?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The host: its own changes, the patches of the others
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (host-commit! sf p author-id)
  ;; @p has been applied to the buffer (the host's own changes): it is the
  ;; next state; the others are told
  (let* ((key (sf-key sf))
         (from (live-current-state key))
         (to (live-apply-patch key p)))
    (cond (to
           (sf-notify-modified! sf)
           (send-peers sf (list 'applied from to author-id #f (patch->scheme p)))
           (sf-trim! key))
          (else
            ;; the live document is not the buffer: start again from the buffer
            (sf-debug "host: live document differs, new start\n")
            (host-reset-live! sf)))))

(define (host-reset-live! sf)
  (let* ((key (sf-key sf))
         (state (live-create key (sf-composite sf))))
    (send-peers sf (list 'welcome-reset state (sf-composite-stree sf)
                         (or (sf-get sf 'dirty) (buffer-modified? (sf-buffer sf)))
                         (roster-of sf) (sf-get sf 'id)))))

(define (host-flush! sf)
  (and-with p (take-local-patch sf)
    (host-commit! sf p (sf-get sf 'id))))

(define (host-handle-patch sf peer id base psexp)
  (sf-debug "host got patch " id " base " base "\n")
  (host-flush! sf)
  (if (<= (id-seq id) (seen-seq sf (peer-id peer)))
      ;; (sent again: it was applied, its author did not hear of it)
      (begin (stat! 'duplicate)
             (msg-send (peer-link peer)
                       (list 'ack id (live-current-state (sf-key sf)))))
      (host-handle-patch* sf peer id base psexp)))

(define (host-handle-patch* sf peer id base psexp)
  (let* ((key (sf-key sf))
         (cur (live-current-state key))
         (p0 (patch-remove-set-cursor (scheme->patch psexp)))
         (p (cond ((== base cur) p0)
                  ((in? base (live-get-history key))
                   (with r (rebase-over-history key base p0)
                     (when r (stat! 'rebased))
                     r))
                  (else #f))))
    (cond ((and p (patch-applicable? p (live-current-document key)))
           ;; (applied to the buffer first: if that fails nothing moves, but
           ;; the buffer and the live document differ: start again from the
           ;; buffer, so that the host heals)
           (if (apply-in-place! sf p (author-of sf (peer-id peer)))
               (let ((to (live-apply-patch key p)))
                 (sf-notify-modified! sf)
                 (seen! sf id)
                 (cond (to
                         (send-peers sf (list 'applied cur to (peer-id peer) id
                                              (patch->scheme p)))
                         (sf-trim! key))
                       (else (host-reset-live! sf))))
               (begin
                 (stat! 'refused)
                 (host-reset-live! sf)
                 (msg-send (peer-link peer) (list 'refused id cur)))))
          (else
            (stat! 'refused)
            (sf-debug "refused a patch of " (peer-id peer) " from " base "\n")
            (msg-send (peer-link peer) (list 'refused id cur))))))

(define (host-send-welcome sf link base id interactive pid inflight)
  ;; the answer to a hello: the document, or what the other lacks
  (let* ((key (sf-key sf))
         (cur (live-current-state key))
         (name (sf-buffer sf)))
    (cond ((not base)
           (msg-send link (list 'welcome cur (tm->stree (buffer-get name))
                                (or (sf-get sf 'dirty) (buffer-modified? name))
                                (roster-of sf) (sf-get sf 'id)
                                (tm->stree (sf-meta sf)))))
          ((and inflight (<= (id-seq inflight) (seen-seq sf id)))
           ;; the patch it had on its way was applied: its author did not hear
           (msg-send link (list 'welcome-reset cur (sf-composite-stree sf)
                                (or (sf-get sf 'dirty) (buffer-modified? name))
                                (roster-of sf) (sf-get sf 'id))))
          ((== base cur)
           (msg-send link (list 'welcome-same cur (roster-of sf)
                                (sf-get sf 'id))))
          ((in? base (live-get-history key))
           (with p (live-get-inverse-patch key base)
             (msg-send link (list 'welcome-catchup base cur (patch->scheme p)
                                  (roster-of sf) (sf-get sf 'id)))))
          (else
            (msg-send link (list 'welcome-reset cur (sf-composite-stree sf)
                                 (or (sf-get sf 'dirty) (buffer-modified? name))
                                 (roster-of sf) (sf-get sf 'id)))))))

(define (host-accept-hello sf link id pid interactive base inflight)
  (host-flush! sf)
  (if (not (local-alive? link))
      (local-close link)
      (host-accept-hello* sf link id pid interactive base inflight)))

(define (host-accept-hello* sf link id pid interactive base inflight)
  (let ((seq (+ (sf-get sf 'seq-counter) 1)))
    (sf-put! sf 'seq-counter seq)
    (sf-put! sf 'peers
             (append (list-filter (sf-get sf 'peers)
                                  (lambda (p) (!= (peer-id p) id)))
                     (list (list id link interactive seq pid))))
    (host-send-welcome sf link base id interactive pid inflight)
    (send-roster sf)
    ;; an interactive process hosts rather than a headless one: it is
    ;; offered the file
    (when interactive (host-offer-handover! sf))))

(define (host-handle-message sf peer msg)
  (cond ((func? msg 'patch)
         (with (tag id base p) msg
           (host-handle-patch sf peer id base p)))
        ((func? msg 'save)
         (host-save sf))
        ((func? msg 'leave)
         (drop-peer! sf peer))
        ((func? msg 'resync)
         (host-flush! sf)
         (msg-send (peer-link peer)
                   (list 'welcome-reset (live-current-state (sf-key sf))
                         (sf-composite-stree sf)
                         (or (sf-get sf 'dirty) (buffer-modified? (sf-buffer sf)))
                         (roster-of sf) (sf-get sf 'id))))
        ))

(define (host-tick sf)
  (host-flush! sf)
  (for (p (sf-get sf 'peers))
    (let loop ()
      (with m (msg-receive (peer-link p))
        (when m
          (host-handle-message sf p m)
          (when (find-peer sf (peer-id p)) (loop)))))
    (when (and (find-peer sf (peer-id p)) (not (local-alive? (peer-link p))))
      (sf-debug "peer " (peer-id p) " is gone\n")
      (drop-peer! sf p)))
  (host-check-lock! sf)
  ;; a headless host retries to give the file to an interactive participant
  (let ((now (texmacs-time)))
    (when (> now (or (sf-get sf 'offer-at) 0))
      (sf-put! sf 'offer-at (+ now 5000))
      (host-offer-handover! sf))))

(define (host-check-lock! sf)
  ;; The lock is a file: if it has been removed (or replaced) another process
  ;; could make and lock a new one. Once a second the host checks that the file
  ;; it holds is the file at the path; if not it makes it again and locks it:
  ;; if that works nobody else hosts and it goes on; if not another process
  ;; hosts, and this one stops at once (its document is saved) and joins it.
  ;; The files are touched once an hour, for the cleaners of /tmp.
  (let ((now (texmacs-time)) (key (sf-key sf)))
    (when (> now (or (sf-get sf 'lock-check) 0))
      (sf-put! sf 'lock-check (+ now 1000))
      (let ((path (lock-path key)) (h (sf-get sf 'lock)))
        (cond ((not path) (host-stop-sharing! sf))
              ((not (shared-lock-valid? h path))
               (with r (shared-lock-renew h path)
                 (cond ((== r 1) (stat! 'lock-renewed)
                        (sf-put! sf 'renew-errors 0)
                        (host-write-entry! sf))
                       ((== r 0) (host-lost-lock! sf))
                       ;; any other error (no descriptor, no room): try again at
                       ;; the next tick, but not for ever: another process may
                       ;; be hosting; after 5 times the lock is taken for lost
                       (else
                         (stat! 'lock-renew-error)
                         (sf-put! sf 'renew-errors (+ 1 (or (sf-get sf 'renew-errors) 0)))
                         (when (>= (sf-get sf 'renew-errors) 5)
                           (host-lost-lock! sf))))))
              (else
                (sf-put! sf 'renew-errors 0)       ; (the lock is valid)
                (when (not (url-exists? (string->url (entry-path key))))
                  (host-write-entry! sf))))
        (when (and path (sf-host? sf) (> now (or (sf-get sf 'touch-at) 0)))
          (sf-put! sf 'touch-at (+ now 3600000))
          (shared-touch path)
          (shared-touch (entry-path key)))))))

(define (host-stop-sharing! sf)
  ;; the folder of the locks can not be had any more: this file is no longer
  ;; shared; the participants seek a host (and find none, or one)
  (stat! 'folder-lost)
  (set-message "Shared file: the folder of the shared files is not available; this file is no longer shared"
               "Shared file")
  (send-peers sf (list 'released (sf-get sf 'token) #f (roster-of sf)))
  (for (p (sf-get sf 'peers)) (local-close (peer-link p)))
  (sf-put! sf 'peers '())
  (sf-destroy! sf))

(define (host-lost-lock! sf)
  (stat! 'lock-lost)
  (sf-put! sf 'renew-errors 0)
  (let ((file (save-recovered-copy! sf)))
    (set-message
     `(concat "Shared file: another process hosts this file now (the lock was removed)."
              ,(if file
                   `(concat " A copy of your document is in " (verbatim ,(url->system file)))
                   ""))
     "Shared file"))
  (send-peers sf (list 'released (sf-get sf 'token) #f (roster-of sf)))
  (for (p (sf-get sf 'peers)) (local-close (peer-link p)))
  (sf-put! sf 'peers '())
  (shared-lock-release (sf-get sf 'lock))
  (sf-put! sf 'lock #f)
  (sf-put! sf 'role 'participant)
  (sf-put! sf 'roster (roster-of sf))
  (sf-put! sf 'host-id (sf-get sf 'id))
  (participant-seek! sf (sf-get sf 'token) #f))

(define (host-save sf)
  ;; (asked by a participant, or the host's own Save which comes here)
  (host-flush! sf)
  (let ((name (sf-buffer sf)))
    (when (sf-get sf 'dirty) (buffer-pretend-modified name))
    (save-buffer-main name)))

(tm-define (shared-file-save-request name)
  (:synopsis "A Save of the buffer @name: handled here when another process hosts it")
  (and-with sf (sf-by-name name)
    (and (== (sf-get sf 'role) 'participant)
         (begin
           ;; (always queued: with no connection to the host (it is being
           ;; sought, or does not answer) this process does not write the file)
           (sf-put! sf 'save-wanted #t)
           (when (not (sf-get sf 'conn)) (stat! 'save-queued))
           (participant-flush! sf)
           (participant-submit! sf)
           (participant-send-save-when-clean! sf)
           (set-message (if (sf-get sf 'conn)
                            "Save asked to the host of the file"
                            "Save queued: the host of the file is not reachable now; it is saved when it answers")
                        "Save file")
           #t))))

(tm-define (shared-file-saved name)
  (:synopsis "Called after the buffer @name has been saved to its file")
  (and-with sf (sf-by-name name)
    (when (sf-host? sf)
      (stat! 'saves)
      (sf-put! sf 'dirty #f)
      (send-peers sf (list 'saved (live-current-state (sf-key sf)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The participant: its changes, the messages of the host
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (participant-flush! sf)
  (and-with p (take-local-patch sf)
    (sf-put! sf 'pending (append (sf-get sf 'pending) (list p)))))

(define (participant-submit! sf)
  ;; one patch at a time is sent: the first of the pending ones
  (when (and (sf-get sf 'conn) (not (sf-get sf 'inflight))
             (nnull? (sf-get sf 'pending))
             (not (sf-get sf 'seeking)))
    (let* ((n (+ (sf-get sf 'msg-id) 1))
           (id (string-append (sf-get sf 'id) "/" (number->string n))))
      (sf-put! sf 'msg-id n)
      (stat! 'submitted)
      (sf-put! sf 'inflight id)
      (let ((msg (list 'patch id
                       (if (sf-get sf 'bad-base) "no such state"
                           (live-current-state (sf-key sf)))
                       (patch->scheme (car (sf-get sf 'pending))))))
        (sf-put! sf 'last-msg msg)
        (msg-send (sf-get sf 'conn) msg)))))

(tm-define (shared-test-resend-last! name)
  (:synopsis "For the tests: send the last patch again")
  (and-with sf (sf-by-name name)
    (when (and (sf-get sf 'conn) (sf-get sf 'last-msg))
      (msg-send (sf-get sf 'conn) (sf-get sf 'last-msg)))))

(tm-define (shared-test-bad-base! name flag)
  (:synopsis "For the tests: submit the patches against a state which does not exist")
  (and-with sf (sf-by-name name)
    (sf-put! sf 'bad-base flag)))

;; Changes of the person which are not kept (refused, dropped when the host's
;; patch could not be combined with them, not sent when the buffer is closed)
;; are not lost silently. What can be recovered is what the person typed: the
;; contents which the lost patches insert or put in place. It is saved in a
;; document in $TEXMACS_HOME_PATH/system/recovered (as the document of a
;; failed typesetting is), the footer says so, and an interactive TeXmacs asks
;; whether to open it (a question stays until it is answered). The deletions
;; and the moves among the lost changes are not recoverable this way (the
;; text they removed is still in the document): the notice says that
;; changes were not kept.

(define (lost-setting-texts kind t)
  ;; a setting or the style which was not kept, as a paragraph
  (cond ((func? t 'collection)
         (map (lambda (a) (string-append "Setting " (cadr a) ": " (object->string (caddr a))))
              (list-filter (cdr t) (lambda (a) (func? a 'associate)))))
        ((func? t 'tuple) (list (string-append "Style: " (object->string (cdr t)))))
        ((string? t) (list (string-append "Setting changed to: " t)))
        (else '())))

(define (patch-leaves p)
  (cond ((patch-pair? p) (list p))
        ((patch-compound? p) (append-map patch-leaves (patch-children p)))
        (else '())))

(define (content-items c)
  (cond ((and (string? c) (== c "")) '())
        ((func? c 'document) (cdr c))
        (else (list c))))

(define (lost-items pp)
  ;; what the person did in the change @pp (a pair of modifications), in a
  ;; readable form: the text typed, the content deleted (it is in the inverse
  ;; modification), a description of a change of structure, the settings
  (let* ((m (modification->scheme (patch-direct pp)))
         (im (modification->scheme (patch-inverse pp)))
         (kind (car m)) (path (cadr m)) (t (caddr m))
         (where (object->string (if (pair? path) (cdr path) path))))
    (cond ((not (pair? path)) '())
          ((== (car path) 0)                    ; the style, the settings
           (cond ((== kind 'remove)
                  (map (lambda (x) (string-append "Removed: " x))
                       (lost-setting-texts 'insert (caddr im))))
                 (else (lost-setting-texts kind t))))
          ((in? kind '(insert assign)) (content-items t))
          ((== kind 'remove)
           (cons "Deleted:" (content-items (caddr im))))
          ((== kind 'split)
           (list (string-append "A paragraph was split at " where)))
          ((== kind 'join)
           (list (string-append "Two paragraphs were joined at " where)))
          ((in? kind '(assign-node insert-node remove-node))
           (list (string-append "Change of structure (" (symbol->string kind)
                                ") at " where
                                (if (string? t) "" (string-append ": " (object->string t))))))
          (else '()))))

(define (lost-contents patches)
  (append-map (lambda (p) (append-map lost-items (patch-leaves p))) patches))

(define lost-keep-days 30)
(define lost-keep-files 50)

(define (prune-lost-files! dir)
  ;; the documents of lost changes are kept 30 days, and the last 50
  (let* ((now (current-time))
         (names (list-filter (list-dir-names dir)
                             (lambda (n) (and (string-starts? n "shared-lost-")
                                              (string-ends? n ".tm")))))
         (stamp (lambda (n)
                  (or (string->number (list-ref (string-split n #\-) 2)) 0)))
         (sorted (sort names (lambda (a b) (< (stamp a) (stamp b))))))
    (let loop ((l sorted) (n (length sorted)))
      (when (nnull? l)
        (when (or (> n lost-keep-files)
                  (> (- now (stamp (car l))) (* lost-keep-days 86400)))
          (system-remove (url-append (url->url dir) (car l))))
        (loop (cdr l) (- n 1))))))

;; (the whole document of the buffer, as it is, in the recovered folder)
(define (save-recovered-copy! sf)
  (set! sf-serial (+ sf-serial 1))
  (let* ((dir (url->url "$TEXMACS_HOME_PATH/system/recovered"))
         (file (url-append dir (string-append
                                "shared-lost-" (number->string (current-time)) "-"
                                (number->string (shared-process-id)) "-"
                                (number->string sf-serial) "-document.tm"))))
    (and (catch #t
           (lambda () (system-mkdir dir)
             (not (buffer-export (sf-buffer sf) file "texmacs")))
           (lambda args #f))
         file)))

(tm-define (shared-test-reset! name body)
  (:synopsis "For the tests: a document of the host (this body) replaces this one")
  (and-with sf (sf-by-name name)
    (participant-reset! sf (live-current-state (sf-key sf))
                        `(shared ,(tm->stree (sf-meta sf)) ,body) #f)))

(define (report-lost! sf patches)
  (when (nnull? patches)
    (stat! 'lost)
    (let* ((items (lost-contents patches))
           (name (url->system (sf-buffer sf)))
           (msg (string-append "Shared file: changes of yours to " name
                               " could not be combined with those of another process")))
      (cond ((null? items)
             (set-message `(concat ,msg ".") "Shared file"))
            (else
              (set! sf-serial (+ sf-serial 1))
              (let* ((dir (url->url "$TEXMACS_HOME_PATH/system/recovered"))
                     (file (url-append dir (string-append
                                            "shared-lost-"
                                            (number->string (current-time)) "-"
                                            (number->string (shared-process-id)) "-"
                                            (number->string sf-serial) ".tm")))
                     (doc `(document (TeXmacs "2.1.5") (style (tuple "generic"))
                             (body (document
                                     ,(string-append "Changes of yours to " name
                                                     " which could not be kept:")
                                     ,@items))))
                     (ok (catch #t
                           (lambda ()
                             (system-mkdir dir)
                             (catch #t (lambda () (prune-lost-files! (url->system dir)))
                               (lambda args #f))
                             (not (not (tree-export (stree->tree doc) file "texmacs"))))
                           (lambda args #f))))
                (stat! 'lost-saved)
                (set-message `(concat ,msg ". "
                                      ,(if ok
                                           `(concat "What you typed is saved in "
                                                    (verbatim ,(url->system file)))
                                           "It could not be saved."))
                             "Shared file")
                (when (and ok (not (headless?)))
                  (user-confirm "Changes of yours to a shared file could not be kept. Open the document with what you typed?" #t
                    (lambda (answ)
                      (when answ (load-buffer file))
                      (noop))))))))))

(define (rollback-pending! sf)
  ;; undo the pending changes in the buffer: it becomes the confirmed
  ;; document. Gives #f if one of the inverse patches could not be applied
  ;; (then the document must be asked from the host).
  (let* ((key (sf-key sf)) (pend (sf-get sf 'pending)) (ok #t))
    (when (nnull? pend)
      (let loop ((l pend) (t (live-current-document key)) (invs '()))
        (if (null? l)
            (for (inv invs)
              (when (not (apply-in-place! sf inv (author-of sf (sf-get sf 'id))))
                (set! ok #f)))
            (loop (cdr l) (patch-apply t (car l))
                  (cons (patch-invert (car l) t) invs))))
      (sf-put! sf 'pending '())
      (sf-put! sf 'inflight #f))
    ok))

(define (participant-reset! sf state doc modified)
  ;; the document of the host replaces everything, pending changes included
  (let* ((key (sf-key sf)) (name (sf-buffer sf)))
    ;; The document of this process is replaced as a whole, never combined
    ;; with the new one (that could revert newer changes of the host). When
    ;; it is not the same, it is first saved in the recovered folder.
    (when (!= (sf-composite-stree sf) doc)
      (let ((file (save-recovered-copy! sf)))
        (stat! 'replaced)
        (set-message
         `(concat "Shared file: the document of the host replaces yours."
                  ,(if file
                       `(concat " Yours is saved in " (verbatim ,(url->system file)))
                       ""))
         "Shared file")))
    (report-lost! sf (sf-get sf 'pending))
    (sf-put! sf 'pending '())
    (sf-put! sf 'inflight #f)
    (sf-put! sf 'collect '())
    ;; (doc is the shared tree: (shared META BODY))
    (with-global sf-applying? #t
      (buffer-set-body name (stree->tree (caddr doc)))
      (sf-put! sf 'meta (stree->tree (cadr doc)))
      (meta-install! sf))
    (live-create key (sf-composite sf) state)
    (if modified (buffer-pretend-modified name) (buffer-pretend-saved name))))

(define (participant-apply-remote! sf r from to)
  ;; a patch of the host, against the confirmed state @from
  (let* ((key (sf-key sf))
         (d0 (live-current-document key))
         (pend (sf-get sf 'pending)))
    (cond ((null? pend)
           (when (apply-in-place! sf r (author-of sf (sf-get sf 'host-id)))
             (live-apply-patch key r to)
             (sf-notify-modified! sf)
             (sf-trim! key)))
          (else
            (stat! 'transformed)
            ;; The pending changes go on top of the patch of the host, as
            ;; the host will rebase them (and one with a tie: its own
            ;; patch is placed first): take them back, apply the patch,
            ;; put them back as far as they can be. If a step fails the
            ;; changes are reported and the document asked from the host.
            (let* ((kept (transform-pending pend (patch-invert r d0)))
                   (inflight (sf-get sf 'inflight)))
              (cond
                ((not (rollback-pending! sf))
                 (report-lost! sf pend)
                 (participant-request-resync! sf))
                ((not (apply-in-place! sf r (author-of sf (sf-get sf 'host-id))))
                 (report-lost! sf pend)
                 (participant-request-resync! sf))
                (else
                  (live-apply-patch key r to)
                  (sf-notify-modified! sf)
                  (let ((ok (let loop ((l kept) (done '()))
                              (cond ((null? l) (reverse done))
                                    ((apply-in-place! sf (car l) (author-of sf (sf-get sf 'id)))
                                     (loop (cdr l) (cons (car l) done)))
                                    (else (reverse done))))))
                    (when (< (length ok) (length pend))
                      (stat! 'dropped)
                      (report-lost! sf (list-tail pend (length ok))))
                    (sf-put! sf 'pending ok)
                    ;; (the patch which was sent is still on its way if it
                    ;; is kept: its confirmation is recognised by the id)
                    (sf-put! sf 'inflight (and (nnull? ok) inflight))
                    (sf-trim! key)))))))))

(define (participant-request-resync! sf)
  (stat! 'resync)
  (sf-debug "resync\n")
  (when (sf-get sf 'conn)
    (msg-send (sf-get sf 'conn) (list 'resync))))

(define (participant-handle-message sf msg)
  (let ((key (sf-key sf)))
    (cond ((func? msg 'applied)
           (with (tag from to author id psexp) msg
             (seen! sf id)
             (cond ((!= from (live-current-state key))
                    (participant-request-resync! sf))
                   ((and id (== id (sf-get sf 'inflight))
                         (== author (sf-get sf 'id)))
                    ;; the host took my patch: it is confirmed
                    (live-apply-patch key (scheme->patch psexp) to)
                    (sf-put! sf 'pending (if (nnull? (sf-get sf 'pending)) (cdr (sf-get sf 'pending)) '()))
                    (sf-put! sf 'inflight #f)
                    (sf-trim! key))
                   (else
                     (participant-apply-remote! sf (scheme->patch psexp)
                                                from to)))))
          ((func? msg 'refused)
           (with (tag id cur) msg
             (when (== id (sf-get sf 'inflight))
               (stat! 'rolled-back)
               (sf-debug "my patch was refused\n")
               (report-lost! sf (sf-get sf 'pending))
               (when (not (rollback-pending! sf))
                 (participant-request-resync! sf))
               (when (!= cur (live-current-state key))
                 (participant-request-resync! sf)))))
          ((func? msg 'ack)
           ;; the host had the patch already: the document is asked
           (when (== (cadr msg) (sf-get sf 'inflight))
             (participant-request-resync! sf)))
          ((func? msg 'roster)
           (sf-put! sf 'roster (cadr msg)))
          ((func? msg 'saved)
           ;; (the time of the last save is the time of the file; the buffer
           ;; stays modified if there are changes the host has not got)
           (buffer-pretend-saved (sf-buffer sf))
           (when (or (sf-get sf 'inflight) (nnull? (sf-get sf 'pending)))
             (buffer-pretend-modified (sf-buffer sf)))
           (sf-put! sf 'dirty #f))
          ((func? msg 'welcome-reset)
           (with (tag state doc modified roster host-id) msg
             (sf-put! sf 'roster roster)
             (sf-put! sf 'host-id host-id)
             (participant-reset! sf state doc modified)))
          ((func? msg 'released)
           (with (tag token pref roster) msg
             (sf-put! sf 'roster roster)
             (participant-seek! sf token pref))))))

(define (participant-tick sf)
  (participant-flush! sf)
  (with c (sf-get sf 'conn)
    (cond ((sf-get sf 'seeking) (seek-step sf))
          ((and c (not (sf-get sf 'reconnecting)))
           (let loop ()
             (with m (msg-receive c)
               (when (and m (== (sf-get sf 'conn) c))
                 (participant-handle-message sf m)
                 (loop))))
           (cond ((not (sf-get sf 'conn)) (noop))
                 ((not (local-alive? c)) (participant-seek! sf (sf-get sf 'host-token) #f))
                 (else
                   (participant-submit! sf)
                   (participant-send-save-when-clean! sf))))
          ((sf-get sf 'reconnecting) (reconnect-step sf)))))

(define (participant-send-save-when-clean! sf)
  ;; a Save waits until the host has all the changes of this process
  (when (and (sf-get sf 'save-wanted) (sf-get sf 'conn)
             (null? (sf-get sf 'pending)) (not (sf-get sf 'inflight)))
    (sf-put! sf 'save-wanted #f)
    (msg-send (sf-get sf 'conn) (list 'save))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the host: release, seeking, a host which is gone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hosting is holding the lock of the file. A host which gives it up (the file
;; is closed, or an interactive process arrived and the host is not
;; interactive) sends all its changes, then (released token preferred roster)
;; to all, closes its links and frees the lock; from then on it is a
;; participant. A host which dies frees the lock by dying. In both cases the
;; participants do the same: each tries to get the lock, the preferred one
;; (the first by priority, interactive first) at once, the next ones 300 ms
;; later each, this process last; the one that gets it hosts (it keeps its
;; pending changes as new states), and the others read its new entry (not the
;; old one: its token is not that of the host they had) and connect to it.
;; Whoever gets the lock, no time decides who hosts: the lock does, so that
;; there is never a moment with two hosts. If nobody has the lock yet, the
;; participants only queue their changes; a preferred process that stalls only
;; delays hosting. A host which does not answer and is alive keeps the lock:
;; it is never replaced, the participants say that it does not answer.

(define (peer->roster p)
  (list (peer-id p) (peer-interactive? p) (peer-seq p) (peer-pid p)))

(define (host-release! sf leaving?)
  (host-flush! sf)
  (let* ((key (sf-key sf)) (peers (sf-get sf 'peers))
         (cands (by-priority (map peer->roster peers)))
         (pref (and (nnull? cands) (car (car cands)))))
    (send-peers sf (list 'released (sf-get sf 'token) pref (roster-of sf)))
    (for (p peers) (local-close (peer-link p)))
    (sf-put! sf 'peers '())
    (when (null? peers) (remove-entry key))
    (when (sf-get sf 'lock)
      (shared-lock-release (sf-get sf 'lock))
      (sf-put! sf 'lock #f))
    (cond (leaving? (sf-destroy! sf))
          (else
            (sf-put! sf 'role 'participant)
            (sf-put! sf 'roster (roster-of sf))
            (sf-put! sf 'host-id (sf-get sf 'id))
            (participant-seek! sf (sf-get sf 'token) pref)))))

(define (host-offer-handover! sf)
  ;; a non-interactive host with an interactive participant gives it the file
  (when (and (not (sf-get sf 'interactive))
             (list-or (map peer-interactive? (sf-get sf 'peers))))
    (host-release! sf #f)))

(define (participant-seek! sf old-token pref)
  ;; the host is gone: try the lock, in the order of the preference
  (let ((c (sf-get sf 'conn)))
    (when c (local-close c)))
  (sf-put! sf 'conn #f)
  (sf-put! sf 'reconnecting #f)
  (when (sf-get sf 'inflight) (sf-put! sf 'last-inflight (sf-get sf 'inflight)))
  (sf-put! sf 'inflight #f)
  (let* ((old-id (sf-get sf 'host-id))
         (ids (map car (by-priority
                        (list-filter (sf-get sf 'roster)
                                     (lambda (r) (!= (car r) old-id))))))
         (order (if pref (cons pref (list-remove ids pref)) ids))
         (me (sf-get sf 'id))
         (rank (let loop ((l order) (i 0))
                 (cond ((null? l) i)
                       ((== (car l) me) i)
                       (else (loop (cdr l) (+ i 1)))))))
    (sf-debug "seeking after " old-id ", rank " rank "\n")
    (sf-put! sf 'seeking (list old-token (+ (texmacs-time) (* 300 rank))))))

(define (seek-step sf)
  (let* ((s (sf-get sf 'seeking)) (old-token (car s)) (at (cadr s)))
    (when (>= (texmacs-time) at)
      (let ((h (lock-acquire (sf-key sf))))
        (cond (h (when (not (become-host! sf h))
                   (sf-put! sf 'seeking (list old-token (+ (texmacs-time) 1000)))))
              (else
                (let ((e (read-entry (sf-key sf))))
                  (if (and e (not (and old-token (token=? (cadddr e) old-token))))
                      (begin
                        (sf-put! sf 'seeking #f)
                        (participant-reconnect! sf (caddr e) (cadddr e)
                                                (list-ref e 6)))
                      (sf-put! sf 'seeking
                               (list old-token (+ (texmacs-time) 200)))))))))))

(define (become-host! sf h)
  ;; this process hosts the file from now on, @h being the lock it has just
  ;; got: its document is the confirmed one, to which its pending changes are
  ;; added as new states. #f if it can not listen (the lock is given back).
  (let ((port (local-listen)) (token (shared-random-token)))
    (cond ((not (> port 0)) (shared-lock-release h) #f)
          (else
            (become-host!* sf h port token)
            #t))))

(define (become-host!* sf h port token)
  (let* ((key (sf-key sf)))
    (participant-flush! sf)
    (let ((pend (sf-get sf 'pending)))
      (sf-put! sf 'pending '())
      (sf-put! sf 'inflight #f)
      (sf-put! sf 'role 'host)
      (sf-put! sf 'lock h)
      (sf-put! sf 'conn #f)
      (sf-put! sf 'reconnecting #f)
      (sf-put! sf 'seeking #f)
      (sf-put! sf 'peers '())
      (sf-put! sf 'token token)
      (sf-put! sf 'host-token token)
      (sf-put! sf 'host-id (sf-get sf 'id))
      (sf-put! sf 'offer-at (+ (texmacs-time) 5000))
      (sf-put! sf 'seq-counter
               (apply max (cons 0 (map caddr (sf-get sf 'roster)))))
      (let loop ((l pend))
        (when (nnull? l)
          (if (live-apply-patch key (car l))
              (loop (cdr l))
              (live-create key (sf-composite sf)))))
      (when (nnull? pend) (sf-put! sf 'dirty #t))
      (host-write-entry! sf)
      (sf-put! sf 'touch-at (+ (texmacs-time) 3600000))
      (reconnect-ok! sf)
      (sf-debug "now hosting " key " on " port "\n")
      ;; a Save asked in the meantime is done here now
      (when (sf-get sf 'save-wanted)
        (sf-put! sf 'save-wanted #f)
        (host-save sf)))))

(define (participant-reconnect! sf port token host-id)
  ;; the host is somewhere: connect to it
  (let ((old (sf-get sf 'conn)))
    (when old (local-close old))
    (let ((c (local-connect port)))
      (cond ((>= c 0)
             (sf-put! sf 'conn c)
             (sf-put! sf 'host-id host-id)
             (sf-put! sf 'host-token token)
             (sf-put! sf 'inflight #f)
             (sf-put! sf 'reconnecting (list c (+ (texmacs-time) 2500)))
             ;; (the last argument: the patch that was on its way to the old
             ;; host: if that host applied it, this is not told to its author,
             ;; and the new host sends the whole document, not a patch which
             ;; would put the change in twice)
             (msg-send c (list 'hello token (sf-key sf) (sf-get sf 'id)
                               (shared-process-id) (sf-get sf 'interactive)
                               (live-current-state (sf-key sf))
                               (sf-get sf 'last-inflight))))
            (else (participant-reconnect-failed! sf))))))

(define (participant-reconnect-failed! sf)
  ;; the host holds the lock but is not reached (busy, stopped, starting):
  ;; try again, once a second, keeping the changes
  (let ((n (+ 1 (or (sf-get sf 'reconnect-fails) 0))))
    (sf-put! sf 'reconnect-fails n)
    (when (== n 2)
      (stat! 'host-silent)
      (set-message "Shared file: the host of this file is not answering; your changes are kept, and sent when it does"
                   "Shared file")))
  (let ((c (sf-get sf 'conn)))
    (when c (local-close c)))
  (sf-put! sf 'conn #f)
  (sf-put! sf 'reconnecting #f)
  (when (sf-get sf 'inflight) (sf-put! sf 'last-inflight (sf-get sf 'inflight)))
  (sf-put! sf 'inflight #f)
  (sf-put! sf 'seeking (list #f (+ (texmacs-time) 1000))))

(define (reconnect-ok! sf)
  (sf-put! sf 'last-inflight #f)
  (sf-put! sf 'reconnect-fails 0))

(define (reconnect-step sf)
  ;; waiting for the answer to the hello sent to the host
  (let* ((r (sf-get sf 'reconnecting)) (c (car r)))
    (let loop ()
      (with m (msg-receive c)
        (cond (m
                (cond ((func? m 'welcome-same)
                       (reconnect-ok! sf)
                       (sf-put! sf 'roster (caddr m))
                       (sf-put! sf 'reconnecting #f))
                      ((func? m 'welcome-catchup)
                       (reconnect-ok! sf)
                       (with (tag from to psexp roster host-id) m
                         (sf-put! sf 'roster roster)
                         (sf-put! sf 'reconnecting #f)
                         (participant-apply-remote! sf (scheme->patch psexp)
                                                    from to)))
                      ((func? m 'welcome-reset)
                       (reconnect-ok! sf)
                       (with (tag state doc modified roster host-id) m
                         (sf-put! sf 'roster roster)
                         (sf-put! sf 'reconnecting #f)
                         (participant-reset! sf state doc modified)))
                      (else (loop))))
              ((not (local-alive? c))
               ;; (closed: that host is not the host, or has just released)
               (participant-reconnect-failed! sf))
              ((> (texmacs-time) (cadr r))
               (participant-reconnect-failed! sf)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leaving a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (shared-file-leave name)
  (:synopsis "Called before the buffer @name is closed (or renamed)")
  (and-with sf (sf-by-name name)
    (cond ((sf-host? sf) (leave-as-host sf))
          (else (leave-as-participant sf)))))

(define (leave-as-participant sf)
  (let ((c (sf-get sf 'conn)))
    (participant-flush! sf)
    ;; the changes the host has not got are sent first (for a moment at
    ;; most); those which still could not be are kept in a document
    (let ((end (+ (texmacs-time) 1000)))
      (let loop ()
        (when (and c (local-alive? c)
                   (or (nnull? (sf-get sf 'pending)) (sf-get sf 'inflight))
                   (< (texmacs-time) end))
          (participant-tick sf)
          (loop))))
    (report-lost! sf (sf-get sf 'pending))
    ;; with no connection to a host (it is being sought, or does not answer)
    ;; what the person has here may be on no disk and in no host: the whole
    ;; document is saved, with a notice, which also covers a queued Save
    (when (and (not (and c (local-alive? c)))
               (or (buffer-modified? (sf-buffer sf))
                   (nnull? (sf-get sf 'pending)) (sf-get sf 'inflight)
                   (sf-get sf 'save-wanted)))
      (let ((file (save-recovered-copy! sf)))
        (stat! 'closed-unconnected)
        (set-message
         `(concat "Shared file: the host of this file was not reachable when it was closed."
                  ,(if file
                       `(concat " Your document is saved in " (verbatim ,(url->system file)))
                       " It could not be saved."))
         "Shared file")))
    (when c
      (msg-send c (list 'leave))
      (local-close c))
    (sf-destroy! sf)))

(define (leave-as-host sf)
  ;; (no waiting: the lock is freed, and the first participant by priority
  ;; takes it)
  (host-release! sf #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The polling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accept-new-links)
  ;; (a few links a tick at most)
  (let loop ((n 0))
    (let ((c (if (< n 4) (local-accept) -1)))
      (when (>= c 0)
        ;; (not too many links which have not said who they are)
        (if (< (length sf-incoming) 16)
            (set! sf-incoming (cons (cons c (texmacs-time)) sf-incoming))
            (local-close c))
        (loop (+ n 1)))))
  (set! sf-incoming
        (list-filter
         sf-incoming
         (lambda (e)
           (let* ((c (car e)) (m (msg-receive c)))
             (cond ((and m (func? m 'hello) (== (length m) 8))
                    (identify-link c m)
                    #f)
                   (m (local-close c) #f)
                   ((not (local-alive? c)) (local-close c) #f)
                   ((> (- (texmacs-time) (cdr e)) 5000) (local-close c) #f)
                   (else #t)))))))

(define (identify-link c m)
  ;; the first packet of a link: it must have the token of the file, else
  ;; the link is closed
  (with (tag token key id pid interactive base inflight) m
    (let ((sf (ahash-ref sf-table key)))
      (cond ((and sf (sf-host? sf) (token=? token (sf-get sf 'token)))
             (local-set-limit c 67108864)
             (host-accept-hello sf c id pid interactive base inflight))
            (else (local-close c))))))

(define (sf-tick sf)
  (catch #t
    (lambda ()
      (cond ((sf-host? sf) (host-tick sf))
            (else (participant-tick sf))))
    (lambda args
      (display* "shared file error: " args "\n"))))

(define (shared-tick)
  (when (not sf-paused?)
    (catch #t
      (lambda () (accept-new-links))
      (lambda args (display* "shared file error: " args "\n")))
    (catch #t
      (lambda () (pending-poll!))
      (lambda args (display* "shared file error: " args "\n")))
    (for (sf (map cdr (ahash-table->list sf-table)))
      (when (ahash-ref sf-table (sf-key sf))
        (sf-tick sf))))
  (if (and (== (ahash-size sf-table) 0) (== (ahash-size sf-pending) 0))
      (begin (set! sf-tick-running? #f) #t)
      20))

(define (shared-ensure-tick!)
  (when (not sf-tick-running?)
    (set! sf-tick-running? #t)
    (exec-delayed-pause shared-tick)))

(tm-define (shared-poll)
  (:synopsis "For the tests: do what the polling does, now")
  (shared-tick))

;; A buffer which is removed without buffer-close (the window and the buffer
;; killed together, a buffer renamed): the hook of the C++ code leaves the
;; sharing, the buffer still exists
(tm-define (notify-buffer-closed name)
  (former name)
  (ahash-remove! sf-unshared name)
  (when (shared-files-in-use?)
    (shared-file-leave (string->url name))))
