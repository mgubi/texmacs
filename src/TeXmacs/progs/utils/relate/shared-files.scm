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

(define (sf-new-id)
  (set! sf-serial (+ sf-serial 1))
  (string-append (number->string (shared-process-id)) "."
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

;; An entry is the file f-<hash of the canonical path>.entry, containing
;; (entry key port token pid interactive host-id): created whole and replaced
;; atomically, so that a reader never sees half of it.

(define (entry-path key)
  (with dir (shared-user-dir)
    (and (!= dir "")
         (string-append dir "/f-" (tree-hash (tm->tree key)) ".entry"))))

(tm-define (shared-entry name)
  (:synopsis "The entry of the file @name in the per-user folder, or #f")
  (and-with key (shared-real-path (url->system name))
    (read-entry key)))

(tm-define (shared-entry-path name)
  (:synopsis "The file of the entry of @name in the per-user folder")
  (and-with key (shared-real-path (url->system name))
    (entry-path key)))

(define (read-entry key)
  (and-with path (entry-path key)
    (with u (string->url path)
      (and (url-exists? u)
           (with e (catch #t
                     (lambda () (string->object (string-load u)))
                     (lambda args #f))
             (cond ((and (func? e 'entry) (== (length e) 7))
                    (and (== (cadr e) key) e))
                   (else
                     ;; empty or damaged (a create that did not end): when
                     ;; it is not new any more it is stale
                     (when (> (- (current-time) (url-last-modified u)) 5)
                       (system-remove u))
                     #f)))))))

(define (remove-entry-if key token)
  ;; (only the entry which has the token: not one made meanwhile)
  (and-with e (read-entry key)
    (when (== (cadddr e) token)
      (system-remove (string->url (entry-path key))))))

(define (write-entry key port token interactive host-id)
  (and-with path (entry-path key)
    (shared-replace-file
     path (object->string (list 'entry key port token (shared-process-id)
                                interactive host-id)))))

(define (create-entry key port token interactive host-id)
  (and-with path (entry-path key)
    (shared-create-file
     path (object->string (list 'entry key port token (shared-process-id)
                                interactive host-id)))))

(define (list-dir-names dir)
  ;; (a tuple of strings comes as the list of the strings)
  (with l (shared-list-dir dir)
    (if (list? l)
        (map (lambda (n) (if (symbol? n) (symbol->string n) n)) l)
        '())))

(define (clean-stale-entries)
  ;; entries of processes which are gone
  (with dir (shared-user-dir)
    (when (!= dir "")
      (for (name (list-dir-names dir))
        (when (and (string? name) (string-starts? name "f-")
                   (string-ends? name ".entry"))
          (let* ((u (string->url (string-append dir "/" name)))
                 (e (catch #t
                      (lambda () (string->object (string-load u)))
                      (lambda args #f))))
            (when (and (func? e 'entry) (== (length e) 7)
                       (not (shared-process-alive? (list-ref e 4))))
              (system-remove u))))))))

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
    (sf-put! sf 'collect '())
    (sf-put! sf 'pending '())     ; local patches not yet in the host's states
    (sf-put! sf 'inflight #f)     ; id of the first of them, sent
    (sf-put! sf 'peers '())       ; host: (id link interactive seq pid)
    (sf-put! sf 'roster '())      ; participant: (id interactive seq pid)
    (sf-put! sf 'seq-counter 0)
    (sf-put! sf 'dirty #f)
    (sf-put! sf 'conn #f)         ; participant: the link to the host
    (sf-put! sf 'host-id #f)
    (sf-put! sf 'handover #f)
    (sf-put! sf 'election #f)
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
  (let ((c (local-connect (caddr entry))))
    (and (>= c 0)
         (begin
           (msg-send c (list 'hello (cadddr entry) key id (shared-process-id)
                             interactive base))
           c))))

(define sf-join-wait 500)   ; ms: the most a join makes the interface wait
(define sf-unshared (make-ahash-table))  ; names opened without sharing

(define (note-unshared! name)
  ;; the host of the file is alive but did not answer in time: the file is
  ;; read as usual, not shared (never a second host: both would write it)
  (ahash-set! sf-unshared (url->string name) (texmacs-time))
  (set-message "Shared file: the process which has this file open did not answer; it is opened from the file, without sharing"
               "Shared file"))

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
                     (with r (join-host name key entry)
                       (when (== r 'alive) (note-unshared! name))
                       (== r #t))))))))

;; (join-host with a buffer which exists, read from the file, replaces it)
(define (join-host name key entry)
  (let* ((id (sf-new-id)) (inter (shared-interactive?))
         (c (do-hello key entry id inter #f))
         (m (and c (msg-wait c sf-join-wait))))
    ;; gives #t (joined), 'alive (the host is alive but did not answer: do
    ;; not take its place) or #f (its entry is stale, and was removed)
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
               (sf-put! sf 'roster roster)
               (live-create key (sf-composite sf) state)
               (if modified
                   (buffer-pretend-modified name)
                   (buffer-pretend-saved name))
               (sf-debug "joined " key " state " state "\n")
               #t)))
          (else
            ;; (a connection that is refused: nobody serves the file there, the
            ;; process is gone or is another program with the same pid; one
            ;; that is made but not answered: the host is busy or stopped)
            (cond (c (local-close c) 'alive)
                  (else (remove-entry-if key (cadddr entry)) #f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hosting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (shared-file-opened name)
  (:synopsis "Called when the buffer @name has been loaded from its file")
  ;; The process hosts the file, unless a host appeared meanwhile (then the
  ;; process joins it, replacing what it has read).
  (cond ((let ((t (ahash-ref sf-unshared (url->string name))))
           (ahash-remove! sf-unshared (url->string name))
           (and t (< (- (texmacs-time) t) 10000)))
         (noop))
        ((and (shared-files-on?) (not (sf-by-name name)))
         (and-with key (shareable-key name)
           (when (not (ahash-ref sf-table key))
             (host-start name key))))))

(define (host-start name key)
  (let ((port (local-listen)))
    (when (> port 0)
      (clean-stale-entries)
      (let* ((token (shared-random-token))
             (sf (sf-make name key 'host)))
        (sf-put! sf 'token token)
        (sf-put! sf 'host-id (sf-get sf 'id))
        (live-create key (sf-composite sf))
        (if (create-entry key port token (sf-get sf 'interactive)
                          (sf-get sf 'id))
            (sf-debug "hosting " key " on " port "\n")
            ;; an entry exists: of a live host (join it), or a stale one
            (let ((e (read-entry key)))
              (sf-destroy! sf)
              (if (not e)
                  ;; (an unreadable entry that was old has just been removed)
                  (when (not (and-with p (entry-path key) (url-exists? (string->url p))))
                    (host-start name key))
                  (with r (join-host name key e)
                  (cond ((== r #t) #t)
                        ((== r 'alive) (note-unshared! name))
                        ;; (join-host removed the stale entry)
                        (else (host-start name key)))))))))))

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
  (let* ((key (sf-key sf))
         (cur (live-current-state key))
         (p0 (patch-remove-set-cursor (scheme->patch psexp)))
         (p (cond ((== base cur) p0)
                  ((in? base (live-get-history key))
                   (with r (rebase-over-history key base p0)
                     (when r (stat! 'rebased))
                     r))
                  (else #f))))
    (cond ((and p (patch-applicable? p (live-current-document key))
                (apply-in-place! sf p (author-of sf (peer-id peer))))
           ;; (applied to the buffer first: if that fails nothing moves)
           (let ((to (live-apply-patch key p)))
             (sf-notify-modified! sf)
             (cond (to
                     (send-peers sf (list 'applied cur to (peer-id peer) id
                                          (patch->scheme p)))
                     (sf-trim! key))
                   (else (host-reset-live! sf)))))
          (else
            (stat! 'refused)
            (sf-debug "refused a patch of " (peer-id peer) " from " base "\n")
            (msg-send (peer-link peer) (list 'refused id cur))))))

(define (host-send-welcome sf link base id interactive pid)
  ;; the answer to a hello: the document, or what the other lacks
  (let* ((key (sf-key sf))
         (cur (live-current-state key))
         (name (sf-buffer sf)))
    (cond ((not base)
           (msg-send link (list 'welcome cur (tm->stree (buffer-get name))
                                (or (sf-get sf 'dirty) (buffer-modified? name))
                                (roster-of sf) (sf-get sf 'id)
                                (tm->stree (sf-meta sf)))))
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

(define (host-accept-hello sf link id pid interactive base)
  (host-flush! sf)
  (if (not (local-alive? link))
      (local-close link)
      (host-accept-hello* sf link id pid interactive base)))

(define (host-accept-hello* sf link id pid interactive base)
  (let ((seq (+ (sf-get sf 'seq-counter) 1)))
    (sf-put! sf 'seq-counter seq)
    (sf-put! sf 'peers
             (append (list-filter (sf-get sf 'peers)
                                  (lambda (p) (!= (peer-id p) id)))
                     (list (list id link interactive seq pid))))
    (host-send-welcome sf link base id interactive pid)
    (send-roster sf)
    ;; an interactive process always hosts: hosting moves to it
    (when (and interactive (not (sf-get sf 'interactive))
               (not (sf-get sf 'handover)))
      (host-start-handover sf id #f))))

(define (host-handle-message sf peer msg)
  (cond ((func? msg 'patch)
         (when (not (sf-get sf 'handover))
           (with (tag id base p) msg
             (host-handle-patch sf peer id base p))))
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
        ((func? msg 'host-ready)
         (host-finish-handover sf peer (cadr msg) (caddr msg)))
        ((func? msg 'host-failed)
         (sf-debug "handover refused\n")
         (sf-put! sf 'handover #f))))

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
  ;; a handover which is not answered
  (and-with h (sf-get sf 'handover)
    (when (> (texmacs-time) (cadr h))
      (sf-debug "handover timed out\n")
      (sf-put! sf 'handover #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         (sf-get sf 'conn)
         (begin
           (sf-put! sf 'save-wanted #t)
           (participant-flush! sf)
           (participant-submit! sf)
           (participant-send-save-when-clean! sf)
           (set-message "Save asked to the host of the file" "Save file")
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
             (not (sf-get sf 'election)))
    (let* ((n (+ (sf-get sf 'msg-id) 1))
           (id (string-append (sf-get sf 'id) "/" (number->string n))))
      (sf-put! sf 'msg-id n)
      (stat! 'submitted)
      (sf-put! sf 'inflight id)
      (msg-send (sf-get sf 'conn)
                (list 'patch id
                      (if (sf-get sf 'bad-base) "no such state"
                          (live-current-state (sf-key sf)))
                      (patch->scheme (car (sf-get sf 'pending))))))))

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
          ((func? msg 'become-host)
           (participant-become-host-on-request sf (cadr msg)))
          ((func? msg 'rehost)
           (with (tag port token host-id) msg
             (participant-reconnect! sf port token host-id)))
          ((func? msg 'bye)
           (participant-host-gone! sf)))))

(define (participant-tick sf)
  (participant-flush! sf)
  (with c (sf-get sf 'conn)
    (cond ((sf-get sf 'election) (election-step sf))
          ((and c (not (sf-get sf 'reconnecting)))
           (let loop ()
             (with m (msg-receive c)
               (when (and m (== (sf-get sf 'conn) c))
                 (participant-handle-message sf m)
                 (loop))))
           (cond ((not (sf-get sf 'conn)) (noop))
                 ((not (local-alive? c)) (participant-host-gone! sf))
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
;; Changing the host: handover, rehosting, a host which is gone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handover (the host gives up its role: the file is closed, or an
;; interactive process arrived and the host is not interactive):
;; 1. the host sends all its changes, then (become-host state) to the
;;    successor and goes on only to hear its answer: it ignores the patches
;;    of the others from now on (they are sent again to the new host);
;; 2. the successor, whose state is that of the host, becomes the host
;;    (listens, writes the entry) and answers (host-ready port token);
;; 3. the host sends (rehost port token id) to all the others, which
;;    connect to the new host and send their pending patches again; the
;;    host itself does so too, unless it is closing.

(define (host-start-handover sf target-id leaving?)
  (host-flush! sf)
  (and-with peer (find-peer sf target-id)
    (sf-put! sf 'handover (list target-id (+ (texmacs-time) (if leaving? 1500 4000)) leaving?))
    (msg-send (peer-link peer)
              (list 'become-host (live-current-state (sf-key sf))))))

(define (host-finish-handover sf peer port token)
  (and-with h (sf-get sf 'handover)
    (let ((target-id (car h)) (leaving? (caddr h)))
      (when (== (peer-id peer) target-id)
        (for (p (sf-get sf 'peers))
          (when (!= (peer-id p) target-id)
            (msg-send (peer-link p) (list 'rehost port token target-id))))
        (sf-put! sf 'handover #f)
        (if leaving?
            (sf-put! sf 'handed-over #t)
            ;; stay as a participant of the new host
            (let ((c (local-connect port)))
              (when (>= c 0)
                (for (p (sf-get sf 'peers)) (local-close (peer-link p)))
                (sf-put! sf 'peers '())
                (sf-put! sf 'role 'participant)
                (sf-put! sf 'conn c)
                (sf-put! sf 'host-id target-id)
                (sf-put! sf 'reconnecting
                         (list c (+ (texmacs-time) 4000)))
                (msg-send c (list 'hello token (sf-key sf) (sf-get sf 'id)
                                  (shared-process-id) (sf-get sf 'interactive)
                                  (live-current-state (sf-key sf)))))))))))

(define (participant-become-host-on-request sf state)
  (let ((old (sf-get sf 'conn)))
    (cond ((and (== state (live-current-state (sf-key sf)))
                (> (local-listen) 0))
           (let ((token (become-host! sf #t)))
             (when old
               (msg-send old (list 'host-ready (local-listen) token))
               (local-close old))))
          (else (when old (msg-send old (list 'host-failed)))))))

;; The entry of another process that serves the file: its process is alive
;; and its port accepts connections (one that is refused is not served: the
;; process is gone, or is another program with the same pid, and the entry is
;; removed). Such an entry is never taken over.
(define (serving-entry key)
  (let ((e (read-entry key)))
    (and e (!= (list-ref e 4) (shared-process-id))
         (shared-process-alive? (list-ref e 4))
         (let ((c (local-connect (caddr e))))
           (cond ((>= c 0) (local-close c) e)
                 (else (remove-entry-if key (cadddr e)) #f))))))

(define (become-host! sf . handover?)
  ;; this process hosts the file from now on: its document is the confirmed
  ;; one, to which its pending changes are added as new states. Unless the
  ;; host hands over to it, it does not when another process serves the file
  ;; (then #f): two hosts would write the same file.
  (if (and (null? handover?) (serving-entry (sf-key sf)))
      #f
      (become-host!* sf)))

(define (become-host!* sf)
  (let* ((key (sf-key sf)) (port (local-listen))
         (token (shared-random-token)))
    (participant-flush! sf)
    (let ((pend (sf-get sf 'pending)))
      (sf-put! sf 'pending '())
      (sf-put! sf 'inflight #f)
      (sf-put! sf 'role 'host)
      (sf-put! sf 'conn #f)
      (sf-put! sf 'reconnecting #f)
      (sf-put! sf 'election #f)
      (sf-put! sf 'peers '())
      (sf-put! sf 'token token)
      (sf-put! sf 'host-id (sf-get sf 'id))
      (sf-put! sf 'seq-counter
               (apply max (cons 0 (map caddr (sf-get sf 'roster)))))
      (let loop ((l pend))
        (when (nnull? l)
          (if (live-apply-patch key (car l))
              (loop (cdr l))
              (live-create key (sf-composite sf)))))
      (when (nnull? pend) (sf-put! sf 'dirty #t))
      (write-entry key port token (sf-get sf 'interactive) (sf-get sf 'id))
      (sf-put! sf 'tried-reconnect #f)
      (sf-debug "now hosting " key " on " port "\n")
      ;; a Save asked in the meantime is done here now
      (when (sf-get sf 'save-wanted)
        (sf-put! sf 'save-wanted #f)
        (host-save sf))
      token)))

(define (participant-reconnect! sf port token host-id)
  ;; the host changed: connect to the new one
  (let ((old (sf-get sf 'conn)))
    (when old (local-close old))
    (let ((c (local-connect port)))
      (cond ((>= c 0)
             (sf-put! sf 'conn c)
             (sf-put! sf 'host-id host-id)
             (sf-put! sf 'inflight #f)
             (sf-put! sf 'reconnecting (list c (+ (texmacs-time) 4000)))
             (sf-put! sf 'tried-reconnect #t)
             (msg-send c (list 'hello token (sf-key sf) (sf-get sf 'id)
                               (shared-process-id) (sf-get sf 'interactive)
                               (live-current-state (sf-key sf)))))
            (else (participant-host-gone! sf))))))

(define (reconnect-step sf)
  ;; waiting for the answer to the hello sent to a new host
  (let* ((r (sf-get sf 'reconnecting)) (c (car r)))
    (let loop ()
      (with m (msg-receive c)
        (cond (m
                (cond ((func? m 'welcome-same)
                       (sf-put! sf 'tried-reconnect #f)
                       (sf-put! sf 'roster (caddr m))
                       (sf-put! sf 'reconnecting #f))
                      ((func? m 'welcome-catchup)
                       (sf-put! sf 'tried-reconnect #f)
                       (with (tag from to psexp roster host-id) m
                         (sf-put! sf 'roster roster)
                         (sf-put! sf 'reconnecting #f)
                         (participant-apply-remote! sf (scheme->patch psexp)
                                                    from to)))
                      ((func? m 'welcome-reset)
                       (sf-put! sf 'tried-reconnect #f)
                       (with (tag state doc modified roster host-id) m
                         (sf-put! sf 'roster roster)
                         (sf-put! sf 'reconnecting #f)
                         (participant-reset! sf state doc modified)))
                      (else (loop))))
              ((not (local-alive? c))
               (sf-put! sf 'reconnecting #f)
               (participant-host-gone! sf))
              ((> (texmacs-time) (cadr r))
               (sf-put! sf 'reconnecting #f)
               (participant-host-gone! sf)))))))

;; A host which is gone (its link is dead, or it said bye): the participant
;; first in priority among those left becomes the host, from its own copy;
;; the others look for the new entry and connect to it. A candidate which
;; does not appear within a while is left out and the next one is awaited.

(define (participant-host-gone! sf)
  (when (not (sf-get sf 'election))
    (let ((c (sf-get sf 'conn)))
      (when c (local-close c))
      (sf-put! sf 'conn #f)
      (sf-put! sf 'inflight #f)
      ;; (deadline for a candidate, those left out, the host that was, the
      ;; time of the next try: after a failed reconnection, a second later)
      (sf-put! sf 'election
               (list (+ (texmacs-time) 3000) (list (sf-get sf 'host-id))
                     (sf-get sf 'host-id)
                     (+ (texmacs-time) (if (sf-get sf 'tried-reconnect) 1000 0))))
      (sf-debug "host gone: " (sf-get sf 'host-id) "\n")
      (election-step sf))))

(define (election-step sf)
  (let* ((e (sf-get sf 'election))
         (deadline (car e)) (excluded (cadr e)) (old-host (caddr e))
         (next-try (cadddr e)))
    (when (>= (texmacs-time) next-try)
      (let ((foreign (serving-entry (sf-key sf)))
            (cands (by-priority
                    (list-filter (sf-get sf 'roster)
                                 (lambda (r) (not (in? (car r) excluded)))))))
        (cond (foreign
               ;; a host is alive (the old one, which did not answer, or a
               ;; new one): go on trying to reach it; never take its place
               (sf-put! sf 'election #f)
               (participant-reconnect! sf (caddr foreign) (cadddr foreign)
                                       (list-ref foreign 6)))
              ((or (null? cands) (== (car (car cands)) (sf-get sf 'id)))
               (become-host! sf))
              ((> (texmacs-time) deadline)
               (sf-put! sf 'election
                        (list (+ (texmacs-time) 3000)
                              (cons (car (car cands)) excluded)
                              old-host next-try))))))))

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
    (when c
      (msg-send c (list 'leave))
      (local-close c))
    (sf-destroy! sf)))

(define (leave-as-host sf)
  (host-flush! sf)
  (let* ((key (sf-key sf))
         (peers (sf-get sf 'peers)))
    (cond ((null? peers)
           (remove-entry-if key (sf-get sf 'token))
           (sf-destroy! sf))
          (else
            (let* ((cands (by-priority
                           (map (lambda (p) (list (peer-id p)
                                                  (peer-interactive? p)
                                                  (peer-seq p) (peer-pid p)))
                                peers))))
              (host-start-handover sf (car (car cands)) #t)
              ;; wait for the end of the handover (the other process answers
              ;; at its next tick)
              (let ((end (+ (texmacs-time) 1500)))
                (let loop ()
                  (host-tick sf)
                  (when (and (sf-get sf 'handover) (< (texmacs-time) end))
                    (loop))))
              (when (not (sf-get sf 'handed-over))
                ;; nobody took over: the participants elect a host
                (send-peers sf (list 'bye))
                (remove-entry-if key (sf-get sf 'token)))
              (for (p (sf-get sf 'peers)) (local-close (peer-link p)))
              (sf-destroy! sf))))))

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
             (cond ((and m (func? m 'hello) (== (length m) 7))
                    (identify-link c m)
                    #f)
                   (m (local-close c) #f)
                   ((not (local-alive? c)) (local-close c) #f)
                   ((> (- (texmacs-time) (cdr e)) 5000) (local-close c) #f)
                   (else #t)))))))

(define (token=? a b)
  ;; without stopping at the first difference
  (and (string? a) (string? b) (== (string-length a) (string-length b))
       (let loop ((i 0) (acc 0))
         (if (>= i (string-length a))
             (== acc 0)
             (loop (+ i 1)
                   (+ acc (if (== (string-ref a i) (string-ref b i)) 0 1)))))))

(define (identify-link c m)
  ;; the first packet of a link: it must have the token of the file, else
  ;; the link is closed
  (with (tag token key id pid interactive base) m
    (let ((sf (ahash-ref sf-table key)))
      (cond ((and sf (sf-host? sf) (token=? token (sf-get sf 'token)))
             (local-set-limit c 67108864)
             (host-accept-hello sf c id pid interactive base))
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
    (for (sf (map cdr (ahash-table->list sf-table)))
      (when (ahash-ref sf-table (sf-key sf))
        (sf-tick sf))))
  (if (== (ahash-size sf-table) 0)
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
