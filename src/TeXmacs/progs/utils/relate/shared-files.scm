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

(define (sf-stat! sf name)
  ;; counted for the process and for the file (for the tools of an agent)
  (stat! name)
  (let ((t (or (sf-get sf 'stats)
               (let ((t (make-ahash-table))) (ahash-set! sf 'stats t) t))))
    (ahash-set! t name (+ 1 (or (ahash-ref t name) 0)))))

(tm-define (shared-file-stat name event)
  (:synopsis "How many times @event (a symbol) happened to the shared file @name")
  (with sf (sf-by-name name)
    (or (and sf (sf-get sf 'stats) (ahash-ref (sf-get sf 'stats) event)) 0)))

(tm-define (shared-stat name)
  (:synopsis "For the tests: how many times @name (a symbol) happened")
  (or (ahash-ref sf-stats name) 0))

(tm-define (shared-collect-mark name)
  (:synopsis "The changes of the shared buffer @name that were not sent yet, to go back to them")
  ;; (for the edits that are made tentatively and cancelled: nothing of them
  ;; must reach the host)
  (with sf (sf-by-name name)
    (if sf (list (sf-get sf 'collect)) #f)))

(tm-define (shared-collect-restore! name mark)
  (:synopsis "Forget the changes of @name made since @mark (see shared-collect-mark)")
  (with sf (sf-by-name name)
    (when (and sf mark)
      (sf-put! sf 'collect (car mark)))))

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

(define sf-agent? #f)                     ; is this process an agent (texmacs --mcp)?

(tm-define (shared-set-agent! flag)
  (:synopsis "Mark this process as an agent (`texmacs --mcp'): the ids it takes start with `agent.'")
  (set! sf-agent? flag))

(tm-define (shared-agent?)
  (:synopsis "Is this process an agent?")
  sf-agent?)

(define (sf-new-id)
  ;; (the id of a participant tells the host that it is an agent)
  (set! sf-serial (+ sf-serial 1))
  (string-append (if sf-agent? "agent." "")
                 (number->string (shared-process-id)) "." sf-run-id "."
                 (number->string sf-serial)))

(tm-define (shared-agent-id? id)
  (:synopsis "Is the participant id @id the one of an agent?")
  (and (string? id) (string-starts? id "agent.")))

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
          (length (sf-get sf 'pending))
          (and (sf-get sf 'inflight) #t))))

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
                     (lambda () (read-object-checked (string-load u)))
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

(define entry-verdicts (make-ahash-table))   ; key -> (time pid port alive?)
(define entry-verdict-ttl 5000)

(define (entry-alive-now? pid port)
  (and (integer? pid) (shared-pid-alive? pid)
       (integer? port)
       (!= (shared-port-probe port) 0)))

(define (entry-alive? e)
  ;; is the host of the entry @e there? The process must exist (kill 0; a
  ;; host that is stopped is alive), and its port must not refuse a
  ;; connection (the port of a process id that was reused by another program,
  ;; or of a zombie, is closed); a port that does not answer within 100 ms is
  ;; taken to be a busy host. No lock is taken. The verdict is kept for 5 s,
  ;; so that listing again and again does not fill the queue of a host that
  ;; is stopped.
  (let* ((key (cadr e)) (pid (list-ref e 4)) (port (list-ref e 2))
         (old (ahash-ref entry-verdicts key)) (now (texmacs-time)))
    (if (and old (< (- now (car old)) entry-verdict-ttl)
             (== (cadr old) pid) (== (caddr old) port))
        (cadddr old)
        (let ((alive (entry-alive-now? pid port)))
          (ahash-set! entry-verdicts key (list now pid port alive))
          alive))))

(tm-define (shared-files-in-use)
  (:synopsis "The files which other processes of the user host: ((path interactive pid) ...)")
  ;; (for `texmacs --mcp': what the person has open; a file of this process
  ;; is not listed, nor an entry whose host is gone, which is found by its
  ;; process being gone)
  (let ((dir (shared-user-dir)))
    (if (== dir "")
        '()
        (let loop ((names (list-dir-names dir)) (acc '()))
          (cond ((null? names) (reverse acc))
                ((not (string-ends? (car names) ".entry")) (loop (cdr names) acc))
                (else
                  (let* ((u (string->url (string-append dir "/" (car names))))
                         (e (catch #t
                              (lambda () (read-object-checked (string-load u)))
                              (lambda args #f))))
                    (if (catch #t
                          (lambda ()
                            (and (func? e 'entry) (== (length e) 8)
                                 (string? (cadr e))
                                 (read-entry (cadr e))
                                 (not (ahash-ref sf-table (cadr e)))
                                 (entry-alive? e)))
                          (lambda args #f))
                        (loop (cdr names)
                              (cons (list (cadr e) (list-ref e 5) (list-ref e 4))
                                    acc))
                        (loop (cdr names) acc)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (msg-text msg)
  ;; s7 prints at most 40 elements of a list unless print-length is raised: a
  ;; document of more than 40 blocks was cut when it was sent
  (if (s7-scheme?)
      (let-temporarily (((*s7* 'print-length) (ash 1 40)))
        (object->string msg))
      (object->string msg)))

(define (msg-send link msg)
  (local-send link (msg-text msg)))

;; The conversions of trees (reading, stree->tree, patch->scheme, object->string)
;; are recursive and a tree that is 100000 levels deep crashes them. A message
;; whose parentheses are nested more than sf-max-depth deep is dropped before it
;; is read. (Documents have a few tens of levels; the agent's content is limited
;; to 200.) Short messages cannot be that deep, so they are not scanned.
(define sf-max-depth 1500)

(define (delimiter? c)
  (or (char=? c #\space) (char=? c #\newline) (char=? c #\tab) (char=? c #\return)
      (char=? c #\() (char=? c #\))))

(define (nesting-ok? s)
  ;; Messages are written by object->string: parentheses, atoms, strings, numbers,
  ;; #t and #f. Anything else outside a string (#\x, #| |#, ; comments, quote
  ;; characters at the start of a token, | symbols, backslashes) makes the message
  ;; refused: the reader knows more syntax than this scanner, and what it hides
  ;; defeats the count. The characters # ' , ` | inside a token (the name of a
  ;; macro like `l'x') are part of the symbol for the reader, and the printer
  ;; writes them so: they are accepted when the previous character belongs to
  ;; an atom (never after a space, a parenthesis or the end of a string, where
  ;; they would start something).
  (or (< (string-length s) (* 2 sf-max-depth))
      (let ((n (string-length s)))
        (let loop ((i 0) (depth 0) (in-str #f) (atom #f))
          (if (>= i n)
              (not in-str)
              (let ((c (string-ref s i)))
                (cond (in-str
                        (cond ((char=? c #\\) (loop (+ i 2) depth #t #f))
                              ((char=? c #\") (loop (+ i 1) depth #f #f))
                              (else (loop (+ i 1) depth #t #f))))
                      ((char=? c #\") (loop (+ i 1) depth #t #f))
                      ((char=? c #\()
                       (and (< depth sf-max-depth) (loop (+ i 1) (+ depth 1) #f #f)))
                      ((char=? c #\)) (loop (+ i 1) (max 0 (- depth 1)) #f #f))
                      ((or (char=? c #\space) (char=? c #\newline) (char=? c #\tab) (char=? c #\return))
                       (loop (+ i 1) depth #f #f))
                      ((and (char=? c #\#) (not atom))
                       (and (< (+ i 1) n) (memv (string-ref s (+ i 1)) '(#\t #\f))
                            (or (>= (+ i 2) n) (delimiter? (string-ref s (+ i 2))))
                            (loop (+ i 2) depth #f #f)))
                      ((memv c '(#\; #\\)) #f)
                      ((memv c '(#\# #\| #\' #\` #\,))
                       (and atom (loop (+ i 1) depth #f #t)))
                      (else (loop (+ i 1) depth #f #t)))))))))

(define (read-object-checked s)
  ;; the object written in @s, if it is not nested too deep (see nesting-ok?)
  (and (nesting-ok? s) (string->object s)))

(define (msg-receive link)
  ;; the next message of a link, or #f
  (with s (local-receive link)
    (and (!= s "")
         (nesting-ok? s)
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

;; The patches of the other processes are not trusted. Their shape is checked
;; before any patch function sees them (a modification with a path that is
;; too short made the process die in C++), and a patch of a participant is
;; rebuilt with the inverses computed from the document, not the ones it sent.

;; (no set-cursor: the patches that are sent never have one, and the editor
;; reads the data of one)
(define mod-kinds '(assign insert remove split join assign-node insert-node remove-node))

(define (mod-min-length k)
  ;; the length of the path under the shared tree (the part 0 or 1 comes first)
  (cond ((memq k '(assign assign-node)) 1)
        ((memq k '(remove split)) 3)
        (else 2)))

(define (mod-shape-ok? x)
  (and (list? x) (= (length x) 3) (memq (car x) mod-kinds)
       (let ((path (cadr x)))
         (and (or (not (eq? (car x) 'assign-node)) (symbol? (caddr x)))
              (list? path)
              (list-and (map (lambda (i) (and (integer? i) (>= i 0) (<= i 1000000))) path))
              (>= (length path) (mod-min-length (car x)))
              (memv (car path) '(0 1))
              #t))))

(define (patch-shape-ok? x depth)
  (and (pair? x) (list? x) (< depth 64)
       (cond ((eq? (car x) 'pair)
              (and (= (length x) 3) (mod-shape-ok? (cadr x)) (mod-shape-ok? (caddr x))))
             ((eq? (car x) 'compound)
              (list-and (map (lambda (c) (patch-shape-ok? c (+ depth 1))) (cdr x))))
             (else #f))))

(define (safe-patch psexp)
  ;; the patch of the shape-checked @psexp, or #f (also if the integers do not fit)
  (and (patch-shape-ok? psexp 0)
       (catch #t (lambda () (scheme->patch psexp)) (lambda args #f))))

(tm-define (shared-body-patch p)
  (:synopsis "For the tests: the part of the patch @p that is under the body")
  (split-patch p 1))

(tm-define (shared-sanitize-patch psexp doc)
  (:synopsis "The patch of a participant on the document @doc with the inverses of the document, or #f (for the tests)")
  (and (patch-shape-ok? psexp 0) (rebuild-patch psexp doc)))

(define (patch-forward-mods x)
  ;; the forward modifications of a patch of the right shape, in order, without
  ;; the cursor
  (cond ((eq? (car x) 'pair)
         (if (eq? (car (cadr x)) 'set-cursor) '() (list (cadr x))))
        (else (append-map patch-forward-mods (cdr x)))))

(define (rebuild-patch x doc)
  ;; the patch of the shape-checked @x on the document @doc, with the inverses
  ;; of the document; #f if it does not apply
  (catch #t
    (lambda ()
      (let loop ((l (patch-forward-mods x)) (t doc) (acc '()))
        (if (null? l)
            (patch-compound (reverse acc))
            (let ((m (scheme->modification (car l))))
              (and (modification-applicable? t m)
                   (let ((p1 (patch-pair m (modification-invert m t))))
                     (loop (cdr l) (patch-apply t p1) (cons p1 acc))))))))
    (lambda args #f)))

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

(define (own-author sf)
  ;; the author of the editor of the buffer: what this process types is
  ;; its own, and its undo skips the others (it is current after
  ;; start-editing)
  (or (sf-get sf 'own-author)
      (with a (catch #t
                (lambda ()
                  (with-buffer (sf-buffer sf)
                    (start-editing)
                    (get-author)))
                (lambda args #f))
        (when a (sf-put! sf 'own-author a))
        a)))

(define (author-of sf peer-id)
  ;; the author (a number) of the changes of the participant @peer-id: its
  ;; own for each one, so that the undo of the person skips them
  (with t (sf-get sf 'authors)
    (or (ahash-ref t peer-id)
        (with a (if (== peer-id (sf-get sf 'id))
                    (or (own-author sf) (new-author))
                    (new-author))
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
  (and (== (get-preference "share-open-files") "on")
       ;; (the browser and Windows have no folder of entries: nothing is shared)
       (!= (shared-user-dir) "")))

(define (shareable-key-peek name)
  ;; as shareable-key, but the per-user folder is not made
  (and (url? name)
       (not (url-scratch? name))
       (not (url-rooted-web? name))
       (not (url-rooted-tmfs? name))
       (url-test? name "f")
       (== (url-format name) "texmacs")
       (with key (shared-real-path (url->system name))
         (and (!= key "") key))))

(define (lock-file-name key)
  (string-append "f-" (tree-hash (tm->tree key)) ".lock"))

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
;; What the footer and the focus bar show
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (shared-agent-short id)
  (:synopsis "A short name of the participant @id: agent 1234, program name")
  (let ((l (string-tokenize-by-char id #\.)))
    (if (and (pair? l) (pair? (cdr l)) (== (car l) "agent"))
        (string-append "agent " (cadr l))
        (if (and (pair? l) (pair? (cdr l))) (string-append "program " (car l)) id))))

(define (count-text n one many)
  (string-append (number->string n) " " (if (== n 1) one many)))

(tm-define (shared-file-pending-count name)
  (:synopsis "The number of proposals of agents waiting for the person on @name")
  (with l (or (shared-proposals name) '())
    (length (list-filter l (lambda (e) (== (cadr e) "pending"))))))

(tm-define (shared-file-footer-text name)
  (:synopsis "The text the footer shows for the shared file @name, or #f (nothing for a file hosted alone)")
  ;; (cheap: tables and counts, no message)
  (let ((sf (sf-by-name name)))
    (cond
      ((not sf)
       (and (shared-file-not-joined name)
            (if (shared-file-not-joined-stale? name)
                (translate "Not shared: the other TeXmacs closed this file; close it and open it again")
                (translate "Not shared: open in another TeXmacs"))))
      ((sf-host? sf)
       (let* ((agents (shared-file-agents name))
              (n (length (sf-get sf 'peers)))
              (windows (+ 1 (- n (length agents)))))
         (cond ((== n 0) #f)
               ((null? agents)
                (string-append (translate "Shared: hosting") ", "
                               (number->string windows) " " (translate "windows")))
               (else
                 (let ((np (shared-file-pending-count name)))
                   (string-append
                    (translate "Shared with") " " (shared-agent-short (car agents))
                    (if (> (length agents) 1)
                        (string-append " " (translate "and") " "
                                       (number->string (- (length agents) 1)) " "
                                       (translate (if (== (length agents) 2) "other" "others")))
                        "")
                    " (" (translate (symbol->string (or (shared-agent-mode name) 'propose))) ")"
                    (if (> np 0)
                        (string-append ", " (number->string np) " "
                                       (translate (if (== np 1) "proposal" "proposals")))
                        "")))))))
      (else (translate "Shared: joined")))))

(tm-define (shared-agents-focus?)
  (:synopsis "Does the current buffer have an agent, here, as host (then the focus bar shows the agent)?")
  (and (shared-files-in-use?)
       (nnull? (shared-file-agents (current-buffer)))))

(tm-define (shared-agent-connection name id)
  (:synopsis "The agent @id of the host @name: connected, (silent SECONDS) or gone")
  ;; (silence is shown only for a peer that has answered a ping once: an
  ;; older program that never answers is taken to be connected)
  (let ((sf (sf-by-name name)))
    (with p (and sf (sf-host? sf) (find-peer sf id))
      (cond ((not p) 'gone)
            ((not (local-alive? (peer-link p))) 'gone)
            ((not (ahash-ref sf-ponged (heard-key sf id))) 'connected)
            (else
              (let* ((t (ahash-ref sf-heard (heard-key sf id)))
                     (dt (if t (- (texmacs-time) t) 0)))
                (if (> dt 5000)
                    (list 'silent (quotient dt 1000))
                    'connected)))))))

(tm-define (shared-agent-connection-text name id)
  (:synopsis "The connection of the agent @id as a short text")
  (with c (shared-agent-connection name id)
    (cond ((== c 'connected) (translate "connected"))
          ((== c 'gone) (translate "disconnected"))
          (else (string-append (translate "busy or not answering for") " "
                               (number->string (cadr c)) " s")))))

(tm-define (shared-agents-focus-text name)
  (:synopsis "The agent of the focus bar: its name, and its connection when it is not as it should be")
  (let ((l (shared-file-agents name)))
    (if (null? l) ""
        (with c (shared-agent-connection name (car l))
          (if (== c 'connected)
              (shared-agent-short (car l))
              (string-append (shared-agent-short (car l)) ": "
                             (shared-agent-connection-text name (car l))))))))

(define sf-check-result (make-ahash-table))   ; name -> text

(tm-define (shared-agent-check-connection name)
  (:synopsis "Ping the agents of @name; the answer is shown in the footer a second later")
  (and-with sf (sf-by-name name)
    (and (sf-host? sf)
         (let ((agents (shared-file-agents name)) (now (texmacs-time)))
           (for (id agents)
             (with p (find-peer sf id)
               (when p (msg-send (peer-link p) (list 'ping now)))))
           (ahash-set! sf-check-result (url->string name) #f)
           (set-message "Shared file: checking the connection to the agent..." "Shared file")
           (delayed (:pause 1000)
             (shared-agent-check-report name now))
           #t))))

(tm-define (shared-agent-check-report name since)
  (:synopsis "Say how the agents of @name answered the ping sent at @since")
  (and-with sf (sf-by-name name)
    (let* ((agents (shared-file-agents name))
           (txt (if (null? agents)
                    "Shared file: no agent is connected"
                    (string-recompose
                     (map (lambda (id)
                            (let ((t (ahash-ref sf-heard (heard-key sf id))))
                              (string-append
                               (shared-agent-short id) ": "
                               (if (and t (>= t since)) "answers"
                                   (if (== (shared-agent-connection name id) 'gone)
                                       "disconnected" "does not answer")))))
                          agents)
                     ", "))))
      (ahash-set! sf-check-result (url->string name) txt)
      (set-message (if (string-starts? txt "Shared") txt (string-append "Shared file: " txt))
                   "Shared file")
      txt)))

(tm-define (shared-agent-last-check name)
  (:synopsis "For the tests: the text of the last connection check")
  (ahash-ref sf-check-result (url->string name)))

;; The proposals of the focus bar: the pending ones, in the order of the
;; document, and the one at the cursor (the block with the cursor)
(define (pending-by-index name)
  (list-sort (list-filter (or (shared-proposals name) '())
                          (lambda (e) (== (cadr e) "pending")))
             (lambda (a b) (< (caddr a) (caddr b)))))

(define (cursor-block-index)
  ;; the index of the block of the body with the cursor, or #f (the path of the
  ;; cursor starts with that of the body)
  (let* ((cp (cursor-path)) (bp (tree->path (buffer-get-body (current-buffer)))))
    (and (list? cp) (list? bp) (> (length cp) (length bp))
         (== (list-head cp (length bp)) bp)
         (integer? (list-ref cp (length bp)))
         (list-ref cp (length bp)))))

(tm-define (shared-proposal-at-cursor name)
  (:synopsis "The id of the pending proposal in the block with the cursor, or #f")
  (let* ((i (cursor-block-index)))
    (and i
         (let ((l (list-filter (pending-by-index name) (lambda (e) (== (caddr e) i)))))
           (and (nnull? l) (car (car l)))))))

(tm-define (shared-proposal-go! name dir)
  (:synopsis "Go to the next (dir 1) or previous (-1) pending proposal; its id or #f")
  (let* ((l (pending-by-index name))
         (i (or (cursor-block-index) -1)))
    (and (nnull? l)
         (let* ((cands (if (> dir 0)
                           (list-filter l (lambda (e) (> (caddr e) i)))
                           (reverse (list-filter l (lambda (e) (< (caddr e) i))))))
                (e (if (nnull? cands) (car cands)
                       (if (> dir 0) (car l) (car (reverse l))))))
           (tree-go-to (tree-ref (buffer-get-body (current-buffer)) (caddr e)) :start)
           (car e)))))

(tm-define (shared-proposal-resolve-all! name which)
  (:synopsis "Accept (new) or reject (old) all pending proposals of @name")
  (for (e (pending-by-index name))
    (shared-proposal-resolve! name (car e) which)))

(tm-define (shared-proposal-resolve-here! name which)
  (:synopsis "Accept or reject the proposal at the cursor")
  (with id (shared-proposal-at-cursor name)
    (and id (shared-proposal-resolve! name id which))))

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
                              ((== r 'busy) (mark-not-joined! name r) (tell-unshared name))
                              ((pair? r) (mark-not-joined! name r) (tell-unshared name r))
                              (else (host-start name key 0)))))
                     ((> now deadline)
                      (pending-done! name)
                      (mark-not-joined! name 'busy)
                      (tell-unshared name)))))
            ((> now deadline)
             ;; nobody took the file: from the disk
             (pending-done! name)
             (host-start name key 0))))))))

(define sf-not-joined (make-ahash-table))  ; (url->string name) -> why it was not joined

(define (unshared-why r)
  ;; r: 'busy or (refused . tag)
  (if (pair? r)
      (string-append "its content cannot be shared: the document has a malformed "
                     (cdr r) " (a wrong number of arguments, or a structure the editor does not accept)")
      "its process does not answer"))

(define (tell-unshared name . r)
  (set-message
   (string-append "Shared file: this file is open in another TeXmacs, but "
                  (unshared-why (if (null? r) 'busy (car r)))
                  "; it is opened from the file, without sharing, and not saved")
   "Shared file"))

(define (mark-not-joined! name r)
  ;; the file is open in a live host and this buffer is not its participant:
  ;; read-only for the life of the buffer (until it is closed and opened again)
  (set-shared-files-in-use! #t)
  (ahash-set! sf-not-joined (url->string name)
              (string-append "open in the person's TeXmacs but not joined: " (unshared-why r))))

(tm-define (shared-file-not-joined name)
  (:synopsis "Why the buffer @name is not joined to the live TeXmacs that holds its file, or #f")
  ;; (kept as long as the buffer lives: when the host has gone the copy is stale,
  ;; and when another host has come the buffer is still not its participant)
  (and (not (sf-by-name name))
       (with why (ahash-ref sf-not-joined (url->string name))
         (and why
              (if (shared-file-not-joined-stale? name)
                  (string-append why "; the person's TeXmacs has closed the file since, so this copy may be out of date: close the document and open it again")
                  why)))))

(tm-define (shared-file-not-joined-stale? name)
  (:synopsis "Is the file of the not joined buffer @name no longer held by a live host?")
  (and (ahash-ref sf-not-joined (url->string name))
       (with key (shareable-key-peek name)
         (and key (not (shared-lock-held? (lock-file-name key)))))))

(tm-define (shared-file-held-elsewhere? name)
  (:synopsis "Does a live TeXmacs hold the file @name (its lock), while this process is not its host or participant?")
  ;; (the lock is there whatever the preference: a process that does not share
  ;; must not write a file that a live host holds)
  (and (url? name)
       (with key (shareable-key-peek name)
         (and key (not (ahash-ref sf-table key))
              (shared-lock-held? (lock-file-name key))))))

(define (stale-save-message)
  "Shared file: the other TeXmacs closed this file; close it and open it again (or use Save as)")

(tm-define (shared-file-save-guard name)
  (:synopsis "A save, or an export, to @name: refuse it (message, #t) while a live host holds the file")
  (let ((u (if (url? name) name (system->url name))))
    (and (shared-file-held-elsewhere? u)
         (begin
           (set-message (translate "Shared file: not saved: this file is open in another TeXmacs which holds it (use Save as for a copy under another name)")
                        "Save file")
           #t))))

(define (arity-bad-tag st)
  ;; the tag of the innermost subtree the arity check refuses
  (if (not (pair? st))
      "?"
      (let loop ((l (cdr st)))
        (cond ((null? l) (if (symbol? (car st)) (symbol->string (car st)) "?"))
              ((and (pair? (car l)) (not (mcp-stree-arity-ok? (car l))))
               (arity-bad-tag (car l)))
              (else (loop (cdr l)))))))

(define (note-unshared! name . r)
  ;; another process holds the lock of the file (it hosts it) but does not
  ;; answer: the file is read as usual, not shared. It is never hosted
  ;; meanwhile: the lock is what makes a host, and a stalled host keeps it.
  (ahash-set! sf-unshared (url->string name) (texmacs-time))
  (mark-not-joined! name (if (null? r) 'busy (car r)))
  (tell-unshared name (if (null? r) 'busy (car r))))

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
                         (with r (join-host* name key entry)
                           (cond ((== r #t) #t)
                                 ((== r 'busy) (note-unshared! name) #f)
                                 ((pair? r) (note-unshared! name r) #f)
                                 (else (abandoned! name key entry)))))))))))

;; (join-host with a buffer which exists, read from the file, replaces it)
(define (join-host* name key entry)
  ;; join-host, with one more try after a short while when the host did not
  ;; answer in time (it may have been busy at that moment)
  (with r (join-host name key entry)
    (if (== r 'busy)
        (with e2 (or (read-entry key) entry)
          (join-host name key e2))
        r)))

(define (join-host name key entry)
  ;; gives #t (joined), 'busy (the lock is held, but the host does not
  ;; answer: it may be busy or stopped) or #f (nobody holds the lock: the
  ;; entry is not of a host)
  (let* ((id (sf-new-id)) (inter (shared-interactive?))
         (c (do-hello key entry id inter #f))
         (m (and (>= c 0) (msg-wait c sf-join-wait))))
    (cond ((and m (func? m 'welcome) (== (length m) 7)
                (not (mcp-stree-arity-ok? (caddr m))))
           (local-close c)
           (cons 'refused (arity-bad-tag (caddr m))))
          ((and m (func? m 'welcome) (== (length m) 7))
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
               (when inter
                 (set-message (translate "Shared file: shared with another TeXmacs window; edits are kept in sync")
                              "Shared file"))
               (refresh-shared-ui! sf)
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
           (when (not (and t (< (- (texmacs-time) t) 10000)))
             (ahash-remove! sf-not-joined (url->string name)))
           (ahash-remove! sf-unshared (url->string name))
           (and t (< (- (texmacs-time) t) 10000)))
         (noop))
        ((and (shared-files-on?) (not (sf-by-name name)))
         (and-with key (shareable-key name)
           (when (not (ahash-ref sf-table key))
             (host-start name key 0))))))

(define (lock-acquire-retry key)
  ;; (the probe of another process holds a shared lock for a moment: when the
  ;; lock is refused and nobody holds it, it is asked again)
  (let loop ((n 0))
    (or (lock-acquire key)
        (and (< n 3)
             (not (shared-lock-held? (lock-file-name key)))
             (begin
               (let ((end (+ (texmacs-time) 5))) (let spin () (when (< (texmacs-time) end) (spin))))
               (loop (+ n 1)))))))

(define (host-start name key tries)
  (let ((port (local-listen)) (h (lock-acquire-retry key)))
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
          ;; (the lock could not be made, and nobody holds it: no folder to write,
          ;; no room: the file is not shared)
          ((not (shared-lock-held? (lock-file-name key))) (noop))
          (else
            ;; someone holds the lock: it hosts the file
            (let ((e (read-entry key)))
              (if (not e)
                  (begin                         ; (it is starting: wait)
                    (pending-join! name key 2000 #f)
                    (pending-start! name))
                  (with r (join-host* name key e)
                    (cond ((== r #t) #t)
                          ((== r 'busy) (note-unshared! name))
                          ((pair? r) (note-unshared! name r))
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
  ;; (the mark "this is a person" goes with the connection: a process that
  ;; claims the id of a marked one while it is connected is not told apart)
  (ahash-set! sf-host-persons (sf-key sf)
              (list-filter (or (ahash-ref sf-host-persons (sf-key sf)) '())
                           (lambda (x) (!= x (peer-id p)))))
  (ahash-remove! sf-heard (heard-key sf (peer-id p)))
  (ahash-remove! sf-ponged (heard-key sf (peer-id p)))
  (local-close (peer-link p))
  (sf-put! sf 'peers (list-filter (sf-get sf 'peers)
                                  (lambda (q) (!= (peer-id q) (peer-id p)))))
  (send-roster sf)
  (refresh-shared-ui! sf))

(define (priority-less? a b)
  ;; entries (id interactive seq pid): interactive first, then the earlier
  (cond ((and (cadr a) (not (cadr b))) #t)
        ((and (cadr b) (not (cadr a))) #f)
        (else (< (caddr a) (caddr b)))))

(define (by-priority l)
  (sort l priority-less?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agents: the mode of a file, the filter, proposals (docs/mcp-server.md)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The host of a file the person has open decides what the patches of the
;; participants marked as agents (ids `agent.*') do: `propose' (the default:
;; each patch becomes a proposal in the document, old and new version side by
;; side, with the markup of Version > Compare), `direct' (applied) or
;; `read-only' (refused). The mode is in the state of the host for the file,
;; not in the document; the agent is told. The content rules of the MCP
;; server are applied by the host to the patches of agents in every mode
;; (mcp-host-check); a refusal carries structured reasons (rule level text),
;; the level being style, links, code or limits. A host which is not
;; interactive (an agent's process, a test) applies the patches as before.

;; Who is an agent. The id `agent.*' is chosen by the participant, so it is not
;; what the host relies on: a participant that is not interactive (a headless
;; process, whatever its id) is treated as an agent unless the person said it is
;; not (shared-treat-as-person!, the menu). The local link is NOT a security
;; boundary against a process of the same user that has a shell (it can claim to
;; be interactive, or edit the file on disk): the host's filter protects the
;; person from the content that an agent writes, through the usual paths.

(define sf-host-modes (make-ahash-table))     ; file key -> mode chosen by the person
(define sf-host-persons (make-ahash-table))   ; file key -> ids of participants taken for persons

(define (peer-person? sf peer)
  (and (member (peer-id peer) (or (ahash-ref sf-host-persons (sf-key sf)) '())) #t))

(define (agent-peer? sf peer)
  (and (not (peer-person? sf peer))
       (or (shared-agent-id? (peer-id peer))
           (not (peer-interactive? peer)))))

(define (agent-gated? sf peer)
  (and (sf-get sf 'interactive) (agent-peer? sf peer)))

(tm-define (shared-treat-as-person! name id flag)
  (:synopsis "The host of @name: take the participant @id for a person (flag #t) or for an agent (#f)")
  (and-with sf (sf-by-name name)
    (and (sf-host? sf)
         (let* ((l (or (ahash-ref sf-host-persons (sf-key sf)) '()))
                (l2 (list-filter l (lambda (x) (!= x id)))))
           (ahash-set! sf-host-persons (sf-key sf) (if flag (cons id l2) l2))
           #t))))

(tm-define (shared-agent-mode name)
  (:synopsis "The mode (propose, direct, read-only) the host of @name applies to agents")
  (and-with sf (sf-by-name name)
    (if (sf-host? sf)
        (or (ahash-ref sf-host-modes (sf-key sf)) (if (sf-get sf 'interactive) 'propose 'direct))
        (sf-get sf 'agent-mode))))

(tm-define (shared-set-agent-mode! name mode)
  (:synopsis "The host of @name: choose the mode for the agents (propose, direct, read-only)")
  (and-with sf (sf-by-name name)
    (and (sf-host? sf) (memq mode '(propose direct read-only))
         (begin
           ;; (in the process, not in the record of the file: it is kept when
           ;; the file is handed over and comes back)
           (ahash-set! sf-host-modes (sf-key sf) mode)
           (for (p (sf-get sf 'peers))
             (when (agent-peer? sf p) (msg-send (peer-link p) (list 'mode mode))))
           #t))))

(tm-define (shared-file-agent-info name id)
  (:synopsis "The host: what is known of the participant @id: (pid command), command a short text or #f")
  (and-with sf (sf-by-name name)
    (with p (find-peer sf id)
      (and p
           (let* ((pid (peer-pid p))
                  (cmd (catch #t
                         (lambda ()
                           (with r (var-eval-system (string-append "ps -o command= -p " (number->string pid) " 2>/dev/null"))
                             (if (> (string-length r) 60) (substring r 0 60) r)))
                         (lambda args #f))))
             (list pid (and cmd (!= cmd "") cmd)))))))

(tm-define (shared-file-agents name)
  (:synopsis "The ids of the agents that have @name open with this host")
  (with sf (sf-by-name name)
    (if (and sf (sf-host? sf))
        (map peer-id (list-filter (sf-get sf 'peers) (lambda (p) (agent-peer? sf p))))
        '())))

;; The blocks that a patch changes: (first n-old n-new), #f if none, 'bad
(tm-define (shared-changed-blocks old new)
  (if (not (and (tm-is? old 'document) (tm-is? new 'document)))
      'bad
      (let* ((ko (tree-children old)) (kn (tree-children new))
             (lo (length ko)) (ln (length kn))
             ;; (equal trees, not equal hashes: a hash can be forged)
             (same? (lambda (a b) (or (tree-eq? a b) (equal? (tm->stree a) (tm->stree b))))))
        (let loop ((a ko) (b kn) (p 0))
          (if (and (pair? a) (pair? b) (same? (car a) (car b)))
              (loop (cdr a) (cdr b) (+ p 1))
              (if (and (null? a) (null? b))
                  #f
                  (let sloop ((ra (reverse a)) (rb (reverse b)) (s 0))
                    (if (and (pair? ra) (pair? rb) (same? (car ra) (car rb)))
                        (sloop (cdr ra) (cdr rb) (+ s 1))
                        (list p (- lo p s) (- ln p s))))))))))

(define (version-tag-in? st)
  (and (pair? st)
       (or (and (memq (car st) '(version-both version-old version-new)) #t)
           (list-or (map version-tag-in? (cdr st))))))

(define (st-has-assign? st)
  (and (pair? st)
       (or (and (memq (car st) '(assign macro xmacro)) #t)
           (list-or (map st-has-assign? (cdr st))))))

(define (patch-arity-ok? p body)
  ;; does the body, after the patch @p of a peer, have no tag with an arity it
  ;; cannot have? (@body: the body it applies to)
  (catch #t
    (lambda ()
      (let ((bp (split-patch p 1)))
        (or (not bp)
            (let* ((new (patch-apply body bp)) (range (shared-changed-blocks body new)))
              (cond ((not range) #t)
                    ((and (tm-is? new 'document) (= (tree-arity new) 0)) #f)
                    ((eq? range 'bad) #f)
                    (else
                      (let* ((sts (map tm->stree (sublist (tree-children new) (car range)
                                                          (+ (car range) (caddr range)))))
                             ;; (a patch that touches a definition: the blocks that use it
                             ;; are not known, so the whole document is looked at)
                             (cands (if (list-or (map st-has-assign? sts))
                                        (map tm->stree (tree-children new))
                                        sts))
                             (bad (list-filter cands (lambda (st) (not (mcp-stree-arity-ok? st))))))
                        ;; only a block that is bad and was not there before makes the patch refused
                        ;; (a legacy block that fails the rule must not stop every other patch)
                        (or (null? bad)
                            (let ((old-sts (map tm->stree (tree-children body))))
                              (not (list-or (map (lambda (st) (not (member st old-sts))) bad))))))))))))
    (lambda args #f)))

(tm-define (shared-patch-acceptable? p body)
  (:synopsis "For the tests: does the host accept the patch @p on the body @body as far as arities and structure go?")
  (patch-arity-ok? p body))

(define (blocks-stree body p n)
  (map tm->stree (sublist (tree-children body) p (+ p n))))

(define (agent-filter u old new range)
  ;; the reasons (rule level text) why the content rules refuse the change, or '()
  (catch #t
    (lambda () (mcp-host-check u old new range))
    (lambda args
      (list (list "check" "code" "the content check of the host failed: the change is refused")))))

(define (agent-gate sf peer p)
  (let* ((t0 (texmacs-time)) (r (agent-gate* sf peer p)))
    (sf-put! sf 'gate-count (+ 1 (or (sf-get sf 'gate-count) 0)))
    (sf-put! sf 'gate-ms (+ (- (texmacs-time) t0) (or (sf-get sf 'gate-ms) 0)))
    r))

(tm-define (shared-file-gate-stats name)
  (:synopsis "For the tests: how many patches of agents the host checked, and the time (ms)")
  (and-with sf (sf-by-name name)
    (list (or (sf-get sf 'gate-count) 0) (or (sf-get sf 'gate-ms) 0))))

(define (agent-gate* sf peer p)
  ;; what the host does with the patch @p of the agent @peer: 'apply, (refuse
  ;; reason ...) or (propose range new-body)
  (let ((mode (shared-agent-mode (sf-buffer sf))) (u (sf-buffer sf)))
    (if (== mode 'read-only)
        (list 'refuse (list "read-only" "mode"
                            "the person has made this document read-only for agents"))
        (let ((mp (split-patch p 0)) (bp (split-patch p 1)))
          (cond
            ((or (not mp) (not bp))
             (list 'refuse (list "patch" "code" "a patch of a kind that the host does not accept from an agent")))
            ((patch-modifies? mp)
             (list 'refuse (list "style" "style" "the style and the settings of the document belong to the person")))
            (else
              (let* ((old (sf-body sf)) (new (patch-apply old bp))
                     (range (shared-changed-blocks old new)))
                (cond
                  ((not range) 'apply)
                  ((== range 'bad)
                   (list 'refuse (list "patch" "code" "the patch does not keep the document a sequence of blocks")))
                  ((list-or (map version-tag-in? (blocks-stree old (car range) (cadr range))))
                   (list 'refuse (list "pending" "mode"
                                       "the blocks hold version markup (a pending proposal): resolve or withdraw it first")))
                  (else
                    (let ((viol (agent-filter u old new range)))
                      (cond ((pair? viol) (cons 'refuse viol))
                            ((== mode 'direct) 'apply)
                            (else (list 'propose range new)))))))))))))

;; Proposals: a pending proposal is a block of the document that holds the
;; markup of Version > Compare (version-both with the old and the new
;; blocks, or the word by word differences of compare-versions when one block
;; replaces one block), so a saved document with pending proposals is a valid,
;; readable document: both versions are in the file. The author and the id
;; are in the host's records (not in the document): after a reload the markup
;; is plain version markup, to be resolved with the Version tools.

(define (proposal-markup old-sts new-sts)
  (define (docof l) (if (null? l) '(document (version-suppressed)) (cons 'document l)))
  (define (coarse) (list 'version-both (docof old-sts) (docof new-sts)))
  (if (and (pair? old-sts) (null? (cdr old-sts)) (pair? new-sts) (null? (cdr new-sts)))
      (or (catch #t
            (lambda ()
              (with r (compare-versions (car old-sts) (car new-sts))
                (and (version-tag-in? r) r)))
            (lambda args #f))
          (coarse))
      (coarse)))

(define (vresolve st k)
  ;; the stree @st with the version @k (0 old, 1 new) of every difference;
  ;; 'none for what is absent in that version
  (cond ((not (pair? st)) st)
        ((memq (car st) '(version-both version-old version-new))
         (vresolve (list-ref st (+ k 1)) k))
        ((eq? (car st) 'version-suppressed) 'none)
        ((eq? (car st) 'document)
         (let loop ((cs (cdr st)) (acc '()))
           (if (null? cs)
               (if (null? acc) 'none (cons 'document (reverse acc)))
               (let ((r (vresolve (car cs) k)))
                 (cond ((eq? r 'none) (loop (cdr cs) acc))
                       ((and (pair? (car cs)) (memq (car (car cs)) '(version-both version-old version-new))
                             (pair? r) (eq? (car r) 'document))
                        (loop (cdr cs) (append (reverse (cdr r)) acc)))
                       (else (loop (cdr cs) (cons r acc))))))))
        (else (cons (car st)
                    (map (lambda (c) (let ((r (vresolve c k))) (if (eq? r 'none) "" r)))
                         (cdr st))))))

(define (resolved-blocks st k)
  ;; the blocks that replace the block @st
  (let ((r (vresolve st k)))
    (cond ((eq? r 'none) '())
          ((and (pair? st) (memq (car st) '(version-both version-old version-new))
                (pair? r) (eq? (car r) 'document))
           (map version-normalize (cdr r)))
          (else (list (version-normalize r))))))

(define (first-text st)
  (cond ((string? st) st)
        ((pair? st) (let loop ((l (cdr st)))
                      (cond ((null? l) #f)
                            ((first-text (car l)))
                            (else (loop (cdr l))))))
        (else #f)))

(define (proposal-preview new-sts old-sts)
  (let ((t (or (list-or (map first-text new-sts)) (list-or (map first-text old-sts)) "")))
    (if (> (string-length t) 40) (string-append (substring t 0 40) "...") t)))

(define (prop-get pr k) (ahash-ref pr k))
(define (prop-put! pr k v) (ahash-set! pr k v))

(define (proposal-tree sf pr)
  ;; the block of the proposal, or #f when it is not in the document
  (let ((t (tree-pointer->tree (prop-get pr 'ptr))) (body (sf-body sf)))
    (and (tree? t) (not (tm-is? t 'uninit)) (not (tree-eq? t body))
         (let ((up (tree-up t))) (and up (tree-eq? up body)))
         t)))

(define (proposal-index sf pr)
  (let ((t (proposal-tree sf pr)))
    (if t (tree-index t) (or (prop-get pr 'index) -1))))

(define (proposal-find sf id)
  (with l (list-filter (or (sf-get sf 'proposals) '()) (lambda (pr) (== (prop-get pr 'id) id)))
    (and (nnull? l) (car l))))

(define (proposal-pending? pr) (== (prop-get pr 'status) "pending"))

(define (proposal-resolve! sf pr which status)
  ;; replace the markup of the proposal by its old or its new version (an edit
  ;; of the person), and note the outcome
  (let ((t (proposal-tree sf pr)))
    (when (and t (version-tag-in? (tm->stree t)))
      (let ((blocks (resolved-blocks (tm->stree t) (if (== which 'new) 1 0)))
            (parent (tree-up t)) (i (tree-index t)))
        (prop-put! pr 'index i)
        (with-buffer (sf-buffer sf)
          (start-editing)
          (cond ((null? blocks) (tree-remove parent i 1))
                (else
                  (tree-assign t (stree->tree (car blocks)))
                  (when (pair? (cdr blocks))
                    (tree-var-insert parent (+ i 1)
                                     (stree->tree (cons 'document (cdr blocks)))))))
          (end-editing))))
    (prop-put! pr 'status status)
    (sf-put! sf 'prop-changed #t)
    (host-flush! sf)))

(tm-define (shared-proposals name)
  (:synopsis "The proposals of agents on @name: (id status index owner preview)")
  (and-with sf (sf-by-name name)
    (if (sf-host? sf)
        (map (lambda (pr) (list (prop-get pr 'id) (prop-get pr 'status)
                                (proposal-index sf pr) (prop-get pr 'owner)
                                (prop-get pr 'preview)))
             (reverse (or (sf-get sf 'proposals) '())))
        (or (sf-get sf 'proposals) '()))))

(tm-define (shared-proposal-resolve! name id which)
  (:synopsis "The person on @name: accept (which = new) or reject (old) the proposal @id")
  (and-with sf (sf-by-name name)
    (and (sf-host? sf)
         (with pr (proposal-find sf id)
           (and pr (proposal-pending? pr)
                (begin
                  (proposal-resolve! sf pr which (if (== which 'new) "accepted" "rejected"))
                  #t))))))

(define (proposals-classify! sf pr)
  ;; the markup of a pending proposal is gone (the person used the Version
  ;; tools, or edited): was the new or the old version kept?
  (let* ((body (sf-body sf))
         (sts (map tm->stree (tree-children body)))
         (has (lambda (seq)
                (let ((n (length seq)))
                  (and (> n 0) (>= (length sts) n)
                       (let loop ((l sts))
                         (cond ((< (length l) n) #f)
                               ((equal? (sublist l 0 n) seq) #t)
                               (else (loop (cdr l))))))))))
    (prop-put! pr 'status
               (cond ((has (prop-get pr 'new)) "accepted")
                     ((has (prop-get pr 'old)) "rejected")
                     (else "resolved")))))

(define (proposals-poll! sf)
  (for (pr (or (sf-get sf 'proposals) '()))
    (when (proposal-pending? pr)
      (let ((t (proposal-tree sf pr)))
        (if (and t (version-tag-in? (tm->stree t)))
            (prop-put! pr 'index (tree-index t))
            (proposals-classify! sf pr))))))

(define (proposals-sync! sf)
  ;; tell each agent about its proposals when they changed
  (when (nnull? (or (sf-get sf 'proposals) '()))
    (proposals-poll! sf))
  (let ((sent (or (sf-get sf 'prop-sent)
                  (let ((t (make-ahash-table))) (sf-put! sf 'prop-sent t) t))))
    (for (p (sf-get sf 'peers))
      (when (agent-peer? sf p)
        (let ((l (map (lambda (pr) (list (prop-get pr 'id) (prop-get pr 'status)
                                         (proposal-index sf pr)))
                      (reverse (list-filter (or (sf-get sf 'proposals) '())
                                            (lambda (pr) (== (prop-get pr 'owner) (peer-id p))))))))
          (when (!= l (or (ahash-ref sent (peer-id p)) '()))
            (ahash-set! sent (peer-id p) l)
            (msg-send (peer-link p) (list 'proposals l))))))))

(define (proposal-create! sf peer range new)
  ;; the patch of the agent @peer becomes a proposal in the document; gives
  ;; its id
  (let* ((u (sf-buffer sf)) (body (sf-body sf))
         (p (car range)) (n-old (cadr range)) (n-new (caddr range))
         (old-sts (blocks-stree body p n-old))
         (new-sts (blocks-stree new p n-new))
         (n (+ 1 (or (sf-get sf 'prop-counter) 0)))
         (id (string-append "p" (number->string n)))
         (markup (proposal-markup old-sts new-sts))
         (au (author-of sf (peer-id peer)))
         (pr (make-ahash-table)))
    (sf-put! sf 'prop-counter n)
    (with-buffer u
      (start-editing)
      (with-author au
        (cond ((> n-old 0)
               (tree-assign (tree-ref body p) (stree->tree markup))
               (when (> n-old 1) (tree-remove body (+ p 1) (- n-old 1))))
              (else
                (tree-var-insert body p (stree->tree (list 'document markup))))))
      (end-editing))
    (prop-put! pr 'id id)
    (prop-put! pr 'owner (peer-id peer))
    (prop-put! pr 'status "pending")
    (prop-put! pr 'ptr (tree->tree-pointer (tree-ref body p)))
    (prop-put! pr 'index p)
    (prop-put! pr 'preview (proposal-preview new-sts old-sts))
    (prop-put! pr 'old old-sts)
    (prop-put! pr 'new new-sts)
    (let ((l (cons pr (or (sf-get sf 'proposals) '()))))
      ;; (the resolved ones are kept for a while, so that the agent hears of them)
      (sf-put! sf 'proposals
               (let loop ((l l) (resolved 0))
                 (cond ((null? l) '())
                       ((proposal-pending? (car l)) (cons (car l) (loop (cdr l) resolved)))
                       ((>= resolved 30) (loop (cdr l) resolved))
                       (else (cons (car l) (loop (cdr l) (+ resolved 1))))))))
    id))

(tm-define (shared-file-send-withdraw name id)
  (:synopsis "The agent: withdraw its proposal @id from the host of @name")
  (and-with sf (sf-by-name name)
    (and (sf-get sf 'conn)
         (begin (msg-send (sf-get sf 'conn) (list 'withdraw id)) #t))))

(tm-define (shared-file-save-refused name)
  (:synopsis "The agent: the reason why the host refused its Save, once, or #f")
  (and-with sf (sf-by-name name)
    (with v (sf-get sf 'save-refused)
      (sf-put! sf 'save-refused #f)
      v)))

(tm-define (shared-file-verdict name)
  (:synopsis "The agent: what the host answered to its last refused patch: (kind id data), once")
  (and-with sf (sf-by-name name)
    (with v (sf-get sf 'verdict)
      (sf-put! sf 'verdict #f)
      v)))


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
         (shape? (patch-shape-ok? psexp 0))
         (base-doc (and shape?
                        (cond ((== base cur) (live-current-document key))
                              ((in? base (live-get-history key))
                               (live-get-document key base))
                              (else #f))))
         (p0 (and base-doc (rebuild-patch psexp base-doc)))
         (p (cond ((not p0) #f)
                  ((== base cur) p0)
                  (else
                    (with r (rebase-over-history key base p0)
                      (when r (stat! 'rebased))
                      r)))))
    (when (not shape?)
      (stat! 'malformed)
      (msg-send (peer-link peer)
                (list 'refused id cur
                      (list (list "patch" "code" "a patch of a shape that the host does not accept")))))
    (cond ((and p (patch-applicable? p (live-current-document key))
                (not (patch-arity-ok? p (tree-ref (live-current-document key) 1)))
                (begin
                  (sf-stat! sf 'arity-refused)
                  (msg-send (peer-link peer)
                            (list 'refused id cur
                                  (list (list "arity" "code" "a tag would have a number of children it cannot have"))))
                  #t))
           #t)
          ((and p (patch-applicable? p (live-current-document key))
                (agent-gated? sf peer)
                (with g (agent-gate sf peer p)
                  (cond ((eq? g 'apply) #f)
                        ((eq? (car g) 'refuse)
                         (sf-stat! sf 'agent-refused)
                         (msg-send (peer-link peer) (list 'refused id cur (cdr g)))
                         #t)
                        (else
                          (sf-stat! sf 'agent-proposed)
                          (let ((pid (proposal-create! sf peer (cadr g) (caddr g))))
                            ;; (the agent takes its change back first, then
                            ;; hears of the markup that the host made)
                            (msg-send (peer-link peer) (list 'proposed id cur pid))
                            (host-flush! sf))
                          #t))))
           #t)
          ((and p (patch-applicable? p (live-current-document key)))
           ;; (applied to the buffer first: if that fails nothing moves, but
           ;; the buffer and the live document differ: start again from the
           ;; buffer, so that the host heals)
           (if (apply-in-place! sf p (author-of sf (peer-id peer)))
               (let ((to (live-apply-patch key p)))
                 (sf-notify-modified! sf)
                 (seen! sf id)
                 (cond (to
                         (stat! 'peer-patches)
                         (send-peers sf (list 'applied cur to (peer-id peer) id
                                              (patch->scheme p)))
                         (sf-trim! key))
                       (else (host-reset-live! sf))))
               (begin
                 (stat! 'refused)
                 (host-reset-live! sf)
                 (msg-send (peer-link peer) (list 'refused id cur)))))
          (shape?
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
    (heard! sf id)
    (host-send-welcome sf link base id interactive pid inflight)
    (when (agent-peer? sf (list id link interactive seq pid))
      (msg-send link (list 'mode (shared-agent-mode (sf-buffer sf))))
      (when (sf-get sf 'prop-sent) (ahash-remove! (sf-get sf 'prop-sent) id)))
    (when (and (sf-get sf 'interactive) (agent-peer? sf (list id link interactive seq pid)))
      (if (shared-agent-id? id)
          (set-message (translate "Shared file: an agent joined this document (Tools > Agents)")
                       "Shared file")
          (begin
            (stat! 'unmarked-joined)
            (set-message "Shared file: a program that is not marked as an agent joined; it is treated as an agent (Tools > Agents)"
                         "Shared file"))))
    (send-roster sf)
    (refresh-shared-ui! sf)
    ;; an interactive process hosts rather than a headless one: it is
    ;; offered the file
    (when interactive (host-offer-handover! sf))))

(define (host-handle-message sf peer msg)
  ;; (a message of a shape that is not expected gets an answer, and does not
  ;; stop the host)
  (let ((ok (catch #t
              (lambda () (host-handle-message* sf peer msg) #t)
              (lambda args #f))))
    (when (not ok)
      (stat! 'malformed)
      (catch #t
        (lambda ()
          (msg-send (peer-link peer)
                    (list 'refused "malformed" (live-current-state (sf-key sf))
                          (list (list "message" "code" "a message that the host does not understand")))))
        (lambda args #f)))))

(define (host-handle-message* sf peer msg)
  (cond ((func? msg 'patch)
         (if (and (list? msg) (= (length msg) 4) (string? (cadr msg)) (list? (cadddr msg)))
             (with (tag id base p) msg
               (host-handle-patch sf peer id base p))
             (throw (quote malformed))))
        ((func? msg 'save)
         (if (and (agent-gated? sf peer) (!= (shared-agent-mode (sf-buffer sf)) 'direct))
             (begin
               (stat! 'save-refused)
               (msg-send (peer-link peer)
                         (list 'save-refused
                               "the person has not let agents save this document: the person saves it")))
             (host-save sf)))
        ((func? msg 'leave)
         (drop-peer! sf peer))
        ((func? msg 'withdraw)
         (when (not (and (list? msg) (= (length msg) 2) (string? (cadr msg))))
           (throw (quote malformed)))
         (with pr (proposal-find sf (cadr msg))
           (when (and pr (proposal-pending? pr) (== (prop-get pr 'owner) (peer-id peer)))
             (proposal-resolve! sf pr 'old "withdrawn"))))
        ((func? msg 'pong) (ahash-set! sf-ponged (heard-key sf (peer-id peer)) #t))
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
          (heard! sf (peer-id p))
          (host-handle-message sf p m)
          (when (find-peer sf (peer-id p)) (loop)))))
    (when (and (find-peer sf (peer-id p)) (not (local-alive? (peer-link p))))
      (sf-debug "peer " (peer-id p) " is gone\n")
      (drop-peer! sf p)))
  (host-check-lock! sf)
  (proposals-sync! sf)
  (host-ping-agents! sf)
  ;; a headless host retries to give the file to an interactive participant
  (let ((now (texmacs-time)))
    (when (> now (or (sf-get sf 'offer-at) 0))
      (sf-put! sf 'offer-at (+ now 5000))
      (host-offer-handover! sf))))

;; Is an agent there? A host with a person pings its agents every two seconds
;; (ping T), which the participants answer with (pong T) at their next tick; any
;; message also counts as heard. The focus bar shows when it was.
(define sf-heard (make-ahash-table))   ; "key|id" -> time of the last message
(define sf-ponged (make-ahash-table))  ; "key|id" -> #t once it has answered a ping

(define (heard-key sf id) (string-append (sf-key sf) "|" id))
(define (heard! sf id) (ahash-set! sf-heard (heard-key sf id) (texmacs-time)))

(define (refresh-shared-ui! sf)
  ;; the footer and the focus bar are drawn again (the window of the file is
  ;; the current one: else they are drawn when it becomes current)
  (when (and (not (headless?)) (== (url->string (current-buffer)) (url->string (sf-buffer sf))))
    (catch #t (lambda () (notify-change 259)) (lambda args #f))))

(define (host-ping-agents! sf)
  (when (sf-get sf 'interactive)
    (let ((now (texmacs-time)))
      (when (> now (or (sf-get sf 'ping-at) 0))
        (sf-put! sf 'ping-at (+ now 2000))
        ;; (what the person sees changed: the connection, the proposals)
        (with ui (list (shared-file-footer-text (sf-buffer sf))
                       (shared-agents-focus-text (sf-buffer sf)))
          (when (!= ui (sf-get sf 'ui-last))
            (sf-put! sf 'ui-last ui)
            (refresh-shared-ui! sf)))
        (for (p (sf-get sf 'peers))
          (when (agent-peer? sf p)
            (msg-send (peer-link p) (list 'ping now))))))))

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
  (if (and (not (sf-by-name name)) (shared-file-not-joined name))
      (begin
        (set-message (if (shared-file-not-joined-stale? name)
                         (translate (stale-save-message))
                         (translate "Shared file: not saved: this file is open in another TeXmacs which holds it (use Save as for a copy under another name)"))
                     "Save file")
        #t)
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
           #t)))))

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

(tm-define (shared-test-send-peers! name msg)
  (:synopsis "For the tests: the host sends @msg to all its participants")
  (and-with sf (sf-by-name name)
    (when (sf-host? sf) (send-peers sf msg) #t)))

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
    (sf-stat! sf 'lost)
    ;; (an agent is told by its tool: nothing is saved for it)
    (when (not sf-agent?)
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
                      (noop)))))))))))

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
  (if (mcp-stree-arity-ok? (caddr doc))
      (participant-reset!* sf state doc modified)
      (begin
        (stat! 'bad-remote)
        (set-message
         (string-append "Shared file: the host sent a document which cannot be shared ("
                        (arity-bad-tag (caddr doc)) "); it is not applied")
         "Shared file"))))

(define (participant-reset!* sf state doc modified)
  ;; the document of the host replaces everything, pending changes included
  (let* ((key (sf-key sf)) (name (sf-buffer sf)))
    ;; The document of this process is replaced as a whole, never combined
    ;; with the new one (that could revert newer changes of the host). When
    ;; it is not the same, it is first saved in the recovered folder.
    (when (!= (sf-composite-stree sf) doc)
      (let ((file (save-recovered-copy! sf)))
        (sf-stat! sf 'replaced)
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

(define (participant-apply-remote! sf r from to author)
  ;; a patch of the host, against the confirmed state @from
  (let* ((key (sf-key sf))
         (d0 (live-current-document key))
         (pend (sf-get sf 'pending)))
    (cond ((or (not (catch #t (lambda () (patch-applicable? r d0)) (lambda args #f)))
               (not (patch-arity-ok? r (tree-ref d0 1))))
           ;; (a patch that does not apply to the confirmed document: patch-invert
           ;; and the others abort on it)
           (stat! 'bad-remote)
           (participant-request-resync! sf))
          ((null? pend)
           (when (apply-in-place! sf r (author-of sf (or author (sf-get sf 'host-id))))
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
                ((not (apply-in-place! sf r (author-of sf (or author (sf-get sf 'host-id)))))
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
                      (sf-stat! sf 'dropped)
                      (report-lost! sf (list-tail pend (length ok))))
                    (sf-put! sf 'pending ok)
                    ;; (the patch which was sent is still on its way if it
                    ;; is kept: its confirmation is recognised by the id)
                    (sf-put! sf 'inflight (and (nnull? ok) inflight))
                    (sf-trim! key)))))))))

(define (participant-request-resync! sf)
  (sf-stat! sf 'resync)
  (sf-debug "resync\n")
  (when (sf-get sf 'conn)
    (msg-send (sf-get sf 'conn) (list 'resync))))

(define (participant-handle-message sf msg)
  (let ((key (sf-key sf)))
    (cond ((func? msg 'ping)
           (when (and (== (length msg) 2) (sf-get sf 'conn)
                      ;; (the variable: for the test of an older program that never answers)
                      (not (getenv "TEXMACS_SHARED_NOPONG")))
             (msg-send (sf-get sf 'conn) (list 'pong (cadr msg)))))
          ((func? msg 'applied)
           (with (tag from to author id psexp) msg
             (seen! sf id)
             (cond ((or (!= from (live-current-state key))
                        (not (safe-patch psexp)))
                    (participant-request-resync! sf))
                   ((and id (== id (sf-get sf 'inflight))
                         (== author (sf-get sf 'id)))
                    ;; the host took my patch: it is confirmed
                    (if (live-apply-patch key (safe-patch psexp) to)
                        (begin
                          (sf-put! sf 'pending (if (nnull? (sf-get sf 'pending)) (cdr (sf-get sf 'pending)) '()))
                          (sf-put! sf 'inflight #f)
                          (sf-trim! key))
                        (participant-request-resync! sf)))
                   (else
                     (participant-apply-remote! sf (safe-patch psexp)
                                                from to author)))))
          ((func? msg 'mode)
           (sf-put! sf 'agent-mode (cadr msg)))
          ((func? msg 'save-refused)
           (sf-put! sf 'save-refused (cadr msg)))
          ((func? msg 'proposals)
           (sf-put! sf 'proposals (cadr msg)))
          ((or (func? msg 'refused) (func? msg 'proposed))
           (with (tag id cur data) (if (= (length msg) 3) (append msg (list #f)) msg)
             (when (== id (sf-get sf 'inflight))
               (sf-stat! sf 'rolled-back)
               (sf-debug "my patch was refused\n")
               (if data
                   (sf-put! sf 'verdict (list tag id data cur))
                   (report-lost! sf (sf-get sf 'pending)))
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
                         (if (safe-patch psexp)
                             (participant-apply-remote! sf (safe-patch psexp)
                                                        from to #f)
                             (participant-request-resync! sf))))
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
  (cond ((and (== (ahash-size sf-table) 0) (== (ahash-size sf-pending) 0))
         (set! sf-tick-running? #f)
         #t)
        ;; a host that nobody has joined polls for a join ten times a second
        ;; (a joiner waits for the welcome 500 ms); with a participant or a
        ;; peer it polls every 20 ms
        ((and (== (ahash-size sf-pending) 0) (null? sf-incoming)
              (list-and (map (lambda (e) (and (sf-host? (cdr e)) (null? (sf-get (cdr e) 'peers))))
                             (ahash-table->list sf-table))))
         100)
        (else 20)))

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
  (ahash-remove! sf-not-joined name)
  (when (shared-files-in-use?)
    (shared-file-leave (string->url name))))
