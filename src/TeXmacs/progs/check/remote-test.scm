;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : remote-test.scm
;; DESCRIPTION : tests of the TeXmacs server and client, without a network
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The remote layer of TeXmacs (progs/server and progs/client):
;;
;;   - the names of remote resources: tmfs://remote-file/<host>/~<pseudo>/...,
;;     tmfs://remote-dir/..., tmfs://chat/<host>/<room>, tmfs://live/<host>/
;;     <name>, with time=<seconds> for past versions (client-tmfs,
;;     with-remote-context of server-tmfs);
;;   - the server data in a database: users, files and directories with
;;     their versions, permissions (owner, readable, writable), chat rooms
;;     and messages, sharing, notifications and live documents;
;;   - the services of the server (tm-service, service-dispatch-table,
;;     server-eval) and the call backs of the client (tm-call-back,
;;     call-back-dispatch-table, client-eval);
;;   - the client data: connections, accounts, the cache of directory
;;     listings, sorting, notifications, the synchronization of local
;;     directories and of databases with the server.
;;
;; There is no network: the server and its clients run in this process.
;; The glue functions server-write and client-write, through which the two
;; sides send their messages, are replaced during the suite, for the
;; connection numbers of the suite only, by functions which queue the
;; messages; loop-pump hands them to server-eval and client-eval, as the
;; idle loops of server-add and client-add would do with the messages read
;; from the sockets. A user is logged in with server-login-uid, without a
;; password (the passwords are tested by crypto-test.scm).
;;
;; The server database (server-database) and the user databases of the
;; client (user-database) are redefined during the suite, to databases in
;; a directory of (url-temp-dir). The server keeps its users in
;; $TEXMACS_HOME_PATH/server/users.scm, its pending users in
;; pending-users.scm and the documents in a repository under the same
;; directory: the suite only runs when TeXmacs does not use the home of the
;; user (tests/scheme/check.sh gives a scratch home), uses users named
;; rt-..., and removes at the end its users and the files it put in the
;; repository.
;;
;; Not tested here: the sockets, TLS and the server process (server-start,
;; tls-client-start, legacy-anonymous-client-start, client-login-then and
;; the other functions which open a connection), passwords and credentials,
;; the dialogs and widgets (client-widgets, client-menu, server-widgets,
;; server-menu), the emails (server-send-email runs the mail command), the
;; preferences of the server which are saved (remote-set-preferences,
;; server-tree-cache-set-enabled), the license (server-public-preferences
;; writes it when it is missing) and the tree cache (server-cache-test.scm).
;; The tmfs handlers which fill buffers (remote files and directories, chat
;; rooms, lists of shared documents), the live-modify call back of the
;; client (it would change the live documents of the server, which share
;; the tables of this process) and the application of a synchronization of
;; databases (the changes are dated to the second) are not run either.
;; The integration tests server-*-test.scm cover the deletion plan, the
;; notifications of chat rooms and the backups.

(texmacs-module (check remote-test)
  (:use (check check-lib)
        (server server-base)
        (server server-tmfs)
        (server server-chat)
        (server server-notifications)
        (server server-live)
        (server server-db)
        (server server-sync)
        (client client-base)
        (client client-tmfs)
        (client client-db)
        (client client-chat)
        (client client-live)
        (client client-sync)
        (client client-db-sync)
        (client client-notifications)
        (notification notification-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remote-dir (url-append (url-temp-dir) "remote-test"))

(define (tmp name) (url-append remote-dir name))

(define (skip what why)
  (display* "  SKIP " what ": " why "\n")
  (force-output))

(define (run-group thunk)
  ;; an error in a group counts as one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))
    ;; with-global does not restore its variable after an error
    (db-reset)
    (set! db-time :now)
    (set! loop-queue '())))

(define (priv module name)
  ;; a function which the module does not export
  (module-ref (resolve-module module) name))

(define (scratch-home?)
  (let ((home (url->system (url-concretize "$TEXMACS_HOME_PATH")))
        (user (url->system (url-concretize "~/.TeXmacs"))))
    (!= home user)))

(define (sorted l) (sort l string<=?))

(define (cork-open) (string (integer->char 16)))
(define (cork-close) (string (integer->char 17)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Databases of the suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remote-active? #f)

(tm-define (server-database)
  (:require remote-active?)
  (tmp "server.tmdb"))

(tm-define (user-database . opt-kind)
  (:require remote-active?)
  (with kind (if (null? opt-kind) "general" (car opt-kind))
    (tmp (string-append "user-" kind ".tmdb"))))

(define-macro (with-db . body)
  `(with-database (server-database)
     (with-user #t
       ,@body)))

(define (field id attr)
  (with-db (db-get-field id attr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connections without a network
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (client number on the server, server number on the client, pseudo)
(define loop-connections
  '((9001 9101 "rt-alice")
    (9002 9102 "rt-bob")
    (9003 9103 "rt-root")
    (9004 9104 #f)
    (9005 9105 "rt-carol")))

(define (loop-client? fd) (nnot (assoc fd loop-connections)))
(define (loop-server? fd)
  (nnot (list-find loop-connections (lambda (c) (== (cadr c) fd)))))
(define (loop-server-of client) (cadr (assoc client loop-connections)))
(define (loop-client-of server)
  (car (list-find loop-connections (lambda (c) (== (cadr c) server)))))

(define loop-queue '())
(define loop-log '())
(define loop-blocked '())
(define loop-active? #f)

;; NOTE: the messages are intercepted where they are sent, in client-send
;; and server-send, and not by replacing the primitives client-write and
;; server-write: S7 calls the primitives which a function used when it was
;; first evaluated, whatever their names are bound to later on.

(define (next-serial module var)
  ;; the serial number @var of the messages sent by @module, incremented
  (with m (resolve-module module)
    (with n (eval var m)
      (eval `(set! ,var ,(+ n 1)) m)
      n)))

(tm-define (client-send server cmd)
  (:require (and loop-active? (loop-server? server)))
  (with n (next-serial '(client client-base) 'client-serial)
    (set! loop-queue
          (rcons loop-queue
                 (list 'server server (object->string* (list n cmd)))))))

(tm-define (server-send client cmd)
  (:require (and loop-active? (loop-client? client)))
  (with n (next-serial '(server server-base) 'server-serial)
    (set! loop-queue
          (rcons loop-queue
                 (list 'client client (object->string* (list n cmd)))))))

(define (loop-install!)
  (set! loop-active? #t))

(define (loop-uninstall!)
  (set! loop-active? #f))

(define (loop-pump)
  ;; deliver the queued messages, including those sent while delivering
  (let loop ((n 0))
    (when (and (pair? loop-queue) (< n 10000))
      (with (side fd s) (car loop-queue)
        (set! loop-queue (cdr loop-queue))
        (with (msg-id cmd) (string->object s)
          (set! loop-log (cons (list side fd cmd) loop-log))
          (cond ((== side 'server)
                 (server-eval (list (loop-client-of fd) msg-id) cmd))
                ((and (pair? cmd) (in? (car cmd) loop-blocked))
                 (noop))
                (else
                  (client-eval (list (loop-server-of fd) msg-id) cmd)))))
      (loop (+ n 1)))))

(define (loop-received side fd head)
  ;; the commands with the label head received by one side, oldest first
  (reverse (list-filter (map caddr
                             (list-filter loop-log
                                          (lambda (x) (and (== (car x) side)
                                                           (== (cadr x) fd)))))
                        (lambda (cmd) (and (pair? cmd) (== (car cmd) head))))))

(define (rcall server cmd)
  ;; the answer of the server to cmd, or (:error message)
  (let ((r '(:no-answer)))
    (client-remote-eval server cmd
                        (lambda (x) (set! r x))
                        (lambda (e) (set! r (list :error e))))
    (loop-pump)
    r))

(define (ralice cmd) (rcall 9101 cmd))
(define (rbob cmd) (rcall 9102 cmd))
(define (rroot cmd) (rcall 9103 cmd))
(define (ranon cmd) (rcall 9104 cmd))
(define (rcarol cmd) (rcall 9105 cmd))

(define (with-cont fun)
  ;; the value which fun passes to its continuation
  (let ((r '(:no-answer)))
    (fun (lambda args (set! r (if (and (pair? args) (null? (cdr args)))
                                  (car args) args))))
    (loop-pump)
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup and cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-pseudos '("rt-alice" "rt-bob" "rt-root" "rt-carol" "rt-dave"))

(define (remove-test-users)
  (for (p test-pseudos)
    (when (server-find-user p)
      (server-remove-user p))
    (server-remove-pending-user p)))

(define repo (url-concretize "$TEXMACS_HOME_PATH/server"))

(define (remove-repository-file loc)
  ;; remove a file of the repository and the directories left empty
  (with f (url-append repo loc)
    (when (url-exists? f) (system-remove f))
    (let loop ((d (url-head f)))
      (when (and (url-descends? d repo) (!= (url->system d) (url->system repo))
                 (url-directory? d) (null? (url-read-directory d "*")))
        (system-rmdir d)
        (loop (url-head d))))))

(define (remove-repository-files)
  (with-db
    (with-time :always
      (for (type '("file" "message" "live"))
        (for (id (db-search `(("type" ,type))))
          (for (loc (db-get-field id "location"))
            (remove-repository-file loc)))))))

(define (remote-setup)
  (check-group "setup")
  (when (url-exists? remote-dir) (system-rmdir-recursive remote-dir))
  (system-mkdir remote-dir)
  (for (name '("server" "user-remote" "user-sync" "user-bib" "user-general"))
    ;; a database file is created before it is opened (see database-test)
    (string-save "" (tmp (string-append name ".tmdb"))))
  (set! remote-active? #t)
  (set! loop-queue '())
  (set! loop-log '())
  (set! loop-blocked '())
  (loop-install!)
  (remove-test-users)
  (check= (url->system (server-database)) (url->system (tmp "server.tmdb")))
  (check= (url->system (user-database "sync"))
          (url->system (tmp "user-sync.tmdb")))
  (check= (url->system (user-database)) (url->system (tmp "user-general.tmdb")))
  (check-false (server-find-user "rt-alice"))
  ;; the accounts
  (server-set-user-info #f "rt-alice" "Alice A" '() "alice@test" #f)
  (server-set-user-info #f "rt-bob" "Bob B" '() "bob@test" #f)
  (server-set-user-info #f "rt-root" "Root R" '() "root@test" #t)
  (server-set-user-info #f "rt-carol" "Carol C" '() "carol@test" #f)
  (for (c loop-connections)
    (with (client server pseudo) c
      (server-add client)
      (client-add server)
      (when pseudo
        (server-login-uid pseudo client pseudo)
        (add-active-connection server "loophost" "6561" pseudo))))
  (check= (server-find-user "rt-alice") "rt-alice")
  (check-true (active-client? 9001))
  (check= (ralice '(remote-logged?)) "yes")
  (check= (ranon '(remote-logged?)) "no"))

(define (remote-cleanup)
  (check-group "cleanup")
  (set! loop-blocked '())
  (for (c loop-connections)
    (with (client server pseudo) c
      (server-logout-client client)
      (server-remove client)
      (client-remove server)))
  (check-false (active-client? 9001))
  (check-false (memv 9101 (client-active-servers)))
  (remove-repository-files)
  (remove-test-users)
  (check-false (server-find-user "rt-alice"))
  (check-false (server-find-pending-user "rt-dave"))
  (loop-uninstall!)
  (set! remote-active? #f)
  (check-false (== (url->system (server-database))
                   (url->system (tmp "server.tmdb"))))
  (system-rmdir-recursive remote-dir)
  (check-false (url-exists? remote-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Names of remote resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-names)
  (check-group "remote file names")
  (check= (remote-file-name "tmfs://remote-file/h/~u/a.tm") "h/~u/a.tm")
  (check= (remote-file-name (string->url "tmfs://remote-file/h/~u/a.tm"))
          "h/~u/a.tm")
  (check= (remote-file-name "tmfs://remote-dir/h/~u") "h/~u")
  (check-false (remote-file-name "tmfs://chat/h/room"))
  (check-false (remote-file-name "/tmp/a.tm"))
  (check-true (remote-file? "tmfs://remote-file/h/~u/a.tm"))
  (check-false (remote-file? "tmfs://remote-dir/h/~u"))
  (check-true (remote-directory? "tmfs://remote-dir/h/~u"))
  (check-false (remote-directory? "tmfs://remote-file/h/~u/a.tm"))
  (check-true (remote-root-directory? (string->url "tmfs://remote-dir/h")))
  (check-false (remote-root-directory? (string->url "tmfs://remote-dir/h/~u")))
  (check-true (remote-root-directory?
               (string->url "tmfs://remote-dir/h/time=5")))
  (check-false (remote-root-directory? (string->url "tmfs://remote-file/h")))
  (check-true (remote-home-directory? (string->url "tmfs://remote-dir/h/~u")))
  (check-false (remote-home-directory?
                (string->url "tmfs://remote-dir/h/~u/sub")))
  (check-false (remote-home-directory? (string->url "tmfs://remote-dir/h")))
  (check= (url->string (remote-parent "tmfs://remote-file/h/~u/a.tm"))
          "tmfs://remote-dir/h/~u")
  (check= (url->string (remote-parent "tmfs://remote-dir/h/~u/sub"))
          "tmfs://remote-dir/h/~u")
  (check= (url->string (remote-parent (string->url "tmfs://remote-dir/h")))
          "tmfs://remote-dir/h")
  (check= (check-unix (url->string (remote-parent (string->url (check-unix-abs "tmp/a/b.tm")))))
          (check-abs "tmp/a"))

  (check-group "versions in remote names")
  (check= (remote-get-time "tmfs://remote-file/h/time=123/~u/a.tm") 123)
  (check= (remote-get-time "tmfs://remote-file/h/~u/a.tm") :now)
  (check-false (remote-get-time "/tmp/a.tm"))
  (check= (remote-strip-time "tmfs://remote-file/h/time=123/~u/a.tm")
          "tmfs://remote-file/h/~u/a.tm")
  (check= (remote-strip-time "tmfs://remote-file/h/~u/a.tm")
          "tmfs://remote-file/h/~u/a.tm")
  (check-true (version-revision? "tmfs://remote-file/h/time=123/~u/a.tm"))
  (check-false (version-revision? "tmfs://remote-file/h/~u/a.tm"))
  (check= (url->string (version-head "tmfs://remote-file/h/time=123/~u/a.tm"))
          "tmfs://remote-file/h/~u/a.tm")
  (check-true (versioned? "tmfs://remote-file/h/~u/a.tm"))

  (check-group "server, port and pseudo of a remote name")
  (check= (remote-file-get-server-name "h:6562/~u/a.tm") "h")
  (check= (remote-file-get-server-name "h/~u/a.tm") "h")
  (check= (remote-file-get-port "h:6562/~u/a.tm") "6562")
  (check= (remote-file-get-port "h/~u/a.tm") "6561")
  (check= (remote-file-get-port "h:x/~u/a.tm") "6561")
  (check= (remote-file-get-pseudo "h/~u/a.tm") "u")
  (check= (remote-file-get-pseudo "h/~u") "u")
  (check-false (remote-file-get-pseudo "h/u/a.tm"))
  (check-false (remote-file-get-pseudo "h"))
  (with split (priv '(client client-base) 'split-server-name-and-port)
    (check= (split "h") '("h" "6561"))
    (check= (split "h:6562") '("h" "6562"))
    (check= (split "h:x") '("h:x" "6561"))
    (check= (split "[::1]:6562") '("::1" "6562"))
    (check= (split "::1") '("::1" "6561")))

  (check-group "types and icons of tmfs urls")
  (check= (tmfs-type (string->url "tmfs://remote-file/h/a.tm")) "file")
  (check= (tmfs-type (string->url "tmfs://remote-dir/h/d")) "dir")
  (check= (tmfs-type (string->url "tmfs://chat/h/room")) "chat")
  (check= (tmfs-type (string->url "tmfs://live/h/doc")) "live")
  (check= (tmfs-icon "chat-room") "tm_cloud_chat.svg")
  (check= (tmfs-icon "dir") "tm_cloud_dir.svg")
  (check= (tmfs-icon "file") "tm_cloud_file.svg")
  (check= (map entry-type-priority '("dir" "file" "chat-room" "chat" "live" "x"))
          '(0 1 2 2 3 4))
  (check-true (chat-room-url? "tmfs://chat/h/room"))
  (check-false (chat-room-url? "tmfs://chat-rooms/h"))
  (check-true (chat-rooms-url? "tmfs://chat-rooms/h"))
  (check-true (mail-box-url? (string->url "tmfs://chat/h/mail-u")))
  (check-false (mail-box-url? (string->url "tmfs://chat/h/room")))
  (check-false (mail-box-url? (string->url "tmfs://remote-file/h/mail-u")))
  (check-true (live-url? "tmfs://live/h/doc"))
  (check-false (live-url? "tmfs://live-list/h"))
  (check-true (live-list-url? "tmfs://live-list/h"))
  (check= ((priv '(client client-chat) 'chat-room-name) "tmfs://chat/h/room")
          "room")

  (check-group "names of live documents")
  (check= (live-get-name "tmfs://live/h/doc") "doc")
  (check= (live-get-name "tmfs://live/h/live/doc") "doc")
  (check= (live-get-name "h/doc") "doc")
  (with live-name (priv '(server server-live) 'live-name)
    (check= (live-name "tmfs://live/h/doc") "doc")
    (check= (live-name "tmfs://live/h/live/doc") "doc")
    (check= (live-name "tmfs://live/h/a/b") "a/b"))

  (check-group "links in messages")
  (with fix-link (priv '(client client-chat) 'fix-link)
    (check= (fix-link "new" "tmfs://remote-file/old/~u/a.tm")
            "tmfs://remote-file/new/~u/a.tm")
    (check= (fix-link "new" "tmfs://remote-dir/old/~u")
            "tmfs://remote-dir/new/~u")
    (check= (fix-link "new" "tmfs://chat/old/room") "tmfs://chat/new/room")
    (check= (fix-link "new" "tmfs://live/old/doc") "tmfs://live/new/doc")
    (check= (fix-link "new" "http://old/x") "http://old/x")
    (check= (fix-link "new" '(concat "x")) '(concat "x")))
  (check= ((priv '(client client-chat) 'fix-links) "new"
           '(document (hlink "a" "tmfs://chat/old/r") "text"))
          '(document (hlink "a" "tmfs://chat/new/r") "text"))
  (with message->share (priv '(client client-chat) 'message->share)
    ;; the link is between Cork quotes
    (check= (message->share "tmfs://chat/h/room")
            `(concat ,(string-append "You were invited to join the chat room "
                                     (cork-open))
                     (hlink "room" "tmfs://chat/h/room")
                     ,(string-append (cork-close) ".")))
    (check= (message->share "tmfs://remote-file/h/~u/a.tm")
            `(concat ,(string-append "The resource " (cork-open))
                     (hlink "a.tm" "tmfs://remote-file/h/~u/a.tm")
                     ,(string-append (cork-close) " has been shared with you."))))
  (with message->document (priv '(client client-chat) 'message->document)
    (with date (pretty-time 1700000000)
      (check= (message->document
               '("send" "u" "User U" "1700000000" (document "hi") "v"))
              `(chat-output "User U" "u" "" ,date (document "hi")))
      (check= (message->document
               '("share" "u" "User U" "1700000000" "tmfs://chat/h/r" "v"))
              `(chat-output "User U" "u" "" ,date
                 (document
                   (concat ,(string-append
                              "You were invited to join the chat room "
                              (cork-open))
                           (hlink "r" "tmfs://chat/h/r")
                           ,(string-append (cork-close) ".")))))))
  (with messages->document (priv '(client client-chat) 'messages->document)
    (check= (messages->document '() "mail-u") '(document (section* "Messages")))
    (check= (messages->document '() "room")
            '(document (section* "Messages") (chat-input "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context of a request on the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remote-context rname)
  (with-remote-context rname
    (list rname db-time past? host head tail next)))

(define (identifier-context id)
  (with-identifier-context id
    (list id db-time)))

(define (test-context)
  (check-group "remote context")
  (check= (remote-context "h/~u/a.tm")
          '("h/~u/a.tm" :now #f "h" "~u" ("~u" "a.tm") "h/~u/a.tm"))
  (check= (remote-context "h/time=12/~u/a.tm")
          '("h/~u/a.tm" "12" #t "h" "time=12" ("~u" "a.tm") "h/~u/a.tm"))
  (check= (remote-context "h") '("h" :now #f "h" "" () "h"))
  (check= db-time :now)
  (check= (identifier-context "x") '("x" :now))
  (check= (identifier-context '("x" "7")) '("x" "7"))
  (check= db-time :now)
  (with msg '("share" "u" "User U" "17" "tmfs://chat/h/r" "v")
    (check= (map (lambda (f) (f msg))
                 (list msg-action msg-pseudo msg-full-name msg-date
                       msg-doc msg-to))
            msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users of the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-users)
  (check-group "user entries")
  (check= (server-get-user-info "rt-alice")
          '("rt-alice" "Alice A" () "alice@test" #f))
  (check= (server-get-user-info "rt-root")
          '("rt-root" "Root R" () "root@test" #t))
  (check-false (server-get-user-info "rt-nobody"))
  (check= (server-uid->pseudo "rt-bob") "rt-bob")
  (check-false (server-uid->pseudo "rt-nobody"))
  (check= (field "rt-alice" "type") '("user"))
  (check= (field "rt-alice" "pseudo") '("rt-alice"))
  (check= (field "rt-alice" "name") '("Alice A"))
  (check= (field "rt-alice" "email") '("alice@test"))
  (check= (field "rt-alice" "owner") '("rt-alice"))
  (check-true (server-pseudo-exists? "rt-bob"))
  (check-false (server-pseudo-exists? "rt-nobody"))
  (check-true (server-pseudo-taken? "rt-bob"))
  (check-false (server-pseudo-taken? "rt-nobody"))

  (check-group "home directories")
  (with homes (with-db (db-search '(("name" "~rt-alice") ("type" "dir"))))
    (check= (length homes) 1)
    (check= (field (car homes) "owner") '("rt-alice"))
    (check= (field (car homes) "dir") '()))
  ;; setting the information again keeps the home directory
  (server-set-user-info #f "rt-alice" "Alice A" '() "alice@test" #f)
  (check= (length (with-db (db-search '(("name" "~rt-alice") ("type" "dir")))))
          1)
  (check= (with-db (file-name->resource "~rt-alice"))
          (car (with-db (db-search '(("name" "~rt-alice"))))))

  (check-group "suspended and deleted users")
  (check-false (server-user-suspended? "rt-carol"))
  (check-true (server-can-login-uid? "rt-carol"))
  (server-suspend-user "rt-carol")
  (check-true (server-user-suspended? "rt-carol"))
  (check-false (server-can-login-uid? "rt-carol"))
  (check-false (server-can-login? "rt-carol"))
  (server-resume-user "rt-carol")
  (check-false (server-user-suspended? "rt-carol"))
  (check-true (server-can-login-uid? "rt-carol"))
  (check-false (server-user-suspended? "rt-nobody"))
  (check-false (server-user-deleted? "rt-carol"))

  (check-group "failed logins")
  (check= (server-get-user-failed-login-counter-uid "rt-carol") 0)
  (check= (server-get-user-last-failed-login-time-uid "rt-carol") 0)
  (with limit (server-get-failed-login-limit)
    (check= limit 3)
    (for (i (.. 0 limit))
      (server-failed-login-uid "rt-carol" 9005 "rt-carol"))
    (check= (server-get-user-failed-login-counter-uid "rt-carol") limit))
  (check-true (> (server-get-user-last-failed-login-time-uid "rt-carol") 0))
  ;; there is no network address without a network
  (check= (server-get-user-last-failed-login-address-uid "rt-carol") "")
  (check-false (server-logged? 9005 "rt-carol"))
  (check-false (server-can-login-uid? "rt-carol"))
  (check-false (server-login-uid "rt-carol" 9005 "rt-carol"))
  (server-set-user-failed-login-counter-uid "rt-carol" 0)
  (check-true (server-can-login-uid? "rt-carol"))
  (check-true (nnot (server-login-uid "rt-carol" 9005 "rt-carol")))
  (check= (server-get-user-failed-login-counter-uid "rt-carol") 0)
  (check-true (>= (server-get-user-last-login-time-uid "rt-carol")
                  (server-get-user-last-failed-login-time-uid "rt-carol")))

  (check-group "logged users")
  (check-true (server-logged? 9001 "rt-alice"))
  (check-false (server-logged? 9002 "rt-alice"))
  (check-false (server-logged? 9004 "rt-alice"))
  (check= (uid-logged? "rt-alice") 9001)
  (check= (pseudo-logged? "rt-bob") 9002)
  (check-false (pseudo-logged? "rt-nobody"))
  (check= (server-get-user '(9001 0)) "rt-alice")
  (check-false (server-get-user '(9004 0)))
  (check-false (server-get-user '(#f 0)))
  (check-true (server-check-admin? '(9003 0)))
  (check-false (server-check-admin? '(9001 0)))
  (check-false (server-check-admin? '(9004 0)))
  (check= (server-get-target-user "rt-bob" '(9003 0)) "rt-bob")
  (check= (server-get-target-user #f '(9003 0)) "rt-root")
  (check= (server-get-target-user "rt-bob" '(9001 0)) "rt-alice")
  (check= (server-get-target-user "rt-bob" '(9004 0)) #f)

  (check-group "pending users")
  (check-false (server-find-pending-user "rt-dave"))
  (server-create-pending-user "rt-dave" "Dave D" '() "dave@test" #f)
  (with l (server-find-pending-user "rt-dave")
    (check= (sublist l 0 4) '("Dave D" () "dave@test" #f))
    (check-true (string-number? (fifth l)))
    (check-true (string-number? (sixth l)))
    (check= (server-get-pending-code "rt-dave") (sixth l)))
  (check-true (server-pending-pseudo-exists? "rt-dave"))
  (check-false (server-pseudo-exists? "rt-dave"))
  (check= (ranon '(confirm-pending-account "rt-dave" "wrong")) "wrong code")
  ;; a wrong code removes the pending user
  (check-false (server-find-pending-user "rt-dave"))
  (server-create-pending-user "rt-dave" "Dave D" '() "dave@test" #f)
  (check= (ranon `(confirm-pending-account "rt-dave"
                    ,(server-get-pending-code "rt-dave")))
          "done")
  (check-false (server-find-pending-user "rt-dave"))
  (check= (server-get-user-info "rt-dave") '("rt-dave" "Dave D" () "dave@test" #f))
  (check= (ranon '(confirm-pending-account "rt-nobody" "1")) "wrong code")

  (check-group "account services")
  (check= (ralice '(remote-get-account #f))
          '(("pseudo" "rt-alice") ("name" "Alice A") ("authentications" ())
            ("email" "alice@test") ("admin" #f)))
  ;; only an administrator reads the account of another user
  (check= (car (ralice '(remote-get-account "rt-bob"))) '("pseudo" "rt-alice"))
  (check= (car (rroot '(remote-get-account "rt-bob"))) '("pseudo" "rt-bob"))
  (check= (ranon '(remote-get-account #f)) '(:error "user not logged"))
  (check= (ralice '(remote-set-account #f (("name" "Alice Z")
                                           ("email" "z@test"))))
          "done")
  (check= (server-get-user-info "rt-alice")
          '("rt-alice" "Alice Z" () "z@test" #f))
  (check= (field "rt-alice" "name") '("Alice Z"))
  (check= (rroot '(remote-set-account "rt-alice" (("name" "Alice A")
                                                  ("email" "alice@test")
                                                  ("unknown" "x")
                                                  ("bad"))))
          "done")
  (check= (server-get-user-info "rt-alice")
          '("rt-alice" "Alice A" () "alice@test" #f))
  (check= (ranon '(remote-set-account #f (("name" "X")))) "user not logged")
  (check= (ralice '(remote-get-accounts 10 0))
          "remote accounts list is not allowed")
  (with l (rroot '(remote-get-accounts 10 0))
    (check-true (list? l))
    (check-true (list-and (map (cut in? <> l)
                               '("rt-alice" "rt-bob" "rt-root" "rt-carol")))))
  (check= (length (rroot '(remote-get-accounts 2 0))) 2)
  (check= (ralice '(remote-protocol-version 1)) "done")
  (check-true (client-version>=? "rt-alice" 1))
  (check-false (client-version>=? "rt-bob" 1))
  (check= (ranon '(remote-protocol-version 1)) '(:error "not logged in"))
  (check= (ralice '(remote-eval (+ 1 2)))
          '(:error "execution of commands is not allowed"))
  (check= (rroot '(remote-eval (+ 1 2))) 3)
  (when (== (get-preference "server service new-account") "off")
    (check= (ranon '(new-account "rt-eve" "Eve" () "eve@test" #t))
            "remote account creation is not allowed"))
  (check-false (server-find-user "rt-eve"))

  (check-group "deleting accounts")
  (with-db
    (server-file-create "rt-dave" "loophost/~rt-dave/d.tm" "<TeXmacs|d>" #f))
  (when (!= (get-preference "server service delete-account") "on")
    (check= (rcarol '(remote-delete-account "rt-carol"))
            "account deletion is not allowed")
    (check= (rcarol '(remote-deletion-plan "rt-carol"))
            "account deletion is not allowed"))
  (check= (ranon '(remote-delete-account "rt-carol"))
          '(:error "user not logged"))
  (check= (rroot '(remote-delete-account "rt-root"))
          '(:error "admin cannot delete own account"))
  (check= (rroot '(remote-deletion-plan "rt-dave"))
          '((("d.tm" "file")) ()))
  (check= (rroot '(remote-delete-account "rt-dave")) "done")
  (check-false (server-find-user "rt-dave"))
  (check-false (server-get-user-info "rt-dave"))
  (check-true (server-user-deleted? "rt-dave"))
  (check-false (server-can-login-uid? "rt-dave"))
  (check-false (with-db (file-name->resource "~rt-dave/d.tm")))
  ;; the pseudo of a deleted user stays taken
  (check-true (server-pseudo-taken? "rt-dave"))
  (check-false (server-remove-account "rt-dave"))

  (check-group "logging out")
  (check= (rcarol '(remote-logout)) "bye")
  (check= (rcarol '(remote-logged?)) "no")
  (check-false (uid-logged? "rt-carol"))
  (check= (rcarol '(remote-logout)) '(:error "user not logged"))
  (server-login-uid "rt-carol" 9005 "rt-carol")
  (check= (rcarol '(remote-logged?)) "yes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files and directories on the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define doc1 "<TeXmacs|2.1>\n\n<\\body>\n  one\n</body>\n")
(define doc2 "<TeXmacs|2.1>\n\n<\\body>\n  two\n</body>\n")
(define doc3 "<TeXmacs|2.1>\n\n<\\body>\n  three\n</body>\n")

(define notes "loophost/~rt-alice/notes.tm")

(define (test-files)
  (check-group "creating files")
  (with r (with-db (server-file-create "rt-alice" notes doc1 "first"))
    (check= (car r) :created)
    (with rid (cadr r)
      (check= (with-db (file-name->resource "~rt-alice/notes.tm")) rid)
      (check= (with-db (search-remote-identifier notes)) rid)
      (check= (with-db (resource->file-name rid)) "~rt-alice/notes.tm")
      (check= (field rid "type") '("file"))
      (check= (field rid "name") '("notes.tm"))
      (check= (field rid "owner") '("rt-alice"))
      (check= (field rid "version-nr") '("1"))
      (check= (field rid "version-msg") '("first"))
      (check= (field rid "version-by") '("rt-alice"))
      (check= (field rid "dir") (list (with-db (file-name->resource "~rt-alice"))))
      (with f (with-db (repository-get rid))
        (check-true (url-exists? f))
        (check-true (url-descends? (system->url f) repo))
        (check= (string-load f) doc1))))
  (check= (with-db (server-file-create "rt-alice" notes doc1 #f))
          '(:error "Error: file already exists"))
  (check= (with-db (server-file-create "rt-alice" "loophost/~rt-alice/no/x.tm"
                                       doc1 #f))
          '(:error "Error: directory does not exist"))
  (check= (with-db (server-file-create #f "loophost/~rt-alice/x.tm" doc1 #f))
          '(:error "Error: not logged in"))
  (check= (with-db (server-file-create "rt-bob" "loophost/~rt-alice/x.tm"
                                       doc1 #f))
          '(:error "Error: directory write access required"))
  (check-false (with-db (file-name->resource "~rt-alice/x.tm")))
  (check-false (with-db (file-name->resource "~rt-nobody/notes.tm")))

  (check-group "loading files")
  (check= (with-db (server-file-load "rt-alice" notes)) (list :loaded doc1))
  (check= (with-db (server-file-load "rt-bob" notes))
          '(:error "Error: read access denied"))
  (check= (with-db (server-file-load #f notes))
          '(:error "Error: not logged in"))
  (check= (with-db (server-file-load "rt-alice" "loophost/~rt-alice/x.tm"))
          '(:error "Error: file does not exist"))

  (check-group "saving versions")
  (let* ((rid (with-db (file-name->resource "~rt-alice/notes.tm")))
         (vid (car (field rid "version-list"))))
    (check= (field vid "type") '("version-list"))
    (check= (field vid "name") '("notes.tm-versions"))
    (check= (field vid "version-current") '("1"))
    (check= (with-db (server-file-save "rt-alice" notes doc1 "same"))
            (list :unchanged rid))
    (check= (with-db (server-file-save "rt-bob" notes doc2 "bob"))
            '(:error "Error: write access denied"))
    (check= (with-db (server-file-save #f notes doc2 #f))
            '(:error "Error: not logged in"))
    ;; a missing file
    (check= (with-db (server-file-save "rt-alice" "loophost/~rt-alice/x.tm"
                                       doc2 #f))
            '(:error "Error: file does not exist"))
    (check= (ralice '(remote-file-save "loophost/~rt-alice/x.tm" "<TeXmacs|x>" #f))
            '(:error "Error: file does not exist"))
    (with r (with-db (server-file-save "rt-alice" notes doc2 "second"))
      (check= (car r) :created)
      (with rid2 (cadr r)
        (check-false (== rid2 rid))
        (check= (with-db (file-name->resource "~rt-alice/notes.tm")) rid2)
        (check= (with-db (db-get-entry rid)) '())
        (check= (field rid2 "version-nr") '("2"))
        ;; the message and the author are those of the new version
        (check= (field rid2 "version-msg") '("second"))
        (check= (field rid2 "version-by") '("rt-alice"))
        (check= (field rid2 "version-list") (list vid))
        (check= (field vid "version-current") '("2"))
        (check= (with-db (server-file-load "rt-alice" notes))
                (list :loaded doc2))
        (check= (with-db ((priv '(server server-tmfs) 'version-get-versions)
                          vid))
                (list rid rid2))
        ;; the first version is still in the repository
        (check= (with-db (with-time :always (string-load (repository-get rid))))
                doc1)
        (with info (with-db ((priv '(server server-tmfs) 'version-get-info)
                             rid2))
          (check= (car info) rid2)
          (check= (cddr info) '("~rt-alice/notes.tm" "rt-alice" "second"))
          (check= (with-db ((priv '(server server-tmfs) 'version->file-name)
                            rid2))
                  (string-append "time=" (cadr info) "/~rt-alice/notes.tm"))))))

  (check-group "permissions of files")
  (let* ((rid (with-db (file-name->resource "~rt-alice/notes.tm"))))
    (check-false (with-db (server-resource-shared? rid "rt-alice")))
    (with-db (db-set-field rid "readable" '("all")))
    (check-false (with-db (server-resource-shared? rid "rt-alice")))
    (check= (with-db (server-file-load "rt-bob" notes)) (list :loaded doc2))
    (with-db (db-set-field rid "readable" '("rt-bob")))
    (check-true (with-db (server-resource-shared? rid "rt-alice")))
    (check= (with-db (server-file-load "rt-bob" notes)) (list :loaded doc2))
    (check= (with-db (server-file-load "rt-carol" notes))
            '(:error "Error: read access denied"))
    (check= (with-db (server-file-save "rt-bob" notes doc3 "bob"))
            '(:error "Error: write access denied"))
    (with-db (db-set-field rid "writable" '("rt-bob")))
    (with r (with-db (server-file-save "rt-bob" notes doc3 "by bob"))
      (check= (car r) :created)
      (with rid3 (cadr r)
        ;; the properties of the previous version are kept
        (check= (field rid3 "readable") '("rt-bob"))
        (check= (field rid3 "writable") '("rt-bob"))
        (check= (field rid3 "owner") '("rt-alice"))
        (check= (field rid3 "version-nr") '("3"))
        (check= (field rid3 "version-by") '("rt-bob"))
        (check= (field rid3 "version-msg") '("by bob"))
        (with-db (server-remove-user-from-acls rid3 "rt-bob"))
        (check= (field rid3 "readable") '())
        (check= (field rid3 "writable") '())
        (check= (field rid3 "owner") '("rt-alice"))
        (check= (with-db (server-file-load "rt-bob" notes))
                '(:error "Error: read access denied")))))

  (check-group "directories")
  (with r (with-db (server-dir-create "rt-alice" "loophost/~rt-alice/sub"))
    (check= (car r) :created)
    (with did (cadr r)
      (check= (with-db (file-name->resource "~rt-alice/sub")) did)
      (check= (field did "type") '("dir"))
      (check= (field did "owner") '("rt-alice"))
      (check= (field did "dir") (list (with-db (file-name->resource "~rt-alice"))))
      (with-db (db-set-field did "readable" '("rt-bob")))
      ;; the files of a directory inherit its permissions
      (with-db (server-file-create "rt-alice" "loophost/~rt-alice/sub/x.tm"
                                   doc1 #f))
      (with xid (with-db (file-name->resource "~rt-alice/sub/x.tm"))
        (check= (field xid "readable") '("rt-bob"))
        (check= (field xid "version-msg") '())
        (check= (with-db (resource->file-name xid)) "~rt-alice/sub/x.tm"))
      (check= (with-db (server-file-load "rt-bob" "loophost/~rt-alice/sub/x.tm"))
              (list :loaded doc1))))
  (check= (with-db (server-dir-create "rt-alice" "loophost/~rt-alice/sub"))
          '(:error "Error: directory already exists"))
  (check= (with-db (server-dir-create "rt-bob" "loophost/~rt-alice/sub2"))
          '(:error "Error: directory write access required"))
  (check= (with-db (server-dir-create "rt-alice" "loophost/~rt-alice/a/b"))
          '(:error "Error: directory does not exist"))
  (check= (with-db (server-dir-create #f "loophost/~rt-alice/sub2"))
          '(:error "Error: not logged in"))
  (with-db (server-file-create "rt-alice" "loophost/~rt-alice/sub/y.tm" doc2 #f))
  (with yid (with-db (file-name->resource "~rt-alice/sub/y.tm"))
    (check= (field yid "readable") '("rt-bob"))
    (with-db (db-set-field yid "readable" '()))
    (check= (field yid "readable") '()))

  (check-group "directory listings")
  (with r (with-db (server-dir-load "rt-alice" "loophost/~rt-alice"))
    (check= (car r) :loaded)
    (check= (map car (cadr r)) '("notes.tm" "sub"))
    (check= (map cadr (cadr r)) '("~rt-alice/notes.tm" "~rt-alice/sub"))
    (check= (map caddr (cadr r)) '(#f #t))
    (with props (cadddr (car (cadr r)))
      (check= (assoc-ref props "name") '("notes.tm"))
      (check= (assoc-ref props "type") '("file"))
      ;; the users are given by their pseudos
      (check= (assoc-ref props "owner") '("rt-alice"))))
  (with r (with-db (server-dir-load "rt-bob" "loophost/~rt-alice/sub"))
    (check= (car r) :loaded)
    (check= (map car (cadr r)) '("x.tm")))
  (check= (map car (cadr (with-db (server-dir-load "rt-alice"
                                                   "loophost/~rt-alice/sub"))))
          '("x.tm" "y.tm"))
  (check= (with-db (server-dir-load "rt-bob" "loophost/~rt-alice"))
          '(:error "Error: read access required"))
  (check= (with-db (server-dir-load "rt-alice" "loophost/~rt-alice/none"))
          '(:error "Error: directory does not exist"))
  (check= (with-db (server-dir-load #f "loophost/~rt-alice"))
          '(:error "Error: not logged in"))

  (check-group "removing files and directories")
  (check= (with-db (server-dir-remove "rt-alice" "loophost/~rt-alice/sub" #f))
          '(:error "Error: directory 'loophost/~rt-alice/sub' is non empty"))
  (check= (with-db (server-dir-remove "rt-bob" "loophost/~rt-alice/sub" #t))
          '(:error "Error: write access denied for 'loophost/~rt-alice/sub'"))
  (check= (with-db (server-dir-remove "rt-alice" "loophost/~rt-alice/none" #t))
          '(:error "Error: directory 'loophost/~rt-alice/none' does not exist"))
  (with did (with-db (file-name->resource "~rt-alice/sub"))
    (check= (with-db (server-dir-remove "rt-alice" "loophost/~rt-alice/sub" #t))
            (list :removed did)))
  (check-false (with-db (file-name->resource "~rt-alice/sub")))
  (check-false (with-db (file-name->resource "~rt-alice/sub/x.tm")))
  (check-false (with-db (file-name->resource "~rt-alice/sub/y.tm")))
  (with-db (server-dir-create "rt-alice" "loophost/~rt-alice/empty"))
  (with did (with-db (file-name->resource "~rt-alice/empty"))
    (check= (with-db (server-dir-remove "rt-alice" "loophost/~rt-alice/empty" #f))
            (list :removed did)))
  (with-db (server-file-create "rt-alice" "loophost/~rt-alice/tmp.tm" doc1 #f))
  (check= (with-db (server-file-remove "rt-bob" "loophost/~rt-alice/tmp.tm"))
          '(:error "Error: write access denied for 'loophost/~rt-alice/tmp.tm'"))
  (with rid (with-db (file-name->resource "~rt-alice/tmp.tm"))
    (check= (with-db (server-file-remove "rt-alice" "loophost/~rt-alice/tmp.tm"))
            (list :removed rid)))
  (check= (with-db (server-file-remove "rt-alice" "loophost/~rt-alice/tmp.tm"))
          '(:error "Error: file 'loophost/~rt-alice/tmp.tm' does not exist"))
  (check= (with-db (server-file-remove #f "loophost/~rt-alice/tmp.tm"))
          '(:error "Error: not logged in")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Services for files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-future)
  (number->string (+ (current-time) 100)))

(define (test-file-services)
  (check-group "remote identifiers")
  (with rid (with-db (file-name->resource "~rt-alice/notes.tm"))
    (check= (ralice `(remote-identifier ,notes)) rid)
    (check-false (rbob `(remote-identifier ,notes)))
    (check-false (ralice '(remote-identifier "loophost/~rt-alice/none.tm")))
    (check= (ranon `(remote-identifier ,notes)) '(:error "Error: not logged in"))
    (with t (in-future)
      (check= (ralice `(remote-identifier
                        ,(string-append "loophost/time=" t "/~rt-alice/notes.tm")))
              (list rid t)))
    ;; the client strips the prefix of the url
    (check= (with-cont (lambda (k) (remote-identifier 9101 (string-append
                                                           "tmfs://remote-file/"
                                                           notes) k)))
            rid)
    (check= (with-cont (lambda (k)
                         (with-remote-identifier r 9101
                           (string->url (string-append "tmfs://remote-file/" notes))
                           (k r))))
            rid))

  (check-group "remote file services")
  (check= (ralice '(remote-file-create "loophost/~rt-alice/s.tm" "<TeXmacs|s>" #f))
          "<TeXmacs|s>")
  (check= (ralice '(remote-file-load "loophost/~rt-alice/s.tm")) "<TeXmacs|s>")
  (check= (ralice '(remote-file-create "loophost/~rt-alice/s.tm" "<TeXmacs|s>" #f))
          '(:error "Error: file already exists"))
  (check= (rbob '(remote-file-load "loophost/~rt-alice/s.tm"))
          '(:error "Error: read access denied"))
  (check= (ralice '(remote-file-save "loophost/~rt-alice/s.tm" "<TeXmacs|t>" "m"))
          "<TeXmacs|t>")
  (check= (ralice '(remote-file-load "loophost/~rt-alice/s.tm")) "<TeXmacs|t>")
  (check= (rbob '(remote-file-save "loophost/~rt-alice/s.tm" "<TeXmacs|u>" #f))
          '(:error "Error: write access denied"))
  (with past (string-append "loophost/time=" (in-future) "/~rt-alice/s.tm")
    (check= (ralice `(remote-file-save ,past "<TeXmacs|u>" #f))
            '(:error "Error: cannot modify past"))
    (check= (ralice `(remote-file-create ,past "<TeXmacs|u>" #f))
            '(:error "Error: cannot modify past"))
    (check= (ralice `(remote-file-remove ,past))
            '(:error "Error: cannot modify past"))
    (check= (ralice `(remote-dir-create ,past))
            '(:error "Error: cannot modify past"))
    (check= (ralice `(remote-dir-remove ,past))
            '(:error "Error: cannot modify past")))
  (with l (ralice '(remote-get-versions "loophost/~rt-alice/s.tm"))
    (check-true (list? l))
    ;; both versions, though the first one was replaced at once (a version
    ;; replaced within 5 seconds was left out)
    (check= (length l) 2)
    (check= (map fifth l) '(#f "m"))
    (check-true (list-and (map (lambda (v) (== (third v) "~rt-alice/s.tm")) l)))
    (check-true (list-and (map (lambda (v) (== (fourth v) "rt-alice")) l))))
  (check= (ralice '(remote-get-versions "loophost/~rt-alice/none.tm"))
          '(:error "Error: file does not exist"))
  (check= (ranon '(remote-get-versions "loophost/~rt-alice/s.tm"))
          '(:error "Error: not logged in"))
  ;; and none of them for who cannot read the file
  (check= (rbob '(remote-get-versions "loophost/~rt-alice/s.tm")) '())
  (check= (ralice '(remote-file-remove "loophost/~rt-alice/s.tm")) "removed")
  (check= (ralice '(remote-file-load "loophost/~rt-alice/s.tm"))
          '(:error "Error: file does not exist"))
  (check= (ralice '(remote-file-remove "loophost/~rt-alice/s.tm"))
          '(:error "Error: file 'loophost/~rt-alice/s.tm' does not exist"))

  (check-group "remote directory services")
  (check= (ralice '(remote-dir-create "loophost/~rt-alice/d")) '())
  (check= (ralice '(remote-dir-create "loophost/~rt-alice/d"))
          '(:error "Error: directory already exists"))
  (check= (ralice '(remote-file-create "loophost/~rt-alice/d/f.tm" "<TeXmacs|f>" #f))
          "<TeXmacs|f>")
  (with l (ralice '(remote-dir-load "loophost/~rt-alice/d"))
    (check= (map car l) '("f.tm"))
    (check= (map cadr l) '("~rt-alice/d/f.tm")))
  (check= (rbob '(remote-dir-load "loophost/~rt-alice/d"))
          '(:error "Error: read access required"))
  (check= (ranon '(remote-dir-load "loophost/~rt-alice/d"))
          '(:error "Error: not logged in"))
  ;; the removal of a directory removes its files
  (check= (ralice '(remote-dir-remove "loophost/~rt-alice/d")) "removed")
  (check-false (with-db (file-name->resource "~rt-alice/d/f.tm")))
  (check= (ralice '(remote-dir-load "loophost/~rt-alice/d"))
          '(:error "Error: directory does not exist"))

  (check-group "service errors")
  ;; a service called with wrong arguments answers with an error
  (with r (ralice '(remote-file-load))
    (check= (car r) :error)
    (check-true (string? (cadr r))))
  (with r (ralice '(remote-file-load "a" "b" "c"))
    (check= (car r) :error))
  (check= (ralice '(remote-no-such-service 1))
          '(:error "invalid command 'remote-no-such-service'"))
  (check= (ralice '(42)) '(:error "invalid command"))
  (check= (ralice "text") '(:error "invalid command")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Services for database entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-db-services)
  (check-group "fields")
  (with rid (with-db (file-name->resource "~rt-alice/notes.tm"))
    (check= (ralice `(remote-get-field ,rid "name")) '("notes.tm"))
    (check= (rbob `(remote-get-field ,rid "name"))
            '(:error "Error: read access required for field"))
    (check= (ranon `(remote-get-field ,rid "name"))
            '(:error "Error: read access required for field"))
    (check= (ralice `(remote-set-field ,rid "title" ("Notes"))) #t)
    (check= (field rid "title") '("Notes"))
    (check= (rbob `(remote-set-field ,rid "title" ("Bob")))
            '(:error "Error: write access required for field"))
    (check= (ranon `(remote-set-field ,rid "title" ("Anon")))
            '(:error "Error: not logged in"))
    (check= (ralice `(remote-set-field (,rid ,(in-future)) "title" ("Later")))
            '(:error "Error: cannot rewrite history"))
    (check= (field rid "title") '("Notes"))
    (check= (ralice `(remote-get-field (,rid ,(in-future)) "title")) '("Notes"))
    (check-true (in? "title" (ralice `(remote-get-attributes ,rid))))
    (check-true (in? "version-list" (ralice `(remote-get-attributes ,rid))))
    (check= (rbob `(remote-get-attributes ,rid))
            '(:error "Error: read access required for entry"))
    (check= (assoc-ref (ralice `(remote-get-entry ,rid)) "title") '("Notes"))
    (check= (rbob `(remote-get-entry ,rid))
            '(:error "Error: read access required for entry"))
    ;; anybody reads an entry readable by all
    (with-db (db-set-field rid "readable" '("all")))
    (check= (ranon `(remote-get-field ,rid "title")) '("Notes"))
    (check= (rbob `(remote-get-field ,rid "title")) '("Notes"))
    (with-db (db-remove-field rid "readable"))
    (check= (with-cont (lambda (k) (remote-get-field 9101 rid "title" k)))
            '("Notes"))
    (check= (with-cont (lambda (k) (with-remote-get-field v 9101 rid "name" (k v))))
            '("notes.tm"))
    (remote-set-field 9101 rid "title" '("Notes 2"))
    (loop-pump)
    (check= (field rid "title") '("Notes 2")))

  (check-group "entries")
  (with id (ralice '(remote-create-entry (("type" "note") ("name" "n1"))))
    (check-true (string? id))
    (check= (field id "owner") '("rt-alice"))
    (check= (field id "name") '("n1"))
    (check= (ralice `(remote-set-entry ,id (("type" "note") ("name" "n2")
                                            ("owner" "rt-alice"))))
            #t)
    (check= (field id "name") '("n2"))
    (check= (rbob `(remote-set-entry ,id (("type" "note") ("name" "n3"))))
            '(:error "Error: write access required for entry"))
    (check= (ranon `(remote-set-entry ,id (("type" "note"))))
            '(:error "Error: not logged in"))
    (check= (ralice '(remote-search (("type" "note")))) (list id))
    (check= (rbob '(remote-search (("type" "note")))) '())
    (check= (with-cont (lambda (k) (remote-search 9101 '(("type" "note")) k)))
            (list id))
    (with e (with-cont (lambda (k) (with-remote-create-entry e 9102
                                     '(("type" "note") ("name" "b1")) (k e))))
      (check= (with-db (db-search '(("type" "note") ("name" "b1")))) (list e))
      (check= (field e "owner") '("rt-bob"))
      (check= (rbob '(remote-search (("type" "note")))) (list e))))
  (check= (ranon '(remote-create-entry (("type" "note"))))
          '(:error "Error: not logged in"))
  (check= (ranon '(remote-search (("type" "note")))) '())

  (check-group "information about users")
  (check= (ralice '(remote-search-user (("pseudo" "rt-bob")))) '("rt-bob"))
  (check= (ralice '(remote-search-user (("pseudo" "rt-nobody")))) '())
  (check= (ralice '(remote-get-user-pseudo "rt-bob")) "rt-bob")
  (check= (ralice '(remote-get-user-name "rt-bob")) "Bob B")
  (check= (ralice '(remote-get-user-name ("rt-bob" "rt-alice")))
          '("Bob B" "Alice A"))
  (check-false (ralice '(remote-get-user-name "rt-nobody")))
  (check-false (ralice '(remote-get-user-name 42)))
  (check= (ranon '(remote-get-user-pseudo "rt-root")) "rt-root")
  (check= (with-cont (lambda (k) (with-remote-get-user-name n 9101 "rt-root" (k n))))
          "Root R")
  (check= (with-cont (lambda (k) (remote-search-user 9101 '(("name" "Bob B")) k)))
          '("rt-bob")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chat rooms, messages and sharing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (messages-to crid)
  (with-db (db-search `(("type" "chat-message") ("to" ,crid)))))

(define (test-chat)
  (check-group "chat rooms")
  (with crid (with-db (server-chat-room-create "rt-alice" "rt-room"))
    (check= (field crid "type") '("chat-room"))
    (check= (field crid "name") '("rt-room"))
    (check= (field crid "owner") '("rt-alice"))
    (check= (field crid "readable") '("all"))
    (check= (field crid "writable") '("all"))
    (check= (with-db (chat-room-id "rt-room")) crid)
    (check= (with-db (search-remote-identifier
                      (string->url "tmfs://chat/loophost/rt-room")))
            crid))
  (check-false (with-db (chat-room-id "rt-none")))
  (with crid (ralice '(remote-chat-room-create "rt-room2"))
    (check= (with-db (chat-room-id "rt-room2")) crid))
  (with l (ralice '(remote-list-chat-rooms))
    (check= (sorted (map car l)) '("rt-room" "rt-room2"))
    (check-true (list-and (map (lambda (e) (string-number? (cadr e))) l))))
  (check= (rbob '(remote-list-chat-rooms)) '())
  (check= (rbob '(remote-chat-room-open "rt-room")) '(#t ()))
  (check= (rbob '(remote-chat-room-open "rt-none"))
          '(:error "Error: unknown chat room"))
  (with crid (with-db (chat-room-id "rt-room2"))
    (with-db (db-set-field crid "readable" '("rt-alice"))
             (db-set-field crid "writable" '("rt-alice")))
    (check= (rbob '(remote-chat-room-open "rt-room2"))
            '(:error "Error: access to chat room denied"))
    (check= (ralice '(remote-chat-room-open "rt-room2")) '(#t ()))
    ;; a room readable by all and writable by its owner is read only
    (with-db (db-set-field crid "readable" '("all")))
    (check= (rbob '(remote-chat-room-open "rt-room2")) '(#f ())))

  (check-group "chat messages")
  (check= (rbob '(remote-send-message "rt-room" "send-document"
                                      (document "hello")))
          #t)
  (with crid (with-db (chat-room-id "rt-room"))
    (with mids (messages-to crid)
      (check= (length mids) 1)
      (with mid (car mids)
        (check= (field mid "action") '("send"))
        (check= (field mid "from") '("rt-bob"))
        (with msg (with-db (chat-message-retrieve mid))
          (check= (msg-action msg) "send")
          (check= (msg-pseudo msg) "rt-bob")
          (check= (msg-full-name msg) "Bob B")
          (check= (msg-doc msg) '(document "hello"))
          (check= (msg-to msg) "rt-alice")
          (check-true (string-number? (msg-date msg)))
          ;; the client of rt-bob, which opened the room, receives it
          (check= (loop-received 'client 9002 'chat-room-receive)
                  `((chat-room-receive "rt-room" ,msg)))
          (check= (loop-received 'client 9001 'chat-room-receive) '())
          (check= (cadr (rbob '(remote-chat-room-open "rt-room"))) (list msg))
          (check= (car (ralice '(remote-chat-room-open "rt-room"))) #t)))))
  ;; messages to a read only room are ignored
  (check= (rbob '(remote-send-message "rt-room2" "send-document" (document "x")))
          #t)
  (check= (messages-to (with-db (chat-room-id "rt-room2"))) '())
  (check= (ralice '(remote-send-message "rt-room2" "send-document"
                                        (document "mine")))
          #t)
  (check= (length (messages-to (with-db (chat-room-id "rt-room2")))) 1)
  (with-db (server-remove-user-chat-messages "rt-bob"))
  (check= (messages-to (with-db (chat-room-id "rt-room"))) '())
  (check= (with-db (chat-messages-sent "rt-alice" (lambda (m) #t)))
          (list (with-db (chat-message-retrieve
                          (car (messages-to (with-db (chat-room-id "rt-room2"))))))))
  (with crid (with-db (chat-room-id "rt-room2"))
    (with-db (server-chat-room-remove crid))
    (check-false (with-db (chat-room-id "rt-room2")))
    (check= (messages-to crid) '()))

  (check-group "mail boxes")
  (check= (ralice '(remote-mail-open)) '())
  (with crid (with-db (chat-room-id "mail-rt-alice"))
    (check= (field crid "owner") '("rt-alice")))
  (check= (rbob '(remote-chat-room-open "mail-rt-alice"))
          '(:error "Error: invalid name of chat room"))
  ;; a message to a mail box which does not exist yet creates it
  (check-false (with-db (chat-room-id "mail-rt-bob")))
  (check= (ralice '(remote-send-message ("mail-rt-bob") "send-document"
                                        (document "letter")))
          #t)
  (with crid (with-db (chat-room-id "mail-rt-bob"))
    (check= (field crid "owner") '("rt-bob"))
    (check= (length (messages-to crid)) 1))
  (with l (rbob '(remote-mail-open))
    (check= (length l) 1)
    (check= (msg-doc (car l)) '(document "letter"))
    (check= (msg-pseudo (car l)) "rt-alice"))
  ;; the mail boxes are not listed with the chat rooms
  (check= (map car (rbob '(remote-list-chat-rooms))) '())

  (check-group "notifications of messages")
  ;; rt-bob is logged in: his client is notified of the message
  (with l (loop-received 'client 9002 'client-push-notifications)
    (check= (length l) 1)
    (with (head nid entry) (car l)
      (check= (assoc-ref entry "kind") '("message"))
      (check= (assoc-ref entry "owner") '("rt-bob"))
      (check= (notification-count 9102 'message) 1)
      (check-true (has-notifications? 9102 'message))
      (check= (notification-kind 9102 nid) 'message)
      (check= (notification-payload 9102 nid) entry)
      (check-true (procedure? (notification-action 9102 nid)))
      (with pending (rbob '(remote-pending-notifications))
        (check= (map car pending) (list nid))
        (check= (cadr (car pending)) entry))))
  (check= (ralice '(remote-pending-notifications)) '())
  (check= (rbob '(remote-ack-notifications all)) "done")
  (check= (rbob '(remote-pending-notifications)) '())
  (clear-notifications 9102 'message)
  (check= (notification-count 9102 'message) 0)
  ;; a notification for a user who is not logged in waits on the server
  (server-logout-client 9005)
  (check= (rbob '(remote-send-message ("mail-rt-carol") "send-document"
                                      (document "for carol")))
          #t)
  (check= (loop-received 'client 9005 'client-push-notifications) '())
  (server-login-uid "rt-carol" 9005 "rt-carol")
  (with pending (rcarol '(remote-pending-notifications))
    (check= (length pending) 1)
    (client-sync-remote-notifications 9105)
    (loop-pump)
    (check= (notification-count 9105 'message) 1))
  (check= (rcarol '(remote-ack-notifications all)) "done")

  (check-group "sharing")
  (check= (ralice '(remote-send-message ("mail-rt-bob" "mail-rt-carol") "share"
                                        "tmfs://chat/loophost/rt-room"))
          #t)
  (with crid (with-db (chat-room-id "rt-room"))
    (with mid (car (with-db (db-search `(("type" "chat-message")
                                         ("action" "share")
                                         ("to" ,(chat-room-id "mail-rt-bob"))))))
      (check= (field mid "resource-id") (list crid))
      (check= (field mid "message") '("tmfs://chat/loophost/rt-room"))))
  (with l (rbob '(remote-shared))
    (check= (length l) 1)
    (check= (msg-action (car l)) "share")
    (check= (msg-pseudo (car l)) "rt-alice")
    (check= (msg-doc (car l)) "tmfs://chat/loophost/rt-room")
    (check= (msg-to (car l)) "rt-bob"))
  ;; the messages are filtered: the letter of rt-alice is not a share
  (check= (length (rbob '(remote-mail-open))) 2)
  (check= (map msg-to (with-db (resource-shared-with "rt-room" "rt-alice")))
          '("rt-bob" "rt-carol"))
  (check= (with-db (resource-shared-with "rt-other" "rt-alice")) '())
  (check= (with-db (resource-shared-with "rt-room" "rt-bob")) '())
  ;; a renamed resource is found again through its identifier
  (with crid (with-db (chat-room-id "rt-room"))
    (with-db (db-set-field crid "name" '("rt-hall")))
    (check= (ralice '(remote-chat-room-messages-reset)) "ok")
    (check= (msg-doc (car (rbob '(remote-shared))))
            "tmfs://chat/loophost/rt-hall")
    (with-db (db-set-field crid "name" '("rt-room")))
    (chat-room-messages-reset))
  ;; a shared remote file is found through its identifier too, also after
  ;; it has been renamed
  (let* ((rid (with-db (file-name->resource "~rt-alice/notes.tm")))
         (url (string-append "tmfs://remote-file/" notes))
         (mid (with-db (remote-send "rt-alice" "mail-rt-bob" "share" url))))
    (check= (with-db (search-remote-identifier url)) rid)
    (check= (field mid "resource-id") (list rid))
    (check= (field mid "message") (list url))
    (with-db (db-set-field rid "name" '("notes2.tm")))
    (check= (msg-doc (with-db (chat-message-retrieve mid)))
            "tmfs://remote-file/loophost/~rt-alice/notes2.tm")
    (with-db (db-set-field rid "name" '("notes.tm")))
    (check= (msg-doc (with-db (chat-message-retrieve mid))) url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live documents on the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The live documents of the server and of the client share the tables of
;; utils/relate in this process: the host of the live documents of the
;; server is not a host of the client, and the live-modify call backs which
;; the server sends to the clients are logged but not run.
(define lid "tmfs://live/srvhost/rt-live")

(define (live-doc) (tm->stree (live-current-document lid)))

(define (test-live)
  (check-group "opening live documents")
  (set! loop-blocked '(live-modify))
  (check-false (live-find-server lid))
  (check= (ralice `(live-exists? ,lid)) #f)
  (with r (ralice `(live-open ,lid))
    (check= (length r) 2)
    (check= (cadr r) '(document ""))
    (check= (car r) (live-current-state lid))
    (check= (live-get-remote-state lid 9001) (car r)))
  (check= (ralice `(live-exists? ,lid)) #t)
  (with rid (with-db (search-remote-identifier (string->url lid)))
    (check= (field rid "type") '("live"))
    (check= (field rid "name") '("rt-live"))
    (check= (field rid "owner") '("rt-alice"))
    (check= (length (field rid "location")) 1))
  (with l (ralice '(remote-list-live))
    (check= (map car l) '("rt-live"))
    (check-true (string-number? (cadr (car l)))))
  (check= (rbob '(remote-list-live)) '())
  (check= (live-get-connections lid) '(9001))

  (check-group "modifying live documents")
  (let* ((s0 (live-current-state lid))
         (s1 "rt-state-1"))
    (check= (ralice `(live-modify ,lid ((assign (0) "hello")) ,s0 ,s1)) #t)
    (check= (live-doc) '(document "hello"))
    (check= (live-current-state lid) s1)
    (check= (live-get-remote-state lid 9001) s1)
    ;; a modification of an older state is refused
    (check= (ralice `(live-modify ,lid ((assign (0) "again")) ,s0 "rt-state-x"))
            #f)
    (check= (live-doc) '(document "hello")))
  (with r (rbob `(live-open ,lid))
    (check= (cadr r) '(document "hello"))
    (check= (car r) "rt-state-1"))
  (check= (sort (live-get-connections lid) <) '(9001 9002))
  (check= (live-remote-connections 9002) (list lid))
  (check= (rbob `(live-modify ,lid ((assign (0) "bob")) "rt-state-1" "rt-state-2"))
          #t)
  (check= (live-doc) '(document "bob"))
  ;; the other client is sent the modification
  (check= (loop-received 'client 9001 'live-modify)
          `((live-modify ,lid ((assign (0) "bob")) "rt-state-1" "rt-state-2")))
  (with rid (with-db (search-remote-identifier (string->url lid)))
    (with-db (db-set-field rid "writable" '("rt-alice")))
    (check= (rbob `(live-modify ,lid ((assign (0) "no")) "rt-state-2" "rt-state-3"))
            '(:error "Error: write access denied"))
    (with-db (db-set-field rid "readable" '("rt-alice")))
    (check= (rcarol `(live-open ,lid)) '(:error "Error: read access denied")))
  (check= (live-doc) '(document "bob"))

  (check-group "saving live documents")
  (with live-load (priv '(server server-live) 'live-load)
    ;; the document is saved when a client disconnects
    (check= (with-db (live-load lid)) '(document ""))
    (server-remove 9002)
    (check= (with-db (live-load lid)) '(document "bob"))
    (check= (live-get-connections lid) '(9001))
    (check= (live-remote-connections 9002) '())
    (server-add 9002)
    (with-db ((priv '(server server-live) 'live-save) lid '(document "saved")))
    (check= (with-db (live-load lid)) '(document "saved"))
    (check-false (with-db (live-load "tmfs://live/srvhost/rt-none"))))
  (set! loop-blocked '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Synchronization of directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sync-remote "tmfs://remote-dir/loophost/~rt-alice/synced")

(define (sync-status local)
  (with-cont (lambda (k) (client-sync-status local (string->url sync-remote) k))))

(define (status-names l)
  ;; (command tail of the local name) for each line of a status list
  (map (lambda (line)
         (list (car line)
               (check-unix (url->string (url-delta (url-append (tmp "sync") "x")
                                                   (system->url (third line)))))))
       l))

(define (test-sync-status)
  (check-group "status of synchronized files")
  (with get-sync-status (priv '(client client-sync) 'get-sync-status)
    (define (status dir? date date* rid rid*)
      (with r (get-sync-status (list dir? (string->url "/l/a") "lid" date date* rid*)
                               (string->url "tmfs://remote-file/h/~u/a") rid)
        (and r (car r))))
    (check-false (status #f "5" "5" "r" "r"))
    (check-false (status #t "6" "5" "s" "r"))
    (check= (status #f "5" "5" #f "r") "local-delete")
    (check= (status #f #f "5" "r" "r") "remote-delete")
    (check= (status #f "5" "5" "s" "r") "download")
    (check= (status #f "6" "5" "r" "r") "upload")
    (check= (status #f #f #f "r" #f) "download")
    (check= (status #f "5" #f #f #f) "upload")
    (check= (status #f "6" "5" "s" "r") "conflict**")
    (check= (status #f #f "5" "s" "r") "conflict-*")
    (check= (status #f "6" "5" #f "r") "conflict*-")
    (with r (get-sync-status (list #f (string->url (check-unix-abs "l/a")) "lid" "5" #f #f)
                             (string->url "tmfs://remote-file/h/~u/a") #f)
      (check= (list (car r) (cadr r) (check-unix (caddr r)) (cadddr r)
                    (list-ref r 4) (list-ref r 5))
              (list "upload" #f (check-abs "l/a") "lid"
                    "tmfs://remote-file/h/~u/a" #f))))
  (with requalify-deleted (priv '(client client-sync) 'requalify-deleted)
    (let* ((del '("local-delete" #t "/l/d" "i1" "tmfs://remote-dir/h/d" "r1"))
           (sub '("download" #f "/l/d/f" "i2" "tmfs://remote-file/h/d/f" "r2"))
           (rdel '("remote-delete" #t "/l/d" "i1" "tmfs://remote-dir/h/d" "r1"))
           (subdir '("download" #t "/l/d/e" "i3" "tmfs://remote-dir/h/d/e" "r3")))
      (check= (requalify-deleted del (list del sub))
              (cons "conflict*-" (cdr del)))
      (check= (requalify-deleted del (list del)) del)
      (check= (requalify-deleted rdel (list rdel subdir))
              (cons "conflict-*" (cdr rdel)))
      (check= (requalify-deleted rdel (list rdel)) rdel)
      ;; a removed directory whose remote files changed
      (check= (requalify-deleted rdel (list rdel sub))
              (cons "conflict-*" (cdr rdel)))
      (check= (requalify-deleted sub (list del sub)) sub)))
  (with t (make-ahash-table)
    (ahash-set! t "/l/a" "Local")
    (ahash-set! t "/l/b" "Remote")
    (ahash-set! t "/l/c" "Keep")
    (check= (car (requalify-conflicting '("conflict*-" #f "/l/a" "i" "r" #f) t))
            "upload")
    (check= (car (requalify-conflicting '("conflict-*" #f "/l/a" "i" "r" #f) t))
            "remote-delete")
    (check= (car (requalify-conflicting '("conflict-*" #f "/l/b" "i" "r" #f) t))
            "download")
    (check= (car (requalify-conflicting '("conflict*-" #f "/l/b" "i" "r" #f) t))
            "local-delete")
    (check= (car (requalify-conflicting '("conflict**" #f "/l/c" "i" "r" #f) t))
            "conflict**")
    (check= (car (requalify-conflicting '("conflict**" #f "/l/z" "i" "r" #f) t))
            "conflict**")
    (check= (car (requalify-conflicting '("upload" #f "/l/a" "i" "r" #f) t))
            "upload"))
  (check= (filter-status-list '(("upload" 1) ("download" 2) ("conflict**" 3)
                                ("conflict-*" 4))
                              "conflict")
          '(("conflict**" 3) ("conflict-*" 4)))

  (check-group "files to be synchronized")
  (with dont-sync? (priv '(client client-sync) 'dont-sync?)
    (check= (map (lambda (s) (dont-sync? (string->url s)))
                 '("/l/a.tm" "/l/.svn" "/l/#a#" "/l/svn-x" "/l/a.tm~" "/l/a.aux"
                   "/l/a.bak" "/l/a.bbl" "/l/a.blg" "/l/a.log" "/l/a.tmp"))
            '(#f #t #t #t #t #t #t #t #t #t #t)))
  (with local (tmp "sync")
    (system-mkdir local)
    (system-mkdir (url-append local "sub"))
    (string-save "<TeXmacs|a>" (url-append local "a.tm"))
    (string-save "<TeXmacs|b>" (url-append local "sub/b.tm"))
    (string-save "x" (url-append local "a.aux"))
    (string-save "x" (url-append local "a.tm~"))
    (string-save "x" (url-append local ".hidden"))
    (with l (client-sync-list local)
      (check= (car l) (list #t local))
      (check= (sorted (map (lambda (x)
                             (check-unix (url->string (url-delta (url-append local "x")
                                                                 (cadr x)))))
                           (cdr l)))
              '("a.tm" "sub" "sub/b.tm"))
      (check= (map car l) '(#t #f #t #f)))
    (check= (client-sync-list (url-append local "a.aux")) '())
    (check= (client-sync-list (url-append local "none")) '())
    (check= (client-sync-list (url-append local "a.tm"))
            (list (list #f (url-append local "a.tm"))))))

(define (test-sync)
  (check-group "uploading a directory")
  (with local (tmp "sync")
    (with l (sync-status local)
      (check= (status-names l)
              '(("upload" "../sync") ("upload" "a.tm") ("upload" "sub")
                ("upload" "sub/b.tm")))
      (check= (map fifth l)
              (list sync-remote
                    "tmfs://remote-file/loophost/~rt-alice/synced/a.tm"
                    "tmfs://remote-dir/loophost/~rt-alice/synced/sub"
                    "tmfs://remote-file/loophost/~rt-alice/synced/sub/b.tm"))
      (check= (map sixth l) '(#f #f #f #f))
      (check= (with-cont (lambda (k) (client-sync-proceed l "sync" (lambda () (k #t)))))
              #t))
    (check= (field (with-db (file-name->resource "~rt-alice/synced")) "type")
            '("dir"))
    (check= (field (with-db (file-name->resource "~rt-alice/synced/sub")) "type")
            '("dir"))
    (check= (ralice '(remote-file-load "loophost/~rt-alice/synced/a.tm"))
            "<TeXmacs|a>")
    (check= (ralice '(remote-file-load "loophost/~rt-alice/synced/sub/b.tm"))
            "<TeXmacs|b>")
    (check= (field (with-db (file-name->resource "~rt-alice/synced/a.tm"))
                   "version-msg")
            '("sync"))
    (check-false (with-db (file-name->resource "~rt-alice/synced/a.aux")))
    ;; the state of the synchronization is kept in the sync database
    (with ids (with-database (user-database "sync")
                (db-search '(("type" "sync"))))
      (check= (length ids) 4))
    (check= (sync-status local) '())
    (with l (ralice `(remote-sync-list "loophost/~rt-alice/synced"))
      (check= (map car l) '(#t #f #t #f))
      (check= (map cadr l) '("~rt-alice/synced" "~rt-alice/synced/a.tm"
                             "~rt-alice/synced/sub" "~rt-alice/synced/sub/b.tm")))
    (check= (rbob `(remote-sync-list "loophost/~rt-alice/synced"))
            '(:error "Error: read access required"))
    (check= (ralice `(remote-sync-list "loophost/~rt-alice/none")) '())
    (check= (ranon `(remote-sync-list "loophost/~rt-alice/synced"))
            '(:error "Error: not logged in"))

    (check-group "downloading changes")
    (with-db (server-file-save "rt-alice" "loophost/~rt-alice/synced/a.tm"
                               "<TeXmacs|a2>" #f))
    (with l (sync-status local)
      (check= (status-names l) '(("download" "a.tm")))
      (check= (sixth (car l))
              (with-db (file-name->resource "~rt-alice/synced/a.tm")))
      (with-cont (lambda (k) (client-sync-proceed l #f (lambda () (k #t))))))
    (check= (string-load (url-append local "a.tm")) "<TeXmacs|a2>")
    (check= (sync-status local) '())

    (check-group "removing files")
    (system-remove (url-append local "sub/b.tm"))
    (with l (sync-status local)
      (check= (status-names l) '(("remote-delete" "sub/b.tm")))
      (with-cont (lambda (k) (client-sync-proceed l #f (lambda () (k #t))))))
    (check-false (with-db (file-name->resource "~rt-alice/synced/sub/b.tm")))
    (check= (sync-status local) '())
    (with-db (server-file-remove "rt-alice" "loophost/~rt-alice/synced/a.tm"))
    (with l (sync-status local)
      (check= (status-names l) '(("local-delete" "a.tm")))
      (with-cont (lambda (k) (client-sync-proceed l #f (lambda () (k #t))))))
    (check-false (url-exists? (url-append local "a.tm")))
    (check= (sync-status local) '())
    ;; a new remote file is downloaded
    (with-db (server-file-create "rt-alice" "loophost/~rt-alice/synced/sub/c.tm"
                                 "<TeXmacs|c>" #f))
    (check= (with-cont (lambda (k) (remote-download local (string->url sync-remote)
                                                    k)))
            #t)
    (check= (string-load (url-append local "sub/c.tm")) "<TeXmacs|c>"))

  (check-group "automatic synchronization")
  (check= (client-auto-sync-list 9101) '())
  (client-auto-sync-add 9101 "/l/x" "tmfs://remote-dir/loophost/~rt-alice/x")
  (check= (client-auto-sync-list 9101)
          '(("/l/x" . "tmfs://remote-dir/loophost/~rt-alice/x")))
  (client-auto-sync-add 9101 "/l/x" "tmfs://remote-dir/loophost/~rt-alice/y")
  (check= (client-auto-sync-list 9101)
          '(("/l/x" . "tmfs://remote-dir/loophost/~rt-alice/y")))
  (client-auto-sync-remove 9101 "/l/x")
  (check= (client-auto-sync-list 9101) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Synchronization of databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The changes are dated to the second: a synchronization is refused when
;; one of the databases changed during the second of the status, so that
;; only the status is computed here.

(define (test-db-sync)
  (check-group "status of database changes")
  (with change-status (priv '(client client-db-sync) 'db-change-status)
    (let* ((v1 '(("type" "article") ("name" "n") ("title" "A")))
           (v2 '(("type" "article") ("name" "n") ("title" "B")))
           (v1* '(("type" "article") ("name" "n") ("title" "A")
                  ("owner" "u") ("date" "17"))))
      (check= (change-status `(("l1" "n" ,v1)) '() "bib")
              `(("n" "upload" "bib" "l1" ,v1)))
      (check= (change-status '() `(("r1" "n" ,v1)) "bib")
              `(("n" "download" "bib" "r1" ,v1)))
      (check= (change-status '(("l1" "n" ())) '() "bib")
              '(("n" "remote-delete" "bib")))
      (check= (change-status '() '(("r1" "n" ())) "bib")
              '(("n" "local-delete" "bib")))
      ;; owners, permissions and dates do not count
      (check= (change-status `(("l1" "n" ,v1)) `(("r1" "n" ,v1*)) "bib") '())
      (check= (change-status `(("l1" "n" ,v1)) `(("r1" "n" ,v2)) "bib")
              `(("n" "conflict" "bib" "l1" "r1" ,v1 ,v2)))
      ;; the lines are sorted by name and the last value of a name counts
      (check= (map car (change-status `(("l1" "b" ,v1) ("l2" "a" ,v1)) '() "bib"))
              '("a" "b"))
      (check= (change-status `(("l1" "n" ,v1) ("l2" "n" ,v2)) '() "bib")
              `(("n" "upload" "bib" "l2" ,v2)))
      (with t (make-ahash-table)
        (with line `("n" "conflict" "bib" "l1" "r1" ,v1 ,v2)
          (check= (db-requalify-conflicting line t)
                  `("n" "download" "bib" "r1" ,v2))
          (ahash-set! t "bib - n" "Local")
          (check= (db-requalify-conflicting line t)
                  `("n" "upload" "bib" "l1" ,v1))
          (check= (db-requalify-conflicting `("n" "conflict" "bib" "l1" "r1" () ,v2)
                                            t)
                  '("n" "remote-delete" "bib"))
          (ahash-set! t "bib - n" "Remote")
          (check= (db-requalify-conflicting `("n" "conflict" "bib" "l1" "r1" ,v1 ())
                                            t)
                  '("n" "local-delete" "bib"))
          (check= (db-requalify-conflicting '("n" "upload" "bib" "l1" ()) t)
                  '("n" "upload" "bib" "l1" ()))))
      (check= (db-filter-status-list '(("a" "upload") ("b" "download") ("c" "upload"))
                                     "upload")
              '(("a" "upload") ("c" "upload")))))

  (check-group "kinds and dates of synchronization")
  (check= (db-sync-kinds 9101) '("bib"))
  (check-true (db-sync-kind? 9101 "bib"))
  (db-sync-kind 9101 "bib" #f)
  (check-false (db-sync-kind? 9101 "bib"))
  (check= (db-sync-kinds 9101) '())
  (db-sync-kind 9101 "bib" #t)
  (check-true (db-sync-kind? 9101 "bib"))
  (check= (length (with-database (user-database "sync")
                    (db-search '(("type" "db-sync-kind")))))
          1)
  (with last-sync (priv '(client client-db-sync) 'db-last-sync)
    (check= (last-sync 9101) '("0" "0"))
    ((priv '(client client-db-sync) 'db-dub-in-sync) 9101 10 20)
    (check= (last-sync 9101) '("11" "21"))
    ((priv '(client client-db-sync) 'db-dub-in-sync) 9101 0 0)
    (check= (last-sync 9101) '("1" "1")))

  (check-group "database changes on both sides")
  (with-database (user-database "bib")
    (db-create-entry '(("type" "article") ("name" "rt-knuth") ("title" "TeX"))))
  (with-db
    (db-create-entry '(("type" "article") ("name" "rt-lamport")
                       ("title" "LaTeX") ("owner" "rt-alice")))
    (db-create-entry '(("type" "article") ("name" "rt-other")
                       ("title" "Other") ("owner" "rt-bob")))
    (db-create-entry '(("type" "note") ("name" "rt-note") ("owner" "rt-alice"))))
  (with r (ralice '(remote-db-changes ("bib") 0))
    (check= (map cadr (car (car r))) '("rt-lamport"))
    (check-true (integer? (cadr r))))
  (check= (ranon '(remote-db-changes ("bib") 0)) '(:error "Error: not logged in"))
  (with r (with-cont (lambda (k) (db-client-sync-status 9101 k)))
    (with (status ltime rtime) r
      (check= (map (lambda (l) (list (car l) (cadr l))) status)
              '(("rt-knuth" "upload") ("rt-lamport" "download")))
      (check= (assoc-ref (fifth (cadr status)) "title") '("LaTeX"))
      (check-true (integer? ltime))
      (check-true (integer? rtime))))
  (check= (ranon '(remote-db-sync () ("bib") 0)) '(:error "Error: not logged in"))
  ;; the server refuses a synchronization when it has unseen changes
  (check-false (ralice '(remote-db-sync () ("bib") 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch of services and call backs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-services
  '(server-remote-result server-remote-error new-account remote-login
    remote-logout remote-logged? confirm-pending-account remote-get-account
    remote-set-account remote-get-accounts remote-delete-account
    remote-deletion-plan remote-protocol-version remote-eval
    remote-identifier remote-get-versions remote-file-create remote-file-load
    remote-file-save remote-file-remove remote-dir-create remote-dir-load
    remote-dir-remove remote-chat-room-create remote-list-chat-rooms
    remote-chat-room-open remote-mail-open remote-shared remote-send-message
    remote-chat-room-messages-reset live-exists? live-open live-modify
    remote-list-live remote-get-field remote-set-field remote-get-attributes
    remote-get-entry remote-set-entry remote-create-entry remote-search
    remote-search-user remote-get-user-pseudo remote-get-user-name
    remote-db-changes remote-db-sync remote-sync-list remote-upload
    remote-download remote-remove-several remote-pending-notifications
    remote-ack-notifications remote-get-cache-ref))

(define test-call-backs
  '(client-remote-result client-remote-error client-account-deleted
    local-eval chat-room-receive live-modify client-push-notifications))

(define (test-dispatch)
  (check-group "dispatch tables")
  (check= (list-filter test-services
                       (lambda (s) (not (procedure?
                                         (ahash-ref service-dispatch-table s)))))
          '())
  (check-true (eq? (ahash-ref service-dispatch-table 'remote-file-load)
                   service-remote-file-load))
  (check-false (ahash-ref service-dispatch-table 'remote-no-such-service))
  (check= (list-filter test-call-backs
                       (lambda (s) (not (procedure?
                                         (ahash-ref call-back-dispatch-table s)))))
          '())
  (check-false (ahash-ref call-back-dispatch-table 'remote-file-load))
  (check-false (ahash-ref service-dispatch-table 'chat-room-receive))

  (check-group "evaluation of commands")
  (set! loop-log '())
  (server-eval '(9004 77) '(remote-logged?))
  (loop-pump)
  (check= (loop-received 'client 9004 'client-remote-result)
          '((client-remote-result 77 "no")))
  (server-eval '(9004 78) '(remote-bogus))
  (server-eval '(9004 79) '(1 2))
  (loop-pump)
  (check= (loop-received 'client 9004 'client-remote-error)
          '((client-remote-error 78 "invalid command 'remote-bogus'")
            (client-remote-error 79 "invalid command")))
  ;; the client answers the commands of the server in the same way
  (set! loop-log '())
  (client-eval '(9104 5) '(server-bogus 1))
  (loop-pump)
  (check= (loop-received 'server 9104 'server-remote-error)
          '((server-remote-error 5 "invalid command 'server-bogus'")))

  (check-group "commands sent by the server")
  (with r '(:no-answer)
    (server-remote-eval 9001 '(client-bogus) (lambda (x) (set! r (list :ok x)))
                        (lambda (e) (set! r (list :error e))))
    (loop-pump)
    (check= r '(:error "invalid command 'client-bogus'")))
  (with r '(:no-answer)
    (server-remote-eval* 9001 '(client-bogus) (lambda (x) (set! r x)))
    (loop-pump)
    (check= r "invalid command 'client-bogus'"))
  ;; local-eval does not answer; an answer from another client is ignored
  (let ((r '(:no-answer))
        (id (priv '(server server-base) 'server-serial)))
    (server-remote-eval 9001 '(local-eval 1) (lambda (x) (set! r x)))
    (loop-pump)
    (check= r '(:no-answer))
    (server-eval (list 9002 0) `(server-remote-result ,id 5))
    (loop-pump)
    (check= r '(:no-answer)))
  (let ((r '(:no-answer))
        (id (priv '(server server-base) 'server-serial)))
    (server-remote-eval 9001 '(local-eval 1) (lambda (x) (set! r x)))
    (loop-pump)
    (server-eval (list 9001 0) `(server-remote-result ,id 5))
    (loop-pump)
    (check= r 5))
  (with r '(:no-answer)
    (client-remote-eval* 9101 '(remote-bogus) (lambda (x) (set! r x)))
    (loop-pump)
    (check= r "invalid command 'remote-bogus'"))
  (check= (with-cont (lambda (k) (client-remote-then-cb 9101 '(remote-list-live)
                                                        (lambda (l) (k (list :ok (map car l))))
                                                        (lambda (e) (k (list :err e))))))
          '(:ok ("rt-live")))
  (check= (with-cont (lambda (k) (client-remote-then-cb 9101 '(remote-bogus)
                                                        (lambda (l) (k (list :ok l)))
                                                        (lambda (e) (k (list :err e))))))
          '(:err "invalid command 'remote-bogus'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connections and accounts of the client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-client-connections)
  (check-group "connections")
  (check= (client-find-server-name 9101) "loophost")
  (check= (client-find-server-port 9101) "6561")
  (check= (client-find-server-pseudo 9102) "rt-bob")
  (check-false (client-find-server-name 9999))
  (check-true (list-and (map (cut in? <> (client-active-servers))
                             '(9101 9102 9103 9105))))
  (check-false (in? 9104 (client-active-servers)))
  (check-true (in? 9104 (active-servers)))
  (check= (client-find-server-by-pseudo "loophost" "rt-alice") 9101)
  (check= (client-find-server-by-pseudo "loophost:6561" "rt-bob") 9102)
  (check= (client-find-server-by-pseudo "loophost:7000" "rt-bob") 9102)
  (check-true (in? (client-find-server "loophost") '(9101 9102 9103 9105)))
  (check= (client-find-server "loophost:6561") (client-find-server "loophost"))
  (check-false (client-find-server "nohost"))
  (check-false (client-find-server "loophost:7000"))
  (check= (find-server-for-name "loophost/~rt-bob/a.tm") 9102)
  (check= (find-server-for-name "loophost/~rt-root") 9103)
  (check= (find-server-for-name "loophost/x") (client-find-server "loophost"))
  (check= (remote-home-directory 9101) "tmfs://remote-dir/loophost/~rt-alice")
  (check-false (remote-home-directory 9104))
  (check= (list-live 9101) "tmfs://live-list/loophost")
  (check= (list-chat-rooms 9101) "tmfs://chat-rooms/loophost")
  (check= (list-shared 9101) "tmfs://shared/loophost")
  (check-false (list-shared 9104))
  (check-false (live-find-server lid))
  (check= (live-find-server "tmfs://live/loophost/doc")
          (client-find-server "loophost"))
  (with errno->string (priv '(client client-base) 'client-start-errno->string)
    (check= (errno->string (priv '(client client-base) 'tm_net_invalid_host))
            "invalid host name")
    (check= (errno->string (priv '(client client-base) 'tm_net_no_gnutls))
            "missing GnuTLS")
    (check= (errno->string -12345) "connection failed"))

  (check-group "accounts")
  (check= (client-accounts) '())
  (client-notify-account "srv.test" "6561" "u" '(tls-password) #f)
  (check= (client-accounts) '(("srv.test" "6561" "u" (tls-password))))
  (check-false (client-account-admin? "srv.test" "6561" "u"))
  (client-notify-account "srv.test" "6561" "u" '(legacy-password) #t)
  (check= (client-accounts)
          '(("srv.test" "6561" "u" (legacy-password tls-password))))
  (check-true (client-account-admin? "srv.test" "6561" "u"))
  (client-remove-account "srv.test" "6561" "u")
  (check= (client-accounts) '())
  (client-notify-account "srv.test" "7000" "u" '(tls-password) #f)
  (check= (client-accounts) '(("srv.test" "7000" "u" (tls-password))))
  (client-remove-account "srv.test" "7000" "u")
  (check= (client-accounts) '())
  ;; the removal of an account on the port 6561 keeps the account of the
  ;; same server and pseudo on another port
  (client-notify-account "srv.test" "6561" "u" '(tls-password) #f)
  (client-notify-account "srv.test" "7000" "u" '(tls-password) #f)
  (client-remove-account "srv.test" "6561" "u")
  (check= (client-accounts) '(("srv.test" "7000" "u" (tls-password))))
  ;; an account saved before the ports is an account on 6561
  (with-database (user-database "remote")
    (db-create-entry '(("type" "account") ("server" "srv.test")
                       ("pseudo" "u"))))
  (check= (length (client-accounts)) 2)
  (client-remove-account "srv.test" "6561" "u")
  (check= (client-accounts) '(("srv.test" "7000" "u" (tls-password))))
  (client-remove-account "srv.test" "7000" "u")
  (check= (client-accounts) '())
  (check= (client-merge-authentications '(tls-password) '(legacy-password tls-password))
          '(tls-password legacy-password))
  (check= (client-normalize-authentications '("tls-password" tls-password unknown))
          '(tls-password))
  (check-false (server-connection-admin? 9103))
  (client-notify-account "loophost" "6561" "rt-root" '(tls-password) #t)
  (check-true (server-connection-admin? 9103))
  (check= (client-active-admin-servers) '(9103))
  (client-remove-account "loophost" "6561" "rt-root")

  (check-group "account requests of the client")
  (check= (with-cont (lambda (k) (client-get-account-then 9102 #f k)))
          '(("pseudo" "rt-bob") ("name" "Bob B") ("authentications" ())
            ("email" "bob@test") ("admin" #f)))
  (check-true (in? "rt-alice"
                   (with-cont (lambda (k) (client-get-accounts-then 9103 10 0 k)))))
  (check= (with-cont (lambda (k) (client-protocol-version-then 9102 k))) "done")
  ;; the mail box of rt-carol was created by a message of rt-bob
  (check= (with-cont (lambda (k) (client-delete-account-plan 9103 "rt-carol" k)))
          '((("mail-rt-carol" "chat-room")) ()))
  (when (!= (get-preference "server service delete-account") "on")
    (check= (with-cont (lambda (k) (client-delete-account 9102 #f k)))
            "account deletion is not allowed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listings, caches and notifications of the client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (asc l) (if (sort-ascending?) l (reverse l)))

(define (test-client-listings)
  (check-group "cache of directory listings")
  (check-false (get-cached-dir-entries "tmfs://remote-dir/loophost/~rt-alice"))
  (cache-dir-entries "tmfs://remote-dir/loophost/~rt-alice" "loophost" 9101
                     '(("a" "~rt-alice/a" #f ())))
  (check= (get-cached-dir-entries "tmfs://remote-dir/loophost/~rt-alice")
          '("loophost" 9101 (("a" "~rt-alice/a" #f ()))))
  (cache-dir-entries "tmfs://remote-dir/loophost/~rt-alice" "loophost" 9101 '())
  (check= (get-cached-dir-entries "tmfs://remote-dir/loophost/~rt-alice")
          '("loophost" 9101 ()))
  (clear-cached-dir-entries "tmfs://remote-dir/loophost/~rt-alice")
  (check-false (get-cached-dir-entries "tmfs://remote-dir/loophost/~rt-alice"))

  (check-group "sorting entries")
  (let* ((l '(("b" "file" "3") ("A" "dir" "5") ("c" "live" "1") ("d" "dir" "x")))
         (sort-by (lambda (f) (map car (sort-entries l car cadr caddr f)))))
    (check= (sort-by "name") (asc '("A" "b" "c" "d")))
    (check= (sort-by "date") (asc '("d" "c" "b" "A")))
    (check= (sort-by "type") (asc '("A" "d" "b" "c")))
    (check= (sort-by "unknown") (asc '("A" "b" "c" "d")))
    (check= (map car (sort-entries l car #f caddr "type"))
            (asc '("A" "b" "c" "d"))))
  (check= (sort-name-entries '(("r2" "20") ("R1" "30") "r3") "name")
          (asc '(("R1" "30") ("r2" "20") "r3")))
  (check= (sort-name-entries '(("r2" "20") ("R1" "30") "r3") "date")
          (asc '("r3" ("r2" "20") ("R1" "30"))))
  (check= (map car ((priv '(client client-tmfs) 'sort-directory-entries)
                    '(("b" "~u/b" #f (("date" "1")))
                      ("a" "~u/a" #f (("date" "2")))
                      ("z" "~u/z" #t (("date" "3"))))
                    "type"))
          (asc '("z" "a" "b")))

  (check-group "headers and actions of listings")
  (let* ((field (get-sort-field))
         (mark (if (sort-ascending?) '<blacktriangleup> '<blacktriangledown>)))
    (check= (sort-header-label field "L") `(concat "L" " " ,mark))
    (check= (sort-header-action field) "(toggle-sort-direction)")
    (check= (sort-header-cell field "L")
            `(dir-header-cell (concat "L" " " ,mark) "(toggle-sort-direction)"))
    (with other (if (== field "name") "date" "name")
      (check= (sort-header-label other "L") '(concat "L" " " <vartriangleright>))
      (check= (sort-header-action other)
              (string-append "(set-sort-field \"" other "\")"))))
  (check= (build-table-share-action 9101 "tmfs://chat/h/r")
          '(action (dir-entry-icon "tm_cloud_share.svg")
                   "(open-permissions-editor 9101 \"tmfs://chat/h/r\")"))
  (check= (build-table-rename-action 9101 "u")
          '(action (dir-entry-icon "tm_replace.svg")
                   "(remote-rename-interactive 9101 \"u\")"))
  (check= (build-table-remove-action 9101 "u")
          '(action (dir-entry-icon "tm_focus_delete.svg")
                   "(remote-remove-interactive 9101 \"u\")"))
  (check= (build-actions-bar 9101 "u")
          `(concat ,(build-table-share-action 9101 "u") (hspace "0.5em")
                   ,(build-table-rename-action 9101 "u") (hspace "0.5em")
                   ,(build-table-remove-action 9101 "u")))
  (check= (remote-file-browser-document '(document "x"))
          `(document (TeXmacs ,(texmacs-version))
                     (style (tuple "generic" "remote-file-browser"))
                     (body (document "x"))))
  (check= ((priv '(client client-chat) 'chat-room-table-entry)
           "loophost" 9101 '("room" ""))
          `(dir-entry "tm_cloud_chat.svg" "room" "tmfs://chat/loophost/room" ""
                      ,(build-table-share-action 9101 "tmfs://chat/loophost/room")))
  (check= ((priv '(client client-live) 'live-table-entry) "loophost" 9101 "doc")
          `(dir-entry "tm_cloud_live.svg" "doc" "tmfs://live/loophost/doc" ""
                      ,(build-table-share-action 9101 "tmfs://live/loophost/doc")))
  (check= ((priv '(client client-chat) 'shared-table-entry)
           '("share" "u" "U" "" "tmfs://remote-file/h/~u/a.tm" "v"))
          '(dir-entry "tm_cloud_file.svg" "a.tm" "tmfs://remote-file/h/~u/a.tm"
                      "" ""))

  (check-group "notifications of the client")
  (check= (notification-count 9199 'message) 0)
  (check-false (has-notifications? 9199 'message))
  (check-true (add-notification 9199 "n1" 'message noop '(("kind" "message"))))
  (check-false (add-notification 9199 "n1" 'message noop '()))
  (check-true (add-notification 9199 "n2" 'message noop '()))
  (check-true (add-notification 9199 "n3" 'other noop '()))
  (check= (notification-count 9199 'message) 2)
  (check= (notification-total-count 9199) 3)
  (check= (sorted (get-notifications 9199 'message)) '("n1" "n2"))
  (check= (notification-payload 9199 "n1") '(("kind" "message")))
  (check= (notification-kind 9199 "n3") 'other)
  (check= (notifiable-icon 9199 'message "a.svg" "b.svg") '(icon "b.svg"))
  (check= (notifiable-icon 9199 'none "a.svg" "b.svg") '(icon "a.svg"))
  (check= (notif-count-label 9199 'message "Mail") "Mail (2)")
  (check= (notif-count-label 9199 'none "Mail") "Mail")
  (check= (cadr (notifiable-entry 9199 'none "Mail" noop)) noop)
  (check= (car (notifiable-entry 9199 'message "Mail" noop))
          `(style ,widget-style-bold "Mail (2)"))
  (clear-notifications 9199 'message)
  (check= (notification-count 9199 'message) 0)
  (check= (notification-total-count 9199) 1)
  (check-false (notification-payload 9199 "n1"))
  (for (i (.. 0 120))
    (add-notification 9199 (number->string i) 'many noop '()))
  (check= (notification-count 9199 'many) 120)
  (check= (notif-count-label 9199 'many "Mail") "Mail (99+)")
  (clear-notifications 9199 'many)
  (clear-notifications 9199 'other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences and emails of the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-server-preferences)
  (check-group "preferences of the server")
  (with prefs (server-admin-preferences)
    (check= (assoc-ref prefs "server port") (get-preference "server port"))
    (check= (assoc-ref prefs "tls-server") (get-preference "tls-server"))
    (check-true (in? "server service login" (map car prefs)))
    (check-false (list-find (map car prefs)
                            (cut string-starts? <> "server public:")))
    (check-false (list-find (map car prefs)
                            (lambda (k) (not (or (string-starts? k "server")
                                                 (string-starts? k "tls-server")))))))
  (with is-on? (priv '(server server-base) 'is-on?)
    (check= (map is-on? (list "on" "true" #t "off" #f "false" 'on))
            '(#t #t #t #f #f #f #t)))
  (with load-prefs (priv '(server server-base) 'load-preferences-in-stree)
    (check= (load-prefs '(document (form-input-text "server port" "a" "b" "c" "0")
                                   (form-checkbox "tls-server" "false")
                                   (form-text-area "other" "a" "b" "c" "x")
                                   "text")
                        '(("server port" . "6562") ("tls-server" . "on")))
            '(document (form-input-text "server port" "a" "b" "c" "6562")
                       (form-checkbox "tls-server" "true")
                       (form-text-area "other" "a" "b" "c" "x")
                       "text")))
  (check= ((priv '(server server-base) 'server-mailer-instantiate)
           "u" "User" "u@test" "123"
           "To: $USER_EMAIL\n$USER_NAME ($USER_PSEUDO), code $USER_CODE")
          "To: u@test\nUser (u), code 123")
  (with credentials->authentications
      (priv '(server server-base) 'credentials->authentications)
    (check= (credentials->authentications '()) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-test-failures)
  (:synopsis "Run the tests of the server and the client")
  (check-suite "remote")
  (run-group test-names)
  (run-group test-context)
  (if (not (scratch-home?))
      (skip "server and client data"
            "TeXmacs uses the home directory of the user")
      (begin
        (run-group remote-setup)
        (run-group test-users)
        (run-group test-files)
        (run-group test-file-services)
        (run-group test-db-services)
        (run-group test-chat)
        (run-group test-live)
        (run-group test-sync-status)
        (run-group test-sync)
        (run-group test-db-sync)
        (run-group test-dispatch)
        (run-group test-client-connections)
        (run-group test-client-listings)
        (run-group test-server-preferences)
        (run-group remote-cleanup)
        (loop-uninstall!)
        (set! remote-active? #f)))
  (check-end))
