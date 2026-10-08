;; The remote tools (the Remote menu) against a running TeXmacs server, from
;; a desktop client or from the page: the same checks in both.
;;
;;   (load ".../remote-check.scm")
;;   (remote-check "admin" "secret123" "bob" #f)
;;
;; logs in on localhost:6561 (the test server of server.scm) and runs the
;; checks one after the other, each with the time for the answers of the
;; server; a line "remote-check: ok <name>" or "remote-check: FAIL <name>"
;; for each, and "remote-check: done, <n> failures" at the end. The last
;; argument says that the other user ran the checks before: what that user
;; shared and sent is then looked for. The connection stays open, for
;; (rc-widget "<name>"), which opens a dialog of the menu, until (rc-logout).

(use-modules (client client-base) (client client-tmfs) (client client-db)
             (client client-widgets) (client client-chat) (client client-live)
             (client client-sync) (client client-remote-config)
             (client client-notifications) (client client-menu)
             (utils relate live-document))

(define rc-fails 0)
(define rc-val (make-ahash-table))
(define rc-last #f) ;; the last answer, shown with a failure
(define (rc-set key)
  (lambda (x) (set! rc-last x) (ahash-set! rc-val key (list x))))
(define (rc-set? key) (pair? (ahash-ref rc-val key)))
(define (rc-get key) (and (rc-set? key) (car (ahash-ref rc-val key))))

(define (rc-report ok? name . info)
  (when (not ok?) (set! rc-fails (+ rc-fails 1)))
  (display* "remote-check: " (if ok? "ok   " "FAIL ") name)
  (for (x info) (display* " " x))
  (display "\n"))

(define (rc-try name thunk)
  (catch #t thunk
         (lambda args (rc-report #f name "error:" args) 'error)))

;; does the Scheme tree t hold the symbol or the piece of text what?
(define (rc-has? t what)
  (cond ((pair? t) (or (rc-has? (car t) what) (rc-has? (cdr t) what)))
        ((string? t) (and (string? what)
                          (string-contains? t what)))
        (else (== t what))))

(define (rc-buf-has? u what)
  (and (buffer-exists? u)
       (rc-has? (tm->stree (buffer-get-body u)) what)))

(define (rc-run steps done)
  (if (null? steps) (done)
      (with (name ms action check) (car steps)
        (rc-try name action)
        (delayed
          (:pause ms)
          (with r (rc-try name check)
            (cond ((== r 'error) (noop))
                  (r (rc-report #t name))
                  (else (rc-report #f name "last answer:" rc-last))))
          (rc-run (cdr steps) done)))))

(define (rc-server) (car (client-active-servers)))

(define (rc-doc text)
  `(document (TeXmacs ,(texmacs-version)) (style (tuple "generic"))
             (body (document ,text))))

(define (rc-save-local u text)
  (string-save (convert (rc-doc text) "texmacs-stree" "texmacs-document") u))

(define rc-file #f) ;; the remote file of the user, for the dialogs

(tm-define (remote-check user pass peer peer?)
  (let* ((stamp (number->string (current-time)))
         (host "localhost")
         (base (string-append host "/~" user "/rc-" user))
         (home (string-append "tmfs://remote-dir/" host "/~" user))
         (dir (string-append "tmfs://remote-dir/" base))
         (file (string-append "tmfs://remote-file/" base "/hello.tm"))
         (file2 (string-append "tmfs://remote-file/" base "/renamed.tm"))
         (up (string-append "tmfs://remote-file/" base "/up.tm"))
         (peer-file (string-append "tmfs://remote-file/" host "/~" peer
                                   "/rc-" peer "/renamed.tm"))
         (mbox (string-append "tmfs://chat/" host "/mail-" user))
         (room (string-append "rc-room-" user "-" stamp))
         (room-u (string-append "tmfs://chat/" host "/" room))
         (live (string-append "rc-live-" user "-" stamp))
         (lid (string-append "tmfs://live/" host "/" live))
         (ldir (system->url (string-append
                              (url-concretize (string->url "$TEXMACS_HOME_PATH"))
                              "/rc-local-" stamp)))
         (lup (url-append ldir "up.tm"))
         (ldown (url-append ldir "down.tm"))
         (lsync (url-append ldir "sync"))
         (rsync (string->url (string-append dir "/sync")))
         (S rc-server)
         (ask (lambda (key cmd)
                (client-remote-eval (S) cmd (rc-set key) (rc-set key)))))
    (set! rc-file file2)
    (set! rc-fails 0)
    (rc-run
     (list
      (list "login" 6000
            (lambda ()
              (client-login-home host "6561" user (list 'tls-password pass)
                                 (lambda args ((rc-set "login") #t))))
            (lambda () (and (rc-get "login") (nnull? (client-active-servers)))))
      (list "administrator or not" 500 noop
            (lambda () (== (not (server-connection-admin? (S)))
                           (!= user "admin"))))
      (list "users of the server" 2500
            (lambda () (remote-search-user (S) (list) (rc-set "users")))
            (lambda () (and (list? (rc-get "users"))
                            (in? peer (rc-get "users"))
                            (in? user (rc-get "users")))))
      (list "remove the directory of a previous run" 3000
            (lambda () (remote-remove dir))
            (lambda () #t))
      (list "home directory" 3500
            (lambda () (load-document home))
            (lambda () (rc-buf-has? home 'dir-list)))
      (list "new remote directory" 3500
            (lambda () (remote-create-dir (S) dir))
            (lambda () (and (== (url->string (current-buffer)) dir)
                            (rc-buf-has? dir 'dir-list))))
      (list "new remote file" 7000
            (lambda ()
              (remote-create-file
               (S) file (rc-doc (string-append "Hello from " user))))
            (lambda () (and (== (url->string (current-buffer)) file)
                            (rc-buf-has? file "Hello from"))))
      (list "save a second version" 3500
            (lambda ()
              (buffer-set-body
               file (stree->tree
                     `(document ,(string-append "Second version by " user))))
              (buffer-pretend-modified file)
              (commit-buffer-message file "second version"))
            (lambda () (not (buffer-modified? file))))
      (list "the file as the server has it" 2500
            (lambda () (ask "load" `(remote-file-load
                                     ,(remote-file-name file))))
            (lambda () (rc-has? (rc-get "load") "Second version by")))
      (list "versions of the file" 2500
            (lambda () (ask "versions" `(remote-get-versions
                                         ,(remote-file-name file))))
            (lambda () (and (list? (rc-get "versions"))
                            (>= (length (rc-get "versions")) 2))))
      (list "history of the file" 3000
            (lambda () (version-history (string->url file)))
            (lambda ()
              (rc-buf-has? (string-append "tmfs://history/"
                                          (url->tmfs-string file))
                           "Version ")))
      (list "listing of the directory" 3500
            (lambda () (revert-buffer-revert (string->url dir)))
            (lambda () (rc-buf-has? dir "hello.tm")))
      (list "rename" 3500
            (lambda () (remote-rename (string->url file) (string->url file2)))
            (lambda () (buffer-exists? file2)))
      (list "identifiers after the renaming" 2500
            (lambda ()
              (remote-identifier (S) file (rc-set "id-old"))
              (remote-identifier (S) file2 (rc-set "id-new")))
            (lambda () (and (rc-set? "id-old") (not (rc-get "id-old"))
                            (string? (rc-get "id-new")))))
      (list "permissions: readable by the other user" 2500
            (lambda ()
              (remote-set-field (S) (rc-get "id-new") "readable" (list peer))
              (remote-get-field (S) (rc-get "id-new") "readable"
                                (rc-set "readable")))
            (lambda () (== (rc-get "readable") (list peer))))
      (list "share with the other user" 2500
            (lambda ()
              (ask "share" `(remote-send-message
                             ,(list (string-append "mail-" peer))
                             "share" ,file2)))
            (lambda () (and (rc-set? "share") (not (string? (rc-get "share"))))))
      (list "send a message" 2500
            (lambda ()
              (ask "send" `(remote-send-message
                            ,(list (string-append "mail-" peer)
                                   (string-append "mail-" user))
                            "send-document"
                            (document ,(string-append "Message from " user)))))
            (lambda () (and (rc-set? "send") (not (string? (rc-get "send"))))))
      (list "incoming messages" 3500
            (lambda () (mail-box-open (S)))
            (lambda ()
              (and (rc-buf-has? mbox (string-append "Message from " user))
                   (or (not peer?)
                       (rc-buf-has? mbox (string-append "Message from " peer))))))
      (list "shared resources" 3500
            (lambda () (load-document (list-shared (S))))
            (lambda ()
              (with u (list-shared (S))
                (and (rc-buf-has? u 'dir-list)
                     (or (not peer?) (rc-buf-has? u "renamed.tm"))))))
      (list "the file shared by the other user" 3500
            (lambda () (when peer? (load-document peer-file)))
            (lambda ()
              (or (not peer?) (rc-buf-has? peer-file "Second version by"))))
      (list "upload" 4000
            (lambda ()
              (system-mkdir ldir)
              (rc-save-local lup (string-append "Uploaded by " user))
              (remote-upload lup (url-append (string->url dir) "up.tm")
                             "uploaded" (rc-set "up")))
            (lambda () (== (rc-get "up") #t)))
      (list "the uploaded file on the server" 2500
            (lambda () (ask "up-load" `(remote-file-load
                                        ,(remote-file-name up))))
            (lambda () (rc-has? (rc-get "up-load") "Uploaded by")))
      (list "download" 4000
            (lambda ()
              (remote-download ldown (string->url file2) (rc-set "down")))
            (lambda () (and (== (rc-get "down") #t)
                            (url-exists? ldown)
                            (rc-has? (string-load ldown) "Second version by"))))
      (list "remove a remote file" 3000
            (lambda () (remote-remove (string->url up)))
            (lambda () #t))
      (list "the removed file is gone" 2500
            (lambda () (remote-identifier (S) up (rc-set "id-up")))
            (lambda () (and (rc-set? "id-up") (not (rc-get "id-up")))))
      (list "synchronization: what is to do" 3500
            (lambda ()
              (system-mkdir lsync)
              (rc-save-local (url-append lsync "one.tm") "Synchronized one")
              (rc-save-local (url-append lsync "two.tm") "Synchronized two")
              (client-auto-sync-add (S) (url->system lsync) (url->system rsync))
              (client-sync-status lsync rsync (rc-set "sync")))
            (lambda ()
              (and (list? (rc-get "sync"))
                   (== (length (filter-status-list (rc-get "sync") "upload"))
                       3))))
      (list "synchronization: done" 5000
            (lambda ()
              (client-sync-proceed (rc-get "sync") "synchronized"
                                   (lambda () ((rc-set "synced") #t))))
            (lambda () (rc-get "synced")))
      (list "synchronization: nothing left to do" 3500
            (lambda () (client-sync-status lsync rsync (rc-set "sync2")))
            (lambda ()
              (and (rc-set? "sync2") (null? (rc-get "sync2"))
                   (in? (cons (url->system lsync) (url->system rsync))
                        (client-auto-sync-list (S))))))
      (list "the synchronized file on the server" 2500
            (lambda ()
              (ask "sync-load" `(remote-file-load
                                 ,(string-append base "/sync/two.tm"))))
            (lambda () (rc-has? (rc-get "sync-load") "Synchronized two")))
      (list "new chat room" 3500
            (lambda () (chat-room-create (S) room))
            (lambda () (and (== (url->string (current-buffer)) room-u)
                            (rc-buf-has? room-u 'chat-input))))
      (list "send in the chat room" 3500
            (lambda ()
              (with l (tree-search (buffer-get-body room-u)
                                   (cut tree-is? <> 'chat-input))
                (tree-set (tree-ref (car l) 0)
                          `(document ,(string-append "Chat from " user)))
                (tree-go-to (car l) 0 0 :end)
                (chat-room-send)))
            (lambda ()
              (and (rc-buf-has? room-u 'chat-output)
                   (rc-buf-has? room-u (string-append "Chat from " user)))))
      (list "chat rooms" 3500
            (lambda () (load-document (list-chat-rooms (S))))
            (lambda () (rc-buf-has? (list-chat-rooms (S)) room)))
      (list "new live document" 4500
            (lambda () (load-document lid))
            (lambda () (and (rc-buf-has? lid 'live-io*)
                            (live-current-state lid) #t)))
      (list "edit the live document" 3500
            (lambda ()
              (with l (tree-search (buffer-get-body lid)
                                   (cut tree-is? <> 'live-io*))
                (tree-go-to (car l) 2 :start)
                (insert (string-append "Live text from " user))))
            (lambda () (rc-buf-has? lid "Live text from")))
      (list "the live document as the server has it" 2500
            (lambda () (ask "live" `(live-open ,lid)))
            (lambda () (rc-has? (rc-get "live") "Live text from")))
      (list "live documents" 3500
            (lambda () (load-document (list-live (S))))
            (lambda () (rc-buf-has? (list-live (S)) live)))
      (list "account" 2500
            (lambda () (client-get-account-then (S) #f (rc-set "account")))
            (lambda () (rc-has? (rc-get "account") user)))
      (list "server infos" 2500
            (lambda () (client-public-preferences-then (S) (rc-set "infos")))
            (lambda () (and (pair? (rc-get "infos")) (list? (rc-get "infos")))))
      (list "pending notifications" 2500
            (lambda ()
              (client-pending-notifications-then (S) (rc-set "notifs")))
            (lambda () (list? (rc-get "notifs"))))
      (list "server preferences (administrator)" 2500
            (lambda ()
              (when (== user "admin")
                (client-admin-preferences-form-then (S) (rc-set "form"))))
            (lambda () (or (!= user "admin") (pair? (rc-get "form")))))
      (list "user management (administrator)" 2500
            (lambda ()
              (when (== user "admin")
                (client-get-accounts-then (S) 20 0 (rc-set "accounts"))))
            (lambda () (or (!= user "admin")
                           (rc-has? (rc-get "accounts") peer)))))
     (lambda ()
       (display* "remote-check: done, " rc-fails " failures\n")))))

;; Two clients at once, in a chat room and a live document (named after
;; tag): the first one, (remote-duet "admin" "secret123" tag #t), makes them
;; and opens them to all; the second one, started some 20 s later with #f,
;; joins. Each checks that what the other one wrote arrived.
(tm-define (remote-duet user pass tag first?)
  (let* ((host "localhost")
         (room (string-append "rc-duet-" tag))
         (room-u (string-append "tmfs://chat/" host "/" room))
         (lid (string-append "tmfs://live/" host "/rc-duet-" tag))
         (S rc-server)
         (open-to-all
          (lambda (key)
            (when (string? (rc-get key))
              (remote-set-field (S) (rc-get key) "readable" (list "all"))
              (remote-set-field (S) (rc-get key) "writable" (list "all")))))
         (say
          (lambda (text)
            (switch-to-buffer room-u)
            (with l (tree-search (buffer-get-body room-u)
                                 (cut tree-is? <> 'chat-input))
              (tree-set (tree-ref (car l) 0) `(document ,text))
              (tree-go-to (car l) 0 0 :end)
              (chat-room-send))))
         (write
          (lambda (text)
            (switch-to-buffer lid)
            (with l (tree-search (buffer-get-body lid)
                                 (cut tree-is? <> 'live-io*))
              (tree-go-to (car l) 2 :end)
              (insert text))))
         (login
          (list "login" 6000
                (lambda ()
                  (client-login-home host "6561" user (list 'tls-password pass)
                                     (lambda args ((rc-set "login") #t))))
                (lambda () (and (rc-get "login")
                                (nnull? (client-active-servers)))))))
    (set! rc-fails 0)
    (rc-run
     (if first?
         (list
          login
          (list "duet: new chat room" 3500
                (lambda () (chat-room-create (S) room))
                (lambda () (rc-buf-has? room-u 'chat-input)))
          (list "duet: new live document" 4500
                (lambda () (load-document lid))
                (lambda () (and (live-current-state lid) #t)))
          (list "duet: identifiers" 2500
                (lambda ()
                  (remote-identifier (S) room-u (rc-set "room-id"))
                  (remote-identifier (S) lid (rc-set "live-id")))
                (lambda () (and (string? (rc-get "room-id"))
                                (string? (rc-get "live-id")))))
          (list "duet: open to all users" 2500
                (lambda () (open-to-all "room-id") (open-to-all "live-id"))
                (lambda () #t))
          (list "duet: first message and text" 40000
                (lambda () (say "First message of the first") (write "AAA "))
                (lambda () (rc-buf-has? room-u "First message of the first")))
          (list "duet: the message of the other arrived" 500 noop
                (lambda () (rc-buf-has? room-u "Message of the second")))
          (list "duet: the text of the other arrived" 500 noop
                (lambda () (and (rc-buf-has? lid "AAA") (rc-buf-has? lid "BBB"))))
          (list "duet: second message and text" 25000
                (lambda () (say "Second message of the first") (write "CCC "))
                (lambda () (and (rc-buf-has? room-u "Second message of the first")
                                (rc-buf-has? lid "CCC"))))
          (list "duet: the cursor of the other is shown" 500 noop
                (lambda () (nnull? (live-participants lid)))))
         (list
          login
          (list "duet: join the chat room" 4000
                (lambda () (chat-room-join (S) room))
                (lambda () (rc-buf-has? room-u "First message of the first")))
          (list "duet: open the live document" 5000
                (lambda () (load-document lid))
                (lambda () (rc-buf-has? lid "AAA")))
          (list "duet: message and text" 30000
                (lambda () (say "Message of the second") (write "BBB "))
                (lambda () (rc-buf-has? room-u "Message of the second")))
          (list "duet: the later message of the other arrived" 500 noop
                (lambda () (rc-buf-has? room-u "Second message of the first")))
          (list "duet: the later text of the other arrived" 500 noop
                (lambda () (and (rc-buf-has? lid "AAA") (rc-buf-has? lid "BBB")
                                (rc-buf-has? lid "CCC"))))
          (list "duet: the cursor of the other is shown" 500 noop
                (lambda () (nnull? (live-participants lid))))))
     (lambda ()
       (display* "remote-check: also in the live document: "
                 (live-participants lid) "\n")
       (display* "remote-check: their cursors: "
                 (map cdr (ahash-table->list
                            (module-ref (resolve-module '(client client-live))
                                        'live-cursors)))
                 ", ours: " (cursor-path) "\n")
       (display* "remote-check: live document: "
                 (tm->stree (buffer-get-body lid)) "\n")
       (display* "remote-check: duet done, " rc-fails " failures\n")))))

;; the dialogs of the menu, by name; and the Remote menu itself
(tm-define (rc-widget name)
  (with S (rc-server)
    (rc-try (string-append "dialog " name)
      (lambda ()
        (cond ((== name "account") (open-account-editor S))
              ((== name "infos") (open-public-preferences S))
              ((== name "permissions")
               (open-permissions-editor S (string->url rc-file)))
              ((== name "share")
               (open-share-document-widget S (string->url rc-file)))
              ((== name "message") (open-message-editor S))
              ((== name "sync") (remote-interactive-sync S))
              ((== name "client-preferences") (open-client-preferences))
              ((== name "server-preferences") (load-remote-config-form S))
              ((== name "users") (open-admin-accounts-editor S))
              ((== name "rename")
               (load-document rc-file)
               (remote-rename-interactive S (string->url rc-file)))
              ((== name "login") (open-remote-login "" "6561" "" (list)))
              ((== name "new-account") (open-remote-account-creator))
              (else (texmacs-error "rc-widget" "no such dialog")))
        (display* "remote-check: opened " name "\n")))))

(tm-define (rc-logout)
  (rc-try "logout" (lambda () (client-logout (rc-server))))
  (delayed
    (:pause 2000)
    (rc-report (null? (client-active-servers)) "logout")))

;; What the user is told about a connection: (remote-feedback user pass)
;; logs in, prints each event of the connections ("remote-check: event ...")
;; and, every 5 s, the state shown in the menu. The script which runs it
;; stops the server for a while (silent, then back), kills it (lost), and
;; has a port which accepts connections and never answers (6599); a login
;; is tried on the dead server after 85 s and on the mute port after 92 s.
(tm-define (remote-feedback user pass)
  (set! client-notify-hook
        (lambda (event msg)
          (display* "remote-check: event " event " " msg "\n")))
  (with login (lambda (port)
                (client-login-home "localhost" port user
                                   (list 'tls-password pass)
                                   (lambda args (noop))))
    (login "6561")
    (with n 0
      (delayed
        (:while (< n 16))
        (:pause 5000)
        (set! n (+ n 1))
        (when (nnull? (client-active-servers))
          (display* "remote-check: status "
                    (client-connection-status (rc-server)) "\n"))))
    (delayed (:pause 85000) (login "6561"))
    (delayed (:pause 92000) (login "6599"))
    (delayed (:pause 110000)
      (display* "remote-check: lost " (client-lost-connections) "\n")
      (display* "remote-check: feedback done\n"))))
