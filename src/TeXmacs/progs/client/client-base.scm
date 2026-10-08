
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-base.scm
;; DESCRIPTION : clients of TeXmacs servers
;; COPYRIGHT   : (C) 2007, 2013  Joris van der Hoeven
;;                   2022  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-base)
  (:use (client client-authentication)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error and success widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(server-define-error-codes)

(tm-widget ((client-error-widget s) quit)
  (padded
    (centered (text s))
    ======
    (bottom-buttons
      >> ("Ok" (quit)) >>)))

(tm-define (client-open-error s)
  (:interactive #t)
  (dialogue-window (client-error-widget s) noop "Error"))

(tm-widget ((client-success-widget s) quit)
  (padded
    (centered (text s))
    ======
    (bottom-buttons
      >> ("Ok" (quit)) >>)))

(tm-define (client-open-success s)
  (:interactive #t)
  (dialogue-window (client-success-widget s) noop "Success"))

(tm-define (client-remote-then-cb server endpoint cb cb-err)
  (with wcb (lambda (ret) (if (list? ret) (cb ret) (cb-err ret)))
    (client-remote-eval* server endpoint wcb)))

(tm-define (client-remote-then server endpoint cb err-msg)
  (client-remote-then-cb
    server endpoint cb (lambda (ret) (client-open-error (string-append err-msg ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaration of call backs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define call-back-dispatch-table (make-ahash-table))

(tm-define-macro (tm-call-back proto . body)
  (if (npair? proto) '(noop)
      (with (fun . args) proto
	`(begin
           (tm-define (,fun envelope ,@args)
             (catch #t
		    (lambda () ,@body)
                    (lambda (key . err-args)
                      (with msg (apply format-err key err-args)
                            (format #t "Client error in callback ~A: ~A\n"
                                    (symbol->string ',fun) msg)
                            (client-error envelope msg)))))
	   (ahash-set! call-back-dispatch-table ',fun ,fun)))))

(tm-define (client-eval envelope cmd)
  (when (debug-get "remote")
    (display* "client-eval " envelope ", " cmd "\n"))
  (cond ((and (pair? cmd) (ahash-ref call-back-dispatch-table (car cmd)))
         (with (name . args) cmd
           (with fun (ahash-ref call-back-dispatch-table name)
             (apply fun (cons envelope args)))))
        ((symbol? (car cmd))
         (with s (symbol->string (car cmd))
           (client-error envelope (string-append "invalid command '" s "'"))))
        (else (client-error envelope "invalid command"))))

(tm-define (client-return envelope ret-val)
  (with (server msg-id) envelope
    (client-send server `(server-remote-result ,msg-id ,ret-val))))

(tm-define (client-error envelope error-msg)
  (with (server msg-id) envelope
    (client-send server `(server-remote-error ,msg-id ,error-msg))))

(tm-call-back (client-account-deleted msg)
  (with server (car envelope)
    (and-with server-con (ahash-ref client-active-connections server)
      (with (server-name server-port server-pseudo) server-con
        (remove-active-connection server server-name server-port server-pseudo)
        (set! remote-client-list (client-active-servers))
        (client-stop server)
        (client-remove server)))
    (client-open-error (or msg "Your account has been deleted"))))

(tm-call-back (local-eval cmd)
  (when #f ;; only set to #t for debugging purposes
    (with ret (eval cmd)
      (when (debug-get "remote")
        (display* "local-eval " cmd " -> " ret "\n"))
      (client-return envelope ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The state of the connections, for the user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What is known of each connection (by its number, server):
;;   client-started      when it was opened (texmacs-time, ms)
;;   client-heard        when the server was last heard (any message)
;;   client-ping-sent    when the ping which is not answered yet was sent
;;   client-latency      how long the last ping took (ms)
;;   client-silent       the user was told that the server does not answer
;;   client-stop-reason  why we close the connection ourselves
;; client-lost is the list of the connections which were lost, each
;; (server-name port pseudo reason), until the user logs in again.

(define client-started (make-ahash-table))
(define client-heard (make-ahash-table))
(define client-ping-sent (make-ahash-table))
(define client-latency (make-ahash-table))
(define client-silent (make-ahash-table))
(define client-stop-reason (make-ahash-table))
(define client-looking (make-ahash-table)) ;; an answer is looked for soon
;; The numbers of the connections are those of their sockets, which the
;; system gives again to later connections: what is scheduled for a
;; connection (its heartbeat, the wait for its first answer) holds a token
;; of its own, and stops when the token of the number is another one.
(define client-token (make-ahash-table))
(define client-token-serial 0)

(define (client-new-token server)
  (set! client-token-serial (+ client-token-serial 1))
  (ahash-set! client-token server client-token-serial)
  client-token-serial)
(define client-lost (list))

(define client-ping-interval 10000) ;; a ping every 10 s
(define client-silent-after 25000)  ;; two pings missed: the server is silent

(define (client-forget-state server)
  (for (t (list client-started client-heard client-ping-sent client-latency
                client-silent client-stop-reason client-looking
                client-token))
    (ahash-remove! t server)))

(define (account-name server-name port pseudo)
  (string-append pseudo "@" server-name (if (== port "6561") "" ":") 
                 (if (== port "6561") "" port)))

;; What tells the user of an event of a connection. event is one of
;;   :connecting :connected :logged-out :silent :back :lost :failed
;; and msg a sentence for it. A procedure, which a test (or another
;; interface) may replace; this one writes in the footer, and opens a
;; dialog for what the user has to know even when looking elsewhere.
(tm-define client-notify-hook
  (lambda (event msg)
    (set-message msg "remote server")
    (when (and (in? event (list :lost)) (not (headless?)))
      (client-open-error msg))))

(tm-define (client-notify event msg)
  (when (debug-get "remote")
    (display* "client-notify " event ", " msg "\n"))
  (client-notify-hook event msg)
  ;; the menus and the icons show the state of the connections
  (set! remote-client-list (client-active-servers)))

(define (client-notify-heard server)
  (ahash-set! client-heard server (texmacs-time))
  (when (ahash-ref client-silent server)
    (ahash-remove! client-silent server)
    (and-with name (client-find-server-name server)
      (client-notify :back (string-append "the server " name
                                          " answers again")))))

(tm-define (client-connection-state server)
  (:synopsis "The state of a connection: :connected, :silent or :closed")
  (cond ((not (ahash-ref client-server-active? server)) :closed)
        ((ahash-ref client-silent server) :silent)
        (else :connected)))

(tm-define (client-connection-latency server)
  (:synopsis "How long the server took to answer the last ping (ms), or #f")
  (ahash-ref client-latency server))

(tm-define (client-connection-status server)
  (:synopsis "A sentence on the state of a connection, for the menus")
  (let* ((name (or (client-find-server-name server) "server"))
         (pseudo (client-find-server-pseudo server))
         (state (client-connection-state server))
         (heard (ahash-ref client-heard server))
         (ms (ahash-ref client-latency server)))
    (cond ((== state :closed) (string-append "Not connected to " name))
          ((== state :silent)
           (string-append "No answer from " name " for "
                          (number->string
                            (quotient (- (texmacs-time) (or heard 0)) 1000))
                          " s"))
          (else
            (string-append "Connected to " name
                           (if pseudo (string-append " as " pseudo) "")
                           (if ms (string-append " (" (number->string ms)
                                                 " ms)") ""))))))

(tm-define (client-lost-connections)
  (:synopsis "The connections which were lost: (server-name port pseudo reason)")
  client-lost)

(tm-define (client-forget-lost server-name port)
  (set! client-lost
        (list-filter client-lost
                     (lambda (x) (not (and (== (car x) server-name)
                                           (== (cadr x) port)))))))

(define (client-connection-lost server reason)
  ;; a connection which was logged in ends, and not by a logout (which
  ;; forgets the connection first)
  (and-with con (ahash-ref client-active-connections server)
    (with (server-name port pseudo) con
      (remove-active-connection server server-name port pseudo)
      (client-forget-lost server-name port)
      (set! client-lost (cons (list server-name port pseudo reason)
                              client-lost))
      (client-notify
        :lost
        (string-append "Connection with " (account-name server-name port pseudo)
                       " lost: " reason
                       ". Its remote files, chat rooms and live documents"
                       " are no longer synchronized; log in again"
                       " to continue.")))))

(define (client-fail-pending server reason)
  ;; whoever waits for an answer of server gets reason as an error
  (with l (list-filter (ahash-table->list client-error-handlers)
                       (lambda (x) (== (cadr x) server)))
    (for (x l)
      (ahash-remove! client-continuations (car x))
      (ahash-remove! client-error-handlers (car x)))
    (for (x l)
      (catch #t
             (lambda () ((caddr x) reason))
             (lambda args (noop))))))

(tm-define (client-close server reason)
  (:synopsis "Close a connection which does not work, and say why")
  (ahash-set! client-stop-reason server reason)
  (client-stop server))

(tm-define (client-heartbeat server)
  (:synopsis "Ask the server for a sign of life, and see whether it gave any")
  ;; any message of the server is a sign of life, the answer to a ping too
  ;; (an older server answers that it does not know the command)
  (let* ((now (texmacs-time))
         (heard (or (ahash-ref client-heard server)
                    (ahash-ref client-started server) now))
         (quiet (- now heard))
         (name (or (client-find-server-name server) "server"))
         (give-up (* 1000 (max 30 (or (client-get-connection-timeout) 100)))))
    (cond ((> quiet give-up)
           (client-close server
                         (string-append "no answer for "
                                        (number->string (quotient quiet 1000))
                                        " s")))
          ((> quiet client-silent-after)
           (when (not (ahash-ref client-silent server))
             (ahash-set! client-silent server #t)
             (client-notify :silent (string-append
                                      "the server " name
                                      " does not answer; still trying")))))
    (when (and (ahash-ref client-server-active? server)
               (not (ahash-ref client-ping-sent server)))
      (ahash-set! client-ping-sent server now)
      (with answered (lambda (ret)
                       (when (ahash-ref client-ping-sent server)
                         (ahash-set! client-latency server
                                     (- (texmacs-time)
                                        (ahash-ref client-ping-sent server)))
                         (ahash-remove! client-ping-sent server)))
        (client-remote-eval server '(remote-ping) answered answered)))))

(define (client-watch server)
  ;; the heartbeat of a connection which is logged in
  (with token (client-new-token server)
    (delayed
      (:while (and (== (ahash-ref client-token server) token)
                   (ahash-ref client-server-active? server)
                   (ahash-ref client-active-connections server)))
      (:pause client-ping-interval)
      (client-heartbeat server))))

(tm-define (client-expect-answer server what)
  (:synopsis "Close the connection unless the server says something in time")
  ;; a server which is not there, or not a TeXmacs server, or behind a
  ;; network which drops the packets, left the user waiting for ever
  (let ((ms (max 1000 (or (client-get-contact-timeout) 10000)))
        (token (client-new-token server)))
    (delayed
      (:pause ms)
      (when (and (== (ahash-ref client-token server) token)
                 (not (ahash-ref client-heard server)))
        (client-close server
                      (string-append "no answer from " what " within "
                                     (number->string (quotient ms 1000))
                                     " s (is a TeXmacs server running"
                                     " there, and reachable?)"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing and finishing connections with servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-server-active? (make-ahash-table))
(define client-serial 0)

(tm-define (active-servers)
  (ahash-set->list client-server-active?))

(tm-define (client-send server cmd)
  (client-write server (object->string* (list client-serial cmd)))
  (set! client-serial (+ client-serial 1))
  ;; an answer is expected: look for it soon (the loop which reads the
  ;; messages of a quiet server looks every 2.5 s only)
  (when (not (ahash-ref client-looking server))
    (ahash-set! client-looking server #t)
    (client-look-soon server (list 5 10 20 40 80 160 320 640 1200))))

(define (client-read-pending server)
  ;; read a message of server, if there is one, and act on it
  (with msg (client-read server)
    (and (!= msg "")
         (begin
           (client-notify-heard server)
           (with (msg-id msg-cmd) (string->object msg)
             (client-eval (list server msg-id) msg-cmd))
           #t))))

(define (client-look-soon server pauses)
  (if (or (null? pauses) (not (ahash-ref client-server-active? server)))
      (ahash-remove! client-looking server)
      (delayed
        (:pause (car pauses))
        (if (client-read-pending server)
            (ahash-remove! client-looking server)
            (client-look-soon server (cdr pauses))))))

(tm-define (client-add server)
  (ahash-set! client-server-active? server #t)
  (ahash-remove! client-heard server)
  (ahash-set! client-started server (texmacs-time))
  (with wait 1
    (delayed
      (:while (ahash-ref client-server-active? server))
      (:pause ((lambda () (inexact->exact (round wait)))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (when (client-read-pending server)
        (set! wait 1)))))

(tm-define (client-remove server)
  ;; the connection with server is over: closed on our side (a logout,
  ;; client-stop), by the server or by the network. Whoever waits for an
  ;; answer of the server is told so; and a connection which was logged in
  ;; and not closed by a logout was lost, which the user is told.
  ;; (a connection which could not be made ends too, maybe before it was
  ;; added: who waits for its answer is told as well)
  (ahash-remove! client-server-active? server)
  (with reason (or (ahash-ref client-stop-reason server)
                   (if (ahash-ref client-heard server)
                       "the connection with the server was lost"
                       (string-append "no connection with the server could"
                                      " be made (is a TeXmacs server"
                                      " running there, and reachable?)")))
    (client-forget-state server)
    (client-connection-lost server reason)
    (client-fail-pending server reason)))

(tm-define (client-remove-notify server msg)
  (client-open-error msg)
  (client-remove server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending asynchroneous commands to servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-continuations (make-ahash-table))
(define client-error-handlers (make-ahash-table))

(tm-define (std-client-error msg)
  ;;(texmacs-error "client-remote-error" "remote error ~S" msg)
  (with t (if (string-ends? msg "\n") msg (string-append msg "\n"))
    (with s (if (string-starts? msg "Error: ") (substring t 7) t)
      (if (headless?)
	  (display-err* "Remote error: " s)
	  (debug-message "remote-error" s)))))

(tm-define (client-remote-eval server cmd cont . opt-err-handler)
  (when (debug-get "remote")
    (display* "client-remote-eval " (list server client-serial) ", " cmd "\n"))
  (with err-handler std-client-error
    (if (nnull? opt-err-handler) (set! err-handler (car opt-err-handler)))
    (ahash-set! client-continuations client-serial (list server cont))
    (ahash-set! client-error-handlers client-serial (list server err-handler))
    (client-send server cmd)))

(tm-define (client-remote-eval* server cmd cont)
  (client-remote-eval server cmd cont cont))

(tm-call-back (client-remote-result msg-id ret)
  (with server (car envelope)
    (and-with val (ahash-ref client-continuations msg-id)
      (ahash-remove! client-continuations msg-id)
      (ahash-remove! client-error-handlers msg-id)
      (with (orig-server cont) val
        (when (== server orig-server)
          (cont ret))))))

(tm-call-back (client-remote-error msg-id err-msg)
  (with server (car envelope)
    (and-with val (ahash-ref client-error-handlers msg-id)
      (ahash-remove! client-continuations msg-id)
      (ahash-remove! client-error-handlers msg-id)
      (with (orig-server err-handler) val
        (when (== server orig-server)
          (err-handler err-msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server names and ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-brackets s)
  (string-replace (string-replace s "[" "") "]" ""))

(define (sub-split-server-name-and-port s)
  (with v (string-decompose s ":")
    (with l (last v)
      (with p (string->number l)
	(if (integer? p)
	    (list (string-recompose (list-drop-right v 1) ":") l)
	    (list s "6561"))))))

(define (aux-split-server-name-and-port s)
  (with v (string-decompose s ":")
    (if (> (length v) 2)
	(if (string-contains? s "]")
	    (with p (sub-split-server-name-and-port s)
	      (list (remove-brackets (first p)) (second p)))
	    (list s "6561"))
	(sub-split-server-name-and-port s))))

(define (split-server-name-and-port s)
  (with l (aux-split-server-name-and-port s)
    ;(display* "split-server-name-and-port " s " --> " l "\n")
    l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accounts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-active-connections (make-ahash-table))

(tm-define (add-active-connection server server-name port pseudo)
  (ahash-set! client-active-connections server (list server-name port pseudo))
  (ahash-set! client-active-connections (list server-name port) server)
  (client-forget-lost server-name port)
  (client-watch server))

(define (remove-active-connection server server-name port pseudo)
  (ahash-remove! client-active-connections server)
  ;; (unless a newer connection with the same server took its place)
  (when (== (ahash-ref client-active-connections (list server-name port))
            server)
    (ahash-remove! client-active-connections (list server-name port))))

(tm-define (client-find-server-by-name-and-port server-name port)
  ;(display* server-name " -> " (ahash-ref client-active-connections
  ;					  server-name) "\n")
  (ahash-ref client-active-connections (list server-name port)))

(tm-define (client-find-server server-name-port)
  (with (server-name port) (split-server-name-and-port server-name-port)
    (client-find-server-by-name-and-port server-name port)))

(tm-define (client-find-server-by-pseudo server-name-port pseudo)
  (with (server-name port) (split-server-name-and-port server-name-port)
    (or (list-find (client-active-servers)
                   (lambda (s)
                     (and-with con (ahash-ref client-active-connections s)
                       (and (== (first con) server-name)
                            (== (second con) port)
                            (== (third con) pseudo)))))
        ;; in the case we are getting host from tmfs links, which do not have the port
        ;; and the server is not bound to the default 6561 port
        ;; TODO: we should modify tmfs urls to support ports,
        ;; eg tmfs::/remote-file/localhost:6562/...
        (list-find (client-active-servers)
                   (lambda (s)
                     (and-with con (ahash-ref client-active-connections s)
                       (and (== (first con) server-name)
                            (== (third con) pseudo)))))
        (client-find-server-by-name-and-port server-name port))))

(tm-define (client-find-server-name server)
  (and-with p (ahash-ref client-active-connections server) (first p)))

(tm-define (client-find-server-port server)
  (and-with p (ahash-ref client-active-connections server) (second p)))

(tm-define (client-find-server-pseudo server)
  (and-with p (ahash-ref client-active-connections server) (third p)))

(tm-define (client-active-servers)
  (list-filter (active-servers) client-find-server-name))

(tm-define (client-active-admin-servers)
  (list-filter (active-servers)
               (lambda (s) (and (client-find-server-name s)
                                (server-connection-admin? s)))))

(tm-define (client-notify-account server-name port pseudo authentications admin?)
  (with-database (user-database "remote")
      (let* ((l `(("type" "account")
                  ("server" ,server-name)
                  ("port" ,port)
                  ("pseudo" ,pseudo)))
             (ids (db-search l)))
        (if (null? ids)
          (db-create-entry (append l
                                   `((authentications . ,authentications))
                                   `((admin . (,admin?)))))
          (let* ((id (car ids))
                 (auth-old (client-normalize-authentications
                             (db-get-field id "authentications")))
                 (auth-new (client-merge-authentications
                             authentications auth-old)))
            ;(display* "updating entry, auth: " auth-new ", admin: " admin?  "\n")
            (db-set-field id "authentications" auth-new)
            (db-set-field id "admin" (list admin?)))))))

(tm-define (client-accounts)
  (with-database (user-database "remote")
    (let* ((ids (db-search `(("type" "account"))))
	   (get (lambda (id)
		  (list (db-get-field-first id "server" "")
			(db-get-field-first id "port" "6561")
			(db-get-field-first id "pseudo" "")
			(client-normalize-authentications
			 (db-get-field id "authentications"))))))
      (map get ids))))

(tm-define (client-remove-account server-name port pseudo)
  (with-database (user-database "remote")
    (with ids (db-search `(("type" "account")
                           ("server" ,server-name)
                           ("port" ,port)
                           ("pseudo" ,pseudo)))
      (when (nnull? ids)
        (db-remove-entry (car ids))))
    (when (== port "6561") ; backward compatibility, accounts without port
      (with ids (list-filter (db-search `(("type" "account")
                                          ("server" ,server-name)
                                          ("pseudo" ,pseudo)))
                             (lambda (id) (null? (db-get-field id "port"))))
	(when (nnull? ids)
	  (db-remove-entry (car ids)))))))

(tm-define (client-account-admin? server-name port pseudo)
  (with-database (user-database "remote")
    (with ids (db-search `(("type" "account")
                           ("server" ,server-name)
                           ("port" ,port)
                           ("pseudo" ,pseudo)
                           ("admin" "#t")))
          (nnull? ids))))

(tm-define (server-connection-admin? server)
  (and-with p (ahash-ref client-active-connections server) 
            (client-account-admin? (first p) (second p) (third p))))

;; New account
(tm-define (client-new-account server infos cb-pending cb-done cb-err)
  (let ((server-name (ahash-ref infos "server-name"))
        (port (ahash-ref infos "port"))
        (pseudo (ahash-ref infos "pseudo"))
        (name (ahash-ref infos "name"))
        (creds (map client-hide-credential (ahash-ref infos "creds")))
        (email (ahash-ref infos "email"))
        (agreed (ahash-ref infos "agreed")))
  ;(display* "creating new account server-name: " server-name ", port: " port ", protocol: " protocol ", pseudo: " pseudo ", name: " name ", email: " email "\n")
  (client-remote-eval*
    server `(new-account ,pseudo ,name ,creds ,email ,agreed)
    (lambda (msg)
      (set-message msg "Creating new remote account")
      (cond
        ((== msg "done")    (cb-done server server-name port pseudo creds))
        ((== msg "pending") (cb-pending server server-name port pseudo creds))
        ((== msg "user already exists")
         (cb-err (string-append "Remote account creation failed: user '"
                                pseudo "' already exists")))
        (else (cb-err (string-append "Remote account creation failed: "
                                     msg))))))))

(tm-define (client-delete-account server user cb)
  (client-remote-eval* server `(remote-delete-account ,user) cb))

(tm-define (client-delete-account-plan server user cb)
  (client-remote-eval* server `(remote-deletion-plan ,user) cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account informations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-get-account-then server user cb)
  (client-remote-then server `(remote-get-account ,user) cb
                      "Cannot retrieve remote account information: "))

(tm-define (client-get-accounts-then server limit offset cb)
  (client-remote-then server `(remote-get-accounts ,limit ,offset) cb
                      "Cannot retrieve user accounts preferences: "))

(tm-define (client-set-account server infos)
  (with cb (lambda (ret)
              (if (== ret "done")
                  (client-open-success "Remote account information updated")
                  (client-open-error
		   (string-append
		    "Cannot set remote account information: " ret))))
    (client-remote-eval* server `(remote-set-account #f ,infos) cb)))

(tm-define (client-admin-set-account server infos user)
  (with cb (lambda (ret)
              (if (== ret "done")
                  (client-open-success "Remote account information updated")
                  (client-open-error
		   (string-append
		    "Cannot set remote account information: " ret))))
    (client-remote-eval* server `(remote-set-account ,user ,infos) cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-protocol-version-then server cb)
  (client-remote-eval* server `(remote-protocol-version
                                 ,(client-protocol-version)) cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (client-start-errno->string e)
  (cond ((== e tm_net_invalid_host) "invalid host name")
	((== e tm_net_invalid_port) "invalid port number")
	((== e tm_net_internal_error) "internal error")
	((== e tm_net_contact_dead) "could not start contact")
	((== e tm_net_wrong_protocol) "wrong protocol")
	((== e tm_net_no_gnutls) "missing GnuTLS")
	(else "connection failed")))

;; Legacy password authentication
(tm-define (legacy-password-client-login-then
	    server-name port pseudo passwd cb)
  (with server (legacy-anonymous-client-start server-name port)
    (if (< server 0) (cb server (client-start-errno->string server))
        (begin
          (client-expect-answer
            server (string-append server-name ":" port))
	  (client-remote-eval* server
			       `(remote-login ,pseudo ,passwd)
                               (lambda (ret) (cb server ret)))))))

;; TLS password authentication
(tm-define (tls-password-client-login-then
	    server-name port pseudo passwd cb)
  (with server (tls-anonymous-client-start server-name port)
    (if (< server 0) (cb server (client-start-errno->string server))
        (begin
          (client-expect-answer
            server (string-append server-name ":" port))
	  (client-remote-eval* server
			       `(remote-login ,pseudo ,passwd)
                               (lambda (ret) (cb server ret)))))))

;; Dispatch
(tm-define (client-login-then server-name port pseudo credential cb)
  (cond
    ((and (list-2? credential) (== (car credential) `legacy-password))
     (legacy-password-client-login-then
      server-name port pseudo (second credential) cb))
    ((and (list-2? credential) (== (car credential) `tls-password))
     (tls-password-client-login-then
      server-name port pseudo (second credential) cb))
    ((or (nlist? credential) (null? credential))
     (cb "unknown credential type"))
    (else (cb (string-append "Unsupported authentication type "
			     (object->string (car credential)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login with code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Legacy code authentication
(tm-define (legacy-client-login-code-then
	    server-name port protocol pseudo code cb)
  (with server (legacy-anonymous-client-start server-name port)
    (if (< server 0) (cb server (client-start-errno->string server))
        (begin
          (client-expect-answer
            server (string-append server-name ":" port))
	  (client-remote-eval* server
			       `(remote-login-code ,pseudo ,code)
                               (lambda (ret) (cb server ret)))))))

;; TLS code authentication
(tm-define (tls-client-login-code-then
	    server-name port protocol pseudo code cb)
  (with server (tls-anonymous-client-start server-name port)
    (if (< server 0) (cb server (client-start-errno->string server))
        (begin
          (client-expect-answer
            server (string-append server-name ":" port))
	  (client-remote-eval* server
			       `(remote-login-code ,pseudo ,code)
                               (lambda (ret) (cb server ret)))))))

;; Dispatch
(tm-define (client-login-code-then server-name port protocol pseudo code cb)
  (cond
    ((== protocol `legacy)
     (legacy-client-login-code-then
      server-name port protocol pseudo code cb))
    ((== protocol `tls)
     (tls-client-login-code-then
      server-name port protocol pseudo code cb))
    (else (cb (string-append "Unsupported authentication type "
			     (object->string protocol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-logout server)
  (and-with server-con (ahash-ref client-active-connections server)
    (with (server-name server-port server-pseudo) server-con
      (with cb (lambda (ret)
		 (if (!= ret "bye")
		     (std-client-error "Logout failed"))
                 (remove-active-connection server server-name server-port server-pseudo)
		 (client-stop server)
                 (client-notify
                   :logged-out
                   (string-append "logged out from " server-name)))
	(client-remote-eval* server `(remote-logout) cb)))))
