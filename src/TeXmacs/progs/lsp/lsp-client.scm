;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-client.scm
;; DESCRIPTION : clients of language servers (Language Server Protocol)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A client is the connection to one language server, for one language and
;; one root directory (a project). The messages go through an LSP link
;; (lsp-start, lsp-send...: src/System/Link/lsp_link.cpp), which gives what
;; the server sends to lsp-client-receive; a client can also be made on
;; other procedures (lsp-client-make), as the tests do.
;;
;; The life of a client: initializing (the request initialize is sent, the
;; other messages wait), running (initialize answered, initialized sent),
;; stopping (shutdown sent), dead (the server has ended or could not
;; start). A request has a handler, called with the result and the error
;; of the answer (one of them #f); the requests which are not answered when
;; the server ends get an error. The requests of the server (configuration,
;; progress...) are answered by the handlers of lsp-server-request-set!,
;; the notifications go to those of lsp-notification-set!.

(texmacs-module (lsp lsp-client)
  (:use (lsp lsp-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The language servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the commands which may start a server for a language (prog-language), the
;; first whose program is found is used
(define lsp-servers (make-ahash-table))

(tm-define (lsp-server-set! lan cmds)
  (:synopsis "Use the first command of @cmds found for the language @lan")
  (ahash-set! lsp-servers lan cmds))

(lsp-server-set! "cpp" '("clangd"))
(lsp-server-set! "c" '("clangd"))
(lsp-server-set! "python" '("pylsp" "pyright-langserver --stdio"
                            "basedpyright-langserver --stdio"))
(lsp-server-set! "javascript" '("typescript-language-server --stdio"))
(lsp-server-set! "java" '("jdtls"))
(lsp-server-set! "scala" '("metals"))
(lsp-server-set! "julia" '("julia-language-server"))
(lsp-server-set! "json" '("vscode-json-language-server --stdio"))
(lsp-server-set! "latex" '("texlab"))

(tm-define (lsp-server-known? lan)
  (:synopsis "Is there a language server for @lan (installed or not)?")
  (and (ahash-ref lsp-servers lan) #t))

(define (command-program cmd)
  (with l (list-filter (string-decompose cmd " ") (lambda (x) (!= x "")))
    (if (null? l) "" (car l))))

(tm-define (lsp-server-command lan)
  (:synopsis "The command of the language server for @lan, or #f")
  (list-find (or (ahash-ref lsp-servers lan) '())
             (lambda (cmd)
               (with p (command-program cmd)
                 (if (>= (string-search-forwards "/" 0 p) 0)
                     (url-exists? (system->url p))
                     (url-exists-in-path? p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a client is a table: name, state, send, listen, stop (procedures of the
;; link), next-id, pending (id -> handler), queue (messages which wait for
;; initialized, newest first), capabilities, root, log, documents

(tm-define (lsp-client-make name send listen stop)
  (:synopsis "A client named @name whose messages go through the procedures")
  (with c (make-ahash-table)
    (ahash-set! c 'name name)
    (ahash-set! c 'state 'new)
    (ahash-set! c 'send send)
    (ahash-set! c 'listen listen)
    (ahash-set! c 'stop stop)
    (ahash-set! c 'next-id 0)
    (ahash-set! c 'pending (make-ahash-table))
    (ahash-set! c 'queue '())
    (ahash-set! c 'capabilities (lsp-object))
    (ahash-set! c 'log '())
    (ahash-set! c 'documents (make-ahash-table))
    c))

(tm-define (lsp-client-ref c key) (ahash-ref c key))
(tm-define (lsp-client-set! c key val) (ahash-set! c key val))

(tm-define (lsp-client-state c) (ahash-ref c 'state))
(tm-define (lsp-client-alive? c)
  (in? (ahash-ref c 'state) '(new initializing running)))
(tm-define (lsp-client-running? c) (== (ahash-ref c 'state) 'running))
(tm-define (lsp-client-capabilities c) (ahash-ref c 'capabilities))
(tm-define (lsp-client-log c) (reverse (ahash-ref c 'log)))

(define lsp-log-length 200)

(tm-define (lsp-client-log-add c kind s)
  (with l (cons (list kind s) (ahash-ref c 'log))
    (ahash-set! c 'log (if (> (length l) lsp-log-length)
                           (sublist l 0 lsp-log-length)
                           l))))

(define (client-send c msg)
  ;; sends @msg (an stree), or keeps it until the server is initialized
  (with st (ahash-ref c 'state)
    (cond ((or (== st 'running) (== st 'stopping)
               (and (== st 'initializing) (== (lsp-ref msg "method")
                                               "initialize")))
           ((ahash-ref c 'send) (lsp-write msg)))
          ((in? st '(new initializing))
           (ahash-set! c 'queue (cons msg (ahash-ref c 'queue))))
          (else #f))))

(tm-define (lsp-request c method params handler)
  (:synopsis "Sends the request @method to @c, the answer goes to @handler")
  ;; @handler is called with the result and the error (one of them #f);
  ;; @params is #f for none. The id of the request is returned.
  (with id (+ (ahash-ref c 'next-id) 1)
    (ahash-set! c 'next-id id)
    (if (lsp-client-alive? c)
        (begin
          (ahash-set! (ahash-ref c 'pending) id handler)
          (client-send c (apply lsp-object
                                `("jsonrpc" "2.0" "id" ,(lsp-number id)
                                  "method" ,method
                                  ,@(if params (list "params" params) '())))))
        (handler #f (lsp-object "code" (lsp-number -32099)
                                "message" "the server has ended")))
    id))

(tm-define (lsp-notify c method params)
  (:synopsis "Sends the notification @method to @c")
  (client-send c (apply lsp-object
                        `("jsonrpc" "2.0" "method" ,method
                          ,@(if params (list "params" params) '())))))

(tm-define (lsp-cancel c id)
  (:synopsis "Cancels the request @id: its handler will not be called")
  (when (ahash-ref (ahash-ref c 'pending) id)
    (ahash-remove! (ahash-ref c 'pending) id)
    (lsp-notify c "$/cancelRequest" (lsp-object "id" (lsp-number id)))))

(tm-define (lsp-client-listen c msecs)
  ((ahash-ref c 'listen) msecs))

(tm-define (lsp-wait c done? msecs)
  (:synopsis "Waits for the messages of @c until @done? or @msecs have passed")
  (with end (+ (texmacs-time) msecs)
    (while (and (not (done?)) (< (texmacs-time) end))
      (lsp-client-listen c (min 50 (max 0 (- end (texmacs-time))))))
    (done?)))

(tm-define (lsp-request-wait c method params msecs)
  (:synopsis "Sends a request to @c and waits for the answer")
  ;; the answer is (result r), (error e), or (timeout)
  (let* ((answer #f)
         (id (lsp-request c method params
                          (lambda (r e)
                            (set! answer (if e (list 'error e)
                                             (list 'result r)))))))
    (if (lsp-wait c (lambda () answer) msecs) answer
        (begin
          (lsp-cancel c id)
          (list 'timeout)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization and end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (root-name root)
  (url->system (url-tail (system->url root))))

(define (client-capabilities)
  ;; what TeXmacs does with the answers of a server
  (lsp-object
   "general" (lsp-object "positionEncodings" (lsp-array "utf-16"))
   "textDocument"
   (lsp-object
    "synchronization" (lsp-object "didSave" (lsp-boolean #t)
                                  "dynamicRegistration" (lsp-boolean #f))
    "publishDiagnostics" (lsp-object "relatedInformation" (lsp-boolean #f))
    "hover" (lsp-object "contentFormat" (lsp-array "plaintext" "markdown"))
    "completion" (lsp-object
                  "completionItem" (lsp-object "snippetSupport"
                                               (lsp-boolean #f)))
    "definition" (lsp-object))
   "workspace" (lsp-object "configuration" (lsp-boolean #t)
                           "workspaceFolders" (lsp-boolean #t))
   "window" (lsp-object "workDoneProgress" (lsp-boolean #t))))

(tm-define (lsp-client-initialize c root)
  (:synopsis "Initializes the server of @c for the directory @root")
  ;; (@root is a system path, or #f)
  (ahash-set! c 'root root)
  (ahash-set! c 'state 'initializing)
  (let* ((uri (if root (lsp-path->uri root) (lsp-null)))
         (folders (if root
                      (lsp-array (lsp-object "uri" uri
                                             "name" (root-name root)))
                      (lsp-null))))
    (lsp-request
     c "initialize"
     (lsp-object "processId" (lsp-null)
                 "clientInfo" (lsp-object "name" "TeXmacs")
                 "rootUri" uri
                 "workspaceFolders" folders
                 "capabilities" (client-capabilities))
     (lambda (r e)
       (if e
           (begin
             (lsp-client-log-add c "error" (or (lsp-ref e "message") ""))
             (lsp-client-kill c))
           (begin
             (ahash-set! c 'capabilities
                         (or (lsp-ref r "capabilities") (lsp-object)))
             (ahash-set! c 'state 'running)
             (lsp-notify c "initialized" (lsp-object))
             (with q (reverse (ahash-ref c 'queue))
               (ahash-set! c 'queue '())
               (for-each (lambda (msg) (client-send c msg)) q))))))))

(tm-define (lsp-client-start cmd root)
  (:synopsis "A client of the server started by @cmd, for @root")
  ;; the client is dead if the server cannot be started (its log says why)
  (letrec ((c #f)
           (id (lsp-start cmd (lambda (kind data)
                                (when c (lsp-client-receive c kind data))))))
    (if (string? id)
        (begin
          (set! c (lsp-client-make cmd ignore ignore ignore))
          (ahash-set! c 'state 'dead)
          (lsp-client-log-add c "error" id))
        (begin
          (set! c (lsp-client-make cmd
                                   (lambda (s) (lsp-send id s))
                                   (lambda (ms) (lsp-listen id ms))
                                   (lambda () (lsp-stop id))))
          (lsp-client-initialize c root)))
    c))

(define (client-ended c)
  ;; the server has ended: the handlers of its requests get an error
  (ahash-set! c 'state 'dead)
  (ahash-set! c 'queue '())
  (let* ((pending (ahash-ref c 'pending))
         (handlers (map cdr (ahash-table->list pending)))
         (e (lsp-object "code" (lsp-number -32099)
                        "message" "the server has ended")))
    (ahash-set! c 'pending (make-ahash-table))
    (for-each (lambda (h) (h #f e)) handlers)))

(tm-define (lsp-client-kill c)
  (:synopsis "Ends the server of @c at once")
  (when (!= (ahash-ref c 'state) 'dead)
    ((ahash-ref c 'stop))
    (client-ended c)))

(tm-define (lsp-client-stop c)
  (:synopsis "Ends the server of @c: shutdown, exit, then at most 3 s")
  (if (not (lsp-client-running? c)) (lsp-client-kill c)
      (begin
        (lsp-request-wait c "shutdown" #f 2000)
        (lsp-notify c "exit" #f)
        (ahash-set! c 'state 'stopping)
        (lsp-wait c (lambda () (== (ahash-ref c 'state) 'dead)) 1000)
        (lsp-client-kill c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What the servers send
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; method -> procedure (client params) giving the result of a request
(define server-requests (make-ahash-table))
;; method -> list of procedures (client params) for a notification
(define notifications (make-ahash-table))

(tm-define (lsp-server-request-set! method proc)
  (:synopsis "Answer the requests @method of the servers with @proc")
  (ahash-set! server-requests method proc))

(tm-define (lsp-notification-set! method key proc)
  (:synopsis "Call @proc (named @key) for the notifications @method")
  (with l (or (ahash-ref notifications method) '())
    (ahash-set! notifications method
                (append (list-filter l (lambda (x) (!= (car x) key)))
                        (list (cons key proc))))))

(define (answer c id result error)
  (client-send c (if error
                     (lsp-object "jsonrpc" "2.0" "id" id "error" error)
                     (lsp-object "jsonrpc" "2.0" "id" id "result" result))))

(define (handle-request c id method params)
  (with proc (ahash-ref server-requests method)
    (if (not proc)
        (answer c id #f (lsp-object "code" (lsp-number -32601)
                                    "message" (string-append
                                               "not supported: " method)))
        (with r (catch #t
                  (lambda () (list (proc c params)))
                  (lambda args #f))
          (if r (answer c id (car r) #f)
              (answer c id #f (lsp-object "code" (lsp-number -32603)
                                          "message" "internal error")))))))

(define (handle-answer c id msg)
  (let* ((n (lsp-number-value id))
         (pending (ahash-ref c 'pending))
         (h (and n (ahash-ref pending n))))
    (when h
      (ahash-remove! pending n)
      (with e (lsp-ref msg "error")
        (if e (h #f e) (h (or (lsp-ref msg "result") (lsp-null)) #f))))))

(define (handle-notification c method params)
  (for-each (lambda (x) ((cdr x) c params))
            (or (ahash-ref notifications method) '())))

(tm-define (lsp-client-handle c msg)
  (:synopsis "Handles the message @msg (an stree) of the server of @c")
  (let ((id (lsp-ref msg "id"))
        (method (lsp-ref msg "method")))
    (cond ((and (string? method) id)
           (handle-request c id method (lsp-ref msg "params")))
          ((string? method)
           (handle-notification c method (lsp-ref msg "params")))
          (id (handle-answer c id msg)))))

(tm-define (lsp-client-receive c kind data)
  (:synopsis "What the link of @c gives: a message, errors or its end")
  (cond ((== kind "message")
         (with msg (lsp-read data)
           ;; (the converter reads anything: what is not an object is not
           ;; a message)
           (if (func? msg 'attr) (lsp-client-handle c msg)
               (lsp-client-log-add c "error"
                                   (string-append "not JSON: " data)))))
        ((== kind "stderr") (lsp-client-log-add c "stderr" data))
        ((== kind "exit") (client-ended c))))

;; the requests of the servers which TeXmacs answers by default

(lsp-server-request-set! "workspace/configuration"
  (lambda (c params)
    ;; no settings: null for each item
    (lsp-list->array (map (lambda (x) (lsp-null))
                          (lsp-items (lsp-ref params "items"))))))

(lsp-server-request-set! "workspace/workspaceFolders"
  (lambda (c params)
    (with root (ahash-ref c 'root)
      (if root
          (lsp-array (lsp-object "uri" (lsp-path->uri root)
                                 "name" (root-name root)))
          (lsp-null)))))

(lsp-server-request-set! "window/workDoneProgress/create"
  (lambda (c params) (lsp-null)))

(lsp-server-request-set! "client/registerCapability"
  (lambda (c params) (lsp-null)))

(lsp-server-request-set! "client/unregisterCapability"
  (lambda (c params) (lsp-null)))

(lsp-server-request-set! "window/showMessageRequest"
  (lambda (c params)
    (lsp-client-log-add c "message" (or (lsp-ref params "message") ""))
    (lsp-null)))

(lsp-notification-set! "window/logMessage" 'log
  (lambda (c params)
    (lsp-client-log-add c "log" (or (lsp-ref params "message") ""))))

(lsp-notification-set! "window/showMessage" 'log
  (lambda (c params)
    (lsp-client-log-add c "message" (or (lsp-ref params "message") ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The clients of the languages and projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define clients (make-ahash-table))

(tm-define (lsp-client-find lan root)
  (:synopsis "The running client for @lan and @root, or #f (none is started)")
  (with c (ahash-ref clients (string-append lan "\n" (or root "")))
    (and c (lsp-client-alive? c) c)))

(tm-define (lsp-client-for lan root)
  (:synopsis "The client for the language @lan and the directory @root")
  ;; a server is started if there is none yet; #f if there is no server
  (let* ((key (string-append lan "\n" (or root "")))
         (c (ahash-ref clients key)))
    (if (and c (lsp-client-alive? c)) c
        (with cmd (lsp-server-command lan)
          (and cmd
               (with c (lsp-client-start cmd root)
                 (ahash-set! clients key c)
                 c))))))

(tm-define (lsp-clients)
  (:synopsis "The clients of the servers which are running")
  (list-filter (map cdr (ahash-table->list clients)) lsp-client-alive?))

(tm-define (lsp-stop-all)
  (:synopsis "Ends all the servers")
  (for-each lsp-client-stop (lsp-clients))
  (set! clients (make-ahash-table)))
