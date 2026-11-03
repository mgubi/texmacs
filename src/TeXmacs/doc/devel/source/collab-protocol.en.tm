<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Transport and message protocol>

  <section|Overview>

  Communication between a client and a server goes through three layers:

  <\enumerate>
    <item>A byte stream over a <abbr|TCP> socket, optionally wrapped in a
    <abbr|TLS> session (<name|GnuTLS>) or, in \Plegacy\Q mode, encrypted
    with a symmetric key exchanged through <name|OpenSSL>.

    <item>A packet layer which splits the stream into messages. Each packet
    is a decimal length, a newline and the payload.

    <item>A <scheme> layer in which each payload is the printed form of a
    list <scm|(<scm-arg|serial> <scm-arg|command>)>, where
    <scm-arg|command> is itself an S-expression naming a <em|service> on the
    server or a <em|call-back> on the client, followed by its arguments.
  </enumerate>

  All requests are asynchronous: the sender registers a continuation and an
  error handler under the serial number of the message, and the receiver
  eventually answers with a special result or error command which carries
  the same serial number.

  <section|Sockets and contacts (<c++>)>

  <subsection|The socket classes>

  The socket code lives in <source-link|Plugins/Qt/QTMSockets.cpp|src/Plugins/Qt/QTMSockets.cpp> (with a
  copy in <source-link|Plugins/Qt6/|src/Plugins/Qt6>). It defines two <name|Qt> objects:

  <\description>
    <item*|<cpp|socket_server_rep>>The listening socket of a server. Its
    <cpp|start> method binds a non blocking socket on the configured port
    (all interfaces, <abbr|IPv4> or <abbr|IPv6> depending on
    <cpp|getaddrinfo>), listens with a backlog of 1024 and installs a
    <cpp|QSocketNotifier>. When the notifier fires, the slot
    <cpp|connection> accepts the connection, records the peer address in
    <cpp|address_from_id>, creates a <em|contact> (see below), wraps it in a
    <cpp|socket_link_rep> and calls the <scheme> function
    <scm|server-add> with the socket number. After a successful start,
    <cpp|start> also calls <scm|server-create-default-admin-account>.

    <item*|<cpp|socket_link_rep>>One end of a connection; it derives from
    both <cpp|QObject> and <cpp|tm_link_rep> (<source-link|System/Link/tm_link.hpp|src/System/Link/tm_link.hpp>).
    Incoming data is accumulated in <cpp|input_buffer> by the slot
    <cpp|data_set_ready> (reads of at most 16384 bytes), outgoing data is
    queued in <cpp|output_buffer> and flushed by <cpp|ready_to_send>. For a
    client side link, a successful start calls <scm|client-add>; when a link
    stops it calls <scm|server-logout-client> and <scm|server-remove> (server
    side) or <scm|client-remove> (client side), so that the <scheme> layer
    can clean up its tables.
  </description>

  A <em|contact> (<source-link|System/Link/tm_contact.hpp|src/System/Link/tm_contact.hpp>) abstracts the
  transport underneath the socket: <cpp|tm_contact_rep> has virtual
  methods <cpp|start>, <cpp|stop>, <cpp|send>, <cpp|receive>,
  <cpp|alive>, <cpp|active> and <cpp|last_error>. There are two families:

  <\itemize>
    <item>Plain contacts <cpp|socket_client_contact_rep> and
    <cpp|socket_server_contact_rep> (<verbatim|System/Link/socket_contact.*>),
    created by <cpp|make_socket_client_contact> and
    <cpp|make_socket_server_contact>.

    <item><abbr|TLS> contacts, created by <cpp|make_tls_client_contact> and
    <cpp|make_tls_server_contact> (<source-link|Plugins/Gnutls/gnutls.cpp|src/Plugins/Gnutls/gnutls.cpp>).
    The server always offers its X.509 certificate
    (<verbatim|$TEXMACS_SERVER_CERT_DIR/cert.pem> and <verbatim|key.pem>,
    where <verbatim|TEXMACS_SERVER_CERT_DIR> defaults to
    <verbatim|$TEXMACS_HOME_PATH/server>) and, if the preference
    <verbatim|"tls-server authentication anonymous"> is on, also anonymous
    Diffie\UHellman (priority string <verbatim|NORMAL:+ANON-DH>). Clients
    always request the <verbatim|anonymous> credential as well. Client
    certificates are not requested.
  </itemize>

  <subsection|The <c++> interface>

  The functions exported to <scheme> are declared in
  <source-link|System/Link/client_server.hpp|src/System/Link/client_server.hpp> and implemented in
  <source-link|texmacs_server.cpp|src/System/Link/texmacs_server.cpp> and <source-link|texmacs_client.cpp|src/System/Link/texmacs_client.cpp>. The glue
  is declared in <source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>. Sockets are
  identified on both sides by their integer file descriptor; this integer is
  what the <scheme> code calls a <em|client> (on the server) or a
  <em|server> (on the client).

  <\explain>
    <scm|(server-start)>, <scm|(server-stop)>,
    <scm|(server-started?)><explain-synopsis|control the listening socket>
  <|explain>
    <scm|server-start> loads the modules <verbatim|(server server-base)>,
    <verbatim|(server server-tmfs)>, <verbatim|(server server-menu)>,
    <verbatim|(server server-live)> and <verbatim|(server server-backup)>,
    optionally resets the server preferences or the administrator password
    (command line options <verbatim|-reset-server-preferences> and
    <verbatim|-reset-admin-password>), and starts a
    <cpp|socket_server_rep> on <cpp|get_server_port ()>: the value given by
    <verbatim|-port>, or else the preference <verbatim|"server port">
    (default 6561). <scm|(server-port-in-use)> returns the port of the
    running server, or 0.
  </explain>

  <\explain>
    <scm|(server-read <scm-arg|client>)>, <scm|(server-write <scm-arg|client>
    <scm-arg|s>)><explain-synopsis|packet input/output on the server>
  <|explain>
    <scm|server-read> returns the next complete packet received from
    <scm-arg|client>, or the empty string if no complete packet is
    available. <scm|server-write> sends <scm-arg|s> as one packet.
    <scm|(server-client-address <scm-arg|client>)> returns the peer address
    recorded when the connection was accepted.
  </explain>

  <\explain>
    <scm|(tls-client-start <scm-arg|host> <scm-arg|port>
    <scm-arg|credentials>)>, <scm|(legacy-client-start <scm-arg|host>
    <scm-arg|port>)><explain-synopsis|open a connection to a server>
  <|explain>
    Connect to a server and return the socket number, or a negative error
    code. The error codes are <cpp|TM_NET_*> constants in
    <source-link|client_server.hpp|src/System/Link/client_server.hpp>; <scm|(server-define-error-codes)> defines
    the corresponding <scheme> variables <scm|tm_net_success>,
    <scm|tm_net_invalid_port>, <scm|tm_net_no_gnutls>,
    <scm|tm_net_connection_failed>, etc. The <scm-arg|credentials> of
    <scm|tls-client-start> are a list of lists of strings; the client code
    always passes <scm|'((anonymous))>. The <scheme> wrappers
    <scm|tls-anonymous-client-start> and
    <scm|legacy-anonymous-client-start> in
    <source-link|client/client-authentication.scm|TeXmacs/progs/client/client-authentication.scm> are what the rest of the
    code uses; the legacy wrapper immediately calls
    <scm|(enter-secure-mode <scm-arg|server>)>.
  </explain>

  <\explain>
    <scm|(client-read <scm-arg|server>)>, <scm|(client-write
    <scm-arg|server> <scm-arg|s>)>, <scm|(client-stop
    <scm-arg|server>)><explain-synopsis|packet input/output on the client>
  <|explain>
    The client side counterparts of <scm|server-read> and
    <scm|server-write>; <scm|client-stop> closes the connection.
    <scm|(client-protocol-version)> returns the constant
    <cpp|TM_PROTOCOL_VERSION> of <source-link|texmacs_client.cpp|src/System/Link/texmacs_client.cpp> (currently 1,
    the version which introduced the tree cache).
  </explain>

  <subsection|Packets and legacy encryption>

  Packets are produced and parsed by <cpp|tm_link_rep::write_packet>,
  <cpp|complete_packet> and <cpp|read_packet> in
  <source-link|System/Link/tm_link.cpp|src/System/Link/tm_link.cpp>. A packet is the decimal length of the
  payload, a newline, and the payload itself:

  <\verbatim-code>
    44

    (3 (remote-file-load "localhost/~joe/a.tm"))
  </verbatim-code>

  In legacy mode (when <abbr|TLS> is not used), the client calls
  <cpp|tm_link_rep::secure_client> right after connecting: it sends the
  byte <verbatim|!> followed by a packet with its <abbr|RSA> public key. The
  server recognizes the leading <verbatim|!> in <cpp|read_packet>, generates
  a random secret with <cpp|secret_generate>, sends it back encrypted with
  the public key (<cpp|secure_server>), and from then on both sides encrypt
  each payload with <cpp|secret_encode>/<cpp|secret_decode>. These
  functions run the external <verbatim|openssl> program through temporary
  files (<source-link|Plugins/Openssl/openssl.cpp|src/Plugins/Openssl/openssl.cpp>). The server itself warns
  that this mode is weak; it is kept for backward compatibility and for
  builds without <name|GnuTLS>.

  <section|Serialization and dispatching (<scheme>)>

  <subsection|Messages>

  The <scheme> side is symmetric; the server part lives in
  <source-link|server/server-base.scm|TeXmacs/progs/server/server-base.scm>, the client part in
  <source-link|client/client-base.scm|TeXmacs/progs/client/client-base.scm>. A message is sent with

  <\scm-code>
    (tm-define (server-send client cmd)

    \ \ (server-write client (object-\<gtr\>string* (list server-serial cmd)))

    \ \ (set! server-serial (+ server-serial 1)))
  </scm-code>

  and similarly <scm|client-send> with <scm|client-serial>. The function
  <scm|object-\<gtr\>string*> (<source-link|kernel/library/base.scm|TeXmacs/progs/kernel/library/base.scm>) prints lists,
  numbers, strings and symbols with <scm|object-\<gtr\>string> and converts trees
  to <scheme> trees first; any other object is transmitted as <scm|#f>.
  Hence arguments of services must be built out of lists, strings, numbers,
  symbols and booleans. Documents are usually transmitted either as
  <scheme> trees or as strings in the <TeXmacs> file format (see for
  instance <scm|remote-file-load>). The receiver parses the payload with
  <scm|string-\<gtr\>object>, i.e. with the <scheme> reader.

  The serial counters are global per process, not per connection; the
  continuation tables therefore store the connection together with the
  continuation, and a result is only accepted if it comes from the
  connection to which the request was sent.

  <subsection|Polling>

  The socket notifiers fill the input buffers, but the <scheme> layer reads
  them by polling. When a connection is established, <scm|server-add> (or
  <scm|client-add>) marks it active and starts a <scm|delayed> loop which
  repeatedly calls <scm|server-read> (resp. <scm|client-read>). The pause
  between two polls starts at 1<nbsp>ms, grows by a factor 1.01 after each
  empty poll up to 2500<nbsp>ms, and is reset to 1<nbsp>ms whenever a message
  is received:

  <\scm-code>
    (tm-define (server-add client)

    \ \ (ahash-set! server-client-active? client #t)

    \ \ (with wait 1

    \ \ \ \ (delayed

    \ \ \ \ \ \ (:while (ahash-ref server-client-active? client))

    \ \ \ \ \ \ (:pause ((lambda () (inexact-\<gtr\>exact (round wait)))))

    \ \ \ \ \ \ (:do (set! wait (min (* 1.01 wait) 2500)))

    \ \ \ \ \ \ (with msg (server-read client)

    \ \ \ \ \ \ \ \ (when (!= msg "")

    \ \ \ \ \ \ \ \ \ \ (with (msg-id msg-cmd) (string-\<gtr\>object msg)

    \ \ \ \ \ \ \ \ \ \ \ \ (server-eval (list client msg-id) msg-cmd)

    \ \ \ \ \ \ \ \ \ \ \ \ (set! wait 1)))))))
  </scm-code>

  Consequently, after a long idle period the latency of a request may reach
  a few seconds. At most one message is handled per poll.

  <subsection|Envelopes and dispatching>

  Each incoming message is evaluated in an <em|envelope>, the list
  <scm|(<scm-arg|connection> <scm-arg|serial>)>. On the server,
  <scm|server-eval> looks up the head symbol of the command in
  <scm|service-dispatch-table> and applies the registered function to the
  envelope and the arguments. Unknown commands are answered with an error
  (<verbatim|invalid command '...'>); in particular, a client can only
  invoke functions which were explicitly declared as services. Services are
  declared with the macro <scm|tm-service>:

  <\scm-code>
    (tm-define-macro (tm-service proto . body)

    \ \ ...

    \ \ `(begin

    \ \ \ \ \ (tm-define (,(symbol-append 'service- fun) envelope ,@args)

    \ \ \ \ \ \ \ (with-database (server-database)

    \ \ \ \ \ \ \ \ \ (catch #t

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (lambda () ,@body)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (lambda (key . err-args)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ... (server-error envelope msg)))))

    \ \ \ \ \ (ahash-set! service-dispatch-table

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ',fun ,(symbol-append 'service- fun))))
  </scm-code>

  So <scm|(tm-service (remote-logout) ...)> defines a function
  <scm|service-remote-logout>, evaluates its body with the global server
  database as current database, catches all errors (they are logged with
  <scm|server-log-write> and sent back as an error) and registers the
  function under the symbol <scm|remote-logout>. The variable
  <scm|envelope> is bound inside the body.

  The client uses the analogous macro <scm|tm-call-back>, the table
  <scm|call-back-dispatch-table> and the evaluator <scm|client-eval>. Note
  that <scm|tm-call-back> defines a function with the name of the call-back
  itself (not prefixed), and that client call-backs do not install a
  database.

  <subsection|Answers and continuations>

  The value returned by a service body is ignored. A service must
  explicitly answer with

  <\explain>
    <scm|(server-return <scm-arg|envelope> <scm-arg|value>)>,
    <scm|(server-error <scm-arg|envelope>
    <scm-arg|message>)><explain-synopsis|answer a request>
  <|explain>
    These send <scm|(client-remote-result <scm-arg|serial>
    <scm-arg|value>)>, resp. <scm|(client-remote-error <scm-arg|serial>
    <scm-arg|message>)>, back to the client, where <scm-arg|serial> is taken
    from the envelope. On the client, <scm|client-return> and
    <scm|client-error> send <scm|server-remote-result> and
    <scm|server-remote-error> in the same way.
  </explain>

  A service which never answers leaves the continuation of the client
  pending forever; there is no time-out at the <scheme> level. Requests are
  sent with

  <\explain>
    <scm|(client-remote-eval <scm-arg|server> <scm-arg|cmd> <scm-arg|cont>
    [<scm-arg|err-handler>])><explain-synopsis|asynchronous request to a
    server>
  <|explain>
    Send <scm-arg|cmd> to <scm-arg|server>, and call <scm-arg|cont> on the
    result, or <scm-arg|err-handler> (default <scm|std-client-error>, which
    displays the message) on an error. The variant
    <scm|client-remote-eval*> uses the same function for both cases. The
    helpers <scm|client-remote-then> and <scm|client-remote-then-cb>
    implement the common convention of <source-link|client-base.scm|TeXmacs/progs/client/client-base.scm> that a
    list is a successful result and a string an error message.
  </explain>

  <\explain>
    <scm|(server-remote-eval <scm-arg|client> <scm-arg|cmd> <scm-arg|cont>
    [<scm-arg|err-handler>])><explain-synopsis|request from the server to a
    client>
  <|explain>
    The server may also send requests to clients, for instance to push chat
    messages (<scm|chat-room-receive>), notifications
    (<scm|client-push-notifications>), live modifications
    (<scm|live-modify>) or to announce that an account was deleted
    (<scm|client-account-deleted>). The client answers through its
    call-backs.
  </explain>

  The results are received by the service <scm|server-remote-result> and
  the call-back <scm|client-remote-result> (and their error variants),
  which look up the continuation under the serial number, check that the
  answer comes from the right connection, remove the entry and call the
  continuation. Thanks to this mechanism, client code is written in
  continuation passing style; for instance, the login dialog first calls
  <scm|client-login-then>, then <scm|client-protocol-version-then>, then
  fetches the account information, each step in the continuation of the
  previous one (see <scm|client-login-home> in
  <source-link|client/client-widgets.scm|TeXmacs/progs/client/client-widgets.scm>).

  <subsection|Debugging>

  With the debug flag <verbatim|remote> switched on (<scm|(debug-set
  "remote" #t)>), both evaluators print every incoming
  message. Socket level traces are printed when the <c++> debug flags for
  sockets and input/output are on (<cpp|DEBUG_SOCKETS>, <cpp|DEBUG_IO>).
  The flag <verbatim|live> traces live editing.

  <section|Protocol version>

  After login, clients send <scm|(remote-protocol-version <scm-arg|n>)>
  with <scm|(client-protocol-version)>. The server stores the number per
  user in <scm|server-client-version> (reset to 0 at each login), and
  services may test it with <scm|(client-version\<gtr\>=? <scm-arg|uid>
  <scm-arg|n>)>. Version 1 means that the client understands
  <markup|cache-ref> trees (see the tree cache in <hlink|the <TeXmacs>
  server|collab-server.en.tm>); a server therefore never sends such trees
  to older clients. When the wire format evolves, increase
  <cpp|TM_PROTOCOL_VERSION> and guard the new behaviour with
  <scm|client-version\<gtr\>=?>.

  <section|List of services>

  The following table lists the services currently declared with
  <scm|tm-service>, grouped by module (all under
  <source-link|progs/server/|TeXmacs/progs/server>).

  <\description>
    <item*|<source-link|server-base.scm|TeXmacs/progs/server/server-base.scm>><scm|remote-login>,
    <scm|remote-login-code>, <scm|remote-logout>, <scm|remote-logged?>,
    <scm|remote-protocol-version>, <scm|new-account>,
    <scm|confirm-pending-account>, <scm|remote-reset-credentials>,
    <scm|remote-get-account>, <scm|remote-set-account>,
    <scm|remote-get-accounts>, <scm|remote-deletion-plan>,
    <scm|remote-delete-account>, <scm|remote-public-preferences>,
    <scm|remote-admin-preferences>, <scm|remote-admin-preferences-form>,
    <scm|remote-set-preferences>, <scm|server-license>, <scm|remote-eval>,
    and the internal <scm|server-remote-result>,
    <scm|server-remote-error>.

    <item*|<source-link|server-tmfs.scm|TeXmacs/progs/server/server-tmfs.scm>><scm|remote-identifier>,
    <scm|remote-get-versions>, <scm|remote-file-create>,
    <scm|remote-file-load>, <scm|remote-file-save>,
    <scm|remote-file-remove>, <scm|remote-dir-create>,
    <scm|remote-dir-load>, <scm|remote-dir-remove>.

    <item*|<source-link|server-db.scm|TeXmacs/progs/server/server-db.scm>><scm|remote-get-field>,
    <scm|remote-set-field>, <scm|remote-get-attributes>,
    <scm|remote-get-entry>, <scm|remote-set-entry>,
    <scm|remote-create-entry>, <scm|remote-search>,
    <scm|remote-search-user>, <scm|remote-get-user-pseudo>,
    <scm|remote-get-user-name>.

    <item*|<source-link|server-sync.scm|TeXmacs/progs/server/server-sync.scm>, <source-link|server-db-sync.scm|TeXmacs/progs/server/server-db-sync.scm>><scm|remote-sync-list>,
    <scm|remote-upload>, <scm|remote-download>,
    <scm|remote-remove-several>, <scm|remote-db-changes>,
    <scm|remote-db-sync>.

    <item*|<source-link|server-live.scm|TeXmacs/progs/server/server-live.scm>><scm|live-open>, <scm|live-modify>,
    <scm|live-exists?>, <scm|remote-list-live>.

    <item*|<source-link|server-chat.scm|TeXmacs/progs/server/server-chat.scm>><scm|remote-chat-room-create>,
    <scm|remote-list-chat-rooms>, <scm|remote-chat-room-open>,
    <scm|remote-chat-room-messages-reset>, <scm|remote-mail-open>,
    <scm|remote-shared>, <scm|remote-send-message>.

    <item*|Others><scm|remote-pending-notifications>,
    <scm|remote-ack-notifications> (<source-link|server-notifications.scm|TeXmacs/progs/server/server-notifications.scm>) and
    <scm|remote-get-cache-ref> (<source-link|server-cache.scm|TeXmacs/progs/server/server-cache.scm>).
  </description>

  The client call-backs are <scm|client-remote-result>,
  <scm|client-remote-error>, <scm|client-account-deleted>,
  <scm|local-eval> (disabled: its body is guarded by <scm|(when #f ...)>),
  <scm|chat-room-receive>, <scm|client-push-notifications> and
  <scm|live-modify>.

  <section|How to add a new service>

  Adding a request type requires a service on the server and, usually, a
  small wrapper on the client.

  <\enumerate>
    <item>Declare the service in a server module. Check the user with
    <scm|server-get-user>, check the access rights with <scm|db-allow?>, and
    always answer with <scm|server-return> or <scm|server-error>:

    <\scm-code>
      (tm-service (remote-word-count rname)

      \ \ (with-remote-context rname

      \ \ \ \ (let* ((uid (server-get-user envelope))

      \ \ \ \ \ \ \ \ \ \ \ (r (server-file-load uid rname)))

      \ \ \ \ \ \ (if (== (car r) :error)

      \ \ \ \ \ \ \ \ \ \ (server-error envelope (cadr r))

      \ \ \ \ \ \ \ \ \ \ (server-return envelope

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (string-length (cadr r)))))))
    </scm-code>

    <item>Make sure that the module is loaded by the server. The modules
    loaded by <cpp|server_start> are <verbatim|server-base>,
    <verbatim|server-tmfs>, <verbatim|server-menu>, <verbatim|server-live>
    and <verbatim|server-backup>; <verbatim|server-menu> in turn uses
    <verbatim|server-widgets>, <verbatim|server-db>, <verbatim|server-sync>
    and <verbatim|server-chat>. Either add your module to the
    <verbatim|:use> list of one of them or load it from
    <cpp|server_start>.

    <item>On the client, wrap the request in a function written in
    continuation passing style:

    <\scm-code>
      (tm-define (remote-word-count server u cont)

      \ \ (client-remote-eval server `(remote-word-count ,u) cont

      \ \ \ \ (lambda (err) (set-message err "word count"))))
    </scm-code>

    <item>If the server needs to push data to clients, declare a call-back
    with <scm|tm-call-back> in a client module which is loaded whenever a
    connection exists (for instance a module used by
    <source-link|client-base.scm|TeXmacs/progs/client/client-base.scm> or <source-link|client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm>), and send it
    with <scm|server-remote-eval>. The call-back must answer with
    <scm|client-return> or <scm|client-error> if the server supplied a
    meaningful continuation.

    <item>If the new service changes the meaning of existing messages,
    increase <cpp|TM_PROTOCOL_VERSION> and test <scm|client-version\<gtr\>=?> on
    the server.

    <item>If the service can be disabled by the administrator, add a
    preference named <verbatim|"server service ..."> with
    <scm|define-preferences> in <source-link|server-authentication.scm|TeXmacs/progs/server/server-authentication.scm> (or in
    your module) and test it in the service body, as done for instance in
    <scm|remote-pending-notifications>. Preferences whose name starts with
    <verbatim|"server"> are automatically exposed to remote administrators
    by <scm|server-admin-preferences>.
  </enumerate>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
