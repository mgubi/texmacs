<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <TeXmacs> server>

  <section|Starting a server>

  <subsection|From the command line>

  A server is an ordinary <TeXmacs> process with server mode enabled. The
  relevant command line options are parsed in
  <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>:

  <\description>
    <item*|<verbatim|-server>>Enable server mode (<cpp|set_server>). At the
    end of the initialization, <cpp|server_start> is called provided that
    <cpp|server_can_start> succeeds. The latter refuses to start when
    <name|GnuTLS> is available, the preference <verbatim|"tls-server"> is on
    and no certificate is present.

    <item*|<verbatim|-port> <em|n>>Override the preference
    <verbatim|"server port"> (default 6561).

    <item*|<verbatim|-headless>, <verbatim|-H>>Run without graphical
    interface. In headless mode, <TeXmacs> normally quits after the
    command line has been processed, but not in server mode.

    <item*|<verbatim|-reset-server-preferences>,
    <verbatim|-reset-admin-password>>Call
    <scm|server-reset-preferences> resp. <scm|server-reset-admin-password>
    when the server starts. The new administrator password is written to the
    server log (and shown in an auxiliary buffer when a graphical interface
    is present).

    <item*|<verbatim|-delete-server-data>>Remove the directory
    <verbatim|$TEXMACS_HOME_PATH/server> with all accounts, files and the
    server database.

    <item*|<verbatim|--tls-no-verify>>Client side option: skip the
    verification of server certificates (useful for headless clients which
    cannot ask the user to trust a self-signed certificate).
  </description>

  A typical invocation on a machine without display is therefore

  <\shell-code>
    texmacs -headless -server -port 6561
  </shell-code>

  The environment variable <verbatim|TEXMACS_SERVER_CERT_DIR> (default
  <verbatim|$TEXMACS_HOME_PATH/server>) indicates where the certificate
  <verbatim|cert.pem> and the private key <verbatim|key.pem> are stored.
  <verbatim|SIGPIPE> is ignored so that a client which aborts its
  connection does not kill the server.

  <subsection|From the user interface>

  In a graphical session, the server is started from the menu
  <menu|Remote|Start server>, which calls <scm|server-safe-start>
  (<source-link|server/server-menu.scm|TeXmacs/progs/server/server-menu.scm>). If <abbr|TLS> is supported and on
  but no certificate exists, the user is first offered to create a
  self-signed certificate (<scm|open-certificate-warning> in
  <source-link|server-widgets.scm|TeXmacs/progs/server/server-widgets.scm>, which ends up in the glue function
  <scm|generate-self-signed-certificate>). The server menu also allows to
  stop and restart the server and to reset the administrator password or
  the preferences. See also the user documentation <hlink|Starting and
  configuring the server|../../main/remote/server/man-start.en.tm> and
  <hlink|TLS certificates|../../main/remote/server/man-certificates.en.tm>.

  <subsection|First start>

  After the listening socket has been created,
  <scm|server-create-default-admin-account> creates an account
  <verbatim|admin> with a random password of 20 characters if no user
  exists yet. The password is written to the log with level
  <scm|warning>; the administrator is expected to change it immediately.

  <section|Persistent state>

  All server data are stored below <verbatim|$TEXMACS_HOME_PATH/server>:

  <\description-paragraphs>
    <item*|<verbatim|global.tmdb>>The server database, returned by
    <scm|(global-database)> (<source-link|database/db-base.scm|TeXmacs/progs/database/db-base.scm>) and
    installed by <scm|tm-service> as the current database of each service
    (<scm|server-database>). It contains one entry per user, file,
    directory, file version list, live document, chat room, chat message,
    message and notification.

    <item*|<verbatim|users.scm>>An association list from user identifiers
    to <scm|(<scm-arg|pseudo> <scm-arg|name> <scm-arg|credentials>
    <scm-arg|email> <scm-arg|admin?>)>, cached in the table
    <scm|server-users>. User entries also exist in the database (with type
    <verbatim|"user">); the user identifier is currently the pseudo itself
    (see <scm|server-set-user-info>).

    <item*|<verbatim|pending-users.scm>,
    <verbatim|reset-credentials-users.scm>>Accounts waiting for email
    confirmation and pending credential reset codes.

    <item*|<verbatim|email-new-account.txt>,
    <verbatim|email-reset-credentials.txt>, <verbatim|license.tm>>Templates
    of the emails sent to users and the license document shown to new users.
    The email templates are copied from
    <verbatim|progs/server/server-email-*.txt> on first use.

    <item*|Numbered directories>The contents of remote files, live
    documents and messages. See <hlink|the remote file
    system|collab-remote-fs.en.tm>.
  </description-paragraphs>

  The database layer is described in the <scheme> modules of
  <verbatim|progs/database/>. The points which matter for the server are:

  <\itemize>
    <item>The <c++> class <cpp|database_rep>
    (<source-link|Plugins/Database/database.hpp|src/Plugins/Database/database.hpp>) stores lines
    <cpp|(id, attr, val, created, expires)> and never forgets anything
    unless history is disabled: removing a field or an entry only sets an
    expiration time. Queries take a time argument, so the state of the
    database at any past moment can be reconstructed. In <scheme>, the time
    is the global variable <scm|db-time>, which is <scm|:now> by default and
    can be changed with <scm|with-time> (<scm|:always> means: at any time).
    The macro <scm|with-time-stamp> makes <scm|db-create-entry> add a
    <verbatim|"date"> field.

    <item>Access rights are ordinary fields: <verbatim|"owner">,
    <verbatim|"readable"> and <verbatim|"writable"> contain lists of user
    identifiers, where the special value <verbatim|"all"> denotes everybody.
    <scm|(db-allow? <scm-arg|id> <scm-arg|uid> <scm-arg|attr>)>
    (<source-link|database/db-users.scm|TeXmacs/progs/database/db-users.scm>) checks whether <scm-arg|uid> (or a
    group it belongs to, see <scm|db-expand-user>) appears in the field, and
    owners are allowed everything. <scm|(with-user <scm-arg|uid> ...)> sets
    <scm|db-current-user>, which the wrappers of <scm|db-get-field>,
    <scm|db-set-field>, etc. take into account; <scm|(with-user #t ...)>
    bypasses the checks. Services usually test <scm|db-allow?> explicitly.
  </itemize>

  <section|Accounts and authentication>

  <subsection|Credentials>

  A client authenticates with a pseudo and a password after the connection
  (legacy or <abbr|TLS>) has been established; the transport layer itself
  is anonymous. On the client, a credential is a list such as
  <scm|(tls-password <scm-arg|p>)> or <scm|(legacy-password
  <scm-arg|p>)>; the kind determines the transport used by
  <scm|client-login-then>. Passwords are sent in clear inside the
  encrypted channel.

  On the server, credentials are stored in a <em|hidden> form
  <scm|(password <scm-arg|encoding> <scm-arg|salt> <scm-arg|hash>
  ...)>, produced by <scm|server-hide-credentials> and checked by
  <scm|server-password-correct?> in
  <source-link|server/server-authentication.scm|TeXmacs/progs/server/server-authentication.scm>. The supported encodings are
  listed by <scm|server-supported-password-encodings>:

  <\description>
    <item*|<verbatim|pbkdf2>>PBKDF2 with HMAC-SHA256 computed by
    <name|GnuTLS> (glue function <scm|hash-password-pbkdf2>).

    <item*|<verbatim|sha512>, <verbatim|sha256>>Crypt-style hashes computed
    by running <verbatim|openssl passwd -6> resp. <verbatim|-5> as an
    external process (not available on Windows).

    <item*|<verbatim|clear>>No hashing (used for backward compatibility).
  </description>

  The preference <verbatim|"server password encoding"> selects the encoding
  of new passwords (by default the first supported one, i.e.
  <verbatim|pbkdf2> when <name|GnuTLS> is present). When the preference
  <verbatim|"server password update"> is on, <scm|server-password-update>
  rehashes a password with the preferred encoding (and a fresh salt if the
  old one had the wrong length) at the next successful login.

  If <verbatim|"server require strong passwords"> is on (the default), new
  passwords must pass <scm|server-strong-password?>: at least 10
  characters, with a lower case letter, an upper case letter, a digit and a
  symbol.

  <subsection|Login>

  The service <scm|remote-login> (<source-link|server/server-base.scm|TeXmacs/progs/server/server-base.scm>) finds
  the user with <scm|server-find-user>, verifies the password with
  <scm|server-password-authentified?> and, on success, calls
  <scm|server-login-uid>, which records the login time and address, resets
  the failure counter and stores the association between the connection
  and the user in <scm|server-logged-table> (in both directions). Later
  services retrieve the user with <scm|(server-get-user envelope)>, which
  returns <scm|#f> for anonymous connections. The answers are strings:
  <verbatim|"ready">, <verbatim|"pending"> (account awaiting
  confirmation), <verbatim|"user not found">, or an error message.

  Brute force attacks are limited by <scm|server-can-login-uid?>: after
  <verbatim|"server failed login limit"> (default 3) failures, logins are
  refused until <verbatim|"server failed login delay"> seconds (default
  3600) have elapsed since the last failure. Accounts can also be suspended
  (<scm|server-suspend-user>) or marked as deleted.

  On the client, <scm|client-login-home> (<source-link|client-widgets.scm|TeXmacs/progs/client/client-widgets.scm>)
  performs the complete sequence: connect and log in
  (<scm|client-login-then>), register the connection with
  <scm|add-active-connection>, send the protocol version, fetch the
  account information, record the account in the client database
  <scm|(user-database "remote")> (<scm|client-notify-account>), open the
  remote home directory and pull pending notifications. Passwords may be
  stored in the <TeXmacs> wallet; the <verbatim|tmfs> load handler of
  remote files uses <scm|wallet-get> to log in automatically when a
  document on a server without active connection is opened.

  <subsection|Account creation, confirmation and reset>

  <\itemize>
    <item><scm|new-account> is only allowed if <verbatim|"server service
    new-account"> is on (default off), or when called by an administrator.
    If <verbatim|"server account confirmation delay"> is non negative, the
    account is first stored as a pending user and an email with a random
    six digit code is sent using the shell command in <verbatim|"server mail
    command"> (<scm|server-send-email>; the placeholders
    <verbatim|$USER_PSEUDO>, <verbatim|$USER_NAME>, <verbatim|$USER_EMAIL>,
    <verbatim|$USER_CODE> and <verbatim|$FILE_NAME> are substituted). The
    service <scm|confirm-pending-account> turns the pending user into a
    real one.

    <item><scm|remote-reset-credentials> (allowed if <verbatim|"server
    service reset-credentials"> is on) emails a one-time code, and
    <scm|remote-login-code> logs in with this code.

    <item><scm|remote-set-account> changes the name, email or credentials of
    the current user; administrators may pass another user.

    <item><scm|remote-delete-account> deletes an account after the checks in
    the macro <scm|with-verify-delete-rights>: non administrators may only
    delete themselves and only if <verbatim|"server service
    delete-account"> is on; the <verbatim|admin> account cannot be deleted.
    <scm|server-remove-user> marks the user as deleted, executes the
    <em|deletion plan> (<scm|server-execute-deletion-plan>: resources owned
    by the user which were not shared through chat messages are removed,
    shared ones are kept), removes the user from all access lists
    (<scm|server-scrub-participations>) and deletes its notifications. The
    service <scm|remote-deletion-plan> lets the client display this plan
    beforehand.
  </itemize>

  <subsection|Administration>

  Users with the admin flag may read and modify the server preferences
  remotely (<scm|remote-admin-preferences>, <scm|remote-set-preferences>,
  <scm|remote-admin-preferences-form>; see
  <source-link|client/client-remote-config.scm|TeXmacs/progs/client/client-remote-config.scm>), list the accounts
  (<scm|remote-get-accounts>) and evaluate arbitrary <scheme> expressions
  on the server with <scm|remote-eval>. The latter is equivalent to shell
  access to the server account and should be kept in mind when granting
  administrator rights. Each preference guarding a service is a string
  <verbatim|"on"> or <verbatim|"off"> declared with
  <scm|define-preferences>; see <hlink|Server
  preferences|../../main/remote/server/man-preferences.en.tm> for the user
  level description. Anonymous clients may retrieve the preferences listed
  in <scm|server-all-preferences> together with the license through
  <scm|remote-public-preferences>, unless disabled by the corresponding
  <verbatim|"server public: ..."> preference.

  <section|Logging>

  <scm|(server-log-write <scm-arg|level> <scm-arg|message>)> accepts the
  syslog levels <scm|emergency>, <scm|alert>, <scm|critical>, <scm|error>,
  <scm|warning>, <scm|notice>, <scm|info> and <scm|debug>. In headless mode
  it calls the glue function <scm|server-log-write-int>, which uses
  <verbatim|syslog> on <name|Unix> (<source-link|Plugins/Unix/unix_server_log.cpp|src/Plugins/Unix/unix_server_log.cpp>),
  <verbatim|os_log> on <name|macOS> (subsystem
  <verbatim|org.texmacs.logging>), and the corresponding modules for
  <name|Windows>; when standard output is a terminal the messages are
  printed instead. With a graphical interface, the messages go to the
  debug console channels <verbatim|server-error>,
  <verbatim|server-warning> and <verbatim|server-debug>.

  <section|Chat rooms, mail boxes and notifications>

  Chat rooms (<source-link|server/server-chat.scm|TeXmacs/progs/server/server-chat.scm>) are database entries of
  type <verbatim|"chat-room">; messages are entries of type
  <verbatim|"chat-message"> with fields <verbatim|"action">,
  <verbatim|"from">, <verbatim|"to"> (the room), <verbatim|"message"> and
  a time stamp. Documents sent to a room (action <verbatim|"send">) are
  stored in the repository as separate entries of type
  <verbatim|"message">, whereas the action <verbatim|"share"> stores the
  <abbr|URL> of the shared resource together with its identifier in
  <verbatim|"resource-id">, so that the link can still be resolved after
  renaming (<scm|resolve-resource-id>). Each user has a private room
  <verbatim|mail-<em|pseudo>>, which is the mail box.

  The server caches the messages of each room in
  <scm|chat-room-messages> and remembers which connections have the room
  open in <scm|chat-room-present>. When a message is posted
  (<scm|remote-send-message>, <scm|remote-send>), <scm|chat-room-notify>
  appends it to the cache and pushes it to all present clients with the
  call-back <scm|chat-room-receive>. Messages to a mail box also create a
  <em|notification>: a database entry of type <verbatim|"notification">
  owned by the recipient, pushed immediately with
  <scm|client-push-notifications> if the recipient is logged in, and
  otherwise retrieved at the next login with
  <scm|remote-pending-notifications>. Clients acknowledge notifications
  with <scm|remote-ack-notifications>, which deletes them on the server.
  See also <hlink|Messages and chat
  rooms|../../main/remote/client/man-chat.en.tm> and <hlink|Shared
  resources|../../main/remote/client/man-share.en.tm>.

  <section|The tree cache>

  Documents with many large images or graphics are expensive to transmit
  again and again. Since protocol version 1, the server replaces such
  subtrees by references into a content addressed cache.

  <\itemize>
    <item>The <c++> side (<verbatim|Data/Tree/tree_cache.*>) maintains, for
    each host name, a directory
    <verbatim|$TEXMACS_HOME_PATH/system/tmp/tree_cache/<em|host>> of
    serialized trees indexed by a 64-bit hash of the tree
    (<cpp|tree_hash>), with a least recently used eviction policy
    (<cpp|disk_lru>, by default at most 1024 entries in memory and 500 MB
    on disk). <cpp|tree_cache::update> traverses a tree and replaces each
    subtree for which <cpp|should_cache> holds (images with embedded raw
    data and graphics) by a <markup|cache-ref> tree containing the hash and
    the original label (for images also the width and height arguments of the
    image and its size in pixels, so that the client can lay out the
    document before the image arrives).

    <item>On the server, the host name is <scm|server-tree-cache-host>
    (<verbatim|"localhost">). <scm|remote-file-load> and
    <scm|remote-chat-room-open> send cached documents to clients for which
    <scm|server-can-handle-cache?> holds (preference <verbatim|"server
    service tree-cache"> on and client protocol version at least 1). The
    service <scm|remote-get-cache-ref> returns the tree for a hash. A
    janitor (<scm|tree-cache-janitor-all>) is run every twelve hours, as
    soon as the processor has been idle for 30 seconds (<scm|delayed> with
    the <scm|:on-cpu-idle> keyword).

    <item>The client (<scm|fetch-missing-cache-refs> in
    <source-link|client/client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm>) uses its own cache with the server
    name as host, requests the missing hashes one by one and substitutes
    the <markup|cache-ref> trees in the buffer as they arrive, without
    marking the buffer as modified.
  </itemize>

  <section|Backups>

  <source-link|server/server-backup.scm|TeXmacs/progs/server/server-backup.scm> implements optional periodic
  snapshots of <verbatim|$TEXMACS_HOME_PATH/server> with <verbatim|rsync>.
  When <verbatim|"server service backup"> is on and <verbatim|"server
  backup destination"> is set, <scm|server-backup-register> schedules
  <scm|server-backup-run> every <verbatim|"server backup interval"> hours
  (again using <scm|:on-cpu-idle>). Each run creates a directory named after
  the current date and time in the destination and calls <verbatim|rsync
  -a --partial --link-dest=<em|previous>>, so that unchanged files are hard
  links into the previous snapshot. <scm|server-backup-prune> then keeps
  the newest snapshot per hour, day, month and year up to the counts given
  by the <verbatim|"server backup keep ..."> preferences. The comments in
  the module explain why running <verbatim|rsync> on a live server is
  considered safe (the database is flushed in the idle cycle, file
  contents are write-once and <verbatim|users.scm> is atomically
  rewritten).

  <section|Security considerations>

  The following points should be kept in mind when deploying a server or
  modifying the code. Some of them are known limitations of the current
  implementation.

  <\itemize>
    <item><em|Transport.> Use <abbr|TLS>. In legacy mode the key exchange is
    done with an unauthenticated <abbr|RSA> key, so it offers no protection
    against an active attacker. With <abbr|TLS>, the server still accepts
    anonymous Diffie\UHellman by default (<verbatim|"tls-server
    authentication anonymous">), and the client does not check that the
    certificate matches the host name (see the comment near
    <cpp|gnutls_session_set_verify_cert> in <source-link|gnutls.cpp|src/Plugins/Gnutls/gnutls.cpp>); when
    the certificate cannot be verified, the user is asked whether to trust
    it (<scm|trust-certificate-interactive>). Disabling anonymous
    authentication on the server and distributing the server certificate to
    clients gives the strongest guarantees currently available.

    <item><em|Parsing.> Messages are parsed with the <scheme> reader and the
    packet length is not bounded, so a malicious peer can make the server
    allocate large amounts of memory. Only symbols registered with
    <scm|tm-service> can be invoked, but services receive arbitrary
    S-expressions as arguments and must validate them.

    <item><em|Access control.> Every service must check the user and the
    rights itself. Several services accept anonymous connections: database
    reads use the pseudo-user <verbatim|"all"> when nobody is logged in,
    <scm|remote-search-user> can search all user entries (a <verbatim|TODO>
    in the code), and <scm|live-exists?> does not require a login. Live
    documents and chat rooms are created readable and writable by
    <verbatim|"all">. The client side permission handler for remote files
    always answers <scm|#t> (a <verbatim|FIXME> in
    <source-link|client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm>); the server is the only line of defence.

    <item><em|Secrets.> Account confirmation and reset codes are six digit
    numbers produced by the <scheme> <scm|random> function. The helper
    <scm|password-correct-sha512?> prints the password being verified with
    <scm|display*>, which ends up on the standard output of the server. The
    administrator password generated at first start is written to the log.

    <item><em|Administrators.> <scm|remote-eval> evaluates arbitrary code
    and the <verbatim|"server mail command"> preference is executed by the
    shell, so an administrator account is equivalent to a shell account on
    the server machine.
  </itemize>

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
