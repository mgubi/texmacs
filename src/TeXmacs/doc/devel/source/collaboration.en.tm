<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Collaboration, the <TeXmacs> server and versioning>

  <section|Introduction>

  <TeXmacs> contains a small client/server system which allows several users
  to store documents on a shared server, to exchange messages, to share
  resources and to edit \Plive\Q documents together. The same <TeXmacs>
  binary plays both roles: an instance started in server mode listens on a
  <abbr|TCP> port and answers requests, while ordinary instances connect to
  one or more servers as clients. Independently of the server, <TeXmacs> also
  provides tools for comparing two versions of a document and a thin
  integration with the external version control systems <name|Subversion>
  and <name|Git>.

  This chapter describes how these facilities are implemented. It is aimed at
  developers who want to understand, debug or extend them. For the user
  point of view, see the user manual chapters <hlink|Setting up a <TeXmacs>
  server|../../main/remote/man-server.en.tm> and <hlink|Remote tools and
  collaborative editing|../../main/remote/man-collaborative.en.tm>.

  <\traverse>
    <branch|Transport and message protocol|collab-protocol.en.tm>

    <branch|The <TeXmacs> server|collab-server.en.tm>

    <branch|The remote file system|collab-remote-fs.en.tm>

    <branch|Synchronization of files and databases|collab-sync.en.tm>

    <branch|Live documents and shared editing|collab-live.en.tm>

    <branch|Versioning and document comparison|collab-versioning.en.tm>
  </traverse>

  <section|Architecture overview>

  <subsection|Who runs what>

  A <TeXmacs> server is a normal <TeXmacs> process (usually started with the
  <verbatim|-server> command line option, possibly together with
  <verbatim|-headless>) in which the <scheme> modules under
  <source-link|progs/server/|TeXmacs/progs/server> have been loaded. A client is any <TeXmacs>
  process in which the modules under <source-link|progs/client/|TeXmacs/progs/client> are loaded;
  they are loaded lazily, the first time the user opens a remote menu or a
  remote <verbatim|tmfs> <abbr|URL> (see the <scm|lazy-define>,
  <scm|lazy-menu> and <scm|lazy-tmfs-handler> declarations in
  <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>). One process may simultaneously be a server
  and a client of other servers (or of itself).

  The work is split between <c++> and <scheme> as follows:

  <\itemize>
    <item><c++> (directory <source-link|src/src/System/Link/|src/System/Link>,
    <verbatim|src/src/Plugins/Qt/QTMSockets.*> and
    <source-link|src/src/Plugins/Gnutls/|src/Plugins/Gnutls>) implements the sockets, the optional
    <abbr|TLS> layer, the legacy encryption layer and the framing of
    messages into packets. Only raw strings cross this layer. The socket code
    is only compiled in the <name|Qt> build (<verbatim|QTTEXMACS>); other
    builds contain stubs which report that sockets are not implemented.

    <item><scheme> implements everything else: serialization of messages as
    S-expressions, dispatching of requests to services, continuations for
    asynchronous answers, user accounts, access rights, the remote file
    system, synchronization, chat rooms, notifications and live documents.

    <item>The server keeps its persistent state in a <TeXmacs> database
    (<verbatim|$TEXMACS_HOME_PATH/server/global.tmdb>, implemented in
    <source-link|src/src/Plugins/Database/|src/Plugins/Database> and wrapped in
    <source-link|progs/database/|TeXmacs/progs/database>) together with a few <scheme> files and a
    directory tree which stores the contents of files. Clients keep their
    own state (accounts, synchronization records) in per-user databases.

    <item>Live editing relies on the <c++> patch algebra of
    <source-link|src/src/Data/History/|src/Data/History> (modifications, patches, inversion and
    commutation), which is also the basis of the undo/redo system.
  </itemize>

  <subsection|Map of the source files>

  <\description>
    <item*|Transport (<c++>)><source-link|System/Link/client_server.hpp|src/System/Link/client_server.hpp>,
    <source-link|texmacs_server.cpp|src/System/Link/texmacs_server.cpp>, <source-link|texmacs_client.cpp|src/System/Link/texmacs_client.cpp>,
    <source-link|tm_link.cpp|src/System/Link/tm_link.cpp> (packet framing and legacy encryption),
    <source-link|tm_contact.hpp|src/System/Link/tm_contact.hpp>, <verbatim|socket_contact.*>,
    <verbatim|Plugins/Qt/QTMSockets.*> (non blocking sockets driven by
    <cpp|QSocketNotifier>), <verbatim|Plugins/Gnutls/gnutls.*> (<abbr|TLS>
    contacts, certificates, <name|PBKDF2>). The <scheme> glue is declared in
    <source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>.

    <item*|Server (<scheme>)><source-link|server/server-base.scm|TeXmacs/progs/server/server-base.scm> (dispatcher,
    accounts, login), <source-link|server-authentication.scm|TeXmacs/progs/server/server-authentication.scm> (preferences,
    logs, password encodings), <source-link|server-tmfs.scm|TeXmacs/progs/server/server-tmfs.scm> (remote file
    system and server side versions), <source-link|server-db.scm|TeXmacs/progs/server/server-db.scm> (remote
    database access), <source-link|server-sync.scm|TeXmacs/progs/server/server-sync.scm> and
    <source-link|server-db-sync.scm|TeXmacs/progs/server/server-db-sync.scm> (synchronization),
    <source-link|server-live.scm|TeXmacs/progs/server/server-live.scm>, <source-link|server-chat.scm|TeXmacs/progs/server/server-chat.scm>,
    <source-link|server-notifications.scm|TeXmacs/progs/server/server-notifications.scm>, <source-link|server-cache.scm|TeXmacs/progs/server/server-cache.scm> (tree
    cache), <source-link|server-backup.scm|TeXmacs/progs/server/server-backup.scm>, <source-link|server-widgets.scm|TeXmacs/progs/server/server-widgets.scm> and
    <source-link|server-menu.scm|TeXmacs/progs/server/server-menu.scm> (user interface), plus the regression tests
    <verbatim|server-*-test.scm> and <source-link|server-fixtures.scm|TeXmacs/progs/server/server-fixtures.scm>. The file
    <source-link|server/todo.tm|TeXmacs/progs/server/todo.tm> contains the original design notes.

    <item*|Client (<scheme>)><source-link|client/client-base.scm|TeXmacs/progs/client/client-base.scm> (dispatcher,
    connections, accounts, login), <source-link|client-authentication.scm|TeXmacs/progs/client/client-authentication.scm>,
    <source-link|client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm> (remote file browser and <verbatim|tmfs>
    handlers), <source-link|client-db.scm|TeXmacs/progs/client/client-db.scm>, <source-link|client-sync.scm|TeXmacs/progs/client/client-sync.scm>,
    <source-link|client-db-sync.scm|TeXmacs/progs/client/client-db-sync.scm>, <source-link|client-live.scm|TeXmacs/progs/client/client-live.scm>,
    <source-link|client-chat.scm|TeXmacs/progs/client/client-chat.scm>, <source-link|client-notifications.scm|TeXmacs/progs/client/client-notifications.scm>,
    <source-link|client-remote-config.scm|TeXmacs/progs/client/client-remote-config.scm> (remote administration of server
    preferences), <source-link|client-widgets.scm|TeXmacs/progs/client/client-widgets.scm>, <source-link|client-menu.scm|TeXmacs/progs/client/client-menu.scm>
    and <source-link|client-markup.scm|TeXmacs/progs/client/client-markup.scm>.

    <item*|Live documents><source-link|utils/relate/live-document.scm|TeXmacs/progs/utils/relate/live-document.scm> (states
    and histories), <source-link|live-connection.scm|TeXmacs/progs/utils/relate/live-connection.scm> (remote peers and
    conversion between patches and lists of modifications),
    <source-link|live-view.scm|TeXmacs/progs/utils/relate/live-view.scm> (views of a live document inside ordinary
    buffers), and the markup in <source-link|packages/utilities/live.ts|TeXmacs/packages/utilities/live.ts> and
    <source-link|packages/miscellaneous/live-document.ts|TeXmacs/packages/miscellaneous/live-document.ts>.

    <item*|Versioning><source-link|version/version-tmfs.scm|TeXmacs/progs/version/version-tmfs.scm> (dispatch to
    back-ends, <verbatim|tmfs> handlers for histories and revisions),
    <source-link|version-svn.scm|TeXmacs/progs/version/version-svn.scm>, <source-link|version-git.scm|TeXmacs/progs/version/version-git.scm>,
    <source-link|version-compare.scm|TeXmacs/progs/version/version-compare.scm> (structural diff),
    <source-link|version-edit.scm|TeXmacs/progs/version/version-edit.scm> and <source-link|version-drd.scm|TeXmacs/progs/version/version-drd.scm> (editing of
    differences), <source-link|version-menu.scm|TeXmacs/progs/version/version-menu.scm> and
    <source-link|version-kbd.scm|TeXmacs/progs/version/version-kbd.scm>. The markup for differences is defined in
    <source-link|packages/standard/std-fold.ts|TeXmacs/packages/standard/std-fold.ts>.

    <item*|Patches and undo (<c++>)><source-link|Kernel/Types/modification.hpp|src/Kernel/Types/modification.hpp>,
    <verbatim|Data/History/patch.*>, <source-link|commute.cpp|src/Data/History/commute.cpp>,
    <verbatim|archiver.*>, and <source-link|kernel/library/patch.scm|TeXmacs/progs/kernel/library/patch.scm> on the
    <scheme> side.
  </description>

  <subsection|Status of the various parts>

  The code base has been developed over a long period (initial design in
  2007 and 2013, <abbr|TLS> and authentication in 2022, accounts,
  notifications, tree cache and backups in 2025\U2026), and its parts have
  different levels of maturity:

  <\itemize>
    <item><em|Stable and used>: the connection layer (legacy and
    <abbr|TLS>), accounts and password login, the remote file system with
    server side versions, sharing through chat messages and permissions,
    chat rooms and mail boxes, the structural comparison of documents and
    the <name|Subversion>/<name|Git> integration.

    <item><em|Recent>: push/pull notifications, account deletion plans, the
    tree cache (protocol version 1) and periodic backups. These parts come
    with regression tests (<scm|regtest-server-notifications>,
    <scm|regtest-server-backup>, <scm|regtest-server-cache>, run from
    <source-link|progs/check/check-master.scm|TeXmacs/progs/check/check-master.scm>).

    <item><em|Experimental>: live documents (the code contains several
    <verbatim|FIXME> notes, prints debugging output on conflicts and only
    persists live documents when a client disconnects), file and database
    synchronization (only the database kind <verbatim|"bib"> is currently
    synchronized), and remote evaluation of commands.
  </itemize>

  The individual sections indicate more precisely what is implemented.

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
