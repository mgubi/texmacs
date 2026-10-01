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
  <verbatim|progs/server/> have been loaded. A client is any <TeXmacs>
  process in which the modules under <verbatim|progs/client/> are loaded;
  they are loaded lazily, the first time the user opens a remote menu or a
  remote <verbatim|tmfs> <abbr|URL> (see the <scm|lazy-define>,
  <scm|lazy-menu> and <scm|lazy-tmfs-handler> declarations in
  <verbatim|init-texmacs.scm>). One process may simultaneously be a server
  and a client of other servers (or of itself).

  The work is split between <c++> and <scheme> as follows:

  <\itemize>
    <item><c++> (directory <verbatim|src/src/System/Link/>,
    <verbatim|src/src/Plugins/Qt/QTMSockets.*> and
    <verbatim|src/src/Plugins/Gnutls/>) implements the sockets, the optional
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
    <verbatim|src/src/Plugins/Database/> and wrapped in
    <verbatim|progs/database/>) together with a few <scheme> files and a
    directory tree which stores the contents of files. Clients keep their
    own state (accounts, synchronization records) in per-user databases.

    <item>Live editing relies on the <c++> patch algebra of
    <verbatim|src/src/Data/History/> (modifications, patches, inversion and
    commutation), which is also the basis of the undo/redo system.
  </itemize>

  <subsection|Map of the source files>

  <\description>
    <item*|Transport (<c++>)><verbatim|System/Link/client_server.hpp>,
    <verbatim|texmacs_server.cpp>, <verbatim|texmacs_client.cpp>,
    <verbatim|tm_link.cpp> (packet framing and legacy encryption),
    <verbatim|tm_contact.hpp>, <verbatim|socket_contact.*>,
    <verbatim|Plugins/Qt/QTMSockets.*> (non blocking sockets driven by
    <cpp|QSocketNotifier>), <verbatim|Plugins/Gnutls/gnutls.*> (<abbr|TLS>
    contacts, certificates, <name|PBKDF2>). The <scheme> glue is declared in
    <verbatim|Scheme/Glue/build-glue-basic.scm>.

    <item*|Server (<scheme>)><verbatim|server/server-base.scm> (dispatcher,
    accounts, login), <verbatim|server-authentication.scm> (preferences,
    logs, password encodings), <verbatim|server-tmfs.scm> (remote file
    system and server side versions), <verbatim|server-db.scm> (remote
    database access), <verbatim|server-sync.scm> and
    <verbatim|server-db-sync.scm> (synchronization),
    <verbatim|server-live.scm>, <verbatim|server-chat.scm>,
    <verbatim|server-notifications.scm>, <verbatim|server-cache.scm> (tree
    cache), <verbatim|server-backup.scm>, <verbatim|server-widgets.scm> and
    <verbatim|server-menu.scm> (user interface), plus the regression tests
    <verbatim|server-*-test.scm> and <verbatim|server-fixtures.scm>. The file
    <verbatim|server/todo.tm> contains the original design notes.

    <item*|Client (<scheme>)><verbatim|client/client-base.scm> (dispatcher,
    connections, accounts, login), <verbatim|client-authentication.scm>,
    <verbatim|client-tmfs.scm> (remote file browser and <verbatim|tmfs>
    handlers), <verbatim|client-db.scm>, <verbatim|client-sync.scm>,
    <verbatim|client-db-sync.scm>, <verbatim|client-live.scm>,
    <verbatim|client-chat.scm>, <verbatim|client-notifications.scm>,
    <verbatim|client-remote-config.scm> (remote administration of server
    preferences), <verbatim|client-widgets.scm>, <verbatim|client-menu.scm>
    and <verbatim|client-markup.scm>.

    <item*|Live documents><verbatim|utils/relate/live-document.scm> (states
    and histories), <verbatim|live-connection.scm> (remote peers and
    conversion between patches and lists of modifications),
    <verbatim|live-view.scm> (views of a live document inside ordinary
    buffers), and the markup in <verbatim|packages/utilities/live.ts> and
    <verbatim|packages/miscellaneous/live-document.ts>.

    <item*|Versioning><verbatim|version/version-tmfs.scm> (dispatch to
    back-ends, <verbatim|tmfs> handlers for histories and revisions),
    <verbatim|version-svn.scm>, <verbatim|version-git.scm>,
    <verbatim|version-compare.scm> (structural diff),
    <verbatim|version-edit.scm> and <verbatim|version-drd.scm> (editing of
    differences), <verbatim|version-menu.scm> and
    <verbatim|version-kbd.scm>. The markup for differences is defined in
    <verbatim|packages/standard/std-fold.ts>.

    <item*|Patches and undo (<c++>)><verbatim|Kernel/Types/modification.hpp>,
    <verbatim|Data/History/patch.*>, <verbatim|commute.cpp>,
    <verbatim|archiver.*>, and <verbatim|kernel/library/patch.scm> on the
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
    <verbatim|progs/check/check-master.scm>).

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
