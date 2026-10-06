<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The remote file system>

  <section|Remote <abbr|URL>s>

  Remote documents are accessed through the <verbatim|tmfs> mechanism: a
  <abbr|URL> of the form <verbatim|tmfs://<em|class>/<em|name>> is loaded
  and saved by <scheme> handlers declared with <scm|tmfs-load-handler>,
  <scm|tmfs-save-handler>, <scm|tmfs-title-handler>,
  <scm|tmfs-permission-handler>, etc. The general mechanism is described in
  <hlink|the <TeXmacs> file system|../scheme/api/tmfs/tmfs.en.tm>; here we
  only describe the classes used for remote work. In all of them, the
  first component of <em|name> is the server name as known to the client,
  optionally followed by <verbatim|:<em|port>>.

  <\description>
    <item*|<verbatim|tmfs://remote-file/<em|server>/~<em|pseudo>/<em|path>>>A
    file stored on the server. The <verbatim|~<em|pseudo>> component is the
    home directory of the user <em|pseudo>; it is also used by
    <scm|find-server-for-name> to select the right connection when several
    accounts on the same server are logged in.

    <item*|<verbatim|tmfs://remote-dir/<em|server>/~<em|pseudo>/<em|path>>>A
    directory, displayed as a document in the style
    <verbatim|remote-file-browser>.

    <item*|<verbatim|tmfs://remote-file/<em|server>/time=<em|t>/~<em|pseudo>/<em|path>>>The
    file as it was at time <em|t> (seconds since the epoch). The same
    <verbatim|time=> component may be used for directories.

    <item*|<verbatim|tmfs://live/<em|server>/<em|name>>,
    <verbatim|tmfs://live-list/<em|server>>>A live document and the list of
    live documents of the user (<source-link|client/client-live.scm|TeXmacs/progs/client/client-live.scm>).

    <item*|<verbatim|tmfs://chat/<em|server>/<em|room>>,
    <verbatim|tmfs://chat-rooms/<em|server>>>A chat room (or the mail box
    when the room name starts with <verbatim|mail->) and the list of chat
    rooms (<source-link|client/client-chat.scm|TeXmacs/progs/client/client-chat.scm>).

    <item*|<verbatim|tmfs://shared/<em|server>>>The list of resources shared
    with the user.
  </description>

  The <verbatim|tmfs> handlers of the client are registered lazily: the
  boot file <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> contains
  <scm|(lazy-tmfs-handler (client client-tmfs) remote-file)>, so that the
  client modules are loaded when a remote <abbr|URL> is first opened. See
  <hlink|Connecting to a <TeXmacs>
  server|../../main/remote/client/man-connect.en.tm> and <hlink|Permissions
  and access control|../../main/remote/client/man-permissions.en.tm> for the
  user interface.

  <section|Server side organization>

  <subsection|Resources>

  The remote file system is a \Pdatabase file system\Q. Each file or
  directory is a <em|resource>, i.e. an entry of the server database whose
  identifier is called a <em|resource identifier> (<scm|rid>). The fields
  used by <source-link|server/server-tmfs.scm|TeXmacs/progs/server/server-tmfs.scm> are:

  <\description>
    <item*|<verbatim|type>><verbatim|"file">, <verbatim|"dir">,
    <verbatim|"version-list">, <verbatim|"live">, <verbatim|"chat-room">,
    ...

    <item*|<verbatim|name>, <verbatim|dir>>The last component of the name
    and the identifier of the parent directory. Home directories are
    directory entries named <verbatim|~<em|pseudo>>, without
    <verbatim|dir> field, created together with the user by
    <scm|server-set-user-info>.

    <item*|<verbatim|owner>, <verbatim|readable>, <verbatim|writable>>Access
    rights, checked with <scm|db-allow?>. A new file or directory inherits
    all non reserved properties of its parent (<scm|copy-properties> with
    <scm|inherit-property?>), in particular its access rights.

    <item*|<verbatim|location>>The path of the contents relative to the
    repository <verbatim|$TEXMACS_HOME_PATH/server>.

    <item*|<verbatim|version-list>, <verbatim|version-nr>,
    <verbatim|version-by>, <verbatim|version-msg>>Version information, see
    below.
  </description>

  Names are resolved component by component: <scm|file-name-\<gtr\>resource>
  maps a name such as <verbatim|localhost/~joe/notes/a.tm> to its resource
  by searching a directory named <verbatim|~joe>, then an entry
  named <verbatim|notes> in it, etc. (<scm|search-file>);
  <scm|resource-\<gtr\>file-name> performs the converse operation. Renaming or
  moving a resource is thus just a change of its <verbatim|name> or
  <verbatim|dir> field (see <scm|remote-rename> on the client, which
  uses <scm|remote-set-field>).

  <subsection|The repository>

  The contents of files are stored as ordinary files below
  <verbatim|$TEXMACS_HOME_PATH/server>, one file per version.
  <scm|repository-add> chooses a fresh location for a new resource: it
  descends into randomly numbered subdirectories <verbatim|0>...<verbatim|9>
  until it finds a directory without a subdirectory <verbatim|_>, creates
  that subdirectory and puts the file <verbatim|<em|rid>.<em|suffix>> in it.
  The relative location is recorded in the <verbatim|location> field, and
  <scm|repository-get> returns the absolute path. Files are never
  overwritten: a new version is a new resource with a new location. This
  is why incremental backups with hard links are efficient.

  <subsection|Services>

  All file system services take a name without the
  <verbatim|tmfs://remote-file/> prefix and start with the macro
  <scm|with-remote-context>, which splits off the server name and an
  optional <verbatim|time=<em|t>> component, and binds <scm|db-time>
  accordingly; modifications are refused with <verbatim|Error: cannot
  modify past> when a time is given. The actual work is done by functions
  <scm|server-file-create>, <scm|server-file-load>,
  <scm|server-file-save>, <scm|server-file-remove>,
  <scm|server-dir-create>, <scm|server-dir-load> and
  <scm|server-dir-remove>, which return <scm|(:error <scm-arg|message>)> or
  a success tag such as <scm|(:created <scm-arg|rid>)> or <scm|(:loaded
  <scm-arg|doc>)>, and can therefore be reused by other services (the
  synchronization services do so).

  <\explain>
    <scm|(remote-file-load <scm-arg|name>)><explain-synopsis|service: load a
    file>
  <|explain>
    Checks that the user is logged in and has read access, and returns the
    file contents as a string in <TeXmacs> format, after replacing large
    subtrees by <markup|cache-ref> trees if the client supports the tree
    cache.
  </explain>

  <\explain>
    <scm|(remote-file-create <scm-arg|name> <scm-arg|doc>
    <scm-arg|msg>)>, <scm|(remote-file-save <scm-arg|name> <scm-arg|doc>
    <scm-arg|msg>)><explain-synopsis|service: create or save a file>
  <|explain>
    Create a file (write access to the directory is required) or save a new
    version of it (write access to the file is required). <scm-arg|msg> is
    an optional commit message (or <scm|#f>). Saving a document identical
    to the stored one does nothing.
  </explain>

  <\explain>
    <scm|(remote-dir-load <scm-arg|name>)><explain-synopsis|service: list a
    directory>
  <|explain>
    Returns a list of entries <scm|(<scm-arg|short-name>
    <scm-arg|full-name> <scm-arg|dir?> <scm-arg|props>)> for the readable
    children, where <scm-arg|props> is the database entry with user
    identifiers replaced by pseudos.
  </explain>

  The other services are <scm|remote-dir-create>, <scm|remote-file-remove>,
  <scm|remote-dir-remove> (recursive), <scm|remote-identifier> (map a name
  to its resource identifier, or to <scm|(<scm-arg|rid> <scm-arg|time>)>
  for past versions, or <scm|#f>) and <scm|remote-get-versions>. The
  generic database services of <source-link|server/server-db.scm|TeXmacs/progs/server/server-db.scm>
  (<scm|remote-get-field>, <scm|remote-set-field>, <scm|remote-get-entry>,
  <scm|remote-search>, ...) give access to the properties of resources;
  they accept an identifier of the form returned by
  <scm|remote-identifier>, so that properties of past versions can be
  read (but not written: <verbatim|Error: cannot rewrite history>). The
  permission editor of the client (<scm|open-permissions-editor>) and the
  title handler of remote files are built on these services.

  <subsection|Versions on the server>

  Every remote file has a version list, an entry of type
  <verbatim|"version-list"> whose field <verbatim|version-current> is a
  counter, and every version of the file is a separate file resource with
  fields <verbatim|version-list> (the list), <verbatim|version-nr>,
  <verbatim|version-by> (the user who saved it) and optionally
  <verbatim|version-msg>. <scm|server-file-save>:

  <\enumerate>
    <item>refuses the save if the version number of the current resource
    differs from the counter (<verbatim|Error: version number mismatch>);

    <item>does nothing if the contents did not change;

    <item>otherwise increments the counter, creates a new resource for the
    new contents with the same name, directory and inherited properties, and
    removes the old resource.
  </enumerate>

  Since the database keeps history, removed resources remain visible at
  earlier times, which is how <verbatim|time=> <abbr|URL>s work. The
  service <scm|remote-get-versions> returns, for each version readable by
  the user, <scm|(<scm-arg|rid> <scm-arg|date> <scm-arg|name>
  <scm-arg|by> <scm-arg|msg>)>, and the client builds the page
  <verbatim|tmfs://history/...> from it (<scm|compute-remote-versions>).
  The generic versioning menu works for remote files because
  <source-link|client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm> overloads <scm|versioned?>,
  <scm|version-status>, <scm|version-history>, <scm|commit-buffer-message>,
  <scm|version-revision?>, <scm|version-head>, etc. with <scm|:require
  (remote-file? ...)>; committing with a message simply saves the buffer
  with <scm|remote-commit-message> bound to the message. See also
  <hlink|Versioning and document comparison|collab-versioning.en.tm>.

  <\remark>
    The client does not send the version on which its modifications are
    based, and the version number of the current resource always equals the
    counter in normal operation. Concurrent saves of the same remote file
    from two clients are therefore resolved by \Plast writer wins\Q; the
    earlier version remains available in the history.
  </remark>

  <section|Client side>

  <source-link|client/client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm> implements:

  <\itemize>
    <item>The load handler of <verbatim|remote-file>: if there is no active
    connection to the server, it looks for an account in
    <scm|(client-accounts)> and a stored credential in the wallet and logs
    in (or opens the login dialog). Otherwise it sends
    <scm|remote-file-load>, returns an empty document immediately and fills
    the buffer when the answer arrives (<scm|remote-file-set>, which uses
    <scm|buffer-set> and <scm|buffer-pretend-saved>). Loading is thus
    asynchronous.

    <item>The save handler, which converts the buffer to a string and sends
    <scm|remote-file-save>; an auto-save handler which redirects
    auto-saves to a local backup file (<scm|url-backup>).

    <item>The load handler of <verbatim|remote-dir>, which builds the file
    browser document from the entries (<scm|dir-page>) and caches the
    entries so that the display can be re-sorted without contacting the
    server (<scm|cache-dir-entries>).

    <item>Interactive commands such as <scm|remote-create-file-interactive>,
    <scm|remote-create-dir-interactive>, <scm|remote-rename> and
    <scm|remote-remove>, and the retrieval of document titles from the
    <verbatim|title> property.
  </itemize>

  Most of these functions follow the same pattern: find the connection with
  <scm|find-server-for-name>, send a request with
  <scm|client-remote-eval>, and update a buffer in the continuation.

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
