<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Catalogue of <verbatim|tmfs> handlers>

  <section|Introduction>

  This page lists the handlers of the <TeXmacs> file system defined in the
  <scheme> sources of <TeXmacs>, with the syntax of the names they accept,
  the operations they implement and the module where they are defined
  (relative to <verbatim|src/TeXmacs/progs/>). In the syntax descriptions,
  <verbatim|<em|file>> stands for a file name encoded with
  <scm|url-\<gtr\>tmfs-string>, such as <verbatim|file/home/joe/paper.tm>
  or <verbatim|tm/doc/main/man-manual.en.tm> (see <hlink|the
  internals|tmfs-internals.en.tm>).

  The operations are abbreviated as follows: <verbatim|load>,
  <verbatim|save>, <verbatim|title>, <verbatim|permission>,
  <verbatim|master>, <verbatim|format>, <verbatim|wrap> and
  <verbatim|autosave>, after the macros <scm|tmfs-load-handler>, etc. When
  no permission handler is listed, the documents of the class are
  read-only.

  A handler is only available once its module has been loaded. For the
  classes registered in <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> with
  <scm|lazy-tmfs-handler> (<verbatim|automate>, <verbatim|grep>,
  <verbatim|help>, <verbatim|apidoc>, <verbatim|part>, <verbatim|db> and
  <verbatim|remote-file>) this happens automatically; the other modules are
  loaded at startup or as a side effect of using the corresponding
  features.

  <section|Kernel handlers>

  These handlers are defined in <source-link|kernel/texmacs/tm-file-system.scm|TeXmacs/progs/kernel/texmacs/tm-file-system.scm>
  and are always available.

  <\description>
    <item*|<verbatim|tmfs://id/<em|text>>>A trivial example: a
    <verbatim|generic> document whose body is <em|text>. Operations:
    <verbatim|load>.

    <item*|<verbatim|tmfs://aux/<em|name>>>Auxiliary buffers used by dialogs
    and side tools, whose contents and masters are stored in the tables
    <scm|aux-buffers> and <scm|aux-masters>. Operations: <verbatim|load>,
    <verbatim|title> (the name), <verbatim|master>. See <hlink|auxiliary
    buffers|tmfs-internals.en.tm> and the functions <scm|aux-name>,
    <scm|aux-set-document>, <scm|aux-set-master> and <scm|open-auxiliary>.

    <item*|<verbatim|tmfs://import/<em|format>/<em|file>>>The file converted
    from <em|format> to <TeXmacs> with <scm|tree-import>. Used by
    <source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> when a file is imported from a
    format other than its natural one. Operations: <verbatim|load>,
    <verbatim|title>.
  </description>

  <section|Documentation>

  <\description>
    <item*|<verbatim|tmfs://help/<em|type>/<em|file>>>Help pages
    (<source-link|doc/tmdoc.scm|TeXmacs/progs/doc/tmdoc.scm>, registered lazily). The <em|type> selects
    how the file is presented:

    <\itemize>
      <item><verbatim|normal>: the file is loaded as is;

      <item><verbatim|book>: the file and the files it refers to in its
      <markup|traverse> branches are expanded into a single book with the
      style given by the preference <verbatim|manual style> (by default
      <verbatim|tmmanual>);

      <item>any other type, such as <verbatim|article> (used by
      <scm|load-help-article>) or <verbatim|plain>: the file is expanded in
      the same way, but into a single document with the <verbatim|tmdoc>
      style.
    </itemize>

    Files with suffix <verbatim|html> and <verbatim|tmml> are converted; a
    missing file yields a <verbatim|Broken link.> page. Operations:
    <verbatim|load>, <verbatim|title> (<verbatim|Help - > followed by the
    title found in the document), <verbatim|permission> (read only). The
    help menus generate such <abbr|URL>s with <scm|tmdoc-expand-help>; for
    instance the <c++> startup code (<source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>)
    opens <verbatim|tmfs://help/plain/tm/doc/about/changes/changes-recent.en.tm>
    to show the recent changes.

    <item*|<verbatim|tmfs://apidoc/type=<em|kind>&what=<em|name>>>Automatically
    generated documentation of <scheme> symbols and modules
    (<source-link|doc/apidoc.scm|TeXmacs/progs/doc/apidoc.scm>, registered lazily). <em|kind> is
    <verbatim|symbol> or <verbatim|module>; an empty <em|name> lists all
    symbols or modules. Operations: <verbatim|load>, <verbatim|title>,
    <verbatim|permission> (always granted). See <scm|apidoc-all-symbols> and
    <scm|apidoc-all-modules>.

    <item*|<verbatim|tmfs://grep/type=<em|where>&what=<em|words>>>Search
    results (<source-link|doc/docgrep.scm|TeXmacs/progs/doc/docgrep.scm>, registered lazily). <em|where> is one
    of <verbatim|doc> (documentation in the current language),
    <verbatim|texts> (files in <verbatim|$TEXMACS_FILE_PATH>),
    <verbatim|recent> (recently opened files), <verbatim|Scheme>,
    <verbatim|Styles>, <verbatim|C++> or <verbatim|All code>; any other
    value searches the English documentation. The query is built with
    <scm|list-\<gtr\>query> by <scm|docgrep-in-doc>, <scm|docgrep-in-src>,
    <scm|docgrep-in-texts> and <scm|docgrep-in-recent>. Operations:
    <verbatim|load>, <verbatim|title>.

    <item*|<verbatim|tmfs://automate/<em|bindings>/<em|file>>>An
    automated document (<source-link|utils/automate/auto-tmfs.scm|TeXmacs/progs/utils/automate/auto-tmfs.scm>, registered
    lazily), built by <scm|build-document> from <em|file> with the variable
    bindings <verbatim|var1=val1,var2=val2>. The document is built in safe
    mode when <em|file> is itself a <verbatim|tmfs://help/...> page (see
    <scm|auto-load-help>). Operations: <verbatim|load>, <verbatim|title>.
  </description>

  <section|Version control>

  These handlers are defined in <source-link|version/version-tmfs.scm|TeXmacs/progs/version/version-tmfs.scm>, except
  for <verbatim|git> which is defined in <source-link|version/version-git.scm|TeXmacs/progs/version/version-git.scm>.
  They are not registered lazily; the modules are loaded by the versioning
  menus and commands.

  <\description>
    <item*|<verbatim|tmfs://history/<em|file>>>The list of revisions of
    <em|file>, with links to the individual revisions. Opened by
    <scm|version-show-history>, which also sets the master of the history
    buffer to the file. Operations: <verbatim|load>, <verbatim|title>.

    <item*|<verbatim|tmfs://revision/<em|rev>/<em|file>>>The contents of
    <em|file> at revision <em|rev>, as returned by <scm|version-revision>
    for the version control tool in use. The <abbr|URL> is built by
    <scm|version-revision-url>; when <em|rev> contains a colon, the colon is
    replaced by a slash and no file is appended. The functions
    <scm|version-revision?>, <scm|version-get-revision> and
    <scm|version-head> analyze such <abbr|URL>s. Operations:
    <verbatim|load>, <verbatim|title>, <verbatim|format> (the format of
    <em|file>).

    <item*|<verbatim|tmfs://commit/<em|rev>/<em|root>>>The description of a
    <name|Git> commit in the repository at <em|root>: message, parents and
    changed files. Built by <scm|tmfs-url-commit>. Operations:
    <verbatim|load>, <verbatim|format>.

    <item*|<verbatim|tmfs://git/<em|which>/<em|root>>>The output of
    <verbatim|git status> (<em|which> is <verbatim|status>) or
    <verbatim|git log> (<em|which> is <verbatim|log>) for the repository at
    <em|root>. Built by <scm|tmfs-url-git>. Operations: <verbatim|load>,
    <verbatim|title>.
  </description>

  <section|Documents and data>

  <\description>
    <item*|<verbatim|tmfs://part/<em|master>[/<em|file>]>>A part of a
    document split into several files (<source-link|part/part-tmfs.scm|TeXmacs/progs/part/part-tmfs.scm>,
    registered lazily). <em|master> is the main file and <em|file> an
    included file, encoded relatively to the master (<verbatim|here/...>) or
    absolutely; the master alone is shown if <em|file> is omitted. The
    functions <scm|part-master> and <scm|part-file> decompose the name and
    <scm|part-url> builds it. The included file is shown with the style,
    references and initial environment of the master, and saving writes the
    changes back into <em|file>. Operations: <verbatim|load>,
    <verbatim|save>, <verbatim|title>, <verbatim|master> and
    <verbatim|wrap> (both return <em|file>).

    <item*|<verbatim|tmfs://db/<em|var>=<em|val>/.../<em|kind>/<em|file>>>A
    view on a database (<source-link|database/db-tmfs.scm|TeXmacs/progs/database/db-tmfs.scm>, registered lazily),
    for instance <verbatim|tmfs://db/bib/global> for the global
    bibliographic database. Leading components containing an equal sign are
    parameters (<verbatim|search>, <verbatim|order>, <verbatim|direction>,
    <verbatim|limit>, <verbatim|present>). Saving stores the modified
    entries back into the database. Operations: <verbatim|load>,
    <verbatim|save>, <verbatim|title>, <verbatim|permission> (read and
    write). See also <hlink|the database <abbr|API>|../../database/scheme-database.en.tm>.

    <item*|<verbatim|tmfs://biblio/<em|bib>/<em|file>>>The entries of the
    local bibliography <em|bib> attached to the document <em|file>
    (<source-link|database/bib-local.scm|TeXmacs/progs/database/bib-local.scm>, loaded lazily through
    <scm|open-biblio>). Built by <scm|biblio-url>. Operations:
    <verbatim|load>, <verbatim|save>, <verbatim|title>,
    <verbatim|permission> (read and write).

    <item*|<verbatim|tmfs://comments/<em|file>>>The comments of the open
    buffer <em|file>, for the comments editor
    (<source-link|tools/comment/comment-widgets.scm|TeXmacs/progs/tools/comment/comment-widgets.scm>). Operations:
    <verbatim|load>, <verbatim|title>, <verbatim|permission> (read only).

    <item*|<verbatim|tmfs://artwork/<em|path>>>An image or pattern from the
    <TeXmacs> artwork collection (<source-link|utils/misc/artwork.scm|TeXmacs/progs/utils/misc/artwork.scm>, loaded
    at startup). The file is downloaded from
    <verbatim|https://www.texmacs.org/artwork> and cached in
    <verbatim|$TEXMACS_HOME_PATH/misc>; if the download fails, a thumbnail
    from <verbatim|$TEXMACS_PATH/misc> is used. Operations: <verbatim|load>,
    <verbatim|format> (from the suffix).

    <item*|<verbatim|tmfs://email/<em|id>>>Email messages read with the
    external program <verbatim|mmail> (<source-link|utils/email/email-tmfs.scm|TeXmacs/progs/utils/email/email-tmfs.scm>,
    loaded at startup only if <verbatim|mmail> is found in the path). The
    special names <verbatim|mailbox> and <verbatim|inbox> list the messages.
    Operations: <verbatim|load>, <verbatim|title>.
  </description>

  <section|Remote file systems and collaboration>

  The following handlers form the client side of the <TeXmacs>
  client/server infrastructure; see <hlink|collaborative
  editing|../../../source/collaboration.en.tm> for the protocol and the
  server. In these <abbr|URL>s, <em|server> is the name of the server,
  possibly followed by <verbatim|:><em|port>, as registered by the client
  when logging in.

  <\description>
    <item*|<verbatim|tmfs://remote-file/<em|server>/~<em|user>/<em|path>>>A
    file stored on a server (<source-link|client/client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm>, registered
    lazily). A component <verbatim|time=<em|t>> right after the server
    name designates an older version. Loading is asynchronous: the handler
    sends <scm|remote-file-load> to the server, returns an empty document,
    and fills the buffer when the answer arrives. If the client is not yet
    connected to the server, it first logs in, asking for credentials if
    they are not stored in the wallet. Operations:
    <verbatim|load>, <verbatim|save> (sends <scm|remote-file-save>),
    <verbatim|title> (the title is fetched asynchronously),
    <verbatim|permission> (always granted), <verbatim|autosave> (in the
    local backup directory).

    <item*|<verbatim|tmfs://remote-dir/<em|server>/~<em|user>/<em|path>>>A
    directory on a server, shown as a file browser
    (<source-link|client/client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm>; not registered lazily). The home
    directory of the current user is returned by
    <scm|remote-home-directory>. Operations: <verbatim|load>.

    <item*|<verbatim|tmfs://chat-rooms/<em|server>>,
    <verbatim|tmfs://shared/<em|server>>>The list of chat rooms and the list
    of resources shared with the user (<source-link|client/client-chat.scm|TeXmacs/progs/client/client-chat.scm>).
    Operations: <verbatim|load>, <verbatim|permission> (read only, and only
    if the client is connected to <em|server>).

    <item*|<verbatim|tmfs://chat/<em|server>/<em|room>>>A chat room
    (<source-link|client/client-chat.scm|TeXmacs/progs/client/client-chat.scm>). Operations: <verbatim|load>,
    <verbatim|title>, <verbatim|permission> (read only).

    <item*|<verbatim|tmfs://live-list/<em|server>>>The list of live
    documents on the server (<source-link|client/client-live.scm|TeXmacs/progs/client/client-live.scm>). Operations:
    <verbatim|load>, <verbatim|permission> (read only, when connected).

    <item*|<verbatim|tmfs://live/<em|server>/<em|name>>>A live document,
    edited simultaneously by several users (<source-link|client/client-live.scm|TeXmacs/progs/client/client-live.scm>).
    Operations: <verbatim|load>, <verbatim|title>, <verbatim|permission>
    (read and write).
  </description>

  The modules <source-link|client/client-chat.scm|TeXmacs/progs/client/client-chat.scm> and
  <source-link|client/client-live.scm|TeXmacs/progs/client/client-live.scm> are imported by
  <source-link|client/client-widgets.scm|TeXmacs/progs/client/client-widgets.scm>, so that their handlers are
  available as soon as the remote tools are used. On the server,
  <source-link|server/server-tmfs.scm|TeXmacs/progs/server/server-tmfs.scm> does not define <verbatim|tmfs>
  handlers, but the services (<scm|remote-file-load>,
  <scm|remote-file-save>, <scm|remote-dir-load>, ...) called by the client
  handlers; it analyzes the names with <scm|tmfs-\<gtr\>list> and the macro
  <scm|with-remote-context>, which also interprets the
  <verbatim|time=<em|t>> component.

  <section|Reserved identifiers>

  <\description>
    <item*|<verbatim|tmfs://view/<em|nr>/<em|buffer>>>Identifier of a view,
    built by the <c++> function <cpp|abstract_view>.

    <item*|<verbatim|tmfs://window/<em|nr>>>Identifier of a window, built
    by the <c++> function <cpp|create_window_id>.
  </description>

  These are not documents and have no handlers; see <hlink|the internals
  page|tmfs-internals.en.tm> and <hlink|the <TeXmacs>
  server|../../../source/server.en.tm>.

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
