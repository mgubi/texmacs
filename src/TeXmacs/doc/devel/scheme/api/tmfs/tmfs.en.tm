<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <TeXmacs> file system>

  The <TeXmacs> file system (<verbatim|tmfs>) is a virtual file system for
  documents which are generated on the fly by <scheme> code rather than
  stored on disk. <abbr|URL>s of the form <verbatim|tmfs://class/name> are
  used for help pages, search results, version histories, auxiliary buffers
  of dialogs, partial documents, databases, and the files, directories, chat
  rooms and live documents of the remote file system provided by a
  <TeXmacs> server. They are also used internally as identifiers for views
  and windows.

  Documents of the <TeXmacs> file system are produced by <em|handlers>,
  which are sets of <scheme> procedures for loading, saving, computing
  titles and permissions, etc. Since the <c++> file layer of <TeXmacs>
  delegates all operations on <verbatim|tmfs> <abbr|URL>s to these handlers,
  such documents can be opened, saved, linked to and included like ordinary
  files.

  <\traverse>
    <branch|A <verbatim|tmfs> primer|tmfs-basics.en.tm>

    <branch|Internals of the <TeXmacs> file system|tmfs-internals.en.tm>

    <branch|Catalogue of <verbatim|tmfs> handlers|tmfs-handlers.en.tm>

    <branch|The <verbatim|tmfs> <scheme> <abbr|API>|tmfs-api.en.tm>
  </traverse>

  See also <hlink|the <abbr|URL> system|../url.en.tm>, <hlink|the <TeXmacs>
  server|../../../source/server.en.tm> for buffers, views and windows, and
  <hlink|collaborative editing|../../../source/collaboration.en.tm> for the
  remote file system.

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
