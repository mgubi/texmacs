<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Getting started with <name|Git>>

  <paragraph*|Requirements>

  The <name|Git> program must be installed on your computer; <TeXmacs> uses
  it for all operations. If it is not found, you may indicate where it is in
  <menu|Version|Git preferences>. <name|Git> also needs to know your name and
  e-mail address, which are recorded in each commit. If you never configured
  them, then type once in a terminal

  <\verbatim-code>
    git config --global user.name "Your Name"

    git config --global user.email "you@example.org"
  </verbatim-code>

  For exchanging changes with a server, <name|Git> must be able to connect
  without asking for a password in a terminal: use an SSH key with an SSH
  agent, or a credential helper (see the documentation of your hosting
  service).

  <paragraph*|The <menu|Version> menu>

  By default, the <menu|Version> menu is shown for all documents which
  belong to a <name|Git> or <name|Subversion> working tree. You may show it
  for all documents, or never, using <menu|Tools|Versioning tool>.

  <paragraph*|Putting a document under version control>

  If the current document does not belong to a repository yet, then
  <menu|Version|Create Git repository> creates a new repository in the
  directory of the document. All files in this directory and its
  subdirectories may then be put under version control. A file
  <verbatim|.gitignore> is created as well, which tells <name|Git> to ignore
  the backup files of <TeXmacs>.

  If your coauthors already have a repository on a server, then use
  <menu|Version|Clone Git repository>. In the dialog, enter the address of
  the repository, as given by the hosting service, and the directory in
  which the copy should be made. The copy is made in the background, after
  which the status of the new working tree is shown. The repositories you
  worked with recently are listed in <menu|Version|Recent Git repositories>.

  <paragraph*|Using <name|Git> in an existing folder>

  The configuration of a repository may instruct <name|Git> to run
  arbitrary programs, even when <TeXmacs> only asks for the status of a
  file. For this reason, <TeXmacs> only runs <name|Git> in the repositories
  which you created or cloned with <TeXmacs>, and in those which you
  explicitly trust. When you open a document inside another repository,
  for instance one which you cloned in a terminal, the <menu|Version> menu
  only contains <menu|Version|Use Git in this folder>. Choose it if you know
  where the repository comes from; the complete menu then becomes
  available. Only do this for folders whose origin you trust, such as your
  own projects: do not do it for an archive you received by e-mail.

  <paragraph*|Choosing a mode>

  The first time you use the <name|Git> tools, <TeXmacs> asks you how you
  want to work:

  <\description>
    <item*|Simple>Save snapshots and synchronize with coauthors. The details
    of <name|Git> remain hidden. See <hlink|the simple
    mode|man-git-snapshots.en.tm>.

    <item*|Full>Staging, branches and remotes, for users who know
    <name|Git>.
  </description>

  You may change your choice at any time in <menu|Version|Git preferences>.

  <paragraph*|Overview of the interface>

  For a document in a repository, the <menu|Version> menu is organized as
  follows:

  <\itemize>
    <item>At the top, if the document has a conflict: <menu|Resolve
    conflict> and <menu|Mark as resolved> (see <hlink|resolving
    conflicts|man-git-conflicts.en.tm>).

    <item>The most frequent actions: <menu|Commit> (or <menu|Save
    snapshot> in the simple mode), <menu|Synchronize> and <menu|Git panel>.

    <item>The actions on the history of the current document:
    <menu|Compare with>, <menu|Restore version>, <menu|History of this
    document> and <menu|Who changed what>.

    <item>The submenus <menu|This file> (full mode only) and
    <menu|Project>, and a submenu for the whole repository, whose name
    summarizes its state, like <menu|Git (main, 3 changed, 1 ahead)>. In
    this manual, we will call it <menu|Version|Git>.

    <item>The submenu <menu|Differences>, for going through the differences
    between two versions, and <menu|Git preferences>.
  </itemize>

  The footer at the bottom of the window also shows the state of the
  repository, for instance

  <\verbatim-code>
    Git main \<#B7\> 2 changes \<#B7\> \<#2191\>1
  </verbatim-code>

  This means that you are on the branch <verbatim|main>, that two files were changed
  since the last commit, and that one commit is waiting to be sent to the
  server.

  <paragraph*|The <name|Git> panel>

  <menu|Version|Git panel> opens a panel on the right of the window, which
  shows the state of the repository of the current document and stays up
  to date while you work. At the top, you find the current branch, the
  number of commits to send and to get, and buttons for getting and sending
  changes (or for synchronizing, in the simple mode). The panel has three
  tabs:

  <\description>
    <item*|Changes>The changed files, grouped into conflicts, staged
    changes, other changes and new files, with buttons for staging,
    comparing and resolving them. Below, a box for a message, with
    <menu|Suggest>, which describes the changes, and <menu|Commit> (or
    <menu|Save snapshot>). Each repository keeps its own message.

    <item*|History>The recent versions of the current document, each of
    which can be compared with the current one or restored.

    <item*|Branches>The branches of the repository, with buttons for
    switching to another branch or creating a new one.
  </description>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
