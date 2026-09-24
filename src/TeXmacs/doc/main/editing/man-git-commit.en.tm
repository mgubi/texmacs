<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Recording changes>

  In <name|Git>, a <em|commit> records the state of some files, together
  with a message which describes the changes, your name and the date. This
  page describes the full mode; in the simple mode, you rather <hlink|save
  snapshots|man-git-snapshots.en.tm>, which are commits of all files.

  <paragraph*|The commit dialog>

  <menu|Version|Commit> (or <key|version c>) opens a dialog with:

  <\itemize>
    <item>a box for the <em|commit message>: a short summary on the first
    line, followed by more details if needed;

    <item>the list of the changed files, where you select those which should
    be part of the commit (the buttons <menu|All> and <menu|None> select all
    files or none);

    <item><menu|Suggest message>, which describes the selected changes: for
    each <TeXmacs> document, it lists the sections which contain changes,
    for instance \PUpdate paper.tm: Introduction, Proofs\Q;

    <item><menu|Amend last commit>, for correcting the last commit instead
    of making a new one, for instance when you forgot a file (with an empty
    message, the previous message is kept);

    <item>the buttons <menu|Cancel> and <menu|Commit>.
  </itemize>

  Initially, the files whose changes were <em|staged> are selected (see
  below). If nothing was staged, then all changed files which are already
  under version control are selected; new files have to be selected
  explicitly. When the corresponding preference is set (it is by default),
  the message is also suggested from the start. Documents with unsaved
  changes are saved before the commit, since <name|Git> only records what is
  on disk. If the commit cannot be done, for instance because the message
  is empty, then the dialog stays open and explains why.

  During a merge (see <hlink|resolving conflicts|man-git-conflicts.en.tm>),
  the dialog proposes the message prepared by <name|Git> and keeps all files
  selected, since a merge commit must contain all changes. It refuses to
  commit as long as some conflicts remain.

  A quicker way of committing is to type a message in the box of the
  <hlink|<name|Git> panel|man-git-start.en.tm> and to click on
  <menu|Commit>: the staged changes are committed or, if nothing was staged,
  the changes of all files under version control.

  <paragraph*|The current file>

  The submenu <menu|Version|This file> contains the operations which apply
  to the current document. Only the entries which make sense in its current
  state are shown:

  <\description>
    <item*|<menu|Add to repository>>Put a new file under version control.
    It will be part of the next commit.

    <item*|<menu|Stage changes>>Mark the current changes of the file as part
    of the next commit.

    <item*|<menu|Unstage changes>>Undo the previous operation. The file
    itself is not modified.

    <item*|<menu|Commit this file>>Save the document and commit it on its
    own, after asking for a message.

    <item*|<menu|Discard changes>>Put back the version of the file which was
    last staged or committed, after a confirmation. The changes since then
    are lost, unless you saved them elsewhere.
  </description>

  <paragraph*|Staging>

  Staging allows you to prepare a commit in several steps, and to commit
  some of your changes while keeping others for later. The <em|staged>
  version of a file is the version which will be recorded by the next
  commit. Besides <menu|Version|This file|Stage changes>, you can stage
  files from the <menu|Changes> tab of the <name|Git> panel, from the status
  page (<menu|Version|Git|Status>), or all at once with
  <menu|Version|Git|Stage all changes>. When a file is selected in the
  commit dialog without having been staged, all its changes are committed;
  when it was staged, it is committed exactly as it was staged.

  If you do not want to deal with staging, then just select the files in
  the commit dialog.

  <paragraph*|Projects with several files>

  A document is often made of several files: included documents, images,
  bibliographies, style files. For <TeXmacs> documents,
  <menu|Version|Project|Commit project> opens the commit dialog with all
  changed files of the project selected. If some of these files are not
  yet under version control, then <menu|Version|Project> also offers to add
  them; forgetting them is a common reason why a coauthor cannot typeset a
  document.

  <paragraph*|The status page>

  <menu|Version|Git|Status> (or <key|version s>) opens a page with the
  current branch, how many commits are waiting to be sent or received, and
  the lists of conflicts, staged changes, other changes and untracked
  files. Each file comes with buttons for comparing, staging, unstaging,
  discarding or adding it. The buttons at the top give access to the commit
  dialog, to the other pages (<menu|Log>, <menu|Graph>, <menu|Branches>,
  <menu|Output>) and to the exchanges with the server. The page is updated
  when you save a document; use <menu|Refresh> after changing files outside
  <TeXmacs>.

  <paragraph*|Signing commits>

  If you use <name|GnuPG>, then your commits and tags can be signed, using
  <menu|Version|Git|Preferences|Sign commits and tags> or the preferences
  dialog. The page of a commit shows whether its signature is valid.

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
