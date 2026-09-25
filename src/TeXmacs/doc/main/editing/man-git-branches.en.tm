<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Branches, tags and stashes>

  The commands of this page are those of the full mode, in the submenu
  <menu|Version|Git>.

  <paragraph*|Branches>

  A <em|branch> is a line of development. Branches allow you to try out a
  change, such as a new structure for a chapter or the answer to a referee,
  without disturbing the main version, and to merge it later if you are
  satisfied. The current branch is shown in the footer, in the panel and in
  the name of the submenu <menu|Version|Git>.

  <\description>
    <item*|<menu|New branch>>Create a branch starting from the current
    version, and switch to it unless you uncheck the corresponding box.

    <item*|<menu|Switch to branch>>Put the files in the state of another
    branch. Your uncommitted changes are kept, unless they concern files
    which differ between both branches: <name|Git> then refuses, and you
    should first commit or stash your changes.

    <item*|<menu|Merge branch>>Merge another branch into the current one.
    Conflicts in <TeXmacs> documents are handled as <hlink|when working with
    coauthors|man-git-remote.en.tm>: if the merge stops, the dialog which
    explains it offers to show the status, from which the conflicts can be
    resolved.
  </description>

  <TeXmacs> offers to save your modified documents before switching or
  merging. The page <menu|Version|Git|Branches and tags> lists the local
  branches and the branches of the remote repositories, with the date of
  their last commit and buttons for switching to them, merging them or
  deleting them. You can also switch to another branch from the
  <menu|Branches> tab of the <name|Git> panel, and compare the current
  document with its version in another branch using
  <menu|Version|Compare with>.

  <paragraph*|Tags>

  A <em|tag> gives a name to a version, for instance
  <verbatim|submitted-v1> for the version which you sent to a journal.
  <menu|Version|Git|Tag this version> asks for the name of the tag and an
  optional message. The tags are listed on the page
  <menu|Version|Git|Branches and tags> and in the graph of the history, and
  the current document can be compared with its tagged versions using
  <menu|Version|Compare with>: this is a convenient way to produce the list
  of changes requested by a journal.

  <paragraph*|Stashes>

  A <em|stash> puts your uncommitted changes aside, and brings the files
  back to the state of the last commit; for instance, when you have to
  correct something urgently in a clean version. New files, which are not
  yet under version control, are not put aside. <menu|Version|Git|Stash
  changes> creates a stash, and <menu|Version|Git|Restore last stash> puts
  the changes back. The stashes are listed on the page
  <menu|Version|Git|Branches and tags>, where they can also be restored or
  dropped.

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
