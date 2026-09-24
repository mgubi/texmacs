<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Comparing and restoring versions>

  <paragraph*|Comparing the document with another version>

  The submenu <menu|Version|Compare with> compares the current document
  with:

  <\description>
    <item*|<menu|Last commit>>the version of the last commit, that is, it
    shows what you changed since then (also <key|version =>);

    <item*|<menu|Staged version>>the version which you staged;

    <item*|<menu|Remote version>>the version on the server, as known after
    the last exchange;

    <item*|<menu|Before the last pull or merge>>the version before the
    changes of your coauthors were merged, that is, it shows what they
    changed;

    <item*|<menu|Other revision>>any revision, given by the name of a
    branch or a tag, or the identifier of a commit;

    <item*|<menu|Tag ...>, <menu|Branch ...>>the version of a tag or of
    another branch;
  </description>

  and, below, with each of the recent versions of the document.

  The differences are shown inside the document itself, with the old
  version in red and the new version in green, as when <hlink|comparing two
  files|man-versioning.en.tm>. A bar at the bottom of the window shows the
  number of differences, with buttons for going to the previous or next
  one, for keeping the old or the new version of the current difference,
  and for choosing which versions are displayed. The bar can be closed and
  reopened with <menu|Version|Differences|Review bar>. The keyboard
  shortcuts of <menu|Version|Differences> can be used as well.

  When you are done, the document contains the versions which you kept.
  Save it to make the result permanent, or close it without saving to
  forget the comparison.

  <paragraph*|The history of the document>

  <menu|Version|History of this document> shows the list of the commits
  which changed the document, with their authors, dates and messages. The
  history follows the document when it is renamed. Each version can be
  opened as a read-only document. When viewing such an old version,
  <menu|Version|Compare with|Current user version> compares it with the
  current one, and <menu|Version|Restore this version> puts it back.

  <paragraph*|Restoring an earlier version>

  <menu|Version|Restore version> lists the recent versions of the current
  document. Choosing one of them replaces the contents of the file by that
  version. This is an ordinary change: the history is not modified, and
  you may compare the result with the last commit, or discard it. Recent
  versions can also be compared and restored from the <menu|History> tab
  of the <name|Git> panel.

  To put back the state of the whole project, use the snapshots of the
  <hlink|simple mode|man-git-snapshots.en.tm>.

  <paragraph*|Who changed what>

  <menu|Version|Who changed what> shows the document with, before each
  paragraph or group of paragraphs, the commit which last changed it: its
  author, its date and its message. Each author is shown in a color of their own,
  and a legend at the top tells how many paragraphs each author last
  changed. Paragraphs which were changed since the last commit are marked
  as not committed.

  Unlike the <verbatim|git blame> command, which works line by line on the
  source file, <TeXmacs> follows the paragraphs of the typeset document
  through its history, so that the result is not disturbed by changes in
  the layout of the source. Only the most recent commits are examined (30
  by default, see the <hlink|preferences|man-git-settings.en.tm>); older
  paragraphs are then marked accordingly. This command is not available
  while the document has a conflict.

  <paragraph*|The history of the repository>

  <menu|Version|Git|Log> lists all commits of the current branch, and
  <menu|Version|Git|Graph> shows the history of all branches as a graph,
  with the names of the branches and tags. Clicking on a commit opens its
  page, with its author, date, message and the list of files which it
  changed, together with the size of the changes. From this page, you can
  open a file as it was in that commit, or compare a <TeXmacs> document
  with its current version.

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
