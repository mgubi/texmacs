<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The simple mode: snapshots>

  In the simple mode, you do not need to know anything about <name|Git>.
  You work on your files as usual and, from time to time, you save a
  <em|snapshot> of the whole project: the state of all its files at that
  moment, together with a short description. You can always come back to
  any of these snapshots, and you exchange them with your coauthors by
  synchronizing.

  <paragraph*|Saving a snapshot>

  Use <menu|Version|Save snapshot>, or <key|version c>, and describe in a few
  words what you did, for instance \PFirst draft of the introduction\Q. If
  some of your documents have unsaved changes, then <TeXmacs> first offers
  to save them. The snapshot contains all files of the project directory,
  including the files which you added since the last snapshot, but not the
  files ignored by <name|Git>. A snapshot can also be saved from the
  <hlink|<name|Git> panel|man-git-start.en.tm>, by typing its description
  in the box of the <menu|Changes> tab.

  <paragraph*|Going back to an earlier state>

  <menu|Version|Git|Restore snapshot> lists your recent snapshots. When you
  choose one of them, all files are put back as they were at that time.
  Nothing is lost when doing so: the current state of your files is first
  saved in an automatic snapshot, and restoring is itself recorded as a new
  snapshot. You can therefore always return to the state before the
  restoration.

  If you only want to recover an earlier version of the current document,
  then use <menu|Version|Restore version> instead, or first compare it with
  the current version using <menu|Version|Compare with>. See
  <hlink|comparing and restoring versions|man-git-history.en.tm>.

  <paragraph*|Synchronizing with your coauthors>

  If your project was cloned from a server, or if a server was added to it,
  then <menu|Version|Synchronize> (or <key|version y>) first gets the
  snapshots of your coauthors, then sends yours. This happens in the
  background, so that you can continue to work in the meantime; the
  progress is shown in the footer.

  If both you and a coauthor saved snapshots since the last
  synchronization, then <TeXmacs> asks whether they should be merged. Parts
  which were changed by only one of you are merged automatically, even
  inside a same paragraph. If you both changed the same words, then a
  conflict remains, which you have to resolve before synchronizing again:
  see <hlink|resolving conflicts|man-git-conflicts.en.tm>.

  It is a good habit to synchronize before starting to work, and after
  saving a snapshot.

  <paragraph*|Other commands>

  The submenu <menu|Version|Git> also gives access to the status of the
  repository, the history of all snapshots, the <name|Git> panel and the
  output of the last <name|Git> commands. The full mode can be selected
  again in <menu|Version|Git|Preferences|Simple mode> or in
  <menu|Version|Git preferences>. Both modes can be mixed freely: a
  snapshot is an ordinary commit.

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
