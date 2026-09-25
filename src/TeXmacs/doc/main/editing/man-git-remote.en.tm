<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Working with coauthors>

  With <name|Git>, each author works on a complete copy of the repository.
  The copies are kept in sync through a <em|remote> repository, usually on
  a server such as <name|GitHub>, <name|GitLab> or the server of your
  institution: each author sends their commits to the remote
  repository and gets those of the others from it.

  <paragraph*|Remote repositories>

  A repository which was cloned already knows its remote, called
  <verbatim|origin>. Otherwise, create an empty repository on the server
  and add it using <menu|Version|Git|Remotes|Add remote>, with a short name
  (like <verbatim|origin>) and the address given by the server. The same
  submenu allows you to send the current branch to a particular remote
  (<menu|Push to>), and to remove a remote.

  <paragraph*|Synchronizing>

  The easiest way to exchange changes is <menu|Version|Synchronize> (or
  <key|version y>). It first gets the commits of your coauthors and merges
  them with yours, then sends your commits. Before synchronizing, commit
  your work: <name|Git> only exchanges commits, not the changes you did not
  commit yet.

  In the full mode, the three steps can also be done separately, using
  <menu|Version|Git>:

  <\description>
    <item*|<menu|Fetch>>Get the commits of the others, without changing
    your files. The number of commits to get is then shown in the panel and
    in the footer, and you may look at them with <menu|Version|Git|Graph>
    or <menu|Version|Compare with|Remote version>.

    <item*|<menu|Get changes (pull)>>Get the commits of the others and
    merge them into your files.

    <item*|<menu|Send changes (push)>>Send your commits. The first time a
    branch is sent, it is associated with the remote branch of the same
    name.
  </description>

  If others sent changes since you last got theirs, then the server refuses
  yours: get their changes first, then send yours again. <TeXmacs> explains
  this and offers to get the changes directly.

  <paragraph*|When both sides changed>

  If you and your coauthors made commits since the last exchange, then the
  two histories have to be combined. By default, <TeXmacs> asks you whether
  they should be merged, which creates a <em|merge commit>. <name|Git>
  merges files line by line, and a paragraph of a <TeXmacs> document is
  usually stored on a single line: as soon as you both changed the same
  paragraph, even in different places, the document is marked as having a
  <em|conflict>. <menu|Version|Resolve conflict> then merges it word by
  word, using the structure of the document, and only asks you to choose
  where the same words were changed in two different ways; see
  <hlink|resolving conflicts|man-git-conflicts.en.tm>. With
  <menu|Version|Git|Merge documents structurally>, this structured merge
  is done directly during the merge, so that only real conflicts
  remain.

  Users of <name|Git> may prefer another behavior, which can be chosen in
  <menu|Version|Git|Preferences|Pull>: <menu|Merge> always merges without
  asking, and <menu|Rebase> replays your commits on top of those of the
  others, which keeps the history linear. The default, <menu|Fast-forward
  only>, is the one which asks.

  <paragraph*|Operations in the background>

  Exchanges with a server may take a while. They run in the background, so
  that you can continue to work: the footer and the panel show that
  <name|Git> is working, and the result is shown in the footer when it is
  done. Only one such operation can run at a time for a repository. It can
  be interrupted with <menu|Version|Cancel running command> or the
  <menu|Cancel> button of the panel.

  <name|Git> may not ask for a password during these operations, since
  there is no terminal for typing it. If the server refuses the access,
  then configure an SSH key or a credential helper, as explained in the
  documentation of your hosting service.

  <paragraph*|Open documents>

  When an operation of <name|Git> changes files on disk (getting changes,
  merging, switching branches, discarding changes, restoring a stash), the
  open documents which were changed are reloaded automatically. A document
  with unsaved changes is never reloaded: you are warned instead. It is
  therefore best to save your documents before such operations, which
  <TeXmacs> offers to do.

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
