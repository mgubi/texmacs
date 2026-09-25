<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Working with <name|Git>>

  <hlink|<name|Git>|https://git-scm.com/> is a version control system: it
  records successive versions of the files of a project, called
  <em|commits>, in a <em|repository>. It allows you to go back to any
  earlier version, to see who changed what, and to combine the changes of
  several authors who work on their own copies of the same project. Most
  hosting services for collaborative work, such as <name|GitHub>,
  <name|GitLab> or <name|Codeberg>, are based on <name|Git>.

  <TeXmacs> can be used as a complete interface to <name|Git> for your
  documents: you do not need to use a terminal or another program. All
  commands are in the <menu|Version> menu, which appears automatically for
  documents inside a <name|Git> repository. Moreover, <TeXmacs> understands
  the structure of its documents, which makes several operations more
  pleasant than with general purpose tools:

  <\itemize>
    <item>differences between two versions are shown inside the typeset
    document, and not as lines of source code;

    <item>when you and a coauthor changed different parts of the same
    paragraph, <TeXmacs> merges both changes, word by word;

    <item>the few real conflicts are resolved in the document itself, one by
    one, by choosing your version or the other one;

    <item><menu|Version|Who changed what> tells for each paragraph when it
    was last changed, and by whom;

    <item>commit messages can be suggested from the sections which were
    changed.
  </itemize>

  <TeXmacs> offers two ways of working. In the <em|simple mode>, you only
  save <em|snapshots> of your project and synchronize them with your
  coauthors; this is sufficient for most collaborations on a paper or a
  book, and requires no knowledge of <name|Git>. The <em|full mode> gives
  access to the usual notions of <name|Git>: staging, branches, tags,
  stashes and remote repositories.

  <\traverse>
    <branch|Getting started|man-git-start.en.tm>

    <branch|Example: a paper with a coauthor|man-git-example.en.tm>

    <branch|The simple mode: snapshots|man-git-snapshots.en.tm>

    <branch|Recording changes|man-git-commit.en.tm>

    <branch|Comparing and restoring versions|man-git-history.en.tm>

    <branch|Working with coauthors|man-git-remote.en.tm>

    <branch|Branches, tags and stashes|man-git-branches.en.tm>

    <branch|Resolving conflicts|man-git-conflicts.en.tm>

    <branch|Preferences, shortcuts and troubleshooting|man-git-settings.en.tm>
  </traverse>

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
