<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Preferences, shortcuts and troubleshooting>

  <paragraph*|Preferences>

  <menu|Version|Git preferences> opens a dialog with the following
  settings:

  <\description>
    <item*|Versioning tool>When the <menu|Version> menu is shown:
    <menu|Automatic> (for documents in a repository), <menu|Always> or
    <menu|Never>. This is the same setting as <menu|Tools|Versioning tool>.

    <item*|Mode>The <hlink|simple mode|man-git-snapshots.en.tm> with
    snapshots, or the full mode with staging and branches.

    <item*|When both sides changed>What to do when your changes and those
    of your coauthors have to be combined: ask before merging
    (<menu|Fast-forward only>), <menu|Merge>, or <menu|Rebase>. See
    <hlink|working with coauthors|man-git-remote.en.tm>.

    <item*|Git executable>The <name|Git> program, if it cannot be found
    automatically.

    <item*|Warn for files larger than>Adding a larger file to the
    repository asks for a confirmation, since large files make the
    repository heavy for all coauthors.

    <item*|Commits examined by blame>How far back in the history
    <menu|Version|Who changed what> looks.

    <item*|Commits per page of the log>The number of commits shown at once
    by <menu|Version|Git|Log>.

    <item*|Sign commits and tags with GnuPG>Sign your commits and tags with
    your <name|GnuPG> key.

    <item*|Suggest commit messages>Whether the commit dialog starts with a
    suggested message.
  </description>

  The folders in which <TeXmacs> may run <name|Git> (see <hlink|getting
  started|man-git-start.en.tm>) are also remembered in the preferences.

  <paragraph*|Keyboard shortcuts>

  The following shortcuts are available in documents which belong to a
  repository:

  <\big-table|<descriptive-table|<tformat|<table|<row|<cell|Shortcut>|<cell|Action>>|<row|<cell|<key|version
  g>>|<cell|Open the <name|Git> panel>>|<row|<cell|<key|version
  c>>|<cell|Commit, or save a snapshot in the simple
  mode>>|<row|<cell|<key|version y>>|<cell|Synchronize with the
  coauthors>>|<row|<cell|<key|version s>>|<cell|Show the status
  page>>|<row|<cell|<key|version =>>|<cell|Compare the document with its last
  commit>>>>>>
    Keyboard shortcuts for <name|Git>.
  </big-table>

  <paragraph*|When something goes wrong>

  When <name|Git> cannot complete an operation, <TeXmacs> opens a dialog
  which explains what happened in plain words, together with the message of
  <name|Git>. When possible, the dialog proposes the next step: for
  instance, when the server refused your changes because others sent
  changes first, it offers to get their changes. The button
  <menu|Details> opens the page <menu|Version|Git|Git output>, which lists
  the last commands executed by <TeXmacs>, with their complete output; this
  page is also useful when asking for help.

  Some common situations:

  <\description>
    <item*|The <menu|Version> menu only offers <menu|Use Git in this
    folder>>The repository was not created by <TeXmacs>; see
    <hlink|getting started|man-git-start.en.tm>.

    <item*|The server refused the access>Configure an SSH key or a
    credential helper: <TeXmacs> cannot ask for passwords on behalf of
    <name|Git>.

    <item*|Some of your changes would be overwritten>Commit your changes, or
    save a snapshot, before getting the changes of others or switching
    branches.

    <item*|The state shown is not up to date>This may happen when files are
    changed by another program. Use <menu|Version|Git|Refresh>, or the
    <menu|Refresh> button of the status page.

    <item*|A document was not reloaded>Documents with unsaved changes are
    never reloaded automatically. Save your changes under another name, or
    revert the document with <menu|File|Revert>.
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
