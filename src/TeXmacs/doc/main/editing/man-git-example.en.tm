<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Example: a paper with a coauthor>

  This page follows a typical collaboration from the beginning: you start a
  paper, put it on a server such as <name|GitHub> or <name|GitLab>, and a
  coauthor joins you. Both of you use the <hlink|simple
  mode|man-git-snapshots.en.tm>.

  <paragraph*|Starting the project>

  Save your paper in a directory of its own, for instance
  <verbatim|paper/paper.tm>, together with its images and bibliography.
  Since the <menu|Version> menu is only shown for documents which are
  already under version control, use <menu|Tools|Versioning tool|Create Git
  repository>. Then open <menu|Version|Git panel>, choose the simple mode,
  and save a first snapshot with <menu|Version|Save snapshot>, for instance
  with the description \PFirst draft\Q.

  <paragraph*|What to put under version control>

  A snapshot contains all files of the directory, except those which are
  ignored by <name|Git>. Version the sources: the documents, the images
  which they use, the bibliographies and your style files. If some of them
  are elsewhere, then <menu|Version|Project> offers to add them. Do not
  version files which can be regenerated, such as exported PDF files, nor
  very large files, which would make the repository heavy for everybody.
  The file <verbatim|.gitignore>, which <TeXmacs> created in the directory,
  lists the files to ignore; it already contains the backup files of
  <TeXmacs>, and a line for PDF files which you only have to uncomment.

  <paragraph*|Putting the project on a server>

  On the web site of your hosting service, create a new, empty project
  (without a <verbatim|README> file), and copy its address, for instance
  <verbatim|git@github.com:you/paper.git>. Connecting your repository to
  the server is done once, in the full mode: select <menu|Version|Git
  preferences> and choose the full mode, use <menu|Version|Git|Remotes|Add
  remote> with the name <verbatim|origin> and this address, then
  <menu|Version|Git|Send changes (push)>. You may then go back to the simple
  mode. Finally, invite your coauthor to the project on the web site.

  <paragraph*|Joining the project>

  Your coauthor uses <menu|Tools|Versioning tool|Clone Git repository>, with
  the address of the project and a directory for the copy, then opens the
  paper from the new directory.

  <paragraph*|Working day by day>

  Each of you works as follows:

  <\enumerate>
    <item><menu|Version|Synchronize> before starting, to get the work of the
    other;

    <item>edit the documents as usual;

    <item><menu|Version|Save snapshot> at the end of a piece of work, with a
    short description;

    <item><menu|Version|Synchronize> again, to send it.
  </enumerate>

  If you both saved snapshots since the last synchronization, then
  <TeXmacs> asks whether they should be merged: answer yes. Documents in
  which you changed different paragraphs are merged directly. If a document
  is reported as having a conflict, open it and use
  <menu|Version|Resolve conflict>: the changes are merged word by word, and
  you only choose between both versions where you changed the same words.
  Then use <menu|Version|Mark as resolved>, save a snapshot and synchronize.
  See <hlink|resolving conflicts|man-git-conflicts.en.tm> for the details.

  At any time, <menu|Version|Compare with> shows how the document differs
  from an earlier version, and <menu|Version|Who changed what> shows who
  last changed each paragraph.

  <paragraph*|Submitting the paper>

  When you submit the paper, give a name to this version with
  <menu|Version|Git|Tag this version> (in the full mode), for instance
  <verbatim|submitted>. When the referee reports arrive, <menu|Version|Compare
  with|Tag submitted> shows everything which was changed since, which helps
  you to write your answer. Note that tags are only kept in your own copy
  of the repository: they are not sent by <menu|Synchronize>.

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
