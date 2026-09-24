<TeXmacs|1.99.10>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Versioning tools>

  When writing documents in collaboration with other authors, it frequently
  arises that one wants to go through changes made by the other authors, and
  either accept, discard or further correct them. For this purpose, a
  special menu <menu|Version> appears in the main menu bar, which makes
  this process automatic. By default, this menu is shown for documents
  under version control; using <menu|Tools|Versioning tool|Always>, it is
  shown for all documents. Below, we will describe in more detail how this
  tool works.

  In addition, there exist many stand-alone programs for maintaining several
  versions of a same file, such as <hlink|<name|Subversion>|https://en.wikipedia.org/wiki/Apache_Subversion>,
  <hlink|<name|Git>|http://git-scm.com/>,
  <hlink|<name|Darcs>|http://darcs.net/>, <hlink|<name|GNU
  Arch>|http://www.gnu.org/software/gnu-arch/>, just to mention a few of
  them. <TeXmacs> currently provides a basic support for <name|Subversion>
  and an extensive support for <name|Git>, but interfaces for the other
  systems could easily be added.

  <paragraph*|Comparing two versions>

  Assume that we have two versions <verbatim|old.tm> and <verbatim|new.tm> of
  the same document. In order to see the changes, first load the newer
  version <verbatim|new.tm>, then click on <menu|Version|Compare|With older
  version> and select the older version <verbatim|old.tm>. The buffer will
  still be named <verbatim|new.tm>, and the changes between both versions
  will be indicated by special markup. If there are any changes, then the
  cursor will be positioned at the first difference. In a similar way, you
  may compare the current buffer with a<nbsp>newer version on disk using
  <menu|Version|Compare|With newer version>.

  It is possible to go through all the differences between the old and new
  versions either from the items in the submenu <menu|Version|Differences>, or using
  the keyboard shortcuts <shortcut|(version-previous-difference)> and
  <shortcut|(version-next-difference)>. One may also use the more general
  structured navigation shortcuts <shortcut|(kbd-select-if-active
  traverse-first)>, <shortcut|(kbd-select-if-active traverse-last)>,
  <shortcut|(kbd-select-if-active traverse-previous)> and
  <shortcut|(kbd-select-if-active traverse-next)>.

  <paragraph*|Visualization of the differences>

  Differences between the two versions can be displayed in three ways: by
  showing only the old version, only the new version, or both versions
  simultaneously. In all cases, the old version is displayed in dark red and
  the new version in dark green.

  The visualization style can be specified individually for each individual
  change, via <menu|Version|Differences|Show> or the keyboard shortcuts
  <shortcut|(version-show 'version-old)> (old version),
  <shortcut|(version-show 'version-new)> (new version) and
  <shortcut|(version-show 'version-both)> (both versions). One may also cycle
  through the different style using the structured variant key
  <shortcut|(variant-circulate (focus-tree) #t)>. If you selected some text,
  then the above actions will apply to the whole selection. In particular, by
  selecting the entire file, you can visualize the older or the newer
  version, or both versions.

  <paragraph*|Retaining a specific version>

  It often occurs that we want to go through the changes between two versions
  and progressively retain either one or the other version for each
  individual difference. Assuming that the cursor is inside a given
  difference, this can be done from entries in the submenu
  <menu|Version|Differences|Retain>. Alternatively, one may use the shortcuts
  <shortcut|(version-retain 0)>, <shortcut|(version-retain 1)> and
  <shortcut|(kbd-control-return)> to retain the old, new and currently
  displayed version, respectively. If both versions are displayed, then
  <shortcut|(kbd-control-return)> retains the new version. After retaining
  one of the versions, we automatically jump to the next difference, which
  can then be processed.

  If you selected some text, then any of the above action will retain the
  appropriate version for each of the differences in the selection. This
  applies in particular to the case when you select the entire document. A
  convenient alternative way to process all differences is to use
  <shortcut|(version-previous-difference)> and
  <shortcut|(version-next-difference)> to go through the differences, use
  <shortcut|(version-show 'version-old)> and <shortcut|(version-show
  'version-new)> to select the preferred version. As soon as all differences
  have been processed, you select the entire document and click on
  <menu|Version|Differences|Retain|Current version>.

  <paragraph*|Grain control and reactualizing the differences>

  The entries in the submenu <menu|Version|Differences|Grain> allow you to control the
  grain with which differences between versions are computed. By default, we
  use the finest grain <menu|Detailed>. It is also possible to compute
  differences on a paragraph-based level, using <menu|Block>. In that case,
  the entire paragraphs in which a change occurs will be highlighted. The
  roughest grain <menu|Rough> will highlight the entire text, if a change
  occurs somewhere inside.

  The grain is used when comparing two documents using
  <menu|Version|Compare>, but it is also possible to change the grain
  for a selected portion of text: simply select the text and choose the new
  grain in the submenu <menu|Version|Differences|Grain>. This can in particular be
  applied on the entire buffer. Similarly, if you change the grain inside a
  difference, then the difference will be recomputed using the new grain.

  Notice that you may also \Pchange\Q the grain to the current grain. This
  has the effect of reactualizing the differences of a selected portion or of
  the current difference at the cursor position. This may be useful, if you
  made some changes to one of the versions. For instance, assume that the old
  version contained a theorem and that we changed it into a lemma in the new
  version and also modified part of its inside text. When visualizing the
  changes, the whole theorem will be highlighted, since there is no
  appropriate markup to indicate that we just changed from a<nbsp>theorem to
  a lemma. Nevertheless, if we want to compare the inside texts, we may turn
  the old theorem into a lemma and then reactualize.

  <paragraph*|Using external programs such as <name|Subversion> for version
  control>

  If the file you are editing belongs to a directory that is under version
  control (only <name|Subversion> and <name|Git> are currently supported,
  although other systems might follow), then the first part of the
  <menu|Version> menu will contain some clickable entries.

  First of all, if the current buffer is under version control, then you may
  take a look at its history using <menu|Version|History of this document>. The history
  contains a list of hyperlinks to older revisions, together with short
  information about who changed what and when. Older revisions cannot be
  saved, but you may compare them to the current user version (on disk or
  being edited) using <menu|Version|Compare with|Current user version>.

  After making some changes to a file under version control, the version
  inside the editor or on disk no longer corresponds to the version in the
  repository. Using<nbsp><menu|Version|Commit>, the current user's version
  can be committed to the repository. When doing so, you will be prompted for
  a small explanatory message about the changes that you have made. A file
  that is not yet under version control can be added to the version control
  system using <menu|Version|Register>. Registering a file does <em|not>
  commit it to the repository: you still have to use <menu|Version|Commit> in
  order to do so.

  If, while you were editing, changes to the file were made in the
  repository, then you may merge the result with your current version using
  <menu|Version|Update>. For <name|Subversion>, no conflict resolution has
  been implemented yet; for <name|Git>, see below.

  <paragraph*|Working with <name|Git>>

  For documents inside a <name|Git> working tree, the <menu|Version> menu
  gives access to a complete interface to <name|Git>: committing changes
  or saving snapshots, comparing the document with any earlier version,
  restoring versions, seeing who changed what, synchronizing with
  coauthors, branches and tags, and the structured resolution of
  conflicts. These tools are described in the chapter <hlink|working with
  <name|Git>|man-git.en.tm>.

  <tmdoc-copyright|2010\U2019|Joris van der Hoeven|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>