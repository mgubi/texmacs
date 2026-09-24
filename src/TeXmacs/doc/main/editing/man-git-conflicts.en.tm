<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Resolving conflicts>

  When two versions of a document are merged, for instance when you get the
  changes of a coauthor, <name|Git> combines the changes made on both sides
  since their common ancestor. For ordinary text files, <name|Git> works
  line by line: two different changes of the same line produce a conflict,
  and conflict markers are written into the file. For <TeXmacs> documents,
  where a whole paragraph is often stored on a single line, this would
  produce many useless conflicts, and the markers would break the document.

  <TeXmacs> therefore merges its documents <em|structurally>: paragraphs,
  words, markup (such as sections or emphasized text) and mathematical
  formulas are compared separately. Every change which was made on only one
  side is applied automatically, even when both authors changed the same
  paragraph. For instance, if the original text is \PThe quick brown fox
  jumps\Q, if you wrote \PThe slow brown fox jumps\Q and your coauthor
  wrote \PThe quick brown fox leaps\Q, then the result is \PThe slow brown
  fox leaps\Q. Only when the same words were changed in two different ways
  a real conflict remains.

  <paragraph*|Resolving the conflicts of a document>

  When a merge leaves conflicts, <TeXmacs> opens the status page, which
  lists the files with conflicts; they are also shown at the top of the
  <name|Git> panel. For a <TeXmacs> document with a conflict, the
  <menu|Version> menu starts with the entries:

  <\description>
    <item*|<menu|Resolve conflict>>Merge the document structurally, and show
    the remaining conflicts in the document. For each of them, your version
    is displayed as the old version (in red) and the version of your
    coauthor as the new version (in green). The bar at the bottom of the
    window shows the number of conflicts; its buttons <menu|Previous> and
    <menu|Next> go from one conflict to the other, the buttons <menu|Mine>
    and <menu|Theirs> after <menu|Keep> retain one of the two versions, and
    those after <menu|Show> choose which versions are displayed. You may of course also edit the
    text of the conflict directly.

    <item*|<menu|Mark as resolved>>Save the document and tell <name|Git>
    that its conflicts are resolved. If some differences were not processed
    yet, then you are warned first.
  </description>

  Once all files are resolved, commit the result with
  <menu|Version|Commit>: the dialog proposes the message prepared by
  <name|Git> for the merge. When you were synchronizing, synchronize again
  to send the merge to your coauthors.

  Other files, such as images or bibliographies, are not merged by
  <TeXmacs>: edit them with the appropriate program, then use
  <menu|Version|Mark as resolved> or the corresponding button of the status
  page. If a document was deleted on one side and changed on the other,
  then you have to decide by hand whether to keep it. If a document was
  created on both sides independently, then both versions are compared
  instead of being merged.

  During a rebase (see <hlink|working with coauthors|man-git-remote.en.tm>),
  the roles are reversed: <name|Git> considers the commits of your
  coauthors as the base, so that <menu|Mine> and <menu|Theirs> are
  swapped.

  <paragraph*|Merging from the command line>

  If you or your coauthors also use <name|Git> in a terminal or in another
  program, then <menu|Version|Git|Merge documents structurally> asks
  <name|Git> to call <TeXmacs> for merging the <TeXmacs> documents of the
  repository. It records in the file <verbatim|.gitattributes> that files
  with the extension <verbatim|.tm> should be merged by <TeXmacs>, and it
  configures your copy of the repository accordingly. The conflicts which
  remain are then written into the documents as differences which
  <TeXmacs> can display, instead of text markers. If the structured merge
  fails, then the usual text merge of <name|Git> is used instead.

  The file <verbatim|.gitattributes> can be committed, but each coauthor
  who wants to benefit from structured merges outside <TeXmacs> has to use
  this command once in their own copy. For the others, <name|Git> keeps
  merging line by line.

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
