<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Versioning and document comparison>

  <section|Introduction>

  <TeXmacs> has three different notions of \Pversions\Q, which should not be
  confused:

  <\enumerate>
    <item>The undo/redo history of a buffer, maintained in <c++> by the
    <cpp|archiver> class (<source-link|Data/History/archiver.hpp|src/Data/History/archiver.hpp>). It lives
    in memory only and is described together with patches in <hlink|Live
    documents and shared editing|collab-live.en.tm>.

    <item>Versions of files managed by an external or remote system: the
    version control systems <name|Subversion> and <name|Git> for local
    files, and the server side version lists for remote files (see
    <hlink|the remote file system|collab-remote-fs.en.tm>). <TeXmacs> gives
    a uniform interface to browse histories, open old revisions and commit.

    <item>The structural comparison of two documents, which produces a
    document with special markup for the differences, and the commands for
    navigating through the differences and retaining one of the versions.
  </enumerate>

  This page covers the last two points. All code lives in
  <source-link|progs/version/|TeXmacs/progs/version>.

  <section|The generic versioning interface>

  <subsection|Dispatch>

  <source-link|version/version-tmfs.scm|TeXmacs/progs/version/version-tmfs.scm> defines a set of generic functions
  which are overloaded, using <scm|tm-define> with a <scm|:require>
  clause, by each back-end:

  <\explain>
    <scm|(version-tool <scm-arg|name>)><explain-synopsis|versioning tool of
    a file>
  <|explain>
    Returns <verbatim|"svn"> if a <verbatim|.svn> directory exists in one of
    the ancestor directories of <scm-arg|name>, <verbatim|"git"> if a
    <verbatim|.git> directory exists, <verbatim|"wrap"> if the
    <abbr|URL> wraps a versioned <abbr|URL> (<scm|url-wrap>), and
    <scm|#f> otherwise. The result is cached per file in
    <scm|version-tool-table>, and the corresponding back-end module
    (<verbatim|version-svn> or <verbatim|version-git>) is loaded the
    first time it is needed. <scm|(versioned? <scm-arg|name>)> tests
    whether there is a tool.
  </explain>

  The functions to be provided by a back-end are:

  <\description-paragraphs>
    <item*|<scm|(version-status <scm-arg|name>)>>One of
    <verbatim|"unknown">, <verbatim|"modified"> or
    <verbatim|"unmodified">.

    <item*|<scm|(version-history <scm-arg|name>)>>A list of
    <scm|(<scm-arg|rev> <scm-arg|by> <scm-arg|date> <scm-arg|msg>)>, or
    <scm|#f>.

    <item*|<scm|(version-revision <scm-arg|name> <scm-arg|rev>)>>The
    contents of the file at revision <scm-arg|rev>, as a string.

    <item*|<scm|(version-beautify-revision <scm-arg|name>
    <scm-arg|rev>)>>A short form of a revision for display (<name|Git>
    hashes are truncated to seven characters).

    <item*|<scm|version-update>, <scm|version-register>,
    <scm|version-unregister>, <scm|(version-commit <scm-arg|name>
    <scm-arg|msg>)>>Operations of the <name|Subversion> style interface;
    they return a message to be displayed.

    <item*|<scm|version-supports-svn-style?>,
    <scm|version-supports-git-style?>>Which of the two interfaces the menu
    should offer.
  </description-paragraphs>

  The buffer level commands <scm|update-buffer>, <scm|register-buffer>,
  <scm|commit-buffer> and <scm|commit-buffer-message> call these
  functions and display the result; <scm|version-interactive-update> and
  <scm|version-interactive-commit> first save the buffer
  (<scm|save-buffer> with the options <scm|:update> and <scm|:commit>).
  The menus are in <source-link|version/version-menu.scm|TeXmacs/progs/version/version-menu.scm> (<scm|version-menu>,
  <scm|version-compare-menu>).

  <subsection|<verbatim|tmfs> classes for histories and revisions>

  <\description>
    <item*|<verbatim|tmfs://history/<em|url>>>A generated page with the
    history of a file (<scm|version-show-history>), with a link to each
    revision.

    <item*|<verbatim|tmfs://revision/<em|rev>/<em|url>>>A file at a given
    revision; its load handler returns <scm|(version-revision <scm-arg|u>
    <scm-arg|rev>)>. <scm|version-revision?>, <scm|version-get-revision>
    and <scm|version-head> decompose such <abbr|URL>s, and
    <scm|version-revision-url> builds them.

    <item*|<verbatim|tmfs://commit/<em|rev>/<em|root>>,
    <verbatim|tmfs://git/<em|which>/<em|root>>>Pages for a <name|Git>
    commit (message, parents and diff statistics) and for the global status
    and log of a <name|Git> repository.
  </description>

  For remote files, <source-link|client/client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm> overloads the
  generic functions so that the history is the server side version list
  and revisions are <verbatim|time=> <abbr|URL>s.

  <subsection|The <name|Subversion> and <name|Git> back-ends>

  Both back-ends simply run the command line tools with
  <scm|eval-system> and parse their output.

  <\itemize>
    <item><source-link|version-svn.scm|TeXmacs/progs/version/version-svn.scm> uses <verbatim|svn status>,
    <verbatim|svn log>, <verbatim|svn cat -r>, <verbatim|svn up --accept
    theirs-full>, <verbatim|svn add>, <verbatim|svn remove --force> and
    <verbatim|svn commit -m>. Note that updating accepts the repository
    version in case of conflicts.

    <item><source-link|version-git.scm|TeXmacs/progs/version/version-git.scm> uses <verbatim|git status
    --porcelain>, <verbatim|git log --follow> (at most 1000 entries) for
    histories, <verbatim|git show <em|rev>:<em|path>> for revisions,
    <verbatim|git add> and <verbatim|git reset HEAD> for registration,
    and <verbatim|git commit -m> for the global commit
    (<scm|git-interactive-commit>). It supports the <name|Git> style
    interface only: there is no per-file commit or update.
  </itemize>

  <subsection|Adding a back-end>

  To support another system, add a detection predicate to the <scm|cond>
  in <scm|version-tool> together with the name of the module to load,
  and write a module which overloads the generic functions above with
  <scm|(:require (== (version-tool name) "<em|tool>"))>. The history,
  revision and comparison facilities then work unchanged.

  <section|Comparing documents>

  <subsection|Version markup>

  The result of a comparison is an ordinary document in which each
  difference is represented by one of the tags of the group
  <scm|version-tag> (<source-link|version/version-drd.scm|TeXmacs/progs/version/version-drd.scm>):

  <\description>
    <item*|<markup|version-both>>Two arguments, the old and the new
    content, shown one after the other (using <markup|version-both-small>
    for inline content and <markup|version-both-big> when the old content
    is a <markup|document>).

    <item*|<markup|version-old>, <markup|version-new>>The same two
    arguments, but only the old resp. new content is shown; the other one
    appears in a balloon when the cursor enters the tag.

    <item*|<markup|version-suppressed>>A placeholder for an absent content
    (inserted or deleted material), rendered as a cross.
  </description>

  The macros are defined in <source-link|packages/standard/std-fold.ts|TeXmacs/packages/standard/std-fold.ts>, and
  the colors are given by the environment variables
  <verbatim|old-version-color> and <verbatim|new-version-color>. Since the
  three tags form a variant group, the user can cycle between them.

  <subsection|The comparison algorithm>

  <scm|(compare-versions <scm-arg|t1> <scm-arg|t2>)> in
  <source-link|version/version-compare.scm|TeXmacs/progs/version/version-compare.scm> takes two <scheme> trees and
  returns a merged tree. It works recursively:

  <\itemize>
    <item>Identical trees are returned as is. With the grain
    <verbatim|"rough">, any difference gives
    <scm|(version-both <scm-arg|t1> <scm-arg|t2>)> for the whole trees;
    with the grain <verbatim|"block">, differences inside a paragraph mark
    the whole paragraph. The grain is the preference <verbatim|"versioning
    grain"> (<scm|version-set-grain>), <verbatim|"detailed"> by default.

    <item>Strings and <markup|concat> are first <em|denormalized>: a
    <markup|concat> is split into a sequence of words and spaces, so that
    differences are computed at the level of words. Sequences of words
    (<markup|concat>) and of paragraphs (<markup|document>) are compared by
    <scm|compare-versions-list>, which looks for a long common
    subsequence (<scm|longest-common>, truncated at 25 elements for
    efficiency and never breaking at spaces or empty paragraphs), recursively
    compares the parts before and after it, and falls back to comparing
    \Pskeletons\Q (the labels and arities of the elements) when no
    common element exists. Two single paragraphs with enough words in
    common (a quarter of the shorter one, <scm|long-common?>) are compared
    word by word. Finally the result is normalized again.

    <item>Other tags with the same label and arity are compared argument by
    argument, unless an argument which differs is not accessible (e.g. a
    hidden parameter), in which case the whole trees are shown as different.
    Tags with different labels or arities, <markup|graphics>, and tables
    whose formats differ (<scm|similar-tables?>) are treated as atomic.
    There are special cases for <markup|hide-preamble>,
    <markup|shared>/<markup|mirror> and bibliographies
    (<markup|bib-list>, whose items are matched by their contents).
  </itemize>

  The comparison is a heuristic tuned for text documents; it is not an
  optimal edit distance and it does not detect moved blocks.

  <subsection|Commands>

  <\explain>
    <scm|(compare-with-older <scm-arg|old>)>, <scm|(compare-with-newer
    <scm-arg|new>)><explain-synopsis|compare the current buffer with another
    file>
  <|explain>
    Load the other version with <scm|tree-load-inclusion>, compare, replace
    the current buffer by the merged document, and move to the first
    difference. <scm|compare-with-newer*> compares with an already open
    buffer. For versioned files and revisions, the submenu <menu|Compare with>
    (<scm|version-compare-menu>) lists the revisions of the history and
    chooses between the two functions with <scm|version-newer?>.
  </explain>

  <\explain>
    <scm|(version-first-difference)>, <scm|(version-next-difference)>,
    ...<explain-synopsis|navigation>
  <|explain>
    Move to the first, previous, next or last tag of the group
    <scm|version-tag>.
  </explain>

  <\explain>
    <scm|(version-show <scm-arg|tag>)>,
    <scm|(version-show-all <scm-arg|tag>)><explain-synopsis|change the
    display of differences>
  <|explain>
    Replace the innermost difference, the differences in the selection, in
    the current paragraph (<scm|version-show-paragraph>) or in the whole
    buffer by <markup|version-old>, <markup|version-new> or
    <markup|version-both>.
  </explain>

  <\explain>
    <scm|(version-retain <scm-arg|which>)>, <scm|(version-retain-all
    <scm-arg|which>)><explain-synopsis|resolve differences>
  <|explain>
    Replace differences by one of their versions: <scm|0> for the old
    one, <scm|1> for the new one, or <scm|'current> for the one which is
    displayed (the new one for <markup|version-both>). Suppressed content
    is removed, and the surrounding <markup|concat> and
    <markup|document> are normalized. This is the only form of
    \Pmerging\Q: there is no automatic three-way merge.
  </explain>

  <scm|reactualize-differences> recomputes the comparison inside the
  current difference or selection, for instance after the user edited one
  of the versions or changed the grain. The keyboard shortcuts are defined
  in <source-link|version/version-kbd.scm|TeXmacs/progs/version/version-kbd.scm>.

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
