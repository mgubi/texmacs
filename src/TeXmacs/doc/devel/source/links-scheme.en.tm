<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Creating and following links from <scheme>>

  The <scheme> side of the linking system lives in
  <verbatim|progs/link/>. Most of it is loaded lazily
  (<verbatim|init-texmacs.scm> declares <scm|link-follow-ids>,
  <scm|link-active-ids>, <scm|link-mouse-ids>, <scm|link-active-upwards>,
  <scm|get-link-locations>, <scm|register-link-locations> and a few others
  with <scm|lazy-define>), so the first event over an active locus loads
  the navigation code.

  <section|Loci and identifiers (<verbatim|locus-edit.scm>)>

  <\description-paragraphs>
    <item*|<scm|(create-unique-id)>>Returns a new identifier
    <verbatim|+<em|xxx><em|yyy>>, made of a random prefix chosen at startup
    and a counter, both in base 62. The identifiers are unique with high
    probability across sessions and users, which is what makes links
    between files possible.

    <item*|<scm|(make-locus)>>Wraps the selection (or an empty string) in
    <verbatim|(locus (id <em|new-id>) ...)>.

    <item*|<scm|(locus-id t)>>The identifier of the locus <scm|t> (its
    first argument), or <scm|#f>.

    <item*|<scm|(id-\<gtr\>loci id)>>The <markup|locus> tags carrying
    <scm|id> (the parents of the trees returned by
    <scm|id-\<gtr\>trees>).

    <item*|<scm|(locus-set id t)>>Replaces the bodies of all loci with
    identifier <scm|id> by <scm|t>.

    <item*|<scm|locus-insert-link>, <scm|locus-remove-link>,
    <scm|locus-remove-all-links>>Insert a link as an argument of a locus
    (just before the body) or remove it.
  </description-paragraphs>

  <section|Making links interactively (<verbatim|link-edit.scm>)>

  Links are built in two steps, through the <menu|Link> menu (<verbatim|link-menu.scm>) and the
  keyboard shortcuts (<verbatim|link-kbd.scm>) of the linking tool, which
  are only present when <scm|with-linking-tool?> holds. First the participants are
  collected in the table <scm|link-participants>, indexed by their
  position: <scm|(link-set-locus <em|nr>)> stores a tree pointer to the
  innermost locus, <scm|link-set-url>, <scm|link-set-target-url>,
  <scm|link-set-script> and <scm|link-set-target-script> store
  <verbatim|url> and <verbatim|script> vertices. Then <scm|(make-link
  <em|types>)> builds one link <verbatim|(link <em|type> <em|v0> <em|v1>
  ...)> per comma separated type and inserts it according to the
  <em|link mode> (<scm|set-link-mode>):

  <\description>
    <item*|<verbatim|simple>>in the source locus only;

    <item*|<verbatim|bidirectional>>in every locus taking part in the link
    (the default);

    <item*|<verbatim|external>>in the locus containing the cursor, which
    need not be a participant.
  </description>

  <scm|remove-link-of-types> and <scm|remove-all-links> remove the links
  of the current locus, taking the mode into account; in bidirectional
  mode they are meant to remove the copies stored in the other loci as
  well (but see <hlink|pitfalls|links-pitfalls.en.tm>). The utilities
  <scm|link-flatten>, <scm|link-type>, <scm|link-attributes>,
  <scm|link-vertices>, <scm|vertex-\<gtr\>id>, <scm|vertex-\<gtr\>url> and
  <scm|vertex-\<gtr\>script> take links apart.

  <section|Link lists and navigation lists (<verbatim|link-navigate.scm>)>

  Following a link goes through two intermediate representations.

  <paragraph|Link lists.>An item <verbatim|(<em|id> <em|type> <em|attrs>
  <em|v1> ... <em|vn>)> describes a link found from the locus
  <em|id>. <scm|(ids-\<gtr\>link-list ids)> collects, for each identifier,
  the links registered for the vertex <verbatim|(id <em|id>)> (that is,
  <scm|vertex-\<gtr\>links>). <scm|exact-link-list>,
  <scm|upward-link-list> (a tree and its ancestors) and
  <scm|complete-link-list> (a tree and its descendants) build link lists
  for trees. When <em|external navigation> is switched off, only the links
  written inside the locus itself are considered.

  The lists are filtered (<scm|filter-link-list>) on three criteria:

  <\itemize>
    <item>unless <em|bidirectional navigation> is on, only links whose
    first vertex is the locus itself are kept;

    <item>the type must be allowed (<menu|Link|Active link types>, stored in
    <scm|navigation-blocked-types>);

    <item>the type must match the event: for <verbatim|"click">, every
    type except <verbatim|focus> and <verbatim|mouse-over>; for
    <verbatim|"hover">, every type; for <verbatim|"focus"> and
    <verbatim|"mouse-over">, only links of that type.
  </itemize>

  The navigation options are the preferences <verbatim|bidirectional
  navigation> (default <verbatim|off>), <verbatim|external navigation>
  (<verbatim|on>) and <verbatim|link pages> (<verbatim|on>).
  <scm|link-active-ids> keeps the identifiers which have a link passing
  the <verbatim|"hover"> filter; this is how the editor decides which loci
  are active.

  <paragraph|Navigation lists.>An item <verbatim|(<em|type> <em|attrs>
  <em|n> <em|source-id> <em|target>)> is one possible jump: to the
  <em|n>-th vertex of a link, for every vertex other than the source. For
  a link with two vertices, <em|n>=1 is the direct direction and
  <em|n>=0 the inverse one; the <em|extended types> returned by
  <scm|navigation-list-xtypes> append a <verbatim|*> to the type of inverse
  jumps.

  <paragraph|Following.><scm|(link-follow-ids ids event)> filters the link
  list of <scm|ids> on the event, turns it into a navigation list and
  calls <scm|navigation-list-follow>, which

  <\enumerate>
    <item>ignores links of type <verbatim|automatic> when there are links
    of other types;

    <item>follows all <verbatim|focus> jumps at once;

    <item>otherwise, if there are several jumps and link pages are
    enabled, opens an auxiliary page listing them
    (<scm|build-navigation-page>);

    <item>otherwise follows the single jump, or asks the user which
    extended type to follow.
  </enumerate>

  Following a jump (<scm|navigation-item-follow>) marks the source and
  the target as visited (<scm|declare-visited>, then <scm|id-update>
  retypesets the affected loci so that they change colour; hard
  identifiers starting with <verbatim|%> are not recorded) and goes to
  the target vertex:

  <\description>
    <item*|<verbatim|(id <em|name>)>><scm|go-to-id> puts the cursor at
    the end of the first locus with that identifier. If none is known, it
    tries to load the file containing it (<scm|resolve-id>, see below)
    and tries again a little later.

    <item*|<verbatim|(url <em|dest>)>><scm|go-to-url> chooses a pair of
    handlers according to the root of the <abbr|URL>
    (<scm|url-handlers>). In all cases the file part is opened with
    <scm|load-browse-buffer> (for local files, a disambiguation page is
    built when the <abbr|URL> has several alternatives). For local files,
    the part after <verbatim|#> or <verbatim|?> is then handled by a post
    handler according to the file format: <scm|go-to-label> for <TeXmacs>
    documents, line and column positioning for source files; for
    <verbatim|http>, <verbatim|https> and <verbatim|doi> it is ignored.

    <item*|<verbatim|(script <em|fun> <em|args...>)>><scm|execute-script>
    runs the script, subject to the security policy (see <hlink|scripts and
    security|security-scripts.en.tm>), using the <verbatim|secure>
    attribute recorded by the typesetter.
  </description>

  <scm|locus-link-follow> (<verbatim|link return> in the linking tool) follows
  the links of the loci around the cursor as a click would.

  <section|Links between files (<verbatim|link-extern.scm>)>

  A link may point to a locus in another file, which is not necessarily
  loaded. To find it, <TeXmacs> keeps a <em|registry> in
  <verbatim|$TEXMACS_HOME_PATH/system/registry.scm>, which assigns a
  unique identifier to each linked file name (<scm|registry-id>) and
  maps identifiers of files back to their names, and stores the necessary
  information in the documents themselves:

  <\itemize>
    <item>When a buffer is saved, <cpp|buffer_export>
    (<verbatim|Texmacs/Data/new_buffer.cpp>) calls <scm|(get-link-locations
    <em|name> <em|body>)> and stores the result as a <markup|links>
    attribute of the document. For every link in the body whose vertices
    are loci located in <em|other> files (found among the open buffers or
    through the registry), it lists the identifier of the current file, the
    other files (<verbatim|target> entries, with relative names) and, for
    each external locus, the file which contains it (<verbatim|locator>
    entries).

    <item>When a document is loaded, <cpp|import_loaded_tree> passes the
    <markup|links> attribute to <scm|(register-link-locations <em|url>
    <em|links>)>, which adds the entries to the registry and to the table
    of locators.

    <item><scm|(resolve-id id)> uses this information to load a file which
    contains the locus <scm|id>, at most once per file and session.
  </itemize>

  <scm|get-constellation> lists all registered files;
  <verbatim|link-extract.scm> builds auxiliary pages listing the linked
  files (<scm|build-constellation-page>), the loci of the current buffer
  (<scm|build-locus-page>), or all environments of a given type, after
  turning them into loci if necessary (<scm|build-environment-page>).

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
