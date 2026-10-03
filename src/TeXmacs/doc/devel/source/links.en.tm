<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Links, loci and references>

  <section|Introduction>

  <TeXmacs> has a general linking mechanism which goes well beyond
  hyperlinks. Any piece of a document can be turned into a <em|locus>, that
  is, a fragment which carries one or more <em|identifiers>; <em|links> are
  typed tuples whose <em|vertices> are identifiers, <abbr|URL>s or scripts.
  Hyperlinks, labels and references, actions, the tooltips which preview a
  reference, the colouring of visited links, the synchronization of
  \Pmirror\Q copies, the notification of modifications of whole buffers to
  <scheme> and the navigation between documents are all built on this
  single mechanism.

  This chapter describes the mechanism as seen from the source code: the
  <c++> registry which maps identifiers to trees and trees to links, the way
  the typesetter registers loci and the editor reacts to them, the
  <scheme> code which creates links and follows them, and the separate but
  closely related machinery of labels and references. The primitives
  themselves (<markup|locus>, <markup|id>, <markup|link>, <markup|url>,
  <markup|script>, <markup|observer>, <markup|hard-id>,
  <markup|set-binding>, ...) are documented for authors in <hlink|linking
  primitives|../format/regular/prim-link.en.tm>, and the security rules for
  scripts in links in <hlink|scripts and security|security-scripts.en.tm>.

  <section|Overview>

  The main objects are:

  <\description>
    <item*|Locus>A <markup|locus> tag <verbatim|(locus <em|a1> ... <em|an>
    <em|body>)> whose arguments <em|a1>, ..., <em|an> are identifiers
    <verbatim|(id <em|name>)>, links <verbatim|(link <em|type> <em|v1>
    ...)> or observers <verbatim|(observer <em|name> <em|callback>)>. The
    standard macros <markup|label>, <markup|reference>, <markup|pageref>,
    <markup|hlink> and <markup|action> expand to loci.

    <item*|Identifier>A string. It names one or more subtrees of open
    documents. Several naming schemes coexist: unique identifiers
    <verbatim|+...> created by <scm|create-unique-id>, \Phard\Q identifiers
    <verbatim|%...> computed by <markup|hard-id> from memory addresses,
    identifiers <verbatim|&...> for loci whose body is not accessible, and
    buffer names for the buffer notifier.

    <item*|Link>A tree <verbatim|(link <em|type> <em|attrs> <em|v1> ...
    <em|vn>)> (the attributes are added by the typesetter). Each vertex is
    <verbatim|(id <em|name>)>, <verbatim|(url <em|dest>)> or
    <verbatim|(script <em|fun> <em|args...>)>. The type (for instance
    <verbatim|hyperlink>, <verbatim|anchor>, <verbatim|action>,
    <verbatim|mouse-over>, <verbatim|focus>, <verbatim|mirror>) decides
    how the link is followed.

    <item*|Link repository>An object which owns a set of loci and links and
    registers them in the global tables while it lives. Each bridge of the
    typesetter has one, so loci exist exactly as long as the corresponding
    part of the document is typeset.
  </description>

  The data flow is:

  <\verbatim-code>
    document tree

    \ \ \| typesetting (build_locus)

    \ \ v

    link_repository of each bridge

    \ \ \| registers

    \ \ v

    id -\<gtr\> tree pointers, tree -\<gtr\> ids, vertex -\<gtr\> links

    \ \ \| queried by

    \ \ v

    editor: ids under the mouse and the cursor

    \ \ \| link-follow-ids

    \ \ v

    Scheme: go-to-id, go-to-url, scripts
  </verbatim-code>

  Labels and references use loci for navigation, but their numbers are
  stored elsewhere: in the reference table <cpp|ref> of the buffer data,
  filled during typesetting by <markup|set-binding>.

  <section|Source files>

  <\description-paragraphs>
    <item*|<verbatim|Data/Observers/link.hpp>,
    <verbatim|link.cpp>>The classes <cpp|soft_link> and
    <cpp|link_repository>, the global tables, the navigation queries
    (<cpp|get_ids>, <cpp|get_trees>, <cpp|get_links>), visited loci and
    locus rendering preferences, and the propagation of modifications
    through mirror links (<cpp|link_announce>).

    <item*|<verbatim|Data/Observers/tree_pointer.cpp>>The observer which
    keeps pointing to a locus while the document is edited, with an
    optional <scheme> callback (<cpp|tree_pointer>, <cpp|scheme_observer>).

    <item*|<verbatim|Typeset/Concat/concat_active.cpp>>
    <cpp|build_locus>, which registers the identifiers and links of a
    locus and chooses its colour, and the inline typesetting of loci
    (<cpp|typeset_locus>, <cpp|typeset_set_binding>).

    <item*|<verbatim|Typeset/Bridge/bridge.cpp>,
    <verbatim|bridge_surround.cpp>, <verbatim|bridge_locus.cpp>,
    <verbatim|Typeset/Line/lazy_typeset.cpp>>The per-bridge link
    repositories and the block level typesetting of loci.

    <item*|<verbatim|Typeset/Boxes/Modifier/change_boxes.cpp>,
    <verbatim|Typeset/Boxes/Basic/boxes.cpp>>Locus boxes, and the
    hyperlinks and anchors emitted when printing.

    <item*|<verbatim|Typeset/Env/env_default.cpp>,
    <verbatim|env_exec.cpp>>The default definitions of <markup|label>,
    <markup|reference>, <markup|pageref>, <markup|hlink> and
    <markup|action>; <cpp|exec_hard_id>, <cpp|exec_set_binding>,
    <cpp|exec_get_binding>.

    <item*|<verbatim|Edit/Interface/edit_mouse.cpp>,
    <verbatim|edit_interface.cpp>, <verbatim|edit_keyboard.cpp>>Active
    loci under the mouse and the cursor, and the calls of
    <scm|link-follow-ids>.

    <item*|<verbatim|Edit/Editor/edit_typeset.cpp>,
    <verbatim|Edit/Interface/edit_cursor.cpp>>The reference and auxiliary
    tables of the editor, <cpp|search_label> and <cpp|go_to_label>.

    <item*|<verbatim|progs/link/locus-edit.scm>>Unique identifiers and
    loci.

    <item*|<verbatim|progs/link/link-edit.scm>>Interactive creation and
    removal of links.

    <item*|<verbatim|progs/link/link-navigate.scm>>Link lists, navigation
    lists and following links.

    <item*|<verbatim|progs/link/link-extern.scm>>Links between files: the
    file registry and the link locations stored in documents.

    <item*|<verbatim|progs/link/link-extract.scm>,
    <verbatim|link-menu.scm>, <verbatim|link-kbd.scm>>Pages listing loci,
    environments and linked files; menus and keyboard shortcuts.

    <item*|<verbatim|progs/link/ref-edit.scm>,
    <verbatim|ref-markup.scm>, <verbatim|ref-menu.scm>>Tools for labels
    and references: broken references, duplicate labels, inferred
    references, previews, smart references.
  </description-paragraphs>

  Paths of <c++> files are relative to <verbatim|src/src/>, paths of
  <scheme> files to <verbatim|src/TeXmacs/>.

  <section|Contents of this chapter>

  <\traverse>
    <branch|The link registry in <c++>|links-kernel.en.tm>

    <branch|Loci in the typesetter and the editor|links-typeset.en.tm>

    <branch|Creating and following links from <scheme>|links-scheme.en.tm>

    <branch|Labels, references and the reference table|links-references.en.tm>

    <branch|Pitfalls and known bugs|links-pitfalls.en.tm>
  </traverse>

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
