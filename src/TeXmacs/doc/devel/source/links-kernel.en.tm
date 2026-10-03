<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The link registry in <c++>>

  <section|The global tables>

  The file <verbatim|Data/Observers/link.cpp> keeps three global tables:

  <\description>
    <item*|<cpp|hashmap\<less\>string,list\<less\>observer\<gtr\> \<gtr\>
    id_resolve>>For each identifier, the observers which point to the
    subtrees carrying it.

    <item*|<cpp|hashmap\<less\>observer,list\<less\>string\<gtr\> \<gtr\>
    pointer_resolve>>The converse: for each observer, the identifiers it
    stands for.

    <item*|<cpp|hashmap\<less\>tree,list\<less\>soft_link\<gtr\> \<gtr\>
    vertex_occurrences>>For each vertex (a tree such as <verbatim|(id
    "x")> or <verbatim|(url "...")>), the links in which it occurs.
  </description>

  and a counter <cpp|type_count> of the registered links of each type,
  which <cpp|all_link_types> enumerates. The low level routines
  <cpp|register_pointer>, <cpp|unregister_pointer>, <cpp|register_link>
  and <cpp|unregister_link> maintain the tables; they are file local in
  practice and only called by link repositories.

  The tables are keyed by <em|observers> rather than by trees because
  trees are edited: a locus must still be found after its body has been
  modified, split or replaced. The observers used for loci are
  <em|tree pointers> (<verbatim|Data/Observers/tree_pointer.cpp>), which
  follow their subtree through modifications: on an assignment, a
  variant split or a variant join they move to the new subtree, on the
  removal of a node they move to the child which is kept, on detachment
  they move to the closest remaining tree, and, when created with
  <cpp|flag> set (as for all loci registered by the typesetter), they
  move up to a node which is inserted above them. <cpp|obtain_tree (obs)>
  returns the current tree of an observer.

  <section|Soft links and link repositories>

  <\explain>
    <cpp|class soft_link><explain-synopsis|a registered link>
  <|explain>
    A concrete handle around a single tree, the link itself. Two soft
    links are equal only if they are the same object; this is what allows
    the same link tree to be registered several times (for instance once
    for each locus which carries it) and unregistered independently.
  </explain>

  <\explain>
    <cpp|class link_repository_rep><explain-synopsis|a set of loci and
    links>
  <|explain>
    Declared in <verbatim|link.hpp>. It holds three lists, <cpp|ids>,
    <cpp|loci> (observers, in parallel with <cpp|ids>) and <cpp|links>,
    and has three methods:

    <\description>
      <item*|<cpp|insert_locus (id, t)>>Creates a tree pointer to
      <cpp|t> (with the flag set), registers it for <cpp|id> and attaches
      it to <cpp|t>.

      <item*|<cpp|insert_locus (id, t, cb)>>The same with a
      <cpp|scheme_observer (t, cb)>, a tree pointer which also calls the
      <scheme> function <cpp|cb> when the tree is modified (see below).

      <item*|<cpp|insert_link (ln)>>Registers a soft link: every vertex
      <cpp|ln-\<gtr\>t[1]>, ..., <cpp|ln-\<gtr\>t[n-1]> is mapped to it,
      and the type counter of <cpp|ln-\<gtr\>t[0]> is incremented. (The
      attribute argument added by the typesetter thus counts as a vertex
      as well; it never matches a real vertex.)
    </description>

    The destructor unregisters and detaches everything. The handle
    <cpp|link_repository> is reference counted; <cpp|link_repository
    (true)> creates a fresh, empty repository, and the null handle means
    \Pno repository\Q.
  </explain>

  Since unregistration happens in the destructor, the lifetime of a
  repository decides how long its loci exist. Two kinds of repositories
  are used:

  <\itemize>
    <item>Every bridge of the typesetter owns one (<cpp|bridge_rep::link_env>).
    Before a bridge is retypeset, <cpp|my_clean_links> replaces it by a
    fresh repository, and while the bridge is typeset, <cpp|env-\<gtr\>link_env>
    points to it, so that <cpp|build_locus> registers the loci of that part
    of the document there (see <hlink|loci in the typesetter and the
    editor|links-typeset.en.tm> and <hlink|bridges|typesetter-bridges.en.tm>).
    The old repository dies, and its loci are unregistered.

    <item>Every buffer owns one (<cpp|tm_buffer_rep::lns>), created by
    <cpp|attach_notifier> for the buffer notifier (see <hlink|the
    buffer classes|server-buffers.en.tm>).
  </itemize>

  <section|Queries>

  <\description>
    <item*|<cpp|get_ids (tree t)>>The identifiers of the loci whose
    pointer is attached to <cpp|t> (in the order of the observers of the
    tree). Exported as <scm|tree-\<gtr\>ids>.

    <item*|<cpp|get_trees (string id)>>The current trees of all pointers
    registered for <cpp|id>, in order of registration. Exported as
    <scm|id-\<gtr\>trees>.

    <item*|<cpp|get_links (tree v)>>The links in which the vertex
    <cpp|v> occurs. Exported as <scm|vertex-\<gtr\>links>.

    <item*|<cpp|all_link_types ()>>Exported as
    <scm|current-link-types>.
  </description>

  Note that <cpp|get_trees> returns the <em|body> of a locus (the tree
  passed to <cpp|insert_locus>), not the <markup|locus> tag itself; the
  <scheme> function <scm|id-\<gtr\>loci> goes one level up. The glue also
  exports bare tree pointers (<scm|tree-\<gtr\>tree-pointer>,
  <scm|tree-pointer-\<gtr\>tree>, <scm|tree-pointer-detach>), which
  <scheme> uses to remember positions in a document across edits.

  <section|Mirror links>

  Modifications of a locus are propagated to the loci it is linked to by
  <verbatim|mirror> links. Every tree pointer calls <cpp|link_announce
  (obs, mod)> when its tree is about to be modified
  (<cpp|tree_pointer_rep::announce>). For each identifier of the observer
  and each link of the form <verbatim|(link "mirror" <em|attrs> (id
  <em|a>) (id <em|b>))> containing that identifier, the same modification
  is applied to the trees of the other identifier, provided it is
  applicable there (<cpp|is_applicable>), and provided those trees are not
  themselves being modified (<cpp|not_done>, which uses
  <cpp|busy_modifying> and <cpp|busy_tree>). Live mirrored documents (see
  <hlink|live documents|collab-live.en.tm>) rely on this mechanism.

  <section|Observers with callbacks>

  A tree pointer created with a callback name (<cpp|scheme_observer>,
  that is, an <verbatim|(observer <em|id> <em|callback>)> argument of a
  locus, or the buffer notifier) calls the <scheme> function

  <\scm-code>
    (<em|callback> 'announce <em|tree> <em|modification>)

    (<em|callback> 'done <em|tree> <em|modification>)

    (<em|callback> 'touched <em|tree> <em|path>)
  </scm-code>

  before and after each modification of its tree, and when the tree
  or one of its descendants is touched (<cpp|touch>, which propagates
  upwards through the ip observers together with the relative path), but only while the tree is attached to the edit tree
  (<cpp|ip_attached>). The buffer notifier installed by
  <cpp|tm_buffer_rep::attach_notifier> is such an observer, on the body of
  the buffer, with the buffer name as identifier and
  <scm|buffer-notify> (in <verbatim|progs/part/part-shared.scm>) as
  callback; it is used to share whole buffers.

  <section|Visited loci and locus colours>

  <cpp|declare_visited (id)> and <cpp|has_been_visited (id)> manage a
  global set of visited keys; by convention the keys are
  <verbatim|"id:"> followed by an identifier or <verbatim|"url:"> followed
  by a destination. The set lives for the whole session and is shared by
  all buffers. The colours of loci are taken from the environment
  variables <verbatim|locus-color> and <verbatim|visited-color>; the value
  <verbatim|global> means that the user preference of the same name is
  used, as returned by <cpp|get_locus_rendering> (defaults
  <verbatim|#404080> and <verbatim|#702070>). The preference
  <verbatim|locus-on-paper> (<verbatim|change> or <verbatim|preserve>)
  decides whether loci keep their colour when printed.

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
