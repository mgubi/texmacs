<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The data relation descriptor (DRD)>

  <section|Introduction>

  A <TeXmacs> document is a tree whose nodes carry arbitrary labels. Only a
  fixed set of labels is known to the <c++> kernel (the <em|primitives>
  <markup|concat>, <markup|frac>, <markup|with>, ...); all other labels are
  macros defined by style files or by the user. Generic tree-manipulating
  code, however, constantly needs to know things about a label which are not
  visible in the tree itself:

  <\itemize>
    <item>how many children a tag may have, and where new children may be
    inserted;

    <item>which children may contain the cursor, which are hidden, which are
    read-only;

    <item>what kind of data each child holds (text, a length, a color, a
    variable name, a <abbr|URL>, ...);

    <item>which environment variables change inside a child (for instance,
    that the body of <markup|equation> is in math mode);

    <item>whether the cursor may stand just before or after the tag;

    <item>whether the tag only modifies the environment (like
    <markup|with> or <markup|strong>);

    <item>a human readable name for the tag and its children.
  </itemize>

  This information is collected in the <em|data relation descriptor>, or
  <abbr|DRD>. The general idea, and the user-level view, are explained in
  <hlink|data relation descriptions|../format/basics/tm-drd.en.tm>; the
  primitive <markup|drd-props>, by which style files declare properties, is
  documented in <hlink|macro primitives|../format/stylesheet/prim-macro.en.tm>.
  This chapter is the developer reference for the whole subsystem, on the
  <c++> side and on the <scheme> side.

  <section|Overview>

  On the <c++> side, a <abbr|DRD> is an object of class <cpp|drd_info>
  (<verbatim|Data/Drd/drd_info.hpp>) which maps tree labels to
  <cpp|tag_info> records (<verbatim|Data/Drd/tag_info.hpp>). There are
  several <abbr|DRD>s at any time:

  <\itemize>
    <item>The <em|standard <abbr|DRD>> <cpp|std_drd>, filled once at
    start-up by <cpp|init_std_drd> (<verbatim|Data/Drd/drd_std.cpp>). It
    describes all built-in primitives and environment variables and also
    registers the string names of the built-in tree labels.

    <item>One <abbr|DRD> per style (more precisely, per style tuple such as
    <verbatim|(article number-europe)>), computed by
    <cpp|compute_env_and_drd> (<verbatim|Data/Document/new_style.cpp>) on
    top of <cpp|std_drd>, partly by <em|heuristic inference> from the macro
    definitions and partly from explicit <markup|drd-props> declarations,
    and cached in memory and on disk.

    <item>One <abbr|DRD> per editor (the member <cpp|editor_rep::drd>),
    obtained from the style <abbr|DRD> and completed with the macros defined
    in the preamble and in the body of the document.

    <item>The global variable <cpp|the_drd>, which points to the
    <abbr|DRD> of the current view and is consulted by all generic tree
    routines (cursor movement, traversal, correction, spell checking, ...).
  </itemize>

  The typesetting environment <cpp|edit_env> holds a reference to the
  <abbr|DRD> of its editor, which is how <markup|drd-props> declarations
  executed during typesetting end up in the right <abbr|DRD>.

  On the <scheme> side, there are two different layers:

  <\itemize>
    <item>Glue functions such as <scm|tree-accessible-child?>,
    <scm|tree-child-type>, <scm|tree-child-env> or <scm|tag-minimal-arity>,
    which query <cpp|the_drd>.

    <item>Purely <scheme> declarative data about tags: <em|tag groups>
    declared by <scm|define-group> in the files <verbatim|*-drd.scm>
    (<scm|variant-tag>, <scm|similar-tag>, <scm|numbered-tag>, ...), and
    tables, groups and dispatchers of the small logic programming engine
    (<scm|logic-group>, <scm|logic-table>, <scm|logic-dispatcher>), used
    mainly by the converters.
  </itemize>

  The two layers are independent: tag groups are not stored in the <c++>
  <abbr|DRD>, and <c++> properties are not visible to the logic engine.

  <section|Source files>

  <\description>
    <item*|<verbatim|Data/Drd/tag_info.hpp>,
    <verbatim|tag_info.cpp>>The records <cpp|parent_info>,
    <cpp|child_info> and <cpp|tag_info>, the type constants
    <verbatim|TYPE_*>, the compact environment table (<cpp|drd_encode>,
    <cpp|drd_decode>) and the conversion of type names
    (<cpp|drd_encode_type>, <cpp|drd_decode_type>).

    <item*|<verbatim|Data/Drd/drd_info.hpp>,
    <verbatim|drd_info.cpp>>The class <cpp|drd_info>: getters, setters and
    freezing of all properties, the high-level queries
    (<cpp|is_accessible_child>, <cpp|get_env_child>, ...) and the heuristic
    inference from macros.

    <item*|<verbatim|Data/Drd/drd_std.hpp>,
    <verbatim|drd_std.cpp>>The standard <abbr|DRD> <cpp|std_drd>, the
    current <abbr|DRD> <cpp|the_drd>, the table <cpp|STD_CODE> of built-in
    tag names and the helper <cpp|with_drd>.

    <item*|<verbatim|Data/Drd/drd_mode.hpp>,
    <verbatim|drd_mode.cpp>>The global access mode and writability mode.

    <item*|<verbatim|Data/Drd/vars.hpp>, <verbatim|vars.cpp>>String
    constants for the names of the built-in environment variables
    (<cpp|MODE>, <cpp|FONT_SIZE>, ...), several hundred of which get a type
    in <cpp|std_drd>.

    <item*|<verbatim|Data/Document/new_style.cpp>>Computation and caching
    of style <abbr|DRD>s (<cpp|compute_env_and_drd>,
    <cpp|get_style_drd>, <cpp|get_document_drd>, <cpp|style_get_cache>,
    <cpp|style_set_cache>).

    <item*|<verbatim|Edit/Editor/edit_typeset.cpp>>Initialization and
    update of the <abbr|DRD> of an editor (<cpp|typeset_style_use_cache>,
    <cpp|typeset_preamble>, <cpp|drd_update>).

    <item*|<verbatim|Typeset/Env/env_exec.cpp>>The evaluation of
    <markup|drd-props> (<cpp|edit_env_rep::exec_drd_props>).

    <item*|<verbatim|Data/Tree/tree_traverse.cpp>>Free functions wrapping
    <cpp|the_drd> (<cpp|is_accessible_child>, <cpp|minimal_arity>,
    <cpp|get_child_type>, ...), most of which are exported to <scheme>.

    <item*|<verbatim|Scheme/Glue/build-glue-basic.scm>>The declarations of
    the <scheme> glue.

    <item*|<verbatim|utils/edit/variants.scm>>The <scheme> macro
    <scm|define-group> and the standard tag groups.

    <item*|<verbatim|*/*-drd.scm>>Tag groups and tables of the various
    editing modes and converters (<verbatim|text/text-drd.scm>,
    <verbatim|math/math-drd.scm>, <verbatim|dynamic/dynamic-drd.scm>, ...).

    <item*|<verbatim|kernel/logic/logic-*.scm>>The logic programming
    engine.
  </description>

  <section|Contents of this chapter>

  <\traverse>
    <branch|The data model|drd-model.en.tm>

    <branch|The standard DRD and new primitives|drd-std.en.tm>

    <branch|Style and document DRDs|drd-documents.en.tm>

    <branch|How the rest of TeXmacs uses the DRD|drd-consumers.en.tm>

    <branch|The DRD from Scheme: glue and tag groups|drd-scheme.en.tm>

    <branch|Pitfalls and known inconsistencies|drd-pitfalls.en.tm>
  </traverse>

  The page <hlink|the data relation descriptor|macro-expansion-drd.en.tm> of
  the chapter on macro expansion describes the heuristic inference of
  properties from macro definitions in more detail; it is summarized, not
  repeated, here.

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
