<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|DRD pitfalls and known inconsistencies>

  <section|Pitfalls when declaring properties>

  <\description>
    <item*|DRD and boxes must agree>The <abbr|DRD> decides where the cursor
    may go in the tree; the boxes decide where it can be displayed. A child
    declared accessible but typeset as a decoration (because the macro
    computes with it, or passes it to <markup|extern>), or a child
    typeset as editable content but declared inaccessible, produces
    invisible cursors or a cursor which cannot reach visible text. See
    <hlink|the macro expansion chapter|macro-expansion-drd.en.tm>.

    <item*|Physical indices>All low-level setters (<cpp|set_type (l, nr,
    tp)>, <cpp|set_accessible>, ...), the declaration methods of
    <verbatim|drd_std.cpp> and the numeric arguments of
    <markup|drd-props> designate <em|records>, not children. With
    <verbatim|CHILD_BIFORM> there are only two records; with
    <verbatim|ARITY_VAR_REPEAT>, record <math|0> is the repeated group.
    Out-of-range indices are silently ignored.

    <item*|Arity first>Child records are created by the arity. In a
    <markup|drd-props> for a tag which has no record yet (a pseudo-label
    <verbatim|extern:f>, or a macro defined later than the declaration),
    properties of children are lost unless <verbatim|arity> comes first.

    <item*|<markup|extern> arities count the function>For a pseudo-label
    <verbatim|extern:f>, the children are those of the <markup|extern> tree,
    whose child <math|0> is the function name. To make the first argument
    accessible, declare an arity of <math|2> and <verbatim|accessible\|1>.

    <item*|Order of chained declarations><cpp|accessible (i)> and
    <cpp|hidden (i)> reset the type of the child to regular; put type
    declarations after them.

    <item*|Field widths><cpp|arity_base> has 6 bits and
    <cpp|arity_extra> 4 bits: a macro with more than 63 parameters, or a
    repeated group of more than 15 children, gets a truncated arity
    (assignments to bit fields wrap silently), while the array of child
    records has the full size.

    <item*|Silent failures>Unknown properties, misspelled property names
    and values other than those listed in <hlink|style and document
    DRDs|drd-documents.en.tm> are ignored without warning. Several
    <verbatim|freeze_*> calls are executed even when the value was not
    recognized, which then blocks the heuristics (for instance
    <verbatim|with-like\|true> freezes the with-like flag to false).

    <item*|Stale properties>Properties are added or overwritten, never
    removed: an editor <abbr|DRD> keeps the record of a macro whose
    definition has been deleted, and on-disk style caches keep the
    properties computed from an older version of a style file until they
    are cleared (<menu|Tools|Update|Styles>).

    <item*|Which DRD?>Generic code consults <cpp|the_drd>, which is the
    <abbr|DRD> of the current view. Code that processes a tree of another
    buffer (converters, background tasks, <scheme> code running on hidden
    buffers) must install the right <abbr|DRD>, with <cpp|with_drd> and
    <cpp|get_document_drd> in <c++> or <scm|set-drd> in <scheme>.

    <item*|Global modes are not exception safe>The access and writable
    modes are global integers which every caller restores by hand. A
    <scheme> error between <scm|set-access-mode> and the restoring call
    leaves the editor in the wrong mode.

    <item*|Memoized logic queries><scm|logic-holds?>, <scm|logic-apply> and
    <scm|logic-apply-list> (<verbatim|kernel/logic/logic-data.scm>) cache
    their results in hash tables which are never cleared. Rules added after
    a query (for instance by a lazily loaded module) are invisible for
    queries already answered; declare logic tables before they are first
    used.

    <item*|<scm|define-group> works at expansion time>The macro resets
    <scm|group-resolve-table> and looks up the previous members of the
    group while it is <em|expanded>, and generates different code for the
    first and for later declarations. It is meant to be used at the top
    level of a module; inside a function, use <scm|eval> as
    <scm|tm-register-new-list-tag> does.
  </description>

  <section|Known bugs and inconsistencies>

  The following problems were found while writing this documentation, by
  reading the code; line numbers refer to the current sources. Some have
  already been reported upstream.

  <subsection|In the <c++> code>

  <\description-paragraphs>
    <item*|<verbatim|drd-props> border <verbatim|no>
    (<verbatim|Typeset/Env/env_exec.cpp:753>)>The value <verbatim|no> sets
    <verbatim|BORDER_INNER> instead of <verbatim|BORDER_NO>, so the 64 or
    so declarations <verbatim|border\|no> in the standard packages behave
    like <verbatim|border\|inner>. (Reported.)

    <item*|<markup|minimum> and <markup|maximum>
    (<verbatim|Data/Drd/drd_std.cpp:380-383>)><markup|minimum> is declared
    with <cpp|repeat (2, 1)> and <markup|maximum> with <cpp|repeat (1,
    1)>. Since both are in the group <scm|binary-operation-tag>
    (<verbatim|utils/edit/variants.scm>), turning a one-argument
    <markup|maximum> into a <markup|minimum> with the variant menu yields a
    tree of invalid arity. (Reported.)

    <item*|Variable type uses the with-like freeze bit
    (<verbatim|Data/Drd/drd_info.cpp:314>, <verbatim|:329>)><cpp|set_var_type>
    tests <cpp|freeze_with> and <cpp|freeze_var_type> sets
    <cpp|freeze_with> instead of <cpp|freeze_var_type>. Declaring
    <verbatim|parameter> or <verbatim|macro-parameter> therefore freezes
    the with-like flag, and declaring <verbatim|with-like> prevents the
    heuristics from setting the variable type.

    <item*|Uninitialized <cpp|freeze_type>
    (<verbatim|Data/Drd/tag_info.cpp:109-124>)>The constructor
    <cpp|parent_info (int, int, int, int, bool)> initializes every field
    except <cpp|freeze_type>, and <cpp|parent_info::operator ==>
    (<verbatim|tag_info.cpp:166-183>) ignores it. The bit is thus
    indeterminate in all records created from scratch (including the
    default record of unknown labels); when it happens to be set, the type
    of the tag can no longer be changed by the heuristics. The record of a
    primitive is also never type-frozen on purpose, contrary to what the
    <cpp|frozen> argument suggests.

    <item*|Wrong child in two declarations
    (<verbatim|Data/Drd/drd_std.cpp:539>, <verbatim|:972>)>For
    <markup|script>, <cpp|name (0, "arguments")> overwrites the name
    <verbatim|function> of child <math|0> (child <math|1> was probably
    meant). For <markup|set>, <cpp|variable (0) -\<gtr\> regular (0)>
    makes the variable name regular content (probably <cpp|regular (1)>
    was meant).

    <item*|Type of <src-var|no-patterns>
    (<verbatim|Data/Drd/drd_std.cpp:1047>)>Declared as a color although
    its value is a boolean.

    <item*|Type names are not symmetric
    (<verbatim|Data/Drd/tag_info.cpp:45-104>)><verbatim|TYPE_RAW> and
    <verbatim|TYPE_BINDING> have no name (they decode as
    <verbatim|unknown>), and <verbatim|obsolete> can be decoded but not
    encoded, so it cannot be used in <markup|drd-props>.

    <item*|Queries modify the DRD
    (<verbatim|Data/Drd/drd_info.cpp:476>, <verbatim|540>,
    <verbatim|604>, <verbatim|641>, <verbatim|654>)>For an
    <markup|extern> tree whose function has no record, the getters
    execute <cpp|ti= info(EXTERN); info(lab)= ti;>. This writes into the
    <abbr|DRD> during a query (possibly a shared style <abbr|DRD>), and
    makes the records of <markup|extern> and <verbatim|extern:f> share the
    same <cpp|tag_info_rep>, so that a later setter on one of them (a
    <markup|drd-props> executed after the first query) also changes the
    other.

    <item*|<cpp|get_env_child> ignores <verbatim|extern:f>>Unlike the
    other high-level queries, it uses the record of <markup|extern>
    itself.

    <item*|Sharing of the cached style DRD
    (<verbatim|Edit/Editor/edit_typeset.cpp:284>)>When a style is not
    found in the cache, <cpp|typeset_style_use_cache> replaces the editor
    <abbr|DRD> by the object returned by <cpp|get_style_drd>, which is
    also stored in <cpp|drd_cached>. The subsequent
    <cpp|heuristic_init (pre)>, the <cpp|drd_update> calls and the
    <markup|drd-props> of the document then modify the in-memory style
    <abbr|DRD>, which is later returned by <cpp|get_style_drd> to other
    users (<cpp|get_document_drd>, widgets). The serialized cache is
    written before these modifications, so editors which hit the cache
    are not affected. The code at <verbatim|Texmacs/Window/tm_button.cpp:58>
    has the same structure.

    <item*|Unused or unimplemented declarations>The parent and child
    <cpp|block> fields are never set nor read; the methods
    <cpp|set_block>, <cpp|get_block> and <cpp|freeze_block> (both
    overloads) and <cpp|drd_info::operator tree> are declared in
    <verbatim|drd_info.hpp> but not defined. The macro <cpp|macro (i)> of
    <verbatim|drd_std.cpp:25> refers to a non-existent
    <verbatim|TYPE_MACRO>.

    <item*|Outdated comments (<verbatim|Data/Drd/tag_info.hpp:62>,
    <verbatim|:150>)>The comment on <verbatim|ARITY_OPTIONS> gives a strict
    upper bound whereas <cpp|correct_arity> uses
    <math|base\<leqslant\>n\<leqslant\>base+extra>; the comment on
    <cpp|child_info> describes a \Pmode\Q field with
    <verbatim|MODE_PARENT>, which has been replaced by <cpp|env>.

    <item*|Mode stored in a <cpp|bool>
    (<verbatim|Data/Tree/tree_traverse.cpp:284>)><cpp|move_valid_bis>
    saves the result of <cpp|set_access_mode> in a <cpp|bool>, which
    would restore <verbatim|DRD_ACCESS_SOURCE> as
    <verbatim|DRD_ACCESS_HIDDEN>. It is harmless in practice because the
    branch is not reached in source mode.

    <item*|Suspicious return value
    (<verbatim|Data/Drd/drd_info.cpp:412>)>For an <markup|or-value> tree,
    <cpp|get_syntax (tree, path)> returns the tree itself instead of the
    syntax <verbatim|r> which it has just found. This may be intentional.

    <item*|Glue name>The <scheme> function <scm|tree-insert_point> has an
    underscore in its name, unlike all other glue functions.
  </description-paragraphs>

  <subsection|In style packages>

  <\description>
    <item*|<verbatim|packages/customize/math/math-check.ts>>The declaration
    <verbatim|\<less\>drd-props\|extern:math-check\|with-like\|true\|arity\|1\|accessible\|all\|regular\|all\<gtr\>>
    uses <verbatim|true>, which is not a recognized value (only
    <verbatim|yes> and <verbatim|no> are), and declares an arity of
    <math|1> although the <markup|extern> trees have two children (the
    function name and the argument). As a result, the argument of
    <verbatim|math-check> is outside the declared layout and therefore not
    accessible, which is probably the opposite of what was intended. The
    same holds for <verbatim|extern:math-check-table>.

    <item*|<verbatim|packages/documentation/standard/scheme-api.ts>>The
    property <verbatim|accesible> (sic) in the declaration of
    <markup|doc-module-header-body> is ignored.
  </description>

  <section|Debugging hints>

  <\itemize>
    <item>From <scheme>, inspect the <abbr|DRD> of the current buffer with
    <scm|tree-child-type>, <scm|tree-accessible-child?>,
    <scm|tree-child-env>, <scm|tree-label-type>, <scm|tag-minimal-arity>,
    ... on the tree at the cursor (<scm|(cursor-tree)>, <scm|(tree-up
    (cursor-tree))>).

    <item>In source mode (<menu|Document|Source|Edit source tree>), the
    arguments of tags are colored according to their child types, which
    gives a quick visual check of <markup|drd-props> declarations.

    <item>In <c++>, <cpp|tag_info> and <cpp|parent_info> have an
    <cpp|operator \<less\>\<less\>> (<cpp|cout \<less\>\<less\>
    drd-\<gtr\>info[l]>); printing a <cpp|drd_info> only prints its name.
    The serialized form of the local entries is
    <cpp|drd-\<gtr\>get_locals ()>.

    <item>The cached style environments and <abbr|DRD>s are the files
    <verbatim|$TEXMACS_HOME_PATH/system/cache/__*>; they contain the
    serialized form in <scheme> syntax and can be removed at any time.

    <item>The warning <verbatim|bad heuristic drd convergence> means that
    the heuristic loop did not reach a fixed point after ten rounds (the
    properties of some macros kept changing from one round to the next);
    the resulting properties are those of the last round.
  </itemize>

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
