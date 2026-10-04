<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The DRD from Scheme: glue and tag groups>

  <section|Querying the <c++> DRD>

  The <c++> <abbr|DRD> is <em|read-only> from <scheme>: there is no glue
  function to set a property. Properties are declared in markup, with
  <markup|drd-props> in a style file, a package or the preamble of a
  document. The query functions are declared in
  <verbatim|Scheme/Glue/build-glue-basic.scm> and implemented, for most of
  them, by the small wrappers of <verbatim|Data/Tree/tree_traverse.cpp>.
  They all consult <cpp|the_drd>, <abbr|i.e.> the <abbr|DRD> of the
  <em|current view>; when working on a tree which belongs to another
  buffer, call <scm|(set-drd <scm-arg|buffer-url>)> first.

  Arguments described as trees may be given as <scheme> trees
  (<scm|stree>s), since the glue converts them as <verbatim|content>.

  <subsection|Labels>

  <\explain>
    <scm|(tree-label-type <scm-arg|label>)><explain-synopsis|type of the
    value of a tag or variable>
  <|explain>
    Returns the type name (<verbatim|"regular">, <verbatim|"length">,
    <verbatim|"color">, ...) of a label given as a symbol. For a variable,
    this is the type of its value.
  </explain>

  <\explain>
    <scm|(tree-label-macro? <scm-arg|label>)>

    <scm|(tree-label-parameter? <scm-arg|label>)><explain-synopsis|kind
    of a label>
  <|explain>
    The first is false only for plain variables (<verbatim|VAR_PARAMETER>),
    the second only for plain macros (<verbatim|VAR_MACRO>); both are true
    for macro parameters such as <markup|enunciation-sep>. Used to build the
    focus menus of style parameters.
  </explain>

  <\explain>
    <scm|(tag-minimal-arity <scm-arg|label>)>

    <scm|(tag-maximal-arity <scm-arg|label>)>

    <scm|(tag-possible-arity? <scm-arg|label> <scm-arg|n>)><explain-synopsis|arity
    of a label>
  <|explain>
    The arity bounds and the admissibility test of
    <hlink|the data model|drd-model.en.tm>; the maximum is
    <scm|2147483647> for repeated arities.
  </explain>

  <subsection|Trees>

  <\explain>
    <scm|(tree-minimal-arity <scm-arg|t>)>

    <scm|(tree-maximal-arity <scm-arg|t>)>

    <scm|(tree-possible-arity? <scm-arg|t> <scm-arg|n>)>

    <scm|(tree-insert_point <scm-arg|t> <scm-arg|i>)>

    <scm|(tree-is-dynamic? <scm-arg|t>)><explain-synopsis|arity of a
    tree>
  <|explain>
    The same for the label of <scm-arg|t>; <scm|tree-insert_point>
    (note the underscore) tests whether children may be inserted at
    position <scm-arg|i>, and <scm|tree-is-dynamic?> whether the arity is
    variable. <scm|focus-can-insert?> (<verbatim|generic/generic-edit.scm>)
    compares <scm|tree-arity> with <scm|tree-maximal-arity>.
  </explain>

  <\explain>
    <scm|(tree-accessible-child? <scm-arg|t> <scm-arg|i>)>

    <scm|(tree-accessible-children <scm-arg|t>)>

    <scm|(tree-all-accessible? <scm-arg|t>)>

    <scm|(tree-none-accessible? <scm-arg|t>)><explain-synopsis|accessibility>
  <|explain>
    Accessibility in the current access mode (see <scm|get-access-mode>).
    The library function <scm|tree-map-accessible-children>
    (<verbatim|kernel/library/tree.scm>) applies a function to the
    accessible children only.
  </explain>

  <\explain>
    <scm|(tree-name <scm-arg|t>)>

    <scm|(tree-long-name <scm-arg|t>)>

    <scm|(tree-child-name <scm-arg|t> <scm-arg|i>)>

    <scm|(tree-child-long-name <scm-arg|t> <scm-arg|i>)>

    <scm|(tree-child-type <scm-arg|t> <scm-arg|i>)><explain-synopsis|names
    and types>
  <|explain>
    The name of the tag (by default its label), the names of its children
    (empty if none was declared or inferred), and the type name of a child.
  </explain>

  <\explain>
    <scm|(tree-child-env <scm-arg|t> <scm-arg|i> <scm-arg|var>
    <scm-arg|default>)>

    <scm|(tree-child-env* <scm-arg|t> <scm-arg|i> <scm-arg|env>)>

    <scm|(tree-descendant-env <scm-arg|t> <scm-arg|p> <scm-arg|var>
    <scm-arg|default>)>

    <scm|(tree-descendant-env* <scm-arg|t> <scm-arg|p>
    <scm-arg|env>)><explain-synopsis|environment of children>
  <|explain>
    The value of the variable <scm-arg|var> inside child <scm-arg|i>
    (<abbr|resp.> at the relative path <scm-arg|p>), or
    <scm-arg|default> if the <abbr|DRD> does not change it; the starred
    versions take and return a whole <markup|attr> tree. The values are not
    evaluated.
  </explain>

  <\explain>
    <scm|(with-like? <scm-arg|t>)><explain-synopsis|is <scm-arg|t> an
    environment modifier?>
  <|explain>
    True for <markup|with>, <markup|with-package> and macros inferred or
    declared with-like, provided <scm-arg|t> has children.
  </explain>

  <subsection|Modes and context>

  <\explain>
    <scm|(get-access-mode)>

    <scm|(set-access-mode <scm-arg|mode>)><explain-synopsis|global access
    mode>
  <|explain>
    <math|0> for normal, <math|1> for hidden and <math|2> for source
    access; <scm|set-access-mode> returns the previous mode, which must be
    restored afterwards.
  </explain>

  <\explain>
    <scm|(set-drd <scm-arg|buffer>)><explain-synopsis|make the DRD of a
    buffer current>
  <|explain>
    Sets <cpp|the_drd> to the <abbr|DRD> of the editor of a view of
    <scm-arg|buffer> (<cpp|set_current_drd>). It is not restored
    automatically; <verbatim|texmacs/texmacs/tm-print.scm> resets it
    explicitly after printing auxiliary buffers.
  </explain>

  A short example, valid in any document:

  <\scm-code>
    (tree-child-type '(hlink "text" "https://www.texmacs.org") 1)

    ;; =\<gtr\> "url"

    (tree-accessible-child? (stree-\<gtr\>tree '(hlink "text" "x")) 1)

    ;; =\<gtr\> #f

    (tree-child-env '(frac "1" "2") 0 "math-display" "")

    ;; =\<gtr\> "false"

    (tag-possible-arity? 'with 4)

    ;; =\<gtr\> #f (variable/value pairs followed by a body)
  </scm-code>

  <section|Uses of the glue in Scheme>

  <\description>
    <item*|Focus bar>The focus toolbar shows an input field for each
    \Phidden\Q child of the focus tag (<scm|hidden-child?> in
    <verbatim|generic/generic-menu.scm>): a child which is not accessible,
    is not the name of a variable in a <markup|with>-like tag, and whose
    type has an input format (<scm|type-\<gtr\>format> maps
    <verbatim|"color"> to a color chooser, <verbatim|"url"> to a file
    field, <verbatim|"adhoc">, <verbatim|"graphical">, ... to none). The
    label of the field is the child name (<scm|tree-child-name*>), with
    fall-backs on the preceding variable name or the type. The tag name
    shown in the variant menu comes from <scm|tree-name> (<scm|focus-tag-name>).

    <item*|Variants>After <scm|variant-set> has replaced the label of a
    tree, the cursor is moved to the first accessible child if its old
    child became inaccessible.

    <item*|Documentation>The automatic documentation of tags
    (<verbatim|generic/generic-doc.scm>) describes the arguments by their
    types and accessibility, and the parameters by
    <scm|tree-label-type>.

    <item*|Converters>The <LaTeX> exporter computes math/text statistics
    following <scm|tree-child-env> (<verbatim|convert/latex/tmtex.scm>);
    the semantic math editor does the same in
    <verbatim|math/math-sem-edit.scm>.
  </description>

  <section|Tag groups>

  Many editing functions need to know that certain tags are \Pof the same
  kind\Q: <markup|section>, <markup|subsection>, ... are variants of each
  other, <markup|theorem> has an unnumbered variant <markup|theorem*>,
  <markup|folded> and <markup|unfolded> are two states of one object. This
  information is not part of the <c++> <abbr|DRD>; it is kept in
  <scheme>, as named <em|tag groups>.

  <subsection|<scm|define-group>>

  <\explain>
    <scm|(define-group <scm-arg|group> <scm-arg|member> ...)><explain-synopsis|declare
    or extend a tag group>
  <|explain>
    Defined in <verbatim|utils/edit/variants.scm>. Each <scm-arg|member> is
    either a tag name (a symbol) or a list <scm|(<scm-arg|subgroup>)>
    naming another group whose members are included. The first declaration
    of a group also defines three functions:

    <\description>
      <item*|<scm|(<scm-arg|group>-list)>>the list of all tags of the group,
      with subgroups expanded recursively (<scm|group-resolve>);

      <item*|<scm|(<scm-arg|group>? <scm-arg|label>)>>membership test;

      <item*|<scm|(inside-<scm-arg|group>?)>>whether the cursor is inside
      a tag of the group.
    </description>

    Further declarations of the same group append members. Groups may be
    referenced as subgroups before they are declared.
  </explain>

  <\explain>
    <scm|(group-resolve <scm-arg|group>)>

    <scm|(group-find <scm-arg|label> <scm-arg|group>)><explain-synopsis|resolution>
  <|explain>
    <scm|group-resolve> returns the flattened list of tags, memoized in
    <scm|group-resolve-table> (the memo table is reset by every
    <scm|define-group>). <scm|group-find> returns the innermost subgroup of
    <scm-arg|group> which directly contains <scm-arg|label>, or
    <scm|#f>: this is how the variants of a tag are found.
  </explain>

  For example, <verbatim|text/text-drd.scm> declares

  <\scm-code>
    (define-group variant-tag

    \ \ (section-tag) (list-tag) (figure-tag)

    \ \ (enunciation-tag) (prominent-tag) ...)

    \;

    (define-group section-tag

    \ \ part chapter appendix

    \ \ section subsection subsubsection

    \ \ paragraph subparagraph)
  </scm-code>

  so that <scm|(variants-of 'subsection)> is the list of the
  <scm|section-tag> group.

  <subsection|The standard groups>

  <\description-paragraphs>
    <item*|<scm|variant-tag>>Groups of interchangeable tags.
    <scm|(variants-of <scm-arg|label>)> returns the subgroup of
    <scm|variant-tag> containing the label (taking numbered and
    unnumbered versions into account); it feeds the variant menu of the
    focus bar (<scm|focus-variants-of> in <verbatim|generic/generic-menu.scm>)
    and <scm|variant-circulate> (keyboard shortcuts for cycling through
    variants). Modes overload <scm|focus-variants-of> and
    <scm|variant-circulate> for special cases (mathematics, switches,
    spreadsheets).

    <item*|<scm|similar-tag>>Groups of tags which are \Psimilar\Q for
    structured navigation: <scm|(similar-to <scm-arg|label>)> is used by
    <scm|traverse-incremental> and <scm|traverse-extremal>
    (<verbatim|generic/generic-edit.scm>) to jump to the previous or next
    tag of the same kind (for instance from a theorem to the next
    proposition).

    <item*|<scm|numbered-tag>>Tags which have an unnumbered variant
    obtained by appending <verbatim|*>. <scm|symbol-numbered?>,
    <scm|symbol-unnumbered?>, <scm|numbered-toggle> and
    <scm|variant-set-keep-numbering> implement the numbering toggle of the
    focus bar.

    <item*|<scm|alternate-tag>, <scm|alternate-first-tag>,
    <scm|alternate-second-tag>>Pairs of tags representing two states of
    one object, declared with <scm|(define-alternate <scm-arg|first>
    <scm-arg|second>)>, which also fills <scm|alternate-table>. They drive
    <scm|alternate-toggle>, <scm|fold>, <scm|unfold> and the folding
    icons; <verbatim|math/math-drd.scm> uses them for
    <markup|wide>/<markup|wide*> and <markup|around>/<markup|around*>.

    <item*|Toggles><verbatim|dynamic/dynamic-drd.scm> defines the macros
    <scm|define-toggle>, <scm|define-fold> and <scm|define-summarize>, which
    put a pair of tags into <scm|toggle-first-tag>/<scm|toggle-second-tag>,
    <scm|folded-tag>/<scm|unfolded-tag> or
    <scm|summarized-tag>/<scm|detailed-tag> and declare them as
    alternates. The session, program and script modules use
    <scm|define-toggle> for their folded and unfolded fields.

    <item*|Others><scm|hidden-tag> (tags with hidden content, for
    <scm|tree-show-hidden>), <scm|mini-flow-tag>, <scm|make-inline-tag>,
    <scm|make-wrapped-tag>, the many specific groups of
    <verbatim|text/text-drd.scm> (<scm|theorem-tag>, <scm|list-tag>,
    <scm|titled-tag>, <scm|doc-title-tag>, ...) used by the structure and
    title routines, <scm|spell-tag> (used with <scm|group-resolve> by
    <verbatim|tools/spell/spell-edit.scm>), and the graphical groups of
    <verbatim|graphics/graphics-drd.scm>.
  </description-paragraphs>

  <subsection|Where groups are declared>

  <\description-paragraphs>
    <item*|<verbatim|utils/edit/variants.scm>>The macro, the standard
    groups for source-like tags (<scm|argument-tag>, <scm|value-tag>,
    <scm|binary-operation-tag>, <scm|reference-tag>, <scm|citation-tag>,
    ...).

    <item*|<verbatim|text/text-drd.scm>>Sections, lists, enunciations,
    figures, frames, titles, ...

    <item*|<verbatim|math/math-drd.scm>>Fractions, vertical scripts,
    textual operators, mathematical annotations.

    <item*|<verbatim|dynamic/dynamic-drd.scm>>Folds, switches, overlays,
    animations; and the session, program, script and spreadsheet
    variants in <verbatim|dynamic/session-drd.scm>,
    <verbatim|program-drd.scm>, <verbatim|scripts-drd.scm>,
    <verbatim|calc-drd.scm>.

    <item*|<verbatim|generic/format-drd.scm>,
    <verbatim|source/source-drd.scm>, <verbatim|version/version-drd.scm>,
    <verbatim|doc/tmdoc-drd.scm>, <verbatim|education/edu-drd.scm>,
    <verbatim|tools/comment/comment-drd.scm>,
    <verbatim|tools/poster/poster-drd.scm>,
    <verbatim|graphics/graphics-drd.scm>>Groups of the corresponding
    modes and packages. Some editing modules also declare groups directly
    (<verbatim|database/db-edit.scm>, <verbatim|table/table-edit.scm>,
    <verbatim|math/math-edit.scm>, ...).
  </description-paragraphs>

  These modules are not loaded at start-up: they are pulled in through the
  <scm|:use> clauses of the corresponding editing modules
  (<verbatim|text/text-edit.scm>, <verbatim|math/math-edit.scm>,
  <verbatim|dynamic/fold-edit.scm>, ...), which are themselves loaded
  lazily. The contents of a group may therefore grow during a session.

  <subsection|Adding tags to groups>

  To make the variants of a new environment available in the focus bar
  and the keyboard, and to allow structured navigation between them, a
  plug-in or a package module declares, for instance:

  <\scm-code>
    (texmacs-module (my-package my-package-drd)

    \ \ (:use (utils edit variants)))

    \;

    (define-group my-box-tag

    \ \ my-box my-shadowed-box my-rounded-box)

    \;

    (define-group variant-tag (my-box-tag))

    (define-group similar-tag (my-box-tag))
  </scm-code>

  To declare <markup|my-claim> as numbered, so that the focus bar offers
  to toggle it with <markup|my-claim*>, add it to <scm|numbered-tag>
  (both macros must be defined in the style):

  <\scm-code>
    (define-group numbered-tag my-claim)
  </scm-code>

  A group can also be extended at run time: since <scm|define-group> is a
  macro, this requires <scm|eval>, as in <scm|tm-register-new-list-tag>
  (<verbatim|text/text-drd.scm>), which is called from the
  <verbatim|std-list> package (through <markup|extern>) to register new
  list environments defined by the user:

  <\scm-code>
    (eval `(define-group new-list-tag ,t))
  </scm-code>

  <section|The logic programming layer>

  The <scheme> kernel also contains a small <name|Prolog>-like engine
  (<verbatim|kernel/logic/>), loaded at start-up by
  <verbatim|init-texmacs.scm> and documented in <hlink|logical programming
  extensions|../scheme/utils/utils-logic.en.tm>. In the code base it is
  the other half of the \P<abbr|DRD>\Q terminology: declarative
  descriptions of tags used by the converters. By convention predicate
  names end with <verbatim|%>. The forms which matter here are:

  <\description-paragraphs>
    <item*|<scm|(logic-group <scm-arg|name> <scm-arg|tag> ...)>>Membership
    facts, tested with <scm|(logic-in? <scm-arg|x> <scm-arg|name>)>. Used
    for instance for the <LaTeX> command classes in
    <verbatim|convert/latex/latex-command-drd.scm>
    (<scm|latex-command-1%>, <scm|latex-modifier-1%>, ...) and the symbol
    classes in <verbatim|latex-symbol-drd.scm>.

    <item*|<scm|(logic-table <scm-arg|name> (<scm-arg|key> <scm-arg|value>)
    ...)>>Tables, read with <scm|logic-ref> and <scm|logic-ref-list>; a
    key <scm|(:or <scm-arg|k1> <scm-arg|k2> ...)> covers several keys.
    Examples: <scm|latex-needs%> and <scm|latex-package-priority%> in
    <verbatim|convert/latex/latex-drd.scm>, the operator tables of
    <verbatim|convert/mathml/mathml-drd.scm>.

    <item*|<scm|(logic-dispatcher <scm-arg|name> (<scm-arg|tag>
    <scm-arg|function>) ...)>>Tables whose values are evaluated
    functions. Every structured converter maps tags to conversion
    routines this way: <scm|tmtex-primitives%> and
    <scm|tmtex-extra-methods%> (<verbatim|convert/latex/tmtex.scm>),
    <scm|tmhtml-primitives%> (<verbatim|convert/html/tmhtml.scm>),
    <scm|htmltm-methods%>, <scm|tmmath-primitives%>, <scm|mathtm-methods%>.

    <item*|<scm|(logic-rules ...)>>General rules, used for instance to
    merge two dispatchers:

    <\scm-code>
      (logic-rules

      \ \ ((tmtex-methods% 'x 'y) (tmtex-primitives% 'x 'y))

      \ \ ((tmtex-methods% 'x 'y) (tmtex-extra-methods% 'x 'y)))
    </scm-code>

    after which <scm|(logic-ref tmtex-methods% <scm-arg|tag>)> finds the
    routine in either table.
  </description-paragraphs>

  Adding support for a new tag in a converter therefore usually means
  adding an entry to the dispatcher of that converter, not touching the
  <c++> <abbr|DRD>. Note that query results are memoized and never
  invalidated (see the <hlink|pitfalls|drd-pitfalls.en.tm>).

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
