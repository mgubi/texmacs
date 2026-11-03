<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Extending the graphics editor and pitfalls>

  <section|Adding a graphical macro>

  The simplest way to add a new kind of object is a <em|graphical macro>:
  a macro whose arguments are points (and possibly labels) and whose
  expansion is graphical markup. The editor creates such objects exactly
  like curves: the user clicks the successive points, which become the
  arguments of the macro. Two ingredients are needed.

  <subsection|The macro itself>

  The macro must be defined in a style file or package. It can be written
  in pure markup, as <markup|rectangle> in
  <source-link|packages/standard/std-graphics.ts|TeXmacs/packages/standard/std-graphics.ts>:

  <\tm-fragment>
    <inactive*|<assign|rectangle|<macro|p1|p2|<cline|<arg|p1>|<point|<look-up|<arg|p1>|0>|<look-up|<arg|p2>|1>>|<arg|p2>|<point|<look-up|<arg|p2>|0>|<look-up|<arg|p1>|1>>>>>>
  </tm-fragment>

  or delegate the computation to <scheme> through <markup|extern>, as
  <markup|arrow-with-text> in the same file:

  <\tm-fragment>
    <inactive*|<assign|arrow-with-text|<macro|p1|p2|t|<extern|arrow-with-text|<arg|p1>|<arg|p2>|<quote-arg|t>>>>>

    <inactive*|<drd-props|arrow-with-text|arity|3|accessible|2>>
  </tm-fragment>

  The <markup|drd-props> declaration makes only the text argument
  accessible, so that the cursor can enter the label but not the points.
  The package <source-link|packages/experimental/graphical-macros.ts|TeXmacs/packages/experimental/graphical-macros.ts> contains
  more examples which use <markup|extern> for all their computations.

  <subsection|Registering the tag with the editor>

  The editor only offers tags which are listed in <scm|gr-tags-user>
  (<source-link|graphics/graphics-drd.scm|TeXmacs/progs/graphics/graphics-drd.scm>). This list is extended by the
  macro <scm|define-graphics> of <source-link|graphics/graphics-markup.scm|TeXmacs/progs/graphics/graphics-markup.scm>,
  which also defines the <scheme> function used by <markup|extern> and
  declares it secure:

  <\scm-code>
    (tm-define-macro (define-graphics head . l)

    \ \ (receive (opts body) (list-break l not-define-option?)

    \ \ \ \ `(begin

    \ \ \ \ \ \ \ (set! gr-tags-user (cons ',(ca*r head) gr-tags-user))

    \ \ \ \ \ \ \ (tm-define ,head ,@opts (:secure #t) ,@body))))
  </scm-code>

  The functions receive their arguments as <scm|stree>s; points are
  <scm|(point x y)> with string coordinates, and missing arguments must be
  tolerated, because the object is typeset while it is being created. The
  module provides helpers for this: <scm|tm-point?>, <scm|point-\<gtr\>complex>,
  <scm|complex-\<gtr\>point> and <scm|graphics-transform>. For instance:

  <\scm-code>
    (define-graphics (rectangle P1 P2)

    \ \ (let* ((p1 (if (tm-point? P1) P1 '(point "0" "0")))

    \ \ \ \ \ \ \ \ \ (p2 (if (tm-point? P2) P2 p1)))

    \ \ \ \ `(cline ,p1 (point ,(tm-x p2) ,(tm-y p1))

    \ \ \ \ \ \ \ \ \ \ \ \ ,p2 (point ,(tm-x p1) ,(tm-y p2)))))
  </scm-code>

  Once the tag is in <scm|gr-tags-user> and the current style defines it
  (<scm|style-has?>), the <menu|Insert> menu of graphics mode
  (<scm|graphics-mode-menu>) offers it, and selecting it sets the mode to
  <scm|(edit <scm-arg|tag>)>. <scm|object_create> then creates
  <scm|(<scm-arg|tag> (point x y) (point x y))> and the usual point
  insertion takes over. User tags receive the attributes of curves
  (<scm|graphics-attributes>).

  <subsection|Optional refinements>

  <\itemize>
    <item>By default the object is complete when it has the maximal arity
    declared in the <abbr|DRD>, and it may be committed as soon as it has
    the minimal arity. If the macro has non-point arguments (labels), the
    predicates <scm|graphics-incomplete?> and <scm|graphics-complete?> and
    the function <scm|graphics-complete> should be overloaded; see
    <markup|triangle-with-text> and <markup|arrow-with-text>, whose
    <scm|graphics-complete> appends a default label.

    <item>Adding the tag to the groups <scm|graphical-contains-curve-tag>
    or <scm|graphical-contains-text-tag> (with <scm|define-group>)
    influences the handling of clicks on labels in
    <scm|edit_left-button>.

    <item>Structured editing commands can be overloaded for the new tag, as
    is done for <scm|kbd-remove> on <markup|arrow-with-text>.
  </itemize>

  <subsection|Loading>

  <scm|gr-tags-user> is only filled when the module containing the
  <scm|define-graphics> forms is loaded. <source-link|graphics-markup.scm|TeXmacs/progs/graphics/graphics-markup.scm> is
  loaded lazily (<source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> declares
  <scm|arrow-with-text> and <scm|arrow-with-text*> with <scm|lazy-define>
  and <scm|define-secure-symbols>), and the menu entries call
  <scm|(import-from (graphics graphics-markup))>. A new module of graphical
  macros must similarly be loaded before its macros are offered in the
  menus. Since <markup|extern> only calls secure functions, the functions of
  a lazily loaded module should also be declared with
  <scm|define-secure-symbols>, as is done for <scm|arrow-with-text>.

  <section|Adding a new primitive>

  A new built-in graphical tag requires changes at all levels. Taking a
  hypothetical closed curve as an example:

  <\enumerate>
    <item><em|Tree label.> Add the label to the enumeration in
    <source-link|Kernel/Types/tree_label.hpp|src/Kernel/Types/tree_label.hpp> (next to <cpp|CSPLINE>) and
    declare it in <source-link|Data/Drd/drd_std.cpp|src/Data/Drd/drd_std.cpp> with
    <cpp|returns_graphical ()> and <cpp|point_type> for the point arguments,
    and an arity (<cpp|repeat>, <cpp|fixed>) which the editor will use to
    decide when the object is complete.

    <item><em|Typesetting.> Add a <cpp|case> to the dispatch in
    <source-link|Typeset/Concat/concater.cpp|src/Typeset/Concat/concater.cpp> and a method in
    <source-link|concat_graphics.cpp|src/Typeset/Concat/concat_graphics.cpp>. Follow <cpp|typeset_line>: evaluate the
    points with <cpp|env-\<gtr\>as_point (env-\<gtr\>exec (t[i]))>, keep the
    paths <cpp|descend (ip, i)> of the points in a <cpp|cip> array, build a
    <cpp|curve> in graphical coordinates, map it with <cpp|env-\<gtr\>fr>,
    apply <cpp|adjust_extremities> and produce a <cpp|curve_box>. Use
    <cpp|BEGIN_MAGNIFY> and <cpp|END_MAGNIFY>. If you write a new box type
    instead, implement <cpp|graphical_select> so that it returns
    selections whose <cpp|cp> paths point to the <markup|point> subtrees;
    otherwise the editor cannot select individual points.

    <item><em|Other C++ code.> Some functions enumerate the graphical tags
    explicitly: <cpp|is_graphical> in
    <source-link|Edit/Interface/edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>, <cpp|complete> in
    <source-link|Typeset/Env/env_animate.cpp|src/Typeset/Env/env_animate.cpp> (closed curves in animations), and
    the upgrader in <source-link|Data/Convert/Texmacs/upgradetm.cpp|src/Data/Convert/Texmacs/upgradetm.cpp>.

    <item><em|Tag groups.> Add the tag to the appropriate group in
    <source-link|graphics/graphics-drd.scm|TeXmacs/progs/graphics/graphics-drd.scm>, for instance
    <scm|graphical-closed-curve-tag>. This automatically puts it into
    <scm|gr-tags-all> and <scm|gr-tags-curves>, gives it the curve
    attributes, and makes <scm|object_create> treat it as a curve.

    <item><em|Interface.> Add entries to <scm|graphics-mode-menu> and to
    <scm|graphics-insert-icons>, and a name in <scm|gr-mode-\<gtr\>string>
    (<source-link|graphics/graphics-menu.scm|TeXmacs/progs/graphics/graphics-menu.scm>).

    <item><em|Documentation.> Describe the tag in <hlink|graphics
    primitives|../format/regular/prim-graphics.en.tm>.
  </enumerate>

  A new editing mode, rather than a new object type, is added by
  overloading the functions <scm|edit_move>, <scm|edit_left-button>,
  <scm|edit_right-button>, <scm|edit_middle-button>,
  <scm|edit_start-drag>, <scm|edit_drag> and <scm|edit_end-drag> with a
  <scm|:require> clause on the mode symbol and the option
  <scm|(:state graphics-state)>, as <source-link|graphics/graphics-single.scm|TeXmacs/progs/graphics/graphics-single.scm>
  does for <scm|'hand-edit>, and by making <scm|graphics-mode-attributes>,
  <scm|graphics-finish> and <scm|gr-mode-\<gtr\>string> aware of it.

  <section|Pitfalls>

  <\description>
    <item*|Two families of variables>Changing <src-var|gr-color> does not
    change the color of anything that is already drawn; it changes the
    color of the next object. Conversely, setting <src-var|color> around a
    <markup|graphics> changes all objects without an explicit color, but
    not the properties offered for new objects.

    <item*|Strings for numbers>All coordinates exchanged between C++ and
    <scheme> are strings. The abbreviations <scm|f2s> and <scm|s2f>
    (<source-link|graphics/graphics-utils.scm|TeXmacs/progs/graphics/graphics-utils.scm>) are used everywhere for the
    conversions.

    <item*|Recomputed props>The variables <scm|current-x>,
    <scm|current-path>, <scm|current-obj>, ... are recomputed only when a
    function with the option <scm|(:state graphics-state)> is entered. A
    helper called from elsewhere (a menu command, a keyboard shortcut) sees
    the values of the last mouse event, which may refer to an object that
    has been modified or removed since. Moreover, the state is global: it
    is not attached to a buffer or a picture, which is why
    <scm|graphics-reset-context> must be called when the cursor enters or
    leaves a picture.

    <item*|Objects outside the document>During an operation, the objects
    being edited are not in the document but in the sketch, as detached
    <scm|stree>s. Code which inspects the document at that moment, or
    which saves it, does not see them. Conversely, in selecting state the
    sketch contains live trees, which become invalid if they are removed by
    other means.

    <item*|Undo during an operation>The removal of objects at checkout is
    a real modification of the document. If an operation ends in any other
    way than <scm|sketch-commit> or <scm|graphics-reset-context>, the
    objects are lost until the user performs an undo. New code which starts
    an operation must set <scm|graphics-undo-enabled> and
    <scm|remove-undo-mark?> consistently, as <scm|sketch-checkout> does.

    <item*|The overlay environment>The graphical object is typeset in the
    environment of the editor, not of the picture; only the frame is
    borrowed. Attributes inherited from <markup|with> tags around the
    picture are therefore not applied automatically, which is why
    <scm|create-graphical-props> puts all attributes explicitly.

    <item*|Snapping happens first><scheme> only receives snapped
    coordinates, and the selection of the object under the mouse is done at
    the snapped position. When a snapping mode makes the mouse jump to a
    grid point, the object under the original position may not be found.
    The snapping category <verbatim|text> used for labels and groups by
    <cpp|can_snap> is not in the list <scm|graphics-snap-types> shown in the
    menus, so it can only be enabled through <verbatim|all>.

    <item*|Labels>Inside a <markup|text-at> (more precisely, when
    <cpp|inside_graphics (true)> is false but <cpp|inside_graphics (false)>
    is true) the editor behaves as a text editor, except that the release
    of the left mouse button is still passed to
    <scm|graphics-release-left>, which positions the cursor inside the
    label.

    <item*|Auto-cropping>When <src-var|gr-auto-crop> is true, the
    extents and the origin are determined by the contents, and
    <scm|graphics-move-origin> and <scm|graphics-change-extents> do
    nothing.

    <item*|Default values>The defaults in
    <scm|attribute-default-table> are supposed to coincide with those of
    <cpp|initialize_default_env>. They currently differ for
    <src-var|line-effects> (<verbatim|normal> in <scheme>,
    <verbatim|none> in C++).

    <item*|Console messages>Messages like <verbatim|Uncaptured graphical
    move> or <verbatim|Uncaptured reset-context> come from the default
    implementations of the dispatch functions and usually indicate a mode
    for which a handler is missing.
  </description>

  <section|Known problems in the code>

  The following problems were noticed while writing this documentation.

  <\itemize>
    <item><scm|graphical-get-selected-attributes>
    (<source-link|graphics/graphics-utils.scm|TeXmacs/progs/graphics/graphics-utils.scm>) calls itself instead of
    <scm|graphical-get-selected-attributes*>, and would loop forever; it is
    currently not used.

    <item>In hand drawing mode, a simple click
    (<scm|edit_left-button> for <scm|'hand-edit> in
    <source-link|graphics/graphics-single.scm|TeXmacs/progs/graphics/graphics-single.scm>) creates a point inside
    <scm|(with "point style" "disk" ...)>: the variable name contains a
    space instead of a hyphen, so the style is not applied.

    <item><cpp|typeset_gr_transform> and <cpp|typeset_gr_effect>
    (<source-link|Typeset/Concat/concat_graphics.cpp|src/Typeset/Concat/concat_graphics.cpp>) call
    <cpp|typeset_error> on a wrong arity but then continue, accessing
    children which may not exist.

    <item>The second component returned by <scm|graphics-complete>,
    apparently meant as a cursor position inside the completed object, is
    ignored by <scm|object_commit>.

    <item>The method <cpp|edit_graphics_rep::find_point> and the field
    <cpp|cur_pos> are unused.
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
