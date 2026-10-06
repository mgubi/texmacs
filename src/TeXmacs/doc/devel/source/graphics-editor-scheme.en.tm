<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <scheme> state machine>

  <section|Modules>

  The <scheme> part of the graphics editor consists of the modules in
  <verbatim|src/TeXmacs/progs/graphics/>. At the bottom,
  <source-link|graphics-drd.scm|TeXmacs/progs/graphics/graphics-drd.scm> is used by <source-link|graphics-utils.scm|TeXmacs/progs/graphics/graphics-utils.scm> and
  <source-link|graphics-markup.scm|TeXmacs/progs/graphics/graphics-markup.scm>; <source-link|graphics-utils.scm|TeXmacs/progs/graphics/graphics-utils.scm> is used by
  <source-link|graphics-env.scm|TeXmacs/progs/graphics/graphics-env.scm>, <source-link|graphics-object.scm|TeXmacs/progs/graphics/graphics-object.scm> and
  <source-link|graphics-main.scm|TeXmacs/progs/graphics/graphics-main.scm>; these three are used by
  <source-link|graphics-single.scm|TeXmacs/progs/graphics/graphics-single.scm>, which is used by
  <source-link|graphics-group.scm|TeXmacs/progs/graphics/graphics-group.scm>, itself used by
  <source-link|graphics-animate.scm|TeXmacs/progs/graphics/graphics-animate.scm>. The module
  <source-link|graphics-edit.scm|TeXmacs/progs/graphics/graphics-edit.scm> combines the single, group and animation
  modules, and <source-link|graphics-kbd.scm|TeXmacs/progs/graphics/graphics-kbd.scm> and
  <source-link|graphics-menu.scm|TeXmacs/progs/graphics/graphics-menu.scm> are on top.

  None of them is loaded at start-up. <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> declares
  the entry points with <scm|lazy-define> (for instance
  <scm|graphics-reset-context>, <scm|graphics-busy?>, the mouse handlers
  of <source-link|graphics-edit.scm|TeXmacs/progs/graphics/graphics-edit.scm>, <scm|make-graphics>), the keyboard map
  with <scm|lazy-keyboard> (activated by the predicate
  <scm|in-active-graphics?>) and the menus with <scm|lazy-menu>. The first
  call of any of these loads the corresponding module and its dependencies.

  <section|Editing modes>

  The current mode is stored in the property <src-var|gr-mode> of the
  picture and read by <scm|(graphics-mode)>
  (<source-link|graphics/graphics-main.scm|TeXmacs/progs/graphics/graphics-main.scm>), which always returns a list of
  two symbols:

  <\description>
    <item*|<scm|(edit <scm-arg|tag>)>><em|Point mode>: create and modify
    objects of type <scm-arg|tag>, which is <scm|point>, a curve tag
    (<scm|line>, <scm|cline>, <scm|spline>, <scm|arc>, <scm|carc>, ...), a
    text tag (<scm|text-at>, <scm|math-at>, <scm|document-at>) or a user
    defined graphical macro.

    <item*|<scm|(hand-edit <scm-arg|pen>)>>Hand drawing with
    <scm|penscript> or <scm|calligraphy>.

    <item*|<scm|(group-edit <scm-arg|op>)>><em|Group mode>: select several
    objects and apply <scm|move>, <scm|zoom>, <scm|rotate>,
    <scm|group-ungroup>, <scm|props> or <scm|edit-props> to them, or
    <scm|animate> them.
  </description>

  A string value of <src-var|gr-mode> (as written by <scm|make-graphics>)
  is interpreted as <scm|(edit <scm-arg|value>)>, and the built-in default
  of the variable is <verbatim|line>. <scm|(graphics-set-mode
  <scm-arg|mode>)> calls <scm|graphics-group-start> (which clears the
  current operation and puts the cursor at the start of the picture), then
  <scm|graphics-enter-mode>, which resets the state when switching between
  point mode and group mode, and finally stores the new mode.
  <scm|graphics-group-mode?> tests for group mode. The menu entries of the
  <menu|Insert> and <menu|Focus> menus and the toolbar icons
  (<source-link|graphics/graphics-menu.scm|TeXmacs/progs/graphics/graphics-menu.scm>) are mostly calls to
  <scm|graphics-set-mode> with the appropriate argument.

  <section|The editor state>

  <subsection|The state object>

  The state of the editor is declared in
  <source-link|graphics/graphics-env.scm|TeXmacs/progs/graphics/graphics-env.scm> with <scm|define-state>
  (<source-link|kernel/texmacs/tm-states.scm|TeXmacs/progs/kernel/texmacs/tm-states.scm>):

  <\scm-code>
    (define-state graphics-state

    \ \ (slots ((graphics-action #f)

    \ \ \ \ \ \ \ \ \ \ (current-graphical-object #f)

    \ \ \ \ \ \ \ \ \ \ (choosing #f)

    \ \ \ \ \ \ \ \ \ \ (sticky-point #f)

    \ \ \ \ \ \ \ \ \ \ (dragging-create? #f)

    \ \ \ \ \ \ \ \ \ \ (dragging-busy? #f)

    \ \ \ \ \ \ \ \ \ \ (leftclick-waiting #f)

    \ \ \ \ \ \ \ \ \ \ (current-point-no #f)

    \ \ \ \ \ \ \ \ \ \ (current-edge-sel? #f)

    \ \ \ \ \ \ \ \ \ \ (current-selection #f)

    \ \ \ \ \ \ \ \ \ \ (previous-selection #f)

    \ \ \ \ \ \ \ \ \ \ (subsel-no #f)

    \ \ \ \ \ \ \ \ \ \ (graphics-undo-enabled #t)

    \ \ \ \ \ \ \ \ \ \ (remove-undo-mark? #f)

    \ \ \ \ \ \ \ \ \ \ (the-sketch '())

    \ \ \ \ \ \ \ \ \ \ ...))

    \ \ (props ((current-x (f2s (get-graphical-x)))

    \ \ \ \ \ \ \ \ \ \ (current-y (f2s (get-graphical-y)))

    \ \ \ \ \ \ \ \ \ \ (sel (if sticky-point

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ #f

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (select-choose (s2f current-x) (s2f current-y))))

    \ \ \ \ \ \ \ \ \ \ (pxy (if sel (car sel) '()))

    \ \ \ \ \ \ \ \ \ \ (current-path (if sticky-point

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (cDr (cursor-path))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (graphics-path pxy)))

    \ \ \ \ \ \ \ \ \ \ (current-obj ...)

    \ \ \ \ \ \ \ \ \ \ (other-inits ...))))
  </scm-code>

  The <em|slots> are ordinary global variables which persist between
  events. The <em|props> are recomputed from the C++ state each time a
  function declared with the option <scm|(:state graphics-state)> is
  entered: <scm|define-option-state> wraps the body of such a function
  into <scm|with-state-by-name>, which loads the slots and then evaluates
  all the props in order. The option <scm|(:state-slots graphics-state)>
  loads the slots without recomputing the props. The props compute:

  <\description>
    <item*|<scm|current-x>, <scm|current-y>>The snapped mouse position, as
    strings.

    <item*|<scm|sel>>Unless an operation is in progress, the graphical
    selection under the mouse, chosen by <scm|select-choose>. It is a list
    of one or two paths, as explained below.

    <item*|<scm|current-path>>The path of the object under the mouse
    (<scm|graphics-path> of the first path of the selection). During an
    operation, it is the parent of the cursor path instead.

    <item*|<scm|current-obj>>The object at <scm|current-path> as an
    <scm|stree>; during an operation, the object in the graphical object; in
    group mode, the dummy <scm|'(point)>.

    <item*|<scm|current-point-no>, <scm|current-edge-sel?>>The index of the
    selected control point in <scm|current-obj>, and whether the mouse is
    on a curve segment (a selection with two paths) rather than on a
    control point.
  </description>

  The most important slot is <scm|sticky-point>. When it is false, the
  editor is <em|selecting>: moving the mouse only changes the decorations
  showing which object would be affected. When it is true, the editor is
  <em|modifying>: some objects have been taken out of the document into the
  sketch, the mouse moves a point or the whole selection, and the props no
  longer depend on what is under the mouse. <scm|graphics-busy?> simply
  returns <scm|sticky-point>.

  <subsection|Graphical selection on the <scheme> side>

  <scm|(graphics-select <scm-arg|x> <scm-arg|y> <scm-arg|d>)> calls the
  glued <scm|graphical-select> and post-processes the result with
  <scm|filter-graphical-select>. Each element of the result is a list of
  one or two paths (from the <cpp|cp> field of the C++ selections) which
  point to <markup|point> subtrees, or into other leaves of the selected
  object. Selections which do not lie in a picture (<scm|graphics-path>
  returns <scm|#f>) are removed. For objects which are not built-in
  graphical tags (user macros), C++ may return paths with negative
  components; these are cut and replaced by the index of the macro argument
  closest to the mouse (<scm|object-closest-point-pos>). The source code
  calls this a hack.

  <scm|(graphics-path <scm-arg|p>)> (<source-link|graphics/graphics-utils.scm|TeXmacs/progs/graphics/graphics-utils.scm>)
  goes up from a path until it reaches a tree whose ancestors include a
  <markup|graphics> without crossing a graphical text tag, and returns the
  path of the object, or <scm|#f>. Hence for a path to the second point of
  a <markup|line>, <scm|graphics-path> returns the path of the
  <markup|line>, and the last component of the original path is the point
  number.

  <scm|select-choose> remembers the list of candidates in
  <scm|current-selection>. When several objects lie under the mouse,
  <scm|select-next> (bound to the <key|tab> key through
  <scm|kbd-variant>, <scm|graphics-choose-point> and <scm|edit_tab-key>)
  increments <scm|subsel-no> to cycle through them.

  <subsection|Event dispatch>

  The handlers called by C++ (<source-link|graphics/graphics-edit.scm|TeXmacs/progs/graphics/graphics-edit.scm>) just
  dispatch on the first component of the mode, unless the cursor is inside
  a label (<scm|inside-graphical-text?>), in which case the events are
  ignored, or a click in a label moves the cursor within the label:

  <\scm-code>
    (tm-define (graphics-move x y)

    \ \ (when (not (inside-graphical-text?))

    \ \ \ \ (edit_move (car (graphics-mode)) x y)))
  </scm-code>

  The functions <scm|edit_move>, <scm|edit_left-button>,
  <scm|edit_middle-button>, <scm|edit_right-button>,
  <scm|edit_start-drag>, <scm|edit_drag>, <scm|edit_end-drag> and
  <scm|edit_tab-key> are defined with <scm|tm-define> and overloaded with
  <scm|:require> clauses on the mode (<scm|'edit>, <scm|'hand-edit>,
  <scm|'group-edit>) and on the current object. The default versions in
  <source-link|graphics/graphics-single.scm|TeXmacs/progs/graphics/graphics-single.scm> print a message; the default
  drag handlers fall back to the button and move handlers. Note that
  <scm|graphics-start-drag-right> and <scm|graphics-end-drag-right> are
  mapped to a right click, and <scm|graphics-dragging-right> to a move.

  <section|The sketch and the graphical object>

  <subsection|The sketch>

  The <em|sketch>, stored in the slot <scm|the-sketch>, is the list of
  objects the editor is working on (<source-link|graphics/graphics-object.scm|TeXmacs/progs/graphics/graphics-object.scm>):

  <\itemize>
    <item>In selecting state, its elements are <em|trees> of the document:
    the selected objects in group mode. <scm|sketch-toggle>,
    <scm|sketch-set!>, <scm|sketch-reset> and <scm|sketch-in?> manage it.
    Objects are always stored by their root, including surrounding
    <markup|with> and <markup|anim-edit> tags (<scm|radical-\<gtr\>enhanced-tree>).

    <item><scm|(sketch-checkout)> starts an operation: every tree of the
    sketch is <em|removed from the document> (<scm|graphics-remove> with
    <scm|'memoize-layer>, which remembers its position among the children
    of the <markup|graphics>) and replaced in the sketch by a detached copy.
    It sets <scm|sticky-point> and clears <scm|graphics-undo-enabled>.

    <item>During the operation, the sketch is modified freely (by
    <scm|sketch-transform> in group mode, or by direct mutation of the
    <scm|stree> in point mode), and only the graphical object is updated.

    <item><scm|(sketch-commit)> ends the operation: the objects are inserted
    back into the picture at their former positions
    (<scm|graphics-group-insert-bis>), the sketch now refers to the newly
    inserted trees, <scm|sticky-point> is cleared, undo is enabled again,
    and if objects were removed at checkout, <scm|remove-undo-mark> merges
    the removal and the insertion into a single undo step.
  </itemize>

  Hence a document never contains an object in a half edited state: while
  the user drags a point, the object exists only in the sketch and in the
  graphical object.

  <subsection|Building the graphical object>

  <scm|(graphics-decorations-update)> recomputes the graphical object from
  the state, and <scm|(graphics-decorations-reset)> clears it. The work is
  done by <scm|create-graphical-object>, whose arguments are the object,
  the source of its properties, the kind of decoration and the selected
  point:

  <\explain>
    <scm|(create-graphical-object <scm-arg|o> <scm-arg|mode> <scm-arg|pts>
    <scm-arg|no>)><explain-synopsis|set the overlay>
  <|explain>
    <scm-arg|mode> says where the attributes of the drawn object are taken
    from: <scm|'active> (the attributes of the object being edited, cached
    in the hash table <scm|graphical-attrs> by <scm|graphical-fetch-props>),
    <scm|'new> (the <verbatim|gr-> properties of the picture),
    <scm|'default> or a path (the attributes in effect at that path; see
    <scm|get-graphical-prop>). <scm-arg|pts> is <scm|'points> (only the
    control points), <scm|'object> or <scm|'object-and-points>.
    <scm-arg|no> is the selected point number or <scm|(edge no)>; the
    values <scm|'group> and <scm|'no-group> force or forbid the group mode
    rendering, in which the contours of all objects of the sketch are drawn
    by <scm|create-graphical-contours>.
  </explain>

  The control points are produced by <scm|create-graphical-contour>, which
  draws the points of the object with the selected one as a square; text
  objects and groups get a rectangle around their box, computed with
  <markup|box-info> (<scm|create-graphical-embedding-box>). Curves with more
  than 50 points (hand drawings) only show every other point, recursively
  (<scm|compress>). The colors are <scm|default-color-go-points> and
  <scm|default-color-selected-points>.

  <section|Point mode>

  The functions which implement point mode are in
  <source-link|graphics/graphics-single.scm|TeXmacs/progs/graphics/graphics-single.scm>. They fall into two layers: the
  <em|basic operations> (<scm|object_create>, <scm|object_set-point>,
  <scm|object_add-point>, <scm|object_remove-point>, <scm|object_checkout>,
  <scm|object_commit>), which act on the sketch, and the <em|edit
  operations> (<scm|move-over>, <scm|start-move>, <scm|move-point>,
  <scm|next-point>, <scm|last-point>, <scm|remove-point>), which implement
  the transitions of the automaton and print the help messages in the
  footer.

  <subsection|Creating a curve>

  <\enumerate>
    <item><em|First click.> With nothing under the mouse,
    <scm|edit_left-button> calls <scm|edit-insert>, which calls
    <scm|(object_create <scm-arg|tag> x y)>. For a curve this creates
    <scm|(<scm-arg|tag> (point x y) (point x y))>, adds the attributes of
    new objects with <scm|graphics-enrich>, saves the initial state
    (<scm|(graphics-store-state 'start-create)>), sets
    <scm|current-point-no> to 1 and checks the object out into the sketch.
    The object is not inserted in the document.

    <item><em|Moving.> With <scm|sticky-point> set, <scm|edit_move> calls
    <scm|move-point>, which moves point number <scm|current-point-no> to the
    mouse (<scm|object_set-point>) and updates the overlay: the user sees a
    rubber band segment.

    <item><em|Next click.> <scm|next-point> checks whether the mouse moved
    since the previous click (<scm|hardly-moved?>, with a tolerance of
    5 pixels). If it did, <scm|leftclick-waiting> is set. The next motion
    then calls <scm|object_add-point>, which pushes a state (for undo) and
    inserts a new point after the current one, unless the object is
    complete (<scm|graphics-complete?>, which compares the arity with the
    maximal arity from the <abbr|DRD>; an <markup|arc> stops at three
    points).

    <item><em|Finishing.> A second click at the same place (that is, with
    <scm|leftclick-waiting> set and the mouse hardly moved) calls
    <scm|last-point> and <scm|object_commit>. If the object is not
    incomplete (<scm|graphics-incomplete?>), <scm|graphics-complete> may
    add missing arguments, the attributes are attached again with
    <scm|graphics-enrich-bis>, animation wrappers are restored
    (<scm|graphics-re-enhance>) and <scm|sketch-commit> inserts the object
    into the picture. A delayed call to <scm|graphics-update-constraints>
    follows.

    <item><em|Cancelling.> A right or middle click (<scm|graphics-delete>)
    pops the last state with <scm|graphics-back-state>, which removes the
    last point. When only the initial state is left, it calls
    <scm|(undo 0)>; since undo is disabled during the operation, the C++
    undo calls <scm|(graphics-reset-context 'undo)>, which restores the
    state saved by <scm|graphics-store-first> and drops the object.
  </enumerate>

  Dragging instead of clicking (<scm|edit_start-drag> without object under
  the mouse) also creates an object; releasing the button
  (<scm|edit_end-drag>) then only moves the current point, so that the
  creation continues with clicks.

  <subsection|Modifying an object>

  When the mouse is over an object in selecting state,
  <scm|edit_move> calls <scm|move-over>, which shows the control points
  and moves the cursor into the object so that the focus menus and the
  footer refer to it. A drag starting on the object
  (<scm|edit_start-drag>) calls <scm|start-move>:

  <\enumerate>
    <item>the state is stored with action <scm|'start-move>;

    <item><scm|object_checkout> removes the object from the document into
    the sketch;

    <item>if the mouse was on a segment rather than on a control point
    (<scm|current-edge-sel?>), a new point is inserted on that segment;

    <item>the following <scm|edit_drag> events move the point, and
    <scm|edit_end-drag> calls <scm|last-point>, which commits.
  </enumerate>

  A right or middle click on an object calls <scm|remove-point>, which
  removes the control point under the mouse, or the whole object if it
  would become degenerate (<scm|graphics-minimal?>), if it is not a
  built-in graphical tag, or if <key|shift> is pressed.

  <subsection|Text, points and hand drawings>

  Objects without interactive construction are inserted directly. For a
  <markup|point> and for the text tags, <scm|object_create> calls
  <scm|object-set!> with the option <scm|'new>, which inserts the enriched
  object into the picture with <scm|graphics-group-enrich-insert>; for
  text, the cursor is placed inside the new label so that the user can
  type immediately. A left click on an existing label in a text mode moves
  the cursor into it.

  In hand drawing mode (<scm|'hand-edit>), <scm|edit_start-drag> creates
  a <markup|penscript> or <markup|calligraphy> object with an
  <markup|ink-meta> tag and the first sample, and checks it out;
  <scm|edit_drag> appends a sample <scm|(tuple x y t p)> (and moves the end
  point); <scm|edit_end-drag> commits. In calligraphy mode a simple click
  is converted into a tiny drag by <scm|graphics-release-left>, while in
  penscript mode it inserts a point. Snapping is disabled in this
  mode (see <scm|graphics-get-snap-mode>).

  <section|Group mode>

  Group mode is implemented in <source-link|graphics/graphics-group.scm|TeXmacs/progs/graphics/graphics-group.scm>.

  <\description>
    <item*|Selecting>A right click calls <scm|toggle-select>, which toggles
    the object under the mouse in the sketch. A right click on an empty
    place unselects everything if there is a selection, and otherwise
    starts a rectangular selection: <scm|multiselecting> is set, the corner
    is stored in <scm|selecting-x0> and <scm|selecting-y0>,
    <scm|edit_move> draws a red rectangle as graphical object, and the next
    right click selects all objects returned by
    <scm|graphics-select-area>. In the modes <scm|(group-edit edit-props)>
    and <scm|(group-edit animate)>, the left button selects instead. A
    middle click unselects everything, or with <key|shift> deletes the
    selection (<scm|remove-selected-objects>).

    <item*|Operating>A left click calls <scm|start-operation>. If nothing is
    selected, the object under the mouse is selected first. Then the
    barycenter of all points is computed (<scm|store-important-points>),
    the sketch is checked out and converted to <scm|stree>s, and a copy is
    kept in <scm|group-first-go>. During the operation, <scm|edit_move>
    applies <scm|group-translate> (incrementally), or <scm|group-zoom> or
    <scm|group-rotate> (always from <scm|group-first-go>, using the angle or
    distance ratio with respect to the barycenter). These transformations
    act on all <markup|point> subtrees (<scm|traverse-transform>); zooming
    also multiplies the <src-var|magnify> attribute. The next left click
    calls <scm|start-operation> again, which commits.

    <item*|Grouping><scm|group-selected-objects> replaces the selection by
    a single <markup|gr-group> and <scm|ungroup-current-object> does the
    converse; both are invoked from <scm|start-operation> in the mode
    <scm|(group-edit group-ungroup)>.

    <item*|Properties>In mode <scm|(group-edit props)>, clicking on an
    object replaces its attributes by the current <verbatim|gr->
    properties (<scm|graphics-assign-props>). In mode <scm|(group-edit
    edit-props)>, <scm|graphics-get-property> and
    <scm|graphics-set-property> are overloaded (with <scm|former> as
    fallback), so that the property menus show and modify the attributes of
    the selected objects directly; a value which differs between objects is
    shown as <verbatim|mixed>.

    <item*|Clipboard><scm|graphics-copy> returns the selected objects
    wrapped in a <markup|graphics> tree, <scm|graphics-cut> also removes
    them, and <scm|graphics-paste> inserts the children of a pasted
    <markup|graphics> and selects them. These only act in group mode.
  </description>

  Animation editing (<scm|(group-edit animate)>,
  <source-link|graphics/graphics-animate.scm|TeXmacs/progs/graphics/graphics-animate.scm>) builds on group mode and on the
  animation editor in <source-link|dynamic/animate-edit.scm|TeXmacs/progs/dynamic/animate-edit.scm>.

  <section|Undo and the state stack>

  Because objects are removed from the document while they are edited, the
  interaction with undo is delicate. Three mechanisms cooperate:

  <\itemize>
    <item><em|The first state.> <scm|(graphics-store-state
    <scm-arg|action>)> with a non-false action saves the complete state in
    <scm|graphics-first-state>, tagged with the action (<scm|'start-create>,
    <scm|'start-move>, <scm|'start-operation>, ...). <scm|graphics-state-get>
    and <scm|graphics-state-set> convert between the global variables and a
    vector.

    <item><em|The state stack.> <scm|(graphics-store-state #f)> pushes the
    current state on <scm|graphics-states>; <scm|graphics-back-state> pops
    it (used to remove the last added point).

    <item><em|<scm|graphics-reset-context>.> This function is called by
    C++ with the argument <scm|'begin> and <scm|'exit> (entering and
    leaving a picture), <scm|'undo> (after or instead of an undo),
    <scm|'text-cursor> and <scm|'graphics-cursor> (updating the mouse
    pointer). On <scm|'exit> during an operation started with
    <scm|'start-move>, it performs an <scm|unredoable-undo> to put the
    removed object back. On <scm|'undo> during an operation, it either
    undoes the removal (for <scm|'start-move> and
    <scm|'start-operation>) or simply forgets the sketch and returns to
    the first state. In all cases it ends with <scm|graphics-group-start>.
  </itemize>

  The C++ undo routine only performs a real undo when
  <scm|graphics-undo-enabled> is true, i.e. when no operation is in
  progress.

  <section|Properties of new objects>

  The default properties of new objects are the <verbatim|gr->
  variables of the picture (<source-link|graphics/graphics-utils.scm|TeXmacs/progs/graphics/graphics-utils.scm>):

  <\explain>
    <scm|(graphics-get-property <scm-arg|var>)><explain-synopsis|read a
    property of the picture>
  <|explain>
    Looks for <scm-arg|var> in the <markup|with> tags above the innermost
    <markup|graphics> (<scm|get-upwards-tree-property>), or returns its
    initial value in the document. The result is an <scm|stree>.
  </explain>

  <\explain>
    <scm|(graphics-set-property <scm-arg|var> <scm-arg|val>)><explain-synopsis|set
    a property of the picture>
  <|explain>
    Inserts or updates <scm-arg|var> in the <markup|with> around the
    picture (<scm|path-insert-with>), or removes it
    (<scm|path-remove-with>) when <scm-arg|val> is <verbatim|default> or
    equals <scm|graphics-attribute-default>. All the menu commands like
    <scm|graphics-set-color>, <scm|graphics-set-line-width>,
    <scm|graphics-set-arrow-end>, <scm|graphics-set-unit>,
    <scm|graphics-set-extents> or <scm|graphics-set-snap> are implemented
    this way (<source-link|graphics/graphics-main.scm|TeXmacs/progs/graphics/graphics-main.scm>).
  </explain>

  <\explain>
    <scm|(graphics-enrich <scm-arg|obj>)><explain-synopsis|add the default
    attributes>
  <|explain>
    Reads all <verbatim|gr-> properties, keeps those which are relevant for
    the tag of <scm-arg|obj> (<scm|graphical-relevant-attributes>) and do
    not have their default value, and wraps <scm-arg|obj> into a
    <markup|with> setting the corresponding unprefixed variables.
  </explain>

  The keys <key|return> and <key|S-return> copy the <verbatim|gr->
  properties to the object under the mouse
  (<scm|graphics-apply-props-at-mouse>) and conversely
  (<scm|graphics-get-props-at-mouse>). For a label which contains the
  cursor, the functions <scm|object-set-property> and the
  commands like <scm|object-set-fill-color> and
  <scm|object-set-text-at-halign> at the end of
  <source-link|graphics/graphics-main.scm|TeXmacs/progs/graphics/graphics-main.scm> modify the attributes of the label
  itself instead.

  <section|Keyboard, menus and toolbars>

  <source-link|graphics/graphics-kbd.scm|TeXmacs/progs/graphics/graphics-kbd.scm> defines a keymap active in
  <scm|in-active-graphics?>: zooming (<key|+>, <key|->, digits), moving
  the origin (arrow keys), changing the size (<key|A-left>, ...), z-order
  (<key|home>, <key|end>, <key|pageup>, <key|pagedown>, implemented by
  <scm|graphics-zmove> in <source-link|graphics/graphics-edit.scm|TeXmacs/progs/graphics/graphics-edit.scm>), grids
  (<key|#>, <key|C-g>), deletion (<key|backspace>, <key|delete> through
  <scm|graphics-kbd-remove>) and 3D rotations. It overrides
  <scm|keyboard-press> so that only these keys are interpreted in a picture
  (other keys are ignored unless they contain a modifier), and overloads
  the structured editing commands (<scm|kbd-variant>,
  <scm|kbd-horizontal>, <scm|geometry-vertical>, ...) for pictures and
  labels. Pinch gestures and the mouse wheel zoom and scroll the picture.

  <source-link|graphics/graphics-menu.scm|TeXmacs/progs/graphics/graphics-menu.scm> defines the <menu|Insert> and
  <menu|Focus> menus of graphics mode (<scm|graphics-insert-menu>,
  <scm|graphics-focus-menu>) and the toolbars (<scm|graphics-icons>,
  <scm|graphics-focus-icons>), which are linked from
  <source-link|texmacs/menus/main-menu.scm|TeXmacs/progs/texmacs/menus/main-menu.scm> when <scm|in-graphics?> holds. The
  focus menu only shows the property submenus which make sense for the
  current mode (<scm|graphics-mode-attribute?>). The check marks are
  obtained with the <scm|:check-mark> option of the setters, together with
  predicates like <scm|graphics-test-property?>.

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
