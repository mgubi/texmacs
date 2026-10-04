<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The C++ side of the graphics editor>

  <section|The class <cpp|edit_graphics_rep>>

  The editor (<cpp|editor_rep>, see the chapter on the
  <hlink|architecture|architecture.en.tm>) is assembled from several
  components by virtual inheritance. The graphics component is
  <cpp|edit_graphics_rep>, declared in
  <verbatim|Edit/Interface/edit_graphics.hpp>. Its public methods are also
  declared as pure virtual functions in <verbatim|Edit/editor.hpp>, so that
  the other components and the glue can call them. Its state is small:

  <\cpp-code>
    class edit_graphics_rep: virtual public editor_rep {

    private:

    \ \ box go_box; \ \ \ \ \ \ \ \ \ \ // The graphical object typesetted as a box

    \ \ double p_x, p_y; \ \ \ \ \ // Last unadjusted (x, y) position

    \ \ double gr_x, gr_y; \ \ \ // Last (x, y) position of the mouse

    \ \ gr_selections gs; \ \ \ \ // Last graphical_select (x, y)

    \ \ grid gr0; \ \ \ \ \ \ \ \ \ \ \ \ // Last grid

    protected:

    \ \ point cur_pos;

    \ \ tree graphical_object;

    ...

    };
  </cpp-code>

  Besides these fields, the snapping parameters are kept in the static
  variables <cpp|snap_mode> and <cpp|snap_distance> of
  <verbatim|edit_graphics.cpp>, set by <cpp|set_snap_mode> and
  <cpp|set_snap_distance>. The field <cpp|cur_pos> is not used.

  <subsection|Where am I?>

  <\explain>
    <cpp|bool inside_graphics (bool b= true)><explain-synopsis|is the
    cursor in a picture?>
  <|explain>
    Walks the cursor path <cpp|tp> down from the root of the document and
    returns whether a <markup|graphics> tag is crossed. With <cpp|b> true
    (the default), entering a <markup|text-at>, <markup|math-at> or
    <markup|document-at> (<cpp|is_graphical_text>) cancels the flag: when
    the cursor is inside a label, the editor is in text mode and keyboard
    events are handled as usual. The glued <scheme> function is
    <scm|in-graphics?>.
  </explain>

  <\explain>
    <cpp|bool inside_active_graphics (bool b= true)><explain-synopsis|in an
    editable picture?>
  <|explain>
    Same, but also requires <src-var|preamble> to be <verbatim|false>, so
    that pictures shown as source code are not edited graphically.
  </explain>

  <\explain>
    <cpp|path graphics_path ()><explain-synopsis|path of the picture>

    <cpp|tree get_graphics ()><explain-synopsis|the picture itself>
  <|explain>
    <cpp|graphics_path> returns the path of the first child of the
    innermost <markup|graphics> above the cursor (or the cursor path if
    there is none); <cpp|get_graphics> returns the innermost
    <markup|graphics> subtree.
  </explain>

  <\explain>
    <cpp|frame find_frame (bool last= false)><explain-synopsis|frame of the
    picture>

    <cpp|grid find_grid ()><explain-synopsis|edit grid of the picture>

    <cpp|void find_limits (point& lim1, point& lim2)><explain-synopsis|visible
    region>
  <|explain>
    These locate the box of the picture with <cpp|eb-\<gtr\>find_box_path>
    and call the corresponding <cpp|box_rep> methods (see <hlink|graphical
    markup and its typesetting|graphics-editor-typeset.en.tm>). They return
    nil objects when the box cannot be found, for instance when the
    picture is not typeset yet. <cpp|find_graphical_region> converts the
    limits into a rectangle in document coordinates.
  </explain>

  <\explain>
    <cpp|bool over_graphics (SI x, SI y)><explain-synopsis|is the mouse
    over the picture?>
  <|explain>
    True if the point lies within the limits of the picture. Outside these
    limits, it returns the value of the <scheme> predicate
    <scm|graphics-busy?>, which is true while an object is being created or
    moved: one can therefore drag a point outside the picture.
  </explain>

  <\explain>
    <cpp|double get_x ()>, <cpp|double get_y ()>, <cpp|double get_pixel
    ()><explain-synopsis|current position and pixel size>
  <|explain>
    The last (snapped) mouse position in graphical coordinates, and the size
    of a screen pixel in graphical units. Glued as
    <scm|get-graphical-x>, <scm|get-graphical-y> and
    <scm|get-graphical-pixel>.
  </explain>

  The method <cpp|find_point> only builds a <markup|point> tree from a
  <cpp|point> and is not used anywhere.

  <section|From a mouse click to a tree modification>

  <subsection|The path of an event>

  A mouse event follows this route:

  <\enumerate>
    <item>The <abbr|GUI> back-end calls
    <cpp|edit_interface_rep::handle_mouse>
    (<verbatim|Edit/Interface/edit_mouse.cpp>). It detects drags
    (<cpp|detect_left_drag>, <cpp|detect_right_drag>), which turns raw
    presses and motions into the event types <verbatim|start-drag-left>,
    <verbatim|dragging-left>, <verbatim|end-drag-left>, and passes the event
    to the <scheme> function <scm|mouse-event>.

    <item>Unless it is overridden (by tooltips, comments and the like),
    <scm|mouse-event> (<verbatim|kernel/gui/kbd-handlers.scm>) calls
    <scm|mouse-any>, glued to <cpp|edit_interface_rep::mouse_any>.

    <item><cpp|mouse_any> performs the generic work (pointer, popups,
    gestures) and then the graphics specific dispatch:

    <\cpp-code>
      if (inside_graphics (type != "release-left")) {

      \ \ path gp= search_upwards (GRAPHICS);

      \ \ if (!is_nil (gp) && gp != previous_gp) {

      \ \ \ \ eval ("(graphics-exit-right)");

      \ \ \ \ mouse_any (type, x, y, mods, t, data);

      \ \ \ \ previous_gp= gp;

      \ \ \ \ return;

      \ \ }

      }

      if (inside_graphics (type != "release-left")) {

      \ \ if (mouse_graphics (type, x, y, mods, t, data)) return;

      \ \ if (!over_graphics (x, y))

      \ \ \ \ eval ("(graphics-reset-context 'text-cursor)");

      }
    </cpp-code>

    The first time an event arrives for a picture which differs from the
    previous one (<cpp|previous_gp>), the cursor is moved behind the picture
    by <scm|graphics-exit-right> and the event is dispatched again, now as
    an ordinary text event. The ordinary click handler
    (<cpp|mouse_select>) then puts the cursor into the picture and calls
    <scm|(graphics-reset-context 'begin)>, which initializes the <scheme>
    state. The argument <cpp|type != "release-left"> means that a left
    click inside a label is still handled graphically, so that one can click
    from a label onto another object.

    <item><cpp|edit_graphics_rep::mouse_graphics> converts the position to
    graphical coordinates, snaps it and calls a <scheme> handler:

    <\cpp-code>
      point p = f [point (x, y)];

      graphical_select (p[0], p[1]); // init the caching for adjust().

      p= adjust (p);

      gr_x= p[0];

      gr_y= p[1];

      string sx= as_string (p[0]);

      string sy= as_string (p[1]);

      invalidate_graphical_object ();

      double pressure= (N(data) == 0? 1.0: data[0]);

      call ("set-keyboard-modifiers", object (m));

      if (type == "move")

      \ \ call ("graphics-move", sx, sy);

      else if (type == "release-left" \|\| type == "double-left")

      \ \ call ("graphics-release-left", sx, sy, (double) t, 1);

      ...

      invalidate_graphical_object ();

      notify_change (THE_CURSOR);

      return true;
    </cpp-code>

    The coordinates are passed to <scheme> as <em|strings>. Events of type
    <verbatim|move> and <verbatim|dragging-left> are dropped when more
    motion events are pending (<cpp|check_event (MOTION_EVENT)>), and
    <verbatim|wheel> events are converted into a relative displacement and
    sent to <scm|graphics-wheel>, which scrolls the picture.

    <item>The <scheme> handlers in <verbatim|graphics/graphics-edit.scm>
    (<scm|graphics-move>, <scm|graphics-release-left>,
    <scm|graphics-start-drag-left>, ...) dispatch on the current mode to the
    functions <scm|edit_move>, <scm|edit_left-button>, ... described in the
    <hlink|next chapter|graphics-editor-scheme.en.tm>. These modify the
    document with the ordinary tree functions (<scm|tree-insert>,
    <scm|tree-assign>, <scm|tree-remove>, ...) and set the graphical
    object.

    <item>At the next repaint, the modified document is retypeset, and
    <cpp|edit_interface_rep::draw_graphics>
    (<verbatim|Edit/Interface/edit_repaint.cpp>) draws the graphical object
    and the graphical cursor.
  </enumerate>

  <descriptive-table|<tformat|<table|<row|<cell|C++ event
  type>|<cell|<scheme> handler>>|<row|<cell|<verbatim|move>>|<cell|<scm|(graphics-move
  x y)>>>|<row|<cell|<verbatim|release-left>,
  <verbatim|double-left>>|<cell|<scm|(graphics-release-left x y t
  p)>>>|<row|<cell|<verbatim|release-middle>>|<cell|<scm|(graphics-release-middle
  x y)>>>|<row|<cell|<verbatim|release-right>,
  <verbatim|double-right>>|<cell|<scm|(graphics-release-right x
  y)>>>|<row|<cell|<verbatim|start-drag-left>, <verbatim|dragging-left>,
  <verbatim|end-drag-left>>|<cell|<scm|(graphics-start-drag-left x y t
  p)> etc.>>|<row|<cell|<verbatim|start-drag-right>,
  <verbatim|dragging-right>, <verbatim|end-drag-right>>|<cell|<scm|(graphics-start-drag-right
  x y)> etc.>>|<row|<cell|<verbatim|drop-object>>|<cell|<scm|(graphics-drop-object
  x y)>>>|<row|<cell|<verbatim|wheel>>|<cell|<scm|(graphics-wheel dx
  dy)>>>>>>

  The left button handlers also receive the time and the pressure (from
  the event data; 1 for clicks), which are recorded in hand drawings. The
  keyboard modifiers are stored on the <scheme> side by
  <scm|set-keyboard-modifiers> and read back with
  <scm|get-keyboard-modifiers>; the handlers test them against
  <scm|ShiftMask>. A drop of an external object first calls the <scheme>
  hook <scm|mouse-drop-event>, which in graphics mode only memorizes the
  object in <scm|the-graphics-drop-object>, and then
  <cpp|mouse_graphics ("drop-object", ...)>, which inserts it as a
  <markup|text-at>.

  <subsection|Other hooks from the generic editor>

  <\description>
    <item*|Entering and leaving a picture><cpp|mouse_select> calls
    <scm|(graphics-reset-context 'begin)> when a click brings the cursor
    into a picture, and invalidates the graphical object and calls
    <scm|(graphics-reset-context 'exit)> when it leaves it (or moves to
    another picture).

    <item*|Repainting><cpp|draw_graphics> calls
    <scm|(graphics-reset-context 'graphics-cursor)> or
    <scm|(graphics-reset-context 'text-cursor)> depending on whether the
    mouse is over an active picture, draws the graphical object, and
    draws a red cross (or a cross with arrows) at the mouse position
    depending on the <scheme> variable <scm|graphics-texmacs-pointer>.
    The editor cursor itself is replaced by the snapped mouse position in
    <cpp|edit_interface_rep::get_cursor>.

    <item*|Applying changes><cpp|edit_interface_rep::apply_changes>
    invalidates the whole region of an active picture when it changed, and
    reads the snapping parameters from <scheme>
    (<scm|graphics-get-snap-mode>, <scm|graphics-get-snap-distance>) into
    <cpp|snap_mode> and <cpp|snap_distance>.

    <item*|Undo><cpp|edit_modify_rep::undo>
    (<verbatim|Edit/Modify/edit_modify.cpp>) consults the <scheme> variable
    <scm|graphics-undo-enabled>. While an object is being created or moved
    this variable is false, and undo only calls
    <scm|(graphics-reset-context 'undo)>, which cancels the current
    operation. Otherwise the undo is performed and
    <scm|(graphics-reset-context 'undo)> resynchronizes the <scheme> state.

    <item*|Clipboard><cpp|selection_copy>, <cpp|selection_paste> and
    <cpp|selection_cut> (<verbatim|Edit/Replace/edit_select.cpp>) call
    <scm|graphics-copy>, <scm|graphics-paste> and <scm|graphics-cut> when
    the cursor is in an active picture.

    <item*|Deletion><cpp|back_in_text_at> is called by the deletion
    routines (<verbatim|Edit/Modify/edit_delete.cpp>) when backspace or
    delete is pressed at the start of an empty label: the
    <markup|text-at> (and its surrounding <markup|with>) is removed from the
    picture.

    <item*|Cursor correction><cpp|edit_typeset_rep::typeset_exec_until>
    moves the cursor to the closest accessible position when it is inside a
    picture but not at an accessible place.
  </description>

  <section|Snapping>

  <subsection|The algorithm>

  Snapping is done in C++ by <cpp|edit_graphics_rep::adjust>, before
  <scheme> sees the coordinates. In <cpp|mouse_graphics> the call to
  <cpp|graphical_select> stores in <cpp|gs> all parts of the picture within
  <cpp|snap_distance> of the mouse (as returned by the boxes), and
  <cpp|adjust> then proceeds as follows:

  <\enumerate>
    <item>The candidate list starts with <cpp|gs>. If the edit grid is not
    empty, the nearest grid point (<cpp|find_point_around>) is added as a
    <verbatim|grid-point> candidate and the nearby grid lines
    (<cpp|get_curves_around>) are added as <verbatim|grid-curve-point>
    candidates, each with its curve, provided they are within the snap
    distance. All distances are measured in the local coordinates of the
    graphics box (<cpp|find_frame (true)>).

    <item><cpp|snap_to_guide> sorts the candidates by distance and chooses:

    <\itemize>
      <item>a candidate without curve (a point, a text, a text handle, a
      group, ...) is returned at once if no grid point was met before it;

      <item>otherwise all pairs of candidates with curves are intersected
      (<cpp|intersection> from <verbatim|Graphics/Types/curve.hpp>, with a
      precision of a tenth of a pixel), except for pairs of two grid lines,
      pairs mixing a point type with a text border, and pairs involving a
      handle; the
      closest admissible intersection is taken if it is closer than the grid
      point;

      <item>else the closest candidate is used, or the mouse position itself
      (type <verbatim|free>) if snapping to it is not allowed.
    </itemize>

    <item>The snapped point is converted back to graphical coordinates.
  </enumerate>

  Whether a candidate is admissible is decided by <cpp|can_snap>, which maps
  the selection types onto the snapping categories shown in the
  <menu|Snap> menu and checks them against <cpp|snap_mode>:

  <descriptive-table|<tformat|<table|<row|<cell|Selection
  type>|<cell|Category in <cpp|snap_mode>>>|<row|<cell|<verbatim|point>,
  <verbatim|curve-handle>, <verbatim|text-handle>>|<cell|<verbatim|control
  point>>>|<row|<cell|<verbatim|curve-point>>|<cell|<verbatim|curve
  point>>>|<row|<cell|<verbatim|curve-point&curve-point>>|<cell|<verbatim|curve-curve
  intersection>>>|<row|<cell|<verbatim|grid-point>>|<cell|<verbatim|grid
  point>>>|<row|<cell|<verbatim|grid-curve-point>>|<cell|<verbatim|grid
  curve point>>>|<row|<cell|<verbatim|curve-point&grid-curve-point> (and
  reversed)>|<cell|<verbatim|curve-grid
  intersection>>>|<row|<cell|<verbatim|text>,
  <verbatim|group>>|<cell|<verbatim|text>>>|<row|<cell|<verbatim|text-border>>|<cell|<verbatim|text
  border>>>|<row|<cell|<verbatim|text-border-point>>|<cell|<verbatim|text
  border point>>>|<row|<cell|<verbatim|box>>|<cell|never>>|<row|<cell|<verbatim|free>>|<cell|always>>>>>

  The snap mode is a tuple of category names; the category
  <verbatim|all> allows everything, and a non-tuple value (no setting) also
  allows everything. On the <scheme> side
  (<verbatim|graphics/graphics-main.scm>) the mode is stored in the
  property <src-var|gr-snap> of the picture; <scm|graphics-get-snap-mode>
  returns the empty tuple in hand drawing mode, which disables snapping
  for hand drawings. The snap distance is <src-var|gr-snap-distance>
  (default <verbatim|10px>).

  <subsection|Snapping and selection are separate>

  Note that the selection used for snapping is not the selection used by
  <scheme> to decide which object is under the mouse. The latter is
  recomputed in <scheme> (<scm|graphics-select>, through the glued
  <scm|graphical-select>) at the <em|snapped> position, with the same
  snapping distance. The editor therefore usually finds the object whose
  control point the mouse was snapped to.

  <section|The graphical object>

  The <em|graphical object> is a tree which is not part of the document but
  is typeset and drawn on top of it. The <scheme> code uses it to show the
  control points of the object under the mouse, the selected objects in
  group mode, the selection rectangle, and above all the objects being
  edited: while an object is created or moved, it is removed from the
  document and only exists in the graphical object (see the sketch in the
  <hlink|next chapter|graphics-editor-scheme.en.tm>).

  <\explain>
    <cpp|void set_graphical_object (tree t)><explain-synopsis|set the
    overlay>
  <|explain>
    Stores <cpp|t> and typesets it immediately with
    <cpp|typeset_as_concat>, in the current typesetting environment but with
    the frame of the picture (<cpp|find_frame ()>). The result is a
    <cpp|composite_box> <cpp|go_box> with one child per non-empty box of
    the concatenation. Glued as <scm|set-graphical-object>; the <scheme>
    wrapper <scm|graphical-object!> takes an <scm|stree>.
  </explain>

  <\explain>
    <cpp|tree get_graphical_object ()><explain-synopsis|get the overlay>
  <|explain>
    Glued as <scm|get-graphical-object>.
  </explain>

  <\explain>
    <cpp|void invalidate_graphical_object ()><explain-synopsis|schedule a
    redraw>
  <|explain>
    Invalidates the ink rectangles of the children of <cpp|go_box>,
    intersected with the region of the picture. It has to be called before
    the overlay is changed (to erase the old one) and after (to draw the new
    one); <scm|graphics-decorations-update> and <cpp|mouse_graphics> do
    both. Glued as <scm|invalidate-graphical-object>.
  </explain>

  <\explain>
    <cpp|void draw_graphical_object (renderer ren)><explain-synopsis|draw
    the overlay>
  <|explain>
    Called from <cpp|draw_graphics>. It retypesets the graphical object if
    <cpp|go_box> is nil, installs a clipping rectangle for the picture and
    draws the children of <cpp|go_box>: point and curve boxes with
    <cpp|display>, other boxes (texts) with <cpp|redraw>.
  </explain>

  The overlay is typeset in the environment of the editor at the moment of
  the call, not in the environment of the picture: only the frame is taken
  from the picture. This is why the <scheme> code wraps the overlay in an
  explicit <markup|with> containing all relevant attributes
  (<scm|create-graphical-props>) and the magnification
  (<scm|get-local-magnify>).

  <section|Glue functions>

  The following functions are exported to <scheme> in
  <verbatim|Scheme/Glue/build-glue-editor.scm> and
  <verbatim|build-glue-basic.scm>:

  <descriptive-table|<tformat|<table|<row|<cell|<scheme>>|<cell|C++>>|<row|<cell|<scm|in-graphics?>>|<cell|<cpp|inside_graphics>>>|<row|<cell|<scm|get-graphical-x>,
  <scm|get-graphical-y>>|<cell|<cpp|get_x>,
  <cpp|get_y>>>|<row|<cell|<scm|get-graphical-pixel>>|<cell|<cpp|get_pixel>>>|<row|<cell|<scm|get-graphical-object>,
  <scm|set-graphical-object>>|<cell|<cpp|get_graphical_object>,
  <cpp|set_graphical_object>>>|<row|<cell|<scm|invalidate-graphical-object>>|<cell|<cpp|invalidate_graphical_object>>>|<row|<cell|<scm|graphical-select>>|<cell|<cpp|graphical_select
  (double, double)>>>|<row|<cell|<scm|graphical-select-area>>|<cell|<cpp|graphical_select
  (double, double, double, double)>>>|<row|<cell|<scm|graphics-set>,
  <scm|graphics-has?>, <scm|graphics-ref>>|<cell|<cpp|set_graphical_value>,
  <cpp|has_graphical_value>, <cpp|get_graphical_value>>>|<row|<cell|<scm|graphics-needs-update?>,
  <scm|graphics-notify-update>>|<cell|<cpp|graphics_needs_update>,
  <cpp|graphics_notify_update>>>>>>

  Conversely, the C++ code calls the following <scheme> functions and
  variables: <scm|graphics-move>, <scm|graphics-release-left>,
  <scm|graphics-release-middle>, <scm|graphics-release-right>,
  <scm|graphics-start-drag-left>, <scm|graphics-dragging-left>,
  <scm|graphics-end-drag-left>, <scm|graphics-start-drag-right>,
  <scm|graphics-dragging-right>, <scm|graphics-end-drag-right>,
  <scm|graphics-drop-object>, <scm|graphics-wheel>,
  <scm|set-keyboard-modifiers>, <scm|graphics-busy?>,
  <scm|graphics-reset-context>, <scm|graphics-exit-right>,
  <scm|graphics-texmacs-pointer>, <scm|graphics-undo-enabled>,
  <scm|graphics-get-snap-mode>, <scm|graphics-get-snap-distance>,
  <scm|graphics-copy>, <scm|graphics-cut>, <scm|graphics-paste> and
  <scm|graphics-notify-extents>. Most of them are declared with
  <scm|lazy-define> in <verbatim|init-texmacs.scm>, so that the graphics
  modules are only loaded when needed.

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
