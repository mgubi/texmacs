<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The event loop, keyboard and mouse, and repaint scheduling>

  This page follows a user event from the toolkit to the screen. The
  central idea is that <TeXmacs> never reacts to an event by redrawing:
  events are queued, the editor updates the document and records
  <em|what> changed in a bit set, and a single routine,
  <cpp|apply_changes>, is later called from the <em|interpose handler> of
  the server to retypeset, recompute the cursor and selections, update the
  menus and invalidate screen regions. The toolkit then repaints the
  invalid regions. Schematically, for the <name|Qt> port:

  <\verbatim-code>
    Qt event (QTMWidget)

    \ \ -\<gtr\> qt_gui_rep::process_keypress / process_mouse / ...\ \ \ (queued_event)

    \ \ -\<gtr\> qt_gui_rep::update \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (single shot timer)

    \ \ \ \ \ \ \|- process_delayed_commands \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (exec_delayed)

    \ \ \ \ \ \ \|- process_queued_events

    \ \ \ \ \ \ \|\ \ \ \ \ -\<gtr\> editor::handle_keypress / handle_mouse

    \ \ \ \ \ \ \|\ \ \ \ \ \ \ \ \ \ -\<gtr\> Scheme keyboard-press / mouse-event

    \ \ \ \ \ \ \|\ \ \ \ \ \ \ \ \ \ -\<gtr\> modifications, notify_change (flags)

    \ \ \ \ \ \ \|- interpose handler \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (tm_server_rep)

    \ \ \ \ \ \ \|\ \ \ \ \ -\<gtr\> apply_changes on every view shown in a window

    \ \ \ \ \ \ \|\ \ \ \ \ -\<gtr\> windows_refresh

    \ \ \ \ \ \ \|- repaint_all \ -\<gtr\> editor::handle_repaint
  </verbatim-code>

  The last two sections describe the keyboard configuration kept by the
  server, which is consulted for every key press, and the user
  preferences. The toolkit side of the event queue is described in more
  detail in <hlink|the <name|Qt> implementation|widgets-qt.en.tm>.

  <section|The GUI loop and the interpose handler>

  With the <name|Qt> back-end (<source-link|Plugins/Qt/qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>, and its
  counterpart in <source-link|Plugins/Qt6/|src/Plugins/Qt6>), events delivered by <name|Qt> to
  <TeXmacs> widgets are not processed immediately. The widgets call
  <cpp|qt_gui_rep::process_keypress>, <cpp|process_mouse>,
  <cpp|process_keyboard_focus>, <cpp|process_resize> or
  <cpp|process_command>, which append a <cpp|queued_event> to a private
  queue with <cpp|add_event> and request an update. The update is performed
  by <cpp|qt_gui_rep::update>, which is triggered by a single shot timer,
  and roughly does the following:

  <\enumerate>
    <item>execute the delayed <scheme> commands whose time has come
    (<cpp|process_delayed_commands>);

    <item>process the queued events with <cpp|process_queued_events>, which
    calls <cpp|handle_keypress>, <cpp|handle_mouse>,
    <cpp|handle_keyboard_focus> or <cpp|handle_notify_resize> on the target
    widget (for the canvas of a document, this widget is the editor);

    <item>call the <em|interpose handler> and then repaint the invalid
    regions of all widgets (<cpp|qt_simple_widget_rep::repaint_all ()>);
    when only ordinary keys were typed during this round, this step is
    postponed for a few milliseconds so that fast typing is not slowed down
    by the updates;

    <item>restart the timer, immediately if events are still pending,
    otherwise after a short delay or when the next delayed command is due.
  </enumerate>

  The interpose handler was installed by the server constructor with
  <cpp|gui_interpose (texmacs_interpose_handler)>; it calls
  <cpp|tm_server_rep::interpose_handler>:

  <\cpp-code>
    void

    tm_server_rep::interpose_handler () {

    \ \ ... \ // communication with plug-ins, pending commands

    \ \ async_eval_pending ();

    \ \ if (!headless_mode) {

    \ \ \ \ for (i=0; i\<less\>N(bufs); i++) {

    \ \ \ \ \ \ tm_buffer buf= (tm_buffer) bufs[i];

    \ \ \ \ \ \ for (j=0; j\<less\>N(buf-\<gtr\>vws); j++) {

    \ \ \ \ \ \ \ \ tm_view vw= (tm_view) buf-\<gtr\>vws[j];

    \ \ \ \ \ \ \ \ if (vw-\<gtr\>win != NULL) vw-\<gtr\>ed-\<gtr\>apply_changes ();

    \ \ \ \ \ \ }

    \ \ \ \ \ \ ... \ // same loop calling animate ()

    \ \ \ \ }

    \ \ \ \ windows_refresh ();

    \ \ }

    \ \ sync_databases ();

    \ \ idle_monitor_tick ();

    }
  </cpp-code>

  Hence <em|only active views> (views displayed in a window) are updated and
  retypeset in the background; passive views are typeset on demand, for
  instance when an environment value is queried. <cpp|windows_refresh>
  sends refresh requests to the widgets of all windows, which in particular
  updates dynamic menus and widgets; it is throttled with
  <cpp|windows_delayed_refresh>. The idle monitor keeps track of the CPU
  usage in order to implement <cpp|cpu_idle_time>.

  <section|Keyboard events><label|sec-keyboard>

  A key press is processed by the editor as follows.

  <\enumerate>
    <item><cpp|edit_interface_rep::handle_keypress (key, t)>
    (<source-link|Edit/Interface/edit_keyboard.cpp|src/Edit/Interface/edit_keyboard.cpp>) records the key for the
    optional display of typed keys, forces a first typesetting if needed,
    calls <cpp|start_editing ()>, and passes the key to the <scheme>
    function <scm|keyboard-press> (or <scm|delayed-keyboard-press> for
    pre-edit strings of input methods).

    <item><scm|keyboard-press> is defined with <scm|tm-define> in
    <source-link|kernel/gui/kbd-handlers.scm|TeXmacs/progs/kernel/gui/kbd-handlers.scm>; its default implementation calls
    <scm|(key-press <scm-arg|key>)>. It is overloaded in several contexts,
    for instance inside input fields of widgets
    (<source-link|utils/misc/gui-utils.scm|TeXmacs/progs/utils/misc/gui-utils.scm>), during interactive spell checking
    or in the shortcut editor.

    <item><scm|key-press> is the glue for <cpp|edit_interface_rep::key_press>.
    This routine handles input method and speech input, then tries to
    interpret the key, preceded by the keys of a pending shortcut (if any),
    as a shortcut with <cpp|try_shortcut>. If this fails, the key is
    inserted as text through the <scheme> function <scm|kbd-insert>
    (default: <scm|insert>), after a call to <cpp|archive_state ()>.

    <item><cpp|try_shortcut> asks the server for the binding with
    <cpp|sv-\<gtr\>get_keycomb> (see below). If the
    key sequence is bound, the pending changes are enclosed in a new undo
    marker (<cpp|mark_start>), the help text of the shortcut is displayed in
    the footer, and either the bound command is executed or the bound string
    is inserted with <scm|kbd-insert>. When the next key extends the
    sequence to a longer shortcut, <cpp|mark_cancel> undoes the effect of
    the prefix before the longer shortcut is applied; this is how, for
    instance, repeated presses of a variant key cycle through symbols.

    <item>Back in <cpp|handle_keypress>, the focus loci are updated,
    <cpp|notify_change (THE_DECORATIONS)> is called and <cpp|end_editing
    ()> confirms the undo step. If an exception is raised during the
    processing, the changes are cancelled with <cpp|cancel_editing>.
  </enumerate>

  Keyboard focus changes are handled by <cpp|handle_keyboard_focus>, which
  updates <cpp|got_focus>, makes the view current when it obtains the focus
  and calls the <scheme> hook <scm|keyboard-focus>.

  <section|Mouse events>

  <cpp|edit_interface_rep::handle_mouse (kind, x, y, mods, t, data)>
  (<source-link|Edit/Interface/edit_mouse.cpp|src/Edit/Interface/edit_mouse.cpp>) first makes sure that the
  document is typeset (the box tree is needed to interpret the coordinates),
  calls <cpp|start_editing>, converts the coordinates according to the
  magnification, detects the start of left and right drags, and passes the
  event to the <scheme> function <scm|mouse-event>. Its default definition
  in <source-link|kbd-handlers.scm|TeXmacs/progs/kernel/gui/kbd-handlers.scm> calls the glue routine <scm|mouse-any>,
  that is, <cpp|edit_interface_rep::mouse_any>. The latter updates the loci
  under the mouse (hyperlinks, tooltips), dispatches to the graphics editor
  when the pointer is inside a <markup|graphics>, and otherwise calls
  <cpp|mouse_click>, <cpp|mouse_drag>, <cpp|mouse_select>,
  <cpp|mouse_extra_click>, <cpp|mouse_paste>, <cpp|mouse_adjust> or
  <cpp|mouse_scroll> depending on the kind of event. Drop events are passed
  to <scm|mouse-drop-event>. As for keyboard events, the handler ends with
  <cpp|end_editing ()>.

  <section|Change notification and <cpp|apply_changes>><label|sec-repaint>

  Editing routines never redraw the screen directly. Instead, they call
  <cpp|notify_change (int flags)>, which adds the flags to
  <cpp|env_change> and asks the GUI for an update (<cpp|needs_update>).
  The flags are defined in <source-link|editor.hpp|src/Edit/editor.hpp>:

  <\description>
    <item*|<cpp|THE_TREE>>The document tree changed; the document has to be
    retypeset.

    <item*|<cpp|THE_ENVIRONMENT>>The initial environment or the style
    changed; everything has to be retypeset.

    <item*|<cpp|THE_CURSOR>, <cpp|THE_SELECTION>, <cpp|THE_FOCUS>>The
    cursor, the selection or the keyboard focus changed.

    <item*|<cpp|THE_EXTENTS>>The size of the document or of the window
    changed.

    <item*|<cpp|THE_DECORATIONS>>Menus, toolbars and footer might have
    changed.

    <item*|<cpp|THE_LOCUS>>The active loci (hyperlinks, ...) have to be
    redrawn.

    <item*|<cpp|THE_MENUS>>Force an update of the menus.

    <item*|<cpp|THE_FREEZE>>Do not scroll to make the cursor visible.

    <item*|<cpp|THE_TOOLTIP>, <cpp|THE_SPELL_ERRORS>>Tooltips and
    highlighted spelling errors.
  </description>

  <cpp|apply_changes> (<source-link|Edit/Interface/edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>),
  called by the interpose handler, processes the accumulated flags:

  <\enumerate>
    <item>If nothing changed, it only updates the menus, toolbars and footer
    with <cpp|update_menus ()>, provided that the document changed since the
    last such update and that the user has been idle for at least 1/6 of a
    second (<cpp|idle_time>).

    <item>It adapts the environment variables which depend on the window:
    the zoom factor, the page size for the <verbatim|automatic> page medium,
    scroll bars and the visibility of window bars.

    <item>On <cpp|THE_ENVIRONMENT> it invalidates the whole typesetting; on
    <cpp|THE_TREE> or <cpp|THE_ENVIRONMENT> it retypesets the invalid
    parts (<cpp|typeset>) and invalidates the corresponding screen
    rectangles.

    <item>On changes of the tree, environment or extents it recomputes the
    extents of the document and passes them to the window.

    <item>On changes of the cursor, selection or focus it recomputes the
    graphical cursor, scrolls to make it visible (unless
    <cpp|THE_FREEZE> is set), and recomputes the rectangles which
    highlight the context, focus and semantic selection.

    <item>It recomputes the rectangles of the selection and of the
    alternative selections, triggers continuous spell checking, updates the
    loci under the mouse and the focus loci (calling the <scheme> function
    <scm|link-follow-ids>), and updates the menus if <cpp|THE_MENUS> is set.

    <item>Finally, it resets <cpp|env_change> and records the time of the
    change in <cpp|last_change>.
  </enumerate>

  All drawing is done by invalidating rectangles (<cpp|invalidate>). The
  GUI then calls <cpp|handle_repaint (renderer, x1, y1, x2, y2)>
  (<source-link|Edit/Interface/edit_repaint.cpp|src/Edit/Interface/edit_repaint.cpp>), which draws the background,
  the typeset boxes, the selections, the cursor and the other decorations
  into the renderer, using a \Pstored\Q or \Pshadow\Q renderer as a
  cache. <cpp|handle_repaint> expects <cpp|env_change> to be zero: all
  changes must have been applied before the screen is repainted.

  <cpp|update_menus ()> rebuilds the main menu, the icon bars and the side
  and bottom tools (through the <cpp|SERVER> macro, see
  <hlink|menus and toolbars|server-windows.en.tm>), updates the footer
  (<cpp|set_footer>), updates the \Pmodified\Q indicator of all windows on
  the buffer, updates the <abbr|DRD>, and saves the user preferences if they
  were modified. The <scheme> routine <scm|update-menus> calls it directly.

  <section|Keyboard configuration><label|sec-config>

  Key bindings are defined in <scheme> with the <scm|kbd-map> macro of
  <source-link|kernel/gui/kbd-define.scm|TeXmacs/progs/kernel/gui/kbd-define.scm>, possibly conditioned on modes and
  contexts, and looked up with <scm|kbd-find-key-binding>. The
  <cpp|tm_config_rep> part of the server implements the lookup of a key
  sequence entered by the user in <cpp|get_keycomb (which, status, cmd,
  shorth, help)>:

  <\enumerate>
    <item>The sequence is simplified with respect to the <em|variant keys>
    (<cpp|variant_simplification>). By default the variant key is
    <verbatim|tab> and the reverse variant key is <verbatim|S-tab>; they
    can be changed with <cpp|set_variant_keys> (<scheme>:
    <scm|set-variant-keys>). Trailing variant keys which do not lead to a
    binding are dropped, and a reverse variant key removes the last variant.

    <item>The <em|post wildcards> are applied (<cpp|apply_wildcards>).
    Wildcards are rewriting rules on key sequences, declared in <scheme>
    with <scm|kbd-wildcards> and registered with
    <cpp|insert_kbd_wildcard> (<scheme>: <scm|insert-kbd-wildcard>). Pre
    wildcards are applied to the keys in the definitions
    (<cpp|kbd_pre_rewrite>), post wildcards at lookup time
    (<cpp|kbd_post_rewrite>).

    <item>The binding is looked up with the <scheme> function
    <scm|kbd-find-key-binding> (<cpp|find_key_binding>). The result
    determines <cpp|status>: 0 if there is no binding, 1 if the binding is
    a command (returned in <cpp|cmd>), 2 if it is a string to be inserted
    (returned in <cpp|shorth>). In the last two cases <cpp|help> contains
    the help text of the binding. The status is increased by 3 if the
    sequence was reduced to a bare variant key.
  </enumerate>

  <cpp|kbd_system_rewrite> translates a key sequence into a tree for
  displaying shortcuts in menus and in the footer, using system specific
  names or symbols for the modifiers and special keys (for instance the
  <name|macOS> modifier symbols). <cpp|kbd_get_command> looks up named
  commands through the <scheme> function <scm|kbd-get-command>.
  <cpp|set_font_rules> installs font substitution rules.

  <section|Preferences>

  User preferences are stored as pairs of strings in
  <verbatim|$TEXMACS_HOME_PATH/system/preferences.scm>. At the <c++> level,
  <cpp|load_user_preferences>, <cpp|save_user_preferences>,
  <cpp|get_user_preference> and <cpp|set_user_preference>
  (<source-link|System/Boot/preferences.cpp|src/System/Boot/preferences.cpp>) access this file directly; they
  are used during startup, before <scheme> is available. Afterwards, code
  should use <cpp|get_preference (var, def)> and <cpp|set_preference (var,
  val)> (<source-link|Scheme/Scheme/object.cpp|src/Scheme/Scheme/object.cpp>), which call the <scheme>
  functions <scm|get-preference> and <scm|set-preference> once the
  preferences have been booted. On the <scheme> side
  (<source-link|kernel/texmacs/tm-preferences.scm|TeXmacs/progs/kernel/texmacs/tm-preferences.scm>), preferences are declared
  with default values and call-back functions using
  <scm|define-preferences>; the call-back is invoked when the preference
  changes (<scm|notify-preference>). Examples can be found in
  <source-link|texmacs/texmacs/tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>. Modified preferences are written
  back to disk by <cpp|save_user_preferences>, which is called from
  <cpp|update_menus>.

  Several preferences are read by the code discussed in this document:
  window decorations in <cpp|new_window>, <verbatim|show full context>,
  <verbatim|show table cells>, <verbatim|show focus> and <verbatim|show only
  semantic focus> in <cpp|apply_changes>, <verbatim|look and feel> for the
  clipboard and keyboard conventions, and <verbatim|case sensitive
  shortcuts> in <cpp|kbd_system_rewrite>.

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
