<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The server classes and their connections>

  The server is the single object through which the rest of the program
  reaches the configuration, the windows and the <scheme> side of
  <TeXmacs>. This page describes its classes: the abstract interface
  <cpp|server_rep>, the two partial implementations <cpp|tm_config_rep>
  (fonts and keyboard) and <cpp|tm_frame_rep> (the current window), and
  the concrete <cpp|tm_server_rep>, together with the way the server is
  constructed and connected to <scheme> and to the editors. The buffers,
  views and windows which the server manages are described in the
  following pages; they are not members of the server but global tables
  of <source-link|Texmacs/Data/|src/Texmacs/Data>.

  <section|The class hierarchy>

  The server is split into an abstract interface, two partial
  implementations and a concrete class which combines them by multiple
  inheritance:

  <\cpp-code>
    class server_rep: public abstract_struct { ... }; \ \ \ \ \ // server.hpp

    class tm_config_rep: virtual public server_rep { ... }; // tm_config.hpp

    class tm_frame_rep: \ virtual public server_rep { ... }; // tm_frame.hpp

    class tm_server_rep: public tm_config_rep, \ \ \ \ \ \ \ \ \ // tm_server.hpp

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ public tm_frame_rep { ... };
  </cpp-code>

  The inheritance from <cpp|server_rep> is virtual, so that there is a
  single <cpp|server_rep> sub-object (and a single reference count) in a
  <cpp|tm_server_rep>. Each of the three implementation classes implements one
  group of the pure virtual methods of <cpp|server_rep>; the two partial
  classes are not meant to be instantiated on their own.

  <\explain>
    <cpp|class server_rep><explain-synopsis|the abstract server interface>
  <|explain>
    Declared in <source-link|Texmacs/server.hpp|src/Texmacs/server.hpp>. Apart from the
    constructor and destructor, all its methods are pure virtual. This
    includes <cpp|get_server ()>, which <cpp|tm_server_rep> implements by
    returning <cpp|this>, and which is used by code holding a
    <cpp|server> handle to obtain the raw pointer. They fall into
    three groups, which match the three implementation classes:

    <\description>
      <item*|Global parameters>Font rules and keyboard configuration;
      implemented by <cpp|tm_config_rep>.

      <item*|<TeXmacs> frames>Everything which concerns the <em|current>
      window; implemented by <cpp|tm_frame_rep>.

      <item*|Miscellaneous>Refreshing, the interpose and wait handlers,
      printing, zoom, typesetting invalidation, <cpp|quit>, <cpp|shell>;
      implemented by <cpp|tm_server_rep>.
    </description>

    The header also declares a few free functions which are not methods:
    <cpp|get_server>, <cpp|cpu_idle_time>, <cpp|menu_merge>,
    <cpp|quit_TeXmacs_code>, <cpp|gui_set_output_language>,
    <cpp|in_rescue_mode>, and the low level <cpp|create_buffer (url,
    tree)> and <cpp|new_buffer_in_this_window>.
  </explain>

  <\explain>
    <cpp|class server><explain-synopsis|the handle>
  <|explain>
    A hand written reference counted handle around <cpp|server_rep*> (it
    does not use the <cpp|ABSTRACT> macros because the destruction must go
    through a <cpp|dynamic_cast> to <cpp|tm_server_rep>, see
    <cpp|server_dec_count> in <source-link|tm_server.cpp|src/Texmacs/Server/tm_server.cpp>). The default
    constructor <cpp|server ()> creates a new <cpp|tm_server_rep>; the
    constructor from a <cpp|server_rep*> wraps an existing one.
  </explain>

  <section|The singleton and its construction>

  There is exactly one server. It is created as the local variable
  <cpp|server sv> in <cpp|TeXmacs_main> (<source-link|texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>) and lives
  until the GUI event loop terminates. The constructor of
  <cpp|tm_server_rep> stores a second handle in the global pointer

  <\cpp-code>
    server* the_server= NULL; \ \ // Server/tm_server.cpp
  </cpp-code>

  which is how the rest of the program reaches it:

  <\description>
    <item*|<cpp|server get_server ()>>Returns <cpp|*the_server> and asserts
    that the server has been started.

    <item*|<cpp|bool is_server_started ()>>Tests whether
    <cpp|the_server> is set. Code which may run before the server exists
    (very early errors, the wait handler, the crash handler) must test this
    first.
  </description>

  Because <cpp|the_server> holds a reference, the reference count of the
  server never drops to zero while the program runs; the server is not
  destroyed when <cpp|sv> goes out of scope, and <cpp|quit> leaves the
  process with <cpp|_exit> anyway (see below).

  The constructor of <cpp|tm_server_rep> is also the place where the
  <scheme> side of <TeXmacs> is booted:

  <\enumerate>
    <item>It registers itself in <cpp|the_server>, so that everything that
    follows may call <cpp|get_server ()>.

    <item><cpp|initialize_scheme ()> (in <source-link|Scheme/Guile/guile_tm.cpp|src/Scheme/Guile/guile_tm.cpp>
    or the corresponding file of the other <scheme> back-ends) evaluates a
    small bootstrap program, installs the <scheme> types for trees,
    <abbr|URL>s, observers, widgets, ... and calls <cpp|initialize_glue ()>
    (<source-link|Scheme/Scheme/glue.cpp|src/Scheme/Scheme/glue.cpp>), which in turn calls
    <cpp|initialize_glue_basic>, <cpp|initialize_glue_editor> and
    <cpp|initialize_glue_server>.

    <item>It installs <cpp|texmacs_interpose_handler> as the GUI interpose
    handler (<cpp|gui_interpose>) and <cpp|texmacs_wait_handler> as the
    wait handler (<cpp|set_wait_handler>).

    <item>It executes the <scheme> initialization file
    <cpp|tm_init_file> (by default
    <verbatim|$TEXMACS_PATH/progs/init-texmacs.scm>, overridden by the
    <verbatim|-i> option) and the user file <cpp|my_init_file>
    (<verbatim|$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm>). Loading
    <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> defines all modules, menus, keyboard
    bindings and so on; this is the bulk of the startup time.

    <item>If command line options produced <scheme> commands
    (<cpp|my_init_cmds>, for instance from <verbatim|-x>, <verbatim|-c> or
    <verbatim|-q>), they are wrapped in a <scm|begin> and scheduled with
    <cpp|exec_delayed>, so that they run once the event loop has started.
  </enumerate>

  <section|The connection to <scheme>>

  The server is connected to <scheme> in both directions.

  <paragraph|From <scheme> to the server.>The routines of
  <cpp|server_rep> are exported to <scheme> by the glue generator. The file
  <source-link|Scheme/Glue/build-glue-server.scm|src/Scheme/Glue/build-glue-server.scm> begins with

  <\scm-code>
    (build

    \ \ "get_server()-\<gtr\>"

    \ \ "initialize_glue_server"

    \ \ (insert-kbd-wildcard insert_kbd_wildcard (void string string bool bool bool))

    \ \ ...

    \ \ (window-get-serial get_window_serial (int))

    \ \ ...

    \ \ (show-header show_header (void bool))

    \ \ ...

    \ \ (quit-TeXmacs quit (void)))
  </scm-code>

  The first string is the prefix with which the generated <c++> code calls
  each routine, so that <scm|(show-header #f)> becomes
  <cpp|get_server()-\<gtr\>show_header (false)>; the second string is the
  name of the generated initialization function. The generated code is in
  <source-link|Scheme/Glue/glue_server.cpp|src/Scheme/Glue/glue_server.cpp>, which is included by
  <source-link|glue.cpp|src/Scheme/Scheme/glue.cpp>. In the same way, <source-link|build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>
  uses the prefix <verbatim|get_current_editor()-\<gtr\>>, and
  <source-link|build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm> exports free functions without a prefix;
  among those are all buffer, view, window and project routines of
  <source-link|Texmacs/Data/|src/Texmacs/Data> (the block starting with the comment
  <verbatim|;; buffers> in that file). The correspondence between the <scheme>
  names and the <c++> routines is listed in <hlink|the <scheme>
  interface|server-scheme.en.tm>.

  <paragraph|From the server to <scheme>.>The server calls into <scheme>
  with the functions of <source-link|Scheme/scheme.hpp|src/Scheme/scheme.hpp>:

  <\description>
    <item*|<cpp|call (string fun, args...)>>Synchronous call of a <scheme>
    function, used for queries whose result is needed immediately:
    <scm|kbd-find-key-binding> and <scm|kbd-get-command> (keyboard),
    <scm|menu-expand>, <scm|make-menu-widget>, <scm|make-menu-widget*> and <scm|cache-menu?>
    (menus), <scm|tmfs-title> (titles of <verbatim|tmfs://> buffers),
    <scm|buffer-initialize> (buffer notifiers), <scm|learn-interactive>
    (interactive commands), <scm|register-link-locations> and
    <scm|get-link-locations> (links), <scm|tree-export-encrypted>
    (encryption hook), <scm|quit-TeXmacs-scheme> and <scm|autosave-all>
    (exit and crash).

    <item*|<cpp|exec_delayed (object cmd)>>Asynchronous execution: the
    command is queued and executed by the event loop. This is used
    whenever the call originates from a GUI callback in which it would be
    unsafe to run arbitrary <scheme> code, for instance in
    <cpp|kill_window_command_rep::apply> (the close box of a window
    schedules <scm|(safely-kill-window <scm-arg|id>)>), in the dialog
    callbacks of <source-link|tm_dialogue.cpp|src/Texmacs/Window/tm_dialogue.cpp> and for the startup
    commands.

    <item*|<cpp|eval (string)> and <cpp|exec_file (url)>>Evaluation of a
    string or of a whole file; <cpp|exec_file> is used for the
    initialization files of the server and of new views, <cpp|eval> for
    <scm|(lazy-initialize-force)> before building menus and to obtain the
    value of a menu symbol.
  </description>

  <section|The connection to the editors>

  Editors are created by the view machinery, not by the server, but each
  of them points back to the server:

  <\cpp-code>
    class editor_rep: public simple_widget_rep {

    public:

    \ \ server_rep* \ sv; \ \ // the underlying texmacs server

    \ \ widget_rep* \ cvw; \ // non reference counted canvas widget

    \ \ tm_view_rep* mvw; \ // master view

    protected:

    \ \ tm_buffer \ \ \ buf; \ // the underlying buffer

    \ \ ...

    };

    editor new_editor (server_rep* sv, tm_buffer buf);
  </cpp-code>

  (<source-link|Edit/editor.hpp|src/Edit/editor.hpp>). <cpp|get_new_view> creates the editor with
  <cpp|new_editor (get_server () -\<gtr\> get_server (), buf)>, that is,
  with the raw <cpp|server_rep*> of the singleton. Through <cpp|sv>, the
  editor accesses the keyboard configuration (<cpp|sv-\<gtr\>get_keycomb>,
  <cpp|sv-\<gtr\>kbd_post_rewrite>, ...) and the window related services
  (footer, zoom, scrolling, menus). Since the latter act on the
  <em|current> window, an editor which needs them for its own window uses
  the <cpp|SERVER> macro, which makes the editor current for the duration
  of the call; see <hlink|working in the context of another
  view|server-views.en.tm>.

  Conversely, the server reaches the editors only through the views:

  <\itemize>
    <item><cpp|get_current_editor ()> (<source-link|new_view.cpp|src/Texmacs/Data/new_view.cpp>) returns
    the editor of the current view; it is the receiver of all routines of
    <source-link|build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>.

    <item><cpp|view_to_editor (url)> returns the editor of a given view.

    <item>Global operations loop over all views: <cpp|style_clear_cache>
    calls <cpp|init_style> on every editor, <cpp|typeset_update> and
    <cpp|typeset_update_all> call <cpp|typeset_invalidate> and
    <cpp|typeset_invalidate_all>, and the interpose handler calls
    <cpp|apply_changes> and <cpp|animate> on every editor attached to a
    window.
  </itemize>

  The editor also holds <cpp|cvw>, the canvas widget of its window (set by
  <cpp|attach_view>, and by <cpp|texmacs_input_widget> for embedded
  editors), and <cpp|mvw>, the <em|master view> of an embedded
  editor: for an editor inside a <cpp|texmacs_input_widget>, <cpp|mvw> is
  the view of the document which contains the widget, and is used to give
  the focus back to it when the embedded widget is closed.

  <section|The partial server <cpp|tm_config_rep>>

  <\explain>
    <cpp|class tm_config_rep><explain-synopsis|fonts and keyboard>
  <|explain>
    Declared in <source-link|Texmacs/tm_config.hpp|src/Texmacs/tm_config.hpp>, implemented in
    <source-link|Texmacs/Server/tm_config.cpp|src/Texmacs/Server/tm_config.cpp>. Its fields are

    <\description>
      <item*|<cpp|var_suffix>, <cpp|unvar_suffix>>The variant and
      inverse variant keys preceded by a space (by default
      <verbatim|" tab"> and <verbatim|" S-tab">), set by
      <scm|set-variant-keys>.

      <item*|<cpp|pre_kbd_wildcards>>Wildcards applied to key sequences
      when bindings are <em|defined> (<cpp|kbd_pre_rewrite>).

      <item*|<cpp|post_kbd_wildcards>>Wildcards applied when keys are
      <em|looked up> (<cpp|kbd_post_rewrite>, <cpp|get_keycomb>).

      <item*|<cpp|system_kbd_decode>>A table, filled on first use, which
      maps modifiers and special keys to their rendering in menus
      (<cpp|kbd_system_rewrite>); its contents depend on whether
      <name|macOS> fonts are used and on whether the GUI is <name|Qt>.
      <cpp|kbd_system_rewrite> also reads two preferences on every call:
      with the <name|macOS> <verbatim|look and feel>, <verbatim|A-<em|x>>
      is shown as <verbatim|escape <em|x>> (<cpp|kbd_system_prevails>), and
      <verbatim|case sensitive shortcuts> controls how letters are
      shown.
    </description>

    The bindings themselves are not stored in <c++>: <cpp|find_key_binding>
    and <cpp|kbd_get_command> call the <scheme> functions
    <scm|kbd-find-key-binding> and <scm|kbd-get-command>. The central
    method is <cpp|get_keycomb (which, status, cmd, shorth, help)>, called by
    the editor for each key press; it simplifies variant keys, applies the
    wildcards, looks the binding up and returns a status (0: no binding, 1:
    a command, 2: a shorthand string to insert, plus 3 when the key was
    reduced to a bare variant key). The details are in
    <hlink|keyboard configuration|server-events.en.tm>.

    <cpp|set_font_rules (rules)> passes a list of pairs to
    <cpp|font_rule>, which installs font substitution rules (see
    <hlink|the font database|font-database.en.tm>).
  </explain>

  <section|The partial server <cpp|tm_frame_rep>>

  <\explain>
    <cpp|class tm_frame_rep><explain-synopsis|the current window>
  <|explain>
    Declared in <source-link|Texmacs/tm_frame.hpp|src/Texmacs/tm_frame.hpp>, implemented in
    <source-link|Texmacs/Window/tm_frame.cpp|src/Texmacs/Window/tm_frame.cpp> and <source-link|Texmacs/Window/tm_dialogue.cpp|src/Texmacs/Window/tm_dialogue.cpp>.
    Almost every method forwards to the <cpp|tm_window_rep> of the current
    view, obtained with <cpp|concrete_window ()>, after checking
    <cpp|has_current_window ()>; the check is there so that <scheme> code
    may call these routines while no window is current (for instance from
    a background buffer). The exceptions are the canvas routines
    (<cpp|get_visible>, <cpp|set_scrollbars>, <cpp|scroll_where>,
    <cpp|scroll_to>, <cpp|get_extents>, <cpp|set_extents>) and
    <cpp|dialogue_start>, which do not check and crash if there is no
    current window. Its own fields are

    <\description>
      <item*|<cpp|full_screen>, <cpp|full_screen_edit>>The full screen
      state. <cpp|full_screen_mode (on, edit)> hides the header and footer
      when entering presentation mode (<cpp|on> without <cpp|edit>) and
      shows them in all other cases, including when leaving full screen,
      even if the user had hidden them before; it then calls <cpp|set_full_screen> on the window
      widget and <cpp|full_screen_mode> on the current editor (which
      sets a flag of the editor and invalidates the display). The state is stored in
      the server, not in the window, so it is global to the program.
      <cpp|in_presentation_mode ()> (<source-link|tm_server.cpp|src/Texmacs/Server/tm_server.cpp>) is a
      shortcut for <cpp|in_full_screen_mode ()>.

      <item*|<cpp|dialogue_win>, <cpp|dialogue_wid>>The single dialog
      window used by <cpp|dialogue_start>, <cpp|choose_file> and
      <cpp|interactive> commands which use a popup (see <hlink|dialogs and
      interactive commands|server-windows.en.tm>). As there is only
      one such slot, a second dialog is silently ignored while the first is
      open (<cpp|choose_file> then still moves the keyboard focus to the
      old dialog).
    </description>

    The methods are grouped as follows (the window side is described in
    <hlink|windows|server-windows.en.tm>):

    <\description>
      <item*|Properties><cpp|get_window_serial>, and
      <cpp|set_window_property> / <cpp|get_window_property> with typed
      variants. Properties are an arbitrary <cpp|tree> to <cpp|tree> table
      in the window. The serial number is used by <scheme> to keep per
      window data, for instance the cursor history in
      <source-link|utils/library/cursor.scm|TeXmacs/progs/utils/library/cursor.scm>.

      <item*|Menus and bars><cpp|menu_main>, <cpp|menu_icons>,
      <cpp|side_tools>, <cpp|bottom_tools> install a menu given by the
      name of a <scheme> symbol; <cpp|show_...> and <cpp|visible_...>
      toggle and query the visibility of the header, the four icon bars,
      the two side tool areas, the two bottom tool areas and the footer.
      <cpp|menu_widget> builds a menu widget without installing it.

      <item*|Canvas><cpp|set_window_zoom_factor> (clamped to
      <math|[0.04,25]> and normalized with <cpp|normal_zoom>),
      <cpp|get_visible>, <cpp|scroll_where>, <cpp|scroll_to>,
      <cpp|get_extents>, <cpp|set_extents>, <cpp|set_scrollbars>.

      <item*|Footer><cpp|set_left_footer>, <cpp|set_right_footer>, and
      <cpp|set_message> and <cpp|recall_message>, which are forwarded to
      the current <em|editor> (the editor decides what to show in the
      footer).

      <item*|Dialogs><cpp|dialogue_start>, <cpp|dialogue_inquire>,
      <cpp|dialogue_end>, <cpp|choose_file>, <cpp|interactive>.
    </description>
  </explain>

  <section|The concrete server <cpp|tm_server_rep>>

  <\explain>
    <cpp|class tm_server_rep><explain-synopsis|the server>
  <|explain>
    Declared in <source-link|Texmacs/tm_server.hpp|src/Texmacs/tm_server.hpp>, implemented in
    <source-link|Texmacs/Server/tm_server.cpp|src/Texmacs/Server/tm_server.cpp>. Its own fields are the default zoom
    factor <cpp|def_zoomf> and three fields of the idle monitor. The
    methods are:

    <\description-paragraphs>
      <item*|<cpp|interpose_handler ()>>Called by the GUI each time it is
      about to wait for events. It processes input from plug-in pipes and
      sockets (under <name|Qt>: <cpp|perform_select>, unless
      <verbatim|QTPIPES> is defined, and <cpp|process_all_pipes>; with the
      other toolkits: <cpp|perform_select> and
      <cpp|exec_pending_commands>), pending asynchronous evaluations
      (<cpp|async_eval_pending>), and, unless in headless mode, goes
      through the buffers and, for each buffer, calls <cpp|apply_changes>
      on the editors of all its views which are attached to a window and
      then <cpp|animate> on the same editors; it finishes with
      <cpp|windows_refresh ()>. It finally synchronizes the databases
      (<cpp|sync_databases>) and ticks the idle monitor. This is the heart
      of the repaint scheduling described in <hlink|the event loop and
      repaint scheduling|server-events.en.tm>.

      <item*|<cpp|idle_monitor_tick ()>, <cpp|cpu_idle_time ()>>At most
      once per second, compare the process CPU time with the elapsed
      time; if <TeXmacs> used less than half of a CPU, an idle counter is
      incremented, otherwise it is reset. <cpp|cpu_idle_time> returns the
      idle duration in milliseconds, so that background tasks can wait
      until the program is really idle.

      <item*|<cpp|wait_handler (message, arg)>>Shows a \Pplease wait\Q
      indicator in the current window, or prints the message on the
      console if there is no window yet.

      <item*|<cpp|refresh ()>>Clears the menu cache of every window
      (<cpp|tm_window_rep::refresh>), forcing menus and toolbars to be
      rebuilt; used for instance after a change of the output language
      (<cpp|gui_set_output_language>).

      <item*|<cpp|style_clear_cache ()>>Invalidates the style cache and
      re-initializes the style of every editor.

      <item*|<cpp|typeset_update (p)>, <cpp|typeset_update_all ()>,
      <cpp|inclusions_gc ()>>Invalidate the typesetting of a subtree or of
      all documents in all views; <cpp|inclusions_gc> also empties the
      cache of included documents.

      <item*|Printing>The printing command, the paper type and the
      resolution are stored in global variables of the printing code.

      <item*|<cpp|set_default_zoom_factor>,
      <cpp|get_default_zoom_factor>>The zoom factor used for new windows
      (clamped and normalized like the window zoom factor).

      <item*|<cpp|is_yes (s)>>Tests whether a user answer means \Pyes\Q in
      the current language (compares the first character with the
      translation of <verbatim|"yes">).

      <item*|<cpp|quit ()>>Closes all pipes to plug-ins, calls the
      <scheme> hook <scm|quit-TeXmacs-scheme> (which runs all the code
      registered with the <scm|on-exit> macro of
      <source-link|kernel/boot/boot.scm|TeXmacs/progs/kernel/boot/boot.scm>), clears the pending commands, destroys
      the <name|Qt> renderer objects and terminates the process with
      <cpp|_exit> (or <cpp|exit> in <verbatim|ADVANCED_DEVELOPER_MODE>).
      A comment in the code explains that <cpp|_exit> is used because
      destructing <name|Qt> objects at exit sometimes crashes.
      <cpp|quit_TeXmacs_code (code)> does the same with a given exit
      code.

      <item*|<cpp|shell (s)>>Runs a shell command.
    </description-paragraphs>
  </explain>

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
