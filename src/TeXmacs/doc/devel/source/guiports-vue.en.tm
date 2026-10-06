<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <name|Vue> port>

  <source-link|Plugins/Vue|src/Plugins/Vue> is an experimental port built on three libraries:
  <name|SDL3> for the windows, the events, the clipboard and the file
  dialogs (<source-link|vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp>); <name|Clay>, an immediate mode layout
  library (the single header <source-link|clay.h|src/Plugins/Vue/clay.h>, compiled once in
  <source-link|clay.c|src/Plugins/Vue/clay.c>), for the layout of the widgets
  (<source-link|vue_widget.cpp|src/Plugins/Vue/vue_widget.cpp>); and the <TeXmacs> renderer of
  <source-link|Plugins/MuPDF|src/Plugins/MuPDF>, or the GPU renderer of <source-link|vue_gpu.cpp|src/Plugins/Vue/vue_gpu.cpp>, for
  the drawing. The texts of the interface are typeset with <TeXmacs> fonts.
  It implements all the widget constructors of the abstract interface (see
  <hlink|the overview|guiports.en.tm>) and runs on <name|macOS>,
  <name|Linux> and, with limitations, <name|Windows>.

  The design notes are in <source-link|docs/vue-graphics-stack.md|docs/vue-graphics-stack.md> (windows,
  event loop, input, renderers), <source-link|docs/vue-widgets.md|docs/vue-widgets.md> and
  <source-link|docs/vue-testing.md|docs/vue-testing.md>, and the state of the port is kept in
  <source-link|Plugins/Vue/TODO|src/Plugins/Vue/TODO>. This page summarizes them.

  <section|Building>

  <verbatim|configure --with-gui=vue --with-mupdf=<em|prefix> --with-sdl3>
  defines <cpp|VUETEXMACS> and compiles the port with
  <verbatim|-std=c++20>; <name|MuPDF> is required. With
  <verbatim|--with-thorvg=<em|prefix>> (<name|ThorVG> built by
  <source-link|misc/thorvg/build-thorvg.sh|misc/thorvg/build-thorvg.sh>) the GPU renderer is compiled in
  (<cpp|USE_THORVG>). <name|CMake> builds the port with
  <verbatim|TEXMACS_GUI=Vue> (<name|SDL3>, <name|SDL3_ttf> and <name|MuPDF>
  required) but has no <name|ThorVG> option, so its build draws with
  <name|MuPDF> only, see <hlink|selecting and building a
  port|guiports-build.en.tm>. <source-link|clay.h|src/Plugins/Vue/clay.h> is upstream <name|Clay>,
  taken verbatim; <source-link|clay_renderer_SDL3.c|src/Plugins/Vue/clay_renderer_SDL3.c> is the example renderer of
  the library and is not compiled.

  <section|Windows>

  <cpp|vue_window_rep> (<source-link|vue_gui.hpp|src/Plugins/Vue/vue_gui.hpp>) is the abstract window.
  <cpp|vue_sdl_base_window_rep> holds what is <name|SDL> (creation,
  visibility, pixel density, size limits),
  <cpp|vue_sdl_mupdf_window_rep> adds the <name|MuPDF> drawing and
  <cpp|vue_sdl_gpu_window_rep> the <name|OpenGL> one; a variant drawing
  through the renderer of <name|SDL> (with <name|SDL3_ttf>) is kept but
  unused. Each window has its own <name|Clay> context and its own input
  state (<cpp|vue_input_state>); <cpp|with_window> makes them current,
  together with the <cpp|retina_factor> of the display the window is on.

  Popup windows (menus, balloons, tooltips) are borderless windows on top,
  sized to their contents and clamped to the screen; while one is visible
  it grabs the pointer, since <source-link|edit_mouse.cpp|src/Edit/Interface/edit_mouse.cpp> relies on the grab
  semantics of <name|X11>. A new window is shown once its contents fit,
  and dialogs are sized to their contents. The close button sends
  <cpp|SLOT_DESTROY>, which runs the quit command of the window.

  <paragraph|Single window mode.>With
  <verbatim|TEXMACS_VUE_SINGLE_WINDOW=1>, and always in the browser, only
  the first window is an <name|SDL> window, the <em|host>; the others
  (dialogs, tools, popups) are virtual windows with a layout context and an
  input state but no <name|SDL> window. The host draws them over its own
  contents (<cpp|composite_virtual_windows>), dialogs with a title bar and
  a frame to move and resize them, and routes the pointer and the keys to
  them (<cpp|route_pointer>, <cpp|route_keys>). In headless mode all
  windows are virtual.

  <section|The event loop>

  <cpp|gui_start_loop> runs <cpp|loop_iteration>, whose steps are: one
  <name|SDL> event is translated into the input state of its window (the
  motion and wheel events queued behind it are merged); every window is
  laid out, which is also where the input is dispatched, since in immediate
  mode a widget reads the input state while it lays itself out; the
  commands which the widgets queued during the layout are run (a widget
  never runs <TeXmacs> code while it is laid out); the interpose handler
  runs (<scheme> delayed commands, socket notifiers); the windows are laid
  out again if a widget was replaced; the editors repaint their backing
  stores (skipped while events are waiting, for at most 16<nbsp>ms); and
  the render commands of every window are replayed. Between iterations the
  loop sleeps in <cpp|SDL_WaitEventTimeout> for a pause which grows from
  10<nbsp>ms to 1<nbsp>s, capped at 40<nbsp>ms while socket notifiers are
  registered. The loop keeps running without windows as long as there are
  servers. A resize is handled inside the event pump of <name|SDL>, in an
  event watch which runs a whole frame for that window, so that the window
  never shows stale contents while it is dragged. In the browser, the
  iteration is called once per animation frame
  (<cpp|emscripten_set_main_loop>), and in headless mode
  <cpp|headless_loop> replaces the loop.

  <section|Layout and drawing>

  <name|Clay> works in device pixels, y downwards; widgets which need input
  or measurement name their elements with ids derived from the serial
  number of the widget. Sizes which <name|Clay> cannot express (the rows of
  <cpp|aligned_widget>, the largest page of a tab widget, ...) are measured
  on the bounding boxes of the previous pass, and a widget without them
  asks for another pass (<cpp|layout_again>), so that the first frame shown
  is already correct. <cpp|render_clay_commands> replays the render
  commands on a <TeXmacs> renderer; the editors and other custom elements
  draw themselves through render callbacks. Every colour of the interface
  is a field of <cpp|vue_theme>: a light and a dark theme ship, chosen by
  the preference <verbatim|gui theme> (<verbatim|default> follows the
  system). The icons are the <abbr|SVG> sets of
  <source-link|TeXmacs/misc/pixmaps/light|TeXmacs/misc/pixmaps/light> and <source-link|dark|TeXmacs/misc/pixmaps/dark>, drawn by
  <name|MuPDF> at the resolution of the window. Button highlights, tool
  panels and zooms are animated (transitions of <name|Clay>, smooth zoom of
  the editor).

  The main window draws its menu bar and icon bars itself (there is no
  native menu bar); contents which do not fit in a bar or a menu are
  clipped and marked rather than given a scroll bar. Pulldown menus are
  popup windows, chained through <cpp|current_popup>. The icon bars can be
  placed in columns at the left of the editor (preference <verbatim|icon
  bars>).

  <section|Keyboard and input methods>

  Each window starts the text input of <name|SDL> (<cpp|SDL_StartTextInput>).
  A key which produces a character is not delivered as a key: the text
  which the system composes with the dead keys and the input method comes
  as <verbatim|SDL_EVENT_TEXT_INPUT>, and that is what the editor receives;
  every other key (<verbatim|return>, arrows, <verbatim|C->,
  <verbatim|M->, <verbatim|A-> combinations) is delivered as a key named by
  <cpp|lookup_key> after the <TeXmacs> conventions. A text event which
  follows a key with a modifier within 30<nbsp>ms belongs to the same
  keystroke and is dropped. With a command modifier, a key of a non Latin
  layout is named by its US key. A composition
  (<verbatim|SDL_EVENT_TEXT_EDITING>) is shown as a pre-edit: the editor
  receives <verbatim|"pre-edit:<em|pos>:<em|text>"> as in <name|Qt>, and
  the text inputs of the dialogs show it too. The focused editor tells
  <name|SDL> where its cursor is (<cpp|SDL_SetTextInputArea>), so that
  candidate windows are placed next to it. The keyboard focus is kept per
  window (<cpp|set_kbd_focus>).

  The mouse follows the <name|Qt> port (on <name|macOS>, control and option
  emulate the right and middle buttons). A trackpad swipe drags the view,
  a wheel notch is travelled over a few frames, and control (command on
  <name|macOS>) with the wheel zooms. Files and texts dropped on a window
  reach the widget under the pointer as a <verbatim|"drop"> mouse event.

  <section|Clipboards, dialogs and printing>

  <paragraph|Clipboards.>The selection <verbatim|"primary"> is the system
  clipboard (<cpp|SDL_SetClipboardData>), with the types
  <verbatim|application/x-texmacs-clipboard>, <verbatim|text/html> and
  plain text (the verbatim version <cpp|sv>); images copied with \PCopy to
  Image\Q are published as <verbatim|image/png>. Pasting takes, in this
  order, a <TeXmacs> snippet, a <abbr|PNG> image, <name|HTML> and plain
  text. The other selections are internal; there is no <name|X11>
  <verbatim|PRIMARY> selection.

  <paragraph|Dialogs.>The file chooser uses the native dialogs of
  <name|SDL> (<cpp|SDL_ShowFileDialogWithProperties>), whose result comes
  back as an event; in the browser, the file input of the page and a
  download replace them. The other dialogs (color picker, printer, prompts,
  tools) are <name|Clay> widgets. <scm|vue-gui?> holds, <scm|qt-gui?> and
  <scm|x-gui?> do not, and most of the <scheme> features which test
  <scm|qt-gui?> also accept <scm|vue-gui?> (print dialog, \PCopy to
  Image\Q, preferences).

  <paragraph|Printing.>The printer dialog lists the printers with
  <verbatim|lpstat> and their options with <verbatim|lpoptions>, and prints
  the file typeset by <TeXmacs> with <verbatim|lpr> and <name|CUPS> options
  (copies, pages, orientation, paper size, two-sided, color).

  <section|Rendering>

  <paragraph|<name|MuPDF>.><cpp|mupdf_renderer_rep>
  (<source-link|Plugins/MuPDF/mupdf_renderer.cpp|src/Plugins/MuPDF/mupdf_renderer.cpp>) draws through a <abbr|PDF> run
  processor on a pixmap. Each editor keeps an opaque backing store which
  is repainted incrementally; a scroll shifts its pixels and repaints the
  exposed strips; plain fills and 1:1 blits write the pixels directly. The
  window is a pixmap copied to the <name|SDL> window surface.

  <paragraph|GPU.>When the port is built with <name|ThorVG>, the windows are
  drawn with <name|OpenGL> (<name|WebGL2> in the browser) unless
  <verbatim|TEXMACS_VUE_GPU=0> is set or no <name|OpenGL> context can be
  made, in which case <name|MuPDF> draws as above. One context serves all
  windows; the backing stores of the editors are textures with a
  framebuffer; text and fills are textured quads in one batch, the glyphs
  being rendered by <name|MuPDF> into an atlas (or, with
  <verbatim|TEXMACS_VUE_SLUG=1>, drawn by the fragment shader from their
  outlines); other vector graphics go to the <name|OpenGL> engine of
  <name|ThorVG>. A window presents a frame only when it differs from the
  last one presented.

  <section|Environment variables>

  <descriptive-table|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-hpart|2>|<cwith|1|-1|2|2|cell-hpart|3>|<table|<row|<cell|Variable>|<cell|Effect>>|<row|<cell|<verbatim|TEXMACS_VUE_GPU>>|<cell|<verbatim|0>
  draws with <name|MuPDF> in a build with <name|ThorVG>>>|<row|<cell|<verbatim|TEXMACS_VUE_GPU_SYNC>>|<cell|wait
  for the GPU at the end of a repaint (profiling)>>|<row|<cell|<verbatim|TEXMACS_VUE_SLUG>>|<cell|glyphs
  drawn from their outlines on the GPU>>|<row|<cell|<verbatim|TEXMACS_VUE_SINGLE_WINDOW>>|<cell|single
  window mode on the desktop>>|<row|<cell|<verbatim|TEXMACS_VUE_SCRIPT>>|<cell|replay
  a test script>>|<row|<cell|<verbatim|TEXMACS_VUE_SNAPSHOT>>|<cell|directory
  in which every redraw of a window is saved as <abbr|PNG>>>|<row|<cell|<verbatim|TEXMACS_VUE_THEME>>|<cell|<verbatim|light>
  or <verbatim|dark>, instead of the preference>>|<row|<cell|<verbatim|TEXMACS_VUE_DENSITY>>|<cell|pixel
  density used instead of that of the display>>|<row|<cell|<verbatim|TEXMACS_VUE_TAB_MODE>>|<cell|presentation
  of the responsive tabs: <verbatim|top>, <verbatim|side>,
  <verbatim|mobile> or <verbatim|grid>>>|<row|<cell|<verbatim|TEXMACS_VUE_BARS>>|<cell|icon
  bars at the <verbatim|top> or on the <verbatim|left>>>|<row|<cell|<verbatim|TEXMACS_VUE_RADIUS>>|<cell|rounding
  of the corners of the theme (<verbatim|0>: square)>>|<row|<cell|<verbatim|TEXMACS_VUE_PROFILE>>|<cell|print
  timings every so many frames>>|<row|<cell|<verbatim|TEXMACS_VUE_DUMP>>|<cell|print
  every render command of <name|Clay>>>|<row|<cell|<verbatim|TEXMACS_VUE_CLAY_DEBUG>>|<cell|F1
  opens the debug view of <name|Clay> (also with
  <verbatim|-debug-qt>)>>>>>

  <section|Scripted tests>

  Since other programs may not capture or drive the windows of <TeXmacs>
  on <name|macOS>, the port has its own test driver: with
  <verbatim|TEXMACS_VUE_SCRIPT=<em|file>>, the loop executes one command of
  the file per iteration, by pushing synthetic <name|SDL> events
  (<verbatim|wait>, <verbatim|window>, <verbatim|move>, <verbatim|click>,
  <verbatim|wheel>, <verbatim|key>, <verbatim|text>, <verbatim|compose>,
  <verbatim|commit>, <verbatim|drop>, <verbatim|resize>,
  <verbatim|scheme>, <verbatim|snapshot>, <verbatim|close>, ...; see
  <cpp|script_step> in <source-link|vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp>). The directory
  <source-link|Plugins/Vue/tests|src/Plugins/Vue/tests> holds about 57 scripts, most with a
  <scheme> file which builds the widget they drive, and
  <source-link|run.sh|src/Plugins/Vue/tests/run.sh>, which runs one of them in a copy of the
  home directory (a test must never change the preferences of the user)
  and checks what the callbacks print. The catalogue of the tests is in
  <source-link|docs/vue-testing.md|docs/vue-testing.md>.

  <section|The browser>

  A <name|WebAssembly> build of the port (<name|Emscripten>) is developed
  on the branch <verbatim|wip_wasm_vue>; this tree has neither its build
  files nor its documentation, only the code of the port which is compiled
  under <cpp|__EMSCRIPTEN__>: single window mode always on, one window
  which fills the page (<verbatim|SDL_WINDOW_FILL_DOCUMENT>), the loop
  driven by <cpp|emscripten_set_main_loop>, file dialogs through the page,
  the <name|Fira> fonts for the interface, <name|WebGL2> in the GPU
  renderer, and no <name|SDL3_ttf>. Headless mode, which opens no display,
  makes the build testable under <name|node>.

  <section|What is missing>

  From <source-link|TODO|src/Plugins/Vue/TODO> and the design notes:

  <\itemize>
    <item>the color picker offers named colours only (the color menus
    route on <scm|qt-gui?> and use the <scheme> picker instead);

    <item>no image preview in the file chooser;

    <item>on <name|Windows>, <cpp|perform_select> and the pipes are stubs:
    no plug-ins and no client/server; printing uses <name|CUPS> commands;

    <item>on <name|Linux>, no <verbatim|PRIMARY> selection, and the
    interface is drawn in the <TeX> fonts rather than a desktop font;

    <item>a few colours are still literals and stay light in the dark
    theme;

    <item>popup windows (menus) cannot be animated, being separate
    <name|SDL> windows;

    <item>on the GPU, <cpp|draw_spacial> and transformed glyphs are
    untested.
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
