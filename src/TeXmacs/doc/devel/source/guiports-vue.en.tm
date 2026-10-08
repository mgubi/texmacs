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
  required), with the GPU renderer when <verbatim|THORVG_DIR> names the
  prefix of a <name|ThorVG> build and with <name|MuPDF> only otherwise,
  see <hlink|selecting and building a port|guiports-build.en.tm>. <source-link|clay.h|src/Plugins/Vue/clay.h> is upstream <name|Clay>,
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
  density used instead of that of the display>>|<row|<cell|<verbatim|TEXMACS_VUE_SCALE>>|<cell|interface
  scaling used instead of the preference <verbatim|gui scaling> (the
  drawing factor of a window is its density times this scaling, rounded to
  an integer of at least 1: <cpp|vue_drawing_factor>)>>|<row|<cell|<verbatim|TEXMACS_VUE_TAB_MODE>>|<cell|presentation
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

  <TeXmacs> also runs in a web page: a <name|WebAssembly> build of this port
  with <name|Emscripten>, on <scheme> <name|S7> (the collector of
  <name|Guile> scans the C stack, which <name|WebAssembly> does not expose).
  It lives in this tree (it was developed on the branch
  <verbatim|wip_wasm_vue> until it was merged into
  <verbatim|maxs_texmacs>). The design notes, measurements and the state of
  the port are in <source-link|docs/wasm/README.md|docs/wasm/README.md>
  (with <source-link|tikzjax.md|docs/wasm/tikzjax.md> and
  <source-link|asymptote.md|docs/wasm/asymptote.md> for two of the plug-ins);
  what the user sees is described in the help page
  <source-link|texmacs-vue.en.tm|TeXmacs/doc/about/welcome/texmacs-vue.en.tm>
  (<menu|Help|TeXmacs in the browser>, in the browser only), whose recent changes list every
  feature and fix of the browser version.

  <subsection|Building>

  The build uses neither <verbatim|configure> nor <name|CMake>: everything is
  in <source-link|misc/wasm|misc/wasm>, run from <verbatim|src>:

  <\verbatim>
    . misc/wasm/emenv.sh build-wasm \ \ \ \ \ # the Emscripten environment

    sh misc/wasm/build-mupdf.sh \ \ \ \ \ \ \ \ \ # the slim MuPDF, once

    make -C build-wasm -f ../misc/wasm/Makefile -j8 web \ # the page

    make -C build-wasm -f ../misc/wasm/Makefile -j8 node # node, headless

    node misc/wasm/serve.mjs \ \ \ \ \ \ \ \ \ \ \ \ # http://localhost:8080/texmacs.html
  </verbatim>

  <source-link|Makefile|misc/wasm/Makefile> compiles the sources listed in
  <source-link|sources.txt|misc/wasm/sources.txt> (those of a desktop
  <name|Vue>+<name|S7> build without the <name|Objective-C>; written again by
  <source-link|list-sources.sh|misc/wasm/list-sources.sh>) with
  <source-link|config.h|misc/wasm/config.h> and
  <source-link|tm_configure.hpp|misc/wasm/tm_configure.hpp> in place of what
  <verbatim|configure> writes. <name|SDL3> is the port of <name|Emscripten>
  (<verbatim|-sUSE_SDL=3>); <name|MuPDF> is a slim build without fonts of its
  own, patched (<source-link|build-mupdf.sh|misc/wasm/build-mupdf.sh>,
  <source-link|mupdf-subset-cff.patch|misc/wasm/mupdf-subset-cff.patch>);
  <name|SDL3_ttf> is not linked; <name|Hunspell> is compiled in for the
  spelling (<source-link|get-hunspell.sh|misc/wasm/get-hunspell.sh>). The
  exceptions and <cpp|setjmp>/<cpp|longjmp> are those of <name|WebAssembly>
  (<verbatim|-fwasm-exceptions>), as in <name|MuPDF>, which rules out
  <verbatim|ASYNCIFY>: the loop cannot block, and gives control back to the
  browser every frame. The files of the directory <verbatim|TeXmacs> are written as
  packages with a manifest by <source-link|package.py|misc/wasm/package.py>,
  the files read at startup (<source-link|boot-files.txt|misc/wasm/boot-files.txt>)
  in the boot package.

  <subsection|In the code of the port>

  What the code of the port compiles under <cpp|__EMSCRIPTEN__> (mostly in
  <source-link|vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp>) does: single window
  mode always on, the windows of the editors being tabs of the page; the
  loop driven by <cpp|emscripten_set_main_loop>, one
  <cpp|loop_iteration> per frame, the keys queued in a frame all handled in
  it (<cpp|web_more_events>); file dialogs through the page; the
  <name|Fira> fonts for the interface; <name|WebGL2> in the GPU renderer;
  the tabs sent to the frame of the page (<cpp|frame_sync>,
  <cpp|vue_web_move_tab>), the input methods (<cpp|vue_web_compose>), the
  clipboard and printing through the page. Headless mode, which opens no
  display, makes the build testable under <name|node>.

  <subsection|The page>

  The page is <source-link|shell.html|misc/wasm/shell.html> and a series of
  scripts given to <name|Emscripten> as <verbatim|--pre-js>, each with a
  header which explains it:

  <\description>
    <item*|<source-link|progress.js|misc/wasm/progress.js>>The progress of
    the loading, until <TeXmacs> runs.

    <item*|<source-link|packages.js|misc/wasm/packages.js>>The tree
    <verbatim|/texmacs>: every file a placeholder, filled by the boot
    package before the start, by the other packages in the background, or
    by a range of its package when it is read first; the fonts fetched one
    by one; all kept in the Cache Storage of the browser.

    <item*|<source-link|web-pre.js|misc/wasm/web-pre.js>>The home directory
    <verbatim|/home/web>, kept in <name|IndexedDB> and written back a moment
    after each change, by the one tab which holds a Web Lock.

    <item*|<source-link|files.js|misc/wasm/files.js>>The Files panel:
    uploads, downloads, folders and zips, the files of <TeXmacs>.

    <item*|<source-link|frame.js|misc/wasm/frame.js>>The frame: a column at
    the left of the canvas with a <TeXmacs> menu and the tabs of the
    windows.

    <item*|<source-link|clipboard.js|misc/wasm/clipboard.js>>The clipboard:
    <TeXmacs> reads it synchronously, the browser gives it to a paste event
    only, so the page keeps what it knows of it.

    <item*|<source-link|ime.js|misc/wasm/ime.js>>Dead keys and input
    methods, which <name|SDL> does not have on the web.

    <item*|<source-link|print.js|misc/wasm/print.js>>Print and preview: the
    PDF of <name|MuPDF> opens in a tab of the browser.

    <item*|<source-link|javascript.js|misc/wasm/javascript.js>>The global
    <verbatim|TeXmacs> of the page (<scheme> from <name|JavaScript>).

    <item*|<source-link|wallet.js|misc/wasm/wallet.js>>The wallet, encrypted
    by <name|WebCrypto> instead of <name|GnuPG>.

    <item*|<source-link|workers.js|misc/wasm/workers.js>>The plug-ins which
    are Web Workers (below).
  </description>

  The node build has <source-link|node-pre.js|misc/wasm/node-pre.js>
  instead, which passes the environment of <name|node> to <TeXmacs>.

  <subsection|Plug-ins as Web Workers>

  A page has no processes: <cpp|posix_spawnp> fails and the plug-ins which
  run a program are not offered. A plug-in may instead say
  <scm|(:worker "url")> in its <scm|plugin-configure>, the url being its
  script relative to the page: the link
  (<source-link|worker_link.cpp|src/System/Link/worker_link.cpp>) sends the
  input of a session to that worker and reads back what it posts, in the
  usual protocol of the plug-ins, as through pipes;
  <source-link|workers.js|misc/wasm/workers.js> describes the messages. The
  scripts are in the <verbatim|web> directory of their plug-ins:
  <name|Python> on <name|Pyodide>
  (<source-link|tm-python.mjs|plugins/python/web/tm-python.mjs>), <name|R> on
  <name|webR> (<source-link|tm-r.mjs|plugins/r/web/tm-r.mjs>), <name|TikZ> on
  <name|TikZJax> (<source-link|tm-tikz.js|plugins/tikz/web/tm-tikz.js>),
  <name|Asymptote> on <name|Asymptote-web>
  (<source-link|tm-asy.mjs|plugins/asymptote/web/tm-asy.mjs>), and
  <name|JavaScript>, which runs in the page itself
  (<scm|(:worker "page:<em|file>")>,
  <source-link|tm-javascript.js|plugins/javascript/web/tm-javascript.js>).
  The <verbatim|web> target of the makefile copies them and their engines
  into the page.

  <subsection|Testing>

  Under <name|node>, which sees the files of the host, the node build
  converts documents as the desktop does:

  <\verbatim>
    TEXMACS_PATH=$PWD/TeXmacs TEXMACS_HOME_PATH=/tmp/tmhome HOME=/tmp/tmhome
    \\

    \ \ node build-wasm/out/node/texmacs.js -headless -c in.tm out.pdf -q
  </verbatim>

  In a browser, <source-link|browser-run.mjs|misc/wasm/browser-run.mjs>
  loads the page in a headless <name|Firefox> (with <verbatim|puppeteer-core>
  in <verbatim|build-wasm/tools>), prints the console and replays a script of
  clicks, keys and screenshots; <source-link|serve.mjs|misc/wasm/serve.mjs>
  serves the page, possibly as over a slow network. The tests of
  <source-link|misc/wasm/test|misc/wasm/test> check the home directory with
  several tabs (<verbatim|home-tabs.mjs>, also in <name|Safari> and
  <name|Chrome>) and the frames drawn while the page is resized
  (<verbatim|resize-flicker.mjs>, <verbatim|resize-jitter.mjs>).

  <subsection|Publishing>

  The workflow <verbatim|.github/workflows/wasm.yml> runs on the branch
  <verbatim|wasm_ci> only: work on <verbatim|maxs_texmacs> triggers
  nothing, and a state is built, tested (the node build turns the Welcome
  document into a PDF) and published on <name|GitHub Pages> by moving
  <verbatim|wasm_ci> to it,

  <\verbatim>
    git push origin maxs_texmacs:wasm_ci
  </verbatim>

  The page of the run is also kept as an artifact
  (<verbatim|texmacs-wasm-web>), to be served with <verbatim|node
  misc/wasm/serve.mjs <em|dir>>.

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
