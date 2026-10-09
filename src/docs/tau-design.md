# Tau: the editor in a worker, the interface in the page

A design, not implemented. Tau is an experimental browser port of TeXmacs
on the branch `wip_tau`: the editor and the Scheme interpreter run in a
Web Worker, and the whole interface is JavaScript and HTML in the main
thread of the page. This note fixes the organisation before any code is
written; the points still open are listed at the end.

C++ file references are relative to `src/`, Scheme ones to `TeXmacs/progs/`.
The starting point is `maxs_texmacs` at `fb9f944135`.

## Decisions

- **No compatibility.** Tau is an experiment. It does not keep the other
  GUIs building and is not meant to stay in sync with `maxs_texmacs`: the
  tree may be cut and reorganised freely.
- **The core has no widgets and no windows.** It has buffers and views,
  and describes menus, tool bars and dialogs to the page as data.
- **The interface is described in the vocabulary of the menu markup**
  (`kernel/gui/menu-define.scm`), with its dynamic parts evaluated and its
  attributes resolved, not as the widgets which `kernel/gui/menu-widget.scm`
  builds from it today.
- **All messages are asynchronous.** The core never waits for an answer
  of the page.
- **Client only.** A Tau can connect to a TeXmacs server over WebSocket;
  it is never a server itself.
- **OpenType fonts only.** No Type 1, TFM, PK or Metafont fonts. The
  family `roman` is Latin Modern, as in Vau.
- **MuPDF draws and writes the PDF.** No PDF Hummus, no Ghostscript.

This goes further than
[editor-frontend-separation.md](editor-frontend-separation.md), which kept
the widgets and ruled out an editor in another process because all the
widget traffic would have to be serialised. Here the widgets are not
serialised: they are not made.

## The two sides

```
  main thread (the page)                    worker
  ---------------------------               ------------------------------
  windows, tabs, panes, dialogs             buffers, views (editors)
  menus, tool bars, footer        <----->   typesetter, MuPDF
  canvases, scroll bars, focus    messages  Scheme (S7): commands, key maps,
  keyboard, pointer, clipboard              menu and dialog definitions
  files of the user                         remote client, file system
```

**The core** keeps `Edit/`, `Typeset/`, `Data/`, `Style/`, `Graphics/`
(without `Graphics/Gui`), `Scheme/`, the Freetype and MuPDF plugins, and a
reduced `Texmacs/` (below). **The page** is new code.

### What leaves the tree

- The GUI plugins (`Plugins/Qt`, `Qt6`, `Qtwk`, `Widkit`, `X11`, `NS`,
  `SDL`, `Vue`) and the widget factories and slots of `Graphics/Gui`
  (`widget.hpp`, `message.hpp`).
- `Texmacs/Window` (`tm_window.cpp`, `tm_frame.cpp`, `tm_dialogue.cpp`,
  `tm_button.cpp`), `Texmacs/Data/new_window.cpp`, and what concerns
  windows in `Texmacs/Server/tm_server.cpp`.
- `Plugins/Metafont`, `Plugins/Pdf`, `Plugins/Ghostscript`, the Type 1
  subsetter of the MuPDF plugin (`mupdf_type1.c`, `mupdf_writet1.c`), and
  the plugins of other platforms and toolkits.
- `System/Link/texmacs_server.cpp` and the Scheme modules of `server/`.
- `TeXmacs/fonts/tfm` and `TeXmacs/fonts/type1`. (The virtual fonts
  `tradi-*.vfn` stay: they build long arrows, negations and the like over
  any font, and the smart font uses them with OpenType fonts too.)

## Buffers, views and windows

| Object | Where | What it is |
|---|---|---|
| buffer | core | a document: its tree, file information, project, links. `tm_buffer_rep` without its list of windows |
| view | core | an editor on a buffer, with a number. It holds a copy of the state of the place where the page shows it (below) |
| window, tab, pane, dialog | page | where views are shown. The core knows none of them |
| focus | page decides | the page tells the core which view has the keyboard |

A view does not know whether it is shown in a pane or inside a dialog (the
"embedded" editors of today, `texmacs_input_widget`). What differed
becomes three properties of the view, given when it is made: whether it
asks for the bars of a window, a background which replaces white, and
whether its size comes from its container.

**The current view.** Today the server has a current view, and the editor
reaches its window by making itself current for the time of a call (the
`SERVER` macro of `Edit/editor.hpp`). In Tau every message of the page
which concerns a view names it; the core makes that view current for the
time it handles the message. Commands run from Scheme without a view
(delayed commands, the remote client) use the view which has the focus.

**What a view knows of its place.** The page sends, whenever they change,
the size of the canvas, the scroll position, the zoom factor and the
density of the screen. The view keeps them, and what the editor asked of
its widget before (`get_size`, `get_visible`, `scroll_where`...) is
answered from this copy. In the other direction the view tells the page
the extents of the document and where to scroll. This is the
`canvas_host` of the earlier design, as stored state.

**The server** (`tm_server`) becomes the loop of the worker: take a
message, run what it asks, typeset the views which changed, send what
resulted. Delayed and idle commands run from the timers of the worker.
The key maps stay in the core: the page sends keys, not commands.

## The protocol

Messages are plain objects (structured clone); pixmaps travel as
transferred buffers. Each has a type `t`; those which concern a view have
`view`.

**From the page**

| Message | Content |
|---|---|
| `key` | the key in the notation of TeXmacs (`"a"`, `"C-x"`, `"S-left"`), or a string of text from an input method |
| `mouse` | kind (`press-left`, `move`, `release-left`, `wheel`...), position in the document, modifiers, time |
| `place` | size of the canvas, scroll position, zoom, density of the screen |
| `focus` | the view which has the keyboard, or none |
| `invoke` | the number of an action (of a menu entry, a button) |
| `expand` | the number of a submenu or of a lazy part: the core answers with its contents |
| `answer` | the number of an input of a dialog and its new value |
| `close` | a view or a dialog was closed by the user |
| `paste`, `drop` | the data, as text, HTML or files |
| `file` | a file of the user, with its bytes, to open |

**From the core**

| Message | Content |
|---|---|
| `view` | a view was made, closed, renamed, modified or saved; its buffer and properties |
| `extents` | size of the document of a view; `scroll` asks for a scroll position |
| `paint` | a rectangle of a view as a pixmap, with the state of `place` it was drawn for |
| `caret` | the cursor and the selections of a view, as rectangles, for the page to draw above the pixmap |
| `chrome` | the contents of a part of the interface: the menu bar, an icon bar, the side tools, the footer |
| `contents` | the answer to `expand` |
| `dialog` | open, update or close a dialog, with its contents |
| `refresh` | new contents for a refreshable part |
| `pointer` | the shape of the pointer over a view |
| `clipboard` | data to put on the clipboard |
| `download`, `open-url` | a file for the user, an address to open |

Rules:

- **Stale answers are dropped.** `place` carries a counter which `paint`
  and `caret` repeat; the page ignores those drawn for an older place.
- **One paint per view per turn.** The core handles a message, typesets,
  and sends at most one `paint` for each view. The page merges the moves of
  the pointer and the scroll positions which it has not sent yet.
- **Actions are numbered per description.** The actions of a menu or a
  dialog are closures of Scheme, kept in a table which belongs to the
  description sent; when that description is replaced or closed the table
  is dropped, so that closures do not pile up in the worker.
- **Numbers belong to a core.** The numbers of views, actions, submenus
  and dialogs are those of the core which sent them; the page keeps them
  with that core (see *More than one worker*).
- **Menus stay lazy.** A submenu crosses as a label and a number. Its
  contents are computed when the page sends `expand`, as the promise
  widgets do today.

## The vocabulary of the interface

What crosses is the markup of `menu-define.scm` after evaluation. The
constructs which compute (`eval`, `dynamic`, `link`, `cond`, `let`, `with`,
`loop`, `receive`, `former`) are run in the core and do not cross. What is
left are nodes with resolved attributes:

| Kind | Nodes | Attributes |
|---|---|---|
| entries | `entry` | label, icon, shortcut (from the key maps), check mark (`v`, `*`, `o` or none), enabled, help text, action |
| | `submenu` (`->`, `=>`) | label, icon, enabled, number for `expand` |
| | `separator` (`---`), `glue` (`===`, `//`, `>>`), `group` | title for `group` |
| inputs | `input`, `enum`, `choice`, `choices`, `filtered-choice`, `toggle`, `color-input`, `tree-view` | current value, possible values or proposals, type, a hint of width, number for `answer` |
| settings | `setting-toggle`, `setting-enum`, `setting-group` | description, and as the inputs |
| contents | `text`, `concat`, `verbatim`, `color` | the string, already translated |
| | `icon` | the path of the file found for its name (below) |
| | `texmacs-output` | a typeset box, sent as a pixmap |
| | `texmacs-input` | the number of a view, which the page shows there |
| layout | `hlist`, `vlist`, `horizontal`, `vertical`, `aligned` with `item`, `tabs` with `tab`, `icon-tabs`, `hsplit`, `vsplit`, `scrollable`, `resize`, `padded`, `centered`, `minibar`, `bottom-buttons`, `division`, `class`, `extend` | sizes as hints |
| styles | `bold`, `grey`, `mono`, `verb`, `inert`, `explicit-buttons`, `plain-style`, `tile` | a property of the node which contains what they apply to |
| lazy | `refreshable` | a kind and a number; `refresh` sends new contents |

The page chooses how a node looks: an `enum` may be a `<select>`, an entry
shows its icon in a bar and its label in a menu, a `setting-group` is a
fieldset. The layout nodes and the sizes of dialogs still cross as they
are written, as hints: the markup of dialogs is not free of layout.

**Icons.** The name of an icon in the markup is not a file: it is looked
up along `TEXMACS_PIXMAP_PATH`, which has the pixmaps of the user before
the directories of the icon set and of the theme chosen in the
preferences, and the format is substituted (the markup says `.xpm`, the
file is an SVG or a PNG, possibly a `_x2` variant). The core has the path
and the preferences, so it resolves: an `icon` node carries the path of
the file which was found. The page draws it, so it fetches:

- a file of the resources is fetched by the page itself, at the address
  which the manifest of the packages gives for that path. The worker never
  reads its bytes: the tree of placeholders is enough for the lookup;
- a file of the home directory of the user, which only the worker has, is
  sent as bytes, once for each path;
- a change of icon set or of theme is a new `chrome` message with other
  paths; the page knows nothing of icon sets.

The icons are then drawn by the browser, not by MuPDF as in the Vue port,
so their rendering may differ slightly from one browser to another.

**The serialiser** replaces `menu-widget.scm`. It walks the markup as that
file does and resolves what the lowering to widgets resolves today:
translations, shortcuts, the files of the icons, the check marks and
balloons which are properties of the commands (`:check-mark`, `:balloon`),
the values of the inputs, which entries are greyed. It emits nodes where `menu-widget.scm` calls the
`widget-*` primitives (47 of them), which disappear from the glue.

## What changes in Scheme

- `kernel/gui/menu-widget.scm` is replaced by the serialiser.
  `menu-define.scm`, `gui-markup.scm` and the definitions of menus,
  widgets and tools in the rest of the code (about a thousand uses of
  `menu-bind`, `tm-menu`, `tm-widget`, `tm-tool`) are not touched.
- Windows: `top-window`, `dialogue-window`, `interactive-window`, the
  `alt-window-*` primitives and the placement of tools (`tool-select`)
  are rewritten on `dialog`, `chrome` and `view` messages. About fifty
  files name them; most only call them.
- Values of forms come with `answer`; nothing reads them back from a
  widget (`get_string_input`, form fields).
- Buffers and views: the commands which switch buffers, open and close
  windows (`texmacs/texmacs/tm-files.scm`, `tm-server.scm`, `tm-view.scm`)
  ask the page to show a view instead of making a window. About sixty
  primitives of the glue concern windows, buffers and views and are to be
  reviewed.
- `server/` goes; `client/` stays.

## Fonts

The family `roman` is Latin Modern Roman, with Latin Modern Math for
formulas (typeset from its MATH table) and Latin Modern Sans and Mono for
the sans serif and typewriter variants. In Vau this was done around code
shared with TeXmacs: an alias in `smart_font.cpp` and functions with the
names of the Metafont plugin (`tex_font`...) which return Latin Modern
fonts. Here the font rules can be rewritten: `roman`, and the families
whose letters are calligraphic, fraktur or blackboard bold (`cal`, `Euler`,
`Bbb`), are defined directly on OpenType fonts, the rules of the TeX fonts
(`fonts/fonts-ec.scm` and the others) go, and the marks of the typesetter
and the default font of the interface ask for Latin Modern by name.

Two fixes found in Vau are needed as well: braces which have no character
of their own (`<underbrace>`) are stretched by OpenType math fonts, and
the rubber font has its virtual font whenever the face has a MATH table.

The page breaks of documents written with the TeX fonts change, since the
metrics do.

## Drawing

The core draws with MuPDF and sends pixmaps of the visible part of a view,
as Vau does. The cursor and the selections are sent as rectangles and
drawn by the page above the pixmap, so that the blinking of the cursor and
the feedback of a selection do not need a new pixmap.

A pixmap is copied once out of the memory of the program, then
transferred to the page, which moves it without a copy. Shared memory
would save that one copy (a few milliseconds for a whole view on a dense
screen), but the 2D canvas does not take shared pixels, the page could
read a pixmap while the next is drawn, and the program would have to be
built with threads. If moving pixels turns out to cost, the first thing
to try is an `OffscreenCanvas`: the page hands a canvas over to the
worker, which draws on it directly, and no pixmap crosses. What makes
scrolling smooth is elsewhere: the page keeps more than the visible part
(tiles around it) and scrolls over them by itself while the core is busy.

## Resources, build and tests

- The resources come in packages, loaded lazily
  (`devel/package.py` and `platform/wasm/vau_packages.js` of Vau, which
  come from `misc/wasm` of this tree). The icons, which Vau leaves out,
  are files of their own, fetched by the page (see *Icons* above).
- The build is the one of `misc/wasm`, without SDL3 and the Vue plugin.
- The surface of the core is the protocol, so the core runs under node
  with a script of messages and its answers are compared with recorded
  ones: tests of the editor without a browser.

## Limits known from the start

- **The core cannot be interrupted.** While it computes it does not see
  the messages of the page, so `gui_interrupted` and `check_event` answer
  no. A long typesetting delays the next key; the page stays responsive.
  A service worker can lift this (see *More than one worker*).
- **The clipboard.** Browsers let a page write to the clipboard only
  shortly after an action of the user, and the copy comes back from the
  worker asynchronously. The browser build of `maxs_texmacs` has the same
  constraint.
- **Keys.** The page must decide at once whether a key is its own or the
  browser's, before the core has looked it up: with a view in focus it
  takes all keys but a short list left to the browser.

## More than one worker

**The core does not split.** Workers share no memory (unless the page is
cross-origin isolated, below), so each is a program of its own, with its
own heap. The core assumes the contrary everywhere: one tree for all the
buffers (`the_et`), reference counts which are not atomic, observers on
the nodes of the tree, one Scheme interpreter. The editor and Scheme call
each other synchronously, many times for each command (the glue of the
editor alone has 320 functions): Scheme in one worker and the editor in
another would make a message of each call. The pages or the paragraphs of
a document cannot be typeset in parallel either: they share the
environment and the tree.

What can run elsewhere is what stands around the core:

| | What it is | Gain | Cost |
|---|---|---|---|
| **job workers** (proposed) | a second core, the same program, started when needed and given a copy of a document: PDF export, conversions, printing | a long job does not freeze editing | a second start (under a second) and a second heap while the job runs |
| a worker which draws | the core sends a display list for each page, and another worker with MuPDF, or the canvas of the page, makes the pixels | scrolling and zooming stay smooth while the core computes; repaints do not compete with typing | the display lists and their protocol: the larger option of point 1 of *Open* |
| a core per document | each open document in a worker of its own | a heavy document does not delay another; a crash is contained | memory, for each document; what spans buffers (projects and includes, links between documents, another buffer in the same view) needs a protocol of its own or is lost |
| sessions and plugins | Python, R... | already workers of their own in `maxs_texmacs` | |

A job is "these bytes in, those bytes out" for the same program as the
core: job workers cost little and can come at any time. The worker which
draws is the one which changes how the editor feels, but it comes with
the display lists. A core per document suits a browser, where tabs are
expected to be independent, and goes against the buffers of TeXmacs,
which know of each other: not a place to start.

**The protocol does not assume one worker** (proposed, from the start).
Views, actions and dialogs are numbered per core, and the page reaches a
core through an object which stands for it, not through a global. Job
workers, a worker which draws or a core per document can then be added
without reworking the page.

**Interrupting the core** is the main gain at stake, and it needs no
split. Two ways, both through a service worker:

- *Shared memory.* A page can be made cross-origin isolated on a site of
  static files too, by a small service worker which adds the headers
  which the server does not send. The page and the core then share a flag
  which the page sets and the core polls: `gui_interrupted` works again,
  and a key can cut a long typesetting short. Isolation is decided when
  the document is fetched, so the page must be fetched again once the
  service worker controls it. To make this cost nothing, the page loads
  in two steps: `index.html` is a launcher of a few hundred bytes which
  checks `crossOriginIsolated`; if it is not, it registers the service
  worker, waits for it and replaces itself by its own address; if it is,
  it starts the application. Nothing large is asked before the check
  passes (the program and the boot package may be fetched meanwhile: they
  are then in the cache of the browser). An isolated page may embed from
  other sites only what allows it, which concerns the plugins loaded from
  elsewhere (Pyodide, webR).
- *Polling.* The core makes, from time to time, a synchronous request to
  an address which the service worker answers itself, with what the page
  told it: a key is waiting, or nothing. No isolation, no second fetch of
  the page, no restriction on what it embeds. A check costs about a
  millisecond where reading shared memory costs nothing, so the core can
  ask every 50 to 100 ms of computation, which is enough to give up a
  long typesetting.

Where a service worker cannot be installed (a page opened as a file, some
private modes) the application starts all the same, and the core is not
interrupted. Proposed: measure typing in a long document at step 2 of the
work; if interruption is needed, polling first. Shared memory is worth
its constraints only with what else it allows (threads, a worker which
draws and shares its data with the core).

## State

**Step 1 is done** (2026-10-09): the core builds without a GUI and runs
under node.

    . misc/wasm/emenv.sh build-tau
    make -C build-tau -f ../misc/tau/Makefile -j8 MUPDF=<sources of MuPDF built for wasm>
    make -C build-tau -f ../misc/tau/Makefile check

- `misc/tau/` has the build: `Makefile`, `config.h` (`TAUTEXMACS`),
  `sources.txt`. `out/node/tau.js` is the core for node; `check` typesets
  a document of the examples and writes its PDF.
- `src/Tau/` is what stands in the place of a GUI. `tau_widget.{hpp,cpp}`
  is the class which the editor derives from: it keeps the state of the
  place of a view and answers the questions of the editor from it (the
  beginning of *What a view knows of its place*). `tau_gui.cpp` has the
  services of `gui.hpp` (the loop, the clipboard) and the constructors of
  widgets, which make nothing.
- Without a GUI the core asked for 71 symbols: the services, and about
  fifty constructors of widgets. These constructors, `Texmacs/Window` and
  the windows of `Texmacs/Data` are still compiled: they go with their
  callers, in steps 3 to 5. So does the server (`texmacs_server.cpp`,
  `progs/server`), with the review of the glue.
- Gone from the tree: the GUI plugins, those of other platforms, Metafont,
  PDF Hummus, Ghostscript, Cairo, Imlib2, Resvg, the previews by LaTeX, the
  TFM and Type 1 fonts and the Type 1 subsetter. The other builds of
  TeXmacs (configure, CMake, `misc/wasm`) do not work any more.
- Fonts: `roman` is Latin Modern (`roman_fix` in `smart_font.cpp`); the
  families `cal`, `Euler` and `Bbb` are alphabets of Latin Modern Math
  (`Graphics/Fonts/alphabet_font.cpp`, `fonts/fonts-alphabets.scm`, in the
  place of the seven files of rules for TeX fonts); the marks of the
  typesetter ask for Latin Modern by name. The old font menus
  (`fonts/font-old-menu.scm`) still list TeX fonts, and ask whether they
  are installed: the answer is no.
- The test document of Vau (70 pages with the TeX fonts) has 73 pages.

**Step 2 is done** (2026-10-09): one view of the editor in a bare canvas.

    make -C build-tau -f ../misc/tau/Makefile -j8 MUPDF=... web
    make -C build-tau -f ../misc/tau/Makefile serve    # http://localhost:8080/

`index.html?arg=/texmacs/doc/...` opens a document of TeXmacs (the `arg`s
are its arguments), `?log` sends its output to the console.

- `out/web` has the core for a worker (`tau.js`, `tau.wasm`), the files of
  TeXmacs in packages (`misc/wasm/package.py` and `packages.js`, unchanged)
  and the page: `misc/tau/web/index.html`, `tau.mjs` (the canvas, the
  keys, the pointer, the wheel) and `tau-worker.js` (the messages).
- The core gives control back to the worker (`gui_start_loop` in
  `tau_gui.cpp`). Each message of the page calls a function (`tau_place`,
  `tau_scroll_by`, `tau_focus`, `tau_key`, `tau_mouse`), and ends with a
  turn: the editors are told of new sizes, the pending commands run (the
  interpose handler of the server), and the views which changed are drawn
  and sent. A turn is also made twenty times a second without a message.
- A view (`tau_widget.cpp`) has its pixels: a picture of MuPDF of the size
  of the canvas, drawn by the editor through `handle_repaint` for the
  invalid regions, in the coordinates of the document at the scroll
  position, as the Vue port did.
- Measured in a headless Firefox, a canvas of 2000 x 1354 pixels, a
  document of twelve screens: 10 ms from a key to its pixels in the page,
  and the same for a step of scrolling.

What differs from the protocol above, for now:

- **The whole canvas is sent** at each paint (11 MB for the canvas above),
  not the rectangles which changed.
- **The core has the scroll position.** The page sends steps (`scroll`,
  from the wheel) and each `paint` tells the extents and the position; the
  page keeps no tiles and does not scroll by itself.
- **The cursor is drawn by the editor**, in the pixels. Its position comes
  with each `paint` (`caret`), and is not used yet.
- **`view`** only says which view is shown; the page shows the last one.
- **No welcome message** over a document given as argument: the home
  directory is new at each visit (in memory), so every start is a first
  one.
- The keys are those of `keydown`; no input methods, no dead keys.

**Step 3 is done** (2026-10-09): the menu bar, the icon bars and the
footer are in the page.

- `kernel/gui/menu-serial.scm` is the serialiser. It walks the markup as
  `menu-widget.scm` does and makes
  nodes, written as JSON: `entry` (label, icon and its file, shortcut,
  check mark, enabled, help, the number of its action), `submenu` (label,
  the number of its contents), `separator`, `glue`, `group`, `text`, the
  containers (`horizontal`, `vertical`, `hlist`, `vlist`, `minibar`, `tile`,
  `refreshable`). What computes is run (`if`, `when`, `for`, `mini`, `link`,
  `dynamic`, `promise`, `style`). The inputs, the tabs and the layout of
  dialogs come out as nodes marked `unsupported`, until step 4.
- The actions and the contents of the submenus are closures kept in a
  table, by part of the interface: they are forgotten when the part is
  described again. `tau-serialize-part`, `tau-expand`, `tau-invoke`.
- In the core, `tm_window_rep::get_menu_widget` describes the menu instead
  of making a widget (`tau_chrome` in `tau_gui.cpp`): the parts are `menu`,
  `icons-0` to `icons-3`, `side-0`..., `bottom-0`... The window of a view
  passes on the texts of the footer (`footer`) and which bars are visible
  (`visible`). `tau_invoke` and `tau_expand` are called by the page.
- In the page, `chrome.mjs` makes the bars and the menus from the nodes.
  A menu asks for its contents when it opens.
- The icons are files of the core, in the packages and not at addresses of
  their own. The serialiser finds the file as the core does when it draws
  an icon (the SVG of that name in the `light` theme along
  `TEXMACS_PIXMAP_PATH`, then next to it, then PNG files), and the worker
  adds to a description the bytes of the files which the page has not got
  yet (`files`), so that a bar comes with its icons and not before them.
- `load-help-article` was only defined once the help menu had been built,
  which the menu bar was, whole, at the start: it is a lazy definition now.

Not there yet: the side and bottom tools (described, not shown), the
context menu of the editor, tooltips other than the titles of the buttons,
keys in an open menu.

**Step 4 is done** (2026-10-09): the dialogs and the questions are in
the page.

- The serialiser makes the nodes of the dialogs: `input` (type, value,
  proposals, width), `enum` (values, value, editable; with a label for a
  `setting-enum`), `choice` (values, chosen, multiple; with a filter for a
  `filtered-choice`), `toggle`, `box` (a `setting-group`), `aligned` (rows
  with a left and a right side), `tabs` (for each a label, maybe an icon,
  and a page; the responsive tabs are tabs), and the layouts `scrollable`,
  `hsplit`, `vsplit`, `resize`, `division`, `class`. An entry in the style
  of a button says so (`button`). `texmacs-input`, `texmacs-output`,
  `tree-view`, `color-input` and `ink` are still marked `unsupported`.
- An input has a number as an action has; the page sends `answer` with the
  number and the arguments of the command (strings, booleans, lists of
  strings), which the worker writes in Scheme for `tau-answer`. The values
  shown are translated; an `enum` maps the answer back.
- A `refreshable` is a part inside its part (`dialog-3/274`), with its own
  closures. `refresh-now` (`windows_refresh` in the core) reaches
  `tau-refresh`, which describes again the parts of that kind and sends
  `refresh` with the number of the node. Only the dialogs keep the node
  in the page; a refreshable part of a bar is not replaced yet.
- What Scheme has to say by itself (a dialog, a refresh) waits in an
  outbox, which leaves at the end of the turn of the core (`tau-outbox`,
  one message `batch` which the worker splits): no new glue.
- `top-window` and `dialogue-window` make dialogs (`tau-dialog-new`,
  `tau-dialog-show`, `tau-dialog-close`): `dialog` with a number, a title
  and the nodes, `close` when the core takes it away. The cross and Escape
  send `close` to the core (`tau-dialog-closed`), which runs the command
  of the window; the page removes a dialog only when the core says so.
- `interactive` asks in a dialog (`tau-interactive` is the
  `tm-interactive-hook`): an aligned list of inputs with Cancel and Ok, or
  the proposals as buttons for a question. Return in an input validates
  it. The prompt in the footer is gone.
- In the page the dialogs are floating boxes, not modal, moved by their
  title (`chrome.mjs`).

Checked in Firefox: Format → Whitespace → Rigid (question, value typed,
the space is inserted), Edit → Preferences (tabs in tabs, 110 controls; a
preference changed is there when the dialog is opened again), Format →
Font (lists, and three parts refreshed when a family is chosen).

Not there yet: `interactive-window` (the printer and colour pickers of
the toolkits), the tooltips and popups made with `alt-window-*`, the
continuous inputs (a value at each key), the sizes in `w` and `h` of
`resize`, the styles of the texts (bold, grey, monospaced).

**Step 5 is done** (2026-10-09): the documents are tabs, the windows of
the core are panes, and the files and the clipboard are those of the
browser.

- **The window of the core stayed**, as the place of a view: a
  `tm_window` has no widgets any more, only a number, the view which it
  shows and the command which closes it. This is less than the design
  asks (the core should know no window at all), and what it costs is
  small: each window is a pane of the page, side by side. The menus and
  the footer are those of the window which has the keyboard.
- At the end of a turn the core says which documents there are (name,
  title, modified) and which view and document each window shows
  (`buffers`), when that changed. The page makes and removes its panes
  from it and draws the tabs, which keep their order. A tab asks
  `buffer` with `switch`, `close` or `new`; the cross of a pane asks
  `close-window`. Closing a modified document asks first, in a dialog
  (`user-ask` goes through the dialogs of the page too now).
- A new document is a tab ("buffer management" is `shared`); "New
  window" makes a pane.
- **The system of the user** is told to the core (`TEXMACS_WEB_PLATFORM`,
  as in the browser build of `maxs_texmacs`), so that the shortcuts are
  those of a Mac on a Mac.
- **Files.** The file system of the core is in memory. `choose-file` asks
  the page (`tau-choose-file` in `texmacs/texmacs/tau-files.scm`): to
  load, the page opens the file chooser of the browser (`pick`) and
  sends the bytes (`open`), which the worker writes under `/user`; a
  file dropped on a pane is opened the same way. To save or export, a
  name is asked in a dialog, the file is written under `/user` and given
  to the user (`download`); saving again a document which lives under
  `/user` gives it again.
- **Clipboard.** What is copied stays in the core as a tree, and its
  text goes to the clipboard of the browser (`clipboard`). The key which
  pastes is left to the browser, whose `paste` event gives the text and
  the HTML to the core (`paste`); they replace what the core kept unless
  the text is the one it gave. Files in the clipboard are opened.

Checked in Firefox: a new tab, typing in it, switching, a dropped file,
Load through the file chooser, Save as and Export to PDF arriving as
downloads, Copy reaching the page, the question on closing a modified
tab, a second pane made and closed. The `paste` event of the browser
could not be produced in the headless test: the message it sends was
checked, not the event.

Not there yet: the files of the user do not survive the page (nothing
is kept in the browser); Edit → Paste and the paste keys of Emacs use
what the core kept, not the clipboard of the browser; nothing but text
is copied out (no HTML, no pictures in); "Close TeXmacs" ends the
worker and leaves the page dead; the panes cannot be resized or split
in the other direction; images are not picked in several formats, and
directories not at all.

## Order of the work

1. **The core alone.** Cut the tree; build without a GUI; under node:
   start, load a document, typeset, write a PDF. Fonts on Latin Modern.
2. **One view.** The protocol for `place`, `paint`, `caret`, `key`,
   `mouse`: editing in a bare canvas.
3. **Menus.** The serialiser; the menu bar, the icon bars and the footer
   in the page; lazy submenus; `invoke`.
4. **Dialogs.** `dialog`, `answer`, `refresh`; `interactive`.
5. **Buffers and views.** Tabs and panes in the page; opening, saving and
   downloading files; the clipboard.
6. **The rest.** Views inside dialogs, `texmacs-output`, tools, the remote
   client, plugins.

## Open

1. **Drawing.** Pixmaps (above), or a display list replayed on a canvas
   of the page, which would scroll and zoom more smoothly and is much more
   work. Proposed: pixmaps first.
2. **The code of the page.** Plain ES modules without a build step, or a
   framework. Proposed: plain.
3. **Plugins.** `maxs_texmacs` runs Python and R sessions in workers of
   their own. Proposed: leave them out until the protocol is stable.
4. **The files of the user.** Reuse the home directory kept in IndexedDB
   by the browser build of `maxs_texmacs`, or something else.
5. **Editing from the start** is assumed (step 2), rather than a viewer
   with menus first.
6. **Interrupting the core**, by polling or by shared memory, both
   through a service worker (see *More than one worker*). Proposed:
   decide after measuring at step 2; polling first.
