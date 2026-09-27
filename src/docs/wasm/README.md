# TeXmacs in the browser (branch `wip_wasm_vue`)

Goal: a WebAssembly build of TeXmacs which runs in a web page, on the Vue GUI
(Clay for the layout, MuPDF for the pixels) and SDL3, with the S7 Scheme
interpreter (merged from `wip_s7` of texmacs/texmacs, see [`../s7/`](../s7/README.md)).

## Starting point

- The branch starts from `wip_other_guis` (Vue, SDL and the other GUIs,
  2026-09-26) and merges the 38 commits of `wip_s7` over upstream
  `fa8da19dd0` ("resvg fix"). The two repositories have unrelated histories
  (two conversions of the SVN trunk; upstream's root is our `src/`), but
  `fa8da19dd0` has the very tree of our `1cc2c1435f` on `svn_sync`: the
  commits were replayed under `src/` on top of it (`git am --keep-cr
  --directory=src`, branch `s7_on_svn`) and merged with a real common base.
- S7 is the default interpreter (`--with-scheme=s7|guile`). Vue on S7 builds
  and passes the scripted tests (key-text, focus-windows, menus, dialogs,
  interactive, tools).

Guile 1.8 could not have come along: its garbage collector scans the C
stack for roots, which WebAssembly does not expose. S7 is a single C file
with a precise collector.

## What the browser does not allow

| desktop | in the browser |
|---|---|
| several windows (dialogs, tools, menus, tooltips) | one canvas |
| the loop owns the thread (`gui_start_loop`) | control goes back to the browser every frame |
| files anywhere, `$HOME/.TeXmacs` | a virtual file system; persistence in IndexedDB; the user's files through upload/download |
| plugins as processes (pipes), sockets, `system ()` | none: no fork, no pipes, no sockets |
| system file dialogs (SDL) | the browser's file input |
| the system clipboard, synchronously | the async Clipboard API (text), only on a user gesture |
| threads (SDL file dialog callbacks) | none unless SharedArrayBuffer and COOP/COEP headers |

## State (2026-09-26)

TeXmacs runs in a web page (tested in headless Firefox 155): it boots, opens
the welcome document, typesets it with its fonts and images, takes the
pointer and the keyboard, opens the menus, and draws its dialogs as virtual
windows in the page. Headless under node it converts documents to PDF (the
MuPDF writer) in about 4 s, boot included.

| | done | not yet |
|---|---|---|
| windows | single-window mode: tabs for the windows of the editors, floating dialogs; the frame of the page | resize handles of the dialogs |
| build | `misc/wasm/Makefile`, the slim MuPDF 1.28.5, S7, SDL3 3.4 | `-Oz` and LTO (not measured) |
| loop | one iteration per frame (`emscripten_set_main_loop`) | all the events of a frame in one iteration |
| files | packages: 9.3 MB before the start, the rest in the background; the home kept in IndexedDB; the Files panel, uploads, downloads, drops | |
| processes | `posix_spawnp` fails cleanly | plugin menus hidden, no external converters offered |
| file dialogs | the Files panel of the page | |
| fonts | Fira for the interface (the TeX fonts lack its arrows) | |
| clipboard | copy (text, HTML) with `navigator.clipboard`; paste by the paste event of the browser; the look and feel of the platform of the browser (Cmd on a Mac) | paste from the menus sees the last paste or copy only |

## Windows and the frame of the page

In single-window mode the only SDL window, the host, is a container with
nothing of its own; every window of TeXmacs is virtual. The windows of the
editors are tabs: each fills the host and only the active one is drawn and
gets the events (dialogs, tools, balloons and popups float above it). In
the browser the page has a frame above the canvas (`misc/wasm/frame.js`):
the tabs, labelled with the names of the windows (the title of a window on
the desktop, and the title of the page for the active one), with a marker
for unsaved changes, a close box (not on the last tab: TeXmacs asks as for
a window whether to save), a `+` for a new window, and a TeXmacs menu: what
this TeXmacs is (version, S7, MuPDF, build date), where its files are, how
many of its packages have come, the storage used, the Files panel, notes
on the keyboard (the browser keeps some shortcuts), texmacs.org, reload,
and a reset (the files kept by the browser deleted). The plugin tells the
frame of the tabs once per frame when they changed (`frame_sync`); the
frame asks it to show, close or open one. Quitting TeXmacs reloads the
page (after the home directory is written to the storage of the browser).

On the desktop, `TEXMACS_VUE_SINGLE_WINDOW=1` gives the same, without the
frame: a tab asks the host to change its size and position, so that it
looks as before; the scripted tests have `tab <id>` to show a tab.

## Building and running

    . misc/wasm/emenv.sh build-wasm       # Emscripten (Python >= 3.10, config)
    sh misc/wasm/build-mupdf.sh           # MuPDF 1.28.5, the slim build
    make -C build-wasm -f ../misc/wasm/Makefile -j8 web    # the page
    make -C build-wasm -f ../misc/wasm/Makefile -j8 node   # node, headless
    node misc/wasm/serve.mjs              # http://localhost:8080/texmacs.html

- `misc/wasm/config.h`, `tm_configure.hpp`: the configuration (wasm32:
  pointers and `long` are 4 bytes), in place of what configure writes.
- `misc/wasm/sources.txt`: the sources, those of a desktop Vue+S7 build
  without the Objective-C (`list-sources.sh` writes it again).
- MuPDF's model of exceptions and of `setjmp`/`longjmp` is WebAssembly's
  (`-fwasm-exceptions -sSUPPORT_LONGJMP=wasm`): TeXmacs is compiled with the
  same, which rules out ASYNCIFY; the main loop gives control back to the
  browser instead (`loop_iteration` in `vue_gui.cpp`). Leaving it unwinds the
  stack, so the server of `TeXmacs_main` is allocated on the heap there.
- The slim MuPDF (`build-mupdf.sh`) has no fonts of its own but the standard
  14 and reads no documents but PDF, SVG and images: `texmacs.wasm` went
  from 59 MB to 22.5 MB (5.2 MB with brotli). Its fonts were 37 MB of it.
- SDL3_ttf serves only the unused rendering through SDL's renderer
  (`VUE_SDL_RENDERER`): not linked.
- The progress of the loading (`misc/wasm/progress.js`, the first pre-js):
  a panel with the phase and a bar, for the program (fetched by
  `Module.instantiateWasm` to count its bytes, compiled by the browser as
  they come), the boot package (`packages.js` reports its bytes), what is
  left to compile, and the boot of TeXmacs, before which the page is
  painted (a run dependency of its own, removed after two frames). The
  sizes are those of the files uncompressed: the build writes that of
  `texmacs.wasm` into the page (`@TM_WASM_SIZE@` in `shell.html`), since a
  compressed response does not give it. `serve.mjs [dir] [port] [KB/s]` and
  `browser-run.mjs --slow <KB/s>` load the page as over a slow network.

## The clipboard

TeXmacs reads the clipboard synchronously when it pastes; the browser gives
its contents to a paste event only (or asynchronously, with the consent of
the user). `misc/wasm/clipboard.js` keeps what the page knows of the
clipboard: the last copy of TeXmacs, or what the last paste event brought.

- **Copy, cut**: `set_selection` (`vue_gui.cpp`) hands the text, and the
  HTML when there is one, to `navigator.clipboard`, which the browser allows
  just after a key or a click. The TeXmacs format stays in the program, and
  is what is pasted as long as the text of the clipboard is the one copied
  with it (copy and paste between tabs lose nothing).
- **Paste**: SDL cancels the keys with Ctrl, and the canvas is not editable,
  so the browser would have no paste event. A hidden text area has the
  focus while Ctrl or Cmd is down; the page takes the key of a paste
  (Ctrl+V, Cmd+V, Shift+Insert) before SDL, with its keypress (Safari has
  one, which SDL cancels, and Safari then cancels the paste), the text area
  gets the paste event of the browser (or the text, when the event has no
  data), and SDL then gets the key: TeXmacs pastes as usual, from
  `get_selection`, which reads the page's clipboard in place of SDL's.
  Tested in Firefox and in Safari (through `safaridriver`, with "Allow
  remote automation"); `?trace-clipboard` logs each paste.
- **Shortcuts**: the look and feel defaults to that of the platform of the
  browser (`TEXMACS_WEB_PLATFORM`, set by `web-pre.js`; `basic.cpp`), so
  that on a Mac copy and paste are Cmd+C and Cmd+V for TeXmacs as for the
  browser. The browser does not act on the other keys with Cmd (Cmd+S would
  save the page), save those it reserves (Cmd+W, Cmd+T, Cmd+N, Cmd+Q).
- Paste from a menu has no paste event: it pastes what the page knows, the
  last copy or paste.

## Printing

Print and Preview (File menu, Cmd+P or Ctrl+P) write the PDF of the
document with MuPDF into `/tmp` of the page (not the home directory, which
is kept in IndexedDB) and call `(web-open-pdf path name)` (`vue_gui.cpp`),
which hands it to `misc/wasm/print.js`: the PDF opens in a tab of its own,
in the viewer of the browser, from which it is printed or saved. The
Scheme side is in `tm-print.scm` (`preview-buffer`, `preview-file`),
`tm-files.scm` (`print-buffer`) and `file-menu.scm` (the Print item).

A browser opens a tab only shortly after a click or a key: when the PDF
took longer (a long document) the tab is refused, and a notice in the page
offers to open it (a click of its own) or to download it. The last four
PDFs are kept for their tabs (`URL.revokeObjectURL` for the older ones).

The fonts are subset by MuPDF (`pdf_subset_fonts`). Its subsetter of CFF
fonts scanned each subroutine apart from the glyphs which call it, with no
stem hints: the hintmasks of a subroutine then had the wrong length, and
the scan read their bytes as operators (`MuPDF error: format error:
Reserved charstring byte c=0x0`), which stopped the subsetting of every
font of the document -- all of them were embedded whole. The Fira fonts,
whose charstrings are subroutinized, have hundreds of such subroutines.
`misc/wasm/mupdf-subset-cff.patch` (applied by `build-mupdf.sh`) executes
the subroutines within the charstrings which call them, as a renderer does.
The PDF of the Welcome document went from 740 KB to 566 KB, the fonts in
it to about 90 KB; the rest is its pictures, kept lossless (Flate), where
the desktop build writes them as JPEG. The desktop build links the MuPDF of
Homebrew, which has the same bug: a document in Fira gets its fonts whole
there.

## The files of TeXmacs in the page

`misc/wasm/package.py` writes the files of `TeXmacs/` (without `bin/` and
the programs and documentation of the plugins: 62.6 MB) as packages with a
manifest, `texmacs-files.json` (each file: its package, offset, size).
`misc/wasm/packages.js` makes the whole tree at `/texmacs` before TeXmacs
starts, every file a placeholder of its size, and loads the boot package;
the others (fonts, icons, languages, documentation, the rest, in pieces of
4 MB) come one after the other once TeXmacs runs. A file read before its
package fetches its bytes alone, a range of the package (synchronously, as
text in the user defined charset: TeXmacs reads its files synchronously),
so that TeXmacs never finds a file of its tree missing, nor records it as
such. The packages are kept in the Cache Storage of the browser.

The boot package is the files TeXmacs opens when it starts
(`misc/wasm/boot-files.txt`, the list of `?trace-files`: boot, the welcome
document, a new document with text and a formula) and some whole groups
read at unforeseeable times (the Scheme code, styles, packages, the metrics
of the fonts, the icons of the light theme): 15.9 MB, 4.0 MB with brotli.
To make the list again: load the page with `?trace-files`, use it, and
save `window.tmTrace` (see `build-wasm/trace.txt` of the notes below).

Measured in a headless Firefox with `misc/wasm/serve.mjs` (brotli, ranges,
304 for what did not change):

| | transferred |
|---|---|
| before TeXmacs starts (program + boot package) | 9.3 MB |
| everything, the first time (16 packages, 2.6 s locally) | 33.3 MB |
| the next visit | 0 (the Cache Storage, and 304 for the program) |
| a document of the help opened before its packages (`?no-background`) | 5 files on demand, 353 KB |

The same page before: one package of 62.6 MB and a program of 59 MB, 122 MB
(56 MB with brotli), all of it before TeXmacs could start.

Headless, under node, which sees the host files (NODERAWFS):

    TEXMACS_PATH=$PWD/TeXmacs TEXMACS_HOME_PATH=/tmp/tmhome HOME=/tmp/tmhome \
      node build-wasm/out/node/texmacs.js -headless -c in.tm out.pdf -q

In a browser: serve `build-wasm/out/web` over HTTP and open `texmacs.html`.
For tests, `misc/wasm/browser-run.mjs` does so in a headless Firefox (with
`puppeteer-core` installed in `build-wasm/tools`), prints the console and
replays a script of clicks, keys and screenshots:

    node misc/wasm/browser-run.mjs --script actions.txt   # see its header

## Plan

1. **Single-window mode, on the desktop first.** Every Vue window except the
   first becomes a *virtual window*: a rectangle composited into the host
   window, with its own Clay context as now. Dialogs and tools get a title
   bar (move, close); menus, tooltips and balloons are undecorated overlays.
   `plain_window` is the only place where windows are created, so the switch
   is there; the host's `process_redraw` draws the virtual windows on top, in
   z-order, and `process_event` routes the pointer by hit-testing them (as
   `popup_grab` already does for popups) and the keys to the focused one.
   Enabled by `TEXMACS_VUE_SINGLE_WINDOW` on the desktop, always in the
   browser. Tested with the scripted harness, which addresses windows by id
   and title and will keep doing so.
2. **No processes.** Plugins, sockets and external converters are disabled
   cleanly when unavailable (menus hidden, no crash), behind one flag.
3. **Toolchain and build.** Emscripten; SDL3 (port `-sUSE_SDL=3`), FreeType,
   zlib, libpng from the Emscripten ports; MuPDF built with `emmake` (it has
   an official WebAssembly build); SDL3_ttf with HarfBuzz; S7 as is. A
   dedicated build script rather than `configure`.
4. **Files.** `TeXmacs/` preloaded or fetched lazily (105 MB, of which fonts
   33 MB, progs 6 MB, doc 8 MB: the essentials first, the rest on demand);
   the home directory in IndexedDB (IDBFS); open and save through
   upload/download.
5. **The loop.** Turned into a per-frame step (`emscripten_set_main_loop`):
   ASYNCIFY cannot be combined with the WebAssembly exceptions of MuPDF.
6. **The page.** `index.html` with the canvas, the loading progress,
   `devicePixelRatio`, keyboard shortcuts kept from the browser, clipboard.

Phase 1 needs no Emscripten and is where most of the GUI work is.
