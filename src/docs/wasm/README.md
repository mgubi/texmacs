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
| windows | single-window mode (virtual windows, title bars, routing) | resize handles of the virtual windows |
| build | `misc/wasm/Makefile`, MuPDF 1.28.5 for wasm, S7, SDL3 3.4 | a size diet (`texmacs.wasm` 59 MB, mostly MuPDF's fonts; `texmacs.data` 63 MB) |
| loop | one iteration per frame (`emscripten_set_main_loop`) | all the events of a frame in one iteration |
| files | `TeXmacs/` preloaded at `/texmacs`, home at `/home/web` | persistence (IDBFS), upload/download, lazy loading |
| processes | `posix_spawnp` fails cleanly | plugin menus hidden, no external converters offered |
| file dialogs | cancelled | the file input of the page |
| fonts | Fira for the interface (the TeX fonts lack its arrows) | |

## Building and running

    . misc/wasm/emenv.sh build-wasm       # Emscripten (Python >= 3.10, config)
    cd build-wasm
    curl -LO https://mupdf.com/downloads/archive/mupdf-1.28.5-source.tar.gz
    tar xzf mupdf-1.28.5-source.tar.gz
    (cd mupdf-1.28.5-source && emmake make -j8 OS=wasm build=release libs)
    cd ..
    make -C build-wasm -f ../misc/wasm/Makefile -j8 web    # the page
    make -C build-wasm -f ../misc/wasm/Makefile -j8 node   # node, headless

- `misc/wasm/config.h`, `tm_configure.hpp`: the configuration (wasm32:
  pointers and `long` are 4 bytes), in place of what configure writes.
- `misc/wasm/sources.txt`: the sources, those of a desktop Vue+S7 build
  without the Objective-C (`list-sources.sh` writes it again).
- MuPDF's model of exceptions and of `setjmp`/`longjmp` is WebAssembly's
  (`-fwasm-exceptions -sSUPPORT_LONGJMP=wasm`): TeXmacs is compiled with the
  same, which rules out ASYNCIFY; the main loop gives control back to the
  browser instead (`loop_iteration` in `vue_gui.cpp`). Leaving it unwinds the
  stack, so the server of `TeXmacs_main` is allocated on the heap there.
- SDL3_ttf serves only the unused rendering through SDL's renderer
  (`VUE_SDL_RENDERER`): not linked.

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
