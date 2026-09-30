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
| plugins as processes (pipes), `system ()` | none: no fork, no pipes |
| TCP sockets (the TeXmacs server and its clients) | WebSockets only: a client, not a server |
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
| windows | single-window mode: tabs for the windows of the editors, floating dialogs, resized by their frame, their contents scrolled when they do not fit; the frame of the page | |
| build | `misc/wasm/Makefile`, the slim MuPDF 1.28.5, S7, SDL3 3.4 | `-Oz` and LTO (not measured) |
| loop | one iteration per frame (`emscripten_set_main_loop`) | all the events of a frame in one iteration |
| files | packages: 9.3 MB before the start, the rest in the background; the home kept in IndexedDB; the Files panel, uploads, downloads, drops | |
| processes | `posix_spawnp` fails cleanly; the plugins which run a program are not offered (their `:require` sees no command), and one started anyway fails at once, its session dead with an error (it froze the page: `fork` fails, and the pipes were read again and again); Scheme sessions work | no external converters offered |
| file dialogs | the Files panel of the page | |
| fonts | Fira for the interface (the TeX fonts lack its arrows) | |
| clipboard | copy (text, HTML) with `navigator.clipboard`; paste by the paste event of the browser; the look and feel of the platform of the browser (Cmd on a Mac) | paste from the menus sees the last paste or copy only |
| remote (TeXmacs server) | client over WebSocket: login, remote files, directories; the servers serve WebSocket clients | `wss` (TLS for the WebSocket); a connection which fails is reported as aborted |

## Windows and the frame of the page

In single-window mode the only SDL window, the host, is a container with
nothing of its own; every window of TeXmacs is virtual. The windows of the
editors are tabs: each fills the host and only the active one is drawn and
gets the events (dialogs, tools, balloons and popups float above it). A
dialog has a title bar, to move and close it, and a frame of 4 points: its
edges and corners resize it (with the double arrows of the system as the
pointer), within the host and the size limits of its contents. Once a
dialog has its size, its contents are laid out in a container which clips
and scrolls (`vue_plain_window_widget_rep::do_layout`): at their own size
at least, larger when the dialog is, with scroll bars and the wheel when
it is smaller (after a resize, or on a page smaller than the dialog, which
is then made to fit). In
the browser the page has a frame above the canvas (`misc/wasm/frame.js`):
the tabs, labelled with the names of the windows (the title of a window on
the desktop, and the title of the page for the active one), with a marker
for unsaved changes, a close box (not on the last tab: TeXmacs asks as for
a window whether to save), a `+` for a new window, and a TeXmacs menu: what
this TeXmacs is (version, S7, MuPDF, build date), where its files are, how
many of its packages have come, the storage used, a popup with more info
and the limitations of the port (the keyboard, the files, what is
missing), the Files panel, reload, a reset (the files kept by the browser
deleted) and a removal from the browser. The plugin tells the
frame of the tabs once per frame when they changed (`frame_sync`); the
frame asks it to show, close or open one. Quitting TeXmacs reloads the
page (after the home directory is written to the storage of the browser). Presentation mode hides the frame and asks the browser for the full
screen (`tmFrame.fullScreen`, from `vue_virtual_window_rep::set_full_screen`):
the browser grants it only shortly after an action of the user (the key or
the menu), otherwise the slides take the whole page; leaving the full screen
from the browser (Escape) leaves presentation mode.

On the desktop, `TEXMACS_VUE_SINGLE_WINDOW=1` gives the same, without the
frame: a tab asks the host to change its size and position, so that it
looks as before; the scripted tests have `tab <id>` to show a tab.

## Building and running

The CI of GitHub (`.github/workflows/wasm.yml`, at the top of the
repository) runs on the branch `vue_ci` only: the work goes on in
`wip_wasm_vue`, which triggers nothing, and a state is built, tested and
published by moving `vue_ci` to it,

    git push origin wip_wasm_vue:vue_ci

(or by hand, from the Actions tab). It builds with Emscripten 6.0.10
(`emsdk`) the slim MuPDF with its patches (cached), the page and the node
build, runs a smoke test (the node build turns the Welcome document into a
PDF), keeps the page as an artifact of the run (`texmacs-wasm-web`: unzip
it and serve it with `node misc/wasm/serve.mjs <dir>`), and publishes it at
https://mgubi.github.io/texmacs/ (GitHub Pages, source "GitHub Actions";
the environment `github-pages` allows the branch `vue_ci`). With emsdk,
`emenv.sh` keeps the configuration of emsdk.

Pages sends files as they are, without the brotli copies of `serve.mjs`:
the build also writes gzip copies of `texmacs.wasm` and of the packages,
which the page decompresses itself (`DecompressionStream`, in `progress.js`
and `packages.js`), 6.2 MB for the program instead of 22.8; the packages
stay as they are too, for the byte ranges of a file needed before its
package. `index.html` is the page.

Locally:

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

## A viewer: `texmacs.html?open=<url>`

The page opens the document at `<url>` once TeXmacs runs, in a tab of its
own, which is the active one: a link to the page with a document in it
makes a viewer of TeXmacs documents on the web
(`texmacs.html?open=https://example.org/paper.tm`; the url is encoded as a
parameter, `%20` for a space). `files.js` fetches it while TeXmacs loads
and opens it when `tmProgress.running` says TeXmacs runs.

- The url is relative to the page or absolute; another site has to let
  the page read it (CORS, `Access-Control-Allow-Origin`), or the page says
  it cannot open it, and why.
- Any format TeXmacs opens (`.tm`, `.tex`, `.html`, `.md`...); a name
  without one of their suffixes is opened as `.tm`.
- The document is kept in `/tmp/web`, in memory, not in the home directory
  of the page: a document viewed is not one of the user's (Save as puts it
  among them).
- What the document refers to (images, included files, a style of its
  own) is not fetched with it.

Other options of the address, joined with `&` (`URLSearchParams`: a
value is written as a parameter, `%26` for `&`):

- `x=<command>`: a Scheme command, as `texmacs -x`, run once TeXmacs runs,
  after the document of `open` (as `-x` after the files of the command
  line); several run in their order. A link is anyone's and a command can
  change or delete the files kept in the browser, so the page shows the
  commands and asks first (`tmFrame.ask`); they go to TeXmacs through
  `_vue_web_scheme` (`files.js`).
- `debug=<flags>` (`-debug-<flag>`, joined with commas; `std` is `-d`) and
  `verbose` (`-V`): options of the command line, which `web-pre.js` puts in
  `Module.arguments`.
- `profile=<n>`, `trace-files`, `trace-clipboard`, `no-background`: for the
  development of the page (see below).

The menu of the TeXmacs Vue button has "Address of the page: open a
document, options…": the options of the address, each with a line to copy,
and a field which makes the link opening a document (`frame.js`,
`addressOptions`). While that dialog is open, or text of the page is
selected, `clipboard.js` leaves the keys to the browser (Cmd+C copies).

## The network

A page runs no program, and what wget and curl do elsewhere is done by the
browser (`web_files.cpp`): `get_from_web` (documents, pictures, styles and
files included by a URL, DOI links) with a synchronous `XMLHttpRequest`
into the temporary file TeXmacs reads; `http_post` and its variants with a
synchronous request, and `async_http_post` and its variants (LanguageTool,
the AI tools) with `fetch`, whose answer the main loop takes
(`async_eval_pending`) and hands to the callback as it does the output of a
program. The bodies are those curl sends (`--data-binary`,
`--data-urlencode` with its `name@file`). A failed download is not asked
for again for ten seconds (loading a document asks for it several times).

- The site has to let the page read its answer (CORS,
  `Access-Control-Allow-Origin`): raw.githubusercontent.com does,
  www.texmacs.org does not; the console says so when it is missing.
- A synchronous request holds the page until the answer comes.
- The browser does not let the page set some headers (User-Agent).

The links which TeXmacs leaves to the system (`load-external`: a page of
the web, a mail address, a PDF or a picture) go to the browser through
`web-open-external` (`misc/wasm/print.js`): a page in a new tab, a mail
address to the mail program, a file of the page in the viewer of the browser
(PDF, pictures, text) or downloaded. When the browser refuses the tab (too
long after the click), a notice offers to open it.

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

## Storage, and the files of TeXmacs

The page keeps two things in the browser: the home directory (IndexedDB,
`web-pre.js`) and the packages of TeXmacs (the Cache Storage,
`packages.js`), some 2 and 44 MB. The TeXmacs Vue menu counts them
itself (`storageUse` in `frame.js`), and not with
`navigator.storage.estimate`: Safari's count grows by the size of each
download and does not go down when the data is deleted (measured: 46 MB
after a first load, still 46 MB once the caches and the databases were
deleted, 92 MB after the reload which fetched the 46 MB again), so that
it reached hundreds of MB for some 46 kept. The temporary
directory of TeXmacs (`.TeXmacs/system/tmp`) is emptied at each start: a
page never quits, where TeXmacs empties it, and its process always has
the same number, so that the pictures of every session piled up there.

- **Reset…** deletes the storage of the page and reloads it.
- **Remove from this browser…** (with a confirmation) deletes it and stops
  TeXmacs: no more saves of the home directory (`tmStorageRemoved`), its
  loop paused, its connection to the database closed (an open database is
  not deleted); the page says so, and a reload starts afresh.

The Files panel shows the files of TeXmacs too (**Files of TeXmacs**:
`/texmacs`, its styles, packages, Scheme code, documentation), which are
not changed there: opened, downloaded, or copied into the TeXmacs folder
of the user with **customize** (`styles/article.ts` becomes
`.TeXmacs/styles/article.ts`), where TeXmacs looks first and where the
copy can be edited.

## The TeXmacs server, from the browser

The collaborative tools (the Remote menu: remote files and directories,
shared documents, chat) talk to a TeXmacs server over TCP, on port 6561,
with TLS (GnuTLS) within the connection. A page has no TCP: Emscripten
makes the sockets of the program WebSockets (`connect` to host:port opens
`ws://host:port/`, subprotocol `binary`), so the page is a client of any
server which speaks WebSocket on its port -- a TeXmacs server of this
branch does.

- **Server** (`src/System/Link/websocket_contact.cpp`): the contact of a
  new client looks at its first bytes; `GET ` opens the handshake of a
  WebSocket (the key through SHA-1 and base64, the subprotocol `binary`
  echoed), and the data goes in binary frames (masked by the client,
  control frames answered). Any other client gets the contact of before
  (TLS or plain, preference `tls-server`), unchanged. A WebSocket client
  has no TLS of its own within the WebSocket: the connection is encrypted
  by `wss`, or local. The preference `server websocket` says which are
  served: `local` (the default: the clients of the same machine), `on`
  (any: behind a proxy which does the TLS of `wss`), `off`. The same in
  the Qt port (`Plugins/Qt/QTMSockets.cpp`). The link now reads its
  contact until it has no more data (`data_set_ready`): a WebSocket frame
  (or a TLS record) may hold more than one read, and the socket does not
  say it is readable again for those.
- **Client** (the page): `try_connect` does not wait for the connection
  (the WebSocket opens only when the page has the hand again; what is
  written before is queued), and a login "Password via TLS" goes through a
  plain contact (`tls_client_start`): accounts need nothing new.
- **S7** (on which the desktop builds of this branch run, as the page):
  the server logged a failed login with Guile's `strftime`, and formatted
  its errors with Guile's `display-error` (`format-err`): both fixed.

Tests (`misc/wasm/remote/`): `server.scm` makes a test server (admin /
secret123, TLS with a self-signed certificate) of a desktop build,

    TEXMACS_HOME_PATH=<a copy of ~/.TeXmacs> TeXmacs/bin/texmacs.bin \
      -headless -server -x '(load "misc/wasm/remote/server.scm")'

`client.mjs` logs in over WebSocket from node; `tls-client.scm` from a
desktop client over TCP and TLS (with `-tls-no-verify`: `TM_ARGS` of the
test runner of Vue); `browser-home.txt` and `browser-create.txt`, scripts
of `browser-run.mjs`, from the page: the login, the home directory, a
remote file created and opened (`_vue_web_scheme` runs a Scheme command
of the page). Checked on 2026-09-27: all of them, a WebSocket client from
another address refused with `local` and served with `on`, none with
`off`; the desktop Vue and the Qt (compiled) ports.

Next: `wss`. A page served over https can open `wss://` only, with a
certificate the browser trusts: the TeXmacs server doing TLS itself for
its WebSocket clients (GnuTLS, a real certificate), or behind a proxy
(Caddy, nginx) with `server websocket` on.

## The files of TeXmacs in the page

`misc/wasm/package.py` writes the files of `TeXmacs/` (without `bin/` and
the programs and documentation of the plugins: 62.6 MB) as packages with a
manifest, `texmacs-files.json` (each file: its package, offset, size).
`misc/wasm/packages.js` makes the whole tree at `/texmacs` before TeXmacs
starts, every file a placeholder of its size, and loads the boot package;
the others (icons, languages, documentation, the rest, in pieces of 4 MB)
come one after the other once TeXmacs runs. A file read before its
package fetches its bytes alone, a range of the package (synchronously, as
text in the user defined charset: TeXmacs reads its files synchronously),
so that TeXmacs never finds a file of its tree missing, nor records it as
such. The packages are kept in the Cache Storage of the browser.

Some servers compress a package as they send it and cut the range out of
what they compress: GitHub Pages answers a range with a range of the gzip
of the package (`content-encoding: gzip`), or 416 beyond its size, when the
browser accepts gzip (Chrome, Safari; Firefox asks ranges without it). The
first such answer (or a range which fails) makes `packages.js` fetch whole
packages instead: the package of the file is fetched at once, which the
server compresses whole and the browser decodes, and all its files are
installed.

The fonts are in no package (they were two thirds of the whole: 42 MB, 28
with brotli, the Type 1 fonts compressing poorly). Each OpenType and Type 1
file of `fonts/truetype/` and `fonts/type1/` is a file of its own,
`tm-font-<digest>.<ext>`, listed in the manifest under `lazy` (its path, its
file, its size); only those of the boot list are in the boot package. Its
placeholder fetches it whole when TeXmacs first reads it (a plain request,
no range: it works on GitHub Pages as anywhere), fills the other
placeholders of the same font (two paths, one digest), and puts it in the
Cache Storage; before TeXmacs starts, the fonts found there are put in place
(`restoreFonts`). A font no document uses is never fetched, and a font is
fetched once: a document in Libertinus fetches `LibertinusSerif-Regular.otf`
(337 KB) the first time, and nothing the next visits.

The boot package is the files TeXmacs opens when it starts
(`misc/wasm/boot-files.txt`, the list of `?trace-files`: boot, the welcome
document, a new document with text and a formula) and some whole groups
read at unforeseeable times (the Scheme code, styles, packages, the metrics
of the fonts, the icons of the default set, neoclassical, in the light
theme): 15.9 MB, 4.0 MB with brotli.
To make the list again: load the page with `?trace-files`, use it, and
save `window.tmTrace` (see `build-wasm/trace.txt` of the notes below).

Measured in a headless Firefox with `misc/wasm/serve.mjs` (brotli, ranges,
304 for what did not change):

| | transferred |
|---|---|
| before TeXmacs starts (program + boot package) | 9.3 MB |
| the other packages, in the background (11 packages, 2.4 s locally) | 8.5 MB (33.3 MB when they had the fonts) |
| a font, the first time a document uses it | its file: 0.1 to 1 MB |
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
