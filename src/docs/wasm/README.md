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
| build | `misc/wasm/Makefile`, the slim MuPDF 1.28.5, S7, SDL3 3.4; `-O2` (`-Os`, `-Oz` and LTO measured: see Optimization) | |
| loop | one iteration per frame (`emscripten_set_main_loop`); the keyboard events queued in a frame all handled in it (`web_more_events`: a layout, the commands and the interpose handler for each, one repaint and one redraw), 80 keys in one frame where they took 2 s at 120 Hz | |
| files | packages: 9.3 MB before the start, the rest in the background; the home kept in IndexedDB; the Files panel, uploads, downloads, drops | |
| processes | `posix_spawnp` fails cleanly; the plugins which run a program are not offered (their `:require` sees no command), and one started anyway fails at once, its session dead with an error (it froze the page: `fork` fails, and the pipes were read again and again); Scheme sessions work | no external converters offered |
| file dialogs | the Files panel of the page | |
| fonts | Fira for the interface (the TeX fonts lack its arrows) | |
| clipboard | copy (text, HTML) with `navigator.clipboard`; paste by the paste event of the browser; the look and feel of the platform of the browser (Cmd on a Mac); Edit > Paste from browser... (a dialog of the page: one Paste button) | |
| python | sessions and folds of Python in a Web Worker (`plugins/python/web/tm-python.mjs`): Pyodide 314.0.7 (Python 3.14) loaded from jsDelivr by the first input, the packages of an input from its imports; the value of the last expression (LaTeX for SymPy), the figures of matplotlib as SVG, top-level `await`; an interrupt stops the worker while Python runs (`{busy}` in `workers.js`: no SharedArrayBuffer to interrupt it) | Pyodide served with the page (offline) |
| r | sessions and folds of R in a Web Worker (`plugins/r/web/tm-r.mjs`): webR 0.6.0 (R 4.6) loaded from webr.r-wasm.org (the npm build imports node's `module`), its PostMessage channel (no SharedArrayBuffer); each input through `captureR` with autoprint, the plots of its canvas device as PNG in an SVG (MuPDF draws the image), `install.packages` by `webr::shim_install`; an interrupt stops the worker while R runs (`{busy}`) | |
| spelling | Hunspell 1.7.2 in the program (`src/Plugins/Ispell/ispell_hunspell.cpp`, `USE_HUNSPELL`, sources fetched by `misc/wasm/get-hunspell.sh`): TeXmacs asks a word at a time and waits, which a worker cannot answer; the dictionary of a language (`dictionary-<code>` of wooorm/dictionaries on npm, through jsDelivr) fetched by the first check, kept in `~/.TeXmacs/system/dictionaries` with the words inserted (`personal-<code>.txt`) | |
| remote (TeXmacs server) | client over WebSocket: login, remote files, directories; `wss` from a page over https; the servers serve WebSocket clients, over TLS too; a failed connection says where it went and why it may have failed | |

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
is then made to fit). In the browser, where the loop never waits, a
resize which comes from an event of the page (the page resized, the column
of the tabs dragged) is handled at once by `event_filter` too, outside an
iteration of the loop: a change of the size of the canvas clears it, and
left to the next frame it showed an empty canvas during a drag (the frames
with events waiting do not repaint the editors).
The page sends a resize only when the place of the canvas changed: SDL
sets the size of the canvas at each resize event, which clears it even
when the size stays, but tells TeXmacs only of a new size (a move of the
mouse which left the width as it was, at a limit or finer than a pixel at
density 2, emptied the canvas until the next change).
`misc/wasm/test/resize-flicker.mjs` counts such frames, and the frames
whose canvas is stretched (0 in Firefox and Chrome, at density 1 and 2,
and in Safari; 116 of 270 before). In
the browser the page has a frame, a column at the left of the canvas
(`misc/wasm/frame.js`), which leaves the whole height to TeXmacs: the tabs,
one under the other, labelled with the names of the windows (the title of a
window on the desktop, and the title of the page for the active one), with
a marker for unsaved changes, a close box (not on the last tab: TeXmacs
asks as for a window whether to save), a "New window", a right edge which
changes its width (100 to 480 pixels and half the page at most, remembered
by the browser; a double click gives the 200 pixels back; TeXmacs follows
the width once per frame during the drag; a drag below 80 pixels folds the
column, keeping the width it had before the drag, and a drag of the folded
column beyond 100 pixels opens it again; the resize is sent from the move
of the mouse, and TeXmacs draws it at once, see below), a chevron which
folds the column to 44 pixels (the logo, and small tabs with the initials
of the windows, or their numbers, "N2" for "No name [2]", whose names show
in a balloon; the browser remembers it, and a page narrower than 900
pixels starts folded), and a TeXmacs menu: what
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

Pages has no brotli: the build also writes gzip copies of `texmacs.wasm`
and of the packages, which the page decompresses itself
(`DecompressionStream`, in `progress.js` and `packages.js`), 6.2 MB for the
program instead of 22.9; the packages stay as they are too, for the byte
ranges of a file needed before its package. `index.html` is the page.
Pages gzips the files it sends anyway (`content-encoding: gzip`, even the
packages, 6.4 MB for the program), so that the copies save it 2 %: they
are for the servers which send files as they are.

What the compression is worth (2026-10-01, a first visit in a headless
Firefox, `browser-run.mjs`, the time until TeXmacs runs): at 2 MB/s a file
(`--slow 2000`), 8.3 s with the gzip copies, 7.6 s with brotli from the
server (`serve.mjs`, the copies removed), 22.7 s with nothing compressed;
at full speed, on the same machine, 2.6 to 2.7 s in all three cases, so
that the decompression costs nothing measurable. The compression is what
makes a first visit bearable; brotli over gzip is a tenth less, where the
server has it.

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
- Each object has the list of the headers it includes (`obj/....d`, from
  `-MMD -MP`): a change of a header, of `config.h` or of the headers of
  MuPDF compiles again the objects which include it. (Before, an object
  built against the old layout of a class stayed, and the program stopped
  at startup with "indirect call signature mismatch".) Objects built before
  have no list: `rm -rf build-wasm/obj` once.
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
  a panel with the icon of TeXmacs Vue, the version (`@TM_VERSION@` in
  `shell.html`, from `tm_configure.hpp`) and three lines about the
  program; a bar with the share downloaded and the time left; and the list
  of the operations, each waiting, running (its share), or done (its
  time): the program (fetched by `Module.instantiateWasm` to count its
  bytes, compiled by the browser as they come), the files (the boot
  packages, `packages.js` reports their bytes), and the boot of TeXmacs,
  before which the page is painted (a run dependency of its own, removed
  after two frames; the boot holds the page, so nothing moves during it).
  The shares are those of the bytes uncompressed: the build writes the
  size of `texmacs.wasm` into the page (`@TM_WASM_SIZE@`), since a
  compressed response does not give it.
- The manifest and the boot packages are fetched as soon as `packages.js`
  runs, while the program comes and compiles; only their installation
  waits for `preRun`. They used to be fetched in `preRun`, after the
  program had been compiled: the two downloads followed each other. `serve.mjs [dir] [port] [KB/s]` and
  `browser-run.mjs --slow <KB/s>` load the page as over a slow network.

## Drawing with the GPU (the default; `texmacs.html?gpu=0` for MuPDF)

A build with ThorVG draws the windows with WebGL2 instead of MuPDF (see
*The GPU renderer* in [../vue-graphics-stack.md](../vue-graphics-stack.md)):

```sh
sh misc/thorvg/build-thorvg.sh build-wasm/thorvg wasm  # found by the Makefile
make -C build-wasm -f ../misc/wasm/Makefile ... web    # or THORVG=<dir>/wasm
```

(the CI does the same, with a cache). The page draws with the GPU (it sets
`TEXMACS_VUE_GPU`) unless its address has `?gpu=0` or the browser has no
WebGL2, where it draws with MuPDF as before; a build without ThorVG draws
with MuPDF whatever the address says. Measured in
headless Firefox on an Apple M1 (Retina, the document of 200 paragraphs of
`TeXmacs.later`-driven forced repaints, `?profile=20`): a full repaint of
the editor takes 2.2 ms of CPU (3.5 ms with `?gpusync=1`, which makes the
profile wait for the GPU) against 6.6 ms with MuPDF (19.1 ms before the
work of October 2026 on the MuPDF renderer), and the canvas upload of
every frame (2 ms) is gone. Scrolling and zooming the math font catalogue,
the GPU path draws more frames than MuPDF in every phase, at 1 to 2.3 ms
of CPU a frame against 4.4 to 5 ms.

`?slug=1` (with `?gpu=1`) draws the glyphs from their outlines instead of
from bitmaps (Slug, see *The GPU renderer*): on the same benchmark the
repaints at zoom 2, where the bitmaps of the glyphs are made, cost 0.4 ms
of CPU a frame instead of 1.2 ms, and the other phases are the same or a
little faster.

## Optimization

`OPT` of the Makefile (`-O2` by default) is that of the compilation and of
the link of TeXmacs; MuPDF is built apart (`build-mupdf.sh`, release), so
LTO covers the code of TeXmacs only. Measured on 2026-10-02 (the program
alone; the speed in headless Firefox, the CPU time by the node build
turning the Welcome document into a PDF, warm runs):

| `OPT` | wasm | gzip | brotli | PDF | rebuild after one change |
|---|---|---|---|---|---|
| `-O2` | 22.97 MB | 6.23 MB | 5.32 MB | 4.8 s | 15 s |
| `-Os` | 21.84 MB | 6.24 MB | 5.26 MB | | |
| `-Oz` | 15.11 MB | 5.48 MB | 4.84 MB | | |
| `-O2 -flto` | 30.39 MB | 6.54 MB | 5.29 MB | | |
| `-Os -flto` | 19.11 MB | 5.76 MB | 4.89 MB | 4.7 s | 70 s |
| `-Oz -flto` | 12.92 MB | 5.02 MB | 4.48 MB | 5.4 s | 70 s |

In the browser all of them are as fast, within the noise of the runs
(the frames of a scroll and of typing at 2x, the typesetting and the
repaints of a page of formulas); the startup is that of the download and
of the compilation, a little shorter for a smaller program. The node build
shows what the browser does not: `-Oz` costs some 12 % of CPU time. LTO
compiles the whole program again at each link, hence the rebuild.

All the builds keep `-O2`: `-Os -flto` would save 8 % of the download
(gzip, what GitHub Pages sends) at the same speed, `-Oz -flto` 19 % for
12 % more CPU time, not worth a slower or separate build. The page and the
PDF are the same with all of them.

## Speed against the desktop (2026-10-05)

Measured in headless Firefox (warm: the profile of the browser kept, as
for a user who comes back) and in the desktop build of Vue (the same
program, native, `-O2`), on an Apple M1, with a document of 100 sections
of text and formulas (`?profile=10`, `TEXMACS_VUE_PROFILE=10`):

| | browser | desktop (native) |
|---|---|---|
| typesetting the document (25 pages) | 442 ms | 324 ms |
| typing 95 characters (interpose + layout + redraw) | 1689 ms (17 ms a key) | 1592 ms |
| 30 page-downs | 555 ms | 737 ms |
| pure Scheme (a list of 200000 strings sorted) | 119 ms | 48 ms |
| the document to LaTeX | 301 ms | 170 ms |
| start, once the program and the files are there | 0.7 s | 0.6 s |

Editing is as fast as on the desktop; S7 is 2 to 2.5 times slower in
WebAssembly (`-O3` for `s7.c` changes nothing measurable), which shows in
what is mostly Scheme. The start was 1.2 s more until the files of TeXmacs
got one time per build (`packages.js`, `buildTime`): with the time of the
visit they all looked changed at each start, the font database of the home
was merged again (`shipped_fonts_changed`), its save emptied the caches
filled during the boot (`cache_refresh`: the CJK fonts which
`fonts-truetype.scm` looks for, 35 ms each when not cached) and the
directories were never up to date (22000 `stat` at each start, now 2300).
Most of the slowness of S7 is that of Firefox: in Chrome (154, headless,
`browser-run.mjs --browser <chrome>`) the same build gives 58 ms for the
pure Scheme (Firefox 119, native 48), 210 ms for the LaTeX (Firefox 301,
native 170), 1302 ms for the typing and 188 ms for the page-downs; the
start of a later visit is the same (0.94 s). In Safari (26.0.1, driven by
`safaridriver`, its WebDriver: Develop > Allow Remote Automation) the pure
Scheme takes 42 ms once warm (90 ms the first time), the LaTeX 213 to
246 ms; a start of a later visit, from the navigation to the first answer
of Scheme, 1.2 s. The standardized encoding of
the exceptions of WebAssembly (`-sWASM_LEGACY_EXCEPTIONS=0`, S7 uses them
for its `setjmp`) was measured in Firefox: the LaTeX 28 % faster, the
typing and the scrolling some 10 %, the pure Scheme slower, a program 15 %
larger, and no older browser (Safari before 18.4); not kept.

`?debug=bench` prints the steps of the start. After the start, three modules
are loaded when the user is idle (`math-adjust-en`, `math-adjust-fr`,
`tmtex-widgets`), about 100 ms each.

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

## The wallet

The wallet of TeXmacs (`progs/security/wallet`) keeps passwords and keys
encrypted; on the desktop GnuPG encrypts it, which a page cannot run. In the
browser `web-wallet.scm` and `misc/wasm/wallet.js` do it with WebCrypto:

- a random data key (AES-GCM, 256 bits) encrypts the table of the wallet;
- the data key is kept wrapped by a key derived from the passphrase
  (PBKDF2, SHA-256, 600000 rounds) and maybe by one derived from a passkey
  (the PRF extension of WebAuthn: Touch ID, Windows Hello, a security key);
- the file, `~/.TeXmacs/system/wallet/browser-wallet.json`, holds only what
  is encrypted, the salts and the id of the passkey. While the wallet is on,
  the table is in Scheme and the data key in the page; turning it off
  forgets both.

WebCrypto and WebAuthn are asynchronous: `tmWallet` answers with
`(web-wallet-answer id ok? text)` through `TeXmacs.later`, and the
dialogues of `wallet-menu.scm` wait for it in the browser (they call
`web-wallet-create`, `-unlock`, `-unlock-passkey`, `-change-passphrase`,
`-add-passkey` with a procedure). `wallet-base.scm` uses the web wallet when
`web-javascript` is defined. The Security tab of the preferences is shown in
the browser, without the part on GnuPG. `wallet-add-on-hook` tells the
plug-ins which take their keys there (the AI ones) that it was turned on.

A page of `mgubi.github.io` shares its storage with every other page of that
origin: what the wallet keeps is encrypted by a key which is not stored.
Tested: the passphrase in Firefox; the passkey in Chrome for Testing with a
virtual authenticator which has PRF (puppeteer, CDP `WebAuthn`).

## The AI plug-ins

`plugins/ai` and `src/Data/Convert/AI/ai.cpp`: every engine is asked by an
`http_post` with a JSON body, sent by the request link of its plug-in
(`request_link.cpp`; with Qt, curl or, in the browser, `fetch`). ChatGPT,
Mistral, Albert, OpenRouter and Ollama (its OpenAI endpoint) use the chat API
of OpenAI (OpenRouter's image models: `modalities`, and `images` in the
answer),
Gemini `generateContent`, Claude the messages API of Anthropic (with
`anthropic-dangerous-direct-browser-access`). The keys come from the wallet,
the preferences or the environment (`ai-api-key` in `init-ai.scm`).

| Engine | Answers a page (CORS) |
|---|---|
| OpenAI, Anthropic, Gemini, Mistral, OpenRouter | yes |
| Ollama | if `OLLAMA_ORIGINS` allows the page |
| Albert | no (405 on the preflight) |

- The Vue server processes the request links in its interpose handler
  (`process_all_requests`; Qt and Cocoa do it with their pipes): without
  it a request session waited forever.
- No answer at all (no network, CORS) is said on the error channel of the
  link, with its URL; an answer wakes the loop of the page.
- `:preferences` and `:session` come before `:require` in the
  `plugin-configure` of the engines, which stops at a failing `:require`:
  the preferences of an engine, where its key is given, exist before it has
  a key. The plug-ins are configured again (`reinit-plugin-single "ai"`)
  when a key changes or the wallet is turned on.
- In the browser the answer of a session or a fold is streamed
  (`"stream": true`, Gemini's `streamGenerateContent?alt=sse`): `fetch`
  reads it as it comes (`slot.parts` in `web_files.cpp`), the request link
  decodes the events so far (`request_link_rep::partial`,
  `ai_stream_text`) and sets their LaTeX as far as all is closed in it
  (`ai_latex_partial`, `ai_latex_closed_prefix`: environments, groups,
  formulas; a verbatim environment as a whole), and the connection gives
  them on the channel
  `"progress"`, which a session shows in grey before the busy sign
  (`session-show-progress`) and a fold through its progress procedure,
  until the output replaces them. A session begins with the engine and its
  model (`set-request-banner!` in `tm-plugins.scm`, used by `plugin-start`).
- The pictures of an answer are set aside before its LaTeX is converted
  (`ai_set_aside`) and put back after (`ai_put_back`): a `tikzpicture`,
  `tikzcd` or `circuitikz` becomes a `script-input` of the TikZ plug-in,
  with the `\usetikzlibrary` and the TikZJax packages of the preamble as
  its first lines, wrapped in `(with "ai-tikz" "pending" ...)` until
  `ai-run-pending-folds` evaluates it once it is in the document; an `<svg>`
  (with its fence and XML declaration) an image of raw data. The answer as
  it came is kept after it, folded (`ai_raw_fold`, preference
  `ai raw answer`), and is what `ai-session-context` sends back.
- The pictures are made as soon as they are complete, in the answer so far
  too (`ai_latex_partial` sets them aside): a TikZ one by a silent
  evaluation of the TikZ plug-in, not in a fold of the document (which the
  next piece replaces), kept by its code (`ai-picture`,
  `ai-picture-request` in `ai-batch.scm`). The fold of a picture which is
  not made yet is pending, filled when it comes (`ai-run-pending-folds`).
- The system prompt of an engine is `ai-instructions` (init-ai.scm): the
  file `~/.TeXmacs/system/ai/<engine>-instructions.txt` when the user has
  one (Instructions, Edit, in the preferences), else a default which tells
  how to write LaTeX which TeXmacs imports well. The options of the lists
  (enumitem, `\begin{itemize}[nosep]`), which the import took for the first
  item, are dropped (`ai_drop_list_options`).
- A fold of a request plug-in, before any session of it, starts its
  connection first (`plugin-connected`, `plugin-starting` in
  `plugin-eval.scm`): it was never made, and the fold waited forever.
- *Update the list of models* (`ai-update-models`) asks each API its models
  with a synchronous GET (an `XMLHttpRequest` through `web-javascript`, curl
  on the desktop) and keeps those which chat in the preference
  `<engine> models`.
- Tested with dummy keys (each service answers its error) and a mock of
  Ollama (`/v1/chat/completions` and `/api/tags`); not yet with real keys
  of every service.

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
- Paste from a menu has no paste event: Edit > Paste pastes what the page
  knows, the last copy or paste, at once. **Edit > Paste from browser...**
  (and Edit > Paste from browser as, with the formats of Paste from, in the
  detailed menus) asks the browser instead, in a dialog of the page
  (`web-paste-dialog`, `vue_gui.cpp`; `tmClipboard.fromBrowser`) with one
  button, Paste. Its click (or Enter) calls `navigator.clipboard.read` (or
  `readText`) in the handler of the gesture, which is the only place the
  browsers allow it (Safari then shows its own Paste button, Chrome asks
  once for the permission): the menu of TeXmacs runs its command a frame
  after the click, where Safari refuses. The paste key in the dialog (or
  the Paste of a long press on a touch screen) is a paste event in a text
  area out of sight, which needs no permission. The dialog has the formats
  of Paste from (Default, Html, LaTeX, Verbatim...), starting on the one of
  the menu; an Html paste takes the HTML of the clipboard, not its text.
  What comes becomes the page's clipboard, and the dialog then runs the
  Scheme command of the paste in the chosen format
  (`clipboard-paste-browser`, `selections.scm`). The entries are
  there only when `web-paste-dialog` is defined (the browser build).

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

The home directory is read from IndexedDB once, before TeXmacs starts
(`FS.syncfs`). After that, the functions of `FS` which change files
(write, truncate, open for writing, mkdir, symlink, rename, unlink, rmdir,
chmod, utime) note the paths of the home directory they touch, and 300 ms
later only these entries are written to the database of IDBFS (or deleted
from it), in one transaction and in its format (`tmHome` in `web-pre.js`).
Before, `FS.syncfs` compared the whole directory with the whole database
every 5 seconds. A tab in the background has its timers slowed down to
1 second; a tab which is hidden or closed writes its changes at once.

One tab writes the home directory: the one which holds the lock
`texmacs-home` (Web Locks). Each tab has its own copy of the directory in
memory, so that two writers would overwrite each other. A tab which does
not get the lock reads the directory but keeps none of its changes, and
says so in a line at the bottom of the page (`#tm-home-notice`, which its
cross hides) and with "(read only)" in its title. **Use TeXmacs here**
asks the tab which has the lock, over a
`BroadcastChannel`, to write its last changes and let the lock go, and
reloads; the reloaded tab waits for the lock (10 s at most) and announces
it (`claimed`). When the lock becomes free otherwise (the tab which had it
was closed), the other tabs offer a reload, after 5 seconds without a
claim (a tab may be reloading to take TeXmacs over). Tested with three
tabs in headless Firefox: the changes reach the database within a second
(files, a renamed folder, a deleted file, a document saved by TeXmacs),
memory and database agree, a read-only tab keeps nothing, and a takeover
keeps the last change of the tab which had TeXmacs
(`node misc/wasm/test/home-tabs.mjs`, after `make ... web`; with `--safari`,
in Safari through its WebDriver; with `--chrome`, in the Chrome for Testing
of `build-wasm/tools/chrome` (installed there by `./node_modules/.bin/browsers
install chrome@stable --path $PWD/chrome`); with `--browser <path>`, in
another browser for puppeteer: passes in Firefox, Safari 26 and Chrome 154).

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
  (TLS or plain, preference `tls-server`), unchanged. A client which
  opens with a TLS record (`0x16`) gets a TLS session first (that of the
  `tls-server` contact, or one of its own when `tls-server` is off), with
  the certificate of the server (`$TEXMACS_SERVER_CERT_DIR/cert.pem`,
  `key.pem`), and its first bytes within TLS say it again: `GET ` is a
  WebSocket over TLS (`wss`), anything else a TeXmacs client over TLS,
  handed to the `tls-server` contact with what was read of it (refused when
  `tls-server` is off). A TeXmacs client speaks first (its login), so this
  never waits for nothing. The preference `server websocket` says which
  WebSocket clients are served: `local` (the default: the clients of the
  same machine), `on` (any: over `wss`, or behind a proxy which does the
  TLS), `off`. The same in the Qt port (`Plugins/Qt/QTMSockets.cpp`). The
  link reads its contact until it has no more data (`data_set_ready`): a
  WebSocket frame (or a TLS record) may hold more than one read, and the
  socket does not say it is readable again for those; for the same reason
  it reads once as soon as the contact is active (`resume_start`): the
  first request of a client over TLS was read to know what it is.
- **Certificate for `wss`**: one the browser trusts, for the name of the
  host (Let's Encrypt...), ECDSA or RSA: the self-signed certificate
  TeXmacs generates (`generate-self-signed-certificate`) is Ed25519, which
  the browsers do not accept for TLS (node does).
- **Client** (the page): `try_connect` does not wait for the connection
  (the WebSocket opens only when the page has the hand again; what is
  written before is queued), and a login "Password via TLS" goes through a
  plain contact (`tls_client_start`): accounts need nothing new. A page
  served over https may open `wss` only (a `ws` from it is blocked as mixed
  content), save to this machine: `try_connect` sets the WebSocket of each
  connection (`SOCKFS.websocketArgs`), `wss://host:port/` to the other
  hosts from a page over https, `ws://` otherwise; `?websocket=wss` (or
  `ws`) in the address of the page says which, whatever the page. The
  browser does not tell a page why a WebSocket failed: a connection which
  fails before any data says where it went, and what may be wrong (no
  server there, `server websocket`, the certificate).
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

`wss` (2026-10-01): `wss-cert.sh <home of the server>` puts an ECDSA
certificate for localhost in place of the Ed25519 one, and
`browser-run.mjs --insecure --query '?websocket=wss'` runs the scripts
over `wss` (the page served over http). Checked: from the page, the login
and the home directory over `wss`, with `tls-server` on and off; from node
(`client.mjs wss://localhost:6561/` with `NODE_TLS_REJECT_UNAUTHORIZED=0`,
and a TeXmacs client over TLS: `tls.connect`, the login packet), on the
same port: `wss`, `ws`, a TeXmacs client over TLS (handed on with
`tls-server` on, closed with it off), `wss` from another address refused
with `local`. The headless desktop client (`tls-client.scm` with
`-headless`) does not finish its TLS handshake: run it with a window.

A server for the page served over https (GitHub Pages): `server
websocket` on, and a real certificate in `$TEXMACS_SERVER_CERT_DIR`
(`cert.pem`: the full chain, `key.pem`), or a proxy (Caddy, nginx) on the
port of the server which does the TLS.

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
