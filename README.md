# <img src="src/TeXmacs/misc/images/texmacs-vue-256.png" alt="The logo of TeXmacs Vue" width="44" align="top"> TeXmacs Vue — GNU TeXmacs in the browser

[![WebAssembly](https://github.com/mgubi/texmacs/actions/workflows/wasm.yml/badge.svg?branch=vue_ci)](https://github.com/mgubi/texmacs/actions/workflows/wasm.yml)

**Try it: <https://mgubi.github.io/texmacs/>** — an *experimental* port of
[GNU TeXmacs](https://texmacs.org) 2.1.5 which runs in a web page (branch
`wip_wasm_vue`). Nothing is installed, and nothing leaves the browser unless
you download it.

![TeXmacs Vue in the browser: tabs for the documents, the tool bars, a formula](src/docs/wasm/texmacs-in-the-browser.png)

* **What it is**: stock TeXmacs compiled to WebAssembly, on **Vue**, a new
  interface for TeXmacs (Clay, SDL3 and MuPDF, below), with the
  [S7](https://ccrma.stanford.edu/software/snd/snd/s7.html) Scheme in place of
  Guile and **OpenType fonts**, OpenType mathematics included.
* **What works**: editing and typesetting, the menus and dialogs (drawn in
  the page), a tab per document, your files kept in the browser (upload,
  drag and drop, zip projects, downloads), printing (the PDF opens in a tab
  of the browser), the clipboard of the system, Scheme sessions, and the
  Remote menu (a TeXmacs server over WebSocket).
* **Not yet**: plugins which run programs (a page has no processes), `wss`
  for TeXmacs servers on other machines, resizing the dialogs. Tested in
  Firefox and Safari.
* **First visit**: some 11 MB before it starts (the program, and the files
  it needs to boot), the rest in the background; a second visit loads
  nothing.
* **Published** by the CI, which runs on the branch `vue_ci` only: the work
  goes on in `wip_wasm_vue` without triggering it, and a state is built,
  tested and published with `git push origin wip_wasm_vue:vue_ci`. Each run
  also keeps the page as an artifact (`texmacs-wasm-web`).
* **Build it**: see [`src/README.md`](src/README.md) and, for the design,
  the notes and the plan, [`src/docs/wasm/`](src/docs/wasm/README.md).

## The Vue interface (desktop), branch `wip_vue`

Work in progress: a graphical back end for [GNU TeXmacs](https://texmacs.org)
which owes nothing to a widget toolkit. The **Vue** plugin draws the editor,
the bars, the menus, the dialogs and the tools itself, on three libraries —
[Clay](https://github.com/nicbarker/clay) for the layout, SDL3 for the
windows, the input and the clipboard, and MuPDF for the pixels.

Everything outside the plugin and its notes is stock TeXmacs, save for the
few places which had to learn that the GUI is neither Qt nor X11.

* Code: [`src/src/Plugins/Vue/`](src/src/Plugins/Vue/), with its `TODO` and
  its `tests/`
* Developer notes: [`src/docs/`](src/docs/README.md) — the graphics stack,
  the widgets, the test harness, how the TeXmacs core talks to a GUI plugin,
  and how to build and debug it
* Build: `./configure --with-gui=vue --with-sdl3 --with-mupdf=<prefix>`,
  from `src/`

## This repository

The layout is the one of the TeXmacs SVN trunk, which this is a mirror of:

| Directory | Contents |
|---|---|
| [`src/`](src/README.md) | the editor: sources, Scheme, styles, documentation, packaging. **Its [`README.md`](src/README.md) is the README of the project** |
| `misc/` | build scripts, plugins and other odds and ends |
| `web/` | the sources of the web site |
| `guile-texmacs/` | the vendored Guile 1.8 |
