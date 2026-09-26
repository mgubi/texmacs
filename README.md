# GNU TeXmacs — branch `wip_vue`

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
