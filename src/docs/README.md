# Developer notes

Notes on the additions of TeXmacs Upsilon: designs, status, audits, plans
and how-tos, written along the work for whoever continues it. What the
additions are, for users, is in the [README](../../README.md) of the
repository; the developer documentation inside TeXmacs (Help ▸ Developer
documentation) is in [`TeXmacs/doc/devel/`](../TeXmacs/doc/devel/), and the
tests in [`tests/`](../tests/README.md).

Paths in these notes are relative to the root of the repository unless they
say otherwise; C++ is under `src/src/`, Scheme under `src/TeXmacs/progs/`.

| File | Contents |
|---|---|
| [TODO.md](TODO.md) | **Open work on `upsilon_texmacs`**: PRs waiting, bugs found and not fixed, offers not taken up |
| [upsilon-screenshots.md](upsilon-screenshots.md) | TeXmacs Upsilon in pictures |

## Interfaces

| File | Contents |
|---|---|
| [texmacs-gui-architecture.md](texmacs-gui-architecture.md) | How the TeXmacs core talks to a GUI plugin: widget factories, slots, Scheme markup, tools, dialogs, the client/server sockets, the other entry points |
| [vue-graphics-stack.md](vue-graphics-stack.md) | The Vue plugin: windows, event loop, layout with Clay, rendering, input model and drag and drop, icons, themes, animation, error handling, conventions |
| [vue-widgets.md](vue-widgets.md) | Catalogue of the widgets implemented in Vue, their layout rules and callback protocols |
| [vue-interactive-footer.md](vue-interactive-footer.md) | The interactive status bar (View › Interactive status bar): the properties at the cursor as menus, the tags around it as buttons which select them |
| [typographic-palettes.md](typographic-palettes.md) | The typographic palettes of the colour menus (Vue): the sets, and how to define your own |
| [vue-testing.md](vue-testing.md) | Snapshot and scripted-event harness of Vue, how to run and write tests |
| [build-and-debug.md](build-and-debug.md) | Configuring, building, dependency tracking, crash reports, pitfalls met along the way (Vue) |
| [ns-port.md](ns-port.md) | The native Cocoa interface: status of the port, test aids, packaging |
| [editor-frontend-separation.md](editor-frontend-separation.md) | Design, not implemented: how the editor depends on the GUI and the window layer, and how to separate them in stages; draft headers in [editor-frontend-separation/](editor-frontend-separation/) |
| [pdf-output-with-mupdf.md](pdf-output-with-mupdf.md) | Writing the PDF with MuPDF instead of PDFHummus: what MuPDF offers, the font subsetting which blocks it, a working prototype |
| [icons/](icons/README.md) | The icon sets: what they look like, how a set is chosen, the scripts that generate them, specimen sheets |
| [wasm/](wasm/README.md) | TeXmacs in the browser: build, design, tests, plug-ins in the page |
| [screenshots/](screenshots/) | Screenshots of the interfaces and the document they show |

## Fonts and mathematics

| File | Contents |
|---|---|
| [OPENTYPEMATH.md](OPENTYPEMATH.md) | OpenType mathematics: an overview, what is implemented, building and testing, a specimen of every profiled font ([opentype-math/](opentype-math/)) |
| [opentype-math-design.md](opentype-math-design.md) | Its design and status log, and what is still missing |
| [opentype-math-fonts-survey.md](opentype-math-fonts-survey.md) | The OpenType math fonts, measured one by one, and why these are shipped |
| [opentype-audit.md](opentype-audit.md) | Audit of the OpenType work (2026-10-13) |
| [font-system-review.md](font-system-review.md) | The TeXmacs font system as a whole |
| [math-symbol-coverage.md](math-symbol-coverage.md) | The mathematical symbols TeXmacs cannot name |

## Scheme interpreters

| File | Contents |
|---|---|
| [s7/](s7/README.md) | TeXmacs on s7: the binding, the boot and the module system, the compatibility layer, open issues, performance |
| [femtolisp/](femtolisp/README.md) | TeXmacs on femtolisp: the binding, modules, the compatibility layer, the browser build |

## Documents and references

| File | Contents |
|---|---|
| [git.md](git.md) | **The Git support**: index of its notes, and the state of the branch `wip_git` |
| [git-features.md](git-features.md) | What the Git support adds: the features by menu, structured merges, safety, APIs, tests, limitations |
| [git-implementation.md](git-implementation.md) | How the Git support is organised: modules, data formats, conventions |
| [git-ui-design.md](git-ui-design.md) | UI design proposal for the Git support |
| [git-plan.md](git-plan.md) | The plan for full Git support, in phases |
| [git-audit.md](git-audit.md), [git-audit-2.md](git-audit-2.md) | Audits of the Git support (2026-09-24 and 2026-10-05) |
| [git-manual-audit.md](git-manual-audit.md) | Audit of the manual chapter *Working with Git* |
| [vcs-current-state.md](vcs-current-state.md) | How the older SVN and Git support works, file by file |
| [texmacs-internals.md](texmacs-internals.md) | The TeXmacs machinery that version control depends on: modules, `tmfs://`, buffers, menus, widgets, processes, document comparison |
| [tests/](tests/) | `run-git-tests.sh`: the headless suites `git` and `version`, or with `--gui` the offscreen `git-gui-test.scm` |
| [zotero-design.md](zotero-design.md) | Zotero as a source of references: design |
| [csl-design.md](csl-design.md) | The CSL processor: design and status |
| [converters-csl-audit.md](converters-csl-audit.md) | Audit of the Scheme code of the converters and of the CSL processor |

## Program code

| File | Contents |
|---|---|
| [code-features.md](code-features.md) | Tree-sitter and language servers together: the common mechanism, and how a build degrades without either |
| [tree-sitter.md](tree-sitter.md) | Tree-sitter in TeXmacs: survey and design |
