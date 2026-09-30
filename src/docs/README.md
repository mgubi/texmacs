# Developer documentation

Notes written while porting the TeXmacs GUI to the **Vue** plugin (Clay
layout library + SDL3 + MuPDF). They are meant as references for future work
on the GUI: what the conventions are, how the pieces fit together and where
to look in the code.

| File | Contents |
|---|---|
| [vue-graphics-stack.md](vue-graphics-stack.md) | The Vue plugin: windows, event loop, layout with Clay, rendering, input model and drag and drop, icons, themes, animation, error handling, conventions |
| [vue-widgets.md](vue-widgets.md) | Catalogue of the widgets implemented in Vue, their layout rules and callback protocols |
| [vue-interactive-footer.md](vue-interactive-footer.md) | The interactive status bar (View › Interactive status bar): the properties at the cursor as menus, the tags around it as buttons which select them; how it works and what could come next |
| [typographic-palettes.md](typographic-palettes.md) | The typographic palettes of the colour menus (Vue): the sets, and how to define your own |
| [vue-testing.md](vue-testing.md) | Snapshot and scripted-event harness, how to run and write tests |
| [texmacs-gui-architecture.md](texmacs-gui-architecture.md) | How the TeXmacs core talks to a GUI plugin: widget factories, slots, Scheme markup, tools, dialogs, the client/server sockets, the other entry points |
| [editor-frontend-separation.md](editor-frontend-separation.md) | Design, not implemented: how the editor depends on the GUI and the window layer, and how to separate them in stages; draft headers in [editor-frontend-separation/](editor-frontend-separation/) |
| [pdf-output-with-mupdf.md](pdf-output-with-mupdf.md) | Writing the PDF with MuPDF instead of PDFHummus: what the Hummus renderer does, what MuPDF offers, the font subsetting which blocks it, and a working prototype |
| [build-and-debug.md](build-and-debug.md) | Configuring, building, dependency tracking, crash reports, pitfalls met along the way |

Source: `src/Plugins/Vue/` (`vue_gui.cpp`, `vue_widget.cpp`, headers, `TODO`,
`tests/`, and the vendored Clay: `clay.h`, `clay.c`, `clay_grid.h`, plus the
unused `clay_renderer_SDL3.c`).
Reference implementations: `src/Plugins/Qt/`, `src/Plugins/X11/` + `src/Plugins/Widkit/`.
