# Developer documentation

Notes written while porting the TeXmacs GUI to the **Vue** plugin (Clay
layout library + SDL3 + MuPDF). They are meant as references for future work
on the GUI: what the conventions are, how the pieces fit together and where
to look in the code.

| File | Contents |
|---|---|
| [vue-graphics-stack.md](vue-graphics-stack.md) | The Vue plugin: windows, event loop, layout with Clay, rendering, input model, conventions |
| [vue-widgets.md](vue-widgets.md) | Catalogue of the widgets implemented in Vue, their layout rules and callback protocols |
| [vue-testing.md](vue-testing.md) | Snapshot and scripted-event harness, how to run and write tests |
| [texmacs-gui-architecture.md](texmacs-gui-architecture.md) | How the TeXmacs core talks to a GUI plugin: widget factories, slots, Scheme markup, tools, dialogs |
| [build-and-debug.md](build-and-debug.md) | Configuring, building, dependency tracking, crash reports, pitfalls met along the way |

Source: `src/Plugins/Vue/` (`vue_gui.cpp`, `vue_widget.cpp`, headers, `TODO`, `tests/`).
Reference implementations: `src/Plugins/Qt/`, `src/Plugins/X11/` + `src/Plugins/Widkit/`.
