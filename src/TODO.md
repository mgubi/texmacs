# Upsilon to dos

This is a list of desiderata for Upsilon TeXmacs. 
See also the file [TeXmacs/doc/about/projects/todo.en.tm](TeXmacs/doc/about/projects/todo.en.tm) for general TeXmacs to dos,
and [docs/TODO.md](docs/TODO.md) for the open work of the moment: bugs found
and not fixed, pull requests waiting, decisions to take.

## To do

- GUI scripting: Investigate extending the scripting capabilities of Vue to the other UIs via a general mechanism
- GUI scripting: Investigate GUI scripting via Scheme or via a specific scripting language like the current implementation?
  (plan: [docs/plans/plan-gui-scripting.md](docs/plans/plan-gui-scripting.md))
- keep track of contributors: code, bug reports, etc.. to Upsilon (and then to TeXmacs in the usual place)
- (?) work on the style rewriter (ongoing in wip_pure_style)
- have the possibility to choose a more modern and uncluttered UI: common operations in UI elements, others in menus or specific dialogs 
- add history to forward and backward buttons (like in a browser)
  (plan: [docs/plans/plan-navigation-history.md](docs/plans/plan-navigation-history.md))
- investigate a protocol to add attachments to documents and refer to them: styles, images, etc.. 
- ensure that PDF attach only one copy of each used image
- allow TMU (unicode format of Mogan) or TMX (our own version), especially to facilitate communication with LLMs and anyway to prepare transition
  (plan: [docs/plans/plan-tmu-tmx.md](docs/plans/plan-tmu-tmx.md))
- serious support for literate programming 
- support for formalization of mathematics via Lean/LSP
  (plan: [docs/plans/plan-lean-lsp.md](docs/plans/plan-lean-lsp.md))
- support of MCP in the sessions (to be considered agents)
- implement tags for commands reacheable from the UI and an interface (command palette) for the users to see and select commands according to tags and activate one command, this could be a modern replacement to toolbars, menu, etc... (to be designed better)
- a well typeset and structured and indented table of contents in the UI with indication of labels for formulas if asked, e.g. on a left panel, or pulldown button. It will allow to navigate the paper better.
- easy editing of slides, e.g. moving them around, switching, thumbnails (at the place of the table of contents) There is some packages in tm-forge, check. 
- Android and iPad port? (we need better UI!)

## Done

- move src/Plugins/Vue/tests to tests/Plugins/Vue
- the commands of the menu of a tag of the interactive footer act on that tag, also when a tag of the same kind is inside it (a displayed formula with an inline formula in it becomes inline from its menu; test `tests/Plugins/Vue/footer-variants`)
- plans for GUI scripting, the history of the back and forward buttons, TMU/TMX and Lean/LSP (in [docs/plans](docs/plans))
- interactive footer: a Document menu (the properties of the document, wherever the cursor is), and the tags around the cursor as menus with their Focus menu, which change them without moving the cursor (see [docs/vue-interactive-footer.md](docs/vue-interactive-footer.md))
