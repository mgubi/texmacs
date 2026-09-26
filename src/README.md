> ## Branch `wip_vue` — the Vue GUI
>
> Work in progress: a graphical back end for TeXmacs which owes nothing to a
> widget toolkit. It draws the editor, the bars, the menus, the dialogs and
> the tools itself, on three libraries — [Clay](https://github.com/nicbarker/clay)
> for the layout, SDL3 for the windows, the input and the clipboard, and
> MuPDF for the pixels.
>
> * Code: [`src/Plugins/Vue/`](src/Plugins/Vue/), with its `TODO` and its `tests/`
> * Developer notes: [`docs/`](docs/README.md) — the graphics stack, the
>   widgets, the test harness, how the TeXmacs core talks to a GUI plugin,
>   and how to build and debug it
> * Build: `./configure --with-gui=vue --with-sdl3 --with-mupdf=<prefix>`
>
> Everything outside `src/Plugins/Vue/` and `docs/` is stock TeXmacs, save
> for the few places which had to learn that the GUI is neither Qt nor X11.

# GNU TeXmacs
[![Join the chat at https://gitter.im/texmacs/Lobby](https://badges.gitter.im/texmacs/Lobby.svg)](https://gitter.im/texmacs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[GNU TeXmacs](https://texmacs.org) is a free wysiwyw (what you see is what you want) editing platform with special features for scientists. The software aims to provide a unified and user friendly framework for editing structured documents with different types of content (text, graphics, mathematics, interactive content, etc.). The rendering engine uses high-quality typesetting algorithms so as to produce professionally looking documents, which can either be printed out or presented from a laptop.

The software includes a text editor with support for mathematical formulas, a small technical picture editor and a tool for making presentations from a laptop. Moreover, TeXmacs can be used as an interface for many external systems for computer algebra, numerical analysis, statistics, etc. New presentation styles can be written by the user and new features can be added to the editor using the Scheme extension language. A native spreadsheet and tools for collaborative authoring are planned for later.

TeXmacs runs on all major Unix platforms and Windows. Documents can be saved in TeXmacs, Xml or Scheme format and printed as Postscript or Pdf files. Converters exist for TeX/LaTeX and Html/Mathml. 

## Documentation
GNU TeXmacs is self-documented. You may browse the manual in the `Help` menu or browse the online [one](https://www.texmacs.org/tmweb/manual/web-manual.en.html).

For developer, see [this](./COMPILE) to compile the project.

## Contributing
Please report any [new bugs](https://www.texmacs.org/tmweb/contact/bugs.en.html) and [suggestions](https://www.texmacs.org/tmweb/contact/wishes.en.html) to us. It is also possible to [subscribe](https://www.texmacs.org/tmweb/help/tmusers.en.html) to the <texmacs-users@texmacs.org> mailing list in order to get or give help from or to other TeXmacs users.

You may contribute patches for TeXmacs using the [patch manager](http://savannah.gnu.org/patch/?group=texmacs) on Savannah or by submitting a [pull request](https://github.com/texmacs/texmacs/pulls) on Github.Please note that while we use SVN on Savannah, GitHub serves only as a mirror. To facilitate synchronization, we have a `svn_mirror` branch. Please refrain from submitting pull requests to the `svn_mirror` branch; instead, use the `development` branch as the base to ensure proper merging and integration into the main SVN trunk.
