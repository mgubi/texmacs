> ## <img src="TeXmacs/misc/images/texmacs-vue-256.png" alt="The logo of TeXmacs Vue" width="36" align="top"> Branch `wip_wasm_vue` — TeXmacs in the browser (TeXmacs Vue)
>
> **Try it: <https://mgubi.github.io/texmacs/>** (experimental)
>
> Work in progress: TeXmacs compiled to WebAssembly and running in a web
> page, an experimental port called TeXmacs Vue, with OpenType fonts
> (OpenType mathematics included). The CI runs on the branch `vue_ci` only,
> which is moved to this one when a state is worth building and publishing:
> `git push origin wip_wasm_vue:vue_ci`. Nothing is installed and
> nothing leaves the browser unless it is downloaded.
>
> ![TeXmacs Vue in the browser: tabs for the documents, the tool bars, a formula](docs/wasm/texmacs-in-the-browser.png)
>
> It is stock TeXmacs on
>
> * the Vue GUI (below) and SDL3, with Emscripten's SDL3 port;
> * the [S7](https://ccrma.stanford.edu/software/snd/snd/s7.html) Scheme
>   interpreter in place of Guile, merged from `wip_s7` of texmacs/texmacs
>   (notes in [`docs/s7/`](docs/s7/README.md)); S7 is also the default of the
>   desktop build on this branch (`--with-scheme=s7|guile`);
> * a slim MuPDF 1.28.5 for the pixels and the PDF output (no fonts of its
>   own but the standard 14, no document formats but PDF, SVG and images).
>
> What the page does:
>
> * **One canvas, many windows.** The browser gives one window, so the
>   windows of TeXmacs become virtual ones: each document is a tab of the
>   frame above the canvas (its name, a dot when modified, × to close, + for
>   a new one, a ribbon which scrolls with the wheel or a drag), and the
>   dialogs float over it with a title bar and a frame which resizes them
  (their contents scroll when they do not fit). The same mode runs on the desktop
>   with `TEXMACS_VUE_SINGLE_WINDOW=1`, which is how it is tested.
> * **The TeXmacs menu** of the page: the version, the state of the files,
>   the storage used, the Files panel, reload and reset.
> * **Files.** *Files of the page…* (also in the File menu) shows the files
>   kept in the browser; files and whole projects (folders, zip archives,
>   with their images) come in by upload or by dropping them on the page,
>   and go out as downloads (a folder as a zip). The open and save dialogs of
>   TeXmacs are this panel. The home directory, with the preferences and the
>   documents, is kept in IndexedDB.
> * **Loading in pieces.** The program is 5.2 MB (brotli) and the files of
>   TeXmacs are packages: 4 MB are needed to start, the other 24 MB come in
>   the background once TeXmacs runs; a file needed before its package is
>   fetched alone (a byte range). Everything is kept in the cache of the
>   browser: a second visit loads nothing.
>
> * **The clipboard of the system**: copy, cut and paste with the other
>   programs (text, and HTML when TeXmacs has it); TeXmacs' own format is
>   kept when a copy is pasted back. On a Mac the shortcuts are Cmd+..., as
>   the browser's.
>
> * **The TeXmacs server**: the Remote menu logs in to a TeXmacs server over
>   WebSocket (the servers of this branch serve WebSocket clients on their
>   usual port), for remote files and shared documents.
>
> Not there yet: plugins and external converters (no processes in a page),
> `wss` for servers elsewhere than on the same machine.
>
> Build and try it (Emscripten, tested with 6.0; Python ≥ 3.10; Node):
>
>     . misc/wasm/emenv.sh build-wasm     # the Emscripten environment
>     sh misc/wasm/build-mupdf.sh         # the slim MuPDF, once
>     make -C build-wasm -f ../misc/wasm/Makefile -j8 web
>     node misc/wasm/serve.mjs            # http://localhost:8080/texmacs.html
>
> (`make ... node` builds a headless TeXmacs for node, which converts
> documents to PDF.) Any web server does, but the page loads faster from one
> which sends the brotli copies and answers range requests, as `serve.mjs`
> does. Details, the design and the plan: [`docs/wasm/`](docs/wasm/README.md).
>
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

## This branch: OpenType mathematical fonts (`wip_opentype`)

This branch teaches TeXmacs to typeset mathematics with the fonts that
LaTeX users know from `unicode-math`, and to offer them the way LaTeX
packages do: a text font and its mathematics chosen together.

![Latin Modern Math](src/opentype-math/latin-modern-math.png)

![Libertinus Math](src/opentype-math/libertinus-math.png)

![Fira Math](src/opentype-math/fira-math.png)

**Formulas laid out from the `MATH` table.** An OpenType math font says how
its formulas are to be built: the position of scripts and limits, fractions
and radicals, italic corrections and the kerning of scripts into the corners
of a letter, the placement of accents, the size variants of every delimiter
and how to assemble one of any size. TeXmacs now reads all of it and follows
it, with script-size alternates, dotless letters, flattened accents and pair
kerning from the `GSUB` and `GPOS` tables. The hand-tuned customizations
TeXmacs already had for STIX and the TeX Gyre fonts keep precedence, and
the experimental preference "Hand tuned math fonts" switches them off for
comparison.

**Fonts that come with TeXmacs.** Latin Modern, New Computer Modern, STIX
Two, Libertinus, Kp Fonts (serif and sans), Erewhon (Utopia), XCharter
(Charter), Euler, Concrete and Fira, each with its text faces (Euler with
TeX Gyre Pagella, Concrete with the Concrete faces of CM Unicode) and,
where the family has them, its sans serif and typewriter companions and a
bold math font, next to the TeX Gyre fonts already shipped. They are
registered in the shipped font database, so a fresh installation uses them
without a scan, and a home directory written by an older version is brought
up to date at the next start, instead of hiding the new fonts (a symbol only
they draw used to come out as its name in red). Fonts are found in their
OpenType form before their Type 1 form, and a rescan of the fonts on disk
takes seconds instead of minutes.

**Fonts TeXmacs knows.** Twenty-four math fonts have a profile that says
what the `MATH` table cannot: which text, sans serif and typewriter fonts go
with them, where their letters come from, and where they belong in the
menus. XITS, Asana, IBM Plex, Garamond, Old Standard, DejaVu, Lete Sans,
New Computer Modern Sans and GFS Neohellenic are used as soon as they are
installed, in the font directories of the system or of TeXmacs, or from TeX
Live (any year on macOS, 2020 to 2022 on Linux, or through
`TEXMACS_FONT_PATH`). Any other font with a `MATH`
table works too, without a profile and outside the menus, named in the
document as `math=Cambria Math,Cambria`.

**Choosing them.** The font button of the focus toolbar offers the
installed pairs under the names LaTeX users know: Times, Palatino, Utopia,
Charter, Euler, Concrete, Libertinus, Kp Fonts and the others in a serif
section, Fira, Kp Sans, Computer Modern Sans and Lete Sans in a sans serif
section, the less common fonts in a submenu, and the text fonts alone in a
last section. The OpenType features of a font, such as old style figures,
small capitals and stylistic sets, are offered by the font browser, and,
when complex actions go through the menus, in `Document > Font > Features`
and `Format > Font features`.

**Symbols.** Two hundred mathematical symbols which TeXmacs could draw but
not name have names, LaTeX equivalents and classes now. In a formula, a
window and a side tool show all the symbols, group by group, with their
markup in a balloon, to insert them with a click.

**Where to read more.**

- `Help > Manual > Fonts` in TeXmacs: choosing fonts, the mathematical
  fonts, with a sample of every font that comes with TeXmacs and a list of
  the others, and the reference chapter *Fonts, from selection to glyph*.
- [`src/OPENTYPEMATH.md`](src/OPENTYPEMATH.md): what the branch implements,
  how to build and test it, and a specimen of every profiled font.
- [`doc/opentype-math-design.md`](doc/opentype-math-design.md): the design,
  the status log and everything that is still missing.
- [`doc/opentype-math-fonts-survey.md`](doc/opentype-math-fonts-survey.md):
  the fonts, measured one by one, and why these are shipped.
- [`doc/font-system-review.md`](doc/font-system-review.md): the TeXmacs
  font system as a whole.
- [`doc/math-symbol-coverage.md`](doc/math-symbol-coverage.md): which
  mathematical symbols TeXmacs can name.
- [`tests/README.md`](tests/README.md): the unit tests, the sample renders
  and the comparison with LuaLaTeX.

The work started from the partial support written by Ke Shi for Mogan
(OSPP 2024), itself built on a `MATH` table reader from 2021.

## Documentation
GNU TeXmacs is self-documented. You may browse the manual in the `Help` menu or browse the online [one](https://www.texmacs.org/tmweb/manual/web-manual.en.html).

For developer, see [this](./COMPILE) to compile the project.

## Contributing
Please report any [new bugs](https://www.texmacs.org/tmweb/contact/bugs.en.html) and [suggestions](https://www.texmacs.org/tmweb/contact/wishes.en.html) to us. It is also possible to [subscribe](https://www.texmacs.org/tmweb/help/tmusers.en.html) to the <texmacs-users@texmacs.org> mailing list in order to get or give help from or to other TeXmacs users.

You may contribute patches for TeXmacs using the [patch manager](http://savannah.gnu.org/patch/?group=texmacs) on Savannah or by submitting a [pull request](https://github.com/texmacs/texmacs/pulls) on Github.Please note that while we use SVN on Savannah, GitHub serves only as a mirror. To facilitate synchronization, we have a `svn_mirror` branch. Please refrain from submitting pull requests to the `svn_mirror` branch; instead, use the `development` branch as the base to ensure proper merging and integration into the main SVN trunk.
