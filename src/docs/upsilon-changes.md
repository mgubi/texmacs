# TeXmacs Upsilon: recent changes

The changes and revisions of [TeXmacs Upsilon](../../README.md), the newest
first: what was added, what was fixed, and what changed in the builds. Items
which concern only the desktop applications or only the browser version say
so; the others concern both. This page is also on the site, at
**<https://mgubi.github.io/texmacs/changes.html>**, next to the
[latest builds](https://mgubi.github.io/texmacs/), the
[features in pictures](https://mgubi.github.io/texmacs/features.html) and
[TeXmacs in the browser](https://mgubi.github.io/texmacs/texmacs.html). The commits of the
branch [`upsilon_texmacs`](https://github.com/mgubi/texmacs/commits/upsilon_texmacs)
have the details, and the problems go to the
[issues of the repository](https://github.com/mgubi/texmacs/issues).

## 1 November 2026

- **Shell scripts** (desktop): a `.sh`, `.bash` or `.zsh` file opens as shell
  code, as a `.lua` file opens as Lua code: highlighting by tree-sitter,
  structured editing and the language server, and it is saved byte for byte
  (the `#!` line, the tabs, the last newline), where it was a plain text
  document before, with its tabs expanded (#734).

- **Structured merge of documents** (desktop): fewer spurious conflicts. A
  block removed or changed by one side next to a block changed by the other,
  and an edit next to a new paragraph of the other side, now merge cleanly.
  Fewer silent mistakes as well: paragraphs are only merged word by word if
  they share enough real words, changes of neighbouring words of a paragraph
  conflict, each side removing one of two neighbouring paragraphs conflicts,
  and in a long list of changed paragraphs a conflict stays with its
  paragraph (#676).

## 31 October 2026

- **Replace and paste** (desktop): several paragraphs next to a hidden tag are
  no longer lost, and that tag stays hidden; where paragraphs cannot go, the
  match is left with a message, and an insertion changes nothing and keeps the
  selection (#675).
- **Copy to LaTeX on Windows** (desktop): the name of an image keeps its drive
  (`D:/a/...`), and a name in `\includegraphics` stays on one line (#717).
- **Clipboard** (desktop): the clipboard commands no longer crash under
  `-headless` (#708).

- **LaTeX and HTML export** (desktop): no crash with an embedded image on
  Edit > Copy to > LaTeX or with a target which cannot be written; the images
  always go next to the exported file, also from scripts and under any name;
  a copy to the clipboard never overwrites a different file, reuses the file
  of the same image, says where it is, and names an image which exists; a
  linked image is found when exporting to another folder (#697).
- **macOS: the applications linked against the current SDK** (desktop,
  Cocoa and Vue): the disk images were linked against the SDK of macOS 13,
  their minimal version, so macOS gave them the behaviour of macOS 13 (for
  instance blank plots, #582); they now use the SDK of the build machine,
  and the minimum version stays macOS 13.0 (#662).
- **Gnuplot: PNG plots without Ghostscript** (desktop): the gnuplot
  sessions make PNG images, and say so at their start, when Ghostscript
  (`gs`), which TeXmacs needs to show EPS images, is not installed; before,
  each plot showed as a question mark. The documentation of the Gnuplot and
  Maxima plug-ins says that their plots need Ghostscript (#662).
- **Structured merge of documents** (desktop): the style packages and
  the settings of the document (initial environment) of the other side
  are no longer lost, in *Resolve conflict* and in the Git merge driver (a
  setting changed on both sides keeps Git's ours, and the message says
  which);
  in a folder which is not trusted, *Merge documents structurally* says
  that nothing was installed instead of reporting success; while
  resolving a conflict, Version › Retain offers *Keep my version* and
  *Keep their version* instead of Old and New version, which keep your
  version also during a rebase, where Git swaps ours and theirs (#676).
- **Cocoa: the scroller of the icon bars in two columns** (desktop, Cocoa
  interface): when the scroll bars are always shown (System Settings) and the
  columns at the left or the right of the document are taller than the
  window, the bar widens by the width of the scroller, which no longer
  covers the last column (#683).
- **Cocoa: a console which opens by itself leaves the focus to the document**
  (desktop, Cocoa interface): when a warning opens the "Error messages" window,
  the document stays the window in use, and what is typed or asked for next goes
  to it, as in the other interfaces. A click in the console gives it the focus,
  and a console opened from the Debug menu takes it at once (#679).
- **Cocoa: the colors black, white, red...** (desktop, Cocoa interface):
  these colors of the program were transparent. A text with an unknown color
  name was invisible, on the screen and in a PDF: it is black now, as in Qt;
  the unknown colors of graphics are black too, and the informative flags
  in brown (of a `no-break`, an `if-page-break`...) are shown, as in Qt
  (#656).
- **Definitions in very large files** (desktop and browser): Definitions and
  Go to definition no longer take gigabytes of memory on a file with
  thousands of definitions on one line, which could freeze the machine. The
  page Outline no longer keeps a copy of itself at each edit; it shows the
  first 5000 definitions and says so.
- **The menu Focus in source files** (desktop and browser): in a `.cpp`, a
  `.py`, a `.scm`, a `.json`... it now has the entries for program code
  (Definitions, Outline, Go to definition, Folds, the syntax and server
  entries), as in a code block. The definitions of a large file are found
  much faster (6666 functions: 40 ms instead of 2.7 s, also for the page
  Outline and Go to definition); the submenu Definitions shows the first 500
  of them, then More definitions, which opens the page Outline (#698).
- **The menu Focus in code blocks** (desktop and browser): its entries for
  program code (Definitions, Outline, Go to definition, Folds, the syntax
  and server entries) are back; they were lost at each start, and TeXmacs
  no longer prints the warning "conditional master routine
  focus-extra-menu" on the terminal, nor the one of `parser-feature` when
  the first file is a JSON file (#684).
- **Search and Replace next to hidden content** (desktop): a replacement
  next to a hidden (folded) tag stays in its place instead of going inside
  the tag (#590); going to a hit inside hidden content shows it in a step
  of undo, so that undo hides it again, and the search goes on after it,
  also in Replace one, which stays one step of undo (#591); a pattern made
  only of a wildcard no longer leaves "Match left" on a theorem or a table
  cell alone in its paragraph (#589).
- **Replace All next to blocks** (desktop): Replace All always ends, also
  on a document of empty paragraphs which a pattern of empty paragraphs
  matches, and a match next to a block (a theorem, a table) which the
  replacement keeps is replaced instead of left (#687).
- **A change of style in every view** (desktop): a package added by the
  document itself (natbib for a bibliography) reaches all the views of the
  buffer, not only the current one (#535).
- **A `raw-data` tag without arguments** (desktop): saving a document with
  it no longer crashes TeXmacs (#685).
- **Hyphenation of Bulgarian, Russian and Ukrainian** (desktop and
  browser): the decoding of the hyphenation patterns stops at the end of
  its string, instead of reading past it (#686).
- **A tag named `string`** (desktop): a tag or macro named `string` works
  like any other; it no longer gives random content or crashes when a
  document with it is loaded (#678).
- **LaTeX import: `\string` and deep nesting** (desktop): `\string` gives
  its next token as text, as in TeX (no more random byte from
  `\string{`), a math `\backslash` alone is kept, and groups nested more
  than 128 deep are skipped with a warning instead of crashing (#658).
- **Cocoa: TeXmacs goes on after an error in a scroll** (desktop, Cocoa
  interface): an error during an update forced by AppKit (a scroll or a
  resize of a document, a change of the appearance) is reported as before,
  but TeXmacs no longer stops updating its windows and running its
  commands afterwards. The checks of the Cocoa interface
  (`tests/cocoa/check.sh`) also cover the dialogs with negative sizes and
  such an error (#666).
- **Rotated text and big glyphs** (desktop and browser): rotations and
  the other transformations of `gr-transform` are drawn the right way round
  by the GPU renderer (Cocoa and Vue) and by the MuPDF renderer (the CPU
  path of Vue and of the browser), which turned them the other way and
  displaced them; a big wide brace (an overbrace of 26 terms at zoom 6) no
  longer stops TeXmacs with "out of memory" (the sizes of the glyphs were
  16-bit numbers); big glyphs with an outline are no longer dropped under a
  transformation or a pattern; and the textures of big glyphs over their
  bound release the least recently drawn ones instead of the whole cache
  of glyphs (#659).
- **Cocoa: very tall documents, the console, the View menu** (desktop,
  Cocoa interface): a document taller than 5,000,000 points (a few hundred
  pages at a high zoom) could not be scrolled to its end; the frame now
  goes up to 10,000,000 points, and a taller document gets a warning
  instead of being cut silently. The console opens at its last message
  even when no window of TeXmacs has the keyboard focus, and the View menu
  no longer fails ("Error in menu") when it is built while the console is
  the current editor (#661).
- **Cocoa: tooltips on the icon bars** (desktop, Cocoa interface): the
  buttons of the icon bars show their balloon text as a tooltip again; the
  hover highlight of a button removed the tracking areas of its tooltip
  (#637).
- **Windows: the output of commands in UTF-8** (desktop): the output of
  the programs TeXmacs runs (git, pandoc, Python, the LaTeX tools...) is
  read as UTF-8 when it is valid UTF-8, and in the OEM code page of the
  system otherwise; before, accented letters in it came out garbled, e.g.
  in the log of Git (#641).
- **Replace All faster without a filter** (desktop): searching without a
  filter, with an empty one, or in a source document no longer searches
  the paragraph again at each hit, about 1.5 to 1.8 times faster in long
  paragraphs (part of #618).

## 30 October 2026

- **HTML export with MathJax: accents and aligned formulas** (desktop):
  the ring accent is written `\mathring`, the inverted breve
  `\overparen`, the accents below a formula with `\underset` and the
  braces over or under it with `\overset`/`\underset`, all known to
  MathJax; an `aligned` (or `split`, `alignedat`...) inside an equation
  becomes `\begin{aligned}` instead of the unknown `equationsbase`, and
  `gathered`, `subarray` and `eqnarray*` inside a formula are kept
  (#607, #611).
- **Shell sessions: the interrupt message** (desktop): interrupting a
  command of a shell session shows the red "Interrupted TeXmacs shell"
  again, never the raw text `(with "color" "red" ...)`, which appeared
  when the interrupt came at a bad moment on a loaded machine (#646).
- **The Git merge driver finds TeXmacs on Windows and Linux** (desktop):
  the command which Git runs to merge `.tm` documents structurally now
  calls `texmacs.exe` of the Windows installer and bundle (with `/` in its
  path, for the `sh` of Git for Windows), the `texmacs.bin` of a `make
  install` (under `TEXMACS_BIN_PATH`) and the AppImage itself (not its
  temporary mount point); before, it fell back on a `texmacs` which was
  not on the `PATH` (Windows) or got a wrong `TEXMACS_PATH` (`make
  install`, AppImage) (#645).
- **Cocoa: errors reported in more places** (desktop, Cocoa interface):
  an error in the drawing or the layout of a window, in the notifications
  of the windows (also while a window closes), in a drop from another
  application, in the text of an input method or in a pop-up window is
  reported in the console instead of ending TeXmacs or being dropped
  without a word, and the sizes which AppKit would refuse are replaced by
  safe ones, with a warning on `debug-aqua`. New checks
  (`tests/cocoa/check.sh`, run on the Cocoa disk image) cover a French
  dialog title, closing the console and an idle console (#621).
- **Version control with spaces in the names** (desktop): the commands
  of Subversion quote their arguments, so that a file in a folder with
  spaces in its name, or a commit message with quotes, `$` or accents,
  reaches `svn` unchanged, also on Windows, where no shell splits the
  command line (#636). Git, which TeXmacs runs without a command line,
  is now checked in a repository with spaces in the names of its folder
  and files, and an accent in a file name (the names in the status list, renamed
  files, a commit message in Cork given to Git in UTF-8), and the checks
  also run on Windows, with the native Git (#625).
- **No false warning of a typesetting loop**: zooming or resizing a
  window for seconds without a key or a click (a pinch, a live resize, a
  zoom by a script) no longer warns that the document "was typeset again
  from scratch ... a loop?"; a real loop, which comes back to the same
  sizes, is still reported (#621). In the Cocoa interface, the item
  "Error in menu" is translated, and any kind of error in a menu is
  caught (#624); the scroll bar of the consoles and of the fields is an
  overlay one, which hides no text, and a console scrolled to its end
  stays there when its window is resized.
- **Cocoa: the consoles scroll** (desktop, Cocoa interface): the editors
  inside a window (the consoles, the search and replace fields, the other
  `texmacs-input`) have a scroll view of their own, as in the Qt
  interface: the wheel and the trackpad scroll them, with a scroll bar
  when the text is longer, and the console keeps showing its last
  messages. The console opens at two thirds of the screen instead of the
  whole screen, and stays resizable.
- **Cocoa: an error in a menu no longer aborts TeXmacs** (desktop, Cocoa
  interface): a menu whose computation fails (such as Debug >
  Miscellaneous > Provoke menu error, computed with all the menus when
  Spotlight opens) shows a disabled item "Error in menu" and the error
  goes to the console, as in the Qt interface (#624).
- **Wide braces drawn by the GPU**: the wide over- and underbraces of a
  long formula and the other glyphs of more than 512 pixels which have no
  outline (they are assembled from pieces by a virtual font) are drawn
  by the GPU renderer, as tiles of their bitmap; they were missing from
  the canvases of the Cocoa interface drawn by the GPU and of the Vue
  port (desktop applications and browser) (#623).
- **Cocoa: errors reported instead of silent**: an error in an action of
  the interface (a menu, a button, a delayed command, the closing of a
  window...) is reported in the terminal and the console of errors, and
  TeXmacs goes on, instead of aborting or of ignoring it without a word;
  a document typeset again and again without any input (a loop) gets a
  warning naming it; the conversions of texts for macOS which fail say so
  on the debug channel `debug-aqua` (#621).
- **Cocoa: still consoles and search fields** (desktop, Cocoa
  interface): the editors inside a window (the consoles, the search and
  replace fields, the other `texmacs-input`) were typeset again from
  scratch at each update, with their page trembling and about one core of
  the processor busy; they now keep the size of their place in the window
  and are typeset only when something changes (#620).
- **Hidden definitions in tables and floats**: what a hidden part
  (`hidden`, `hide-preamble`) assigns in a table cell whose lines are
  broken, or in a float, holds for what follows, as when it is not
  hidden: a macro defined in a hidden preamble inside the abstract of
  `tmarticle` was undefined in the text of the abstract (#503).

## 29 October 2026

- **Japanese and Korean text**: the kanji of a Japanese text (and the
  hanja of a Korean text) come from the Japanese (Korean) font of the
  system, as the kana (Hangul) next to them, instead of the Chinese
  fallback; with the fonts which the menus set for Japanese and Korean
  documents, the CJK characters are no longer shrunk to 70-80% of their
  size (#196).
- **Replace with wildcards, continued**: Replace All after a Replace no
  longer replaces again inside the replacement just made; Replace All
  works with more than 10,000 hits; in ordinary text, a hit which Search
  shows across formulas is replaced whole, by Replace and by Replace All
  alike (#596).
- **Cursor positions from scripts**: a `go-to` to a position which is not
  in the document (below a text, out of range, the document itself) puts
  the cursor at the closest valid place, and the next key no longer
  crashes TeXmacs (#599).
- **LaTeX import of plain text**: a source which is only text, without
  `\begin{document}`, no longer crashes TeXmacs, and a source of one
  paragraph keeps its commands (`\emph{b}` gave `b`) (#608).
- **Views**: the numbers of the views are never given again after a
  rename, and a window whose only view is deleted shows a view of another
  buffer instead of staying empty (#610).
- **Commands run in the background** (the requests of the AI sessions
  through the curl program, `async-eval-system`): their real exit status
  and their errors reach TeXmacs, so that a failed request is reported
  as such; an interrupted answer stops its command at once; the new
  `async-eval-system-full` gives the exit status, the output and the
  errors of a command (#437).

- **Views and windows**: deleting a view shown in a window, or the
  current view, no longer leaves TeXmacs with a pointer to a deleted view
  (crashes and hangs afterwards); the views of a renamed buffer get names
  of their own, and Window > Clone window after File > Save as no longer
  quits TeXmacs (#559, #560).
- **A cursor path below a text** no longer makes TeXmacs hang when it
  reports the error (#592).
- **LaTeX import**: sources which end too early (a `\` or a command
  without its argument at the end, a `$` at the end) are no longer read
  past their end (#561).
- **Plots of the sessions in the Cocoa build** (desktop, Cocoa): the
  plots of gnuplot, Maxima and Sage, and any PostScript (`.ps`, `.eps`)
  image, were blank, only their space reserved. macOS no longer draws
  PostScript, and the application took the empty image which macOS
  still gave for it; PostScript is now converted with Ghostscript, as
  in the other builds, which requires Ghostscript (`gs`, e.g. from
  Homebrew) (#582).
- **The file dialogs of the system on Linux** (Vue, desktop): Open, Save
  and the other file choosers show again the dialog of the desktop (its
  portal, or zenity) rather than the file chooser of TeXmacs. SDL gave
  zenity labels for the buttons which zenity 4 refuses ("Could not run
  zenity: exit code 255"), and the SDL of the Linux packages was built
  without D-Bus, hence without the desktop portal (#456).
- **Clicks and selections in long documents**: a click at the right of
  a line (after the square which ends a proof) or at the left of the
  lines no longer puts the cursor at the end or the start of the
  document, and a selection dragged there no longer becomes the whole
  document. It happened in documents of 512 paragraphs or more, and in
  any document with a footnote, a float or several columns, outside
  paper mode (#584, #585).
- **Previews of references and balloons while completing**: Tab through
  the completions of a reference (`preview-ref` package) shows the
  preview of the label proposed alone; the previews of the labels
  before, and the balloons of the keyboard (focus balloons, comments of
  versions), no longer stay on the screen (#583).

- **References after an edit**: a reference to a label which an edit
  removes shows "?" at once, and its old number no longer stays until
  Document > Update; an undo, a label added after its references, or
  equations numbered again by an insertion also update the references
  placed before them; the same for a label made only in a running header
  (#571).
- **Bindings changed by an edit with the classic cache**
  (`TEXMACS_TYPESET_READS=off`): what reads a binding (`get-binding`,
  a reference, the parts of a surround) is typeset again when an edit
  gives it another value (#576).
- **Replace with wildcards** (#520, #543): the text which a wildcard took
  is kept by the replacement, also when the match has another number of
  parts than the pattern, when its text lies in a tag argument, or in a
  formula; Replace All gives the same as Replace pressed for each match,
  in one undo step, and replaces each match once, left to right; Replace
  takes only matches which Search shows, a wildcard never goes across
  paragraphs, and a match is never extended over the base of a script;
  a match which cannot be replaced without losing text is left, with a
  message; after an error the document is left as it was; Replace goes
  on after the parts which the mode or language filter leaves out.

- **Dialogs at a fractional scaling** (desktop Vue builds, Wayland): with
  the display scaled by a fraction such as 150% or 167% (GNOME on
  Wayland), a dialog sized to its contents (the question of Ctrl+Q with
  unsaved documents) came out too small, its text cut on the right and
  its buttons on the bottom, legible only once resized (or too large at
  125%). The windows now have the sizes of the Qt port at those scalings,
  each by the scaling of the display it is on. Window sizes saved at such
  a scaling are read accordingly, so the windows may open once at
  another size (#376).
- **No crash after closing a dialog by a key on GNOME/Wayland** (desktop
  Vue builds): Esc in the question of Ctrl+Q, with the pointer on the
  title bar or the border of the question, could crash TeXmacs at the
  next motion of the mouse, in the window decorations of GNOME (a bug of
  libdecor up to 0.2.5, which TeXmacs now works around: a window leaves
  the screen a moment before its decorations are destroyed) (#376).
- **Faster Document > Update > All**: on a large document Update All
  no longer typesets everything three or four times. The automatic labels
  are numbered again by typesetting only what makes labels and table
  entries; the clean-up of the included files and of the pictures keeps
  the layout of what did not change (an included file changed on disk, or
  a picture whose file or size changed, is still typeset again). On the
  2000-page book of the report, an Update All takes about 2 seconds of
  CPU instead of 25 to 33 (#408, #425).
- **Opening documents with old references**: a document whose saved
  table of references holds labels which it no longer defines is
  typeset once when opened, not twice (half the time on the chapters of
  the same book); a reference to such a label still shows "?" (#408).
- **Typesetting from scratch** is about a quarter faster: only the
  paragraphs record what they read for the reuse of the layout, not
  every part of them; editing is also somewhat faster (#408).

## 28 October 2026

- **Windows closed from their own editor**: a window closed by a key or
  a click in the editor it shows (C-Return in the editor of a comment, an
  action of the document) no longer crashes TeXmacs, in the Vue builds
  (desktop and browser) and the Qt build; nor does, in single-window mode
  and in the browser, a window closed by a key of another window. In the
  Vue build the window is hidden at once and freed once its layout is
  done; its buffers are still closed at once (#574).
- **Windows closed by a program (Vue)**: a window which a program closes
  (a dialog closed by one of its buttons, a script, `alt-window-delete`)
  closes the buffers of the editors it shows, as the close button of the
  window does: the buffers of the console (`tmfs://aux/console-...`) and
  of the other embedded editors stayed open, and its quit command ran
  afterwards on a window which no longer existed (#566).
- **Font design in the Cocoa build** (desktop, Cocoa): Font design, in
  the menu of the font of the document (the "Roman" button of the focus
  bar), no longer quits TeXmacs at once. The page of the fonts follows
  the size of the window, and the Cocoa window did not answer when the
  editor asked for its size; the same happened to any document whose
  page type is "automatic" (#558).
- **Title bar of the documents** (Vue, macOS): the window of a saved
  document has the icon of its file next to the title, which can be
  dragged to the Finder, a mail or a Terminal, and a Cmd-click on the title
  shows the folders of the file, to open one in the Finder; unsaved changes
  are shown by the dot in the close button instead of a `*` after the
  name, as in the other applications of macOS. Untitled documents, help
  pages and remote files have no icon (#545).
- **File > Preview on macOS** (desktop, Vue): the preview opens again
  when TeXmacs is started from the Finder or the Dock. The Vue build makes
  the PDF with Ghostscript, and such an application only had the PATH of
  the system (no `/usr/local/bin`, `/opt/homebrew/bin` or
  `/Library/TeX/texbin`): no PDF was made and nothing was shown. TeXmacs
  now adds the PATH of a login shell, as the Qt and Cocoa builds already
  did, which also finds LaTeX and the programs of the plug-ins; the PATH
  is read more carefully (the messages of a login script are left out,
  and a login shell which hangs is stopped after 5 seconds). A preview
  which cannot be made is now reported in the footer, instead of showing
  nothing or an older preview (#546).
- **Presentation mode**: the informative flags are hidden only in the
  window which is in full screen, no longer in the other windows of the
  same document; entering and leaving it no longer marks the document
  modified, and saving during a presentation keeps the informative flags
  of the document instead of writing "none" (#549).
- **Save as**: after a Save as to a file of the same name in another
  folder, the window shows the new file (its path, and on macOS the icon
  of the title bar), no longer the old one (#564).

## 27 October 2026

- **Emoji in LaTeX**: the export writes the emoji as `\emoji{...}` of the
  LaTeX package `emoji`, by its name there (also those of older documents,
  in code points), and loads the package in the preamble when the
  document has emoji: they come in color with LuaLaTeX, and as their
  names (`:grinning-face:`) with pdfLaTeX and XeLaTeX, which the package
  does not support, so that the document still compiles with all three.
  An emoji newer than the package shows as its characters with LuaLaTeX
  and XeLaTeX and as its code points with pdfLaTeX; the import gives back
  the same emoji.
- **Names of the emoji**: the emoji have names in the universal encoding
  of TeXmacs, those of the LaTeX package `emoji` (the short names of
  Unicode): `<emoji-grinning-face>`, and one name for a whole sequence,
  `<emoji-family-man-woman-girl>`, `<emoji-flag-italy>`,
  `<emoji-keycap-1>`. An emoji typed, pasted or imported comes into the
  document by its name, as a Greek letter does; the documents which have
  emoji as code points (`<#1F600>`) look the same as before and are not
  converted when they are loaded; search finds an emoji in either form,
  and a family or a skin tone typed code point after code point is stored
  by the name of the whole. The footer shows the emoji before the cursor,
  a sequence whole, with its name, where it showed a square (Vue, the
  browser) or the last character of the sequence alone (Qt);
  `\emoji{grinning-face}` of LaTeX is imported as the emoji, also by the
  other names of the package (`\emoji{smile}`, `\emoji{+1}`). Note: stock
  TeXmacs and older Upsilon builds read `<emoji-...>` as an unknown symbol;
  in TMML an emoji in code points is now written as a `<tm-sym>` (as
  `<#3B1>` is), since it would come back by its name (#533).
- **Maple** (desktop): the helper of the plug-in builds with the compilers
  of Xcode 15 and later and of GCC 14, which rejected the types of its
  callbacks, and on macOS it finds the Maple library by itself, also when
  it is built from an Intel shell on Apple silicon. A failure of Maple to
  start (a licence, for instance) is shown in the session, and the helper
  stops when the session ends instead of using a processor in full;
  `Maple.framework/Versions/Current` is searched first (#517).
- **Printing**: a shortcut which prints, such as `M-e p` bound to
  `print-to-file`, no longer leaves the "M-e" it showed while waiting for
  the second key on the screen (#519).
- **Part views**: saving the view of the master writes the changes of its
  initial environment (Document → Font size, for instance) and keeps the
  master's own entries, which were dropped at every save. Each variable is
  merged from what the view loaded, what it has now and what the file has
  on disk, so that a change made on disk or by the open buffer of the file
  is no longer overwritten (#421).
- **Search**: a formula inside a tag argument of the pattern and a pattern
  with regions to select (`select-region`) are matched as text, by the
  rules of the other patterns, the same way by Search and Replace; a match
  which follows one ending inside a tag is found (#500, #501).
- **Fonts**: kana and the other CJK blocks go to the CJK fallback, and the
  Korean and Chinese fallbacks are no longer shrunk to the size of Latin
  letters. With `math-stix` or a TeX Gyre math font and the default text
  font, formulas are set in the real math font, with its big operators,
  radicals and wide accents (#196).
- **AI and LanguageTool**: the codes which stand for the markup sent to
  the engines are freed after each request, or with their chat session,
  instead of piling up for the whole session (#438).
- **Linux, Qt**: a stopped session plug-in no longer stays a zombie process
  until the next event (#525).
- **Linux AppImage**: Maxima starts when it is built on GCL; the paths of
  TeXmacs in the AppImage no longer contain "//", which GCL rejects (#511).
- **Search**: with the cursor inside a tag which matches the pattern (a
  fraction, for instance), that tag is found; the search from the cursor
  skipped it (#523).
- **Views**: a change of the initial environment (Document → Font size,
  a style variable) reaches all the views of the buffer, also the clones
  and the part views, instead of being lost when another view saves (#528).
- **AI**: a paragraph translated in the background is no longer put in as
  a document nested in the document (#526).
- **Crash**: closing a buffer after a translation or a LanguageTool check
  which could not start no longer crashes TeXmacs (#537).
- **Fonts**: with SimSun or FandolSong installed, as on every Windows, the
  characters of the CJK fallback keep their width of one em (#196).
- **Windows, fonts**: the Japanese, Chinese and Korean fonts of Windows
  (MS Gothic, Yu Gothic, Meiryo, MingLiU, Gulim...) are known from the
  first start, without a scan of the disk, and the Japanese font is
  found also without the supplemental fonts (#538).

## 26 October 2026

- **Maple** (desktop): the session no longer hangs at its start on macOS.
  The launcher handed the path of TeXmacs to `make` without quotes, and
  the path of the application has spaces; without a license file in the
  Maple tree, it also dated a file with a `touch` option which macOS
  lacks. It now keeps the helpers which are in the home, and any failure
  (no `maple` command, a build which fails, with what the compiler says)
  is shown in the session. Maple 2015 and later are found in
  `/Library/Frameworks` (#517).
- **Qt**: only the part of the document in the window is repainted after
  typesetting. Opening a long document with saved references, or a new
  paragraph at its start, used to repaint every page that moved, and so
  converted every PDF image with Ghostscript: a document with 63 figures
  opens in 4 s instead of 10, and Enter at its start repaints in under a
  millisecond instead of 1.5 s (#458, #457).
- Search and Replace find the same matches in the text: a match may start
  and end inside a word, and a wildcard takes the shortest text, possibly
  none; a string of the pattern still has to match a whole argument of a
  tag when replacing. Patterns of several paragraphs are found when the
  first part reaches the end of its paragraph, from the leftmost start; an
  empty paragraph in a pattern stands for a paragraph break (#497).
- **Browser**: Clone window opens a second tab on the document; the number
  of each of its tabs, "(1)", "(2)", stays in sight when the name is too
  long for the column, and the folded column shows it after the initials
  ("N11", "N12").
- A macro defined in a hidden part (`hide-preamble`) inside the abstract of
  an article was shown in error in the paragraphs of the abstract which
  follow it, and worked only after the abstract. The same in any table
  cell whose lines are broken (`cell-hyphen` `t`): what a hidden part
  assigns there now holds for the paragraphs after it
  ([#503](https://github.com/mgubi/texmacs/pull/503)).
- **Licenses**: the packages bring the licenses of the software built into
  TeXmacs or shipped with it (s7, femtolisp, tree-sitter, Clay, SDL3,
  ThorVG, MuPDF and its libraries, GnuTLS, FreeType, Hunspell...), in
  `TeXmacs/misc/licenses` with a README naming each component, where it
  is used and where its sources are; the Linux and Windows bundles add
  those of the libraries of the system they bring (`misc/licenses/system`).
  The browser version serves them in `licenses/`, its help page names the
  license of each component, and TikZJax and Asymptote-web come with their
  licenses and the places of their sources. The ports of tree-sitter,
  of its scanners and of the indentation of nvim-treesitter name their
  authors and licenses; the fonts of Libertine and TeX Gyre got their
  license files, and the notices of the fonts and the icons no longer
  claim one license for all. Help → TeXmacs Upsilon thanks the authors of
  these works, the emoji of Twemoji included. An unused grid layout of the
  Vue interface, which came without a license, is gone.
- **Vue on Linux**: a trackpad scrolls as in the Qt port, a wheel notch for
  each unit of the device (ten pixels of the fingers, fifteen on Xorg with
  libinput), under X11 and Wayland alike, and stops with the fingers; it
  scrolled the movement of the fingers, some 17 times less in a window
  1000 pixels high (a graphics still follows the fingers). And under X11
  (also XWayland) the mouse wheel no longer stays slow after the trackpad
  was used: SDL gives the times of the smooth scrolling in milliseconds of
  the X server, where it gives the others in nanoseconds, so that no stream
  of events ever ended
  ([#459](https://github.com/mgubi/texmacs/issues/459)).

## 25 October 2026

- **Clone window**: File → Clone window and View → Clone window open a
  second window on the document, to see two parts of it side by side: it
  starts at the cursor and the zoom of the first one, beside it when the
  screen has room (else a little lower), and what is typed in one window
  shows in the other. The windows of the document are numbered in their
  titles, "a.tm (1)", "a.tm (2)", and Go → Windows has an entry for each of
  them ([#464](https://github.com/mgubi/texmacs/issues/464)).
- **Cocoa**: a window given the size of another one (a cloned window) was
  taller by the height of the title bar.

## 24 October 2026

- **Server log**: the line written at each login check gives the date of
  the user's last failed login, or "none"; it showed the word `iso8601`
  instead of the date. Desktop applications only.
- **macOS**: Edit → Preferences opened nothing when Inkscape is installed
  in `/Applications`: the preferences put its folder in the path with a
  function which the Scheme of Upsilon does not have, and stopped there.
  The same in the "manual path" of the sessions
  ([#465](https://github.com/mgubi/texmacs/issues/465)).
- **Cocoa**: a dialog whose translated title has an accent, like the
  preferences in French ("Préférences de l'utilisateur"), was not shown, and
  the application no longer answered afterwards (closing a window, ⌘Q).
  The texts given to macOS are now converted from the encoding of
  TeXmacs whenever they are not UTF-8, so labels, menus and messages in
  other languages can no longer be empty or stop the application
  ([#468](https://github.com/mgubi/texmacs/issues/468)).
- Two windows on one document: closing one of them leaves the document
  in the other, instead of emptying it, and asks about unsaved changes
  only when the last window of the document is closed. *File → Load* of
  a document which is already open brings its window forward instead of
  doing nothing
  ([#496](https://github.com/mgubi/texmacs/pull/496)).
- Search: with the matching of Replace, a part of the pattern between a
  tag (or a leading wildcard) and a wildcard no longer has to cover a
  whole string (`_x *bb* _y` finds `_x abba _y`)
  ([#493](https://github.com/mgubi/texmacs/pull/493)).

## 23 October 2026

- **Emoji in color**: TeXmacs now comes with an emoji font in color,
  Twemoji Mozilla, and draws the emoji with it on every platform and in
  the browser, in the text, in formulas and at any size, also in the
  exported PDF (as vectors). They were written out as their codes, like
  `<#1F600>`. The sequences are one picture each: the families, the
  flags, the skin tones, the keycaps, and a symbol followed by U+FE0F,
  like the red heart. The emoji fonts of the systems (Apple Color Emoji,
  Noto Color Emoji) are not used yet.
- **Emoji of the system**: the preference "Emoji" (Edit > Preferences >
  General) takes the emoji of the system instead of the shipped Twemoji,
  from the next start: Apple Color Emoji on a Mac, Noto Color Emoji on
  Linux, Segoe UI Emoji on Windows (not tried yet there). They are drawn in color on the
  screen and in the PDF (as pictures), the families, the flags, the skin
  tones and the keycaps as one picture each, and they copy out of a PDF
  as their characters. Desktop applications only.
- **Editing emoji**: a sequence (a family, a flag, a skin tone, a keycap)
  is one character for the editor: the cursor keys step over it at once,
  and Delete and Backspace remove it whole; the cursor stopped invisibly
  between its characters, and a deletion took off one person of a family
  at a time.
- **PDF**: the emoji copy out of an exported PDF as their characters, a
  sequence whole (a family, a flag), in Preview as in the readers which
  take the text from the ActualText: they came out as U+FFFD, or as
  nothing. With the MuPDF writer (the browser, or the preference "native
  pdf renderer"), the mathematical letters of the OpenType fonts copy
  right too, which Preview dropped.
- **Vue**: the balloons of a language server under the mouse no longer
  vanish as soon as they show on Linux under Wayland: a balloon was a
  window of its own, which the compositor placed where it liked and gave
  the keyboard to, so that the editor, losing the focus, closed it. The
  balloons are now tooltips attached to the window of the editor, which
  never take the keyboard nor the pointer
  ([#399](https://github.com/mgubi/texmacs/issues/399)).
- **AI sessions**: the field of the API key in the preferences of an AI
  session says "saved" next to it once the key typed is saved (with
  Return, or when leaving the field), or "removed" when it was emptied:
  nothing showed it, and Return seemed to do nothing
  ([#455](https://github.com/mgubi/texmacs/issues/455)).
- The messages of the error console (Debug > Error messages, which opens
  by itself on a warning) and of the debugging console are shown in a
  read-only editor: they can be selected and copied as in a document,
  and nothing typed, pasted, dropped, undone or done from a menu changes
  them. A new message is added at the end without losing the selection;
  a console opens at its latest messages and follows the new ones while
  its cursor is at the end, also when it is not the window in use. The
  console which opens by itself no longer takes the keyboard from the
  document being typed in, and closing it leaves no buffer behind.
- **Vue**: closing a window closes the buffers of the editors inside it
  (texmacs-input, as in the macro editors), also inside popups, balloons
  and scrolled areas: they stayed among the buffers.
- Searching for a formula is much faster in long documents: a pattern was
  tried again from every position of every formula, so that each keystroke
  in the search field took seconds in a book of 2000 pages (2.5 s in its
  longest chapter) and grew with the square of the length of a formula; it
  now takes about a tenth of a second there, a few milliseconds per formula
  ([#451](https://github.com/mgubi/texmacs/pull/451)).
- The layouts of tables and of the streams of a page are freed after use:
  they were never freed, and the memory grew at each typesetting of a
  document with tables
  ([#442](https://github.com/mgubi/texmacs/pull/442)).
- Markup typesets the same whether written out or produced by a macro or
  a `with`: the invisible places for the cursor at their edges no longer
  have a height nor move what is next to them. Scripts sat elsewhere (the
  E of the TeXmacs logo of beamer slides 3 pt higher, subscripts after a
  macro up to 2 pt off), a script could attach to such a place, empty
  macro lines took the height of the font cmr (loaded only for that), a
  line, a cell or a fraction ending with a macro counted the space after
  its last item, and an accent over a glyph glued to them lost its place.
  The cursor at the end of a macro or a `with` stays next to its content,
  and the slant of an italic letter at its end stays inside it (in
  `<em|f>)` the cursor was drawn inside the f); a bar over italic content
  (overline) is placed as the wide accents place it. Most documents move
  by less than half a point
  ([#448](https://github.com/mgubi/texmacs/pull/448)).

- Searching for a formula with wildcards or empty parts gives the
  right hits: a pattern with an empty part no longer hangs TeXmacs (its
  memory grew without bound); a wildcard at the end after a tag, or
  several hits in one tag or string, give one hit to the end, with no
  empty or inverted ones; a hit inside a string no longer hides a match
  which starts later in it; case-insensitive search also applies inside
  the tags of a pattern; a pattern which starts with a string that
  overlaps itself (aa_x in aaa_x) is found, and before a wildcard the
  match starts at the first occurrence, as for the same text in one
  string; and a wildcard in the middle which fails late is fast on long
  formulas
  ([#471](https://github.com/mgubi/texmacs/pull/471),
  [#472](https://github.com/mgubi/texmacs/pull/472),
  [#474](https://github.com/mgubi/texmacs/pull/474),
  [#475](https://github.com/mgubi/texmacs/pull/475),
  [#476](https://github.com/mgubi/texmacs/pull/476),
  [#478](https://github.com/mgubi/texmacs/pull/478)).
- **Jupyter**: without Python, no kernels are looked for: on Windows,
  each start of a plugin showed a warning and opened the window of the
  error messages
  ([#467](https://github.com/mgubi/texmacs/pull/467)).
- Search: a pattern with a long string is found fast, and no longer
  crashes; a string in the middle of a pattern must match a whole string
  of the text; patterns which start with a string followed by wildcards
  are found; the hits after a match inside a string are no longer missed
  ([#483](https://github.com/mgubi/texmacs/pull/483),
  [#484](https://github.com/mgubi/texmacs/pull/484),
  [#485](https://github.com/mgubi/texmacs/pull/485),
  [#486](https://github.com/mgubi/texmacs/pull/486)).
- Search: a formula with very many children (12000 and more) no longer
  crashes the search
  ([#491](https://github.com/mgubi/texmacs/pull/491)).

## 22 October 2026

- A subscript or superscript whose content comes from a macro argument
  is placed as the same script written out: after the italic correction
  of its content it kept part of the widening (up to 0.6pt with
  TeX Gyre Termes math)
  ([#426](https://github.com/mgubi/texmacs/pull/426)).
- **Windows**: the plugins which run one command per input (`:cmdline`)
  did nothing: their command is now run by the command interpreter
  (`cmd /c`), with its output read in the background, and stopping or
  interrupting it ends it and all it started
  ([#443](https://github.com/mgubi/texmacs/pull/443)).
- A table whose width in `par` is given by a value (as
  `<value|figure-width>` in the big figures) takes the width of the
  paragraph where it is placed, not that of the page: a figure in a
  frame or in a table cell was as wide as the page and off-centre, and
  two figures side by side overlapped
  ([#429](https://github.com/mgubi/texmacs/pull/429)).
- Stopping a plugin whose program ignores the termination signal no
  longer blocks the interface for 2 s: the program is killed and reaped
  later, in the background (Linux and macOS, all interfaces)
  ([#419](https://github.com/mgubi/texmacs/pull/419)).
- **Vue**: the window of the error messages (Debug > Error messages)
  behaves as an editor window: it opens at the size of a new editor
  window whatever the messages are, and they fill it, follow its size and
  scroll: they were three lines tall in a grey window as wide as their
  longest line. The same holds for any window or tab whose page is an
  editor.

## 21 October 2026

- A large delimiter of a negative size (`<left|[|-5>`, a bracket made
  smaller than its content) no longer crashes TeXmacs in the fonts which
  assemble their delimiters, such as Fira in a beamer presentation
  (`beamer` with `fira-font`): it has the size of the font
  ([#411](https://github.com/mgubi/texmacs/pull/411)).
- Part views: Document → Part also offers Show one part, Show several
  parts and Show all parts for the view itself, and in the modes one and
  several the list of its parts, as in an ordinary document
  ([#434](https://github.com/mgubi/texmacs/pull/434)).
- **Windows**: saving a part view (Document > Part) wrote the included
  files as `D/dir/a.tm` (the full path without the colon of the drive) in
  place of `a.tm`, and an included file open in its own buffer was not kept
  in step with its copy in the part view; both work as on the other
  systems.
- Mirrored texts, comments and the part views of a master are
  synchronized again after an edit which their incremental update could
  not apply: the synchronization was never scheduled with S7 (a test of
  the code of 2015 which Guile answered and S7 refused,
  [#431](https://github.com/mgubi/texmacs/pull/431); comments are
  mirrors, and every comment of a document showed a "wrong-number-of-args"
  error; a check, [#428](https://github.com/mgubi/texmacs/pull/428)).
- Document parts (Document → Part): the parts are told apart by their
  number, so that parts with the same title are different entries, and the
  menu shows the file of a part which starts with an included file
  (`Same (pa.tm)`); saving a part view keeps the parts and the inclusions
  of the master, which got inline copies instead; a change of style in a
  part view goes to the master when the view is saved (also to the master
  open in another window, which is left unsaved when it has changes of its
  own); a hidden part which starts with an included file no longer holds a
  second copy of it ([#77](https://github.com/mgubi/texmacs/issues/77)).
- Document parts: saving a part view no longer marks as saved the same
  file open in another window with unsaved changes of its own (they were
  lost on closing): the file is saved from that window when its changes
  are outside the text, else the save is refused with a message.
- Generated passwords take their randomness from GnuTLS at the level of
  keys, else from the system (`/dev/urandom`, `rand_s` on Windows), never
  from the random numbers of Scheme seeded by the time
  ([#66](https://github.com/mgubi/texmacs/issues/66)).
- Opening a large document is much faster: TeXmacs looked through the whole
  document for an encrypted part, which took seconds for a chapter of a few
  MB (and overflowed the stack of Guile with some thousands of paragraphs);
  it now looks only where an encrypted document keeps it
  ([#418](https://github.com/mgubi/texmacs/issues/418)).
- A reference to a label which was removed shows `?` after the update
  which removes it, instead of the old number until a second update
  ([#417](https://github.com/mgubi/texmacs/issues/417)).
- Update All no longer gets slower at each use in a master document: the
  lists of observers of its included files no longer keep the empty
  places of the observers which left
  ([#408](https://github.com/mgubi/texmacs/issues/408)).
- The fixes of the branch `wip_fixes` (fixes of the official TeXmacs)
  merged there on the evening of 20 October, in the items which follow.
- Keyboard shortcuts: removing a shortcut (also one of your own, in Tools →
  Keyboard) removes it whatever the way its keys are written (`S-C-`,
  `var`), which removed nothing before; redefining or removing the key of a
  command keeps the other keys of that command in the menus; the shortcuts
  made under a condition (`:require`) can be removed again.
- **Desktop**: a TeXmacs server makes its periodic backups (preferences
  Server backup, Rsync destination directory, Backup interval), which it
  never did, and a change of these preferences takes effect without a
  restart of the server; the users who may write in the database of a
  server may also set its fields and entries.
- Projects: after Document → Project → Attach master, the open views of a
  file find the references of the master at once (they needed a new view),
  and a renamed document is trusted or not according to its new name.
- Hyphenation: the patterns which end with the end of a word are used
  (`along-side`, `acety-lene`, `aero-stat` in English, which had no break),
  so the line breaks of some documents change slightly.
- Encodings: a file with a character which cannot be converted (a wrong
  byte, or a character the target encoding has not) is converted with a
  replacement character in its place; the whole file was left undecoded.
- Help: Tools → Update → Help reads again the folders of the documentation,
  so that a help page added or removed while TeXmacs runs is found by the
  search and the links (only a restart did it).
- Document parts: Save in a part view (Document → Part) writes the
  included files changed in it, which were dropped; a file open elsewhere
  with other unsaved changes is refused, and the footer names the files
  written.
- Security: the checker of the scripts of documents (`secure?`) refuses
  the expressions which could reach functions which are not secure (calls
  through variables, `set!`, renamed special forms...).
- Multi-file documents and links: `<shared>` works, an included file is read
  again after it is saved, a cycle of inclusions is reported instead of
  hanging, and several fixes of the parts and of the links.
- TeX fonts: the kerning at the end of a word is used for the positions of
  the cursor too, the ligatures of the Greek TeX fonts (final sigma before a
  punctuation) keep the punctuation, and some PK fonts load.
- HTML export: the files included with `<include>` are exported with their
  formulas, sections and numbers.
- PDF export: a hyperlink which starts a paragraph of an included file can
  be clicked.
- **Desktop (Qt, Cocoa)**: in the PDF export (the native renderer of the
  Qt and Cocoa builds; Vue exports with MuPDF, which these fixes do not
  concern), the bookmarks
  have no backslashes, the symbols of the TeX math fonts are in the text
  layer (copy and search), the web addresses are escaped, and a `<video>`
  is embedded as a video (with ImageMagick and ffmpeg), not as its current
  picture.
- Comments: a pasted copy of a comment is a comment of its own, not a clone
  which edits the other.
- Tables: a tall cell which spans several rows no longer sticks out above
  the table; decorations of cells no longer corrupt the memory (crashes at
  exit), and the structured moves keep the joined cells.
- Undo: undoing one step after a removal and an insertion at the same place
  takes back that step only.
- Macros: an `<arg>` error names the argument (garbage before), and the
  structured cursor moves (path-next, path-previous) follow the
  accessibility of the tags.
- An index entry in a section title no longer turns the page numbers of the
  table of contents and of the index into `?`.
- **Desktop (Qt)**: raster images (GIF, BMP, WebP...) are exported to PDF
  by Qt again, without ImageMagick; a picture whose file changed is loaded
  again; with Qt 5, the style sheet of a theme is scaled for screens of
  high resolution.
- AI sessions: LaTeX commands which begin with `\n` (`\nu`, `\ni`,
  `\nonumber`...) are kept in the answers, each session of an engine has
  its own history, the newlines of the input are kept, and a prompt with a
  control character is no longer refused.
- **Desktop**: the wallet (GnuPG 2.1 and later) can be opened, and a wrong
  passphrase is refused also after the right one.
- Colors and graphics: the reversed colors of the dark mode near grey are
  right, and frame and curve operations which stopped with "not yet
  implemented" work.
- Pictures: the size of an SVG no longer depends on the converter, and the
  PostScript printing of pictures with effects works.
- Long documents shown without pages (source files): typing is about a
  third faster in a file of 50000 lines.

## 20 October 2026

- **Desktop**: the balloons of a language server under the mouse stay
  while the mouse rests on the word, and go when it rests elsewhere: they
  were closed at the next small move of the hand (the cursor was said to
  have changed after each layout), and in Vue and Qt they took the pointer,
  so that a balloon over the next lines stayed there, and the words under
  it had none. The balloons of TeXmacs let the pointer through to the
  document ([#399](https://github.com/mgubi/texmacs/issues/399)).
- A book made of included files loads and updates faster: an included
  file keeps its layout from one typesetting to the next when its document
  is the same (each pass typeset again every chapter), and the attributes
  of the hyperlinks are no longer link vertices (each hyperlink was removed
  from one list of all of them, a quadratic time to close a document or to
  typeset again a part with many links). The load of a book of about 2000
  pages in 17 files went from about 52 s to 22 s, an insertion at the start
  of a chapter opened through the master from 15 s to 2 s
  ([#408](https://github.com/mgubi/texmacs/issues/408)).
- **Desktop**: the language servers of Julia: TeXmacs uses `jetls serve`
  (JETLS.jl), `fatou lsp`, or the package LanguageServer.jl run by `julia`,
  the first which is installed; it looked for a program
  `julia-language-server`, which is not a usual one
  ([#409](https://github.com/mgubi/texmacs/issues/409)).
- **Desktop**: the mathematical symbols of the TeX fonts (binary operators
  and relations such as ∘ and ·, Greek letters) were drawn as squares, in
  the document, in the PDF and in the menus, on a system with TrueType
  copies of these fonts, such as the fonts of LyX which matplotlib
  installs on Linux (`fonts-lyx`): TeXmacs took those copies, whose
  characters are at other places, rather than its own Type 1 fonts. A TeX
  font (one with a TFM of its name: `cmsy10`, `msam10`...) comes again from
  its Type 1 file; the other families keep their OpenType or TrueType file
  ([#407](https://github.com/mgubi/texmacs/issues/407)).
- **Builds, Linux**: the Vue interface compiles with GCC (it needed clang):
  the fields of the Clay layouts are given in the order of their
  declaration, as C++20 requires, and the font writer of the PDF export is
  split so that GCC 13 no longer stops with an internal error. A Vue build
  of GCC also no longer aborts when it quits ("free(): invalid pointer").
- **Windows**: the documents in the TeXmacs folder of the user were not
  trusted (their scripts asked first): the folder, whose path begins with
  a drive such as `C:`, was read as two paths.
- **Browser**: the page says how long the browser keeps your files. At the
  first save of a file it asks the browser to keep them
  (`navigator.storage.persist()`: Firefox asks the user, Chrome decides
  by itself, Safari grants it mostly to Home Screen web apps); the top of
  File → Files in this browser says "Kept until you clear the data of this
  site" or "The browser may delete these files", with the date of the last
  backup and a Back up button; and a short reminder at the bottom of the
  page, at most once a day, when there are documents and no backup for
  seven days. The help page and the limitations in the About text say
  the same, with what Safari does
  ([#406](https://github.com/mgubi/texmacs/issues/406)).
- **Vue, Cocoa**: the main and mode icon bars can be put at the right of the
  document (Preferences → General → Main and mode icon bars: At the right
  of the document; `?bars=right` in the browser), in two columns mirrored
  from those at the left: the main bar along the edge of the window, and
  the menus of the buttons opening to their left
  ([#404](https://github.com/mgubi/texmacs/issues/404)).
- **Site**: a page for [TeXmacs Fixes](https://mgubi.github.io/texmacs/fixes.html),
  the conservative version: the stock TeXmacs (Guile 1.8, Qt 6) with the
  bug fixes of `wip_fixes` and nothing else, for macOS (Apple silicon and
  Intel), Linux (an AppImage) and Windows (an installer and a zip), built
  by the CI of `wip_fixes` (`fixes_ci`) into the pre-release `fixes-latest`.
  It installs beside the stock TeXmacs and TeXmacs Upsilon, and shares the
  home of the stock TeXmacs.
- **Vue**: the input fields of the dialogs copy and paste the accented
  letters and the symbols as they are, in UTF-8: an alpha copied became
  the text `<alpha>` (before, nothing), and an accented letter pasted from
  another application became garbage.
- The fixes of the branch `wip_fixes` (fixes of the official TeXmacs)
  merged on 20 October, in the items which follow.
- Undo: Tools → Miscellaneous → Clear undo history clears the history of
  the current document only, and a saved document stays saved: it cleared
  the history of every open document and marked them all as modified.
  Undoing back to the saved state makes a document unmodified again after
  several changes inside one command. An undo after a menu action, in a
  document where another author (or a script) changed something before,
  takes back the menu action, not the step before it or the whole history.
- Undo and redo keep the numbers of the automatic labels: a section
  removed and put back by an undo, or inserted, undone and redone, keeps
  its number, where it took a new one each time.
- A cancelled change (after an error in a key press or in a menu action)
  no longer puts a document which was closed back in the place of another
  document, which then showed the closed one.
- Document parts: undo and redo put the cursor where it can be seen, not
  in a part which is hidden or in the preamble which is not shown, where
  typing went unseen.
- **Desktop**: on the command line, `-q` quits after the files and the
  commands of `-x`, `-c`, `-W` and `-U`, which it dropped when they came
  after it, and the headless mode (`-headless`) loads the files and runs
  `-test-suite` before it quits; the files and the commands of `-x` are
  run in the order of the command line (`texmacs file.tm -x cmd` runs `cmd`
  in `file.tm`). `-build-manual`, `-reference-suite` and `-test-suite`
  without their argument no longer keep TeXmacs running for ever.
- Security: `<use-module|...>` in a document only takes the names of
  Scheme modules (`(utils misc doxygen)`) and of plugins; anything else is
  ignored with a message: a document could close the expression and add
  its own code, which ran when the document was typeset. The documents in
  `system/tmp` and `system/cache` of the home folder (downloads, documents
  taken out of PDF files) are no longer trusted: their scripts ask before
  they run.
- Pictures given by a web address are downloaded again by Tools → Update →
  Pictures, and once in each session (with an effect, such as in the art
  boxes, they were made once and never fetched again); offline, the copy of
  before is kept.
- **Desktop**: a download from the web gives up when the server does not
  answer for the time of the preference `http request timeout` (10 s): a
  picture or a file from a server which never answered held TeXmacs for
  ever. A download which fails, or a page of error, no longer replaces the
  copy of before.
- LaTeX export: struck-through text is `\sout` of `ulem` and highlighted
  text `\tmmarked` (they were `\strikethrough` and `\marked`, which LaTeX
  does not know, so that `pdflatex` stopped), with lines which break with
  the text, also around formulas, inside groups and in titles; columns load
  `multicol`, and the colours given by value (`#ff8800`) are defined under
  a name, as `xcolor` wants them. The preference "Store tracking
  information in LaTeX files" works: when it is off, the export with source
  tracking no longer adds the TeXmacs source at the end of the file.
- Bibliographies with BibTeX: many fixes of the BibTeX engine of TeXmacs
  and of the bibliography databases. Pages are one page or one range (`12--34`,
  `e1002-e1010`, `xii-xv`, also with an en dash), other pages are kept as
  written; `crossref` and `xref` are kept as keys; the months `jan` to
  `dec` are defined as in BibTeX; an undefined `@string` gives nothing,
  with a warning; a field whose LaTeX gives nothing (`\relax`) is empty;
  the external `bibtex` runs in a folder of its own and its errors are
  shown. In Document → Bibliography, the preview shows the first ten
  entries (with those they cross-reference) and Modify keeps the name of
  the file in the document. The databases keep their history across
  synchronisations and reloads, apply the offset and limit of a query to
  its results, and keep new local entries.
- `<symbol|945>` gives an alpha: a code above 255 is a Unicode code point,
  it lost its upper bits.
- A Scheme expression evaluated with M-x, such as `(insert "<alpha>")`,
  inserts an alpha again: the names of the symbols were dropped. The
  raw HTML of the HTML export is in UTF-8, as the rest of the page.
- Version control: Version → First difference and Last difference find a
  difference at the very start or end of the document; the block grain
  compares whole paragraphs.
- A table macro in source mode (a `tformat` without a table) no longer
  crashes or hangs TeXmacs on Shift+Up (savannah #62850).
- Graphics: ungrouping or removing a group just after making it works
  (it removed the object and stopped with an error).
- The side tools of a window (search and replace, the format tools) no
  longer close another window when their document is closed.
- Source → Quasiquote inserts the quasiquote tag; closing a document
  closes all of its views (every other one stayed, pointing to the
  closed document).
- A file of preferences with a single preference is read: that preference
  was lost at the next start.
- **Desktop (Qt)**: the TeXmacs server closes the socket of a client once,
  not a second time when it may already be the socket of another client.
  **Desktop (Windows)**: the programs run by TeXmacs close each of their
  pipes once.
- A document saved under a name without a suffix is saved as a TeXmacs
  file, with `.tm` added: it was saved as plain text. A source file keeps
  the name it is given (a `Makefile`). **Desktop (Vue)**: the dialog of the
  system for saving a document the first time proposes `untitled.tm` in the
  folder; it proposed the name of the folder, on Linux the name of the user
  ([#400](https://github.com/mgubi/texmacs/issues/400)).
- **Desktop (macOS, Cocoa and Vue)**: the dialog to save a document adds
  `.tm` to a name typed without a suffix before it asks about replacing a
  file: it asked about the file of that name without the suffix (saving as
  `test` warned about a file `test`), and then replaced an existing
  `test.tm` without a question. **Cocoa**: the dialogs to open and save
  files show those of the type (TeXmacs documents, images...), with a menu
  for all the files, as in Qt; they showed every file. **Vue**: the filter
  of the type in the dialogs of the system has the suffixes of the type
  (it took every file). Where a dialog still gives a name without a suffix,
  TeXmacs asks before replacing the `.tm` it saves to
  ([#400](https://github.com/mgubi/texmacs/issues/400)).
- Tests: the dialogs of the system to open and save files, on macOS with
  the three interfaces (`tests/gui/save-dialog`, `open-dialog`;
  `docs/gui-scripting.md`).
- In a source file which a language server follows, the mouse resting on
  an error shows its message (and then what the server says of the word):
  the balloon showed only the latter, which for an unknown name is
  nothing, and each one gave warnings, "The required path does not exist",
  which Qt showed in a window
  ([#399](https://github.com/mgubi/texmacs/issues/399)). The same with
  Shift+F12 at the cursor. The lines of the balloon are no longer
  justified.
- **Remote tools**: in Remote > Client preferences, three switches, all on
  by default: the heartbeat which watches a connection, the window opened
  when a connection is lost (now once for connections lost together, not
  one per server), and the cursors of the others in live documents. A live
  document closed is no longer followed every 300 ms, and the cursors of the
  others are drawn again only when its views change; the server no longer
  sends cursors to a TeXmacs which does not know of them, nor keeps those of
  a client which left. The sentences of the connections are translatable,
  and Return in the field of Remote > Rename renames, as "Ok" does.
- In a live document, a move of the cursor of another user repaints the
  places where it was and where it goes, not the whole window, which every
  move of every user did.
- **Desktop (Qt)**: Insert → Session → Preferences opens the preferences of
  the AI sessions, where the API key is given: it stopped on an error, and
  a session asked a question without a key hung, since it opened them to
  ask for the key ([#398](https://github.com/mgubi/texmacs/issues/398)).
  A row of a dialog with two buttons, such as Edit and Default of the
  instructions, threw its columns out of step.
- **Vue**: an icon of one's own in SVG (`(icon "pen.svg")` in a
  `menu-bind`) is drawn at the size of the icons of the bars when its file
  declares no size (only a `viewBox`) or one too large for a bar, as Qt
  does: it was drawn at the size of its `viewBox`, 1000 points, and took
  the whole bar (#394).
- **Desktop (Vue)**: the wheel over a graphics moves the graphics as with
  Qt: it moved it 256 times too little, and the other way.
- A list of thanks to the people who reported problems or sent code: in
  Help > TeXmacs Upsilon features (section Thanks), on the site
  ([thanks](https://mgubi.github.io/texmacs/thanks.html)) and on GitHub,
  all from one file, `TeXmacs/doc/about/welcome/upsilon-thanks.md`.
- **Desktop (Vue)**: on Linux and Windows the interface is written in Fira
  Sans, a sans serif as the system font of the Qt port, instead of Computer
  Modern, the look of the old X11 interface (#391); macOS keeps Lucida
  Grande.
- **Vue**: the input fields of the dialogs take the keys of the fields of
  Qt: on Linux and Windows control-c, control-x, control-v, control-a,
  control-z and control-shift-z (or control-y) copy, cut, paste, select all,
  undo and redo, as do shift-insert, control-insert and shift-delete; on a
  Mac the same with command. Control-v did nothing, so a path could not be
  pasted in the file chooser, and control-c cancelled the field (#391).
  The menu of the right button has Undo and Redo too.
- **Desktop (Vue)**: on Linux and Windows a notch of the mouse wheel
  scrolls as far as in the Qt port, three lines of a sixteenth of the
  window, and the wheels with fine steps (which Wayland gives in eighths of
  a notch) are no longer taken for a trackpad, which scrolled them eight
  times slower (#391).
- **Desktop (Vue)**: the dialogs are dialogs for the window manager: on
  X11 they have the type of a dialog, and on Linux and Windows the document
  window as their parent, so that the tiling window managers (i3, sway,
  Hyprland, bspwm) float them instead of tiling them (#391).
- **Desktop (Vue)**: on X11 the text copied from TeXmacs can be pasted in
  every application: it is also offered in the formats of X11 (`UTF8_STRING`,
  `STRING`), which `xclip`, `xterm` and the older toolkits ask for.
- **Desktop**: TeXmacs Upsilon builds again with Qt 5 (the Qt of Ubuntu
  22.04 and of the distributions of its age), which failed at the link
  (#389); with Qt 5 the requests to web services (the AI sessions, Zotero
  on the web) are not available. The CI now builds it with Qt 5 and GCC.
- **Desktop**: TeXmacs Upsilon has the flat icon (the one of Vue and of the
  browser version) on every system and with every interface, instead of the
  chip of the stock TeXmacs, so that the two are told apart at a glance: the
  macOS applications, the AppImages, and on Windows `texmacs.exe`, the
  installers, the Start menu and the uninstaller.
- **Desktop (Qt)**: a question asked in the footer is no longer answered
  (empty) when you go to another application: you answer it when you come
  back.
- **Desktop (Cocoa)**: the tree views of the dialogs show the text of each
  item, as in Qt, instead of the name of its tag (`item`), and the data of
  an item are no longer shown as rows under it.
- The tests of the interface can drive TeXmacs on Vue, Qt and Cocoa alike,
  through one driver in Scheme: they click the texts and icons they name
  by the bars and menus they are in (`Format > Font > Bold`, `footer >
  equation*`, `mainbar > icon:tm_save`), in English whatever the language
  of the interface, and check what came out (`src/docs/gui-scripting.md`).
  Seven tests run on all three (`tests/gui`).

## 19 October 2026

- TeXmacs Upsilon installs beside the stock TeXmacs without touching it.
  **Desktop**: on macOS the applications are "TeXmacs Upsilon" (Cocoa),
  "TeXmacs Upsilon Vue" and "TeXmacs Upsilon Qt", each with preferences of
  its own, and they open `.tm` files only when the stock TeXmacs is not
  there or when they are chosen (all three were `TeXmacs.app`, with the
  identity of the stock one). With Qt, a TeXmacs Upsilon and a stock
  TeXmacs running together no longer pass their files to each other.
  On Linux there are AppImages of the Qt and the Vue builds, one file each
  for most distributions, with menu entries and icons of their own. On
  Windows there are installers of the Qt and the Vue builds, with folders,
  Start menu entries and uninstallers of their own, which no longer write
  `TEXMACS_PATH` into the environment; TeXmacs Upsilon uses the files it
  is installed with even when the stock TeXmacs has set `TEXMACS_PATH`.
- **Browser**: on a touch screen, a finger put down on the handle of a
  scroll bar drags it: it moved the view the other way.
- **Desktop**: with the Vue interface on Linux, the windows are drawn by
  the graphics card also on a system without the packages of development
  of OpenGL: the library which draws the vector graphics (ThorVG) looked
  for a name of OpenGL which only those packages have, and TeXmacs then
  drew without the graphics card, which was slow and broken (the main
  window stayed empty, the dialogs were malformed and slow to resize,
  [#376](https://github.com/mgubi/texmacs/issues/376)). A dialog also gets
  its size before it is drawn.
- **Browser**: on a tablet (an iPad), a keyboard button at the bottom right
  of the page opens the keyboard of the system, and closes it: no keyboard
  came without an external one
  ([#377](https://github.com/mgubi/texmacs/issues/377)).
  `?keyboard` in the address shows the button anywhere, `?keyboard=0` hides
  it.
- **Desktop**: TeXmacs Upsilon has a home of its own, `~/.TeXmacs_Upsilon`
  (on Windows `%APPDATA%\TeXmacs_Upsilon`), for its preferences, caches and
  the files of the user, instead of the `~/.TeXmacs` of the official
  TeXmacs. Neither changes the settings of the other, and TeXmacs Upsilon
  no longer loads what was made for another version, such as plugins and
  Scheme code in `~/.TeXmacs`; nor does it look for plugins in
  `/usr/share/TeXmacs` and `/etc/TeXmacs` any more. Nothing is copied: to
  use your own files of `~/.TeXmacs` with it, copy them into the same places
  of `~/.TeXmacs_Upsilon`. The handwriting examples follow
  `TEXMACS_HOME_PATH` too.
- The interactive status bar (View → Interactive status bar, in Vue and
  Cocoa) has a **Document** menu on its left: the style, paper, fonts and
  language of the document, wherever the cursor is. Each tag around the
  cursor on its right is a menu: Select, then its Focus menu, whose
  commands change that tag where it is, without moving the cursor (from
  inside a strong, the theorem around it becomes a lemma). They act on that
  tag also when one of its kind is inside it: a displayed formula with an
  inline formula in it becomes inline from its own menu.
- **Browser**: after a turn of the wheel of a mouse the page no longer goes
  on moving for two seconds (the wheel was taken for a trackpad, after which
  the page glided); the page glides only after a finger, the browsers and
  the systems glide after a trackpad themselves
  ([#375](https://github.com/mgubi/texmacs/issues/375)).
- **Desktop**: with the Vue interface on Linux, a wheel turned fast scrolls
  as far as one turned slowly (the notches which came fast were taken for a
  trackpad, and scrolled 10 points instead of 80;
  [#373](https://github.com/mgubi/texmacs/issues/373)); where no file
  dialog of the system can be shown (neither zenity nor the desktop
  portal), TeXmacs shows a file chooser of its own, made of its widgets in
  Scheme, as the one of the X11 version: a folder, its folders and its
  files, a name, the files of the format or all of them
  ([#374](https://github.com/mgubi/texmacs/issues/374));
  `TEXMACS_VUE_NO_FILE_DIALOG=1` asks for it everywhere. Its window can be
  resized, **Up** goes to the folder above, a line shows the format of the
  chosen file, and the pointer over a file shows its format, its
  size and its date. The wheel scrolls the list under the pointer.
- With the Vue interface, Escape closes a dialog, as its close
  box does.
- **Browser**: touch screens (an iPad, a phone). A tap on an item of a menu
  chooses it: nothing happened, the items of the menus only worked with a
  mouse or a trackpad. A finger dragged over a document scrolls it, and goes
  on gliding when it is lifted while it moves: it selected text, and a
  document could not be scrolled without a mouse or a trackpad. A finger which
  rests a moment before it moves selects, as a mouse does.
- Bibliographies in a style of the Citation Style Language (APA, IEEE,
  Chicago...) keep the capitals of the titles which the `.bib` file protects
  with braces (`Introduction to {Fourier} Analysis` gave "Introduction to
  fourier analysis"), and the titles in another language (a `language` field)
  stay as written, also when the database tool is on.
- **Desktop**: with the Vue interface, TeXmacs could crash when another
  program read the clipboard just before a new copy (the data of the former
  copy had been freed).
- **Desktop**: the Windows builds with Qt pass all the test suites and are
  among the downloads again; the macOS disk images are smaller (compressed
  with lzma).
- Program code: a long region of code is parsed by parts, and a long document
  without pages is typeset by chunks of lines, so that typing in a long file
  stays fast; the footer names the tab of a source file.
- The main page of the site, <https://mgubi.github.io/texmacs/>, is now
  TeXmacs Upsilon and its builds (it was TeXmacs in the browser, which is at
  [texmacs.html](https://mgubi.github.io/texmacs/texmacs.html); the links
  with options, such as `?open=`, still open it), with a picture of OpenType
  mathematics. The features page shows the Cocoa interface too, and has the
  Scheme interpreters and the typesetting with their measures against the
  standard TeXmacs.
- Two new pages on the site: the
  [features in pictures](https://mgubi.github.io/texmacs/features.html)
  (also [on GitHub](upsilon-screenshots.md)) and this list of changes.

## 18 October 2026

- This version of TeXmacs is now called TeXmacs Upsilon (it was Max's
  TeXmacs, and TeXmacs Vue in the browser), and its branch `upsilon_texmacs`.
  **Browser**: the menu of the page is **Upsilon**, and its title and its
  messages say TeXmacs Upsilon; Vue remains the name of the interface.
- Help → Upsilon features, a new page, says what this version of TeXmacs adds
  to the official one.
- A page of the site, [downloads.html](https://mgubi.github.io/texmacs/downloads.html),
  lists the builds of TeXmacs Upsilon for macOS (Cocoa, Vue and Qt), Linux and
  Windows (Qt and Vue), which anyone can download.
- **Desktop**: the packages are smaller (the Vue disk image of macOS from 235
  to about 180 MB): MuPDF is built without the fonts of its own, which
  TeXmacs does not use, as in the browser. The Qt builds no longer use MuPDF
  at all, which made them crash.
- The table at the cursor shows the lines between its rows and its columns,
  as in the Qt version (**Show table cells** in the preferences, now on by
  default with Vue and Cocoa too).
- A large zoom in a long document (a full manual, far from its start) no
  longer breaks the view: the page went off the screen, and scrolling left
  smeared copies of the text. The zoom now stops before the coordinates of
  the window overflow (in the full manuals, at about 800% far down; a short
  document still goes to 2500%).
- Program code is colored by its syntax tree, in 22 languages (C, C++,
  Python, Scheme, Java, JavaScript, TypeScript, R, Julia, Shell, Go, Rust,
  Lua, Lean, Haskell, OCaml, HTML, CSS, TOML, YAML, JSON and LaTeX), also
  inside the scripts and styles of HTML; a bracket without its partner is
  shown in red.
- In program code, brackets and quotes stay balanced as they are typed and
  deleted, Return closes a block with its keyword (`end`, `fi`...) or an
  environment of LaTeX, and the end tag of HTML comes with its start tag.
- The menu Focus of program code goes from definition to definition,
  selects, deletes, swaps and comments whole parts of the code, removes or
  adds brackets, and folds blocks; the lines are indented by the structure of
  the code. Folds nest, and change only the view: the hidden lines are
  printed.
- In program code, the syntax errors are underlined after a pause in the
  typing, and `F8` goes from one to the next; `F12` goes to the definition of
  the name at the cursor, and the outline of the code shows on a page; tab
  completes a name with the names defined in the code.
- Language servers: the code blocks of a document are each a document of the
  server of their language, or, for a language, all of them as one file (a
  name defined in a block is known in the next ones), with colors and hints
  in the blocks.
- The lines of program code are cut into the tokens of their syntax tree,
  and typing in a long source file costs much less (a file of 100000 lines of
  C: three times less).
- In a source file which is indented by tabs, tab in the search looks for a
  tab, and the columns between the tab stops (Edit → Preferences) change in
  the files which are open.
- Chatbots: files can be attached to the questions (**Attach** in the focus
  bar of a session or of an executable fold, or a file dropped on the page
  while the cursor is in a question). The files of a conversation are
  checked and limited, and neither an answer nor a document can make TeXmacs
  read or send a file of the system; the Context of Ollama and Albert is
  set; with Qt 6, the timeouts and the errors of the requests are reported.
- **Browser**: the windows are drawn with WebGPU in Chrome and the other
  browsers of Chromium (Edge...), as fast as with WebGL2 and faster when
  zooming; the other browsers draw with WebGL2 as before. `?gpu=webgpu`,
  `?gpu=webgl` and `?gpu=0` (MuPDF) in the address of the page choose; the
  menu **Upsilon** says which one draws.
- femtolisp, the third Scheme interpreter (an option of the build): flat
  closures, inlined `let`, and three bugs of the original fixed.

## Until 17 October 2026: the browser version

Until 17 October this list was kept for TeXmacs in the browser
(Help → TeXmacs in the browser, which still has it). Most of these changes
concern the desktop applications too: the converters, the citations, the
fonts, the chatbots, the editing.

### 17 October 2026

- At a large zoom the letters no longer disappear.
- A page made as wide as the window (View → Fit to screen width) is in its middle, not against its left side.

### 15 October 2026

- The text on the screen is darker, as heavy as in TeXmacs on the desktop, and drawn as fast as before.
- Edit → Preferences, tab General, chooses how the letters are drawn on the screen (Glyphs on the screen), how bold they are and how they are fitted to the pixels; a change shows at once.
- Word and OpenDocument files keep their formatting by hand in both directions: the size, font, color and highlighting of the text, capitals, lower case and words beginning with capitals, the kinds of underlines and of lines through, the spacing of the letters, and the indentations, spaces, line spacing, background and border of the paragraphs. A font which TeXmacs does not have is shown in the closest family and written back with its name, and the titles of the sections take the look of the headings of the file.
- A picture with the text around it in a Word or OpenDocument file is shown on its side, in a paragraph of its own, and floats again in the file which is written. Cropped pictures of OpenDocument files stay cropped, and the line between the columns of a section is kept.
- A label in the text of a Word or OpenDocument file comes back at its place after a save, the captions of the figures of LibreOffice are recognized, the underline of links no longer changes when Word saves the file, and a formula alone in a cell stays in the cell.
- Boxes of text and shapes (rectangles, ellipses...) of Word and OpenDocument files are shown as framed boxes with their fill, on their side, their text can be edited, and they are written back as boxes and shapes.
- After a review of the Word and OpenDocument converters, text is no longer lost in formulas with tracked changes, after a field which does not end, in ruby text or in repeated table rows; the numbering of lists, the references to pages and the formatting switched off inside a paragraph come out right; and the files which are written have no section breaks in cells or notes, keep the dashes and quotes of TeXmacs and define the styles they use. Addresses of links and descriptions of pictures written with `&amp;lt;` or `&amp;amp;` are read correctly. A file saved in the other format keeps the look of its named styles, the bibliography of Zotero in a Word file stays a field, pictures linked to the web stay links, a comment in a note stays in the note, and a formula on its own line inside a paragraph is shown on its own line.
- A footnote in a formula is no longer lost in Word and OpenDocument files: its mark follows the formula, and the point after the mark stays with the formula when the file is read. A paragraph in a style `Caption` which is not under a figure keeps its look, plain paragraphs no longer take the look of another style, and a style of the document on the text of a link is kept in Word.
- A Markdown file keeps the tabs at the start of its lines (in code, formulas, tables and the YAML header), a formula of a single symbol is read back as a formula, and files with many formulas open faster. After a review, typographic quotes stay as they are, a file which begins with a byte order mark or holds an entity such as `&#1e3;` is read, `<pre>` and `<script>` blocks run to their closing tag, and the Markdown which is written reads back the same in more cases (nested lists, emphasis next to code, links). All the named entities of HTML5 are read.
- A Word or OpenDocument file which was changed in TeXmacs is saved back with the changed headers, footers and styles, and with fields inside fields; the title of a file keeps the look of its style; a document of the style `office` exports to LaTeX.

### 14 October 2026

- A Markdown file keeps more of itself when it is saved: its tabs, the definitions of links to which nothing refers (and the comments `[//]: # (text)`), footnotes defined in quotations or lists, marked several times or not defined, and a YAML header with any keys. Its export to LaTeX compiles.
- Word and OpenDocument files of any length are read and written (ten thousand paragraphs stopped the converters), and a damaged or forged file gives a message instead of ending TeXmacs.
- A table of a Markdown file keeps the links and footnotes whose definitions only it uses, a heading between two rules is no longer taken for a YAML header, and a header edited in TeXmacs is read back as a header. Many footnotes are written much faster.
- A small forged Word or OpenDocument file can no longer keep TeXmacs busy for hours.
- Word and OpenDocument files keep their page (size, orientation, margins) and the look of their usual paragraphs (size of the font, a font without serifs, alignment, indentation, spacing) in both directions, the ways of numbering of their lists, and their headings without numbers. The text of boxes and shapes is imported, hidden text is left out, and the chapters of a book are exported with their numbers.
- A Word or OpenDocument file opens in the new style `office`, which keeps its named styles, headers and footers, fields, comments, tracked changes and columns, and writes the file back with its own styles, pages and settings. Cropped pictures, pictures of Windows (through LibreOffice), arrays of equations and all the screens of a presentation are converted too.

### 13 October 2026

- A Markdown file is opened as a document of the new style `markdown`, with text without serifs and headings without numbers, and is saved as it was read: its YAML header, its formulas, tables and Html keep their text, and so do the levels of its headings, its lists, its code, images and footnotes, as long as you do not edit them. See Help → Reference guide → Compatibility with other formats.
- A TeXmacs server now answers the browsers at a port of its own, the one after its usual port (6562 for 6561), and leaves its usual port to the other versions of TeXmacs. Nothing changes in the login: the port to give is still the one of the server, and the page goes to the next one. A server started by an older TeXmacs Upsilon has to be updated together with the page.
- To a server on another computer, a login over TLS goes through secure WebSockets (`wss`), whatever the page; to a server on the same computer it stays in clear, without any certificate. Such a server serves the browsers of other computers when its preference `server websocket` is `on`.
- Editing a long document is faster. A new section, a numbered equation or the join of two paragraphs used to typeset everything after it again, since the numbers changed: now only what shows one of these numbers is. The page breaks are searched faster, and not at all when no line changed its height; the pages which did not change, with their headers and footers, are no longer put together again at every key. In the user manual opened as a book, some 380 pages, a letter takes about 50 ms in this page instead of 730, the join of two paragraphs 50 instead of 2100 and a new section 200 instead of 3200 (the typesetting alone, on a recent laptop; the pages are the same as before).
- The entries of a table of contents and of an index keep pointing to their places when sections or index entries are added before them: they no longer wait for Document → Update → All to be right again.
- A long document sometimes lost what comes at its end when it was made by a program: the user manual, opened as a book from the Help menu, was now and then shown as one endless page instead of its pages. The memory of such a document could be reused while it was still being built.

### 9 October 2026

- Font design, in the menu of the fonts (the font button of the focus bar), opens a page to choose the fonts of the document by looking at them. Its top lists the parts of a document with the font of each: the text, the mathematics, the sans serif and the typewriter text, and the blackboard bold, calligraphic and fraktur letters of the formulas. The name of a part is a button which shows the fonts for it, each with a few words, a sample of that part and a button to choose it. The choice is listed at the top of the page with a sample, and Use for the document gives it to the document. The samples are pictures: the page fetches no font but those of the choice. Such fonts show as Custom on the font button; Save as on the page, or Save these fonts in the menu, keeps them under a name, which the menu then offers for every document (Saved fonts).
- A slow network no longer leaves signs out of the first screen. The fonts of the first screen which are not among the files of the start (those of the welcome page and, on a Mac, the one of the command sign and of the other keys of the shortcuts in the menus) are fetched with those files, under the progress bar: TeXmacs used to fetch each when it first drew with it, holding the page meanwhile, and a request which failed left that font out until the next visit (a question mark for the command sign). A request which fails is now made again. A font which still does not come is asked for in the background, the page going on without it, until it comes: the documents and the menus are then drawn again with it.
- A login from the browser to a server of another version of TeXmacs, such as `cloud.texmacs.org`, fails: a page has only WebSockets, which those servers do not answer. The message now says so. Until they do, a bridge on your own machine passes the connection on: run `node misc/wasm/remote/ws-bridge.mjs` (from the sources of TeXmacs Upsilon; `cloud.texmacs.org` is its default), and log in with the server `localhost` and the port 6562 (the bridge listens at the next one, as a server does).
- The menus drop in without stopping halfway. On a display of 120 Hz the page drew a frame every 8 ms, more than the graphics card follows with a large window: the browser then held the frames back for a tenth of a second at a time, in the middle of the movement at an interface scaling of 2. The page now draws 60 frames a second at most.
- The interface follows the zoom of the browser. Its size was that of a whole number of pixels of the screen per point: on a display of density 2, a zoom of 115% or 120% made it smaller, and one of 125% larger than asked.

### 8 October 2026

- More fonts come with TeXmacs, those of the ConTeXt distribution which were missing: DejaVu (serif, sans serif, typewriter and condensed, with its mathematical font), Gentium Plus, the Polish families Antykwa Torunska, Antykwa Poltawskiego, Iwona and Kurier, the light, semi bold and condensed faces of IBM Plex, and the typewriter of variable width of Latin Modern. As the other fonts, each is fetched when a document first uses it.
- The menu of the fonts (the font button of the focus bar, and the font in the footer) sorts them by design: Serif, Sans serif, Typewriter and Decorative. A submenu lists first the fonts which come with their mathematics, such as Palatino, DejaVu or Fira, then those which change the text only, such as Gentium Plus or Iwona. Text with mathematics was a long list in the menu itself, and the text fonts were apart.
- The documents of Word (`.docx`) and of LibreOffice (`.odt`) are opened and written by TeXmacs itself, in the browser too: add such a file to the files of the browser and open it, or use File → Export → Word and File → Export → OpenDocument and save the result on the computer. Formulas, tables, images, footnotes, the numbers and the references are kept in both directions; see Help → Reference guide → Compatibility with other formats. Markdown files (`.md`) are read and written in the same way.
- The shadows of the chat rooms and of the remote directories are grey again: in the browser they were stripes of green and of noise. The browser uses the small copies of the pictures of the shadows which come with TeXmacs, in grey with transparency, a format which was read as if it were in colours. And the pieces of a shadow for which a box has no room are left out: they were drawn whole, across the box and the page.
- The remote tools say what happens with the server. The footer says `connecting to...`, then `connected to ... as ...`; the top of the Remote menu and the cloud icon say `Connected to host as user (40 ms)`, the time being that of the last answer of the server, which is asked for a sign of life every ten seconds. A server which does not answer for 25 seconds is said so, and when it answers again; meanwhile its cloud has an amber badge, in place of the green one of a connection which works. The cloud is struck through after a connection was lost. A connection which ends otherwise than by Remote → Logout is said to be lost, in a dialog and in the menu (`Connection lost: user@host`), until you log in again; a login to a server which is not there, or which never answers, fails with a message after ten seconds. It all went unsaid before: the menus stayed as they were and the requests waited for ever.
- In a live document you see where the others are: the cursor of each other user is a bar of a colour of its own, with the name of the user above it, which follows what that user does; Remote → Live document lists who else is in the document.
- The input fields whose width is left to the interface (the passphrases of GnuPG and of the wallet, the names of the remote files) have the usual width of a field, and take the room of their dialog: they were 30 points wide, three or four letters. The widths in `em` of the dialogs follow the font of the interface.
- The thumbs of the scroll bars are never shorter than 30 points: in a long document they shrank to a few pixels, which the mouse could hardly pick.
- The tabs of the windows have a cross to close them and nothing else: their menu, with Move to a new browser tab, is gone.
- Save a backup (it was called Download a backup) and Restore a backup... are buttons of File → Files in this browser... too, next to **Close**.
- File → Files in this browser... has a third place, **Your TeXmacs folder**, between **Your files** and **Files of TeXmacs**: it shows `.TeXmacs`, with the preferences and the styles, packages, fonts and Scheme code you added, which can be opened, renamed, deleted, saved as copies and added to as your other files. It is there in the dialogs to open and to save too.
- The remote tools (Tools → Remote tool, then the Remote menu) were checked against a TeXmacs server, from the browser and from the desktop version, alone and together in a chat room and in a live document. The corrections: the name asked by Remote → Rename has the width of its dialog (it showed a few letters), and its button Ok takes the name as typed (it renamed nothing unless Return was pressed in the field); the preferences of the server say whether the server has `rsync` (they showed an error); the history of a remote file keeps a version saved again within five seconds; the tabs of remote directories and of the lists of chat rooms, shared resources and live documents have a title, not their address; and the browser, which cannot be a server, no longer offers Remote → Start server.
- Save a backup and Restore a backup..., in the menu **Upsilon**: all your files and your preferences in one zip archive, to keep them safe or to move them to another browser (see **Your files**).
- Edit → Preferences → General → Interface scaling is obeyed, as in the Qt version, and at once, without a new start of TeXmacs: the whole interface, with the documents and the windows themselves, is larger or smaller by that factor. It had no effect. In the browser it scales what TeXmacs draws, not the tabs and the menu of the page, which follow the zoom of the browser; the address `?scale=1.5` sets it for one visit.

### 7 October 2026

- A new look, in the colours of before: rounder menus whose items follow their corners, rounded tabs in the column of tabs, and short animations when the menus, lists and dialogs appear.
- A new icon set, Lucide, with thin lines and pastel colours inside which tell the kind of an action (documents in blue, editing in amber, searching in violet, inserting in green, the look of the text in rose): choose it in Edit → Preferences → General → Icon set (the default is still the neo-classical set). The buttons of the icons are square, and the two columns of icons at the left match.
- The browser asks before closing or reloading the page while documents have unsaved changes.
- Each tab of the column has a menu (its `...`, or a right click): Move to a new browser tab moves its document to a new tab of the browser.
- The other tabs of the browser in which TeXmacs Upsilon is open keep the documents they save (only the preferences are kept by one tab), and each tab sees the documents the others saved.
- The page is built from the branch `upsilon_texmacs`, which gathers the other lines of work of the repository: it has their newest state (the OpenType mathematics, the citations from Zotero, the icon sets) and the whole developer documentation (Help → Developer documentation). Its sources are on [GitHub](https://github.com/mgubi/texmacs/tree/upsilon_texmacs).
- More fonts for text and mathematics come with the page: Garamond, Old Standard, IBM Plex, Noto Sans, Lete Sans, GFS Neohellenic and Computer Modern Sans (in the font menus of the footer and of Document → Font), with Inconsolata as the typewriter font of Palatino, Times, Bookman, Schoolbook and Euler. As the other fonts, each file is fetched the first time a document uses it, and kept by the browser for the next visits.
- The typewriter font of sessions (and of the prompt of AI sessions) is the one of the font of the document, as in the rest of the text, and the spaces of a monospaced font take one cell.

### 6 October 2026

- The tabs of the windows, the menu **Upsilon** and its dialogs follow the dark theme, as the rest of the interface (the theme of the preferences, or the appearance of the system).
- This page tells which commit made the build, the components of the program with their versions and the checksums of their sources, the sizes of the program and of the files, and the plug-ins with the programs they run, where they come from and how large they are (*This build*, above).
- The plots of the examples of R and Python and the figure of the page layout (Help → Developer documentation) are shown: they were PostScript, which the browser cannot draw, and are now PDF.
- The help of the Python and R plug-ins is there (Help → Plug-ins), with how they run in the browser: it was left out of the files of the page.
- PDF pictures are shown again (they were a question mark since the windows are drawn by the GPU).
- The menu **Upsilon** says how the page draws: with the GPU (WebGL2 and ThorVG, whose version is given with the other libraries) or with MuPDF, and why.
- The tabs of the windows are in a column at the left of the page, which leaves the whole height to the document; the column folds to small tabs with the initials of the windows, which grow into whole tabs (the name and a cross to close the window) under the mouse.
- The tabs of the windows can be dragged to another place; when they do not fit in the column, chevrons above and below them scroll them; **New window** comes after the last tab. Its right edge changes its width, and folds the column when it is dragged far enough to the left.
- The document no longer flickers or jumps while the page or the column is resized.

### 5 October 2026

- Your files are kept as soon as they change (within a second), and no longer every five seconds; a large folder of files no longer slows this down.
- Two tabs no longer overwrite each other's files: a second tab of TeXmacs Upsilon is read-only, says so, and can take TeXmacs over with **Use TeXmacs here**.
- A reference of Zotero cited from the search window is copied at once into the database (or into the BibTeX file of the bibliography), and the document remembers its item: the bibliography no longer depends on finding the citation key on `zotero.org`, which does not search the keys.
- The panel *Updating current buffer, please wait* of Document → Update → All goes away once the update is done (it stayed until a key or a click), and its rounded corners no longer show a square frame. A bibliography inserted without file takes the references of Zotero which the document cites, in a file exported from Zotero, at Document → Update → All.
- While Zotero answers come from `zotero.org`, the footer says what is asked (a search, the references of a bibliography...) and for how long, then how long it took or why it failed; the search window of a citation shows *Searching zotero.org...* under its results.
- The window which turns on the wallet takes the passphrase at `↩` as well as with **Ok**, and says so when the passphrase is wrong (before, `↩` did nothing and a wrong passphrase was not reported).
- Citations from Zotero, read from your library on `zotero.org` with an API key (made on `zotero.org/settings/keys`, given in Document → Bibliography → Zotero settings..., and kept in the wallet when it is on): the search window of a citation (Focus → Search references) lists the references of Zotero, the keys are completed from it, and the bibliography takes its references. The Zotero application itself cannot be reached from a web page. The answers of `zotero.org` come in the background, without stopping the page. Without a key, TeXmacs asks for it when it is needed, after opening the wallet if it is closed (as for the keys of the chatbots). See the page *Citations from Zotero* of the manual, in the chapter on links and bibliographies.
- The wallet works in the browser, encrypted by the browser and opened with a passphrase or a passkey (Touch ID...): the tab **Security** of the preferences. The key of the Albert service of artificial intelligence goes there when it is on.
- Sessions of chatbots: ChatGPT, Claude (new), Gemini, Mistral and Ollama, asked by the browser. Their keys are given in Insert → Session → Preferences, and kept in the wallet when it is on; **Update the list of models** asks each one which models the key gives. A session begins with the name of its model, and the answers are shown as they come, set as far as their LaTeX is complete (an environment or a formula which is not closed yet waits for its end); executable folds of chatbots work too. A question is sent with the conversation above it in the session as its context. The TikZ pictures of an answer become folds of the TikZ plug-in, its SVG pictures images, and the answer as it came is kept in a fold after it. See the help of the AI plug-in in Help → Plug-ins.
- The SVG pictures (those of TikZ among them) are in the PDF which is printed, as drawings: they were the sign of an unknown image.
- The pictures of an answer of a chatbot are made as soon as they are complete, while the rest of the answer comes.
- The chatbots are told how to write LaTeX which TeXmacs takes well; these instructions can be changed for each one (Instructions, Edit, in its preferences). The lists with options of `enumitem` (`\begin{itemize}[nosep]`) of an answer are no longer lost.
- Desktop version without Qt (Vue): the requests to the web (the chatbots) are made with the library libcurl rather than the program curl, when it is there (as on macOS and Linux): an interrupted answer really stops (the chatbot stops writing it), and the keys are no longer on a command line.
- Tools → AI engine, **Correct** and **Translate**: without a key, the key is asked for; an error of the chatbot is said on the status bar, and the selection is replaced only by an answer (it was cut first, and an error took its place).
- Desktop version of the chatbots: their answers are shown as they come, as in the browser, and a question which is interrupted stops its request; their TikZ pictures are drawn by the TikZ plug-in of the desktop (they made a whole page). A question interrupted before the chatbot began to answer no longer gives an error.
- The menu of the font in the footer (when it is interactive) is the one of the focus bar: the fonts of text and mathematics, those of text only by kind, and the selector for the others; it sets the font at the cursor (in a formula, its font). It was the old menu of the fonts.
- Answers of the chatbots: an error of OpenRouter says what the provider of the model answered (a free model which is *temporarily rate-limited upstream*, for instance), where it said *Provider returned error*; an answer in plain text (the words with an image) is in the font of the text; the TikZ pictures may compute coordinates (`$(a)!0.5!(b)$`, library `calc`) without loading it.
- Each session of a chatbot has its own model, kept in the document: two sessions of Gemini may ask two models. The menu of the focus bar changes the model of its session, and each answer says which model gave it (in the fold of the answer as it came).
- When the wallet is asked to be turned on twice at once (a key asked for, a key given), there is one dialogue, whose answer goes to both: a second one stayed open.
- LaTeX import: no multiplication is put before a text in a formula (`$a\text{ if }b$` gave $a \text{ if } b$), and `\parbox[t][3cm][c]{2cm}{...}` is read with all its options.
- Code copied out of TeXmacs (a code block, the answer of a chatbot as it came) keeps its `...`, which became an ellipsis (`\foreach \x in {0,...,5}` of TikZ then failed).
- An error of TeXmacs (a menu of a plug-in which cannot be built...) is shown in the window of the error messages and in the status bar, as on the desktop: it stopped the page, which had to be reloaded.
- LaTeX import (and answers of the chatbots): a space which begins the argument of `\text`, `\textbf`, `\emph`... is kept (`f\text{ continuous}` showed *fcontinuous*), and the lists with options (`\begin{itemize}[nosep]`, a `description` with options) keep their items.
- The text of a `\parbox` imported from LaTeX is text, also in a formula (it was read as mathematics). In an answer of a chatbot, a `\parbox` alone in a displayed formula is shown as a paragraph.
- The chatbots are together in Insert → Session → AI, each a submenu of its models: the session starts with the one chosen (and the executable folds of Insert → Fold → Executable are grouped the same).
- The model of a session of a chatbot is shown in its focus bar, in a menu which changes it for the next questions (by provider for OpenRouter), with **Other model**, **Update the list of models** and its preferences. (It was there twice after a key was given or the wallet opened.)
- The answers of OpenRouter which come after its messages of waiting (`: OPENROUTER PROCESSING`) are read; they were an unexpected answer.
- OpenRouter sessions: the models of many providers with a single key (`anthropic/claude-sonnet-4.5`, `deepseek/deepseek-chat`..., or `openrouter/auto` which chooses), its image models among them. **Update the list of models** lists them all (a list of models was cut at the first backquote of the descriptions of the models).
- The TikZ pictures of an answer whose code has `...` (as `\foreach \x in {0,1,...,5}`) are drawn: the dots became an ellipsis, which TikZ refuses. The conversation sent again keeps them too.
- The chatbots are in Insert → Session → AI before they have a key. A session without a key asks for it: it opens the preferences of the chatbot, or first the wallet if it is closed (it may hold the key). A key given while the wallet is closed opens it, to be kept there.
- Images in PNG or JPEG in the answers of the chatbots: the models which draw (`gemini-2.5-flash-image` of Gemini, `gpt-image-1` of ChatGPT...) make a painting or an artistic rendition of what is asked, shown as an image of the answer; their instructions say so.
- The answer of a chatbot can be stopped (the stop button of the session, **Interrupt execution**, or Focus → Interrupt execution): its service is told to stop writing it, and the question can be changed and asked again.
- The name of a file can be typed again in the panel of File → Save as (the characters did not come in).
- TikZ: the surfaces of `pgfplots` (`\addplot3[surf]`) are drawn; their drawing went too deep for MuPDF.
- TikZ: the diagrams of `tikz-cd` whose drawing was lost (its SVG was not well formed) are drawn. In an answer of a chatbot, a diagram alone in a displayed formula is a picture, and a picture which was cut by the limit of the length of the answer is shown as its code, with the rest of the answer; Claude may write longer answers.
- TikZ: the libraries of `pgfplots` which TikZJax lacked (`fill between`, group plots, polar axes...). The pictures of the answers of the chatbots keep the settings of their preamble, and their minipages are set as their contents; an answer which comes no longer gives errors for a command whose arguments have not come yet.
- The **Busy** sign of a session or a fold which waits for its answer is animated.
- After an update of the page, the caches of TeXmacs in your browser are cleared: the styles, files and documentation of the previous version were sometimes still used.
- The input fields of the dialogs whose width is given as a multiple of the default one are no longer as wide as the window (the buttons of the dialogs of the wallet were out of sight).
- Faster Scheme: the macros of a piece of code are expanded once, not each time the code runs (the export to LaTeX is about 15% faster).
- The windows are drawn by the graphics card (WebGL2): scrolling, zooming and every frame cost the processor two to four times less, with the same text as before. Add `?gpu=0` to the address of the page to draw as before, in a browser where something looks wrong.
- Dates: date gives the date (it gave nothing), its formats such as `MMMM d, yyyy` or `dd/MM/yy` work, and numbers keep their zeros (`2026-10-05`). The names of the months and days are in English.
- Long HTML documents are imported (about two thousand paragraphs stopped the import without a message), and the equation arrays of LaTeX (`eqnarray`, `align`...) are imported as in a desktop version which has run before (the first time, their content did not become a block).
- Questions about the document: Tools → Ask about the selection puts a session of the chatbot of Tools → AI engine after the paragraph of the selection, with the selection in its input, for the question which is typed after it; Tools → Ask about the document puts one at the cursor which sends the document with each question. The menu of the model in the focus bar of a session turns this on or off (**Send the document as context**): the document is sent as LaTeX, without the sessions of chatbots, and Claude keeps it in its cache, so that the next questions about it cost less.
- Chatbots which reason: their reasoning is shown while it comes and kept folded before the answer; **Reasoning** in the focus bar of the session (or in its preferences) asks for more or less of it.
- Each answer of a chatbot is followed by its tokens (and its cost with OpenRouter); the menu of the model gives the sum for the session. **Insert answer** in the focus bar puts an answer after the session, as paragraphs of the document.
- A question which an engine refuses for a while (too many requests, an engine overloaded) is asked again after a few seconds, three times at most.
- The long lists of models (OpenRouter) are in alphabetical ranges: a few entries at the top, then the providers, then their models. Insert → Session → AI lists the chatbots alone: a session starts with the model of the preferences, and another one is chosen in its focus bar.
- Executable folds of chatbots (Insert → Fold → Executable → AI): each asks its question alone (with the document, if it is chosen in its focus bar), with its own model, and keeps its answer, which is the answer alone; unfolding it again does not ask again (`Return` in its question, or **Ask again**, does). Its focus bar says **Question changed** when its question was changed since its answer.
- The cost of an answer of ChatGPT, Claude, Gemini or Mistral is estimated from the prices of its model (**about \$...**); the menu of the model also gives the tokens and the cost of all the answers of the document (those of the folds among them).
- Python sessions and executable folds (Insert → Session → Python): Python 3.14 runs in the page (Pyodide, loaded from the network by the first input), with the packages which an input imports (`numpy`, `sympy`, `matplotlib`, `pandas`, `scipy`...). The results of SymPy are formulas, the figures of matplotlib pictures. Stop ends Python while it computes; it starts again with the next input. See Help → Plug-ins → Python.
- Spell checking (Edit → Spell, and the words underlined while typing with **Continuous spell checking** in the preferences): Hunspell is in the page, and the dictionary of a language is fetched the first time it is needed (from the dictionaries of [github.com/wooorm/dictionaries](https://github.com/wooorm/dictionaries), for about thirty languages), then kept in the browser with the words which you insert. The misspelled words are highlighted, not hidden by a box.
- R sessions and executable folds (Insert → Session → R): R runs in the page (webR, loaded from the network by the first input), its plots are shown after the inputs which make them, and `install.packages` installs the packages built for webR. Stop ends R while it computes; it starts again with the next input. See Help → Plug-ins → R.
- A third faster start on the next visits (about 1 s instead of 1.5 s once the page is loaded): the files of TeXmacs had the time of each visit, so that it merged its font database again at every start and lost the caches of its fonts and of its directories.

### 4 October 2026

- Fixes in the Scheme interpreter: a crash which could happen the second time some functions ran (for instance when editing graphics), an error in a document no longer escapes from the typesetting, long runs of one character in a document are saved and cached correctly, and plug-ins keep all their settings when TeXmacs starts from its plug-in cache.
- In a JavaScript session, `TeXmacs.show` shows TeXmacs content at once, while asynchronous code runs; executable folds show their output as it comes, as sessions do, with every plug-in.
- More examples in the help of the Asymptote, TikZ and JavaScript plug-ins, and a link to it at the start of their sessions.
- A simpler panel while the page loads: a bar and one line which says what happens. It fades out once TeXmacs is ready.
- A click puts the help balloon of a button away, and no balloon comes while a menu is open, where it would hide the menu.
- The arrows of the submenus, and of the other menus and lists, are solid triangles.
- The keys which compose: the dead keys of many keyboards (the `^` or the diaeresis of a Swiss or a French keyboard), the accents of a Mac, and the input methods of Chinese, Japanese or Korean; the text being composed is shown in the document, and the panel of the system (the accents of a held key, the candidates of an input method) appears at the cursor.
- A copy keeps its structure when it is pasted into another tab of TeXmacs, or into the page reloaded (it was pasted as text).
- The shortcuts with Option, Control or Command work in browsers which hide these keys from the page, as LibreWolf (Option+arrows moved by characters instead of words).
- The Asymptote plug-in in the browser: sessions and executable folds (Insert → Session → Asymptote) make Asymptote pictures with Asymptote itself, compiled to WebAssembly; their labels are set by TeXmacs from their LaTeX and can be edited, and in a fold the code follows. See Help → Plug-ins → Asymptote.
- JavaScript sessions and executable folds (Insert → Session → JavaScript): the JavaScript of the page itself, with TeXmacs at hand (`TeXmacs.scheme`), and a file `my-init-javascript.js` run at each start (Developer → Open my-init-javascript.js). See Help → Plug-ins → JavaScript.
- Copy in Safari: `⌘C` and `⌘X` copy for the other programs too (the system kept its old clipboard), with the HTML of the copy; a copy from a menu offers the dialog **Copy for other programs**.
- Executable folds evaluate again in every case (depending on the order TeXmacs started in, `↩` could turn a fold into an empty output).
- A crash of the page on some keys (in the editing of a formula) fixed in the interpreter of Scheme.
- The help pages keep the backslashes of their examples of code.

### 3 October 2026

- The TikZ plug-in in the browser: sessions and executable folds (Insert → Session → TikZ) make TikZ pictures with TikZJax, a TeX which runs in the page; the text of a picture is set by TeXmacs and can be edited, and in a fold the source follows. See Help → Plug-ins → TikZ.
- Plug-ins in the browser, which run beside the page (as Web Workers); the list of the plug-ins is made again after an update of the page, and a plug-in which fails no longer hides the others.

### 2 October 2026

- Tighter tool bars.
- The full manuals (Help → Full manuals) no longer crash the page.

### 1 October 2026

- Edit → Paste from browser...: a dialog which gets what was copied in another page or program, in the formats of Edit → Paste from.
- Fast typing no longer lags behind the keys.
- The Remote menu connects with secure WebSockets (`wss`) when the page is served over `https`.
- The loading panel shows TeXmacs, its version, and what it loads.
- The keys of the shortcuts are shown with the symbols of a Mac, in the font of the menus.

### 30 September 2026

- This page, Help → TeXmacs in the browser.
- The fonts are loaded one by one, the first time a document uses them, instead of all of them in the background (28 MB less).
- An interactive status bar: the character before the cursor, a swatch of the colour, and a right click on a tag opens its Focus menu.
- The panel of the files: a PDF or an image opens in a tab of the browser; its buttons say that the files stay in this browser.
- On a Mac, Control+click is a right click.

### 29 September 2026

- Downloads and requests to the web are made by the browser: documents can be opened from the web.
- The links to other sites open in the browser.
- The logo of TeXmacs Upsilon, which is also the icon of the page.
- A document at an address with a port opens instead of crashing.

### 28 September 2026

- The address of the page opens a document (`?open=...`) and passes options to TeXmacs.
- Presentation mode, in full screen.
- The shortcuts with Shift and the command key on a Mac.
- The page uses no processor while nothing happens.
- The bars and the side tools of the View menu apply to all the tabs.

### 27 September 2026

- TeXmacs Upsilon is published on GitHub Pages, built by the continuous integration of the repository.
- The clipboard of the system, with the shortcuts of the platform of the browser.
- Printing: the PDF of the document in a tab of the browser.
- Scrolling with a trackpad follows the fingers.
- The Remote menu, through a TeXmacs server which accepts WebSocket clients.
- The storage of the page: its size, its removal, and the files of TeXmacs itself.
- The **Upsilon** menu: the progress of the loading, the software of the page, more information and the limits.
- The fonts of the PDF files are embedded correctly (a fix to MuPDF).
- A plug-in which cannot start its program no longer freezes the page.

### 26 September 2026

- The first version of TeXmacs in the browser: TeXmacs compiled to WebAssembly with the Vue interface, a tab per document, the frame of the page, the files of the page (with the files and folders dropped on it), and the files of TeXmacs loaded in the background.

(c) 2026 Massimiliano Gubinelli

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".
