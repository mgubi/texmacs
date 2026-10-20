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

## 20 October 2026

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
