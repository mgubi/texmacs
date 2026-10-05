<TeXmacs|2.1.5>

<style|<tuple|tmdoc|old-dots|old-lengths>>

<\body>
  <tmdoc-title|<TeXmacs> in the browser>

  <TeXmacs> <name|Vue> is an experimental port of <TeXmacs> which runs in a page of
  a web browser. It is the same <TeXmacs>, compiled to WebAssembly, with a
  new interface, <name|Vue>, which draws the menus, the tool bars and the dialogs
  itself. Nothing is installed on your computer, and nothing you write is
  sent anywhere: the documents stay in the browser, on your computer, until
  you save a copy of them.

  This page describes what is different from the desktop program, and what
  does not work yet.

  <section|Windows, tabs and dialogs>

  A web page has a single window. The windows of <TeXmacs> are therefore the
  <em|tabs> above the page: one per document, with its name, a dot when it
  has unsaved changes, a cross to close it, and a <verbatim|+> for a new
  document. The dialogs of <TeXmacs> float over the page, and can be moved
  by their title bar and resized by their edges.

  The <with|font-series|bold|TeXmacs <name|Vue>> button at the top left of the
  page opens a menu of the page itself: the version of <TeXmacs>, the state
  of its files and the storage they use, <menu|Files in this browser...>,
  <menu|Reload>, <menu|Reset...> (which deletes your files and preferences),
  and <menu|Remove from this browser...>. Its panel also has more
  information and the options of the address of the page (see below).

  <section|Your files>

  Your documents are kept in the storage of the browser, for this site
  only. <menu|File|Files in this browser...> shows them:

  <\itemize>
    <item><with|font-series|bold|Add files...>, <with|font-series|bold|Add a
    folder...> and <with|font-series|bold|Add a zip...> bring files from
    your computer, with whole projects (a folder or a zip archive, with the
    images of its documents). Files and folders can also be dropped on the
    page: the documents open, and the images dropped on a document are
    inserted where they fall.

    <item><with|font-series|bold|save copy> saves a copy of a file on your
    computer (a folder as a zip archive).

    <item><with|font-series|bold|open> opens a document in <TeXmacs>. A PDF
    or an image, which <TeXmacs> does not open as a document, is shown in a
    new tab of the browser.

    <item>The dialogs <menu|File|Load> and <menu|File|Save as> are this same
    panel.

    <item><with|font-series|bold|Files of <TeXmacs>> shows the files of
    <TeXmacs> itself (its styles, packages and documentation). They cannot be
    changed, but <with|font-series|bold|customize> copies one into your own
    <verbatim|.TeXmacs> folder, where <TeXmacs> looks first.
  </itemize>

  <\warning*>
    Clearing the data of the site deletes your files, and Safari deletes the
    data of a site which was not visited for seven days. Save a copy of the
    documents you want to keep.
  </warning*>

  Your files are kept by one tab of the browser at a time. When <TeXmacs>
  <name|Vue> is already open in another tab, a new tab shows your files but
  does not keep its changes, and says so; <with|font-series|bold|Use
  TeXmacs here> moves <TeXmacs> to it (the other tab keeps its last changes
  first). When the tab which has <TeXmacs> is closed, the others offer to
  reload.

  <section|Passwords and keys: the wallet>

  The wallet of <TeXmacs> keeps passwords and keys (the keys of the
  services of artificial intelligence, the passwords of <TeXmacs> servers)
  encrypted. In the browser it is encrypted by the browser itself, and
  opened with a passphrase or with a passkey: <menu|Edit|Preferences>, tab
  <with|font-series|bold|Security>.

  <\itemize>
    <item><with|font-series|bold|Initialize> makes the wallet, with its
    passphrase. Nothing of it is kept in the clear: only what is encrypted
    goes to the storage of the browser.

    <item>While the wallet is on, <with|font-series|bold|Add a passkey> lets
    the passkey of your computer or phone (Touch ID, Windows Hello, a
    security key...) open it too, where the browser allows it.

    <item><with|font-series|bold|Turn on> opens the wallet, with the
    passphrase or the passkey, once per session; <with|font-series|bold|Turn
    off> forgets what it holds until it is opened again.
  </itemize>

  While the wallet is on, the code which runs in the page (a
  <name|JavaScript> session for instance) could read what it holds: only
  run code which you trust.

  <section|Keyboard, clipboard and printing>

  <TeXmacs> keeps its usual keyboard shortcuts, but the browser keeps some
  for itself (a new window, a new tab, closing a tab, reloading the page):
  use the menus of <TeXmacs>, or the <verbatim|+> of the tabs, for those. On
  a Mac, they use the command key, as those of the browser: <key|M-c>,
  <key|M-x> and <key|M-v> copy, cut and paste. The keys which compose (dead
  keys, the accents of a Mac, the input methods of Chinese or Japanese) work
  as in the other programs.

  Copy, cut and paste go through the clipboard of the system, so that text
  can be exchanged with the other programs; a copy made in <TeXmacs> keeps
  its structure when it is pasted back into <TeXmacs>. The browser lets a
  page read its clipboard only when a key pastes, so that
  <menu|Edit|Paste> pastes the last copy or paste of <TeXmacs>; use
  <menu|Edit|Paste from browser...> for what was copied in another page or
  program since: it opens a small dialog, in which <key|M-v> (or its button)
  pastes.

  In the same way, some browsers (Safari) let a page write its clipboard
  only during a key or a click: <key|M-c> and <key|M-x> always copy for the
  other programs, but a copy from a menu (<menu|Edit|Copy>, the formats of
  <menu|Edit|Copy to>) opens a small dialog, <with|font-series|bold|Copy for
  other programs>, whose button gives it to them.

  <menu|File|Print> makes a PDF of the document and opens it in a new tab
  of the browser, from which it can be printed or saved.

  <section|Loading and fonts>

  The first visit loads some 9<nbsp>MB before <TeXmacs> starts, and 8<nbsp>MB
  more in the background while you work. The fonts which come with
  <TeXmacs> are loaded one by one, the first time a document uses them. All
  of it is kept by the browser: the next visits load nothing.

  Only the fonts which come with <TeXmacs> are available: a web page cannot
  see the fonts installed on your computer.

  <section|Opening a document from the web>

  A link to the page can open a document published on the web, which makes
  the page a viewer of <TeXmacs> documents:

  <\verbatim-code>
    https://mgubi.github.io/texmacs/?open=https://example.org/paper.tm
  </verbatim-code>

  Any format <TeXmacs> opens will do (<verbatim|.tm>, <verbatim|.tex>,
  <verbatim|.html>, <verbatim|.md>...). The site of the document has to
  allow other pages to read it, and the images the document refers to are
  not fetched with it. Other options of the address run Scheme commands (the
  page asks before running them) or show debugging messages: see the
  options of the address in the <with|font-series|bold|TeXmacs <name|Vue>> menu.

  <section|What does not work>

  <\itemize>
    <item>A web page cannot run other programs. There are therefore no
    sessions of other programs (Maxima...), and the tools which need an
    external program are missing: the compilation with <LaTeX>,
    Ghostscript, ImageMagick, Git. The sessions which
    exist in the browser are those whose program runs in the page itself:
    <name|Python> (<name|Pyodide>) and <name|R> (<name|webR>), loaded
    from the network,
    <name|JavaScript>, <name|TikZ> and <name|Asymptote> (see the help of
    their plug-ins, in <menu|Help|Plug-ins>), and those of the chatbots
    which the page asks through the web: <name|ChatGPT>, <name|Claude>,
    <name|Gemini>, <name|Mistral> with a key, and <name|Ollama> on your
    computer if it allows the page. <name|Albert> does not answer web pages.

    <item>The <menu|Remote> menu connects to a <TeXmacs> server over
    WebSocket, on the same computer only for now.

    <item><TeXmacs> <name|Vue> types, scrolls and typesets as fast as the
    desktop program, but its <name|Scheme> runs two to three times slower,
    which shows in the commands which are mostly <name|Scheme> (the
    conversions, some menus) and at the start.
  </itemize>

  <section|Reporting problems>

  <TeXmacs> <name|Vue> is experimental. A problem which does not happen with the
  <TeXmacs> you install on your computer is a problem of this port: please
  report it on the <hlink|issue page of the
  port|https://github.com/mgubi/texmacs/issues>, not to the <TeXmacs>
  project. The sources and the notes of the port are on
  <hlink|GitHub|https://github.com/mgubi/texmacs/tree/wip_wasm_vue>.

  <section|Recent changes>

  The additions to <TeXmacs> <name|Vue>, the newest first.

  <paragraph|5 October 2026>

  <\itemize>
    <item>Your files are kept as soon as they change (within a second),
    and no longer every five seconds; a large folder of files no longer
    slows this down.

    <item>Two tabs no longer overwrite each other's files: a second tab of
    <TeXmacs> <name|Vue> is read-only, says so, and can take <TeXmacs> over
    with <with|font-series|bold|Use TeXmacs here>.

    <item>A reference of <name|Zotero> cited from the search window is
    copied at once into the database (or into the <BibTeX> file of the
    bibliography), and the document remembers its item: the bibliography
    no longer depends on finding the citation key on <verbatim|zotero.org>,
    which does not search the keys.

    <item>The panel <with|font-shape|italic|Updating current buffer, please
    wait> of <menu|Document|Update|All> goes away once the update is done
    (it stayed until a key or a click), and its rounded corners no longer
    show a square frame. A bibliography inserted without
    file takes the references of <name|Zotero> which the document cites,
    in a file exported from <name|Zotero>, at <menu|Document|Update|All>.

    <item>While <name|Zotero> answers come from <verbatim|zotero.org>,
    the footer says what is asked (a search, the references of a
    bibliography...) and for how long, then how long it took or why it
    failed; the search window of a citation shows
    <with|font-shape|italic|Searching zotero.org...> under its results.

    <item>The window which turns on the wallet takes the passphrase at
    <shortcut|(kbd-return)> as well as with <with|font-series|bold|Ok>, and
    says so when the passphrase is wrong (before, <shortcut|(kbd-return)>
    did nothing and a wrong passphrase was not reported).

    <item>Citations from <name|Zotero>, read from your library on
    <verbatim|zotero.org> with an API key (made on
    <verbatim|zotero.org/settings/keys>, given in
    <menu|Document|Bibliography|Zotero settings...>, and kept in the wallet
    when it is on): the search window of a citation (<menu|Focus|Search
    references>) lists the references of <name|Zotero>, the keys are
    completed from it, and the bibliography takes its references. The
    Zotero application itself cannot be reached from a web page. The
    answers of <verbatim|zotero.org> come in the background, without
    stopping the page. Without a key, <TeXmacs> asks for it when it is needed,
    after opening the wallet if it is closed (as for the keys of the
    chatbots). See the
    page <with|font-shape|italic|Citations from Zotero> of the manual, in
    the chapter on links and bibliographies.

    <item>The wallet works in the browser, encrypted by the browser and
    opened with a passphrase or a passkey (Touch ID...): the tab
    <with|font-series|bold|Security> of the preferences. The key of the
    <name|Albert> service of artificial intelligence goes there when it is
    on.

    <item>Sessions of chatbots: <name|ChatGPT>, <name|Claude> (new),
    <name|Gemini>, <name|Mistral> and <name|Ollama>, asked by the browser.
    Their keys are given in <menu|Insert|Session|Preferences>, and kept in
    the wallet when it is on; <with|font-series|bold|Update the list of
    models> asks each one which models the key gives. A session begins with
    the name of its model, and the answers are shown as they come, set as
    far as their <LaTeX> is complete (an environment or a formula which is
    not closed yet waits for its end); executable folds of chatbots work
    too. A question is sent with the conversation above it in the session
    as its context. The <name|TikZ> pictures of an answer become folds of
    the <name|TikZ> plug-in, its <name|SVG> pictures images, and the answer
    as it came is kept in a fold after it. See the help of the AI plug-in in <menu|Help|Plug-ins>.

    <item>The <name|SVG> pictures (those of <name|TikZ> among them) are in
    the PDF which is printed, as drawings: they were the sign of an unknown
    image.

    <item>The pictures of an answer of a chatbot are made as soon as they
    are complete, while the rest of the answer comes.

    <item>The chatbots are told how to write <LaTeX> which <TeXmacs> takes
    well; these instructions can be changed for each one (Instructions,
    Edit, in its preferences). The lists with options of
    <verbatim|enumitem> (<verbatim|\\begin{itemize}[nosep]>) of an answer
    are no longer lost.

    <item>Desktop version without <name|Qt> (Vue): the requests to the web
    (the chatbots) are made with the library libcurl rather than the program
    curl, when it is there (as on macOS and Linux): an interrupted answer
    really stops (the chatbot stops writing it), and the keys are no longer
    on a command line.

    <item><menu|Tools|AI engine>, <with|font-series|bold|Correct> and
    <with|font-series|bold|Translate>: without a key, the key is asked for;
    an error of the chatbot is said on the status bar, and the selection is
    replaced only by an answer (it was cut first, and an error took its
    place).

    <item>Desktop version of the chatbots: their answers are shown as they
    come, as in the browser, and a question which is interrupted stops its
    request; their <name|TikZ> pictures are drawn by the <name|TikZ>
    plug-in of the desktop (they made a whole page). A question interrupted
    before the chatbot began to answer no longer gives an error.

    <item>The menu of the font in the footer (when it is interactive) is
    the one of the focus bar: the fonts of text and mathematics, those of
    text only by kind, and the selector for the others; it sets the font at
    the cursor (in a formula, its font). It was the old menu of the fonts.

    <item>Answers of the chatbots: an error of <name|OpenRouter> says what
    the provider of the model answered (a free model which is
    <with|font-shape|italic|temporarily rate-limited upstream>, for
    instance), where it said <with|font-shape|italic|Provider returned
    error>; an answer in plain text (the words with an image) is in the font
    of the text; the <name|TikZ> pictures may compute coordinates
    (<verbatim|$(a)!0.5!(b)$>, library <verbatim|calc>) without loading it.

    <item>Each session of a chatbot has its own model, kept in the
    document: two sessions of <name|Gemini> may ask two models. The menu of
    the focus bar changes the model of its session, and each answer says
    which model gave it (in the fold of the answer as it came).

    <item>When the wallet is asked to be turned on twice at once (a key
    asked for, a key given), there is one dialogue, whose answer goes to
    both: a second one stayed open.

    <item><LaTeX> import: no multiplication is put before a text in a
    formula (<verbatim|$a\\text{ if }b$> gave <math|a*<text| if >b>), and
    <verbatim|\\parbox[t][3cm][c]{2cm}{...}> is read with all its options.

    <item>Code copied out of <TeXmacs> (a code block, the answer of a
    chatbot as it came) keeps its <verbatim|...>, which became an ellipsis
    (<verbatim|\\foreach \\x in {0,...,5}> of <name|TikZ> then failed).

    <item>An error of <TeXmacs> (a menu of a plug-in which cannot be
    built...) is shown in the window of the error messages and in the status
    bar, as on the desktop: it stopped the page, which had to be reloaded.

    <item><LaTeX> import (and answers of the chatbots): a space which
    begins the argument of <verbatim|\\text>, <verbatim|\\textbf>,
    <verbatim|\\emph>... is kept (<verbatim|f\\text{ continuous}> showed
    <with|font-shape|italic|fcontinuous>), and the lists with options
    (<verbatim|\\begin{itemize}[nosep]>, a <verbatim|description> with
    options) keep their items.

    <item>The text of a <verbatim|\\parbox> imported from <LaTeX> is
    text, also in a formula (it was read as mathematics). In an answer of a
    chatbot, a <verbatim|\\parbox> alone in a displayed formula is shown as
    a paragraph.

    <item>The chatbots are together in <menu|Insert|Session|AI>, each a
    submenu of its models: the session starts with the one chosen (and the
    executable folds of <menu|Insert|Fold|Executable> are grouped the same).

    <item>The model of a session of a chatbot is shown in its focus bar, in
    a menu which changes it for the next questions (by provider for
    <name|OpenRouter>), with <with|font-series|bold|Other model>,
    <with|font-series|bold|Update the list of models> and its preferences.
    (It was there twice after a key was given or the wallet opened.)

    <item>The answers of <name|OpenRouter> which come after its messages of
    waiting (<verbatim|: OPENROUTER PROCESSING>) are read; they were an
    unexpected answer.

    <item><name|OpenRouter> sessions: the models of many providers with a
    single key (<verbatim|anthropic/claude-sonnet-4.5>,
    <verbatim|deepseek/deepseek-chat>..., or <verbatim|openrouter/auto>
    which chooses), its image models among them.
    <with|font-series|bold|Update the list of models> lists them all (a
    list of models was cut at the first backquote of the descriptions of
    the models).

    <item>The <name|TikZ> pictures of an answer whose code has
    <verbatim|...> (as <verbatim|\\foreach \\x in {0,1,...,5}>) are drawn:
    the dots became an ellipsis, which <name|TikZ> refuses. The conversation
    sent again keeps them too.

    <item>The chatbots are in <menu|Insert|Session|AI> before they have a
    key. A session without a key asks for it: it opens the preferences of the
    chatbot, or first the wallet if it is closed (it may hold the key). A key
    given while the wallet is closed opens it, to be kept there.

    <item>Images in <name|PNG> or <name|JPEG> in the answers of the
    chatbots: the models which draw (<verbatim|gemini-2.5-flash-image> of
    <name|Gemini>, <verbatim|gpt-image-1> of <name|ChatGPT>...) make a
    painting or an artistic rendition of what is asked, shown as an image
    of the answer; their instructions say so.

    <item>The answer of a chatbot can be stopped (the stop
    button of the session, <with|font-series|bold|Interrupt execution>, or
    <menu|Focus|Interrupt execution>): its service is told to stop writing
    it, and the question can be changed and asked again.

    <item>The name of a file can be typed again in the panel of
    <menu|File|Save as> (the characters did not come in).

    <item><name|TikZ>: the surfaces of <verbatim|pgfplots>
    (<verbatim|\\addplot3[surf]>) are drawn; their drawing went too deep
    for <name|MuPDF>.

    <item><name|TikZ>: the diagrams of <verbatim|tikz-cd> whose drawing
    was lost (its <name|SVG> was not well formed) are drawn. In an answer of
    a chatbot, a diagram alone in a displayed formula is a picture, and a
    picture which was cut by the limit of the length of the answer is shown
    as its code, with the rest of the answer; <name|Claude> may write longer
    answers.

    <item><name|TikZ>: the libraries of <verbatim|pgfplots> which
    <name|TikZJax> lacked (<verbatim|fill between>, group plots, polar
    axes...). The pictures of the answers of the chatbots keep the settings
    of their preamble, and their minipages are set as their contents; an
    answer which comes no longer gives errors for a command whose arguments
    have not come yet.

    <item>The <with|font-series|bold|Busy> sign of a session or a fold
    which waits for its answer is animated.

    <item>After an update of the page, the caches of <TeXmacs> in your
    browser are cleared: the styles, files and documentation of the previous
    version were sometimes still used.

    <item>The input fields of the dialogs whose width is given as a
    multiple of the default one are no longer as wide as the window (the
    buttons of the dialogs of the wallet were out of sight).

    <item>Faster Scheme: the macros of a piece of code are expanded once,
    not each time the code runs (the export to <LaTeX> is about 15% faster).

    <item>The windows are drawn by the graphics card (WebGL2): scrolling,
    zooming and every frame cost the processor two to four times less, with
    the same text as before. Add <verbatim|?gpu=0> to the address of the page
    to draw as before, in a browser where something looks wrong.

    <item>Dates: <markup|date> gives the date (it gave nothing), its formats
    such as <verbatim|MMMM d, yyyy> or <verbatim|dd/MM/yy> work, and numbers
    keep their zeros (<verbatim|2026-10-05>). The names of the months and
    days are in English.

    <item>Long HTML documents are imported (about two thousand paragraphs
    stopped the import without a message), and the equation arrays of LaTeX
    (<verbatim|eqnarray>, <verbatim|align>...) are imported as in a desktop
    version which has run before (the first time, their content did not
    become a block).

    <item>Questions about the document: <menu|Tools|Ask about the selection>
    puts a session of the chatbot of <menu|Tools|AI engine> after the
    paragraph of the selection, with the selection in its input, for the
    question which is typed after it; <menu|Tools|Ask about the document>
    puts one at the cursor which sends the document with each question. The
    menu of the model in the focus bar of a session turns this on or off
    (<with|font-series|bold|Send the document as context>): the document is
    sent as <LaTeX>, without the sessions of chatbots, and <name|Claude>
    keeps it in its cache, so that the next questions about it cost less.

    <item>Chatbots which reason: their reasoning is shown while it comes and
    kept folded before the answer; <with|font-series|bold|Reasoning> in the
    focus bar of the session (or in its preferences) asks for more or less
    of it.

    <item>Each answer of a chatbot is followed by its tokens (and its cost
    with <name|OpenRouter>); the menu of the model gives the sum for the
    session. <with|font-series|bold|Insert answer> in the focus bar puts an
    answer after the session, as paragraphs of the document.

    <item>A question which an engine refuses for a while (too many requests,
    an engine overloaded) is asked again after a few seconds, three times
    at most.

    <item>The long lists of models (<name|OpenRouter>) are in alphabetical
    ranges: a few entries at the top, then the providers, then their models.
    <menu|Insert|Session|AI> lists the chatbots alone: a session starts with
    the model of the preferences, and another one is chosen in its focus
    bar.

    <item>Executable folds of chatbots (<menu|Insert|Fold|Executable|AI>):
    each asks its question alone (with the document, if it is chosen in its
    focus bar), with its own model, and keeps its answer, which is the answer
    alone; unfolding it again does not ask again (<key|Return> in its
    question, or <with|font-series|bold|Ask again>, does). Its focus bar
    says <with|font-series|bold|Question changed> when its question was
    changed since its answer.

    <item>The cost of an answer of <name|ChatGPT>, <name|Claude>,
    <name|Gemini> or <name|Mistral> is estimated from the prices of its
    model (<with|font-series|bold|about $...>); the menu of the model also
    gives the tokens and the cost of all the answers of the document (those
    of the folds among them).

    <item><name|Python> sessions and executable folds
    (<menu|Insert|Session|Python>): <name|Python> 3.14 runs in the page
    (<name|Pyodide>, loaded from the network by the first input), with the
    packages which an input imports (<verbatim|numpy>, <verbatim|sympy>,
    <verbatim|matplotlib>, <verbatim|pandas>, <verbatim|scipy>...). The
    results of <name|SymPy> are formulas, the figures of <name|matplotlib>
    pictures. <menu|Stop> ends <name|Python> while it computes; it starts
    again with the next input. See <menu|Help|Plug-ins|Python>.

    <item>Spell checking (<menu|Edit|Spell>, and the words underlined while
    typing with <with|font-series|bold|Continuous spell checking> in the
    preferences): <name|Hunspell> is in the page, and the dictionary of a
    language is fetched the first time it is needed (from the dictionaries
    of <hlink|github.com/wooorm/dictionaries|https://github.com/wooorm/dictionaries>,
    for about thirty languages), then kept in the browser with the words
    which you insert. The misspelled words are highlighted, not hidden by a
    box.

    <item><name|R> sessions and executable folds (<menu|Insert|Session|R>):
    <name|R> runs in the page (<name|webR>, loaded from the network by the
    first input), its plots are shown after the inputs which make them, and
    <verbatim|install.packages> installs the packages built for
    <name|webR>. <menu|Stop> ends <name|R> while it computes; it starts
    again with the next input. See <menu|Help|Plug-ins|R>.

    <item>A third faster start on the next visits (about 1<nbsp>s instead of
    1.5<nbsp>s once the page is loaded): the files of <TeXmacs> had the time
    of each visit, so that it merged its font database again at every start
    and lost the caches of its fonts and of its directories.
  </itemize>

  <paragraph|4 October 2026>

  <\itemize>
    <item>Fixes in the Scheme interpreter: a crash which could happen the
    second time some functions ran (for instance when editing graphics), an
    error in a document no longer escapes from the typesetting, long runs of
    one character in a document are saved and cached correctly, and plug-ins
    keep all their settings when <TeXmacs> starts from its plug-in cache.

    <item>In a JavaScript session, <verbatim|TeXmacs.show> shows <TeXmacs>
    content at once, while asynchronous code runs; executable folds show
    their output as it comes, as sessions do, with every plug-in.

    <item>More examples in the help of the Asymptote, TikZ and JavaScript
    plug-ins, and a link to it at the start of their sessions.

    <item>A simpler panel while the page loads: a bar and one line which
    says what happens. It fades out once <TeXmacs> is ready.

    <item>A click puts the help balloon of a button away, and no balloon
    comes while a menu is open, where it would hide the menu.

    <item>The arrows of the submenus, and of the other menus and lists, are
    solid triangles.

    <item>The keys which compose: the dead keys of many keyboards (the
    <verbatim|^> or the diaeresis of a Swiss or a French keyboard), the accents of a Mac, and the input methods of Chinese,
    Japanese or Korean; the text being composed is shown in the document,
    and the panel of the system (the accents of a held key, the candidates of
    an input method) appears at the cursor.

    <item>A copy keeps its structure when it is pasted into another tab of
    <TeXmacs>, or into the page reloaded (it was pasted as text).

    <item>The shortcuts with Option, Control or Command work in browsers
    which hide these keys from the page, as LibreWolf (Option+arrows moved
    by characters instead of words).

    <item>The <name|Asymptote> plug-in in the browser: sessions and
    executable folds (<menu|Insert|Session|Asymptote>) make <name|Asymptote>
    pictures with <name|Asymptote> itself, compiled to <name|WebAssembly>;
    their labels are set by <TeXmacs> from their <LaTeX> and can be edited,
    and in a fold the code follows. See <menu|Help|Plug-ins|Asymptote>.

    <item><name|JavaScript> sessions and executable folds
    (<menu|Insert|Session|JavaScript>): the <name|JavaScript> of the page
    itself, with <TeXmacs> at hand (<verbatim|TeXmacs.scheme>), and a file
    <verbatim|my-init-javascript.js> run at each start
    (<menu|Developer|Open my-init-javascript.js>). See
    <menu|Help|Plug-ins|JavaScript>.

    <item>Copy in Safari: <key|M-c> and <key|M-x> copy for the other
    programs too (the system kept its old clipboard), with the HTML of the
    copy; a copy from a menu offers the dialog <with|font-series|bold|Copy
    for other programs>.

    <item>Executable folds evaluate again in every case (depending on the
    order <TeXmacs> started in, <key|return> could turn a fold into an
    empty output).

    <item>A crash of the page on some keys (in the editing of a formula)
    fixed in the interpreter of <name|Scheme>.

    <item>The help pages keep the backslashes of their examples of code.
  </itemize>

  <paragraph|3 October 2026>

  <\itemize>
    <item>The <name|TikZ> plug-in in the browser: sessions and executable
    folds (<menu|Insert|Session|TikZ>) make <name|TikZ> pictures with
    <name|TikZJax>, a <name|TeX> which runs in the page; the text of a
    picture is set by <TeXmacs> and can be edited, and in a fold the source
    follows. See <menu|Help|Plug-ins|TikZ>.

    <item>Plug-ins in the browser, which run beside the page (as <name|Web
    Workers>); the list of the plug-ins is made again after an update of the
    page, and a plug-in which fails no longer hides the others.
  </itemize>

  <paragraph|2 October 2026>

  <\itemize>
    <item>Tighter tool bars.

    <item>The full manuals (<menu|Help|Full manuals>) no longer crash the
    page.
  </itemize>

  <paragraph|1 October 2026>

  <\itemize>
    <item><menu|Edit|Paste from browser...>: a dialog which gets what was
    copied in another page or program, in the formats of
    <menu|Edit|Paste from>.

    <item>Fast typing no longer lags behind the keys.

    <item>The <menu|Remote> menu connects with secure WebSockets
    (<verbatim|wss>) when the page is served over <verbatim|https>.

    <item>The loading panel shows <TeXmacs>, its version, and what it loads.

    <item>The keys of the shortcuts are shown with the symbols of a Mac, in
    the font of the menus.
  </itemize>

  <paragraph|30 September 2026>

  <\itemize>
    <item>This page, <menu|Help|TeXmacs in the browser>.

    <item>The fonts are loaded one by one, the first time a document uses
    them, instead of all of them in the background (28<nbsp>MB less).

    <item>An interactive status bar: the character before the cursor, a
    swatch of the colour, and a right click on a tag opens its
    <menu|Focus> menu.

    <item>The panel of the files: a PDF or an image opens in a tab of the
    browser; its buttons say that the files stay in this browser.

    <item>On a Mac, Control+click is a right click.
  </itemize>

  <paragraph|29 September 2026>

  <\itemize>
    <item>Downloads and requests to the web are made by the browser:
    documents can be opened from the web.

    <item>The links to other sites open in the browser.

    <item>The logo of <TeXmacs> <name|Vue>, which is also the icon of the
    page.

    <item>A document at an address with a port opens instead of crashing.
  </itemize>

  <paragraph|28 September 2026>

  <\itemize>
    <item>The address of the page opens a document
    (<verbatim|?open=...>) and passes options to <TeXmacs>.

    <item>Presentation mode, in full screen.

    <item>The shortcuts with Shift and the command key on a Mac.

    <item>The page uses no processor while nothing happens.

    <item>The bars and the side tools of the <menu|View> menu apply to all
    the tabs.
  </itemize>

  <paragraph|27 September 2026>

  <\itemize>
    <item><TeXmacs> <name|Vue> is published on <name|GitHub Pages>, built
    by the continuous integration of the repository.

    <item>The clipboard of the system, with the shortcuts of the platform of
    the browser.

    <item>Printing: the PDF of the document in a tab of the browser.

    <item>Scrolling with a trackpad follows the fingers.

    <item>The <menu|Remote> menu, through a <TeXmacs> server which accepts
    WebSocket clients.

    <item>The storage of the page: its size, its removal, and the files of
    <TeXmacs> itself.

    <item>The <with|font-series|bold|TeXmacs <name|Vue>> menu: the progress
    of the loading, the software of the page, more information and the
    limits.

    <item>The fonts of the PDF files are embedded correctly (a fix to
    <name|MuPDF>).

    <item>A plug-in which cannot start its program no longer freezes the
    page.
  </itemize>

  <paragraph|26 September 2026>

  <\itemize>
    <item>The first version of <TeXmacs> in the browser: <TeXmacs> compiled
    to <name|WebAssembly> with the <name|Vue> interface, a tab per
    document, the frame of the page, the files of the page (with the files
    and folders dropped on it), and the files of <TeXmacs> loaded in the
    background.
  </itemize>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>
