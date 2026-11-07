<TeXmacs|2.1.5>

<style|<tuple|article|doc>>

<\body>
  <doc-data|<doc-title|Customizing <TeXmacs>>|<doc-running-title|Customizing
  <TeXmacs>>|<doc-author|<author-data|<author-name|Nicolas
  Ratier>>>|<doc-misc|Updated for <TeXmacs> 2.1.5 in 2026.>>

  This tutorial shows how <TeXmacs> organizes its keyboard and its menus, and
  how you can add your own menus, keyboard shortcuts and icons. All the
  customizations go into one <scheme> file of yours, which <TeXmacs> reads
  each time it starts:

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/progs/my-init-texmacs.scm
  </verbatim>

  Here <verbatim|$TEXMACS_HOME_PATH> is your personal <TeXmacs> folder:
  <verbatim|~/.TeXmacs> on <name|Linux> and <name|macOS>,
  <verbatim|%appdata%\\TeXmacs> on <name|Windows> (<TeXmacs> creates its
  folders <verbatim|progs> and <verbatim|misc/pixmaps>). With the developer
  tool (<menu|Tools|Developer tool>), the menu <menu|Developer|Open
  my-init-texmacs.scm> opens this file for editing. After a change, restart
  <TeXmacs> to see its effect.

  The complete file used for this tutorial, together with its icons, is
  <hlink|<verbatim|my-init-texmacs.scm>|my-init-texmacs.scm> (the icons are
  in <verbatim|images/icons>; copy them to
  <verbatim|$TEXMACS_HOME_PATH/misc/pixmaps>, see
  section<nbsp><reference|sec-icons>).

  <\table-of-contents|toc>
    <vspace*|1fn><with|font-series|bold|math-font-series|bold|1<space|2spc>The
    T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>
    keyboard> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
    <no-break><pageref|auto-3><vspace|0.5fn>

    <vspace*|1fn><with|font-series|bold|math-font-series|bold|2<space|2spc>How
    T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>
    defines its own menus> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
    <no-break><pageref|auto-9><vspace|0.5fn>

    <vspace*|1fn><with|font-series|bold|math-font-series|bold|3<space|2spc>How
    to create your own menus> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
    <no-break><pageref|auto-15><vspace|0.5fn>

    <vspace*|1fn><with|font-series|bold|math-font-series|bold|4<space|2spc>How
    to create your own keyboard shortcuts>
    <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
    <no-break><pageref|auto-30><vspace|0.5fn>

    <vspace*|1fn><with|font-series|bold|math-font-series|bold|5<space|2spc>How
    to create your own icon bar> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
    <no-break><pageref|auto-32><vspace|0.5fn>
  </table-of-contents>

  <section|The <TeXmacs> keyboard>

  <TeXmacs> uses four modifier keys, written as follows in the manual and in
  the <scheme> files:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Modifier>|<cell|Meaning>|<cell|PC
  keyboard>|<cell|<name|Apple> keyboard>>|<row|<cell|<verbatim|S->>|<cell|Shift>|<cell|Shift>|<cell|Shift>>|<row|<cell|<verbatim|C->>|<cell|Control>|<cell|Ctrl>|<cell|Control>>|<row|<cell|<verbatim|A->>|<cell|Alternate>|<cell|Alt>|<cell|Option>>|<row|<cell|<verbatim|M->>|<cell|Meta>|<cell|Windows
  key>|<cell|Command>>>>>>
    The modifier keys of <TeXmacs>.
  </big-table>

  The Hyper modifier <verbatim|H-> of the old versions of <TeXmacs>, which
  one had to map onto the Caps-lock key, is gone: special symbols are now
  entered with the <key|tab> key (in a formula, <key|p tab> gives
  <math|\<pi\>>, <key|a tab> gives <math|\<alpha\>>, <key|\<less\> => gives
  <math|\<leqslant\>>) or with the <prefix|symbol> prefix.

  Which modifier does what depends on the <em|look and feel> chosen in
  <menu|Edit|Preferences|General>: by default the one of your system. The
  shortcuts are therefore written in terms of three <em|prefixes>, which the
  look and feel translates into modifiers:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Prefix>|<cell|Use>|<cell|<name|macOS>>|<cell|<name|Windows>,
  <name|Linux>>>|<row|<cell|<verbatim|std>>|<cell|standard (copy,
  save)>|<cell|<verbatim|M-> (Command)>|<cell|<verbatim|C->
  (Ctrl)>>|<row|<cell|<verbatim|cmd>>|<cell|mode
  commands>|<cell|<verbatim|C-> (Control)>|<cell|<verbatim|A->
  (Alt)>>|<row|<cell|<verbatim|altcmd>>|<cell|compound
  commands>|<cell|<verbatim|M-C-> (Cmd+Ctrl)>|<cell|<verbatim|M-> (Windows
  key)>>>>>>
    The main prefixes of the keyboard shortcuts.
  </big-table>

  For instance, <verbatim|std s> saves the document (Command-S on a Mac,
  Ctrl-S elsewhere), <verbatim|math s> (that is <verbatim|cmd s> in a
  formula) inserts a square root and <verbatim|altcmd t> starts the table
  commands. On the Mac which produced this document they are shown as
  <key|std s>, <key|math s> and <key|altcmd t>. When the system takes a
  combination for itself, the <key*|escape> key gives the modifiers instead:
  on <name|macOS>, <key*|escape> acts as <verbatim|A->, <key*|escape escape>
  as <verbatim|C-> and <key*|escape escape escape> as <verbatim|M->.

  <TeXmacs> can also show a keyboard on the screen, which you can click
  (<menu|Developer|Show keyboard>, with the developer tool):

  <\big-figure|<image|images/virtual-keyboard.png|0.6par|||>>
    The keyboard of <TeXmacs> on the screen.
  </big-figure>

  <section|How <TeXmacs> defines its own menus>

  The menus of <TeXmacs> are written in <scheme>, in the folder
  <verbatim|$TEXMACS_PATH/progs> of the installation. The menu bar is defined
  in <verbatim|progs/texmacs/menus/main-menu.scm> (abridged here):

  <\scm-code>
    (menu-bind texmacs-menu

    \ \ (=\<gtr\> "File" (link file-menu))

    \ \ (=\<gtr\> "Edit" (link edit-menu))

    \ \ (if (in-graphics?)

    \ \ \ \ \ \ (=\<gtr\> "Insert" (link graphics-insert-menu))

    \ \ \ \ \ \ (link texmacs-extra-menu)

    \ \ \ \ \ \ (=\<gtr\> "Focus" (link graphics-focus-menu)))

    \ \ (if (not (in-graphics?))

    \ \ \ \ \ \ (=\<gtr\> "Insert" (link insert-menu))

    \ \ \ \ \ \ (if (or (in-source?) (with-source-tool?))

    \ \ \ \ \ \ \ \ \ \ (=\<gtr\> "Source" (link source-menu)))

    \ \ \ \ \ \ ...

    \ \ \ \ \ \ (link plugin-menu)

    \ \ \ \ \ \ (link texmacs-extra-menu)

    \ \ \ \ \ \ (=\<gtr\> "Focus" (link focus-menu))

    \ \ \ \ \ \ (=\<gtr\> "Format" (link format-menu)))

    \ \ (=\<gtr\> "Document" (link document-menu))

    \ \ (if (project-attached?) (=\<gtr\> "Project" (link project-menu)))

    \ \ (=\<gtr\> "View" (link view-menu))

    \ \ (=\<gtr\> "Go" (link go-menu))

    \ \ (if (detailed-menus?) (=\<gtr\> "Tools" (link tools-menu)))

    \ \ ...

    \ \ (if (with-developer-tool?)

    \ \ \ \ \ \ (=\<gtr\> "Developer" (link developer-menu)))

    \ \ (=\<gtr\> "Help" (link help-menu)))
  </scm-code>

  <\itemize>
    <item>The constructor <scm|menu-bind> defines (or redefines) a menu.

    <item>The constructor <scm|link> inserts a menu defined elsewhere.

    <item><scm|(=\<gtr\> "File" ...)> makes a pulldown menu in the menu bar.

    <item><scm|if> shows entries only when a condition holds: the
    <menu|Source> menu, for instance, only appears in source mode or with the
    source macros tool.
  </itemize>

  The <menu|File> menu comes from <verbatim|progs/texmacs/menus/file-menu.scm>:

  <\scm-code>
    (menu-bind file-menu

    \ \ ("New" (new-document))

    \ \ ("Load" (open-document))

    \ \ ("Revert" (revert-buffer))

    \ \ (-\<gtr\> "Recent"

    \ \ \ \ \ \ (link recent-file-menu)

    \ \ \ \ \ \ ...)

    \ \ ---

    \ \ ("Save" (save-buffer))

    \ \ ("Save as" (choose-file save-buffer-as "Save TeXmacs file"
    "texmacs"))

    \ \ ---

    \ \ (link print-menu)

    \ \ ---

    \ \ (-\<gtr\> "Import"

    \ \ \ \ \ \ (link import-import-menu)

    \ \ \ \ \ \ ...)

    \ \ (-\<gtr\> "Export"

    \ \ \ \ \ \ (link export-export-menu)

    \ \ \ \ \ \ ---

    \ \ \ \ \ \ ("Pdf" (choose-file wrapped-print-to-file "Save pdf file"
    "pdf"))

    \ \ \ \ \ \ ...)

    \ \ ---

    \ \ (if (window-per-buffer?)

    \ \ \ \ \ \ ("Close window" (close-document)))

    \ \ (if (not (window-per-buffer?))

    \ \ \ \ \ \ ("Close document" (close-document)))

    \ \ ("Close TeXmacs" (safely-quit-TeXmacs)))
  </scm-code>

  <\big-figure|<image|images/file-menu.png|0.3par|||>>
    The <menu|File> menu defined above.
  </big-figure>

  An entry is a label followed by the <scheme> code to execute;
  <scm|(-\<gtr\> "Import" ...)> makes a submenu (pullright) and <scm|---> a
  separator. The shortcuts in the menus are found by <TeXmacs> itself. They
  are defined in the same way, with <scm|kbd-map>, in files such as
  <verbatim|progs/generic/generic-kbd.scm> and
  <verbatim|progs/math/math-kbd.scm>:

  <\scm-code>
    (kbd-map

    \ \ (:profile std)

    \ \ ("std a" (select-all))

    \ \ ("std b" (toggle-bold))

    \ \ ("std c" (kbd-copy))

    \ \ ("std s" (save-buffer))

    \ \ ("std v" (kbd-paste))

    \ \ ...)

    \;

    (kbd-map

    \ \ (:mode in-math?)

    \ \ ("math f" (make-fraction))

    \ \ ("math s" (make-sqrt))

    \ \ ("\<less\> =" "\<less\>leqslant\<gtr\>")

    \ \ ("- \<gtr\>" "\<less\>rightarrow\<gtr\>")

    \ \ ...)
  </scm-code>

  and the prefixes of section<nbsp>1 are defined in
  <verbatim|progs/texmacs/keyboard/prefix-kbd.scm>:

  <\scm-code>
    (when (like-macos?)

    \ \ (kbd-wildcards pre

    \ \ \ \ ("std" "M-" #t) ; Command

    \ \ \ \ ("cmd" "C-") ; Control

    \ \ \ \ ("altcmd" "M-C-") ; Command+Control

    \ \ \ \ ...))

    \;

    (when (like-windows?) ; idem for Gnome, KDE, Emacs

    \ \ (kbd-wildcards pre

    \ \ \ \ ("std" "C-" #t) ; Ctrl

    \ \ \ \ ("cmd" "A-") ; Alt

    \ \ \ \ ("altcmd" "M-") ; Windows key

    \ \ \ \ ...))
  </scm-code>

  <section|How to create your own menus>

  The menu bar contains an empty menu, <scm|texmacs-extra-menu>, reserved for
  the user. Put the following lines in your <verbatim|my-init-texmacs.scm>:

  <\scm-code>
    (menu-bind texmacs-extra-menu

    \ \ (former)

    \ \ (=\<gtr\> "AAA" (link aaa-menu))

    \ \ (=\<gtr\> "BBB" (link aaa-menu)))

    \;

    (menu-bind aaa-menu

    \ \ ("AAA 111" (insert "Menu: aaa 111"))

    \ \ ("AAA 222" (insert "Menu: aaa 222"))

    \ \ ---

    \ \ (-\<gtr\> "AAA 333" (link file-menu))

    \ \ (-\<gtr\> "AAA 444" (link file-menu)))
  </scm-code>

  <\itemize>
    <item>The constructor <scm|=\<gtr\>> makes a pulldown menu (here two new
    entries <menu|AAA> and <menu|BBB> of the menu bar).

    <item>The constructor <scm|-\<gtr\>> makes a pullright menu (a submenu):
    <menu|AAA|AAA 333> opens the <menu|File> menu.

    <item>The constructor <scm|---> separates groups of entries.

    <item><scm|(former)> keeps what other parts of <TeXmacs> (some plugins
    and styles, such as <tmstyle|literate>) put into the same menu.
  </itemize>

  After a restart of <TeXmacs>, the two menus appear between <menu|Insert>
  and <menu|Focus>; <menu|AAA|AAA 111> and <menu|AAA|AAA 222> insert their
  text in the document:

  <\big-figure|<image|images/menu-aaa.png|0.8par|||>>
    The new menu <menu|AAA>, with its submenu <menu|AAA 333>.
  </big-figure>

  <section|How to create your own keyboard shortcuts>

  Shortcuts are defined with <scm|kbd-map>. The option <scm|:mode> limits
  them to a mode: <scm|in-text?>, <scm|in-math?>, <scm|in-prog?>... A key
  combination may be a single modified key (<verbatim|"M-A-w">) or a sequence
  of keys separated by spaces (<verbatim|"e q u .">): when you type the last
  key of a sequence, the characters typed before are removed and the action
  is done.

  <\scm-code>
    ;; Keyboard shortcuts only valid in text mode

    \;

    (kbd-map

    \ \ (:mode in-text?)

    \ \ ("M-A-w" (insert "Welcome to TeXmacs!"))

    \ \ ("e q u ." (insert-go-to "Eq. ()" '(5)) (make 'reference) (insert
    "equ:"))

    \ \ ("f i g ." (insert "Fig. ") (make 'reference) (insert "fig:"))

    \ \ ("t a b ." (insert "Table ") (make 'reference) (insert "tab:"))

    \ \ ("r e f ." (make 'cite) (insert "bib:")))

    \;

    ;; Keyboard shortcuts only valid in math mode

    \;

    (kbd-map

    \ \ (:mode in-math?)

    \ \ ("g r a d ." (insert "\<less\>nabla\<gtr\>"))

    \ \ ("d i v ." (insert "\<less\>nabla\<gtr\>\<less\>cdot\<gtr\>"))

    \ \ ("r o t ." (insert "\<less\>nabla\<gtr\>\<less\>times\<gtr\>"))

    \ \ ("e q u ." (make 'label) (insert "equ:")))
  </scm-code>

  Notice that the same sequence <verbatim|e q u .> does different things in
  text and in formulas. Typing, in text mode, <key|M-A-w>, and then
  <verbatim|grad.>, <verbatim|div.>, <verbatim|rot.> in a formula,
  <verbatim|equ.max> at the start of an equation and <verbatim|equ.max>,
  <verbatim|fig.lines>, <verbatim|tab.data>, <verbatim|ref.jackson> in the
  text gives:

  <\big-figure|<image|images/shortcuts-demo.png|0.8par|||>>
    The new shortcuts at work: the citation is still being typed (the
    references to a figure and a table which do not exist are shown as
    <with|color|dark red|?>).
  </big-figure>

  The result of a shortcut can also be a string: <scm|("p i ."
  "\<less\>pi\<gtr\>")> inserts <math|\<pi\>>. A shortcut which already
  exists in <TeXmacs> for the same mode is replaced by yours (a shortcut of a
  more specific mode, for instance of formulas, still takes precedence in
  that mode).

  <section|How to create your own icon bar><label|sec-icons>

  Below the toolbars of <TeXmacs> there is room for a bar of your own icons,
  <scm|texmacs-extra-icons>. Each icon opens a menu; here the menus are grids
  (<scm|tile>) of mathematical symbols:

  <\scm-code>
    (menu-bind texmacs-extra-icons

    \ \ (former)

    \ \ (=\<gtr\> (balloon (icon "my_lowgre.png") "Lowercase Greek")

    \ \ \ \ \ \ (tile 6 (link my-lowercase-greek)))

    \ \ (=\<gtr\> (balloon (icon "my_uppgre.png") "Uppercase Greek")

    \ \ \ \ \ \ (tile 4 (link my-uppercase-greek)))

    \ \ \|

    \ \ (=\<gtr\> (balloon (icon "my_binope.png") "Binary operations")

    \ \ \ \ \ \ (tile 5 (link my-binary-operations)))

    \ \ (=\<gtr\> (balloon (icon "my_binrel.png") "Binary relations")

    \ \ \ \ \ \ (tile 5 (link my-binary-relations)))

    \ \ (=\<gtr\> (balloon (icon "my_negrel.png") "Negated relations")

    \ \ \ \ \ \ (tile 5 (link my-negated-relations)))

    \ \ \|

    \ \ (=\<gtr\> (balloon (icon "my_arrows.png") "Arrows")

    \ \ \ \ \ \ (tile 5 (link my-arrows)))

    \ \ (=\<gtr\> (balloon (icon "my_missym.png") "Miscellaneous symbols")

    \ \ \ \ \ \ (tile 5 (link my-miscellaneous-symbols))))
  </scm-code>

  The original version of this tutorial had an eighth icon, for special
  delimiters (<math|\<langle\>>, <math|\<rangle\>>, <math|\<lfloor\>>,
  <math|\<rfloor\>>...); it is left as an exercise: add a menu
  <scm|my-delimiters> and its icon in the same way.

  with menus of symbols such as:

  <\scm-code>
    (menu-bind my-lowercase-greek

    \ \ (symbol "\<less\>alpha\<gtr\>") (symbol "\<less\>beta\<gtr\>")
    (symbol "\<less\>gamma\<gtr\>")

    \ \ (symbol "\<less\>delta\<gtr\>") (symbol "\<less\>epsilon\<gtr\>")
    (symbol "\<less\>varepsilon\<gtr\>")

    \ \ (symbol "\<less\>zeta\<gtr\>") (symbol "\<less\>eta\<gtr\>") (symbol
    "\<less\>theta\<gtr\>")

    \ \ (symbol "\<less\>vartheta\<gtr\>") (symbol "\<less\>iota\<gtr\>")
    (symbol "\<less\>kappa\<gtr\>")

    \ \ (symbol "\<less\>lambda\<gtr\>") (symbol "\<less\>mu\<gtr\>") (symbol
    "\<less\>nu\<gtr\>")

    \ \ (symbol "\<less\>xi\<gtr\>") (symbol "\<less\>pi\<gtr\>") (symbol
    "\<less\>varpi\<gtr\>")

    \ \ (symbol "\<less\>rho\<gtr\>") (symbol "\<less\>varrho\<gtr\>")
    (symbol "\<less\>sigma\<gtr\>")

    \ \ (symbol "\<less\>varsigma\<gtr\>") (symbol "\<less\>tau\<gtr\>")
    (symbol "\<less\>upsilon\<gtr\>")

    \ \ (symbol "\<less\>phi\<gtr\>") (symbol "\<less\>varphi\<gtr\>")
    (symbol "\<less\>chi\<gtr\>")

    \ \ (symbol "\<less\>psi\<gtr\>") (symbol "\<less\>omega\<gtr\>") (symbol
    "\<less\>varkappa\<gtr\>"))

    \;

    (menu-bind my-uppercase-greek

    \ \ (symbol "\<less\>Gamma\<gtr\>") (symbol "\<less\>Delta\<gtr\>")
    (symbol "\<less\>Theta\<gtr\>")

    \ \ (symbol "\<less\>Lambda\<gtr\>") (symbol "\<less\>Xi\<gtr\>") (symbol
    "\<less\>Pi\<gtr\>")

    \ \ (symbol "\<less\>Sigma\<gtr\>") (symbol "\<less\>Upsilon\<gtr\>")
    (symbol "\<less\>Phi\<gtr\>")

    \ \ (symbol "\<less\>Psi\<gtr\>") (symbol "\<less\>Omega\<gtr\>") (symbol
    "\<less\>digamma\<gtr\>"))
  </scm-code>

  <\itemize>
    <item>The constructor <scm|\|> separates groups of icons.

    <item><scm|(balloon (icon "my_lowgre.png") "Lowercase Greek")> shows the
    icon, with a tooltip.

    <item><scm|(tile 6 ...)> lays out the entries of a menu in a grid of 6
    columns.

    <item><scm|(symbol "\<less\>alpha\<gtr\>")> shows a symbol and inserts it
    in the document.
  </itemize>

  The icons are <verbatim|.png>, <verbatim|.svg> or <verbatim|.xpm> files
  that you put in the folder

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/misc/pixmaps
  </verbatim>

  For a sharp picture on high resolution screens, give the icon
  (20<math|\<times\>>20 pixels) a companion of twice the width and height
  (40<math|\<times\>>40) and the suffix <verbatim|_x2>:
  <verbatim|my_lowgre.png> and <verbatim|my_lowgre_x2.png>. The icons of this
  tutorial are <image|images/icons/my_lowgre_x2.png|1em|||>
  <image|images/icons/my_uppgre_x2.png|1em|||>
  <image|images/icons/my_binope_x2.png|1em|||>
  <image|images/icons/my_binrel_x2.png|1em|||>
  <image|images/icons/my_negrel_x2.png|1em|||>
  <image|images/icons/my_arrows_x2.png|1em|||>
  <image|images/icons/my_missym_x2.png|1em|||>.

  Finally, the bar of the user's icons must be made visible, with
  <menu|View|User provided icons>:

  <\big-figure|<image|images/view-menu.png|0.6par|||>>
    <menu|View|User provided icons>.
  </big-figure>

  <\big-figure|<image|images/user-icons-bar.png|0.8par|||>>
    The bar of the user's icons, below the toolbars of <TeXmacs>.
  </big-figure>

  <\big-figure|<image|images/user-icons-greek.png|0.8par|||>>
    The menu of the first icon.
  </big-figure>

  <\with|par-mode|right>
    \<copyright\> Nicolas Ratier
  </with>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|?|1>>
    <associate|auto-10|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-11|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-12|<tuple|2|3>>
    <associate|auto-14|<tuple|2|3>>
    <associate|auto-15|<tuple|3|4>>
    <associate|auto-16|<tuple|<with|mode|<quote|math>|\<bullet\>>|5>>
    <associate|auto-17|<tuple|<with|mode|<quote|math>|\<bullet\>>|5>>
    <associate|auto-18|<tuple|<with|mode|<quote|math>|\<bullet\>>|5>>
    <associate|auto-19|<tuple|<with|mode|<quote|math>|\<bullet\>>|5>>
    <associate|auto-2|<tuple|?|1>>
    <associate|auto-20|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|literate>|5>>
    <associate|auto-21|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|literate>|5>>
    <associate|auto-22|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|literate>|5>>
    <associate|auto-23|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|literate>|5>>
    <associate|auto-24|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|literate>|5>>
    <associate|auto-25|<tuple|3|5>>
    <associate|auto-28|<tuple|3|5>>
    <associate|auto-29|<tuple|3|5>>
    <associate|auto-3|<tuple|1|1>>
    <associate|auto-30|<tuple|4|5>>
    <associate|auto-31|<tuple|4|6>>
    <associate|auto-32|<tuple|5|7>>
    <associate|auto-33|<tuple|<with|mode|<quote|math>|\<bullet\>>|8>>
    <associate|auto-34|<tuple|5|8>>
    <associate|auto-36|<tuple|5|8>>
    <associate|auto-37|<tuple|6|8>>
    <associate|auto-38|<tuple|7|8>>
    <associate|auto-4|<tuple|1|1>>
    <associate|auto-5|<tuple|1|1>>
    <associate|auto-6|<tuple|2|1>>
    <associate|auto-7|<tuple|2|2>>
    <associate|auto-8|<tuple|1|2>>
    <associate|auto-9|<tuple|2|2>>
    <associate|sec-icons|<tuple|5|7>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|figure>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        The keyboard of T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>
        on the screen.
      </surround>|<pageref|auto-8>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|2>|>
        The <with|font-family|<quote|ss>|File> menu defined above.
      </surround>|<pageref|auto-12>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|3>|>
        The new menu <with|font-family|<quote|ss>|AAA>, with its submenu
        <with|font-family|<quote|ss>|AAA 333>.
      </surround>|<pageref|auto-25>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|4>|>
        The new shortcuts at work: the citation is still being typed (the
        references to a figure and a table which do not exist are shown as
        <with|color|<quote|dark red>|?>).
      </surround>|<pageref|auto-31>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|5>|>
        <with|font-family|<quote|ss>|View><with|mode|<quote|math>|\<rightarrow\>><with|font-family|<quote|ss>|User
        provided icons>.
      </surround>|<pageref|auto-34>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|6>|>
        The bar of the user's icons, below the toolbars of
        T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>.
      </surround>|<pageref|auto-37>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|7>|>
        The menu of the first icon.
      </surround>|<pageref|auto-38>>
    </associate>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Tools>|<with|font-family|<quote|ss>|Developer
      tool>>|<pageref|auto-1>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Developer>|<with|font-family|<quote|ss>|Open
      my-init-texmacs.scm>>|<pageref|auto-2>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Edit>|<with|font-family|<quote|ss>|Preferences>|<with|font-family|<quote|ss>|General>>|<pageref|auto-5>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Developer>|<with|font-family|<quote|ss>|Show
      keyboard>>|<pageref|auto-7>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Source>>|<pageref|auto-10>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>>|<pageref|auto-11>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>>|<pageref|auto-14>>

      <tuple|<tuple|<with|font-family|<quote|ss>|AAA>>|<pageref|auto-16>>

      <tuple|<tuple|<with|font-family|<quote|ss>|BBB>>|<pageref|auto-17>>

      <tuple|<tuple|<with|font-family|<quote|ss>|AAA>|<with|font-family|<quote|ss>|AAA
      333>>|<pageref|auto-18>>

      <tuple|<tuple|<with|font-family|<quote|ss>|File>>|<pageref|auto-19>>

      <tuple|<tuple|<with|font-family|<quote|tt>|language|<quote|verbatim>|color|<quote|brown>|literate>>|<pageref|auto-20>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>>|<pageref|auto-21>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>>|<pageref|auto-22>>

      <tuple|<tuple|<with|font-family|<quote|ss>|AAA>|<with|font-family|<quote|ss>|AAA
      111>>|<pageref|auto-23>>

      <tuple|<tuple|<with|font-family|<quote|ss>|AAA>|<with|font-family|<quote|ss>|AAA
      222>>|<pageref|auto-24>>

      <tuple|<tuple|<with|font-family|<quote|ss>|AAA>>|<pageref|auto-28>>

      <tuple|<tuple|<with|font-family|<quote|ss>|AAA 333>>|<pageref|auto-29>>

      <tuple|<tuple|<with|font-family|<quote|ss>|View>|<with|font-family|<quote|ss>|User
      provided icons>>|<pageref|auto-33>>

      <tuple|<tuple|<with|font-family|<quote|ss>|View>|<with|font-family|<quote|ss>|User
      provided icons>>|<pageref|auto-36>>
    </associate>
    <\associate|table>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        The modifier keys of T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>.
      </surround>|<pageref|auto-4>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|2>|>
        The main prefixes of the keyboard shortcuts.
      </surround>|<pageref|auto-6>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>The
      T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>
      keyboard> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>How
      T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>
      defines its own menus> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>How
      to create your own menus> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-15><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>How
      to create your own keyboard shortcuts>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-30><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|5<space|2spc>How
      to create your own icon bar> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-32><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>