<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Widgets reference guide>

  This is a reference list of all the keywords which may be used inside
  <scm|menu-bind>, <scm|tm-menu> and <scm|tm-widget> definitions. The
  authoritative list is the table <scm|gui-make-table> in
  <source-link|progs/kernel/gui/menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm>: each keyword is translated
  by <scm|gui-make> into one of the <scm|$>-macros of
  <source-link|progs/kernel/gui/gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm>, and the resulting menu items
  are turned into actual widgets by <scm|make-menu-widget> in
  <source-link|progs/kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>. How this works is explained
  in \P<hlink|The <scheme> widget language and its
  interpreter|../../source/widgets-scheme.en.tm>\Q.

  In what follows, <scm-arg|items> stands for any number of menu items
  (widgets), <scm-arg|label> for a menu label (see below) and
  <scm-arg|cmd> for arbitrary <scheme> code which is executed when the
  widget is activated. For input widgets, the code <scm-arg|cmd> is
  wrapped into a function of one argument, so that the value entered by the
  user is available in <scm-arg|cmd> as the variable <scm|answer>. Most
  other arguments (lists of values, default values, predicates) are
  re-evaluated each time the widget is built.

  <section|Buttons, labels and text>

  <\explain>
    <scm|(<scm-arg|label> <scm-arg|cmd> ...)><explain-synopsis|a menu entry
    or button>
  <|explain>
    A list whose first element is a label (a string or one of the label
    constructs below) is a menu entry: when it is selected, the remaining
    elements are evaluated as a <scheme> body. Inside a window, the entry is
    rendered as a flat (toolbar style) button, unless it is enclosed in
    <scm|explicit-buttons>. When the command is a function with an
    <scm|:interactive> property, dots are appended to the label; a
    <scm|:check-mark> property yields a check-mark, a <scm|:balloon> or
    <scm|:synopsis> property yields a tooltip, and a keyboard shortcut for
    the command is displayed automatically.
  </explain>

  <\explain>
    <scm|(text <scm-arg|s>)><explain-synopsis|a text label>
  <|explain>
    Displays the (translated) string <scm-arg|s>.
  </explain>

  <\explain>
    <scm|(group <scm-arg|s>)><explain-synopsis|a menu group title>
  <|explain>
    A title for a group of entries inside a menu.
  </explain>

  <\explain>
    <scm|(icon <scm-arg|file>)><explain-synopsis|an icon label>
  <|explain>
    Uses the pixmap <scm-arg|file> (for instance <scm|"tm_new.xpm">, looked
    up in <verbatim|$TEXMACS_PIXMAP_PATH>) as the label of an entry, as in
    <scm|((icon "tm_new.xpm") (new-document))>.
  </explain>

  <\explain>
    <scm|(concat <scm-arg|s1> ... <scm-arg|sn>)>

    <scm|(verbatim <scm-arg|s1> ... <scm-arg|sn>)>

    <scm|(replace <scm-arg|s> <scm-arg|arg1> ...
    <scm-arg|argn>)><explain-synopsis|composite labels>
  <|explain>
    Labels made of several translatable strings, labels which should not be
    translated, and labels <scm-arg|s> containing placeholders <verbatim|%1>,
    ..., <verbatim|%n> which are replaced by the (translated) arguments, as
    in <scm|((replace "Open %1" (verbatim "my-init-texmacs.scm")) ...)>.
  </explain>

  <\explain>
    <scm|(balloon <scm-arg|label> <scm-arg|tooltip>)><explain-synopsis|a
    label with a tooltip>
  <|explain>
    Attaches the tooltip <scm-arg|tooltip> to the entry with label
    <scm-arg|label>, as in <scm|((balloon (icon "tm_save.xpm") "Save this
    buffer") (save-buffer))>.
  </explain>

  <\explain>
    <scm|(check <scm-arg|label> <scm-arg|mark> <scm-arg|pred>)><explain-synopsis|a
    label with a check-mark>
  <|explain>
    Displays the check-mark <scm-arg|mark> (usually <scm|"v"> or
    <scm|"*">) in front of the entry whenever the predicate <scm-arg|pred>
    holds.
  </explain>

  <\explain>
    <scm|(shortcut <scm-arg|label> <scm-arg|keys>)><explain-synopsis|a
    label with an explicit shortcut>
  <|explain>
    Displays the keyboard shortcut <scm-arg|keys> next to the entry,
    instead of the automatically found one.
  </explain>

  <\explain>
    <scm|(symbol <scm-arg|sym>)>

    <scm|(symbol <scm-arg|sym> <scm-arg|cmd>)><explain-synopsis|a symbol
    button>
  <|explain>
    A button showing the <TeXmacs> symbol <scm-arg|sym> (like
    <scm|"\<less\>alpha\<gtr\>">). By default, pressing it inserts the
    symbol; otherwise <scm-arg|cmd> is executed.
  </explain>

  <\explain>
    <scm|(invisible <scm-arg|x>)><explain-synopsis|an invisible item>
  <|explain>
    Produces no widget at all; used internally by <scm|push-focus>.
  </explain>

  <section|Separators and glue>

  <\explain>
    <scm|(glue <scm-arg|hext?> <scm-arg|vext?> <scm-arg|w>
    <scm-arg|h>)><explain-synopsis|possibly extensible space>
  <|explain>
    Whitespace of minimal size <scm-arg|w><math|\<times\>><scm-arg|h>
    (in pixels), which may extend horizontally and/or vertically. See
    \P<hlink|Containers, glue, refresh and co.|scheme-gui-advanced.en.tm>\Q.
  </explain>

  <\explain>
    <scm|(color <scm-arg|col> <scm-arg|hext?> <scm-arg|vext?> <scm-arg|w>
    <scm-arg|h>)><explain-synopsis|colored glue>
  <|explain>
    Like <scm|glue>, but filled with the color (or pattern)
    <scm-arg|col>. Colored glue is typically used as the label of color
    selection buttons.
  </explain>

  <\explain>
    <scm|---> <scm|\|> <scm|/> <scm|===> <scm|======> <scm|//> <scm|///>
    <scm|\<gtr\>\<gtr\>> <scm|\<gtr\>\<gtr\>\<gtr\>><explain-synopsis|separators
    and glue abbreviations>
  <|explain>
    <scm|---> is a horizontal separator line (between the items of a
    vertical menu), <scm|\|> and <scm|/> are vertical separators. The other
    symbols are abbreviations for <scm|glue>: <scm|===> and <scm|======>
    for vertical space of 5 <abbr|resp.> 15 pixels, <scm|//> and <scm|///>
    for horizontal space of 5 <abbr|resp.> 15 pixels, <scm|\<gtr\>\<gtr\>>
    and <scm|\<gtr\>\<gtr\>\<gtr\>> for horizontally extensible space.
  </explain>

  <section|Input widgets>

  <\explain>
    <scm|(input <scm-arg|cmd> <scm-arg|type> <scm-arg|proposals>
    <scm-arg|width>)><explain-synopsis|a text input field>
  <|explain>
    A one line text field. <scm-arg|type> is a string such as
    <scm|"string">, <scm|"password">, <scm|"file">, <scm|"directory">,
    <scm|"search"> or <scm|"replace-what">, <scm-arg|proposals> a list of
    strings whose first element is the initial value, and <scm-arg|width>
    a length such as <scm|"20em"> or <scm|"1w">. The widths of the widgets
    are given in four units only: <verbatim|px>, <verbatim|em> (14 pixels),
    and <verbatim|w> or <verbatim|h>, a multiple of the default width or
    height of the widget in <name|Qt> and <name|Cocoa>, but of the extents
    of the window in <name|Vue> and <name|Widkit> (a field of
    <scm|"2w"> is then wider than its window: prefer <verbatim|em> in
    dialogs). <scm-arg|cmd> is executed with <scm|answer> set to the entered
    string when the user confirms the input (or to <scm|#f> when the input
    is cancelled).
  </explain>

  <\explain>
    <scm|(toggle <scm-arg|cmd> <scm-arg|on?>)><explain-synopsis|a check
    box>
  <|explain>
    A check box with initial state <scm-arg|on?>; <scm-arg|cmd> is executed
    with <scm|answer> set to the new state.
  </explain>

  <\explain>
    <scm|(enum <scm-arg|cmd> <scm-arg|vals> <scm-arg|val>
    <scm-arg|width>)><explain-synopsis|a combo box>
  <|explain>
    A combo box with the list of strings <scm-arg|vals> and the current
    value <scm-arg|val>. If the last element of <scm-arg|vals> is
    <scm|"">, then the user may also type an arbitrary value. See
    \P<hlink|Displaying lists and trees|scheme-gui-lists-trees.en.tm>\Q.
  </explain>

  <\explain>
    <scm|(choice <scm-arg|cmd> <scm-arg|vals> <scm-arg|val>)>

    <scm|(choices <scm-arg|cmd> <scm-arg|vals>
    <scm-arg|selected>)><explain-synopsis|list boxes>
  <|explain>
    A list of items allowing for the selection of one <abbr|resp.> several
    items. For <scm|choices>, <scm-arg|selected> and <scm|answer> are lists.
  </explain>

  <\explain>
    <scm|(filtered-choice <scm-arg|cmd> <scm-arg|vals> <scm-arg|val>
    <scm-arg|filter>)><explain-synopsis|a list box with a filter>
  <|explain>
    Like <scm|choice>, but with a text field on top which filters the
    displayed items; <scm|answer> holds the selected item and <scm|filter>
    the current contents of the filter field.
  </explain>

  <\explain>
    <scm|(tree-view <scm-arg|cmd> <scm-arg|data>
    <scm-arg|roles>)><explain-synopsis|a tree view (not in the
    <name|X11>/<name|Widkit> port)>
  <|explain>
    Displays the <TeXmacs> tree <scm-arg|data>; see \P<hlink|Displaying
    lists and trees|scheme-gui-lists-trees.en.tm>\Q. Contrary to the other
    input widgets, <scm-arg|cmd> must be a procedure.
  </explain>

  <\explain>
    <scm|(color-input <scm-arg|cmd> <scm-arg|background?>
    <scm-arg|proposals>)><explain-synopsis|a color picker>
  <|explain>
    A color picker, which also allows for the selection of patterns if
    <scm-arg|background?> holds.
  </explain>

  <\explain>
    <scm|(pick-color <scm-arg|cmd>)>

    <scm|(pick-background <scm-arg|scale> <scm-arg|cmd>)><explain-synopsis|palettes
    of colors and patterns>
  <|explain>
    Tiles of buttons for standard colors, <abbr|resp.> colors and
    background patterns; <scm-arg|cmd> is executed with <scm|answer> set to
    the selected color or pattern. In <name|Vue>, the color menu also offers
    a <with|font-series|bold|Palette> enumeration: typographic palettes,
    defined with <scm|define-typographic-palette> or
    <scm|define-typographic-palette-from-colors>, replace the standard
    colors (preference <verbatim|typographic palette set>).
  </explain>

  <\explain>
    <scm|(texmacs-output <scm-arg|doc> <scm-arg|style>)>

    <scm|(texmacs-input <scm-arg|doc> <scm-arg|style>
    <scm-arg|name>)><explain-synopsis|embedded <TeXmacs> documents>
  <|explain>
    A read-only <abbr|resp.> editable <TeXmacs> document <scm-arg|doc>
    (a tree or <scheme> tree), typeset with the given style, such as
    <scm|'(style "generic")>. For <scm|texmacs-input>, <scm-arg|name> is
    the <abbr|URL> of the auxiliary buffer which holds the document (or
    <scm|#f>).
  </explain>

  <\explain>
    <scm|(ink <scm-arg|cmd>)><explain-synopsis|a handwriting area>
  <|explain>
    An area for drawing with the mouse or a pen; <scm|answer> contains the
    recorded strokes.
  </explain>

  <\explain>
    <scm|(setting-toggle <scm-arg|cmd> <scm-arg|description>
    <scm-arg|on?>)>

    <scm|(setting-enum <scm-arg|cmd> <scm-arg|description>
    <scm-arg|vals> <scm-arg|val> <scm-arg|width>)>

    <scm|(setting-group <scm-arg|title> <scm-arg|items>)><explain-synopsis|preference
    panel controls>
  <|explain>
    Variants of <scm|toggle> and <scm|enum> which display an additional
    description text, and a titled group of such controls, intended for
    preference panels.
  </explain>

  <section|Menus>

  <\explain>
    <scm|(-\<gtr\> <scm-arg|label> <scm-arg|items>)>

    <scm|(=\<gtr\> <scm-arg|label> <scm-arg|items>)><explain-synopsis|submenus>
  <|explain>
    A button which opens the submenu made of <scm-arg|items> to the right
    (pullright) <abbr|resp.> below (pulldown). The submenu is only built
    when it is opened.
  </explain>

  <\explain>
    <scm|(horizontal <scm-arg|items>)>

    <scm|(vertical <scm-arg|items>)><explain-synopsis|menu bars and menus>
  <|explain>
    Horizontal <abbr|resp.> vertical menus, such as menu bars and toolbars
    <abbr|resp.> ordinary menus.
  </explain>

  <\explain>
    <scm|(tile <scm-arg|columns> <scm-arg|items>)><explain-synopsis|a tiled
    menu>
  <|explain>
    Arranges <scm-arg|items> in a table with <scm-arg|columns> columns, as in
    the palettes for mathematical symbols; <scm-arg|columns> must be an
    integer written in the menu (not an expression).
  </explain>

  <\explain>
    <scm|(minibar <scm-arg|items>)>

    <scm|(mini <scm-arg|pred> <scm-arg|items>)><explain-synopsis|minibars>
  <|explain>
    Small toolbars inside menus <abbr|resp.> items which are rendered in a
    smaller size when <scm-arg|pred> holds. Both only have an effect when the
    preference <verbatim|"use minibars"> is on; otherwise the items are
    displayed normally.
  </explain>

  <\explain>
    <scm|(link <scm-arg|menu>)><explain-synopsis|include another menu>
  <|explain>
    Includes the menu defined by <scm|(menu-bind <scm-arg|menu> ...)>; the
    menu is looked up each time the widget is built.
  </explain>

  <section|Layout>

  <\explain>
    <scm|(hlist <scm-arg|items>)>

    <scm|(vlist <scm-arg|items>)><explain-synopsis|horizontal and vertical
    lists>
  <|explain>
    Arrange <scm-arg|items> horizontally <abbr|resp.> vertically.
  </explain>

  <\explain>
    <scm|(aligned (item <scm-arg|left> <scm-arg|right>) ...)>

    <scm|(meti <scm-arg|right> <scm-arg|left>)><explain-synopsis|two column
    layout>
  <|explain>
    A two column table, typically with labels on the left and input
    widgets on the right. <scm|meti> is <scm|item> with the two arguments
    in the reverse order (so that a toggle can be written before its
    label).
  </explain>

  <\explain>
    <scm|(tabs (tab <scm-arg|label> <scm-arg|items>) ...)>

    <scm|(icon-tabs (icon-tab <scm-arg|icon> <scm-arg|label>
    <scm-arg|items>) ...)><explain-synopsis|tabbed widgets>
  <|explain>
    A tab widget with one page for each <scm|tab> <abbr|resp.>
    <scm|icon-tab>. The variants <scm|responsive-tabs> /
    <scm|responsive-tab> and <scm|responsive-icon-tabs> /
    <scm|responsive-icon-tab> have the same syntax but adapt their layout
    to the available space.
  </explain>

  <\explain>
    <scm|(section-tabs <scm-arg|name> <scm-arg|key> (section-tab
    <scm-arg|title> <scm-arg|items>) ...)><explain-synopsis|light-weight
    tabs>
  <|explain>
    Tabs made of ordinary buttons, implemented in the widget language
    itself using <scm|refreshable>; the index of the active tab is stored
    under the given <scm-arg|name> and <scm-arg|key> (see
    <scm|section-tab-ref>).
  </explain>

  <\explain>
    <scm|(hsplit <scm-arg|left> <scm-arg|right>)>

    <scm|(vsplit <scm-arg|top> <scm-arg|bottom>)><explain-synopsis|split
    panes>
  <|explain>
    Two widgets separated by a movable border.
  </explain>

  <\explain>
    <scm|(scrollable <scm-arg|items>)><explain-synopsis|scroll bars>
  <|explain>
    Puts <scm-arg|items> (vertically) inside a scroll area.
  </explain>

  <\explain>
    <scm|(resize <scm-arg|w> <scm-arg|h> <scm-arg|items>)><explain-synopsis|set
    the size>
  <|explain>
    Sets the size of <scm-arg|items>. Each of <scm-arg|w> and <scm-arg|h>
    is evaluated and is either a length string, like <scm|"200px">, or a
    list of three strings (minimal, default and maximal size), optionally
    followed by the initial position of the scrolled contents
    (<scm|"left">, <scm|"center"> or <scm|"right">, <abbr|resp.>
    <scm|"top">, <scm|"center"> or <scm|"bottom">; only honoured by
    <name|Vue>). Lists must be
    quoted, as in <scm|(resize '("100px" "200px" "400px") "50px" ...)>.
  </explain>

  <\explain>
    <scm|(padded <scm-arg|items>)>

    <scm|(centered <scm-arg|items>)>

    <scm|(bottom-buttons <scm-arg|items>)><explain-synopsis|layout helpers>
  <|explain>
    Surround <scm-arg|items> by fixed glue, center them, <abbr|resp.> put
    them as explicit buttons in a horizontal bar below a separator. These
    are expanded into combinations of <scm|vlist>, <scm|hlist> and
    <scm|glue>.
  </explain>

  <\explain>
    <scm|(division <scm-arg|name> <scm-arg|items>)>

    <scm|(class <scm-arg|name> <scm-arg|items>)><explain-synopsis|style
    sheet classes>
  <|explain>
    Vertical <abbr|resp.> horizontal groups of items to which the style
    sheet class <scm-arg|name> (like <scm|"title">, <scm|"plain">) is
    applied; used for the side tools and dialogs when a style sheet is
    active.
  </explain>

  <\explain>
    <scm|(extend <scm-arg|widget> <scm-arg|items>)><explain-synopsis|enlarge
    a widget>
  <|explain>
    Extends the size of <scm-arg|widget> to the maximum of the sizes of the
    <scm-arg|items> (the <scm-arg|items> themselves are not displayed).
    This is ignored by the <name|Qt> and <name|Cocoa> ports.
  </explain>

  <section|Styles>

  <\explain>
    <scm|(explicit-buttons <scm-arg|items>)> <scm|(inert <scm-arg|items>)>
    <scm|(bold <scm-arg|items>)> <scm|(grey <scm-arg|items>)> <scm|(mono
    <scm-arg|items>)> <scm|(verb <scm-arg|items>)> <scm|(plain-style
    <scm-arg|items>)><explain-synopsis|style attributes>
  <|explain>
    Render the entries of <scm-arg|items> as explicit buttons, as inactive
    (greyed out) widgets, in bold face, in grey, in a monospaced font,
    without translation of the labels; <scm|plain-style> keeps the style
    as it is (it is meant to reset it, but its implementation, a style of
    <scm|0> in <scm|make-menu-style>, changes nothing).
  </explain>

  <section|Control structures>

  <\explain>
    <scm|(if <scm-arg|pred> <scm-arg|items>)>

    <scm|(when <scm-arg|pred> <scm-arg|items>)>

    <scm|(assuming <scm-arg|pred> <scm-arg|items>)><explain-synopsis|conditional
    items>
  <|explain>
    <scm|if> only shows <scm-arg|items> when <scm-arg|pred> holds, and
    <scm|when> greys them out when it does not; in both cases,
    <scm-arg|pred> is re-evaluated each time the widget is built.
    <scm|assuming> includes or omits the items once and for all when the
    menu is computed. Notice that there is no <em|else> branch.
  </explain>

  <\explain>
    <scm|(cond (<scm-arg|pred> <scm-arg|items>) ...)>

    <scm|(let <scm-arg|bindings> <scm-arg|items>)> <scm|(let*
    <scm-arg|bindings> <scm-arg|items>)>

    <scm|(with <scm-arg|var> <scm-arg|val> <scm-arg|items>)> <scm|(receive
    <scm-arg|vars> <scm-arg|val> <scm-arg|items>)><explain-synopsis|<scheme>
    control structures>
  <|explain>
    The usual <scheme> (and <TeXmacs>) constructs, whose bodies are lists
    of widgets.
  </explain>

  <\explain>
    <scm|(for (<scm-arg|x> <scm-arg|l>) <scm-arg|items>)>

    <scm|(loop (<scm-arg|x> <scm-arg|l>) <scm-arg|items>)><explain-synopsis|loops>
  <|explain>
    Repeat <scm-arg|items> for each element <scm-arg|x> of the list
    <scm-arg|l>. For <scm|for>, the list is re-evaluated each time the
    widget is built; for <scm|loop>, only when the menu is computed.
  </explain>

  <\explain>
    <scm|(dynamic <scm-arg|expr>)><explain-synopsis|include a computed
    menu>
  <|explain>
    Evaluates <scm-arg|expr>, which usually is a call of a function defined
    with <scm|tm-menu> or <scm|tm-widget>, and includes the resulting items.
    This is the standard way to reuse a widget with arguments inside another
    one, as in <scm|(dynamic (my-widget cmd))>.
  </explain>

  <\explain>
    <scm|(eval <scm-arg|expr>)><explain-synopsis|computed label>
  <|explain>
    Evaluates <scm-arg|expr>, which should return a label or a menu item.
    For instance, <scm|(-\<gtr\> (eval (car p)) ...)> uses a computed label
    for a submenu.
  </explain>

  <\explain>
    <scm|(promise <scm-arg|expr>)><explain-synopsis|delayed items>
  <|explain>
    Evaluates <scm-arg|expr> each time the widget is built; the result must
    be a menu item.
  </explain>

  <\explain>
    <scm|(former)><explain-synopsis|previous definition>
  <|explain>
    Inside a redefinition of a menu with <scm|menu-bind> or
    <scm|tm-menu>, includes the items of the previous definition. See
    <hlink|contextual overloading|../overview/overview-overloading.en.tm>.
  </explain>

  <\explain>
    <scm|(push-focus <scm-arg|t> <scm-arg|items>)><explain-synopsis|focus
    dependent items>
  <|explain>
    Builds <scm-arg|items> with the variables <scm|pushed-tree> and
    <scm|pushed-focus> bound to <scm-arg|t> and its fingerprint; used by the
    focus menus and toolbars.
  </explain>

  <section|Refreshing>

  <\explain>
    <scm|(refreshable <scm-arg|kind> <scm-arg|items>)>

    <scm|(refresh <scm-arg|widget> <scm-arg|kind>)>

    <scm|(refresh <scm-arg|widget>)>

    <scm|(cached <scm-arg|kind> <scm-arg|valid?>
    <scm-arg|items>)><explain-synopsis|widgets which can be rebuilt>
  <|explain>
    A <scm|refreshable> group of items is rebuilt each time
    <scm|(refresh-now <scm-arg|kind>)> is called, where <scm-arg|kind> is
    (an expression evaluating to) a string. <scm|refresh> includes the menu
    or widget <scm-arg|widget> (the name of a <scm|menu-bind> or of a
    <scm|tm-widget> without arguments, not evaluated) in the same way; here
    <scm-arg|kind> is written as a symbol, as in <scm|(refresh my-widget
    auto)>; without <scm-arg|kind>, the kind is the name of the widget.
    Widgets of kind <verbatim|"auto"> are also refreshed
    automatically after each user command, and widgets of kind
    <verbatim|"any"> by any call of <scm|refresh-now>. A <scm|cached> group
    is rebuilt at a <scm|refresh-now> of its kind only if <scm-arg|valid?>
    does not hold or after <scm|(invalidate-now <scm-arg|kind>)>. A refresh
    makes the widgets again from the items computed when the enclosing
    widget was built: texts, labels and <scm|dynamic> keep their value,
    while conditions, <scm|for> lists and <scm|promise>s are evaluated
    again. See \P<hlink|Containers,
    glue, refresh and co.|scheme-gui-advanced.en.tm>\Q.
  </explain>

  <section|Forms>

  <\explain>
    <scm|(form <scm-arg|name> <scm-arg|items>)>

    <scm|(form-input <scm-arg|field> <scm-arg|type> <scm-arg|proposals>
    <scm-arg|width>)>

    <scm|(form-enum <scm-arg|field> <scm-arg|vals> <scm-arg|val>
    <scm-arg|width>)>

    <scm|(form-choice <scm-arg|field> <scm-arg|vals> <scm-arg|val>)>

    <scm|(form-choices <scm-arg|field> <scm-arg|vals>
    <scm-arg|selected>)>

    <scm|(form-toggle <scm-arg|field> <scm-arg|on?>)><explain-synopsis|forms>
  <|explain>
    Named input fields whose values are stored under the name of the
    enclosing form; see \P<hlink|Forms|scheme-gui-forms.en.tm>\Q.
  </explain>

  <tmdoc-copyright|2012\U2026|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
