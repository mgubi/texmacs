<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <scheme> widget language and its interpreter>

  <section|Overview>

  The <scheme> part of the widget system lives in
  <source-link|progs/kernel/gui/|TeXmacs/progs/kernel/gui>:

  <\description>
    <item*|<source-link|gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm>>the style constants
    <scm|widget-style-*> and the low-level macros <scm|$list>, <scm|$when>,
    <scm|$-\<gtr\>>, <scm|$input>, ... which build <em|menu items>;
    also the <scm|$form> macros and, in its second half, macros for
    generating documents (<scm|$para>, <scm|$itemize>, ...), which are
    not discussed here.

    <item*|<source-link|menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm>>the user-level macros
    <scm|menu-bind>, <scm|tm-menu>, <scm|tm-widget>, <scm|menu-dynamic>
    and the translator <scm|gui-make> with its table
    <scm|gui-make-table>.

    <item*|<source-link|menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>>the grammar of menu items, the
    interpreter <scm|make-menu-widget> which turns them into <c++> widgets,
    <scm|menu-expand>, the top-level window functions (<scm|top-window>,
    <scm|dialogue-window>, ...) and the side tools
    (<scm|tm-tool>).

    <item*|<source-link|menu-convert.scm|TeXmacs/progs/kernel/gui/menu-convert.scm>>an alternative, experimental
    interpreter which renders menu items as <TeXmacs> markup.

    <item*|<source-link|menu-test.scm|TeXmacs/progs/kernel/gui/menu-test.scm>>test widgets.
  </description>

  How to <em|write> menus and widgets is explained in \P<hlink|Extending the
  graphical user interface|../scheme/gui/scheme-gui.en.tm>\Q. Here we
  describe what happens behind the scenes.

  <section|Three representations>

  It is important to distinguish three stages.

  <\enumerate>
    <item><em|Source.> The body of a <scm|menu-bind>, <scm|tm-menu> or
    <scm|tm-widget> is written in the widget language, for instance

    <\scm-code>
      (tm-widget (my-widget cmd)

      \ \ (hlist

      \ \ \ \ (text "Name:")

      \ \ \ \ (input (set! my-name answer) "string" (list my-name) "20em")

      \ \ \ \ // (explicit-buttons ("Ok" (cmd "ok")))))
    </scm-code>

    At <em|macro expansion> time, each keyword is translated by
    <scm|gui-make> into a call of a <scm|$>-macro of
    <source-link|gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm>.

    <item><em|Menu items.> When <scm|(my-widget cmd)> is called, the
    <scm|$>-macros build a plain list, the <em|menu item>, such as

    <\scm-code>
      ((hlist (text "Name:")

      \ \ \ \ \ \ \ \ (input #\<less\>procedure\<gtr\> "string"
      #\<less\>procedure\<gtr\> "20em")

      \ \ \ \ \ \ \ \ (glue #f #f 5 0)

      \ \ \ \ \ \ \ \ (style 32 ("Ok" #\<less\>procedure\<gtr\>))))
    </scm-code>

    The parts of the widget that must be recomputed later (actions,
    predicates, proposals) are closures. The syntax of menu items is
    specified by the grammar <scm|:menu-item> given with
    <scm|define-regexp-grammar> at the beginning of
    <source-link|menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>.

    <item><em|<c++> widgets.> The interpreter <scm|make-menu-widget> walks
    through the menu item and calls the glued constructors
    (<scm|widget-hlist>, <scm|widget-text>, <scm|widget-input>,
    ...), producing a single <c++> <cpp|widget>.
  </enumerate>

  Menu items are also what the <c++> kernel manipulates: the strings
  passed to <cpp|menu_main>, <cpp|menu_icons> or <cpp|side_tools>, such as
  <verbatim|"(horizontal (link texmacs-menu))">, are quoted menu items,
  not source in the widget language. This is why both levels have keywords
  like <scm|link>, <scm|dynamic> or <scm|vertical>.

  <section|The definition macros>

  All definition forms end up in <scm|menu-dynamic>:

  <\scm-code>
    (tm-define-macro (menu-dynamic . l)

    \ \ `($list ,@(map gui-make l)))
  </scm-code>

  <\explain>
    <scm|(menu-bind <scm-arg|name> . <scm-arg|body>)><explain-synopsis|define
    a menu without arguments>

    <scm|(tm-menu (<scm-arg|name> . <scm-arg|args>) . <scm-arg|body>)>

    <scm|(tm-widget (<scm-arg|name> . <scm-arg|args>) .
    <scm-arg|body>)><explain-synopsis|define a menu or widget>
  <|explain>
    These expand into <scm|tm-define> of a function returning
    <scm|(menu-dynamic . <scm-arg|body>)>. Leading options such as
    <scm|(:require <scm-arg|pred>)> are passed to <scm|tm-define>, so that
    menus can be overloaded contextually like any other <scm|tm-define>d
    function, and <scm|(former)> inside the body refers to the previous
    definition. <scm|tm-menu> and <scm|tm-widget> are currently identical;
    <scm|menu-bind> defines a function without arguments. The plain
    variants <scm|define-menu> and <scm|define-widget> use <scm|define>.
  </explain>

  <\explain>
    <scm|(lazy-menu <scm-arg|module> . <scm-arg|names>)><explain-synopsis|lazy
    loading>
  <|explain>
    Declares that the menus <scm-arg|names> are defined in
    <scm-arg|module>, which is loaded on first use (via
    <scm|lazy-define>), or after an idle delay. It is used extensively in
    <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>. Before building the bars of a window, the
    kernel evaluates <scm|(lazy-initialize-force)>.
  </explain>

  <scm|$list> evaluates its arguments and flattens the sublists of the form
  <scm|(list . <scm-arg|items>)> (with <scm|gui-normalize>): this is how a
  keyword can expand into zero, one or several items.

  <section|The translation <scm|gui-make>>

  <scm|gui-make> maps each keyword of the widget language to a function of
  <scm|gui-make-table> which returns a <scm|$>-form. Symbols and strings
  are handled directly: <scm|---> becomes the separator <scm|$--->,
  <scm|/> and <scm|\|> the vertical separator, <scm|===>, <scm|//>,
  <scm|\<gtr\>\<gtr\>> and similar symbols become <scm|glue> items, and a
  list whose head is a string or a label, as <scm|("Ok" (cmd "ok"))>,
  becomes a button <scm|($\<gtr\> "Ok" (cmd "ok"))>, which evaluates to
  <scm|(list "Ok" (lambda () (cmd "ok")))>. New keywords can be added with
  <scm|extend-table>, as <source-link|menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm> itself does for
  <scm|section-tabs>, <scm|pick-color> and <scm|pick-background>.

  The main subtlety is <em|when> the various parts are evaluated. The
  following table lists the translations of the control keywords.

  <descriptive-table|<tformat|<table|<row|<cell|Keyword>|<cell|Translation>|<cell|Evaluated>>|<row|<cell|<scm|(link
  <scm-arg|m>)>>|<cell|<scm|(link <scm-arg|m>)>>|<cell|when the widget is
  built: the function <scm-arg|m> is called>>|<row|<cell|<scm|(dynamic
  <scm-arg|expr>)>>|<cell|splice the value of
  <scm-arg|expr>>|<cell|when the menu item is
  built>>|<row|<cell|<scm|(if <scm-arg|pred>
  . <scm-arg|items>)>>|<cell|<scm|(if <scm-arg|thunk> .
  <scm-arg|items>)>>|<cell|when the widget is built: items omitted if
  false>>|<row|<cell|<scm|(when <scm-arg|pred> .
  <scm-arg|items>)>>|<cell|<scm|(when <scm-arg|thunk> .
  <scm-arg|items>)>>|<cell|when the widget is built: items greyed if
  false>>|<row|<cell|<scm|(assuming <scm-arg|pred> .
  <scm-arg|items>)>>|<cell|<scm|$when>: splice or
  nothing>|<cell|when the menu item is
  built>>|<row|<cell|<scm|(for (<scm-arg|x> <scm-arg|l>) .
  <scm-arg|items>)>>|<cell|<scm|(for <scm-arg|fun>
  <scm-arg|thunk>)>>|<cell|when the widget is
  built>>|<row|<cell|<scm|(loop (<scm-arg|x> <scm-arg|l>) .
  <scm-arg|items>)>>|<cell|splice <scm|append-map>>|<cell|when the menu item
  is built>>|<row|<cell|<scm|let>, <scm|let*>, <scm|with>, <scm|cond>,
  <scm|receive>>|<cell|the <scheme> construct around
  <scm|menu-dynamic>>|<cell|when the menu item is
  built>>|<row|<cell|<scm|(promise <scm-arg|expr>)>>|<cell|<scm|(promise
  <scm-arg|thunk>)>>|<cell|when the widget is built; must return a menu
  item>>|<row|<cell|<scm|(-\<gtr\> <scm-arg|label> . <scm-arg|items>)>,
  <scm|(=\<gtr\> ...)>>|<cell|<scm|(-\<gtr\> <scm-arg|label> .
  <scm-arg|items>)>>|<cell|items built with the parent; submenu widget
  built when opened>>>>>

  In particular, <scm|when> and <scm|assuming> are <em|not> synonyms: the
  former greys out its items (by adding <cpp|WIDGET_STYLE_INERT> and
  <cpp|WIDGET_STYLE_GREY>) and re-evaluates the predicate each time the
  widget is built, while the latter includes or omits the items once and
  for all when the menu item is computed. (The macro names are swapped:
  <scm|when> is translated with <scm|$assuming>, and <scm|assuming> with
  <scm|$when>.)

  Leaf keywords take closures as well. For example <scm|(input
  <scm-arg|cmd> <scm-arg|type> <scm-arg|proposals> <scm-arg|width>)> is
  translated into

  <\scm-code>
    (list 'input (lambda (answer) <scm-arg|cmd>) <scm-arg|type> (lambda ()
    <scm-arg|proposals>) <scm-arg|width>)
  </scm-code>

  which explains why the result of input widgets is available as the
  variable <scm|answer> in the user code. <scm|toggle>, <scm|enum>,
  <scm|choice>, <scm|choices>, <scm|filtered-choice>, <scm|color-input>,
  <scm|tree-view>, <scm|texmacs-input> and <scm|texmacs-output> follow the
  same pattern (see their <scm|$>-macros in <source-link|gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm>).
  Style keywords (<scm|inert>, <scm|explicit-buttons>, <scm|bold>,
  <scm|grey>, <scm|mono>, <scm|verb>, <scm|plain-style>) become
  <scm|(style <scm-arg|n> . <scm-arg|items>)>, where <scm-arg|n> is a
  <scm|widget-style-*> constant; a positive <scm-arg|n> is or-ed into the
  current style, a negative one removed from it. The layout helpers
  <scm|padded>, <scm|centered> and <scm|bottom-buttons> are expanded into
  <scm|vlist>/<scm|hlist> combinations with glue.

  <section|The interpreter <scm|make-menu-widget>>

  <\explain>
    <scm|(make-menu-widget <scm-arg|p> <scm-arg|style>)><explain-synopsis|menu
    item to widget>
  <|explain>
    Transforms the menu item <scm-arg|p> into a <c++> widget, using the
    initial style <scm-arg|style> (usually 0). The item must produce
    exactly one widget, which is why callers wrap menus into
    <scm|(vertical ...)> or <scm|(horizontal ...)>. Malformed items are reported
    and replaced by a text widget displaying <verbatim|"Error">.
  </explain>

  The recursive work is done by <scm|(make-menu-items <scm-arg|p>
  <scm-arg|style> <scm-arg|bar?>)>, which returns a <em|list> of widgets.
  The flag <scm-arg|bar?> is true inside horizontal containers
  (<scm|horizontal>, <scm|hlist>, <scm|minibar>) and changes the rendering
  of buttons (icon bar buttons show a pressed state instead of a check
  mark). The dispatch works as follows:

  <\itemize>
    <item>the symbols <scm|---> and <scm|\|> produce separators
    (<scm|widget-separator>);

    <item>a list whose head is a label (a string, <scm|(icon ...)>,
    <scm|(balloon ...)>, <scm|(check ...)>, <scm|(shortcut ...)>, ...)
    is a menu entry, built by <scm|make-menu-entry>;

    <item>a list whose head is a symbol is looked up in
    <scm|make-menu-items-table>, which associates to each keyword a pattern
    for its arguments and a builder; for instance the entry for
    <scm|hlist> calls <scm|make-menu-hlist>, i.e.
    <scm|(widget-hlist (make-menu-items (cdr p) style #t))>;

    <item>any other list is treated as a sequence of items.
  </itemize>

  The builders of the dynamic keywords (<scm|if>, <scm|when>, <scm|for>,
  <scm|mini>, <scm|link>, <scm|dynamic>, <scm|promise>) evaluate their
  closures at this moment and recurse, which is why a widget reflects the
  state of the editor at the time it is built.

  <subsection|Menu entries>

  <scm|make-menu-entry> is the most elaborate builder. Given an entry
  <scm|(<scm-arg|label> <scm-arg|action>)>, it inspects the action. If the
  action is a closure of the form <scm|(lambda () (<scm-arg|f>
  <scm-arg|args> ...))> (this is tested by <scm|promise-source>), the
  properties of the <scm|tm-define>d function <scm-arg|f> are used:

  <\itemize>
    <item><scm|:check-mark> gives the check mark (<cpp|pre> argument of
    <cpp|menu_button>) and the predicate deciding whether it is shown;

    <item><scm|:balloon> or <scm|:synopsis> provide the help balloon
    (<scm|widget-balloon>);

    <item><scm|:interactive> appends <verbatim|"..."> to the label;

    <item><scm|:applicable> greys the entry when the predicate fails;

    <item>the keyboard shortcut is found by <scm|kbd-find-shortcut>, which
    searches the inverse keyboard bindings for the source expression of the
    action.
  </itemize>

  The label is turned into a widget by <scm|make-menu-label>
  (<scm|widget-text> after translation, <scm|widget-xpm> for icons,
  <scm|widget-color> for color samples, ...) and the action into a
  <c++> command by

  <\scm-code>
    (define (delay-command cmd)

    \ \ (object-\<gtr\>command (lambda () (exec-delayed cmd))))

    \;

    (define-macro (make-menu-command cmd)

    \ \ `(delay-command (lambda () (protected-call (lambda () ,cmd)))))
  </scm-code>

  so that the action is executed later, from the main loop, surrounded by
  the editor's <cpp|before_menu_action> and <cpp|after_menu_action> (see
  \P<hlink|Windows, the main <TeXmacs> widget and the flow of
  events|widgets-window.en.tm>\Q). Call-backs with arguments are wrapped
  by <scm|menu-protect>, which does the same for any number of arguments.
  Finally the entry becomes <scm|(widget-menu-button <scm-arg|label>
  <scm-arg|command> <scm-arg|check> <scm-arg|shortcut>
  <scm-arg|style>)>.

  <subsection|Submenus and lazy widgets>

  <scm|make-menu-submenu> does not build the contents of a submenu. It
  passes a promise to <scm|widget-pulldown-button> or
  <scm|widget-pullright-button>:

  <\scm-code>
    (object-\<gtr\>promise-widget

    \ \ (lambda () (make-menu-widget (list 'vertical items) style)))
  </scm-code>

  The port evaluates this promise each time the submenu is about to be
  shown, so that every opening of a menu reflects the current state (the
  closures inside <scm-arg|items> are re-run). Only the list
  <scm-arg|items> itself is computed together with the parent menu; to
  make the <em|list of entries> of a submenu dynamic, one uses <scm|link>
  or a <scm|dynamic> call inside it.

  <section|Expansion and caching>

  Building widgets is expensive, and the menus and icon bars of the main
  window are requested after almost every change. The kernel therefore
  only rebuilds them when their <em|expansion> changes.

  <\explain>
    <scm|(menu-expand <scm-arg|p>)><explain-synopsis|closure-free form of a
    menu item>
  <|explain>
    Returns a copy of <scm-arg|p> in which links, <scm|dynamic> parts,
    <scm|if>, <scm|for> and <scm|promise> items are evaluated and spliced,
    <scm|when> predicates are replaced by <scm|#t> or <scm|#f>, the current
    values of input fields, toggles, enumerations and choices are
    evaluated, and all remaining closures are replaced by their source code
    (<scm|replace-procedures>, using <scm|procedure-source>). Submenus
    (<scm|-\<gtr\>>, <scm|=\<gtr\>>) and refresh widgets are not expanded.
    The per-keyword behaviour is given by <scm|menu-expand-table>.
  </explain>

  Two menu items with equal expansions produce equivalent widgets. The
  <c++> function <cpp|tm_window_rep::get_menu_widget> uses the expansion
  as the key of a per-window cache and skips the update when the
  expansion of a bar did not change. The predicate
  <scm|(cache-menu? <scm-arg|r>)> forbids caching expansions which contain
  an <scm|input> field (whose state lives in the toolkit widget).

  A consequence is that a menu depending on some state which is invisible
  in its expansion (for instance a closure that reads a global variable,
  compared by its source) will not be updated automatically. In such cases,
  use an explicit <scm|refresh>/<scm|refreshable> widget, or make the state
  visible, for instance by testing it in an <scm|if> or <scm|when>.

  <section|Refresh widgets>

  Widgets inside dialogs and side tools are not rebuilt by
  <cpp|update_menus>. Their dynamic parts use one of three keywords, which
  map to the <c++> constructors <cpp|refresh_widget> and
  <cpp|refreshable_widget>.

  <\explain>
    <scm|(refresh <scm-arg|name> <scm-arg|kind>)><explain-synopsis|refresh
    by name>
  <|explain>
    Becomes <scm|(widget-refresh "<scm-arg|name>" "<scm-arg|kind>")>
    (<scm-arg|kind> is written as a symbol and defaults to <scm-arg|name>).
    The <c++> side evaluates <verbatim|(vertical (link
    <em|name>))>, i.e. <scm-arg|name> must be a menu without arguments. On
    each refresh of a matching kind it computes <scm|menu-expand> of this
    item and only rebuilds (with <scm|make-menu-widget>) if the expansion
    differs from the displayed one; built widgets are cached by expansion
    when <cpp|menu_caching> is on.
  </explain>

  <\explain>
    <scm|(refreshable <scm-arg|kind> . <scm-arg|items>)><explain-synopsis|refresh
    a closure>
  <|explain>
    Becomes <scm|(widget-refreshable <scm-arg|thunk> <scm-arg|kind>)>,
    where <scm-arg|thunk> builds <scm|(widget-vmenu ...)> from
    <scm-arg|items>. The widget is rebuilt on every refresh of a matching
    kind (unless the thunk returns the very same widget object). The
    <scm-arg|kind> is an expression, evaluated when the item is
    interpreted.
  </explain>

  <\explain>
    <scm|(cached <scm-arg|kind> <scm-arg|valid?> . <scm-arg|items>)><explain-synopsis|refresh
    with memoization>
  <|explain>
    Like <scm|refreshable>, but the built widget is stored in the table
    <scm|cached-widgets> under <scm-arg|kind> and reused as long as
    <scm-arg|valid?> holds. <scm|(invalidate-now <scm-arg|kind>)> removes
    the cached widget and refreshes.
  </explain>

  A refresh is requested with <scm|(refresh-now <scm-arg|kind>)>, which
  sends <cpp|SLOT_REFRESH> to the auxiliary windows. The kind
  <verbatim|"any"> in a widget matches every refresh, including the
  automatic ones that follow each menu action. A typical pattern:

  <\scm-code>
    (define my-counter 0)

    \;

    (tm-widget (my-tool)

    \ \ (refreshable "my-counter"

    \ \ \ \ (text (number-\<gtr\>string my-counter)))

    \ \ ("Increment" (set! my-counter (+ my-counter 1)) (refresh-now
    "my-counter")))
  </scm-code>

  <section|Windows and dialogs>

  <source-link|menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm> provides the standard ways of showing a
  widget in its own window, on top of the <scm|alt-window-*> primitives:

  <\explain>
    <scm|(top-window <scm-arg|menu-promise> <scm-arg|name> .
    <scm-arg|opts>)><explain-synopsis|show a widget in a window>
  <|explain>
    Calls <scm|(<scm-arg|menu-promise>)> to obtain the menu item, builds
    <scm|(vertical ...)> around it with <scm|make-menu-widget*>, creates a
    window with a fresh handle and shows it.
  </explain>

  <\explain>
    <scm|(dialogue-window <scm-arg|menu-promise> <scm-arg|cmd>
    <scm-arg|name> . <scm-arg|opts>)><explain-synopsis|show a dialog>
  <|explain>
    Like <scm|top-window>, but <scm-arg|menu-promise> is called with a
    closure which applies <scm-arg|cmd> to its arguments and then deletes
    the window. Widgets meant for dialogs therefore take this closure as
    their last argument (often named <scm|quit> or <scm|cmd>) and call it
    from their buttons.
  </explain>

  In both functions, <scm-arg|opts> may start with buffer <abbr|URL>s,
  followed by a command to be executed when the window is closed. The
  buffers are associated to the window with <scm|make-window-deleter>: a
  new dialog attached to the same buffer closes the previous one. The
  function <scm|interactive-window> is used for widgets built directly in
  <c++> (the printer and color picker dialogs): its promise receives a
  <c++> command and returns a widget.

  <scm|(interactive <scm-arg|fun>)> (<source-link|kernel/texmacs/tm-dialogue.scm|TeXmacs/progs/kernel/texmacs/tm-dialogue.scm>)
  asks the user for the arguments of a function, using its
  <scm|:argument>, <scm|:proposals> and <scm|:default> properties. It calls
  <scm|tm-interactive-hook>, which is <scm|tm-interactive-new> of
  <source-link|generic/generic-menu.scm|TeXmacs/progs/generic/generic-menu.scm>: when side tools are enabled, the
  question is shown in a transient bottom tool built in <scheme>;
  otherwise the <c++> function <cpp|tm_frame_rep::interactive> (glue
  <scm|tm-interactive>) is used, which asks in the footer or builds an
  <cpp|inputs_list_widget>. <scm|user-ask> and <scm|user-confirm> are thin
  layers over <scm|tm-interactive>.

  <section|Forms>

  <scm|(form <scm-arg|name> . <scm-arg|items>)> groups input fields whose
  values are collected by name, without explicit call-backs. The macro
  <scm|$form> binds the variables <scm|form-name>, <scm|form-entries> and
  <scm|form-text-entries> around its items; the field keywords
  (<scm|form-input>, <scm|form-enum>, <scm|form-choice>,
  <scm|form-choices>, <scm|form-toggle>) register the field in
  <scm|form-entries> and translate into ordinary input widgets whose
  call-back stores the answer with <scm|form-named-set> in the global
  table <scm|form-last>. Inside the form, <scm|(form-ref
  <scm-arg|field>)>, <scm|(form-fields)> and <scm|(form-values)> give access
  to the collected values. A <scm|form-input> passes an input type of the
  form <verbatim|<em|field>#form-<em|name>-<em|n>:<em|type>> to
  <cpp|input_text_widget>; the <name|Qt> port parses it
  (<cpp|QTMLineEdit::set_type>) and commits the text of such fields
  continuously, so that <scm|form-last> is always up to date.

  <section|Side tools>

  The side, left and bottom tools of the main window are widgets too. The
  bars are filled by the kernel with <verbatim|(dynamic (texmacs-side-tools
  <em|win>))> and similar expressions; <scm|texmacs-side-tools> (in
  <source-link|texmacs/menus/main-menu.scm|TeXmacs/progs/texmacs/menus/main-menu.scm>) lists, for each position, the
  tools attached to the window with <scm|(window-\<gtr\>tools <scm-arg|win>
  . <scm-arg|positions>)> and displays each of them with
  <scm|texmacs-side-tool>.

  A tool is defined with <scm|(tm-tool (<scm-arg|name> <scm-arg|win> .
  <scm-arg|args>) . <scm-arg|body>)> (or <scm|tm-tool*>, without the
  default centering and indentation). The macro defines a widget
  <scm-arg|name> and an overloaded case of <scm|texmacs-side-tool>
  (with <scm|:require>) which adds a title bar with a close button, built
  from the options <scm|(:name ...)> and <scm|(:quit ...)>. Tools are
  attached to windows with <scm|set-window-tools>,
  <scm|tool-select>, <scm|tool-toggle> and <scm|tool-close>, which also
  toggle the visibility of the side and bottom bars. Modules providing
  tools can be loaded lazily with <scm|lazy-tool>.

  <section|The markup interpreter>

  <source-link|menu-convert.scm|TeXmacs/progs/kernel/gui/menu-convert.scm> contains a second interpreter,
  <scm|build-menu-widget>, which has the same structure as
  <scm|make-menu-widget> but turns menu items into <TeXmacs> <em|markup>
  (via functions <scm|markup-hlist>, <scm|markup-menu-button>, ...).
  <scm|make-menu-widget**> then displays this markup in a
  <scm|texmacs-input> widget using the style packages
  <verbatim|gui-bright>, <verbatim|gui-dark> or <verbatim|gui-button>.
  Actions are stored in a table and referenced from the markup through
  <scm|eval-nullary-mangled> and <scm|eval-unary-mangled>. This path is
  taken by <scm|make-menu-widget*> when <scm|(has-markup-gui?)> holds,
  that is when both preferences <verbatim|markup gui> and
  <verbatim|developer tool> are on; it is experimental, and described in
  <hlink|the graphical user interface through markup|gui-markup.en.tm>.

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
