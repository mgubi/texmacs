<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Modes, conditional definitions and lazy loading>

  <section|Defining modes>

  Modes are declared with the macro <scm|texmacs-modes> of
  <source-link|kernel/texmacs/tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm>. Each entry has the form

  <\scm-code>
    (<scm-arg|name>% <scm-arg|test> <scm-arg|parent>% ...)
  </scm-code>

  where the name must end with <verbatim|%>, <scm-arg|test> is a <scheme>
  expression or <scm|#t>, and the optional parents are other modes. The
  entry defines a public predicate whose name is obtained by replacing the
  final <verbatim|%> by <verbatim|?>; the predicate holds when the test
  <em|and> all parent predicates hold. For instance, the standard modes
  include

  <\scm-code>
    (texmacs-modes

    \ \ (in-text% (and (== (get-env "mode") "text") (not (in-graphics?))))

    \ \ (in-math% (and (== (get-env "mode") "math") (not (in-graphics?))))

    \ \ (in-table% (and (inside? 'table) (not (in-graphics?))))

    \ \ (in-session% (and (or (inside? 'session) (inside? 'program))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (not (in-graphics?))))

    \ \ (in-math-in-session% #t in-math% in-session%)

    \ \ (in-sem-math% (== (get-preference "semantic correctness") "on")
    in-math%)

    \ \ ...)
  </scm-code>

  so that <scm|(in-math-in-session?)> is <scm|(and (in-math?)
  (in-session?))>. Besides the predicate, the macro

  <\itemize>
    <item>associates the predicate with both symbols <scm|in-math%> and
    <scm|in-math?> (<scm|set-symbol-procedure!>), which is how
    <scm|texmacs-in-mode?> evaluates a mode given by name;

    <item>records each parent relation as a rule of the <hlink|logic
    programming layer|drd-scheme.en.tm>, so that <scm|(texmacs-submode?
    <scm-arg|what> <scm-arg|of>)> can decide whether one mode is a sub-mode
    of another. Every mode is a sub-mode of <scm|always%>, and
    <scm|prevail%> is a sub-mode of every mode.
  </itemize>

  The predicates are defined in the module <verbatim|texmacs-user>, so they
  are visible everywhere. <source-link|tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm> defines the modes for the
  editing context (<scm|in-source?>, <scm|in-text?>, <scm|in-math?>,
  <scm|in-prog?>, <scm|in-hybrid?>, <scm|in-table?>, <scm|in-session?>,
  ...), for the document style (<scm|in-std?>, <scm|in-beamer?>,
  <scm|in-manual?>, <scm|in-database?>, ...), for the current language
  (<scm|in-french?>, <scm|in-math-german?>, ...), for the programming
  language (<scm|in-prog-python?>, ...), for the look and feel and for the
  input mode of the editor (<scm|search-mode?>, <scm|spell-mode?>, ...).
  Plug-ins add their own modes in the same way.

  Mode predicates are evaluated often, typically for every key press and
  every menu update, so they must be cheap and free of side effects.

  <section|Conditional definitions>

  <subsection|Functions>

  A function defined with <scm|tm-define> may be redefined any number of
  times, in any module, under a condition given by the option
  <scm|:mode> (a mode predicate) or <scm|:require> (an arbitrary
  expression over the arguments):

  <\scm-code>
    (tm-define (kbd-enter t shift?)

    \ \ (:require (list-context? t))

    \ \ (if shift? (make-return-after) (make-item)))
  </scm-code>

  The implementation in <source-link|kernel/texmacs/tm-define.scm|TeXmacs/progs/kernel/texmacs/tm-define.scm> is simple:
  a conditional redefinition replaces the global binding by a function of
  the form

  <\scm-code>
    (lambda (t shift?)

    \ \ (if (list-context? t)

    \ \ \ \ \ \ (if shift? (make-return-after) (make-item))

    \ \ \ \ \ \ (former t shift?)))
  </scm-code>

  where <scm|former> is the previous binding. Consequently

  <\itemize>
    <item>the most recently <em|loaded> definition is tried first, so the
    order in which modules are loaded matters, and a general definition
    must be loaded before the specific ones;

    <item>several conditions (several <scm|:mode> and <scm|:require>
    options) are combined with <scm|and>;

    <item>the body of a definition may call <scm|former> explicitly to
    extend rather than replace the previous behaviour; this is how
    <source-link|math/math-sem-edit.scm|TeXmacs/progs/math/math-sem-edit.scm> wraps <scm|kbd-insert>,
    <scm|kbd-backspace> and <scm|make>;

    <item>a first definition with a condition gets an empty
    <scm|former> and causes the warning \Pconditional master routine\Q
    on the console.
  </itemize>

  Other options attach properties instead of conditions (<scm|:synopsis>,
  <scm|:argument>, <scm|:proposals>, <scm|:check-mark>, <scm|:balloon>,
  <scm|:interactive>, <scm|:secure>, <scm|:applicable>, ...). They are used
  by menus and by the interactive commands. <scm|tm-property> attaches such
  properties to an existing function without redefining it.

  <subsection|Keyboard shortcuts>

  Keyboard shortcuts are declared with <scm|kbd-map>
  (<source-link|kernel/gui/kbd-define.scm|TeXmacs/progs/kernel/gui/kbd-define.scm>):

  <\scm-code>
    (kbd-map

    \ \ (:mode in-math?)

    \ \ ("math:small (" (math-bracket-open "(" ")" #f))

    \ \ ...

    \ \ ...)
  </scm-code>

  Each entry maps a key sequence to either a string (a shorthand which is
  inserted with <scm|kbd-insert>) or to code, which is wrapped in a
  procedure. The options <scm|(:mode <scm-arg|pred>)> and <scm|(:require
  <scm-arg|expr>)> at the start of the block become a list of condition
  procedures which is stored with each binding. The table
  <scm|kbd-map-table> maps every key sequence to a list of
  (<em|conditions>, <em|binding>) pairs; a new binding for the same key and
  the same conditions replaces the old one, other bindings are put in front
  of the list. <scm|kbd-find-key-binding> returns the first binding whose
  conditions all hold, so here too the most recently loaded definition
  wins.

  Before a binding is stored, the key sequence is rewritten with the
  <em|pre-wildcards> of the server (<cpp|kbd_pre_rewrite>, see <hlink|the
  partial server tm_config_rep|server-classes.en.tm>). This is how
  symbolic prefixes such as <verbatim|math:small>, <verbatim|structured:cmd>
  or <verbatim|table> are turned into concrete modifier combinations; the
  prefixes are declared with <scm|kbd-wildcards> in
  <source-link|texmacs/keyboard/prefix-kbd.scm|TeXmacs/progs/texmacs/keyboard/prefix-kbd.scm> and depend on the look and
  feel. The suffix <verbatim|var> stands for the variant key (by default
  <key|tab>): <verbatim|"a var"> is the binding for <key|a> followed by
  <key|tab>.

  The full path of a key press is:

  <\enumerate>
    <item>the toolkit calls <cpp|edit_interface_rep::handle_keypress>,
    which calls the <scheme> function <scm|keyboard-press>
    (<source-link|kernel/gui/kbd-handlers.scm|TeXmacs/progs/kernel/gui/kbd-handlers.scm>), which by default calls back
    the editor with <scm|key-press>;

    <item>the editor (<source-link|Edit/Interface/edit_keyboard.cpp|src/Edit/Interface/edit_keyboard.cpp>) appends
    the key to the pending shortcut and asks the server for a binding
    (<cpp|get_keycomb>), which ends in <scm|kbd-find-key-binding>;

    <item>if a binding is found, its command is executed, or its shorthand
    is inserted with <scm|(kbd-insert <scm-arg|s>)>;

    <item>otherwise, a single printable character is inserted with
    <scm|(kbd-insert <scm-arg|s>)>.
  </enumerate>

  <scm|kbd-insert> is itself overloaded: in math mode
  (<source-link|math/math-edit.scm|TeXmacs/progs/math/math-edit.scm>) it removes a space typed before an infix
  operator, and in semantic math mode it checks the syntactic correctness
  of the result. The details of the <c++> side are in <hlink|keyboard
  events|server-events.en.tm>.

  <subsection|Menus and icon bars>

  A menu is a zero argument function which returns a menu description:
  <scm|(menu-bind <scm-arg|name> . <scm-arg|items>)> expands into
  <scm|(tm-define (<scm-arg|name>) ...)>, and <scm|tm-menu> defines menus
  with arguments, typically the focus tree. Both accept the same options as
  <scm|tm-define>, so menus are overloaded exactly like functions:

  <\scm-code>
    (menu-bind texmacs-mode-icons

    \ \ (:mode in-database?)

    \ \ (link db-insert-icons)

    \ \ ...)
  </scm-code>

  replaces the mode dependent icon bar inside bibliographic databases. The
  mode dependent parts of the interface are reached from a few fixed
  entry points:

  <\description>
    <item*|<scm|texmacs-mode-icons>>The mode dependent icon bar
    (<source-link|texmacs/menus/main-menu.scm|TeXmacs/progs/texmacs/menus/main-menu.scm>), which links to
    <scm|text-icons>, <scm|math-icons>, <scm|prog-icons>, ... according
    to the mode.

    <item*|<scm|format-menu>>The <menu|Format> menu
    (<source-link|generic/format-menu.scm|TeXmacs/progs/generic/format-menu.scm>), which links to
    <scm|text-format-menu> or <scm|math-format-menu>.

    <item*|<scm|texmacs-focus-icons> and <scm|focus-menu>>The focus icon
    bar and the <menu|Focus> menu (<source-link|generic/generic-menu.scm|TeXmacs/progs/generic/generic-menu.scm>),
    which are built from the focus tree, see <hlink|the generic structured
    editing hooks|modes-structured.en.tm>.

    <item*|<scm|texmacs-side-tools>, <scm|texmacs-bottom-tools>>The side
    and bottom tool areas, see <hlink|widgets from
    <scheme>|widgets-scheme.en.tm>.
  </description>

  The editor rebuilds these menus whenever the context may have changed;
  how the result is cached is described in <hlink|windows, menus, dialogs
  and embedded widgets|server-windows.en.tm>.

  <section|Lazy loading>

  Most mode modules are not loaded at startup. <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>
  only declares when they are needed, with the macros of
  <hlink|lazy definitions|../scheme/overview/overview-lazyness.en.tm>:

  <\scm-code>
    (lazy-keyboard (math math-kbd) in-math?)

    (lazy-keyboard (math math-sem-edit) in-sem-math?)

    (lazy-menu (math math-menu) math-format-menu math-format-icons ...)

    (lazy-initialize (math math-menu) (in-math?))

    (lazy-define (math math-edit) brackets-refresh)
  </scm-code>

  <\description>
    <item*|<scm|lazy-keyboard>>Registers a module with a mode. Every key
    lookup first calls <scm|lazy-keyboard-force>, which loads the modules
    of all modes which currently hold. In addition, each module is loaded
    unconditionally after 250<nbsp>ms of idle time.

    <item*|<scm|lazy-menu>>Declares the menus provided by a module (with
    <scm|lazy-define>) and loads the module after 500<nbsp>ms of idle time.

    <item*|<scm|lazy-define>>Declares functions provided by a module; the
    first call loads the module.

    <item*|<scm|lazy-initialize>>Loads a module when a predicate holds at
    the next call of <scm|lazy-initialize-force> (which happens before
    the menus and toolbars of a window are rebuilt, see
    <cpp|tm_window_rep::menu_main>), or after 5<nbsp>s of idle time.
  </description>

  Lazy loading interacts with conditional definitions: since the most
  recently loaded definition is tried first, a module which is loaded late
  takes precedence over modules loaded earlier, whatever the order of the
  declarations in <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>. A module therefore declares
  the modules it extends in its <scm|:use> clause, which forces them to be
  loaded first; for example <source-link|math/math-kbd.scm|TeXmacs/progs/math/math-kbd.scm> uses
  <source-link|generic/generic-kbd.scm|TeXmacs/progs/generic/generic-kbd.scm>, <source-link|math/math-edit.scm|TeXmacs/progs/math/math-edit.scm> and
  <source-link|table/table-edit.scm|TeXmacs/progs/table/table-edit.scm>.

  <section|Organization of a mode>

  The modules of a mode follow a naming convention, which is also the
  order in which they usually depend on each other:

  <\description>
    <item*|<verbatim|<em|mode>-drd.scm>>Tag groups (<scm|define-group>,
    <scm|define-alternate>) describing the markup of the mode: which tags
    are variants of each other, which are lists, sections, enunciations,
    ... The context predicates of the edit module are written in terms of
    these groups. See <hlink|tag groups|drd-scheme.en.tm>.

    <item*|<verbatim|<em|mode>-edit.scm>>Editing routines: context
    predicates (<scm|list-context?>, <scm|script-context?>, ...), commands
    called from menus and keyboard (<scm|make-section>,
    <scm|math-bracket-open>, ...), and the redefinitions of the generic
    hooks for the tags of the mode.

    <item*|<verbatim|<em|mode>-kbd.scm>>Keyboard shortcuts, in
    <scm|kbd-map> blocks conditioned on the mode.

    <item*|<verbatim|<em|mode>-menu.scm>>Menus and icon bars, including the
    redefinitions of the focus menus for the tags of the mode.

    <item*|<verbatim|<em|mode>-markup.scm>><scheme> functions which are
    called while tags of the mode are typeset, through the <markup|extern>
    primitive of the style files (for instance <scm|screens-index> in
    <source-link|dynamic/fold-markup.scm|TeXmacs/progs/dynamic/fold-markup.scm>, or the callbacks of spreadsheets in
    <source-link|dynamic/calc-markup.scm|TeXmacs/progs/dynamic/calc-markup.scm>).

    <item*|<verbatim|<em|mode>-doc.scm>>Documentation hooks, such as the
    help about tags shown by the focus menu.

    <item*|<verbatim|<em|mode>-widgets.scm>, <verbatim|<em|mode>-tools.scm>>Dialogs
    and side tools.

    <item*|<verbatim|<em|mode>-speech*.scm>>Speech input for the mode
    (<source-link|kernel/gui/speech-define.scm|TeXmacs/progs/kernel/gui/speech-define.scm>), loaded together with the
    keyboard.
  </description>

  Not every mode has every file, and some files mix several roles (tag
  groups for tables are declared in <source-link|table/table-edit.scm|TeXmacs/progs/table/table-edit.scm>, for
  instance).

  <section|Pitfalls>

  <\itemize>
    <item>A redefinition with <scm|tm-define> which repeats the condition of
    an earlier one completely hides it; this happens for instance with the
    <scm|standard-parameters> of the <markup|input>, <markup|output>,
    <markup|errput> and <markup|textput> tags, which are defined both in
    <source-link|dynamic/session-edit.scm|TeXmacs/progs/dynamic/session-edit.scm> and in
    <source-link|dynamic/program-edit.scm|TeXmacs/progs/dynamic/program-edit.scm> (the bodies currently compute the
    same result).

    <item>Because the order of loading determines the precedence, the same
    sequence of user actions can behave differently depending on whether a
    lazy module has already been loaded. Code which overrides a definition
    from a lazily loaded module should <scm|:use> that module.

    <item>A definition such as <scm|(tm-define (make . l) ...)> in
    <source-link|math/math-sem-edit.scm|TeXmacs/progs/math/math-sem-edit.scm> has no condition: once the module is
    loaded, it wraps every call of <scm|make> in the whole program. It
    tests the <verbatim|semantic correctness> preference on each call.

    <item><source-link|kernel/gui/kbd-define.scm|TeXmacs/progs/kernel/gui/kbd-define.scm> accepts a bare symbol at the
    start of a <scm|kbd-map> block (<scm|kbd-map-body>), but turns it into
    the condition list <scm|(0 <scm-arg|symbol>)>, whose first element is
    not a procedure; no keymap uses this form, and it would fail at the
    first lookup of such a key.
  </itemize>

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
