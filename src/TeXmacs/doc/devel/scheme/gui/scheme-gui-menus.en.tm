<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Menus and toolbars>

  As we said before, menus are special collections of widgets: they are
  written in the same language as the widgets of dialogs, and the same
  keywords may be used in both cases (see the <hlink|reference
  guide|scheme-gui-reference.en.tm>). The main differences are the way
  they are defined and the containers used to display them.

  <paragraph*|Defining menus>

  A menu is defined with

  <\scm-code>
    (menu-bind <scm-arg|name>

    \ \ <scm-arg|options>

    \ \ <scm-arg|items>)
  </scm-code>

  which defines a function <scm-arg|name> without arguments, or with
  <scm|(tm-menu (<scm-arg|name> <scm-arg|args>) <scm-arg|options>
  <scm-arg|items>)> for menus with arguments. Both are thin wrappers around
  <scm|tm-define>, so that the <scm-arg|options> <scm|(:mode
  <scm-arg|mode?>)> and <scm|(:require <scm-arg|pred>)> may be used for
  <hlink|contextual overloading|../overview/overview-overloading.en.tm>,
  and <scm|(former)> may be used inside a redefinition in order to include
  the previous definition. For instance, the following code adds an entry
  to the <menu|Insert> menu in the mode <scm|in-database?> only (similar
  code can be found in <verbatim|progs/database/db-menu.scm>):

  <\scm-code>
    (menu-bind insert-menu

    \ \ (:mode in-database?)

    \ \ (former)

    \ \ ---

    \ \ ("Database entry" (interactive make-db-entry)))
  </scm-code>

  The items of a menu are typically

  <\itemize>
    <item>entries <scm|(<scm-arg|label> <scm-arg|cmd> ...)>, where the
    label is a string or a composite label built with <scm|icon>,
    <scm|balloon>, <scm|check>, <scm|shortcut>, <scm|concat>,
    <scm|verbatim> or <scm|replace>;

    <item>submenus <scm|(-\<gtr\> <scm-arg|label> <scm-arg|items>)>
    (pullright) and <scm|(=\<gtr\> <scm-arg|label> <scm-arg|items>)>
    (pulldown, as in menu bars and toolbars);

    <item>separators <scm|---> (in vertical menus) and <scm|\|> (in
    horizontal menus), and group titles <scm|(group <scm-arg|title>)>;

    <item>links <scm|(link <scm-arg|other-menu>)> which include the
    contents of another menu, and <scm|(dynamic (<scm-arg|some-menu>
    <scm-arg|args>))> for menus with arguments;

    <item>conditional items <scm|(if <scm-arg|pred> <scm-arg|items>)>
    (hidden when <scm-arg|pred> does not hold) and <scm|(when <scm-arg|pred>
    <scm-arg|items>)> (greyed out when <scm-arg|pred> does not hold), and
    loops <scm|(for (<scm-arg|x> <scm-arg|l>) <scm-arg|items>)>;

    <item>tiles <scm|(tile <scm-arg|columns> <scm-arg|items>)> for
    palettes, such as the menus of mathematical symbols.
  </itemize>

  Check-marks, dots for interactive commands, tooltips and keyboard
  shortcuts are usually not specified in the menu itself, but deduced from
  the properties of the command (see \P<hlink|Meta information and logical
  programming|../overview/overview-meta.en.tm>\Q):

  <\scm-code>
    (tm-define (toggle-session-math-input)

    \ \ (:synopsis "Toggle mathematical input in sessions")

    \ \ (:check-mark "v" session-math-input?)

    \ \ ...)

    \;

    (menu-bind session-input-menu

    \ \ ...

    \ \ (when (in-plugin-with-converters?)

    \ \ \ \ ("Mathematical input" (toggle-session-math-input)))

    \ \ ...)
  </scm-code>

  Since menus are recomputed each time they are displayed, conditions like
  <scm|(if (in-math?) ...)> are evaluated with respect to the current cursor
  position.

  <paragraph*|The main menus and toolbars>

  The <c++> part of <TeXmacs> asks the <scheme> code for the menus of each
  window (see <verbatim|src/Edit/Interface/edit_interface.cpp>). The menu
  bar is <scm|(horizontal (link texmacs-menu))> and the four toolbars are
  built from <scm|texmacs-main-icons>, <scm|texmacs-mode-icons>,
  <scm|texmacs-focus-icons> and <scm|texmacs-extra-icons>. The context menu
  which is opened by a right click is <scm|texmacs-popup-menu> (or
  <scm|texmacs-alternative-popup-menu> when the shift or control key is
  held down). The
  side and bottom panels are made of the widgets <scm|texmacs-side-tools>,
  <scm|texmacs-left-tools> and <scm|texmacs-bottom-tools>, which display
  the <em|tools> defined with <scm|tm-tool>. Most of these menus are
  defined in <verbatim|progs/texmacs/menus/main-menu.scm>.

  The menus <scm|texmacs-extra-menu> (inserted in the menu bar before
  <menu|Focus>), <scm|texmacs-extra-icons>, <scm|plugin-menu> and
  <scm|plugin-icons> are empty by default and are intended to be extended by
  users and plug-ins. For instance:

  <\scm-code>
    (menu-bind texmacs-extra-menu

    \ \ (former)

    \ \ (=\<gtr\> "Greetings"

    \ \ \ \ \ \ ("Hello" (insert "Hello"))

    \ \ \ \ \ \ ("Goodbye" (insert "Goodbye"))))
  </scm-code>

  In a toolbar, the entries are usually icons with a tooltip, as in the
  following excerpt of <scm|texmacs-main-icons>:

  <\scm-code>
    (=\<gtr\> (balloon (icon "tm_open.xpm") "Load a file") (link load-menu))

    ((balloon (icon "tm_build.xpm") "Update this buffer")

    \ (update-document "all"))
  </scm-code>

  <paragraph*|Menus in other places>

  A menu can also be displayed in a separate window with
  <scm|(top-window <scm-arg|menu> <scm-arg|title>)>, or included in a
  dialog with <scm|(link <scm-arg|menu>)>. Conversely, the entries of a
  menu are rendered as flat buttons outside menus, unless they are enclosed
  in <scm|explicit-buttons>. Menus which are expensive to compute should be
  declared lazily in the initialization files with <scm|(lazy-menu
  <scm-arg|module> <scm-arg|names>)> (see \P<hlink|The module system and
  lazy definitions|../overview/overview-lazyness.en.tm>\Q).

  The way menus are expanded and turned into native widgets is described in
  \P<hlink|The <scheme> widget language and its
  interpreter|../../source/widgets-scheme.en.tm>\Q.

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
