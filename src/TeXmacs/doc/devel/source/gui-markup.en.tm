<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The graphical user interface through markup (experimental)>

  The <em|graphical user interface through markup> builds dialogs and other
  widgets as <TeXmacs> documents, shown in a <TeXmacs> view, instead of
  native widgets of the toolkit. The widgets are then typeset, themed and
  rendered as any document is, which would unify the various rendering
  mechanisms of the interface (the native widgets of <name|Qt>, <name|Vue>
  or <name|Cocoa>, and the typesetter), and make the interface stylable by
  style files. It is in development: it covers the dialogs and the top
  windows made from <scm|tm-widget> descriptions, while the menus, the
  tool bars and the side tools remain native widgets.

  <section|Turning it on>

  The path is taken when <scm|(has-markup-gui?)> holds
  (<source-link|tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm:128>),
  that is when the preferences <verbatim|markup gui> and <verbatim|developer
  tool> are both on: enable <menu|Tools|Developer tool>, then
  <menu|View|GUI through markup> (<scm|toggle-markup-gui>,
  <source-link|tm-view.scm|TeXmacs/progs/texmacs/texmacs/tm-view.scm>).
  <scm|make-menu-widget*>
  (<source-link|menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm:1205>)
  then builds markup instead of native widgets for <scm|top-window> and
  <scm|dialogue-window>.

  <section|Two generations>

  The style packages of <verbatim|packages/gui>
  (<verbatim|gui>, <verbatim|gui-widget>, <verbatim|gui-form>,
  <verbatim|gui-layout> and the themes <verbatim|gui-metal>,
  <verbatim|gui-brushed>, <verbatim|gui-granite>, from 2007) are a first
  attempt: forms with inputs (<markup|short-input>, <markup|wide-input>,
  ...), buttons, toggles and radio buttons whose actions are links
  (<markup|action>), framed boxes made with <markup|ornament>, and textures
  as themes. They are used by the old widgets of
  <source-link|kernel/old-gui|TeXmacs/progs/kernel/old-gui> (<scm|widget-popup>, <scm|widget-ref>,
  <scm|widget-set!>), which show them in an auxiliary buffer; this
  generation is legacy.

  The current generation is in <verbatim|packages/new-gui> (from 2022):
  its elements react to the mouse by themselves, through two primitives of
  the typesetter, and the interpreter of <scm|tm-widget> descriptions
  produces it. The rest of this chapter is about it.

  <section|The style packages>

  <\description>
    <item*|<source-link|gui-button.ts|TeXmacs/packages/new-gui/gui-button.ts>>The
    core package (<scm|gui-system-font>, colours <src-var|gui-bg-color>,
    <src-var|button-bg-color>, <src-var|gui-input-color>...). It defines:

    <\itemize>
      <item>buttons: <markup|action-button>, <markup|action-button*>,
      <markup|menu-button>, <markup|menu-button*>, each with its looks
      <verbatim|-normal>, <verbatim|-hover> and <verbatim|-pressed>
      (<markup|ornament>s, with a blurred contour <markup|gui-contour>);

      <item>toggles: <markup|toggle-button>, <markup|toggle-on-button>,
      <markup|toggle-off-button>;

      <item>layout: <markup|hlist>, <markup|vlist>, <markup|tiled>,
      <markup|align-tiled> (rewritten into tables by <scheme>),
      <markup|glue>, <markup|raw-table>;

      <item>inputs: <markup|input-area>, <markup|input-field>,
      <markup|input-popup>, <markup|input-list>, and the lists
      <markup|choice-list>, <markup|check-list>;

      <item>tabs: <markup|tabs>, <markup|tabs-bar>, <markup|tabs-body>,
      <markup|active-tab>, <markup|passive-tab>;

      <item>forms: <markup|form-input-text>, <markup|form-text-area>,
      <markup|form-checkbox>;

      <item>styles of text and sections (<markup|title-style>,
      <markup|section-style>, <markup|plain-style>...), sizes
      (<markup|minipar>) and the whole widget, <markup|top-widget>.
    </itemize>

    <item*|<verbatim|gui-bright>, <verbatim|gui-dark>>Themes: they load
    <verbatim|gui-button> and set its colours. <scm|get-gui-style>
    (<source-link|menu-convert.scm|TeXmacs/progs/kernel/gui/menu-convert.scm:1086>)
    chooses one from the preference <verbatim|gui theme>, which also
    chooses the theme of <name|Vue>.

    <item*|<verbatim|gui-base>, <verbatim|side-tools>>Settings of a
    document shown as a widget, and a smaller magnification for the side
    tools.

    <item*|<source-link|gui-keyboard.ts|TeXmacs/packages/new-gui/gui-keyboard.ts>>Virtual
    keyboards: <markup|keyboard>, <markup|std-key>, <markup|extended-key>,
    <markup|simple-key>, <markup|mod-key>; with the style
    <verbatim|new-gui> (<verbatim|styles/test/new-gui.ts>).
  </description>

  <section|How the elements react>

  An element of the new generation is drawn by <markup|dynamic-case>, which
  chooses one of its looks from the messages which its box receives, and
  sends its events to <scheme> with <markup|relay>. For instance
  <markup|action-button*> is

  <\tm-fragment>
    <inactive*|<dynamic-case|click,drag|<relay|<action-button-pressed*|x>|gui-on-select|cmd>|mouse-over|<relay|<action-button-hover*|x>|gui-on-select|cmd>|any|<relay|<action-button-normal*|x>|gui-on-select|cmd>>>
  </tm-fragment>

  <\itemize>
    <item><markup|dynamic-case> is typeset as a <cpp|case_box>
    (<source-link|case_boxes.cpp|src/Typeset/Boxes/Composite/case_boxes.cpp>),
    with all its branches; a message (<verbatim|click>,
    <verbatim|mouse-over>, <verbatim|focus>, a broadcast message, or a
    comma separated list of them; <verbatim|any> matches all) shows the
    branch of the first case which it satisfies.

    <item><markup|relay> is typeset as a <cpp|relay_box>
    (<source-link|change_boxes.cpp|src/Typeset/Boxes/Modifier/change_boxes.cpp:955>):
    a message calls the <scheme> function given as second argument with the
    type of the event, its position and the other (evaluated) arguments,
    through <scm|secure-eval>; a result other than <scm|#f> invalidates the
    box, and a string or tree result is given back.

    <item>In an editor (a document, an auxiliary buffer, a
    <scm|texmacs-input> widget), <cpp|edit_interface_rep::mouse_click> and
    <cpp|mouse_select> send <verbatim|click> and <verbatim|select> to the
    boxes first (<source-link|edit_mouse.cpp|src/Edit/Interface/edit_mouse.cpp>); a
    non-empty answer stops the usual moving of the cursor. In a
    <scm|texmacs-output> widget, <cpp|box_widget_rep::handle_mouse>
    (<source-link|tm_button.cpp|src/Texmacs/Window/tm_button.cpp:191>) does
    the same.

    <item><scm|broadcast-message> sends a message to all the boxes of the
    view (<cpp|edit_interface_rep::broadcast_message>): the keyboard uses it
    to show its keys in their shifted form (<verbatim|shift>,
    <verbatim|no-shift>) without typesetting again.
  </itemize>

  Some elements are links instead (<markup|passive-tab> is an
  <markup|action>): they follow the usual links, with a double click in an
  editable document and the security checks of scripts.

  <section|The <scheme> support>

  <source-link|gui-utils.scm|TeXmacs/progs/utils/misc/gui-utils.scm>, loaded by
  <verbatim|gui-button> (<markup|use-module>), receives the events:

  <\description>
    <item*|<scm|gui-on-select>>On <verbatim|click> and <verbatim|drag> it
    only consumes the event; on <verbatim|select> (the button released) it
    gives the focus back to the master buffer, evaluates the command of the
    element (a string) when idle, and closes the tooltips.

    <item*|<scm|gui-on-toggle>>Changes the value of the toggle in the
    document and runs its command with <scm|answer> (and <scm|name>, for
    <markup|form-checkbox>) bound.

    <item*|<scm|gui-on-choice>>Updates the current item of a choice list
    and runs its command with <scm|answer>.

    <item*|<scm|keyboard-press>>Overloaded in input fields: Return runs the
    command of an <markup|input-field> with its value; in the fields of
    forms, every key does.

    <item*|<scm|tab-select>>Switches the tabs in place.

    <item*|<scm|gui-hlist-table>, <scm|gui-vlist-table>, <scm|gui-tiled>>Rewrite
    the lists into tables (<markup|extern>).
  </description>

  The commands in the markup are evaluated with <scm|secure-eval>: they
  may only use secure functions (<scm|tm-define> with <scm|(:secure
  #t)>), which is why the functions above are secure. The keyboard emulates
  keys with <scm|emu-key> and <scm|emu-toggle-modifier>
  (<source-link|gui-keyboard.scm|TeXmacs/progs/utils/misc/gui-keyboard.scm>
  generates its layouts).

  <section|The interpreter of widgets>

  <source-link|menu-convert.scm|TeXmacs/progs/kernel/gui/menu-convert.scm>
  turns a <scm|tm-widget> description into this markup, as
  <scm|make-menu-widget> turns it into native widgets (see <hlink|the
  markup interpreter|widgets-scheme.en.tm>): <scm|build-menu-widget> calls
  a <scm|markup-...> function for each item (texts, menu buttons, toggles,
  inputs and enumerations, choices, tabs, lists, balloons, divisions,
  <scm|texmacs-output> and <scm|texmacs-input>). The commands are kept in
  a table and referred to by <scm|(eval-nullary-mangled <var|n>)> or
  <scm|(eval-unary-mangled <var|n> answer)>. <scm|make-menu-widget**>
  shows the document <scm|(top-widget ...)> in a <scm|texmacs-input> widget
  of the auxiliary buffer <verbatim|tmfs://aux/gui/...>, whose master is the
  current buffer, with the style <scm|(tuple "generic" <var|theme>)>.

  Not done yet: a <scm|refreshable> is expanded once, without being shown
  again on <scm|refresh-now>; separators, <scm|extend> and
  <scm|scrollable> give nothing; colour pickers, tree views and ink
  widgets are placeholders; shortcuts are not shown; the table of commands
  is never emptied.

  <section|Where it is used>

  <\itemize>
    <item>The dialogs and top windows, with <menu|View|GUI through markup>.

    <item>The custom keyboard (<menu|Developer|Custom keyboard>), shown in
    a <scm|texmacs-output> widget with the style <verbatim|new-gui>, or as
    native buttons (<name|Qt>) built from the same markup.

    <item>The preferences of a server, a form
    (<verbatim|progs/forms/server-preferences.tm>) with
    <markup|form-input-text> and <markup|form-checkbox>.
  </itemize>

  <section|A small example>

  A document with the style <verbatim|gui-button> (or <verbatim|gui-dark>,
  <verbatim|gui-bright>) shows such a widget; the commands are written as
  strings, and may use <scm|answer> and <scm|name>:

  <\verbatim-code>
    \<less\>TeXmacs\|2.1.4\<gtr\>

    \<less\>style\|\<less\>tuple\|generic\|gui-button\<gtr\>\<gtr\>

    \<less\>\\body\<gtr\>

    \ \ \<less\>\\top-widget\<gtr\>

    \ \ \ \ \<less\>vlist\|\<less\>title-style\|Demo\<gtr\>

    \ \ \ \ \ \ \|\<less\>hlist\|\<less\>action-button\|Hello\|(display* "hello\\n")\<gtr\>

    \ \ \ \ \ \ \ \ \ \ \ \  \|\<less\>form-checkbox\|flag\|false\|(display* name " = " answer "\\n")\<gtr\>\<gtr\>

    \ \ \ \ \ \ \|\<less\>choice-list\|(display* answer "\\n")\|Red\|Red\|Green\|Blue\<gtr\>

    \ \ \ \ \ \ \|\<less\>input-field\|string\|(display* answer "\\n")\|10em\|type here\<gtr\>

    \ \ \ \ \ \ \|\<less\>tabs\|\<less\>tabs-bar\|\<less\>active-tab\|One\<gtr\>\|\<less\>passive-tab\|Two\<gtr\>\<gtr\>

    \ \ \ \ \ \ \ \ \ \ \ \  \|\<less\>tabs-body\|\<less\>shown\|First page\<gtr\>\|\<less\>hidden\|Second page\<gtr\>\<gtr\>\<gtr\>\<gtr\>

    \ \ \<less\>/top-widget\<gtr\>

    \<less\>/body\<gtr\>
  </verbatim-code>

  The same markup can be shown in a dialog with <scm|texmacs-output>, as
  <scm|(texmacs-output '(...) '(style (tuple "generic" "gui-button")))>.

  <section|Loose ends>

  <\itemize>
    <item><markup|toggle-on-button> and <markup|toggle-off-button> mark an
    argument <src-arg|x> which they do not have.

    <item><scm|gui-on-toggle> builds <verbatim|(with name '<var|cmd>)>
    for a <markup|toggle-button> (which has no name), so that the command
    is not run: the toggles made by the interpreter do nothing, while
    <markup|form-checkbox> works.

    <item>The keyboard <scm|narrow-us-keyboard> tests modifiers in upper
    case, while they are kept in lower case.
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
