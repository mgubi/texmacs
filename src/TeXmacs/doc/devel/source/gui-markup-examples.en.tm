<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Examples of the graphical user interface through markup>

  These documents show the elements of the <hlink|graphical user interface
  through markup|gui-markup.en.tm>. Open one of them (a click on its link),
  then use its elements: each one runs a command which shows in the footer
  what it did (with <scm|gui-message>, a secure function of the module
  <verbatim|(doc gui-markup-examples)>, which the documents load with
  <markup|use-module>; the commands of the markup may only use secure
  functions). They are ordinary documents with the style
  <verbatim|gui-button> (or a theme, or <verbatim|gui-keyboard>): their
  source, shown with <menu|Document|Source|Edit source tree>, is an example of
  the markup.

  <\itemize>
    <item><hlink|Buttons|gui-markup/gui-buttons.tm>: action and menu buttons, with their normal, hover and pressed looks.

    <item><hlink|Toggles|gui-markup/gui-toggles.tm>: form-checkbox, and a toggle-button whose command does not run.

    <item><hlink|Choice lists|gui-markup/gui-choices.tm>: choice-list, check-list and a popup of choices (input-popup).

    <item><hlink|Input fields|gui-markup/gui-inputs.tm>: input-field (Return), the fields of forms (form-input-text, form-text-area).

    <item><hlink|Layout|gui-markup/gui-layout.tm>: hlist, vlist, glue, tiled, align-tiled and the styles of text.

    <item><hlink|Tabs|gui-markup/gui-tabs.tm>: tabs, tabs-bar, tabs-body, active-tab and passive-tab.

    <item><hlink|Default theme|gui-markup/gui-theme-default.tm>: a small dialog with gui-button.

    <item><hlink|Bright theme|gui-markup/gui-theme-bright.tm>: the same dialog with gui-bright.

    <item><hlink|Dark theme|gui-markup/gui-theme-dark.tm>: the same dialog with gui-dark.

    <item><hlink|A virtual keyboard|gui-markup/gui-keyboard.tm>: keys which type at the cursor, and a modifier shown through broadcast messages.

    <item><hlink|The primitives|gui-markup/gui-primitives.tm>: dynamic-case (mouse-over, broadcast messages) and relay used directly.
  </itemize>

  In a document which can be edited, some elements react to a double
  click only (the tabs, which are links), and the cursor may enter the
  elements: a click outside of them leaves them.

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
