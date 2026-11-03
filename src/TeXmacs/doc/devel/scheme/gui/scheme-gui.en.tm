<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Extending the graphical user interface>

  Most of the user interface to <TeXmacs> is dynamically created from within
  the interpreted <scheme> code. New menus and buttons can be added, or the
  existing ones reused and rearranged, even the main editor can be embedded
  anywhere.

  Imagine you want to implement some feature which requires interaction with
  the user. One possible approach is to use the facility <scm|interactive>,
  which according to the user's preferences will either popup a dialog or ask
  in the footer bar, based in metadata you provide inside your
  <scm|tm-define>'d function. See \P<hlink|Meta information and logical
  programming|../overview/overview-meta.en.tm>\Q for more on this topic.
  However, automatically generated content is not always the best approach,
  so you might want to explicitly design your interface placing it inside a
  complicated dialog. The following sections should help you with this.
  The implementation of the widget language is described in \P<hlink|The
  <scheme> widget language and its
  interpreter|../../source/widgets-scheme.en.tm>\Q.

  The same widgets are drawn by each graphical port of <TeXmacs>:
  <name|Qt>, <name|Cocoa> (the native port for <name|macOS>), <name|Vue>
  (drawn with <name|SDL3>) and the older
  <name|X11>/<name|Widkit> port; the differences between the ports are
  noted where they matter, and the ports themselves are described in
  \P<hlink|Adding new widgets and porting to other toolkits|../../source/widgets-port.en.tm>\Q. When
  <scm|(has-markup-gui?)> holds, the dialogs are instead rendered as
  <TeXmacs> documents, see \P<hlink|The graphical user interface through
  markup|../../source/gui-markup.en.tm>\Q. Keyboard shortcuts are defined
  as explained in \P<hlink|Keyboard
  bindings|../utils/utils-keyboard.en.tm>\Q.

  <\traverse>
    <branch|An introduction to widgets.|scheme-gui-intro.en.tm>

    <branch|Menus and toolbars.|scheme-gui-menus.en.tm>

    <branch|Displaying lists and trees.|scheme-gui-lists-trees.en.tm>

    <branch|Dialogs and composite widgets.|scheme-gui-dialogs.en.tm>

    <branch|Forms.|scheme-gui-forms.en.tm>

    <branch|Containers, glue and refresh widgets and other advanced
    topics.|scheme-gui-advanced.en.tm>

    <branch|Complete reference guide of all available
    widgets.|scheme-gui-reference.en.tm>
  </traverse>

  \;

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>