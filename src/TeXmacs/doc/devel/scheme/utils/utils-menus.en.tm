<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Defining menus>

  Menus, toolbars and widgets are defined using the macros below, which are
  implemented in <verbatim|kernel/gui/menu-define.scm>. The syntax of the
  menu entries and the available widgets are described in detail in the
  chapter on <hlink|widgets in <scheme>|../gui/scheme-gui.en.tm>, in
  particular in the sections <hlink|menus and
  toolbars|../gui/scheme-gui-menus.en.tm> and <hlink|widgets reference
  guide|../gui/scheme-gui-reference.en.tm>. See also the user manual section
  on <hlink|creating your own dynamic
  menus|../../../main/scheme/man-menus.en.tm>.

  <\explain>
    <scm|(menu-bind <scm-arg|name> <scm-arg|option> ... <scm-arg|entry>
    ...)>

    <scm|(tm-menu (<scm-arg|name> <scm-arg|arg> ...) <scm-arg|option> ...
    <scm-arg|entry> ...)><explain-synopsis|define a menu>
  <|explain>
    Define (or redefine) the menu <scm-arg|name>. Both macros expand into
    a <scm|tm-define>, so that the options for <hlink|contextual
    overloading|utils-overload.en.tm> (such as <scm|:mode> and
    <scm|:require>) may be used, and the entry <scm|(former)> includes the
    previous definition of the menu. The macro <scm|tm-menu> in addition
    allows for menus with arguments. For instance:

    <\scm-code>
      (tm-menu (tools-menu)

      \ \ (former)

      \ \ ---

      \ \ ("Say hello" (set-message "Hello" "tools menu")))
    </scm-code>
  </explain>

  <\explain>
    <scm|(tm-widget (<scm-arg|name> <scm-arg|arg> ...) <scm-arg|entry>
    ...)><explain-synopsis|define a widget>
  <|explain>
    Similar to <scm|tm-menu>, but for widgets which are to be shown in
    dialogue windows or side panes (see <hlink|dialogs and composite
    widgets|../gui/scheme-gui-dialogs.en.tm>).
  </explain>

  <\explain>
    <scm|(lazy-menu <scm-arg|module> <scm-arg|name> ...)><explain-synopsis|lazy
    loading of menus>
  <|explain>
    Declare that the menus <scm-arg|name> ... are defined in
    <scm-arg|module>, which is only loaded when one of these menus is needed
    (or after some idle time). This mechanism is used extensively in
    <verbatim|init-texmacs.scm> in order to speed up the boot process.
  </explain>

  The older macro <scm|menu-extend> is deprecated; use <scm|tm-menu> or
  <scm|menu-bind> together with <scm|(former)> instead.

  <tmdoc-copyright|2005--2026|Joris van der Hoeven, the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
