<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|An introduction to widgets>

  In <TeXmacs> you create and extend the visual interface using <em|widgets>.
  This word means either the basic building blocks you have at your disposal,
  like buttons, popup lists, etc. or the collections of those into dialogs,
  menu bars or whatever. This rather loose concept might be confusing,
  especially when we refer to what usually are know as dialogs as widgets,
  but it makes sense because all sorts of widgets can be aggregated to build
  more complicated ones as well.<\footnote>
    If you miss some particular \Pbuilding block\Q from your OS, you might
    see whether it's feasible as an aggregation of simpler ones or try and
    play with the UI interface code in C++ (but you'll have to add it for
    every supported platform!).
  </footnote>

  However, it must be kept in mind that items intended to be inserted in a
  menu bar won't necessarily display as they do in a separate window:
  complicated aggregations of widgets might be better placed in a separate
  window or dialogue, as explained in \P<hlink|Dialogs and composite
  widgets|scheme-gui-dialogs.en.tm>\Q.

  A complete reference with all the available widgets is the \P<hlink|Widgets
  reference guide|scheme-gui-reference.en.tm>\Q, and you can find some
  examples in the other subsections of \P<hlink|Extending the graphical user
  interface|scheme-gui.en.tm>\Q. If you'd rather see the sources, the whole
  list of keywords is in the table <scm|gui-make-table> inside
  <hlink|<source-link|menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm>|$TEXMACS_PATH/progs/kernel/gui/menu-define.scm>.
  How the widget language is implemented is explained in \P<hlink|The
  <scheme> widget language and its
  interpreter|../../source/widgets-scheme.en.tm>\Q.

  To create a widget, you'll first need to use <scm|tm-widget> to define a
  new one. This macro works like <scm|tm-define>, but its body is written
  in a special language, with many keywords for the creation of widgets. But we'll start with some buttons and
  labels.\ 

  Execute the following two lines to get the unavoidable example and click
  on the \PHello\Q button.

  <\session|scheme|default>
    <\input|Scheme] >
      (tm-widget (example1) ("Hello" (set-message "world!" "Hello")))
    </input>

    <\input|Scheme] >
      (top-window example1 "A first try")
    </input>
  </session>

  As you can see, buttons are implicitly created by simply writing a list
  with the button's title, followed by the <scheme> code to be executed when
  the button is pressed (here, a message is displayed in the footer of the
  main window). This is exactly the syntax of menu entries, and the button
  is rendered as a flat <with|font-shape|italic|toolbar> button. What you
  probably want is this:

  <\session|scheme|default>
    <\input|Scheme] >
      (tm-widget (example2) (explicit-buttons ("Hello" (noop))))
    </input>

    <\input|Scheme] >
      (top-window example2 "A nicer button")
    </input>
  </session>

  The command is now a no-operation, or <scm|(noop)>. Try
  changing it for <scm|(display "World")> or anything that suits you.

  The next step is to add some text next to the button, i.e. a label. This is
  done with the <scm|text> keyword, as in <scm|(text "Hello")>, but in order
  to have both widgets sit side by side, you'll need a container widget as
  described in "<hlink|Containers, glue, refresh and
  co.|scheme-gui-advanced.en.tm>", such as <scm|hlist>:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (example3)

      \ \ (hlist\ 

      \ \ \ \ (text "Hello")\ 

      \ \ \ \ (explicit-buttons ("world" (display "!\\n")))))
    <|unfolded-io>
      \;
    </unfolded-io>

    <\input|Scheme] >
      (top-window example3 "Some text")
    </input>
  </session>

  That was nice, but as you see, the two widgets are packed together until
  you resize the window. We need to explicitly tell <TeXmacs> to insert some
  space between them:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (example3)

      \ \ (hlist\ 

      \ \ \ \ (text "Hello")

      \ \ \ \ \<gtr\>\<gtr\>\<gtr\>

      \ \ \ \ (explicit-buttons ("world" (display "!\\n")))))
    <|unfolded-io>
      \;
    </unfolded-io>

    <\input|Scheme] >
      (top-window example3 "Some text")
    </input>
  </session>

  The special symbol <scm|\<gtr\>\<gtr\>\<gtr\>> is just one of the
  predefined glue widgets described in "<hlink|Containers, glue, refresh and
  co.|scheme-gui-advanced.en.tm>".

  Text attributes may be changed for <scm|text> widgets and many others by
  enclosing them inside what we'll name <with|font-shape|italic|style
  widgets>. These attributes are <scm|mono>, <scm|grey>, <scm|inert>,
  <scm|bold>, <scm|verb> and <scm|explicit-buttons>, and respectively:
  choose a <tt|monospaced font>, set the color to <with|color|grey|grey>,
  deactivate the widget (meaning it is rendered, but greyed out and
  inactive), choose a bold face, do not translate the labels and render
  entries as explicit buttons. (The layout helper <scm|centered> is not a
  style, but it can be used in a similar way.) Here is an example:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (example3)

      \ \ (hlist\ 

      \ \ \ \ (bold (text "Hello"))

      \ \ \ \ \<gtr\>\<gtr\>\<gtr\>

      \ \ \ \ (inert (explicit-buttons ("world" (display "!\\n"))))))
    <|unfolded-io>
      \;
    </unfolded-io>

    <\input|Scheme] >
      (top-window example3 "Some text")
    </input>
  </session>

  From here you can go on reading \P<hlink|Extending the graphical user
  interface|scheme-gui.en.tm>\Q or see the sample widgets in
  <hlink|<source-link|menu-test.scm|TeXmacs/progs/kernel/gui/menu-test.scm>|$TEXMACS_PATH/progs/kernel/gui/menu-test.scm>.
  This file also defines the helper <scm|(show <scm-arg|widget>)>, which is
  short for <scm|(top-window <scm-arg|widget> "Simple widget")> and is used
  in the examples of the following sections.

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