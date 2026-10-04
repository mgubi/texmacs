<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Example of a plug-in with <scheme> code>

  <paragraph*|The <verbatim|world> plug-in>

  Consider the <verbatim|world> plug-in in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  This plug-in shows how to extend <TeXmacs> with some additional <scheme>
  code in the file

  <\verbatim>
    \ \ \ \ <example-plugin-link|world/progs/init-world.scm>
  </verbatim>

  In order to test the <verbatim|world> plug-in, you should recursively copy
  the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/world
  </verbatim>

  to <verbatim|$TEXMACS_PATH/plugins> or <verbatim|$TEXMACS_HOME_PATH/plugins>.
  When relaunching <TeXmacs>, the plug-in should now be automatically
  recognized: shortly after startup, the message <verbatim|Using world
  plug-in!> is printed in the terminal from which <TeXmacs> was
  launched.

  <paragraph*|How it works>

  The file <verbatim|init-world.scm> essentially contains the following code:

  <\scm-code>
    (plugin-configure world

    \ \ (:require #t))

    \;

    (when (supports-world?)

    \ \ (display* "Using world plug-in!\\n"))
  </scm-code>

  The configuration option <scm|:require> specifies a condition which needs
  to be satisfied for the plug-in to be detected by <TeXmacs> (later on, this
  will for instance allow us to check whether certain programs exist on the
  system). The configuration is aborted if the requirement is not fulfilled.

  Assuming that the configuration succeeds, the <verbatim|supports-world?>
  predicate will evaluate to <verbatim|#t>. In our example, the body of the
  <scm|when> statement corresponds to some further initialization code, which
  just sends a message to the standard output that we are using our plug-in.
  In general, this kind of initialization code should be very short and
  rather load a module which takes care of the real initialization. Indeed,
  keeping the <verbatim|init-<em|myplugin>.scm> files simple will reduce the
  startup time of<nbsp><TeXmacs>. Notice also that initialization files are
  loaded <em|lazily>: <TeXmacs> only loads them once it has been idle for
  about one second, or as soon as it needs information about all
  plug-ins (for instance when opening the <menu|Insert|Session> menu).

  A plug-in with more <scheme> code usually puts it into separate modules
  in its <verbatim|progs> directory, which is automatically added to the
  load path. For instance, a module <verbatim|progs/world-menus.scm>
  starting with <scm|(texmacs-module (world-menus))> can be loaded from
  the initialization file using <scm|(import-from (world-menus))>, or
  lazily using <scm|lazy-menu>, <scm|lazy-keyboard> or <scm|lazy-define>.
  In order to add a menu to the main menu bar, such a module may extend
  the menu <scm|plugin-menu>, as in

  <\scm-code>
    (menu-bind plugin-menu

    \ \ (:require (in-world?))

    \ \ (=\<gtr\> "World" ("Hello" (insert "Hello world"))))
  </scm-code>

  where <scm|in-world?> is the predicate which is automatically defined by
  <scm|plugin-configure> (see the <hlink|summary of configuration
  options|plugin-config.en.tm>). Since the <verbatim|world> plug-in does
  not provide sessions, this predicate would only hold inside documents
  whose programming language has been set to <verbatim|world>; a menu
  which should always be present may use <scm|(:require
  (supports-world?))> instead.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
