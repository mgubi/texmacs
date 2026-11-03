<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Sending commands to <TeXmacs>>

  The application may use <verbatim|command> as a very particular output
  format in order to send <scheme> commands to <TeXmacs>. In other words, the
  block

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN>command:<em|cmd><render-key|DATA_END>>>
  </quotation>

  will send the command <verbatim|<em|cmd>> to <TeXmacs>. Such commands are
  executed immediately after reception of <render-key|DATA_END>. We also
  recall that such command blocks may be incorporated recursively in larger
  <render-key|DATA_BEGIN>-<render-key|DATA_END> blocks.

  <paragraph*|The <verbatim|menus> plug-in>

  The <verbatim|menus> plug-in shows how an application can modify the
  <TeXmacs> menus in an interactive way. The plug-in consists of the files

  <\verbatim>
    \ \ \ \ <example-plugin-link|menus/Makefile>

    \ \ \ \ <example-plugin-link|menus/progs/init-menus.scm>

    \ \ \ \ <example-plugin-link|menus/src/menus.cpp>
  </verbatim>

  The body of the main loop of <source-link|menus.cpp|TeXmacs/examples/plugins/menus/src/menus.cpp> simply contains

  <\cpp-code>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "command:(menus-add
    \\""

    \ \ \ \ \ \<less\>\<less\> buffer \<less\>\<less\> "\\")"
    \<less\>\<less\> DATA_END;

    cout \<less\>\<less\> "Added " \<less\>\<less\> buffer \<less\>\<less\> "
    to menu";

    cout \<less\>\<less\> DATA_END;

    cout.flush ();
  </cpp-code>

  The <scheme> function <scm|menus-add> is defined in
  <source-link|init-menus.scm|TeXmacs/examples/plugins/menus/progs/init-menus.scm>, which contains

  <\scm-code>
    (plugin-configure menus

    \ \ (:require (url-exists-in-path? "menus.bin"))

    \ \ (:launch "menus.bin")

    \ \ (:session "Menus"))

    \;

    (when (supports-menus?)

    \ \ (define menu-items '("Hi"))

    \;

    \ \ (tm-menu (menus-menu)

    \ \ \ \ (for (entry menu-items)

    \ \ \ \ \ \ ((eval entry) (insert entry))))

    \;

    \ \ (tm-define (menus-add entry)

    \ \ \ \ (set! menu-items (cons entry menu-items)))

    \;

    \ \ (menu-bind plugin-menu

    \ \ \ \ (:require (in-menus?))

    \ \ \ \ (=\<gtr\> "Menus" (link menus-menu))))
  </scm-code>

  The configuration of <verbatim|menus> proceeds as usual. The additional
  code is only executed if the plug-in is operational. It defines a
  dynamic menu <scm|menus-menu>, whose entries are computed from the list
  <scm|menu-items> each time the menu is displayed, and attaches it to the
  main menu bar through the <scm|plugin-menu> hook, which is reserved for
  plug-ins. The predicate <scm|in-menus?> ensures that the menu is only
  visible inside <verbatim|menus> sessions.

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