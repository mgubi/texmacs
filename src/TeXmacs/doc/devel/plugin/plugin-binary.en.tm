<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Example of a plug-in with <name|C++> code>

  <paragraph*|The <verbatim|minimal> plug-in>

  Consider the example of the <verbatim|minimal> plug-in in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  It consists of the following files:

  <\verbatim>
    \ \ \ \ <example-plugin-link|minimal/Makefile>

    \ \ \ \ <example-plugin-link|minimal/progs/init-minimal.scm>

    \ \ \ \ <example-plugin-link|minimal/src/minimal.cpp>
  </verbatim>

  In order to try the plug-in, you first have to recursively copy the
  directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/minimal
  </verbatim>

  to <verbatim|$TEXMACS_PATH/plugins> or <verbatim|$TEXMACS_HOME_PATH/plugins>.
  Next, running the <verbatim|Makefile> in the copied directory using

  <\verbatim>
    \ \ \ \ make
  </verbatim>

  will compile the program <source-link|minimal.cpp|TeXmacs/examples/plugins/minimal/src/minimal.cpp> and create a binary

  <\verbatim>
    \ \ \ \ minimal/bin/minimal.bin
  </verbatim>

  The <verbatim|Makefile> simply runs <verbatim|g++> on each file in
  <verbatim|src> and expects the directory <verbatim|bin> to exist (create
  it using <verbatim|mkdir bin> if necessary). When relaunching <TeXmacs>,
  the plug-in should now be automatically recognized. Notice that
  <TeXmacs> has to be restarted after the compilation: the
  <verbatim|bin> directories of plug-ins are only added to the
  <verbatim|PATH> at startup, and the result of the <scm|:require> test is
  cached (use <menu|Tools|Update|Plugins> if the plug-in is still not
  recognized).

  <paragraph*|How it works>

  The <verbatim|minimal> plug-in demonstrates a minimal interface between
  <TeXmacs> and an extern program; the program <source-link|minimal.cpp|TeXmacs/examples/plugins/minimal/src/minimal.cpp> is
  <hlink|explained|../interface/interface-pipes.en.tm> in more detail in
  the chapter about writing interfaces. The initialization file
  <source-link|init-minimal.scm|TeXmacs/examples/plugins/minimal/progs/init-minimal.scm> essentially contains the following code:

  <\scm-code>
    (plugin-configure minimal

    \ \ (:require (url-exists-in-path? "minimal.bin"))

    \ \ (:launch "minimal.bin")

    \ \ (:session "Minimal"))
  </scm-code>

  The <scm|:require> option checks whether <verbatim|minimal.bin>
  indeed exists in the path (so this will fail if you forgot to run the
  <verbatim|Makefile>). The <scm|:launch> option specifies how to
  launch the extern program. The <scm|:session> option indicates that it
  will be possible to create sessions for the <verbatim|minimal> plug-in
  using <menu|Insert|Session|Minimal>.

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
