<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Writing your own plug-ins>

  In order to write a plug-in <verbatim|<em|myplugin>>, you should start by
  creating a directory

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>
  </verbatim>

  where to put all your files (recall that <verbatim|$TEXMACS_HOME_PATH>
  defaults to <verbatim|$HOME/.TeXmacs>). In addition, you may create the
  following subdirectories (when needed):

  <\description-dash>
    <item*|<verbatim|bin>>For binary files. This directory is appended to
    the <verbatim|PATH>.

    <item*|<verbatim|doc>>For documentation. This directory is added to
    <verbatim|TEXMACS_DOC_PATH>.

    <item*|<verbatim|langs/natural/dic>>For dictionaries. This directory is
    added to <verbatim|TEXMACS_DIC_PATH>.

    <item*|<verbatim|lib>>For shared libraries. This directory is added to
    <verbatim|LD_LIBRARY_PATH>.

    <item*|<verbatim|misc/patterns>, <verbatim|misc/pixmaps>,
    <verbatim|misc/themes>>For background patterns, icons and themes.

    <item*|<verbatim|packages>>For style packages.

    <item*|<verbatim|progs>>For <scheme> programs. This directory is added
    to the load path of <scheme> modules.

    <item*|<verbatim|src>>For source files (this directory is not used by
    <TeXmacs> itself).

    <item*|<verbatim|styles>>For style files.

    <item*|<verbatim|texts>>For text files.
  </description-dash>

  As a general rule, files which are present in these subdirectories will be
  automatically recognized by <TeXmacs> at startup. For instance, if you
  provide a <verbatim|bin> subdirectory, then

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/bin
  </verbatim>

  will be automatically added to the <verbatim|PATH> environment variable at
  startup. Notice that the subdirectory structure of a plug-in is very
  similar to the subdirectory structure of <verbatim|$TEXMACS_PATH>. Since
  these search paths are computed only once at startup, <TeXmacs> has to be
  restarted after the creation of a new plug-in or of a new subdirectory
  of a plug-in.

  Similarly, plugin documentation is intended to be automatically added to
  the <menu|Help|Plug-ins> submenu. For this automation to work, the
  <verbatim|myplugin/doc/> directory should contain at least two files

  <\verbatim>
    \ \ \ \ myplugin.en.tm

    \ \ \ \ myplugin-abstract.en.tm
  </verbatim>

  The first file is the main entry point to the plugin's documentation and
  should follow <hlink|the general conventions for structuring <TeXmacs>
  documentation|../../about/contribute/documentation/traversal.en.tm>. The
  <verbatim|-abstract> file provides a short description of the plugin's
  functionality.

  <\example>
    The easiest type of plug-in only consists of data files, such as a
    collection of style files and packages. In order to create such a
    plug-in, it suffices to create directories

    <\verbatim>
      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/styles

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/packages
    </verbatim>

    and to put your style files and packages in the last two directories.
    After restarting <TeXmacs>, your style files and packages will
    automatically appear in the <menu|Document|Style> and <menu|Document|Use
    package> menus.
  </example>

  For more complex plug-ins, such as plug-ins with additional <scheme> or
  <c++> code, one usually has to provide a <scheme> configuration file

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/progs/init-<em|myplugin>.scm
  </verbatim>

  The name of this file has to be exactly <verbatim|init-<em|myplugin>.scm>,
  where <verbatim|<em|myplugin>> is the name of the plug-in directory. The
  file is not loaded immediately at startup, but about one second later,
  when <TeXmacs> becomes idle (or earlier, when information about all
  plug-ins is needed); see the section on <hlink|internals|plugin-internals.en.tm>.
  This configuration file should contain an instruction of the following form

  <\scm-code>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>)
  </scm-code>

  Here the <verbatim|<em|configuration-options>> describe the principal
  actions which have to be undertaken at startup, including sanity checks for
  the plug-in. In the next sections, we will describe some simple examples of
  plug-ins and their configuration. Many other examples can be found in the
  directories

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins

    \ \ \ \ $TEXMACS_PATH/plugins
  </verbatim>

  In the source code of <TeXmacs>, the second directory corresponds to
  <verbatim|src/plugins>. Some of these plug-ins are
  <hlink|described|../interface/interface.en.tm> in more detail in the
  chapter about writing new interfaces.

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

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