<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Important <TeXmacs> paths>

  Before writing your own style file, it is useful to know the following
  important <TeXmacs> paths (see also <verbatim|init_env_vars> in
  <verbatim|src/System/Boot/init_texmacs.cpp>). Each of them may be overridden
  by setting the corresponding environment variable before launching
  <TeXmacs>.

  <\itemize>
    <item><verbatim|$TEXMACS_PATH> is the main path for <TeXmacs>.

    <item><verbatim|$TEXMACS_HOME_PATH> is the main user path for <TeXmacs>
    files (documents, styles or programs). By default, this path is set to
    <verbatim|~/.TeXmacs>.

    <item><verbatim|$TEXMACS_STYLE_ROOT> the root directories for style
    files. By default, this path contains
    <verbatim|$TEXMACS_HOME_PATH/styles>, <verbatim|$TEXMACS_PATH/styles>
    and the <verbatim|styles> subdirectories of the installed plug-ins.

    <item><verbatim|$TEXMACS_PACKAGE_ROOT> the root directories for style
    packages. By default, this path contains
    <verbatim|$TEXMACS_HOME_PATH/packages>,
    <verbatim|$TEXMACS_PATH/packages> and the <verbatim|packages>
    subdirectories of the installed plug-ins.

    <item><verbatim|$TEXMACS_STYLE_PATH> contains the path for including
    style files and packages. By default, this path contains all
    subdirectories (recursively) of <verbatim|$TEXMACS_STYLE_ROOT> and
    <verbatim|$TEXMACS_PACKAGE_ROOT>. In particular, the subdirectory
    structure of these directories is irrelevant when referring to a style
    or package by its name.

    <item><verbatim|$TEXMACS_TEXT_ROOT> and <verbatim|$TEXMACS_TEXT_PATH>
    are similar paths for text files; by default the roots are
    <verbatim|$TEXMACS_HOME_PATH/texts>, <verbatim|$TEXMACS_PATH/texts> and
    the <verbatim|texts> subdirectories of the plug-ins.

    <item><verbatim|$TEXMACS_FILE_PATH> contains the path for searching
    files. By default, this path contains <verbatim|$TEXMACS_TEXT_PATH> and
    <verbatim|$TEXMACS_STYLE_PATH>.
  </itemize>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
