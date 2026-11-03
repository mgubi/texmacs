<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Introduction>

  In this chapter we describe how to interface <TeXmacs> with an extern
  application. Such interfaces should be distributed in the form of
  <hlink|plugins|../plugin/plugins.en.tm>. The plug-in may either
  contain the extern application, or provide the \Pglue\Q between <TeXmacs>
  and the application. Usually, interfaces are used interactively in shell
  sessions (see <menu|Insert|Session>). But they may also be designed for
  background tasks, such as spell checking or typesetting.

  The communication between <TeXmacs> and the application takes place using a
  customizable input format and the special <em|<TeXmacs> meta-format> for
  output from the plug-in. The meta-format enables you to send structured
  output to <TeXmacs>, using any common format like <verbatim|verbatim>,
  <LaTeX>, <name|PostScript>, <name|HTML>, image files, or <TeXmacs> itself. This is
  useful when adding a <TeXmacs> interface to an existing system, since
  <LaTeX> or <name|Postscript> output routines are often already implemented.
  It will then suffice to put the appropriate markers in order to make a
  first interface with <TeXmacs>.

  As soon as basic communication between your application and <TeXmacs> is
  working, you may improve the interface in many ways. Inside shell sessions,
  there is support for prompts, default inputs, tab-completion, mathematical
  and multi-line input, <abbr|etc.> In general, your application may take
  control of <TeXmacs> and modify the user interface (menus, keyboard,
  <abbr|etc.>) or add new <scheme> routines to <TeXmacs>. Your
  application may even extend the typesetter.

  In the directory <verbatim|$TEXMACS_PATH/examples/plugins>, you can find
  many examples of simple plug-ins. In the next sections, we will give a more
  detailed explanation of the interfacing features of <TeXmacs> on the hand
  of these examples. In order to try one of these examples, we recall that
  you just have to copy it to either one of the directories

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/plugins

    \ \ \ \ $TEXMACS_HOME_PATH/plugins
  </verbatim>

  and run the <verbatim|Makefile> (if there is one), before restarting
  <TeXmacs>. The examples with a <verbatim|Makefile> compile a small <c++>
  program into the <verbatim|bin> subdirectory of the plug-in; the examples
  whose names start with <verbatim|py> are written in <name|Python> and do
  not need to be compiled.

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