<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Further customization of the interface>

  Having written a working interface between your system and <TeXmacs>, you
  may want to improve it further. Below we will discuss a few directions for
  possible improvement.

  First of all, you may want to customize the keyboard behavior inside a
  <verbatim|myplugin>-session and add appropriate menus. The procedure for
  doing that is described in the chapter about the <scheme> extension
  language and you may add such support to the file
  <verbatim|init-myplugin.scm> (or, better, to separate modules which are
  loaded from this file). The predicate <scm|in-myplugin?>, which is
  automatically defined by <scm|plugin-configure>, can be used in order to
  restrict keyboard shortcuts and menus to sessions of your plug-in. We
  again recommend you to take a look at the plug-ins which are shipped with
  <TeXmacs> inside the directory <verbatim|$TEXMACS_PATH/plugins>.

  Certain output from your system might require special markup. For
  instance, assume that you want to associate an invisible type to each
  subexpression in the output. Then you may create a macro
  <verbatim|exprtype> with two arguments in a style package
  <verbatim|myplugin.ts> and send <LaTeX> expressions like
  <verbatim|\\exprtype{1}{Integer}> to <TeXmacs> during the output. If the
  package <verbatim|myplugin.ts> can be found in the style path (for
  instance in <verbatim|myplugin/packages/session/myplugin.ts>), then it is
  automatically added to the document when a <verbatim|myplugin>-session is
  inserted.

  In case you connected your system to <TeXmacs> using pipes, you may
  directly execute <TeXmacs> commands during the output from your system by
  incorporating pieces of code of the form

  <\verbatim-code>
    [DATA_BEGIN]command:scheme-program[DATA_END]
  </verbatim-code>

  in your output. Inversely, a <scheme> program may send input to your
  system and retrieve the result using <scm|(plugin-eval "myplugin"
  "default" <scm-arg|input>)>, which is synchronous, or using the
  asynchronous routine <scm|silent-feed>. The command <verbatim|extern-exec>
  which was used for this purpose in older versions of <TeXmacs> no longer
  exists. See the <hlink|internals of the plug-in
  system|plugin-internals.en.tm> for more details.

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
