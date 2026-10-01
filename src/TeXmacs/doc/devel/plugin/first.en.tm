<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Writing your first interface with <TeXmacs>>

  In order to write your first interface to <TeXmacs>, we recommend you to
  follow the following steps:

  <\enumerate>
    <item>Create a <verbatim|--texmacs> option for your program which will
    be used for calling your program from inside <TeXmacs>.

    <item>Modify your output routines in a such a way that the appropriate
    output is sent to <TeXmacs> when your program is started with the
    <verbatim|--texmacs> option.

    <item>Make your program available under the name <verbatim|mycas> in
    your path (for instance using a small script which executes your
    program with the <verbatim|--texmacs> option), so that the
    <verbatim|mycas> plug-in, whose configuration launches
    <verbatim|mycas --texmacs>, will find it.
  </enumerate>

  After doing this, your program will be available under the name
  <menu|Mycas> in <menu|Insert|Session>. We will explain later how to make
  your system listed under its own name, how to customize it, and how to get
  the interface incorporated into the main <TeXmacs> distribution.

  Usually, step 2 is the most complicated one and the time it will cost you
  depends on how your system was designed. If you designed clean output
  routines (including the routines for displaying error messages), then it
  usually suffices to modify these by mimicking the <verbatim|mycas> example
  and reusing existing <LaTeX> output routines, which most systems provide.

  <LaTeX> is the most widely used transmission format for mathematical
  formulas, since most systems are able to produce it. Other possibilities
  are to send <TeXmacs> trees directly in the <verbatim|scheme> format, or
  to send mathematical expressions in prefix notation in the
  <verbatim|math> format (see the <hlink|internals of the plug-in
  system|plugin-internals.en.tm>). We recommend you to keep in mind the
  possibility of sending your output in tree format, which is semantically
  safer.

  Nevertheless, we enriched standard <LaTeX> with the <verbatim|\\*> and
  <verbatim|\\bignone> commands for multiplication and closing big
  operators. This allows us to distinguish between

  <\verbatim-code>
    a \\* (b + c)
  </verbatim-code>

  (or <math|a> multiplied by <math|b+c>) and

  <\verbatim-code>
    f(x + y)
  </verbatim-code>

  (or <math|f> applied to <math|x+y>). Similarly, in

  <\verbatim-code>
    \\sum_{i=1}^m a_i \\bignone + \\sum_{j=1}^n b_j \\bignone
  </verbatim-code>

  the <verbatim|\\bignone> command is used in order to specify the scopes of
  the <verbatim|\\sum> operators.

  It turns out that the systematic use of the <verbatim|\\*> and
  <verbatim|\\bignone> commands, in combination with clean <LaTeX> output
  for the remaining constructs, makes it <em|a priori> possible to associate
  an appropriate meaning to your output. In particular, this usually makes
  it possible to write additional routines for copying and pasting formulae
  between different systems.

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
