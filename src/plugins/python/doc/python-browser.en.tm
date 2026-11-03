<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<name|Python> in a web browser>

  In the version of <TeXmacs> which runs in a web browser, the <name|Python>
  plug-in does not need a <name|Python> on your computer: <name|Python>
  itself runs in the page. It is <name|Pyodide>
  (<hlink|pyodide.org|https://pyodide.org>), <name|CPython> 3.14 compiled to
  <name|WebAssembly>, free software under the Mozilla Public License 2.0.

  <name|Pyodide> is loaded from the network (the CDN of <name|jsDelivr>) by
  the first input of a session, which takes a few seconds (about
  10<nbsp>MB, kept by the browser afterwards). It runs apart from the page,
  so that <TeXmacs> stays responsive while <name|Python> computes.

  <paragraph|Sessions and executable folds>

  A session is started with <menu|Insert|Session|Python>, an executable
  fold with <menu|Insert|Fold|Executable|Python>. Each input is run as in
  the interactive <name|Python>:

  <\itemize>
    <item>what it prints is shown, and the value of its last expression (not
    when the input ends with <verbatim|;>, as in <name|Jupyter>);

    <item>the results of <name|SymPy> are shown as formulas;

    <item>the figures of <name|matplotlib> which an input makes are shown
    as pictures after it (<verbatim|plt.show ()> is not needed);

    <item>an error is shown with its traceback.
  </itemize>

  The variables of a session are kept from one input to the next.

  <paragraph|Packages>

  The packages which an input imports are loaded with it, from the same CDN:
  <verbatim|numpy>, <verbatim|sympy>, <verbatim|matplotlib>,
  <verbatim|pandas>, <verbatim|scipy> and the other packages of
  <name|Pyodide> (more than three hundred). The first import of a large
  package takes a few seconds. Other pure <name|Python> packages can be
  installed with <verbatim|micropip>:

  <\verbatim-code>
    import micropip

    await micropip.install ("some-package")
  </verbatim-code>

  <paragraph|Limitations>

  <\itemize>
    <item>A web page cannot run other programs, nor read the files of your
    computer: <name|Python> sees a file system of its own, which starts empty
    with each session.

    <item><name|Python> cannot be interrupted while it computes:
    <menu|Stop> then ends it, its variables are lost, and it starts again
    with the next input.

    <item>Without a network, <name|Python> cannot be loaded.
  </itemize>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software
  Foundation; with no Invariant Sections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of the license is included in the
  section entitled "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>
