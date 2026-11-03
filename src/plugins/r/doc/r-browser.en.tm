<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<name|R> in a web browser>

  In the version of <TeXmacs> which runs in a web browser, the <name|R>
  plug-in does not need an <name|R> on your computer: <name|R> itself runs in
  the page. It is <name|webR> (<hlink|webr.r-wasm.org|https://webr.r-wasm.org>),
  <name|R> compiled to <name|WebAssembly> by the <name|R> project, free
  software under the MIT license (<name|R> under the GNU General Public
  License).

  <name|webR> is loaded from the network (<verbatim|webr.r-wasm.org>) by the
  first input of a session, which takes some seconds (about 20<nbsp>MB,
  kept by the browser afterwards). It runs apart from the page, so that
  <TeXmacs> stays responsive while <name|R> computes.

  <paragraph|Sessions and executable folds>

  A session is started with <menu|Insert|Session|R>, an executable fold with
  <menu|Insert|Fold|Executable|R>. Each input is run as at the prompt of
  <name|R>: its values are printed, its messages and errors shown, and the
  plots which it makes are shown as pictures after it. The variables of a
  session are kept from one input to the next.

  <paragraph|Packages>

  <verbatim|install.packages ("ggplot2")> installs a package from the
  packages built for <name|webR> (<hlink|repo.r-wasm.org|https://repo.r-wasm.org>,
  thousands of the packages of CRAN), then <verbatim|library (ggplot2)> loads
  it, as usual. A package is installed again in each new session.

  <paragraph|Limitations>

  <\itemize>
    <item>A web page cannot run other programs, nor read the files of your
    computer: <name|R> sees a file system of its own, which starts empty with
    each session.

    <item><name|R> cannot be interrupted while it computes: <menu|Stop> then
    ends it, its variables are lost, and it starts again with the next input.

    <item>Plots are pictures (bitmaps): they are not as sharp as vector
    graphics when they are enlarged.

    <item>Without a network, <name|R> cannot be loaded.
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
