<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Reusing your macros in other documents>

  The macros of the preamble only exist in their document. If you want to
  use them in other documents too, put them in a <em|style package>: a file
  with the suffix <verbatim|.ts> which contains macro definitions, and which
  you can add to any document.

  <paragraph*|Creating the package>

  Open the document with your macros and choose
  <menu|Tools|Macros|Extract style package>. <TeXmacs> opens a new document
  in the <tmstyle|source> style, which contains a header (with the package
  name <verbatim|love-macros>, which you may change into <verbatim|love>),
  the style parameters of your document if any, and all the definitions of
  the preamble:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>

    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  Save this document in the <verbatim|packages> folder of your personal
  <TeXmacs> folder, under the name <verbatim|love.ts>. This personal folder
  is <verbatim|~/.TeXmacs> on <name|Linux> and <name|macOS> (that is, the
  folder <verbatim|.TeXmacs> in your home folder), and
  <verbatim|%APPDATA%\\TeXmacs> on <name|Windows>; <TeXmacs> creates the
  folder <verbatim|packages> inside it at startup. On <name|macOS>, the file
  chooser hides folders whose name starts with a dot: press
  <verbatim|Cmd-Shift-.> in order to show <verbatim|.TeXmacs>. The example
  package of this tutorial is <verbatim|examples/love.ts>.

  <paragraph*|Using the package>

  In any document, you may now choose
  <menu|Document|Style|Add package|Add other package> and type
  <verbatim|love>: the macros <markup|hi> and <markup|love> become
  available, exactly as if they had been defined in the preamble of the
  document. If you change the package later, all the documents which use it
  take the change into account. A package can be removed from a document in
  the <menu|Document|Style> menu.

  You may also create a style package from scratch, by choosing
  <menu|Document|Style|source> for a new document, typing the definitions,
  and saving it with the suffix <verbatim|.ts>. Writing style files is
  explained in detail in <menu|Help|Manual|Writing your own style files>.

  <tmdoc-copyright|1998\U2026|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
