<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<TeXmacs> style files>

  One of the fundamental strengths of <TeXmacs> is the possibility to write
  your own style files and packages. The purpose of style files is multiple:

  <\itemize>
    <item>They allow the abstraction of repetitive elements in texts, like
    sections, theorems, enumerations, etc.

    <item>They form a mechanism which allow you to structure your text. For
    instance, you may indicate that a given portion of your text is an
    abbreviation, a quotation or ``important''.

    <item>Standard document styles enable you to write professionally looking
    documents, because the corresponding style files have been written with a
    lot of care by people who know a lot about typography and aesthetics.
  </itemize>

  To a document, it is possible to associate one or several document styles,
  which are either standard or user defined. The main document style of a
  document is selected in the <menu|Document|Style> menu. Extra style
  packages can be added using <menu|Document|Style|Add package>.

  From the editor point of view, each style or package corresponds to a
  <verbatim|.ts> file (styles are searched in <verbatim|$TEXMACS_PATH/styles>
  and packages in <verbatim|$TEXMACS_PATH/packages>, as well as in the
  corresponding subdirectories of <verbatim|~/.TeXmacs>). The files corresponding to each style are processed in as if they
  were usual documents, but at the end, the editor only keeps the final
  environment as the initial environment for the main document. More
  precisely, the style files are processed in order as well as their own
  styles, in a recursive manner.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
