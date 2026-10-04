<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <TeXmacs> document format>

  <TeXmacs> documents are trees. This part describes how these trees are
  built and stored, and which built-in constructs they may contain:

  <\itemize>
    <item>the general structure of documents, their serialization to files
    (the <verbatim|.tm>, <name|XML> and <scheme> formats), an overview of
    the typesetting process, data relation descriptors, and length units;

    <item>the environment variables which control the typesetting
    (fonts, paragraphs, pages, mathematics, tables, graphics, ...);

    <item>the built-in primitives which produce typeset material;

    <item>the primitives of the style-sheet language, in which macros and
    style files are written.
  </itemize>

  The same chapters are also part of the <hlink|reference
  guide|../../main/man-reference.en.tm>. How the kernel implements them is
  explained in <hlink|about the source code of
  <TeXmacs>|../source/source.en.tm>.

  <\traverse>
    <branch|The <TeXmacs> format|basics/basics.en.tm>

    <branch|Built-in environment variables|environment/environment.en.tm>

    <branch|Built-in <TeXmacs> primitives|regular/regular.en.tm>

    <branch|Primitives for writing style files|stylesheet/stylesheet.en.tm>
  </traverse>

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
