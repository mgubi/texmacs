<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Editing source files>

  <TeXmacs> opens the source files of programs (<verbatim|.cpp>,
  <verbatim|.py>, <verbatim|.scm>, <verbatim|.java>...) as programs: one
  line of the document for each line of the file, in the font of programs,
  coloured by their language. A source file is saved as it was opened: only
  the lines which you have edited change in the file, which keeps the
  differences of a version control system such as <name|Git> to the lines
  which have changed.

  <paragraph*|Tabs>

  The tabs of a file stay tabs. A tab goes to the next tab stop, and is shown
  by a light arrow at the start of its gap, so that you can see whether a
  line is indented by tabs or by spaces. The cursor moves over a tab at once,
  as over a character, and <key|backspace> or <key|delete> removes it.

  The tab stops are every 8 columns, or as many as set in
  <menu|Edit|Preferences|Convert|Verbatim> (<menu|Columns between tab
  stops>, the preference <verbatim|editor:code tab width>), when the file is
  opened. A column is a character: the font of programs gives the same width
  to all of them.

  <paragraph*|Indentation>

  When you start a new line (<key|return>), or re-indent a line, <TeXmacs>
  computes the column where the line starts, as the language wants it (for
  instance one more level after a line of <name|Python> which ends by a
  colon), and writes the indentation as the file is indented: by tabs (and
  spaces for the columns which are left) in a file whose indented lines
  mostly start by a tab, else by spaces. One level of indentation is then a
  tab. A file which has no indented lines yet is indented as set in
  <menu|Indent new files with> (the preference <verbatim|editor:code
  indent>), except for the languages which want tabs (<name|Go> and
  makefiles). The other lines are left as they are.

  Code which you paste in a program keeps its tabs too.

  <paragraph*|Line ends and encodings>

  A file keeps its line ends: those of <name|Unix> (LF), of <name|Windows>
  (CR LF) or of old <name|Mac> systems (CR). A file whose line ends are mixed
  keeps them too: its line ends are LF, and the CR which precede some of them
  stay in their lines. Whether a file ends by a line end is kept as well.

  A file is read as <name|UTF-8>; one which is not valid <name|UTF-8> is
  read as ISO-8859-1 (Latin-1), and it is saved in ISO-8859-1. The other
  control characters of a file, such as form feeds, are kept as they are.

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
