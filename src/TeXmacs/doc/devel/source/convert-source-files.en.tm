<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Source files>

  The formats of source code (<verbatim|cpp>, <verbatim|python>,
  <verbatim|scheme>, <verbatim|java>... in
  <source-link|code-format.scm|plugins/code/progs/code-format.scm>,
  <source-link|python-format.scm|plugins/python/progs/python-format.scm>,
  <source-link|prog-format.scm|TeXmacs/progs/prog/prog-format.scm>) load a
  file with <scm|code-\<gtr\>texmacs> and save it with
  <scm|texmacs-\<gtr\>verbatim> and the encoding <verbatim|SourceCode>
  (<source-link|init-rewrite.scm|TeXmacs/progs/convert/rewrite/init-rewrite.scm>).
  A file is saved as it was loaded, byte for byte, apart from the lines
  which were edited (suite <verbatim|source-files>,
  <source-link|source-files-test.scm|TeXmacs/progs/check/source-files-test.scm>).

  <paragraph*|Loading>

  <scm|code-\<gtr\>texmacs> calls <cpp|source_document_to_tree>
  (<scm|cpp-source-\<gtr\>texmacs>,
  <source-link|verbatim.cpp|src/Data/Convert/Verbatim/verbatim.cpp>), which
  differs from <cpp|verbatim_document_to_tree> in what it keeps:

  <\itemize>
    <item>The tabs and the other control characters (below 32, and 127)
    are the characters <verbatim|\<less\>#9\<gtr\>>,
    <verbatim|\<less\>#C\<gtr\>>... of Cork: a byte 9 of Cork is a macron,
    so that a tab left as it is was shown as a macron and saved as U+00AF,
    and the form feeds were saved as other characters. They are not
    expanded (<cpp|un_special> expands the tabs to spaces for the other
    verbatim imports), and a backspace is not applied.

    <item>A file which is not valid <name|UTF-8> is read as ISO-8859-1.

    <item>The line ends are found: LF, CR LF or CR when all the lines end so;
    when they are mixed, the lines end by LF and a CR left in a line is the
    character <verbatim|\<less\>#D\<gtr\>>.

    <item>The indentation of the lines is found: by tabs when the lines
    which start by a tab are more than those which start by two spaces, by
    spaces when there are some of those, else none.
  </itemize>

  The encoding, the line ends and the indentation are in the initial
  environment of the document: <verbatim|source-encoding>
  (<verbatim|utf-8>, <verbatim|iso-8859-1>), <verbatim|source-line-ends>
  (<verbatim|unix>, <verbatim|dos>, <verbatim|mac>) and
  <verbatim|source-indent> (<verbatim|tabs>, <verbatim|spaces> or empty).
  <cpp|attach_subformat>
  (<source-link|new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>) sets
  <verbatim|prog-tab-width> there too, from the preference
  <verbatim|editor:code tab width> (8 by default).

  Code which is pasted in a program is converted by
  <cpp|source_snippet_to_tree> (<scm|code-snippet-\<gtr\>texmacs>), which
  keeps its tabs and its control characters in the same way; its line ends
  are LF.

  <paragraph*|Saving>

  <cpp|tree_to_verbatim> with the encoding <verbatim|SourceCode> follows
  the initial environment of a document which has
  <verbatim|source-line-ends> (<cpp|tree_to_source>): the lines are
  converted by <cpp|cork_to_sourcecode>, which writes a character
  <verbatim|\<less\>#9\<gtr\>> as the byte 9 (and the ASCII as it is, unlike
  <cpp|cork_to_utf8>, which writes a backquote as an opening quote and three
  dots as an ellipsis); a file of ISO-8859-1 is written in it; the line ends
  are those of the file (not those of the system, as for the other
  documents). A document without these variables is saved as before.

  <paragraph*|Tabs in the typesetter>

  <cpp|typeset_prog_string>
  (<source-link|concat_text.cpp|src/Typeset/Concat/concat_text.cpp>)
  typesets a tab as a gap up to the next tab stop, every
  <verbatim|prog-tab-width> columns. The column of a tab is counted from
  the start of its line (<cpp|prog_column>: a tab goes to its stop, any
  other character of Cork is one column, the font of programs being
  monospaced), also when the line is typeset from its middle (the range
  primitive). The gap (<cpp|typeset_prog_tab>) is a light arrow resized to
  the width of the gap, whose box has a decorated path, between two empty
  boxes at the ends of the tab: the cursor goes over it at once, as over a
  space. A token of a language which contains tabs (a string, a comment) is
  typeset in pieces (<cpp|typeset_prog_piece>), the tabs at their stops.

  The tokenizers take a tab for a blank (<cpp|is_source_tab> in
  <source-link|analyze.hpp|src/Data/String/analyze.hpp>): the
  <cpp|blanks_parser> of the generic program languages, and
  <cpp|cpp_language_rep>, <cpp|scheme_language_rep> and
  <cpp|verb_language_rep>, so that the words after a tab keep their
  colours.

  <paragraph*|Indentation>

  <source-link|prog-edit.scm|TeXmacs/progs/prog/prog-edit.scm> counts the
  indentation in columns, a tab going to its stop (<scm|string-column>,
  <scm|string-get-indent>), and writes it as the program is indented
  (<scm|program-indent-tabs?>: <verbatim|source-indent>, else tabs for
  <name|Go> and makefiles, else the preference <verbatim|editor:code
  indent>): <scm|string-set-indent> writes as many tabs as fit and spaces
  for the rest, or spaces. One level of indentation
  (<scm|program-indent-step>, used by the indentation of <name|Python>,
  <name|Java>, <name|Scala>, <name|Fortran> and dot) is a tab where the
  lines are indented by tabs, else <scm|get-tabstop>. <scm|insert-tabstop>
  inserts a tab there, else spaces up to the next stop from the column of
  the cursor. <scm|program-indent-line> returns the offset of the end of
  the indentation, where the cursor goes (with tabs, it is not the
  column).

  The indentation of <name|Python> strips the comments of a line before it
  looks at its last character: a <verbatim|#> of a character of Cork, as
  in <verbatim|\<less\>#9\<gtr\>>, is not a comment
  (<source-link|python-edit.scm|TeXmacs/progs/prog/python-edit.scm>).

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
