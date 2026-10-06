<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Look at an example>

  Before writing your own style file, it may be useful to take a look at some
  standard style files. For instance, you may open <source-link|book.ts|TeXmacs/styles/book.ts> using
  <menu|File|Open>; it can be found in the directory
  <verbatim|$TEXMACS_PATH/styles>. Alternatively, when editing a document
  whose style is <tmstyle|book>, you may use <menu|Document|Style|Edit style>.
  Style files are shown in source mode, so that all macro and environment
  declarations are visible.

  The file <source-link|book.ts|TeXmacs/styles/book.ts> itself is very short: it essentially loads the
  packages <tmpackage|std>, <tmpackage|env>, <tmpackage|title-book>,
  <tmpackage|header-book> and <tmpackage|section-book> and sets a few style
  parameters. Most declarations are contained in these packages and in the
  packages on which they are based in their turn. For instance,
  <tmpackage|std> loads <tmpackage|std-markup>, <tmpackage|std-list>,
  <tmpackage|std-math>, <tmpackage|std-automatic> and several other packages
  (in <verbatim|$TEXMACS_PATH/packages/standard>), which respectively contain
  basic markup, itemize-like environments, mathematical markup, automatically
  generated content (tables of contents, bibliographies, <abbr|etc.>), and so
  on. Similarly, <tmpackage|env> loads <tmpackage|env-base>,
  <tmpackage|env-math>, <tmpackage|env-theorem>, <tmpackage|env-float> and
  <tmpackage|env-program> (in <verbatim|$TEXMACS_PATH/packages/environment>),
  which contain theorem-like, mathematical, floating and programming
  environments. Sectional commands are defined in <tmpackage|section-base>
  and customized in <tmpackage|section-book>.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
