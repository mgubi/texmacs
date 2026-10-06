<TeXmacs|1.0.7.17>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing the treatment of title information>

  <TeXmacs> uses the <markup|doc-data> tag in order to specify global data
  for the document. These data are treated in two stages.
  <hlink|First|../../../main/styles/header/header-title-global.en.tm>, the
  document data are separated into several categories, according to whether
  the data should be rendered as a part of the main title or in footnotes or
  running headers. <hlink|Secondly|../../../main/styles/header/header-title-customize.en.tm>,
  the data in each category are rendered using suitable macros. In recent
  versions of <TeXmacs>, the first stage is no longer implemented by
  macros: the <markup|doc-data> macro of <tmpackage|title-base> is defined
  as <inactive*|<xmacro|args|<extern|doc-data|<quote-arg|args>|>>>, and the
  actual reorganization of the data is done by the <scheme> function
  <scm|doc-data> in <source-link|progs/database/title-markup.scm|TeXmacs/progs/database/title-markup.scm> (which is
  loaded by <tmpackage|title-base> through <markup|use-module>). Similarly,
  <markup|author-data> is handled by the <scheme> function
  <scm|author-data>.

  Each child of the <markup|doc-data> is a tag with some specific information
  about the document. Currently implemented tags are <markup|doc-title>,
  <markup|doc-subtitle>, <markup|doc-author>, <markup|doc-date>,
  <markup|doc-misc>, <markup|doc-note>, <markup|doc-running-title>,
  <markup|doc-running-author> and <markup|doc-inactive>. The
  <markup|doc-author> tag may occur several times. The optional tag
  <markup|doc-title-options> may be used in order to specify options for the
  treatment of the title data, such as <verbatim|abbreviate-authors>,
  <verbatim|cluster-all>, <verbatim|cluster-by-affiliation> or
  <verbatim|ams-title>. The <markup|author-data> tag is used in order to
  specify structured data for each of the authors of the document. Each
  child of the <markup|author-data> tag is a tag with information about the
  corresponding author. Currently implemented tags with author information
  are <markup|author-name>, <markup|author-affiliation>,
  <markup|author-email>, <markup|author-homepage>, <markup|author-misc> and
  <markup|author-note>. Keywords and subject classifications are no longer
  part of the title data, but specified using the <markup|abstract-data> tag
  (with children <markup|abstract>, <markup|abstract-keywords>,
  <markup|abstract-msc>, <markup|abstract-acm>, <markup|abstract-arxiv> and
  <markup|abstract-pacs>).

  Most of the tags listed above also correspond to macros for rendering the
  corresponding information as part of the main title. For instance, if the
  date should appear in bold italic at a distance of at least <verbatim|1fn>
  from the other title fields, then you may redefine <markup|doc-date> as

  <\tm-fragment>
    <\inactive*>
      <assign|doc-date|<macro|body|<style-with|src-compact|none|<vspace*|1fn><doc-title-block|<with|font-shape|italic|font-series|bold|<arg|body>>><vspace|1fn>>>>
    </inactive*>
  </tm-fragment>

  The <markup|doc-title-block> macro is used in order to make the text span
  appropriately over the width of the title; for author information, the
  analogous macros <markup|doc-author-block> and <markup|doc-authors-block>
  are used. In order to customize only the font of the title, it suffices to
  redefine <markup|doc-title-name> (which defaults to <markup|strong>). Some
  styles, such as <tmpackage|title-book> and <tmpackage|title-seminar>,
  introduce additional hooks like <markup|doc-render-title> and
  <markup|author-render-name>.

  Notice also that the <markup|doc-running-title> and
  <markup|doc-running-author> macros do not render anything, but rather call
  the <markup|header-title> and <markup|header-author> call-backs for setting
  the appropriate global page headers and footers. By default, the running
  title and author are extracted from the usual title and author names.

  In addition to the rendering macros which are present in the document, the
  main title (including author information, the date, <abbr|etc.>) is
  rendered using the <markup|doc-make-rich-title> macro, which takes the
  hidden data (running title and author, footnote texts) and the main title
  as its arguments, and which relies on <markup|doc-make-title>. When the
  document has several authors, they are grouped together using
  <markup|doc-authors>. Notes attached to the title or to one of the authors
  (<markup|doc-note> and <markup|author-note>) are turned into footnotes:
  their references are rendered using <markup|doc-note-ref> and the
  footnotes themselves using <markup|doc-footnote-text>; the
  <markup|author-...-note> variants of the author tags (like
  <markup|author-email-note>) are used for author information which is
  rendered as a note.

  The first stage of processing the document data is more complex and the
  reader is invited to take a look at the <hlink|short
  descriptions|../../../main/styles/header/header-title-global.en.tm> of the
  macros which are involved in this process. It is also good to study the
  definitions of these macros in the <hlink|package
  itself|$TEXMACS_PATH/packages/header/title-base.ts> and the <scheme> code
  in <verbatim|$TEXMACS_PATH/progs/database/title-markup.scm>. For instance,
  the function <scm|doc-data-impl> can be overloaded (using
  <scm|tm-define> with a <scm|:require> clause on the title options, as is
  done for the <verbatim|ams-title> option) in order to completely change
  the way the title data are organized.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>