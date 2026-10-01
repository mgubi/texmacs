<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Page breaking primitives>

  The physical lines in a document are broken into pages in a way similar to
  how paragraphs are hyphenated into lines. The page breaker performs
  <def-index|page filling>, it tries to distribute page items evenly so text
  runs to the bottom of every page. It also tries to avoid <def-index|orphans
  and widows>, which are single or pairs of soft lines separated from the
  rest of their paragraph by a page break, but these can be produced when
  there is no better solution.

  <\explain>
    <explain-macro|no-page-break><explain-synopsis|prevent automatic page
    breaking after this paragraph>
  <|explain>
    Prevent the occurrence of an automatic page break between the current
    paragraph and the next one, by setting an infinite page breaking penalty
    after the last line of the current paragraph, similarly to
    <markup|no-break>.

    Forbidden page breaking points are overridden by ``new page'' and ``page
    break'' primitives.
  </explain>

  <\explain>
    <explain-macro|no-page-break*><explain-synopsis|prevent automatic page
    breaking before this paragraph>
  <|explain>
    Similar to <markup|no-page-break>, but forbid a page break between the
    previous paragraph and the current one.
  </explain>

  <\explain>
    <explain-macro|no-break-here>

    <explain-macro|no-break-here*><explain-synopsis|prevent page breaking
    after or before this line>
  <|explain>
    These tags work at the level of physical lines instead of paragraphs:
    <markup|no-break-here> forbids a page break just after the line which
    contains the tag, and <markup|no-break-here*> forbids a page break just
    before it.
  </explain>

  <\explain>
    <explain-macro|no-break-start>

    <explain-macro|no-break-end><explain-synopsis|prevent page breaking in a
    range>
  <|explain>
    Forbid all page breaks between the lines which have been typeset
    between a <markup|no-break-start> and the next <markup|no-break-end>.
  </explain>

  <\explain>
    <explain-macro|new-page><explain-synopsis|start a new page after this
    line>
  <|explain>
    Cause the next line to appear on a new page, without filling the current
    page. The page breaker will not try to position the current line at the
    bottom of the page.
  </explain>

  <\explain>
    <explain-macro|new-page*><explain-synopsis|start a new page before this
    line>
  <|explain>
    Similar to <markup|new-page>, but start the new page before the current
    line. This directive is appropriate to use in chapter headings.
  </explain>

  <\explain>
    <explain-macro|page-break><explain-synopsis|force a page break after this
    line>
  <|explain>
    Force a page break after the current line. A forced page break is
    different from a new page, the page breaker will try to position the
    current line at the bottom of the page.

    Use only to fine-tune the automatic page breaking. Ideally, this should
    be a hint similar to <markup|line-break>, but this is implemented as a
    directive, use only with extreme caution.
  </explain>

  <\explain>
    <explain-macro|page-break*><explain-synopsis|force a page break before
    this line>
  <|explain>
    Similar to <markup|page-break>, but force a page break before the current
    line.
  </explain>

  <\explain>
    <explain-macro|new-dpage>

    <explain-macro|new-dpage*><explain-synopsis|start a new double page>
  <|explain>
    Similar to <markup|new-page> and <markup|new-page*>, but the new page
    is always an odd-numbered (right-hand) page: an empty page is inserted
    when necessary.
  </explain>

  When several ``new page'' and ``page break'' directives apply to the same
  point in the document, only the first one is effective. Any
  <markup|new-page> or <markup|page-break> after the first one in a line is
  ignored. Any <markup|new-page> or <markup|page-break> in a line overrides
  any <markup|new-page*> or <markup|page-break*> in the following line. Any
  <markup|new-page*> or <markup|page-break*> after the first one in a line is
  ignored.

  <\explain>
    <explain-macro|if-page-break|where|content><explain-synopsis|content
    inserted at page breaks>
  <|explain>
    The <src-arg|content> is only displayed if a page break occurs at this
    point, in which case it is typeset at the top of the new page. For
    instance, the index macros of <verbatim|std-automatic.ts> use an
    <markup|if-page-break> tag with <src-arg|where> equal to
    <verbatim|t> in order to repeat the main index entry at the top of a
    page when its subentries are split across pages. The evaluated <src-arg|where> argument is passed to the page
    breaker together with the <src-arg|content>. This primitive is only
    taken into account by the new page breaker, which is enabled by default
    through the <verbatim|new style page breaking> preference.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>