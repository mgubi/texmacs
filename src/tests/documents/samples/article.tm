<TeXmacs|2.1.5>

<style|article>

<\body>
  <doc-data|<doc-title|A sample article>|<doc-author|<author-data|<author-name|Ada
  Lovelace>|<author-affiliation|Analytical Engine Society>>>|<doc-date|1
  October 2026>>

  <abstract-data|<abstract|This document exercises the structure of an
  article: sections, lists, footnotes, references to labels and theorem-like
  environments. Its extracted text and its number of pages are compared with
  stored references, so any change of the layout shows up.>>

  <section|Introduction><label|sec-intro>

  The typesetter breaks paragraphs into lines with a global algorithm, which
  takes the whole paragraph into account when choosing the break points.
  Long words such as <em|incomprehensibilities>, <em|characteristically> and
  <em|disproportionately> invite hyphenation, and a justified paragraph
  stretches its spaces so that every line but the last fills the measure.
  This paragraph is long enough to need several lines and to show the
  effect.<footnote|A footnote is set at the bottom of the page on which its
  call occurs.>

  Section<nbsp><reference|sec-lists> shows lists, and
  Section<nbsp><reference|sec-theorems> theorems. Text may be <strong|strong>,
  <em|emphasized>, <verbatim|verbatim> or <with|font-shape|small-caps|in small
  capitals>.

  <section|Lists><label|sec-lists>

  <\itemize>
    <item>A first item, short.

    <item>A second item which is long enough to wrap onto a second line of
    the page, so that the indentation of continuation lines is checked.

    <\itemize>
      <item>A nested item.

      <item>Another nested item.
    </itemize>
  </itemize>

  <\enumerate>
    <item>One.

    <item>Two.

    <item>Three.
  </enumerate>

  <\description>
    <item*|Term>The description of the term.

    <item*|Longer term>A longer description, which also wraps onto a second
    line when the measure is not wide enough to hold all of it.
  </description>

  <section|Theorems><label|sec-theorems>

  <\definition>
    A <em|prime number> is a natural number greater than one which has no
    divisor other than one and itself.
  </definition>

  <\theorem>
    <label|thm-primes>There are infinitely many prime numbers.
  </theorem>

  <\proof>
    Suppose there are finitely many, multiply them all and add one: the
    result has a prime divisor which is none of them.
  </proof>

  Theorem<nbsp><reference|thm-primes> is due to Euclid.

  <subsection|A subsection>

  <\remark>
    Subsections are numbered within their section.
  </remark>
</body>

<initial|<\collection>
</collection>>
