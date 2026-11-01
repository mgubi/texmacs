<TeXmacs|2.1.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|markdown|1.0>

      <\src-purpose>
        The style of the documents which are read from Markdown files. Its
        macros keep what Markdown says and the standard tags do not: a file
        which is read and written again is the same Markdown.
      </src-purpose>

      <\src-copyright|2026>
        the TeXmacs team
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|generic>

  <\active*>
    <\src-comment>
      Text without serifs, as the programs which display Markdown have it.
    </src-comment>
  </active*>

  <assign|font-family|ss>

  <\active*>
    <\src-comment>
      The headings of Markdown have no numbers. Those of the six levels are
      section, subsection, subsubsection, paragraph, subparagraph and md-h6.
    </src-comment>
  </active*>

  <assign|part-display-numbers|<macro|false>>

  <assign|chapter-display-numbers|<macro|false>>

  <assign|section-display-numbers|<macro|false>>

  <assign|subsection-display-numbers|<macro|false>>

  <assign|subsubsection-display-numbers|<macro|false>>

  <assign|md-h6|<macro|title|<subparagraph|<with|font-shape|italic|<arg|title>>>>>

  <\active*>
    <\src-comment>
      The header of a file, in YAML: one md-meta for each of its lines, with
      the key and its value, or without key for a line which has none (an
      item of a list, a comment, the next line of a text).
    </src-comment>
  </active*>

  <assign|md-metadata|<\macro|body>
    <\padded>
      <\with|font-size|0.84|color|dark grey|par-par-sep|0fn>
        <arg|body>
      </with>
    </padded>
  </macro>>

  <assign|md-meta|<macro|key|value|<if|<equal|<arg|key>|>|<with|font-family|tt|<arg|value>>|<with|font-series|bold|<arg|key>:> <arg|value>>>>

  <\active*>
    <\src-comment>
      What is written in a file in another way than TeXmacs writes it, or
      what TeXmacs does not understand all of: a formula, a table, a piece
      of HTML. The body, the text of the file for it, and a copy of the
      body: the text is written again as long as the body is this copy,
      and the body is written when it was edited. md-source is for a piece
      of a paragraph and md-block-source for paragraphs of their own.
    </src-comment>
  </active*>

  <assign|md-source|<macro|body|source|copy|<if|<equal|<arg|body>|>|<flag|source|dark green>|<arg|body>>>>

  <assign|md-block-source|<\macro|body|source|copy>
    <\if|<equal|<arg|body>|>>
      <flag|source|dark green>
    <|if>
      <arg|body>
    </if>
  </macro>>

  <drd-props|md-source|arity|3|accessible|0>

  <drd-props|md-block-source|arity|3|accessible|0>

  <\active*>
    <\src-comment>
      A list whose items are separated by blank lines; code with the words
      which follow its fence; an image with its description and its title;
      a footnote with its label, and the place where its text is written
      in the file when this is not after everything else.
    </src-comment>
  </active*>

  <assign|md-loose|<\macro|body>
    <\with|item-vsep|<macro|1fn>>
      <arg|body>
    </with>
  </macro>>

  <assign|md-fence|<\macro|info|body>
    <arg|body>
  </macro>>

  <assign|md-image|<macro|body|alt|title|<arg|body>>>

  <assign|md-footnote|<macro|label|body|<footnote|<arg|body>>>>

  <assign|md-footnote-text|<macro|label|<flag|<arg|label>|dark green>>>

  <drd-props|md-fence|arity|2|accessible|1>

  <drd-props|md-image|arity|3|accessible|0>

  <drd-props|md-footnote|arity|2|accessible|1>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
