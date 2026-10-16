<TeXmacs|2.1.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|office|1.0>

      <\src-purpose>
        The style of the documents which are read from Word and
        OpenDocument files. Its macros keep what the office formats say and
        the standard tags do not: the named styles of the paragraphs and of
        the text, the fields, the comments and the tracked changes, the
        sections in columns. The page, the usual paragraph, the headers and
        footers, the definitions of the named styles and the parts of the
        file which TeXmacs does not edit are in the initial environment of
        the document, so that a file which is read and written again keeps
        them.
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
      A paragraph or a piece of text of a named style of the file: the
      style is the macro office-par-name or office-span-name of the
      document, when it has one, and else the text is as it is.
    </src-comment>
  </active*>

  <assign|office-plain|<\macro|body>
    <arg|body>
  </macro>>

  <assign|office-par|<\macro|name|body>
    <compound|<if|<provides|<merge|office-par-|<arg|name>>>|<merge|office-par-|<arg|name>>|office-plain>|<arg|body>>
  </macro>>

  <assign|office-span|<macro|name|body|<compound|<if|<provides|<merge|office-span-|<arg|name>>>|<merge|office-span-|<arg|name>>|office-plain>|<arg|body>>>>

  <drd-props|office-par|arity|2|accessible|1>

  <drd-props|office-span|arity|2|accessible|1>

  <\active*>
    <\src-comment>
      An underline or a line through of another kind than a single line
      (double, wave, dotted, dash, thick), text whose letters are spaced,
      text in a font which TeXmacs does not have (shown in the family of
      fonts which is the closest), and text whose words begin with
      capitals (shown as it is): they are shown as TeXmacs can, and written
      as they were read.
    </src-comment>
  </active*>

  <assign|office-underline|<macro|kind|body|<if|<equal|<arg|kind>|double>|<underline|<underline|<arg|body>>>|<underline|<arg|body>>>>>

  <assign|office-strike|<macro|kind|body|<strike-through|<arg|body>>>>

  <assign|office-letters|<macro|space|body|<arg|body>>>

  <assign|office-font|<macro|name|family|body|<with|font-family|<arg|family>|<arg|body>>>>

  <drd-props|office-font|arity|3|accessible|2>

  <assign|office-capitalize|<macro|body|<arg|body>>>

  <drd-props|office-capitalize|arity|1|accessible|0>

  <drd-props|office-underline|arity|2|accessible|1>

  <drd-props|office-strike|arity|2|accessible|1>

  <drd-props|office-letters|arity|2|accessible|1>

  <\active*>
    <\src-comment>
      A picture which floats beside its paragraph, on a side (left, right,
      center), with where it is and how the text goes around it, as
      "key=value ...": TeXmacs shows it in a paragraph of its own.
    </src-comment>
  </active*>

  <assign|office-float|<macro|side|spec|body|<arg|body>>>

  <drd-props|office-float|arity|3|accessible|2>

  <\active*>
    <\src-comment>
      A box of text which floats beside its paragraph, or a shape with
      text: its side, the rest as "key=value ..." (where it is, its height
      and its shape), its width, its fill and its line (colors, none, or
      nothing), and its text. A shape without text: its side, all the rest,
      and a drawing of it.
    </src-comment>
  </active*>

  <assign|office-box|<macro|side|spec|width|fill|line|body|<tabular|<tformat|<twith|table-width|<arg|width>>|<twith|table-hmode|<if|<equal|<arg|width>|>|auto|exact>>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-background|<if|<equal|<arg|fill>|none>||<arg|fill>>>|<cwith|1|1|1|1|cell-lborder|<if|<equal|<arg|line>|none>|0ln|<if|<equal|<arg|line>|>|0ln|1ln>>>|<cwith|1|1|1|1|cell-rborder|<if|<equal|<arg|line>|none>|0ln|<if|<equal|<arg|line>|>|0ln|1ln>>>|<cwith|1|1|1|1|cell-tborder|<if|<equal|<arg|line>|none>|0ln|<if|<equal|<arg|line>|>|0ln|1ln>>>|<cwith|1|1|1|1|cell-bborder|<if|<equal|<arg|line>|none>|0ln|<if|<equal|<arg|line>|>|0ln|1ln>>>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <assign|office-shape|<macro|side|spec|drawing|<arg|drawing>>>

  <drd-props|office-box|arity|6|accessible|5>

  <drd-props|office-shape|arity|3>

  <\active*>
    <\src-comment>
      A field of the office programs (a page number, a date, the title of
      the document...): its instruction, and its result as the file had
      it.
    </src-comment>
  </active*>

  <assign|office-field|<macro|code|result|<arg|result>>>

  <drd-props|office-field|arity|2|accessible|1>

  <\active*>
    <\src-comment>
      A comment, with its author and its date, after its mark; a comment
      on a range of the text, after the text, which is marked; the text
      which was inserted and the one which was deleted, while the changes
      are tracked.
    </src-comment>
  </active*>

  <assign|office-comment-color|dark magenta>

  <assign|office-comment|<macro|author|date|body|<with|color|<value|office-comment-color>|<rsup|<math|\<ast\>>><with|font-size|0.71|[<with|font-series|bold|<arg|author>>: <arg|body>]>>>>

  <assign|office-commented|<macro|text|author|date|body|<with|marked-color|pastel magenta|<marked|<arg|text>>><office-comment|<arg|author>|<arg|date>|<arg|body>>>>

  <assign|office-ins|<macro|author|date|body|<with|color|dark green|<underline|<arg|body>>>>>

  <assign|office-del|<macro|author|date|body|<with|color|dark red|<strike-through|<arg|body>>>>>

  <drd-props|office-comment|arity|3|accessible|2>

  <drd-props|office-commented|arity|4|accessible|all>

  <drd-props|office-ins|arity|3|accessible|2>

  <drd-props|office-del|arity|3|accessible|2>

  <\active*>
    <\src-comment>
      A section of the file in several columns; with the line between them
      (which TeXmacs does not draw) and the space between them.
    </src-comment>
  </active*>

  <assign|office-columns|<\macro|n|body>
    <\with|par-columns|<arg|n>>
      <arg|body>
    </with>
  </macro>>

  <assign|office-columns*|<\macro|n|line|gap|body>
    <\with|par-columns|<arg|n>|par-columns-sep|<if|<equal|<arg|gap>|>|<value|par-columns-sep>|<arg|gap>>>
      <arg|body>
    </with>
  </macro>>

  <drd-props|office-columns|arity|2|accessible|1>

  <drd-props|office-columns*|arity|4|accessible|3>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
