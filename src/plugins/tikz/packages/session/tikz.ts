<TeXmacs|2.1.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|tikz|1.0>

    <\src-purpose>
      Markup for TikZ sessions.
    </src-purpose>

    <src-copyright|2026|Massimiliano Gubinelli>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      A TikZ picture made in the browser (plugins/tikz/web/tm-tikz.js): its
      source, and the picture, an image of its drawing with its text set by
      TeXmacs over it. The source is kept for a picture made again from it.
    </src-comment>
  </active*>

  <assign|tikz-picture|<macro|src|body|<arg|body>>>

  <drd-props|tikz-picture|arity|2|accessible|none|border|no>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
