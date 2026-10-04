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

  <drd-props|tikz-picture|arity|2|accessible|1|border|no>

  <\active*>
    <\src-comment>
      The text of a node of the picture, typeset by TeXmacs from its source
      and editable: n is the number of the node, orig the text as it came
      (an edited one is put back into the source of an executable fold,
      plugins/tikz/progs/tikz-edit.scm).
    </src-comment>
  </active*>

  <assign|tikz-label|<macro|n|orig|body|<arg|body>>>

  <\active*>
    <\src-comment>
      The drawing of the picture, under its labels: not accessible, so that
      a click on a label goes to the label (a superpose gives a click to the
      first accessible child under it)
    </src-comment>
  </active*>

  <assign|tikz-drawing|<macro|body|<arg|body>>>

  <drd-props|tikz-drawing|arity|1|accessible|none|border|no>

  <\active*>
    <\src-comment>
      The source of an executable fold of TikZ: code, a backslash being a
      backslash (as the input of a converter, scripts.ts)
    </src-comment>
  </active*>

  <assign|tikz-script-input|<macro|language|session|in|out|<style-with|src-compact|none|<compound|<if|<equal|<get-label|<arg|in>>|document>|render-big-script|render-small-script>|<arg|language>|<with|mode|prog|prog-language|verbatim|<arg|in>>>>>>

  <drd-props|tikz-label|arity|3|accessible|2|border|no>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
