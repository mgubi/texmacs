<TeXmacs|2.1.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|asymptote|1.0>

    <\src-purpose>
      Markup for Asymptote sessions.
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
      An Asymptote picture made in the browser
      (plugins/asymptote/web/tm-asy.mjs): its source, and the picture, an
      image of its drawing with its labels set by TeXmacs over it, in a
      graphics. The source is kept for a picture made again from it.
    </src-comment>
  </active*>

  <assign|asy-picture|<macro|src|body|<arg|body>>>

  <drd-props|asy-picture|arity|2|accessible|1|border|no>

  <\active*>
    <\src-comment>
      A label of the picture, set by TeXmacs from its LaTeX and editable: n
      is its number, orig its text as it came (an edited one goes back into
      the source of an executable fold, asymptote-edit.scm).
    </src-comment>
  </active*>

  <assign|asy-label|<macro|n|orig|body|<arg|body>>>

  <drd-props|asy-label|arity|3|accessible|2|border|no>

  <\active*>
    <\src-comment>
      The drawing of the picture, under its labels: not accessible, so that
      a click on a label goes to the label
    </src-comment>
  </active*>

  <assign|asy-drawing|<macro|body|<arg|body>>>

  <drd-props|asy-drawing|arity|1|accessible|none|border|no>

  <\active*>
    <\src-comment>
      The source of an executable fold of Asymptote: code, a backslash being
      a backslash (as the input of a converter, scripts.ts)
    </src-comment>
  </active*>

  <assign|asymptote-script-input|<macro|language|session|in|out|<style-with|src-compact|none|<compound|<if|<equal|<get-label|<arg|in>>|document>|render-big-script|render-small-script>|<arg|language>|<with|mode|prog|prog-language|verbatim|<arg|in>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
