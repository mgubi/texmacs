<TeXmacs|2.1.5>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|font-design|1.0>

    <\src-purpose>
      Internal style package for the page of the design of the fonts of a document.
    </src-purpose>

    <src-copyright|2026|Massimiliano Gubinelli>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|gui>

  <\active*>
    <\src-comment>
      The page is a widget (gui-markup): it fits its window, with small
      margins. Buttons (inside action tags), muted text, the frame of a sample, the rule between two fonts and the scrolling list of the fonts.
    </src-comment>
  </active*>

  <assign|font-design-button|<macro|body|<short-raise|<arg|body>>>>

  <assign|font-design-muted|<macro|body|<with|color|#707070|<arg|body>>>>

  <assign|font-design-sample|<macro|body|<with|ornament-color|#f8f6ee|ornament-sunny-color|#d6d2c4|ornament-shadow-color|#d6d2c4|ornament-hpadding|1spc|ornament-vpadding|1spc|ornament-border|1ln|<ornament|<arg|body>>>>>

  <assign|font|pagella>

  <assign|font-family|rm>

  <assign|par-par-sep|0.4em>

  <assign|font-base-size|9>

  <assign|math-font|math-pagella>

  <assign|font-design-top-height|25em>

  <assign|font-design-list-height|<macro|<merge|<look-up|<maximum|<minus|1pag|<value|font-design-top-height>>|10em>|0>|tmpt>>>

  <assign|font-design-list|<macro|scroll|body|<with|canvas-type|e|canvas-color|white|canvas-hpadding|1spc|canvas-vpadding|1spc|ornament-border|1ln|ornament-sunny-color|#b8b8c0|ornament-shadow-color|#b8b8c0|<canvas||<merge|t-|<font-design-list-height>>|1par||0%|<arg|scroll>|<arg|body>>>>>

  <assign|font-design-rule|<macro|<with|color|#d4d4dc|<hrule>>>>

  <drd-props|font-design-sample|arity|1>

  <drd-props|font-design-rule|arity|0>

  <drd-props|font-design-list|arity|2|enable-writability|all>

  <drd-props|font-design-button|arity|1>

  <drd-props|font-design-muted|arity|1>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
