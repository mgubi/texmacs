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

  <\active*>
    <\src-comment>
      Buttons (inside action tags), muted text, the frame of a sample and the scrolling list of the fonts.
    </src-comment>
  </active*>

  <assign|font-design-button|<macro|body|<small|<with|ornament-color|#e8e8ec|ornament-sunny-color|#f6f6f8|ornament-shadow-color|#b8b8c0|ornament-hpadding|0.5spc|ornament-vpadding|0.25ex|ornament-border|1ln|<ornament|<arg|body>>>>>>

  <assign|font-design-badge|<macro|kind|body|<small|<with|font-series|bold|color|<case|<equal|<arg|kind>|conflict>|#b01010|<equal|<arg|kind>|staged>|#207020|<equal|<arg|kind>|new>|#2040a0|#707070>|<arg|body>>>>>

  <assign|font-design-muted|<macro|body|<with|color|#707070|<arg|body>>>>

  <assign|font-design-note|<macro|color|body|<small|<with|color|<arg|color>|<arg|body>>>>>

  <assign|font-design-sample|<macro|body|<with|ornament-color|#f8f6ee|ornament-sunny-color|#d6d2c4|ornament-shadow-color|#d6d2c4|ornament-hpadding|1spc|ornament-vpadding|1spc|ornament-border|1ln|<ornament|<arg|body>>>>>

  <assign|page-medium|automatic>

  <assign|font|pagella>

  <assign|font-base-size|9>

  <assign|math-font|math-pagella>

  <assign|font-design-list-height|<macro|0.5pag>>

  <assign|font-design-list|<macro|scroll|body|<with|canvas-type|e|canvas-color|white|canvas-hpadding|1spc|canvas-vpadding|1spc|ornament-border|1ln|ornament-sunny-color|#b8b8c0|ornament-shadow-color|#b8b8c0|<canvas||<merge|t-|<font-design-list-height>>|1par||0%|<arg|scroll>|<arg|body>>>>>

  <drd-props|font-design-sample|arity|1>

  <drd-props|font-design-list|arity|2|enable-writability|all>

  <drd-props|font-design-button|arity|1>

  <drd-props|font-design-badge|arity|2>

  <drd-props|font-design-muted|arity|1>

  <drd-props|font-design-note|arity|2>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
