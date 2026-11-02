<TeXmacs|2.1.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-mode-combo|1.0|dark-mode-combo|1.0>

    <\src-purpose>
      Combined dark mode theme.
    </src-purpose>

    <src-copyright|2026|the TeXmacs team>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-combo|dark-mode-scene>

  <copy-theme|dark-mode|dark>

  <select-theme|dark-mode|dark-mode-scene>

  <\active*>
    <\src-comment>
      Ornaments: the standard borders are black, and the background of
      titles white
    </\src-comment>
  </active*>

  <assign|dark-mode-ornament-sunny-color|#8a8a94>

  <assign|dark-mode-ornament-shadow-color|#8a8a94>

  <assign|dark-mode-ornament-extra-color|#3c3c44>

  <\active*>
    <\src-comment>
      Highlighters which do not know themes (those with the colors of the
      preferences) and the flags of the editor get colors which are
      readable on the dark paper
    </\src-comment>
  </active*>

  <assign|adapt-dark-colors|true>

  <\active*>
    <\src-comment>
      Colors of the source tags: the standard ones are blue, dark green,
      brown or black, which are not readable on a dark paper
    </\src-comment>
  </active*>

  <assign|src-tag-color|#7fb2ff>

  <assign|src-regular|<macro|body|<with|color|#d4d4d8|<arg|body>>>>

  <assign|src-var|<macro|body|<with|mode|src|color|#98c080|font-shape|italic|<arg|body>>>>

  <assign|src-arg|<macro|body|<with|mode|src|color|#d8a080|font-shape|italic|<arg|body>>>>

  <assign|src-tt|<macro|body|<with|mode|src|color|#9aa8ff|font-family|tt|<arg|body>>>>

  <assign|src-numeric|<macro|body|<with|mode|src|color|#d98ad9|<arg|body>>>>

  <assign|src-textual|<macro|body|<with|mode|src|color|#d4d4d8|<arg|body>>>>

  <assign|src-length|<macro|body|<with|mode|src|color|#6fd0d0|<arg|body>>>>

  <assign|src-unknown|<macro|body|<with|color|#e08aa0|<arg|body>>>>

  <assign|src-error|<macro|body|<with|color|#ff6b6b|<arg|body>>>>

</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>
