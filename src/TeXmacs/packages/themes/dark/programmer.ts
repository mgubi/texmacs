<TeXmacs|2.1.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|programmer|1.0|programmer|1.0>

    <\src-purpose>
      Programmer mode: dark mode with a monospace text font, for documents which look like an editor.
    </src-purpose>

    <src-copyright|2026|the TeXmacs team>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-mode-combo>

  <apply-theme|dark-mode>

  <\active*>
    <\src-comment>
      Text font: IBM Plex Mono, shipped in TeXmacs/fonts/truetype/plex
    </src-comment>
  </active*>

  <assign|font|IBM Plex>

  <assign|font-family|tt>

</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>
