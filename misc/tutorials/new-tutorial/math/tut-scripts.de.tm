<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Indizes, Exponenten und Brüche>

  Viele mathematische Konstrukte, etwa Indizes, Exponenten, Brüche und
  Wurzeln, bestehen aus einem oder mehreren Teilen, die Sie ausfüllen
  müssen. <TeXmacs> fügt das Konstrukt mit leeren Teilen ein und setzt den
  Cursor in den ersten davon; Sie geben dann seinen Inhalt ein und
  verlassen ihn, wenn Sie fertig sind.

  <paragraph*|Indizes und Exponenten>

  Um <math|x<rsup|2>> einzugeben, tippen Sie <key|x>, dann <key|^> für
  einen Exponenten und dann <key|2>. Der Cursor befindet sich nun im
  Exponenten, wie der cyanfarbene Kasten anzeigt:

  <\big-envbox>
    <math|x<rsup|<small-focus|2<math-cursor>>>>
  </big-envbox>

  Alles, was Sie jetzt eingeben, landet im Exponenten. Drücken Sie
  <key*|right>, um den Cursor aus dem Exponenten herauszubewegen, und
  setzen Sie die Formel fort. Indizes erhalten Sie auf dieselbe Weise mit
  <key|_>, wie in <math|x<rsub|n>>.

  <paragraph*|Brüche>

  Um unsere Formel zu <math|x<rsup|2>+<frac|a|b>> zu ergänzen, tippen Sie
  <key|+> und dann <key|math f> (oder wählen Sie <menu|Insert|Fraction>).
  Es erscheint ein leerer Bruch, mit dem Cursor im Zähler:

  <\big-envbox>
    <math|x<rsup|2>+<frac|<small-focus|a<math-cursor>>|>>
  </big-envbox>

  Tippen Sie <key|a>, drücken Sie dann <key*|down>, um in den Nenner zu
  wechseln, und tippen Sie <key|b>:

  <\big-envbox>
    <math|x<rsup|2>+<frac|a|<small-focus|b<math-cursor>>>>
  </big-envbox>

  Drücken Sie schlieÿlich <key*|right>, um den Bruch zu verlassen. Auf
  dieselbe Weise fügt <key|math s> eine Quadratwurzel ein, wie in
  <math|<sqrt|x+1>>, und <key|math s var> eine <math|n>-te Wurzel, wie in
  <math|<sqrt|x+1|3>>.

  <paragraph*|Ein Konstrukt entfernen>

  Wenn Sie ein Konstrukt versehentlich eingefügt haben, entfernt es
  <key*|backspace> am Anfang eines leeren Teils. Allgemeiner verfolgt
  <TeXmacs> die Struktur Ihrer Formel: Der cyanfarbene Kasten zeigt Ihnen
  stets, in welchem Teil welches Konstrukts sich der Cursor befindet, und
  der Statusbalken listet alle Konstrukte um den Cursor auf, etwa
  <verbatim|math> und <verbatim|denominator>.

  <tmdoc-copyright|1998\U2026|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
