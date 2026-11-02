<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Der Cursor und die Struktur>

  Betrachten Sie den folgenden Satz, in dem die Wörter \Pbold blue\Q fett
  (mit <shortcut|(toggle-bold)>) und dann blau (mit <menu|Format|Color>)
  gesetzt wurden:

  <\quote-env>
    Some <with|font-series|bold|<with|color|blue|bold blue>> text.
  </quote-env>

  Die Wörter \Pbold blue\Q stehen in zwei Auszeichnungen: einer für die
  fette Schrift und einer für die blaue Farbe. Wohin kommt ein Buchstabe,
  den Sie neben diesen Wörtern eingeben? <TeXmacs> beantwortet diese Frage,
  indem es dem Cursor mehrere Positionen am Rand der Auszeichnung gibt, die
  auf dem Bildschirm gleich aussehen, im Dokument aber verschieden sind.

  Setzen Sie den Cursor hinter \PSome \Q, und drücken Sie mehrmals
  <key*|right>, während Sie den Statusbalken und die Kästen um den Cursor
  beobachten:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Cursor>|<cell|Position>>|<row|<cell|Some
  <text-cursor><with|font-series|bold|<with|color|blue|bold blue>>
  text.>|<cell|auÿerhalb der Auszeichnung>>|<row|<cell|Some
  <with|font-series|bold|<small-focus|<text-cursor><with|color|blue|bold
  blue>>> text.>|<cell|in der fetten Schrift>>|<row|<cell|Some
  <with|font-series|bold|<small-envbox|<small-focus|<with|color|blue|<text-cursor>bold
  blue>>>> text.>|<cell|in der fetten Schrift und der blauen
  Farbe>>|<row|<cell|Some <with|font-series|bold|<with|color|blue|b<text-cursor>old
  blue>> text.>|<cell|nach dem Buchstaben <verbatim|b>>>>>>>
    Der Cursor bewegt sich in \Pbold blue\Q hinein.
  </big-table>

  Die ersten Drücke auf <key*|right> bewegen den Cursor nicht auf dem
  Bildschirm, sondern bewegen ihn in die Auszeichnung hinein, eine Ebene
  nach der anderen. Wenn Sie an einer dieser Positionen einen Buchstaben
  eingeben, erhält er die Schrift und die Farbe dieser Position: in der
  ersten Zeile einen gewöhnlichen schwarzen Buchstaben, in der zweiten einen
  fetten schwarzen Buchstaben und in der dritten einen fetten blauen
  Buchstaben. Dasselbe geschieht am Ende von \Pbold blue\Q.

  Wie wir gesehen haben, ist die innerste Auszeichnung um den Cursor von
  einem cyanfarbenen Kasten umgeben und die äuÿeren von hellgrauen Kästen;
  der rechte Teil des Statusbalkens listet sie alle auf. Mit ein wenig
  Übung erkennen Sie an diesen Kästen auf einen Blick, wohin Ihr nächster
  Buchstabe kommt.

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
