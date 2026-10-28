<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Makros mit Argumenten und die Präambel>

  <paragraph*|Die Präambel>

  Die Makros, die Sie definieren, werden in der <em|Präambel> Ihres
  Dokuments gespeichert, einem verborgenen Teil an seinem Anfang. Sie
  können sie mit <menu|Tools|Macros|Edit preamble> ansehen; dabei wird nur
  die Präambel angezeigt, und zwar als <em|Quellcode>: Statt ihres
  Ergebnisses sehen Sie die Definitionen selbst. Nachdem Sie das Makro
  <markup|hi> aus dem vorigen Abschnitt definiert haben, enthält die
  Präambel

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>
  </tm-fragment>

  Das bedeutet: Weise (<em|assign>) dem Namen <verbatim|hi> das <em|Makro>
  zu, dessen Rumpf <verbatim|Hello world!> ist. Wählen Sie
  erneut <menu|Tools|Macros|Edit preamble>, um zu Ihrem Text
  zurückzukehren.

  <paragraph*|Ein Makro mit einem Argument>

  Definieren wir nun ein Makro <markup|love>, sodass
  <inactive*|<love|free software>> den Text \PI love free software
  <with|color|red|<math|\<heartsuit\>>>\Q erzeugt. Der Text nach \PI
  love\Q ändert sich, er wird daher zu einem <em|Argument> des Makros, das
  wir <src-arg|name> nennen. Die Definition lautet

  <\tm-fragment>
    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  Im Makro-Editor (<menu|Tools|Macros|New macro>) würden Sie den Namen
  <verbatim|love> des Makros eingeben, ihm dann ein Argument geben und den
  Rumpf tippen. Sie können die Definition auch direkt in die Präambel
  schreiben, genauso wie den Quellcode jedes anderen Dokuments:

  <\enumerate>
    <item>Wählen Sie <menu|Tools|Macros|Edit preamble> und setzen Sie den
    Cursor an das Ende der Präambel.

    <item>Tippen Sie <key|\\ a s s i g n> und <key|return>: Ein leeres
    <markup|assign> erscheint. Tippen Sie den Namen <verbatim|love> in sein
    erstes Feld, und wechseln Sie mit <key*|right> in das zweite Feld.

    <item>Tippen Sie <key|\\ m a c r o> und <key|return>, dann den Namen
    des Arguments, <verbatim|name>. Drücken Sie <key|tab>: Dahinter wird ein
    neues Feld für den Rumpf des Makros eingefügt.

    <item>Tippen Sie im Rumpf \PI love \Q und dann den Namen des Arguments
    nach der Taste <key|\\>, also <key|\\ n a m e>: Der Statusbalken meldet,
    dass <key|return> das Argument <verbatim|name> einfügen wird. Drücken
    Sie <key|return>: An dieser Stelle wird der Wert des Arguments
    erscheinen. Tippen Sie ein Leerzeichen und fügen Sie zum Schluss das
    Herz hinzu: Wählen Sie die rote Farbe unter <menu|Format|Color>, drücken
    Sie <key|$> und tippen Sie <key|\\ h e a r t s u i t>
    und <key|return>.

    <item>Verlassen Sie die Präambel mit <menu|Tools|Macros|Edit preamble>.
  </enumerate>

  Wenn Sie nun in Ihrem Text <key|\\ l o v e> und <key|return> tippen,
  erscheint ein Kasten mit einem Feld, in das Sie das Argument eingeben:

  <\big-envbox>
    I love <small-focus|free software<text-cursor>>
    <with|color|red|<math|\<heartsuit\>>>
  </big-envbox>

  Das Dokument <verbatim|examples/love.tm> enthält die Makros dieses
  Kapitels und einige Anwendungen davon.

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
