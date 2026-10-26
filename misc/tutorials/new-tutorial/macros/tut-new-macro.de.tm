<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Ein erstes Makro>

  Definieren wir ein Makro <markup|hi>, das den Text \PHello
  world!\Q erzeugt.

  <paragraph*|Das Makro definieren>

  Am einfachsten definieren Sie ein Makro ausgehend von einem Beispiel.
  Tippen Sie den Text <verbatim|Hello world!> in Ihr Dokument, wählen Sie
  ihn aus, und wählen Sie <menu|Tools|Macros|New macro>. Der
  <em|Makro-Editor> erscheint: Er zeigt den Rumpf des neuen Makros, also
  den ausgewählten Text, unter einem Feld für den Namen des Makros:

  <\big-envbox>
    <inline-tag|<with|color|red|font-shape|italic|enter-name>>
    <math|\<assign\>>

    Hello world!
  </big-envbox>

  Klicken Sie auf das Feld <with|color|red|font-shape|italic|enter-name>
  und tippen Sie den Namen <verbatim|hi>. Sie können den Rumpf noch ändern;
  er kann beliebige Inhalte enthalten: formatierten Text, Formeln,
  Tabellen, Bilder ... Klicken Sie dann auf <menu|Ok>. Das Makro
  <markup|hi> ist nun in Ihrem Dokument definiert.

  Wenn Sie den Makro-Editor ohne Auswahl öffnen, sind Name und Rumpf leer,
  und Sie geben beide auf dieselbe Weise ein.

  <paragraph*|Das Makro verwenden>

  Überall, wo nun der Text \PHello world!\Q erscheinen soll, tippen Sie
  <key|\\ h i> und <key|return>. Während Sie den Namen eingeben, bestätigt
  der Statusbalken, dass ein Makro <markup|hi> existiert. Sie können auch
  <menu|Insert|Macro|hi> wählen: Das Menü <menu|Insert|Macro> listet alle
  Makros Ihres Dokuments auf.

  Steht der Cursor neben dem vom Makro erzeugten Text, zeigt der
  Statusbalken <verbatim|hi> an: <TeXmacs> merkt sich, dass dieser Text
  eine Anwendung des Makros ist und kein gewöhnlicher Text. Wenn Sie
  insbesondere später die Definition von <markup|hi> ändern, zum Beispiel
  in \PHello everybody!\Q, ändert sich der Text sofort überall in Ihrem
  Dokument. Um die Definition zu ändern, setzen Sie den Cursor neben eine
  Anwendung des Makros und wählen <menu|Focus|Preferences|Edit macro>,
  oder Sie verwenden
  <menu|Tools|Macros|Edit macros>.

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
