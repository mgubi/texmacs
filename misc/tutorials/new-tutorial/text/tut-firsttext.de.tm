<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Einen einfachen Text schreiben, speichern, drucken und laden>

  <paragraph*|Schreiben>

  Sobald Sie Ihrem Dokument einen Namen gegeben haben, können Sie mit dem
  Schreiben beginnen. Tippen Sie Ihren Text einfach wie auf einer
  Schreibmaschine: <TeXmacs> bricht die Zeilen automatisch um, wenn sie zu
  lang werden, und setzt sie im Blocksatz, das heiÿt, es passt die
  Abstände zwischen den Wörtern so an, dass alle Zeilen gleich breit sind.
  Sie müssen nur <key|return> drücken, um einen neuen Absatz zu beginnen.
  Der folgende Text ist eine einfache Übung:

  <\big-figure>
    <\big-envbox>
      Hey you there!

      Would you be so kind to enroll the gnus and gnats around you to try
      <TeXmacs> once?<text-cursor>
    </big-envbox>
  <|big-figure>
    Ein einfacher, mit <TeXmacs> geschriebener Text
    (<verbatim|examples/test.tm>).
  </big-figure>

  Wenn Sie sich vertippen, löscht <key*|backspace> das Zeichen vor dem
  Cursor, und <menu|Edit|Undo> (<shortcut|(undo 0)>) macht Ihre letzten
  Änderungen rückgängig. Enthält Ihr Text Zeichen, die nicht auf Ihrer
  Tastatur vorhanden sind, etwa Buchstaben mit Akzenten, so können Sie sie
  mit den Methoden Ihres Systems eingeben; wie man mathematische Symbole
  eingibt, sehen wir später.

  <paragraph*|Speichern>

  Wenn Sie mit dem Schreiben fertig sind, speichern Sie Ihr Dokument mit
  <menu|File|Save>, mit dem Eintrag <menu|Save> des Symbolmenüs
  <icon|tm_save.png> oder mit <shortcut|(save-buffer)>. Eine Meldung im
  Statusbalken bestätigt, dass das Dokument gespeichert wurde.

  Zu Ihrer Sicherheit speichert <TeXmacs> Ihr Dokument auÿerdem alle zwei
  Minuten <em|automatisch> in einer separaten Sicherungsdatei. Wenn Sie
  vergessen, Ihr Dokument vor dem Beenden von <TeXmacs> zu speichern, oder
  wenn Ihr Computer aus irgendeinem Grund abstürzt, bietet Ihnen <TeXmacs>
  beim nächsten Öffnen des Dokuments an, die nicht gespeicherten Änderungen
  wiederherzustellen.

  <paragraph*|Drucken und Exportieren>

  Meist werden Sie Ihr Dokument drucken oder an Personen schicken wollen,
  die <TeXmacs> nicht verwenden. Sie drucken Ihr Dokument mit
  <menu|File|Print|Print buffer> auf den Druckern Ihres Systems. Sie können
  es auch mit <menu|File|Export|Pdf> in eine PDF-Datei umwandeln, die sich
  auf jedem Computer lesen lässt. Das Papierformat (A4 in Europa, Letter in
  den USA) und die Ränder stellen Sie unter <menu|Document|Page> ein.

  <paragraph*|Laden>

  Um Ihren Text nach dem Beenden von <TeXmacs> wieder zu öffnen, verwenden
  Sie <menu|File|Load>, den Eintrag <menu|Load> des Symbolmenüs
  <icon|tm_open.png> oder das Tastenkürzel
  <shortcut|(open-document)>. Es erscheint ein Dateiauswahldialog, in dem
  Sie die Datei <verbatim|test.tm> auswählen und bestätigen oder einfach
  doppelt darauf klicken. Ihre zuletzt verwendeten Dokumente sind auÿerdem
  unter <menu|File|Recent> aufgeführt.

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
