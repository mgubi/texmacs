<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Das Fenster von <TeXmacs>>

  Die folgende Abbildung zeigt die wichtigsten Teile eines
  <TeXmacs>-Fensters. Wir werden in diesem Tutorial häufig darauf verweisen.

  <\big-figure>
    <tabular|<tformat|<twith|table-width|0.9par>|<twith|table-hmode|exact>|<cwith|1|-1|1|1|cell-hyphen|n>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|-1|-1|1|1|cell-bborder|1ln>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|2|2|cell-halign|l>|<cwith|1|-1|2|2|cell-lsep|1em>|<cwith|1|-1|1|-1|cell-valign|c>|<cwith|1|1|1|1|cell-background|pastel
    blue>|<cwith|1|1|1|1|cell-halign|c>|<cwith|2|5|1|1|cell-background|light
    grey>|<cwith|2|5|1|1|cell-bborder|0.5ln>|<cwith|6|6|1|1|cell-height|6em>|<cwith|6|6|1|1|cell-vmode|exact>|<cwith|6|6|1|1|cell-valign|t>|<cwith|6|6|1|1|cell-tsep|1em>|<cwith|6|6|1|1|cell-lsep|1.5em>|<cwith|7|7|1|1|cell-background|light
    grey>|<cwith|7|7|1|1|cell-tborder|0.5ln>|<table|<row|<cell|<with|font-family|ss|test.tm>>|<cell|<em|Titelleiste>>>|<row|<cell|<with|font-size|0.84|<menu-item|File><space|0.5em><menu-item|Edit><space|0.5em><menu-item|Insert><space|0.5em><menu-item|Focus><space|0.5em><menu-item|Format><space|0.5em><menu-item|Document><space|0.5em><menu-item|View><space|0.5em><menu-item|Go><space|0.5em><menu-item|Tools><space|0.5em><menu-item|Help>>>|<cell|<em|Menüleiste>>>|<row|<cell|<icon|tm_new.png> <icon|tm_open.png>
    <icon|tm_save.png> <icon|tm_print.png> \ <icon|tm_copy.png>
    <icon|tm_undo.png>>|<cell|<em|Hauptsymbolleiste>>>|<row|<cell|<with|font-family|ss|<em|Werkzeuge
    für den aktuellen Modus (Text, Mathematik ...)>>>|<cell|<em|Modus-Symbolleiste>>>|<row|<cell|<with|font-family|ss|<em|Werkzeuge
    für das Tag am Cursor>>>|<cell|<em|Fokus-Symbolleiste>>>|<row|<cell|<text-cursor>Hello>|<cell|<em|Arbeitsfläche>
    mit dem <em|Cursor>>>|<row|<cell|<with|font-family|ss|text roman 10<htab|1em>start>>|<cell|<em|Statusbalken>>>>>>
  <|big-figure>
    Die wichtigsten Teile des <TeXmacs>-Fensters.
  </big-figure>

  Die <em|Titelleiste> zeigt den Namen Ihres Dokuments an. Solange Sie
  Ihrem Dokument keinen Namen gegeben haben, heiÿt es <verbatim|No name [1]>
  (bzw. <verbatim|No name [2]> für das nächste neue Dokument usw.). An den
  Seiten der Titelleiste können Sie das Fenster mit den
  <em|Fensterdekorationen> minimieren, maximieren oder schlieÿen. Sie werden
  von Ihrem Betriebssystem gezeichnet, nicht von <TeXmacs>, und sehen daher
  je nach System unterschiedlich aus.

  Ihr Text selbst wird auf der <em|Arbeitsfläche> angezeigt, die Ihr
  Dokument so darstellt, wie es gedruckt wird. Wenn der Text nicht in das
  Fenster passt, können Sie mit den <em|Bildlaufleisten>, dem Mausrad oder
  Ihrem Touchpad durch ihn blättern. Der <em|Cursor> (auch
  <em|Einfügemarke> genannt) zeigt die Stelle im Text an, an der Sie
  schreiben; er wird im Text rot und in mathematischen Formeln violett
  dargestellt. Sie können ihn mit den Pfeiltasten oder durch einen
  Mausklick bewegen.

  Die <em|Menüs> in der <em|Menüleiste> bieten Zugang zu allen Funktionen
  von <TeXmacs>. So können Sie zum Beispiel mit dem Menü <menu|File>
  Dokumente erstellen, laden, speichern, exportieren und drucken, mit dem
  Menü <menu|Insert> neue Inhalte wie Abschnitte, Listen, Formeln, Tabellen
  und Bilder einfügen, mit dem Menü <menu|Format> die Schrift oder die
  Farbe Ihres Textes ändern und mit dem Menü <menu|Document> die globalen
  Eigenschaften Ihres Dokuments ändern, etwa seinen Stil, seine Sprache oder
  sein Seitenformat. Unter <name|macOS> befindet sich die Menüleiste am
  oberen Rand des Bildschirms statt am oberen Rand des Fensters.

  Die <em|Symbole> in den <em|Symbolleisten> ermöglichen einen schnelleren
  Zugriff auf die häufigsten Aktionen. Die Hauptsymbolleiste ändert sich
  kaum; die <em|Modus-Symbolleiste> ändert sich, wenn Sie vom Text zur
  Mathematik wechseln, und die <em|Fokus-Symbolleiste> ändert sich mit der
  Auszeichnung, in der sich der Cursor befindet, wie wir im Kapitel über
  strukturierten Text sehen werden. Viele Symbole sind eigentlich
  <em|Symbolmenüs>: Wenn Sie darauf klicken, erscheint ein Menü. Mit dem
  Menü <menu|View> können Sie die Symbolleisten aus- und einblenden.

  Der <em|Statusbalken> am unteren Rand des Fensters zeigt nützliche
  Informationen über Ihr Dokument an. Sein linker Teil sagt Ihnen, ob Sie
  gerade Text oder Mathematik eingeben und welche Schrift und Schriftgröÿe
  an der Position des Cursors verwendet werden. Sein
  rechter Teil beschreibt die Position des Cursors im Dokument: Hier steht
  der Cursor am Anfang (<verbatim|start>) eines Absatzes; später zeigt er
  Ihnen, welches Zeichen Sie gerade eingegeben haben und in welchen
  Auszeichnungen sich der Cursor befindet. Der Statusbalken dient auch zur
  Anzeige von Meldungen, zum Beispiel wenn ein Dokument gespeichert wurde.

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
