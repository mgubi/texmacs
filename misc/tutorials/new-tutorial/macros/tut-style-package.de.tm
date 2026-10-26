<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Ihre Makros in anderen Dokumenten wiederverwenden>

  Die Makros der Präambel existieren nur in ihrem Dokument. Wenn Sie sie
  auch in anderen Dokumenten verwenden möchten, legen Sie sie in einem
  <em|Stilpaket> ab: einer Datei mit der Endung <verbatim|.ts>, die
  Makrodefinitionen enthält und die Sie zu jedem Dokument hinzufügen
  können.

  <paragraph*|Das Paket erstellen>

  Öffnen Sie das Dokument mit Ihren Makros und wählen Sie
  <menu|Tools|Macros|Extract style package>. <TeXmacs> öffnet ein neues
  Dokument im Stil <tmstyle|source>, das einen Kopf (mit dem Paketnamen
  <verbatim|love-macros>, den Sie in <verbatim|love> ändern können), die
  Stilparameter Ihres Dokuments, falls vorhanden, und alle Definitionen der
  Präambel enthält:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>

    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  Speichern Sie dieses Dokument im Ordner <verbatim|packages> Ihres
  persönlichen <TeXmacs>-Ordners unter dem Namen <verbatim|love.ts>. Dieser
  persönliche Ordner ist <verbatim|~/.TeXmacs> unter <name|Linux> und
  <name|macOS> (also der Ordner <verbatim|.TeXmacs> in Ihrem
  Benutzerverzeichnis) und <verbatim|%APPDATA%\\TeXmacs> unter
  <name|Windows>; <TeXmacs> legt darin beim Start den Ordner
  <verbatim|packages> an. Unter <name|macOS> blendet der
  Dateiauswahldialog Ordner aus, deren Name mit einem Punkt beginnt:
  Drücken Sie <verbatim|Cmd-Shift-.>, um <verbatim|.TeXmacs> anzuzeigen.
  Das Beispielpaket dieses Tutorials ist <verbatim|examples/love.ts>.

  <paragraph*|Das Paket verwenden>

  In jedem Dokument können Sie nun
  <menu|Document|Style|Add package|Add other package> wählen und
  <verbatim|love> eingeben: Die Makros <markup|hi> und <markup|love> stehen
  dann zur Verfügung, genau so, als wären sie in der Präambel des Dokuments
  definiert worden. Wenn Sie das Paket später ändern, übernehmen alle
  Dokumente, die es verwenden, diese Änderung. Ein Paket entfernen Sie aus
  einem Dokument im Menü <menu|Document|Style>.

  Sie können ein Stilpaket auch von Grund auf neu erstellen, indem Sie für
  ein neues Dokument <menu|Document|Style|source> wählen, die Definitionen
  eingeben und es mit der Endung <verbatim|.ts> speichern. Wie man
  Stildateien schreibt, wird ausführlich unter
  <menu|Help|Manual|Writing your own style files> erklärt.

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
