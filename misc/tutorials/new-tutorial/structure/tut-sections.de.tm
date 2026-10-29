<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Abschnitte>

  Längere Dokumente werden in Abschnitte gegliedert. Um einen neuen
  Abschnitt zu beginnen, wählen Sie <menu|Insert|Section|Section>, oder
  tippen Sie <key|\\ s e c t i o n> und danach <key|return>. Der Cursor
  befindet sich nun in einer leeren Abschnittsüberschrift, und der
  Statusbalken zeigt <verbatim|section> an:

  <\big-envbox>
    <section*|1<space|1em><small-focus|<text-cursor>>>
  </big-envbox>

  Tippen Sie die Überschrift des Abschnitts, etwa \PMy new section\Q, und
  drücken Sie <key|return>. Der Cursor verlässt die Überschrift und springt
  in einen neuen Absatz, in dem Sie den Inhalt Ihres Abschnitts eingeben:

  <\big-envbox>
    <section*|1<space|1em>My new section>

    The body of my section.<text-cursor>
  </big-envbox>

  Abschnitte werden automatisch nummeriert, im Stil <tmstyle|generic>
  ebenso wie im Stil <tmstyle|article>. Wenn
  Sie vor den vorhandenen Abschnitten einen neuen einfügen, werden die
  folgenden Abschnitte neu nummeriert, ebenso die Verweise darauf. Wie
  Abschnitte nummeriert und dargestellt werden, hängt nur vom Stil ab: Wenn
  Sie von <tmstyle|article> zu <tmstyle|book> wechseln, behalten die
  Abschnitte ihre Überschriften, erhalten aber das Layout des Buchstils.

  Ebenso enthält <menu|Insert|Section> Unterabschnitte und Paragraphen, und
  im Stil <tmstyle|book> beginnt <menu|Insert|Chapter|Chapter> ein neues
  Kapitel. Ein Inhaltsverzeichnis erzeugen Sie mit
  <menu|Insert|Automatic|Table of contents>, gefolgt von
  <menu|Document|Update|All>.

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
