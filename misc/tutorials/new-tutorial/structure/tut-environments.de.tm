<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Sätze, Beweise und Listen>

  Mathematische Texte bestehen aus Definitionen, Sätzen, Beweisen,
  Bemerkungen und so weiter. In <TeXmacs> ist jedes dieser Elemente eine
  <em|Umgebung>, die Sie unter <menu|Insert|Enunciation> finden: <TeXmacs>
  nummeriert sie und stellt sie entsprechend dem Stil des Dokuments dar.
  Geben wir den folgenden Text ein, der auch als
  <verbatim|examples/ramsey.tm> vorliegt:

  <\big-envbox>
    <\theorem>
      Among six people, there are always three people who mutually know
      each other or three people who mutually do not know each other.
    </theorem>

    <\proof>
      Let <math|P> be an arbitrary person among the six. Then either one of
      the following cases holds:

      <\description>
        <item*|Case 1><math|P> knows at least three other persons, which
        may be called <math|Q>, <math|R> and <math|S>.

        <item*|Case 2><text-cursor>
      </description>
    </proof>
  </big-envbox>

  <paragraph*|Die Umgebungen erstellen>

  Wählen Sie <menu|Insert|Enunciation|Theorem> und tippen Sie die Aussage
  des Satzes; das Wort \PSatz\Q und seine Nummer fügt <TeXmacs> hinzu.
  Verlassen Sie dann den Satz, indem Sie den Cursor mit <key*|right> über
  sein Ende hinaus bewegen oder <shortcut|(structured-exit-right)>
  verwenden, und wählen Sie <menu|Insert|Enunciation|Proof>. Im Beweis
  beginnen Sie mit <menu|Insert|Description|Default> eine beschreibende
  Liste: Jedes Mal, wenn Sie <key|return> drücken, wird ein neuer Eintrag
  erzeugt, dessen Bezeichnung Sie vor seinem Text eingeben.

  <paragraph*|Der Kontext des Cursors>

  An der Position des Cursors im obigen Beispiel zeigt der rechte Teil des
  Statusbalkens <verbatim|proof description item*> an: Der Cursor steht in
  einem Eintrag einer beschreibenden Liste innerhalb eines Beweises. Dieser
  <em|Kontext> bestimmt auch den Inhalt des Menüs <menu|Focus> und der
  Fokus-Symbolleiste, die die Aktionen anbieten, die für die innerste
  Umgebung sinnvoll sind.

  <paragraph*|Eine Umgebung ändern>

  Da <TeXmacs> weiÿ, dass die Aussage ein Satz ist, können Sie es sich
  leicht anders überlegen. Setzen Sie den Cursor in den Satz und drücken
  Sie <shortcut|(variant-circulate (focus-tree) #t)>, oder verwenden Sie
  die Fokus-Symbolleiste: Aus dem Satz wird eine Proposition, ein Lemma,
  ein Korollar ..., während sein Text unverändert bleibt. Ebenso entfernt
  <shortcut|(numbered-toggle (focus-tree))> die Nummer des Satzes oder
  stellt sie wieder her.

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
