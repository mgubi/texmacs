<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Den Mathematikmodus aufrufen und verlassen>

  <TeXmacs> unterscheidet zwischen gewöhnlichem Text und mathematischen
  Formeln. In Formeln werden Buchstaben kursiv gesetzt, die Abstände um die
  Symbole folgen den mathematischen Konventionen, und die Tastatur verhält
  sich anders, sodass sich Symbole schnell eingeben lassen. Um eine Formel
  einzugeben, müssen Sie daher zuerst in den <em|Mathematikmodus>
  wechseln.

  <paragraph*|Formeln im Text>

  Nehmen wir an, Sie haben \PHere comes a formula:\Q getippt und möchten
  nun eine kleine Formel in derselben Zeile eingeben. Wählen Sie
  <menu|Insert|Mathematics|Inline formula>, oder drücken Sie einfach die
  Taste <key|$>:

  <\big-envbox>
    Here comes a formula: <math|<small-focus|<math-cursor>>>
  </big-envbox>

  Dabei ist einiges geschehen: Der Cursor ist violett geworden, eine leere
  Formel, umgeben von einem cyanfarbenen Kasten, wurde eingefügt, der linke
  Teil des Statusbalkens zeigt nun <verbatim|math> statt <verbatim|text>
  an, und die Modus-Symbolleiste enthält Symbole mit mathematischen
  Zeichen. Alles, was Sie jetzt eingeben, landet in der Formel, bis Sie sie
  verlassen. Am einfachsten verlassen Sie die Formel, indem Sie den Cursor
  an ihrem Ende mit <key*|right> aus ihr herausbewegen.

  <paragraph*|Abgesetzte Formeln>

  Gröÿere Formeln stellt man besser in einer eigenen Zeile dar, etwa

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>.
  </equation*>

  Solche <em|abgesetzten Formeln> fügen Sie mit
  <menu|Insert|Mathematics|Displayed formula> oder
  <shortcut|(make-equation*)> ein. Mit
  <shortcut|(numbered-toggle (focus-tree))> können Sie einer abgesetzten
  Formel eine Nummer geben, und mit <shortcut|(variant-circulate
  (focus-tree) #t)> verwandeln Sie eine Formel im Text in eine abgesetzte
  Formel (oder umgekehrt).

  In den nächsten Abschnitten sehen wir, wie man den Inhalt von Formeln
  eingibt.

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
