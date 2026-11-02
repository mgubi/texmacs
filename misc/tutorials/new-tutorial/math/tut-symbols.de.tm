<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Griechische Buchstaben und mathematische Symbole>

  In einer Formel erhalten Sie die meisten mathematischen Symbole mit
  kurzen Tastenfolgen. Während Sie eine solche Folge eingeben, zeigt der
  Statusbalken die bisher getippten Tasten und das Symbol an, das sie
  erzeugen.

  <paragraph*|Griechische Buchstaben>

  Einen griechischen Buchstaben erhalten Sie, indem Sie den entsprechenden
  lateinischen Buchstaben und danach <key|var> tippen. Um zum Beispiel die
  Formel <math|\<lambda\>*ln x> einzugeben, gehen Sie wie folgt vor:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Tasten>|<cell|Formel>|<cell|Bemerkung>>|<row|<cell|<key|l>>|<cell|<math|l<math-cursor>>>|<cell|ein
  gewöhnlicher Buchstabe <math|l>>>|<row|<cell|<key|var>>|<cell|<math|\<lambda\><math-cursor>>>|<cell|aus
  dem <math|l> wird ein <math|\<lambda\>>>>|<row|<cell|<key|*>>|<cell|<math|\<lambda\>*<math-cursor>>>|<cell|eine
  unsichtbare Multiplikation>>|<row|<cell|<key|l n>>|<cell|<math|\<lambda\>*ln<math-cursor>>>|<cell|der
  Name des Logarithmus>>|<row|<cell|<key|space>>|<cell|<math|\<lambda\>*ln
  <math-cursor>>>|<cell|eine Funktionsanwendung>>|<row|<cell|<key|x>>|<cell|<math|\<lambda\>*ln
  x<math-cursor>>>|<cell|das Argument>>>>>>
    Eingabe der Formel <math|\<lambda\>*ln x>.
  </big-table>

  Beachten Sie, dass die mit <key|*> eingegebene Multiplikation und die mit
  <key|space> eingegebene Funktionsanwendung unsichtbar sind, <TeXmacs>
  aber mitteilen, was Ihre Formel bedeutet. Wenn Sie <key|var> mehrmals
  drücken, erhalten Sie die Varianten des Buchstabens: <key|e var> ergibt
  <math|\<varepsilon\>>, <key|e var var> ergibt die Konstante
  <math|\<mathe\>> und <key|e var var var> ergibt <math|\<epsilon\>>.
  Griechische Groÿbuchstaben erhalten Sie auf dieselbe Weise: <key|G var>
  ergibt <math|\<Gamma\>>.

  <paragraph*|Pfeile und andere Symbole>

  Die meisten Symbole erhalten Sie, indem Sie sie mit der Tastatur
  \Pzeichnen\Q. So ergibt zum Beispiel <key|- \<gtr\>> das Symbol
  <math|\<rightarrow\>>, <key|= \<gtr\>> ergibt <math|\<Rightarrow\>>,
  <key|\<less\> => ergibt <math|\<leqslant\>> und <key|= /> ergibt
  <math|\<neq\>>. Allgemein wird ein Symbol durch ein nachfolgendes <key|/>
  <em|verneint>, und <key|@> vor einem Symbol setzt es in einen Kreis:
  <key|@ +> ergibt <math|\<oplus\>> und <key|@ x> ergibt <math|\<otimes\>>.

  <paragraph*|Varianten>

  Wie bei den griechischen Buchstaben schaltet die Taste <key|var> durch
  die Varianten eines Symbols. Wenn Sie zum Beispiel mit <key|\<less\>>
  beginnen und mehrmals <key|var> drücken, erhalten Sie nacheinander:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Tasten>|<cell|Symbol>>|<row|<cell|<key|\<less\>>>|<cell|<math|\<less\>>>>|<row|<cell|<key|\<less\>
  var>>|<cell|<math|\<in\>>>>|<row|<cell|<key|\<less\> var
  var>>|<cell|<math|\<subset\>>>>|<row|<cell|<key|\<less\> var var
  var>>|<cell|<math|\<prec\>>>>|<row|<cell|<key|\<less\> var var var
  =>>|<cell|<math|\<preccurlyeq\>>>>|<row|<cell|<key|\<less\> var var var =
  var>>|<cell|<math|\<preceq\>>>>|<row|<cell|<key|\<less\> var var var = var
  />>|<cell|<math|\<npreceq\>>>>>>>>
    Varianten des Symbols <math|\<less\>>.
  </big-table>

  Wenn Sie zu weit gegangen sind, kehrt <key|S-var> zur vorherigen Variante
  zurück.

  <paragraph*|Symbole über ihren Namen>

  Wenn Sie den <LaTeX>-Namen eines Symbols kennen, können Sie ihn auch nach
  der Taste <key|\\> eingeben. Wenn Sie zum Beispiel
  <key|\\ h e a r t s u i t> und dann <key|return> tippen, erhalten Sie
  <math|\<heartsuit\>>. Während Sie den Namen eingeben, zeigt Ihnen der
  Statusbalken, welches Symbol beim Drücken von <key|return> eingefügt
  wird.

  <paragraph*|Symbolmenüs>

  Schlieÿlich stehen alle Symbole auch in den Symbolmenüs der
  Modus-Symbolleiste und unter <menu|Insert|Symbol> zur Verfügung. Wenn Sie
  den Mauszeiger in diesen Menüs auf einem Symbol ruhen lassen, zeigt ein
  Tooltip das zugehörige Tastenkürzel an, sodass Sie die Tastenkürzel der
  Symbole lernen können, die Sie häufig verwenden.

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
