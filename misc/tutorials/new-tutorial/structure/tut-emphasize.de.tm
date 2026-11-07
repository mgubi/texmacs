<TeXmacs|2.1.4>

<style|<tuple|tmdoc|german>>

<\body>
  <tmdoc-title|Text hervorheben>

  Nehmen wir an, Sie möchten den Satz

  <\quote-env>
    A <em|gnu> is a hairy animal, which is not a clone of a unix.
  </quote-env>

  schreiben, in dem das Wort \Pgnu\Q hervorgehoben ist. Statt auf eine
  kursive Schrift umzuschalten, teilen Sie <TeXmacs> mit, dass das Wort
  <em|hervorgehoben> ist, indem Sie es in das Tag <markup|em> setzen. Dafür
  gibt es mehrere Möglichkeiten.

  <paragraph*|Über die Menüs>

  Tippen Sie \PA \Q und wählen Sie <menu|Insert|Content tag|Emphasize>. Es
  erscheint ein leerer hervorgehobener Bereich, umgeben von einem
  cyanfarbenen Kasten, und der Statusbalken zeigt an, dass sich der Cursor
  in <verbatim|em> befindet:

  <\big-envbox>
    A <em|<small-focus|<text-cursor>>>
  </big-envbox>

  Tippen Sie das Wort \Pgnu\Q: Es erscheint kursiv, da es im
  hervorgehobenen Bereich steht.

  <\big-envbox>
    A <em|<small-focus|gnu<text-cursor>>>
  </big-envbox>

  Um mit gewöhnlichem Text fortzufahren, drücken Sie <key*|right>: Der
  Cursor verlässt den hervorgehobenen Bereich (der cyanfarbene Kasten
  verschwindet), ohne sich auf dem Bildschirm zu bewegen. Tippen Sie nun den
  Rest des Satzes.

  <paragraph*|Mit einem Hybridbefehl>

  Statt das Menü zu verwenden, können Sie auch den Namen des Tags nach der
  Taste <key|\\> eingeben. Tippen Sie <key|\\ e m>: Der Statusbalken meldet
  nun, dass ein Druck auf <key|return> den Text hervorheben wird. Drücken
  Sie <key|return>, und Sie befinden sich in derselben Situation wie oben.
  Das funktioniert für jedes Tag, dessen Namen Sie kennen, und auch für die
  <LaTeX>-Befehle, die eine Entsprechung in <TeXmacs> haben; so erzeugt zum
  Beispiel <key|\\ s e c t i o n> eine Abschnittsüberschrift.

  <paragraph*|Über die Auswahl>

  Wenn der Text schon getippt ist, wählen Sie schlieÿlich das Wort
  \Pgnu\Q mit der Maus aus und wählen dann
  <menu|Insert|Content tag|Emphasize>: Der ausgewählte Text wird in das Tag
  gesetzt.

  <paragraph*|Warum nicht einfach kursiv?>

  Hervorgehobener Text sieht meist wie kursiver Text aus, aber darüber
  entscheidet der Stil. In einem Satz zum Beispiel, der bereits kursiv
  gesetzt ist, wird hervorgehobener Text aufrecht gesetzt, damit er sichtbar
  bleibt:

  <\quote-env>
    <\theorem>
      A <em|gnu> is a hairy animal.
    </theorem>
  </quote-env>

  Ebenso kennzeichnet das Tag <markup|strong>
  (<menu|Insert|Content tag|Strong>) <strong|wichtigen> Text, und das Tag
  <markup|name> (<menu|Insert|Content tag|Name>) kennzeichnet Namen von
  Personen wie <name|Joris>. Wenn Sie später entscheiden, dass wichtiger
  Text rot statt fett dargestellt werden soll, müssen Sie nur den Stil
  ändern und nicht die vielen Stellen, an denen <markup|strong> verwendet
  wurde.

  Das Beispiel dieses Abschnitts finden Sie in
  <verbatim|examples/hairy.tm>.

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
