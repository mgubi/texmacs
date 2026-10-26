<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Sections>

  Les documents assez longs sont divisés en sections. Pour commencer une
  nouvelle section, choisissez <menu|Insert|Section|Section>, ou tapez
  <key|\\ s e c t i o n> suivi de <key|return>. Le curseur se trouve
  maintenant dans un titre de section vide, et la barre d'état indique
  <verbatim|section> :

  <\big-envbox>
    <section*|1<space|1em><small-focus|<text-cursor>>>
  </big-envbox>

  Tapez le titre de la section, par exemple \PMy new section\Q, et appuyez
  sur <key|return>. Le curseur quitte le titre et passe à un nouveau
  paragraphe, où vous tapez le corps de votre section :

  <\big-envbox>
    <section*|1<space|1em>My new section>

    The body of my section.<text-cursor>
  </big-envbox>

  Les sections sont numérotées automatiquement, dans le style
  <tmstyle|generic> comme dans le style <tmstyle|article>. Si vous insérez
  une nouvelle section avant les sections existantes, les sections suivantes
  sont renumérotées, de même que les références à celles-ci. La façon dont
  les sections sont numérotées et affichées ne dépend que du style :
  lorsque vous passez de <tmstyle|article> à <tmstyle|book>, les sections
  conservent leurs titres mais prennent la mise en page du style livre.

  De même, <menu|Insert|Section> contient les sous-sections et les
  paragraphes, et dans le style <tmstyle|book>,
  <menu|Insert|Chapter|Chapter> commence un nouveau chapitre. Une table des
  matières est produite par <menu|Insert|Automatic|Table of contents>,
  suivi de <menu|Document|Update|All>.

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
