<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Secciones>

  Los documentos más largos se dividen en secciones. Para empezar una nueva
  sección, seleccione <menu|Insert|Section|Section>, o escriba
  <key|\\ s e c t i o n> seguido de <key|return>. El cursor está ahora
  dentro de un título de sección vacío, y la barra de estado indica
  <verbatim|section>:

  <\big-envbox>
    <section*|1<space|1em><small-focus|<text-cursor>>>
  </big-envbox>

  Escriba el título de la sección, por ejemplo \PMy new section\Q, y pulse
  <key|return>. El cursor sale del título y pasa a un nuevo párrafo, donde
  escribe el cuerpo de su sección:

  <\big-envbox>
    <section*|1<space|1em>My new section>

    The body of my section.<text-cursor>
  </big-envbox>

  Las secciones se numeran automáticamente, tanto en el estilo
  <tmstyle|generic> como en el estilo <tmstyle|article>. Si
  inserta una nueva sección antes de las existentes, las secciones
  siguientes se vuelven a numerar, y también las referencias a ellas. La
  forma en que se numeran y se muestran las secciones depende únicamente
  del estilo: cuando pasa de <tmstyle|article> a <tmstyle|book>, las
  secciones conservan sus títulos pero adoptan la presentación del estilo
  de libro.

  Del mismo modo, <menu|Insert|Section> contiene subsecciones y párrafos, y
  en el estilo <tmstyle|book> <menu|Insert|Chapter|Chapter> empieza un nuevo capítulo. Un
  índice se genera con <menu|Insert|Automatic|Table of
  contents>, seguido de <menu|Document|Update|All>.

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
