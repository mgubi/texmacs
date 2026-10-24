<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Subíndices, superíndices y fracciones>

  Muchas construcciones matemáticas, como los subíndices, los superíndices,
  las fracciones y las raíces, tienen una o varias partes que hay que
  rellenar. <TeXmacs> inserta la construcción con las partes vacías y pone
  el cursor en la primera; usted escribe entonces su contenido y sale de
  ella cuando termina.

  <paragraph*|Subíndices y superíndices>

  Para escribir <math|x<rsup|2>>, escriba <key|x>, luego <key|^> para un
  superíndice, y luego <key|2>. El cursor está ahora dentro del
  superíndice, como indica el recuadro cian:

  <\big-envbox>
    <math|x<rsup|<small-focus|2<math-cursor>>>>
  </big-envbox>

  Todo lo que escriba a partir de ahora irá al superíndice. Pulse
  <key*|right> para sacar el cursor del superíndice, y continúe la fórmula.
  Los subíndices se obtienen de la misma manera con <key|_>, como en
  <math|x<rsub|n>>.

  <paragraph*|Fracciones>

  Para continuar nuestra fórmula hasta <math|x<rsup|2>+<frac|a|b>>, escriba
  <key|+> y luego <key|math f> (o seleccione <menu|Insert|Fraction>).
  Aparece una fracción vacía, con el cursor en el numerador:

  <\big-envbox>
    <math|x<rsup|2>+<frac|<small-focus|a<math-cursor>>|>>
  </big-envbox>

  Escriba <key|a>, pulse <key*|down> para pasar al denominador, y escriba
  <key|b>:

  <\big-envbox>
    <math|x<rsup|2>+<frac|a|<small-focus|b<math-cursor>>>>
  </big-envbox>

  Por último, pulse <key*|right> para salir de la fracción. De la misma
  manera, <key|math s> inserta una raíz cuadrada, como en
  <math|<sqrt|x+1>>, y <key|math s var> una raíz <math|n>-ésima, como en
  <math|<sqrt|x+1|3>>.

  <paragraph*|Cómo eliminar una construcción>

  Si ha insertado una construcción por error, <key*|backspace> al principio
  de una parte vacía la elimina. De forma más general, <TeXmacs> sigue la
  estructura de su fórmula: el recuadro cian le muestra siempre en qué
  parte de qué construcción está el cursor, y la barra de estado enumera
  todas las construcciones que rodean el cursor, como <verbatim|math> y
  <verbatim|denominator>.

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
