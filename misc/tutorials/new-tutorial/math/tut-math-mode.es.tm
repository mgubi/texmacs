<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Cómo entrar en el modo matemático y salir de él>

  <TeXmacs> distingue entre el texto ordinario y las fórmulas matemáticas.
  Dentro de las fórmulas, las letras se componen en cursiva, el espaciado
  alrededor de los símbolos sigue las convenciones matemáticas y el teclado
  se comporta de otra manera, para que los símbolos se puedan escribir
  rápidamente. Por tanto, para escribir una fórmula, primero tiene que
  entrar en el <em|modo matemático>.

  <paragraph*|Fórmulas en línea>

  Supongamos que ha escrito \PHere comes a formula:\Q y que ahora quiere
  escribir una pequeña fórmula dentro de la misma línea. Seleccione
  <menu|Insert|Mathematics|Inline formula>, o simplemente pulse la tecla
  <key|$>:

  <\big-envbox>
    Here comes a formula: <math|<small-focus|<math-cursor>>>
  </big-envbox>

  Han ocurrido varias cosas: el cursor se ha vuelto morado, se ha insertado
  una fórmula vacía rodeada por un recuadro cian, la parte izquierda de la
  barra de estado indica ahora <verbatim|math> en lugar de <verbatim|text>,
  y la barra de herramientas de modo muestra iconos con símbolos
  matemáticos. Todo lo que escriba a partir de ahora irá a la fórmula,
  hasta que salga de ella. La forma más sencilla de salir de la fórmula es
  sacar el cursor de ella con <key*|right> cuando esté al final.

  <paragraph*|Fórmulas destacadas>

  Las fórmulas más grandes se muestran mejor en una línea aparte, como

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>.
  </equation*>

  Estas <em|fórmulas destacadas> se insertan con
  <menu|Insert|Mathematics|Displayed formula> o
  <shortcut|(make-equation*)>. Puede numerar una fórmula destacada con
  <shortcut|(numbered-toggle (focus-tree))>, y convertir una fórmula en
  línea en una destacada (o al revés) con <shortcut|(variant-circulate
  (focus-tree) #t)>.

  En las secciones siguientes veremos cómo escribir el contenido de las
  fórmulas.

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
