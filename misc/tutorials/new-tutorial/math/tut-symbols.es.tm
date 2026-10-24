<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Letras griegas y símbolos matemáticos>

  Dentro de una fórmula, la mayoría de los símbolos matemáticos se obtienen
  con secuencias cortas de teclas. Mientras escribe una de estas
  secuencias, la barra de estado muestra las teclas escritas hasta el
  momento y el símbolo que producen.

  <paragraph*|Letras griegas>

  Una letra griega se obtiene escribiendo la letra latina correspondiente
  seguida de <key|var>. Por ejemplo, para escribir la fórmula
  <math|\<lambda\>*ln x>, proceda como sigue:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Teclas>|<cell|Fórmula>|<cell|Observación>>|<row|<cell|<key|l>>|<cell|<math|l<math-cursor>>>|<cell|una
  letra ordinaria <math|l>>>|<row|<cell|<key|var>>|<cell|<math|\<lambda\><math-cursor>>>|<cell|la
  <math|l> se convierte en una <math|\<lambda\>>>>|<row|<cell|<key|*>>|<cell|<math|\<lambda\>*<math-cursor>>>|<cell|una
  multiplicación invisible>>|<row|<cell|<key|l n>>|<cell|<math|\<lambda\>*ln<math-cursor>>>|<cell|el
  nombre del logaritmo>>|<row|<cell|<key|space>>|<cell|<math|\<lambda\>*ln
  <math-cursor>>>|<cell|una aplicación de función>>|<row|<cell|<key|x>>|<cell|<math|\<lambda\>*ln
  x<math-cursor>>>|<cell|el argumento>>>>>>
    Cómo escribir la fórmula <math|\<lambda\>*ln x>.
  </big-table>

  Observe que la multiplicación escrita con <key|*> y la aplicación de
  función escrita con <key|space> son invisibles, pero le indican a
  <TeXmacs> lo que significa su fórmula. Pulsar <key|var> varias veces da
  las variantes de la letra: <key|e var> produce <math|\<varepsilon\>>,
  <key|e var var> produce la constante <math|\<mathe\>> y <key|e var var
  var> produce <math|\<epsilon\>>. Las letras griegas mayúsculas se
  obtienen de la misma manera: <key|G var> produce <math|\<Gamma\>>.

  <paragraph*|Flechas y otros símbolos>

  La mayoría de los símbolos se obtienen \Pdibujándolos\Q con el teclado.
  Por ejemplo, <key|- \<gtr\>> produce <math|\<rightarrow\>>,
  <key|= \<gtr\>> produce <math|\<Rightarrow\>>, <key|\<less\> => produce
  <math|\<leqslant\>> y <key|= /> produce <math|\<neq\>>. En general,
  <key|/> después de un símbolo lo <em|niega>, y <key|@> antes de un
  símbolo lo encierra en un círculo: <key|@ +> produce <math|\<oplus\>> y
  <key|@ x> produce <math|\<otimes\>>.

  <paragraph*|Variantes>

  Igual que con las letras griegas, la tecla <key|var> recorre las
  variantes de un símbolo. Por ejemplo, empezando por <key|\<less\>> y
  pulsando <key|var> varias veces, obtiene sucesivamente:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Teclas>|<cell|Símbolo>>|<row|<cell|<key|\<less\>>>|<cell|<math|\<less\>>>>|<row|<cell|<key|\<less\>
  var>>|<cell|<math|\<in\>>>>|<row|<cell|<key|\<less\> var
  var>>|<cell|<math|\<subset\>>>>|<row|<cell|<key|\<less\> var var
  var>>|<cell|<math|\<prec\>>>>|<row|<cell|<key|\<less\> var var var
  =>>|<cell|<math|\<preccurlyeq\>>>>|<row|<cell|<key|\<less\> var var var =
  var>>|<cell|<math|\<preceq\>>>>|<row|<cell|<key|\<less\> var var var = var
  />>|<cell|<math|\<npreceq\>>>>>>>>
    Variantes del símbolo <math|\<less\>>.
  </big-table>

  Si se ha pasado, <key|S-var> vuelve a la variante anterior.

  <paragraph*|Símbolos por su nombre>

  Si conoce el nombre <LaTeX> de un símbolo, también puede escribirlo
  después de la tecla <key|\\>. Por ejemplo, escribir
  <key|\\ h e a r t s u i t> y luego <key|return> produce
  <math|\<heartsuit\>>. Mientras escribe el nombre, la barra de estado le
  indica qué símbolo se insertará cuando pulse <key|return>.

  <paragraph*|Menús de símbolos>

  Por último, todos los símbolos están también disponibles en los menús de
  iconos de la barra de herramientas de modo, y en <menu|Insert|Symbol>.
  Cuando deja el puntero del ratón sobre un símbolo en estos menús, un
  globo de ayuda muestra su atajo de teclado, de modo que puede aprender
  los atajos de los símbolos que usa a menudo.

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
