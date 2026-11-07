<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Teoremas, demostraciones y listas>

  Los textos matemáticos se componen de definiciones, teoremas,
  demostraciones, observaciones, etc. En <TeXmacs>, cada uno de ellos es un
  <em|entorno>, que se encuentra en <menu|Insert|Enunciation>: <TeXmacs>
  los numera y los muestra según el estilo del documento. Escribamos el
  texto siguiente, que también está disponible como
  <verbatim|examples/ramsey.tm>:

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

  <paragraph*|Cómo crear los entornos>

  Seleccione <menu|Insert|Enunciation|Theorem> y escriba el enunciado del
  teorema; <TeXmacs> añade la palabra \PTeorema\Q y su número. Después
  salga del teorema llevando el cursor más allá de su final con
  <key*|right>, o con <shortcut|(structured-exit-right)>, y seleccione
  <menu|Insert|Enunciation|Proof>. Dentro de la demostración, se empieza
  una lista descriptiva con <menu|Insert|Description|Default>: cada vez que
  pulsa <key|return>, se crea un nuevo elemento, cuya etiqueta escribe
  antes de su texto.

  <paragraph*|El contexto del cursor>

  En la posición del cursor del ejemplo anterior, la parte derecha de la
  barra de estado muestra <verbatim|proof description item*>: el cursor
  está en un elemento de una descripción, dentro de una demostración. Este
  <em|contexto> determina también el contenido del menú <menu|Focus> y de
  la barra de herramientas de foco, que ofrecen las acciones que tienen
  sentido para el entorno más interno.

  <paragraph*|Cómo cambiar un entorno>

  Como <TeXmacs> sabe que el enunciado es un teorema, es fácil cambiar de
  opinión. Coloque el cursor dentro del teorema y pulse
  <shortcut|(variant-circulate (focus-tree) #t)>, o use la barra de
  herramientas de foco: el teorema se convierte en una proposición, un
  lema, un corolario... mientras su texto sigue siendo el mismo. Del mismo
  modo, <shortcut|(numbered-toggle
  (focus-tree))> quita o restablece el número del teorema.

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
