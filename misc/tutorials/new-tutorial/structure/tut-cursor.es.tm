<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|El cursor y la estructura>

  Considere la frase siguiente, en la que las palabras \Pbold blue\Q se han
  escrito en negrita (con <shortcut|(toggle-bold)>) y después en azul (con
  <menu|Format|Color>):

  <\quote-env>
    Some <with|font-series|bold|<with|color|blue|bold blue>> text.
  </quote-env>

  Las palabras \Pbold blue\Q están dentro de dos elementos de marcado: uno
  para la negrita y otro para el color azul. ¾Adónde va una letra cuando la
  escribe junto a estas palabras? <TeXmacs> responde a esta pregunta dando
  al cursor varias posiciones en el borde del marcado, que se ven iguales
  en la pantalla pero son distintas en el documento.

  Coloque el cursor después de \PSome \Q y pulse <key*|right> varias veces
  mientras observa la barra de estado y los recuadros alrededor del cursor:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Cursor>|<cell|Posición>>|<row|<cell|Some
  <text-cursor><with|font-series|bold|<with|color|blue|bold blue>>
  text.>|<cell|fuera del marcado>>|<row|<cell|Some
  <with|font-series|bold|<small-focus|<text-cursor><with|color|blue|bold
  blue>>> text.>|<cell|dentro de la negrita>>|<row|<cell|Some
  <with|font-series|bold|<small-envbox|<small-focus|<with|color|blue|<text-cursor>bold
  blue>>>> text.>|<cell|dentro de la negrita y del color
  azul>>|<row|<cell|Some <with|font-series|bold|<with|color|blue|b<text-cursor>old
  blue>> text.>|<cell|después de la letra <verbatim|b>>>>>>>
    Cómo mover el cursor dentro de \Pbold blue\Q.
  </big-table>

  Las primeras pulsaciones de <key*|right> no mueven el cursor en la
  pantalla, sino que lo mueven dentro del marcado, un nivel cada vez. Cuando
  escribe una letra en una de estas posiciones, toma el tipo de letra y el
  color de esa posición: en la primera fila, una letra negra ordinaria; en
  la segunda, una letra negra en negrita; y en la tercera, una letra azul
  en negrita. Lo mismo ocurre al final de \Pbold blue\Q.

  Como hemos visto, el elemento de marcado más interno alrededor del cursor
  está rodeado por un recuadro cian, y los exteriores por recuadros gris
  claro; la parte derecha de la barra de estado los enumera todos. Con un
  poco de práctica, estos recuadros le permiten ver de un vistazo adónde
  irá su próxima letra.

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
