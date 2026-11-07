<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|La ventana de <TeXmacs>>

  La figura siguiente muestra las partes principales de una ventana de
  <TeXmacs>. Nos referiremos a ellas con frecuencia en este tutorial.

  <\big-figure>
    <tabular|<tformat|<twith|table-width|0.9par>|<twith|table-hmode|exact>|<cwith|1|-1|1|1|cell-hyphen|n>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|-1|-1|1|1|cell-bborder|1ln>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|2|2|cell-halign|l>|<cwith|1|-1|2|2|cell-lsep|1em>|<cwith|1|-1|1|-1|cell-valign|c>|<cwith|1|1|1|1|cell-background|pastel
    blue>|<cwith|1|1|1|1|cell-halign|c>|<cwith|2|5|1|1|cell-background|light
    grey>|<cwith|2|5|1|1|cell-bborder|0.5ln>|<cwith|6|6|1|1|cell-height|6em>|<cwith|6|6|1|1|cell-vmode|exact>|<cwith|6|6|1|1|cell-valign|t>|<cwith|6|6|1|1|cell-tsep|1em>|<cwith|6|6|1|1|cell-lsep|1.5em>|<cwith|7|7|1|1|cell-background|light
    grey>|<cwith|7|7|1|1|cell-tborder|0.5ln>|<table|<row|<cell|<with|font-family|ss|test.tm>>|<cell|<em|barra
    de título>>>|<row|<cell|<with|font-size|0.84|<menu-item|File><space|0.5em><menu-item|Edit><space|0.5em><menu-item|Insert><space|0.5em><menu-item|Focus><space|0.5em><menu-item|Format><space|0.5em><menu-item|Document><space|0.5em><menu-item|View><space|0.5em><menu-item|Go><space|0.5em><menu-item|Tools><space|0.5em><menu-item|Help>>>|<cell|<em|barra
    de menús>>>|<row|<cell|<icon|tm_new.png> <icon|tm_open.png>
    <icon|tm_save.png> <icon|tm_print.png> \ <icon|tm_copy.png>
    <icon|tm_undo.png>>|<cell|<em|barra de herramientas
    principal>>>|<row|<cell|<with|font-family|ss|<em|herramientas
    del modo actual (texto, matemáticas...)>>>|<cell|<em|barra de
    herramientas de modo>>>|<row|<cell|<with|font-family|ss|<em|herramientas
    para la etiqueta del cursor>>>|<cell|<em|barra de herramientas de
    foco>>>|<row|<cell|<text-cursor>Hello>|<cell|<em|lienzo>
    con el <em|cursor>>>|<row|<cell|<with|font-family|ss|text roman 10<htab|1em>start>>|<cell|<em|barra de estado>>>>>>
  <|big-figure>
    Las partes principales de la ventana de <TeXmacs>.
  </big-figure>

  La <em|barra de título> muestra el nombre de su documento. Mientras no le
  haya dado un nombre a su documento, este se llama <verbatim|No name [1]>
  (o <verbatim|No name [2]> para el siguiente documento nuevo, y así
  sucesivamente). A los lados de la barra de título, las <em|decoraciones de
  la ventana> le permiten minimizar, maximizar o cerrar la ventana. Las
  dibuja su sistema operativo, no <TeXmacs>, por lo que su aspecto depende
  de su sistema.

  El texto propiamente dicho se muestra en el <em|lienzo>, que presenta su
  documento tal como se imprimirá. Si el texto no cabe en la ventana, puede
  usar las <em|barras de desplazamiento>, la rueda del ratón o el panel
  táctil para desplazarse por él. El <em|cursor> (también llamado
  <em|punto de inserción>) indica la posición del texto en la que está
  escribiendo; se dibuja en rojo en el texto y en morado en las fórmulas
  matemáticas. Puede moverlo con las teclas de flecha o haciendo clic con
  el ratón.

  Los <em|menús> de la <em|barra de menús> dan acceso a todas las funciones
  de <TeXmacs>. Por ejemplo, el menú <menu|File> le permite crear, cargar,
  guardar, exportar e imprimir documentos; el menú <menu|Insert>, insertar
  nuevo material como secciones, listas, fórmulas, tablas e imágenes; el
  menú <menu|Format>, cambiar el tipo de letra o el color del texto; y el
  menú <menu|Document>, cambiar las propiedades globales de su documento,
  como su estilo, su idioma o el tamaño de página. En <name|macOS>, la
  barra de menús se encuentra en la parte superior de la pantalla, en lugar
  de en la parte superior de la ventana.

  Los <em|iconos> de las <em|barras de herramientas> dan un acceso más
  rápido a las acciones más frecuentes. La barra de herramientas principal
  apenas cambia; la <em|barra de herramientas de modo> cambia cuando pasa
  del texto a las matemáticas, y la <em|barra de herramientas de foco>
  cambia según el elemento de marcado en el que se encuentra el cursor,
  como veremos en el capítulo sobre el texto estructurado. Muchos iconos son
  en realidad <em|menús de iconos>: al hacer clic en ellos aparece un menú.
  Puede ocultar o mostrar las barras de herramientas mediante el menú
  <menu|View>.

  La <em|barra de estado>, en la parte inferior de la ventana, muestra
  información útil sobre su documento. Su parte izquierda le indica si está
  escribiendo texto o matemáticas, y qué tipo y tamaño de letra se usan en
  la posición del cursor. Su
  parte derecha describe la posición del cursor en el documento: aquí, el
  cursor está al principio (<verbatim|start>) de un párrafo; más adelante,
  le mostrará qué carácter acaba de escribir y en qué elementos de marcado
  se encuentra el cursor. La barra de estado también sirve para mostrar
  mensajes, por ejemplo cuando se ha guardado un documento.

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
