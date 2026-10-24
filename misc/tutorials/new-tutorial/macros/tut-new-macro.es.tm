<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Una primera macro>

  Definamos una macro <markup|hi> que produzca el texto \PHello
  world!\Q.

  <paragraph*|Cómo definir la macro>

  La forma más sencilla de definir una macro es partir de un ejemplo.
  Escriba el texto <verbatim|Hello world!> en su documento, selecciónelo y
  elija <menu|Tools|Macros|New macro>. Aparece el <em|editor de macros>:
  muestra el cuerpo de la nueva macro, que es el texto seleccionado, debajo
  de un campo para el nombre de la macro:

  <\big-envbox>
    <inline-tag|<with|color|red|font-shape|italic|enter-name>>
    <math|\<assign\>>

    Hello world!
  </big-envbox>

  Haga clic en el campo <with|color|red|font-shape|italic|enter-name> y
  escriba el nombre <verbatim|hi>. Todavía puede cambiar el cuerpo, que
  puede contener cualquier tipo de contenido: texto con formato, fórmulas,
  tablas, imágenes... Después haga clic en <menu|Ok>. La macro <markup|hi>
  queda definida en su documento.

  Si abre el editor de macros sin ninguna selección, tanto el nombre como
  el cuerpo están vacíos, y los escribe de la misma manera.

  <paragraph*|Cómo usar la macro>

  Ahora, dondequiera que quiera que aparezca el texto \PHello world!\Q,
  escriba <key|\\ h i> y <key|return>. Mientras escribe el nombre, la barra
  de estado confirma que existe una macro <markup|hi>. También puede elegir
  <menu|Insert|Macro|hi>: el menú <menu|Insert|Macro> enumera todas las
  macros de su documento.

  Cuando el cursor está junto al texto producido por la macro, la barra de
  estado muestra <verbatim|hi>: <TeXmacs> recuerda que este texto es una
  aplicación de la macro, y no texto ordinario. En particular, si más
  adelante cambia la definición de <markup|hi>, por ejemplo por \PHello
  everybody!\Q, el texto cambia de inmediato en todo su documento. Para
  cambiar la definición, coloque el cursor junto a una aplicación de la
  macro y elija <menu|Focus|Preferences|Edit macro>, o use
  <menu|Tools|Macros|Edit macros>.

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
