<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Macros con argumentos y el preámbulo>

  <paragraph*|El preámbulo>

  Las macros que define se guardan en el <em|preámbulo> de su documento,
  una parte oculta situada al principio. Puede verlo con
  <menu|Tools|Macros|Edit preamble>, que solo muestra el preámbulo, y lo
  muestra como <em|código fuente>: en lugar de su resultado, ve las propias
  definiciones. Después de definir la macro <markup|hi> de la sección
  anterior, el preámbulo contiene

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>
  </tm-fragment>

  Esto se lee: <em|asignar> al nombre <verbatim|hi> la <em|macro> cuyo
  cuerpo es <verbatim|Hello world!>. Seleccione de nuevo
  <menu|Tools|Macros|Edit preamble> para volver a su texto.

  <paragraph*|Una macro con un argumento>

  Definamos ahora una macro <markup|love>, tal que
  <inactive*|<love|free software>> produzca el texto \PI love free software
  <with|color|red|<math|\<heartsuit\>>>\Q. El texto que sigue a \PI love\Q
  varía, así que se convierte en un <em|argumento> de la macro, al que
  llamamos <src-arg|name>. La definición es

  <\tm-fragment>
    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  En el editor de macros (<menu|Tools|Macros|New macro>), introduciría el
  nombre <verbatim|love> de la macro, luego le daría un argumento y
  escribiría el cuerpo. También puede escribir la definición directamente
  en el preámbulo, de la misma manera que el código fuente de cualquier
  documento:

  <\enumerate>
    <item>Elija <menu|Tools|Macros|Edit preamble> y coloque el cursor al
    final del preámbulo.

    <item>Escriba <key|\\ a s s i g n> y <key|return>: aparece una etiqueta
    <markup|assign> vacía. Escriba el nombre <verbatim|love> en su primer
    campo, y pase al segundo campo con <key*|right>.

    <item>Escriba <key|\\ m a c r o> y <key|return>, y luego el nombre del
    argumento, <verbatim|name>. Pulse <key|tab>: se inserta un nuevo campo
    a continuación, para el cuerpo de la macro.

    <item>En el cuerpo, escriba \PI love \Q y luego el nombre del argumento
    después de la tecla <key|\\>, <key|\\ n a m e>: la barra de estado
    indica que <key|return> insertará el argumento <verbatim|name>. Pulse
    <key|return>: el valor del argumento aparecerá en este lugar. Escriba un espacio
    y, por último, añada el corazón: seleccione el color rojo en
    <menu|Format|Color>, pulse <key|$> y escriba <key|\\ h e a r t s u i t>
    y <key|return>.

    <item>Salga del preámbulo con <menu|Tools|Macros|Edit preamble>.
  </enumerate>

  Cuando escriba ahora <key|\\ l o v e> y <key|return> en su texto, aparece
  un recuadro con un campo, en el que escribe el argumento:

  <\big-envbox>
    I love <small-focus|free software<text-cursor>>
    <with|color|red|<math|\<heartsuit\>>>
  </big-envbox>

  El documento <verbatim|examples/love.tm> contiene las macros de este
  capítulo y algunas aplicaciones de ellas.

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
