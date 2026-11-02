<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Cómo escribir, guardar, imprimir y cargar un texto sencillo>

  <paragraph*|Escribir>

  Una vez que haya dado un nombre a su documento, puede empezar a escribir.
  Escriba su texto como en una máquina de escribir: <TeXmacs> corta
  automáticamente las líneas cuando son demasiado largas, y además las
  justifica, es decir, ajusta los espacios entre las palabras para que
  todas las líneas tengan la misma anchura. Solo tiene que pulsar
  <key|return> para empezar un nuevo párrafo. El texto siguiente es un
  ejercicio sencillo:

  <\big-figure>
    <\big-envbox>
      Hey you there!

      Would you be so kind to enroll the gnus and gnats around you to try
      <TeXmacs> once?<text-cursor>
    </big-envbox>
  <|big-figure>
    Un texto sencillo escrito con <TeXmacs> (<verbatim|examples/test.tm>).
  </big-figure>

  Si se equivoca, <key*|backspace> borra el carácter anterior al cursor, y
  <menu|Edit|Undo> (<shortcut|(undo 0)>) deshace sus últimos cambios. Si su
  texto contiene caracteres que no están en su teclado, como letras
  acentuadas, puede escribirlos con los métodos de su sistema; más adelante
  veremos cómo escribir símbolos matemáticos.

  <paragraph*|Guardar>

  Cuando termine de escribir, guarde su documento con <menu|File|Save>, con
  la entrada <menu|Save> del menú del icono <icon|tm_save.png>, o con <shortcut|(save-buffer)>. Un
  mensaje en la barra de estado confirma que el documento se ha guardado.

  Por seguridad, <TeXmacs> también <em|guarda automáticamente> su documento
  cada dos minutos, en un fichero de copia de seguridad aparte. Si olvida
  guardar su documento antes de cerrar <TeXmacs>, o si su ordenador se
  detiene por algún motivo, <TeXmacs> le propondrá recuperar los cambios no
  guardados la próxima vez que abra el documento.

  <paragraph*|Imprimir y exportar>

  Normalmente querrá imprimir su documento, o enviarlo a personas que no
  usan <TeXmacs>. Su documento se imprime con <menu|File|Print|Print buffer>,
  usando las impresoras de su sistema. También puede convertirlo en un
  fichero PDF, que se puede leer en cualquier ordenador, con
  <menu|File|Export|Pdf>. El tamaño del papel (A4 en Europa, Letter en
  Estados Unidos) y los márgenes se definen en <menu|Document|Page>.

  <paragraph*|Cargar>

  Para recuperar su texto después de cerrar <TeXmacs>, use
  <menu|File|Load>, la entrada <menu|Load> del menú del icono
  <icon|tm_open.png>, o el atajo
  <shortcut|(open-document)>. Aparece un selector de ficheros, en el que
  selecciona el fichero <verbatim|test.tm> y confirma, o simplemente hace
  doble clic en él. Sus documentos recientes también aparecen en
  <menu|File|Recent>.

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
