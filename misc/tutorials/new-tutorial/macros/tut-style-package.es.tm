<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Cómo reutilizar sus macros en otros documentos>

  Las macros del preámbulo solo existen en su documento. Si quiere usarlas
  también en otros documentos, póngalas en un <em|paquete de estilo>: un
  fichero con el sufijo <verbatim|.ts> que contiene definiciones de macros
  y que puede añadir a cualquier documento.

  <paragraph*|Cómo crear el paquete>

  Abra el documento con sus macros y elija
  <menu|Tools|Macros|Extract style package>. <TeXmacs> abre un nuevo
  documento con el estilo <tmstyle|source>, que contiene un encabezado (con
  el nombre de paquete <verbatim|love-macros>, que puede cambiar por
  <verbatim|love>), los parámetros de estilo de su documento, si los hay, y
  todas las definiciones del preámbulo:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>

    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  Guarde este documento en la carpeta <verbatim|packages> de su carpeta
  personal de <TeXmacs>, con el nombre <verbatim|love.ts>. Esta carpeta
  personal es <verbatim|~/.TeXmacs> en <name|Linux> y <name|macOS> (es
  decir, la carpeta <verbatim|.TeXmacs> de su carpeta de inicio), y
  <verbatim|%APPDATA%\\TeXmacs> en <name|Windows>; <TeXmacs> crea la
  carpeta <verbatim|packages> dentro de ella al iniciarse. En <name|macOS>,
  el selector de ficheros oculta las carpetas cuyo nombre empieza por un
  punto: pulse <verbatim|Cmd-Shift-.> para mostrar <verbatim|.TeXmacs>. El
  paquete de ejemplo de este tutorial es <verbatim|examples/love.ts>.

  <paragraph*|Cómo usar el paquete>

  En cualquier documento, ahora puede elegir
  <menu|Document|Style|Add package|Add other package> y escribir
  <verbatim|love>: las macros <markup|hi> y <markup|love> quedan
  disponibles, exactamente como si se hubieran definido en el preámbulo del
  documento. Si más adelante cambia el paquete, todos los documentos que lo
  usan tienen en cuenta el cambio. Un paquete se puede quitar de un
  documento en el menú <menu|Document|Style>.

  También puede crear un paquete de estilo desde cero, eligiendo
  <menu|Document|Style|source> para un documento nuevo, escribiendo las
  definiciones y guardándolo con el sufijo <verbatim|.ts>. La escritura de
  ficheros de estilo se explica en detalle en
  <menu|Help|Manual|Writing your own style files>.

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
