<TeXmacs|2.1.4>

<style|<tuple|tmdoc|spanish>>

<\body>
  <tmdoc-title|Cómo enfatizar texto>

  Supongamos que quiere escribir la frase

  <\quote-env>
    A <em|gnu> is a hairy animal, which is not a clone of a unix.
  </quote-env>

  en la que la palabra \Pgnu\Q está enfatizada. En lugar de cambiar a un
  tipo de letra cursiva, usted le dice a <TeXmacs> que la palabra está
  <em|enfatizada>, poniéndola dentro de la etiqueta <markup|em>. Hay varias
  maneras de hacerlo.

  <paragraph*|Con los menús>

  Escriba \PA \Q y seleccione <menu|Insert|Content tag|Emphasize>. Aparece
  una región enfatizada vacía, rodeada por un recuadro cian, y la barra de
  estado indica que el cursor está dentro de <verbatim|em>:

  <\big-envbox>
    A <em|<small-focus|<text-cursor>>>
  </big-envbox>

  Escriba la palabra \Pgnu\Q: aparece en cursiva, ya que está dentro de la
  región enfatizada.

  <\big-envbox>
    A <em|<small-focus|gnu<text-cursor>>>
  </big-envbox>

  Para continuar con texto ordinario, pulse <key*|right>: el cursor sale de
  la región enfatizada (el recuadro cian desaparece) sin moverse en la
  pantalla. Escriba ahora el resto de la frase.

  <paragraph*|Con una orden híbrida>

  En lugar de usar el menú, también puede escribir el nombre de la etiqueta
  después de la tecla <key|\\>. Escriba <key|\\ e m>: la barra de estado
  indica ahora que al pulsar <key|return> se enfatizará el texto. Pulse
  <key|return> y se encontrará en la misma situación que antes. Esto
  funciona con cualquier etiqueta, siempre que conozca su nombre, y también
  con las órdenes de <LaTeX> que tienen un equivalente en <TeXmacs>; por
  ejemplo, <key|\\ s e c t i o n> crea un título de sección.

  <paragraph*|Con la selección>

  Por último, si el texto ya está escrito, seleccione la palabra \Pgnu\Q con
  el ratón y elija después <menu|Insert|Content tag|Emphasize>: el texto
  seleccionado se coloca dentro de la etiqueta.

  <paragraph*|¾Por qué no usar simplemente la cursiva?>

  El texto enfatizado suele parecer texto en cursiva, pero es el estilo el
  que decide. Por ejemplo, dentro de un teorema, que ya está en cursiva, el
  texto enfatizado se compone en redonda, para que siga destacando:

  <\quote-env>
    <\theorem>
      A <em|gnu> is a hairy animal.
    </theorem>
  </quote-env>

  Del mismo modo, la etiqueta <markup|strong>
  (<menu|Insert|Content tag|Strong>) marca el texto <strong|importante>, y
  la etiqueta <markup|name> (<menu|Insert|Content tag|Name>) marca nombres
  de personas como <name|Joris>. Si más adelante decide que el texto
  importante debe mostrarse en rojo en lugar de en negrita, solo tiene que
  cambiar el estilo, y no los muchos lugares donde se usó <markup|strong>.

  El ejemplo de esta sección está en <verbatim|examples/hairy.tm>.

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
