<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Entrando no modo matemático e saindo dele>

  O <TeXmacs> distingue entre o texto comum e as fórmulas matemáticas.
  Dentro das fórmulas, as letras são compostas em itálico, o espaçamento em
  torno dos símbolos segue as convenções matemáticas e o teclado se comporta
  de maneira diferente, para que os símbolos possam ser digitados
  rapidamente. Para digitar uma fórmula, portanto, você precisa primeiro
  entrar no <em|modo matemático>.

  <paragraph*|Fórmulas na linha>

  Suponha que você digitou \PHere comes a formula:\Q e que agora quer
  digitar uma pequena fórmula na mesma linha. Selecione
  <menu|Insert|Mathematics|Inline formula>, ou simplesmente pressione a
  tecla <key|$>:

  <\big-envbox>
    Here comes a formula: <math|<small-focus|<math-cursor>>>
  </big-envbox>

  Várias coisas aconteceram: o cursor ficou roxo, uma fórmula vazia cercada
  por uma caixa ciano foi inserida, a parte esquerda da barra de status agora
  indica <verbatim|math> em vez de <verbatim|text>, e a barra de ferramentas
  do modo mostra ícones com símbolos matemáticos. Tudo o que você digitar
  agora vai para a fórmula, até que você saia dela. A maneira mais simples
  de sair da fórmula é mover o cursor para fora dela com <key*|right> quando
  você estiver no seu final.

  <paragraph*|Fórmulas destacadas>

  Fórmulas maiores ficam melhor exibidas em uma linha própria, como

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>.
  </equation*>

  Essas <em|fórmulas destacadas> são inseridas com
  <menu|Insert|Mathematics|Displayed formula> ou
  <shortcut|(make-equation*)>. Você pode numerar uma fórmula destacada com
  <shortcut|(numbered-toggle (focus-tree))>, e transformar uma fórmula na
  linha em uma fórmula destacada (ou vice-versa) com
  <shortcut|(variant-circulate (focus-tree) #t)>.

  Nas próximas seções, veremos como digitar o conteúdo das fórmulas.

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
