<TeXmacs|2.1.5>

<style|article>

<\body>
  <assign|hi|<macro|x|<with|color|red|font-shape|italic|<arg|x>>>>

  <section|A title with <math|x+y>>

  Text <strong|strong text> and <em|emphasis> and <hi|macro text>, with
  <math|a<rsup|2>+b<rsub|i>> inline and

  <\equation*>
    <frac|1|2>+\<alpha\>
  </equation*>

  <\itemize>
    <item>First <verbatim|code here>

    <item>Second <with|font-series|bold|bold <math|z>>
  </itemize>

  <\cpp-code>
    int x = 1;
  </cpp-code>

  <tabular|<tformat|<table|<row|<cell|cell a>|<cell|<math|c>>>>>>

  <\theorem>
    A theorem with <math|\<int\>f>.
  </theorem>
</body>

<initial|<\collection>
</collection>>
