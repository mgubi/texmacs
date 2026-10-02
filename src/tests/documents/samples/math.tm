<TeXmacs|2.1.5>

<style|article>

<\body>
  <section|Inline formulas>

  Inline formulas such as <math|x<rsup|2>+y<rsup|2>=z<rsup|2>>,
  <math|<frac|a|b>>, <math|<sqrt|2>>, <math|<sqrt|x|3>>,
  <math|e<rsup|i*\<pi\>>+1=0> and <math|<big|sum><rsub|k=1><rsup|n>k=<frac|n*<around*|(|n+1|)>|2>>
  sit in the line and must not change its spacing too much.

  <section|Displayed formulas>

  <\equation>
    <label|eq-gauss><big|int><rsub|-\<infty\>><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<sqrt|\<pi\>>
  </equation>

  Equation<nbsp><eqref|eq-gauss> is the Gaussian integral.

  <\equation*>
    f<around*|(|x|)>=<choice|<tformat|<table|<row|<cell|x<rsup|2>>|<cell|if
    x\<geqslant\>0,>>|<row|<cell|-x>|<cell|otherwise.>>>>>
  </equation*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|<around*|(|a+b|)><rsup|2>>|<cell|=>|<cell|a<rsup|2>+2*a*b+b<rsup|2>>>|<row|<cell|<around*|(|a-b|)><rsup|2>>|<cell|=>|<cell|a<rsup|2>-2*a*b+b<rsup|2>>>>>
  </eqnarray*>

  <section|Matrices and delimiters>

  <\equation*>
    A=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>|<cell|3>>|<row|<cell|4>|<cell|5>|<cell|6>>|<row|<cell|7>|<cell|8>|<cell|9>>>>>,<space|2em>det<around*|(|<matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>|)>=a*d-b*c
  </equation*>

  <\equation*>
    <around*|\||<frac|<big|sum><rsub|i=1><rsup|n>x<rsub|i>|n>-\<mu\>|\|>\<leqslant\><around*|\<\|\|\>|x|\<\|\|\>><rsub|\<infty\>>,<space|2em><around*|{|<frac|1|1+<frac|1|1+<frac|1|x>>>|}>
  </equation*>

  <section|Accents, scripts and operators>

  <\equation*>
    <wide|x|^>,<space|1em><wide|x|~>,<space|1em><wide|x|\<bar\>>,<space|1em><wide|x+y|\<wide-bar\>>,<space|1em><wide|A*B*C|\<wide-hat\>>,<space|1em>x<rsub|i><rsup|2>,<space|1em><rsup|n>C<rsub|k>,<space|1em><big|prod><rsub|p<text|
    prime>><frac|1|1-p<rsup|-s>>,<space|1em>lim<rsub|n\<rightarrow\>\<infty\>><around*|(|1+<frac|1|n>|)><rsup|n>=e
  </equation*>

  <\equation*>
    \<alpha\>\<beta\>\<gamma\>\<delta\>\<varepsilon\>,<space|1em>\<Gamma\>\<Delta\>\<Theta\>\<Lambda\>\<Omega\>,<space|1em>\<bbb-R\>,\<bbb-C\>,<space|1em>\<cal-F\>,\<frak-g\>,<space|1em>a\<in\>A\<subseteq\>B,<space|1em>\<forall\>\<varepsilon\>\<gtr\>0\<exists\>\<delta\>\<gtr\>0
  </equation*>
</body>

<initial|<\collection>
</collection>>
