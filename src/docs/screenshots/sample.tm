<TeXmacs|2.1.4>

<style|generic>

<\body>
  <doc-data|<doc-title|The Gaussian integral>|<doc-author|<author-data|<author-name|Ada Sample>>>>

  <section|A classical computation>

  The Gaussian integral is one of the few integrals over the whole line
  which can be computed in closed form, although <math|e<rsup|-x<rsup|2>>>
  has no elementary primitive.

  <\theorem>
    For every <math|a\<gtr\>0>,

    <\equation*>
      <big|int><rsub|-\<infty\>><rsup|+\<infty\>>e<rsup|-a*x<rsup|2>>*\<mathd\>x=<sqrt|<frac|\<pi\>|a>>.
    </equation*>
  </theorem>

  <\proof>
    Let <math|I> be the integral. By Fubini and polar coordinates,

    <\equation*>
      I<rsup|2>=<big|int><rsub|\<bbb-R\><rsup|2>>e<rsup|-a*<around*|(|x<rsup|2>+y<rsup|2>|)>>*\<mathd\>x*\<mathd\>y=<big|int><rsub|0><rsup|2*\<pi\>><big|int><rsub|0><rsup|\<infty\>>e<rsup|-a*r<rsup|2>>*r*\<mathd\>r*\<mathd\>\<theta\>=<frac|\<pi\>|a>,
    </equation*>

    and <math|I\<gtr\>0>.
  </proof>

  <\center>
    <block*|<tformat|<table|<row|<cell|<math|a>>|<cell|<math|1>>|<cell|<math|2>>|<cell|<math|\<pi\>>>>|<row|<cell|<math|I>>|<cell|<math|<sqrt|\<pi\>>>>|<cell|<math|<sqrt|\<pi\>/2>>>|<cell|<math|1>>>>>>
  </center>
</body>

<initial|<\collection>
</collection>>
