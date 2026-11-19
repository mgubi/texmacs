<TeXmacs|2.1.2>

<style|<tuple|generic|dark-mode>>

<\body>
  <doc-data|<doc-title|Heat flow on the circle>>

  <section|The heat kernel>

  Let <math|u> solve the heat equation <math|\<partial\><rsub|t>u=\<partial\><rsub|x><rsup|2>u>
  on the circle <math|\<bbb-T\>=\<bbb-R\>/2*\<pi\>*\<bbb-Z\>>, with
  <math|u<around*|(|0|)>=f>. In <hlink|Fourier series|https://en.wikipedia.org/wiki/Fourier_series>
  each mode decays at its own rate:

  <\equation>
    <label|eq-heat>u<around*|(|t,x|)>=<big|sum><rsub|k\<in\>\<bbb-Z\>>e<rsup|-k<rsup|2>*t>*<wide|f|^><around*|(|k|)>*e<rsup|i*k*x>,<space|2em><wide|f|^><around*|(|k|)>=<frac|1|2*\<pi\>>*<big|int><rsub|0><rsup|2*\<pi\>>f<around*|(|x|)>*e<rsup|-i*k*x>*d*x.
  </equation>

  <\theorem>
    <label|thm-decay>If <math|f\<in\>L<rsup|2><around*|(|\<bbb-T\>|)>> has
    mean zero, then <math|<around*|\<\|\|\>|u<around*|(|t|)>|\<\|\|\>><rsub|L<rsup|2>>\<leqslant\>e<rsup|-t>*<around*|\<\|\|\>|f|\<\|\|\>><rsub|L<rsup|2>>>
    for all <math|t\<geqslant\>0>.
  </theorem>

  <\proof>
    By Parseval and (<reference|eq-heat>), <math|<around*|\<\|\|\>|u<around*|(|t|)>|\<\|\|\>><rsup|2>=2*\<pi\>*<big|sum><rsub|k\<neq\>0>e<rsup|-2*k<rsup|2>*t>*<around*|\||<wide|f|^><around*|(|k|)>|\|><rsup|2>\<leqslant\>e<rsup|-2*t>*<around*|\<\|\|\>|f|\<\|\|\>><rsup|2>>.
  </proof>

  <section|A numerical check>

  Theorem <reference|thm-decay> with the fast Fourier transform:

  <\python-code>
    import numpy as np

    \;

    def heat(f, t):

    \ \ \ \ """Solve u_t = u_xx on the circle, by Fourier modes."""

    \ \ \ \ k = np.fft.fftfreq(len(f), 1 / len(f))

    \ \ \ \ return np.fft.ifft(np.exp(-k**2 * t) * np.fft.fft(f)).real

    \;

    x = np.linspace(0, 2 * np.pi, 256, endpoint=False)

    u = heat(np.sign(np.sin(x)), 1.0) \ # a square wave
  </python-code>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (map (lambda (t) (exp (- t))) '(0 1 2))
    <|unfolded-io>
      (1.0 0.36787944117144233 0.1353352832366127)
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|info-flag|none>
    <associate|page-medium|paper>
  </collection>
</initial>
