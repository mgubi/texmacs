<TeXmacs|2.1.5>

<style|article>

<\body>
  <section|The main document><label|sec-main>

  This document includes the file <verbatim|include/include-part.tm>,
  which becomes Section<nbsp><reference|sec-part>. The counters run on
  across the two files: the included file holds
  Equation<nbsp><eqref|eq-part> and Theorem<nbsp><reference|thm-part>, on
  page<nbsp><pageref|thm-part>, while this file holds

  <\equation>
    <label|eq-main>e<rsup|i*\<pi\>>+1=0
  </equation>

  <include|include/include-part.tm>

  <section|After the inclusion><label|sec-after>

  The numbering continues after the included file: this is
  Section<nbsp><reference|sec-after>, and the next equation follows
  Equation<nbsp><eqref|eq-part>:

  <\equation>
    <label|eq-after>x<rsup|n>+y<rsup|n>\<neq\>z<rsup|n>
  </equation>

  <\theorem>
    A theorem after the inclusion, numbered after
    Theorem<nbsp><reference|thm-part>.
  </theorem>
</body>

<initial|<\collection>
<associate|page-medium|paper>
</collection>>
