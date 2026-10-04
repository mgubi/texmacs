<TeXmacs|2.1.5>

<style|article>

<\body>
  <section|An included section><label|sec-part>

  This section is kept in the file <verbatim|include/include-part.tm> and
  included in the main document, where it is numbered with the rest. It
  refers back to Section<nbsp><reference|sec-main> and to
  Equation<nbsp><eqref|eq-main> of the main document, and defines a label
  of its own:

  <\equation>
    <label|eq-part>a<rsup|2>+b<rsup|2>=c<rsup|2>
  </equation>

  <\theorem>
    <label|thm-part>A theorem stated in the included file.
  </theorem>
</body>

<initial|<\collection>
</collection>>
