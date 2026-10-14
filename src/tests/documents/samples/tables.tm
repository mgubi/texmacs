<TeXmacs|2.1.5>

<style|article>

<\body>
  <section|Tables>

  A plain table with borders, and cells aligned left, centered and right:

  <\big-table|<tabular|<tformat|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|1|cell-halign|l>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|3|3|cell-halign|r>|<cwith|1|1|1|-1|cell-background|pastel
  grey>|<table|<row|<cell|Left>|<cell|Center>|<cell|Right>>|<row|<cell|apple>|<cell|1>|<cell|0.50>>|<row|<cell|banana>|<cell|12>|<cell|12.25>>|<row|<cell|cherry>|<cell|123>|<cell|1230.00>>>>>>
    A table with a header row.
  </big-table>

  A table whose cells span several rows and columns:

  <\big-table|<tabular|<tformat|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-row-span|2>|<cwith|1|1|2|2|cell-col-span|2>|<cwith|1|-1|1|-1|cell-valign|c>|<table|<row|<cell|Two
  rows>|<cell|Two columns>|<cell|>>|<row|<cell|>|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>|<cell|e>>>>>>
    Merged cells.
  </big-table>

  A table whose cells hold paragraphs, which must wrap within their width:

  <tabular|<tformat|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-width|5cm>|<cwith|1|-1|1|-1|cell-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<table|<row|<\cell>
    A long paragraph in a cell of fixed width, which the typesetter breaks
    into several lines.
  </cell>|<\cell>
    Another paragraph, shorter.
  </cell>>>>>

  A matrix-like table inside a formula:
  <math|<det|<tformat|<table|<row|<cell|1>|<cell|0>>|<row|<cell|0>|<cell|1>>>>>=1>.
</body>

<initial|<\collection>
</collection>>
