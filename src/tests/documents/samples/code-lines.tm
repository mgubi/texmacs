<TeXmacs|2.1.5>

<style|<tuple|article>>

<\body>
  <section|Lines of code>

  A block of lines of source code (the tag code-lines), typeset as lines on
  a grid, with the colors of the syntax and soft line breaks.

  <\with|font-family|tt|language|verbatim|mode|prog|prog-language|cpp|par-first|0fn>
    <\code-lines>
      /* a comment of several lines,

      \ \ \ which ends here */ int first= 1;

      #define TWICE(x) \\

      \ \ ((x) + (x))

      // an inline comment with /* in it

      int identifiers_are_drawn (int a, int b) {

      \ \ return a \<less\> b? TWICE (a): b;

      }

      \;

      int <strong|bold_name>= 2; // <em|emphasis> in a comment

      int <with|color|red|red_value>= 3;

      double area= <math|\<pi\> r<rsup|2>>; /* math */

      int <underline|underlined>= 4;

      double ratio= <math|<frac|<frac|a|b>|c+d>>; // a tall line

      int total= alpha_one + alpha_two + alpha_three + alpha_four + alpha_five + alpha_six + alpha_seven + alpha_eight + alpha_nine + alpha_ten;
    </code-lines>
  </with>
</body>

<initial|<\collection>
</collection>>
