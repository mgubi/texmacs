<TeXmacs|2.1.5>

<style|<tuple|article|std-shell>>

<\body>
  <section|Program code>

  Code is set in a typewriter font, with the colors of the syntax of its
  language.

  <\cpp-code>
    #include \<less\>iostream\<gtr\>

    \;

    int main () {

    \ \ for (int i= 0; i \<less\> 10; i++)

    \ \ \ \ std::cout \<less\>\<less\> i \<less\>\<less\> "\\n";

    \ \ return 0;

    }
  </cpp-code>

  <\python-code>
    def fib (n):

    \ \ \ \ a, b = 0, 1

    \ \ \ \ for _ in range (n):

    \ \ \ \ \ \ \ \ a, b = b, a + b

    \ \ \ \ return a
  </python-code>

  <\scm-code>
    (define (square x) (* x x))

    (map square '(1 2 3))
  </scm-code>

  Inline code: <cpp|std::vector\<less\>int\<gtr\>>, <scm|(car l)> and
  <verbatim|plain verbatim text>.

  <\verbatim>
    A verbatim block keeps

    \ \ its spaces and

    \ \ \ \ its line breaks.
  </verbatim>
</body>

<initial|<\collection>
</collection>>
