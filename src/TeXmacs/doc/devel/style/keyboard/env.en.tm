<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Macros and environment variables>

  The main key-combinations that you should know to write style files are the
  following (see <source-link|progs/source/source-kbd.scm|TeXmacs/progs/source/source-kbd.scm> for the complete
  list):

  <\description>
    <item*|<key|inactive =>>creates a new assignment. The first argument is a
    new command name and the second argument an expression.

    <item*|<key|inactive w>>permits to locally change one or more environment
    variables. With statements are of the form
    <math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<rsub|n>\|b\<rangle\>>,
    where the <math|x<rsub|i>> are the names of the variables, the
    <math|a<rsub|i>> their local values, and <math|b> the text on which the
    local environment applies.

    <item*|<key|inactive m>>creates a macro. Arguments to the macro can be
    inserted using <shortcut|(structured-insert-right)> and
    <shortcut|(structured-insert-left)>.

    <item*|<key|inactive x>>creates a macro with a variable number of
    arguments (<markup|xmacro>).

    <item*|<key|inactive a>, <key|inactive #>>get the value of a macro
    argument.

    <item*|<key|inactive v>>get the value of an environment variable.

    <item*|<key|inactive c>>applies a macro to zero or more arguments
    (<markup|compound>).

    <item*|<key|inactive d>>specifies logical properties of tags
    (<markup|drd-props>).

    <item*|<key|inactive q>, <key|inactive `>, <key|inactive ,>,
    <key|inactive '>, <key|inactive !>>the evaluation control primitives
    <markup|quasi>, <markup|quasiquote>, <markup|unquote>, <markup|quote> and
    <markup|eval>.
  </description>

  More precisely, when evaluating a macro application
  <math|\<langle\>a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>\<rangle\>>
  created by <key|inactive c>, the following action is undertaken:

  <\itemize>
    <item>If <math|a> is not a string nor a macro, then <math|a> is evaluated
    once. This results either in a macro name or a macro expression
    <math|f>.

    <item>If we obtain a macro name, then we replace <math|f> by the value of
    the environment variable <math|f>. If, after this, <math|f> is still not
    a macro expression, then we return <math|f>.

    <item>Let <math|y<rsub|1>,\<ldots\>,y<rsub|n>> be the arguments of
    <math|f> and <math|b> its body (superfluous arguments are discarded;
    missing arguments take the value <markup|uninit>). Then we substitute
    <math|x<rsub|i>> for each <math|y<rsub|i>> in <math|b> and return the
    evaluated result.
  </itemize>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
