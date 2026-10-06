<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Computational markup>

  The following commands can be used for performing dynamic computations
  (see <source-link|progs/source/source-kbd.scm|TeXmacs/progs/source/source-kbd.scm> for the complete list; the same
  primitives are also available from the menus <menu|Source|Arithmetic>,
  <menu|Source|Text>, <menu|Source|Tuple> and <menu|Source|Condition>):

  <\description>
    <item*|<key|executable \|>>sequential or of two conditions.

    <item*|<key|executable ^>>exclusive or of two conditions.

    <item*|<key|executable &>>sequential and of two conditions.

    <item*|<key|executable !>>negation of a condition.

    <item*|<key|executable +>>add two numbers or lengths.

    <item*|<key|executable ->>subtract two numbers or lengths.

    <item*|<key|executable *>>multiply two numbers.

    <item*|<key|executable />>divide two numbers.

    <item*|<key|executable d>, <key|executable m>>integer division and
    remainder.

    <item*|<key|executable ;>>concatenate two strings (or tuples).

    <item*|<key|executable l>>length of a string or a tuple.

    <item*|<key|executable ,>>extract a range from a string or a tuple.

    <item*|<key|executable #>>display a number in Arabic, roman, Roman, alpha
    or Alpha (used for instance in enumerations).

    <item*|<key|executable @>, <key|executable C-@>>current date, possibly
    formatted.

    <item*|<key|executable t>>translate a word from a source language into a
    destination language (see the dictionaries in
    <verbatim|$TEXMACS_PATH/langs/natural/dic>).

    <item*|<key|executable q>>test whether an expression is a tuple.

    <item*|<key|executable [>>look up an element of a tuple.

    <item*|<key|executable =>>test equality.

    <item*|<key|executable C-=>>test inequality.

    <item*|<key|executable \<less\>>, <key|executable \<gtr\>>,
    <key|executable C-\<less\>>, <key|executable C-\<gtr\>>>comparisons
    (less, greater, less or equal, greater or equal).

    <item*|<key|executable f>>find a file.

    <item*|<key|inactive ?>>insert an <markup|if> statement with an optional
    else part.
  </description>

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
