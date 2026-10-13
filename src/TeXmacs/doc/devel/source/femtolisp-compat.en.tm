<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|femtolisp: Scheme and Guile on femtolisp>

  femtolisp is close to Scheme, with names of its own (<verbatim|aref>,
  <verbatim|string.sub>, <verbatim|table>, <verbatim|trycatch>...), strings
  which know UTF-8, and a small library. Two files give <TeXmacs> the R5RS and
  the Guile 1.8 which its code is written for.

  <section|<source-link|r5rs-femtolisp.scm|TeXmacs/progs/kernel/boot/r5rs-femtolisp.scm>: the language>

  Global definitions, loaded before the modules exist.

  <\description-long>
    <item*|Redefining a builtin>The builtins of femtolisp are constants, and
    the compiler compiles the <verbatim|set!> of a constant as nothing.
    <verbatim|define-override> makes the name redefinable when it is expanded,
    before the definition is compiled; <verbatim|define-public> does it too.
    The original is kept under a name starting with <verbatim|%fl-> when the
    new definition needs it (<verbatim|%fl-eval>, <verbatim|%fl-read>,
    <verbatim|%fl-write>...).

    <item*|Symbols and keywords><verbatim|:foo> is a keyword and not a symbol,
    as in Guile with keywords written with a colon. <verbatim|symbol?> is
    false for the keywords and for the unspecified value, which femtolisp
    makes a symbol, <verbatim|#\<less\>unspecified\<gtr\>> (patch 0019).

    <item*|Numbers>There are no exact rationals: <verbatim|(/ 1 2)> is
    <verbatim|0.5>, and <verbatim|string-\<gtr\>number> reads <verbatim|1/2>
    as an inexact number; written in a source file, <verbatim|1/2> is read as
    a symbol. The exact integers have 64 bits: a larger one in a source file
    is read as an inexact number, <verbatim|string-\<gtr\>number> gives
    <verbatim|#f> for it, and an operation which overflows wraps around
    without an error (<verbatim|(expt 2 70)> is <verbatim|0>).
    <verbatim|quotient>, <verbatim|remainder> and <verbatim|modulo> have the
    signs of R5RS; <verbatim|\<gtr\>>, <verbatim|\<less\>=>,
    <verbatim|\<gtr\>=> take any number of arguments (<verbatim|\<less\>> and
    <verbatim|=> do in femtolisp, patch 0017).

    <item*|Complex numbers>A complex number is the vector
    <verbatim|#(%complex re im)>. <verbatim|+ - * / => stay the fast
    instructions of femtolisp: for an operand which is not a number they call
    <verbatim|*arith-fallback*> (patch 0020), defined here. <verbatim|number?>
    is false for a complex number, and the other functions on numbers do not
    handle one: some raise an error (<verbatim|sqrt>), others give a result
    without meaning (<verbatim|abs>, <verbatim|\<less\>>).

    <item*|Characters and strings>A character is a byte and a string a string
    of bytes, in the Cork encoding: <verbatim|string-length> counts bytes,
    <verbatim|char-upcase> and <verbatim|char-alphabetic?> know ASCII only, as
    in Guile 1.8. The basic functions on strings (<verbatim|string-length>,
    <verbatim|string-ref>, <verbatim|substring>, <verbatim|string-set!>,
    <verbatim|make-string>, <verbatim|list-\<gtr\>string>,
    <verbatim|string-\<gtr\>list>, the search and the comparison) are C
    builtins of <source-link|fl_core.c|src/Scheme/Femtolisp/fl_core.c>; the others are written in Scheme on
    them. femtolisp reads the octal characters of Guile wrongly
    (<verbatim|#\\04> is read as the character <verbatim|0> followed by
    <verbatim|4>): write <verbatim|(integer-\<gtr\>char 4)>.

    <item*|Control><verbatim|call/cc> only escapes: a continuation cannot be
    entered again. <verbatim|dynamic-wind>, <verbatim|delay> and
    <verbatim|force> are there.

    <item*|Errors>An error is the list <verbatim|(key . args)>, as in Guile:
    <verbatim|(throw 'my 1 2)> is caught as <verbatim|(my 1 2)>. The errors of
    <verbatim|error> and <verbatim|scm-error> are
    <verbatim|(key subr message args rest)>; those of the glue have no
    <verbatim|rest>. The errors of femtolisp itself (<verbatim|type-error>,
    <verbatim|unbound-error>...) are translated by <verbatim|%guile-error>,
    for the handlers of <verbatim|catch> and for the C++ code.
    <verbatim|catch>, <verbatim|throw>, <verbatim|error>,
    <verbatim|scm-error>, <verbatim|false-if-exception>.

    <item*|Reading and writing>The ports are the streams of femtolisp.
    <verbatim|write> writes on one line, with labels only for cycles
    (<verbatim|*print-shared*>, patch 0005) and closures as
    <verbatim|#\<less\>procedure f\<gtr\>> (<verbatim|*print-closures*>, patch
    0018); the bytes from 128 of a string are written as they are (patch
    0006). <verbatim|display> and <verbatim|write> without a port write to the
    output of <TeXmacs> (<verbatim|tm-output>), as with Guile; this is done in
    <source-link|boot-femtolisp.scm|TeXmacs/progs/kernel/boot/boot-femtolisp.scm>.
  </description-long>

  <section|<source-link|compat-femtolisp.scm|TeXmacs/progs/kernel/boot/compat-femtolisp.scm>: the library>

  The module <verbatim|(kernel boot compat-femtolisp)>, modelled on
  <source-link|compat-s7.scm|TeXmacs/progs/kernel/boot/compat-s7.scm>: the hash tables of Guile on the tables of
  femtolisp, the association lists, SRFI-1 (<verbatim|fold>,
  <verbatim|filter-map>, <verbatim|partition>...) and the sorts, the string
  functions of SRFI-13, the character sets (a tagged vector which holds 256
  booleans), <verbatim|object-property>, <verbatim|procedure-name>,
  <verbatim|procedure-source>, <verbatim|procedure-arity>, <verbatim|format>
  (<verbatim|~a ~s ~% ~~>), <verbatim|pretty-print>, records, and the
  <verbatim|while> of Guile with <verbatim|break> and <verbatim|continue>.

  Two rules for this file:

  <\itemize>
    <item><em|Never define a function which the glue defines.> The C++
    function would be lost for all the code (<verbatim|string-replace> is
    one).

    <item><TeXmacs> defines an editor command <verbatim|fold>, which replaces
    the <verbatim|fold> of SRFI-1 as it does in Guile: the functions of this
    file use the private <verbatim|fold*>.
  </itemize>

  <section|What differs from Guile>

  <\itemize>
    <item>No exact rationals; exact integers of 64 bits, which wrap around;
    complex numbers only with <verbatim|+ - * / => and their own functions.

    <item><verbatim|call/cc> only escapes.

    <item>The hash tables are traversed in another order: code whose result
    depends on the order of <verbatim|ahash-table-\<gtr\>list> differs.

    <item>A macro whose expansion contains a call of itself expands without
    end, until an out of memory error: when the file is loaded if the call is
    at its top level, at the first call of the function if it is in a body
    (<verbatim|concat-isolate!> in <source-link|math-edit.scm|TeXmacs/progs/math/math-edit.scm> was made a
    function).

    <item>The forms at the top level of a file are expanded when it is loaded,
    also those under a test which is false.

    <item><verbatim|:use> does not restrict the names which a module sees.

    <item>A builtin of femtolisp is written <verbatim|#.car>, a C function of
    the glue <verbatim|#fn(string-replace)>.
  </itemize>

  <section|Branches in the shared Scheme code>

  <verbatim|(femtolisp-scheme?)> is <verbatim|#t> here and <verbatim|#f> with
  the other interpreters. Elsewhere femtolisp runs the code written for Guile,
  except in a few files which have a branch for it:

  <\description-long>
    <item*|<source-link|kernel/boot/prologue.scm|TeXmacs/progs/kernel/boot/prologue.scm>>The modules are loaded by
    <source-link|boot-femtolisp.scm|TeXmacs/progs/kernel/boot/boot-femtolisp.scm>, as for S7.

    <item*|<source-link|kernel/boot/ahash-table.scm|TeXmacs/progs/kernel/boot/ahash-table.scm>>The <verbatim|ahash->
    functions are directly on the tables of femtolisp.

    <item*|<source-link|kernel/regexp/regexp-select.scm|TeXmacs/progs/kernel/regexp/regexp-select.scm>><verbatim|select> as on
    MinGW, without the <verbatim|select> of Guile.

    <item*|<source-link|kernel/texmacs/tm-define.scm|TeXmacs/progs/kernel/texmacs/tm-define.scm>>Global definitions, see
    <hlink|the modules|femtolisp-modules.en.tm>.

    <item*|<source-link|prog/scheme-autocomplete.scm|TeXmacs/progs/prog/scheme-autocomplete.scm>>The symbols come from the
    environment of femtolisp.

    <item*|<source-link|check/define-test.scm|TeXmacs/progs/check/define-test.scm>, <source-link|check/macro-drd-test.scm|TeXmacs/progs/check/macro-drd-test.scm>>Two
    tests skip checks, as they do for S7: that <verbatim|:use> restricts the
    names, and dates computed with the functions of Guile.
  </description-long>

  Keep such branches rare: a difference is better hidden in the two files
  above, so that the Scheme code of <TeXmacs> stays the same for all the
  interpreters.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
