<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|femtolisp: the binding with C++>

  <section|The layers>

  The rest of <TeXmacs> knows the Scheme interpreter through
  <verbatim|scheme.hpp> (the class <verbatim|object>, <verbatim|call>,
  <verbatim|eval>...). Below it, <verbatim|Scheme/Scheme/object.cpp>,
  <verbatim|glue.cpp> and the generated <verbatim|Scheme/Glue/glue_*.cpp> use
  only the type <verbatim|tmscm> and the functions and macros around it
  (<verbatim|tmscm_cons>, <verbatim|string_to_tmscm>,
  <verbatim|TMSCM_ASSERT>...). Each interpreter implements these: for
  femtolisp, <verbatim|femtolisp_tm.hpp> and <verbatim|femtolisp_tm.cpp>,
  which <verbatim|object.hpp> includes when <verbatim|USE_FEMTOLISP> is
  defined.

  The C++ code never includes the headers of femtolisp: their macros and
  functions (<verbatim|car>, <verbatim|cdr>, <verbatim|ptr>, <verbatim|tag>,
  <verbatim|symbol>...) clash with names of <TeXmacs>. It uses
  <verbatim|fl_tm.h>, a small C interface whose functions start with
  <verbatim|fltm_>. <verbatim|fl_core.c> implements it; that file also
  includes the sources of femtolisp, as one compilation unit, so the interface
  can use the insides of the interpreter (its stack, its types).

  <section|A <verbatim|tmscm> is a root of the garbage collector>

  femtolisp has a copying collector: any allocation may move every object. A
  value kept in a C variable is then wrong after the next allocation, and the
  glue keeps values so all the time, as in

  <\cpp-code>
    p= tmscm_cons (scheme_tree_to_tmscm (t[i]), p);
  </cpp-code>

  So <verbatim|tmscm> is a class: a value (<verbatim|fltm_value v>) and the
  links of a doubly linked list, <verbatim|tmscm_roots>. The constructors link
  the object, the destructor unlinks it, and at each collection femtolisp
  calls <verbatim|tmscm_relocate_roots>, which updates the value of every
  object of the list. Any <verbatim|tmscm> stays right across allocations, be
  it a local variable, a temporary, an argument or a member of an object of
  the heap.

  What follows from it:

  <\itemize>
    <item>The constructor from a raw value is <verbatim|explicit>: an integer
    does not become a <verbatim|tmscm> silently.

    <item><verbatim|tmscm_object_rep> holds a <verbatim|tmscm>: the stack of
    protected objects of the Guile and S7 interfaces is not needed.

    <item>A raw <verbatim|fltm_value> must not be kept across a call which
    allocates. The functions of <verbatim|fl_tm.h> marked <em|allocates>
    protect their own arguments, not the other values of their caller. In C++,
    keep a <verbatim|tmscm>; in <verbatim|fl_core.c>, push the value on the
    stack of femtolisp.

    <item>A <verbatim|tmscm> costs a little more than a pointer: a call of the
    glue is as fast as with S7, the conversions of values a little slower.
  </itemize>

  <section|Errors never cross C++ code>

  An error of femtolisp is a <verbatim|longjmp>. Over a C++ function it would
  skip the destructors of its <verbatim|tmscm>s, and leave addresses of the
  dead stack in the list of roots: the next collection would write there. Two
  rules prevent it.

  <\itemize>
    <item><em|From C++ to Scheme.> Every call goes through
    <verbatim|fltm_apply> or <verbatim|fltm_eval_string>
    (<verbatim|call_scheme>, <verbatim|eval_scheme>,
    <verbatim|eval_scheme_file>). They catch the error, report it (the Scheme
    function <verbatim|%report-error>), and return it as a value, in the form
    of Guile's errors.

    <item><em|From Scheme to C++.> A function of the glue runs inside the
    template <verbatim|tmscm_proc>, whose body is the macro
    <verbatim|TMSCM_CALL_GLUE>: the arguments are copied into
    <verbatim|tmscm>s and the function runs in a <verbatim|try> block; an
    error is raised by <verbatim|tmscm_raise> only after the block, when no
    C++ object with a destructor is left. <verbatim|TMSCM_ASSERT> throws the
    C++ exception <verbatim|tmscm_error>, which this block catches; any other
    C++ exception becomes the error <verbatim|misc-error>.
  </itemize>

  A new C function called from Scheme must follow the second rule: install it
  with <verbatim|tmscm_install_procedure>, which wraps it, and report its
  errors with <verbatim|TMSCM_ASSERT> or by throwing a <verbatim|tmscm_error>.
  Never call <verbatim|fltm_raise> with a C++ object alive.

  One error escapes these rules: the out of memory error, which an allocation
  raises (<verbatim|fltm_cons>, <verbatim|fltm_string>...) from wherever it is
  called, also from C++ code with live <verbatim|tmscm>s. The stack of
  femtolisp is printed before (<verbatim|fl_out_of_memory_hook>); the state of
  <TeXmacs> after such an error is not to be trusted.

  <section|Values>

  <\description-long>
    <item*|Strings><verbatim|string_to_tmscm> copies the bytes of the
    <TeXmacs> string into a femtolisp string, <verbatim|tmscm_to_string>
    copies them back. There is no conversion of encoding: the bytes of Cork go
    through. A string may hold the byte 0.

    <item*|Numbers>An integer is a fixnum, or a boxed 64-bit integer when it
    does not fit (<verbatim|fltm_integer>); the fixnums have 62 bits on a
    64-bit machine, 30 bits in WebAssembly. <verbatim|tmscm_to_int> throws a
    <verbatim|tmscm_error> (<verbatim|out-of-range>) for a value out of the
    range of <verbatim|int>. <verbatim|tmscm_is_double> is true for every
    number.

    <item*|Symbols><verbatim|symbol_to_tmscm> and <verbatim|tmscm_to_symbol>,
    by their names.

    <item*|The objects of <TeXmacs>>A tree, a url, a command... is an opaque
    value of femtolisp which holds a pointer to a <verbatim|blackbox> of the
    heap (<verbatim|blackbox_to_tmscm>, <verbatim|tmscm_to_blackbox>). Its
    type, made by <verbatim|fltm_define_opaque_type>, has four functions: to
    print the value, to delete the <verbatim|blackbox> when the value is
    collected, to compare two values for <verbatim|equal?> (the <verbatim|==>
    of the <verbatim|blackbox>: trees are compared by their contents), and to
    hash it in agreement with this comparison.
  </description-long>

  <section|The glue>

  <verbatim|tmscm_install_procedure (name, f, n, 0, 0)> defines the builtin
  <verbatim|name> of femtolisp as <verbatim|tmscm_proc\<less\>f\<gtr\>>, which
  checks that it gets as many arguments as <verbatim|f> takes (0 to 10;
  <verbatim|n> is not used). The generated files
  <verbatim|Scheme/Glue/glue_*.cpp> are the same for the three interpreters.
  When they must be made again, the femtolisp build runs the generators on the
  small S7 interpreter <verbatim|s7-run>, as the S7 build does.

  A few builtins which <TeXmacs> needs and femtolisp lacks are written in C in
  <verbatim|fl_core.c> (the table <verbatim|fltm_builtin_info>): the functions
  on strings of bytes (<verbatim|string-length>, <verbatim|string-ref>,
  <verbatim|substring>, <verbatim|list-\<gtr\>string>...),
  <verbatim|%keyword?>, <verbatim|%symbol?>, <verbatim|%table-ref>,
  <verbatim|%fingerprint>, <verbatim|%function-become!>, <verbatim|system>.
  Their names starting with <verbatim|%> are for the Scheme files of the
  interface only.

  <section|Starting>

  <verbatim|start_scheme> calls <verbatim|fltm_init> with the size of the heap
  and the boot image, which is in the program (<verbatim|fl_boot.h>): nothing
  is read from the disk to start femtolisp. It then gives
  <verbatim|tmscm_relocate_roots> to the collector.
  <verbatim|initialize_scheme> defines the builtins of the glue and the type
  of the objects of <TeXmacs>, and <TeXmacs> loads
  <verbatim|scheme_init_file ()>, which is
  <verbatim|progs/init-femtolisp.scm>.

  The heap has two halves of the same size, which the collector copies one
  into the other; a half is doubled when more than 80% of it is still in use
  after a collection. Each half starts with 48 MB, about the heap which S7 has
  in <TeXmacs>. The environment variable <verbatim|TEXMACS_FL_HEAP> gives
  another size, in MB (it is not checked: keep it well below 512). The size of
  a half is a 32-bit number in femtolisp, so a half which has reached 512 MB
  is not doubled any more, and an out of memory error is raised (patch 0012):
  starting with 48 MB, a half ends at 768 MB, the heap at 1.5 GB.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
