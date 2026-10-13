<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The femtolisp Scheme interpreter>

  <TeXmacs> runs its Scheme code on one of three interpreters, chosen when it
  is built: Guile, S7 or femtolisp. This chapter is for the developers who
  maintain the femtolisp one: how it is put together, what it relies on, how
  to change it and how to find what goes wrong.

  femtolisp is a small Lisp close to Scheme, by Jeff Bezanson, with a compiler
  to bytecode written in Lisp and a copying garbage collector. Its sources are
  in the tree, with the local patches: a build with femtolisp needs nothing
  else. It is about a sixth of the size of S7, in lines of source, and as fast
  or faster once the code is loaded.

  <section|Building and running>

  <\shell-code>
    ./configure --with-scheme=femtolisp

    make
  </shell-code>

  With CMake the option is <verbatim|-DSCHEME_IMPL=femtolisp> (not tested
  yet). The choice defines <verbatim|USE_FEMTOLISP> and compiles the directory
  <verbatim|src/Scheme/Femtolisp>. In Scheme, <verbatim|(femtolisp-scheme?)>
  tells that the interpreter is femtolisp, as <verbatim|(s7-scheme?)> does for
  S7.

  <section|Where things are>

  <\description-long>
    <item*|<verbatim|src/Scheme/Femtolisp/femtolisp/>>femtolisp itself, with
    the local patches applied: its C sources, its library <verbatim|llt>, its
    compiler and standard library (<verbatim|system.lsp>,
    <verbatim|compiler.lsp>) and the boot image made from them
    (<verbatim|flisp.boot>).

    <item*|<verbatim|src/Scheme/Femtolisp/patches/>>The local patches, in
    order, as <verbatim|git format-patch> writes them.

    <item*|<verbatim|src/Scheme/Femtolisp/fl_core.c>, <verbatim|fl_llt.c>>The
    two compilation units: femtolisp and its C interface for <TeXmacs>, and
    the library <verbatim|llt>.

    <item*|<verbatim|src/Scheme/Femtolisp/fl_tm.h>>The C interface, the only
    header of femtolisp which the C++ code includes.

    <item*|<verbatim|src/Scheme/Femtolisp/femtolisp_tm.hpp>, <verbatim|femtolisp_tm.cpp>>The
    interface of the Scheme interpreters of <TeXmacs> (<verbatim|tmscm>) on
    femtolisp, as <verbatim|S7/s7_tm.*> and <verbatim|Guile/guile_tm.*>.

    <item*|<verbatim|src/Scheme/Femtolisp/fl_boot.h>>The boot image as a C
    array, made by <verbatim|make-boot-header.sh>.

    <item*|<verbatim|TeXmacs/progs/init-femtolisp.scm>>The first Scheme file
    loaded. It loads the two files below and the module of the third, then the
    <verbatim|init-kernel.scm> and <verbatim|init-texmacs.scm> of all the
    interpreters.

    <item*|<verbatim|TeXmacs/progs/kernel/boot/r5rs-femtolisp.scm>>R5RS and
    the basic functions of Guile, defined on femtolisp.

    <item*|<verbatim|TeXmacs/progs/kernel/boot/boot-femtolisp.scm>>The
    modules, the loading of files, the lazy function bodies and the caches of
    compiled code.

    <item*|<verbatim|TeXmacs/progs/kernel/boot/compat-femtolisp.scm>>The
    library functions of Guile (hash tables, lists, strings...), as a module.

    <item*|<verbatim|docs/femtolisp/>>The working notes: the measurements, the
    history of the decisions.
  </description-long>

  <section|The main ideas>

  <\itemize>
    <item><em|Scheme values held by C++ are roots of the garbage collector.>
    The collector of femtolisp moves the objects, so a <verbatim|tmscm>
    registers itself in a list which the collector updates.

    <item><em|The errors of femtolisp never cross C++ code.> They are
    <verbatim|longjmp>s, which would skip the destructors.

    <item><em|Strings are strings of bytes>, in the Cork encoding of
    <TeXmacs>, without any conversion between C++ and Scheme.

    <item><em|A module is a set of renamed global names.> femtolisp has one
    global environment: the private name <verbatim|f> of the module
    <verbatim|(a b)> is the global name <verbatim|f@a/b>.

    <item><em|The body of a function is expanded and compiled when it is first called>,
    as Guile and S7 do, and the compiled code is kept in a cache between the
    sessions.
  </itemize>

  <section|The chapters>

  <\traverse>
    <branch|The binding with C++|femtolisp-binding.en.tm>

    <branch|Loading files, and the modules|femtolisp-modules.en.tm>

    <branch|Lazy function bodies and the caches of compiled code|femtolisp-lazy.en.tm>

    <branch|Scheme and Guile on femtolisp|femtolisp-compat.en.tm>

    <branch|The sources of femtolisp and their patches|femtolisp-vendored.en.tm>

    <branch|Debugging, testing and common tasks|femtolisp-howto.en.tm>
  </traverse>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
