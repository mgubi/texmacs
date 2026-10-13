<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|femtolisp: its sources and their patches>

  <section|What is in the tree>

  <source-link|src/Scheme/Femtolisp/femtolisp/|src/Scheme/Femtolisp/femtolisp> holds femtolisp at its commit
  <verbatim|ec76010>, under its BSD license, with the local patches applied.
  Only the sources, the boot files and its two main test files are kept (the
  tests run in a clone, see below).

  <\description-long>
    <item*|The C core><source-link|flisp.c|src/Scheme/Femtolisp/femtolisp/flisp.c> (the values, the collector, the
    bytecode interpreter <verbatim|apply_cl>; it includes
    <source-link|cvalues.c|src/Scheme/Femtolisp/femtolisp/cvalues.c>, which includes <source-link|operators.c|src/Scheme/Femtolisp/femtolisp/operators.c>,
    <source-link|types.c|src/Scheme/Femtolisp/femtolisp/types.c>, <source-link|print.c|src/Scheme/Femtolisp/femtolisp/print.c>, <source-link|read.c|src/Scheme/Femtolisp/femtolisp/read.c> and
    <source-link|equal.c|src/Scheme/Femtolisp/femtolisp/equal.c>), <source-link|builtins.c|src/Scheme/Femtolisp/femtolisp/builtins.c>, <source-link|string.c|src/Scheme/Femtolisp/femtolisp/string.c>,
    <source-link|table.c|src/Scheme/Femtolisp/femtolisp/table.c>, <source-link|equalhash.c|src/Scheme/Femtolisp/femtolisp/equalhash.c>, <source-link|iostream.c|src/Scheme/Femtolisp/femtolisp/iostream.c>. About
    8800 lines with the headers.

    <item*|The library <verbatim|llt>>By the same author, with three files of
    others (<source-link|lookup3.c|src/Scheme/Femtolisp/femtolisp/llt/lookup3.c>, <source-link|mt19937ar.c|src/Scheme/Femtolisp/femtolisp/llt/mt19937ar.c>,
    <source-link|wcwidth.c|src/Scheme/Femtolisp/femtolisp/llt/wcwidth.c>): the streams (<verbatim|ios>), UTF-8, hash tables
    keyed by pointers, the hash functions, bit vectors, random numbers, time,
    paths. About 5800 lines. <source-link|socket.c|src/Scheme/Femtolisp/femtolisp/llt/socket.c> and
    <source-link|bitvector-ops.c|src/Scheme/Femtolisp/femtolisp/llt/bitvector-ops.c> are compiled and not used.

    <item*|The Lisp part><verbatim|system.lsp> (the standard library and the
    macro expander) and <verbatim|compiler.lsp> (the compiler to bytecode),
    about 1900 lines, and the boot image made from them,
    <verbatim|flisp.boot>: their compiled code, as text.
  </description-long>

  <source-link|fl_core.c|src/Scheme/Femtolisp/fl_core.c> and <source-link|fl_llt.c|src/Scheme/Femtolisp/fl_llt.c> include these sources, as two
  compilation units. They are compiled with the directory of <verbatim|llt> on
  the include path of these two files only: <verbatim|llt> has headers with
  common names (<source-link|utils.h|src/Scheme/Femtolisp/femtolisp/llt/utils.h>...).

  <section|The patches>

  Each change of femtolisp is a patch of <verbatim|patches/>, numbered in the
  order they apply. The hooks and the flags do nothing unless <TeXmacs> sets
  them; the syntax of the reader and of the printer, and a few forms (curried
  definitions, <verbatim|\<less\>> and <verbatim|=> with any number of
  arguments, the unspecified value), are changed for good. The tests of
  femtolisp, adapted to the new syntax of vectors by patch 0015, are run after
  a change (<verbatim|make test> below).

  <\description-long>
    <item*|For the embedding>0002: hooks for the roots of the collector and
    for the equality and the hash of opaque values. 0003:
    <verbatim|resolve-global>, the hook on the global names of the expanded
    and compiled code (the modules). 0009: the functions of
    <verbatim|system.lsp> and <verbatim|compiler.lsp> call each other through
    private names. 0010: <verbatim|compile-unknown-call>. 0011:
    <verbatim|*keep-source*>, the compiled functions keep their source. 0012:
    the heap stops growing at 1 GB with an error. 0013:
    <verbatim|*defer-macro-errors*>. 0022: a function with its source hashes
    as its source.

    <item*|To read and write as Guile>0004: <verbatim|\|> and <verbatim|\\> in
    symbols, <verbatim|#{...}#>, long tokens, integers too large read as
    inexact, floating point numbers written in the shortest form. 0005:
    vectors written <verbatim|#(...)>; <verbatim|*print-shared*>: with
    <verbatim|#f>, labels only for cycles. 0006: strings written as strings of
    bytes. 0014: a datum at the very end of the input is read. 0015:
    <verbatim|[> and <verbatim|]> are characters of symbols (they wrote
    vectors). 0018: <verbatim|*print-closures*>: with <verbatim|#f>, a closure
    is written <verbatim|#\<less\>procedure name\<gtr\>>. 0019: the
    unspecified value is the symbol <verbatim|#\<less\>unspecified\<gtr\>>,
    which evaluates to itself.

    <item*|To evaluate as Guile>0007: a builtin called with a wrong number of
    arguments is an error when the code runs, not when it is compiled. 0008:
    curried definitions, <verbatim|(define ((f a) b) ...)>. 0016: a local
    variable does not hide a special form at the head of a form (a parameter
    named <verbatim|begin> captured the <verbatim|begin> which <verbatim|cond>
    expands to). 0017: <verbatim|\<less\>> and <verbatim|=> with any number of
    arguments. 0020: <verbatim|*arith-fallback*>.

    <item*|Fixes>0001: <verbatim|llt> builds on any architecture (arm64,
    WebAssembly). 0021: the reader makes a vector without a label once its
    elements are read (growing it called the collector at each step).
  </description-long>

  <section|Changing femtolisp>

  <em|A change of a C file> is compiled with <TeXmacs>. Make the patch too, so
  that <verbatim|patches/> still gives the sources: in a clone of femtolisp
  with the patches applied, commit the change and run
  <verbatim|git format-patch>.

  <em|A change of <verbatim|system.lsp> or <verbatim|compiler.lsp>> needs a
  new boot image, which a standalone femtolisp makes, compiling the compiler
  with itself until the image no longer changes:

  <\shell-code>
    git clone https://github.com/JeffBezanson/femtolisp && cd femtolisp

    git checkout ec76010 && git am \<less\>tree\<gtr\>/src/Scheme/Femtolisp/patches/*.patch

    # change system.lsp or compiler.lsp here, commit, git format-patch

    make -C llt && make release

    ./flisp mkboot0.lsp system.lsp compiler.lsp \<gtr\> flisp.boot.new

    mv flisp.boot.new flisp.boot

    ./flisp mkboot1.lsp && ./flisp mkboot1.lsp

    make test

    cp system.lsp compiler.lsp flisp.boot \<less\>tree\<gtr\>/src/Scheme/Femtolisp/femtolisp/

    \<less\>tree\<gtr\>/src/Scheme/Femtolisp/make-boot-header.sh
  </shell-code>

  Here <verbatim|\<less\>tree\<gtr\>> is the directory <verbatim|src> of
  <TeXmacs>, which holds <verbatim|configure>. <verbatim|mkboot1.lsp> is run
  twice to reach a fixed point. <source-link|make-boot-header.sh|src/Scheme/Femtolisp/make-boot-header.sh> writes
  <source-link|fl_boot.h|src/Scheme/Femtolisp/fl_boot.h>, the image as a C array. When <TeXmacs> starts,
  <verbatim|initialize_scheme> computes a checksum of the image and defines it
  as <verbatim|*fl-boot-id*>, which is part of the key of the caches of
  compiled code: a new compiler makes them invalid by itself.

  <em|A new version of femtolisp>: apply the patches to it, in order, fix what
  no longer applies, copy the sources, make the boot image as above. Then
  check the two places which depend on the insides of femtolisp:
  <source-link|fl_core.c|src/Scheme/Femtolisp/fl_core.c>, which uses its stack and its types, and the lazy
  function bodies (<verbatim|%expand-top> follows <verbatim|expand>, and the
  stubs depend on the code which the compiler makes for them, see
  <hlink|the lazy bodies|femtolisp-lazy.en.tm>).

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
