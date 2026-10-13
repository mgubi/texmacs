<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|femtolisp: loading files, and the modules>

  <section|What is loaded at the start>

  <verbatim|init-femtolisp.scm> is loaded with the <verbatim|load> of
  femtolisp. It loads <verbatim|r5rs-femtolisp.scm>, then
  <verbatim|boot-femtolisp.scm>, both as plain global definitions; from there
  on <verbatim|load> is the one of <verbatim|boot-femtolisp.scm>. It then
  loads the module <verbatim|(kernel boot compat-femtolisp)>, and the
  <verbatim|init-kernel.scm> and <verbatim|init-texmacs.scm> which all the
  interpreters share.

  <section|A module is a set of renamed global names>

  femtolisp has one global environment, and the modules of <TeXmacs> define
  the same private names over and over (more than two hundred names are
  private to several modules). So the private definitions of a module are
  renamed. When the file of a module is loaded (<verbatim|%load-forms>):

  <\itemize>
    <item>Its forms are all read first, and scanned
    (<verbatim|%scan-definitions>) for the plain <verbatim|define> and
    <verbatim|define-macro> at the top level, also inside <verbatim|begin>,
    <verbatim|if>, <verbatim|when>, <verbatim|unless> and <verbatim|cond>.

    <item>Each such name <verbatim|f> of the module <verbatim|(a b)> is given
    the global name <verbatim|f@a/b> (<verbatim|%module-declare-private!>).

    <item>Each form is then expanded and compiled with
    <verbatim|*current-module*> bound to the module. The expander and the
    compiler of femtolisp call the hook <verbatim|resolve-global> on every
    global name (patch 0003); <verbatim|boot-femtolisp.scm> defines it to map
    the private names of the current module to their global names. This covers
    the variables, the functions, <verbatim|set!> and the macros.
  </itemize>

  The public definitions are global under their own names:
  <verbatim|define-public>, <verbatim|define-public-macro>,
  <verbatim|provide-public>, <verbatim|tm-define>, <verbatim|tm-define-macro>,
  <verbatim|tm-menu>, <verbatim|tm-widget>, <verbatim|menu-bind>,
  <verbatim|tm-property>, <verbatim|export>. A name which a module defines
  both with a plain <verbatim|define> and with <verbatim|tm-define> stays
  private in it, as in Guile, where the binding of the module hides the one of
  the user module.

  More exactly: only a name written as a plain <verbatim|define> or
  <verbatim|define-macro> at one of the places which are scanned is private.
  Every other definition is global, also the ones which a macro makes
  (<verbatim|define-table>, <verbatim|define-preferences>...). And a plain
  <verbatim|define> of a name is made public only by a
  <verbatim|define-public>, <verbatim|define-public-macro>,
  <verbatim|provide-public>, <verbatim|export> or <verbatim|re-export> of the
  same name in the file.

  A module is the vector <verbatim|#(module name privates)>, kept in the table
  <verbatim|*modules*>; <verbatim|privates> is the table of its private names.
  In the user module <verbatim|*current-module*> is <verbatim|#f> and nothing
  is renamed; <verbatim|(current-module)> then gives the symbol
  <verbatim|texmacs-user>.

  What differs from Guile:

  <\itemize>
    <item><verbatim|:use>, <verbatim|use-modules>, <verbatim|inherit-modules>
    and <verbatim|import-from> load the modules; they do not restrict what a
    module sees. Every public name is visible everywhere, as with S7.

    <item>The global name of a private definition can be used to reach it from
    outside, for debugging: <verbatim|(top-level-value (symbol "f@a/b"))>, or
    <verbatim|(module-ref (resolve-module '(a b)) 'f)>.
  </itemize>

  <section|<verbatim|tm-define>>

  <verbatim|kernel/texmacs/tm-define.scm> has branches for femtolisp, close to
  those for S7:

  <\itemize>
    <item>a definition is <verbatim|(define-global! 'f value)>: global under
    its quoted name, which <verbatim|resolve-global> does not rename;

    <item><verbatim|former> and the table of the names of the defined
    functions read the global function with <verbatim|(top-level-value 'f)>,
    also in a module which defines <verbatim|f> privately;

    <item><verbatim|tm-define-macro> defines the macro with
    <verbatim|define-public-macro>;

    <item><verbatim|lazy-define> finds the function with
    <verbatim|module-ref>.
  </itemize>

  <section|When macros are expanded>

  femtolisp expands the macros of a form when it compiles it; Guile and S7
  expand them when they first evaluate it. The code of <TeXmacs> relies on the
  second behaviour in three ways, each of which was a source of bugs:

  <\itemize>
    <item><em|A function uses a macro defined later>, in a module loaded later
    (<verbatim|delayed>, in <verbatim|tm-plugins.scm>).

    <item><em|Code which never runs is never expanded>: a call of a macro
    which fails to expand, in a widget which a configuration never shows, is
    harmless.

    <item><em|A macro with a side effect when it is expanded> has it only if
    the code runs: <verbatim|(when (supports-coq?) (lazy-keyboard ...))>
    declared the keyboard of Coq without Coq.
  </itemize>

  The lazy function bodies of the <hlink|next chapter|femtolisp-lazy.en.tm>
  answer the first two for the code inside functions: a body is expanded when
  it is first called. The third remains for the forms at the top level of a
  file, which are expanded when the file is loaded, whatever they test. Such
  macros were changed to have their effect when the code runs:
  <verbatim|lazy-format>, <verbatim|lazy-keyboard>, <verbatim|lazy-define>. A
  new macro must do the same:
  <em|its expansion may compute, it must not register anything.>

  With <verbatim|TEXMACS_FL_EAGER=1> the bodies are expanded when the file is
  loaded, as femtolisp does by itself. Two older devices then take over: a
  call of a name which is neither bound, nor a macro, nor defined by the file
  being loaded when it is compiled, is compiled at its first evaluation (the
  hook <verbatim|compile-unknown-call>, patch 0010, and
  <verbatim|%late-call>), and a macro whose expansion fails gives code which
  raises the error when it runs (<verbatim|*defer-macro-errors*>, patch 0013).

  <section|The functions of the compiler are protected>

  The functions of <verbatim|system.lsp> and <verbatim|compiler.lsp> call each
  other by their global names, and <TeXmacs> defines public functions with the
  same names (<verbatim|make-label>, <verbatim|print>...), which broke the
  compiler. The boot image is therefore made by compiling these two files
  twice (patch 0009): the second time, their references to each other go to
  private names <verbatim|%fl:name>, so <TeXmacs> may redefine the public
  ones.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
