<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <scheme> interpreter and the <c++>/<scheme> glue>

  <section|Introduction>

  Most of the user interface of <TeXmacs> (menus, keyboard bindings,
  editing commands, converters, dialogs) is written in <scheme>, while the
  document model, the typesetter and the rendering are written in <c++>.
  This chapter describes the layer which connects the two: how the
  interpreter is started, how <c++> code holds, inspects and calls <scheme>
  values, and how <c++> routines are made available as <scheme> procedures
  by the <em|glue>.

  It is written for developers who want to export a new <c++> routine to
  <scheme>, call <scheme> code from <c++>, or debug a problem at the
  boundary. The user level view of the <scheme> side (modules,
  <scm|tm-define>, contextual overloading) is described in the <hlink|Scheme
  developer guide|../scheme/scheme.en.tm>, and the list of all exported
  routines in <hlink|the glue auto-documentation|../scheme/api/glue-auto-doc.en.tm>.

  All file names below are relative to <verbatim|src/src/> unless stated
  otherwise.

  <section|Overview>

  The connection consists of four layers:

  <\description>
    <item*|The interpreter>The only back-end which is compiled is
    <name|Guile> (<verbatim|Scheme/Guile/>). Everything which depends on
    the <name|Guile> version is hidden behind a thin abstraction, the
    <verbatim|tmscm> layer: the type <cpp|tmscm> (a <name|Guile>
    <cpp|SCM>) and functions such as <cpp|tmscm_cons>,
    <cpp|tmscm_is_string> or <cpp|string_to_tmscm>.

    <item*|<c++> objects>The class <cpp|object> (<verbatim|Scheme/scheme.hpp>,
    <verbatim|Scheme/Scheme/object.cpp>) is a reference counted handle on a
    <scheme> value which protects it from the garbage collector. It comes
    with constructors from the usual <TeXmacs> types, predicates,
    conversions (<cpp|as_int>, <cpp|as_tree>, ...) and the functions
    <cpp|eval>, <cpp|call> and <cpp|exec_delayed>, by which <c++> code runs
    <scheme> code.

    <item*|Boxed <c++> values>Trees, <abbr|URL>s, commands, widgets,
    observers, patches and modifications are passed to <scheme> as
    <em|black boxes>: a single <name|Guile> <em|smob> type which wraps a
    <cpp|blackbox>, that is, a type tagged copy of the <c++> value.

    <item*|The glue>About 1200 <c++> routines are exported to <scheme>.
    They are declared in three tables (<verbatim|Scheme/Glue/build-glue-basic.scm>,
    <verbatim|build-glue-editor.scm>, <verbatim|build-glue-server.scm>),
    from which a generator written in <scheme> produces <c++> wrapper
    functions (<verbatim|glue_basic.cpp>, <verbatim|glue_editor.cpp>,
    <verbatim|glue_server.cpp>) that check and convert the arguments, call
    the routine and convert the result.
  </description>

  Calls go in both directions:

  <\verbatim-code>
    \ \ C++\ calls\ Scheme:\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Scheme\ calls\ C++:

    \;

    \ \ call\ ("menu-expand",\ ...)\ \ \ \ \ \ \ \ \ \ \ (show-header\ #f)

    \ \ \ \ \ \|\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ \ \ \ v\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ v

    \ \ object\ -\<gtr\>\ call_scheme\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ tmg_show_header\ (tmscm\ arg1)

    \ \ \ \ \ \|\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ check,\ convert,\ call

    \ \ \ \ \ v\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ Scheme\ code\ (progs/*.scm)\ \ \ \ \ \ \ \ \ \ \ \ \ v

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ get_server()-\<gtr\>show_header\ (false)
  </verbatim-code>

  <section|Source files>

  <\description>
    <item*|<verbatim|Scheme/scheme.hpp>>The interface seen by the rest of
    <TeXmacs>: the class <cpp|object>, its predicates and conversions,
    <cpp|eval>, <cpp|call>, <cpp|exec_delayed>, <cpp|protected_call>,
    <cpp|scheme_cmd> and the preference routines.

    <item*|<verbatim|Scheme/Scheme/object.hpp>, <verbatim|object.cpp>>The
    representation <cpp|tmscm_object_rep> of objects and the
    implementation of the interface above.

    <item*|<verbatim|Scheme/Scheme/glue.hpp>, <verbatim|glue.cpp>>The
    conversions between <cpp|tmscm> and all <TeXmacs> types used by the
    glue, the corresponding argument checks
    (<verbatim|TMSCM_ASSERT_<em|TYPE>>), a few helper routines which only
    exist to be exported, and <cpp|initialize_glue>, which installs all
    glue routines. The generated files are compiled as part of
    <verbatim|glue.cpp>, which includes them.

    <item*|<verbatim|Scheme/Guile/guile_tm.hpp>, <verbatim|guile_tm.cpp>>The
    <name|Guile> back-end: the <verbatim|tmscm> layer, the selection of the
    <name|Guile> <abbr|API> generation (<verbatim|GUILE_A> to
    <verbatim|GUILE_D>), <cpp|start_scheme>, <cpp|initialize_scheme>,
    evaluation with error catching, and the black box smob.

    <item*|<verbatim|Scheme/Tiny/>>An experimental <name|TinyScheme>
    back-end, not compiled by any of the build systems.

    <item*|<verbatim|Scheme/Glue/build-glue.scm>>The glue generator.

    <item*|<verbatim|Scheme/Glue/build-glue-basic.scm>,
    <verbatim|build-glue-editor.scm>, <verbatim|build-glue-server.scm>>The
    declarations of the exported routines.

    <item*|<verbatim|Scheme/Glue/glue_basic.cpp>, <verbatim|glue_editor.cpp>,
    <verbatim|glue_server.cpp>>The generated wrappers (do not edit).

    <item*|<verbatim|Scheme/Glue/build-glue>, <verbatim|build-auto-doc>,
    <verbatim|make-apidoc-module.scm>, <verbatim|make-apidoc-doc.scm>>Shell
    scripts which run the generator, and generators for the list of glue
    symbols (<verbatim|TeXmacs/progs/prog/glue-symbols.scm>) and for the
    glue documentation (<verbatim|TeXmacs/doc/devel/scheme/api/glue-auto-doc.en.tm>).

    <item*|<verbatim|Kernel/Abstractions/blackbox.hpp>>The type tagged
    containers <cpp|blackbox> and <cpp|whitebox_rep\<less\>T\<gtr\>>.

    <item*|<verbatim|TeXmacs/progs/kernel/texmacs/tm-define.scm>>The macros
    <scm|tm-define>, <scm|tm-property> and <scm|lazy-define>.
  </description>

  <section|Contents of this chapter>

  <\traverse>
    <branch|The interpreter and the tmscm layer|scheme-bridge-interpreter.en.tm>

    <branch|Scheme objects in C++|scheme-bridge-objects.en.tm>

    <branch|The glue and how to extend it|scheme-bridge-glue.en.tm>

    <branch|Pitfalls|scheme-bridge-pitfalls.en.tm>
  </traverse>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
