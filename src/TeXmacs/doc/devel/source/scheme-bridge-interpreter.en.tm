<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The interpreter and the tmscm layer>

  <section|Back-ends>

  <TeXmacs> currently compiles exactly one <scheme> back-end, <name|Guile>.
  Both build systems only include the directories
  <verbatim|Scheme/Scheme> and <verbatim|Scheme/Guile>:
  <name|CMake> through the <verbatim|TeXmacs_Scheme_SRCS> glob in the top
  level <verbatim|CMakeLists.txt> (in the directory above
  <verbatim|src/src/>), the traditional build through
  <verbatim|scheme_src> in <verbatim|makefile.in>. The header
  <verbatim|Scheme/Scheme/object.hpp> includes
  <verbatim|Scheme/Guile/guile_tm.hpp> unconditionally.

  <\description>
    <item*|<name|Guile>>The <name|CMake> option <verbatim|SCHEME_IMPL>
    selects either the embedded <name|Guile> 1.8 (the default
    <verbatim|embedded18>, which expects a <verbatim|tm-guile188>
    directory next to the top level <verbatim|CMakeLists.txt>; it is not
    part of this source tree and has to be provided separately) or a
    system <name|Guile> found with <name|pkg-config> (<verbatim|guile-1.8>,
    <verbatim|guile-3.0>, <verbatim|guile-2.2> or <verbatim|guile-2.0>).
    The traditional build detects <name|Guile> with
    <verbatim|misc/m4/guile.m4>.

    <item*|<name|TinyScheme>>The directory <verbatim|Scheme/Tiny/>
    contains an experimental back-end based on <name|TinyScheme>
    (<verbatim|tinyscheme_tm.cpp>, <verbatim|tinyscheme_tm.hpp> and the
    interpreter itself). It is not compiled by either build system, and
    the commented out include in <verbatim|object.hpp> refers to a file
    name (<verbatim|tinytmscm_tm.hpp>) which does not exist. It is useful
    as an illustration of what a back-end must provide, but should not be
    expected to build.

    <item*|Other interpreters>No other back-end (such as <name|S7>) is
    part of this source tree.
  </description>

  <section|<name|Guile> versions>

  The <name|Guile> <abbr|API> changed several times. The configuration
  defines exactly one of the macros <verbatim|GUILE_A>,
  <verbatim|GUILE_B>, <verbatim|GUILE_C> or <verbatim|GUILE_D>
  (<verbatim|System/config.h.cmake>), and <verbatim|guile_tm.hpp> maps a
  common set of names (<cpp|scm_is_list>, <cpp|scm_str2scm>,
  <cpp|scm_new_procedure>, <cpp|scm_lookup_string>, ...) onto the
  corresponding calls of that generation:

  <\description>
    <item*|<verbatim|GUILE_A>, <verbatim|GUILE_B>>Old versions which still
    use the <verbatim|gh_> interface (<verbatim|guile/gh.h>).

    <item*|<verbatim|GUILE_C>><name|Guile> 1.8 (also the embedded
    version), with <verbatim|libguile.h> or, if
    <verbatim|GUILE_HEADER_18> is defined, <verbatim|libguile18.h>.

    <item*|<verbatim|GUILE_D>>Newer versions (<name|CMake> selects it for a
    system <name|Guile> 2.0 or later). The main difference with
    <verbatim|GUILE_C> is the cast of procedure pointers in
    <cpp|scm_new_procedure>.
  </description>

  The glue routine <scm|scheme-dialect> returns <verbatim|"guile-a"> to
  <verbatim|"guile-d">. For instance, <verbatim|init-texmacs.scm> uses it to
  decide how to wrap <scm|primitive-load>: for the newer versions it binds
  the <scm|current-reader> fluid explicitly, so that files loaded through
  the module system are read with the right reader options.

  <section|The tmscm layer>

  Code outside <verbatim|Scheme/Guile/> should not use the <name|Guile>
  <abbr|API> directly, but the abstraction of <verbatim|guile_tm.hpp>:

  <\description>
    <item*|The type>A <cpp|tmscm> is a <name|Guile> <cpp|SCM>.

    <item*|Constructors and accessors><cpp|tmscm_null>, <cpp|tmscm_true>,
    <cpp|tmscm_false>, <cpp|tmscm_cons>, <cpp|tmscm_car>, <cpp|tmscm_cdr>
    and the usual compositions up to <cpp|tmscm_cadddr>,
    <cpp|tmscm_set_car>, <cpp|tmscm_set_cdr>.

    <item*|Predicates><cpp|tmscm_is_null>, <cpp|tmscm_is_pair>,
    <cpp|tmscm_is_list>, <cpp|tmscm_is_bool>, <cpp|tmscm_is_int>,
    <cpp|tmscm_is_double>, <cpp|tmscm_is_string>, <cpp|tmscm_is_symbol>,
    <cpp|tmscm_is_equal> (<scm|equal?>) and <cpp|tmscm_is_blackbox>.

    <item*|Basic conversions><cpp|bool_to_tmscm>, <cpp|int_to_tmscm>,
    <cpp|long_to_tmscm>, <cpp|double_to_tmscm>, <cpp|string_to_tmscm>,
    <cpp|symbol_to_tmscm> and, in the other direction,
    <cpp|tmscm_to_bool>, <cpp|tmscm_to_int>, <cpp|tmscm_to_uint>,
    <cpp|tmscm_to_double>, <cpp|tmscm_to_string>, <cpp|tmscm_to_symbol>.
    The conversions of all other types are in
    <verbatim|Scheme/Scheme/glue.cpp> (see <hlink|the glue|scheme-bridge-glue.en.tm>).

    <item*|Evaluation><cpp|eval_scheme (string)>,
    <cpp|eval_scheme_file (string)> and <cpp|call_scheme (fun, ...)> with
    up to four arguments or an <cpp|array\<less\>tmscm\<gtr\>>.

    <item*|Installing procedures><cpp|tmscm_install_procedure (name, func,
    args, opt, rest)> defines a <scheme> procedure implemented by a
    <c++> function taking <cpp|tmscm> arguments
    (<cpp|scm_c_define_gsubr> for recent versions).

    <item*|Argument checks><cpp|TMSCM_ASSERT (cond, arg, pos, name)>
    raises a <scheme> <verbatim|wrong-type-arg> error;
    <verbatim|TMSCM_ARG1> to <verbatim|TMSCM_ARG10> give the argument
    position, and <verbatim|TMSCM_UNSPECIFIED> is the value returned by
    procedures without result.
  </description>

  <section|Starting the interpreter>

  Starting the interpreter takes two steps, which are described from the
  point of view of the main program in <hlink|the main program and crash
  handling|server-layer-startup.en.tm>:

  <\description>
    <item*|<cpp|start_scheme (argc, argv, call_back)>>Called by
    <cpp|texmacs_entrypoint>. For <verbatim|GUILE_C> and
    <verbatim|GUILE_D> it calls <cpp|scm_boot_guile>, which runs
    <cpp|call_back> (that is, <cpp|TeXmacs_main>) inside the <name|Guile>
    runtime and never returns; for the older versions it uses
    <cpp|gh_enter>.

    <item*|<cpp|initialize_scheme ()>>Called by the constructor of the
    server. It evaluates a small bootstrap program which sets the reader
    options (keywords written <verbatim|:key>, source positions,
    debugging), defines <scm|display-to-string>, <scm|object-\<gtr\>string>,
    <scm|texmacs-version> and the list <scm|object-stack> (see <hlink|Scheme
    objects in C++|scheme-bridge-objects.en.tm>), registers the black box
    smob type (<cpp|initialize_smobs>) and installs all glue routines
    (<cpp|initialize_glue>). Only then does the server load
    <verbatim|init-texmacs.scm>.
  </description>

  <section|Error handling>

  Unless <verbatim|DEBUG_ON> is defined, every evaluation started from
  <c++> (<cpp|eval_scheme>, <cpp|eval_scheme_file>, <cpp|call_scheme>) is
  wrapped in two <name|Guile> catches:

  <\enumerate>
    <item>an inner <em|lazy> catch (<cpp|TeXmacs_lazy_catcher>), which
    prints the error message and backtrace on the error port while the
    stack is still intact and then rethrows;

    <item>an outer catch (<cpp|TeXmacs_catcher>), which stops the error and
    returns the pair <verbatim|(<em|key> . <em|args>)> as the result of the
    evaluation.
  </enumerate>

  So a <scheme> error never unwinds into the <c++> caller: the caller
  simply receives a pair instead of the expected value. Since the
  conversions of <cpp|object> return neutral values for unexpected types,
  the error is in practice only visible in the console. With
  <verbatim|DEBUG_ON>, the evaluations are not protected at all.

  <section|Black boxes>

  All <c++> values which have no natural <scheme> representation are
  passed as black boxes. A <cpp|blackbox> (<verbatim|Kernel/Abstractions/blackbox.hpp>)
  is a reference counted pointer to a <cpp|whitebox_rep\<less\>T\<gtr\>>,
  which holds a copy of a value of type <cpp|T> together with the type
  identifier <cpp|type_helper\<less\>T\<gtr\>::id>; <cpp|close_box> and
  <cpp|open_box> wrap and unwrap values, and <cpp|open_box> asserts that
  the type matches.

  On the <scheme> side there is a single smob type, created by
  <cpp|initialize_smobs> in <verbatim|guile_tm.cpp>. The smob holds a
  heap allocated <cpp|blackbox>; its free function deletes it (which
  decrements the reference count of the boxed value), its print function
  shows <verbatim|\<less\>tree ...\<gtr\>>, <verbatim|\<less\>url
  ...\<gtr\>>, <verbatim|\<less\>widget\<gtr\>> and so on depending on
  the type, and its equality function makes <scm|equal?> compare the
  boxed <c++> values with their <cpp|operator ==>. Trees, <abbr|URL>s,
  observers, widgets, widget promises, commands, modifications and patches
  are all represented this way; the predicates <scm|tree?>,
  <scm|observer?>, <scm|url?>, <scm|modification?>, <scm|patch?> and
  <scm|blackbox?> test the type tag. (<abbr|URL>s are special: the glue
  also accepts a plain string wherever a <abbr|URL> is expected.)

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
