<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The glue and how to extend it>

  <section|Declaring glue routines>

  The <c++> routines which are visible from <scheme> are listed in three
  declaration files in <verbatim|Scheme/Glue/>. Each of them is a single
  call of the macro <scm|build>:

  <\scm-code>
    (build

    \ \ "get_server()-\<gtr\>" \ \ \ \ \ \ \ \ \ \ \ \ ; prefix of every call

    \ \ "initialize_glue_server" \ \ \ \ \ ; name of the generated init function

    \ \ (show-header show_header (void bool))

    \ \ (kbd-pre-rewrite kbd_pre_rewrite (string string))

    \ \ ...)
  </scm-code>

  Each entry has the form <verbatim|(<em|scheme-name> <em|cpp-name>
  (<em|return-type> <em|argument-type> ...))>. The three files differ by
  their prefix:

  <\description>
    <item*|<verbatim|build-glue-basic.scm>>No prefix: free functions. This
    is the largest file (about 820 routines): trees, paths, <abbr|URL>s,
    files, buffers, views and windows, widgets, conversions, fonts,
    databases, and so on.

    <item*|<verbatim|build-glue-editor.scm>>Prefix
    <verbatim|get_current_editor()-\<gtr\>>: methods of the current editor
    (about 310 routines), which must be declared in the abstract class
    <cpp|editor_rep> (<verbatim|Edit/editor.hpp>).

    <item*|<verbatim|build-glue-server.scm>>Prefix
    <verbatim|get_server()-\<gtr\>>: methods of the server (about 40
    routines), declared in <cpp|server_rep> (see <hlink|the server
    classes|server-classes.en.tm>).
  </description>

  <section|The generator>

  <verbatim|Scheme/Glue/build-glue.scm> defines <scm|build> as a macro
  which, when the declaration file is loaded, prints <c++> code on the
  standard output. For each entry it produces a function named after the
  <scheme> name: <verbatim|tmg_> followed by the name in which
  <verbatim|-> becomes <verbatim|_>, <verbatim|?> becomes <verbatim|P>,
  <verbatim|!> becomes <verbatim|S>, <verbatim|\<less\>> becomes
  <verbatim|F>, <verbatim|\<gtr\>> becomes <verbatim|2>, <verbatim|=>
  becomes <verbatim|Q>, and a final <verbatim|*> becomes
  <verbatim|_dot>. For instance, <scm|path-exists?> becomes
  <cpp|tmg_path_existsP>, <scm|cpp-path-\<gtr\>tree> becomes
  <cpp|tmg_cpp_path_2tree> and <scm|cursor-path*> becomes
  <cpp|tmg_cursor_path_dot>. The generated function

  <\enumerate>
    <item>checks each argument with <verbatim|TMSCM_ASSERT_<em|TYPE>>,
    where <em|TYPE> is the argument type in upper case;

    <item>converts it with <verbatim|tmscm_to_<em|type>>;

    <item>calls <verbatim|<em|prefix><em|cpp-name> (in1, in2, ...)>;

    <item>converts the result with <verbatim|<em|type>_to_tmscm>, or
    returns <verbatim|TMSCM_UNSPECIFIED> for <verbatim|void>.
  </enumerate>

  For example, the entry <verbatim|(kbd-pre-rewrite kbd_pre_rewrite
  (string string))> of <verbatim|build-glue-server.scm> yields

  <\cpp-code>
    tmscm

    tmg_kbd_pre_rewrite (tmscm arg1) {

    \ \ TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "kbd-pre-rewrite");

    \;

    \ \ string in1= tmscm_to_string (arg1);

    \;

    \ \ // TMSCM_DEFER_INTS;

    \ \ string out= get_server()-\<gtr\>kbd_pre_rewrite (in1);

    \ \ // TMSCM_ALLOW_INTS;

    \;

    \ \ return string_to_tmscm (out);

    }
  </cpp-code>

  At the end, the generator emits the initialization function named in the
  declaration file, which installs every routine with
  <cpp|tmscm_install_procedure> (all arguments are required; there are no
  optional or rest arguments). <cpp|initialize_glue>
  (<verbatim|Scheme/Scheme/glue.cpp>) calls the three initialization
  functions, after installing by hand the predicates <scm|tree?>,
  <scm|tm?>, <scm|observer?>, <scm|url?>, <scm|modification?>,
  <scm|patch?> and <scm|blackbox?>. The generated files are not compiled
  separately: <verbatim|glue.cpp> includes them after all the headers that
  the glued routines need.

  <section|Types>

  A type name <em|t> can be used in a declaration as soon as
  <verbatim|glue.cpp> (or <verbatim|guile_tm.hpp>) provides
  <verbatim|TMSCM_ASSERT_<em|T>>, <verbatim|tmscm_to_<em|t>> (for
  arguments) and <verbatim|<em|t>_to_tmscm> (for results). The types
  currently used are:

  <\description>
    <item*|Immediate values><verbatim|bool>, <verbatim|int>,
    <verbatim|uint> (arguments only), <verbatim|long> (results only),
    <verbatim|double>, <verbatim|string>, <verbatim|tree_label> (a
    symbol).

    <item*|Black boxes><verbatim|tree>, <verbatim|url> (a string is also
    accepted), <verbatim|observer>, <verbatim|widget>,
    <verbatim|promise_widget>, <verbatim|command>,
    <verbatim|modification>, <verbatim|patch>.

    <item*|Structured values><verbatim|path> (a list of integers),
    <verbatim|list_string> and <verbatim|list_tree> (results only),
    <verbatim|array_int>, <verbatim|array_SI> (results only),
    <verbatim|array_double>, <verbatim|array_array_array_double>
    (arguments only), <verbatim|array_string>, <verbatim|array_tree>,
    <verbatim|array_url>, <verbatim|array_widget> (arguments only),
    <verbatim|array_patch> (arguments only), <verbatim|array_path>.

    <item*|Generic values><verbatim|object> (any value, not checked),
    <verbatim|scheme_tree> and <verbatim|content>.
  </description>

  The last two deserve a comment. A <verbatim|scheme_tree> is a
  <cpp|tree> which encodes a <scheme> expression: lists become
  <markup|tuple>s, symbols become strings, strings are stored quoted,
  booleans become <verbatim|#t> or <verbatim|#f>, integers become their
  decimal representation and trees are converted with
  <cpp|tree_to_scheme_tree>; anything else, including floating point
  numbers, becomes <verbatim|"?">. A <verbatim|content> argument accepts
  a string, a tree, or a <scheme> expression <verbatim|(<em|label>
  <em|child> ...)> whose children are again content, and converts it to a
  tree; results of type <verbatim|content> are trees.

  To add a new type, define the three items above in
  <verbatim|glue.cpp> (and usually a <verbatim|tmscm_is_<em|t>>
  predicate). For a <c++> class which only needs to be passed around, the
  easiest is to box it, as is done for <cpp|command>:

  <\cpp-code>
    tmscm command_to_tmscm (command o) {

    \ \ return blackbox_to_tmscm (close_box\<less\>command\<gtr\> (o)); }

    command tmscm_to_command (tmscm o) {

    \ \ return open_box\<less\>command\<gtr\> (tmscm_to_blackbox (o)); }
  </cpp-code>

  together with a <verbatim|tmscm_is_<em|t>> test on <cpp|type_box> and
  the <verbatim|TMSCM_ASSERT_<em|T>> macro. If values of the new type
  should print nicely, add a case to <cpp|print_blackbox> in
  <verbatim|guile_tm.cpp>.

  <section|Regenerating the glue>

  The generated files <verbatim|glue_basic.cpp>, <verbatim|glue_editor.cpp>
  and <verbatim|glue_server.cpp> are part of the repository, and the
  <name|CMake> build compiles them as they are: it has no rule to
  regenerate them. After changing a declaration file, regenerate the
  corresponding file by hand, in the directory <verbatim|Scheme/Glue/>:

  <\verbatim-code>
    ./build-glue build-glue-basic.scm glue_basic.cpp [<em|guile-binary>]
  </verbatim-code>

  This needs a <name|Guile> interpreter (<verbatim|guile> by default, or
  the binary given as third argument or in <verbatim|GUILE_BIN>). In the
  traditional build, <verbatim|make GLUE> in <verbatim|src/src/>
  regenerates all three files with the <verbatim|GUILE_BIN> found by
  <verbatim|configure>.

  <verbatim|build-glue> first runs <verbatim|build-auto-doc>, which
  regenerates two files from the three declaration files:

  <\itemize>
    <item><verbatim|TeXmacs/progs/prog/glue-symbols.scm>
    (<verbatim|make-apidoc-module.scm>), the list of all glued symbols
    returned by <scm|all-glued-symbols>, used for the completion of
    <scheme> code (<verbatim|prog/scheme-autocomplete.scm>);

    <item><verbatim|TeXmacs/doc/devel/scheme/api/glue-auto-doc.en.tm>
    (<verbatim|make-apidoc-doc.scm>), <hlink|the reference of all glue
    routines|../scheme/api/glue-auto-doc.en.tm>.
  </itemize>

  These two files, too, are committed, and should be regenerated together
  with the glue so that they stay in sync. At the time of writing, all
  three generated <c++> files and <verbatim|glue-symbols.scm> agree with
  the declarations (1181 routines in total).

  <section|Adding a glue routine, step by step>

  <\enumerate>
    <item>Write the <c++> routine. For a free function, declare it in a
    header and make sure that this header is included in the
    \PGluing\Q section near the end of <verbatim|Scheme/Scheme/glue.cpp>
    (many headers are already there). For an editor method, add a pure
    virtual declaration to <cpp|editor_rep> in <verbatim|Edit/editor.hpp>
    and implement it in the appropriate <verbatim|edit_*_rep> class; for a
    server method, likewise in <cpp|server_rep> and the classes of
    <verbatim|Texmacs/>. Use only argument and return types from the list
    above, or add a new type.

    <item>Add an entry to the right declaration file, for instance (for a
    hypothetical routine)

    <\scm-code>
      (buffer-word-count buffer_word_count (int url))
    </scm-code>

    Follow the naming conventions of <scheme>: <verbatim|?> for
    predicates, <verbatim|!> for destructive operations, and the prefix
    <verbatim|cpp-> for low level routines that are meant to be wrapped by
    a <scheme> function of the same name without the prefix (there are
    about 50 of these, such as <scm|cpp-buffer-close>, which is wrapped by
    <scm|buffer-close> in <verbatim|texmacs/texmacs/tm-server.scm>).

    <item>Regenerate the glue as explained above and check the diff: the
    generated files, <verbatim|glue-symbols.scm> and
    <verbatim|glue-auto-doc.en.tm> should all have changed.

    <item>Rebuild <TeXmacs>. The new procedure is defined when the
    interpreter starts, before <verbatim|init-texmacs.scm> is loaded, and
    can be called from any module.

    <item>If the routine is part of the user level <abbr|API>, wrap it
    with <scm|tm-define> on the <scheme> side to give it a synopsis,
    argument descriptions, or contextual variants.
  </enumerate>

  <section|On the <scheme> side: <scm|define> and <scm|tm-define>>

  <scheme> code in <TeXmacs> is organized in modules declared with
  <scm|texmacs-module> (see <hlink|the module system and lazy
  definitions|../scheme/overview/overview-lazyness.en.tm>). Within a
  module, <scm|define> creates a private definition and
  <scm|define-public> an exported one. <scm|tm-define>
  (<verbatim|kernel/texmacs/tm-define.scm>) is different:

  <\itemize>
    <item>it always defines the function in the global module
    <scm|texmacs-user>, so that it is visible everywhere;

    <item>a second <scm|tm-define> of the same name does not replace the
    function but <em|overloads> it: the new definition may carry
    conditions such as <scm|(:mode in-math?)> or <scm|(:require ...)>, and
    falls back on the previous definition (available as <scm|former>)
    when they are not met (see <hlink|contextual
    overloading|../scheme/overview/overview-overloading.en.tm>);

    <item>it accepts properties such as <scm|:synopsis>, <scm|:argument>,
    <scm|:proposals>, <scm|:secure>, <scm|:interactive>,
    <scm|:check-mark> or <scm|:balloon>, which are used by menus,
    interactive commands and the documentation; <scm|tm-property> adds
    such properties to an existing function.
  </itemize>

  <scm|lazy-define> declares that a function is defined in a module which
  is only loaded when the function is first called. Glue routines are
  plain procedures: they have none of these properties unless a
  <scm|tm-define> or <scm|tm-property> is written for them.

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
