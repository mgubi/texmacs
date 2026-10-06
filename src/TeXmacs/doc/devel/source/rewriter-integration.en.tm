<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Integration of the style rewriter with the editor>

  <section|Enabling the code>

  The preprocessor symbol <verbatim|EXPERIMENTAL> controls both the
  compilation of <verbatim|src/src/Style/> and the hooks in the editor:

  <\itemize>
    <item>CMake: <verbatim|cmake -DENABLE_EXPERIMENTAL=ON ...>. The option
    is declared in <source-link|CMakeLists.txt:466|src/CMakeLists.txt:466>; when it is on,
    <verbatim|Style/*.cpp> is globbed into <verbatim|TeXmacs_Style_SRCS> and
    <verbatim|add_compile_definitions (EXPERIMENTAL=1)> is executed;
    otherwise <verbatim|TeXmacs_Style_SRCS> is empty. The include
    directories of <verbatim|Style/> are added in both cases.

    <item>Autotools: <verbatim|./configure --enable-experimental>. The
    generated <verbatim|configure> then defines <verbatim|EXPERIMENTAL> in
    the configuration header and sets <verbatim|CONFIG_EXPERIMENTAL> to
    <verbatim|"Memorizer Environment Evaluate">, which
    <source-link|src/makefile.in|src/makefile.in> uses as the list of subdirectories of
    <verbatim|Style> to compile (<verbatim|style_src>).
  </itemize>

  In the default configuration the symbol is undefined
  (<verbatim|System/config.h> contains <verbatim|/* #undef
  EXPERIMENTAL */>), none of the hooks below exist and no file of
  <verbatim|Style/> is built. As explained in <hlink|the
  pitfalls|rewriter.en.tm>, an experimental build currently fails in three
  files of <verbatim|Style/Evaluate/>.

  <section|State kept by the editor>

  Under <verbatim|EXPERIMENTAL>, <cpp|editor_rep> (<source-link|Edit/editor.hpp|src/Edit/editor.hpp>)
  has three extra fields and one extra virtual method:

  <\description>
    <item*|<cpp|environment ste>>The \Pstyle environment\Q: a persistent
    environment holding the values of all environment variables at the
    start of the document.

    <item*|<cpp|tree cct>>The <em|clean copy> of the document body.

    <item*|<cpp|memorizer mem>>The memorizer of the last evaluation of
    <cpp|cct> in <cpp|ste>; its tree is the rewritten document.

    <item*|<cpp|environment_update ()>>Rebuilds <cpp|ste>; implemented by
    <cpp|edit_typeset_rep::environment_update>.
  </description>

  <section|The clean copy>

  The editor's own document lives in the global edit tree <cpp|the_et> and
  is modified <em|in place> by the modification routines; it cannot serve
  as a key for memoization, since an unchanged pointer does not mean
  unchanged contents. The clean copy is a second tree with the same
  contents, which is only ever updated <em|functionally>:

  <\itemize>
    <item>The constructor of <cpp|edit_main_rep>
    (<source-link|Edit/Editor/edit_main.cpp|src/Edit/Editor/edit_main.cpp>) sets <verbatim|cct= copy
    (subtree (et, rp))> and calls <cpp|copy_ip (subtree (et, rp), cct)>,
    and initializes <cpp|mem> to the null memorizer.

    <item>After every modification of the document, <cpp|edit_done>
    (<verbatim|Edit/Modify/edit_modify.cpp:253-255>) calls
    <cpp|copy_announce (subtree (ed-\<gtr\>et, ed-\<gtr\>rp), ed-\<gtr\>cct,
    mod / ed-\<gtr\>rp)>.
  </itemize>

  <cpp|copy_announce> (<source-link|Style/Memorizer/clean_copy.cpp|src/Style/Memorizer/clean_copy.cpp>) replaces
  <cpp|cct> by <cpp|clean_apply (cct, mod)>. The functions
  <cpp|clean_assign>, <cpp|clean_insert>, ... of
  <source-link|Kernel/Types/modification.cpp|src/Kernel/Types/modification.cpp> rebuild only the nodes on the
  path of the modification and reuse all other subtrees, so that
  unmodified parts of the new clean copy are <em|the same tree objects> as
  in the old one. <cpp|copy_ip> then gives the new nodes the inverse paths
  of the corresponding nodes of the real document: it walks both trees in
  parallel and, wherever the inverse paths differ, prepends an
  <cpp|ip_observer> with the source path to the observers of the copy. The
  recursion stops at shared subtrees, whose inverse paths already agree.
  Thanks to these inverse paths, the rewritten trees produced from the
  clean copy can be related to positions in the real document
  (<cpp|decorate_ip> in the evaluator).

  Note that <cpp|edit_done> is called for every modification kind,
  including <verbatim|MOD_SET_CURSOR>, for which <cpp|clean_apply> calls
  <cpp|clean_set_cursor>.

  <section|Re-evaluation after changes>

  <cpp|edit_interface_rep::apply_changes>
  (<verbatim|Edit/Interface/edit_interface.cpp:841-850>) contains, right
  after the normal typesetting step:

  <\cpp-code>
    #ifdef EXPERIMENTAL

    \ \ if (env_change & THE_ENVIRONMENT)

    \ \ \ \ environment_update ();

    \ \ if (env_change & THE_TREE) {

    \ \ \ \ cout \<less\>\<less\> HRULE;

    \ \ \ \ mem= evaluate (ste, cct);

    \ \ \ \ tree rew= mem-\<gtr\>get_tree ();

    \ \ \ \ cout \<less\>\<less\> HRULE;

    \ \ \ \ cout \<less\>\<less\> tree_to_texmacs (rew) \<less\>\<less\> LF;

    \ \ }

    #endif
  </cpp-code>

  <cpp|environment_update> (<verbatim|Edit/Editor/edit_typeset.cpp:351-362>)
  prepares the typesetting environment as for normal typesetting
  (<cpp|typeset_prepare>), writes <verbatim|base-file-name>,
  <verbatim|cur-file-name> and <verbatim|secure> into it, reads the whole
  environment into a hash map and calls <cpp|primitive (ste, h)>, which
  replaces <cpp|ste> by a new primitive environment built from the map.
  Since <cpp|ste> is a new object after each environment change, and the
  memo table is keyed on environment pointers, a change of the environment
  (style, document settings) causes a complete re-evaluation; a change of
  the tree alone keeps <cpp|ste> and can reuse earlier results.

  <cpp|evaluate (ste, cct)> returns the memorizer of the top-level
  evaluation, which is stored in <cpp|mem>. Storing it is what keeps the
  previous computation alive: its memorizers stay referenced from
  <cpp|mem> (through the children arrays) until the next evaluation has
  found them again, which is how <cpp|is_memorized> recognizes them (see
  <hlink|memoization|rewriter-memoization.en.tm>).

  The rewritten tree is printed and then dropped. No other part of
  <TeXmacs> reads <cpp|mem>: a search for <cpp|-\<gtr\>mem>, <cpp|ste> and
  <cpp|cct> outside these hooks finds no users. In particular the
  typesetter keeps using <cpp|edit_env_rep::exec>, and the result of the
  rewriter is not compared with it.

  <section|What a revival would need>

  Turning the prototype into something useful requires, at least:

  <\enumerate>
    <item>fixing the compilation errors listed in <hlink|the
    pitfalls|rewriter.en.tm>;

    <item>removing or guarding the unconditional traces in
    <cpp|evaluate>, <cpp|rewrite> and <cpp|evaluate_inactive>, and the
    printing in <cpp|apply_changes>;

    <item>implementing the missing primitives (see <hlink|the
    evaluator|rewriter-evaluator.en.tm>) and replacing the placeholders;

    <item>memoizing environment changes on the <em|contents> of the local
    bindings rather than on the pointer of a freshly created
    <cpp|assoc_environment>;

    <item>an actual consumer of the rewritten tree, for instance a
    comparison with the typesetter's expansion in a test suite.
  </enumerate>

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
