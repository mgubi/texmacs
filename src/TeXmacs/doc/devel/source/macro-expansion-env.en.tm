<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The typesetting environment>

  <section|The class <cpp|edit_env>>

  The typesetting environment is an object of class <cpp|edit_env>, a
  reference-counted pointer to an <cpp|edit_env_rep>
  (<verbatim|Typeset/env.hpp>). Each editor owns exactly one environment
  (the member <cpp|env> of <cpp|edit_typeset_rep>), which is shared by the
  typesetter of that editor (<cpp|typesetter_rep::env>) and by all its
  bridges and concaters. Temporary environments are also created for
  computing the environment of a style (<cpp|compute_env_and_drd>, see
  below) and in a few other places.

  The most important data members are:

  <\description>
    <item*|<cpp|drd_info& drd>>A <em|reference> to the data relation
    descriptor of the buffer (the member <cpp|drd> of <cpp|editor_rep>). The
    environment uses it for <markup|drd-props> and for accessibility tests,
    and the DRD is in turn largely computed from the macros of the
    environment; see <hlink|the data relation
    descriptor|macro-expansion-drd.en.tm>.

    <item*|<cpp|hashmap\<less\>string,tree\<gtr\> env>>(private) The current
    values of all variables. Macros are ordinary values of the form
    <scm|(macro x1 ... xn body)>, <scm|(xmacro x body)> or <scm|(func ...)>.

    <item*|<cpp|hashmap\<less\>string,tree\<gtr\> back>>(private) The old
    values of variables written with the <em|monitored> write methods since
    the last <cpp|local_start>. It is used by bridges to compute and cache
    the net effect of a paragraph on the environment.

    <item*|<cpp|list\<less\>hashmap\<less\>string,tree\<gtr\> \<gtr\>
    macro_arg>>The stack of macro argument frames. The head of the list
    binds the parameters of the innermost macro which is being expanded to
    the corresponding argument trees.

    <item*|<cpp|list\<less\>hashmap\<less\>string,path\<gtr\> \<gtr\>
    macro_src>>A parallel stack which binds the same parameters to the
    <em|inverse paths> of the argument trees in the source document (or to a
    decoration path, when the argument is not part of the document).

    <item*|<cpp|hashmap\<less\>string,int\<gtr\>& var_type>>A reference to
    the global table <cpp|default_var_type> which assigns an <verbatim|Env_*>
    category to each built-in variable (see <cpp|update> below).

    <item*|<cpp|base_file_name>, <cpp|cur_file_name>, <cpp|secure>>The
    master file of the buffer, the file currently being typeset (which
    differs inside an <markup|include>) and whether that file is trusted
    for the execution of <markup|extern> scripts.

    <item*|<cpp|local_ref>, <cpp|global_ref>, <cpp|local_aux>,
    <cpp|global_aux>, <cpp|local_att>, <cpp|global_att>>References to the
    tables of labels, auxiliary data and attachments of the buffer (or of the
    project), which are read and written by primitives like
    <markup|set-binding> and <markup|get-binding>.

    <item*|<cpp|complete>, <cpp|read_only>, <cpp|missing>,
    <cpp|redefined>, <cpp|touched>, <cpp|link_env>>Information collected
    while typesetting, such as missing references or loci.
  </description>

  The remaining members (<cpp|fn>, <cpp|pen>, <cpp|lan>, <cpp|mode>,
  <cpp|index_level>, <cpp|page_width>, <cpp|point_style>, ...) are C++
  caches of the values of certain environment variables, which the
  typesetter reads directly for speed. They are kept up to date by the
  <cpp|update> methods.

  The member <cpp|src> (a <cpp|hashmap\<less\>string,path\<gtr\>>
  initialized to a decoration path) is declared but does not appear to be
  used anywhere else in the code.

  <section|Reading and writing variables>

  <\explain>
    <cpp|tree read (string s)>

    <cpp|bool provides (string s)><explain-synopsis|basic access>
  <|explain>
    <cpp|read> returns the current value of <cpp|s> (or <verbatim|UNINIT>
    if <cpp|s> is undefined) and <cpp|provides> tests whether <cpp|s> is
    defined. The helpers <cpp|get_bool>, <cpp|get_int>, <cpp|get_double>,
    <cpp|get_string>, <cpp|get_length>, <cpp|get_vspace> and
    <cpp|get_color> convert the value; note that they return a default value
    (<cpp|false>, <cpp|0>, <cpp|"">) when the value is a compound tree, and
    that they do <em|not> evaluate the value.
  </explain>

  <\explain>
    <cpp|void write (string s, tree t)>

    <cpp|void write_update (string s, tree t)>

    <cpp|void monitored_write (string s, tree t)>

    <cpp|void monitored_write_update (string s, tree t)><explain-synopsis|low
    level writes>
  <|explain>
    The <verbatim|_update> variants call <cpp|update (s)> after writing, so
    that the C++ caches are recomputed. The <verbatim|monitored_> variants
    first record the old value of <cpp|s> in <cpp|back> (using
    <cpp|hashmap::write_back>, which only records the <em|first> old value),
    so that the change can be undone or cached by the enclosing bridge. None
    of these methods evaluates <cpp|t>.
  </explain>

  <\explain>
    <cpp|void assign (string s, tree t)><explain-synopsis|evaluate and
    assign>
  <|explain>
    Evaluates <cpp|t> with <cpp|exec> and, if the result differs from the
    current value, stores it in a monitored way and calls <cpp|update (s)>.
    This is the implementation of the <markup|assign> primitive.
  </explain>

  <\explain>
    <cpp|tree local_begin (string s, tree t)>

    <cpp|void local_end (string s, tree t)><explain-synopsis|temporary
    changes from C++>
  <|explain>
    <cpp|local_begin> writes <cpp|t> into <cpp|s>, updates and returns the
    old value, which must later be restored with <cpp|local_end>. This is the
    idiom used by the typesetter itself for temporary changes such as the
    script level (<cpp|local_begin_script>) or the extents of a box
    (<cpp|local_begin_extents>). Such changes are not monitored.
  </explain>

  <\explain>
    <cpp|void update (string s)>

    <cpp|void update ()><explain-synopsis|recompute C++ caches>
  <|explain>
    <cpp|update (s)> dispatches on <cpp|var_type[s]>: for
    <verbatim|Env_User> and <verbatim|Env_Fixed> nothing happens; for
    <verbatim|Env_Font>, <verbatim|Env_Font_Size> the font <cpp|fn> is
    recomputed by <cpp|update_font>; for <verbatim|Env_Mode> the mode,
    language and font are recomputed; for <verbatim|Env_Color>,
    <verbatim|Env_Page>, <verbatim|Env_Frame> and so on the corresponding
    <verbatim|update_*> method is called. <cpp|update ()> recomputes all
    caches. The table <cpp|default_var_type> is filled by
    <cpp|initialize_default_var_type> in <verbatim|env_semantics.cpp>; all
    variables which are not listed there, and in particular all macros and
    user variables, are of category <verbatim|Env_User>.
  </explain>

  When you add a new built-in environment variable whose value must be
  cached in a C++ field of <cpp|edit_env_rep>, you have to declare its name
  in <verbatim|Data/Drd/vars.hpp> and <verbatim|vars.cpp>, give it a
  default value in <cpp|initialize_default_env>
  (<verbatim|env_default.cpp>), assign it a category in
  <cpp|initialize_default_var_type> and handle that category in both
  <cpp|update> methods.

  <section|Dynamic scoping>

  Variables in <TeXmacs> are <em|dynamically scoped>. There is a single
  environment, which is modified while the typesetter or the evaluator
  traverses the document, and every tree sees the values which were current
  at the moment it is processed:

  <\itemize>
    <item><markup|with> evaluates the new values, writes them with
    <cpp|write_update>, processes its body and then restores the old values
    in reverse order (<cpp|edit_env_rep::exec_with>,
    <cpp|concater_rep::typeset_with>, <cpp|bridge_with_rep::my_typeset>).

    <item><markup|assign> changes the value for the <em|remainder> of the
    traversal. There is no automatic restoration at the end of a macro body
    or of a paragraph: an assignment inside a macro remains visible after the
    macro application, and an assignment in one paragraph is visible in all
    following paragraphs. This is how counters and style definitions work.

    <item>A macro body is expanded in the environment of the <em|call site>,
    not of the definition site: <scm|(macro "x" (value "font-size"))> yields
    the font size at the place where the macro is used.

    <item>Macro parameters are <em|not> environment variables. They are
    bound in a separate stack of frames (<cpp|macro_arg>), and
    <markup|arg> always looks in the innermost frame only (see <hlink|the
    evaluator|macro-expansion-exec.en.tm>).
  </itemize>

  <subsection|Caching the effect of paragraphs>

  Because <markup|assign> has global effect, the typesetter needs to know
  how each paragraph changes the environment, in order to be able to skip
  paragraphs which do not need to be re-typeset. This is implemented by the
  monitored writes and by three methods of <cpp|edit_env_rep>, which are
  called by <cpp|bridge_rep::typeset> (<verbatim|Typeset/Bridge/bridge.cpp>)
  around the typesetting of a bridge:

  <\cpp-code>
    hashmap\<less\>string,tree\<gtr\> prev_back (UNINIT);

    ...

    env-\<gtr\>local_start (prev_back);

    my_typeset (desired_status);

    env-\<gtr\>local_update (ttt-\<gtr\>old_patch, changes);

    env-\<gtr\>local_end (prev_back);
  </cpp-code>

  <cpp|local_start> saves the current <cpp|back> table and starts a fresh
  one; during <cpp|my_typeset>, every monitored write records the original
  value of the variable in <cpp|back>. <cpp|local_update> computes from
  <cpp|back> and the current <cpp|env> the table <cpp|changes> of the new
  values of all variables which were modified (<cpp|invert (back, env)>),
  and updates the typesetter's <cpp|old_patch>. <cpp|local_end> merges the
  recorded old values into the saved outer table, so that the outer bridge
  also sees the changes. The next time the bridge is found to be valid, the
  typesetter does not re-typeset it but simply replays its changes with
  <cpp|monitored_patch_env (changes)>.

  Only writes which are meant to <em|persist> must be monitored:
  <cpp|assign> and <cpp|exec_until_*> use monitored writes, whereas
  <markup|with>, whose effect is undone at the end, uses plain
  <cpp|write_update> during typesetting. When you write C++ code which
  changes the environment in a persistent way, use a monitored write, or
  cached paragraphs will not reproduce the change.

  <section|The default environment>

  The global table <cpp|default_env> is filled once by
  <cpp|initialize_default_env> in <verbatim|Typeset/Env/env_default.cpp>.
  It contains the default values of all built-in variables (as strings or
  trees), and also a few built-in macros, for instance

  <\cpp-code>
    tree identity_m (MACRO, "x", tree (ARG, "x"));

    tree tabular_m (MACRO, "x", tree (TFORMAT, tree (ARG, "x")));

    ...

    env (IDENTITY)         = identity_m;  // identity macro

    env (TABULAR)          = tabular_m;   // tabular macro
  </cpp-code>

  The constructor of <cpp|edit_env_rep> copies <cpp|default_env>, calls
  <cpp|style_init_env> (which reads <verbatim|dpi>, page flexibility and
  the first page number and resets <cpp|back>) and <cpp|update ()>.
  <cpp|write_default_env> resets the whole environment to a copy of
  <cpp|default_env>. The built-in variables are documented for users in
  <hlink|built-in environment
  variables|../format/environment/environment.en.tm>.

  <section|Lengths>

  Lengths are strings such as <verbatim|2cm>, <verbatim|1.5fn> or
  <verbatim|0.5par>, or trees of the form <scm|(tmlen def)> or
  <scm|(tmlen min def max)> whose entries are numbers of internal units
  (<cpp|SI>). The conversion is done by <cpp|as_tmlen> in
  <verbatim|Typeset/Env/env_length.cpp>:

  <\cpp-code>
    parse_length (s (start, n), len, unit);

    if (unit == "error" \|\| is_empty (unit)) {

    \ \ return tree (TMLEN, "0");

    } else {

    \ \ return tmlen_times (len, as_tmlen (exec (compound (unit * "-length"))));

    }
  </cpp-code>

  In other words, a unit <verbatim|u> is the value of the tag
  <markup|u-length>. For the standard units this is a built-in primitive
  (<markup|cm-length> is evaluated by <cpp|exec_cm_length>,
  <markup|fn-length> by <cpp|exec_fn_length>, etc.), which uses the cached
  C++ fields of the environment (such as <cpp|inch>, <cpp|magn_len> or the
  metrics of the current font <cpp|fn>). Since <cpp|exec> expands macros,
  a style file can define new units by defining macros, for instance
  <scm|(assign "foo-length" (macro "3mm"))>; this is also why lengths like
  <verbatim|par> or <verbatim|pag> depend on the environment in which they
  are evaluated. <cpp|as_length> returns the default value of the
  <markup|tmlen> as an <cpp|SI>; the variants <cpp|as_hspace> and
  <cpp|as_vspace> keep the stretchability. Length arithmetic
  (<cpp|tmlen_plus>, <cpp|tmlen_times>, <cpp|tmlen_over>, ...) is used by
  the arithmetic primitives <markup|plus>, <markup|times> and so on when
  their arguments are lengths. See also <hlink|<TeXmacs>
  lengths|../format/basics/lengths.en.tm>.

  <section|From style files to the initial environment>

  The initial environment of a buffer is computed from its style (the
  <markup|style> tuple of the document) and from its initial
  environment (the <markup|initial> collection). This happens in
  <verbatim|Edit/Editor/edit_typeset.cpp>:

  <\cpp-code>
    void

    edit_typeset_rep::typeset_preamble () {

    \ \ env-\<gtr\>write_default_env ();

    \ \ typeset_style_use_cache (the_style);

    \ \ env-\<gtr\>update ();

    \ \ env-\<gtr\>read_env (stydef);

    \ \ env-\<gtr\>patch_env (init);

    \ \ env-\<gtr\>update ();

    \ \ env-\<gtr\>read_env (pre);

    \ \ drd-\<gtr\>heuristic_init (pre);

    }
  </cpp-code>

  The environment after loading the style is saved in <cpp|stydef>, the
  environment after adding the <markup|initial> values in <cpp|pre>.
  Finally the DRD is refined heuristically using the macros in <cpp|pre>.
  Before every typesetting pass, <cpp|typeset_prepare> resets the
  environment to <cpp|pre>:

  <\cpp-code>
    env-\<gtr\>write_default_env ();

    env-\<gtr\>patch_env (pre);

    env-\<gtr\>style_init_env ();

    env-\<gtr\>update ();
  </cpp-code>

  so that assignments made in the body of the document during the previous
  pass do not leak into the next one.

  <subsection|Loading and caching styles>

  <cpp|typeset_style_use_cache> first calls <cpp|preprocess_style>
  (<verbatim|Data/Document/new_style.cpp>), which turns an atomic style into
  a tuple and replaces each package name <verbatim|p> by the absolute name
  of a file <verbatim|p.ts> found in the directory of the document or one of
  its ancestors, if there is such a file (this allows documents to override
  standard packages locally). It then looks up the style in a two-level
  cache (<cpp|style_get_cache>): an in-memory table, and files in
  <verbatim|$TEXMACS_HOME_PATH/system/cache> whose names are derived from
  the style tuple. The cache stores, for each style tuple, the resulting
  environment and the local part of the DRD
  (<cpp|drd_info_rep::get_locals>).

  On a cache miss, <cpp|get_style_env> and <cpp|get_style_drd> call
  <cpp|compute_env_and_drd>, which

  <\enumerate>
    <item>creates a fresh <cpp|drd_info> named <verbatim|none> which
    inherits from <cpp|std_drd>, and a fresh <cpp|edit_env> with dummy
    reference tables;

    <item>evaluates <scm|(use-package p1 ... pn)> in that environment;

    <item>reads back the resulting environment with <cpp|read_env> and runs
    <cpp|drd_info_rep::heuristic_init> on it;

    <item>stores both in the <cpp|style_data_rep> tables
    <cpp|style_cached> and <cpp|drd_cached>.
  </enumerate>

  A table <cpp|style_busy> protects against styles which (directly or
  indirectly) require themselves. <cpp|edit_env_rep::exec_use_package> is
  the workhorse: it resolves each package in
  <verbatim|$TEXMACS_STYLE_PATH> (and in the directory of the document),
  parses it, extracts the <markup|body> and evaluates it with
  <cpp|exec> after applying <cpp|filter_style>, which strips the
  <markup|style-with>, <markup|style-only>, <markup|active> and
  <markup|inactive> tags used for the presentation of style sources. Since
  a style file is just a document whose body consists of <markup|assign>
  statements (and nested <markup|use-package>, <markup|use-module>,
  <markup|drd-props> ...), evaluating it fills the environment with the
  macros it defines. Note that it is evaluated, not typeset.

  Finally <cpp|use_modules> evaluates the <scheme> modules listed in the
  variable <cpp|THE_MODULES> (<verbatim|the-modules>), which is extended by
  every <markup|use-module> primitive
  (<cpp|edit_env_rep::exec_use_module>).

  The style caches are cleared by <cpp|style_invalidate_cache>, called by
  <cpp|tm_server_rep::style_clear_cache> (<scheme> function
  <scm|style-clear-cache>), which also removes the cache files and
  re-initializes the styles of all views. If you change a <verbatim|.ts>
  file during development and nothing happens, this is the cache at work.

  <subsection|Other ways of importing definitions>

  <\description>
    <item*|<markup|with-package>>Rewritten by
    <cpp|with_package_definitions> (<verbatim|Texmacs/Data/new_buffer.cpp>)
    into a <markup|with> which binds all variables assigned at the top
    level of the package (loaded with <cpp|load_style_tree>, cached in
    <cpp|style_tree_cache>). Nested packages and non-<markup|assign>
    statements are ignored, as the <verbatim|FIXME> there admits.

    <item*|<markup|include>>The included document is loaded by
    <cpp|load_inclusion> (cached in <cpp|document_inclusions>) and typeset in
    place, with <cpp|cur_file_name> and <cpp|secure> temporarily set
    according to the included file (<cpp|concater_rep::typeset_include>,
    <cpp|bridge_rewrite_rep::my_typeset>).
  </description>

  <section|The environment at the cursor>

  Many editing routines need the value of an environment variable at the
  cursor position or at some other path, for instance to know whether the
  cursor is in math mode. This is computed by
  <cpp|edit_typeset_rep::typeset_exec_until (path p)>, which caches the
  results per path in the table <cpp|cur> (up to 25 entries). The public
  accessors <cpp|get_env_value>, <cpp|get_env_string>,
  <cpp|defined_at_cursor> and <cpp|get_full_env> are built on it.

  There are two strategies:

  <\itemize>
    <item>The exact strategy calls <cpp|exec_until (ttt, p / rp)>
    (<verbatim|Typeset/Bridge/typesetter.cpp>), which delegates to the
    bridges; valid bridges replay their cached changes and invalid ones call
    the partial evaluator <cpp|edit_env_rep::exec_until> described in
    <hlink|the evaluator|macro-expansion-exec.en.tm>.

    <item>When <cpp|enable_fastenv> is set (from the preference
    <verbatim|fast environments> through the <scheme> function
    <scm|set-fast-environments>), a much cheaper approximation is used:
    descending along the path, it only executes top-level
    <markup|assign> statements and preambles (<cpp|restricted_exec>),
    handles table formats (<cpp|table_descend>) and, for any other tag,
    applies the environment which the DRD associates to the child through
    <cpp|drd_info_rep::get_env_child>. Macro bodies are not traversed; only
    the environment changes which the DRD heuristics could infer from the
    macro definitions are taken into account.
  </itemize>

  In source mode (outside the preamble), <cpp|define_style_macros> in
  addition writes all top-level macro definitions of the document into the
  environment. The variant <cpp|var_texmacs_exec> (the <scheme> function
  <scm|texmacs-exec*>) evaluates a tree in the environment at the cursor,
  whereas <cpp|texmacs_exec> (<scm|texmacs-exec>) uses the current state of
  the environment.

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
