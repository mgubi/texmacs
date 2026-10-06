<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Macro expansion during typesetting>

  <section|Inverse paths and decorations>

  Every box produced by the typesetter carries an <em|inverse path>
  <verbatim|ip>, the reversed path of its source subtree in the edit tree
  (see <hlink|the boxes produced by the typesetter|boxes.en.tm>). An
  inverse path whose head is negative is a <em|decoration>: the box does
  not correspond to editable content, and the rest of the path indicates
  where the cursor should go when the box is clicked. The relevant
  definitions are in <source-link|Typeset/boxes.hpp|src/Typeset/boxes.hpp>:

  <\cpp-code>
    #define DECORATION        (-1)

    #define DECORATION_LEFT   (-2)

    #define DECORATION_MIDDLE (-3)

    #define DECORATION_RIGHT  (-4)

    #define DETACHED          (-5)

    #define is_accessible(p) ((is_nil (p)) \|\| ((p)-\<gtr\>item \<gtr\>= 0))

    #define is_decoration(p) ((!is_nil (p)) && ((p)-\<gtr\>item \<less\> 0))

    inline path descend (path ip, int i) {

    \ \ return (is_nil (ip) \|\| (ip-\<gtr\>item \<gtr\>= 0))? path (i, ip): ip; }

    inline path decorate_right (path ip) {

    \ \ return (is_nil (ip) \|\| (ip-\<gtr\>item \<gtr\>= 0))? path (DECORATION_RIGHT, ip): ip; }
  </cpp-code>

  Descending into a decoration yields the same decoration, and decorating a
  decoration does not stack a second negative number. Hence all subtrees of
  a decorated tree share the decoration of its root.

  Trees know their own inverse path through an <cpp|ip_observer>
  (<source-link|Data/Observers/ip_observer.cpp|src/Data/Observers/ip_observer.cpp>). <cpp|obtain_ip (t)> returns
  it, or <verbatim|DETACHED> if <cpp|t> has no inverse path or if its path
  contains a negative number anywhere. Trees which are produced by the
  evaluator are detached. The function

  <\explain>
    <cpp|tree attach_dip (tree ref, path dip)><explain-synopsis|give a tree
    a (decoration) inverse path>
  <|explain>
    returns <cpp|ref> itself if it already has a valid inverse path, and
    otherwise a copy of <cpp|ref> whose nodes are recursively attached to
    <cpp|dip> and its descendants (<source-link|Typeset/Boxes/Basic/boxes.cpp|src/Typeset/Boxes/Basic/boxes.cpp>).
    The macros <cpp|attach_here (t, ip)> and <cpp|attach_right (t, ip)>
    expand to the two arguments <cpp|attach_dip (t, ip), ip>, respectively
    <cpp|attach_dip (t, decorate_right (ip)), decorate_right (ip)>, which
    are passed to the typesetting routines taking a tree and a path.
  </explain>

  The first rule of <cpp|attach_dip> is what makes the whole scheme work:
  subtrees of the document which end up inside a computed tree keep their
  own inverse path. Conversely, <cpp|concater_rep::typeset (tree t, path
  ip)> starts with

  <\cpp-code>
    if (!is_accessible (ip)) {

    \ \ path ip2= obtain_ip (t);

    \ \ if (ip2 != path (DETACHED))

    \ \ \ \ ip= ip2;

    }
  </cpp-code>

  so that a document subtree met while typesetting decorated material is
  typeset with its genuine location, and is therefore editable.

  <section|Typesetting a macro application>

  Inline macro applications are typeset by
  <cpp|concater_rep::typeset_compound (tree t, path ip)>
  (<source-link|Typeset/Concat/concat_macro.cpp|src/Typeset/Concat/concat_macro.cpp>). After looking up the macro
  <cpp|f> exactly as <cpp|exec_compound> does (an undefined tag is typeset
  with <cpp|typeset_error>), it proceeds as follows:

  <\cpp-code>
    if (is_applicable (f)) {

    \ \ int i, n=N(f)-1, m=N(t)-d;

    \ \ env-\<gtr\>macro_arg= list\<less\>hashmap\<less\>string,tree\<gtr\> \<gtr\> (

    \ \ \ \ hashmap\<less\>string,tree\<gtr\> (UNINIT), env-\<gtr\>macro_arg);

    \ \ env-\<gtr\>macro_src= list\<less\>hashmap\<less\>string,path\<gtr\> \<gtr\> (

    \ \ \ \ hashmap\<less\>string,path\<gtr\> (path (DECORATION)), env-\<gtr\>macro_src);

    \ \ if (L(f) == XMACRO) {

    \ \ \ \ if (is_atomic (f[0])) {

    \ \ \ \ \ \ string var= f[0]-\<gtr\>label;

    \ \ \ \ \ \ env-\<gtr\>macro_arg-\<gtr\>item (var)= t;

    \ \ \ \ \ \ env-\<gtr\>macro_src-\<gtr\>item (var)= ip;

    \ \ \ \ }

    \ \ }

    \ \ else for (i=0; i\<less\>n; i++)

    \ \ \ \ if (is_atomic (f[i])) {

    \ \ \ \ \ \ string var= f[i]-\<gtr\>label;

    \ \ \ \ \ \ env-\<gtr\>macro_arg-\<gtr\>item (var)=

    \ \ \ \ \ \ \ \ i\<less\>m? t[i+d]: attach_dip (tree (UNINIT), decorate_right(ip));

    \ \ \ \ \ \ env-\<gtr\>macro_src-\<gtr\>item (var)= i\<less\>m? descend (ip,i+d): decorate_right(ip);

    \ \ \ \ }

    \ \ if (is_decoration (ip))

    \ \ \ \ typeset (attach_here (f[n], ip));

    \ \ else {

    \ \ \ \ marker (descend (ip, 0));

    \ \ \ \ typeset (attach_right (f[n], ip));

    \ \ \ \ marker (descend (ip, 1));

    \ \ }

    \ \ env-\<gtr\>macro_arg= env-\<gtr\>macro_arg-\<gtr\>next;

    \ \ env-\<gtr\>macro_src= env-\<gtr\>macro_src-\<gtr\>next;

    }
  </cpp-code>

  So, in comparison with <cpp|exec_compound>:

  <\itemize>
    <item>Each parameter is bound to the argument subtree <em|and> to its
    inverse path <cpp|descend (ip, i+d)> in <cpp|macro_src>. Missing
    arguments are bound to a decorated <verbatim|UNINIT>.

    <item>The body is <em|not> evaluated. It is typeset as it is, after
    attaching the decoration <cpp|decorate_right (ip)> to it: text coming
    from the macro body cannot be edited, and clicking on it puts the cursor
    just after the macro application.

    <item>Two markers with the accessible paths <cpp|descend (ip, 0)> and
    <cpp|descend (ip, 1)> are inserted around the result; they provide the
    cursor positions just before and just after the tag.

    <item>When the application itself is part of decorated material (for
    instance it occurs inside the body of another macro), no markers are
    inserted and the body simply inherits the decoration.
  </itemize>

  Everything else happens when the typesetter meets the primitives of the
  body. Since these are typeset (not evaluated), the ordinary typesetting
  code applies: <markup|with> changes the environment around its body,
  <markup|if> evaluates its condition and typesets only the chosen branch
  (<cpp|concater_rep::typeset_if>, with the path <cpp|descend (ip, 1)> or
  <cpp|descend (ip, 2)>), nested macro applications are expanded
  recursively, and so on. The contents of the chosen branch of an
  <markup|if> remain editable if they come from an argument; the value of an
  <markup|eval> or of a computation does not.

  <section|Typesetting an argument>

  <cpp|concater_rep::typeset_argument> handles <scm|(arg "x" i1 ... ik)>:

  <\cpp-code>
    string name = r-\<gtr\>label;

    tree   value= env-\<gtr\>macro_arg-\<gtr\>item [name];

    path   valip= decorate_right (ip);

    if (!is_func (value, BACKUP)) {

    \ \ path new_valip= env-\<gtr\>macro_src-\<gtr\>item [name];

    \ \ if (is_accessible (new_valip)) valip= new_valip;

    }

    marker (descend (ip, 0));

    list\<less\>hashmap\<less\>string,tree\<gtr\> \<gtr\> old_var= env-\<gtr\>macro_arg;

    list\<less\>hashmap\<less\>string,path\<gtr\> \<gtr\> old_src= env-\<gtr\>macro_src;

    if (!is_nil (env-\<gtr\>macro_arg)) env-\<gtr\>macro_arg= env-\<gtr\>macro_arg-\<gtr\>next;

    if (!is_nil (env-\<gtr\>macro_src)) env-\<gtr\>macro_src= env-\<gtr\>macro_src-\<gtr\>next;

    ... // descend into value and valip along i1, ..., ik

    typeset (attach_here (value, valip));

    env-\<gtr\>macro_arg= old_var;

    env-\<gtr\>macro_src= old_src;

    marker (descend (ip, 1));
  </cpp-code>

  The argument is typeset with the inverse path recorded in
  <cpp|macro_src> (if it is accessible), in the frame of the caller. For an
  argument coming from the document this path is the true location of the
  argument, so the boxes of the argument are accessible, the cursor can
  enter them and modifications are routed to the right place. The markers
  around it have the path of the <markup|arg> tag in the macro body, which
  is a decoration. With an index path, <verbatim|value> and
  <verbatim|valip> are descended in parallel, so <scm|(arg "x" "1")>
  typesets the second child of the argument with the path of that child.

  If the argument is itself decorated material (for instance because the
  macro was called from inside the body of another macro, with an argument
  which was computed there), <cpp|macro_src> contains a decoration and
  <cpp|valip> falls back to <cpp|decorate_right (ip)>. A typical case is an
  argument which is passed on to another macro, as in
  <scm|(macro "x" (strong (arg "x")))>: inside <markup|strong>, the bound
  argument is the (decorated) tree <scm|(arg "x")> of the outer body.
  Typesetting it pops the inner frame and resolves the reference in the
  outer frame, where the true path of the document subtree is found. In
  this way arguments remain editable through any number of macro levels.

  <subsection|A worked example>

  Consider the document fragment <scm|(hello "world")> at inverse path
  <verbatim|ip>, with the definition
  <scm|(assign "hello" (macro "name" (concat "Hello " (arg "name") "!")))>.
  The typesetter

  <\enumerate>
    <item>pushes a frame binding <verbatim|name> to the string
    <verbatim|world> of the document and to the path
    <cpp|descend (ip, 0)>;

    <item>inserts a marker at <cpp|descend (ip, 0)> (cursor before the
    tag);

    <item>typesets the body <scm|(concat "Hello " (arg "name") "!")>, to
    which it attaches the path <cpp|decorate_right (ip)>; the strings
    <verbatim|Hello > and <verbatim|!> become decorated boxes;

    <item>when it reaches <scm|(arg "name")>, pops the frame and typesets
    <verbatim|world> with path <cpp|descend (ip, 0)>, that is, as the
    editable first child of <markup|hello>;

    <item>restores the frame, inserts a marker at <cpp|descend (ip, 1)>
    (cursor after the tag) and pops the frame.
  </enumerate>

  By contrast, if the macro were defined as
  <scm|(macro "name" (merge "Hello " (arg "name") "!"))>, the
  <markup|merge> primitive would be typeset by
  <cpp|typeset_executable>: the whole body, including the argument, would
  be evaluated by <cpp|exec_merge> into the fresh string
  <verbatim|Hello world!>, which would be typeset as a decoration. The
  visual result is the same, but the word <verbatim|world> can no longer
  be edited in place.

  <section|Other macro-related primitives>

  <\description>
    <item*|<markup|mark>><scm|(mark (arg "x") body)> typesets <verbatim|body>
    but surrounds it with markers carrying the paths of the <em|argument>
    <verbatim|x> (<cpp|concater_rep::typeset_mark>). It is used when a macro
    displays a transformed version of an argument: the cursor positions at
    the borders of the rendering are identified with the borders of the
    argument. The source code rendering of inactive markup uses it
    extensively.

    <item*|<markup|expand-as>><scm|(expand-as x body)> typesets
    <verbatim|body> (with path <cpp|descend (ip, 1)>), and evaluates to the
    value of <verbatim|body>, but for the purposes of <cpp|expand> it stands
    for <verbatim|x>. This tells routines which look for the accessible part
    of a macro (loci, <markup|hard-id>, <markup|find-accessible>) which
    argument the rendering represents.

    <item*|<markup|value>, <markup|or-value>>The value of the variable is
    typeset with <cpp|typeset_dynamic>, as a decoration. A macro stored in a
    variable and typeset through <markup|value> is thus displayed as
    source code, not expanded.

    <item*|Executable primitives>Arithmetic, string and comparison
    primitives, <markup|quasiquote>, <markup|while>, <markup|provides> and
    the like are evaluated with <cpp|exec> and the result is typeset by
    <cpp|typeset_dynamic>, which attaches <cpp|decorate_right (ip)> and
    inserts markers around it (<cpp|concater_rep::typeset_executable>).

    <item*|<markup|eval>, <markup|quasi>><cpp|typeset_eval> evaluates the
    argument once and typesets the result dynamically.

    <item*|Rewritable primitives><cpp|typeset_rewrite> typesets
    <cpp|env-\<gtr\>rewrite (t)> with <cpp|typeset_dynamic>. Parts of the
    result which are document subtrees keep their inverse paths thanks to
    <cpp|attach_dip>; this is why <markup|extern> with <markup|quote-arg>
    and <markup|map-args> preserve editability.

    <item*|<markup|drd-props>, <markup|assign>, <markup|provide>>These are
    executed for their side effects when they are typeset, and leave a
    control item or a flag (<cpp|typeset_drd_props>, <cpp|typeset_assign>).
  </description>

  <section|Macro expansion in bridges>

  At the paragraph level the document is typeset incrementally by
  <em|bridges> (see the chapter on the <hlink|typesetter|typesetter.en.tm>).
  <cpp|make_bridge> (<source-link|Typeset/Bridge/bridge.cpp|src/Typeset/Bridge/bridge.cpp>) chooses the
  bridge class according to the label:

  <\description>
    <item*|<cpp|bridge_compound>>User tags (labels from
    <verbatim|START_EXTENSIONS> on), <markup|compound>, <markup|include>,
    <markup|hlink>, <markup|action>, <markup|active>, <markup|style-only>
    and their variants.

    <item*|<cpp|bridge_argument>><markup|arg>.

    <item*|<cpp|bridge_with>><markup|with>.

    <item*|<cpp|bridge_rewrite>><markup|extern>, <markup|include*>,
    <markup|with-package>, <markup|rewrite-inactive>.

    <item*|<cpp|bridge_eval>><markup|eval>, <markup|quasi>,
    <markup|map-args> and the animation primitives <markup|anim-static>,
    <markup|anim-dynamic>.

    <item*|<cpp|bridge_expand_as>, <cpp|bridge_mark>><markup|expand-as>,
    <markup|mark>.

    <item*|<cpp|bridge_auto>>Trees which are rendered through a fixed
    macro: <markup|inactive>, <markup|inactive*>, error trees, and all
    trees when the environment is in preamble mode
    (<cpp|make_inactive_bridge>).
  </description>

  <cpp|bridge_compound_rep::my_typeset> pushes a frame exactly like
  <cpp|typeset_compound>, and then calls <cpp|initialize (f[n], d, f)>,
  which creates (or reuses) a <em|body bridge> for
  <cpp|attach_right (f[n], ip)>. A marker for the application is inserted
  unless the DRD declares the tag <em|child enforcing>
  (<cpp|the_drd-\<gtr\>is_child_enforcing>, which holds for tags with an
  <verbatim|inner> border, see <markup|drd-props>). <cpp|bridge_argument_rep::my_typeset>
  similarly creates a bridge for the argument, <cpp|attach_here (value,
  valip)>, and typesets it with the frame popped.

  <subsection|Propagating edits through macros>

  The payoff of this organization is that edits inside macro arguments are
  cheap, even when the macro is a large block environment. Suppose the user
  types inside the second argument of a theorem-like block macro. The
  modification arrives at the <cpp|bridge_compound> of the application as
  <cpp|notify_assign (p, u)> (or <cpp|notify_insert>,
  <cpp|notify_remove>) with <cpp|p-\<gtr\>item> equal to the child
  number. The bridge translates the child number into the name of the
  macro parameter and calls

  <\cpp-code>
    notify_macro (MACRO_ASSIGN, fun[p-\<gtr\>item-delta]-\<gtr\>label, -1, p-\<gtr\>next, u);
  </cpp-code>

  <cpp|bridge_compound_rep::notify_macro> pushes the argument frame (as in
  <cpp|my_typeset>) and forwards the notification to its body bridge with
  level <cpp|l+1>, that is 0. The body bridges forward it further:
  <cpp|bridge_with> simply passes it to its body; a nested
  <cpp|bridge_compound> pushes its own frame and increments the level; a
  <cpp|bridge_argument> at level 0 checks that the parameter name (and the
  index prefix of an <markup|arg> with indices) matches, and then applies
  the modification directly to the bridge of the argument; at a positive
  level it pops a frame and forwards with level <cpp|l-1>. The level
  therefore counts, as in <cpp|exec_until>, the number of macro frames
  between the current position and the frame which binds the parameter.
  Every bridge on the way which is affected marks itself as
  <verbatim|CORRUPTED>, so that exactly the paragraphs which depend on the
  argument are re-typeset.

  Bridges which cannot forward precisely (<cpp|bridge_rewrite>,
  <cpp|bridge_eval>, and invalid bridges) fall back on
  <cpp|edit_env_rep::depends> to decide whether they depend on the
  modified parameter at the given level, and re-typeset themselves
  completely if they do.

  <section|Inactive markup>

  Source code is displayed by rewriting rather than by special boxes. The
  <markup|inactive> tags, the preamble and the style file editor all end
  up in <cpp|edit_env_rep::rewrite_inactive (tree t, tree var)>, where
  <cpp|t> is the tree to display and <cpp|var> is an <markup|arg>
  reference to it. The result is ordinary markup using the style macros
  <markup|src-regular>, <markup|src-var>, <markup|src-arg>,
  <markup|src-unknown> and so on (see the static function
  <cpp|highlight> in <source-link|env_inactive.cpp|src/Typeset/Env/env_inactive.cpp>), whose children are
  again <markup|arg> references with index paths such as
  <scm|(arg "x" "1" "0")>. When this rendering is typeset, each reference is
  resolved by <cpp|typeset_argument>, which yields boxes with the correct
  source path, so that the source remains editable. The rendering depends
  on the environment variables <verbatim|src-style>,
  <verbatim|src-special>, <verbatim|src-compact> and <verbatim|src-close>
  (cached in <cpp|src_style>, <cpp|src_special>, ...) and on the DRD types
  of the children (<cpp|drd_info_rep::get_type_child>).

  How source is entered, edited and presented to the user is described in
  <hlink|source mode|source-mode.en.tm>.

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
