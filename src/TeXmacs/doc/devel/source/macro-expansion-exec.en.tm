<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The evaluator>

  <section|Evaluation of trees>

  <\explain>
    <cpp|tree edit_env_rep::exec (tree t)><explain-synopsis|evaluate a tree
    in the current environment>
  <|explain>
    Returns the value of <cpp|t>. Strings evaluate to themselves. For
    compound trees, <cpp|exec> dispatches on the label
    (<verbatim|Typeset/Env/env_exec.cpp>). Environment primitives,
    macro primitives, control structures, computational primitives, length
    units, graphical and animation primitives all have their own
    <cpp|exec_*> method. The result is a new tree without source location.
    Errors are not signalled by exceptions but returned as trees of the form
    <scm|(error "message")> (label <verbatim|_ERROR>), which the typesetter
    displays as source code in error style (<cpp|concater_rep::typeset_error>
    applies <scm|(rewrite-inactive (arg "x") "error")> to it).
  </explain>

  The end of the dispatch shows how the remaining tags are handled:

  <\cpp-code>
    default:

    \ \ if (L(t) \<less\> START_EXTENSIONS) {

    \ \ \ \ int i, n= N(t);

    \ \ \ \ tree r (t, n);

    \ \ \ \ for (i=0; i\<less\>n; i++) r[i]= exec (t[i]);

    \ \ \ \ return r;

    \ \ }

    \ \ else return exec_compound (t);
  </cpp-code>

  Built-in tags without special semantics for the evaluator (typesetting
  primitives like <markup|concat>, <markup|frac> or <markup|document>) are
  evaluated componentwise, while all <em|user> tags, whose labels are
  allocated dynamically above <verbatim|START_EXTENSIONS>, are macro
  applications. A few built-in tags are also treated as macro applications
  by name: <markup|hlink>, <markup|action>, <markup|active>,
  <markup|inactive>, <markup|style-only> and their variants are
  dispatched to <cpp|exec_compound>, because they are actually implemented
  as macros in the default environment (see <cpp|initialize_default_env>,
  which for instance defines <markup|inactive> as
  <scm|(macro "body" (rewrite-inactive (arg "body") "once"))> and
  <markup|include> as <scm|(macro "name" (include* (arg "name")))>).
  Some primitives only evaluate part of their arguments: <markup|macro> and
  <markup|xmacro> are returned as copies, <markup|quote> returns its
  argument unevaluated, and <markup|move>, <markup|shift>,
  <markup|resize> and <markup|clipped> only evaluate their first argument.

  <cpp|exec_string (t)> is a shortcut which returns the label of the
  result, or the empty string if the result is compound.

  <section|Macro application>

  A macro application <scm|(f a1 ... am)> (or
  <scm|(compound f' a1 ... am)>, where <verbatim|f'> is evaluated to obtain
  the name or the macro itself) is evaluated by <cpp|exec_compound>:

  <\cpp-code>
    tree

    edit_env_rep::exec_compound (tree t) {

    \ \ int d; tree f;

    \ \ if (L(t) == COMPOUND) {

    \ \ \ \ d= 1;

    \ \ \ \ f= t[0];

    \ \ \ \ if (is_compound (f)) f= exec (f);

    \ \ \ \ if (is_atomic (f)) {

    \ \ \ \ \ \ string var= f-\<gtr\>label;

    \ \ \ \ \ \ if (!provides (var)) return tree (_ERROR, "compound " * var);

    \ \ \ \ \ \ f= read (var);

    \ \ \ \ }

    \ \ }

    \ \ else {

    \ \ \ \ string var= as_string (L(t));

    \ \ \ \ if (!provides (var)) return tree (_ERROR, "compound " * var);

    \ \ \ \ d= 0;

    \ \ \ \ f= read (var);

    \ \ }

    \ \ if (is_applicable (f)) {

    \ \ \ \ int i, n=N(f)-1, m=N(t)-d;

    \ \ \ \ macro_arg= list\<less\>hashmap\<less\>string,tree\<gtr\> \<gtr\> (

    \ \ \ \ \ \ hashmap\<less\>string,tree\<gtr\> (UNINIT), macro_arg);

    \ \ \ \ macro_src= list\<less\>hashmap\<less\>string,path\<gtr\> \<gtr\> (

    \ \ \ \ \ \ hashmap\<less\>string,path\<gtr\> (path (DECORATION)), macro_src);

    \ \ \ \ if (L(f) == XMACRO) {

    \ \ \ \ \ \ if (is_atomic (f[0]))

    \ \ \ \ \ \ \ \ macro_arg-\<gtr\>item (f[0]-\<gtr\>label)= t;

    \ \ \ \ }

    \ \ \ \ else for (i=0; i\<less\>n; i++)

    \ \ \ \ \ \ if (is_atomic (f[i])) {

    \ \ \ \ \ \ \ \ tree st= i\<less\>m? t[i+d]: tree (UNINIT);

    \ \ \ \ \ \ \ \ macro_arg-\<gtr\>item (f[i]-\<gtr\>label)= st;

    \ \ \ \ \ \ \ \ macro_src-\<gtr\>item (f[i]-\<gtr\>label)= obtain_ip (st);

    \ \ \ \ \ \ }

    \ \ \ \ tree r= exec (f[n]);

    \ \ \ \ macro_arg= macro_arg-\<gtr\>next;

    \ \ \ \ macro_src= macro_src-\<gtr\>next;

    \ \ \ \ return r;

    \ \ }

    \ \ else return exec (f);

    }
  </cpp-code>

  The points to remember are:

  <\itemize>
    <item>The value of the tag name is looked up in the environment at the
    moment of the application (dynamic binding), and <cpp|is_applicable>
    accepts <markup|macro>, <markup|xmacro> and <markup|func> trees.

    <item>A new frame is pushed on <cpp|macro_arg> and <cpp|macro_src>. For
    a <markup|macro> with parameters <verbatim|x1>, ..., <verbatim|xn>, the
    parameter <verbatim|xi> is bound to the <em|unevaluated> argument tree
    (call by name). Missing arguments are bound to <verbatim|UNINIT>; extra
    arguments are ignored. For an <markup|xmacro> the single parameter is
    bound to the whole application <cpp|t>, so that
    <scm|(arg "x" "i")> designates the <verbatim|i>-th child.

    <item>The body is evaluated, and the frame is popped again.

    <item>If the value of the tag is not a macro, the value itself is
    evaluated. Thus a variable can be used as a tag without arguments:
    <markup|foo> evaluates to the value of the variable <verbatim|foo>.

    <item>If the tag is not defined at all, the result is an error tree
    <verbatim|compound foo>.
  </itemize>

  <section|Accessing arguments>

  <\explain>
    <cpp|tree exec_arg (tree t)><explain-synopsis|the <markup|arg>
    primitive>
  <|explain>
    For <scm|(arg "x" i1 ... ik)>, looks up <verbatim|x> in the
    <em|innermost> frame only (an error <verbatim|arg x> is returned if it is
    not bound there), descends into the children with the evaluated indices
    <verbatim|i1>, ..., <verbatim|ik>, and evaluates the resulting subtree.
    Crucially, the frame is <em|popped> while the argument is evaluated, and
    pushed back afterwards:

    <\cpp-code>
      r= macro_arg-\<gtr\>item [r-\<gtr\>label];

      list\<less\>hashmap\<less\>string,tree\<gtr\> \<gtr\> old_var= macro_arg;

      list\<less\>hashmap\<less\>string,path\<gtr\> \<gtr\> old_src= macro_src;

      if (!is_nil (macro_arg)) macro_arg= macro_arg-\<gtr\>next;

      if (!is_nil (macro_src)) macro_src= macro_src-\<gtr\>next;

      ...

      else r= exec (r);

      macro_arg= old_var;

      macro_src= old_src;
    </cpp-code>

    The argument tree comes from the caller, so any <markup|arg> it contains
    refers to the parameters of the caller. This is what makes nested
    macro calls such as <scm|(macro "x" (strong (arg "x")))> work: when
    <markup|strong> in turn evaluates its own <scm|(arg "body")>, the
    <markup|arg> of the outer macro is found one frame further down.
  </explain>

  <\explain>
    <cpp|tree exec_quote_arg (tree t)><explain-synopsis|the
    <markup|quote-arg> primitive>
  <|explain>
    Like <markup|arg>, but returns the argument tree <em|without>
    evaluating it. The returned tree is the argument tree as it was bound in
    the frame, not a copy. During typesetting this is the very subtree of the
    document, so it still carries its inverse path; this is important for
    <markup|extern> (see below). When the static flag
    <cpp|quote_substitute> is set (during <cpp|exec_until_quasi>), the result
    is instead a tree whose children are <markup|arg> references to the
    children of the argument.
  </explain>

  <\explain>
    <cpp|tree exec_value (tree t)>

    <cpp|tree exec_quote_value (tree t)>

    <cpp|tree exec_or_value (tree t)><explain-synopsis|reading variables>
  <|explain>
    <markup|value> evaluates its argument to a name and returns the
    <em|evaluated> value of that variable; <markup|quote-value> returns the
    value unevaluated; <markup|or-value> returns the value of the first of
    its arguments which is defined and not <verbatim|UNINIT>. Since stored
    values may themselves be expressions (for instance a length like
    <scm|(plus "1fn" (value "par-sep"))>), <markup|value> may trigger further
    evaluation.
  </explain>

  <markup|get-label> and <markup|get-arity> evaluate their argument and
  return its label, respectively its arity; <markup|provides> tests
  whether a variable is defined.

  <section|Environment primitives>

  <\explain>
    <cpp|tree exec_with (tree t)><explain-synopsis|local variables>
  <|explain>
    Evaluates all variable names and new values <em|before> changing
    anything, then writes the new values, evaluates the body and restores
    the old values. The result is <scm|(with var1 (quote val1) ... body')>,
    where <verbatim|body'> is the evaluated body: the evaluated values are
    wrapped in <markup|quote> so that the result can safely be evaluated
    again.
  </explain>

  <\explain>
    <cpp|tree exec_assign (tree t)><explain-synopsis|global assignment>
  <|explain>
    Evaluates the name, then calls <cpp|assign>, which evaluates the value
    and stores it. Note that a <markup|macro> evaluates to (a copy of)
    itself, which is why <scm|(assign "foo" (macro ...))> stores the macro
    unevaluated. The result is <scm|(assign var val)>, again with
    <markup|quote> around compound values other than macros.
    <markup|provide> does the same, except when the variable is already
    defined.
  </explain>

  The table formatting primitives (<markup|tformat>, <markup|dlines>,
  <markup|datoms>, <markup|dpages>) are also environment primitives: they
  append their formatting arguments to the corresponding variable
  (<cpp|CELL_FORMAT>, <cpp|LINE_DECORATIONS>, ...) while the last argument
  is evaluated (<cpp|exec_formatting>). See <hlink|environment
  primitives|../format/stylesheet/prim-env.en.tm> for the user-level
  description.

  <section|Evaluation control>

  The primitives <markup|eval>, <markup|quote>, <markup|quasiquote>,
  <markup|unquote>, <markup|unquote*>, <markup|quasi> and <markup|copy>
  (see <hlink|evaluation control
  primitives|../format/stylesheet/prim-evaluation.en.tm>) are implemented
  directly in <cpp|exec>:

  <\description>
    <item*|<markup|eval>><cpp|exec (exec (t[0]))>: evaluates the argument
    and then evaluates the result once more.

    <item*|<markup|quote>>Returns <cpp|t[0]> unevaluated.

    <item*|<markup|quasiquote>>Calls <cpp|exec_quasiquoted>, which copies
    the tree while replacing each <scm|(unquote u)> by the value of
    <verbatim|u> and splicing in the children of the value of each
    <scm|(unquote* u)>.

    <item*|<markup|quasi>>Evaluates the result of <markup|quasiquote>
    once more; this is the idiomatic way to build a tree with computed
    parts and then evaluate (or typeset) it.
  </description>

  The control structures <markup|if>, <markup|case>, <markup|while> and
  <markup|for-each> (<cpp|exec_if>, <cpp|exec_case>, ...) require their
  conditions to evaluate to the strings <verbatim|true> or
  <verbatim|false>; anything else yields an error tree. <markup|while>
  returns the <markup|concat> of the values of its body, whereas
  <markup|for-each> applies a macro to each element of a tuple only for its
  side effects and returns the empty string.

  <section|Rewriting>

  <\explain>
    <cpp|tree edit_env_rep::rewrite (tree t)><explain-synopsis|one rewriting
    step>
  <|explain>
    Transforms a tree with one of the labels <verbatim|EXTERN>,
    <verbatim|MAP_ARGS>, <verbatim|VAR_INCLUDE>, <verbatim|WITH_PACKAGE> or
    <verbatim|REWRITE_INACTIVE> into another tree, and returns other trees
    unchanged. <cpp|exec_rewrite (t)> is simply
    <cpp|exec (rewrite (t))>; the typesetter instead typesets the rewritten
    tree (<cpp|concater_rep::typeset_rewrite>,
    <cpp|bridge_rewrite_rep>). Rewriting is <em|not> memoized: each time a
    rewritable tree is evaluated or typeset, <cpp|rewrite> is called again.
  </explain>

  The cases are:

  <\description>
    <item*|<markup|map-args>><scm|(map-args f g x start end)> builds a tree
    with label <verbatim|g> whose children are
    <scm|(f (arg x i) i)> for <verbatim|i> between <verbatim|start> and
    <verbatim|end> (or just <scm|(arg x i)> if <verbatim|f> is
    <verbatim|identity>). Note that the result contains <markup|arg>
    <em|references> to the children of the argument, not copies of these
    children. When the result is typeset, these references are resolved by
    the typesetter, which therefore knows the source location of each child
    and keeps them editable.

    <item*|<markup|include*>>(label <verbatim|VAR_INCLUDE>, produced by the
    <markup|include> macro) loads the document with
    <cpp|load_inclusion>, relative to <cpp|base_file_name>.

    <item*|<markup|with-package>>See <hlink|the typesetting
    environment|macro-expansion-env.en.tm>.

    <item*|<markup|rewrite-inactive>>Produces the source code rendering of
    an argument (<cpp|rewrite_inactive> in
    <verbatim|Typeset/Env/env_inactive.cpp>). This is how
    <markup|inactive> tags, the preamble and style files are displayed.
    The rendering is built from <markup|arg> references with index paths
    (the function <cpp|subvar> appends an index to the current
    <markup|arg> reference) and <markup|mark> tags, so that the displayed
    source remains editable.

    <item*|<markup|extern>>Calls <scheme>; see the next section.
  </description>

  <section|The <markup|extern> primitive and <scheme> macros>

  <scm|(extern f a1 ... an)> applies the <scheme> function <verbatim|f> to
  the trees <verbatim|a1>, ..., <verbatim|an> and returns the result,
  converted back to a tree. The implementation is the <verbatim|EXTERN> case
  of <cpp|rewrite>:

  <\cpp-code>
    string fun= tm_decode(exec_string (t[0]));

    tree r (TUPLE, n);

    for (i=1; i\<less\>n; i++)

    \ \ r[i]= exec (t[i]);

    object expr= null_object ();

    for (i=n-1; i\<gtr\>0; i--)

    \ \ expr= cons (object (r[i]), expr);

    expr= cons (string_to_object (fun), expr);

    if (!secure && script_status \<less\> 2) {

    \ \ if (!as_bool (call ("secure?", expr)))

    \ \ \ \ return tree (_ERROR, "insecure script");

    }

    edit_env old_env= current_rewrite_env;

    current_rewrite_env= edit_env (this);

    object o= eval (expr);

    current_rewrite_env= old_env;

    return content_to_tree (o);
  </cpp-code>

  Several details matter in practice:

  <\itemize>
    <item>The first argument is evaluated and parsed as <scheme> code; it can
    be a function name such as <verbatim|ext-hello> or a whole
    <scm|lambda> expression.

    <item>The arguments are <em|evaluated> with <cpp|exec> before they are
    passed. With <scm|(arg "x")>, <scheme> receives a fresh, evaluated copy
    of the argument without source location; if it is inserted into the
    result, it will be typeset as a non-editable decoration. With
    <scm|(quote-arg "x")>, <scheme> receives the original subtree of the
    document, and if the function returns it (or one of its subtrees)
    unchanged inside its result, the typesetter recognizes its inverse path
    and the user can edit it. The demonstration package
    <verbatim|packages/example/extern-demo.ts> uses this idiom:

    <\verbatim-code>
      \<less\>assign\|hello\|\<less\>macro\|body\|\<less\>extern\|ext-hello\|\<less\>quote-arg\|body\<gtr\>\<gtr\>\<gtr\>\<gtr\>
    </verbatim-code>

    with, in <verbatim|utils/misc/extern-demo.scm>,

    <\scm-code>
      (tm-define (ext-hello t)

      \ \ (:secure #t)

      \ \ `(concat "Hello " ,t "!"))
    </scm-code>

    <item>Unless the document is trusted (<cpp|secure>, determined by
    <cpp|is_secure> from the file name) or the user accepted all scripts
    (<cpp|script_status> is 2), the expression is first checked by the
    <scheme> predicate <scm|secure?> (<verbatim|kernel/texmacs/tm-secure.scm>).
    A function passes this check if it has the property <scm|:secure>, which
    is set by the <scm|(:secure #t)> option of <scm|tm-define> or by
    <scm|define-secure-symbols>. Otherwise the result is the error
    <verbatim|insecure script>.

    <item>While the <scheme> function runs, the static variable
    <cpp|current_rewrite_env> points to the environment. The function
    <cpp|texmacs_exec> (the <scheme> primitive <scm|texmacs-exec>) uses this
    environment when it is set, so that <scheme> code called from
    <markup|extern> can evaluate markup in the environment of the macro
    application. The old value is saved and restored, which makes the
    mechanism re-entrant.

    <item>The result is converted with <cpp|content_to_tree>, and it is
    <em|evaluated> (by <cpp|exec_rewrite>) or <em|typeset>, so it may itself
    contain macro applications.

    <item>By default, the children of <markup|extern> are not accessible
    for the DRD. A style can declare accessibility for a particular
    function <verbatim|f> through the pseudo tag <verbatim|extern:f>, as in
    <verbatim|packages/customize/math/math-check.ts>:

    <\verbatim-code>
      \<less\>drd-props\|extern:math-check\|with-like\|true\|arity\|1\|accessible\|all\|regular\|all\<gtr\>
    </verbatim-code>

    (see <cpp|drd_info_rep::is_accessible_child>).
  </itemize>

  <scm|tm-define-macro> is unrelated to markup macros: it defines
  <scheme> macros in the same way as <scm|tm-define> defines functions.
  There is no <markup|ext> primitive; the prefix <verbatim|ext-> is only a
  naming convention for <scheme> functions meant to be called through
  <markup|extern>.

  <section|Partial evaluation: <cpp|exec_until>>

  The environment at a given position inside a tree is computed by
  <em|partial evaluation>, which performs all side effects of the part of
  the tree which precedes the position, and the environment changes of
  the constructs which enclose it.

  <\explain>
    <cpp|void exec_until (tree t, path p)><explain-synopsis|execute
    <cpp|t> up to the position <cpp|p>>
  <|explain>
    <cpp|p> is a tree path inside <cpp|t> in the usual sense: the last item
    is a position (0 or 1 for a compound tree, a character index for a
    string). If <cpp|p> is atomic and nonzero, the whole of <cpp|t> is
    executed. For a <markup|with>, the new values are written with
    <cpp|monitored_write_update> and the evaluation continues inside the
    body without restoring anything; for a <markup|tformat> or table the
    cell format is updated; for other built-in tags, all children before
    <cpp|p-\<gtr\>item> are executed for their side effects and the
    recursion continues in child <cpp|p-\<gtr\>item>. Macro applications
    are handled by <cpp|exec_until_compound (t, p)>.
  </explain>

  Positions inside the argument of a macro are the difficult case, since
  the argument is not part of the macro body: the body contains
  <scm|(arg "x")>, possibly deep inside other constructs or even inside
  other macro calls. <cpp|exec_until_compound (t, p)> determines the name
  <verbatim|var> of the parameter which corresponds to child
  <cpp|p-\<gtr\>item>, pushes a frame of arguments and calls the second
  form of <cpp|exec_until>:

  <\explain>
    <cpp|bool exec_until (tree t, path p, string var, int level)><explain-synopsis|search
    for an argument>
  <|explain>
    Traverses <cpp|t> in evaluation order, executing everything it meets,
    until it finds the place where the argument <cpp|var> is used; then it
    continues with <cpp|exec_until (arg, p)> inside the actual argument and
    returns <cpp|true>. If it is not found, <cpp|false> is returned and
    <markup|with> changes made on the way are undone.

    The parameter <cpp|level> counts how many macro frames lie between the
    current frame and the one in which <cpp|var> is bound.
    <cpp|exec_until_compound (t, p, var, level)> pushes a frame and recurses
    with <cpp|level+1>; <cpp|exec_until_arg (t, p, var, level)> pops a frame
    and, if <cpp|level> is zero and the parameter name is <cpp|var>, has
    found the target (taking into account index paths such as
    <scm|(arg "x" "1")>); otherwise it recurses into the argument with
    <cpp|level-1>. <cpp|exec_until_mark> handles <markup|mark>, which is used
    to indicate that a subtree represents a given argument for cursor
    positions on its border.
  </explain>

  Rewritable primitives are handled by applying <cpp|rewrite> and
  searching the result (<cpp|exec_until_rewrite>); <markup|quasi> is
  handled by <cpp|exec_until_quasi>, which sets <cpp|quote_substitute> so
  that quoted arguments are replaced by <markup|arg> references which can
  be searched. The bridges have their own <cpp|my_exec_until> methods,
  which reuse cached environment changes when possible (see
  <cpp|bridge_rep::exec_until> and <cpp|bridge_with_rep::my_exec_until>).

  <section|Expansion and dependency tests>

  <\explain>
    <cpp|tree expand (tree t, bool search_accessible= false)><explain-synopsis|substitute
    arguments>
  <|explain>
    Replaces <markup|arg> and <markup|quote-arg> references in <cpp|t> by
    the corresponding argument trees (recursively expanding <markup|arg>
    through the frames, but without evaluating anything else) and
    <markup|expand-as> by its first argument. With
    <cpp|search_accessible> set, it instead returns the first subtree which
    is accessible, that is, which has a valid inverse path in the document
    and is an accessible child according to the DRD. This is used by
    <markup|find-accessible> (<cpp|exec_find_accessible>), <markup|hard-id>
    (<cpp|exec_hard_id>), <cpp|build_locus> and
    <cpp|concater_rep::typeset_set_binding> to find the part of the
    document which is represented by a macro argument.
  </explain>

  <\explain>
    <cpp|bool depends (tree t, string s, int level)><explain-synopsis|does a
    tree depend on an argument?>
  <|explain>
    Tests whether <cpp|t> refers, directly (<cpp|level> zero) or through
    <cpp|level> intermediate macro frames, to the parameter <cpp|s> via
    <markup|arg>, <markup|quote-arg>, <markup|map-args> or
    <markup|eval-args>. The bridges use it to decide whether a modification
    of a macro argument invalidates them. As the <verbatim|FIXME> in the
    code says, dependencies created by rewriting (for instance through
    <markup|extern>) are not detected.
  </explain>

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
