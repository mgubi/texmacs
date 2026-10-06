<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Selection of subexpressions>

  Besides pattern matching on trees, <TeXmacs> provides the routine
  <scm|select> for pattern matching along paths. Given a tree, this mechanism
  typically allows the user to select all subtrees which are reached
  following a path which meets specific criteria. For instance, one might to
  select the second child of the last child or all square roots inside
  numerators of fractions. The syntax of the selection patterns is also used
  for high level tree accessors. The implementation can be found in
  <source-link|kernel/regexp/regexp-select.scm|TeXmacs/progs/kernel/regexp/regexp-select.scm>.

  <\explain>
    <scm|(select <scm-arg|expr> <scm-arg|pattern>)><explain-synopsis|select
    subexpressions following a pattern>
  <|explain>
    Select all subtrees inside a hybrid tree <scm-arg|expr> according to a
    specific path <scm-arg|pattern>. The result is a list of subtrees.
  </explain>

  <\explain>
    <scm|(tm-ref <scm-arg|expr> <scm-arg|pattern-1> ...
    <scm-arg|pattern-n>)><explain-synopsis|first selected subexpression>
  <|explain>
    Return the first subexpression selected by the path pattern
    <scm|(<scm-arg|pattern-1> ... <scm-arg|pattern-n>)>, or <scm|#f> if no
    subexpression matches. For instance, <scm|(tm-ref t 1 0)> returns the
    first child of the second child of <scm|t>.
  </explain>

  Patterns are lists of atomic patterns of one of the following forms:

  <\explain>
    <scm|0>, <scm|1>, <scm|2>, ...<explain-synopsis|select a specific child>
  <|explain>
    Given an integer <scm|n>, select the <scm|n>-th child of the input tree.
    For instance, <scm|(select '(frac "1" "2") '(0))> returns <scm|("1")>.
  </explain>

  <\explain>
    <scm|:first>, <scm|:last><explain-synopsis|select first or last child>
  <|explain>
    Select first or last child of the input tree.
  </explain>

  <\explain>
    <scm|(:range <scm-arg|start> <scm-arg|end>)><explain-synopsis|select
    children in a range>
  <|explain>
    Select all children with indices <math|i> such that
    <math|<scm-arg|start>\<leqslant\>i\<less\><scm-arg|end>>.
  </explain>

  <\explain>
    <scm-arg|label><explain-synopsis|select children with a given label>
  <|explain>
    Select all compound subtrees with the specified <scm-arg|label>. Example:

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (select '(document (strong "x") (math "a+b") (strong "y")) '(strong))
      <|unfolded-io>
        ((strong "x") (strong "y"))
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(:exclude <scm-arg|label-1> ... <scm-arg|label-n>)><explain-synopsis|select
    children with other labels>
  <|explain>
    Select all compound children whose label is none of <scm-arg|label-1>
    until <scm-arg|label-n>.
  </explain>

  <\explain>
    <scm|:%1>, <scm|:%2>, <scm|:%3>, ...<explain-synopsis|select descendants
    of a given generation>
  <|explain>
    The pattern <scm|:%n>, where <scm|n> is a number, selects all descendants
    of the <scm|n>-th generation. Example:

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (select '(foo (bar "x" "y") (slash (dot))) '(:%2))
      <|unfolded-io>
        ("x" "y" (dot))
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|:*><explain-synopsis|select all descendants>
  <|explain>
    This pattern selects all descendants of the tree. For instance,
    <scm|(select t '(:* frac 0 :* sqrt))> selects all square roots inside
    numerators of fractions inside <scm|t>.
  </explain>

  <\explain>
    <scm|(:match <scm-arg|pattern>)><explain-synopsis|matching>
  <|explain>
    This pattern matches the input tree if and only the input tree matches
    the specified <scm-arg|pattern> according to <scm|match?>. Example:

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (select '(foo "x" (bar)) '(:%1 (:match :string?)))
      <|unfolded-io>
        ("x")
      </unfolded-io>
    </session>

    Any <scheme> predicate can be used in this way (see the description of
    <scm|:<scm-arg|pred?>> in the section on <hlink|matching regular
    expressions|utils-match.en.tm>). Notice that the predicate is applied to
    the subexpression as it is: for instance, <scm|:atomic-tree?> does not
    hold for the <scheme> string <scm|"x">, so that <scm|(select '(foo "x"
    (bar)) '(:* (:match :atomic-tree?)))> returns <scm|()>. A predicate
    which only accepts trees, such as the glue routine
    <scm|tree-atomic?>, raises an error here, since <scm|:*> also applies it
    to the lists.
  </explain>

  <\explain>
    <scm|(:replace <scm-arg|expr>)><explain-synopsis|substitution>
  <|explain>
    Select <scm-arg|expr>, in which the variables bound by the previous
    patterns are replaced by their values. For instance,

    <\scm-code>
      (select '(foo "x" (bar "y")) '(:* (:match (bar 'a)) (:replace (baz 'a))))
    </scm-code>

    returns <scm|((baz "y"))>.
  </explain>

  <\explain>
    <scm|'<scm-arg|var>><explain-synopsis|variables>
  <|explain>
    Select the input tree itself, while binding it to the variable
    <scm-arg|var>. As in the case of <scm|match?>, the same variable may
    occur several times in a pattern, in which case the corresponding
    subexpressions must coincide.
  </explain>

  <\explain>
    <scm|(:or <scm-arg|pattern-1> ... <scm-arg|pattern-n>)>

    <scm|(:and <scm-arg|pattern-1> ... <scm-arg|pattern-n>)><explain-synopsis|boolean
    expressions>
  <|explain>
    These rules allow for the selection of all subtrees which satisfy one
    among or all patterns <scm-arg|pattern-1> until <scm-arg|pattern-n>.
  </explain>

  <\explain>
    <scm|(:and-not <scm-arg|pattern> <scm-arg|pattern-1> ...
    <scm-arg|pattern-n>)><explain-synopsis|difference>
  <|explain>
    Select all subtrees which are selected by <scm-arg|pattern>, but by none
    of the patterns <scm-arg|pattern-1> until <scm-arg|pattern-n>.
  </explain>

  <\explain>
    <scm|(:group <scm-arg|pattern-1> ... <scm-arg|pattern-n>)><explain-synopsis|grouping>
  <|explain>
    Group the path pattern <scm|(<scm-arg|pattern-1> ...
    <scm-arg|pattern-n>)> into a single atomic pattern, which is useful
    inside <scm|:or> and <scm|:and>.
  </explain>

  In the case when the input tree is active, the function <scm|select>
  supports some additional patterns which allow the user to navigate inside
  the tree.

  <\explain>
    <scm|:up><explain-synopsis|parent>
  <|explain>
    This pattern selects the parent of the input tree, if it exists.
  </explain>

  <\explain>
    <scm|:down><explain-synopsis|child containing the cursor>
  <|explain>
    If the cursor is inside some child of the input tree, then this pattern
    will select this child.
  </explain>

  <\explain>
    <scm|:next><explain-synopsis|next child>
  <|explain>
    If the input tree is the <math|i>-th child of its parent, then this
    pattern will select the <math|<around|(|i+1|)>>-th child.
  </explain>

  <\explain>
    <scm|:previous><explain-synopsis|previous child>
  <|explain>
    If the input tree is the <math|i>-th child of its parent, then this
    pattern will select the <math|<around|(|i-1|)>>-th child.
  </explain>

  The patterns <scm|:first> and <scm|:last> also work for active trees, in
  which case they select the first <abbr|resp.> last child of the input
  tree, and the pattern <scm|:same> selects the input tree itself.

  <tmdoc-copyright|2007|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>