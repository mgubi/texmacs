<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<TeXmacs> abbreviations>

  The <TeXmacs> <scheme> kernel defines a few abbreviations and control
  structures which are used throughout the <scheme> code of <TeXmacs>. Most
  of them are defined in <verbatim|kernel/boot/abbrevs.scm>,
  <verbatim|kernel/boot/srfi.scm> and <verbatim|kernel/boot/ahash-table.scm>
  (relative to <verbatim|src/TeXmacs/progs/>). Since they are loaded at
  boot time, they are available in all modules.

  <paragraph|Predicates>

  <\explain>
    <scm|(== <scm-arg|x> <scm-arg|y>)>

    <scm|(!= <scm-arg|x> <scm-arg|y>)><explain-synopsis|structural equality>
  <|explain>
    <scm|==> is an abbreviation for <scm|equal?> and <scm|!=> for its
    negation.
  </explain>

  <\explain>
    <scm|(nnull? <scm-arg|x>)>, <scm|(npair? <scm-arg|x>)>, <scm|(nlist?
    <scm-arg|x>)>, <scm|(nstring? <scm-arg|x>)>, <scm|(nsymbol?
    <scm-arg|x>)><explain-synopsis|negated predicates>
  <|explain>
    Negations of the corresponding standard predicates.
  </explain>

  <\explain>
    <scm|(list-1? <scm-arg|x>)>, <scm|(list-2? <scm-arg|x>)>,
    <scm|(list-3? <scm-arg|x>)>, <scm|(list-4? <scm-arg|x>)>,
    <scm|(list\<gtr\>0? <scm-arg|x>)>, <scm|(list\<gtr\>1?
    <scm-arg|x>)><explain-synopsis|lists of given lengths>
  <|explain>
    Test whether <scm-arg|x> is a list of length <math|1>, <math|2>,
    <math|3>, <math|4>, <math|\<gtr\>0> <abbr|resp.> <math|\<gtr\>1>. The
    negations are <scm|nlist-1?>, <abbr|etc.>
  </explain>

  <\explain>
    <scm|(in? <scm-arg|x> <scm-arg|l>)>

    <scm|(nin? <scm-arg|x> <scm-arg|l>)><explain-synopsis|membership>
  <|explain>
    Test whether <scm-arg|x> is (not) a member of the list <scm-arg|l>
    (using <scm|equal?>). The routine <scm|(cons-new <scm-arg|x>
    <scm-arg|l>)> adds <scm-arg|x> in front of <scm-arg|l> unless it is
    already a member.
  </explain>

  <paragraph|Control structures>

  <\explain>
    <scm|(when <scm-arg|cond> <scm-arg|body> ...)>

    <scm|(unless <scm-arg|cond> <scm-arg|body> ...)><explain-synopsis|conditional
    execution>
  <|explain>
    Execute <scm-arg|body> if <scm-arg|cond> holds <abbr|resp.> does not
    hold.
  </explain>

  <\explain>
    <scm|(with <scm-arg|var> <scm-arg|val> <scm-arg|body> ...)><explain-synopsis|local
    binding>
  <|explain>
    Abbreviation for <scm|(let ((<scm-arg|var> <scm-arg|val>))
    <scm-arg|body> ...)>. If <scm-arg|var> is a list of variables, then
    <scm-arg|val> should evaluate to a list whose elements are bound to
    these variables, as in

    <\scm-code>
      (with (x y) (list 1 2) (+ x y))
    </scm-code>
  </explain>

  <\explain>
    <scm|(and-with <scm-arg|var> <scm-arg|val> <scm-arg|body>
    ...)><explain-synopsis|local binding of a non-false value>
  <|explain>
    Bind <scm-arg|var> to <scm-arg|val> and evaluate <scm-arg|body> only if
    <scm-arg|val> is not <scm|#f>; otherwise return <scm|#f>.
  </explain>

  <\explain>
    <scm|(and-let* (<scm-arg|clause> ...) <scm-arg|body>
    ...)><explain-synopsis|sequential conditional bindings (SRFI-2)>
  <|explain>
    Each <scm-arg|clause> is either of the form <scm|(<scm-arg|var>
    <scm-arg|expr>)>, <scm|(<scm-arg|expr>)> or a variable. The clauses are
    evaluated in sequence and <scm|#f> is returned as soon as one of them
    evaluates to <scm|#f>; otherwise, <scm-arg|body> is evaluated with the
    bindings made by the clauses. For instance:

    <\scm-code>
      (and-let* ((t (tree-innermost 'frac))

      \ \ \ \ \ \ \ \ \ \ \ (num (tree-ref t 0))

      \ \ \ \ \ \ \ \ \ \ \ ((tree-atomic? num)))

      \ \ (tree-\<gtr\>string num))
    </scm-code>
  </explain>

  <\explain>
    <scm|(receive <scm-arg|vars> <scm-arg|expr> <scm-arg|body>
    ...)><explain-synopsis|multiple values (SRFI-8)>
  <|explain>
    Bind the multiple values returned by <scm-arg|expr> to <scm-arg|vars>
    and evaluate <scm-arg|body>. The kernel also provides the SRFI
    constructs <scm|case-lambda>, <scm|cut> and <scm|cute>.
  </explain>

  <\explain>
    <scm|(with-global <scm-arg|var> <scm-arg|val> <scm-arg|body>
    ...)><explain-synopsis|temporary assignment>
  <|explain>
    Temporarily set the (global) variable <scm-arg|var> to <scm-arg|val>
    while evaluating <scm-arg|body>, and restore the old value afterwards.
    The value of the last expression of <scm-arg|body> is returned.
  </explain>

  <\explain>
    <scm|(with-result <scm-arg|result> <scm-arg|body> ...)><explain-synopsis|return
    a value computed beforehand>
  <|explain>
    Evaluate <scm-arg|result>, then <scm-arg|body>, and return the value of
    <scm-arg|result>.
  </explain>

  <\explain>
    <scm|(for (<scm-arg|x> <scm-arg|l>) <scm-arg|body> ...)>

    <scm|(for (<scm-arg|i> <scm-arg|start> <scm-arg|end>) <scm-arg|body>
    ...)>

    <scm|(for (<scm-arg|i> <scm-arg|start> <scm-arg|end> <scm-arg|step>)
    <scm-arg|body> ...)><explain-synopsis|loops>
  <|explain>
    The first form evaluates <scm-arg|body> for each element <scm-arg|x> of
    the list <scm-arg|l>. The other forms evaluate <scm-arg|body> for
    <math|<scm-arg|i>=<scm-arg|start>,<scm-arg|start>+<scm-arg|step>,\<ldots\>>,
    as long as <math|<scm-arg|i>\<less\><scm-arg|end>> (or
    <math|<scm-arg|i>\<gtr\><scm-arg|end>> if <scm-arg|step> is negative);
    the default step is <math|1>. The related macros <scm|(repeat
    <scm-arg|n> <scm-arg|body> ...)> and <scm|(twice <scm-arg|body> ...)>
    evaluate <scm-arg|body> <scm-arg|n> times <abbr|resp.> twice.
  </explain>

  <\explain>
    <scm|(.. <scm-arg|start> <scm-arg|end>)>

    <scm|(... <scm-arg|start> <scm-arg|end>)><explain-synopsis|ranges>
  <|explain>
    Return the list of integers from <scm-arg|start> to <scm-arg|end>, with
    <scm-arg|end> excluded <abbr|resp.> included. An optional third argument
    specifies the step.
  </explain>

  <\explain>
    <scm|(toggle! <scm-arg|var>)><explain-synopsis|toggle a boolean
    variable>
  <|explain>
    Abbreviation for <scm|(set! <scm-arg|var> (not <scm-arg|var>))>.
  </explain>

  <paragraph|Hash tables>

  <TeXmacs> uses its own names for hash tables based on <scm|equal?>, so as
  to be independent of the underlying <scheme> implementation:

  <\explain>
    <scm|(make-ahash-table)>

    <scm|(ahash-ref <scm-arg|h> <scm-arg|key>)>

    <scm|(ahash-set! <scm-arg|h> <scm-arg|key> <scm-arg|val>)>

    <scm|(ahash-remove! <scm-arg|h> <scm-arg|key>)><explain-synopsis|basic
    operations>
  <|explain>
    Create a new table, look up a key (<scm|#f> is returned for missing
    keys), set <abbr|resp.> remove an entry. Further routines are
    <scm|ahash-size>, <scm|ahash-fold>, <scm|(ahash-ref* <scm-arg|h>
    <scm-arg|key> <scm-arg|default>)>, <scm|ahash-table-\<gtr\>list>,
    <scm|list-\<gtr\>ahash-table>, <scm|ahash-table-map>,
    <scm|ahash-table-invert>, <scm|ahash-table-append> and
    <scm|ahash-table-difference>.
  </explain>

  <\explain>
    <scm|(define-table <scm-arg|name> (<scm-arg|key> <scm-arg|val> ...)
    ...)><explain-synopsis|global tables>
  <|explain>
    Define a public hash table <scm-arg|name> (if not yet defined) and add
    the given entries to it: each <scm-arg|key> is associated to the list
    <scm|(<scm-arg|val> ...)>. The entries are quasi-quoted.
  </explain>

  <paragraph|Strings and lists>

  Among the many small utilities on strings and lists defined in
  <verbatim|kernel/library/base.scm> and <verbatim|kernel/library/list.scm>,
  let us mention <scm|string-starts?>, <scm|string-ends?>,
  <scm|string-contains?>, <scm|string-tail>, <scm|string-split-lines>,
  <scm|string-decompose>, <scm|list-filter>, <scm|list-find>,
  <scm|list-remove-duplicates>. The <c++> glue provides further string
  routines, such as <scm|string-search-forwards>,
  <scm|string-search-backwards> and <scm|string-replace>.

  <paragraph|Running external programs>

  <\explain>
    <scm|(eval-system <scm-arg|cmd>)>

    <scm|(var-eval-system <scm-arg|cmd>)><explain-synopsis|run a shell
    command>
  <|explain>
    Run the shell command <scm-arg|cmd> and return its standard output as a
    string. The variant <scm|var-eval-system> removes trailing newlines.
  </explain>

  <\explain>
    <scm|(evaluate-system <scm-arg|args> <scm-arg|fd-in> <scm-arg|in>
    <scm-arg|fd-out>)><explain-synopsis|run a program with redirections>
  <|explain>
    Run the program whose name and arguments are given by the list of
    strings <scm-arg|args>, without passing through a shell. The strings in
    the list <scm-arg|in> are written to the file descriptors in the list
    <scm-arg|fd-in>, and the output on the file descriptors in the list
    <scm-arg|fd-out> is collected. The result is a list whose first element
    is the exit code (as a string), followed by the collected outputs. For
    instance:

    <\scm-code>
      (evaluate-system (list "sort") '(0) (list "b\\na\\n") '(1 2))
    </scm-code>

    returns <scm|("0" "a\\nb\\n" "")>.
  </explain>

  <\explain>
    <scm|(async-eval-system <scm-arg|cmd> <scm-arg|call-back>)><explain-synopsis|asynchronous
    shell command>
  <|explain>
    Start the shell command <scm-arg|cmd> in the background and call
    <scm-arg|call-back> with its output when it has finished. Returns
    <scm|#t> on error.
  </explain>

  <tmdoc-copyright|2005--2026|Joris van der Hoeven, the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
