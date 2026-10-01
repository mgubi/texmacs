<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Persistent environments and memoization>

  This page describes <verbatim|src/src/Style/Environment/> and
  <verbatim|src/src/Style/Memorizer/>. The design summary in <hlink|the
  experimental evaluator|macro-expansion-style.en.tm> lists the classes; the
  emphasis here is on how they cooperate and on what is actually memoized.

  <section|Identity versus equality>

  Everything in the rewriter is keyed on <em|identity>.
  <verbatim|environment.hpp> defines

  <\cpp-code>
    inline int \ weak_hash (tree t) \ \ \ \ \ \ \ \ \ \ { return hash ((void*) t.rep); }

    inline bool weak_equal (tree t1, tree t2) { return t1.rep == t2.rep; }
  </cpp-code>

  (and the same for <cpp|environment> and <cpp|std_environment>). Two
  computations are considered the same if and only if they receive the very
  same tree object and the very same environment object. This makes lookups
  cheap and is sound as long as trees and environments are never modified
  in place, which is why the rewriter works on the <hlink|clean
  copy|rewriter-integration.en.tm> of the document and only ever builds new
  environments.

  <section|Environment classes>

  Keys are integers: variable names are converted with
  <cpp|make_tree_label>, so that the same table serves for built-in and
  user-defined names.

  <\description>
    <item*|<cpp|assoc_environment>>An array of <cpp|assoc_node> (key,
    value) of fixed size, filled with <cpp|raw_write (i, key, val)>. It is
    the representation of the bindings of one <markup|assign>, one
    <markup|with> or one macro application.

    <item*|<cpp|basic_environment>>An open hash table of <cpp|hash_node>
    (key, value, <cpp|start>, <cpp|next>) whose capacity is a power of two;
    it supports <cpp|multiple_insert>, <cpp|multiple_write>,
    <cpp|multiple_remove> and <cpp|resize>.

    <item*|<cpp|list_environment>>A linked list of basic environments,
    searched from the head. Each node counts its lookup <cpp|misses>; when
    they exceed the combined size of the node and its successor,
    <cpp|raw_read> calls <cpp|compress>, which merges the successor into a
    new hash table and unlinks it. This modifies the node in place, but
    without changing the set of bindings it represents, so identity-based
    memoization is not affected.

    <item*|<cpp|std_environment>>The environment of the evaluator
    (<verbatim|std_environment.cpp>). Fields: <cpp|pure> (true for the
    environment opened by a <markup|with>), <cpp|env> (the local bindings),
    <cpp|next> (the enclosing environment), <cpp|accel> (a list environment
    with all bindings visible here, used for lookups) and <cpp|args> (macro
    argument frames, only used by the classical macro expansion). Reads
    go through <cpp|accel>.
  </description>

  <section|Operations on environments>

  The evaluator never writes into an environment. It replaces the current
  environment (the global <cpp|std_env>) by the result of one of the
  following operations:

  <\description>
    <item*|<cpp|primitive (env, h)>>Builds a fresh top-level environment
    from a hash map of variables (each value is copied). Used once per
    environment change by the editor.

    <item*|<cpp|assign (env, local)>>Adds the bindings <cpp|local> in front
    of the <cpp|accel> list. If the current environment is pure (we are
    directly inside a <markup|with>), the result is a new impure
    environment on top of it; otherwise the local bindings are prepended to
    those of the current impure environment.

    <item*|<cpp|begin_with (env, local)>>Pushes a new <em|pure> environment
    whose bindings are <cpp|local>.

    <item*|<cpp|end_with (env)>>Leaves a <markup|with>. If no assignment
    happened inside it, the environment below the pure one is restored.
    Otherwise the assignments made inside the <markup|with> survive:
    <cpp|end_with_environment> flattens them, removes the variables bound by
    the <markup|with> itself, and re-applies the remaining patch to the
    enclosing environment with <cpp|assign_environment>.

    <item*|<cpp|macro_down>, <cpp|macro_redown>, <cpp|macro_up>>Push and pop
    macro argument frames. They are only compiled with
    <verbatim|CLASSICAL_MACRO_EXPANSION>, which is commented out in
    <verbatim|environment.hpp> in favour of
    <verbatim|ALTERNATIVE_MACRO_EXPANSION> (expansion by substitution).
  </description>

  Each operation is wrapped in a memorizer (<cpp|assign_memorizer_rep>,
  <cpp|begin_with_memorizer_rep>, <cpp|end_with_memorizer_rep>, ...) with
  the pattern

  <\cpp-code>
    void

    assign (environment& env, assoc_environment local) {

    \ \ memorizer mem= tm_new\<less\>assign_memorizer_rep\<gtr\> (env, local);

    \ \ if (!is_memorized (mem)) mem-\<gtr\>compute ();

    \ \ env= mem-\<gtr\>get_environment ();

    }
  </cpp-code>

  The memorizers of <cpp|assign> and <cpp|begin_with> are keyed on the
  pointers of <em|both> arguments. As explained in the <hlink|pitfalls of
  this chapter|rewriter.en.tm>, the evaluator creates a new
  <cpp|assoc_environment> for every evaluation of an <markup|assign> or a
  <markup|with>, so these memorizers never find an earlier result; only
  <cpp|end_with>, keyed on the environment alone, can.

  <section|Memorizers>

  <verbatim|Style/Memorizer/memorizer.hpp> defines the abstract
  <cpp|memorizer_rep> with the virtual methods <cpp|type>, <cpp|hash>,
  <cpp|equal>, <cpp|print>, <cpp|compute>, the accessors
  <cpp|get_tree>/<cpp|set_tree> and
  <cpp|get_environment>/<cpp|set_environment>, and
  <cpp|get_children>/<cpp|set_children> (implemented by
  <cpp|compound_memorizer_rep>, which stores an array of child
  memorizers). The types are numbered: <verbatim|MEMORIZE_EVALUATE> (0),
  <verbatim|MEMORIZE_REWRITE> (1), <verbatim|MEMORIZE_INACTIVE> (2) for tree
  computations, and <verbatim|MEMORIZE_ASSIGN> (10) to
  <verbatim|MEMORIZE_MACRO_UP> (15) for environment operations.

  The tree memorizers are compound: for instance
  <cpp|evaluate_memorizer_rep> (<verbatim|Style/Evaluate/evaluate_main.cpp>)
  stores the input environment and tree, the output environment and tree,
  and the memorizers of all sub-evaluations, so that the result of an
  evaluation is a <em|tree of memorizers> mirroring the computation.

  <subsection|Hash-consing>

  The handle class <cpp|memorizer> is not an ordinary reference counted
  pointer. Its constructor from a <cpp|memorizer_rep*>
  (<verbatim|memorizer.cpp:288>) first looks the new object up in a global
  hash table (<cpp|bigmem_insert>: buckets indexed by
  <cpp|hash () & mask>, compared with <cpp|type> and <cpp|equal>, the
  number of buckets doubling as the table grows). If an equal memorizer
  already exists, the new object is deleted and the existing one is used.
  The destructor and the assignment operator remove a memorizer from the
  table (<cpp|bigmem_remove>) when its reference count drops to zero.

  <subsection|The stack of sub-computations>

  The constructor also records the memorizer on a stack, which is how the
  children of a computation are collected without passing them around:

  <\description>
    <item*|<cpp|memorize_initialize ()>>Allocates the stack (and prints
    <verbatim|"Memorize initialize">).

    <item*|<cpp|memorize_start ()>>Opens a new level whose first free slot
    is the current position.

    <item*|construction>Writes the memorizer into the next slot of the
    current level, taking one reference for the handle and one for the
    slot (unless the slot already held the same memorizer). If the slot
    held another memorizer, that one loses its slot reference.

    <item*|<cpp|memorize_end ()>>Closes the level and gives the memorizers
    created in it to the memorizer just below the level, as its children
    (<cpp|set_children>).

    <item*|<cpp|memorize_finalize ()>>Returns the memorizer in slot 0, the
    top-level computation, and frees the stack.
  </description>

  <cpp|evaluate (environment env, tree t)> brackets the whole computation
  with <cpp|memorize_initialize> and <cpp|memorize_finalize>; each
  recursive <cpp|evaluate (tree)> creates its memorizer in the current
  level and then brackets its own sub-computations with
  <cpp|memorize_start> and <cpp|memorize_end>.

  <subsection|Detecting earlier results>

  Whether a memorizer is \Palready computed\Q is decided by its reference
  count:

  <\cpp-code>
    inline friend bool is_memorized (const memorizer& mem) {

    \ \ return mem.rep-\<gtr\>ref_count \<gtr\>= 3; }
  </cpp-code>

  A memorizer just created holds two references (stack slot and handle).
  If the hash-consing found an existing object, that object is also
  referenced from somewhere else, typically from the children array of the
  previous evaluation, which the editor keeps alive in its field
  <cpp|mem>; its count is then at least three. The test is therefore a
  heuristic for \Pthis computation is part of a result still in use\Q, and
  it relies on the previous result being kept until the new one has been
  built. When it succeeds, <cpp|evaluate> returns the stored tree and
  restores the stored output environment without descending into the
  subtree.

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
