<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Basic data types>

  In this chapter, we give a rough description of <TeXmacs>'s basic data
  types, most of which can be found in the directory <verbatim|Kernel> of
  the <c++> sources (<source-link|src/src/Kernel|src/Kernel>). The description of the
  exported functions is non exhaustive and we refer to the corresponding
  header files for more precision. How these types are implemented, what
  their operations cost and which pitfalls follow are explained in
  <hlink|inside the kernel|kernel.en.tm>.

  <section|Memory allocation and data structures in <TeXmacs>>

  The file <source-link|System/Misc/fast_alloc.hpp|src/System/Misc/fast_alloc.hpp> declares the <TeXmacs>
  memory allocation routines. Objects should be created with
  <cpp|tm_new\<less\>T\<gtr\> (args)> and destroyed with <cpp|tm_delete>;
  arrays are handled by <cpp|tm_new_array> and <cpp|tm_delete_array>. These
  routines are very fast for small sizes (below <cpp|MAX_FAST> bytes), since
  for each such size, <TeXmacs> maintains a linked list of freed objects of
  that size. Larger blocks are allocated with the standard <cpp|malloc>.
  There is no garbage collection: memory is managed through reference
  counting.

  Modulo a few exceptions, all <TeXmacs> composite data structures are
  constructed using the macros <cpp|CONCRETE>, <cpp|ABSTRACT>,
  <cpp|CONCRETE_NULL> and <cpp|ABSTRACT_NULL> (as well as their
  <cpp|_TEMPLATE> variants), which are defined in
  <source-link|Kernel/Abstractions/basic.hpp|src/Kernel/Abstractions/basic.hpp>. Consequently, these data
  structures are pointers to representation classes, which derive from
  <cpp|concrete_struct> or <cpp|abstract_struct>, which may be abstract in
  the case of <cpp|ABSTRACT> and <cpp|ABSTRACT_NULL>, and which always
  contain a reference counter. Because of the reference counter, the <c++>
  copy operator is very fast. Most of the implemented data structures also
  export a function <cpp|copy>, which should be used if one really wants to
  physically duplicate an object. Conversely, one should keep in mind that
  an assignment like <cpp|a= b> makes <cpp|a> and <cpp|b> share the same
  representation, so that a subsequent in-place modification of <cpp|b>
  also affects <cpp|a>.

  For classes constructed using <cpp|CONCRETE_NULL> or <cpp|ABSTRACT_NULL>,
  the pointer to the representation class is allowed to be <cpp|NULL> and we
  have a default constructor which initializes this pointer with
  <cpp|NULL>. Instances of these classes are tested to be <cpp|NULL> using
  the function <cpp|is_nil>. Examples of such classes are lists, commands
  and widgets.

  <section|Array-like structures>

  <TeXmacs> implements three \Parray-like\Q structures:

  <\itemize>
    <item><cpp|string> (<source-link|Kernel/Types/string.hpp|src/Kernel/Types/string.hpp>) is the string
    type, which may contain <verbatim|'\\0'> characters. Strings are
    sequences of bytes; the interpretation of these bytes is described in
    the chapter on the <hlink|general architecture|architecture.en.tm>.

    <item><cpp|tree> (<source-link|Kernel/Types/tree.hpp|src/Kernel/Types/tree.hpp>) is the tree type.
    A tree is either <em|atomic> (a leaf labeled by a string) or
    <em|compound> (a node labeled by a <cpp|tree_label> with an array of
    children).

    <item><cpp|array\<less\>T\<gtr\>> (<source-link|Kernel/Containers/array.hpp|src/Kernel/Containers/array.hpp>)
    is the generic array type with elements of type <cpp|T>.
  </itemize>

  Array-like structures export the following operations:

  <\itemize>
    <item><cpp|N> computes the length of an array.

    <item><cpp|[]> accesses an element.

    <item><cpp|\<less\>\<less\>> is used for appending elements or arrays.

    <item>For strings, <cpp|*> concatenates two strings and
    <cpp|s (start, end)> extracts a substring. For arrays, the analogous
    operations are <cpp|append> and <cpp|range>; for trees, <cpp|t (start,
    end)> yields a tree with the same label and a subrange of the children.
  </itemize>

  For an atomic tree <cpp|t>, <cpp|t-\<gtr\>label> yields the string label
  of the tree. For a compound tree, <cpp|L(t)> yields its label,
  <cpp|A(t)> the array of its children and <cpp|t[i]> its <cpp|i>-th child.
  The predicates <cpp|is_atomic> and <cpp|is_compound> distinguish between
  both kinds of trees and <cpp|is_func (t, lab, n)> tests whether <cpp|t> is
  a compound tree with label <cpp|lab> and arity <cpp|n>. The second
  argument of <cpp|\<less\>\<less\>> for trees is either a tree or an array
  of trees. Besides its label or its children, every tree also carries an
  <em|observer> (the field <cpp|obs>), which is used for keeping track of
  modifications (see <source-link|Kernel/Abstractions/observer.hpp|src/Kernel/Abstractions/observer.hpp> and the
  chapter on the <hlink|general architecture|architecture.en.tm>).

  The implementation has been made such that the <cpp|\<less\>\<less\>>
  operation is fast, which is useful when considering arrays as buffers.
  Actually, the allocated space for arrays with more than five elements
  (<abbr|resp.> strings with more than 23 characters) is always a power of two, so
  that new elements can be appended quickly (see <cpp|round_length> in
  <source-link|array.cpp|src/Kernel/Containers/array.cpp> and <source-link|string.cpp|src/Kernel/Types/string.cpp>).

  <section|Lists and paths>

  Generic lists are implemented by the class <cpp|list\<less\>T\<gtr\>> in
  <source-link|Kernel/Containers/list.hpp|src/Kernel/Containers/list.hpp>. The \Pnil\Q list is created using
  <cpp|list\<less\>T\<gtr\>()>, an atom using
  <cpp|list\<less\>T\<gtr\>(T x)> and a general list using
  <cpp|list\<less\>T\<gtr\>(T x, list\<less\>T\<gtr\> next)>. If <cpp|l> is
  a list, <cpp|l-\<gtr\>item> and <cpp|l-\<gtr\>next> correspond to its
  label and its successor respectively (<scm|car> and <scm|cdr> in
  <scheme>). The functions <cpp|is_nil> and <cpp|is_atom> test whether a
  list is nil or an atom. The function <cpp|N> computes the length of a
  list, <cpp|l * x> appends an element at the end, and <cpp|reverse>,
  <cpp|last_item>, <cpp|head> and <cpp|tail> have the usual meanings.

  The type <cpp|list\<less\>int\<gtr\>> is also denoted by <cpp|path>
  (<source-link|Kernel/Types/path.hpp|src/Kernel/Types/path.hpp>), because some additional functions are
  defined for it. Indeed, paths are used for accessing descendants in tree
  like structures. For instance, we implemented the functions
  <cpp|tree& subtree (tree& t, path p)>, <cpp|path_up>, <cpp|path_less>
  and <cpp|p / q>, which removes the prefix <cpp|q> from <cpp|p>. The
  <em|inverse paths> which are stored in typeset boxes are also of type
  <cpp|path>; see the chapter on <hlink|boxes|boxes.en.tm>.

  <section|Hash tables>

  The <cpp|hashmap\<less\>T,U\<gtr\>> class
  (<source-link|Kernel/Containers/hashmap.hpp|src/Kernel/Containers/hashmap.hpp>) implements hash tables with
  entries in <cpp|T> and values in <cpp|U>. A function <cpp|hash> should be
  implemented for <cpp|T> (see <source-link|Kernel/Containers/hashfunc.hpp|src/Kernel/Containers/hashfunc.hpp>).
  The constructor <cpp|hashmap\<less\>T,U\<gtr\> (U init)> specifies the
  default value <cpp|init> which is returned for keys without an entry.
  Given a hash table <cpp|H>, we set elements through

  <\cpp-code>
    H(x)= y;
  </cpp-code>

  and access elements through

  <\cpp-code>
    H[x]
  </cpp-code>

  The methods <cpp|H-\<gtr\>contains (x)> and <cpp|H-\<gtr\>reset (x)> test
  whether <cpp|x> has an entry, <abbr|resp.> remove the entry for
  <cpp|x>. Similarly, <cpp|hashset\<less\>T\<gtr\>> implements sets of
  elements of type <cpp|T>.

  We also implemented a variant <cpp|rel_hashmap\<less\>T,U\<gtr\>> of hash
  tables (<source-link|Kernel/Containers/rel_hashmap.hpp|src/Kernel/Containers/rel_hashmap.hpp>), which also have a
  list-like structure. The methods <cpp|extend> and <cpp|shorten> push and
  pop a new level of definitions, which makes them useful for implementing
  recursive environments. They are used for instance by the <LaTeX> importer
  in order to handle local macro definitions.

  <section|Other data structures>

  <\itemize>
    <item><cpp|iterator\<less\>T\<gtr\>>
    (<source-link|Kernel/Containers/iterator.hpp|src/Kernel/Containers/iterator.hpp>) implements generic
    iterators. An iterator <cpp|it> over the keys of a hash table <cpp|H> is
    obtained using <cpp|iterate (H)>; one then loops using
    <cpp|while (it-\<gtr\>busy ()) { T x= it-\<gtr\>next (); ... }>.

    <item><cpp|pair\<less\>T1,T2\<gtr\>> and similar tuples
    (<source-link|Kernel/Containers/ntuple.hpp|src/Kernel/Containers/ntuple.hpp>), hash trees
    (<source-link|hashtree.hpp|src/Kernel/Containers/hashtree.hpp>) and promises (<source-link|promise.hpp|src/Kernel/Containers/promise.hpp>).

    <item><cpp|command> (<source-link|Kernel/Abstractions/command.hpp|src/Kernel/Abstractions/command.hpp>)
    implements abstract commands, with a virtual method <cpp|apply>.

    <item><cpp|blackbox> (<source-link|Kernel/Abstractions/blackbox.hpp|src/Kernel/Abstractions/blackbox.hpp>)
    allows to store values of an arbitrary type in a type-safe way.

    <item><cpp|observer> and <cpp|modification>
    (<source-link|Kernel/Abstractions/observer.hpp|src/Kernel/Abstractions/observer.hpp>,
    <source-link|Kernel/Types/modification.hpp|src/Kernel/Types/modification.hpp>) implement the observers
    attached to trees and the elementary modifications of trees.

    <item><cpp|rectangle> and <cpp|rectangles>
    (<source-link|Kernel/Types/rectangles.hpp|src/Kernel/Types/rectangles.hpp>) implement rectangles and lists
    of rectangles.

    <item><cpp|space> (<source-link|Kernel/Types/space.hpp|src/Kernel/Types/space.hpp>) implements
    stretchable spaces with a minimal, a default and a maximal size.

    <item><cpp|url> (<source-link|System/Classes/url.hpp|src/System/Classes/url.hpp>) implements file
    names, which may be local or remote, and search paths.

    <item>Files are read and written using functions like
    <cpp|load_string> and <cpp|save_string> in
    <source-link|System/Files/file.hpp|src/System/Files/file.hpp>, and timers using functions like
    <cpp|texmacs_time> and <cpp|bench_start> in
    <source-link|System/Classes/tm_timer.hpp|src/System/Classes/tm_timer.hpp>.
  </itemize>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
