<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Strings, arrays, lists, trees and hash tables>

  <section|Strings and arrays>

  A <cpp|string> (<source-link|string.hpp|src/Kernel/Types/string.hpp>)
  is a handle to a <cpp|string_rep> with a length <cpp|n> and a buffer
  <cpp|a> of bytes, which may contain zeros and is not terminated; use
  <cpp|c_string> to obtain a C string. An <cpp|array\<less\>T\<gtr\>>
  (<source-link|array.hpp|src/Kernel/Containers/array.hpp>) is the same
  with elements of type <cpp|T>.

  The buffers grow by doubling: <cpp|round_length> gives the allocated size
  for a length <var|n>, which is <var|n> itself for short arrays (fewer than
  6 elements) and short strings (rounded to 4 bytes below 24), and the next
  power of two above (from 8 elements, <abbr|resp.> 32 bytes). Appending
  with <cpp|\<less\>\<less\>> is therefore cheap on average, while
  <cpp|s1 * s2>, <cpp|append> and <cpp|s (i, j)> always build a new
  object. The buffer is reallocated when it grows past its allocated size,
  and shrinks when the length falls far enough below it (<cpp|resize>).

  The hash function of strings (<source-link|string.cpp:203|src/Kernel/Types/string.cpp:203>)
  rotates and adds the bytes; hashing a string costs its length.

  <section|Lists>

  A <cpp|list\<less\>T\<gtr\>> (<source-link|list.hpp|src/Kernel/Containers/list.hpp>)
  is a nil handle or a cell with <cpp|item> and <cpp|next>. Only the
  operations at the front are cheap. <cpp|N (l)>, <cpp|last_item>,
  <cpp|l * x> (append at the end), <cpp|l1 * l2> and <cpp|copy> walk the
  whole list <em|recursively>, and the appending functions copy every cell
  (<source-link|list.cpp:140|src/Kernel/Containers/list.cpp:140>). Lists
  are fine for short sequences such as paths and the chains of hash tables;
  a loop which appends with <cpp|l= l * x> is quadratic, and a very long
  list can exhaust the stack in these functions. <cpp|path> is
  <cpp|list\<less\>int\<gtr\>>.

  <section|Trees and tree labels>

  A <cpp|tree> (<source-link|tree.hpp|src/Kernel/Types/tree.hpp>) points
  to a <cpp|tree_rep>, which holds the label <cpp|op> and the observer
  <cpp|obs> of the tree, and is one of three classes:

  <\itemize>
    <item><cpp|atomic_rep>, for strings: the label is <cpp|TMSTRING> (0) and
    the string is in <cpp|label> (accessed as <cpp|t-\<gtr\>label>);

    <item><cpp|compound_rep>, for compound trees: a positive label and an
    <cpp|array\<less\>tree\<gtr\>> of children (<cpp|A (t)>, <cpp|t[i]>);

    <item><cpp|generic_rep>, with a negative label: a tree which wraps an
    arbitrary C++ value as a <cpp|blackbox> (<source-link|generic_tree.hpp|src/Kernel/Types/generic_tree.hpp>),
    used to pass C++ objects through <scheme> and tree-based interfaces.
  </itemize>

  The default constructor <cpp|tree ()> creates the empty string, not a
  nil tree. Equality <cpp|t1 == t2> compares the trees recursively;
  <cpp|strong_equal> compares the representations. <cpp|hash (t)> and
  <cpp|copy (t)> also traverse the whole tree.

  Labels are integers of the enumeration <cpp|tree_label>
  (<source-link|tree_label.hpp|src/Kernel/Types/tree_label.hpp>): the
  primitives of the document format have fixed values, and any other name
  becomes an <em|extension> label the first time it is used, through
  <cpp|make_tree_label> or <cpp|compound (name, ...)>. The two tables
  <cpp|CONSTRUCTOR_NAME> and <cpp|CONSTRUCTOR_CODE> translate in both
  directions; they only grow, and the numbers of the extensions depend on
  the order in which names are met, so they must not be stored. Unknown
  names give <cpp|UNKNOWN> with <cpp|as_tree_label>.

  The observer field is what makes the editing machinery work: every tree of
  a document can carry observers, which are told about modifications (see
  <hlink|the general architecture|architecture.en.tm> and <hlink|undo|undo.en.tm>).
  Since the observers belong to the representation, a tree shared between
  two places of a document shares its observers too.

  <section|Hash tables>

  A <cpp|hashmap\<less\>T,U\<gtr\>> (<source-link|hashmap.hpp|src/Kernel/Containers/hashmap.hpp>,
  <source-link|hashmap.cpp|src/Kernel/Containers/hashmap.cpp>) has an array
  of <cpp|n> buckets, a power of two, each a list of entries holding the
  hash value, the key and the value; a key goes in bucket
  <cpp|hash (key) & (n-1)>. The table also stores a default value
  <cpp|init>, given to the constructor.

  <\itemize>
    <item><cpp|H[x]> (<cpp|bracket_ro>) returns the value, or <cpp|init> if
    there is none, and never modifies the table.

    <item><cpp|H(x)> (<cpp|bracket_rw>) returns a <em|reference> to the
    value, creating the entry with <cpp|init> if there is none. It is meant
    for assignments, <cpp|H(x)= y>, but also inserts when only used for
    reading.

    <item>When the number of entries reaches <cpp|n> times <cpp|max> (1 by
    default), the table doubles (<cpp|resize>, <source-link|hashmap.cpp:49|src/Kernel/Containers/hashmap.cpp:49>),
    rebuilding every bucket with new list cells. <cpp|reset (x)> removes an
    entry and halves the table when it becomes less than half full;
    <cpp|clear> empties the buckets but keeps their number.
  </itemize>

  <cpp|hashset\<less\>T\<gtr\>> is the same without values.
  <cpp|iterate (H)> (<source-link|iterator.cpp|src/Kernel/Containers/iterator.cpp>)
  returns an iterator which keeps the table and walks its buckets lazily, so
  the order of the keys is that of the buckets, which depends on the hash
  values (for pointers, on addresses) and changes when the table grows.
  Code which needs a stable order sorts the keys.

  The hash functions are defined next to the types: integers hash to
  themselves, pointers to their address, strings and trees as above. For
  a type used as a key, <cpp|hash> and <cpp|==> must agree.

  Relatives of <cpp|hashmap>:

  <\description-paragraphs>
    <item*|<cpp|rel_hashmap>>A chain of hash maps for nested scopes;
    <cpp|extend> pushes a level and <cpp|shorten> pops it, and a lookup goes
    down the chain (<source-link|rel_hashmap.hpp|src/Kernel/Containers/rel_hashmap.hpp>).

    <item*|<cpp|hashfunc>>A function with a cache of its results
    (<source-link|hashfunc.hpp|src/Kernel/Containers/hashfunc.hpp>).

    <item*|<cpp|hashtree>>A trie: each node has a value and a hash map of
    children (<source-link|hashtree.hpp|src/Kernel/Containers/hashtree.hpp>).

    <item*|<cpp|hashmap\<less\>string,tree\<gtr\>> extras>The typesetting
    environment is a <cpp|hashmap\<less\>string,tree\<gtr\>>, for which
    <source-link|hashmap_extra.cpp|src/Kernel/Containers/hashmap_extra.cpp>
    adds patches: <cpp|changes>, <cpp|invert>, <cpp|pre_patch>,
    <cpp|post_patch> and <cpp|write_back> compute and apply the
    differences between two environments.
  </description-paragraphs>

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
