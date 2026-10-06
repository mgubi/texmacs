<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Kernel pitfalls>

  The items marked <em|(checked)> were reproduced on 2026-10-06 with a small
  program compiled against the kernel headers with <verbatim|-DNO_FAST_ALLOC>
  and <name|AddressSanitizer>.

  <\itemize>
    <item><em|References into containers do not survive growth> (checked).
    <cpp|H(x)> returns a reference into a list cell of the hash map; any
    later insertion or removal which resizes the table rebuilds the cells
    and frees the old ones. Writing through the old reference is a
    use-after-free, and with the fast allocator the value silently lands in
    a recycled block:

    <\cpp-code>
      hashmap\<less\>int,int\<gtr\> H (0);

      int& r= H(1);

      for (int i=2; i\<less\>100; i++) H(i)= i; \ \ // the table grows

      r= 42; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // use after free

      cout \<less\>\<less\> H[1]; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // still 0
    </cpp-code>

    The same holds for <cpp|a[i]> and <cpp|t[i]> after the array or tree
    grows (<cpp|\<less\>\<less\>>), and for references obtained through
    <cpp|subtree> after the tree is modified. Keep handles, not references,
    across such operations. A single assignment <cpp|H(x)= y> is safe, even
    <cpp|H(x)= H(z)> when both insert (checked): <TeXmacs> is compiled as
    C++17, where the right-hand side of an assignment is evaluated before
    the left-hand side.

    <item><em|Reading with <cpp|H(x)> inserts> (checked). Use <cpp|H[x]>
    or <cpp|H-\<gtr\>contains (x)> to read; <cpp|H(x)> creates the entry
    with the default value, which grows the table and makes
    <cpp|contains> true afterwards.

    <item><em|Modifying a table while iterating> (checked). An iterator
    walks the buckets of the live table. Inserting during the iteration can
    make it see keys twice or miss some: inserting 40 keys while iterating
    over 8 gave 53 visits for 48 keys. Collect the keys first (for instance
    in an <cpp|array>) and then modify.

    <item><em|<cpp|clear> keeps the count of entries> (issue #306 of
    <verbatim|mgubi/texmacs>).
    <cpp|hashmap_rep::clear> (<source-link|hashmap.cpp:127|src/Kernel/Containers/hashmap.cpp:127>)
    empties the buckets but does not reset <cpp|size>, so after
    <cpp|H-\<gtr\>clear ()>, <cpp|N (H)> still gives the old number of
    entries, <cpp|empty> is false, and the table grows earlier than
    needed. Assign a new table (<cpp|H= hashmap\<less\>T,U\<gtr\> (init)>)
    instead.

        <item><em|Shared representations.> <cpp|a= b> shares; an in-place change
    through one handle (<cpp|\<less\>\<less\>>, <cpp|b[i]= ...>,
    <cpp|H(x)= ...>) is visible through all.
    This is the most common source of \Pimpossible\Q changes, for instance
    a hash map stored in two structures, or a tree of the document modified
    through a copy which was not made with <cpp|copy>.

    <item><em|Lists are recursive.> <cpp|N>, <cpp|last_item>, appending and
    <cpp|copy> recurse over the whole list; they are slow and use stack in
    proportion to the length. Build long sequences in an <cpp|array>, or by
    prepending and then <cpp|reverse>.

    <item><em|Unstable iteration order.> The order of <cpp|iterate> depends
    on the hash values and on the size of the table, and for pointer keys on
    the addresses, which vary from run to run. Output which must be
    reproducible (files, tests) has to sort the keys.

    <item><em|Extension labels are not stable.> The number of an extension
    tree label depends on the order in which names were first seen; never
    store it, store the name (<cpp|as_string (L (t))>).

    <item><em|Memory is not given back.> Small blocks return to their free
    list and the chunks to nobody, so the process does not shrink after
    closing a large document. Memory checkers need <verbatim|NO_FAST_ALLOC>;
    with the fast allocator, use-after-free errors corrupt other objects
    of the same size instead of crashing.

    <item><em|Cycles leak.> Two handles which point to each other keep each
    other alive forever; use a plain pointer for the back link and make
    sure it does not outlive its target.

    <item><em|Not thread-safe.> Reference counts and free lists are not
    atomic. Code running in another thread (a <name|Qt> worker, a callback
    of a network library) must not touch kernel objects; it has to hand its
    results to the main thread as plain C++ data.
  </itemize>

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
