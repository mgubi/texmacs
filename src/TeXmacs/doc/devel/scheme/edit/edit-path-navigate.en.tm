<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Path-based navigation>

  As explained in the section on the <hlink|<TeXmacs> editing
  model|edit-model.en.tm>, cursor positions are represented by
  <em|paths>, <abbr|i.e.> lists of integers. The kernel provides a set of
  routines which compute new cursor paths from old ones, following the
  logical structure of the document. These routines are implemented in
  <c++> (in <source-link|src/Data/Tree/tree_cursor.cpp|src/Data/Tree/tree_cursor.cpp> and
  <source-link|src/Data/Tree/tree_traverse.cpp|src/Data/Tree/tree_traverse.cpp>) and glued to <scheme> (see
  <source-link|src/Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>). They do not move the
  cursor themselves; this is done by <scm|(go-to <scm-arg|p>)>, and the
  current cursor path is returned by <scm|(cursor-path)>.

  All routines below take a tree <scm-arg|t> (usually <scm|(root-tree)>)
  and a cursor path <scm-arg|p> inside <scm-arg|t>, and return a new cursor
  path inside <scm-arg|t>. When no suitable position exists, <scm-arg|p>
  itself is usually returned. The exceptions, which take the path of a
  subtree instead of a cursor path, are indicated below.

  <\explain>
    <scm|(path-start <scm-arg|t> <scm-arg|p>)>

    <scm|(path-end <scm-arg|t> <scm-arg|p>)><explain-synopsis|start and end
    of a subtree>
  <|explain>
    Here <scm-arg|p> is the path of a subtree of <scm-arg|t> (not a cursor
    path). Return the first <abbr|resp.> last valid cursor position inside
    this subtree. For instance, <scm|(path-start (root-tree)
    (buffer-path))> is the start of the current buffer.
  </explain>

  <\explain>
    <scm|(path-next <scm-arg|t> <scm-arg|p>)>

    <scm|(path-previous <scm-arg|t> <scm-arg|p>)><explain-synopsis|next and
    previous valid positions>
  <|explain>
    Return the next <abbr|resp.> previous valid cursor position, as with
    the arrow keys in text mode.
  </explain>

  <\explain>
    <scm|(path-next-word <scm-arg|t> <scm-arg|p>)>

    <scm|(path-previous-word <scm-arg|t> <scm-arg|p>)><explain-synopsis|word
    based motion>
  <|explain>
    Move to the end of the next word <abbr|resp.> the start of the previous
    word.
  </explain>

  <\explain>
    <scm|(path-next-node <scm-arg|t> <scm-arg|p>)>

    <scm|(path-previous-node <scm-arg|t> <scm-arg|p>)><explain-synopsis|node
    based motion>
  <|explain>
    Move to the next <abbr|resp.> previous position which is not inside the
    same leaf of the document tree.
  </explain>

  <\explain>
    <scm|(path-next-tag <scm-arg|t> <scm-arg|p> <scm-arg|labels>)>

    <scm|(path-previous-tag <scm-arg|t> <scm-arg|p>
    <scm-arg|labels>)><explain-synopsis|move to the next tag of a given
    kind>
  <|explain>
    Move to the next <abbr|resp.> previous occurrence of a tag whose label
    is <scm-arg|labels> (a symbol) or belongs to <scm-arg|labels> (a list of
    symbols). The variants <scm|path-next-tag-same-argument> and
    <scm|path-previous-tag-same-argument> try to preserve the argument of
    the tag in which the cursor is.
  </explain>

  <\explain>
    <scm|(path-next-argument <scm-arg|t> <scm-arg|p>)>

    <scm|(path-previous-argument <scm-arg|t> <scm-arg|p>)><explain-synopsis|move
    between arguments>
  <|explain>
    Here <scm-arg|p> is the path of an argument of a tag (a subtree path
    <scm|(<scm-arg|q> ... <scm-arg|i>)>, not a cursor path). Return the
    cursor path at the start of the next <abbr|resp.> the end of the
    previous accessible argument of this tag, or the empty path if there is
    no such argument. In particular, given a cursor path inside a string,
    the result is the empty path: use <scm|(cDr <scm-arg|p>)> to obtain the
    path of the argument which contains the cursor.
  </explain>

  <\explain>
    <scm|(path-previous-section <scm-arg|t> <scm-arg|p>)><explain-synopsis|move
    to the previous section>
  <|explain>
    Return the path of the previous sectional tag (like <markup|section>)
    before <scm-arg|p> (a subtree path, not a cursor position), or
    <scm-arg|p> itself if there is none.
  </explain>

  Paths can be compared with <scm|path-less?>, <scm|path-less-eq?>,
  <scm|path-inf?> and <scm|path-inf-eq?>, and <scm|(path-exists?
  <scm-arg|p>)> tests whether <scm-arg|p> is the path of an existing
  subtree of the root tree.

  <paragraph*|Moving the cursor>

  The module <source-link|progs/utils/library/cursor.scm|TeXmacs/progs/utils/library/cursor.scm> (to be
  included with <scm|(:use (utils library cursor))>) defines
  corresponding commands which move the cursor inside the current buffer:
  <scm|go-to-next>, <scm|go-to-previous>, <scm|go-to-next-word>,
  <scm|go-to-previous-word>, <scm|go-to-next-node>,
  <scm|go-to-previous-node>, <scm|(go-to-next-tag <scm-arg|labels>)>,
  <scm|(go-to-previous-tag <scm-arg|labels>)>, <abbr|etc.> They are all
  built on top of

  <\scm-code>
    (tm-define (go-to-same-buffer fun)

    \ \ (with p (fun (root-tree) (cursor-path))

    \ \ \ \ (when (list-starts? (cDr p) (buffer-path))

    \ \ \ \ \ \ (go-to p)

    \ \ \ \ \ \ (select-from-cursor))))
  </scm-code>

  which applies a path-based navigation routine and moves the cursor
  provided that the new position lies in the same buffer. The same module
  also provides more general commands, whose argument <scm-arg|fun> is a
  cursor command without arguments, such as <scm|go-to-next> (not a
  <verbatim|path-> routine), which is called as <scm|(<scm-arg|fun>)>:

  <\itemize>
    <item><scm|(go-to-next-such-that <scm-arg|fun> <scm-arg|pred?>)>
    repeatedly calls <scm-arg|fun> until the tree at the cursor satisfies
    <scm-arg|pred?>, or the cursor no longer moves (it then goes back to the
    initial position);

    <item><scm|(go-to-next-inside <scm-arg|fun> . <scm-arg|pattern>)>
    repeatedly calls <scm-arg|fun> until the cursor is inside a tree which
    matches <scm-arg|pattern>, a list of tag names, predicates and argument
    numbers;

    <item><scm|(go-to-remain-inside <scm-arg|fun> . <scm-arg|pattern>)>
    calls <scm-arg|fun> once, and goes back if the cursor left the
    innermost tree which matches <scm-arg|pattern>.
  </itemize>

  Finally, <scm|(go-to-next-tag-argument <scm-arg|labels> <scm-arg|i>)>
  moves the cursor to the start of the argument <scm-arg|i> of the next tag
  with one of the <scm-arg|labels>.

  For moving the cursor relative to a given tree, it is often more
  convenient to use <scm|tree-go-to>, as in <scm|(tree-go-to <scm-arg|t> 0
  :start)>, or to compute paths with <scm|(tree-\<gtr\>path <scm-arg|t>
  <scm-arg|accessors>)>.

  <paragraph*|Trees and the cursor>

  The module <source-link|utils/library/tree.scm|TeXmacs/progs/utils/library/tree.scm>
  provides the following routines, where the optional arguments
  <scm-arg|l> are accessors as for <scm|tree-ref>, possibly followed by
  <scm|:start>, <scm|:end> or an integer position, as for
  <scm|tree-\<gtr\>path>:

  <\explain>
    <scm|(tree-search-upwards <scm-arg|t> <scm-arg|what>)>

    <scm|(inside-which <scm-arg|labels>)><explain-synopsis|search
    upwards>
  <|explain>
    The first routine returns the first ancestor of <scm-arg|t>, starting
    with <scm-arg|t> itself, which matches <scm-arg|what> (a tag name, a
    list of tag names or a predicate), or <scm|#f>; the search stops at the
    buffer. The second one returns the label of the innermost tree around
    the cursor whose label is in the list <scm-arg|labels>, or <scm|#f>.
  </explain>

  <\explain>
    <scm|(tree-start <scm-arg|t> . <scm-arg|l>)>

    <scm|(tree-end <scm-arg|t> . <scm-arg|l>)><explain-synopsis|leaves at
    the extremities>
  <|explain>
    Return the tree which contains the first <abbr|resp.> the last cursor
    position of the subtree of <scm-arg|t> given by <scm-arg|l>.
  </explain>

  <\explain>
    <scm|(tree-cursor-at? <scm-arg|t> . <scm-arg|l>)>

    <scm|(tree-select <scm-arg|t> . <scm-arg|l>)><explain-synopsis|cursor
    and selection>
  <|explain>
    Test whether the cursor is at the position given by <scm-arg|l> inside
    <scm-arg|t> (for instance <scm|(tree-cursor-at? t 0 :end)>),
    <abbr|resp.> select the subtree of <scm-arg|t> given by <scm-arg|l>.
  </explain>

  <tmdoc-copyright|2005\U2026|Joris van der Hoeven and the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
