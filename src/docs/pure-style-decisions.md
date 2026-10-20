# The style rewriter: architectural decisions

The decisions taken on the branch `wip_pure_style` for the style rewriter
(`src/src/Style`, `--enable-experimental`) and for its use by the editor
(`TEXMACS_STYLE_TYPESET=1`), each with the alternatives which were
considered and why they were set aside. The design, the state and the
measurements are in [pure-style.md](pure-style.md), Joris's goals in
[pure-style-goals.md](pure-style-goals.md). Each decision names what
decided it: a measurement, a comparison with the usual typesetting, or a
choice made by hand that could be revisited.

## 1. What the rewriter gives the typesetter

The rewriter evaluates the style and gives the typesetter a tree of
typesetting primitives. Two kinds of values cannot be computed before the
layout, and the rule for both is the same: leave them to the typesetter.

**1.1 Residuals (partial evaluation).** What only the typesetter knows is a
*residual*, and a computation on a residual is a residual too (the
computation with its arguments evaluated): the size of boxes (`box-info`,
`frame-direct`, `frame-inverse`), the lengths in units, the automatic
labels and the label tables (`auto-id`, `get-binding`, `has-binding`,
`get-attachment`), patterns, `hard-id`; and the margins (`par-left`,
`par-right`) in the body of an ornament, a canvas or an art box, which the
typesetter sets itself there (`evaluate_ornamented`).
*Alternatives:* compute an approximation (lengths in absolute units, as the
rewriter of 2007 did): wrong, the typesetter converts units at the scale at
which it lays out (twice the size headless), with the font, the paragraph,
the box at the place of use; evaluate `box-info` by typesetting in the
rewriter: duplicates the typesetter.
*Decided by:* the comparison of the printed pages (`tests/style/compare.sh`),
which showed each of these as a difference (the margins in ornaments on the
two generated help pages, `tag-help` and `top-help`).

**1.2 References, not copies.** A value which holds a residual is referred to
(`value (x)` stays, with the `with` or `assign` which binds `x`), so that the
typesetter computes it where it is bound.
*Alternative:* substitute the residual expression at each use (the obvious
partial evaluation): computed elsewhere, in another environment
(`box-info` typesets its argument again at another size).
*Decided by:* the graphics of the conventions page.

**1.3 Assignments stay; their values are protected.** Every `assign` stays in
the result with its place, also those which define macros, and the values of
`assign` and `with` are quoted (`quote`, or `quasiquote` with the residuals
unquoted) because the typesetter evaluates them again.
*Alternatives:* drop the assignments (the rewriter consumed them): the
typesetter reads variables of its own (the pager's `page-odd-header`), and
the line of a macro definition lost its place for the cursor; keep only
non-macro assignments: same loss of place.
*Decided by:* the running headers (page number `0` instead of `1`, a quoted
`page-the-page` expanded at the assignment) and the click maps.

**1.4 Markup and values are not the same evaluation.** The concater and `exec`
differ on a few tags: `if*` is a phantom for the concater (kept), `quote` is
shown by the concater; the expansions of macro calls are wrapped (1.6) in
markup positions only, not in values.
The two are evaluated apart, with the mode in the key of the memorizers
(`evaluate_markup`, `evaluate`): the document is markup; the parts of a
typesetting primitive have the mode of the primitive (`exec` executes the
parts of a primitive as values); the data, the conditions, the values bound
by `with` and `assign` are values; the tags whose result is that of another
tree (a macro call, `value`, `extern`, `eval`, the branch of an `if`) give
it in their own mode (`evaluate_result`). In markup, `quote` and
`quote-value` stay.
*Alternatives:* one evaluation for both (the rewriter of 2007): `phantom`
lost its body, `plus ((two), 1)` would see a wrapper, and a `quote` returned
by an `extern` in markup was evaluated (`std-utils-dtd`); the mode for the
`quote` tags only, without it in the keys: the same tree would get the
result of the other mode from the memorizers.
*Decided by:* the comparison of the printed pages (`std-utils-dtd`
identical, no other page changed); the click maps of 30 pages, the same
with and without the mode; a first version which evaluated the bodies of
tables as values lost the boundaries of the calls in them (680 points of
5000 with another place on the conventions page).

**1.5 What the typesetter does better is left to it.** Inactive markup
(`inactive`, `inactive*` stay unevaluated; `rewrite-inactive` inside other
macros goes through a macro made on the spot), `set-binding` (the
typesetter makes the binding and the locus).
*Alternative:* keep the rewriter's own rendering of inactive markup (2007):
it had drifted from the typesetter's (DRD types, `src-*` classes, the DRD of
the document).
*Decided by:* the comparison of the printed pages.

**1.6 The boundaries of macro calls.** The expansion of a macro call of the
document is kept in an `expand-as` with the place of the call
(`evaluate_markup`).
*Alternative:* nothing (the call disappears): the cursor could not be put
before or after a tag (typing at the start of a list item went after the
bullet).
*Decided by:* the comparison of cursor moves.

## 2. How the editor typesets the rewritten tree

**2.1 Behind a flag, in the experimental build.** `TEXMACS_STYLE_TYPESET=1`;
without it nothing changes (the typesetter's suite is the check).

**2.2 The places are those of the document.** The boxes carry places of the
document, found from the places which the rewriter attaches to its trees
(`rewritten_place`): a tree of the document has its place; below it, a tree
which does not come from there has a decoration; within the expansion of a
call, the place is derived from the call, as when the typesetter expands the
call itself (`attach_right` on the body of the `expand-as`). The same rule in
the three paths which typeset (the concater, the bridges, the lazy
typesetting of paragraphs), for the typesetters made while typesetting too,
and the "dirty hack" `bridge_rep::my_resync` is skipped.
*Alternatives:* derive the places from the rewritten tree (paths of a tree
which is not the document: the cursor followed paths which do not exist);
always take the attached places (the memorized expansions are shared between
calls and carry the place of only one).
*Decided by:* the click maps (`tests/style/compare-places.sh`), cursor moves.

**2.3 The environment at the cursor is computed on the document**
(`edit_env_rep::exec_until`), as the bridges do before they are typeset.
*Alternatives:* a second typesetter on the document, kept for `exec_until`
(more memory, the notifications kept twice); the "fast environments"
(an approximation, a preference of the user).
*Decided by:* `tests/style/compare-environments.sh`.

**2.4 Incremental: the changes of the rewritten tree.** The old and the new
rewritten trees are compared (`typeset_rewritten_changes`), by address since
the unchanged subtrees are the same trees, atoms by their strings; the
typesetter is told about the smallest subtrees which changed. In a
`document` whose number of paragraphs changed, the paragraphs before and
after the change are kept and the others inserted or removed;
`bridge_document` then leaves the places of the following paragraphs alone,
since they are the document's own `path` cells, which the document moves in
place. Another node whose number of children changed is replaced. It goes down only through the
trees whose bridge gives the change to the bridge of a child (document,
`with`, `expand-as`, `surround`, `mark`, `locus`, the formattings, at that
child); below, the new tree is given whole when its content or its places
changed, since the other bridges substitute the change in their tree
(`substitute`), which rebuilds the trees above it without the places which
the rewriter attached (2.2), and typeset that tree whole anyway.
*Alternatives:* the whole tree at each change (half a second per keystroke
on 77 pages); a node whose number of children changed replaced whole, also
a document (done first: a new paragraph typeset the whole document again,
6.1 ms instead of 0.9 on the structure page); places of the rewritten trees
copied instead of shared (the bridges could then move them as usual, but
every place would have to be updated by the rewriter after each change).
*Alternatives for the places below:* a `substitute` which keeps the places
(in the kernel, used everywhere, or in the 48 places of the bridges which
call it).
*Decided by:* `tests/style/bench-typing.sh`, `tests/style/compare-edits.sh`
(which joins paragraphs too), the click maps after new and joined paragraphs;
the places below by the labels of the welcome page and the click maps
after keystrokes.

**2.5 The loci are on the trees of the document.** Hyperlinks, labels and
observers are registered on the document's trees at the places of the
bodies, never on the rewriter's trees, which are replaced without the
modifications being told.
*Decided by:* a double free when a paragraph with a hyperlink was typeset
again.

**2.6 A failure falls back to the document.** When typesetting the rewritten
tree fails, the editor typesets the document instead; it never resets the
document to an empty one (what it does otherwise on a typesetting failure).

## 3. The identities which the caches rely on

Both caches rely on identities: the rewriter's memorizers on the addresses
of trees and environments, the typesetter's (#333) on the tags of the
document for the automatic labels and on the fingerprint of the environment.
The analysis of a keystroke (pure-style.md) found that each extra
millisecond with the flag came from an identity lost; with 3.1 and 3.2 a
keystroke with the flag costs what it costs without it.

**3.1 The automatic labels find their tag from the `expand-as` of the call.**
The typesetter numbers the automatic labels stably with the tag of the
document which makes them, found through the macro calls which it expands
(`auto_label_tag`, `macro_src`). With the rewritten tree it expands none,
gave new numbers at each pass (labels not stable, `auto-last` changing, every
bridge after the edit checked again). Decision: when the typesetter typesets
the `expand-as` of a call of the document, it opens a level of `macro_src`
with the place of the call, so that `auto_label_tag` finds the tag as when
the call is expanded.
The level holds the call only: `set-binding` and `flag` with a third
argument, the name of a macro argument (substituted in the rewritten tree),
keep their own place there, as before.
*Alternatives:* number the labels in the rewriter (they depend on the order
of the document and on data kept with the buffer: the rewriter would need
that state, and its memorization would break); look for the tag from the
place of the label's tree (the residual `auto-id` has a decorated place, not
the call's); turn the stable labels off with the flag (gives up #333's
stability).
*Decided by:* the labels made before and after keystrokes, the same with
and without the flag on six documents (conventions, structure,
mathematics, `env-par`, welcome, the environments of the tests); the
counters of the typesetter on the conventions page (9 bridges typeset again
instead of 21, 1.53 ms instead of 2.82).

**3.2 New environments in the rewriter: stable values.** A value which is
made again as a new tree although it did not change (the cell format of a
`tformat`, a macro body with its arguments substituted) gave a new key, a
new environment, and every evaluation after it missed its memorizer;
Joris's reminder about `the-tags` and `the-label` and his `FIXME: should we
remember partial expansions?` are this case. Decision: such values are
interned (`intern_tree`): a tree with the same label and the same children
is the tree made before. The children are compared by address, atoms by
their strings unless they carry a place of the document (an argument of
one call with the same text as one of another call would bring its place).
Interned: the cell formats (`evaluate_formatting`) and the parts of macro
bodies which hold arguments (`expand`, bottom up); the empty cell format of
a `table` is one static tree. The interned trees are memorizers, in the
same table and with the same lifetime as the others.
*Alternatives:* keys made of the inputs: key the memorizers by the values
actually read (the input hashes of #333) instead of the addresses of the
environments. General (an unchanged label no longer makes a new environment
for the rest of the document, whatever made the new object), but replaces
the core of the memorizers and costs the recording of the reads; not
needed after the interning, kept for a case it does not cover. Memorizing
the expansion of each call by its arguments: covers the bodies, not the
cell formats, and a call with one changed argument is expanded again whole.
Comparing values by content in the keys (tried at step 3): a deep
comparison at each lookup, and no gain then.
*Decided by:* the trace of the evaluations which leave with another
environment than before (none left on the conventions, mathematics and
welcome pages); the evaluations per keystroke (1660 to 93 on the
conventions page, 1.4 to 0.5 ms); the keystroke as fast as without the
flag; the first rewriting of the glue reference 58 ms instead of 51, with
more memorizers (120000 instead of 68000).
