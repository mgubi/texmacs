# The experimental style rewriter

Branch `wip_pure_style` (from `upsilon_texmacs`, 2026-10-19) reviews the
style rewriter that Joris van der Hoeven started in January 2007 (commits
e8e500db55 to be7a72a74b, "use --enable-experimental to test") and never
finished. This note describes what the code does, how it is hooked into the
editor, what state it is in, and what a continuation would involve.

## Idea

The typesetter evaluates style markup (`assign`, `with`, `value`, macros,
arithmetic, `if`, ...) while it builds boxes, in `edit_env_rep::exec`
(`src/src/Typeset/Env/env_exec.cpp`), and mutates one environment as it goes.
The rewriter separates the two: it evaluates the document into a tree that
contains only typesetting primitives, as a **pure function** of an
environment and a tree,

    evaluate : (environment, tree) -> (environment', tree')

and memorizes every call. Environments are persistent (an update makes a new
environment that shares the old one), and the document is kept as a *clean
copy* whose unchanged subtrees keep their addresses after an edit. A call is
therefore identified by the pair of addresses (environment, tree), compared
by pointer (`weak_hash`, `weak_equal`), and after an edit only the calls on
the path to the change are recomputed.

## What Joris wanted

Joris did not write a design document for the rewriter; his intentions are
in a few places of the documentation and in the code itself (summary with
the sources in [pure-style-goals.md](pure-style-goals.md)).

- **Separate evaluation from typesetting.** *The typesetting process*
  (`TeXmacs/doc/devel/format/basics/typesetting.en.tm`, 2004): typesetting
  "can be subdivided into two major parts (which are currently done at the
  same stage, but this may change in the future): evaluation of the
  TeXmacs tree using the stylesheet language, and the actual typesetting."
  The rewriter is the first of the two parts on its own.
- **To make the typesetter lazy.** *To do list for improving the current
  implementation* (`TeXmacs/doc/about/projects/todo.en.tm`), under
  *Improving the speed*: "Increased laziness. The typesetter should be made
  lazy in a more fundamental way (see upcoming style rewriting tool)", next
  to "Better caching mechanism: optimize performance as a function of cost
  of recomputation and storage requirement" (which is what the memorizers
  are for). So the result is meant to **feed the typesetter**, which then
  only deals with primitives and is redone only where the evaluation
  changed.
- **The section *Upcoming style rewriting tool*** of the same list
  (reminders for himself):
  - "Attention to implicitly changed variables, such as `the-tags`,
    `the-label`, etc., which may cause the entire document to be
    retypeset": the variables that markup changes as a side effect break
    the locality of the memorization (a label changes the environment of
    everything after it).
  - "Attention to `base-file-name`, `cur-file-name`, `current-part`. Also
    put these variables in `vars.hpp`" (they are set by hand in
    `environment_update`).
  - "Replace `arg` primitive by quasi-quoting mechanism. This seems to be
    quite necessary: consider `<assign|foo|<macro|body|...>>` and a document
    piece `<\foo> ... </foo>` with a large body. When making changes to the
    body, the environment for typesetting the body also changes, because we
    need to store the body in the foo environment argument variable. For
    this reason, we implemented an alternative mechanism, which is enabled
    using `#define ALTERNATIVE_MACRO_EXPANSION`." This is why the
    substitution of arguments is the default: with arguments in the
    environment, an edit inside a long macro argument would change the
    environment, hence the memorization key, of everything inside it.
  - "More systematic usage of symbols instead of strings" (the
    environments are keyed by tree labels).
  - Hyperlinks and multiple views: which locus to prefer when one is
    ambiguous (the rewriter's `hard-id` combines the addresses of the
    environment and the tree).
  - **Auxiliary data in hidden markup** (labels, loci, links, indexes):
    "We might want to associate auxiliary data to tree nodes instead of
    files", distinguishing contents that are not evaluated when hidden but
    whose auxiliary data must be kept (a hidden chapter), contents that are
    "evaluated but not typeset: auxiliary data may be kept in macro
    expander" (a fold), and contents that are ignored altogether (a
    comment). The middle case only exists once evaluation and typesetting
    are separate.
- **Macro semantics.** *Macro expansion* (`devel/style/design/style-eval`)
  explains that a macro argument is bound to the *unevaluated* expression and
  evaluated at each use, like a Scheme macro wrapping the argument in a
  thunk; substitution (the alternative expansion) keeps that meaning.
- **He kept the code alive.** Long after 2007 he added the new primitives to
  the rewriter as well: `blend` (2015), `with-package` (2017), `provide` and
  `or-value` (2019, marked "provisory"), the `paw` unit (2020). The project
  was paused, not abandoned.

## Code

All of it is in `src/src/Style/` (about 4800 lines), compiled only with
`--enable-experimental` (`EXPERIMENTAL`, `src/misc/m4/tm_debug.m4`;
`ENABLE_EXPERIMENTAL` with CMake).

- `Environment/`: environments as maps from tree labels (`int`) to trees.
  `basic_environment` is an open hash table; `assoc_environment` a small
  array of bindings (the arguments of a `with`, an `assign` or a macro call);
  `list_environment` a chain of them; `std_environment` the environment
  of the evaluator, with the local bindings `env`, the enclosing environment
  `next`, a flattened `accel` chain for lookups and, for the classical macro
  expansion, the macro arguments `args`. A `pure` flag decides whether an
  `assign` makes a new layer or merges into the current one.
  `std_environment.cpp` defines the operations (`primitive`, `assign`,
  `begin_with`, `end_with`, `macro_down/up`), each memorized like the
  evaluations.
- `Memorizer/`: `memorizer_rep` records one computation (type, inputs,
  outputs, children). All memorizers are interned in one global hash table
  (`bigmem_*` in `memorizer.cpp`): constructing a memorizer for a computation
  already in the table returns the old one, and `is_memorized` (reference
  count at least 3) tells that it holds a result. The stack
  `memorize_start/end` attaches to each memorizer the memorizers of the
  calls made inside it, which keeps their results alive as long as the
  parent's. `clean_copy.cpp` keeps the clean copy of the document up to date
  (`copy_announce`, from `edit_done`) and copies the ips onto it.
- `Evaluate/`: `evaluate_main.cpp` has `evaluate (tree)` (memorized, the
  current environment in the global `std_env`) and the dispatch
  `evaluate_impl` on the tag. The other files follow `env_exec.cpp`:
  macros (`evaluate_macro.cpp`), control, booleans, numbers, lengths, text,
  miscellaneous primitives, quoting, and `evaluate_rewrite.cpp` for the tags
  that rewrite into other markup before evaluation (`extern`, `include`,
  `with-package`, `rewrite-inactive`), also memorized.
  `evaluate_inactive.cpp` is the rewriter's version of the rendering of
  inactive markup (source mode).

Macro expansion has two variants (`environment.hpp`):
`CLASSICAL_MACRO_EXPANSION` (arguments in a separate environment, as the
typesetter does) and `ALTERNATIVE_MACRO_EXPANSION` (the default): a call
substitutes its arguments into the body (`expand` in `evaluate_macro.cpp`)
and evaluates the result, so `arg` never needs the caller's environment.

## Hooks in the editor

Only with `EXPERIMENTAL`:

- `editor_rep` (`src/src/Edit/editor.hpp`) has `ste` (the style environment),
  `cct` (the clean copy) and `mem` (the memorizer of the last evaluation).
- `edit_main_rep` makes the clean copy when the editor is created and drops
  the memorizer when it is destroyed.
- `edit_done` (`edit_modify.cpp`) applies each modification to the clean
  copy.
- `edit_typeset_rep::environment_update` fills `ste` with the typesetter's
  environment after the style (`typeset_prepare`, `read_env`).
- `edit_interface_rep::apply_changes` calls `environment_update` on
  `THE_ENVIRONMENT` and, on `THE_TREE`, evaluates the clean copy and **prints
  the result to the terminal**. Nothing uses the result: the typesetter
  still does its own evaluation. `apply_changes` does nothing headless, so
  the rewriter runs only with a window.

With the debug flag `style` (Debug ▸ style, or `(debug-set "style" #t)`)
the evaluator traces every call to `cout` (`Evaluate`, `Memorized`,
`Computed`, with the addresses of the tree and the environment), and the
editor prints the rewritten document after each change. The flag is read
once per evaluation of the document (`memorize_initialize`), so the
indentation stays balanced.

## State (2026-10-19)

PR #82 (2026-10-02) made the files compile again, but the build still did not
link. On this branch:

- `makefile.in`: the Style objects need `Texmacs/Data` on the include path
  (`new_buffer.hpp`, for `with_package_definitions`).
- `evaluate_inactive.cpp`: `arg_type`, removed from the typesetter in 2010
  (54f8c9de1d) when highlighting moved to DRD types, is defined again on top
  of `the_drd->get_type_child`.
- `evaluate_main.cpp`: `hard-id` passed its first child instead of itself to
  `evaluate_hard_id`; this crashed on every `hlink`.
- `evaluate_macro.cpp`: the `xmacro` case of the alternative expansion tested
  `t[i]` with `i` uninitialized instead of `t[0]`.

With these fixes the Vue+S7 build with `--enable-experimental` opens and
edits documents without crashing (the welcome page, *About*, the
conventions and style pages of the manual). On an edit, only the path to the
change is evaluated again and the siblings come from the memorizers, as
designed.

### Primitives

`evaluate_impl` handled 130 tags against 177 in `edit_env_rep::exec`. The
rewriter keeps the tags it does not know and evaluates their children, which
is right for the typesetting primitives (`eff-*`, `anim-*`, `morph`,
`filter-style`) but was wrong for those that compute a value. Ported from
the typesetter on this branch:

- `minimum`, `maximum` (with `tmlen_min`, `tmlen_max`): their absence broke
  every real document (the `itemize` counters compute
  `mod (..., minimum (...))`);
- `rgb-color`, `rgb-access`, `has-binding`, `occurs-inside` (its second
  argument is taken as it stands, since the alternative expansion has
  already substituted the `arg`), `find-file-upwards`, `var-mark`, the
  units `ms` and `s`;
- `or` and `and` with the typesetter's meaning (the first value which is not
  `false`, resp. `false` or the last value), not strict booleans:
  `tmdoc-file` relies on it;
- `move`, `shift`, `resize`, `clipped`: their lengths may use the extents
  of the box (`1r`, `1w`, ...), which the typesetter binds while it lays the
  box out (`local_begin_extents`); they are evaluated partially (below).

Still missing, because they are not pure functions of the environment and
the markup: `auto-id` and `get-attachment` (counters and data stored on the
buffer), `find-accessible`, the themes (`new-theme`, `copy-theme`,
`apply-theme`, `select-theme`), and the lengths that depend on the screen or
on the ornament being typeset (`guipx`, `lcorner`, `bcorner`, `rcorner`,
`tcorner`).

With these changes and partial evaluation (below), **every English page of
the documentation (696) is rewritten without an error in its result**, and
on the pages traced (*About*, the welcome page, the *style* page, the
conventions, structure and mathematics pages of the manual) no error is
computed at all, intermediate values included. The last fixes found by
rewriting the whole documentation: `translate` of markup gives the markup,
as in the typesetter; the rendering of inactive markup used `src-integer`,
which the typesetter renamed `src-numeric` in 2010. The whole documentation
takes 2.06 million evaluations, 5.2 s (about 2.5 µs per evaluation).

### Partial evaluation

Some values are only known to the typesetter:

- the size of a typeset box (`box-info`, `frame-direct`, `frame-inverse`);
- what depends on the order of the document and on data kept with the
  buffer: `auto-id`, which numbers the automatic labels (the typesetter does
  it itself since #333), and `get-attachment`;
- **every length in a unit**. The typesetter converts `3fn`, `1cm` or `1r`
  where the length is used, with the scale at which it lays the document
  out and, for some units, with the font, the paragraph, the box (`w`, `h`,
  `l`, `b`, `r`, `t`, which `move`, `shift`, `resize` and `clipped` bind)
  or the ornament being typeset. An absolute length computed outside (a
  `tmlen`) misses that scale: in a headless typesetting at twice the size,
  every unit is doubled but a `tmlen` is not, and the lists of the rewritten
  documents lost half of their indentation. So the rewriter converts no
  length; the units themselves (`fn-length` and the like) stay too.

Such a value is a *residual* (`is_residual` in `evaluate_main.cpp`), and a
computation on a residual is a residual too:

- a strict computation (`plus`, `look-up`, `merge`, the comparisons, ...)
  first evaluates its arguments; when one is a residual it returns itself
  with the evaluated arguments, else it computes as before, its second
  evaluation of the arguments being found in the memorizers. A length in a
  unit counts as a residual only in a computation on lengths (`merge` of
  `1` and `r` is the string `1r`, `plus (2, 3)` is `5`);
- `if`, `case`, `or`, `and` whose condition is a residual keep the remaining
  branches, evaluated;
- **a value which holds a residual is referred to, not copied**: `value (x)`
  stays `value (x)`, and the `with` or `assign` which binds `x` stays in the
  result, so that the typesetter computes the value where it was bound. A
  copy would be computed elsewhere, in another environment: `box-info`
  typesets its argument again, a length is converted with another font.

Macro arguments, values and macros in a residual are substituted, so that
the typesetter can finish it without the definitions of the document; the
variables it refers to are bound in the result. The suite checks these
cases (group *partial evaluation*). The cost on *About*: 609 evaluations
instead of 489, 96 of them found in the memorizers (the second evaluations
of the arguments). The branches kept by a residual condition are evaluated
although the typesetter may not take them, which matters only for side
effects (step 3).

### Typesetting the result

The first stage of step 4 checks the meaning of the result without changing
the typesetter: for a document D, a copy of D whose body is the rewritten
body (same style, same initial environment) is typeset and printed, and its
pages are compared with those of D, rasterized (`mutool`). If the rewriter
is right, they are identical, since the result only holds typesetting
primitives, residuals and the bindings they need. Each difference was a
fault of the rewriter:

- **lengths** were converted to absolute `tmlen` (above);
- **copied residuals**: `box-info` measured its argument again deep inside
  a graphics, at another size (above);
- **`if*`** was evaluated like `if`. For the typesetter of markup it is a
  phantom (`concater_rep::typeset_var_if`: the body typeset invisibly, with
  its size), which `exec` treats like `if`; the result is markup, so `if*`
  stays, evaluated (`phantom` is `if* (false, …)`);
- **assignments** were consumed. The typesetter reads variables of its own
  (`page-odd-header`, `page-this-header` for the pager), so an assignment
  stays in the result unless it defines a macro (macro calls are expanded);
- **values evaluated twice**: the typesetter evaluates the values of the
  `assign` and `with` of the result again, which expanded the quoted
  `page-the-page` of a running header at the assignment (page 0) instead of
  in the pager. The values are protected: `quote` of the value, or
  `quasiquote` with the residuals in `unquote`.

- **labels and references**: `get-binding` was answered from the
  rewriter's own label table, empty, so references printed `?`. The label
  tables are filled by the typesetter while it typesets (`set-binding`, as
  Joris's FIXME in `evaluate_set_binding` said): `set-binding` stays in the
  result, `get-binding` and `has-binding` are residuals;
- **inactive markup**: the rewriter's own rendering of 2007
  (`evaluate_inactive.cpp`) has drifted from the typesetter's (DRD types of
  the arguments, `src-*` classes, the DRD of the document). `inactive` and
  `inactive*` stay in the result, their source unevaluated, and a
  `rewrite-inactive` inside another macro is given its source through a
  macro applied on the spot (the typesetter's `rewrite-inactive` only takes
  a macro argument). The tags are kept rather than that macro form, since
  the concater lays out their children by their DRD (block sources);
- **patterns**: their size was computed by the rewriter, at its scale; a
  `pattern` is a residual, so that the `with` which binds it lets the
  typesetter evaluate it.

The harness had faults of its own, found on the way: the copy kept the
version of old documents and was upgraded a second time when loaded (a
`--` became a dash); written as text, adjacent strings of a `concat` were
read back as one string, which kerns otherwise; relative images were
resolved from the directory of the copy. `tests/style/compare.sh` avoids
them (current version, the rewritten body set as a tree, the master of the
copy set to the document).

**Result: of the 696 English pages of the documentation, 692 are identical
to the pixel.** Left:

- `main/beamer/man-animations`: an animation, which differs between two
  printings of the document itself;
- `main/styles/std/std-utils-dtd`: an example renders a `quote` returned by
  an `extern` (`map`). The typesetter typesets the result of a rewrite as
  markup, where `quote` is shown (`typeset_inactive`), while the rewriter
  evaluates it as a value (`quote` gives its argument). The concater and
  `exec` differ on a few tags (`quote`, `quote-value`, `quote-arg`, `if*`,
  which is handled); a rewriter faithful on all of them has to know whether
  it evaluates markup or a value, and memorize the two apart. Also a
  slightly different spacing of an inactive `map`, which the DRD of the
  copy may explain (the DRD completes the tags from their uses in the
  document, and the active uses are expanded in the copy; typesetting the
  copy once with the original body first did not change it);
- `main/automated/tag-help` and `main/automated/top-help`, generated pages
  which show Scheme templates as inactive markup: small differences of
  spacing and indentation, not explained yet.

The syntax highlighting of programs is left out of the comparison
(`tree-sitter-set-highlighting #f`). The tree-sitter highlighter
(`treesit_language_rep::highlight`) takes a compound subtree which is not a
line of a program as coloured, with no colour: a `with` around inline code
written in the document swallows the colours of its text, while the same
`with` coming from a macro body (here `cpp`) does not. The rewriter writes
macro bodies into the document, so the code of 215 pages lost its colours.
It is a property of the highlighter, to be decided there.

### Typesetting in the editor

The second stage of step 4: with `TEXMACS_STYLE_TYPESET=1` (in the
experimental build), the editor typesets the result of the rewriter instead
of the document.

- **The tree.** Before each typesetting pass (`edit_typeset_rep::
  typeset_sub`) the document is rewritten, and when the result changed the
  typesetter gets the whole result (`notify_assign` at the root): for now
  every change typesets everything again; the modifications of the
  document no longer reach the typesetter (`edit_modify_rep::
  rewritten_document`), they only mark the tree as changed.
- **The places.** The boxes must carry places of the document, for the
  cursor and the selections. The rewriter attaches them to its trees
  (`transfer_ip`, `decorate_ip` for what comes from macro bodies). The
  typesetter normally derives the place of a child from that of its parent
  (`descend`); with a rewritten tree, the bridges (`make_bridge`) and the
  concater (`concater_rep::typeset`) take the place attached to the tree
  instead (`typesetter_rep::rewritten`, `edit_env_rep::rewritten_tree`), and
  the root has no place in the document. A derived place would look
  accessible (only its first item is tested) while holding the negative item
  of the root: the cursor followed paths which do not exist.
- **`bridge_rep::my_resync`**, "a dirty hack [which] ensures a perfect
  coherence between the bridge and the edit tree", replaces the tree of a
  bridge by the subtree of the document at its place. On a rewritten tree it
  gave a `with` bridge a paragraph of the document: it is skipped.
- **Safety.** When typesetting fails, the editor resets the document to an
  empty one (`assign (rp, tree (DOCUMENT, ""))`); with the rewritten tree it
  goes back to typesetting the document instead, which is left intact.
- `build_locus` looked up the subtree of the document at the place of a
  locus body coming from a macro; that place may not exist with a rewritten
  tree: the lookup is skipped then (`has_subtree`).

Result (`tests/style/compare-editor.sh`, both printings in the same
process each): **686 to 695 of the 696 pages print identically with and
without the flag**, with no fallback. The animation page differs between
any two printings; 9 developer pages differ in the colours of their
program code, or not, from one run to the next: the older highlighter
depends on the documents printed before in the same process (printed
alone, they are identical). The pages which differed in the comparison
of the copies because of their DRD (`tag-help`, `top-help`, the `map`
example of `std-utils-dtd`) are identical here, where the DRD is the
document's. The 10 left: the animation page, and 9 developer pages whose
program code is coloured otherwise by the older highlighter, which depends
on the documents printed before in the same process (printed alone, they
are identical).

**Boundaries of macro calls.** Around a macro call (an `item`), the editor
can put the cursor before or after the tag: the concater marks the
boundaries of the call (`typeset_compound`, `marker (descend (ip, 0))` and
`(ip, 1)`). In the rewritten tree the call is expanded, and at first the
cursor went to the nearest place inside (`XYZ` typed at the start of a list
item went after the bullet, not before). The rewriter now keeps the
expansion of a macro call of the document (a node with a place in the
document) in an `expand-as` with the place of the call, whose boundaries
the concater marks (`typeset_expand_as`); `exec` of an `expand-as` gives its
body. This is done where markup is assembled (`evaluate_markup`: the
children of the typesetting primitives other than data such as `tuple`,
the body of a `with`), not in the values, on which the rewriter computes.

With it, the same moves (`go-down` and `go-up` 25 times, `go-right` 40
times, ends of lines, typing `XYZ`, more moves) from the start of the list
document of the tests, *About*, the welcome, *style*, conventions,
structure and mathematics pages give the same cursor paths and the same
document with and without the flag, at every step. (Headless, the cursor
does not move: the moves need a window, `(apply-changes)` after each.)
**The environment at the cursor** (what the menus and the keyboard read:
the mode, the fonts, the language) is computed by running the typesetter's
tree until the cursor (`typeset_exec_until`, `exec_until` of the bridges),
along a path of the document, which with the flag is not a path of the
typesetter's tree. With the flag, the document itself is executed until the
cursor instead (`edit_env_rep::exec_until`), as the bridges do when they
have not been typeset yet. `tests/style/compare-environments.sh` puts the
cursor at the start of the strings of a document and compares some
variables of the environment there: on `tests/style/environments.tm` (math,
code, a list, a table, a theorem, a user macro) and five pages of the
manual (up to 378 positions each), none differs. (Putting the cursor in a
macro definition of the document hangs, also without the flag: the tool
leaves them out.)

Still open: everything is typeset again at each change (the modifications
of the rewritten tree are step 5).

**The places, precisely** (`tests/style/compare-places.sh`, which compares
the paths of the document found at 5000 points of the boxes, what a click
finds, with and without the flag). The rule, in `rewritten_place`
(`bridge.cpp`), used by the bridges, the concater and the lazy typesetting
of paragraphs (`make_lazy`, for the cells of tables):

- a tree which comes from the document (its attached place is a place of
  the document, `is_document_place`) has that place;
- below a tree of the document, a tree which does not come from there (a
  value, a tag which the rewriter keeps) has its own decoration, or one at
  the right of the parent: the place derived from the parent would be a
  path of the document which is not this tree's;
- within the expansion of a macro call, the place is derived from the
  call, as when the call is typeset: the `expand-as` of a call typesets its
  body as the body of a macro (`attach_right`), in the concater, in
  `bridge_expand_as` and in `make_lazy_expand_as`. The places which the
  rewriter attached to the expansions of macro bodies are left aside: the
  same expansion, memorized, is shared between the calls with the same
  environment and carries the place of only one of them.

The typesetters made while typesetting (for the cells of tables) inherit
the mode from the environment. An assignment stays in the result also when
it defines a macro, with the place of the assignment: it is typeset
invisibly, as in the document, and a click on its line finds it. On the
list document of the tests, *About*, the welcome, *style*, conventions,
structure and mathematics pages, and a paragraph with three identical
macro calls, all the points find the same paths, but 2 on the conventions
page, at the start of a key in a table, which find the same place one
level deeper (in the `small` inside a `with`).

## Relation to the typesetter caches

PR #333 (incremental typesetting) and PR #250 (typing caches), merged in
October 2026, made the typesetter reuse its work after an edit. The rewriter
is meant to do the same one level up, so the two are worth comparing.

**In the code they do not interact.** Without `EXPERIMENTAL` the rewriter
is not compiled. With it, the typesetter makes its usual pass with its caches
and, separately, the rewriter evaluates its clean copy with its memorizers;
nothing is shared and the rewriter's result is dropped. The one contact
point is `environment_update`, which calls `typeset_prepare` and writes
`base-file-name`, `cur-file-name` and `secure` into the typesetter's
environment outside a typesetting pass. Since #250, `typeset_prepare` writes
back the variables changed since the prepared table, so this should be
harmless, and the check modes of the caches confirm it: the group
*typesetter caches* of the suite calls `environment_update` between edits
and incremental passes of the welcome page, with every reused bridge
typeset again and compared, and finds no mismatch.

**They solve the same problem differently:**

| | Rewriter (2007) | Typesetter caches (#333, #250) |
|---|---|---|
| Unit of reuse | one evaluation, (environment, tree) → result | one bridge (paragraph, tag, …) |
| Cache key | the **addresses** of the environment and of the tree | the **inputs actually read**: variables read before written, with their values, folded into a 64-bit hash; an environment fingerprint |
| Side effects | not handled (Joris's "implicitly changed variables") | outputs recorded per bridge (label bindings, `out_bindings`) and replayed on reuse; label tables carry stamps |
| `auto-id` | stateful, breaks the memorization | automatic labels numbered by the tag that produces them, stored as `ADDENDUM_AUTO_ID` on the tree node |
| Zoom, pixel lengths | evaluated with the current zoom | the zoom is a recorded input; a zoom does not retypeset |

Two consequences:

- **The address key is Joris's worry.** Any change of the environment (a
  counter, a label) makes a new environment object, and every evaluation
  after it misses its memorizer, even when it never reads the changed
  variable. Substituting macro arguments removes one instance of this;
  recording what is actually read, as #333 does, removes it in general.
- **#333 already does part of Joris's plan, in the typesetter.** Automatic
  labels attached to tree nodes are a form of his "auxiliary data on tree
  nodes", and the recorded label outputs handle side effects without giving
  up reuse.

## Open questions for the review

- **What the result is for.** The output is never typeset. Joris's goal
  (above) is that the typesetter consumes it: boxes from primitives only,
  with `exec` reduced to what needs layout information, and laziness from
  the memorizers. Uses on the way there: the expanded document for
  converters, checking styles, or a cache key for incremental typesetting
  (compare the typing caches of PR #250/#333).
- **Layout-dependent values.** Handled by partial evaluation (above), except
  the zoom. Whether the typesetter finishes the residuals correctly is only
  tested once it consumes the result (step 4).
- **Side effects.** `extern` calls Scheme with a re-entrancy hack
  (`reenter_rewrite_env`); `set-binding`, `write`, counters, labels and
  `auto-id` (which numbers the automatic labels from data stored on the
  buffer) depend on document order. Memorizing them by address is only
  correct if they are pure. #333 shows a working answer in the typesetter:
  record what each unit reads and writes, replay the writes on reuse, and
  number the automatic labels by the tag that produces them (see the plan).
- **Memory.** Memorizers are released by reference counts; the global table
  grows with the document and the edits. Nothing measures it yet.

## Plan

1. **Keep it building, make it testable** (done). No CI job: the build with `--enable-experimental` and the
   suite `style-rewrite-test.scm` are run locally (see *Testing*) after a
   change of the typesetter or of the rewriter; the link was broken from
   2010 to 2026 without anyone noticing. `(style-rewrite-buffer)` runs the
   rewriter headless. The contact point with the typesetter caches
   (`environment_update`) is checked with their check modes: no mismatch.
2. **Partial evaluation** (done, see above). Values that need layout (`box-info`,
   `frame-direct`, `frame-inverse`, `guipx`, the corner lengths, the box
   lengths of `move` and the like) become residuals; an operation on a
   residual gives a residual. A residual is closed: the known `value`s and
   macros inside it are substituted, so that only the unknown leaves remain
   and the typesetter can finish it without the document's definitions.
   Target: a `look-up` or `plus` of a `box-info` gives a residual (a check
   in the suite), and the graphics of the conventions page compute no
   errors, not even in values which are dropped.
3. **State and keys.** Measured first (a document of 50 sections, each
   followed by a paragraph, `style-rewrite-statistics`):

   | Edit | Evaluations | Memorized |
   |---|---|---|
   | first evaluation | 9701 | 350 |
   | text of paragraph 20 | 51 | 50 |
   | title of section 20 | 181 | 86 |
   | a section inserted at 10 | 5445 | 1497 |

   Edits are local, also in a section whose macros assign counters: after
   the substitution of the arguments, only the parts of the expansion which
   hold the edited argument are new trees, the rest is shared code of the
   macros, evaluated in the same environment; and a value computed again is
   the memorized tree itself, so an assignment of the same value finds its
   memorizer and gives the same environment (comparing atomic values by
   content in the keys, tried, changed nothing). Only a real change of a
   counter (a section inserted) evaluates everything after it again, 56 % of
   a first evaluation, though most of that work does not read the counter.
   - *Done:* `auto-id` and `get-attachment` are left to the typesetter
     (above), which removed the last errors.
   - *Left:* keys made of the inputs actually read (the environment input
     hash of #333) instead of addresses would save most of the 56 %; it is
     an optimisation, worth doing with step 4, when the cost of the
     rewriter adds to the typesetter's. The side effects of `write`,
     `set-binding` and counters in the branches kept by a residual condition
     are not handled.
4. **The typesetter consumes the result.** Split the work instead of
   stacking two caches: the rewriter evaluates the macros and memorizes by
   what it reads; the typesetter's recording shrinks to what needs layout
   (zoom, pixel lengths, box extents, residuals). The interface is a patch:
   unchanged subtrees of the result keep their addresses, so the changed
   part is cheap to find and is handed to the typesetter as modifications,
   as edits are now. First stage (above, *Typesetting the result*): the
   result typeset in a copy of the document, compared pixel for pixel with
   the document. Second stage (*Typesetting in the editor*): the editor
   typesets the result, behind `TEXMACS_STYLE_TYPESET=1`, the boxes carrying
   the places of the document; the boundaries of macro calls, the places
   of the boxes and the environment at the cursor are those of the usual
   typesetting (*Typesetting in the editor*).
5. **Laziness.** Connect the memorizers to the typesetter's invalidation, so
   that an edit only retypesets what the evaluation changed; benchmark
   against the current typesetter with the #333 scripts; bound the memory
   of the global memorizer table.

On the way, the #333 check modes and fuzzer can compare the rewriter's
expansion with what `exec` computes for the same document.

## Testing

The check is local, not in CI. Build with the usual configure line plus
`--enable-experimental`, then run the suite:

    tests/scheme/check.sh TeXmacs/progs/check/style-rewrite-test.scm

and the comparison by typesetting (about 12 minutes for the whole
documentation, or give some documents):

    tests/style/compare.sh [document.tm...]

which prints the documents whose rewriting does not typeset like them
(four known ones, above), and exits with 1 if there is any; and the
typesetting of the rewritten tree by the editor itself (*Typesetting in the
editor*):

    tests/style/compare-editor.sh [document.tm...]

and the places of the boxes in the editor (*Typesetting in the editor*):

    tests/style/compare-places.sh document.tm...

and the environment at the cursor:

    tests/style/compare-environments.sh document.tm...

It is not listed in `check-master.scm`, since the rewriter exists only in
the experimental build (elsewhere the suite fails at once, saying so). It
runs headless through two commands of the experimental build
(`build-glue-style.scm`, `glue_style.cpp`, included by `glue.cpp` under
`EXPERIMENTAL`):

- `(style-rewrite-buffer)`: the rewritten document of the current buffer
  (`edit_typeset_rep::style_rewrite`, which `apply_changes` also does with
  a window);
- `(style-rewrite-statistics)`: evaluations, memorized ones and size of the
  memorizer table, for the last evaluation of a document;
- `(style-environment-update)`: `environment_update`, which `apply_changes`
  calls when the environment changed.

The groups: macro expansion and the ported primitives on small documents;
the memorization (numbers of evaluations after loading, without a change,
after an edit of one paragraph); pages of the manual, whose errors in the
result are compared with the known ones (a new error and a fixed one both
fail: update `manual-pages` when the rewriter improves); every English page
of the documentation, none of which may leave an error (25 s); the typesetter
caches, which `environment_update` must not disturb. That last group counts
mismatches only in the check modes of the caches:

    TEXMACS_TYPESET_READS=check TEXMACS_TYPESET_CACHES=check \
    TEXMACS_PAGE_REUSE=check TEXMACS_PAGE_BREAK_FAST=check \
    TEXMACS_PAGE_BREAK_CACHE=check \
    tests/scheme/check.sh TeXmacs/progs/check/style-rewrite-test.scm

Run the suite both ways. The log
(`tests/build/scheme/style-rewrite-test.log`) gives the evaluations and the
time of each page.

To look at the evaluation itself, run TeXmacs with a window, switch on
Debug ▸ style and watch the terminal, or `(debug-set "style" #t)` in a `-x`
script. Use a separate `TEXMACS_HOME_PATH` so the runs write no
preferences.
