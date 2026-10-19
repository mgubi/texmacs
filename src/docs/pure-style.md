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
- `move`, `shift`, `resize`, `clipped`: only the body is evaluated. Their
  lengths may use the extents of the box (`1r`, `1w`, ...), which the
  typesetter binds while it lays the box out (`local_begin_extents`), so
  they are left to it.

Still missing, because they are not pure functions of the environment and
the markup: `auto-id` and `get-attachment` (counters and data stored on the
buffer), `find-accessible`, the themes (`new-theme`, `copy-theme`,
`apply-theme`, `select-theme`), and the lengths that depend on the screen or
on the ornament being typeset (`guipx`, `lcorner`, `bcorner`, `rcorner`,
`tcorner`).

With these changes, *About* evaluates without errors. The welcome page and
the *style* page keep one `bad merge` (from `auto-id` in `the-auto`); the
conventions page also fails in its graphics, where
`look-up (box-info (..., wh), 1)` picks apart the residual `box-info` and the
arithmetic above it fails (`bad plus/minus`, `bad times/over`).

## Open questions for the review

- **What the result is for.** The output is never typeset. Joris's goal
  (above) is that the typesetter consumes it: boxes from primitives only,
  with `exec` reduced to what needs layout information, and laziness from
  the memorizers. Uses on the way there: the expanded document for
  converters, checking styles, or a cache key for incremental typesetting
  (compare the typing caches of PR #250/#333).
- **Layout-dependent values: partial evaluation.** Some markup needs
  information that only the typesetter has: the box extents in `move` and
  the like (handled above by not evaluating those arguments), `box-info`,
  `frame-direct`, `frame-inverse`, the screen and ornament lengths. The
  rewriter keeps such tags as they are, but an operation applied to a
  residual tag (`look-up`, `plus`, `merge`) then computes garbage. A sound
  rule is partial evaluation: a value that depends on a residual stays a
  residual expression, to be finished by the typesetter. That residual must
  then still find the definitions it uses (macros and variables assigned in
  the document are consumed by the rewriter today).
- **Side effects.** `extern` calls Scheme with a re-entrancy hack
  (`reenter_rewrite_env`); `set-binding`, `write`, counters, labels and
  `auto-id` (which numbers the automatic labels from data stored on the
  buffer) depend on document order. Memorizing them by address is only
  correct if they are pure; they need either an explicit state threaded
  through the evaluation (as the environment already is) or to be left as
  residuals.
- **Memory.** Memorizers are released by reference counts; the global table
  grows with the document and the edits. Nothing measures it yet.

## Testing

Build: the usual configure line plus `--enable-experimental`. Run TeXmacs
with a window, switch on Debug ▸ style and look at the terminal; for a
scripted run, `(debug-set "style" #t)`, load a file with `-x`, edit after a
pause (`delayed (:pause 4000) ...`) and quit. Counting
`Evaluate`, `Memorized` and `error (` in the output gives a quick measure.
Use a separate `TEXMACS_HOME_PATH` so the runs write no preferences.
