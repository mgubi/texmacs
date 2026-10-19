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

The evaluator traces every call to `cout` (`Evaluate`, `Memorized`,
`Computed`), which makes the build unusable for anything but this test.

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

### Primitives the rewriter lacks

`evaluate_impl` handles 130 tags; `edit_env_rep::exec` 177. The rewriter
keeps the tags it does not know and evaluates their children, which is right
for the typesetting primitives (`move`, `resize`, `clipped`, `shift`,
`eff-*`, `anim-*`, `morph`, `filter-style`) but wrong for those that compute
a value:

- `minimum`, `maximum`: this is what breaks every real document now (the
  `itemize` counters compute `mod (..., minimum (...))`, which gives
  `error (bad modulo)` and then `bad plus/minus`, `bad merge`,
  `bad times/over` above it);
- `rgb-color`, `rgb-access`;
- `has-binding`, `get-attachment`, `auto-id`, `find-accessible`,
  `occurs-inside`, `find-file-upwards`, `var-mark`;
- `new-theme`, `copy-theme`, `apply-theme`, `select-theme`;
- the length units `ms`, `s`, `guipx` and the corner lengths (`lcorner`,
  `bcorner`, `rcorner`, `tcorner`).

## Open questions for the review

- **What the result is for.** The output is never typeset. Continuing means
  either making the typesetter consume the rewritten tree (boxes from
  primitives only, with `exec` reduced to the primitives that need layout
  information), or using it elsewhere first: as the expanded document for
  converters, for checking styles, or as a cache key for incremental
  typesetting (compare the typing caches of PR #250/#333).
- **Layout-dependent values.** Some markup needs information that only the
  typesetter has (`box-info`, `frame-direct`, `frame-inverse` are commented
  out; page and font lengths read the environment). Which values the pure
  pass may compute and which it has to leave to the typesetter needs a
  clear rule.
- **Side effects.** `extern` calls Scheme with a re-entrancy hack
  (`reenter_rewrite_env`); `set-binding`, `write`, counters and labels
  depend on document order. Memorizing them by address is only correct if
  they are pure.
- **Memory.** Memorizers are released by reference counts; the global table
  grows with the document and the edits. Nothing measures it yet.
- **Tracing.** The `cout` traces should go behind a debug flag before any
  further testing.

## Testing

Build: the usual configure line plus `--enable-experimental`. Run TeXmacs
with a window and look at the terminal; for a scripted run, load a file with
`-x`, edit after a pause (`delayed (:pause 4000) ...`) and quit. Counting
`Evaluate`, `Memorized` and `error (` in the output gives a quick measure.
Use a separate `TEXMACS_HOME_PATH` so the runs write no preferences.
