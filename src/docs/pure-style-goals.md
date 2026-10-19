# The style rewriter: Joris's design goals

Joris van der Hoeven started the style rewriter (`src/src/Style/`,
`--enable-experimental`) in January 2007 and never wrote a design document
for it. This note collects what his own texts and commits say it was for.
The state of the code and the review are in [pure-style.md](pure-style.md).

## Goals

1. **Separate evaluation from typesetting.** Typesetting does two things,
   evaluating the style language and laying out boxes, and he meant to
   split them. The rewriter is the first half on its own: it turns the
   document into a tree of typesetting primitives.
2. **A lazy, incremental typesetter.** The rewriter's output is meant to
   feed the typesetter. Every evaluation is memorized (the memorizers), so
   after an edit only what changed is evaluated again, and so typeset again.
3. **Keep the memorization local.** Two things would make one edit
   invalidate far more than it touches:
   - Variables that markup changes as a side effect (`the-tags`,
     `the-label`) change the environment of everything after them.
   - Macro arguments stored in the environment: an edit inside a long
     argument changes the environment of the whole macro body. Hence the
     *alternative macro expansion* (`ALTERNATIVE_MACRO_EXPANSION`, the
     default), which substitutes the arguments into the body ("quasi-quoting")
     instead of binding them.
4. **Auxiliary data on tree nodes.** Labels, loci, links and index entries
   should be attached to tree nodes rather than to files. Separate evaluation
   makes three kinds of hidden markup possible:
   - not evaluated, but its auxiliary data are kept (a hidden chapter);
   - evaluated but not typeset (a fold);
   - ignored altogether (a comment).
5. **Housekeeping.** Symbols (tree labels) instead of strings as keys.
   `base-file-name`, `cur-file-name` and `current-part` become proper
   variables in `vars.hpp`. A rule for which locus a hyperlink prefers when
   several views show it.

The macro semantics stay as documented: an argument is bound to the
unevaluated expression and evaluated at each use, which substitution
preserves.

## Sources

- **To-do list**, `src/TeXmacs/doc/about/projects/todo.en.tm`:
  - *Improving the speed*: "Increased laziness. The typesetter should be made
    lazy in a more fundamental way (see upcoming style rewriting tool)", and
    "Better caching mechanism: optimize performance as a function of cost of
    recomputation and storage requirement" (goals 1–2).
  - Section *Upcoming style rewriting tool* (anchor `style-rew-sec`):
    the reminders on implicitly changed variables, `base-file-name` and the
    others, replacing `arg` by quasi-quoting, symbols, links, and the
    paragraph on auxiliary data in hidden markup (goals 3–5).
- **The typesetting process**,
  `src/TeXmacs/doc/devel/format/basics/typesetting.en.tm` (2004): "The global
  typesetting process can be subdivided into two major parts (which are
  currently done at the same stage, but this may change in the future):
  evaluation of the TeXmacs tree using the stylesheet language, and the
  actual typesetting" (goal 1).
- **Macro expansion**, `src/TeXmacs/doc/devel/style/design/style-eval.en.tm`:
  arguments bound to unevaluated expressions, like a Scheme macro wrapping
  each argument in a thunk.
- **Code comments**: `src/src/Style/Environment/environment.hpp` (the two
  expansion modes); `memorizer.hpp`, "memorizing computations during style
  rewriting".
- **Commits**:
  - e8e500db55 (2007-01-10), "Start new style rewriting mechanism (use
    --enable-experimental to test)";
  - a03447880e (2007-01-11), "Tree rewriting before evaluation, inactive
    markup and alternative macro expansion";
  - be7a72a74b (2007-01-13), "Fix for macros as arguments (alternative
    macro expansion)";
  - later upkeep, showing the project was paused rather than dropped:
    bdf8ecd83b `blend` (2015), 2bb12ffdb1 `with-package` (2017),
    6dc655a3f3 `provide` and `or-value`, "provisory" (2019), 1018a2954c
    the `paw` unit (2020).

The pages `src/TeXmacs/doc/devel/source/rewriter-*.en.tm` are recent
developer documentation (2026), not Joris's.
