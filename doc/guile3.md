# TeXmacs on Guile 3 (prototype, branch `wip_guile3`)

Status of 2026-10-05. The branch starts from `wip_fixes` and builds both
with Guile 1.8 (as before) and with Guile 3.0.11. With Guile 3 the Scheme
code of TeXmacs runs **interpreted** (no auto-compilation). Guile 2/3 code
is behind `#ifdef GUILE_D` in C++ and `cond-expand (guile-2 ...)` in Scheme.

## How to build and run

Guile 3 (Homebrew, macOS):

    cd src
    ./configure --with-guile=/opt/homebrew/bin/guile-config --enable-guile2
    make -j8
    TEXMACS_PATH=$PWD/TeXmacs TEXMACS_HOME_PATH=/some/scratch/home \
      TeXmacs/bin/texmacs.bin

`--enable-guile2` is required by `misc/m4/guile.m4` for any Guile 2/3
(an existing safety switch of configure). Configure is unchanged
otherwise; add the usual flags (e.g. `LDFLAGS`/`CPPFLAGS` for libpng) to
get the native PDF renderer. `GUILE_AUTO_COMPILE=0` need not be set:
TeXmacs sets it before Guile boots. CMake already selected `GUILE_D` for
Guile >= 2.0 (`pkg_search_module (Guile ... guile-3.0 ...)`), so the
upstream commit "CMake: support Guile 3" (20b585be01, which edits a
`FindGuile.cmake` we do not have) was not needed.

Guile 1.8: as before (`./configure --with-guile=.../guile-1.8.7/usr/bin/guile-config`).

## What was done

1. `git merge origin/guile3`: the Guile 2/3 port of 2020-2021 (C++ glue in
   `src/src/Scheme/Guile/guile_tm.{cpp,hpp}`, `guile.m4`, kernel Scheme:
   `boot.scm`, `tm-define`, `tm-convert`, `eval-when`, `tm-cond-expand`, ...).
   Conflicts resolved by keeping the `wip_fixes` code and re-applying the
   Guile 2/3 adaptation on top (details in the merge commit).
2. From texmacs/texmacs#72 (hammerfunctor), with authorship: Guile 3's own
   `compose`, `define-collection` without `define` in expression context,
   `compose` not exported on Guile 3, a `define` inside `if` in tmimage.scm.
3. C++ (`guile_tm.cpp`, behind `GUILE_D`):
   - `GUILE_AUTO_COMPILE=0` is set before `scm_boot_guile`.
   - Strings: the peeking at libguile internals (`SCM_I_STRINGBUF_F_WIDE`)
     is gone. TeXmacs bytes go to Guile as Latin-1 characters
     (`scm_from_latin1_stringn`); back, the string is read with
     `scm_to_utf8_stringn`: if all its characters are Latin-1 they are
     the TeXmacs bytes again, otherwise the string is Unicode text and is
     converted with `utf8_to_cork`. Symbols use the same Latin-1
     convention (`scm_from_latin1_symboln`) instead of the locale.
   - **Source files are read as Latin-1**: Guile 2/3 read source files as
     UTF-8 (hard-coded, independent of the locale), so a string literal
     with non-ASCII bytes did not hold the bytes of the file as with
     Guile 1.8 (51 TeXmacs .scm files have such literals: keyboard maps,
     LaTeX/Coq converters, ...). `primitive-load` and `primitive-load-path`
     are redefined to read the files of TeXmacs (not those of Guile's own
     directories) as Latin-1, form by form; `eval_scheme_file` goes
     through them. The standard output ports use Latin-1 too.
4. Scheme, by class of problem (all portable, Guile 1.8 unaffected unless
   said otherwise):
   - **Eager macro expansion.** Guile 1.8 expanded a macro when the code
     first ran; Guile 2/3 expand a whole top-level form when it is
     evaluated. Consequences fixed:
     - a macro used above its definition (`with-verify-delete-rights`
       in server-base.scm);
     - `inherit-modules` loaded all its modules before re-exporting any,
       so `ahash-table.scm` did not see `for`/`with` of `abbrevs.scm`:
       modules are now re-exported one by one;
     - `texmacs-module` expanded its options before the new module used
       `guile-user`, so `:inherit` was not a macro;
     - macros with side effects at expansion time (`lazy-format`,
       `lazy-input-converter`) registered their module even in the branch
       of a conditional which is not taken (Coq formats declared without
       Coq): they now register at run time;
     - errors which Guile 1.8 only raised when the code ran now break the
       loading of the module: an `if` with three branches
       (client-widgets.scm), an unquoted `()` (tmtex-ieee.scm), the
       deliberate invalid item of the debug menu (now raised when the menu
       is made).
   - **`eval-when (expand load eval)`** (from the guile3 branch, meant for
     the compiler) evaluates its body twice in the interpreter (once when
     expanding, once when evaluating): `define-group` listed every tag
     twice, `tm-define`s were overloaded twice. Replaced by
     `(eval-when (load eval) ...)` except in the idempotent module code
     of `boot.scm`.
   - **tm-define (guile3 rewrite)**: `tm-defined-name` (behind
     `procedure-name`) named the previous procedure; with Guile 1.8 this
     broke `procedure-name`, `procedure-sources`, `property` (synopsis,
     check marks) and `help`. Lazy-define stubs whose module does not
     define their name now raise "Could not retrieve" instead of calling
     themselves for ever (hung the kbd-menu tests on Guile 3; same fix
     for Guile 1.8).
   - **Procedure sources**: Guile 2/3 keep no source; `procedure-source`
     returns the `tm-source` property that `tm-define`, `tagged-lambda`
     and `lazy-define` set; `promise-source` copes with no source.
   - **Number printing**: Guile 3 prints `66.60000000000001` where Guile
     1.8 printed `66.6` (at most 15 significant digits). `number->string`
     keeps the Guile 1.8 behaviour (`display`/`write` of numbers do not).
   - **Hash table order** differs between Guile versions: HTML attributes
     now keep their order instead of going through a hash table, and
     `sxml-set-attrs` puts its attributes first in their order.
   - **Load order**: two `:use` additions of the guile3 branch (for
     compiler warnings) changed the order in which modules load and
     broke `(make 'math)` (mode stayed "text"), also with Guile 1.8; they
     were undone. `module-provide`, lost in the merge, is back.
   - Tests: `closure?` and the `arity` procedure property do not exist in
     Guile 2/3 (glue-test, kbd-menu-test use `primitive-code?` and
     `procedure-minimum-arity` there).

## Results

Regression suites (`run-regression-suite`, one process per suite,
headless, `QT_QPA_PLATFORM=offscreen`, private home), checks failed:

| suite        | 1.8 wip_fixes | 1.8 this branch | Guile 3 |
|--------------|---------------|-----------------|---------|
| 30 suites    | 0             | 0               | 0       |
| graphics-edit| 0             | 0               | 2 (hash order of `with` attributes) |
| tm-define    | 0             | 2               | 2 (lazy-define stub not a tm-define) |
| editing, math-edit, table, text-structure, plugins | crash | crash | crash |

The five crashing suites crash at the same place with all three builds
(segmentation fault in `qt_gui_rep::get_selection` / `connection_rep::start`
headless; not related to Guile), with no failure before the crash.
kbd-menu (656 checks) passes on all three. Integration tests
(`run-integration-tests`): the same 7 failures (3 of 4 suites) with all
three builds.

All 417 non-test modules of `TeXmacs/progs` and the 66 plugin modules
load with Guile 3; the 4 which fail (cyrillic keyboards, coq-kbd,
mupad-input: context-dependent predicates) fail the same way with 1.8.

Typesetting `doc/main/start/man-conventions.en.tm` to PDF gives the same
pixels with Guile 3 and Guile 1.8 (same configure).

Start-up, `texmacs.bin -headless -x '(quit-TeXmacs)'`, warm, macOS arm64:
Guile 1.8 wip_fixes 1.6-1.7 s; this branch on Guile 1.8 2.0-2.2 s;
Guile 3 interpreted 1.85-1.95 s.

## Remaining issues

- Hash-table order: code which writes the entries of a hash table in
  documents (`with` attributes of new graphics in graphics-utils.scm /
  graphics-object.scm, `graphics-all-attributes`) produces another order
  on Guile 3. Making these orders deterministic would also change the
  output of Guile 1.8: a decision for the maintainer.
- The guile3 rewrite of tm-define / lazy-define (also used with Guile 1.8)
  changes the bookkeeping: lazy stubs are no longer tm-defines (2 checks of
  the tm-define suite), and a conditional "master" routine defined before
  the unconditional one now warns at start-up ("conditional master routine
  focus-hidden-menu"): `generic-menu` loads `graphics-menu`, whose
  overloads are then replaced by the master of `generic-menu`.
- This branch is slower to start with Guile 1.8 than wip_fixes (about
  +0.3 s); not investigated.
- `display`/`write` of inexact numbers print up to 17 digits on Guile 3.
- File names: TeXmacs passes file names to Guile as Latin-1 strings, which
  Guile encodes with the locale when it opens files: non-ASCII paths are
  likely broken on Guile 3 (not tested).
- Warning at start-up: "imported module (kernel boot abbrevs) overrides
  core binding `...'".
- Compiled mode is not supported (TeXmacs files do not compile); with
  `GUILE_AUTO_COMPILE=0` forced, nothing is cached in `~/.cache`.
- Not tested: GUI sessions, plugins with external programs, Windows,
  Guile 2.x.
