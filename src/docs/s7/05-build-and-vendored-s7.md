# 5. Build, the vendored s7, and the branch

## 5.1 Choosing the interpreter

The interpreter is a build option, and **s7 is the default**. An s7 build
needs no Guile at all: nothing to install and nothing linked, and the glue
is regenerated with s7 too.

| Build system | s7 (default) | Guile |
|---|---|---|
| autotools | `./configure` or `./configure --with-scheme=s7` | `./configure --with-scheme=guile` |
| CMake | `-DSCHEME_IMPL=s7` | `-DSCHEME_IMPL=embedded18` (the embedded Guile of `tm-guile188`) or `guile` (a system Guile, found with pkg-config) |

The option drives everything else:

- **Macros:** it defines `USE_S7` or `USE_GUILE` in `config.h` (from
  `misc/m4/scheme.m4` and `config.h.cmake`).
- **Sources:** it picks the backend directory, `SCHEME_DIR=S7` or `Guile`.
  `src/makefile.in` compiles `src/Scheme/{Scheme,$(SCHEME_DIR)}`, and CMake
  globs the same directory.
- **C++:** `object.hpp` includes `s7_tm.hpp` or `guile_tm.hpp`, and the
  backend names its init file, `init-s7.scm` or `init-guile.scm` (§2.1).
- **Guile detection:** `LC_GUILE` (detection, flags, `-lguile`) runs only
  for Guile.
- **Platform code:** the Guile hooks in the Unix, Windows64 and Android
  files are compiled only with `USE_GUILE`.
- **Packaging:** the rules in the top-level `Makefile.in` copy Guile's
  `ice-9` directory only when there is one.

**Both interpreters build, boot and pass the tests.**
- **s7:** checked on Linux, macOS and Windows by CI (§5.4).
- **Guile:** checked with autotools on macOS, with Guile 1.8.7. Guile runs
  all the regression suites except the two that test s7 specifically.

**CMake.** The interpreter option was redone on upstream's CMake files of
September 2026, which set `SCHEME_DIR` and `USE_S7`/`USE_GUILE` from
`SCHEME_IMPL`. With `SCHEME_IMPL=s7`, CMake configures and the whole tree
compiles, including `s7.c`. On macOS the link still fails, for two upstream
reasons that don't depend on the Scheme choice:
- no platform sources are compiled on macOS: the `APPLE` branch of the OS
  sources is empty, so the functions of `src/Plugins/Unix` are missing;
- `gnutls` is linked by name without its library directory.

The man page target also expects `misc/man/texmacs.1`, which only
`configure` generates. CI uses autotools.

**Xcode.** `packages/macos/TeXmacs.xcodeproj` compiles `s7_tm.cpp` and
`s7.c` in three targets.

<a id="glue-regeneration"></a>
### Glue regeneration

The generated `glue_*.cpp` files work with either interpreter, and so do
the generators (`build-glue.scm`, `make-apidoc-*.scm`).

- **On s7,** `make -C src GLUE` first builds `Objects/s7-run`: a small
  command-line s7, built from `src/Scheme/Glue/s7-run.c` and the vendored
  `s7.c`, that accepts Guile's `-l FILE -c EXPR` options. Its output is
  byte-identical to the committed files.
- **Failures don't truncate the glue.** `build-glue` and `build-auto-doc`
  write to a temporary file and replace their output only when the
  generator succeeds.

### After a rebase

- **Run `make -C src clean` before building.** Upstream changes function
  signatures in headers, and stale objects then fail at link time.
- **Untracked files are expected.** `configure` also generates a few
  (`packages/msix/*.xml`, `packages/android/res/values/`).
- **`configure` was regenerated with Autoconf 2.73**; the previous one was
  made with 2.72. Most of its diff is version noise.

<a id="s7-version-and-local-patch"></a>
## 5.2 The vendored s7

`src/Scheme/S7/s7.c` and `s7.h` are **s7 11.9 (21-Sep-2026) as released**
(`https://ccrma.stanford.edu/software/s7/s7.tar.gz`), with five local
patches (below). The patches are kept in `src/Scheme/S7/patches/`, made
one after the other from stock s7, with a description at the top of each
and a `README.md`; the changed code is marked `TeXmacs:` in `s7.c`.
`mus-config.h` is an empty placeholder that `s7.c` includes.

- **Compiled as C.** s7 11.9 no longer compiles as C++: in C++ mode it
  disables complex numbers, and its stub `clog` then becomes ambiguous under
  clang++. `src/makefile.in` compiles `src/Scheme/S7/*.c` with the C
  compiler (`cc_incl`), as CMake does.
- **Default options.** No `-DWITH_*` flags are set, so s7 builds with its
  defaults: `WITH_GMP 0`, `WITH_PURE_S7 0`, `WITH_SYSTEM_EXTRAS 1`,
  `WITH_HISTORY 0`, `WITH_WARNINGS 0`, `WITH_MAIN 0`.
- **Runtime settings,** made in `start_scheme` (§1.3):
  `(*s7* 'symbol-quote?)` is `#t`, `(*s7* 'cache-macro-expansions?)` is `#t`
  (patch 0005), and the initial heap is 1 M cells.

**s7 11 behaviors that TeXmacs adapts to** (see §2.1–2.3 and
[03](03-compat-layer.md)):
- the reading of `'x`;
- `varlet` refusing already-bound symbols;
- read-time expansions, which TeXmacs no longer uses;
- a `load` during an expansion ending the outer load;
- internal definitions in macro bodies;
- how lookups use let ids.

**Patch 0001, in the printer.** `string_to_port` writes a string of
more than 1000 copies of one character as `(make-string n c)`, in every
mode, `display` and `write` included. TeXmacs writes trees as Scheme data
(`object->string`, `save-object`, the tree cache, the client/server
protocol) and reads them back with `read`, which gives a list instead of the
string. The patch keeps the abbreviation in readable mode only, where it
evaluates back to the string.

**Patch 0002, in the optimizer.** A call site optimized for a closure
records it (`opt1_lambda`), and the optimizer annotates that closure's body
for the fast paths (`fx_annotate_arg` for `op_safe_closure_p_a`).
`closure_is_ok_1`, `closure_is_fine_1` and `closure_star_is_fine_1` then
accepted any other closure of the same type and arity at that call site,
whose body did not have the annotations. A local function remade from the
same source has the same body, so this never mattered in ordinary code, but
a closure made from new code each time does not: a `lambda` built by a
run-time macro, as when `define` was a macro for curried definitions.
`op_safe_closure_p_a_1` then called a null `fx` function, which crashed
graphics-edit (§3.1). The patch (`closure_has_same_body`) also requires the
same body; otherwise the call site goes back to the general path. Ordinary
code is not slower, and s7's own `s7test.scm` gives the same output with and
without the patch. Reported upstream; the reproducer is in the patch.

**Patch 0003, curried `define`.** Guile's `define` accepts curried heads,
`(define ((f a) b) . body)` for `(define (f a) (lambda (b) . body))` (also
SRFI 219), and the TeXmacs code uses them. `check_define` rewrites such a
form in place the first time it checks it, so what is evaluated and
optimized afterwards is an ordinary definition, at no cost per call.
`define*` is left alone. `define-public` (`boot-s7.scm`) takes the name
from the innermost head. Before the patch, `define` in the user module was a
run-time macro: it was expanded again each time an internal definition ran,
and it triggered the crash of patch 0002.

**Patch 0004, a fix from upstream.** In the `p_pi` case of its tree
rewriting, the optimizer replaced `string_ref_p_pi` by `string_ref_p_p0`, a
`p_pp` function which `fx_c_ti_direct` then called as `p_pi`. Native code
does not mind; WebAssembly traps ("indirect call signature mismatch"), on
`(string-ref a 0)` where `a` is a parameter. s7 5-Oct-2026 has the same fix,
so the patch goes with the next upgrade.

**Patch 0005, caching macro expansions.** s7 expands a macro call each time
it is evaluated; Guile 1.8 expands it once and keeps the expansion, and the
TeXmacs code was written for that. The patch adds an `*s7*` field,
`cache-macro-expansions?` (`#f` by default, so stock behavior is
unchanged), which `start_scheme` sets to `#t`.
- A call evaluated again reuses its expansion if its macro is still the
  same one. The key is the argument list of the call, which is the same
  list each time the same code runs, in a weak `eq?` hash table.
- Only calls from code (`op_macro_d`) are cached: a macro applied as a
  function (`apply`, `for-each`, `sort!`) can get a list which s7 reuses
  with other contents. Bacros, calls without arguments and expansions
  returning several values are not cached.
- The optimizer treats an expansion as code evaluated once and leaves
  information there which holds only for that evaluation: reusing the
  same pairs made a named let in the expansion of a loop macro see the
  variables of the first call. So the pairs which the macro made (not
  those of its arguments, which are the caller's code) are copied each time
  the expansion is used.
- `s7test.scm` gives the same results with the field on and off. In
  TeXmacs, the warm LaTeX exports of the change log are 13–20% faster; the
  editing suites gain about 5%, within the noise of a loaded machine. Less
  than the time the macros took (§6.2) because the copied pairs are still
  optimized again at each use.

**No patch of the lookups.** Earlier versions of the port patched s7's symbol
lookup:
- first by moving found slots to the front of their let, which was unsound,
  because it reordered lets that s7 iterates over or refills by position;
- then with an id check.

Both patches were needed only because TeXmacs copied every export into one
huge user environment. Since public definitions are published in the rootlet
(§2.2–2.3), stock s7 is as fast as the patched one. The tests of the
`lookup` group in `boot-s7-test.scm` still check the two properties the
first patch broke.

### Upgrading s7

1. Copy the new `s7.c` and `s7.h` into `src/Scheme/S7` and apply the
   patches in order, as `src/Scheme/S7/patches/README.md` says. Drop a patch
   which upstream has made unnecessary, and refresh one which no longer
   applies. The `patches` group of `boot-s7-test.scm` tests all five.
2. Rebuild from clean.
3. Run `run-all-tests` and the portable suites on both interpreters (§4.4).
4. Check the timings of [07](07-performance.md), at least boot and the LaTeX
   export loop. The module system relies on how s7 caches lookups (§2.3), so
   a change there would show up as a slowdown, not as a failure.

## 5.3 The branch

Development happens on the branch **`wip_s7` of
[mgubi/texmacs](https://github.com/mgubi/texmacs)**. In that repository the
TeXmacs source tree is the `src` directory, so paths in these notes are
relative to `src/`. The CI configuration (`.github/`) is at the root of the
repository.

The branch is `svn_sync` of that repository, as of 2026-09-24 (`8629ced4f7`),
plus a linear series:

1. **`ae6b005002` "S7 Scheme support (squashed from wip_s7)":** the whole
   original port as one commit.
2. **Fixes and new work,** each in its own commit:
   - bug fixes, the update to s7 11.9 and the tests;
   - the build option and the kernel shared with Guile;
   - the module-system and lookup work;
   - the init files, the `latex-needs?` cache and the HTML export fixes;
   - CI.
3. **Commits whose subject starts with `docs/s7:`,** which only touch
   these notes.

**Where the series comes from.** It was developed on `wip_s7` of
`texmacs/texmacs`, whose tree is this `src/` directory, on top of
`svn_sync_20260921`. It was moved here on 2026-09-27 by replaying every
commit under `src/` (`git format-patch`, then `git am --directory=src`).
- The only conflicts were in the CMake files, which upstream had rewritten
  in the meantime. The interpreter option was redone on the new files in a
  separate commit.
- The original history, with all commits and authors, is on the branch
  `wip_s7_pre_rebase_20260924` of `texmacs/texmacs` (`dd11d3310a`).

In short, that history is:
- the port was written in 2020–2022 by Massimiliano Gubinelli;
- it was imported into the TeXmacs repository by Darcy Shen (沈达) in
  November 2021, with CMake support and fixes;
- s7 was updated in January 2022;
- the branch was rebased onto upstream in July 2025, and again in
  September 2026.

**To rebase onto a later snapshot**, run
`git rebase --onto <new-svn_sync> <old-svn_sync>`. Then:

1. **Merge upstream's changes to the init files.** Upstream edits
   `init-texmacs.scm`, which is split here into four files:
   - changes to its start belong in `init-guile.scm`, and possibly in
     `init-s7.scm`;
   - changes to the kernel imports belong in `init-kernel.scm`;
   - the rest stays in `init-texmacs.scm`.
2. **Check new upstream Scheme code for Guile-only builtins,** such as
   SRFI-13/14 functions, `(ice-9 …)` modules or `procedure-property`. Add
   what's missing to `compat-s7.scm`.
3. **Rebuild from clean and run the tests on both interpreters.**

<a id="ci"></a>
## 5.4 Continuous integration

`.github/workflows/ci.yml`, at the root of the repository, runs on every
push to `wip_s7`, on pull requests to `wip_s7`, and by hand from the Actions
tab. Its jobs run in `src/`.
It has one job per platform, each limited to 60 minutes. Each job:
1. installs the dependencies;
2. configures with `--with-scheme=s7`;
3. builds;
4. runs the regression suites headless, with
   `../.github/scripts/run-tests.sh`;
5. uploads a runnable bundle, kept for 14 days.

| Platform | Runner and dependencies | Artifact |
|---|---|---|
| Linux | Ubuntu 24.04, Qt 6 and libraries from apt | `texmacs-linux-x86_64`: a tarball of `TeXmacs/` |
| macOS | macOS 14 (Apple silicon), Qt 6 and libraries from Homebrew | `texmacs-macos-arm64`: the zipped `TeXmacs.app` (unsigned) |
| Windows | MSYS2 MinGW-w64, Qt 6 and libraries from MSYS2 | `texmacs-windows-x86_64`: the zipped `WINDOWS_BUNDLE` folder |

A full run takes about 15 minutes for Linux and macOS, and 20–25 minutes for
Windows.

**Platform details:**
- **Tests.** TeXmacs' exit status doesn't reflect test failures, so
  `run-tests.sh` runs `run-all-tests` inside a `catch` and checks for a
  marker line. A test script that fails to load quits at once, with a
  timeout as a last resort.
- **macOS.** `packages/macos/bundle-libs.sh` knows the library locations
  of MacPorts, Fink and `/usr/local`, but not Homebrew's `/opt/homebrew`.
  The job therefore runs `make MACOS_BUNDLE MACOS_DEPLOY=none`, which skips
  that script, and deploys with Qt's `macdeployqt`.
- **Windows.**
  - Qt is found with `--with-qt-find-method=pkgconfig`. With qmake, Qt
    defines `QT_NEEDS_QMAIN`, which `windows64_entrypoint.cpp` refuses.
  - `moc`, `uic` and `rcc` are in `/mingw64/share/qt6/bin`, which goes on
    the `PATH`.
  - Winsock is linked with `LIBS=-lws2_32`.
  - `WINDOWS_BUNDLE` gets `QT_PLUGINS_PATH=/mingw64/share/qt6/plugins`,
    so that the bundle contains `platforms/qwindows.dll`.

**Worth fixing upstream:**
- `configure` should add `ws2_32` itself for MinGW;
- the `#error` in `windows64_entrypoint.cpp` spells the option
  `pkg-config`, which `configure` rejects;
- `bundle-libs.sh` should learn Homebrew's paths.
