# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GNU TeXmacs is a free, cross-platform scientific document editor providing a "WYSIWYW" (What You See Is What You Want) editing environment. The codebase combines C++17 core functionality with Scheme/Guile scripting and Qt for the GUI.

## Build Commands

### Primary Build System (Autotools)
Run from `src/`. Guile 1.8 must come first in `PATH` when configuring and building.
```bash
./configure ...       # once; ./config.status --recheck after configure/makefile.in change
make -j10             # dynamic TeXmacs (TeXmacs/bin/texmacs.bin)
make PLUGINS          # build plugins
make install          # install the application
```
Header dependencies are computed during compilation (`-MMD`, files in `src/Deps`), so changing a header rebuilds what includes it. An object file copied from another tree is not checked against its sources: after copying build products, `touch` the sources that differ.

### Alternative Build System (CMake)
```bash
mkdir build && cd build
cmake ..
make -j8
```

### Configuration Options
```bash
./configure --prefix=[directory]     # Custom install location
./configure --enable-debug           # Debug build
./configure --disable-optimize       # Disable optimizations
```

## Testing

All runners are run from `src/`, after building, and exit non-zero on failure.

| Command | What it runs | Time |
|---|---|---|
| `tests/scheme/check.sh` | the regression suites of `TeXmacs/progs/check/check-master.scm`, in one TeXmacs process | about 100 s |
| `tests/scheme/check.sh <name>` | one suite of check-master (e.g. `latex`, `table`) | seconds |
| `tests/scheme/check.sh path/foo-test.scm` | a suite file not yet registered | seconds |
| `tests/scheme/check.sh integration` | the integration suites (server backup, cache...) | |
| `make -C tests` | the C++ unit tests (QtTest), linked against `src/Objects` | minutes |
| `tests/documents/check.sh [-u] [-p]` | typesets `tests/documents/samples/*.tm` to PDF and compares their text with `tests/documents/ref` (`-u` updates, `-p` also compares pixels) | about 1 min |
| `tests/docs/check.sh [-b\|-f] [-l lan] [-u]` | the whole documentation: the Help books to PDF and every `TeXmacs/doc` file; problems are compared with `tests/docs/ref/summary.txt` | about 9 min |

Logs are in `tests/build/<runner>/`. Some references depend on the machine's fonts (the `cjk` sample and the variable font part of `opentype-math` use macOS fonts).

### Writing a Scheme suite
- A suite is a module `TeXmacs/progs/check/<name>-test.scm` using `(check check-lib)` and defining `(<name>-test-failures)`: `(check-suite "name")`, groups with `(check-group "...")`, checks with `check=`, `check-true`, `check-false`, `check-error`, then `(check-end)`, which returns the number of failures. Wrap each group so that an error counts as one failure (see `run-group` in `plugins-test.scm` or `with-buffer-body` in `editing-test.scm`).
- Register it in `check-master.scm` (the `:use` list and `regression-suites`). The order matters: suites run in ONE process, so a suite must leave no state behind:
  - close every buffer it opens, including buffers renamed by `save-buffer-as` (compare `(buffer-list)` before and after);
  - restore every global it changes; never call `lazy-keyboard-force` or anything that loads all keyboard maps;
  - after `buffer-set-body`, run `(archive-state) (start-editing) (end-editing) (clear-undo-history)` before testing undo;
  - word completion uses all open buffers, and buffer indices change with the number of open buffers: do not depend on them;
  - files go under `(url-temp-dir)/<suite>`, removed at the start and at the end.
- Tests must never write preferences (no `set-preference`, no zoom or toggles that save them).
- A check that fails because of a bug in TeXmacs is left out, with a comment `;; FIXME: <what is wrong> (<file>:<line>): <repro> gives <x>, expected <y>.` When the bug is fixed, the FIXME is replaced by checks, which must fail without the fix (revert the fix, rebuild, run, re-apply).

## Running TeXmacs headless
- ALWAYS set `TEXMACS_HOME_PATH` to a scratch directory (the runners do, `TM_TEST_HOME` overrides theirs). Without it TeXmacs writes into the user's real `~/.TeXmacs` (settings, font database, caches). Never run `texmacs.bin` by hand without it, not even for a probe.
- `texmacs.bin -x '<expr>' -q`: an error in `-x` keeps TeXmacs running, so wrap the expression in `(catch #t ... )` and `(exit ...)`/`quit`, and use a timeout. `use-modules` and `lazy-define` are not allowed in `-x`; `(load "file.scm")` is.
- Without a window: nothing is typeset until `update-forced`; `delayed` and idle work never run; auxiliary data needs `generate-all-aux` (see `tests/documents/export.scm`); page numbers are `?` unless the page medium is paper; modifier key presses and box-based cursor moves do not work.
- One process gets slower over many loaded documents (leaked views, #61); restart it every few hundred documents.

## Known pitfalls
- `.tm` files are Cork-encoded: never write raw UTF-8 into them; use `\<name\>` or `\<#xxxx\>` (never a bare `<#xxxx>`), or the Cork bytes already used in the file. In Scheme tests, build Cork characters with `(integer->char n)`.
- The first run in an empty home fails 4 `latex` equation checks (#229): warm the home with `tests/scheme/check.sh latex` first.
- Some `plugins` checks depend on timing (pipes, python sessions) and fail under machine load; rerun before suspecting a change.
- A focus event for the widget of a closed view crashed TeXmacs while a bibliography or aux data was generated (#174): suites that generate aux data used to run before the ones that close views.

## Contributing workflow (mgubi/texmacs fork)
- Bugs are filed as issues on `mgubi/texmacs` (title "Area: a, b, c", numbered items with file:line and a repro). Before filing or fixing, search the open issues AND the open PRs (by words and by changed files): many bugs already have a fix PR.
- Fix PRs target `wip_fixes` (not `svn_sync`), one PR per issue, each with its tests (the FIXME replaced by checks), "Fixes #N" or "Part of #N (items ...)" in the commit message.
- Work in a separate worktree per PR (e.g. `~/t/lab/fix/<branch>`). To avoid a full build, copy the build products of a built worktree with APFS clones (`cp -c`: `src/Objects`, `src/Deps`, `TeXmacs/bin`, `TeXmacs/lib`, `TeXmacs/plugins`, the generated makefiles and config headers, untracked generated files), fix the paths in the generated makefiles, then `touch` the sources that differ between the two trees and `make`.
- Commit only your own files: the mode changes of `src/plugins/powershell` and `src/plugins/scilab` that git shows in fresh worktrees are not part of any change. Never run `git config` (shared configuration) and never use a bare `git stash`.

## Architecture

### Core Source Structure (`src/`)
- **`Kernel/`** - Core data structures, containers, types, and abstractions
- **`System/`** - System-level functionality (files, networking, boot, language support)
- **`Graphics/`** - Rendering engine, fonts, colors, mathematics, GUI abstractions
- **`Typeset/`** - Document typesetting and box-based layout system
- **`Edit/`** - Editor functionality, interface, modification, and process handling
- **`Style/`** - Document styling and evaluation system
- **`Texmacs/`** - Main application logic, server, and window management
- **`Scheme/`** - Scheme/Guile integration and language bindings
- **`Data/`** - Data conversion, parsing, and observer patterns
- **`Plugins/`** - Platform-specific implementations (Qt, Unix)

### Application Structure (`TeXmacs/`)
- **`progs/`** - Scheme programs and extensions (`progs/check/`: the test suites)
- **`styles/`**, **`packages/`** - Document style definitions and packages
- **`plugins/`** - Built copy of `src/plugins` (edit the sources in `src/plugins`)
- **`doc/`** - The documentation (tmdoc), checked by `tests/docs/check.sh`
- **`fonts/`** - Font files and definitions

### Key Architectural Patterns
1. **Modular Design**: Clear layered separation (kernel → system → graphics → typeset → editor)
2. **Plugin Architecture**: Extensible system supporting Computer Algebra Systems, Programming Languages, Graphics tools
3. **Dual Language Core**: C++ for performance-critical components, Scheme for high-level logic and extensions
4. **Document Object Model**: Tree-based document representation with observer patterns
5. **Cross-platform Abstraction**: Platform-specific code isolated in plugins directory

## Development Notes

- **Environment**: Set `TEXMACS_PATH` (and, for anything automated, `TEXMACS_HOME_PATH`) for runtime
- **Dependencies**: Requires Guile Scheme 1.8, FreeType 2, libiconv; optional aspell/hunspell, ImageMagick, mutool (document tests)
- **Version Control**: Primary SVN repository on Savannah, GitHub mirror for visibility
- **Plugin Integration**: New plugins follow established patterns in `plugins/` directory
