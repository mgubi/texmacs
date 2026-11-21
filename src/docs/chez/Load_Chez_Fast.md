# Fast Boot & Memory Footprint Findings (`wip-chez`)

## 1. Overview of Startup Mechanisms

TeXmacs on Chez Scheme keeps **only two startup options** (the intermediate Approach 1 consolidated `texmacs.so` path has been phased out to keep the bootstrap pipeline simple):

1. **Normal Init from Individual `.so` Files (`init-texmacs-chez.scm`, `TEXMACS_NO_BOOT=1` or when no `texmacs.boot` is present)** — *for debugging and module development*: Loads core modules at startup via `use-modules` and resolves remaining subsystems on demand through `module-provide` (`load-module-file` $\to$ `primitive-load` on individual `.so` files using `module-deps.scm`).
   - Loading *all* 453 `.so` modules sequentially via `primitive-load` requires 454 filesystem `open`/`read`/`close` syscalls, **1,493 ms** wall time (**1,258 ms** CPU, **137 ms** GC), and **105.7 MB** (`105,744,384` bytes) of transient Scheme heap allocations.
2. **Fast Boot Option (`texmacs.boot` + `init-texmacs-boot-file.scm`)** — *default fast startup*: Built via `make-boot-file` + `vfasl-convert-file` (`src/Scheme/Chez/make-boot.scm`) over the curated 178-module startup set and registered via `Sregister_boot_file()` prior to `Sbuild_heap()`, while remaining non-resident `.so` modules lazy-load on demand.
   - Enabled by static C foreign registration (`initialize_glue_chez()` via `Sforeign_symbol` in `custom_init()` + `foreign-procedure` bindings in `TeXmacs/progs/kernel/boot/glue-chez.scm`).
   - `Sbuild_heap()` is deferred to `initialize_scheme()` inside `tm_server_rep::tm_server_rep()` so `the_server` is live during boot-file evaluation.
   - Runtime procedure handles are cached via `chez_cache_runtime_procedures` at the end of `boot-chez.scm`.
   - `module-provide` remains inert during `Sbuild_heap()` (`(chez-heap-built?)` returns `#f`) so resident modules do not probe the filesystem while the heap is still constructing.

---

## 2. Empirical Startup & Memory Benchmarks

### 2.1 Chez Scheme Startup Configurations (macOS arm64, Apple M1 Pro VM)

#### Current Two-Mode Comparison (`wip-chez`, Qt6, clean `tmp-home`, `QT_QPA_PLATFORM=offscreen`)

Measured on an **Apple M1 Pro virtual machine** (macOS arm64) over 3 runs per configuration with `/usr/bin/time -l` after compiling all 453 `.so` modules, compiling `init-texmacs-boot-file.scm` $\to$ `init-texmacs-boot-file.so` (`466 KB`, with 77 redundant resident `use-modules`/`lazy-*` forms removed and direct `(import ...)`), and regenerating VFASL `texmacs.boot` (`16.69 MiB` / `17,501,531` bytes, 178 modules total = `boot-chez.so` + 177 app modules):

| Bootstrap Configuration | Modules Loaded | `TIME-SERVER-INIT` | `TIME-OPEN-WINDOW` | `TIME-PRE-GUI-LOOP` | Chez Heap (`current-memory-bytes`) | Max RSS | Peak Memory Footprint |
| :--- | :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| **Fast Boot (`texmacs.boot` VFASL + `init-texmacs-boot-file.so`, 50 ms idle)** | **177** app (+0 disk `.so`) | **332 ms** avg *(308–379)* | **317 ms** avg *(307–330)* | **650 ms** avg *(616–709)* | **171.2 MB** *(163.30 MiB)* | **292.5 MB** *(278.99 MiB)* | **238.3 MB** *(227.24 MiB)* |
| **Fast Boot (`texmacs.boot` VFASL + `init-texmacs-boot-file.so`, 300 ms idle)** | **226** app (+49 lazy `.so`) | **263 ms** avg *(242–301)* | **326 ms** avg *(317–332)* | **590 ms** avg *(575–619)* | **174.1 MB** *(166.03 MiB)* | **295.4 MB** *(281.70 MiB)* | **241.0 MB** *(229.82 MiB)* |
| *Fast Boot before compiling `init` (`init-texmacs-boot-file.scm`, 50 ms idle)* | *177 app* | *1,194 ms avg (1,155–1,267)* | *487 ms avg* | *1,682 ms avg* | *173.2 MB* | *292.3 MB* | *238.3 MB* |
| **Normal `.so` (`TEXMACS_NO_BOOT=1`, 50 ms idle)** | **172** app (all disk `.so`) | **1,266 ms** avg *(1,214–1,311)* | **541 ms** avg *(502–584)* | **1,809 ms** avg *(1,717–1,896)* | **180.6 MB** *(172.27 MiB)* | **302.1 MB** *(288.14 MiB)* | **248.0 MB** *(236.50 MiB)* |
| **Normal `.so` (`TEXMACS_NO_BOOT=1`, 300 ms idle)** | **221** app (all disk `.so`) | **1,288 ms** avg *(1,233–1,382)* | **536 ms** avg *(508–552)* | **1,825 ms** avg *(1,743–1,935)* | **185.5 MB** *(176.86 MiB)* | **307.9 MB** *(293.67 MiB)* | **253.7 MB** *(241.98 MiB)* |

*(Notes: Across all 6 runs of each mode prior to the idle timer, Fast Boot with compiled `init-texmacs-boot-file.so` averaged `TIME-SERVER-INIT = 297 ms`, `TIME-OPEN-WINDOW = 322 ms`, `TIME-PRE-GUI-LOOP = 620 ms` vs. `1,277 ms`, `539 ms`, and `1,817 ms` for Normal `.so` Init (**4.3× faster server init**, **2.9× faster `TIME-PRE-GUI-LOOP`**), with **-9.7 MB to -12.7 MB lower peak footprint** and **-9.4 MB to -11.4 MB smaller Chez heap** than Normal `.so` Init. `bytes-allocated` was `76.73 MiB` (`80.5 MB`) / `79.85 MiB` (`83.7 MB`) in Fast Boot vs. `50.80 MiB` / `52.46 MiB` in Normal `.so` Init; total `/usr/bin/time` wall time averaged `1.13 s` @ 50 ms and `1.32 s` @ 300 ms vs. `2.47 s` and `2.84 s` for Normal `.so` Init.)*

#### Historical Pre-Qt6 / Retired Approach 1 Baselines

| Bootstrap Configuration | Modules / Payload | `TIME-SERVER-INIT` | `TIME-PRE-GUI-LOOP` | Max RSS | Peak Memory Footprint | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **Full `texmacs.so` (FASL, retired)** | 452 modules (13.06 MB) | ~2,025 – 2,127 ms | ~2,090 – 2,202 ms | **274.2 MB** | **244.0 MB** | Runtime FASL deserialization + nursery GC churn |
| **Full `texmacs.so` (VFASL, retired)** | 452 modules (32.05 MB) | ~2,382 – 2,402 ms | ~3,020 – 3,140 ms | **498.8 MB** | **472.3 MB** | Counterproductive for `primitive-load`: +16,264 VM page faults/reclaims (+82% RSS) |
| **Full `texmacs.boot` (FASL)** | 454 modules (16.67 MB) | ~891 – 1,056 ms | ~851 – 1,154 ms | — | — | Loaded directly into static generation during `Sbuild_heap()` |
| **Full `texmacs.boot` (VFASL)** | 454 modules (33.37 MB) | **~708 – 807 ms** | **~802 – 880 ms** | — | — | Full-image baseline (prior build) |
| **Lazy `.so` Boot (`TEXMACS_NO_BOOT=1`, 50 ms idle, prior Qt5 build)** | **172 `.so` modules (5.83 MB)** | **832 ms** | **1,481 ms** | **192.3 MB** | **138.3 MB** | Prior build baseline before Qt6 & encoding updates |
| **Lazy `.so` Boot (`TEXMACS_NO_BOOT=1`, 300 ms idle + welcome doc, prior Qt5 build)** | **230 `.so` modules (7.12 MB)** | **1,013 ms** | **2,094 ms** | **235.8 MB** | **179.8 MB** | Includes +58 modules triggered by `new-welcome.en.tm` & 250 ms `lazy-keyboard` timer |

### 2.2 Cross-Engine Memory Baseline

From `mgubi-texmacs/src/docs/femtolisp/07-performance.md` (macOS arm64, offscreen):

| Engine | Cold Boot Peak Memory |
| :--- | :--- |
| **Guile 1.8.7** | **142 MB** |
| **Femtolisp** | **160 MB** |
| **s7 11.9** | **217 MB** |
| **Chez (prior Qt5 Lazy `.so`, 172 modules)** | **138.3 MB** peak footprint (**192.3 MB** Max RSS) |
| **Chez (current Qt6 Fast Boot VFASL, 178 modules @ 50 ms)** | **238.3 MB** peak footprint (**292.5 MB** Max RSS; Chez heap **171.2 MB**) |
| **Chez (current Qt6 Normal `.so`, 172 modules @ 50 ms)** | **248.0 MB** peak footprint (**302.1 MB** Max RSS; Chez heap **180.6 MB**) |
| **Chez (retired Full 454-module `texmacs.so` FASL)** | **244.0 MB** peak footprint (**274.2 MB** Max RSS) |

---

## 3. Why the Full-Boot Image Inflates Memory Footprint

1. **Permanent Static Generation Placement**:
   - In Chez Scheme, objects, procedures, and global tables allocated while evaluating `texmacs.boot` during `Sbuild_heap()` are placed into the **static generation**, which is never swept or reclaimed by garbage collection.
2. **84.6% of the Image Is Unused at Startup**:
   - `boot-order-full.scm` was captured by calling `force-all-lazy-initialization!` (forcing every `lazy-keyboard`, converter, `lazy-initialize`, plugin hook, and 169 `lazy-*` subsystems) and appending 76 remaining leaf modules from `compile-inventory.scm`.
   - Of the **453** compiled application modules (**13.10 MB** total `.so` size, ~11,710 definitions):
     - The **76** Stage-1 modules (`init-texmacs-chez.scm` top-level `use-modules` + deps) occupy only **2.00 MB** (**15.3%** of total `.so` bytes; 2.28 MB including `boot-chez.so`).
     - The remaining **377** modules occupy **11.10 MB** (**84.7%**).
   - Full boot pins heavy, rarely used subsystems permanently in RAM: document converters (`convert/latex/tmtex.so` alone is 362 KB; `tmhtml.so` is 143 KB; Coq, BibTeX, MathML, TMML), remote collaboration (`client/*`, `server/*`), GPG/wallet (`security/*`), multilingual speech packs (`*-speech-en/fr.so`), and **19 test suite modules (`*-test.so`)**.
   - Consistent with the finding in `femtolisp/08-lazy-bodies.md`, only ~16% of defined Scheme functions (1,425 of 8,773) are invoked during startup.
3. **Chez Heap Representation Multiplier**:
   - Chez Scheme stores strings as 32-bit UCS-4 scalar arrays (3–4× larger than 8-bit strings in Guile/Femtolisp/s7), alongside native ARM64 machine code, relocation tables, `tm-defined-table` ASTs, `procedure-source-table` entries, and DRD/logic tables.
4. **Multimethod Overload Chain Depth (`tm-define`)**:
   - Every conditioned `(tm-define (f ...) (:mode ...))` wraps the previous `former` closure. Eagerly loading all 454 modules chains inactive mode overloads onto hot global dispatchers even in plain text editing.

---

## 4. Architectural Conclusion: Essential `texmacs.boot` + Lazy `.so` Loading

**Yes — packing only the essential first-window modules into `texmacs.boot` and loading all other subsystems lazily from `.so` files is the optimal trade-off.**

- **Fast, zero-disk-probe startup**: Baking the essential first-window module set into `texmacs.boot` eliminates disk `.so` loading during startup while keeping cold boot under 1 second.
- **Substantially lower RAM footprint**: Excluding ~280–300 non-essential modules drops peak memory footprint by ~105 MB (-43%), bringing Chez in line with Guile (~142 MB).
- **Negligible lazy-load cost**: Unlike Guile 1.8 (which had to read, expand, and interpret raw `.scm` files on demand), Chez's `load-module-file` loads precompiled native `.so` files via `primitive-load`. At ~3 ms per module on average, loading a 1–6 module feature (e.g., LaTeX export, BibTeX, preferences) on first use takes ~3–20 ms.

### Key Invariants for a Partial `texmacs.boot`

1. **Keep `lazy-*` Stubs Active**: A partial boot image must execute the `lazy-define`, `lazy-menu`, `lazy-keyboard`, `lazy-format`, `lazy-tool`, and `lazy-tmfs-handler` registrations (as implemented in `init-texmacs-boot-file.scm`, unlike a full-image bootstrap that omits them).
2. **Mark Only Resident Modules in `*texmacs-modules-loaded*`**: Only modules actually packed into `texmacs.boot` may be marked `#t` in `*texmacs-modules-loaded*`, so `module-provide` resolves non-resident `.so` files on demand once `(chez-heap-built?)` is `#t`.
3. **Preserve `generic/generic-menu.so` Before `generic/document-menu.so`**: `generic-menu` defines the unconditioned `(standard-focus-icons t)` fallback, whereas `document-menu` adds the `(:require (tree-is-buffer? t))` overload. Because `generic-menu` is reached after `document-menu` in unforced window-open traces, `generic-menu.so` must explicitly precede `document-menu.so` in the boot manifest (matching `make-boot:patch-cyclic-boot-order` in `src/Scheme/Chez/make-boot.scm`).
4. **Watch the `lazy-keyboard` 250 ms Idle Timer (`kbd-define.scm:28-33`)**: `lazy-keyboard` currently registers `(delayed (:idle 250) (ahash-set! lazy-keyboard-done ',module #t) (module-provide ',module))`, which unconditionally forces *all* keyboard modules (including `prog-kbd`, `table-kbd`, `graphics-kbd`, `edu-kbd`, `bib-kbd`, `link-kbd`, `version-kbd`, `chinese`, etc.) 250 ms after startup, pulling in ~48 additional modules even when those modes are never entered. Relying strictly on mode-predicated `(lazy-keyboard-force)` keeps those subsystems lazy until needed.

---

## 5. Necessary Modules for Starting Up the First Buffer Window

Measured via live unforced GUI trace (`TEXMACS_NO_BOOT=1`, `QT_QPA_PLATFORM=offscreen`) across the three startup stages of a clean `no_name_1.tm` buffer window.

### Summary by Startup Stage

| Stage | Trigger | New Modules | Cumulative Modules | Cumulative `.so` Size |
| :--- | :--- | ---: | ---: | ---: |
| **Stage 0** | Engine pre-glue bootstrap (`boot-chez.so`) | 1 | 1 | 0.28 MB (287 KB) |
| **Stage 1** | `init-texmacs-chez.scm` top-level `use-modules` | 76 | 77 | 2.28 MB |
| **Stage 2** | `open_window()` $\to$ `init-buffer.scm` + `edit_interface_rep::resume()` | +76 | **153** | **5.21 MB** |
| **Stage 3** | First event-loop `update-menus` / `drd_update` (`:idle 50`) | +20 | **173** | **6.11 MB** |
| *(Stage 4 — Avoidable)* | `lazy-keyboard` `(:idle 250)` timer + first-install welcome doc | +58 | 231 | 7.40 MB |

*(Note: Module counts above include `boot-chez.so`; application `.so` counts without `boot-chez.so` are 76, 152, 172, and 230, with `.so` sizes 2.00 MB, 4.93 MB, 5.83 MB, and 7.12 MB.)*

---

### Stage 1: Server & Bootstrap Modules (76 modules + `boot-chez.so`, 2.00 MB)

Loaded unconditionally by `init-texmacs-chez.scm` before `TIME-SERVER-INIT`:

1. **Tier 1 Kernel (40 modules)**:
   - `kernel/boot/`: `chez-compat.so`, `compat.so`, `abbrevs.so`, `debug.so`, `srfi.so`, `ahash-table.so`, `prologue.so`
   - `kernel/library/`: `base.so`, `list.so`, `tree.so`, `content.so`, `patch.so`
   - `kernel/regexp/`: `regexp-match.so`, `regexp-select.so`
   - `kernel/logic/`: `logic-bind.so`, `logic-unify.so`, `logic-rules.so`, `logic-query.so`, `logic-data.so`
   - `kernel/texmacs/`: `tm-define.so`, `tm-preferences.so`, `tm-dialogue.so`, `tm-plugins.so`, `tm-modes.so`, `tm-secure.so`, `tm-convert.so`, `tm-language.so`, `tm-file-system.so`, `tm-states.so`
   - `kernel/gui/`: `gui-markup.so`, `menu-define.so`, `kbd-define.so`, `menu-widget.so`, `speech-define.so`, `kbd-handlers.so`, `menu-test.so`
   - `kernel/old-gui/`: `old-gui-widget.so`, `old-gui-factory.so`, `old-gui-form.so`, `old-gui-test.so`
2. **Core Utilities, Editor, Server & View (28 modules)**:
   - `utils/library/`: `cpp-wrap.so`, `tree.so`, `smart-table.so`, `length.so`, `cursor.so`, `ptrees.so`
   - `utils/base/environment.so`, `utils/edit/variants.so`, `utils/plugins/plugin-convert.so`, `utils/handwriting/handwriting.so`
   - `utils/misc/`: `markup-funcs.so`, `artwork.so`, `tooltip.so`
   - `convert/rewrite/tmtm-brackets.so`
   - `bibtex/`: `bib-utils.so`, `bib-complete.so`
   - `source/macro-search.so`
   - `generic/`: `generic-edit.so`, `document-style.so`, `document-edit.so`
   - `texmacs/texmacs/`: `tm-tools.so`, `tm-server.so`, `tm-view.so`, `tm-print.so`, `tm-files.so`
   - `texmacs/keyboard/config-kbd.so`, `texmacs/menus/main-menu.so`, `tools/ai/ai-batch.so`
3. **Font Rules (8 modules)**:
   - `fonts/`: `fonts-ec.so`, `fonts-adobe.so`, `fonts-x.so`, `fonts-math.so`, `fonts-foreign.so`, `fonts-misc.so`, `fonts-composite.so`, `fonts-truetype.so`

---

### Stage 2: First Buffer Creation & Window Resume (+76 modules; 152 app modules total, 4.93 MB)

Loaded synchronously inside `open_window()` (`src/Texmacs/Data/new_window.cpp`) $\to$ `init-buffer.scm` (`buffer-set-default-style`) and `edit_interface_rep::resume()` (`menu_main`, `menu_icons` 0..3, `side_tools`, `bottom_tools`):

1. **Buffer Style & Title Setup (2 modules)**:
   - `database/title-transform.so`, `database/title-markup.so`
2. **Active Keyboard Maps & Mode Editing Dependencies Forced by `kbd-find-inv-binding` $\to$ `(lazy-keyboard-force)` (`always?` and `in-text?` modes) (53 modules)**:
   - `utils/edit/`: `selections.so`, `auto-close.so`
   - `utils/library/process.so`, `utils/plugins/plugin-eval.so`, `utils/plugins/plugin-cmd.so`
   - `texmacs/keyboard/`: `prefix-kbd.so`, `latex-kbd.so`
   - `generic/`: `format-edit.so`, `embedded-edit.so`, `format-drd.so`, `format-geometry-edit.so`, `generic-kbd.so`, `generic-speech.so`, `generic-speech-en.so`, `generic-speech-fr.so`
   - `text/`: `text-drd.so`, `text-edit.so`, `text-kbd.so`, `text-speech.so`, `text-speech-en.so`, `text-speech-fr.so`, `text-structure.so`
   - `source/`: `source-drd.so`, `source-edit.so`, `source-kbd.so`
   - `math/`: `math-drd.so`, `math-edit.so`, `math-kbd.so`, `math-stats.so`, `math-speech.so`, `math-speech-en.so`, `math-speech-fr.so` *(pulled in via `:use` edges from `latex-kbd` / `generic-kbd` / `text-kbd` / speech modules)*
   - `table/table-edit.so`, `convert/tools/tmconcat.so`, `doc/help-funcs.so`
   - `dynamic/`: `dynamic-drd.so`, `fold-edit.so`, `fold-kbd.so`, `scripts-drd.so`, `scripts-edit.so`, `scripts-kbd.so`, `session-drd.so`, `session-edit.so`, `calc-drd.so`, `calc-edit.so`, `calc-table.so`, `calc-kbd.so`
   - `link/locus-edit.so`, `version/version-drd.so`, `version/version-edit.so`
   - `tools/spell/`: `spell-edit.so`, `spell-lantool.so`, `spell-kbd.so`
3. **Top-Level Menu Bar, Main/Mode/Focus Toolbars & Theme/Doc Menu Hooks (21 modules)**:
   - `texmacs/menus/`: `print-widgets.so`, `file-menu.so`, `developer-menu.so`, `view-widgets.so`, `view-menu.so`
   - `generic/`: `generic-menu.so`, `document-menu.so` *(note: `generic-menu` must precede `document-menu` in the final boot order)*, `insert-menu.so`
   - `text/text-menu.so`
   - `tools/theme/`: `theme-edit.so`, `theme-menu.so`
   - `convert/rewrite/init-rewrite.so`
   - `doc/`: `apidoc-collect.so`, `apidoc-funcs.so`, `apidoc-widgets.so`, `apidoc-markup.so`, `apidoc.so`
   - `prog/`: `scheme-tools.so`, `scheme-menu.so`
   - `language/natural.so`, `utils/misc/gui-utils.so`

*(Observation: Of the 76 Stage-2 modules, 53 are pulled in purely because `make-menu-entry-shortcut` calls `kbd-find-inv-binding` $\to$ `(lazy-keyboard-force)`, which forces all `always?` shortcuts—including `fold-kbd`, `scripts-kbd`, `calc-kbd`, `source-kbd`, `spell-kbd`, `generic-speech-{en,fr}`, and `text-speech-{en,fr}`—while constructing the initial menu bar.)*

---

### Stage 3: First Event-Loop `update-menus` Pass (+20 modules; 172 app modules total, 5.83 MB)

Loaded during the first idle `update-menus` / `drd_update` cycle after the window opens (`:idle 50`):

- **Database Toolbar/Menu Check (`with-database-tool?` / `db-menu`) (9 modules)**:
  - `database/`: `db-base.so`, `db-format.so`, `db-users.so`, `db-version.so`, `db-edit.so`, `db-convert.so`, `db-tmfs.so`, `db-widgets.so`, `db-menu.so`
- **Graphics & Animation DRD / Editing (`graphics-drd` / `animate-edit`) (10 modules)**:
  - `graphics/`: `graphics-drd.so`, `graphics-utils.so`, `graphics-env.so`, `graphics-object.so`, `graphics-main.so`, `graphics-single.so`, `graphics-group.so`, `graphics-animate.so`, `graphics-edit.so`
  - `dynamic/animate-edit.so`
- **Keyboard Query Helper (1 module)**:
  - `utils/misc/gui-keyboard.so`

---

### Stage 4 (Avoidable): Deferred `lazy-keyboard` Timer & Welcome Doc (+58 modules; 230 app modules total, 7.12 MB)

Triggered only if the process remains idle past 250 ms (`kbd-define.scm:30`) or opens the first-install `new-welcome.en.tm` help article:

- **Triggered by `new-welcome.en.tm` / Help Expansion (10 modules)**:
  - `language/std-symbols.so`, `language/std-math.so`, `doc/tmdoc.so`, `doc/tmdoc-markup.so`, `convert/images/init-images.so`, `convert/bibtex/init-bibtex.so`, `convert/html/init-html.so`, `convert/latex/init-latex.so`, `convert/tmml/init-tmml.so`, `prog/prog-format.so`
- **Triggered Unconditionally by `lazy-keyboard`'s `(:idle 250)` Timer (48 modules)**:
  - `utils/automate/`: `auto-edit.so`, `auto-build.so`, `auto-tmfs.so`, `auto-kbd.so`
  - `text/chinese/chinese.so`
  - `math/`: `math-sem-edit.so`, `math-adjust-en.so`, `math-adjust-fr.so`
  - `prog/`: `prog-edit.so`, `glue-symbols.so`, `scheme-autocomplete.so`, `scheme-edit.so`, `dot-edit.so`, `java-edit.so`, `scala-edit.so`, `cpp-edit.so`, `python-edit.so`, `fortran-edit.so`, `prog-kbd.so`; `utils/misc/tm-keywords.so`
  - `table/table-kbd.so`, `graphics/graphics-kbd.so`
  - `education/`: `edu-drd.so`, `edu-edit.so`, `edu-kbd.so`
  - `doc/`: `tmdoc-kbd.so`, `apidoc-kbd.so`
  - `convert/tools/output.so`, `convert/bibtex/bibtexout.so`
  - `convert/latex/`: `latex-command-drd.so`, `latex-symbol-drd.so`, `latex-texmacs-drd.so`, `latex-define.so`, `latex-overload.so`, `latex-drd.so`, `latex-tools.so`, `texout.so`
  - `database/`: `bib-db.so`, `bib-manage.so`, `bib-menu.so`, `bib-kbd.so`
  - `link/`: `link-edit.so`, `link-extern.so`, `link-navigate.so`, `link-extract.so`, `link-kbd.so`
  - `version/`: `version-compare.so`, `version-kbd.so`

### Summary Recommendation for `texmacs.boot` Scope

- **Active Two-Mode Design**:
  1. **Normal Debug Init (`TEXMACS_NO_BOOT=1`)**: Runs `init-texmacs-chez.scm` directly from individual `.so` files for debugging and hot module iteration.
  2. **Fast Boot (`texmacs.boot` + `init-texmacs-boot-file.scm`)**: Default startup using a **178-module** VFASL `texmacs.boot` (**16.69 MiB** / **17.50 MB**; `boot-chez.so` + 172 unforced startup modules + 5 top-level menu modules `edit-menu`, `embedded-menu`, `format-menu`, `help-menu`, `math-menu`). All remaining modules (~276 modules including export converters, `tmtex.so`, `tmhtml.so`, Coq, remote client/server, GPG/wallet, and regression tests) remain lazy on disk. Legacy Approach 1 (`texmacs.so`) generation has been removed.

---

## 6. How to Obtain the Module Loading Order

`TeXmacs/progs/init-texmacs-chez.scm` wraps `primitive-load` with `register-traced-so!`, which records every `.so` file in DFS completion order (dependencies before callers) into `*traced-so-files*`, and provides `(dump-boot-order-manifest! out-path)` to write `"kernel/boot/boot-chez.so"` followed by `(reverse *traced-so-files*)` to disk.

*(Use `TEXMACS_NO_BOOT=1` so `init-texmacs-chez.scm` runs and individual `.so` files load from `TEXMACS_BOOT_PATH`; add `TEXMACS_TRACE_VERBOSE=1` to print each `[TRACE-SO]` load as it completes.)*

### 6.1 Dump the Unforced First-Window Startup Order

Set `TEXMACS_DUMP_BOOT_ORDER` (or call `dump-boot-order-manifest!` once the window and idle timers have settled):

```zsh
TEXMACS_NO_BOOT=1 QT_QPA_PLATFORM=offscreen \
TEXMACS_PATH="$PWD/TeXmacs" \
TEXMACS_BOOT_PATH="$PWD/build-texmacos/boot" \
TEXMACS_HOME_PATH="$PWD/build-texmacos/tmp-home" \
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs \
  -x '(delayed (:idle 50) (dump-boot-order-manifest! "build-texmacos/boot/boot-order.scm") (quit-TeXmacs))'
```

*(Note: Setting `TEXMACS_TRACE_MODULES=1` or `TEXMACS_DUMP_BOOT_ORDER=/path/to/file.scm` in the environment dumps at the end of `init-texmacs-chez.scm`, before GUI window creation and `:idle` timers run; calling `dump-boot-order-manifest!` inside `(delayed (:idle 50) ...)` captures Stages 1–3.)*

### 6.2 Dump the Complete All-Modules Loading Order

To capture the full lazy-subsystem DFS loading order (previously triggered automatically in `init-texmacs-chez.scm` whenever `TEXMACS_TRACE_MODULES` or `TEXMACS_DUMP_BOOT_ORDER` was set), evaluate the following `force-all-lazy-initialization!` helper under `TEXMACS_NO_BOOT=1` before calling `dump-boot-order-manifest!`:

```scheme
(define (force-all-lazy-initialization!)
  (when (top-level-bound? 'lazy-keyboard-force)
    (lazy-keyboard-force #t))
  (when (top-level-bound? 'converters-from)
    (converters-from "texmacs")
    (converters-to "texmacs"))
  (when (top-level-bound? 'lazy-initialize-force)
    (lazy-initialize-force))
  (when (top-level-bound? 'lazy-plugin-force)
    (lazy-plugin-force))
  (for-each
    (lambda (m)
      (guard (e (else #f))
        (resolve-module m)))
    '(
      (kernel gui menu-convert)
      (utils library cursor)
      (utils edit variants)
      (utils cas cas-out)
      (utils plugins plugin-cmd)
      (utils test test-convert)
      (utils automate auto-tmfs)
      (utils misc gui-keyboard)
      (utils automate auto-kbd)
      (bibtex bib-complete)
      (bibtex bib-widgets)
      (texmacs texmacs tm-files)
      (texmacs keyboard prefix-kbd)
      (texmacs keyboard latex-kbd)
      (texmacs menus file-menu)
      (texmacs menus edit-menu)
      (texmacs menus view-menu)
      (texmacs menus tools-menu)
      (texmacs menus preferences-menu)
      (texmacs menus preferences-widgets)
      (texmacs menus preferences-tools)
      (generic generic-kbd)
      (generic generic-speech-en)
      (generic generic-speech-fr)
      (generic generic-menu)
      (generic format-menu)
      (generic document-menu)
      (generic document-part)
      (generic insert-menu)
      (generic document-edit)
      (generic generic-edit)
      (generic generic-doc)
      (generic search-widgets)
      (generic spell-widgets)
      (generic format-widgets)
      (generic pattern-selector)
      (generic document-widgets)
      (generic format-tools)
      (generic document-tools)
      (generic pattern-tools)
      (text text-kbd)
      (text text-speech-en)
      (text text-speech-fr)
      (text chinese chinese)
      (text text-menu)
      (text text-drd)
      (math math-kbd)
      (math math-sem-edit)
      (math math-speech-en)
      (math math-adjust-en)
      (math math-speech-fr)
      (math math-adjust-fr)
      (math math-menu)
      (math math-edit)
      (prog prog-format)
      (prog prog-kbd)
      (prog prog-menu)
      (source source-kbd)
      (source source-menu)
      (source macro-edit)
      (source macro-menu)
      (source macro-widgets)
      (source shortcut-edit)
      (source shortcut-widgets)
      (table table-kbd)
      (table table-menu)
      (table table-edit)
      (table table-widgets)
      (table table-tools)
      (graphics graphics-kbd)
      (graphics graphics-menu)
      (graphics graphics-object)
      (graphics graphics-utils)
      (graphics graphics-edit)
      (graphics graphics-main)
      (graphics graphics-markup)
      (language minimal)
      (language std-math)
      (language natural)
      (education edu-kbd)
      (education edu-menu)
      (dynamic fold-kbd)
      (dynamic scripts-kbd)
      (dynamic calc-kbd)
      (dynamic fold-menu)
      (dynamic session-menu)
      (dynamic scripts-menu)
      (dynamic calc-menu)
      (dynamic animate-menu)
      (dynamic fold-edit)
      (dynamic session-edit)
      (dynamic calc-edit)
      (dynamic scripts-plot)
      (doc tmdoc-kbd)
      (doc apidoc-kbd)
      (doc tmdoc-menu)
      (doc help-menu)
      (doc help-funcs)
      (doc tmdoc)
      (doc docgrep)
      (doc tmdoc-search)
      (doc tmweb)
      (doc apidoc)
      (doc apidoc-menu)
      (convert rewrite init-rewrite)
      (convert tmml init-tmml)
      (convert latex init-latex)
      (convert html init-html)
      (convert bibtex init-bibtex)
      (convert images init-images)
      (convert images tmimage)
      (convert html tmhtml)
      (convert html tmhtml-expand)
      (convert latex latex-drd)
      (convert latex tmtex)
      (convert latex latex-tools)
      (convert latex tmtex-widgets)
      (part part-shared)
      (part part-menu)
      (part part-tmfs)
      (database db-widget)
      (database db-menu)
      (database db-convert)
      (database bib-db)
      (database bib-manage)
      (database bib-local)
      (database bib-kbd)
      (database db-tmfs)
      (security wallet wallet-menu)
      (security wallet wallet-base)
      (security gpg gpg-edit)
      (security gpg gpg-widgets)
      (security gpg gpg-base)
      (security gpg gpg-menu)
      (client client-base)
      (client client-remote-config)
      (client client-tmfs)
      (server server-menu)
      (client client-menu)
      (client client-notifications)
      (notification notification-base)
      (link link-menu)
      (link link-kbd)
      (link link-edit)
      (link link-navigate)
      (link link-extern)
      (link ref-menu)
      (link ref-edit)
      (version version-menu)
      (version version-kbd)
      (tools spell spell-kbd)
      (version version-tmfs)
      (debug debug-menu)
      (texmacs menus developer-menu)
      (debug debug-widgets)
      (tools poster poster-menu)
      (tools theme theme-menu)
      (tools theme theme-edit)
      (tools spell spell-edit)
      (tools spell spell-lantool)
      (tools ai ai-translate)
      (fonts font-old-menu)
      (fonts font-new-widgets)
      (check check-master)
    )))
```

Run it headless via `-x` after saving the snippet (e.g., to `/tmp/force-all-lazy.scm`) to produce `boot-order-full.scm`:

```zsh
TEXMACS_NO_BOOT=1 QT_QPA_PLATFORM=offscreen \
TEXMACS_PATH="$PWD/TeXmacs" \
TEXMACS_BOOT_PATH="$PWD/build-texmacos/boot" \
TEXMACS_HOME_PATH="$PWD/build-texmacos/tmp-home" \
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs \
  -x '(delayed (:idle 50) (load "/tmp/force-all-lazy.scm") (force-all-lazy-initialization!) (dump-boot-order-manifest! "build-texmacos/boot/boot-order-full.scm") (quit-TeXmacs))'
```

*(Note: Any remaining leaf modules in `src/Scheme/Chez/compile-inventory.scm` that are not reachable through lazy registrations, such as standalone `*-test.so` modules, can be appended afterward if an exhaustive image is ever desired.)*
- **Further Trim Candidate**: Removing the unconditional `(delayed (:idle 250) ...)` preload in `kbd-define.scm` prevents Stage 4's 48 non-active keyboard/converter modules from loading in the background; additionally, splitting `always?` speech/calc/fold/spell shortcuts out of eager startup could trim Stage 2 further if even smaller static memory is desired.
