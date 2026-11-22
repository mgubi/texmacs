# Profiling the `ui.scm` Benchmark

A self-contained account of how to profile `docs/chez/bench/ui.scm`, what the
numbers mean, and where a keystroke's time and memory actually go.

Everything below was measured on this machine in one session:

| | |
|---|---|
| Machine | Apple M1 Pro (Virtual), 8 cores, macOS, load average 2.5 |
| Qt | 6.12.0, `QT_QPA_PLATFORM=offscreen` |
| Scheme | Chez Scheme **10.4.1** (Homebrew), machine type `tarm64osx` |
| Binary | `build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs`, Release |
| Modules | 453/453 compiled, 0 failures |
| Tree | `wip-chez` @ `48479b85c` |

---

## 1. The build directory, and why it had to be rebuilt

`build-texmacos` is **not** a Chez Scheme installation. It is a CMake *build
directory* — 169 MB of object files, a `TeXmacs.app`, and a `boot/` tree holding
the 453 compiled Scheme modules. The Chez compiler and runtime it uses is the
one Homebrew already had:

```
CHEZ_ROOT    = /opt/homebrew/Cellar/chezscheme/10.4.1/lib/csv10.4.1
CHEZ_MACHINE = tarm64osx
```

So the answer to "wasn't there a Chez from brew, can't you use that?" is: **yes,
and this build does.**

### What was there before

The `build-texmacos` that shipped in this workspace was configured against a
different `CHEZ_ROOT`:

```
CHEZ_ROOT = /Users/king-kong/Documents/my-repo/docker-sandbox-opencode/chez-macos
```

That directory holds Chez **10.5.0-pre-release.1**. Chez locks three things
together — `libkernel.a` (linked statically into the binary), `scheme.h`, and
the `petite.boot` / `scheme.boot` heap images. The version string is compiled
into the executable:

```
$ strings build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs | grep -E '^10\.'
10.5.0-pre-release.1
```

so pointing it at Homebrew's images fails at startup:

```
petite.boot is for Version 10.4.1; need Version 10.5.0-pre-release.1
```

There is no way around this from outside the process: the boot file and the
kernel must agree, and the kernel is already inside the executable.

### What was done

| Option | What happens | Verdict |
|---|---|---|
| Keep the 10.5 tree + `chez-macos` images | Works, but keeps a dependency on the retired pre-release | Rejected |
| Reconfigure the existing tree onto Homebrew Chez | Changing `CHEZ_ROOT` changes `scheme.h`'s path, so every Chez-touching object recompiles and the existing `boot/` modules become unloadable | Rejected — destroys a working tree |
| Build fresh on Homebrew Chez, then swap it in | Old tree moved out of the repository, new build renamed to `build-texmacos` | **Done** |

The module recompile reported `Recompiled: 453 modules / Failures: 0`.

**Renaming a CMake build directory is not free.** The build system embeds its own
absolute path: `CMakeCache.txt`, `Makefile`, and five files under `CMakeFiles/`
all referenced the old name, so a plain `mv` leaves a tree that refuses to
configure —

```
CMake Error: The current CMakeCache.txt directory .../build-texmacos/CMakeCache.txt
is different than the directory .../build-chez1041 where CMakeCache.txt was created.
```

The fix is one extra `cmake -S . -B build-texmacos` (the first attempt rewrites
the directory recorded in the cache, the second succeeds) followed by
`cmake --build`. The 453 compiled modules are unaffected: **none** of the `.so`
files embed the build directory, so `boot/` survives the rename untouched.

Two other environment corrections worth recording:

- `texmacs/doc` does not exist. The documentation is `texmacs/docs/chez/`; the
  *content* tree used by `TEXMACS_DOC_PATH` is `TeXmacs/doc/`.
- The module FASLs are Chez-version-locked too, so `boot/` must be rebuilt
  whenever the Chez version changes — a `boot/` produced by 10.5 cannot be
  loaded by a 10.4 binary.

---

## 2. Running the benchmark and the profiler

Baseline, unmodified:

```bash
cd texmacs
QT_QPA_PLATFORM=offscreen \
SCHEMEHEAPDIRS=/opt/homebrew/Cellar/chezscheme/10.4.1/lib/csv10.4.1/tarm64osx \
HOME=$PWD/fakehome \
TEXMACS_PATH=$PWD/TeXmacs \
TEXMACS_BOOT_PATH=$PWD/build-texmacos/boot \
TEXMACS_DOC_PATH=$PWD/TeXmacs/doc \
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs -x '(load "docs/chez/bench/ui.scm")'
```

Profiling variant (`docs/chez/bench/ui-profile.scm`, same five tasks, same
cold + 3-warm protocol):

```bash
# ... same environment ...
TEXMACS_BENCH_PHASES=1 \
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs \
    -x '(load "docs/chez/bench/ui-profile.scm")'
```

| Knob | Effect |
|---|---|
| `TEXMACS_BENCH_PHASES=1` | per-phase split of the key path, and of load vs typeset |
| `TEXMACS_BENCH_COLLECT=1` | `(collect 2)` before each warm run |
| `TEXMACS_BENCH_GC_NOTIFY=1` | `collect-notify`: one line per collection |
| `TEXMACS_BENCH_ONLY=<substr>` | run only matching tasks |
| `TEXMACS_BENCH_BIG=1` | also the 273-page manual task |

`ui.scm` itself is never modified, so the published numbers stay comparable.

---

## 3. The numbers

### 3.1 Baseline (`ui.scm`, milliseconds)

| Task | Cold | Warm (median of 3) |
|---|---|---|
| `menus-text` | 963 | **98** |
| `menus-math` | 211 | **73** |
| `typing-text-520-keys` | 1479 | **1521** (2.93 ms/key) |
| `typing-math-270-keys` | 2065 | **1818** (6.73 ms/key) |
| `open-change-log` | 1210 | **229** |

These are ~20–60% slower than the numbers in `README.md` §7.3 (83 / 64 / 1147 /
1152 / 185), which were taken on an M2 VM at low load. The gap is the machine
and the load average, not the tree — which is exactly why any comparison has to
be same-session and same-load.

Re-run after the directory rename at a lower load average, as a reproducibility
check: `menus-text` 93, `menus-math` 70, `typing-text-520-keys` 1375,
`typing-math-270-keys` 1416, `open-change-log` 222 ms — same shape, and the
phase split reproduces to within a tenth of a point (54.5% `key-press`,
42.3% `update-forced`).

### 3.2 Resource attribution (`ui-profile.scm`)

Means per warm run. "Bytes" is gross Scheme-heap allocation (see §4).

| Task | Wall ms | CPU ms | GC count | GC ms | Bytes/round | Bytes per unit |
|---|---|---|---|---|---|---|
| `menus-text` | 115 | 115 | 4.7 | 7 (6%) | 38.2 MB | 386 KB/menu (99 menus) |
| `menus-math` | 75 | 76 | 2.7 | 6 (9%) | 23.7 MB | 224 KB/menu (106 menus) |
| `typing-text-520-keys` | 1660 | 1637 | 3.3 | 1 (0.06%) | 27.2 MB | **52.3 KB/key** |
| `typing-math-270-keys` | 1529 | 1529 | 4.7 | 2 (0.1%) | 37.2 MB | **137.7 KB/key** |
| `open-change-log` | 229 | 227 | 0.7 | 2 (1%) | 3.45 MB | — |

Wall ≈ CPU everywhere, so this benchmark is CPU-bound with essentially no
waiting: it is a measurement of work, not of scheduling.

### 3.3 Where a keystroke goes

`ui.scm` drives one key press as five calls:

```scheme
(define (key k)
  (archive-state)      ; C++ undo snapshot
  (start-editing)      ; C++ edit session begin
  (key-press k)        ; glue -> C++ -> Scheme -> C++
  (end-editing)        ; C++ edit session end
  (update-forced))     ; C++ typesetting
```

Splitting those five (per key, averaged over 3 warm rounds):

| Phase | text — 2.94 ms/key | math — 5.58 ms/key | Net Scheme bytes/key |
|---|---|---|---|
| `archive-state` | 42 µs — 1.5% | 119 µs — 2.2% | 0 |
| `start-editing` | 21 µs — 0.7% | 23 µs — 0.4% | 0 |
| **`key-press`** | **1561 µs — 54.5%** | **3221 µs — 59.3%** | **46.5 KB / 126.7 KB** |
| `end-editing` | 31 µs — 1.1% | 40 µs — 0.7% | 0 |
| **`update-forced`** | **1207 µs — 42.2%** | **2030 µs — 37.4%** | **0** |

`open-change-log` (232 ms warm) splits **23% `load-buffer` / 77% `update-forced`**.

Three conclusions that follow directly:

1. **~40% of typing is C++ typesetting.** `update-forced` runs entirely inside
   the engine. No amount of Scheme-level optimization touches it; only a
   profiler that sees C++ frames does.
2. **All of the allocation is in `key-press`**, and `update-forced` allocates
   **exactly zero** bytes on the Scheme heap. Typesetting's memory is C++
   `new`, invisible to any Scheme statistic.
3. **GC is not a typing problem.** 0.06% of typing time. It is 6–9% of the menu
   tasks, so GC tuning helps menus, not keystrokes.

---

## 4. What "bytes/round" actually is

**It is the total number of bytes allocated on the Chez heap during one round of
the task** — one full 520-key pass, or one full 99-menu expansion. It is not a
delta of resident memory, and it is not specifically FFI overhead; it is
everything Scheme allocated, of which FFI marshalling is one component.

### 4.1 Which primitive, and why the obvious one is wrong

`sstats-bytes` (from the `(statistics)` snapshot) is **gross** allocation. A
controlled test in a fresh process — allocate 300 000 ten-element vectors and
retain nothing:

| Metric | Value |
|---|---|
| gross by construction | ~26.4 MB |
| `sstats-bytes` | **29.1 MB** ← gross |
| `sstats-gc-bytes` (reclaimed) | 25.2 MB |
| `(bytes-allocated)` delta | 3.9 MB ← **net** |
| `(time ...)` printed | "28,801,968 bytes allocated, including 25,259,872 bytes reclaimed" |

So `sstats-bytes` and the `time` macro agree on gross allocation, while
`(bytes-allocated)` reports net heap growth and can even go negative. **Use
`sstats-bytes` for churn; never `bytes-allocated`.**

### 4.2 What the 46 KB per keystroke is made of

Measured per-call costs on the Scheme heap:

| Glue call | Bytes/call | Notes |
|---|---|---|
| `archive-state` | 32 | returns unspecified |
| `get-env` | 96 | returns a string |
| `style-has?` | 112 | returns a boolean |
| `current-buffer-url` | 176 | returns a url (blackbox) |
| `cpp-get-preference` | 208 | returns a string |
| **`key-press`** | **44 903** | the whole editing round trip |

`key-press` alone accounts for essentially the entire 46.5 KB. At 32 glue calls
per key (§5), the boundary *calls* cost roughly `10×96 + 4×208 + 4×112 + … ≈
2.5 KB` — about **5% of the total**. The other ~95% is allocated inside the
editing path that `key-press` re-enters.

The mechanism that makes object-returning calls expensive is in
`src/Scheme/Chez/chez_tm.cpp`:

```cpp
tmscm
blackbox_to_tmscm (blackbox b) {
  blackbox* p= tm_new<blackbox> (b);            // C++ heap allocation
  tmscm box= Sbox (Sinteger ((iptr) p));        // Scheme heap allocation
  if (tm_blackbox_guardian) {
    Scall1 (tm_blackbox_guardian, box);         // one more Scheme call
  }
  return box;
}
```

Every `tree`, `url` or `path` handed back to Scheme costs one C++ allocation,
one Scheme box object, and (if the guardian is armed) one extra call into
Scheme. A keystroke that touches many nodes pays this per node. That is the real
form of "extra memory for calling in and out of C++" — not the call itself, but
the *marshalling of every object that crosses*.

A second, purely Scheme-side cost sits on the other direction: `tm-apply`, the
C++→Scheme entry, wraps every call in recursive transcoding —

```scheme
(recur-string->utf8 (apply proc (recur-utf8->string args)))
```

— which walks the argument list and the result, converting strings and
bytevectors, on **every** `call("name", ...)` from C++.

### 4.3 What it does *not* include

- C++ `new`/`malloc`: all typesetting memory. Hence `update-forced` = 0 bytes.
- Peak RSS. Use `current-memory-bytes` / `maximum-memory-bytes` for footprint;
  they answer a different question.

---

## 5. What `key-press` does behind the scenes

### 5.1 It is not Scheme

`key-press` is a glue procedure (`src/Scheme/Glue/glue_editor.cpp:1740`):

```cpp
tmscm tmg_key_press (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "key-press");
  string in1= tmscm_to_string (arg1);          // Scheme string -> C++ string
  get_current_editor()->key_press (in1);
  return TMSCM_UNSPECIFIED;
}
```

So a "keystroke" measured as one Scheme call is really a C++ method that calls
*back* into Scheme. The chain
(`src/Edit/Interface/edit_keyboard.cpp:204`):

```
Scheme  (key-press "a")
  └─ C++  edit_interface_rep::key_press
       ├─ replace (gkey, "<#0>", zero)          string rebuild
       ├─ try_shortcut (new_sh)                 shortcut table lookup
       ├─ archive_state ()                      undo snapshot
       └─ call ("kbd-insert", rew)   ─────────► back into Scheme
            └─ tm-define dispatcher, :require (in-math?) picks
               math/math-edit.scm:73  or  kernel/gui/kbd-handlers.scm:40
                 └─ (insert s) ────────────────► back into C++
                      └─ cpp-insert / insert_tree  tree mutation
```

Three things follow: the boundary is crossed at least four times per keystroke
(in, out for `kbd-insert`, in again for `insert`, out for `insert_tree`), every
crossing that carries a string pays transcoding, and every object returned
allocates a blackbox.

### 5.2 Counted: 32 glue calls per keystroke

Measured by redefining `raw-tm-call-by-name`, the single Scheme→C++ trampoline
that every installed glue procedure forwards to (52 keys, warm):

```
CALLS scheme->c++ 1656   per_key 32
CALLS kbd-insert    52   per_key 1
CALLS bytes    2425760   per_key 46649
```

Breakdown, per key:

| Glue procedure | Calls/key | Role |
|---|---|---|
| `get-env` | **10.4** | current document environment lookup |
| `cpp-get-preference` | **4.2** | preference read |
| `style-has?` | **4.2** | style package query |
| `inside?` | 1.9 | context test |
| `in-graphics?` | 1.1 | context test |
| `url-rooted-tmfs?` | 1.0 | url test |
| `url-none?` | 1.0 | url test |
| `current-buffer-url` | 1.0 | buffer identity |
| `default-look-and-feel` | 1.0 | theme |
| `key-press` | 1 | the actual key |
| `archive-state` | 1 | undo snapshot |
| `start-editing` | 1 | edit session |
| `end-editing` | 1 | edit session |
| `cpp-insert` | 1 | the actual tree mutation |
| `update-forced` | 1 | typesetting |

**Only 6 of the 32 calls are the editing work itself.** About **19 of 32 (59%)**
are environment, preference and style lookups — `get-env` alone is nearly a
third of all boundary traffic, and it is not cheap: it resolves the environment
at the cursor, which walks the document and merges style packages.

`kbd-insert` was counted at exactly 1/key, confirming the C++→Scheme re-entry
happens once per keystroke for a plain character.

### 5.3 Why the C++→Scheme count could not be measured the same way

`tm-apply` is resolved **once at startup** and cached:

```cpp
tm_apply_proc= Stop_level_value (Sstring_to_symbol ("tm-apply"));   // chez_tm.cpp:606
Slock_object (tm_apply_proc);
```

so redefining `tm-apply` from Scheme has no effect — the counter stayed at zero,
which is how the caching was discovered. Wrapping the *targets* by name works
(`kbd-insert` is resolved with `top-level-value` on each call), which is what
produced the 1/key figure.

---

## 6. Verdict on the five profiling approaches

| # | Approach | Verdict |
|---|---|---|
| 1 | `(time expr)` | **Works** inside TeXmacs; its byte figure matches `sstats-bytes`. But it is one expression at a time, prints to the console interleaved with TeXmacs's own `TIME-*`/`DBG` lines, and cannot be aggregated over 520 keys. Spot checks only. |
| 2 | `sstats` | **Right tool, wrong API.** There is no `(sstats)` in 10.4.1 — the snapshot is `(statistics)` (returns an sstats record, prints nothing, ~70 ns). And `(time-second diff)` truncates to whole seconds, so a 1.7 ms delta reads `0`; the fraction is in `time-nanosecond`. With both fixed it is what `ui-profile.scm` is built on. |
| 3 | `compile-profile` + `profile-dump-html` | **Possible, not attempted.** All primitives are bound. It needs all 453 modules recompiled with instrumentation — point `-compile-modules <script>` at a variant compiler and set a separate `TEXMACS_BOOT_PATH` so the working tree is untouched. The HTML annotates the *generated* sources under `boot/`, not `TeXmacs/progs/`. And it can only ever see the 54% that is Scheme; the 42% in `update-forced` is C++. |
| 4 | Allocation tracking | **`bytes-allocated` is the wrong primitive** (net, can go negative). Use `sstats-bytes` (gross) or the `time` macro. `current-memory-bytes` / `maximum-memory-bytes` are fine for footprint. `collect-notify` works and prints one line per collection, but `sstats-gc-count` is better for aggregation. |
| 5 | A `bench` macro harness | **Sound in shape**, but the sketch as written does not run: `(sstats)` is unbound and `(time-second ...)` truncates. Also `(collect 2)` before *every* iteration changes what is measured — better once, or behind a flag. `ui-profile.scm` is that harness with those fixes. |

### Traps specific to this environment

- `current-time` is **not** Chez's inside TeXmacs. A 0-argument glue procedure is
  installed under that name, so `(current-time 'time-monotonic)` raises
  `incorrect number of arguments 1 to #<procedure current-time>`. Take wall time
  from the sstats snapshot instead.
- `top-level-bound?` is `#f` for syntax, so `time` looks unbound. Test with
  `(procedure? (top-level-syntax 'time))`.
- `sstats-bytes` can report a small negative number in a window where a major
  collection reclaims a large pre-existing heap. Cross-check with `(time ...)`
  if a figure looks impossible.
- A `(statistics)` call allocates ~160 bytes, so a phase bracketed by two
  snapshots has a ~320-byte floor. `ui-profile.scm` measures that floor at
  startup and subtracts it (`bytes_net`), otherwise every phase that allocates
  nothing reports the same non-zero number.

---

## 7. Caveats

- `update-forced` is stricter than the GUI, which defers typesetting to the
  paint path. These typing numbers are a typesetting proxy, not literal
  interactive latency.
- Offscreen removes WindowServer/Cocoa blitting, so the 42% typesetting share is
  the *engine's* share and excludes rasterization.
- Load average matters more than any difference in this tree. All numbers here
  are single-session and same-load.
- The 46 KB/key figure is Chez-heap only. It says nothing about C++ allocation,
  which is where typesetting's memory lives.

---

## 8. Where to look next

Ordered by what the measurements actually support:

1. **The 19 configuration calls per keystroke.** `get-env` (10.4), `style-has?`
   (4.2) and `cpp-get-preference` (4.2) are recomputed from scratch on every key
   even though none of them can change between two keystrokes in the same
   buffer. This is the largest single lever the profile exposes: 59% of the
   boundary traffic is configuration, not editing.
2. **The ~45 KB/key allocated inside `key-press`.** Since `update-forced`
   allocates zero, all of it is in the Scheme re-entry — `kbd-insert` → `insert`
   → tree operations, plus one blackbox per object returned. This is where
   `compile-profile` would pay off, because it is precisely the 54% Scheme share
   and the source-level question is "which expression conses".
3. **Not GC.** 0.06% of typing. Only menus (6–9%) would benefit.
4. **Not the FFI call overhead itself.** ~2.5 KB of the 46 KB, about 5%. The
   cost is in marshalling and in how often the boundary is crossed, not in the
   crossing.
