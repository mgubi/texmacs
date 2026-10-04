# 6. Open issues, fragile spots and next steps

## 6.1 Open bugs and limitations

- **The s7 optimizer could mis-apply closures called from a loop.** This
  was an upstream bug, worked around in TeXmacs. After a loop had run once
  with a closure of one shape, calling it with a closure of another shape
  could run the wrong one:

  ```scheme
  (define (mk s) (let ((chars (string->list s))) (lambda (ch) (and (memv ch chars) #t))))
  (define (inter . css) (lambda (ch) (let loop ((cl css)) (or (null? cl) (and ((car cl) ch) (loop (cdr cl)))))))
  (define (count cs) (let loop ((i 0) (n 0)) (if (= i 256) n (loop (+ i 1) (if (cs (integer->char i)) (+ n 1) n)))))
  (count (inter (mk "!?") (mk "aB!")))   ; => 1
  (count (mk "abc"))                     ; => error: memv second argument, #\null, ... should be a list
  ```

  - **Workaround:** the char-sets of `compat-s7.scm` are hash tables, not
    closures, which is how the bug first showed up.
  - **No longer reproduces** (checked on 4 October 2026): the example gives
    `1` and `3` with the vendored s7 11.9, with and without the local
    patches, with s7 5-Oct-2026, and inside TeXmacs. The workaround stays.
- **The internal definitions of a macro body do not see each other.** This
  is an upstream regression (s7 24-Sep-2025 is fine; s7 11.9 and 5-Oct-2026
  are not), worked around. In the body of a `define-macro` or
  `define-macro*`, a helper which calls another helper defined there does
  not find it:

  ```scheme
  (define-macro (m . args)
    (define (one l) (car l))
    (define (both k) (list (one k) (one k)))
    `(quote ,(both args)))
  (m a b c)        ; => error: unbound variable one in (one k)
  ```

  - The same body in a function works, and so does a `(let () …)` around
    the definitions in the macro body.
  - When the helpers are defined by another macro (as when `define` was a
    macro), s7 crashes instead (`s_lookup_1: k unbound` with
    `S7_DEBUGGING`). The original `case-lambda` of `srfi.scm` raised
    `unbound variable alength` that way.
  - **Workaround:** TeXmacs macros define their helpers at module level
    (§3.2).
  - `define` is no longer a macro (curried definitions are s7's own, patch
    0003), so TeXmacs does not meet this case. The crash of an internal
    recursive definition, the second time the enclosing function ran, was
    another bug, fixed by patch 0002 ([05](05-build-and-vendored-s7.md#s7-version-and-local-patch));
    this one still reproduces with that patch.
  - **Report:** drafted with these two examples, to send upstream.
- **Memo tables no longer cache `#f`.** Storing `#f` in an s7 hash table
  doesn't create an entry, so `logic-holds?` (`logic-data.scm`) and
  `texmacs-submode?` (`tm-modes.scm`) recompute negative answers on every
  call. Results stay correct, but the work is repeated. **Fix:** store a
  sentinel value.
- **The apidoc source scanner uses Guile-only functions (not verified).**
  `doc/apidoc-funcs.scm` (`parse-form`) uses `source-property` and
  `def-keywords`, which exist only on Guile. The "module exported symbols"
  part of the API docs probably fails on s7.
- **`texmacs-module` ignores unknown options silently.** A misplaced
  parenthesis in a module header therefore goes unnoticed; it once kept
  `cite-sort-test` from loading.
- **`run-all-tests` stops at the first failing suite,** which hides the
  later ones. `docs/s7/bench/suites.scm` runs the portable suites one by one.

## 6.2 Fragile or surprising behavior

- **The user module must not be entered** with `with-let` or `with-module`
  after boot. It would make lookups slower, not wrong (§2.3).
- **A `set!` of a public variable in its own module** is not seen by the
  other modules, which use the rootlet binding (§2.2). Macros which expand
  into references to such variables are affected too, since the expansion
  is evaluated in the caller's module. `tm-plugins.scm` gives accessors for
  its flag and table for this reason (§4.3).
- **`:use` is not enforced.** Every module sees the rootlet and the user
  module.
- **Macros were expanded on every evaluation** (s7 run-time macros; Guile
  expands a macro call once and keeps the expansion). Patch 0005 now caches
  the expansions ([05](05-build-and-vendored-s7.md#s7-version-and-local-patch)).
  Measured on 4 October 2026, before the patch, with a counter in `s7.c`
  around the macro bodies (outermost expansions only, so the time is the
  expansion itself, not running its result):

  | Workload | Expansions | Time expanding | Of the total |
  |---|---:|---:|---:|
  | Boot | 57 000 | 0.05 s | about 7% |
  | 8 warm LaTeX exports of the change log | 1 120 000 | 0.67 s | 27% (2.49 s) |
  | 22 headless regression suites | 950 000 | 0.50 s | 4% (12.1 s) |
  | Editing suites (editing, math-edit, table, text-structure) | 1 490 000 | 1.29 s | 9% (13.9 s) |

  A few macros make most of them: `with` everywhere; `receive`, `cut` and
  `logic-ref` in the LaTeX export; `match-cup` and the menu macros (`$list`,
  `$menu-link`, `$balloon`, `$=>`…) while editing, when the menus are
  rebuilt. With patch 0005 the warm LaTeX exports are 13–20% faster.
- **s7 11 quirks to keep in mind when writing kernel code:**
  - `varlet` refuses already-bound symbols in non-root lets;
  - macro bodies should not define local helper functions (§3.2);
  - shared files can't use s7 reader syntax such as `#_define`.
- **Some compat functions differ from their Guile originals:**
  - `assoc-set!` returns a new list instead of mutating, and it is defined
    twice in `compat-s7.scm`;
  - `ahash-get-handle` returns a fresh cons, so a `set-cdr!` on it doesn't
    write through (no caller does this today);
  - `iota` takes only one argument;
  - `append!` and `delq` are non-destructive;
  - `lazy-catch` unwinds before running the handler.
- **`with-global` is not unwind-safe:** a non-local exit leaves the variable
  changed. The Guile version has the same problem.
- **Glue error messages are vague.** Argument errors say
  `"some other thing"` instead of the expected type, and
  `tmscm_install_procedure` ignores the optional and rest argument counts.
- **`developer-mode?` is `#f` on s7.** The Guile version reads the
  preference.
- **The empty symbol doesn't survive printing.** The XML parser names the
  PI `<??>` with the empty symbol, which C code can create. s7 prints it as
  nothing (Guile prints `#{}#`), so writing such an s-expression with
  `object->string` and reading it back loses the symbol.
- **s7 needs more memory than Guile** on large workloads: about 90 MB more
  for repeated LaTeX export, and 80 MB more for regenerating the manual
  (see [07](07-performance.md#memory)).
- **Output differs slightly between the interpreters:**
  - LaTeX: the order of packages, `---` versus `\textemdash`;
  - HTML: the order of attributes, float digits.

  Reference outputs made with one interpreter won't match the other.

## 6.3 Guile leftovers

- **Guile-flavoured names:**
  - the function `init_guile` in `init_texmacs.cpp`;
  - the empty `texmacs_init_guile_hooks` in s7 builds;
  - `$GUILE_LOAD_PATH`, still the module search path;
  - `unescape_guile`.
- **Scheme files that work only under Guile:**
  - `init-guile.scm`, `kernel/boot/boot.scm` and `kernel/boot/compat.scm`,
    which only the Guile boot loads;
  - `utils/misc/doxygen.scm`, which uses `(ice-9 rdelim)`;
  - the trace facility in `kernel/boot/debug.scm`, which uses
    `procedure-property`.

## 6.4 Before merging upstream

1. **Build and test the remaining configurations.**
   - CI builds and tests s7 on Linux, macOS and Windows (MinGW), see
     [05](05-build-and-vendored-s7.md#ci).
   - Not built yet: MSVC, Android, CMake builds, and Guile builds outside
     macOS.
2. **Exercise plugins and user code.** Scheme code written for Guile, such
   as `my-init-texmacs.scm` or plugin `progs`, can use features that
   `compat-s7.scm` lacks or only partly provides (§6.2). No plugin has been
   tried yet.
3. **Split the branch into a reviewable series:**
   - the build option, the vendored s7 and the C++ binding;
   - the shared kernel with its dialect branches;
   - the compatibility layer and the s7 boot;
   - the fixes and speed-ups to shared code, which are useful even with
     Guile (§4.3).

   Upstream should regenerate `configure` itself.
4. **Report the two s7 bugs above upstream.**
5. **Look at the rest of startup, on both interpreters** (see
   [07](07-performance.md#boot)):
   - **Plugin detection** spawns a shell (`which`) for each plugin, about
     0.6 s in all. Searching `$PATH` directly, as `resolve_in_path` can
     already do, or caching the answers, would avoid it.
   - **Qt's dock widget and font database** take about half of the time to
     the first window.
6. **Optional:**
   - a direct tree ↔ s7 conversion for `tree->stree` and `stree->tree`
     (§1.7);
   - tuning of the s7 heap if memory matters more than speed (see
     [07](07-performance.md#memory)).
