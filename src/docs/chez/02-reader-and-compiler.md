# 2. Reading Guile code and compiling it

The sources of `TeXmacs/progs` are Guile code, shared by all the Schemes.
Chez does not interpret them: each module is read, turned into a Chez module
and compiled to machine code before TeXmacs runs. This puts three constraints
on the code, which the reader and the compiler handle:

- Chez's reader does not read all of Guile's syntax;
- a Chez module body admits no definition after an expression, and no
  definition under an `if`;
- macros are expanded once, when the module is compiled, not when the code
  first runs, and the compiler is a separate run of TeXmacs.

## 2.1 The reader (`kernel/boot/chez-reader.scm`)

An R6RS library, `(chez-reader)`, used both by the module compiler and at run
time, for the files loaded from source (plug-ins, user files).
`tm-read-guile-file` reads a file as Guile reads it:

- **Bytes.** The file is read as Latin-1: one character per byte, as in all
  the strings of this branch ([03](03-runtime.md#31-byte-strings)).
- **Guile's lexical syntax**, rewritten before Chez's reader sees it:
  `#{...}#` symbols, `|`, `[ ]` and `{ }` as symbol characters, `\` in
  symbols, octal characters `#\NNN`, `\xNN` escapes without `;`.
  Non-ASCII bytes outside strings are escaped as `\xHH;`, and comments are
  cleaned of them.
- **The byte 0x85 in a string** is escaped: Chez's reader takes it for NEL,
  a line ending, and turns it into a newline. It occurs in the UTF-8 of many
  letters (Å is `C3 85`).
- **Dialect tests are folded** when the code is read: `(chez-scheme?)` is
  true, `(s7-scheme?)` and `(femtolisp-scheme?)` are false, and the `if`,
  `cond`, `when`, `and`, `or` around them are simplified, only where a test
  changed. So the branches for the other Schemes, which may use syntax or
  definitions Chez cannot compile, are gone. Definition heads are never
  rewritten.
- **`with-module`** is unwrapped; a top-level `cond` becomes a chain of `if`s.
- **`let`** evaluates its initial values in order (`sequence-lets`): Chez
  may evaluate them in any order, and TeXmacs code relies on Guile's.

## 2.2 The module compiler

`texmacs.bin -compile-modules` starts TeXmacs headless with
`init-chez-compile.scm`, which runs `src/Scheme/Chez/compile-texmacs.scm`:

1. **Which modules, in which order:** `compile-inventory.scm`, seven tiers
   in dependency order, ending with the test modules. A module missing from
   the inventory is not compiled (and is then loaded from source, slowly).
2. **Scan** (`compiler-scan.scm`, records in `compiler-db.scm`): each source
   is read with the reader above, then:
   - a curried `(define ((f a) b) ...)` becomes a `define` returning a lambda;
   - a second definition of a name becomes a `set!`;
   - definitions that `chez-compat` provides, or that Chez has
     (`when`, `unless`, `case-lambda`...), are dropped;
   - private helpers called by macros of other modules are made public
     (`chez-promoted-names`);
   - private names which the tests reach in their module
     (`chez-global-names`, by file) become global definitions.
3. **Transpile** (`convert-to-r6rs.scm`): the TeXmacs module becomes a Chez
   `(module ...)` with its imports and exports, written under
   `$TEXMACS_BOOT_PATH`. `chez-compat` and `abbrevs` become R6RS libraries,
   imported by all. Expressions between definitions become dummy definitions
   (`%tm-expr-N`), so the source order is kept; `tm-module-start!` and
   `tm-module-end!` around the body record the current module (for
   `tm-define`).
4. **Compile** with Chez's `compile-file` into a `.so`, then **load** it, so
   that the macros and definitions are there for the modules compiled after
   it. `compiler-oracle.scm` finds the helpers a macro needs when it is
   expanded.
5. `init-kernel.scm` and `init-texmacs.scm` are compiled the same way
   (`compile-guile-script!`), then `init-texmacs-boot-file.scm`.

A module is recompiled when its source is newer than its `.so`;
`-recompile-modules` recompiles all.

## 2.3 Macros that act when they are expanded

Some TeXmacs macros do their work while they expand (they register a
converter, a state slot, a group of variants...). With Guile this happens
when the module is loaded; with Chez the expansion happens once, in the
compiler. Those macros have a Chez branch that expands into a call doing the
same work at load time: `converter` (`converter-define!`), `define-group`
(`group-define!`), `define-secure-symbols`, `state-slots`, `define-table`,
`define-collection`, `tm-define` itself (`tm-define-overloaded-chez`). See
[05](05-progs-changes.md).

## 2.4 The boot image

`src/Scheme/Chez/make-boot.scm` packs the `.so` of the start-up into
`texmacs.boot`, a Chez boot file which Chez loads into its heap when it
starts (`Sbuild_heap`): 177 modules, 18 MB, from `boot-chez.so` to the menus
used at start-up. The list is `*startup-boot-modules*`, in dependency order.
A module whose loading needs a view (a window) must not be in it: the heap
is built before the first window (this is why `developer-menu` is lazy).

## 2.5 Files

| File | Role |
|---|---|
| `TeXmacs/progs/kernel/boot/chez-reader.scm` | the reader of Guile code |
| `TeXmacs/progs/init-chez-compile.scm` | start-up of the compiler run |
| `src/Scheme/Chez/compile-texmacs.scm` | the driver |
| `src/Scheme/Chez/compile-inventory.scm` | the modules and their order |
| `src/Scheme/Chez/compiler-scan.scm`, `compiler-db.scm` | scanning the sources, the database of modules |
| `src/Scheme/Chez/convert-to-r6rs.scm` | writing the Chez modules |
| `src/Scheme/Chez/compiler-helper.scm`, `compiler-oracle.scm`, `compile-graph.scm` | compiling one module, the helpers of macros, the dependency graph |
| `src/Scheme/Chez/make-boot.scm` | the boot image |
| `src/Scheme/Chez/*-test.scm` | tests of the compiler (`chez --script`) |
