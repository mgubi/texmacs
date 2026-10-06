# 5. Build and the vendored femtolisp

## 5.1 Choosing the interpreter

- autotools: `./configure --with-scheme=femtolisp` (`misc/m4/scheme.m4`
  defines `USE_FEMTOLISP` and `SCHEME_DIR=Femtolisp`; `configure` is
  regenerated with autoconf 2.73).
- CMake: `-DSCHEME_IMPL=femtolisp` (not tested yet).
- The makefile compiles `Scheme/Femtolisp/*.cpp` and `*.c`; the two C files
  get `-IScheme/Femtolisp/femtolisp/llt -w` (llt has headers such as
  `utils.h`, which must not be on the global include path).
- The glue is regenerated, when needed, with the s7 interpreter `s7-run`.

## 5.2 Files

```
src/Scheme/Femtolisp/
  femtolisp/          upstream femtolisp ec76010 with the patches applied
                      (sources, llt, system.lsp, compiler.lsp, flisp.boot, tests)
  patches/            0001-0020, made with git format-patch
  fl_core.c, fl_llt.c the two compilation units
  fl_tm.h             the C interface
  fl_boot.h           flisp.boot as a C array (make-boot-header.sh)
  femtolisp_tm.*      the tmscm layer
  README              short version of these notes
```

## 5.3 The patches

| Patch | Change |
|---|---|
| 0001 | llt builds on any architecture (arm64, WebAssembly): byte swaps with compiler builtins |
| 0002 | hooks: extra GC roots, equality and hash of opaque values |
| 0003 | `resolve-global`: hook for the global names in `expand` and the compiler (modules) |
| 0004 | reader and printer as Guile's: `\|` and `\` in symbols, `#{...}#`, long tokens, overflowing integers read inexact, floats written shortest, Guile style |
| 0005 | vectors written `#(...)`; with `*print-shared*` `#f`, labels only for cycles |
| 0006 | strings written as strings of bytes |
| 0007 | a builtin called with a wrong number of arguments is an error at run time, not at compile time |
| 0008 | curried definitions, `(define ((f a) b) ...)` |
| 0009 | the functions of `system.lsp` and `compiler.lsp` call each other through private names `%fl:name` (`mkboot1.lsp` compiles them twice) |
| 0010 | `compile-unknown-call`: hook for the calls of names not defined at compile time |
| 0011 | `*keep-source*`: compiled functions keep their source |
| 0012 | the heap stops growing at 1 GB with an out-of-memory error (`heapsize` has 32 bits), after `fl_out_of_memory_hook` |
| 0013 | `*defer-macro-errors*` |
| 0014 | the reader returns a datum at the very end of the input (the end of input only when nothing is left) |
| 0015 | `[` and `]` are symbol characters, as in Guile |
| 0016 | a local variable does not shadow a special form at the head of a form (a parameter `begin` captured the `begin` of `cond`'s expansion) |
| 0017 | `<` and `=` with any number of arguments |
| 0018 | `*print-closures*`: with `#f`, closures written `#<procedure f>` |
| 0019 | the unspecified value is the self-evaluating symbol `#<unspecified>` (it was `#t`) |
| 0020 | `*arith-fallback*` for `+ - * / =` on non-numbers; `(- 0)` is the fixnum 0 |

The new behaviours are off by default (the flags and hooks), except where
femtolisp was wrong or where its own boot needs them, so upstream's tests
(`make test`) pass at every step.

## 5.4 Upgrading femtolisp or changing `system.lsp` / `compiler.lsp`

Apply the patches in order to a clone of femtolisp (`git am patches/*.patch`),
copy the sources into `femtolisp/`, then rebuild the boot image with a
standalone `flisp` until it no longer changes:

```
make -C llt && make release
./flisp mkboot0.lsp system.lsp compiler.lsp > flisp.boot.new && mv flisp.boot.new flisp.boot
./flisp mkboot1.lsp && ./flisp mkboot1.lsp     # a fixed point: the file no longer changes
make test
cp flisp.boot <tree>/src/Scheme/Femtolisp/femtolisp/
<tree>/src/Scheme/Femtolisp/make-boot-header.sh
```

The boot image keeps shared structure (`*print-shared*` is `#t` while it is
written): two closures may share one object, such as the marker list of
`values`.
