# 1. The C++ binding

## 1.1 Layering

```
rest of TeXmacs ── scheme.hpp: object, call(), eval() ...       (interpreter-neutral)
                      │
                   Scheme/object.cpp, Scheme/glue.cpp, Glue/glue_*.cpp (generated)
                      │   uses only: tmscm, tmscm_*, *_to_tmscm, tmscm_to_*, TMSCM_*
                      │
            Femtolisp/femtolisp_tm.{hpp,cpp}      (USE_FEMTOLISP)
                      │   uses only fl_tm.h
            Femtolisp/fl_core.c   femtolisp (one unit) + the C interface
            Femtolisp/fl_llt.c    the library llt of femtolisp (one unit)
```

The C++ code never includes `flisp.h`: its macros (`car`, `cdr`, `ptr`,
`symbol`, `tag`...) would clash with TeXmacs names. `fl_tm.h` is a small C
interface (`fltm_*`) implemented in `fl_core.c`, which compiles femtolisp's
sources as one unit and so can use its internals (the stack, the cvalue
types).

`object.hpp` includes `femtolisp_tm.hpp` when `USE_FEMTOLISP` is defined.

## 1.2 `tmscm` is a GC root

femtolisp has a copying collector: any allocation may move every object. A
Scheme value held in a C++ variable would become invalid after the next
allocation, and the glue holds values in this way all the time, for instance
`p= tmscm_cons (scheme_tree_to_tmscm (t[i]), p)`.

So `tmscm` is a class: a value plus links in a doubly linked list of roots
(`tmscm_roots`). Its constructors link it, its destructor unlinks it, and the
collector calls `tmscm_relocate_roots` (patch 0002, `fl_gc_extra_roots`) to
update every value in the list. Any `tmscm` — local variable, temporary,
argument, member of a heap object — stays valid across allocations. The
constructor from a raw value is `explicit`, so that integers do not convert
silently.

Consequences:
- `tmscm_object_rep` holds a `tmscm` directly: the object stack of the Guile
  and s7 backends is not used.
- The functions of `fl_tm.h` that allocate protect their own arguments (on
  femtolisp's stack), but raw `fltm_value`s must not be kept across them.
- `TEXMACS_FL_CHECK_ROOTS=1` checks every root at each collection.

## 1.3 Errors never cross C++ frames

femtolisp raises errors with `longjmp`. A `longjmp` over a C++ frame skips the
destructors of its `tmscm`s, which would leave dead stack addresses in the
list of roots. Hence:
- every call from C++ into Scheme goes through `fltm_apply` or
  `fltm_eval_string`, which catch the errors (`FL_TRY_EXTERN`), report them
  (`%report-error`) and return them in Guile form (`%guile-error`);
- a glue function runs inside `tmscm_proc`, which copies the arguments into
  `tmscm`s, runs the function in a C++ `try` block, and raises the femtolisp
  error only after the block, when no C++ object is alive.
  `TMSCM_ASSERT` throws a C++ exception (`tmscm_error`) carrying the faulty
  argument.

## 1.4 Blackboxes

TeXmacs trees, urls, commands... are opaque femtolisp values whose data is a
pointer to a heap-allocated `blackbox` (`fltm_define_opaque_type`). Their type
has hooks (patch 0002) to:
- print them (`<tree ...>`, `<url ...>`);
- delete the blackbox when the value is collected;
- compare them for `equal?` with the blackbox's `==` (structural for trees, as
  in Guile);
- hash them consistently (`hash (tree)` for trees).

## 1.5 Glue

`tmscm_install_procedure` defines a femtolisp builtin (`cbuiltin`) wrapped by
`tmscm_proc<PROC>`, which checks the number of arguments. The generated
`glue_*.cpp` files are the same as for s7 and Guile; when they must be
regenerated, the femtolisp build uses the small s7 interpreter `s7-run`, as
the s7 build does.

## 1.6 Strings, numbers, symbols

- `string_to_tmscm` copies the bytes; `tmscm_to_string` copies them back. No
  encoding conversion: Cork bytes pass through unchanged.
- Integers are fixnums or 64-bit boxed integers; `tmscm_to_int` raises an
  out-of-range error, as Guile's `scm_to_int`.
- `tmscm_is_double` is true for all numbers.
- The boot image (`fl_boot.h`) is embedded in the binary; nothing is read
  from disk to start femtolisp.
