# TeXmacs on guile_cpp

[guile_cpp](https://github.com/PikachuHy/guile_cpp) is GNU Guile 1.8.8
rewritten in C++ by PikachuHy (PR #982): it runs the Guile code of TeXmacs
(`TeXmacs/progs/init-guile.scm` and the same progs as Guile 1.8) without the
old Guile, with a bytecode compiler and VM. It lives on the branch
`wip_guile_cpp`; its license is pending with its author (it is derived from
Guile 1.8.8, LGPL 2.1 or later), so no package may ship it yet.

## Building

```
./configure --with-scheme=guile_cpp ...      # macOS and Linux only
make
```

- guile_cpp is vendored in `src/src/Scheme/guile_cpp/guile_cpp` (sources,
  headers and the Scheme library of Guile 1.8); its `README` gives the
  commit and every change made to it. The makefile compiles its sources as
  `Objects/gcpp_<name>.o` (C++17, the flags of TeXmacs); its tests, its
  benchmarks, its command line and its ARM64 JIT are left out.
- The glue generators run on `Objects/s7-run`, as with femtolisp and Chez.
- `make` copies the Scheme library (`ice-9`, `oop`, `srfi`) into
  `TeXmacs/boot/guile_cpp` (target `GUILE_CPP_DATA`), where the binding
  finds it from `TEXMACS_PATH`: the packages carry it with the TeXmacs
  folder (`INSTALL_DATA` copies it), nothing is compiled in.
- The rest of TeXmacs sees only `guile_cpp/include/guile_cpp/value.hpp`
  (the tagged values and the pairs), through `guile_cpp_tm.hpp`; the other
  headers of guile_cpp are seen by the objects of `src/Scheme/guile_cpp`.
- The CMake build does not know guile_cpp (autotools only).

## The binding

`src/src/Scheme/guile_cpp/guile_cpp_tm.cpp` implements the `tmscm`
interface, as the other backends. guile_cpp reports its errors and escapes
by C++ exceptions (`guile::SchemeThrow`, `guile::ContinuationEscape`): every
call from TeXmacs into Scheme catches them and returns `(key . args)` as
the Guile backend does. The collector scans the C stack conservatively, as
Guile 1.8, so C++ variables holding Scheme values need no protection.

The guards of the stack: guile_cpp stops with "Stack overflow" when the C
stack is deeper than 5 MB from `start_scheme`; the binding sets
`scheme_stack_low` (`System/Misc/stack_guard.hpp`) to that limit, so that
the guards of TeXmacs stop the recursions of C++ first and a deep document
gives "nesting too deep". An exception of TeXmacs in a glue routine becomes
an error of Scheme (`tmscm_raise_error`); any other exception of C++ which
reaches a call from C++ into Scheme unwinds the frames of `dynamic-wind`
it left and is reported as `(misc-error ...)`.

## The containers

guile_cpp is written with the standard library; on this branch
`std::function`, the hash tables, `unique_ptr`, `deque` and `std::pair` are
replaced by function pointers and the containers of TeXmacs, and the
stacks of the bytecode VM are arrays. `std::string`, `std::string_view`
and `std::vector` remain (about 500 uses): the vendored `README` says why
(the sharing of TeXmacs strings and arrays on copy, the freeing of an
array resized to 0) and what else of the standard library is used.

## Open problems

- The license of guile_cpp.
- No CMake build, no Windows, no browser.
- `(read-enable 'positions)` and `(debug-enable 'debug)` only with
  `DEBUG_ON` (the Guile backend sets them always); errors are printed after
  the unwinding, without a backtrace.
- Trees used as keys of `equal?` hash tables are hashed by content
  (`hash_blackbox_smob`), Guile 1.8 gives smobs a constant hash.
