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
