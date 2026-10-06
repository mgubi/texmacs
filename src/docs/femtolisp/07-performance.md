# 7. Performance

## 7.1 Boot

`texmacs.bin -x '(exit 0)' -q`, Qt offscreen, macOS arm64, 3 runs each
(2026-10-06; the s7 build is the `wip_s7` build at 67f2e2d0c2):

| | real time | user time | peak memory |
|---|---|---|---|
| femtolisp | 1.33 s | 1.16 s | 165 MB |
| s7 | 0.65 s | 0.46 s | 400 MB |

femtolisp boots about twice as slowly, with less than half the memory (s7
starts with a large preallocated heap).

## 7.2 Where the time goes

- **Compilation.** Every top-level form of every loaded file is expanded and
  compiled to bytecode by femtolisp's compiler, itself written in Lisp. s7
  interprets the code directly, and only the code that runs is analysed.
- **Module scanning.** Each module file is read whole, and scanned for its
  private definitions before it is compiled; `resolve-global` is called for
  every global name compiled.
- **Kept sources.** Each compiled lambda keeps its source (for
  `procedure-source`), which costs memory more than time.

Ideas, not tried yet:
- compile the bodies of top-level functions lazily, at their first call;
- cache the compiled bytecode of the kernel modules on disk;
- keep the sources only for the lambdas of menus and keyboard bindings.

## 7.3 Run time

Not measured yet against s7 or Guile. The arithmetic stays femtolisp's
bytecode instructions (the fallback for complex numbers costs nothing for
real numbers), and the late calls add one closure call per call site.
