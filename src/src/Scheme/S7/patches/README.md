# Local patches to the vendored s7

`../s7.c` is stock s7 (the version in `../s7.h`, from
<https://ccrma.stanford.edu/software/s7/s7.tar.gz>) with these patches,
applied in order. Each one starts with a description; the changed code is
marked `TeXmacs:` in `s7.c`.

| Patch | What |
|---|---|
| `0001-write-long-strings-of-one-character.patch` | `write`/`display` no longer print long strings of one character as `(make-string n c)` |
| `0002-call-site-keeps-closure-only-with-same-body.patch` | fixes a segfault when a call site is given a closure made from new code (reported upstream) |
| `0003-curried-define.patch` | Guile's curried `define`, `(define ((f a) b) ...)` |
| `0004-string-ref-p0-signature-for-webassembly.patch` | `(string-ref s 0)` on a parameter no longer traps in WebAssembly; upstream since s7 5-Oct-2026, drop it then |
| `0005-cache-macro-expansions.patch` | `(*s7* 'cache-macro-expansions?)`: a macro call evaluated again reuses its expansion, as in Guile (TeXmacs sets it in `start_scheme`) |

To upgrade s7, from `src/src/Scheme/S7`:

    cp /path/to/new/s7/s7.c /path/to/new/s7/s7.h .
    for p in patches/*.patch; do patch -p1 < "$p" || break; done

then check, for each patch, whether upstream has fixed or changed the code
(drop or refresh the patch: make it with
`diff -u --label a/s7.c --label b/s7.c old/s7.c new/s7.c`), rebuild from clean
and run the tests (`src/docs/s7/05-build-and-vendored-s7.md`).
