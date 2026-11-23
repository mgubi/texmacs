# 4. The C++ side

## 4.1 The `tmscm` interface

`src/Scheme/Chez/chez_tm.{hpp,cpp}` implement for Chez the interface that the
rest of TeXmacs uses for every Scheme (`tmscm`, `tmscm_car`,
`string_to_tmscm`, `blackbox_to_tmscm`, `eval_scheme`, `call_scheme`...).
`tmscm` is Chez's `ptr`.

- **Start-up** (`initialize_scheme`): register Chez's heap images and
  `texmacs.boot`, build the heap, load `boot-chez.scm` when there is no boot
  image, cache the Scheme procedures the kernel calls (`tm-eval-string`,
  `tm-call-0`... `tm-call-n`, `list?`, `equal?`, `string->symbol`) and lock
  them against the collector, then run the init file
  ([01](01-build.md#14-running)).
- **Garbage collection:** Chez's collector moves objects. A C++ `object`
  locks its Scheme value (`Slock_object`, `object.cpp`); the builders of
  lists and vectors in `glue.cpp` keep their partial results on a locked
  root stack (`ChezRoot`).
- **Strings** cross as they are, byte strings on both sides (#947):
  `string_to_tmscm` makes a Scheme string of the bytes of a TeXmacs string
  (`chez_bytes_to_string`), `tmscm_to_string` reads them back; a character
  above 255, which only `integer->char` makes, comes out in UTF-8, and a
  bytevector is accepted too (`string-save` of binary data). Nothing is
  converted on the Scheme side.
- **Strings and symbols made by C++** (`eval_scheme`, `symbol_to_tmscm`, the
  error messages of the glue) are built byte by byte
  (`chez_bytes_to_string`): Chez's `Sstring_of_length` and
  `Sstring_to_symbol` read signed chars and turn a byte from 0x80 into an
  invalid character.
- **Paths for the system** (`eval_scheme_file`, `load_scheme_file`) are
  UTF-8 (`Sstring_utf8`), since Chez gives them to the system.
- **Calls from C++** go through `tm-call-N`, which catches the errors and
  returns `(key . args)`; `scheme_caught_errors ()` counts them.

## 4.2 The glue

The specifications of the glue (`src/Scheme/Glue/build-glue-*.scm`) are the
same for every Scheme. `src/Scheme/Chez/build-glue-chez.scm`, run by Chez,
generates `src/Scheme/Chez/glue_chez.cpp`: one C function `tmg_NAME` per
routine, wrapped in a shim (`tmgc_tmg_NAME`) that catches C++ exceptions,
and the table of the routines (1402): the Scheme name, the number of
arguments and the shim of each. `initialize_glue_chez`, called from
`custom_init` before the boot files are loaded, registers one foreign
symbol, `tm_glue_table`, which returns the table as a list of
`(name arity . address)`.

As with Guile and s7, nothing is generated for the Scheme side.
`boot-chez.scm` reads the table when it is loaded and defines each routine
with `tm-glue-procedure`: the `foreign-procedure` of its number of arguments
(0 to 10), called at the address of the shim, and `tm-glue-check`, which
raises the errors of the C++ side. Chez cannot make a procedure of a C
function from C, so the Scheme side is these eleven foreign procedures for
all the routines; one `foreign-procedure` and one procedure for each routine
made 5 MB of compiled code (#944). Each procedure takes exactly the
arguments of its routine (its arity is checked by `glue-test.scm`), and
`procedure-name` gives the name of a routine (`tm-glue-names`). The
arguments and the result go through unchanged.

## 4.3 Errors across the boundary

A C++ exception must not cross Chez's frames. The shim of each routine
catches it and returns `(tm-glue-error message)`, which the Scheme wrapper
raises as a Guile error ([03](03-runtime.md#34-errors)).

- **A wrong argument** (`TMSCM_ASSERT`, a value out of range in
  `tmscm_to_int`) is an ordinary Scheme error, as with s7: no crash report,
  no message on stderr.
- **Any other exception** (a `tm_throw`, a `FAILED`) is reported on stderr
  (`[GLUE EXCEPTION in NAME]`), with the crash report of `tm_throw`, and
  raised in Scheme.
- `TM_GLUE_TRACE=1` prints the Scheme frames of a routine which fails.

## 4.4 Blackboxes

`blackbox_to_tmscm` allocates a C++ `blackbox` and wraps its address in a
`tm-blackbox` record ([03](03-runtime.md#35-blackboxes)), registered with a
guardian: when the record is collected, the guardian's reaper calls
`tmscm_delete_blackbox` and zeroes the address. `tmscm_blackbox_equal`,
`tmscm_blackbox_hash` and `tmscm_blackbox_to_string` give C++ equality,
hashing and printing to Chez.

## 4.5 Changes outside `src/Scheme/Chez`

All under `USE_CHEZ`, so the other Schemes build the same code as before:

- `misc/m4/scheme.m4`: `--with-scheme=chez`, `--with-chez=DIR`;
  `src/System/config.in`, `src/makefile.in`: the libraries of Chez, the
  generation of the glue;
- `src/Scheme/Scheme/object.{hpp,cpp}`: locked handles, the hash of a
  blackbox, `call` by symbol;
- `src/Scheme/Scheme/glue.cpp`: the builders of lists and vectors, the
  checks of `tmscm_to_path` and `tmscm_to_url` (Chez's `tmscm_car` does not
  check its argument);
- `src/Data/String/analyze.cpp`: `unescape_guile` reads Chez's escapes
  (`\x1;`, as few digits as needed), as s7's;
- `src/Texmacs/Texmacs/texmacs.cpp`: `-compile-modules`,
  `-recompile-modules`.
