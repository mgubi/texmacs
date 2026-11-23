# 3. The runtime: Guile on Chez

`TeXmacs/progs/kernel/boot/boot-chez.scm` (loaded first, also into the boot
image) and `kernel/boot/chez-compat.scm` (an R6RS library imported by every
module) give the TeXmacs code the Guile it expects.

## 3.1 Byte strings

A Scheme string is a string of **bytes**: one character per byte, codes 0 to
255. This is what Guile 1.8 does and what the C++ code expects: a TeXmacs
string is a byte string, holding Cork text or UTF-8 bytes depending on its
use.

- The current transcoder is Latin-1, so reading and writing a file keep its
  bytes; the console ports are file-descriptor ports, without transcoding.
- Strings cross to C++ as they are (see [04](04-cpp-binding.md)).
- File names are byte strings too; `tm-os-path` (`os-path` in
  `chez-compat`) converts them to the Unicode strings that Chez gives to the
  system, for opening files, `file-exists?`, directories.
- `utf8-bytes->string` and its inverse are copies, since the bytes are
  already the characters.

The original port used UTF-8 strings beside Cork text
([Handling_Encoding_Chez.md](Handling_Encoding_Chez.md)); with byte strings
accented text works as on the other Schemes, without a conversion at each
crossing.

## 3.2 Loading code

- **Compiled modules** are loaded from their `.so` (`load-module-file`), or
  are already in the heap (boot image). A module without a `.so` is loaded
  from source.
- **Sources** (plug-ins, user files, `load`): `tm-load-file` reads them with
  `tm-read-guile-file` ([02](02-reader-and-compiler.md#21-the-reader-kernelbootchez-readerscm))
  and evaluates their forms in the user environment; a top-level conditional
  with definitions inside goes through `tm-eval-toplevel`. The files written
  for Chez itself (`tm-chez-sources`) are read with Chez's reader.
- `tm-load-init` loads `init-kernel` and `init-texmacs`, compiled or else
  from source.

## 3.3 Modules

TeXmacs modules are Chez modules; their public names are also top-level
definitions, so that `module-ref`, `eval`, the glue and code loaded at run
time find them. `boot-chez.scm` provides `texmacs-module`, `use-modules`,
`module-provide`, `resolve-module`, `module-ref` (top level first),
`module-available?`, `module-defined?`, `module-public-interface`,
`current-module`, and `eval` with a module as its environment. While a
module is loaded `tm-current-module-name` is its name (`tm-define` records
it). `module-file-name` also looks in `$GUILE_LOAD_PATH`, for the Scheme
files of plug-ins.

## 3.4 Errors

Errors follow Guile: a key and arguments.

- `throw` and `scm-error` raise a condition of type `&tm-throw`, which
  carries the key and the arguments; `catch` (and `lazy-catch`) match the
  key, `#t` matching all.
- Errors raised by Chez itself get a Guile key from their message
  (`tm-condition-key`): `unbound-variable`, `wrong-type-arg`,
  `out-of-range`, `wrong-number-of-args`, `misc-error`...
- A glue routine which fails in C++ returns `(tm-glue-error message)`; its
  Scheme wrapper (`tm-glue-check`) raises it as a Guile error (key
  `wrong-type-arg`, `out-of-range` or `misc-error`). While the boot image is
  loaded, such errors are only reported (`GLUE-ERROR-AT-BOOT`).
- A call from C++ which fails (`tm-call-N`, `tm-eval-string`) prints the
  error and its Scheme frames, and returns `(key . args)` to C++, as Guile and
  s7 do; it is counted for `scheme_caught_errors ()`.

## 3.5 Blackboxes

C++ values (trees, urls, commands, widgets...) are records of type
`tm-blackbox` holding the address of a C++ `blackbox`. A guardian deletes the
C++ object when the record is collected. `equal?` and `equal-hash` call C++
(so `equal?`, `member`, `assoc` and hash tables work on trees and urls), and
a blackbox prints as with Guile and s7: `<tree ...>`, `<url a/b>`.

## 3.6 What Chez lacks or does differently

`chez-compat.scm` and `boot-chez.scm` add or adapt:

- **Guile functions:** `delq`, `hash-remove!` (returns the entry), the
  char-sets, `string-skip`, `list-set!`, `stable-sort`, `display-error`,
  `procedure-property`, `procedure-name` (the routines of the glue, Chez's
  own procedures and the `tm-define`d functions), `gc`, Guile's debug and
  read options (as no-ops)...
- **Order:** Chez may evaluate the arguments of `map` and the initial values
  of `let` in any order; `map` and `let` keep the order of Guile.
- **`number->string`** writes the digits above 9 in lower case, as Guile.
- **Opening a file for output** replaces an existing file.
- **`symbol->string`** returns `""` itself for the empty symbol `||`: in
  compiled code Chez 10.4.1 tests `(equal? x "")` against its unique
  mutable empty string, and the immutable empty string which names `||` is
  not `equal?` to it.
- **Sources of procedures:** Chez keeps none. The commands of menus and
  keyboards record theirs (`tm-thunk-with-source`, `source-record!`), which
  the shortcuts and the help use; `procedure-source` of any other closure
  is `#f`.
