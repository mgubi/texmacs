# 2. Boot and modules

## 2.1 Boot sequence

1. `start_scheme` initializes femtolisp from the embedded boot image and
   installs the GC-roots hook.
2. `initialize_scheme` defines the glue builtins, the blackbox type and a few
   C builtins (`current-time`, `getpid`).
3. TeXmacs loads `scheme_init_file ()`, that is `progs/init-femtolisp.scm`,
   with femtolisp's own `load`. It loads:
   - `kernel/boot/r5rs-femtolisp.scm`: R5RS and the Guile basics, as global
     definitions (see [03](03-compat-layer.md));
   - `kernel/boot/boot-femtolisp.scm`: the module system and the redefined
     `load` and `eval`;
   - the module `(kernel boot compat-femtolisp)`: the Guile library functions;
   - the shared `init-kernel.scm` and `init-texmacs.scm`, as for s7 and Guile.

## 2.2 Modules: renamed global names

femtolisp has one global environment, and different TeXmacs modules define
the same private names (222 names are privately defined by more than one
module, `run-group` 11 times). So private definitions are renamed:

- When a module file is loaded, its forms are read first, and scanned for the
  plain `define` and `define-macro` at top level (also inside `begin`, `if`,
  `when`, `unless`, `cond`).
- Each such name `f` of module `(a b)` gets the global name `f@a/b`.
- Then each form is expanded and compiled with `*current-module*` set to the
  module. femtolisp's expander and compiler call `resolve-global` on every
  global name (patch 0003); `boot-femtolisp.scm` defines it to map the
  module's private names to their global names. This covers variables,
  functions, `set!` and macros.
- Public definitions (`define-public`, `define-public-macro`, `provide-public`,
  `export`) are global under their own names. A name defined both privately
  and by `tm-define` stays private in its module, as in Guile, where the
  module's binding hides the one of the user module.

`use-modules`, `inherit-modules` and `:use` load the module; they do not
restrict what a module sees (as with s7, every public name is visible).
`resolve-module`, `module-ref`, `module-defined?`, `with-module` and
`eval ... module` work on module records.

Each module file goes through the cache of compiled files: its forms are
expanded, and their compiled code is taken from
`$TEXMACS_HOME_PATH/system/cache/femtolisp/` when the expansion has not
changed (see [07](07-performance.md#76-what-made-it-faster)).

## 2.3 Macros used before their definition

femtolisp expands macros when a form is compiled, Guile when it first
evaluates it. TeXmacs code relies on Guile's behaviour: a function may use a
macro defined later, in a module loaded later (`delayed` in `tm-plugins.scm`).

The compiler calls the hook `compile-unknown-call` (patch 0010) for a call
`(f ...)` whose head is neither bound, nor a macro, nor a local variable.
`boot-femtolisp.scm` compiles such a call, unless `f` is a function defined
by the file being loaded, into `(%late-call site (list v...))`. At its first
evaluation the site compiles `(lambda (v...) (f ...))` over the local
variables in scope, caches it and applies it. When `f` is still unknown then,
the call is a plain one, which raises an unbound-variable error.

A boot creates about 370 such sites. Their limit: a `set!` of a local
variable inside the call does not reach the enclosing function.

## 2.4 Macro expansion errors

A macro whose expansion fails stops the load of the whole file, whereas in
Guile only code that runs is expanded. With `*defer-macro-errors*` (patch
0013) such a call compiles into code raising the error when it runs. This is
needed for widget definitions which are never shown in some configurations.

## 2.5 `tm-define`

`tm-define.scm` has femtolisp branches, close to the s7 ones:
- a definition is `(define-global! 'f value)`, global under its quoted name;
- `former` and `tm-defined-name` use `(top-level-value 'f)`, the global
  function, also in a module which defines `f` privately;
- `tm-define-macro` defines the macro with `define-public-macro`;
- `lazy-define` retrieves the function with `module-ref`.

## 2.6 The compiler is protected from TeXmacs names

The functions of femtolisp's `system.lsp` and `compiler.lsp` look each other
up by global name at run time. TeXmacs defines public functions with the same
names (`make-label`, `print`...), which broke the compiler. The boot image is
therefore built by compiling these files twice (patch 0009): the second time
their references go to private names `%fl:name`, so TeXmacs may redefine the
public ones.
