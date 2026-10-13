<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|femtolisp: lazy function bodies and the caches of compiled code>

  <section|Why>

  A boot of <TeXmacs> defines about 9000 functions and calls about 1500 of
  them. Expanding and compiling the others is lost time, and it expands macros
  earlier than the code of <TeXmacs> expects (see
  <hlink|the modules|femtolisp-modules.en.tm>). So the body of a function is
  expanded and compiled when the function is first called.

  <section|The top level only>

  A form of a loaded file is expanded by <verbatim|%expand-top>, not by the
  <verbatim|expand> of femtolisp. It expands the macros as <verbatim|expand>
  does, but it does not enter a <verbatim|lambda>: each lambda it reaches
  (whose free variables are therefore global) is replaced by a call which
  makes a stub,

  <\scm-code>
    (%make-lazy '(lambda (x) body...) 'name '(the module) "TM%progs%...")
  </scm-code>

  So the macros of the top level (<verbatim|tm-define>, <verbatim|menu-bind>,
  <verbatim|define-preferences>...) still run when the file is loaded, and the
  tables they fill are ready; the bodies wait. The arguments are the lambda,
  the name of the function (or <verbatim|#f>), the name of its module
  (<verbatim|#f> for the user module) and the name of the cache of its file
  (<verbatim|#f> without the caches).

  A lambda inside a <verbatim|let> at the top level does not wait.
  <verbatim|let> expands to the call of a lambda: that lambda is made a stub,
  which is called at once, so all it contains is expanded and compiled when
  the file is loaded. This is the case of the overloaded definitions of
  <verbatim|tm-define>, which are inside <verbatim|(let ((former ...)) ...)>:
  their bodies are compiled at the load, as before.

  <verbatim|%expand-top> follows the cases of <verbatim|expand> in
  <verbatim|system.lsp> (a bound global name hides a macro of the same name,
  curried definitions, <verbatim|let-syntax>, quoted data).
  <em|If the expander of femtolisp changes, <verbatim|%expand-top> must follow.>

  <section|The stubs>

  A stub is a function made with the constructor <verbatim|function> of
  femtolisp from three things:

  <\itemize>
    <item>the code of <verbatim|%lazy-template>, shared by all the stubs:
    <verbatim|(lambda args (apply (%lazy-force! '%lazy-record) args))>, where
    <verbatim|%make-lazy> puts the record of the stub in the place of the
    constant <verbatim|%lazy-record>;

    <item>its own constants: a record
    <verbatim|#(lambda module stub forced? file)> and, last,
    <verbatim|(%source . lambda)>;

    <item>the name of the function.
  </itemize>

  The last constant is where <verbatim|procedure-source> reads the source of a
  function, so <verbatim|procedure-source>, <verbatim|procedure-name> and
  <verbatim|procedure-arity> are right for a function which was never called.
  (All this relies on <verbatim|*keep-source*>, which
  <verbatim|r5rs-femtolisp.scm> turns on: the compiler then keeps the source
  of each function as its last constant.)

  <section|The first call>

  <verbatim|%lazy-force!> expands and compiles the lambda, with
  <verbatim|*current-module*> bound to the module where it was loaded, then
  calls <verbatim|%function-become!> (in <verbatim|fl_core.c>): the stub takes
  the code, the constants, the environment and the name of the compiled
  function. The stub <em|is> now the compiled function: whatever kept it (a
  hook, a menu, the former definitions of <verbatim|tm-define>, a table) calls
  the compiled code, and <verbatim|eq?> still holds.

  Two things make this safe, and must stay true:

  <\itemize>
    <item><em|After <verbatim|%lazy-force!> returns, the stub only makes a tail call.>
    The interpreter keeps a pointer into the code which runs, and the
    constants of the stub were just replaced. The code of the template after
    the call is <verbatim|loada0>, <verbatim|tapply>, which read neither. The
    code itself stays alive and in place: all the stubs share it, and
    femtolisp does not move code. Check it after a change of the compiler with
    <verbatim|(disassemble %lazy-template)>.

    <item><em|A function hashes as its source> (patch 0022). femtolisp hashed
    a function by its code, which changes at the first call: the table of the
    names of the functions of <verbatim|tm-define>, whose keys are functions,
    lost them. The compiled function is given the source of its stub
    (<verbatim|%with-source-of>), the lambda before its expansion.
  </itemize>

  A macro which runs while a body is compiled may call the very function being
  compiled. The record has a flag for this: the inner call compiles the body
  and makes the stub become it; the outer compilation then finds the flag set,
  drops its own result and uses the stub.

  <section|The caches of compiled code>

  Compiling is the larger part of the work of loading a file, so the compiled
  code is kept in <verbatim|$TEXMACS_HOME_PATH/system/cache/femtolisp/>. For
  the source file whose name in the cache is <verbatim|N>:

  <\description-long>
    <item*|<verbatim|N.flc>: the forms of the top level>For each form, the
    fingerprint of its expansion and its compiled code. It starts with a key
    (the compiler, the version of <TeXmacs>, the format of the cache) and the
    private names of the module, which the compiled code depends on.
    <verbatim|%eval-forms-cached> reads it in step with the forms: a form is
    still expanded, since the macros may have effects, and its compiled code
    is taken from the cache when the fingerprints agree. The file is written
    again when a form was compiled.

    <item*|<verbatim|N.lazy>: the function bodies>For each body compiled so
    far, the fingerprint of its expansion, together with its module and the
    private names of that module, and its compiled function. It is read at the
    first call of a function of the file (<verbatim|%lazy-file-table>), and
    each compilation adds a line to it. It starts again
    (<verbatim|%lazy-file-changed!>) at the first form of the file which can
    be cached and is not found in <verbatim|N.flc>: the file changed, or it
    had no valid cache. The forms are compared in their order, so a form added
    to a file also compiles again those after it.
  </description-long>

  The name <verbatim|N> is the path of the file with <verbatim|/>,
  <verbatim|\\>, <verbatim|:> and the spaces replaced by <verbatim|%>; a file
  of <TeXmacs> is named by its path inside <verbatim|$TEXMACS_PATH>
  (<verbatim|TM%progs%...>), so that a cache made elsewhere fits. A
  distribution, or the page of the browser version, can ship the compiled code
  in <verbatim|$TEXMACS_PATH/cache/femtolisp/>: its <verbatim|N.flc> is read
  when the home has no valid one, and its <verbatim|N.lazy> is read before the
  one of the home.

  A fingerprint is 128 bits of hash of the structure of a value, as 32
  hexadecimal digits (<verbatim|%fingerprint>, in <verbatim|fl_core.c>). It is
  <verbatim|#f> when the value holds a closure, an uninterned symbol, a table
  or an object of <TeXmacs>, or is very large: the form is then compiled at
  each load. So is a form whose compiled code holds an uninterned symbol, a
  table, an object of <TeXmacs> or a closure with an environment
  (<verbatim|%cache-writable?>); the cache of a file with such a form is
  written again at each load.

  <em|When to change the format.> The key does not see a change of
  <verbatim|boot-femtolisp.scm>. After a change which makes the same expansion
  compile to other code (the arguments of <verbatim|%make-lazy>, the late
  calls...), increase <verbatim|%cache-format> (it has two numbers, one with
  lazy bodies and one for <verbatim|TEXMACS_FL_EAGER>): the old caches are
  then ignored and written again. A change of <verbatim|system.lsp> or
  <verbatim|compiler.lsp> changes the boot image, which is part of the key.

  <verbatim|TEXMACS_FL_NO_CACHE=1> runs without the caches. They are worth 0.2
  s at a boot, and 0.4 s at each start in the browser.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
