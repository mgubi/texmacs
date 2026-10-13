<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|femtolisp in the browser>

  The browser version of <TeXmacs> (the Vue interface compiled to WebAssembly,
  see <source-link|docs/wasm/README.md|docs/wasm/README.md>) can be built with femtolisp instead of
  S7. femtolisp compiles for 32-bit WebAssembly as it is: its fixnums have 30
  bits there, and larger integers are boxed.

  <section|Building the page>

  <\shell-code>
    . misc/wasm/emenv.sh build-wasm

    make -C build-wasm -f ../misc/wasm/Makefile -j8 SCHEME=femtolisp web
  </shell-code>

  With <verbatim|SCHEME=femtolisp>, <source-link|misc/wasm/Makefile|misc/wasm/Makefile> replaces the
  sources of S7 by <source-link|fl_core.c|src/Scheme/Femtolisp/fl_core.c>, <source-link|fl_llt.c|src/Scheme/Femtolisp/fl_llt.c> and
  <source-link|femtolisp_tm.cpp|src/Scheme/Femtolisp/femtolisp_tm.cpp>, and defines <verbatim|USE_FEMTOLISP>;
  <source-link|misc/wasm/config.h|misc/wasm/config.h> then leaves <verbatim|USE_S7> out. The objects
  go to <verbatim|build-wasm/obj-femtolisp> and the page to
  <verbatim|build-wasm/out-femtolisp/web>, so a tree can hold both builds;
  without the option, the build is the one of S7, in <verbatim|build-wasm/obj>
  and <verbatim|build-wasm/out>. The target <verbatim|node> makes the program
  for node, <verbatim|build-wasm/out-femtolisp/node/texmacs.js>.

  The files which <TeXmacs> reads before it starts are in the boot package of
  the page: <source-link|misc/wasm/boot-files.txt|misc/wasm/boot-files.txt> lists the four files of
  femtolisp (<source-link|init-femtolisp.scm|TeXmacs/progs/init-femtolisp.scm> and the three of
  <source-link|kernel/boot|TeXmacs/progs/kernel/boot>) besides those of S7.

  <section|The cache of compiled code shipped in the page>

  Without a cache, the first visit of the page would compile all the code it
  loads. So the build makes the caches of compiled code (see
  <hlink|the lazy bodies and the caches|femtolisp-lazy.en.tm>) and ships them
  in the boot package, as <verbatim|/texmacs/cache/femtolisp>, where
  <source-link|boot-femtolisp.scm|TeXmacs/progs/kernel/boot/boot-femtolisp.scm> reads them: for each file of which the home of
  the page has no valid cache, and always for the function bodies, before
  those of the home.

  <\itemize>
    <item>The target <verbatim|femtolisp-cache> of
    <source-link|misc/wasm/Makefile|misc/wasm/Makefile>, made at every build of the page with
    femtolisp, runs the program for node on
    <source-link|misc/wasm/femtolisp-cache.scm|misc/wasm/femtolisp-cache.scm>: it loads what <TeXmacs> loads at
    its start and when all the menus are opened, in text and in math. The same
    compiler, in WebAssembly, makes the code which the page will run.

    <item>Its cache is copied to <verbatim|build-wasm/femtolisp-cache> (the
    <verbatim|.flc> and the <verbatim|.lazy> files of the files of <TeXmacs>,
    whose names start with <verbatim|TM%>).

    <item><source-link|misc/wasm/package.py|misc/wasm/package.py> takes a fourth argument,
    <verbatim|\<less\>dir\<gtr\>:\<less\>prefix\<gtr\>>, which adds the files
    of a directory to those of the page: here
    <verbatim|build-wasm/femtolisp-cache:cache/femtolisp>. <verbatim|cache/>
    is one of the groups of the boot package.
  </itemize>

  The shipped cache makes the boot package about 0.9 MB larger (compressed
  with brotli), and the first visit about a second shorter (3.1 s instead of
  4.2 s: without it, the first visit compiles the code and writes the cache).
  What the page compiles anyway goes to the cache of its home, in the storage
  of the browser.

  <section|The functions of Scheme for the page>

  <verbatim|web-javascript>, <verbatim|web-files>, <verbatim|web-open-pdf>,
  <verbatim|web-open-external> and <verbatim|web-paste-dialog> are defined in
  <source-link|src/Plugins/Vue/vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp> on the interface of the interpreters
  (<verbatim|tmscm_install_procedure>, <verbatim|TMSCM_ASSERT>,
  <verbatim|string_to_tmscm>): one definition for S7 and femtolisp. A new
  function of this kind must be written the same way, not with the functions
  of one interpreter.

  <section|Debugging and testing in the page>

  <\description-long>
    <item*|<verbatim|?env=NAME=VALUE> in the address of the page>Sets a
    variable of the environment of <TeXmacs>
    (<source-link|misc/wasm/web-pre.js|misc/wasm/web-pre.js>); it may be given several times. For
    instance <verbatim|?env=TEXMACS_FL_TRACE=1>, or
    <verbatim|?env=TEXMACS_FL_NO_CACHE=1>.

    <item*|<verbatim|TeXmacs.scheme (code)>, in the JavaScript of the page>Evaluates
    Scheme code and returns its value as text
    (<source-link|misc/wasm/javascript.js|misc/wasm/javascript.js>):
    <verbatim|TeXmacs.scheme ("%cache?")> tells whether the caches are used.

    <item*|<source-link|misc/wasm/browser-run.mjs|misc/wasm/browser-run.mjs>>Runs a page in a browser
    without a window, with a script of actions (<verbatim|wait>,
    <verbatim|click>, <verbatim|type>, <verbatim|key>, <verbatim|shot>,
    <verbatim|eval>). Its option <verbatim|--dir> gives the page
    (<verbatim|build-wasm/out-femtolisp/web>), <verbatim|--profile> a profile
    of the browser kept between the runs (the home of the page),
    <verbatim|--query> what follows the address, and <verbatim|--timeout> the
    seconds an <verbatim|eval> may take (180 by default).
  </description-long>

  The regression suites run in the page with the script

  <\verbatim-code>
    wait 8000

    eval TeXmacs.scheme("(run-all-tests)")
  </verbatim-code>

  and <verbatim|--timeout 1800>. A page cannot do all that the suites check:
  with S7 ten suites have failing checks there, and femtolisp fails the same
  ones, with its two own (<verbatim|htmltm>, <verbatim|graphics-edit>); one
  group of <verbatim|math-edit> fails with other checks than with S7. Compare
  the failing checks of the two pages, not their number. A check on fonts may
  fail in a new profile, while the fonts are fetched.

  <em|When measuring>, give each option of <verbatim|browser-run.mjs> as words
  of its own (<verbatim|--query> and its value are two arguments), and check
  in the page that an option which changes what is measured was taken: a
  comparison of the page with and without the caches once measured the same
  page twice.

  <section|How it compares>

  The start of the page, as it reports it (the time of its first drawing):
  with femtolisp about 3.1 s at the first visit and 1.2 s at the later ones,
  with S7 about 2.8 s and 1.05 s. With the code of the caches removed,
  femtolisp would need 3.6 s and 1.55 s. The program is 0.7 MB smaller with
  femtolisp (23.5 MB against 24.2 MB). The measurements are in
  <source-link|docs/femtolisp/10-benchmarks.md|docs/femtolisp/10-benchmarks.md>.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
