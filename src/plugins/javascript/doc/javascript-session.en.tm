<TeXmacs|2.1.5>

<style|<tuple|tmdoc|javascript|english>>

<\body>
  <tmdoc-title|<name|JavaScript> sessions in a web browser>

  When <TeXmacs> runs in a web browser, it is a program of the page,
  compiled to <name|WebAssembly>, next to the <name|JavaScript> of the page.
  A <name|JavaScript> session evaluates its input there, in the global scope
  of the page: whatever the page holds is at hand (<verbatim|document>,
  <verbatim|window>, the program of <TeXmacs>), and <TeXmacs> itself through
  the object <verbatim|TeXmacs> (see below).

  <paragraph|Sessions and executable folds>

  A session is started with <menu|Insert|Session|JavaScript>; an executable
  fold with <menu|Insert|Fold|Executable|JavaScript>.
  <shortcut|(kbd-return)> evaluates the input, <shortcut|(kbd-shift-return)>
  starts a new line. Then:

  <\itemize>
    <item>declarations with <verbatim|var> and <verbatim|function> stay for
    the next inputs; <verbatim|let> and <verbatim|const> hold only within
    one input;

    <item>a value is shown as text: strings, numbers, arrays and objects
    (as <name|JSON>), maps and sets, elements of the page (as their tag);
    nothing for <verbatim|undefined>;

    <item>a promise is waited for, and its value shown; code with
    <verbatim|await> is run as the body of an asynchronous function, whose
    value is the one it returns (<verbatim|return>);

    <item>what <verbatim|console.log> and the other functions of the console
    write while an input runs is shown in the session too;

    <item>an exception is shown in red, with its place in the input.
  </itemize>

  <paragraph|Examples>

  Each example below is an executable fold: put the cursor inside it and
  press <shortcut|(kbd-return)> (<shortcut|(kbd-shift-return)> for those of
  several lines), then <shortcut|(kbd-return)> again to see its source.

  The browser:

  <script-input|javascript|default|navigator.userAgent|>

  The page which runs <TeXmacs>:

  <script-input|javascript|default|[...document.querySelectorAll ("*")].length + " elements, " + innerWidth + "x" + innerHeight + " pixels"|>

  <TeXmacs>, asked from <name|JavaScript>:

  <script-input|javascript|default|TeXmacs.scheme ("(length (buffer-list))") + " documents are open"|>

  <TeXmacs>, told to do something (look at the status bar):

  <script-input|javascript|default|TeXmacs.later ('(set-message "Hello from JavaScript" "")')|>

  A value of <name|JavaScript> as <TeXmacs> content, here a table made by a
  program:

  <\script-input|javascript|default>
    TeXmacs.output ("scheme",

    \ \ "(tabular (table " +

    \ \ [1, 2, 3, 4, 5].map (n =\<gtr\> '(row (cell "' + n + '") (cell "' + n * n + '"))').join (" ") +

    \ \ "))")
  <|script-input>
    
  </script-input>

  <name|HTML> works too:

  <script-input|javascript|default|TeXmacs.output ("html", "\<less\>b\<gtr\>bold\<less\>/b\<gtr\>, \<less\>i\<gtr\>italic\<less\>/i\<gtr\> and \<less\>code\<gtr\>code\<less\>/code\<gtr\>")|>

  A request to the server of the page, waited for:

  <\script-input|javascript|default>
    var response = await fetch (location.href);

    return response.status + " " + response.headers.get ("content-type");
  <|script-input>
    
  </script-input>

  Values of several kinds:

  <script-input|javascript|default|({today: new Date ().toDateString (), squares: [1, 2, 3].map (x =\<gtr\> x * x), tm: typeof TeXmacs})|>

  <paragraph|<name|JavaScript> and <TeXmacs>>

  The global object <verbatim|TeXmacs> gives <name|JavaScript> access to
  <TeXmacs>:

  <\description>
    <item*|<verbatim|TeXmacs.scheme (expr)>>evaluates the <name|Scheme>
    expression <verbatim|expr> at once, and gives its value as text (a
    string as it is, the other values as <name|Scheme> writes them, an error
    as <verbatim|(error ...)>).

    <item*|<verbatim|TeXmacs.later (expr)>>evaluates <verbatim|expr> after
    the current input, without value: for commands which change the
    documents and the windows.

    <item*|<verbatim|TeXmacs.output (format, data)>>a value which a session
    shows as <TeXmacs> content, <verbatim|data> being in a format of the
    plug-ins: <verbatim|"scheme"> (a <TeXmacs> tree), <verbatim|"html">,
    <verbatim|"latex">...

    <item*|<verbatim|TeXmacs.show (format, data)>>shows <TeXmacs> content
    at once in the output of the input which runs, before its value: code
    which waits (<verbatim|await>) shows what it makes as it goes, as
    <verbatim|console.log> does for text. <verbatim|TeXmacs.show (value)>
    shows a value (a string as text). Its value is <verbatim|false>, and
    nothing is shown, when no input runs.

    <item*|<verbatim|TeXmacs.module>>the program of <TeXmacs>, as
    <name|Emscripten> made it.
  </description>

  The other way, <verbatim|(web-javascript code)> evaluates
  <name|JavaScript> from <name|Scheme> and gives its value as a string.
  <verbatim|TeXmacs.scheme> may not be used in code which <TeXmacs> runs
  itself, as such code: only from a session, an event of the page, a timer
  or a promise.

  <paragraph|Customizing <TeXmacs> in <name|JavaScript>>

  The file <verbatim|my-init-javascript.js>, in the directory
  <verbatim|progs> of the user's <TeXmacs> directory, is run each time
  <TeXmacs> starts in the browser, after <verbatim|my-init-texmacs.scm>. It
  is kept by the browser with the other files of the user, and opened with
  <menu|Developer|Open my-init-javascript.js> (the menu
  <menu|Developer> appears with <menu|Tools|Developer tool>). For instance,
  <key|F8> for a new document:

  <\verbatim-code>
    // F8: a new document

    window.addEventListener ("keydown", function (e) {

    \ \ if (e.key == "F8") TeXmacs.later ("(new-document)");

    });
  </verbatim-code>

  Try such code in a session first: it takes effect at once.

  <paragraph|Limits>

  <\itemize>
    <item>The plug-in exists only in a web browser.

    <item>The code runs in the page, with <TeXmacs>: a loop which never ends
    stops both. Interrupting a session forgets the code which runs (its
    value is not shown), but cannot stop it.

    <item>Code given to a session runs with the rights of the page, which
    holds the files kept by <TeXmacs> in the browser: only evaluate code
    which you trust.
  </itemize>

  <paragraph|More examples>

  More examples, which use the browser and <TeXmacs> together. As above, each is an executable fold: <shortcut|(kbd-shift-return)> evaluates it.

  A drawing made by a program, as a <TeXmacs> picture:

  <\script-input|javascript|default>
    var pts = [];

    for (var i = 0; i \<less\>= 80; i++) {

    \ \ var x = -4 + i / 10;

    \ \ pts.push ('(point "' + x + '" "' + Math.sin (2 * x) * Math.exp (-x * x / 8) + '")');

    }

    TeXmacs.output ("scheme",

    \ \ '(with "gr-geometry" (tuple "geometry" "9cm" "3cm") "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh")) ' +

    \ \ '(graphics (with "color" "grey" (line (point "-4" "0") (point "4" "0"))) ' +

    \ \ '(with "color" "blue" "line-width" "1.5ln" (line ' + pts.join (" ") + '))))')
  <|script-input>
    
  </script-input>

  Exact integers of any size (<verbatim|BigInt>):

  <\script-input|javascript|default>
    var f = 1n;

    for (var i = 1n; i \<less\>= 25n; i++) f *= i;

    "25! = " + f
  <|script-input>
    
  </script-input>

  A matrix written in <LaTeX>, here the triangle of Pascal:

  <\script-input|javascript|default>
    var n = 6, rows = [];

    for (var i = 0; i \<less\> n; i++) {

    \ \ var r = [];

    \ \ for (var j = 0; j \<less\> n; j++) r.push (j \<gtr\> i ? "" : String (binom (i, j)));

    \ \ rows.push (r.join (" & "));

    }

    function binom (a, b) { var c = 1; for (var k = 1; k \<less\>= b; k++) c = c * (a - k + 1) / k; return c; }

    TeXmacs.output ("latex", "$\\\\begin{pmatrix}" + rows.join (" \\\\\\\\ ") + "\\\\end{pmatrix}$")
  <|script-input>
    
  </script-input>

  The files of <TeXmacs> in this browser, a table made from a file of the page:

  <\script-input|javascript|default>
    var m = await (await fetch ("texmacs-files.json")).json ();

    var row = (a, b) =\<gtr\> '(row (cell "' + a + '") (cell "' + b + '"))';

    return TeXmacs.output ("scheme", '(tabular (table ' + row ("package", "size (KB)") + ' ' +

    \ \ m.packages.map (p =\<gtr\> row (p.name, Math.round (p.size / 1024))).join (" ") + '))');
  <|script-input>
    
  </script-input>

  A digest of a text by the cryptography of the browser:

  <\script-input|javascript|default>
    var data = new TextEncoder ().encode ("GNU TeXmacs");

    var hash = await crypto.subtle.digest ("SHA-256", data);

    return [...new Uint8Array (hash)].map (b =\<gtr\> b.toString (16).padStart (2, "0")).join ("");
  <|script-input>
    
  </script-input>

  Today in several languages:

  <\script-input|javascript|default>
    var langs = ["en", "fr", "de", "it", "es", "nl"];

    TeXmacs.output ("scheme", '(tabular (table ' + langs.map (l =\<gtr\>

    \ \ '(row (cell "' + l + '") (cell "' + new Date ().toLocaleDateString (l, {weekday: "long", year: "numeric", month: "long", day: "numeric"}) + '"))').join (" ") + '))')
  <|script-input>
    
  </script-input>

  Content of <TeXmacs> shown as it is computed (<verbatim|TeXmacs.show>), here the partial sums of a series:

  <\script-input|javascript|default>
    var s = 0;

    for (var n = 1; n \<less\>= 6; n++) {

    \ \ s += 1 / (n * n);

    \ \ TeXmacs.show ("latex", "$\\\\sum_{k=1}^{" + n + "} \\\\frac{1}{k^2} = " + s.toFixed (6) + "$");

    \ \ await new Promise (ok =\<gtr\> setTimeout (ok, 600));

    }

    return "the limit is pi^2/6 = " + (Math.PI * Math.PI / 6).toFixed (6);
  <|script-input>
    
  </script-input>

  A count down, waited for, with what it writes shown as it goes:

  <\script-input|javascript|default>
    for (var i = 3; i \<gtr\> 0; i--) {

    \ \ console.log (i + "...");

    \ \ await new Promise (ok =\<gtr\> setTimeout (ok, 1000));

    }

    return "lift-off";
  <|script-input>
    
  </script-input>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software
  Foundation; with no Invariant Sections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of the license is included in the
  section entitled "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>
