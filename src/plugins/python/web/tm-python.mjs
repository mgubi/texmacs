// The Python plugin in the browser: the worker of its sessions and folds, a
// module
// (plugins/python, see src/docs/wasm/README.md).
//
// A plugin of the page is a Web Worker (src/System/Link/worker_link.cpp,
// misc/wasm/workers.js): it reads its input from {input} messages and writes
// its output in {out} and {err} messages, in the protocol of the plugins, as
// a program does on its pipes. This one runs Python in WebAssembly
// (Pyodide), loaded from the CDN of jsDelivr by the first input; the
// packages which an input imports (numpy, sympy, matplotlib...) are loaded
// then. Each input is run as in the interactive Python: the value of its
// last expression is shown (a formula for those of SymPy), and the figures
// of matplotlib which it made are shown as pictures.
//
// Python cannot be interrupted here (that needs a SharedArrayBuffer, which
// the page has not): while it runs, the worker says it is busy, and an
// interrupt then stops the worker (workers.js), Python being started again
// by the next input.

var PYODIDE_VERSION = '314.0.7';
var PYODIDE_URL = 'https://cdn.jsdelivr.net/pyodide/v' + PYODIDE_VERSION + '/full/';

var B = '\x02', E = '\x05';
var PROMPT = B + 'prompt#>>> ' + E;
var decoder = new TextDecoder ();

function out (s) { postMessage ({ out: s }); }
function err (s) { postMessage ({ err: s }); }
function busy (b) { postMessage ({ busy: b }); }

// a Scheme string, for the "scheme:" blocks
function schemeString (s) {
  return '"' + s.replace (/\\/g, '\\\\').replace (/"/g, '\\"') + '"';
}

// the text of an output: without the characters of the protocol
function verbatim (s) {
  return s.replace (/[\x02\x05\x1b]/g, '');
}

/******************************************************************************
* Pyodide, and what runs an input
******************************************************************************/

// the value of the last expression, if any, as the interactive Python shows
// it (not after a ; at the end, as in Jupyter): repr, or LaTeX for an object
// of SymPy (or one which has _repr_latex_); the figures of matplotlib as SVG
var HELPER = [
  'import ast, sys, io, inspect, traceback, warnings',
  'warnings.filterwarnings ("ignore", message = ".*non-interactive.*")',
  'def _tm_value (v):',
  '    if v is None: return None',
  '    m = sys.modules.get ("sympy")',
  '    if m is not None and isinstance (v, m.Basic):',
  '        return ("latex", m.latex (v))',
  '    f = getattr (v, "_repr_latex_", None)',
  '    if callable (f):',
  '        try:',
  '            s = f ()',
  '            if isinstance (s, str) and s.strip (): return ("latex", s.strip ().strip ("$"))',
  '        except Exception: pass',
  '    return ("text", repr (v))',
  'def _tm_figures ():',
  '    plt = sys.modules.get ("matplotlib.pyplot")',
  '    if plt is None: return []',
  '    r = []',
  '    for n in plt.get_fignums ():',
  '        b = io.StringIO ()',
  '        plt.figure (n).savefig (b, format = "svg", bbox_inches = "tight")',
  '        r.append (b.getvalue ())',
  '    plt.close ("all")',
  '    return r',
  'async def _tm_run (src, ns):',
  '    flags = ast.PyCF_ALLOW_TOP_LEVEL_AWAIT',
  '    try:',
  '        tree = ast.parse (src, "<input>", "exec")',
  '        last = None',
  '        if tree.body and isinstance (tree.body[-1], ast.Expr):',
  '            last = ast.Expression (tree.body.pop ().value)',
  '        c = eval (compile (tree, "<input>", "exec", flags = flags), ns)',
  '        if inspect.iscoroutine (c): await c',
  '        v = None',
  '        if last is not None:',
  '            v = eval (compile (last, "<input>", "eval", flags = flags), ns)',
  '            if inspect.iscoroutine (v): v = await v',
  '            if v is not None: ns["_"] = v',
  '            if src.rstrip ().endswith (";"): v = None',
  '        return (None, _tm_value (v), _tm_figures ())',
  '    except BaseException as e:',
  '        tb = e.__traceback__',
  '        if not isinstance (e, SyntaxError) and tb is not None: tb = tb.tb_next',
  '        text = "".join (traceback.format_exception (type (e), e, tb))',
  '        try: figs = _tm_figures ()',
  '        except Exception: figs = []',
  '        return (text, None, figs)'
].join ('\n');

var pyodide = null;     // a promise of Pyodide, with the helper defined
var printed = '';        // what the input printed (stdout and stderr)

function python () {
  if (pyodide) return pyodide;
  pyodide = (async function () {
    // (a module: Pyodide runs in module workers only)
    var mod = await import (PYODIDE_URL + 'pyodide.mjs');
    var py = await mod.loadPyodide ({ indexURL: PYODIDE_URL });
    var write = function (buf) { printed += decoder.decode (buf, { stream: true }); return buf.length; };
    py.setStdout ({ write: write });
    py.setStderr ({ write: write });
    // matplotlib without a window: its figures are taken after each input
    py.runPython ('import os\nos.environ["MPLBACKEND"] = "agg"');
    py.runPython (HELPER);
    py.runPython ('_tm_ns = {"__name__": "__main__"}');
    return py;
  }) ();
  pyodide.catch (function () { pyodide = null; });
  return pyodide;
}

/******************************************************************************
* The session
******************************************************************************/

function figure (svg) {
  return '(image (tuple (raw-data ' + schemeString (svg) + ') "figure.svg") "" "" "" "")';
}

async function evaluate (code) {
  var py;
  busy (true);
  try { py = await python (); }
  catch (e) {
    busy (false);
    err (B + 'utf8:Python could not start (Pyodide ' + PYODIDE_VERSION +
         ', from ' + PYODIDE_URL + '): ' + e + E);
    out (B + 'verbatim:' + PROMPT + E);
    return;
  }
  printed = '';
  var r = null;
  try {
    // the packages which the input imports (numpy, sympy...), from the CDN
    await py.loadPackagesFromImports (code, { messageCallback: function () {},
                                              errorCallback: function () {} });
    var run = py.globals.get ('_tm_run');
    var ns = py.globals.get ('_tm_ns');
    var res = await run (code, ns);   // (top-level await: micropip...)
    r = res.toJs ();
    res.destroy (); run.destroy (); ns.destroy ();
  }
  catch (e) {
    r = [String (e), null, []];
  }
  busy (false);
  var answer = '';
  if (printed !== '') answer += B + 'verbatim:' + verbatim (printed.replace (/\n$/, '')) + E;
  var error = r[0], value = r[1], figs = r[2] || [];
  if (value) {
    if (answer !== '') answer += '\n';
    if (value[0] === 'latex') answer += B + 'latex:$' + verbatim (value[1]) + '$' + E;
    else answer += B + 'verbatim:' + verbatim (value[1]) + E;
  }
  figs.forEach (function (svg) {
    if (answer !== '') answer += '\n';
    answer += B + 'scheme:' + figure (svg) + E;
  });
  if (error) err (B + 'utf8:' + verbatim (error.replace (/\n$/, '')) + E);
  out (B + 'verbatim:' + answer + PROMPT + E);
}

var input = '';
var queue = Promise.resolve ();

onmessage = function (e) {
  var msg = e.data || {};
  if (msg.interrupt) return;   // (stopped by the page while Python runs)
  if (!msg.input) return;
  input += decoder.decode (msg.input, { stream: true });
  var i;
  // the serializer of the plugin ends an input by a line <EOF>
  while ((i = input.indexOf ('\n<EOF>\n')) >= 0) {
    var code = input.slice (0, i);
    input = input.slice (i + 7);
    queue = queue.then (function (c) { return function () { return evaluate (c); }; } (code));
  }
};

// the banner of a session: what runs Python, and how to use the session
var HELP_URL = 'tmfs://help/article/tm/plugins/python/doc/python-browser.en.tm';
function banner () {
  var s = schemeString;
  var line = function () { return '(concat ' + Array.prototype.join.call (arguments, ' ') + ')'; };
  var tt = function (x) { return '(verbatim ' + s (x) + ')'; };
  return '(with "mode" "text" "font-family" "rm" (document ' + [
    line ('(strong ' + s ('Python in the browser') + ')'),
    line (s ('Python 3.14 runs in this page (Pyodide ' + PYODIDE_VERSION + ', '),
          '(hlink ' + s ('pyodide.org') + ' ' + s ('https://pyodide.org') + ')',
          s ('), loaded from jsDelivr by the first input (about 10 MB).')),
    line (s ('The packages which an input imports are loaded then: '), tt ('numpy'), s (', '),
          tt ('sympy'), s (', '), tt ('matplotlib'), s (', '), tt ('pandas'), s (', '), tt ('scipy'),
          s ('... Figures of matplotlib are shown, results of SymPy as formulas.')),
    line (s ('Stop ends Python while it runs: its variables are then lost.')),
    line ('(hlink ' + s ('Help') + ' ' + s (HELP_URL) + ')', s (' (Help > Plug-ins > Python)'))
  ].join (' ') + '))';
}

out (B + 'verbatim:' + B + 'scheme:' + banner () + E + PROMPT + E);
