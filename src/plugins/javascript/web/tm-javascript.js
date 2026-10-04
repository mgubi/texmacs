// The JavaScript session of TeXmacs in the browser (plugins/javascript): the
// JavaScript of the page itself, so that a session sees and changes what
// runs there (the page, the Module of Emscripten, TeXmacs through
// TeXmacs.scheme, misc/wasm/javascript.js).
//
// A plugin of the page (misc/wasm/workers.js, (:worker "page:...")): this
// file is the body of a function of tm, which returns the plugin. It gets
// the input of the session in {input} messages, out of TeXmacs, and answers
// with tm.post ({out}, {err}) in the protocol of the plugins.
//
// The code is evaluated by an indirect eval, in the global scope: var and
// function declarations stay for the next inputs (let and const only within
// one input). A promise is waited for; code with await is run as the body of
// an async function, whose value is the one it returns. What console.log
// and the others write meanwhile is shown in the session too.

var B = '\x02', E = '\x05';
var PROMPT = B + 'prompt#js] ' + E;
var LIMIT = 100000;             // the longest text shown for one value
var decoder = new TextDecoder ();
var input = '';
var queue = Promise.resolve ();
var generation = 0;             // an interrupt forgets what was running
var running = false;            // an answer is open (its prompt not sent)

function out (s) { tm.post ({ out: s }); }
function clean (s) { return String (s).replace (/[\x02\x05]/g, ''); }
function text (s) {
  s = clean (s);
  if (s.length > LIMIT) s = s.slice (0, LIMIT) + '\n... (' + (s.length - LIMIT) + ' more characters)';
  return B + 'utf8:' + s + E;
}
function schemeString (s) {
  return '"' + String (s).replace (/\\/g, '\\\\').replace (/"/g, '\\"') + '"';
}

/******************************************************************************
* Values as text
******************************************************************************/

function describe (v, depth, seen) {
  var t = typeof v;
  if (v === null) return 'null';
  if (t === 'undefined') return 'undefined';
  if (t === 'string') return depth === 0 ? JSON.stringify (v) : JSON.stringify (v);
  if (t === 'number' || t === 'boolean') return String (v);
  if (t === 'bigint') return String (v) + 'n';
  if (t === 'symbol') return v.toString ();
  if (t === 'function') {
    var src = Function.prototype.toString.call (v);
    var head = src.split ('\n') [0];
    return head.length < 120 && src.indexOf ('\n') < 0 ? src : head.replace (/\{\s*$/, '') + '{...}';
  }
  if (typeof Node !== 'undefined' && v instanceof Node) {
    if (v.nodeType === 1) {
      var h = v.outerHTML || '';
      var open = h.slice (0, h.indexOf ('>') + 1);
      return open + (v.childNodes.length ? '...</' + v.tagName.toLowerCase () + '>' : '');
    }
    return '#' + v.nodeName + (v.nodeValue ? ' ' + JSON.stringify (v.nodeValue) : '');
  }
  if (v instanceof Error) return v.name + ': ' + v.message;
  if (v instanceof Date) return 'Date ' + (isNaN (v) ? 'Invalid' : v.toISOString ());
  if (v instanceof RegExp) return String (v);
  if (seen.indexOf (v) >= 0) return '[circular]';
  if (depth >= 3) return Array.isArray (v) ? '[...]' : '{...}';
  seen = seen.concat ([v]);
  var pad = '  '.repeat (depth + 1), end = '  '.repeat (depth);
  var parts = [], more = 0, i = 0;
  function add (s) { if (parts.length < 100) parts.push (s); else more++; }
  if (Array.isArray (v) || ArrayBuffer.isView (v)) {
    for (i = 0; i < v.length; i++) add (describe (v[i], depth + 1, seen));
    if (more) parts.push ('... ' + more + ' more');
    var flat = '[' + parts.join (', ') + ']';
    return flat.length < 80 ? flat : '[\n' + pad + parts.join (',\n' + pad) + '\n' + end + ']';
  }
  if (v instanceof Map) {
    v.forEach (function (val, key) { add (describe (key, depth + 1, seen) + ' => ' + describe (val, depth + 1, seen)); });
    return 'Map {' + parts.join (', ') + (more ? ', ... ' + more + ' more' : '') + '}';
  }
  if (v instanceof Set) {
    v.forEach (function (val) { add (describe (val, depth + 1, seen)); });
    return 'Set {' + parts.join (', ') + (more ? ', ... ' + more + ' more' : '') + '}';
  }
  var name = v.constructor && v.constructor !== Object && v.constructor.name ? v.constructor.name + ' ' : '';
  var keys;
  try { keys = Object.keys (v); } catch (e) { return name + '{?}'; }
  keys.forEach (function (k) {
    var val;
    try { val = v[k]; } catch (e) { val = '[' + e.name + ']'; }
    add ((/^[A-Za-z_$][\w$]*$/.test (k) ? k : JSON.stringify (k)) + ': ' + describe (val, depth + 1, seen));
  });
  if (more) parts.push ('... ' + more + ' more');
  if (parts.length == 0) return name + '{}';
  var line = name + '{' + parts.join (', ') + '}';
  return line.length < 80 ? line : name + '{\n' + pad + parts.join (',\n' + pad) + '\n' + end + '}';
}

// the answer for a value: nothing for undefined, TeXmacs content for the
// values of TeXmacs.output, text for the others
function answer (v) {
  if (v === undefined) return '';
  if (v && typeof v === 'object' && v.texmacsOutput && typeof v.data === 'string')
    return B + v.texmacsOutput + ':' + clean (v.data) + E;
  return text (describe (v, 0, []));
}

// an exception: its message, and where it happened in the input (the first
// line of its stack in the evaluated code: "eval:1:13" in Firefox,
// "<anonymous>:1:13" in Chrome), one line less in code run with await
function errorText (e, shift) {
  if (e instanceof Error) {
    var s = e.name + ': ' + e.message;
    var where = (e.stack || '').split ('\n').filter (function (l) { return /eval|<anonymous>/.test (l); }) [0];
    var m = where && /(\d+):(\d+)\)?\s*$/.exec (where);
    if (m && +m[1] - shift >= 1)
      s += ' (line ' + (+m[1] - shift) + ', column ' + m[2] + ' of the input)';
    return s;
  }
  return 'Uncaught ' + describe (e, 0, []);
}

/******************************************************************************
* The console, while an input runs
******************************************************************************/

var METHODS = ['log', 'info', 'debug', 'warn', 'error'];

function capture (gen) {
  var saved = {};
  METHODS.forEach (function (m) {
    saved[m] = console[m];
    console[m] = function () {
      saved[m].apply (console, arguments);
      if (gen !== generation) return;
      var line = Array.prototype.map.call (arguments, function (a) {
        return typeof a === 'string' ? a : describe (a, 0, []);
      }).join (' ');
      if (m === 'warn' || m === 'error') tm.post ({ err: text (line + '\n') });
      else out (text (line + '\n'));
    };
  });
  return function () { METHODS.forEach (function (m) { console[m] = saved[m]; }); };
}

/******************************************************************************
* Evaluation
******************************************************************************/

function evaluate (code, gen) {
  if (gen !== generation) return Promise.resolve (); // dropped by an interrupt
  running = true;
  out (B + 'verbatim:');
  var release = capture (gen);
  var shift = 0;   // the lines added before the input (await)
  return new Promise (function (done) {
    function finish (ok, v) {
      release ();
      if (gen === generation) {
        if (ok) out (answer (v));
        else tm.post ({ err: text (errorText (v, shift)) });
        out (PROMPT + E);
        running = false;
      }
      done ();
    }
    var v;
    try { v = (0, eval) (code); }
    catch (e) {
      // code with await: the body of an async function
      if (e instanceof SyntaxError && /\bawait\b/.test (code)) {
        shift = 1;
        try { v = (0, eval) ('(async function () {\n' + code + '\n}) ()'); }
        catch (e2) { finish (false, e2); return; }
      }
      else { finish (false, e); return; }
    }
    if (v && typeof v.then === 'function')
      v.then (function (r) { finish (true, r); }, function (e) { finish (false, e); });
    else finish (true, v);
  });
}

/******************************************************************************
* The session
******************************************************************************/

// its help page, in the help of TeXmacs (Help > Plug-ins > JavaScript)
var HELP_URL = 'tmfs://help/article/tm/plugins/javascript/doc/javascript-session.en.tm';

function banner () {
  var s = schemeString;
  function line () { return '(concat ' + Array.prototype.join.call (arguments, ' ') + ')'; }
  function tt (x) { return '(verbatim ' + s (x) + ')'; }
  var where = typeof navigator !== 'undefined' ? navigator.userAgent : '';
  return '(with "mode" "text" "font-family" "rm" (document ' + [
    line ('(strong ' + s ('JavaScript of this page') + ')', s (' (the interpreter of the browser which runs TeXmacs)')),
    line (s ('Everything of the page is at hand: '), tt ('document'), s (', '), tt ('window'),
          s (', the Module of TeXmacs '), tt ('TeXmacs.module'), s ('; and TeXmacs itself: '),
          tt ('TeXmacs.scheme ("(+ 1 2)")'), s (' evaluates Scheme now and gives its value as text, '),
          tt ('TeXmacs.later ("(new-document)")'), s (' runs it after the current input.')),
    line (s ('Return evaluates, Shift+Return starts a new line. '), tt ('var'), s (' and '), tt ('function'),
          s (' declarations stay for the next inputs, '), tt ('let'), s (' and '), tt ('const'),
          s (' do not. A promise is waited for; with '), tt ('await'), s (', the value is the one of '),
          tt ('return'), s ('.')),
    line (tt ('TeXmacs.output ("scheme", "(strong \\"hi\\")")'), s (' shows TeXmacs content ("html" and "latex" too).')),
    line (s ('JavaScript run at each start: Developer > Open my-init-javascript.js (with Tools > Developer tool).')),
    line ('(hlink ' + s ('Examples and help') + ' ' + s (HELP_URL) + ')', s (' (Help > Plug-ins > JavaScript)')),
    line (s (where))
  ].join (' ') + '))';
}

out (B + 'verbatim:' + B + 'scheme:' + banner () + E + PROMPT + E);

return {
  message: function (m) {
    if (m.interrupt) {
      generation++;
      if (running) {
        tm.post ({ err: text ('Interrupted (the code which was running goes on, its result is not shown)\n') });
        out (PROMPT + E);
        running = false;
      }
      queue = Promise.resolve ();
      return;
    }
    if (!m.input) return;
    input += decoder.decode (m.input, { stream: true });
    var i;
    // the serializer of the plugin ends an input by a line <EOF>
    while ((i = input.indexOf ('\n<EOF>\n')) >= 0) {
      var code = input.slice (0, i);
      input = input.slice (i + 7);
      queue = queue.then (function (c, g) { return function () { return evaluate (c, g); }; } (code, generation));
    }
  }
};
