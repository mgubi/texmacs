// TeXmacs from the JavaScript of its page (a --pre-js of the browser build;
// the C++ side is in src/Plugins/Vue/vue_gui.cpp).
//
// The global TeXmacs, for the JavaScript session (plugins/javascript), the
// file my-init-javascript.js and the console of the browser:
//
//   TeXmacs.scheme ("(+ 1 2)")       "3": a Scheme expression evaluated now,
//                                    its value as text (a string as it is,
//                                    the rest as written by object->string;
//                                    an error as (error ...))
//   TeXmacs.later ("(new-document)") the same, run by the loop of TeXmacs
//                                    after the current event (nothing back)
//   TeXmacs.output ("scheme", "(strong \"hi\")")
//                                    a value which the JavaScript session
//                                    shows as TeXmacs content, in a format
//                                    of the plugins (scheme, html, latex...)
//   TeXmacs.show ("latex", "$x^2$") or TeXmacs.show (value)
//                                    shown at once in the output of the
//                                    input of the JavaScript session which
//                                    runs (asynchronous code shows what it
//                                    makes as it goes); false, and nothing
//                                    shown, when no input runs
//   TeXmacs.module                   the Module of Emscripten
//
// TeXmacs.scheme may not be used while TeXmacs itself runs (from code that
// TeXmacs calls, as (web-javascript ...)): only from events of the page,
// timers, promises, the session or the console.

var TeXmacs = (function () {
  return {
    scheme: function (code) {
      return withStackSave (function () {
        return UTF8ToString (_vue_web_scheme_eval (stringToUTF8OnStack (String (code))));
      });
    },
    later: function (code) {
      withStackSave (function () {
        _vue_web_scheme (stringToUTF8OnStack (String (code)));
      });
    },
    output: function (format, data) {
      return { texmacsOutput: String (format), data: String (data) };
    },
    show: function (format, data) {
      var v = arguments.length > 1 ? TeXmacs.output (format, data) : format;
      if (!TeXmacs.showSink) return false;
      TeXmacs.showSink (v);
      return true;
    },
    // set by the JavaScript session while one of its inputs runs
    showSink: null,
    module: Module
  };
})();
if (typeof window !== 'undefined') window.TeXmacs = TeXmacs;
