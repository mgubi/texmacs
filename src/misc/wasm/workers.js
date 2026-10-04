// The plugins of TeXmacs which are Web Workers (a --pre-js of the browser
// build; the C++ side is src/System/Link/worker_link.cpp).
//
// A page has no processes, so the plugin of a session which needs one is a
// worker: a script which runs apart from the page, and to which the page
// talks with messages, as TeXmacs talks to a program through its pipes. A
// plugin says so with (:worker "url") in its plugin-configure, the url being
// that of its script, relative to the page.
//
// The messages, which are those of a program:
//
//   page -> worker   {input: Uint8Array}   what the program reads (stdin)
//                    {interrupt: true}     what Control-C would be
//   worker -> page   {out: string}         what it writes (stdout), in the
//                                          protocol of the plugins: \2 the
//                                          format, the data, \5...
//                    {err: string}         its errors (stderr)
//                    {exit: code}          it ended
//
// What a worker sends is kept here until TeXmacs takes it (tmWorkers.take,
// at each pass of the interpose handler of the server), and the loop of the
// page is woken up, which otherwise sleeps while nothing happens.

var tmWorkers = (function () {
  var workers = {}, next = 1;
  var encoder = typeof TextEncoder !== 'undefined' ? new TextEncoder () : null;

  function bytes (x) {
    if (x instanceof Uint8Array) return x;
    if (x instanceof ArrayBuffer) return new Uint8Array (x);
    return encoder.encode (String (x));
  }
  function keep (w, channel, data) {
    w.pending[channel].push (bytes (data));
    if (typeof _vue_web_wake !== 'undefined') _vue_web_wake ();
  }

  return {
    // a new worker, its number (or -1)
    start: function (url) {
      var w;
      try { w = { worker: new Worker (url), pending: [[], []], alive: true }; }
      catch (e) {
        console.error ('TeXmacs: cannot start the worker ' + url + ': ' + e);
        return -1;
      }
      var id = next++;
      workers[id] = w;
      w.worker.onmessage = function (e) {
        var m = e.data || {};
        if (m.out !== undefined) keep (w, 0, m.out);
        if (m.err !== undefined) keep (w, 1, m.err);
        if (m.exit !== undefined) {
          w.alive = false;
          w.worker.terminate ();
          if (typeof _vue_web_wake !== 'undefined') _vue_web_wake ();
        }
      };
      w.worker.onerror = function (e) {
        // a script which fails to load or throws: its error, then it ends
        keep (w, 1, 'Error in the worker ' + url + ': ' + (e.message || e) + '\n');
        w.alive = false;
        w.worker.terminate ();
        e.preventDefault ();
      };
      return id;
    },
    write: function (id, data) {
      var w = workers[id];
      if (w && w.alive) w.worker.postMessage ({ input: data }, [data.buffer]);
    },
    // what the worker sent on a channel (0: out, 1: err) since last time,
    // in one array, or null
    take: function (id, channel) {
      var w = workers[id];
      if (!w || w.pending[channel].length == 0) return null;
      var parts = w.pending[channel], n = 0;
      w.pending[channel] = [];
      parts.forEach (function (p) { n += p.length; });
      var r = new Uint8Array (n), at = 0;
      parts.forEach (function (p) { r.set (p, at); at += p.length; });
      return r;
    },
    alive: function (id) {
      var w = workers[id];
      return !!(w && (w.alive || w.pending[0].length || w.pending[1].length));
    },
    interrupt: function (id) {
      var w = workers[id];
      if (w && w.alive) w.worker.postMessage ({ interrupt: true });
    },
    stop: function (id) {
      var w = workers[id];
      if (!w) return;
      w.worker.terminate ();
      delete workers[id];
    }
  };
})();
