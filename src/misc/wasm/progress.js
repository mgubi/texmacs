// The progress of the loading of the page (a --pre-js of the browser build,
// the first): a panel with the icon, the version and a few lines about the
// program, a bar with the share downloaded and the time left, and the list
// of the operations below (waiting, running with its share, or done with
// the time it took), until TeXmacs runs.
//
//   the program     texmacs.wasm, fetched here (Module.instantiateWasm) to
//                   count its bytes, compiled by the browser as they come
//                   (instantiateStreaming); its size, uncompressed, is
//                   written into the page by the build (@TM_WASM_SIZE@ in
//                   shell.html), since a compressed response does not say it
//   the files       the boot package (packages.js reports its bytes)
//   compiling       what is left to compile once the program has come
//   starting        the boot of TeXmacs (Scheme, fonts, the first window):
//                   it holds the page, so the page is painted just before,
//                   through a run dependency of its own
//
// The bar counts the bytes of the program and of the files (as they come
// out of the decompression: the share is what matters, and the time left
// is computed from it); the last two phases have a moving bar. Errors
// (Module.setStatus) stay on the panel.

var tmProgress = (function () {
  var panel = null, bar = null, fill = null, detail = null, rows = {};
  var program = { loaded: 0, total: 0 }, files = { loaded: 0, total: 0 };
  var state = 'loading', failed = false, compiled = false;
  // the operations shown in the panel: when they started and ended (ms)
  var steps = {
    program:  { name: 'The program', start: 0, end: 0 },
    files:    { name: 'The files of TeXmacs', start: 0, end: 0 },
    starting: { name: 'Starting TeXmacs', start: 0, end: 0 }
  };
  var t0 = typeof performance !== 'undefined' ? performance.now () : 0;
  function now () { return performance.now (); }
  function begin (k) { if (!steps[k].start) steps[k].start = now (); }
  function end (k) { begin (k); if (!steps[k].end) steps[k].end = now (); }
  function secs (ms) { return ms < 10000 ? (ms / 1000).toFixed (1) + ' s' : Math.round (ms / 1000) + ' s'; }

  // the version, written into the page by the build (shell.html)
  var version = String (Module['tmVersion'] || '');
  if (/^@/.test (version)) version = '';

  var style = `
    #tm-loading { position:fixed; left:50%; top:42%; transform:translate(-50%,-50%);
      width:420px; max-width:calc(100% - 32px); box-sizing:border-box; padding:20px 24px 18px;
      background:#f4f4f4; border:1px solid #999; border-radius:10px;
      box-shadow:0 6px 24px rgba(0,0,0,.25); z-index:40;
      font:14px -apple-system,"Fira Sans",Helvetica,sans-serif; color:#222 }
    #tm-loading .tm-head { display:flex; align-items:center; gap:14px; margin-bottom:12px }
    #tm-loading .tm-head img { width:56px; height:56px; flex:none }
    #tm-loading .tm-title { font-weight:bold; font-size:18px }
    #tm-loading .tm-version { font-size:12px; color:#555; margin-top:2px }
    #tm-loading .tm-badge { display:inline-block; margin-left:8px; padding:1px 6px;
      font-size:11px; font-weight:normal; color:#1f4e8c; background:#e3eefc; border:1px solid #9cbce8;
      border-radius:8px; vertical-align:middle }
    #tm-loading .tm-about { font-size:12.5px; line-height:1.45; color:#444; margin:0 0 14px }
    #tm-loading .tm-about p { margin:0 0 5px }
    #tm-loading .tm-about a { color:#1f4e8c }
    #tm-loading .tm-bar { height:8px; background:#d4d4d4; border-radius:4px; overflow:hidden;
      position:relative }
    #tm-loading .tm-fill { height:100%; width:0; background:#4a7bc8; border-radius:4px;
      transition:width .15s linear }
    #tm-loading .tm-bar.busy .tm-fill { width:30%; position:absolute;
      animation:tm-busy 1.2s ease-in-out infinite }
    @keyframes tm-busy { from { left:-30% } to { left:100% } }
    #tm-loading .tm-detail { margin-top:6px; font-size:12px; color:#555; min-height:1.3em }
    #tm-loading .tm-steps { list-style:none; margin:10px 0 0; padding:0; font-size:13px }
    #tm-loading .tm-steps li { display:flex; align-items:baseline; gap:8px; padding:2px 0; color:#888 }
    #tm-loading .tm-steps li.run { color:#222; font-weight:600 }
    #tm-loading .tm-steps li.done { color:#444 }
    #tm-loading .tm-mark { width:14px; flex:none; text-align:center }
    #tm-loading .tm-steps li.done .tm-mark { color:#2e7d32 }
    #tm-loading .tm-steps li.run .tm-mark { color:#4a7bc8 }
    #tm-loading .tm-name { flex:1 }
    #tm-loading .tm-info { font-weight:normal; font-size:12px; color:#666;
      font-variant-numeric:tabular-nums }
    #tm-loading.failed .tm-detail { color:#a00; font-size:13px }
  `;

  function build () {
    if (panel || typeof document === 'undefined' || !document.body) return;
    var st = document.createElement ('style');
    st.textContent = style;
    document.head.appendChild (st);
    panel = document.createElement ('div');
    panel.id = 'tm-loading';
    panel.innerHTML =
      '<div class="tm-head"><img src="texmacs-vue-128.png" alt="">' +
      '<div><div class="tm-title">TeXmacs Vue<span class="tm-badge">experimental</span></div>' +
      '<div class="tm-version"></div></div></div>' +
      '<div class="tm-about">' +
      '<p>GNU TeXmacs is a free editor for scientific documents: text and ' +
      'mathematics, typeset as you write them, with structured editing.</p>' +
      '<p>This port runs entirely in the browser: nothing is installed, and ' +
      'your documents stay in the storage of this browser until you download them.</p>' +
      '<p>The first visit downloads the program and its files; the next ones ' +
      'take them from the cache. More at <a href="https://www.texmacs.org" ' +
      'target="_blank" rel="noopener">texmacs.org</a>.</p></div>' +
      '<div class="tm-bar"><div class="tm-fill"></div></div><div class="tm-detail"></div>' +
      '<ul class="tm-steps"></ul>';
    panel.querySelector ('.tm-version').textContent =
      'GNU TeXmacs' + (version ? ' ' + version : '') + ' in the browser';
    bar = panel.querySelector ('.tm-bar');
    fill = panel.querySelector ('.tm-fill');
    detail = panel.querySelector ('.tm-detail');
    var list = panel.querySelector ('.tm-steps');
    Object.keys (steps).forEach (function (k) {
      var li = document.createElement ('li');
      li.innerHTML = '<span class="tm-mark"></span><span class="tm-name"></span><span class="tm-info"></span>';
      li.querySelector ('.tm-name').textContent = steps[k].name;
      list.appendChild (li);
      rows[k] = li;
    });
    document.body.appendChild (panel);
    var old = document.getElementById ('status');
    if (old) old.remove ();
    render ();
  }

  function pct (x) { return x.total ? Math.min (100, Math.floor (100 * x.loaded / x.total)) + '%' : ''; }

  // a step: waiting (a dot), running (an arrow and what it does), or done
  // (a check and the time it took)
  function row (k, info) {
    var st = steps[k], li = rows[k];
    var cls = st.end ? 'done' : st.start ? 'run' : '';
    li.className = cls;
    li.querySelector ('.tm-mark').textContent = st.end ? '✓' : st.start ? '▸' : '·';
    li.querySelector ('.tm-info').textContent =
      st.end ? secs (st.end - st.start) : st.start ? info : '';
  }

  function render () {
    build ();
    if (!panel || failed) return;
    var total = program.total + files.total, loaded = program.loaded + files.loaded;
    var downloading = state === 'loading' && (program.loaded < program.total || !program.total ||
                                              files.loaded < files.total || !files.total);
    if (downloading) {
      bar.classList.toggle ('busy', !total);
      fill.style.width = total ? Math.min (100, 100 * loaded / total).toFixed (1) + '%' : '';
      // the time left, from the share of the bytes which came so far
      var spent = now () - Math.min (steps.program.start || now (), steps.files.start || now ());
      var share = total ? loaded / total : 0;
      var left = share > 0.03 && spent > 500 ? spent * (1 - share) / share : 0;
      detail.textContent = 'Downloading' + (total ? ' — ' + Math.floor (100 * share) + '%' : '…') +
        (left ? ', about ' + (left < 60000 ? Math.ceil (left / 1000) + ' s' : Math.ceil (left / 60000) + ' min') + ' left' : '');
    } else {
      bar.classList.add ('busy');
      fill.style.width = '';
      // the boot holds the page: nothing moves until it is over
      detail.textContent = state === 'starting' ? 'Downloaded — starting, a few seconds' :
        'Downloaded — compiling the program';
    }
    // the shares only: the bytes counted are those out of the decompression,
    // several times what travels
    row ('program', program.total && program.loaded >= program.total ?
         'compiling…' : 'downloading ' + pct (program));
    row ('files', files.total && files.loaded >= files.total ? 'waiting for the program' :
         'downloading ' + pct (files));
    row ('starting', 'Scheme, fonts, first window');
  }

  function set (s) {
    if (state === 'done' || state === s) return;
    state = s;
    if (s === 'starting') { end ('program'); end ('files'); begin ('starting'); }
    console.log ('TeXmacs: ' + s + ' (' + Math.round (performance.now ()) + ' ms)');
    render ();
  }
  var onRunning = []; // what waits for TeXmacs to run (tmProgress.running)
  function hide () {
    console.log ('TeXmacs: running (' + Math.round (performance.now ()) + ' ms)');
    state = 'done';
    if (panel && !failed) { panel.remove (); panel = null; }
    onRunning.splice (0).forEach (function (f) { f (); });
  }
  function error (text) {
    build ();
    failed = true;
    if (!panel) return;
    panel.classList.add ('failed');
    bar.style.display = 'none';
    detail.textContent = text + ' Reload the page to try again.';
  }

  if (typeof document !== 'undefined') {
    if (document.readyState === 'loading') document.addEventListener ('DOMContentLoaded', render);
    else render ();
  }

  // the program: its bytes counted as the browser compiles them
  program.total = Number (Module['tmWasmSize']) || 0;
  if (typeof WebAssembly !== 'undefined' && WebAssembly.instantiateStreaming &&
      typeof TransformStream !== 'undefined' && typeof document !== 'undefined') {
    // texmacs.wasm.gz, decompressed here, when the browser can: the servers
    // of static files (GitHub Pages) may not compress the program (22 MB,
    // 7 MB in gzip); texmacs.wasm otherwise, or if there is no gzip copy
    var gunzip = typeof DecompressionStream !== 'undefined';
    var fetchProgram = function () {
      var base = new URL ('texmacs.wasm', document.baseURI).href;
      var plain = function () { return fetch (base, { credentials: 'same-origin' }); };
      if (!gunzip) return plain ();
      return fetch (base + '.gz', { credentials: 'same-origin' }).then (function (resp) {
        if (!resp.ok) return plain ();
        return new Response (resp.body.pipeThrough (new DecompressionStream ('gzip')));
      }, plain);
    };
    Module['instantiateWasm'] = function (imports, receive) {
      begin ('program');
      fetchProgram ().then (function (resp) {
        if (!resp.ok) throw new Error ('cannot load texmacs.wasm: ' + resp.status);
        if (!program.total && !resp.headers.get ('content-encoding'))
          program.total = Number (resp.headers.get ('content-length')) || 0;
        var counted = resp.body.pipeThrough (new TransformStream ({
          transform: function (chunk, out) {
            program.loaded += chunk.length;
            if (program.loaded > program.total) program.total = program.loaded;
            render ();
            out.enqueue (chunk);
          },
          flush: function () {
            program.total = program.loaded;
            // the files may still come: they have the bar, else compiling
            if (files.total && files.loaded >= files.total) set ('compiling');
            else render ();
          }
        }));
        return WebAssembly.instantiateStreaming (
          new Response (counted, { headers: { 'Content-Type': 'application/wasm' } }), imports);
      }).then (function (r) {
        compiled = true;
        end ('program');
        render ();
        receive (r.instance, r.module);
      }).catch (function (e) {
        error ('Cannot load the program of TeXmacs: ' + e.message);
        console.error (e);
      });
      return {};
    };
  }

  // the page is painted before the boot of TeXmacs, which holds it
  var waiting = false, seenMore = false;
  Module['preRun'] = Module['preRun'] || [];
  Module['preRun'].push (function () {
    addRunDependency ('tm-paint');
    waiting = true;
  });
  Module['monitorRunDependencies'] = function (left) {
    if (left > 1) seenMore = true;
    if (waiting && seenMore && left === 1) {
      waiting = false;
      set ('starting');
      requestAnimationFrame (function () {
        requestAnimationFrame (function () { removeRunDependency ('tm-paint'); });
      });
    }
  };
  // Emscripten says 'Running...' before the boot and '' after it
  Module['setStatus'] = function (text) {
    if (text === '') hide ();
    else if (text !== 'Running...' && !/^Loading TeXmacs/.test (text)) error (text);
  };

  return {
    // the bytes of the boot package (packages.js)
    files: function (loaded, total) {
      begin ('files');
      files.loaded = loaded; files.total = total;
      if (total && loaded >= total) end ('files');
      if (state === 'loading' && program.total && program.loaded >= program.total &&
          loaded >= total && !compiled) set ('compiling');
      else render ();
    },
    error: error,
    // f is called once TeXmacs runs (now if it does)
    running: function (f) { if (state === 'done') f (); else onRunning.push (f); }
  };
})();
