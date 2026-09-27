// The progress of the loading of the page (a --pre-js of the browser build,
// the first): a panel with the phase and a bar, until TeXmacs runs.
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
// The bar counts the bytes of the program and of the files; the last two
// phases have a moving bar. Errors (Module.setStatus) stay on the panel.

var tmProgress = (function () {
  var panel = null, phase = null, detail = null, bar = null, fill = null;
  var program = { loaded: 0, total: 0 }, files = { loaded: 0, total: 0 };
  var state = 'loading', failed = false, compiled = false;

  function mb (n) { return (n / 1e6).toFixed (1); }

  var style = `
    #tm-loading { position:fixed; left:50%; top:40%; transform:translate(-50%,-50%);
      width:360px; max-width:calc(100% - 32px); box-sizing:border-box; padding:20px 24px;
      background:#f4f4f4; border:1px solid #999; border-radius:8px;
      box-shadow:0 6px 24px rgba(0,0,0,.25); z-index:40;
      font:14px -apple-system,"Fira Sans",Helvetica,sans-serif; color:#222 }
    #tm-loading .tm-title { font-weight:bold; font-size:17px; margin-bottom:4px }
    #tm-loading .tm-about { font-size:12px; color:#555; margin-bottom:12px }
    #tm-loading .tm-badge { display:inline-block; margin-left:8px; padding:1px 6px;
      font-size:11px; font-weight:normal; color:#8a4b00; background:#ffe9c7; border:1px solid #e8b56b;
      border-radius:8px; vertical-align:middle }
    #tm-loading .tm-phase { margin-bottom:8px }
    #tm-loading .tm-bar { height:8px; background:#d4d4d4; border-radius:4px; overflow:hidden;
      position:relative }
    #tm-loading .tm-fill { height:100%; width:0; background:#4a7bc8; border-radius:4px;
      transition:width .15s linear }
    #tm-loading .tm-bar.busy .tm-fill { width:30%; position:absolute;
      animation:tm-busy 1.2s ease-in-out infinite }
    @keyframes tm-busy { from { left:-30% } to { left:100% } }
    #tm-loading .tm-detail { margin-top:8px; font-size:12px; color:#555; min-height:1.3em }
    #tm-loading.failed .tm-phase { color:#a00 }
  `;

  function build () {
    if (panel || typeof document === 'undefined' || !document.body) return;
    var st = document.createElement ('style');
    st.textContent = style;
    document.head.appendChild (st);
    panel = document.createElement ('div');
    panel.id = 'tm-loading';
    panel.innerHTML = '<div class="tm-title">TeXmacs Vue<span class="tm-badge">experimental</span></div>' +
      '<div class="tm-about">An experimental port of GNU TeXmacs to the browser, ' +
      'with OpenType fonts</div><div class="tm-phase"></div>' +
      '<div class="tm-bar"><div class="tm-fill"></div></div><div class="tm-detail"></div>';
    phase = panel.querySelector ('.tm-phase');
    bar = panel.querySelector ('.tm-bar');
    fill = panel.querySelector ('.tm-fill');
    detail = panel.querySelector ('.tm-detail');
    document.body.appendChild (panel);
    var old = document.getElementById ('status');
    if (old) old.remove ();
    render ();
  }

  function render () {
    build ();
    if (!panel || failed) return;
    if (state === 'loading') {
      var total = program.total + files.total, loaded = program.loaded + files.loaded;
      phase.textContent = program.loaded < program.total || !program.total ?
        'Downloading the program…' : 'Downloading the files of TeXmacs…';
      bar.classList.toggle ('busy', !total);
      fill.style.width = total ? Math.min (100, 100 * loaded / total).toFixed (1) + '%' : '';
      var parts = [];
      if (program.total) parts.push ('program ' + mb (program.loaded) + ' of ' + mb (program.total) + ' MB');
      if (files.total) parts.push ('files ' + mb (files.loaded) + ' of ' + mb (files.total) + ' MB');
      detail.textContent = parts.join (' · ');
    } else {
      phase.textContent = state === 'compiling' ? 'Compiling the program…' : 'Starting TeXmacs…';
      bar.classList.add ('busy');
      fill.style.width = '';
      detail.textContent = state === 'compiling' ? 'by the browser, once per version of TeXmacs' :
                           'the Scheme code, the fonts and the first window';
    }
  }

  function set (s) {
    if (state === 'done' || state === s) return;
    state = s;
    console.log ('TeXmacs: ' + s + ' (' + Math.round (performance.now ()) + ' ms)');
    render ();
  }
  function hide () {
    console.log ('TeXmacs: running (' + Math.round (performance.now ()) + ' ms)');
    state = 'done';
    if (panel && !failed) { panel.remove (); panel = null; }
  }
  function error (text) {
    build ();
    failed = true;
    if (!panel) return;
    panel.classList.add ('failed');
    phase.textContent = text;
    bar.style.display = 'none';
    detail.textContent = 'Reload the page to try again.';
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
      files.loaded = loaded; files.total = total;
      if (state === 'loading' && program.total && program.loaded >= program.total &&
          loaded >= total && !compiled) set ('compiling');
      else render ();
    },
    error: error
  };
})();
