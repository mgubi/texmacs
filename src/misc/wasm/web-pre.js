// --pre-js of the browser build: where TeXmacs finds its files.
//
// TeXmacs/ is packaged at /texmacs (see the "web" target of
// misc/wasm/Makefile). The home directory /home/web is kept in the
// IndexedDB of the page (IDBFS): it is read before TeXmacs starts, and each
// file which changes is written back to it a moment later (tmHome below),
// so that the preferences and the documents of the user survive a reload.
// One tab of the browser writes it; the other tabs of the page show it
// without keeping their changes, until the user moves TeXmacs to them.

// The options of the address of the page (texmacs.html?a&b=...; the list is
// in the dialog "Address of the page", frame.js) which are options of the
// command line of TeXmacs: ?debug=<flags> (-debug-<flag>, several joined
// with commas: events,io,keyboard...; std is -d) and ?verbose (-V).
// ?open and ?x are for once TeXmacs runs (files.js).
var tmAddress = new URLSearchParams (typeof location !== 'undefined' ? location.search : '');
(function () {
  var flags = ['std', 'all', 'events', 'io', 'sockets', 'gnutls', 'bench', 'history',
               'keyboard', 'packrat', 'flatten', 'parser', 'correct', 'convert',
               'remote', 'live'];
  var args = [];
  tmAddress.getAll ('debug').forEach (function (v) {
    v.split (',').forEach (function (f) {
      f = f.trim ();
      if (f === '') return;
      if (flags.indexOf (f) < 0) console.warn ('TeXmacs: no debugging flag ' + f);
      else args.push (f === 'std' ? '-d' : '-debug-' + f);
    });
  });
  if (tmAddress.has ('verbose')) args.push ('-V');
  if (args.length > 0) Module['arguments'] = (Module['arguments'] || []).concat (args);
})();

Module['preRun'] = Module['preRun'] || [];
Module['preRun'].push(function () {
  // ?trace-files: the files of /texmacs which TeXmacs opens, in the order
  // it opens them (window.tmTrace), to choose the files needed at boot
  if (tmAddress.has ('trace-files')) {
    var seen = {}, open = FS.open;
    window.tmTrace = [];
    FS.open = function (path, flags, mode) {
      var p = typeof path === 'string' ? path : '';
      if (p.startsWith ('/texmacs/') && !seen[p]) { seen[p] = true; window.tmTrace.push (p); }
      return open.apply (FS, arguments);
    };
  }
  ENV['TEXMACS_PATH'] = '/texmacs';
  ENV['HOME'] = '/home/web';
  ENV['TEXMACS_HOME_PATH'] = '/home/web/.TeXmacs';
  ENV['LANG'] = 'en_US.UTF-8';
  // the windows of TeXmacs are the tabs of the page (vue_gui.cpp, where
  // this is always so in the browser): for Scheme (tm-view.scm)
  ENV['TEXMACS_VUE_SINGLE_WINDOW'] = '1';
  // ?env=NAME=VALUE (several may be given): a variable of the environment
  // of TeXmacs, for debugging (TEXMACS_FL_TRACE=catch, say)
  for (const e of tmAddress.getAll ('env')) {
    const i = e.indexOf ('=');
    if (i > 0) ENV[e.slice (0, i)] = e.slice (i + 1);
  }
  // the look and feel of TeXmacs follows the platform of the browser, whose
  // shortcuts are Cmd+... on a Mac (src/Kernel/Abstractions/basic.cpp)
  var platform = (typeof navigator === 'undefined') ? '' :
    (navigator.userAgentData && navigator.userAgentData.platform) || navigator.platform || '';
  ENV['TEXMACS_WEB_PLATFORM'] = /mac|iphone|ipad/i.test (platform) ? 'macos' :
                                /win/i.test (platform) ? 'windows' : 'other';
  // texmacs.html?profile=<n>: the profile of the loop, every n frames, in
  // the console (TEXMACS_VUE_PROFILE, see vue_profile_frame in vue_gui.cpp)
  var prof = /^\d+$/.exec (tmAddress.get ('profile') || '');
  if (prof) ENV['TEXMACS_VUE_PROFILE'] = prof[0];
  // drawn by WebGL2 (vue_gpu.cpp) in a build with ThorVG, unless the
  // address says texmacs.html?gpu=0 or the browser has no WebGL2 (the window
  // of SDL would then have no surface for MuPDF to draw on)
  if (tmAddress.get ('gpu') !== '0') {
    var webgl2 = false;
    try {
      var probe = document.createElement ('canvas');
      webgl2 = !!(probe.getContext && probe.getContext ('webgl2'));
    } catch (e) {}
    if (webgl2) ENV['TEXMACS_VUE_GPU'] = '1';
    else console.warn ('TeXmacs: no WebGL2, drawing with MuPDF');
  }
  // ?bars=top: the main and mode icon bars above the document, rather than
  // in a column at its left (TEXMACS_VUE_BARS, see in_side_bar in
  // vue_widget.cpp)
  if (tmAddress.get ('bars') === 'top') ENV['TEXMACS_VUE_BARS'] = 'top';
  // ?slug=1: the glyphs drawn from their outlines (vue_gpu.cpp, Slug)
  if (tmAddress.get ('slug') === '1') ENV['TEXMACS_VUE_SLUG'] = '1';
  // ?gpusync=1: the profile waits for the GPU (vue_gpu.cpp, gpu_finish)
  if (tmAddress.get ('gpusync') === '1') ENV['TEXMACS_VUE_GPU_SYNC'] = '1';
  FS.mkdirTree ('/home/web');
  FS.mount (IDBFS, { autoPersist: false }, '/home/web');
  addRunDependency ('home');
  tmHome.claim (function () {
    FS.syncfs (true, function (err) {
      if (err) console.error ('TeXmacs: cannot read the saved home directory', err);
      // from here on, the changes of the home directory are written back
      tmHome.track ();
      try { FS.mkdirTree ('/home/web/.TeXmacs'); } catch (e) {}
      // the temporary files of the previous sessions: TeXmacs empties its
      // temporary directory when it quits, which a page never does (and its
      // process has always the same number: they piled up in the storage)
      tmRemoveTree ('/home/web/.TeXmacs/system/tmp', false);
      removeRunDependency ('home');
    });
  });
});

// remove a directory of the file system of the page: its contents, and the
// directory itself when self is true
function tmRemoveTree (path, self) {
  var st;
  try { st = FS.stat (path); } catch (e) { return; }
  if (FS.isDir (st.mode)) {
    FS.readdir (path).forEach (function (x) {
      if (x !== '.' && x !== '..') tmRemoveTree (path + '/' + x, true);
    });
    if (self) try { FS.rmdir (path); } catch (e) {}
  }
  else try { FS.unlink (path); } catch (e) {}
}

// The home directory in IndexedDB.
//
// Writing: the functions of FS which change files (write, truncate, open
// for writing, mkdir, symlink, rename, unlink, rmdir, chmod, utime) note the
// paths of the home directory they touch, and a moment later these entries
// alone are written to the database of IDBFS, or deleted from it, in one
// transaction, in the format of IDBFS.syncfs (which reads the directory at
// the start). The whole directory and the whole database were compared
// every 5 seconds before, which grew with the files of the user and lost
// the last 5 seconds when the browser stopped.
//
// One tab: the tab which holds the lock "texmacs-home" (Web Locks) writes
// the home directory. Another tab of the page reads it, but keeps none of
// its changes (they would overwrite those of the first tab: each tab has
// its own copy in memory), and says so; "Use TeXmacs here" asks the first
// tab (BroadcastChannel) to write its last changes and let go of the lock,
// and reloads, so that the tab starts again from what was written, and
// waits for the lock. When the first tab is closed, the others say that a
// reload brings TeXmacs to them.
//
// tmSaveHome () writes the changes now (files.js calls it after a change);
// never again once the page removed its storage (tmFrame, "Remove from this
// browser"), or it would write it back.
var tmSaveHome, tmStorageRemoved = false;
var tmHome = (function () {
  var HOME = '/home/web', LOCK = 'texmacs-home', TAKEOVER = 'texmacs-home-takeover';
  var readOnly = false, release = null, channel = null, notice = null;
  var dirty = new Set (), tracking = false, timer = null, busy = false, again = false;
  var hasLocks = typeof navigator !== 'undefined' && navigator.locks &&
                 typeof BroadcastChannel !== 'undefined';

  function inHome (p) { return p === HOME || p.startsWith (HOME + '/'); }

  function mark (p) {
    if (!tracking || typeof p !== 'string') return;
    if (p.charAt (0) === '/' && !p.startsWith (HOME)) return; // most of them
    try { p = PATH_FS.resolve (p); } catch (e) { return; }
    if (!inHome (p)) return;
    dirty.add (p);
    if (!timer) timer = setTimeout (function () { timer = null; flush (); }, 300);
  }

  // every path of a tree (a directory which is renamed or removed)
  function markTree (p) {
    mark (p);
    var st;
    try { st = FS.lstat (p); } catch (e) { return; }
    if (FS.isDir (st.mode))
      FS.readdir (p).forEach (function (x) {
        if (x !== '.' && x !== '..') markTree (p + '/' + x);
      });
  }

  function writes (flags) {
    if (typeof flags === 'string') return /[wa+]/.test (flags);
    return (flags & 3) !== 0 || (flags & 512) !== 0; // O_WRONLY, O_RDWR, O_TRUNC
  }

  function wrap (name, before, after) {
    var f = FS[name];
    if (typeof f !== 'function') return false;
    FS[name] = function () {
      if (before) try { before.apply (null, arguments); } catch (e) {}
      var r = f.apply (FS, arguments);
      if (after) try { after.apply (null, [r].concat (Array.prototype.slice.call (arguments))); } catch (e) {}
      return r;
    };
    return true;
  }

  function streamPath (fd) {
    try { return FS.getStreamChecked (fd).path; } catch (e) { return null; }
  }

  // the changes are noted once the home directory was read (its reading
  // writes every file into memory, which changes nothing in the database)
  function track () {
    var ok = true;
    ok = wrap ('write', null, function (r, stream) { mark (stream.path); }) && ok;
    ok = wrap ('open', null, function (r, path, flags) {
      if (writes (flags || 0)) mark (r.path || path);
    }) && ok;
    ok = wrap ('truncate', null, function (r, path) { mark (path); }) && ok;
    ok = wrap ('ftruncate', null, function (r, fd) { mark (streamPath (fd)); }) && ok;
    ok = wrap ('mkdir', null, function (r, path) { mark (path); }) && ok;
    ok = wrap ('symlink', null, function (r, oldpath, newpath) { mark (newpath); }) && ok;
    ok = wrap ('rename', function (oldp) { markTree (oldp); },
                         function (r, oldp, newp) { markTree (newp); }) && ok;
    ok = wrap ('unlink', null, function (r, path) { mark (path); }) && ok;
    ok = wrap ('rmdir', null, function (r, path) { mark (path); }) && ok;
    ok = wrap ('chmod', null, function (r, path) { mark (path); }) && ok;
    ok = wrap ('utime', null, function (r, path) { mark (path); }) && ok;
    if (!ok) {
      // a FS without these functions: the whole directory, as before
      console.warn ('TeXmacs: the home directory is saved as a whole');
      setInterval (function () { saveAll (); }, 5000);
    }
    tracking = true;
  }

  function saveAll () {
    if (readOnly || tmStorageRemoved || !runtimeInitialized) return;
    FS.syncfs (false, function (err) {
      if (err) console.error ('TeXmacs: cannot save the home directory', err);
    });
  }

  // the noted entries to the database, in one transaction
  function flush (done) {
    if (timer) { clearTimeout (timer); timer = null; }
    if (readOnly || tmStorageRemoved || dirty.size === 0) { if (done) done (); return; }
    if (busy) { again = true; if (done) setTimeout (function () { flush (done); }, 50); return; }
    var paths = Array.from (dirty).sort ();
    dirty.clear ();
    busy = true;
    var failed = false;
    function fail (err) {
      if (failed) return;
      failed = true;
      busy = false;
      paths.forEach (function (p) { dirty.add (p); });
      console.error ('TeXmacs: cannot save the home directory', err);
      if (done) done ();
    }
    IDBFS.getDB (HOME, function (err, db) {
      if (err) return fail (err);
      var tx;
      try { tx = db.transaction ([IDBFS.DB_STORE_NAME], 'readwrite'); }
      catch (e) { return fail (e); }
      tx.oncomplete = function () {
        busy = false;
        if (again) { again = false; flush (); }
        if (done) done ();
      };
      tx.onerror = tx.onabort = function () { fail (tx.error); };
      var store = tx.objectStore (IDBFS.DB_STORE_NAME);
      paths.forEach (function (p) {
        try {
          var there = true;
          try { FS.lstat (p); } catch (e) { there = false; }
          if (!there) { store.delete (p); return; }
          IDBFS.loadLocalEntry (p, function (e, entry) {
            if (e) throw e;
            store.put (entry, p);
          });
        } catch (e) { console.error ('TeXmacs: cannot save ' + p, e); }
      });
    });
  }

  // a line above the bottom of the page (not to hide the menus), with
  // buttons and a cross which hides it (shown once the page is there)
  function say (text, buttons) {
    if (typeof document === 'undefined') return;
    if (!document.body) {
      document.addEventListener ('DOMContentLoaded', function () { say (text, buttons); });
      return;
    }
    if (!notice) {
      notice = document.createElement ('div');
      notice.id = 'tm-home-notice';
      notice.style.cssText = 'position:fixed;left:50%;bottom:40px;transform:translateX(-50%);' +
        'z-index:20;max-width:min(640px,calc(100vw - 32px));padding:9px 14px;' +
        'background:#fff7d6;border:1px solid #d8c06a;border-radius:6px;' +
        'box-shadow:0 2px 8px rgba(0,0,0,.18);font:13px -apple-system,"Fira Sans",' +
        'Helvetica,sans-serif;color:#222;line-height:1.4';
      document.body.appendChild (notice);
    }
    notice.textContent = text;
    notice.style.display = '';
    var x = document.createElement ('span');
    x.textContent = '\u00d7'; x.title = 'Hide';
    x.style.cssText = 'margin-left:10px;cursor:pointer;color:#777;font-size:16px;vertical-align:-1px';
    x.onclick = function () { notice.style.display = 'none'; };
    (buttons || []).forEach (function (b) {
      var e = document.createElement ('button');
      e.textContent = b[0];
      e.style.cssText = 'margin-left:10px;font:12px -apple-system,Helvetica,sans-serif;padding:2px 9px';
      e.onclick = b[1];
      notice.appendChild (e);
    });
    notice.appendChild (x);
  }

  // TeXmacs to this tab: the tab which has it writes its last changes and
  // lets go (see the channel), and this one starts again and takes it
  function takeOver () {
    try { sessionStorage.setItem (TAKEOVER, '1'); } catch (e) {}
    if (channel) channel.postMessage ({ type: 'takeover' });
    location.reload ();
  }

  // the tab which has TeXmacs is closed: a reload brings it here. After it
  // let go of the lock for another tab, a tab waits until that tab says it
  // has it ("claimed"), or it would take the lock back while the other
  // reloads (at most 15 seconds: the other tab may have been closed). The
  // lock may also be free because a tab reloads to take TeXmacs over: the
  // lock is let go at once, and TeXmacs is said to be closed only when no
  // tab claims it within 5 seconds
  var watching = false, claimWait = null, claims = 0;
  function watchOwner () {
    if (watching) return;
    watching = true;
    if (claimWait) { clearTimeout (claimWait); claimWait = null; }
    navigator.locks.request (LOCK, function () {
      var seen = claims;
      setTimeout (function () {
        watching = false;
        if (claims !== seen) { watchOwner (); return; }
        say ('TeXmacs Vue is no longer open in another tab. Reload this page to use it ' +
             'here with your saved files (the changes made in this tab are not kept).',
             [['Reload', function () { location.reload (); }]]);
      }, 5000);
    });
  }

  function becomeReadOnly (why, handedOver) {
    readOnly = true;
    if (typeof document !== 'undefined' && !/^\(read only\) /.test (document.title))
      document.title = '(read only) ' + document.title;
    say (why, [['Use TeXmacs here', takeOver]]);
    if (handedOver) claimWait = setTimeout (watchOwner, 15000);
    else watchOwner ();
  }

  // which tab writes: cont () once it is decided (the home directory is
  // then read, by every tab)
  function claim (cont) {
    if (!hasLocks) { cont (); return; }
    channel = new BroadcastChannel (LOCK);
    channel.onmessage = function (e) {
      if (!e.data) return;
      if (e.data.type === 'claimed') { claims++; if (readOnly && !watching) watchOwner (); return; }
      if (e.data.type !== 'takeover' || readOnly || !release) return;
      flush (function () {
        var r = release; release = null;
        becomeReadOnly ('TeXmacs Vue is now used in another tab of this browser: the ' +
                        'changes made in this tab are no longer kept.', true);
        r ();
      });
    };
    var takeover = false;
    try { takeover = sessionStorage.getItem (TAKEOVER) === '1';
          sessionStorage.removeItem (TAKEOVER); } catch (e) {}
    var opts = { ifAvailable: true };
    if (takeover && typeof AbortSignal !== 'undefined' && AbortSignal.timeout)
      opts = { signal: AbortSignal.timeout (10000) };
    var decided = false;
    navigator.locks.request (LOCK, opts, function (lock) {
      decided = true;
      if (!lock) {
        becomeReadOnly ('TeXmacs Vue is already open in another tab of this browser. ' +
                        'This tab shows your files, but its changes are not kept.');
        cont ();
        return;
      }
      channel.postMessage ({ type: 'claimed' });
      cont ();
      return new Promise (function (r) { release = r; });
    }).catch (function (e) {
      // the tab which had TeXmacs did not let go in time
      if (decided) return;
      becomeReadOnly ('TeXmacs Vue is still open in another tab of this browser. ' +
                      'This tab shows your files, but its changes are not kept.');
      cont ();
    });
  }

  if (typeof window !== 'undefined') {
    window.addEventListener ('pagehide', function () { flush (); });
    document.addEventListener ('visibilitychange', function () {
      if (document.visibilityState === 'hidden') flush ();
    });
  }
  tmSaveHome = function () { flush (); };
  return { claim: claim, track: track, flush: flush,
           readOnly: function () { return readOnly; },
           pending: function () { return dirty.size; } };
})();
