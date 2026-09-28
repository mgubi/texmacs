// --pre-js of the browser build: where TeXmacs finds its files.
//
// TeXmacs/ is packaged at /texmacs (see the "web" target of
// misc/wasm/Makefile). The home directory /home/web is kept in the
// IndexedDB of the page (IDBFS): it is read before TeXmacs starts, and
// written back every few seconds and when the page goes away, so that the
// preferences and the documents of the user survive a reload.
Module['preRun'] = Module['preRun'] || [];
Module['preRun'].push(function () {
  // ?trace-files: the files of /texmacs which TeXmacs opens, in the order
  // it opens them (window.tmTrace), to choose the files needed at boot
  if (typeof location !== 'undefined' && location.search.indexOf ('trace-files') >= 0) {
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
  // the look and feel of TeXmacs follows the platform of the browser, whose
  // shortcuts are Cmd+... on a Mac (src/Kernel/Abstractions/basic.cpp)
  var platform = (typeof navigator === 'undefined') ? '' :
    (navigator.userAgentData && navigator.userAgentData.platform) || navigator.platform || '';
  ENV['TEXMACS_WEB_PLATFORM'] = /mac|iphone|ipad/i.test (platform) ? 'macos' :
                                /win/i.test (platform) ? 'windows' : 'other';
  // texmacs.html?profile=<n>: the profile of the loop, every n frames, in
  // the console (TEXMACS_VUE_PROFILE, see vue_profile_frame in vue_gui.cpp)
  var prof = (typeof location !== 'undefined') && /[?&]profile=(\d+)/.exec (location.search);
  if (prof) ENV['TEXMACS_VUE_PROFILE'] = prof[1];
  FS.mkdirTree ('/home/web');
  FS.mount (IDBFS, { autoPersist: false }, '/home/web');
  addRunDependency ('home');
  FS.syncfs (true, function (err) {
    if (err) console.error ('TeXmacs: cannot read the saved home directory', err);
    try { FS.mkdirTree ('/home/web/.TeXmacs'); } catch (e) {}
    // the temporary files of the previous sessions: TeXmacs empties its
    // temporary directory when it quits, which a page never does (and its
    // process has always the same number: they piled up in the storage)
    tmRemoveTree ('/home/web/.TeXmacs/system/tmp', false);
    removeRunDependency ('home');
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

// the home directory to IndexedDB now (files.js calls it after a change);
// never again once the page removed its storage (tmFrame, "Remove from this
// browser"), or it would write it back
var tmSaveHome, tmStorageRemoved = false;
(function () {
  var busy = false;
  function save () {
    if (busy || !runtimeInitialized || tmStorageRemoved) return;
    busy = true;
    FS.syncfs (false, function (err) {
      busy = false;
      if (err) console.error ('TeXmacs: cannot save the home directory', err);
    });
  }
  tmSaveHome = save;
  setInterval (save, 5000);
  if (typeof window !== 'undefined') {
    window.addEventListener ('pagehide', save);
    document.addEventListener ('visibilitychange', function () {
      if (document.visibilityState === 'hidden') save ();
    });
  }
})();
