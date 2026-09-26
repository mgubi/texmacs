// --pre-js of the browser build: where TeXmacs finds its files.
//
// TeXmacs/ is packaged at /texmacs (see the "web" target of
// misc/wasm/Makefile). The home directory /home/web is kept in the
// IndexedDB of the page (IDBFS): it is read before TeXmacs starts, and
// written back every few seconds and when the page goes away, so that the
// preferences and the documents of the user survive a reload.
Module['preRun'] = Module['preRun'] || [];
Module['preRun'].push(function () {
  ENV['TEXMACS_PATH'] = '/texmacs';
  ENV['HOME'] = '/home/web';
  ENV['TEXMACS_HOME_PATH'] = '/home/web/.TeXmacs';
  ENV['LANG'] = 'en_US.UTF-8';
  FS.mkdirTree ('/home/web');
  FS.mount (IDBFS, { autoPersist: false }, '/home/web');
  addRunDependency ('home');
  FS.syncfs (true, function (err) {
    if (err) console.error ('TeXmacs: cannot read the saved home directory', err);
    try { FS.mkdirTree ('/home/web/.TeXmacs'); } catch (e) {}
    removeRunDependency ('home');
  });
});

// the home directory to IndexedDB now (files.js calls it after a change)
var tmSaveHome;
(function () {
  var busy = false;
  function save () {
    if (busy || !runtimeInitialized) return;
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
