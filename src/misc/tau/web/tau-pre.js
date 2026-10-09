// --pre-js of the core of Tau for the page (misc/tau/Makefile, "web"): the
// environment of TeXmacs in the file system of the program. The files of
// TeXmacs are brought by misc/wasm/packages.js, under /texmacs.
//
// The home directory, /home/tau, is kept in the IndexedDB of the browser
// (IDBFS): the preferences of TeXmacs (~/.TeXmacs) and the documents of the
// user (~/Documents) are there again when the page is loaded again. It is
// read before TeXmacs starts, and written back a moment after each change.
//
// One page only keeps it: each page has its own copy of the directory in
// memory, and writing two of them back would lose the files of one. The
// page which holds the lock "tau-home" (Web Locks) does; another one works
// in memory only and says so (Module.tauHomeKept is false).
var TAU_HOME = '/home/tau';

Module['preRun'] = Module['preRun'] || [];
Module['preRun'].push(function () {
  ENV['TEXMACS_PATH'] = '/texmacs';
  ENV['HOME'] = TAU_HOME;
  ENV['TEXMACS_HOME_PATH'] = TAU_HOME + '/.TeXmacs';
  ENV['LANG'] = 'en_US.UTF-8';
  if (new URLSearchParams (self.location.search).has ('trace-scroll')) ENV['TAU_DEBUG_SCROLL'] = '1';
  // the system of the user, for the shortcuts of TeXmacs: Cmd+... on a Mac
  // (default_look_and_feel in src/Kernel/Abstractions/basic.cpp)
  var platform = (typeof navigator === 'undefined') ? '' :
    (navigator.userAgentData && navigator.userAgentData.platform) || navigator.platform || '';
  ENV['TEXMACS_WEB_PLATFORM'] = /mac|iphone|ipad/i.test (platform) ? 'macos' :
                                /win/i.test (platform) ? 'windows' : 'other';

  // the global TeXmacs of the JavaScript plugin (misc/wasm/javascript.js),
  // for the code which a session evaluates in this worker
  if (typeof TeXmacs !== 'undefined') self.TeXmacs = TeXmacs;

  FS.mkdirTree (TAU_HOME);
  Module.tauHomeKept = false;
  if (new URLSearchParams (self.location.search).has ('nohome')) {
    // ?nohome: nothing is kept (the tests start from nothing)
    FS.mkdirTree (TAU_HOME + '/Documents');
    return;
  }
  addRunDependency ('tau-home');
  var start = function (kept) {
    var done = function () {
      try { FS.mkdirTree (TAU_HOME + '/.TeXmacs'); FS.mkdirTree (TAU_HOME + '/Documents'); } catch (e) {}
      // the temporary files of the sessions before: TeXmacs empties its
      // temporary directory when it quits, which a page seldom does
      tauRemoveTree (TAU_HOME + '/.TeXmacs/system/tmp', false);
      removeRunDependency ('tau-home');
    };
    if (!kept) { done (); return; }
    try {
      FS.mount (IDBFS, { autoPersist: true }, TAU_HOME);
      FS.syncfs (true, function (err) {
        if (err) console.error ('Tau: the home directory kept in the browser cannot be read: ' + err);
        else Module.tauHomeKept = true;
        done ();
      });
    } catch (e) {
      console.error ('Tau: the home directory cannot be kept in the browser: ' + e);
      done ();
    }
  };
  if (typeof navigator !== 'undefined' && navigator.locks && navigator.locks.request)
    navigator.locks.request ('tau-home', { ifAvailable: true }, function (lock) {
      start (!!lock);
      // (the lock is held as long as this worker lives)
      return lock ? new Promise (function () {}) : undefined;
    }).catch (function () { start (true); });
  else start (true);
});

// what was changed is written to the browser now, then done is called
Module.tauSaveHome = function (done) {
  if (!Module.tauHomeKept) { if (done) done (); return; }
  FS.syncfs (false, function (err) {
    if (err) console.error ('Tau: the home directory cannot be written to the browser: ' + err);
    if (done) done ();
  });
};

// remove a directory of the file system: its contents, and the directory
// itself when self is true
function tauRemoveTree (path, self) {
  var st;
  try { st = FS.stat (path); } catch (e) { return; }
  if (FS.isDir (st.mode)) {
    FS.readdir (path).forEach (function (x) {
      if (x !== '.' && x !== '..') tauRemoveTree (path + '/' + x, true);
    });
    if (self) try { FS.rmdir (path); } catch (e) {}
  }
  else try { FS.unlink (path); } catch (e) {}
}
