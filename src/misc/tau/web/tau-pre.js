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
  // ?profile: the times of the typesetting and of the drawing, in the console
  if (new URLSearchParams (self.location.search).has ('profile')) ENV['TEXMACS_EDIT_PROFILE'] = '1';
  // ?debug&env=NAME=value: a variable of the environment of TeXmacs, for
  // its switches (TEXMACS_PAGE_HF_CACHE=check...)
  if (new URLSearchParams (self.location.search).has ('debug'))
    new URLSearchParams (self.location.search).getAll ('env').forEach (function (e) {
      // (the switches of the typesetter and of its profiles, and no other
      // variable: a link must not say where TeXmacs finds its programs)
      var i = e.indexOf ('=');
      if (i > 0 && /^TEXMACS_(PAGE|EDIT|TYPESET)_[A-Z_]+$/.test (e.slice (0, i)))
        ENV[e.slice (0, i)] = e.slice (i + 1);
    });
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
      // What changes in the home is written to the browser by itself
      // (autoPersist), through IDBFS.syncfs, which is watched here:
      // - nothing is written while the home is being read: a write then
      //   took what was read so far for the whole home, and removed the
      //   rest from the browser;
      // - nothing is written at all when the home could not be read: the
      //   little which was read would replace what the browser keeps;
      // - a write which fails (no room left in the browser) is said to
      //   the page: it said nothing, and what was saved was gone at the
      //   next visit.
      // - the page is told while something is being written (from the
      //   change to the end of its write): it asks before it is closed
      //   then, which would lose the write;
      // - another page of Tau which deletes what is kept ("Forget
      //   everything") is let through: the browser waits for this page
      //   to let go of the database, which it did only when it was closed,
      //   long after, deleting what was saved meanwhile. Nothing is
      //   written any more, and the page says so.
      var realSync = IDBFS.syncfs, reading = true, read = false, held = [];
      var pending = 0, toldSaving = false;
      var tellSaving = function () {
        var mount = null;
        try { mount = FS.lookupPath (TAU_HOME).node.mount; } catch (e) {}
        var on = pending > 0 || !!(mount && mount.idbPersistState);
        if (on === toldSaving) return;
        toldSaving = on;
        try { postMessage ({ t: 'saving', on: on }); } catch (e) {}
      };
      IDBFS.syncfs = function (mount, populate, callback) {
        if (!populate && reading) { held.push (callback); return; }
        if (!populate && !read) { if (callback) callback (null); return; }
        if (!populate) { pending++; tellSaving (); }
        realSync.call (IDBFS, mount, populate, function (err) {
          if (err && !populate) {
            console.error ('Tau: the home directory cannot be written to the browser: ' + err);
            try { postMessage ({ t: 'storage', text: String (err && err.message || err) }); } catch (e) {}
          }
          if (!populate) pending--;
          if (callback) callback (err);
          if (!populate) setTimeout (tellSaving, 0);
        });
      };
      if (IDBFS.queuePersist) {
        var realQueue = IDBFS.queuePersist;
        IDBFS.queuePersist = function (mount) { realQueue.call (IDBFS, mount); tellSaving (); };
      }
      var realGetDB = IDBFS.getDB;
      IDBFS.getDB = function (name, callback) {
        realGetDB.call (IDBFS, name, function (err, db) {
          if (db && !db.tauWatched) {
            db.tauWatched = true;
            db.onversionchange = function () {
              db.close ();
              delete IDBFS.dbs[name];
              read = false;
              Module.tauHomeKept = false;
              if (!self.tauForgetting) try { postMessage ({ t: 'storage-erased' }); } catch (e) {}
            };
          }
          callback (err, db);
        });
      };
      FS.mount (IDBFS, { autoPersist: true }, TAU_HOME);
      FS.syncfs (true, function (err) {
        reading = false;
        if (err) {
          console.error ('Tau: the home directory kept in the browser cannot be read: ' + err);
          Module.tauHomeError = String (err && err.message || err);
        }
        else { Module.tauHomeKept = true; read = true; }
        // (what was asked to be written meanwhile: now, once, or never)
        var waiting = held; held = [];
        if (read && waiting.length > 0)
          IDBFS.syncfs (FS.lookupPath (TAU_HOME).node.mount, false, function (e) {
            waiting.forEach (function (c) { if (c) c (e); });
          });
        else waiting.forEach (function (c) { if (c) c (null); });
        done ();
      });
    } catch (e) {
      console.error ('Tau: the home directory cannot be kept in the browser: ' + e);
      Module.tauHomeError = String (e && e.message || e);
      done ();
    }
  };
  // (a browser without the locks cannot tell which page keeps the home:
  // two of them would write over each other, so none keeps it)
  var noLocks = function () {
    Module.tauHomeError = 'this browser cannot keep them for one page of Tau only';
    start (false);
  };
  var asked = false;
  if (typeof navigator !== 'undefined' && navigator.locks && navigator.locks.request)
    navigator.locks.request ('tau-home', { ifAvailable: true }, function (lock) {
      asked = true;
      start (!!lock);
      // (the lock is held as long as this worker lives)
      return lock ? new Promise (function () {}) : undefined;
    }).catch (function () { if (!asked) noLocks (); });
  else noLocks ();
});

// what was changed is written to the browser now, then done is called,
// with the error when it could not be (no room left in the browser)
Module.tauSaveHome = function (done) {
  if (!Module.tauHomeKept) { if (done) done (null); return; }
  FS.syncfs (false, function (err) {
    if (err) console.error ('Tau: the home directory cannot be written to the browser: ' + err);
    if (done) done (err || null);
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
