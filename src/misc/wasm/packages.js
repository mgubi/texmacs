// The files of TeXmacs in the page (a --pre-js of the browser build).
//
// misc/wasm/package.py writes the files of TeXmacs/ as packages, with a
// manifest (texmacs-files.json) giving for each file its package, offset and
// size. Before TeXmacs starts, the whole tree is created under /texmacs,
// every file as a placeholder of its size, and the boot package is loaded
// into it. Once TeXmacs runs, the other packages are loaded one after the
// other, in the background, and fill in their placeholders. A placeholder
// which is read before its package has come fetches its bytes alone (a
// range request, synchronous: TeXmacs reads its files synchronously), so
// that TeXmacs never finds a file of its tree missing and never waits for
// more than that file. The packages are kept in the Cache Storage of the
// browser, so that the next visit takes them from there.
//
// ?trace-files keeps the list of the files opened (window.tmTrace), which is
// how misc/wasm/boot-files.txt, the files of the boot package, is made.

var tmPackages = (function () {
  var ROOT = '/texmacs';
  var CACHE = 'texmacs-packages';
  var manifest = null;
  var stats = { onDemand: 0, onDemandBytes: 0, loaded: 0, start: 0 };
  var pending = {}; // package name -> [node] still to fill

  function url (name) {
    return (typeof document !== 'undefined') ? new URL (name, document.baseURI).href : name;
  }

  // the bytes of a package, from the cache of the browser or from the network
  async function fetchPackage (pkg) {
    var cache = null;
    try { if (typeof caches !== 'undefined') cache = await caches.open (CACHE); } catch (e) {}
    var u = url (pkg.url), resp = cache ? await cache.match (u) : null;
    if (!resp) {
      resp = await fetch (u);
      if (!resp.ok) throw new Error ('cannot load ' + pkg.url + ': ' + resp.status);
      if (cache) try { await cache.put (u, resp.clone ()); } catch (e) {}
    }
    return new Uint8Array (await resp.arrayBuffer ());
  }
  // the packages of an older build go (their names carry a digest)
  async function prune () {
    try {
      if (typeof caches === 'undefined') return;
      var cache = await caches.open (CACHE), keep = {};
      manifest.packages.forEach (function (p) { keep[url (p.url)] = true; });
      (await cache.keys ()).forEach (function (req) {
        if (!keep[req.url]) cache.delete (req);
      });
    } catch (e) {}
  }

  // the bytes of one file, now: a range of its package (synchronous; the
  // bytes come as text in the "user defined" charset, the only way for a
  // synchronous request on the main thread)
  function fetchNow (node) {
    var xhr = new XMLHttpRequest ();
    xhr.open ('GET', url (node.tmPackage.url), false);
    xhr.setRequestHeader ('Range', 'bytes=' + node.tmOffset + '-' + (node.tmOffset + node.tmSize - 1));
    xhr.overrideMimeType ('text/plain; charset=x-user-defined');
    xhr.send (null);
    var s = xhr.responseText, from = 0;
    if (xhr.status === 200) from = node.tmOffset; // the whole package: no ranges
    else if (xhr.status !== 206) throw new Error ('cannot load ' + node.tmPath + ': ' + xhr.status);
    var bytes = new Uint8Array (node.tmSize);
    for (var i = 0; i < node.tmSize; i++) bytes[i] = s.charCodeAt (from + i) & 0xff;
    stats.onDemand++;
    stats.onDemandBytes += node.tmSize;
    console.log ('TeXmacs: ' + node.tmPath + ' loaded on demand (' + node.tmSize + ' bytes)');
    return bytes;
  }

  function fill (node, bytes) {
    node.contents = bytes;
    node.tmPackage = null;
  }
  function materialize (node) {
    if (node.tmPackage) fill (node, fetchNow (node));
  }

  // a file of the tree, before its bytes: its size is known, a read brings
  // its bytes if its package has not yet
  function placeholder (dir, name, pkg, offset, size) {
    var node = FS.createFile (dir, name, {}, true, false);
    node.contents = null;
    node.tmPackage = pkg;
    node.tmOffset = offset;
    node.tmSize = size;
    node.tmPath = dir + '/' + name;
    Object.defineProperty (node, 'usedBytes', {
      get: function () { return this.contents ? this.contents.length : this.tmSize; },
      set: function (v) {}, configurable: true
    });
    var base = node.stream_ops, ops = {};
    for (var k in base) ops[k] = base[k];
    ops.read = function (stream, buffer, offset, length, position) {
      materialize (node);
      return base.read (stream, buffer, offset, length, position);
    };
    if (base.mmap) ops.mmap = function () {
      materialize (node);
      return base.mmap.apply (null, arguments);
    };
    node.stream_ops = ops;
    return node;
  }

  function createTree () {
    var made = {};
    function mkdir (d) {
      if (made[d]) return;
      made[d] = true;
      try { FS.mkdirTree (d); } catch (e) {}
    }
    manifest.packages.forEach (function (pkg) {
      pending[pkg.name] = [];
      pkg.files.forEach (function (f) {
        var p = ROOT + '/' + f[0], i = p.lastIndexOf ('/'), dir = p.slice (0, i);
        mkdir (dir);
        pending[pkg.name].push (placeholder (dir, p.slice (i + 1), pkg, f[1], f[2]));
      });
    });
  }

  // the bytes of a package into its placeholders (by slices when in the
  // background, so that the page keeps responding)
  async function install (pkg, bytes, slices) {
    var nodes = pending[pkg.name], t = performance.now ();
    for (var i = 0; i < nodes.length; i++) {
      var n = nodes[i];
      if (n.tmPackage) fill (n, bytes.subarray (n.tmOffset, n.tmOffset + n.tmSize));
      if (slices && performance.now () - t > 8) {
        await new Promise (function (ok) { setTimeout (ok, 0); });
        t = performance.now ();
      }
    }
    pending[pkg.name] = [];
    stats.loaded++;
  }

  async function background () {
    var rest = manifest.packages.filter (function (p) { return !p.boot; });
    for (var i = 0; i < rest.length; i++) {
      try { await install (rest[i], await fetchPackage (rest[i]), true); }
      catch (e) { console.error ('TeXmacs: package ' + rest[i].name + ': ' + e.message); }
    }
    console.log ('TeXmacs: all the files are there (' + stats.loaded + ' packages in ' +
                 Math.round (performance.now () - stats.start) + ' ms, ' + stats.onDemand +
                 ' files on demand before)');
    prune ();
  }

  Module['preRun'] = Module['preRun'] || [];
  Module['preRun'].push (function () {
    addRunDependency ('texmacs-files');
    stats.start = performance.now ();
    fetch (url ('texmacs-files.json')).then (function (r) { return r.json (); })
      .then (async function (m) {
        manifest = m;
        createTree ();
        var boot = manifest.packages.filter (function (p) { return p.boot; });
        for (var i = 0; i < boot.length; i++) await install (boot[i], await fetchPackage (boot[i]), false);
        console.log ('TeXmacs: boot files in ' + Math.round (performance.now () - stats.start) + ' ms');
        removeRunDependency ('texmacs-files');
        // the rest once TeXmacs runs (and has had its first frames);
        // ?no-background leaves it to the demand, to test that path
        var noBackground = typeof location !== 'undefined' &&
                           location.search.indexOf ('no-background') >= 0;
        if (!noBackground) setTimeout (background, 500);
      })
      .catch (function (e) {
        console.error ('TeXmacs: cannot load its files', e);
        if (Module.setStatus) Module.setStatus ('Cannot load the files of TeXmacs: ' + e.message);
      });
  });

  return { stats: stats, manifest: function () { return manifest; } };
})();
