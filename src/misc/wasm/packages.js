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
// The fonts (the manifest's "lazy" files: package.py) are in no package:
// each is a file of its own, fetched whole when TeXmacs first reads it (the
// read waits for it, as for a file of a package not there yet), and kept in
// the Cache Storage too; the fonts found there are put in place before
// TeXmacs starts. A font which is never used is never fetched.
//
// The network may be slow, or fail for a moment. The fonts of the first
// screen (EARLY_FONTS) therefore come with the boot files, before TeXmacs
// starts, without holding the page: read by TeXmacs, each would be a
// request during which the page does not respond, the larger one for as
// long as its megabyte takes. A request of a read which fails is made
// again (TRIES times, a pause in between), and a font which still does not
// come is fetched in the background, for the next read of it or the next
// visit: TeXmacs was told that the file could not be read, and draws the
// characters of that font as missing until then.
//
// ?trace-files keeps the list of the files opened (window.tmTrace), which is
// how misc/wasm/boot-files.txt, the files of the boot package, is made.

var tmPackages = (function () {
  var ROOT = '/texmacs';
  var CACHE = 'texmacs-packages';
  var manifest = null;
  var stats = { onDemand: 0, onDemandBytes: 0, loaded: 0, start: 0,
                fonts: 0, fontBytes: 0, fontsCached: 0,
                fontsEarly: 0, fontsFailed: 0 };
  var pending = {}; // package name -> [node] still to fill
  var lazyNodes = {}; // the url of a font -> its placeholders (the same font
                      // may be at several places of the tree)

  function url (name) {
    return (typeof document !== 'undefined') ? new URL (name, document.baseURI).href : name;
  }

  // the bytes of a package, from the cache of the browser or from the network
  // (onBytes (n): the bytes which came, for the progress of the page). Its
  // gzip copy when the browser can decompress it (DecompressionStream): the
  // servers of static files (GitHub Pages) do not compress the packages
  var gunzip = typeof DecompressionStream !== 'undefined';
  async function fetchPackage (pkg, onBytes) {
    var cache = null;
    try { if (typeof caches !== 'undefined') cache = await caches.open (CACHE); } catch (e) {}
    var name = (gunzip && pkg.gz) ? pkg.gz : pkg.url;
    var u = url (name), resp = cache ? await cache.match (u) : null;
    if (!resp) {
      resp = await fetch (u);
      if (!resp.ok) throw new Error ('cannot load ' + name + ': ' + resp.status);
      if (cache) try { await cache.put (u, resp.clone ()); } catch (e) {}
    }
    if (name !== pkg.url && resp.body)
      resp = new Response (resp.body.pipeThrough (new DecompressionStream ('gzip')));
    if (!onBytes || !resp.body) return new Uint8Array (await resp.arrayBuffer ());
    var bytes = new Uint8Array (pkg.size), at = 0, reader = resp.body.getReader ();
    for (;;) {
      var r = await reader.read ();
      if (r.done) break;
      bytes.set (r.value, at);
      at += r.value.length;
      onBytes (at);
    }
    return bytes;
  }
  // the packages of an older build go (their names carry a digest)
  async function prune () {
    try {
      if (typeof caches === 'undefined') return;
      var cache = await caches.open (CACHE), keep = {};
      manifest.packages.forEach (function (p) {
        keep[url (p.url)] = true;
        if (p.gz) keep[url (p.gz)] = true;
      });
      if (manifest.lazy) manifest.lazy.forEach (function (f) { keep[url (f[1])] = true; });
      (await cache.keys ()).forEach (function (req) {
        if (!keep[req.url]) cache.delete (req);
      });
    } catch (e) {}
  }

  // a request now (synchronous; the bytes come as text in the "user
  // defined" charset, the only way for a synchronous request on the main
  // thread)
  function getOnce (address, range) {
    var xhr = new XMLHttpRequest ();
    xhr.open ('GET', address, false);
    if (range) xhr.setRequestHeader ('Range', range);
    xhr.overrideMimeType ('text/plain; charset=x-user-defined');
    xhr.send (null);
    return xhr;
  }
  // The same, made again when the network fails (send throws) or the answer
  // is cut short of the size expected: `whole' is the size of the file
  // when no range was asked for. An answer of the server, whatever its
  // status, is returned as it is (the callers know what to make of it).
  var TRIES = 3;
  function pause (ms) { // the page waits for the read anyway
    var t = performance.now ();
    while (performance.now () - t < ms) {}
  }
  function getNow (address, range, whole) {
    var last = null;
    for (var i = 0; i < TRIES; i++) {
      if (i) pause (300 * i);
      try {
        var xhr = getOnce (address, range);
        if (xhr.status === 0 ||
            (whole && xhr.status === 200 && xhr.responseText.length !== whole))
          last = new Error ('incomplete answer (' + xhr.status + ', ' +
                            xhr.responseText.length + ' bytes)');
        else return xhr;
      } catch (e) { last = e; }
      console.warn ('TeXmacs: ' + address + (range ? ' (' + range + ')' : '') +
                    ': ' + last + (i + 1 < TRIES ? ', asked again' : ''));
    }
    throw last;
  }
  function textBytes (s, from, n) {
    var bytes = new Uint8Array (n);
    for (var i = 0; i < n; i++) bytes[i] = s.charCodeAt (from + i) & 0xff;
    return bytes;
  }

  // Some servers compress a package as they send it and cut the range out of
  // what they compress: GitHub Pages answers a range of a package with a
  // range of its gzip (content-encoding: gzip), or 416 beyond the size of
  // that. Their ranges are then of no use, and the whole package is fetched
  // at once (which they compress whole, and the browser decodes).
  var rangesUseless = false;

  // the bytes of one file, now: a range of its package, or else the whole
  // package, all of whose files are then installed
  function fetchNow (node) {
    var pkg = node.tmPackage;
    if (pkg.lazy) return fetchFont (node);
    if (!rangesUseless) {
      try {
        var xhr = getNow (url (pkg.url), 'bytes=' + node.tmOffset + '-' +
                                         (node.tmOffset + node.tmSize - 1));
        var enc = xhr.getResponseHeader ('Content-Encoding');
        var plain = !enc || enc === 'identity';
        var s = xhr.responseText;
        if (xhr.status === 206 && plain && s.length === node.tmSize) {
          stats.onDemand++;
          stats.onDemandBytes += node.tmSize;
          console.log ('TeXmacs: ' + node.tmPath + ' loaded on demand (' + node.tmSize + ' bytes)');
          return textBytes (s, 0, node.tmSize);
        }
        if (xhr.status === 200 && s.length === pkg.size) {
          // the whole package: no ranges
          installNow (pkg, textBytes (s, 0, pkg.size));
          console.log ('TeXmacs: ' + node.tmPath + ' loaded on demand, with its package ' +
                       pkg.name + ' (the server sends no ranges)');
          return textBytes (s, node.tmOffset, node.tmSize);
        }
        console.warn ('TeXmacs: the ranges of ' + pkg.url + ' are of no use (' + xhr.status +
                      (plain ? '' : ', ' + enc) + '): the whole packages are fetched');
      } catch (e) {
        console.warn ('TeXmacs: a range of ' + pkg.url + ' failed (' + e + '): the whole packages are fetched');
      }
      rangesUseless = true;
    }
    var all = getNow (url (pkg.url), null, pkg.size);
    if (all.status !== 200 || all.responseText.length !== pkg.size)
      throw new Error ('cannot load ' + node.tmPath + ': ' + all.status);
    var bytes = textBytes (all.responseText, 0, pkg.size);
    installNow (pkg, bytes);
    console.log ('TeXmacs: ' + node.tmPath + ' loaded on demand, with its package ' +
                 pkg.name + ' (' + pkg.size + ' bytes)');
    return bytes.subarray (node.tmOffset, node.tmOffset + node.tmSize);
  }

  // a font, now: the whole file, into every placeholder of it, and into the
  // cache for the next visits
  function fetchFont (node) {
    var pkg = node.tmPackage, u = url (pkg.url);
    var xhr, s;
    try {
      xhr = getNow (u, null, pkg.size);
      s = xhr.responseText;
      if (xhr.status !== 200 || s.length !== pkg.size)
        throw new Error ('status ' + xhr.status + ', ' + s.length + ' bytes');
    } catch (e) {
      console.error ('TeXmacs: cannot load ' + node.tmPath + ' (' + e + '): fetched in the background');
      stats.fontsFailed++;
      fetchFontLater (pkg);
      throw new FS.ErrnoError (29); // EIO: see materialize
    }
    var bytes = textBytes (s, 0, pkg.size);
    stats.fonts++;
    stats.fontBytes += pkg.size;
    console.log ('TeXmacs: ' + node.tmPath + ' loaded on demand (' + pkg.size + ' bytes)');
    (lazyNodes[u] || []).forEach (function (n) { if (n !== node && n.tmPackage) fill (n, bytes); });
    if (typeof caches !== 'undefined')
      caches.open (CACHE).then (function (cache) {
        return cache.put (u, new Response (bytes, {
          headers: { 'Content-Type': 'application/octet-stream',
                     'Content-Length': String (bytes.length) } }));
      }).catch (function () {});
    return bytes;
  }

  // a font which did not come when TeXmacs read it: without holding the
  // page, into its placeholders and into the cache
  var later = {};
  function fetchFontLater (pkg) {
    var u = url (pkg.url);
    if (later[u]) return;
    later[u] = true;
    fetchPackage ({ url: pkg.url, size: pkg.size }).then (function (bytes) {
      if (bytes.length !== pkg.size) throw new Error (bytes.length + ' bytes');
      (lazyNodes[u] || []).forEach (function (n) { if (n.tmPackage) fill (n, bytes); });
      console.log ('TeXmacs: ' + pkg.name + ' came in the background');
    }).catch (function (e) {
      later[u] = false;
      console.warn ('TeXmacs: ' + pkg.name + ' did not come in the background (' + e + ')');
    });
  }

  // The fonts of the first screen, which are not in the boot package: the
  // typewriter and the sans serif fonts of the welcome page and, where the
  // keys of the shortcuts are written with the symbols of a Mac (the menus,
  // the welcome page), the font those come from (widget_symbol_font in
  // vue_gui.cpp, kbd_render in tm_config.cpp). Without the last one the
  // command sign of the menus waited for 800 kB during a frame.
  var MAC = typeof navigator !== 'undefined' && /Mac|iPhone|iPad|iPod/i.test (
    (navigator.userAgentData && navigator.userAgentData.platform) || navigator.platform || '');
  var EARLY_FONTS = ['fonts/truetype/inconsolata/Inconsolatazi4-Regular.otf',
                     'fonts/truetype/texgyre/texgyreheros-regular.otf']
    .concat (MAC ? ['fonts/truetype/stix2/STIXTwoMath-Regular.otf'] : []);
  function earlyFonts (m) {
    return (m.lazy || []).filter (function (f) { return EARLY_FONTS.indexOf (f[0]) >= 0; });
  }

  // the fonts which an earlier visit fetched, from the cache, before TeXmacs
  // starts (a read of one of them would fetch it again)
  async function restoreFonts () {
    if (typeof caches === 'undefined' || !manifest.lazy) return;
    try {
      var cache = await caches.open (CACHE), have = {};
      (await cache.keys ()).forEach (function (req) { have[req.url] = true; });
      for (var u in lazyNodes) {
        if (!have[u]) continue;
        var resp = await cache.match (u);
        if (!resp) continue;
        var bytes = new Uint8Array (await resp.arrayBuffer ());
        var nodes = lazyNodes[u];
        if (bytes.length !== nodes[0].tmSize) continue;
        if (!nodes.some (function (n) { return n.tmPackage; })) continue; // there already
        nodes.forEach (function (n) { if (n.tmPackage) fill (n, bytes); });
        stats.fontsCached++;
      }
    } catch (e) {}
  }

  function fill (node, bytes) {
    node.contents = bytes;
    node.tmPackage = null;
  }
  // The bytes of a placeholder, for a read. A file which cannot be fetched
  // is an input/output error of the read, as for a file of a disk: an
  // exception of another kind went through TeXmacs up to the loop of the
  // page and left the frame it was drawing half done.
  function materialize (node) {
    if (!node.tmPackage) return;
    var bytes;
    try { bytes = fetchNow (node); }
    catch (e) {
      if (!(e && e.name === 'ErrnoError'))
        console.error ('TeXmacs: cannot load ' + node.tmPath + ' (' + e + ')');
      throw new FS.ErrnoError (29); // EIO
    }
    fill (node, bytes);
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

  // The time of the files of TeXmacs: one per build, the same at each visit.
  // They were made at the time of the visit, so that TeXmacs saw them all
  // changed at each start (last_modified): it merged the font database of
  // its home again (shipped_fonts_changed), which emptied the caches of the
  // fonts looked for at the start (cache_refresh), and found its directories
  // never up to date. From the hashes of the packages: another build has
  // another time (its files may differ).
  function buildTime () {
    var s = ENV['TEXMACS_WEB_BUILD'] || '', h = 0;
    for (var i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt (i)) >>> 0;
    return Date.UTC (2020, 0, 1) + (h % (5 * 365 * 86400)) * 1000;
  }
  function setTime (node, t) {
    node.timestamp = node.atime = node.mtime = node.ctime = t;
  }

  function createTree () {
    var made = {};
    var time = buildTime ();
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
        var node = placeholder (dir, p.slice (i + 1), pkg, f[1], f[2]);
        setTime (node, time);
        pending[pkg.name].push (node);
      });
    });
    // the fonts: a placeholder each, whose "package" is the font's own file
    (manifest.lazy || []).forEach (function (f) {
      var p = ROOT + '/' + f[0], i = p.lastIndexOf ('/'), dir = p.slice (0, i);
      mkdir (dir);
      var pkg = { name: f[0], url: f[1], size: f[2], lazy: true };
      var u = url (f[1]);
      var node = placeholder (dir, p.slice (i + 1), pkg, 0, f[2]);
      setTime (node, time);
      (lazyNodes[u] = lazyNodes[u] || []).push (node);
    });
    // the directories last (a file made in one changes its time)
    Object.keys (made).forEach (function (d) {
      for (; d.length >= ROOT.length; d = d.slice (0, d.lastIndexOf ('/')))
        try { setTime (FS.lookupPath (d).node, time); } catch (e) {}
    });
  }

  // a whole package fetched for one of its files (fetchNow): its
  // placeholders filled at once, and not fetched again in the background
  function installNow (pkg, bytes) {
    if (pkg.tmInstalled) return;
    var nodes = pending[pkg.name];
    for (var i = 0; i < nodes.length; i++)
      if (nodes[i].tmPackage) fill (nodes[i], bytes.subarray (nodes[i].tmOffset, nodes[i].tmOffset + nodes[i].tmSize));
    pending[pkg.name] = [];
    pkg.tmInstalled = true;
    stats.loaded++;
  }

  // the bytes of a package into its placeholders (by slices when in the
  // background, so that the page keeps responding)
  async function install (pkg, bytes, slices) {
    if (pkg.tmInstalled) return;
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
    pkg.tmInstalled = true;
    stats.loaded++;
  }

  async function background () {
    var rest = manifest.packages.filter (function (p) { return !p.boot; });
    for (var i = 0; i < rest.length; i++) {
      if (rest[i].tmInstalled) continue; // fetched on demand, whole
      try { await install (rest[i], await fetchPackage (rest[i]), true); }
      catch (e) { console.error ('TeXmacs: package ' + rest[i].name + ': ' + e.message); }
    }
    console.log ('TeXmacs: all the files are there (' + stats.loaded + ' packages in ' +
                 Math.round (performance.now () - stats.start) + ' ms, ' + stats.onDemand +
                 ' files on demand before)');
    prune ();
  }

  // The manifest and the bytes of the boot packages are fetched as soon as
  // the page runs this, while the program comes and compiles: they used to
  // wait for preRun, which comes once the program is compiled, so that the
  // two downloads followed each other (and the bar of the page knew the
  // size of the files only then). Only their installation, which needs the
  // file system, waits for preRun.
  var early = null;
  function fetchBoot () {
    if (early) return early;
    stats.start = performance.now ();
    early = fetch (url ('texmacs-files.json')).then (function (r) {
      if (!r.ok) throw new Error ('cannot load texmacs-files.json: ' + r.status);
      return r.json ();
    }).then (async function (m) {
      var boot = m.packages.filter (function (p) { return p.boot; });
      var fonts = earlyFonts (m);
      var total = 0, done = 0, bytes = [], fontBytes = [];
      boot.forEach (function (p) { total += p.size; });
      fonts.forEach (function (f) { total += f[2]; });
      var progress = typeof tmProgress !== 'undefined' ? tmProgress.files : function () {};
      progress (0, total);
      for (var i = 0; i < boot.length; i++) {
        bytes.push (await fetchPackage (boot[i], function (n) { progress (done + n, total); }));
        done += boot[i].size;
        progress (done, total);
      }
      // the fonts of the first screen (from the cache at a later visit); one
      // which does not come is left to the read of TeXmacs
      for (var j = 0; j < fonts.length; j++) {
        var b = null;
        for (var k = 0; k < TRIES && !b; k++)
          try {
            b = await fetchPackage ({ url: fonts[j][1], size: fonts[j][2] },
                                    function (n) { progress (done + n, total); });
            if (b.length !== fonts[j][2]) b = null;
          } catch (e) { console.warn ('TeXmacs: ' + fonts[j][0] + ': ' + e.message); }
        fontBytes.push (b);
        done += fonts[j][2];
        progress (done, total);
      }
      return { manifest: m, boot: boot, bytes: bytes, fonts: fonts, fontBytes: fontBytes };
    });
    return early;
  }
  if (typeof document !== 'undefined' && typeof fetch !== 'undefined')
    fetchBoot ().catch (function () {}); // reported by preRun

  Module['preRun'] = Module['preRun'] || [];
  Module['preRun'].push (function () {
    addRunDependency ('texmacs-files');
    fetchBoot ()
      .then (async function (r) {
        manifest = r.manifest;
        // the build, for TeXmacs: the hashes of the contents of its
        // packages; the cache of the plugins (tm-plugins.scm) is made again
        // when it changes (a new build may have other plugins)
        ENV['TEXMACS_WEB_BUILD'] = manifest.packages.map (function (p) {
          var h = /-([0-9a-f]+)\.pack$/.exec (p.url || '');
          return h ? h[1] : '';
        }).join ('');
        createTree ();
        for (var i = 0; i < r.boot.length; i++) {
          await install (r.boot[i], r.bytes[i], false);
          r.bytes[i] = null;
        }
        r.fonts.forEach (function (f, j) {
          if (!r.fontBytes[j]) return;
          (lazyNodes[url (f[1])] || []).forEach (function (n) { if (n.tmPackage) fill (n, r.fontBytes[j]); });
          stats.fontsEarly++;
        });
        await restoreFonts ();
        console.log ('TeXmacs: boot files in ' + Math.round (performance.now () - stats.start) + ' ms' +
                     (stats.fontsEarly ? ', with ' + stats.fontsEarly + ' fonts of the first screen' : '') +
                     (stats.fontsCached ? ', ' + stats.fontsCached + ' fonts from the cache' : ''));
        removeRunDependency ('texmacs-files');
        // the rest once TeXmacs runs (and has had its first frames);
        // ?no-background leaves it to the demand, to test that path
        var noBackground = typeof location !== 'undefined' &&
                           new URLSearchParams (location.search).has ('no-background');
        if (!noBackground) setTimeout (background, 500);
      })
      .catch (function (e) {
        console.error ('TeXmacs: cannot load its files', e);
        if (typeof tmProgress !== 'undefined') tmProgress.error ('Cannot load the files of TeXmacs: ' + e.message);
        else if (Module.setStatus) Module.setStatus ('Cannot load the files of TeXmacs: ' + e.message);
      });
  });

  return { stats: stats, manifest: function () { return manifest; } };
})();
