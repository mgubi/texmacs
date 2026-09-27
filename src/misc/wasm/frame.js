// The frame of the page (a --pre-js of the browser build): a bar above the
// canvas with the tabs of the windows of TeXmacs and a TeXmacs menu.
//
// In the browser every window of an editor is a tab (see "Single-window
// mode" in src/Plugins/Vue/vue_gui.cpp): the plugin tells the frame of the
// tabs (tmFrame.update) and of the application (tmFrame.info); the frame
// asks it to show, close or open a tab (_vue_web_activate_tab,
// _vue_web_close_tab, _vue_web_new_tab). The name of a window, which is its
// title on the desktop, is the label of its tab and the title of the page.

var tmFrame = (function () {
  var tabs = [], app = {}, bar = null, strip = null, menu = null;

  function el (tag, cls, text) {
    var e = document.createElement (tag);
    if (cls) e.className = cls;
    if (text !== undefined) e.textContent = text;
    return e;
  }

  var style = `
    #tm-frame { display:flex; align-items:stretch; height:32px; background:#d8d8d8;
      border-bottom:1px solid #a8a8a8; font:13px -apple-system,"Fira Sans",Helvetica,sans-serif;
      color:#222; user-select:none; flex:none }
    #tm-frame .tm-app { display:flex; align-items:center; padding:0 12px; font-weight:bold;
      cursor:pointer; border-right:1px solid #b8b8b8 }
    #tm-frame .tm-app:hover, #tm-frame .tm-app.open { background:#c8c8c8 }
    #tm-frame .tm-tabs { display:flex; flex:1; overflow:hidden; scrollbar-width:none }
    #tm-frame .tm-tabs::-webkit-scrollbar { display:none }
    #tm-frame .tm-tabs.dragging { cursor:grabbing }
    #tm-frame .tm-tab { display:flex; align-items:center; max-width:240px; min-width:90px;
      padding:0 6px 0 12px; border-right:1px solid #b8b8b8; cursor:default; background:#d0d0d0 }
    #tm-frame .tm-tab.active { background:#f0f0f0 }
    #tm-frame .tm-tab .tm-title { flex:1; overflow:hidden; white-space:nowrap; text-overflow:ellipsis }
    #tm-frame .tm-tab .tm-close { margin-left:6px; width:18px; height:18px; line-height:18px;
      text-align:center; border-radius:3px; color:#555 }
    #tm-frame .tm-tab .tm-close:hover { background:#bbb; color:#000 }
    #tm-frame .tm-new { display:flex; align-items:center; padding:0 12px; cursor:pointer; font-size:17px }
    #tm-frame .tm-new:hover { background:#c8c8c8 }
    #tm-menu { position:fixed; left:4px; top:34px; width:340px; background:#f6f6f6;
      border:1px solid #999; border-radius:6px; box-shadow:0 6px 24px rgba(0,0,0,.3);
      font:13px -apple-system,"Fira Sans",Helvetica,sans-serif; color:#222; z-index:30; padding:6px 0 }
    #tm-menu .tm-head { padding:8px 14px 4px; font-weight:bold; font-size:14px }
    #tm-menu .tm-badge, #tm-loading .tm-badge { display:inline-block; margin-left:8px; padding:1px 6px;
      font-size:11px; font-weight:normal; color:#1f4e8c; background:#e3eefc; border:1px solid #9cbce8;
      border-radius:8px; vertical-align:middle }
    #tm-menu .tm-text { padding:2px 14px; color:#444 }
    #tm-menu .tm-sep { height:1px; background:#ccc; margin:6px 0 }
    #tm-menu .tm-item { padding:5px 14px; cursor:pointer }
    #tm-menu .tm-item:hover { background:#dde6f0 }
    #tm-menu a { color:#036 }
    #tm-menu .tm-soft { display:grid; grid-template-columns:auto 1fr; column-gap:10px;
      row-gap:2px; padding:2px 14px 4px; color:#444 }
    #tm-menu .tm-soft a { text-decoration:none }
    #tm-menu .tm-soft a:hover { text-decoration:underline }
    #tm-menu .tm-soft .tm-ver { color:#666; font-variant-numeric:tabular-nums }
  `;

  function build () {
    if (bar || typeof document === 'undefined') return;
    var st = el ('style'); st.textContent = style; document.head.appendChild (st);
    bar = document.getElementById ('tm-frame');
    if (!bar) return;
    var appButton = el ('div', 'tm-app', 'TeXmacs Vue');
    appButton.title = 'About TeXmacs Vue, an experimental port of GNU TeXmacs';
    appButton.onclick = function (e) { e.stopPropagation (); toggleMenu (appButton); };
    strip = el ('div', 'tm-tabs');
    ribbon (strip);
    var plus = el ('div', 'tm-new', '+');
    plus.title = 'New window';
    plus.onclick = function () { _vue_web_new_tab (); };
    bar.appendChild (appButton);
    bar.appendChild (strip);
    bar.appendChild (plus);
    // a press outside the menu closes it; not one on the TeXmacs button,
    // whose click toggles it (else the press closed it and the click
    // opened it again)
    document.addEventListener ('mousedown', function (e) {
      if (menu && !menu.contains (e.target) && !appButton.contains (e.target)) closeMenu ();
    });
  }

  // the ribbon of the tabs has no scroll bar: the wheel (either way) and a
  // drag of the ribbon move it, and the active tab is brought into view
  var dragged = false;
  function ribbon (r) {
    r.addEventListener ('wheel', function (e) {
      var d = Math.abs (e.deltaX) > Math.abs (e.deltaY) ? e.deltaX : e.deltaY;
      if (e.deltaMode === 1) d *= 16;
      r.scrollLeft += d;
      e.preventDefault ();
    }, { passive: false });
    var startX = 0, startScroll = 0, down = false;
    r.addEventListener ('pointerdown', function (e) {
      if (e.button !== 0 || e.target.classList.contains ('tm-close')) return;
      down = true; dragged = false;
      startX = e.clientX; startScroll = r.scrollLeft;
    });
    window.addEventListener ('pointermove', function (e) {
      if (!down) return;
      var dx = e.clientX - startX;
      if (!dragged && Math.abs (dx) > 4) { dragged = true; r.classList.add ('dragging'); }
      if (dragged) r.scrollLeft = startScroll - dx;
    });
    window.addEventListener ('pointerup', function () {
      if (!down) return;
      down = false;
      r.classList.remove ('dragging');
      // the click which follows the release sees whether it was a drag
      setTimeout (function () { dragged = false; }, 0);
    });
  }

  function render () {
    build ();
    if (!strip) return;
    strip.textContent = '';
    tabs.forEach (function (t) {
      var tab = el ('div', 'tm-tab' + (t.active ? ' active' : ''));
      tab.dataset.id = t.id;
      tab.title = t.title;
      var title = el ('span', 'tm-title', (t.modified ? '• ' : '') + t.title);
      tab.appendChild (title);
      // a click shows the tab, unless the ribbon was dragged (see ribbon)
      tab.onclick = function (e) { if (!dragged) _vue_web_activate_tab (t.id); };
      tab.onmousedown = function (e) {
        if (e.button === 1) { e.preventDefault (); if (tabs.length > 1) _vue_web_close_tab (t.id); }
      };
      if (tabs.length > 1) {
        var x = el ('span', 'tm-close', '×');
        x.title = 'Close';
        x.onmousedown = function (e) { e.stopPropagation (); };
        x.onclick = function (e) { e.stopPropagation (); _vue_web_close_tab (t.id); };
        tab.appendChild (x);
      }
      strip.appendChild (tab);
    });
    var active = tabs.filter (function (t) { return t.active; })[0];
    var at = strip.querySelector ('.tm-tab.active');
    if (at) {
      var l = at.offsetLeft - strip.offsetLeft, r = l + at.offsetWidth;
      if (l < strip.scrollLeft) strip.scrollLeft = l;
      else if (r > strip.scrollLeft + strip.clientWidth) strip.scrollLeft = r - strip.clientWidth;
    }
    document.title = active ? (active.modified ? '• ' : '') + active.title + ' — TeXmacs Vue'
                            : 'TeXmacs Vue';
  }

  /****************************************************************************
  * The TeXmacs menu
  ****************************************************************************/

  function closeMenu () {
    if (!menu) return;
    menu.remove ();
    menu = null;
    var b = bar && bar.querySelector ('.tm-app');
    if (b) b.classList.remove ('open');
  }

  // what the page keeps: the home directory (in IndexedDB) and the packages
  // of TeXmacs (in the Cache Storage), counted here, and what the browser
  // counts for the site (navigator.storage.estimate: its database files
  // do not shrink when data is replaced, so it is often more)
  function storageUse () {
    var home = 0;
    (function walk (p) {
      var st;
      try { st = FS.stat (p); } catch (e) { return; }
      if (FS.isDir (st.mode))
        FS.readdir (p).forEach (function (x) { if (x !== '.' && x !== '..') walk (p + '/' + x); });
      else home += st.size;
    }) ('/home/web');
    var cache = (typeof caches === 'undefined') ? Promise.resolve (0) :
      caches.open ('texmacs-packages').then (function (c) {
        return c.keys ().then (function (ks) {
          return Promise.all (ks.map (function (k) {
            return c.match (k).then (function (r) {
              var n = r && Number (r.headers.get ('content-length'));
              return n || (r ? r.blob ().then (function (b) { return b.size; }) : 0);
            });
          }));
        });
      }).then (function (l) { return l.reduce (function (a, b) { return a + b; }, 0); },
               function () { return 0; });
    var browser = (navigator.storage && navigator.storage.estimate) ?
      navigator.storage.estimate ().then (function (e) { return e.usage || 0; },
                                          function () { return 0; }) : Promise.resolve (0);
    return Promise.all ([cache, browser]).then (function (r) {
      return { home: home, cache: r[0], browser: r[1] };
    });
  }

  // everything the page keeps in the browser goes, and TeXmacs stops: its
  // saves of the home directory, its loop, its connection to the database
  // (a database which is open is not deleted); the page says so
  function removeAll () {
    tmStorageRemoved = true;
    try { if (Module.pauseMainLoop) Module.pauseMainLoop (); } catch (e) {}
    try {
      if (typeof IDBFS !== 'undefined' && IDBFS.dbs)
        for (var k in IDBFS.dbs) { try { IDBFS.dbs[k].close (); } catch (e) {} delete IDBFS.dbs[k]; }
    } catch (e) {}
    var jobs = [];
    if (window.indexedDB && indexedDB.databases)
      jobs.push (indexedDB.databases ().then (function (dbs) {
        return Promise.all (dbs.map (function (d) {
          return new Promise (function (ok) {
            var r = indexedDB.deleteDatabase (d.name); r.onsuccess = r.onerror = r.onblocked = ok;
          });
        }));
      }));
    else if (window.indexedDB)
      jobs.push (new Promise (function (ok) {
        var r = indexedDB.deleteDatabase ('/home/web'); r.onsuccess = r.onerror = r.onblocked = ok;
      }));
    if (window.caches)
      jobs.push (caches.keys ().then (function (ks) {
        return Promise.all (ks.map (function (k) { return caches.delete (k); }));
      }));
    try { localStorage.clear (); sessionStorage.clear (); } catch (e) {}
    Promise.all (jobs).then (done, done);
    function done () {
      document.title = 'TeXmacs Vue removed';
      document.body.innerHTML = '';
      var box = el ('div');
      box.style.cssText = 'max-width:460px;margin:15vh auto;padding:24px;background:#f6f6f6;' +
        'border:1px solid #999;border-radius:8px;font:14px -apple-system,"Fira Sans",Helvetica,sans-serif;' +
        'color:#222;line-height:1.45';
      box.appendChild (el ('div', null, 'TeXmacs Vue has been removed from this browser.'));
      var p = el ('div', null, 'Your files and preferences and the files of TeXmacs are no longer ' +
                               'kept here. Reload the page to start TeXmacs Vue again.');
      p.style.marginTop = '8px'; p.style.color = '#555';
      box.appendChild (p);
      var b = el ('button', null, 'Reload');
      b.style.marginTop = '14px';
      b.onclick = function () { location.reload (); };
      box.appendChild (b);
      document.body.appendChild (box);
    }
  }

  function human (n) {
    return n < 1e6 ? Math.round (n / 1e3) + ' KB' : (n / 1e6).toFixed (1) + ' MB';
  }

  function toggleMenu (button) {
    if (menu) { closeMenu (); return; }
    button.classList.add ('open');
    menu = el ('div');
    menu.id = 'tm-menu';
    function head (t) { menu.appendChild (el ('div', 'tm-head', t)); }
    function text (t) { var d = el ('div', 'tm-text', t); menu.appendChild (d); return d; }
    function sep () { menu.appendChild (el ('div', 'tm-sep')); }
    function item (t, f) {
      var d = el ('div', 'tm-item', t);
      d.onclick = function () { closeMenu (); f (); };
      menu.appendChild (d);
    }
    var h = el ('div', 'tm-head', 'TeXmacs Vue');
    h.appendChild (el ('span', 'tm-badge', 'experimental'));
    menu.appendChild (h);
    // a paragraph of texts and links ([text, url])
    function para (parts) {
      var d = el ('div', 'tm-text');
      parts.forEach (function (x) {
        if (typeof x === 'string') { d.appendChild (document.createTextNode (x)); return; }
        var a = el ('a', null, x[0]);
        a.href = x[1]; a.target = '_blank'; a.rel = 'noopener';
        d.appendChild (a);
      });
      menu.appendChild (d);
      return d;
    }
    para (['An experimental port of ', ['GNU TeXmacs', 'https://www.texmacs.org'], ' ' +
           (app.version || '') + ', the structured editor for scientists, running in this ' +
           'page: nothing is installed, and nothing leaves the browser unless you download it.']);
    para (['Vue is a new interface for TeXmacs (Clay, SDL3 and MuPDF), here on WebAssembly, ' +
           'with the ' + (app.scheme || 'S7') + ' Scheme and OpenType fonts, OpenType ' +
           'mathematics included. Expect rough edges. ',
           ['Sources and notes on GitHub', 'https://github.com/mgubi/texmacs/tree/wip_wasm_vue'],
           '.']);
    // the software this page is made of, with their versions as the program
    // reports them (gui_open in vue_gui.cpp), and their pages
    var soft = [
      ['MuPDF', 'https://mupdf.com', app.mupdf, 'the pixels, the pictures, the PDF'],
      ['SDL', 'https://www.libsdl.org', app.sdl, 'the window, the input'],
      ['S7 Scheme', 'https://ccrma.stanford.edu/software/snd/snd/s7.html',
       app.s7 ? app.s7 + (app.s7date ? ' (' + app.s7date + ')' : '') : '',
       'the extension language'],
      ['Emscripten', 'https://emscripten.org', app.emscripten, 'the compiler to WebAssembly']
    ];
    var grid = el ('div', 'tm-soft');
    soft.forEach (function (e) {
      if (!e[2]) return;
      var a = el ('a', null, e[0]);
      a.href = e[1]; a.target = '_blank'; a.rel = 'noopener';
      a.title = e[3];
      grid.appendChild (a);
      grid.appendChild (el ('span', 'tm-ver', e[2]));
    });
    menu.appendChild (grid);
    text ('Built ' + (app.built || '') + '.');
    sep ();
    var files = text ('Files of TeXmacs: …');
    if (typeof tmPackages !== 'undefined' && tmPackages.manifest ()) {
      var m = tmPackages.manifest (), s = tmPackages.stats;
      files.textContent = 'Files of TeXmacs: ' + s.loaded + ' of ' + m.packages.length +
        ' packages loaded' + (s.onDemand ? ', ' + s.onDemand + ' files fetched on demand' : '') + '.';
    }
    var storage = text ('Your files and preferences are kept in the storage of this browser.');
    storageUse ().then (function (u) {
      storage.textContent = 'Kept in this browser: your files and preferences, ' +
        human (u.home) + ', and the files of TeXmacs, ' + human (u.cache) + '.' +
        (u.browser ? ' (The browser counts ' + human (u.browser) + ' for this site, with ' +
                     'the space it has not given back yet.)' : '');
    });
    sep ();
    item ('Files of the page…', function () { tmFiles.browse (); });
    item ('Keyboard…', function () {
      window.alert ('Keyboard shortcuts in the browser\n\n' +
        'TeXmacs uses its usual shortcuts, but the browser keeps some of them ' +
        'for itself (new window, new tab, close tab, reload...). ' +
        'Use the menus of TeXmacs, or the + of the tabs, for those.\n\n' +
        'Opening and saving go through the Files panel: files come in by ' +
        'upload or by dropping them on the page, and go out as downloads.');
    });
    sep ();
    item ('Reload', function () { location.reload (); });
    item ('Reset…', function () {
      if (!window.confirm ('Delete your files and preferences kept in this browser, ' +
                           'and the files of TeXmacs it keeps, and reload?')) return;
      var jobs = [];
      if (window.indexedDB && indexedDB.databases)
        jobs.push (indexedDB.databases ().then (function (dbs) {
          return Promise.all (dbs.map (function (d) {
            return new Promise (function (ok) {
              var r = indexedDB.deleteDatabase (d.name); r.onsuccess = r.onerror = r.onblocked = ok;
            });
          }));
        }));
      if (window.caches) jobs.push (caches.delete ('texmacs-packages'));
      Promise.all (jobs).then (function () { location.reload (); });
    });
    item ('Remove from this browser…', function () {
      if (!window.confirm ('Remove TeXmacs Vue from this browser?\n\n' +
                           'This deletes everything this page keeps in the browser: ' +
                           'your files and preferences (download the ones you want to ' +
                           'keep first, from Files of the page) and the files of TeXmacs. ' +
                           'TeXmacs stops; it is downloaded again if you come back.')) return;
      removeAll ();
    });
    document.body.appendChild (menu);
  }

  if (typeof document !== 'undefined') {
    if (document.readyState === 'loading') document.addEventListener ('DOMContentLoaded', build);
    else build ();
  }

  return {
    update: function (state) { tabs = state.tabs || []; render (); },
    info: function (d) { app = d || {}; },
    tabs: function () { return tabs; }
  };
})();
