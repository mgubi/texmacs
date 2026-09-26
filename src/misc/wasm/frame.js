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
    #tm-frame .tm-tabs { display:flex; flex:1; overflow-x:auto; overflow-y:hidden }
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
    #tm-menu .tm-text { padding:2px 14px; color:#444 }
    #tm-menu .tm-sep { height:1px; background:#ccc; margin:6px 0 }
    #tm-menu .tm-item { padding:5px 14px; cursor:pointer }
    #tm-menu .tm-item:hover { background:#dde6f0 }
    #tm-menu a { color:#036 }
  `;

  function build () {
    if (bar || typeof document === 'undefined') return;
    var st = el ('style'); st.textContent = style; document.head.appendChild (st);
    bar = document.getElementById ('tm-frame');
    if (!bar) return;
    var appButton = el ('div', 'tm-app', 'TeXmacs');
    appButton.title = 'About this TeXmacs';
    appButton.onclick = function (e) { e.stopPropagation (); toggleMenu (appButton); };
    strip = el ('div', 'tm-tabs');
    var plus = el ('div', 'tm-new', '+');
    plus.title = 'New window';
    plus.onclick = function () { _vue_web_new_tab (); };
    bar.appendChild (appButton);
    bar.appendChild (strip);
    bar.appendChild (plus);
    document.addEventListener ('mousedown', function (e) {
      if (menu && !menu.contains (e.target)) closeMenu ();
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
      tab.onmousedown = function (e) {
        if (e.button === 0) _vue_web_activate_tab (t.id);
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
    document.title = active ? (active.modified ? '• ' : '') + active.title + ' — TeXmacs'
                            : 'GNU TeXmacs';
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
    head ('GNU TeXmacs ' + (app.version || ''));
    text ('A structured editor for scientists, running in this page: nothing is installed, ' +
          'and nothing leaves the browser unless you download it.');
    text ('WebAssembly build (' + (app.scheme || 'S7') + ' Scheme, MuPDF ' + (app.mupdf || '') +
          '), built ' + (app.built || '') + '.');
    sep ();
    var files = text ('Files of TeXmacs: …');
    if (typeof tmPackages !== 'undefined' && tmPackages.manifest ()) {
      var m = tmPackages.manifest (), s = tmPackages.stats;
      files.textContent = 'Files of TeXmacs: ' + s.loaded + ' of ' + m.packages.length +
        ' packages loaded' + (s.onDemand ? ', ' + s.onDemand + ' files fetched on demand' : '') + '.';
    }
    var storage = text ('Your files: kept in the storage of this browser for this site.');
    if (navigator.storage && navigator.storage.estimate)
      navigator.storage.estimate ().then (function (e) {
        storage.textContent = 'Your files and preferences are kept in the storage of this ' +
          'browser for this site: ' + human (e.usage || 0) + ' used.';
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
    item ('texmacs.org', function () { window.open ('https://www.texmacs.org', '_blank'); });
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
