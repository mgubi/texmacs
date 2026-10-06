// The frame of the page (a --pre-js of the browser build): a column at the
// left of the canvas with a TeXmacs menu and the tabs of the windows of
// TeXmacs. The column folds down to the logo and small tabs (the initials
// of the windows, their names in a balloon over them), which the browser
// remembers (localStorage).
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

  // the logo of TeXmacs Vue (misc/icons/vue-logo), next to the page (see
  // WEB_ICONS in misc/wasm/Makefile)
  function logo (cls, srcset) {
    var img = el ('img', cls);
    img.srcset = srcset;
    img.src = srcset.split (' ')[0];
    img.alt = '';
    img.draggable = false;
    return img;
  }

  var style = `
    #tm-frame { position:relative; display:flex; flex-direction:column; width:200px; flex:none; background:#d8d8d8;
      border-right:1px solid #a8a8a8; font:13px -apple-system,"Fira Sans",Helvetica,sans-serif;
      color:#222; user-select:none; overflow:hidden }
    #tm-frame.collapsed { width:44px !important }
    #tm-frame .tm-resize { position:absolute; top:0; right:0; width:5px; height:100%;
      cursor:col-resize; z-index:5 }
    #tm-frame .tm-resize:hover, #tm-frame .tm-resize.dragging { background:rgba(91,127,168,.45) }
    body.tm-resizing, body.tm-resizing * { cursor:col-resize !important; user-select:none }
    #tm-frame .tm-app { display:flex; align-items:center; height:36px; flex:none; padding:0 12px;
      font-weight:bold; cursor:pointer; color:#fff; background:#5b7fa8;
      border-bottom:1px solid #4a6b91; white-space:nowrap }
    #tm-frame .tm-app:hover, #tm-frame .tm-app.open { background:#6a8db5 }
    #tm-frame .tm-app .tm-logo { width:20px; height:20px; margin-right:7px; flex:none }
    #tm-frame.collapsed .tm-app { padding:0; justify-content:center }
    #tm-frame.collapsed .tm-app .tm-logo { width:24px; height:24px; margin:0 }
    #tm-frame.collapsed .tm-app .tm-name { display:none }
    #tm-frame .tm-tabs { flex:1; min-height:0; overflow-y:auto; overflow-x:hidden;
      scrollbar-width:none; padding:5px 0 }
    #tm-frame .tm-tabs::-webkit-scrollbar { display:none }
    #tm-frame .tm-tab { position:relative; display:flex; align-items:center; height:28px;
      margin:1px 5px; padding:0 3px 0 9px; border-radius:5px; cursor:default }
    #tm-frame .tm-tab:hover { background:#cacaca }
    #tm-frame .tm-tab.active { background:#f6f6f6; box-shadow:inset 0 0 0 1px #b4b4b4 }
    #tm-frame .tm-tab .tm-title { flex:1; overflow:hidden; white-space:nowrap; text-overflow:ellipsis }
    #tm-frame .tm-tab .tm-close { flex:none; margin-left:4px; width:18px; height:18px;
      line-height:18px; text-align:center; border-radius:3px; color:#555; visibility:hidden }
    #tm-frame .tm-tab:hover .tm-close, #tm-frame .tm-tab.active .tm-close { visibility:visible }
    #tm-frame .tm-tab .tm-close:hover { background:#bbb; color:#000 }
    #tm-frame .tm-tab .tm-short { display:none; font-size:11.5px; font-weight:600; letter-spacing:.2px }
    #tm-frame .tm-tab .tm-dot { display:none; position:absolute; top:4px; right:4px; width:6px;
      height:6px; border-radius:3px; background:#5b7fa8 }
    #tm-frame.collapsed .tm-tab { justify-content:center; padding:0; margin:2px 6px; height:30px }
    #tm-frame.collapsed .tm-tab .tm-title, #tm-frame.collapsed .tm-tab .tm-close { display:none }
    #tm-frame.collapsed .tm-tab .tm-short { display:block }
    #tm-frame.collapsed .tm-tab.modified .tm-dot { display:block }
    #tm-frame .tm-new, #tm-frame .tm-fold { display:flex; align-items:center; flex:none; height:30px;
      padding:0 14px; cursor:pointer; color:#333; white-space:nowrap }
    #tm-frame .tm-new:hover, #tm-frame .tm-fold:hover { background:#c8c8c8 }
    #tm-frame .tm-new .tm-plus { font-size:17px; width:16px; text-align:center; margin-right:8px }
    #tm-frame .tm-fold { border-top:1px solid #c0c0c0; color:#555; justify-content:flex-end }
    #tm-frame .tm-fold svg, #tm-frame .tm-new svg { width:16px; height:16px; fill:none;
      stroke:currentColor; stroke-width:1.6; stroke-linecap:round; stroke-linejoin:round }
    #tm-frame.collapsed .tm-new, #tm-frame.collapsed .tm-fold { padding:0; justify-content:center }
    #tm-frame.collapsed .tm-new .tm-plus { margin:0 }
    #tm-frame.collapsed .tm-new .tm-label { display:none }
    #tm-balloon { position:fixed; z-index:35; padding:4px 9px; border-radius:5px; background:#333;
      color:#fff; font:12.5px -apple-system,"Fira Sans",Helvetica,sans-serif; white-space:nowrap;
      pointer-events:none; box-shadow:0 2px 8px rgba(0,0,0,.25); max-width:60vw; overflow:hidden;
      text-overflow:ellipsis }
    #tm-menu { position:fixed; left:4px; top:4px; width:340px; max-height:calc(100vh - 8px);
      overflow:auto; box-sizing:border-box; background:#f6f6f6;
      border:1px solid #999; border-radius:6px; box-shadow:0 6px 24px rgba(0,0,0,.3);
      font:13px -apple-system,"Fira Sans",Helvetica,sans-serif; color:#222; z-index:30; padding:6px 0 }
    #tm-menu .tm-head { padding:8px 14px 4px; font-weight:bold; font-size:14px;
      display:flex; align-items:center }
    #tm-menu .tm-head .tm-logo { width:40px; height:40px; margin-right:10px; flex:none }
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
    #tm-about { position:fixed; inset:0; z-index:40; background:rgba(0,0,0,.25);
      display:flex; align-items:center; justify-content:center; padding:16px }
    #tm-about .tm-box { position:relative; max-width:560px; max-height:min(80vh, calc(100vh - 32px)); overflow:auto;
      background:#f6f6f6; border:1px solid #999; border-radius:8px; box-shadow:0 6px 24px rgba(0,0,0,.3);
      padding:18px 22px; font:14px -apple-system,"Fira Sans",Helvetica,sans-serif; color:#222;
      line-height:1.45 }
    #tm-about h2 { font-size:16px; margin:0 0 8px }
    #tm-about h3 { font-size:14px; margin:14px 0 4px }
    #tm-about ul { margin:0; padding-left:20px }
    #tm-about li { margin:3px 0; color:#333 }
    #tm-about .tm-x { position:absolute; top:8px; right:10px; width:24px; height:24px; line-height:24px;
      text-align:center; border-radius:4px; cursor:pointer; color:#555; font-size:18px }
    #tm-about .tm-x:hover { background:#ddd; color:#000 }
    #tm-about .tm-box { user-select:text; -webkit-user-select:text }
    #tm-about p { margin:4px 0; color:#333 }
    #tm-about code { font:12.5px ui-monospace,Menlo,monospace; background:#e8e8e8;
      padding:1px 4px; border-radius:3px }
    #tm-about .tm-opt { margin:12px 0 }
    #tm-about .tm-name { font-size:13.5px; background:none; padding:0 }
    #tm-about b { font-weight:600; color:#111 }
    #tm-about .tm-line { display:flex; align-items:center; gap:6px; margin:4px 0 }
    #tm-about .tm-line code { flex:1; min-width:0; overflow-wrap:anywhere; padding:4px 6px }
    #tm-about .tm-button { flex:none; font:12px -apple-system,Helvetica,sans-serif; padding:3px 9px;
      border:1px solid #999; border-radius:4px; background:#fff; cursor:pointer; user-select:none }
    #tm-about .tm-button:hover { background:#eef3f9 }
    #tm-about .tm-icon { flex:none; display:flex; align-items:center; justify-content:center;
      width:26px; height:26px; padding:0; border:none; border-radius:4px; background:none;
      color:#888; cursor:pointer }
    #tm-about .tm-icon:hover { background:#dde3ea; color:#333 }
    #tm-about .tm-icon.done { color:#3a7d44 }
    #tm-about .tm-icon svg { width:15px; height:15px; fill:none; stroke:currentColor;
      stroke-width:1.6; stroke-linecap:round; stroke-linejoin:round }
    #tm-about .tm-buttons { display:flex; justify-content:flex-end; gap:8px; margin-top:14px }
    #tm-about .tm-buttons button { font-size:13px; padding:4px 14px }
    #tm-about .tm-default { background:#5b7fa8; color:#fff; border-color:#4a6b91 }
    #tm-about .tm-default:hover { background:#6a8db5 }
    #tm-about input { width:100%; box-sizing:border-box; font:13px ui-monospace,Menlo,monospace;
      padding:4px 6px; border:1px solid #999; border-radius:4px }
    #tm-about .tm-note { font-size:12.5px; color:#666 }
    #tm-about .tm-format { display:block; margin:10px 0 0; font-size:13px }
    #tm-about .tm-format select { margin-left:4px; font:13px -apple-system,"Fira Sans",Helvetica,sans-serif }
  `;

  // folded (only the logo and small tabs) or not, as the browser remembers
  // it; a narrow page starts folded
  var FOLD = 'texmacs-sidebar';
  function folded () {
    var v = null;
    try { v = localStorage.getItem (FOLD); } catch (e) {}
    if (v === 'collapsed') return true;
    if (v === 'expanded') return false;
    return typeof window !== 'undefined' && window.innerWidth < 900;
  }

  // the width of the column when it is open, which its right edge changes
  // (a drag; a double click gives the default back), as the browser
  // remembers it. A drag below FOLD_AT folds the column, and a drag of the
  // folded column beyond MIN_WIDTH opens it again
  var WIDTH = 'texmacs-sidebar-width', DEFAULT_WIDTH = 200, MIN_WIDTH = 100, FOLD_AT = 80;
  function clampWidth (w) {
    var most = Math.max (MIN_WIDTH, Math.min (480, Math.floor (window.innerWidth / 2)));
    return Math.max (MIN_WIDTH, Math.min (most, Math.round (w)));
  }
  function savedWidth () {
    var v = null;
    try { v = Number (localStorage.getItem (WIDTH)); } catch (e) {}
    return clampWidth (v > 0 ? v : DEFAULT_WIDTH);
  }
  var width = DEFAULT_WIDTH;
  function setWidth (w, remember) {
    width = clampWidth (w);
    if (bar) bar.style.width = width + 'px';
    if (remember) try { localStorage.setItem (WIDTH, String (width)); } catch (e) {}
    resized ();
  }
  // TeXmacs follows the width it is left. At once, from the event which
  // changed it (a move of the mouse comes before the frame, at most one per
  // frame): a change of the size of the canvas clears it, and TeXmacs draws
  // in the callbacks of the frame (emscripten_set_main_loop), so that a
  // resize sent from a callback of its own came after the drawing, and the
  // frame showed an empty canvas (the page flickered during a drag)
  function resized () {
    if (menu) placeMenu ();
    window.dispatchEvent (new Event ('resize'));
  }
  function edge (handle) {
    var startX = 0, startW = 0, openW = 0, dragging = false;
    handle.addEventListener ('pointerdown', function (e) {
      if (e.button !== 0) return;
      e.preventDefault ();
      hideBalloon ();
      dragging = true; startX = e.clientX; startW = bar.getBoundingClientRect ().width;
      openW = width; // the width given back when the drag folds the column
      handle.setPointerCapture (e.pointerId);
      handle.classList.add ('dragging');
      document.body.classList.add ('tm-resizing');
    });
    handle.addEventListener ('pointermove', function (e) {
      if (!dragging) return;
      var w = startW + e.clientX - startX, folded = bar.classList.contains ('collapsed');
      if (!folded && w < FOLD_AT) {
        width = openW; bar.style.width = width + 'px';
        setFolded (true, false);
      }
      else if (folded && w >= MIN_WIDTH) { setFolded (false, false); setWidth (w, false); }
      else if (!folded) setWidth (w, false);
    });
    function end (e) {
      if (!dragging) return;
      dragging = false;
      handle.classList.remove ('dragging');
      document.body.classList.remove ('tm-resizing');
      var folded = bar.classList.contains ('collapsed');
      setFolded (folded, true);
      if (!folded) setWidth (width, true);
    }
    handle.addEventListener ('pointerup', end);
    handle.addEventListener ('pointercancel', end);
    handle.addEventListener ('dblclick', function () {
      if (bar.classList.contains ('collapsed')) setFolded (false, true);
      setWidth (DEFAULT_WIDTH, true);
    });
  }

  function svg (d) {
    return '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="' + d + '"/></svg>';
  }
  var FOLD_ICON = 'M10 3.5 5.5 8 10 12.5', UNFOLD_ICON = 'M6 3.5 10.5 8 6 12.5';

  var fold = null, newButton = null, balloon = null;
  function build () {
    if (bar || typeof document === 'undefined') return;
    var st = el ('style'); st.textContent = style; document.head.appendChild (st);
    bar = document.getElementById ('tm-frame');
    if (!bar) return;
    var appButton = el ('div', 'tm-app');
    appButton.appendChild (logo ('tm-logo', 'texmacs-vue-32.png 1x, texmacs-vue-48.png 2x'));
    appButton.appendChild (el ('span', 'tm-name', 'TeXmacs Vue'));
    appButton.title = 'About TeXmacs Vue, an experimental port of GNU TeXmacs';
    appButton.onclick = function (e) { e.stopPropagation (); toggleMenu (appButton); };
    strip = el ('div', 'tm-tabs');
    newButton = el ('div', 'tm-new');
    newButton.appendChild (el ('span', 'tm-plus', '+'));
    newButton.appendChild (el ('span', 'tm-label', 'New window'));
    newButton.onclick = function () { hideBalloon (); _vue_web_new_tab (); };
    hover (newButton, function () { return 'New window'; });
    fold = el ('div', 'tm-fold');
    fold.onclick = function () { hideBalloon (); setFolded (!bar.classList.contains ('collapsed'), true); };
    hover (fold, function () { return bar.classList.contains ('collapsed') ? 'Show the names of the windows'
                                                                            : 'Fold the column'; });
    var handle = el ('div', 'tm-resize');
    handle.title = 'Drag to change the width; double-click for the default';
    edge (handle);
    bar.appendChild (appButton);
    bar.appendChild (strip);
    bar.appendChild (newButton);
    bar.appendChild (fold);
    bar.appendChild (handle);
    width = savedWidth ();
    bar.style.width = width + 'px';
    setFolded (folded (), false);
    // a smaller page may leave the column too wide
    window.addEventListener ('resize', function () {
      if (clampWidth (width) !== width) setWidth (width, false);
    });
    // a press outside the menu closes it; not one on the TeXmacs button,
    // whose click toggles it (else the press closed it and the click
    // opened it again)
    document.addEventListener ('mousedown', function (e) {
      if (menu && !menu.contains (e.target) && !appButton.contains (e.target)) closeMenu ();
    });
  }

  // the column folded or not; TeXmacs takes the width it leaves (SDL
  // follows the size of the canvas when the window is resized)
  function setFolded (on, remember) {
    if (!bar) return;
    bar.classList.toggle ('collapsed', on);
    fold.innerHTML = svg (on ? UNFOLD_ICON : FOLD_ICON);
    if (remember) try { localStorage.setItem (FOLD, on ? 'collapsed' : 'expanded'); } catch (e) {}
    resized ();
  }

  // the balloon of an element of the column, at its right: the names of the
  // windows when the column is folded (text () gives it, or nothing)
  function hover (e, text) {
    e.addEventListener ('mouseenter', function () {
      var t = text ();
      if (!t) return;
      if (!balloon) { balloon = el ('div'); balloon.id = 'tm-balloon'; document.body.appendChild (balloon); }
      balloon.textContent = t;
      balloon.style.display = 'block';
      var r = e.getBoundingClientRect ();
      balloon.style.left = (r.right + 8) + 'px';
      balloon.style.top = Math.max (4, r.top + r.height / 2 - balloon.offsetHeight / 2) + 'px';
    });
    e.addEventListener ('mouseleave', hideBalloon);
  }
  function hideBalloon () { if (balloon) balloon.style.display = 'none'; }

  // the short name of a window in the folded column: the initials of its
  // first two words ("Help - Welcome..." gives HW, "paper.tm" gives P), or
  // its first initial and its number ("No name [2]" gives N2)
  function initials (title) {
    title = String (title);
    var words = title.replace (/\.[a-z0-9]+$/i, '').match (/[\p{L}\p{N}]+/gu) || ['?'];
    var n = /\[(\d+)\]\s*$/.exec (title);
    if (n) return (words[0].charAt (0).toUpperCase () + n[1]).slice (0, 3);
    return words.slice (0, 2).map (function (w) { return w.charAt (0).toUpperCase (); }).join ('');
  }

  function render () {
    build ();
    if (!strip) return;
    hideBalloon ();
    strip.textContent = '';
    tabs.forEach (function (t) {
      var tab = el ('div', 'tm-tab' + (t.active ? ' active' : '') + (t.modified ? ' modified' : ''));
      tab.dataset.id = t.id;
      var name = (t.modified ? '• ' : '') + t.title;
      tab.appendChild (el ('span', 'tm-title', name));
      tab.appendChild (el ('span', 'tm-short', initials (t.title)));
      tab.appendChild (el ('span', 'tm-dot'));
      // the name in a balloon when the column is folded, or when it does
      // not fit in the tab
      hover (tab, function () {
        var title = tab.querySelector ('.tm-title');
        return (bar.classList.contains ('collapsed') ||
                title.scrollWidth > title.clientWidth) ? name : '';
      });
      tab.onclick = function () { _vue_web_activate_tab (t.id); };
      tab.onmousedown = function (e) {
        if (e.button === 1) { e.preventDefault (); if (tabs.length > 1) _vue_web_close_tab (t.id); }
      };
      if (tabs.length > 1) {
        var x = el ('span', 'tm-close', '×');
        x.title = 'Close';
        x.onmousedown = function (e) { e.stopPropagation (); };
        x.onclick = function (e) { e.stopPropagation (); hideBalloon (); _vue_web_close_tab (t.id); };
        tab.appendChild (x);
      }
      strip.appendChild (tab);
    });
    var active = tabs.filter (function (t) { return t.active; })[0];
    var at = strip.querySelector ('.tm-tab.active');
    if (at) {
      var top = at.offsetTop - strip.offsetTop, bottom = top + at.offsetHeight;
      if (top < strip.scrollTop) strip.scrollTop = top;
      else if (bottom > strip.scrollTop + strip.clientHeight) strip.scrollTop = bottom - strip.clientHeight;
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
  // of TeXmacs (in the Cache Storage), counted here (not with
  // navigator.storage.estimate: Safari's count grows with each download
  // and does not go down when the data is deleted)
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
    return cache.then (function (c) { return { home: home, cache: c }; });
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

  // more about the port and its limitations, from the text of the menu
  var about = [
    ['In the browser', [
      'The windows of TeXmacs are the tabs in the column at the left of the page (its ' +
      'chevron folds it to small tabs); the dialogs float over the page.',
      'TeXmacs uses its usual shortcuts, but the browser keeps some of them for itself ' +
      '(new window, new tab, close tab, reload...): use the menus of TeXmacs, or the + ' +
      'of the tabs, for those.',
      'Copy, cut and paste go through the clipboard of the system; on a Mac the ' +
      'shortcuts use \u2318 (Cmd), as the browser\'s.',
      'Opening and saving go through the Files panel (Files in this browser...): files are ' +
      'added from your computer or dropped on the page, and a copy of one is saved back ' +
      'on your computer with its "save copy". The files of TeXmacs can be browsed there, ' +
      'and copied to your own to customize them.',
      'Printing opens the document as a PDF in a new tab, to print from there.']],
    ['Limitations', [
      'Your files are kept in the storage of this browser, for this site only: clearing ' +
      'the data of the site deletes them, and Safari deletes the data of a site which was ' +
      'not visited for seven days. Save a copy of the files you want to keep.',
      'No plugins and no sessions (Maxima, Python, R...): a page cannot run other ' +
      'programs. For the same reason, the converters and tools which need an external ' +
      'program (LaTeX, Ghostscript, ImageMagick, the spell checker, Git) are missing.',
      'Only the fonts which come with TeXmacs: the page cannot see the fonts of the system.',
      'The remote tools (the Remote menu) connect over WebSocket to a TeXmacs server of ' +
      'this branch, on this machine only for now (no encrypted wss yet).',
      'TeXmacs Vue keeps your files from one tab of the browser at a time: in another ' +
      'tab it shows them, but does not keep its changes, until you move TeXmacs there ' +
      '("Use TeXmacs here").',
      'It is slower than the desktop program. Its first visit loads some 9 MB before it ' +
      'starts and 8 MB more in the background; the fonts come when a document first uses them.']]
  ];

  function showAbout () {
    dialog ('TeXmacs Vue: more info and limitations', function (box) {
      about.forEach (function (sec) {
        box.appendChild (el ('h3', null, sec[0]));
        var ul = el ('ul');
        sec[1].forEach (function (t) { ul.appendChild (el ('li', null, t)); });
        box.appendChild (ul);
      });
    });
  }

  // The options of the address of the page (texmacs.html?...), as the
  // scripts read them: files.js (open), web-pre.js (profile, trace-files),
  // clipboard.js (trace-clipboard), packages.js (no-background)
  // [name, its value ('' for a flag), an example, what it does]; the
  // texts are in the markup of rich (): **...** in bold
  var addressOptions = [
    ['open', '<url>', 'open=https://example.org/paper.tm',
     'Opens the document at **<url>** in a tab of its own once TeXmacs runs: a link to ' +
     'the page with **open** is a viewer of the document. **<url>** is absolute, or ' +
     'relative to the page, written as a parameter (%20 for a space...). Another site ' +
     'has to allow the page to read it (CORS: Access-Control-Allow-Origin). Any format ' +
     'TeXmacs opens (.tm, .tex, .html, .md...); the images and files the document ' +
     'refers to are not fetched with it. It is not kept in the storage of the ' +
     'browser: Save as keeps it.'],
    ['x', '<command>', 'open=https://example.org/paper.tm&x=' +
       encodeURIComponent ('(change-zoom-factor 1.5)'),
     'Runs the Scheme **<command>**, as texmacs -x <command>: once TeXmacs runs, after ' +
     'the document of **open**. Several **x** run in their order. The page shows ' +
     'the commands and asks before running them.'],
    ['debug', '<flags>', 'debug=events,keyboard',
     'Debugging messages of TeXmacs in the console of the browser, as the ' +
     '-debug-<flag> options of the command line. **<flags>**: among **std**, ' +
     '**events**, **io**, **keyboard**, **convert**, **parser**, **correct**, ' +
     '**packrat**, **flatten**, **history**, **bench**, **remote**, **live**, ' +
     '**sockets**, **gnutls**, **all**, joined with commas.'],
    ['verbose', '', 'verbose', 'More messages of TeXmacs in the console, as -V.'],
    ['profile', '<n>', 'profile=60',
     'Prints the profile of the main loop of TeXmacs in the console of the browser, ' +
     'every **<n>** frames.'],
    ['trace-files', '', 'trace-files',
     'Records the files of TeXmacs opened, in their order, in window.tmTrace (to make ' +
     'the list of the files needed at boot).'],
    ['trace-clipboard', '', 'trace-clipboard',
     'Logs each paste, and what the clipboard brought, in the console.'],
    ['no-background', '', 'no-background',
     'Does not download the files of TeXmacs in the background once it runs: they ' +
     'come on demand only (to test that path).']
  ];

  // an element of text with parts in bold: **...**
  function rich (tag, text) {
    var e = el (tag);
    text.split ('**').forEach (function (part, k) {
      if (part === '') return;
      if (k % 2 === 1) e.appendChild (el ('b', null, part));
      else e.appendChild (document.createTextNode (part));
    });
    return e;
  }

  // the address of this page with the options q (without the old ones)
  function pageAddress (q) {
    return location.origin + location.pathname + (q ? '?' + q : '');
  }

  // a small line drawing: the paths of a 16x16 box
  function icon (paths) {
    var ns = 'http://www.w3.org/2000/svg', svg = document.createElementNS (ns, 'svg');
    svg.setAttribute ('viewBox', '0 0 16 16');
    svg.setAttribute ('aria-hidden', 'true');
    paths.forEach (function (d) {
      var p = document.createElementNS (ns, 'path');
      p.setAttribute ('d', d);
      svg.appendChild (p);
    });
    return svg;
  }
  var COPY_ICON = ['M5.5 5.5h7a1 1 0 0 1 1 1v7a1 1 0 0 1-1 1h-7a1 1 0 0 1-1-1v-7a1 1 0 0 1 1-1z',
                   'M2.5 10.5v-7a1 1 0 0 1 1-1h7'];
  var DONE_ICON = ['M3 8.5l3.2 3.2L13 4.8'];

  // a button which copies get () to the clipboard; a check mark says it did
  function copyButton (get) {
    var b = el ('button', 'tm-icon');
    b.title = 'Copy';
    b.setAttribute ('aria-label', 'Copy');
    b.appendChild (icon (COPY_ICON));
    b.onclick = function () {
      var t = get (), done = function () {
        b.replaceChildren (icon (DONE_ICON));
        b.classList.add ('done');
        b.title = 'Copied';
        setTimeout (function () {
          b.replaceChildren (icon (COPY_ICON));
          b.classList.remove ('done');
          b.title = 'Copy';
        }, 1500);
      };
      if (navigator.clipboard && navigator.clipboard.writeText)
        navigator.clipboard.writeText (t).then (done, function () { fallback (t); done (); });
      else { fallback (t); done (); }
    };
    function fallback (t) {
      var a = document.createElement ('textarea');
      a.value = t; a.style.cssText = 'position:fixed;left:-1000px;top:0;opacity:0';
      document.body.appendChild (a); a.select ();
      try { document.execCommand ('copy'); } catch (e) {}
      a.remove ();
    }
    return b;
  }
  // a line of code with its Copy button
  function codeLine (box, get) {
    var line = el ('div', 'tm-line');
    var c = el ('code', null, get ());
    line.appendChild (c);
    line.appendChild (copyButton (get));
    box.appendChild (line);
    return c;
  }

  function showAddressOptions () {
    dialog ('The address of the page', function (box) {
      box.appendChild (rich ('p', 'Options go after the address of the page: ' +
        'texmacs.html?**option**, or texmacs.html?**option**=**value**. They are read ' +
        'when the page loads.'));
      box.appendChild (rich ('p', 'Several options are joined with **&**, in any order: ' +
        'texmacs.html?**open**=paper.tm**&x**=(...)**&verbose**. A value is written as ' +
        'a parameter of an address: **%20** for a space, **%26** for &, **%3D** for = ' +
        '(the lines to copy below are).'));
      box.appendChild (el ('h3', null, 'A link to open a document'));
      box.appendChild (rich ('p', 'The address of a document (.tm, .tex, .html, .md...), ' +
        'and the link with **open** which opens it in TeXmacs Vue:'));
      var input = el ('input');
      input.type = 'url';
      input.placeholder = 'https://example.org/paper.tm';
      box.appendChild (input);
      var link = function () {
        var u = input.value.trim () || input.placeholder;
        return pageAddress ('open=' + encodeURIComponent (u));
      };
      var out = codeLine (box, link);
      input.oninput = function () { out.textContent = link (); };
      box.appendChild (el ('h3', null, 'The options'));
      addressOptions.forEach (function (o) {
        var d = el ('div', 'tm-opt');
        var name = el ('code', 'tm-name');
        name.appendChild (el ('b', null, o[0]));
        if (o[1]) name.appendChild (document.createTextNode ('=' + o[1]));
        d.appendChild (name);
        d.appendChild (rich ('p', o[3]));
        codeLine (d, function () { return pageAddress (o[2]); });
        box.appendChild (d);
      });
    });
  }

  // a dialog above the page, closed by its x, a click beside it, or Escape;
  // fill (box, close) makes its contents, closed (true when a button of
  // them closed it) is called once it is closed
  function dialog (title, fill, closed) {
    var old = document.getElementById ('tm-about');
    if (old) old.remove ();
    var back = el ('div'); back.id = 'tm-about';
    var box = el ('div', 'tm-box');
    var x = el ('div', 'tm-x', '\u00d7'); x.title = 'Close';
    box.appendChild (x);
    box.appendChild (el ('h2', null, title));
    fill (box, function () { close (true); });
    back.appendChild (box);
    // the keys are for the popup, not for TeXmacs below it
    var done = false;
    function close (byButton) {
      if (done) return;
      done = true;
      back.remove ();
      ['keydown', 'keypress', 'keyup'].forEach (function (t) { window.removeEventListener (t, key, true); });
      if (closed) closed (byButton === true);
    }
    function key (e) {
      e.stopPropagation ();
      if (e.key === 'Escape') { e.preventDefault (); if (e.type === 'keydown') close (false); }
    }
    x.onclick = function () { close (false); };
    back.addEventListener ('mousedown', function (e) { e.stopPropagation (); if (e.target === back) close (false); });
    ['keydown', 'keypress', 'keyup'].forEach (function (t) { window.addEventListener (t, key, true); });
    document.body.appendChild (back);
  }

  // ask before doing something: the lines of code it would run, and a
  // button for it; true when it was pressed (files.js: ?x=)
  function ask (title, text, lines, okLabel) {
    return new Promise (function (answer) {
      var ok = false;
      dialog (title, function (box, close) {
        box.appendChild (el ('p', null, text));
        lines.forEach (function (t) {
          var line = el ('div', 'tm-line');
          line.appendChild (el ('code', null, t));
          box.appendChild (line);
        });
        var bar = el ('div', 'tm-buttons');
        var no = el ('button', 'tm-button', 'Cancel'), yes = el ('button', 'tm-button tm-default', okLabel);
        no.onclick = function () { close (); };
        yes.onclick = function () { ok = true; close (); };
        bar.appendChild (no); bar.appendChild (yes);
        box.appendChild (bar);
        setTimeout (function () { no.focus (); }, 0);
      }, function () { answer (ok); });
    });
  }

  // the menu beside the column, at the top
  function placeMenu () {
    if (!menu || !bar) return;
    menu.style.left = (bar.getBoundingClientRect ().right + 4) + 'px';
  }

  function toggleMenu (button) {
    if (menu) { closeMenu (); return; }
    hideBalloon ();
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
    var h = el ('div', 'tm-head');
    h.appendChild (logo ('tm-logo', 'texmacs-vue-64.png 1x, texmacs-vue-128.png 2x'));
    h.appendChild (document.createTextNode ('TeXmacs Vue'));
    h.appendChild (el ('span', 'tm-badge', 'experimental'));
    menu.appendChild (h);
    // a paragraph of texts and links ([text, url], or [text, function])
    function para (parts) {
      var d = el ('div', 'tm-text');
      parts.forEach (function (x) {
        if (typeof x === 'string') { d.appendChild (document.createTextNode (x)); return; }
        var a = el ('a', null, x[0]);
        if (typeof x[1] === 'function') {
          a.href = '#';
          a.onclick = function (e) { e.preventDefault (); closeMenu (); x[1] (); };
        }
        else { a.href = x[1]; a.target = '_blank'; a.rel = 'noopener'; }
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
           'mathematics included. Expect rough edges: ',
           ['more info and limitations', showAbout], '. ',
           ['Sources and notes on GitHub', 'https://github.com/mgubi/texmacs/tree/wip_wasm_vue'],
           '.']);
    para (['A link to this page can open a document from the web, and pass options and ' +
           'Scheme commands to TeXmacs: see ', ['the options of the address', showAddressOptions],
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
        human (u.home) + ', and the files of TeXmacs, ' + human (u.cache) + '.';
    });
    sep ();
    item ('Files in this browser…', function () { tmFiles.browse (); });
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
                           'your files and preferences (save a copy of the ones you want ' +
                           'to keep first, from Files in this browser) and the files of TeXmacs. ' +
                           'TeXmacs stops; it is downloaded again if you come back.')) return;
      removeAll ();
    });
    document.body.appendChild (menu);
    placeMenu ();
  }

  if (typeof document !== 'undefined') {
    if (document.readyState === 'loading') document.addEventListener ('DOMContentLoaded', build);
    else build ();
  }

  // Presentation mode (the plugin, vue_virtual_window_rep::set_full_screen):
  // the frame goes, and the page asks the browser for the full screen. The
  // browser grants it only shortly after an action of the user (the key or
  // the menu which asked for it); when it does not, the slides still take
  // the whole page. Leaving the full screen from the browser (Escape) also
  // leaves presentation mode.
  var presenting = false;
  function fullScreenElement () {
    return document.fullscreenElement || document.webkitFullscreenElement || null;
  }
  function fullScreen (on) {
    presenting = on;
    if (bar) bar.style.display = on ? 'none' : '';
    var d = document, e = d.documentElement;
    try {
      if (on && !fullScreenElement ()) {
        var p = e.requestFullscreen ? e.requestFullscreen ()
              : (e.webkitRequestFullscreen ? e.webkitRequestFullscreen () : null);
        if (p && p.catch) p.catch (function () {});
      }
      else if (!on && fullScreenElement ()) {
        var q = d.exitFullscreen ? d.exitFullscreen ()
              : (d.webkitExitFullscreen ? d.webkitExitFullscreen () : null);
        if (q && q.catch) q.catch (function () {});
      }
    } catch (err) {}
    // the canvas takes the place of the frame: SDL follows the size of the
    // canvas when the window is resized
    window.dispatchEvent (new Event ('resize'));
  }
  function fullScreenChanged () {
    if (!fullScreenElement () && presenting && typeof _vue_web_scheme !== 'undefined')
      withStackSave (function () {
        _vue_web_scheme (stringToUTF8OnStack ('(when (full-screen?) (toggle-full-screen-mode))'));
      });
  }
  if (typeof document !== 'undefined') {
    document.addEventListener ('fullscreenchange', fullScreenChanged);
    document.addEventListener ('webkitfullscreenchange', fullScreenChanged);
  }

  return {
    update: function (state) { tabs = state.tabs || []; render (); },
    info: function (d) { app = d || {}; },
    tabs: function () { return tabs; },
    fullScreen: fullScreen,
    ask: ask,
    dialog: dialog
  };
})();
