// The clipboard of the system in the page (a --pre-js of the browser build).
//
// TeXmacs reads the clipboard when it pastes, synchronously; the browser
// gives its contents only to a paste event, or asynchronously and with the
// consent of the user (navigator.clipboard.read). So the page keeps what it
// knows of the clipboard (tmClipboard.read): what TeXmacs copied last, or
// what the last paste event brought.
//
// Copy: TeXmacs writes its text (and its HTML, when it has one) with
// navigator.clipboard, which Firefox and Chrome allow just after a key or a
// click of the user, which is when TeXmacs copies. Safari allows it only in
// the handler of the key itself, and TeXmacs copies a frame later: for the
// keys of a copy (Ctrl+C, Cmd+C, Ctrl+X, Cmd+X) the page starts the write
// in their keydown, with a promise of the text (a ClipboardItem of
// promises, as Safari wants), which the copy of TeXmacs fulfils; when that
// write fails (an older browser), TeXmacs's copy writes as before. Without
// it, the system kept its old clipboard, which the next paste brought back.
//
// Paste: SDL cancels the keys with Ctrl, which cancels the paste event of
// the browser with them, and the canvas is not editable, so that the browser
// has no paste for it anyway. The focus goes to a hidden text area as soon
// as Ctrl or Cmd is down (Safari enables its Paste, the command of Cmd+V,
// only when an editable element had the focus before the V), until it is
// up again. The key of a paste (Ctrl+V, Cmd+V, Shift+Insert) is kept from
// SDL, with its keypress (SDL cancels it, and Safari then cancels the
// paste), and the text area gets the paste event; its contents become those
// of tmClipboard, and SDL gets the key, again:
// TeXmacs pastes as it always does. A browser whose paste event has no
// data (Safari, at times) pastes into the text area, which is read a moment
// later, as editors in the browser do. When the browser has no paste for
// the key (nothing to paste), SDL gets the key all the same.
//
// On a Mac the shortcuts of TeXmacs are those of the Mac, Cmd+... (its look
// and feel follows the platform of the browser, see web-pre.js), and SDL
// leaves the keys with Cmd to the browser as well: the browser does not
// act on those (Cmd+S would save the page), save those it keeps for itself
// (Cmd+W, Cmd+T, Cmd+N, Cmd+Q).
//
// The C++ side is in src/Plugins/Vue/vue_gui.cpp ("Clipboard support").

var tmClipboard = (function () {
  var known = { plain: '', html: '' };
  var pending = null; // the key of a paste, until its paste event
  var sink = null, back = null; // the hidden text area, the focus before it
  var held = false; // Ctrl or Cmd is down, the focus is in the text area
  var trace = typeof location !== 'undefined' &&
              new URLSearchParams (location.search).has ('trace-clipboard');
  function log (s) { if (trace) console.log ('clipboard: ' + s); }

  function editable (t) {
    return t && t.nodeType === 1 && t !== sink &&
           (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test (t.tagName));
  }
  // a dialog of the page is open (frame.js), or text of the page is
  // selected: the keys (Cmd+C...) are the browser's, not TeXmacs's
  function pageOwnsKeys () {
    if (document.getElementById ('tm-about')) return true;
    var sel = window.getSelection && window.getSelection ();
    return !!(sel && !sel.isCollapsed && String (sel) !== '');
  }
  function isCopy (e) {
    var k = (e.key || '').toLowerCase ();
    var c = k === 'c' || k === 'x' || e.code === 'KeyC' || e.code === 'KeyX';
    return c && (e.ctrlKey || e.metaKey) && !e.altKey && !e.shiftKey;
  }
  function isPaste (e) {
    var v = (e.key === 'v' || e.key === 'V' || e.code === 'KeyV');
    return (v && (e.ctrlKey || e.metaKey) && !e.altKey) ||
           (e.key === 'Insert' && e.shiftKey && !e.ctrlKey && !e.metaKey);
  }

  var WAIT = 60; // ms for the paste of a key into the text area
  var DIALOG_OFF = 'tm-copy-dialog-off'; // the copy dialog turned off (localStorage)

  // the write of a copy key, waiting for the text of TeXmacs's copy
  var copying = null;
  var COPY_WAIT = 1000; // ms: no copy by then (nothing selected), no write
  function startCopy () {
    var c = typeof navigator !== 'undefined' && navigator.clipboard;
    if (!c || !c.write || typeof ClipboardItem === 'undefined') return;
    if (copying) copying.fail ();
    // the types are given before TeXmacs copies: the text, and HTML (that
    // of TeXmacs, or the text as HTML when it has none)
    var job = {};
    var text = new Promise (function (ok, ko) { job.ok = ok; job.ko = ko; });
    var html = new Promise (function (ok, ko) { job.okHtml = ok; job.koHtml = ko; });
    job.fail = function () {
      copying = copying === job ? null : copying;
      job.ko (new Error ('nothing copied'));
      job.koHtml (new Error ('nothing copied'));
    };
    job.timer = setTimeout (job.fail, COPY_WAIT);
    try {
      job.written = c.write ([new ClipboardItem ({ 'text/plain': text, 'text/html': html })])
        .then (function () { log ('copy written in the key'); return true; },
               function (e) { log ('copy in the key: ' + e); return false; });
      copying = job;
    }
    catch (e) { log ('copy in the key: ' + e); clearTimeout (job.timer); }
  }

  function release () {
    if (!pending) return;
    var p = pending;
    pending = null;
    clearTimeout (p.timer);
    if (!p.pasted && sink && sink.value)
      known = { plain: sink.value.replace (/\r\n?/g, '\n'), html: '' };
    if (sink) sink.value = '';
    log ('the key to TeXmacs, ' + (p.pasted ? 'with a paste event' : 'the text area: ' +
         JSON.stringify (known.plain.slice (0, 40))));
    if (!held) giveBack ();
    window.dispatchEvent (new KeyboardEvent ('keydown', p.init));
    // the keys released meanwhile (Cmd, V), after the key
    p.ups.forEach (function (u) { window.dispatchEvent (new KeyboardEvent ('keyup', u)); });
  }

  // On a Mac, the key of Shift+Cmd+... is that of the key without the Shift
  // ("=" for Shift+Cmd+=, whose "+" is Cmd++, the zoom): SDL would note it
  // in its keymap, as the key of Shift and that key, and TeXmacs, which
  // asks the keymap, would then get M-S-= instead of M-+. SDL gets no key
  // for those: it then finds it in its keymap, which knows the layout from
  // the keys typed (or it has the US one).
  var mac = typeof navigator !== 'undefined' &&
    /mac|iphone|ipad/i.test ((navigator.userAgentData && navigator.userAgentData.platform) ||
                             navigator.platform || '');
  function unshifted (e) {
    return mac && e.metaKey && e.shiftKey && !e.ctrlKey && !e.altKey &&
           typeof e.key === 'string' && e.key.length === 1;
  }
  // the key again, for SDL, without its key
  function redispatch (e) {
    e.preventDefault ();
    e.stopImmediatePropagation ();
    window.dispatchEvent (new KeyboardEvent (e.type, init (e)));
  }

  function init (e) {
    return { key: unshifted (e) ? 'Unidentified' : e.key,
             code: e.code, location: e.location, repeat: e.repeat,
             ctrlKey: e.ctrlKey, shiftKey: e.shiftKey, altKey: e.altKey,
             metaKey: e.metaKey, bubbles: true, cancelable: true };
  }

  // the focus to the text area, and back
  function borrow () {
    if (!makeSink ()) return;
    if (document.activeElement !== sink) {
      back = document.activeElement;
      sink.value = '';
      sink.focus ({ preventScroll: true });
    }
  }
  function giveBack () {
    if (back && document.activeElement === sink) back.focus ({ preventScroll: true });
    back = null;
  }

  function makeSink () {
    if (sink || !document.body) return sink;
    sink = document.createElement ('textarea');
    sink.setAttribute ('aria-hidden', 'true');
    sink.tabIndex = -1;
    sink.style.cssText = 'position:fixed;left:-1000px;top:0;width:10px;height:10px;opacity:0';
    document.body.appendChild (sink);
    return sink;
  }

  if (typeof window !== 'undefined') {
    // before SDL (its listeners are on window, in the bubbling phase)
    window.addEventListener ('keydown', function (e) {
      if (!e.isTrusted || editable (e.target) || pageOwnsKeys ()) return;
      if (e.key === 'Meta' || e.key === 'Control') {
        held = true;
        borrow ();
        return;
      }
      if (!isPaste (e)) {
        if (isCopy (e)) startCopy ();
        if (unshifted (e)) redispatch (e);
        else if (e.metaKey) e.preventDefault ();
        return;
      }
      e.stopImmediatePropagation ();
      log ('paste key ' + e.key + ', focus in ' + (document.activeElement && document.activeElement.nodeName));
      borrow ();
      if (pending) release ();
      pending = { init: init (e), ups: [], pasted: false,
                  timer: setTimeout (release, WAIT) };
    }, true);
    // Safari has a keypress for Cmd+V: SDL would cancel it, and the paste
    window.addEventListener ('keypress', function (e) {
      if (e.isTrusted && pending && isPaste (e)) e.stopImmediatePropagation ();
    }, true);
    window.addEventListener ('keyup', function (e) {
      if (!e.isTrusted) return;
      if (e.key === 'Meta' || e.key === 'Control') {
        held = false;
        if (!pending) giveBack ();
      }
      if (!pending) {
        if (unshifted (e) && !editable (e.target)) redispatch (e);
        return;
      }
      e.stopImmediatePropagation ();
      pending.ups.push (init (e));
    }, true);
    // Cmd+Tab: its key up goes to another application
    window.addEventListener ('blur', function (e) {
      if (e.target === window) { held = false; if (!pending) giveBack (); }
    });
    document.addEventListener ('paste', function (e) {
      if (editable (e.target)) return;
      var d = e.clipboardData;
      log ('paste event on ' + e.target.nodeName + ', types ' + (d ? Array.from (d.types) : 'none'));
      var plain = d ? d.getData ('text/plain') || '' : '', html = d ? d.getData ('text/html') || '' : '';
      if (!plain && !html) return; // into the text area, read by release
      known = { plain: plain, html: html };
      e.preventDefault ();
      if (pending) pending.pasted = true;
      release ();
    });
  }

  // Edit > Paste from browser (web-paste-dialog in vue_gui.cpp). A menu has
  // no paste event, so the plain Paste of the menus pastes what the page
  // knows (the last copy or paste); this one asks the browser, in a dialog
  // of the page with one button, Paste, which reads navigator.clipboard in
  // its click: the browsers give the clipboard only to the handler of a
  // gesture of the user (Safari then shows its own Paste button, Chrome
  // asks once for the permission), and a menu of TeXmacs runs its command
  // a frame later, outside of it. Enter does the same, and the paste key
  // (or the Paste of a long press, on a touch screen) is a paste event,
  // which needs no permission, in a hidden text area which has the focus.
  // What comes becomes the page's clipboard, and the Scheme command of the
  // format chosen in the dialog pastes it. choices: one format a line, its
  // name and its command separated by a tab (clipboard-paste-browser,
  // selections.scm); chosen: the line selected at first.
  function fromBrowser (choices, chosen) {
    if (typeof tmFrame === 'undefined') return;
    var formats = String (choices).split ('\n').map (function (l) {
      var i = l.indexOf ('\t');
      return i < 0 ? { name: '', cmd: l } : { name: l.slice (0, i), cmd: l.slice (i + 1) };
    }).filter (function (f) { return f.cmd; });
    if (formats.length === 0) return;
    var cmd = formats[Math.max (0, Math.min (chosen | 0, formats.length - 1))].cmd;
    var got = false, before = document.activeElement, enter = null;
    tmFrame.dialog ('Paste from the browser', function (box, close) {
      var key = mac ? '\u2318V' : 'Ctrl+V';
      var c = typeof navigator !== 'undefined' && navigator.clipboard;
      var api = !!(c && (c.read || c.readText));
      function p (cls, t) {
        var e = document.createElement ('p');
        if (cls) e.className = cls;
        e.textContent = t;
        box.appendChild (e);
        return e;
      }
      p (null, api ? 'Paste what was copied in another page or program (or press ' + key + ').'
                   : 'Press ' + key + ' to paste what was copied in another page or program.');
      // the format of the paste, among those of Edit > Paste from
      if (formats.length > 1) {
        var row = document.createElement ('label');
        row.className = 'tm-format';
        row.appendChild (document.createTextNode ('Format: '));
        var pick = document.createElement ('select');
        formats.forEach (function (f) {
          var o = document.createElement ('option');
          o.textContent = f.name;
          o.value = f.cmd;
          if (f.cmd === cmd) o.selected = true;
          pick.appendChild (o);
        });
        // the focus goes back to the text area, for the paste key
        pick.onchange = function () { cmd = pick.value; area.focus ({ preventScroll: true }); };
        row.appendChild (pick);
        box.appendChild (row);
      }
      var note = p ('tm-note', '');
      // the paste key: a paste event in a text area out of sight
      var area = document.createElement ('textarea');
      area.setAttribute ('aria-hidden', 'true');
      area.tabIndex = -1;
      area.style.cssText = 'position:fixed;left:-1000px;top:0;width:10px;height:10px;opacity:0';
      box.appendChild (area);
      function take (plain, html) {
        plain = (plain || '').replace (/\r\n?/g, '\n');
        if (!plain && !html) { note.textContent = 'The clipboard has no text.'; return; }
        known = { plain: plain, html: html || '' };
        log ('from the browser: ' + JSON.stringify (plain.slice (0, 40)));
        got = true;
        close ();
      }
      area.addEventListener ('paste', function (e) {
        var d = e.clipboardData;
        var plain = d ? d.getData ('text/plain') || '' : '', html = d ? d.getData ('text/html') || '' : '';
        if (!plain && !html) { // into the text area (Safari, at times)
          setTimeout (function () { take (area.value, ''); area.value = ''; }, 0);
          return;
        }
        e.preventDefault ();
        take (plain, html);
      });
      // in the handler of the click (or of Enter), not after it
      function read () {
        readClipboard (c).then (function (r) { take (r.plain, r.html); }, function (e) {
          note.textContent = 'The browser did not give the clipboard: press ' + key + '.';
          log ('read: ' + e);
          area.focus ({ preventScroll: true });
        });
      }
      // Enter: on the window before the dialog, which keeps the keys from
      // the page (frame.js)
      enter = function (e) {
        if (e.key === 'Enter' && api && e.type === 'keydown' && e.target !== no &&
            e.target.tagName !== 'SELECT') {
          e.preventDefault ();
          read ();
        }
      };
      window.addEventListener ('keydown', enter, true);
      var bar = document.createElement ('div');
      bar.className = 'tm-buttons';
      var no = document.createElement ('button');
      no.className = 'tm-button';
      no.textContent = 'Cancel';
      no.onclick = function () { close (); };
      bar.appendChild (no);
      if (api) {
        var yes = document.createElement ('button');
        yes.className = 'tm-button tm-default';
        yes.textContent = 'Paste';
        yes.onclick = read;
        bar.appendChild (yes);
      }
      box.appendChild (bar);
      setTimeout (function () { area.focus ({ preventScroll: true }); }, 0);
    }, function () {
      if (enter) window.removeEventListener ('keydown', enter, true);
      if (before && before.focus) before.focus ({ preventScroll: true });
      if (got && typeof _vue_web_scheme !== 'undefined')
        withStackSave (function () { _vue_web_scheme (stringToUTF8OnStack (cmd)); });
    });
  }

  // the text and the HTML of the clipboard, with navigator.clipboard (the
  // HTML when the browser has read (), else the text only)
  function readClipboard (c) {
    if (!c.read || typeof ClipboardItem === 'undefined')
      return c.readText ().then (function (t) { return { plain: t, html: '' }; });
    return c.read ().then (function (items) {
      var r = { plain: '', html: '' }, jobs = [];
      items.forEach (function (it) {
        ['text/plain', 'text/html'].forEach (function (m) {
          if (it.types.indexOf (m) < 0) return;
          jobs.push (it.getType (m).then (function (b) { return b.text (); }).then (function (t) {
            if (m === 'text/html') { if (!r.html) r.html = t; }
            else if (!r.plain) r.plain = t;
          }));
        });
      });
      return Promise.all (jobs).then (function () { return r; });
    });
  }

  return {
    fromBrowser: fromBrowser,
    // what TeXmacs copies: kept here, and given to the system
    write: function (plain, html) {
      known = { plain: plain, html: html };
      if (typeof navigator === 'undefined' || !navigator.clipboard) return;
      // the write started by the key of the copy (Safari), else as below
      var job = copying;
      if (job) {
        copying = null;
        clearTimeout (job.timer);
        job.ok (new Blob ([plain], { type: 'text/plain' }));
        job.okHtml (new Blob ([html || asHtml (plain)], { type: 'text/html' }));
        job.written.then (function (ok) { if (!ok) writeLate (plain, html); });
        return;
      }
      writeLate (plain, html);
    },
    // what TeXmacs pastes: null when there is nothing of that type
    read: function (mime) {
      var s = mime === 'text/html' ? known.html
            : /^text\/plain/.test (mime) ? known.plain : '';
      return s ? s : null;
    }
  };

  // the text of a copy as HTML, for the HTML of the write of a key
  function asHtml (plain) {
    return '<pre>' + plain.replace (/&/g, '&amp;').replace (/</g, '&lt;')
                          .replace (/>/g, '&gt;') + '</pre>';
  }

  // a message in the status bar of TeXmacs
  function message (msg) {
    if (typeof _vue_web_scheme === 'undefined') return;
    var cmd = '(set-message ' + JSON.stringify (msg) + ' "Copy")';
    withStackSave (function () { _vue_web_scheme (stringToUTF8OnStack (cmd)); });
  }

  // the dialog of a copy which did not reach the system, unless the user
  // turned it off (the choice kept by the browser, DIALOG_OFF)
  function dialogOff () {
    try { return localStorage.getItem (DIALOG_OFF) === '1'; } catch (e) { return false; }
  }

  // a copy which did not reach the system (Safari, after a menu: the browser
  // gives its clipboard only to the handler of a key or a click, and the
  // menus of TeXmacs run their commands later): TeXmacs has it, the other
  // programs have not. A dialog of the page offers to give it to them: its
  // button is a click of the user, in whose handler the browser lets the
  // page write, in any format of Copy to (with its HTML). Without it, the
  // status bar says how to copy for them.
  function notReached (plain, html, e) {
    console.warn ('TeXmacs: cannot copy to the clipboard: ' + e);
    if (typeof tmFrame !== 'undefined' && !dialogOff ()) { toSystem (plain, html); return; }
    if (typeof _vue_web_scheme === 'undefined') return;
    var key = mac ? 'Cmd+C' : 'Ctrl+C';
    message ('Copied in TeXmacs only: ' + key + ' copies for other programs too');
  }

  // the text of a copy for the other programs, in a dialog of the page
  function toSystem (plain, html) {
    var c = navigator.clipboard;
    var enter = null, before = document.activeElement;
    tmFrame.dialog ('Copy for other programs', function (box, close) {
      function p (cls, t) {
        var e = document.createElement ('p');
        if (cls) e.className = cls;
        e.textContent = t;
        box.appendChild (e);
        return e;
      }
      p (null, 'TeXmacs has the copy; this browser gives its clipboard to other programs ' +
               'only from a key (' + (mac ? '\u2318C' : 'Ctrl+C') + ') or a button, not from a menu.');
      var view = document.createElement ('pre');
      view.style.cssText = 'max-height:12em;overflow:auto;white-space:pre-wrap;word-break:break-word;' +
                           'margin:8px 0;padding:6px 8px;border:1px solid #ccc;border-radius:4px;' +
                           'background:#fafafa;font:12px Menlo,Monaco,monospace';
      var shown = plain.length > 4000 ? plain.slice (0, 4000) + '\n...' : plain;
      view.textContent = shown;
      box.appendChild (view);
      var note = p ('tm-note', html ? 'With its HTML, for the programs which take it.' : '');
      var row = document.createElement ('label');
      row.style.cssText = 'display:flex;align-items:center;gap:6px;margin-top:10px;font-size:13px';
      var off = document.createElement ('input');
      off.type = 'checkbox';
      off.style.cssText = 'width:auto;flex:none;margin:0'; // not the text inputs of the dialogs
      row.appendChild (off);
      row.appendChild (document.createTextNode ('Do not show this again (copy in TeXmacs only)'));
      box.appendChild (row);
      function remember () {
        if (!off.checked) return;
        try { localStorage.setItem (DIALOG_OFF, '1'); } catch (e) {}
      }
      // in the handler of the click (or of Enter), not after it
      function copy () {
        var job;
        try {
          if (html && c.write && typeof ClipboardItem !== 'undefined')
            job = c.write ([new ClipboardItem ({
              'text/plain': new Blob ([plain], { type: 'text/plain' }),
              'text/html': new Blob ([html], { type: 'text/html' }) })]);
          else job = c.writeText (plain);
        } catch (e) { job = Promise.reject (e); }
        job.then (function () {
          remember ();
          close ();
          message ('Copied for other programs too');
        }, function (e) {
          note.textContent = 'The browser did not take it: ' + (e && e.message ? e.message : e);
        });
      }
      enter = function (e) {
        if (e.key === 'Enter' && e.type === 'keydown' && e.target !== no && e.target !== off) {
          e.preventDefault ();
          copy ();
        }
      };
      window.addEventListener ('keydown', enter, true);
      var bar = document.createElement ('div');
      bar.className = 'tm-buttons';
      var no = document.createElement ('button');
      no.className = 'tm-button';
      no.textContent = 'Not now';
      no.onclick = function () { remember (); close (); };
      var yes = document.createElement ('button');
      yes.className = 'tm-button tm-default';
      yes.textContent = 'Copy';
      yes.onclick = copy;
      bar.appendChild (no);
      bar.appendChild (yes);
      box.appendChild (bar);
      setTimeout (function () { yes.focus ({ preventScroll: true }); }, 0);
    }, function () {
      if (enter) window.removeEventListener ('keydown', enter, true);
      if (before && before.focus) before.focus ({ preventScroll: true });
    });
  }

  // the write of a copy after the key or the menu (Firefox, Chrome accept
  // it), its HTML too when it has one
  function writeLate (plain, html) {
    var done = function () {};
    var failed = function (e) { notReached (plain, html, e); };
    try {
      if (html && navigator.clipboard.write && typeof ClipboardItem !== 'undefined')
        navigator.clipboard.write ([new ClipboardItem ({
          'text/plain': new Blob ([plain], { type: 'text/plain' }),
          'text/html': new Blob ([html], { type: 'text/html' }) })])
          .then (done, function () { navigator.clipboard.writeText (plain).then (done, failed); });
      else navigator.clipboard.writeText (plain).then (done, failed);
    } catch (e) { failed (e); }
  }
})();

// Control and a click on a Mac (the right click of a trackpad or of a mouse
// with one button): the browser sends a mousedown of the right button (and
// a contextmenu) but no pointerdown, then a pointerup of the left button.
// SDL, which listens to the pointer events, sees a release and no press,
// and TeXmacs no click at all. The press is given to SDL as a pointerdown
// of the right button, and the release as a pointerup of the right button.
(function () {
  if (typeof window === 'undefined') return;
  var pressed = false;   // a pointerdown came for this press
  var ours = false;      // the press was ours: its release is too
  function pointer (type, e, buttons) {
    return new PointerEvent (type, {
      bubbles: true, cancelable: true, composed: true,
      clientX: e.clientX, clientY: e.clientY, screenX: e.screenX, screenY: e.screenY,
      button: 2, buttons: buttons, pointerId: 1, pointerType: 'mouse', isPrimary: true,
      ctrlKey: e.ctrlKey, shiftKey: e.shiftKey, altKey: e.altKey, metaKey: e.metaKey });
  }
  window.addEventListener ('pointerdown', function (e) {
    if (e.isTrusted) pressed = true;
  }, true);
  window.addEventListener ('mousedown', function (e) {
    if (e.button === 2 && e.ctrlKey && !pressed && e.target && e.target.tagName === 'CANVAS') {
      ours = true;
      e.target.dispatchEvent (pointer ('pointerdown', e, 2));
    }
    pressed = false;
  }, true);
  window.addEventListener ('pointerup', function (e) {
    if (ours && e.isTrusted) {
      ours = false;
      e.stopImmediatePropagation ();
      e.preventDefault ();
      (e.target || window).dispatchEvent (pointer ('pointerup', e, 0));
    }
  }, true);
})();
