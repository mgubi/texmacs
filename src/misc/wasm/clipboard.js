// The clipboard of the system in the page (a --pre-js of the browser build).
//
// TeXmacs reads the clipboard when it pastes, synchronously; the browser
// gives its contents only to a paste event, or asynchronously and with the
// consent of the user (navigator.clipboard.read). So the page keeps what it
// knows of the clipboard (tmClipboard.read): what TeXmacs copied last, or
// what the last paste event brought.
//
// Copy: TeXmacs writes its text (and its HTML, when it has one) with
// navigator.clipboard, which the browser allows just after a key or a click
// of the user, which is when TeXmacs copies.
//
// Paste: SDL cancels the keys with Ctrl, which cancels the paste event of
// the browser with them, and the canvas is not editable, so that Firefox has
// no paste event for it anyway. The key of a paste (Ctrl+V, Cmd+V,
// Shift+Insert) is kept from SDL, and the focus goes for a moment to a
// hidden text area, which gets the paste event; its contents become those
// of tmClipboard, the focus comes back, and SDL gets the key, again:
// TeXmacs pastes as it always does. When the browser has no paste event for
// the key (nothing to paste), SDL gets the key anyway.
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

  function editable (t) {
    return t && t.nodeType === 1 &&
           (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test (t.tagName));
  }
  function isPaste (e) {
    var v = (e.key === 'v' || e.key === 'V' || e.code === 'KeyV');
    return (v && (e.ctrlKey || e.metaKey) && !e.altKey) ||
           (e.key === 'Insert' && e.shiftKey && !e.ctrlKey && !e.metaKey);
  }

  function release () {
    if (!pending) return;
    var p = pending;
    pending = null;
    if (back && document.activeElement === sink) back.focus ({ preventScroll: true });
    back = null;
    window.dispatchEvent (new KeyboardEvent ('keydown', p));
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
      if (!e.isTrusted || editable (e.target)) return;
      if (!isPaste (e)) {
        if (e.metaKey) e.preventDefault ();
        return;
      }
      e.stopImmediatePropagation ();
      if (makeSink ()) {
        back = document.activeElement;
        sink.value = '';
        sink.focus ({ preventScroll: true });
      }
      pending = { key: e.key, code: e.code, location: e.location, repeat: e.repeat,
                  ctrlKey: e.ctrlKey, shiftKey: e.shiftKey, altKey: e.altKey,
                  metaKey: e.metaKey, bubbles: true, cancelable: true };
      setTimeout (release, 0);
    }, true);
    document.addEventListener ('paste', function (e) {
      if (editable (e.target) && e.target !== sink) return;
      var d = e.clipboardData;
      if (d) known = { plain: d.getData ('text/plain') || '', html: d.getData ('text/html') || '' };
      e.preventDefault ();
      release ();
    });
  }

  return {
    // what TeXmacs copies: kept here, and given to the system
    write: function (plain, html) {
      known = { plain: plain, html: html };
      if (typeof navigator === 'undefined' || !navigator.clipboard) return;
      var done = function () {};
      var failed = function (e) { console.warn ('TeXmacs: cannot copy to the clipboard: ' + e); };
      try {
        if (html && navigator.clipboard.write && typeof ClipboardItem !== 'undefined')
          navigator.clipboard.write ([new ClipboardItem ({
            'text/plain': new Blob ([plain], { type: 'text/plain' }),
            'text/html': new Blob ([html], { type: 'text/html' }) })])
            .then (done, function () { navigator.clipboard.writeText (plain).then (done, failed); });
        else navigator.clipboard.writeText (plain).then (done, failed);
      } catch (e) { failed (e); }
    },
    // what TeXmacs pastes: null when there is nothing of that type
    read: function (mime) {
      var s = mime === 'text/html' ? known.html
            : /^text\/plain/.test (mime) ? known.plain : '';
      return s ? s : null;
    }
  };
})();
