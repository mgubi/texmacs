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
  function isPaste (e) {
    var v = (e.key === 'v' || e.key === 'V' || e.code === 'KeyV');
    return (v && (e.ctrlKey || e.metaKey) && !e.altKey) ||
           (e.key === 'Insert' && e.shiftKey && !e.ctrlKey && !e.metaKey);
  }

  var WAIT = 60; // ms for the paste of a key into the text area

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
