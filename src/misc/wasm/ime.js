// Input methods in the page (a --pre-js of the browser build; the C++ side
// is vue_web_compose in src/Plugins/Vue/vue_gui.cpp).
//
// SDL has no input method on the web: its text comes from the keypress of
// each key, one character, and a key which composes (a dead key, as ^ or ¨
// of a Swiss or a French keyboard; the accents of a Mac, held or typed with
// Option; the input methods of Chinese, Japanese, Korean...) has no
// keypress: the browser composes in an editable element only, and the
// canvas is not one.
//
// So the keys go to a hidden text area, which has the focus whenever the
// canvas would have it. Its composition is the pre-edit of TeXmacs while it
// lasts (compositionupdate: vue_web_compose (text, 0), shown in the
// document as the input methods of the Qt port are), and its text when it
// ends (compositionend: vue_web_compose (text, 1)). The keys of a
// composition are kept from SDL, which would type them a second time. The
// other keys go to SDL as before (they reach the window, where SDL listens):
// it prevents their keypress, so that nothing is typed into the text area;
// a text which comes into it all the same, outside of a composition (the
// accent chosen in the popup of a held key, a virtual keyboard), is typed
// into TeXmacs and taken out of it.
//
// The text area is put at the cursor of TeXmacs (tmIme.caret, from
// update_text_input_area in src/Plugins/Vue/vue_widget.cpp), where the
// system shows the window of the candidates of an input method, or the
// accents of a held key; where the user last clicked until it is known. Its
// text does not wrap, so that its own caret stays there while it composes.
//
// ?trace-ime in the address logs what happens.

var tmIme = (function () {
  if (typeof window === 'undefined' || typeof document === 'undefined') return null;
  var area = null;
  var composing = false;
  var lastX = 60, lastY = 60, lastH = 16; // in the canvas, CSS pixels
  var trace = typeof location !== 'undefined' &&
              new URLSearchParams (location.search).has ('trace-ime');
  function log (s) { if (trace) console.log ('ime: ' + s); }

  function make () {
    if (area || !document.body) return area;
    area = document.createElement ('textarea');
    area.setAttribute ('data-tm-input', 'true');      // not a field of the page
    area.setAttribute ('aria-hidden', 'true');
    area.setAttribute ('autocomplete', 'off');
    area.setAttribute ('autocorrect', 'off');
    area.setAttribute ('autocapitalize', 'off');
    area.spellcheck = false;
    area.tabIndex = -1;
    // out of sight but in the page (an element which is not shown has no
    // composition); 16px, so that a phone does not zoom on it
    area.style.cssText = 'position:fixed;width:2px;padding:0;border:0;' +
      'margin:0;opacity:0;resize:none;overflow:hidden;white-space:pre;' +
      'font-size:16px;line-height:1;pointer-events:none;z-index:-1';
    document.body.appendChild (area);
    place ();
    area.addEventListener ('compositionstart', function () {
      composing = true;
      place ();
      log ('composition starts');
    });
    area.addEventListener ('compositionupdate', function (e) {
      log ('composition: ' + JSON.stringify (e.data));
      compose (e.data || '', false);
    });
    area.addEventListener ('compositionend', function (e) {
      composing = false;
      log ('composed: ' + JSON.stringify (e.data));
      compose (e.data || '', true);
      area.value = '';
    });
    area.addEventListener ('input', function (e) {
      if (composing || e.isComposing || !area.value) return;
      // a text which came without a composition nor a keypress for SDL
      log ('text: ' + JSON.stringify (area.value));
      compose (area.value, true);
      area.value = '';
    });
    return area;
  }

  function compose (text, commit) {
    if (typeof _vue_web_compose === 'undefined') return;
    withStackSave (function () {
      _vue_web_compose (stringToUTF8OnStack (text), commit ? 1 : 0);
    });
  }

  function place () {
    if (!area) return;
    var c = document.getElementById ('canvas');
    var r = c ? c.getBoundingClientRect () : { left: 0, top: 0 };
    area.style.left = (r.left + lastX) + 'px';
    area.style.top = (r.top + lastY) + 'px';
    area.style.height = lastH + 'px';
  }

  // the cursor of TeXmacs (points of the window, which are the CSS pixels
  // of the canvas): x, the top y and the height h of a box around it
  var known = false;
  function caret (x, y, h) {
    known = true;
    lastX = x; lastY = y; lastH = h > 0 ? h : 16;
    place ();
    log ('caret at ' + x + ', ' + y);
  }

  // a dialog of the page has its own fields
  function dialogOpen () { return !!document.getElementById ('tm-about'); }

  function focus () {
    if (!make () || dialogOpen ()) return;
    if (document.activeElement !== area) area.focus ({ preventScroll: true });
  }

  // the keys of a composition, kept from SDL (not from the browser, which
  // composes with them)
  function ofComposition (e) {
    return composing || e.isComposing || e.key === 'Dead' || e.key === 'Process' ||
           e.keyCode === 229;
  }
  function keep (e) {
    if (!e.isTrusted || !area || e.target !== area) return;
    if (ofComposition (e)) {
      e.stopImmediatePropagation ();
      log (e.type + ' ' + e.key + ' kept from SDL');
    }
  }
  window.addEventListener ('keydown', keep, true);
  window.addEventListener ('keypress', keep, true);
  window.addEventListener ('keyup', keep, true);

  // the focus: to the text area when the canvas would have it, and at a
  // click on the canvas (after SDL has it)
  document.addEventListener ('focusin', function (e) {
    if (e.target && e.target.id === 'canvas') setTimeout (focus, 0);
  });
  window.addEventListener ('pointerdown', function (e) {
    if (!e.target || e.target.id !== 'canvas') return;
    if (!known) {
      var r = e.target.getBoundingClientRect ();
      lastX = e.clientX - r.left; lastY = e.clientY - r.top;
      place ();
    }
    setTimeout (focus, 0);
  }, true);
  window.addEventListener ('resize', place);
  if (document.readyState === 'loading')
    document.addEventListener ('DOMContentLoaded', function () { setTimeout (focus, 0); });
  else setTimeout (focus, 0);

  return {
    focus: focus,
    caret: caret,
    composing: function () { return composing; }
  };
})();
