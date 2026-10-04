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
// The text area is put where the user last clicked, near the cursor of
// TeXmacs, for the window of the candidates of an input method.
//
// ?trace-ime in the address logs what happens.

var tmIme = (function () {
  if (typeof window === 'undefined' || typeof document === 'undefined') return null;
  var area = null;
  var composing = false;
  var lastX = 60, lastY = 60;
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
    area.style.cssText = 'position:fixed;width:2px;height:1.2em;padding:0;border:0;' +
      'margin:0;opacity:0;resize:none;overflow:hidden;font-size:16px;' +
      'pointer-events:none;z-index:-1;left:' + lastX + 'px;top:' + lastY + 'px';
    document.body.appendChild (area);
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
    area.style.left = lastX + 'px';
    area.style.top = lastY + 'px';
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
    lastX = e.clientX; lastY = e.clientY;
    setTimeout (focus, 0);
  }, true);
  if (document.readyState === 'loading')
    document.addEventListener ('DOMContentLoaded', function () { setTimeout (focus, 0); });
  else setTimeout (focus, 0);

  return {
    focus: focus,
    composing: function () { return composing; }
  };
})();
