// Printing in the page (a --pre-js of the browser build).
//
// Print and Preview in TeXmacs write the PDF of the document (the MuPDF
// writer) into the file system of the page and call (web-open-pdf path
// name) (src/Plugins/Vue/vue_gui.cpp, TeXmacs/progs/texmacs/texmacs/
// tm-print.scm), which comes here: the PDF opens in a tab of its own, in
// the viewer of the browser, from which it is printed.
//
// A browser opens a tab only just after a click or a key of the user; the
// PDF of a long document can take longer than that, and the tab is then
// refused: a notice in the page offers to open it (a click of its own) or
// to download it.

var tmPrint = (function () {
  var urls = [], notice = null;

  var style = `
    #tm-print { position:fixed; left:50%; top:40%; transform:translate(-50%,-50%);
      width:320px; max-width:calc(100% - 32px);
      box-sizing:border-box; padding:12px 14px; background:#f6f6f6; border:1px solid #999;
      border-radius:6px; box-shadow:0 6px 24px rgba(0,0,0,.3); z-index:35;
      font:13px -apple-system,"Fira Sans",Helvetica,sans-serif; color:#222 }
    #tm-print .tm-text { margin-bottom:10px }
    #tm-print .tm-buttons { display:flex; gap:8px; justify-content:flex-end }
    #tm-print button { font:inherit; padding:3px 10px }
  `;

  function closeNotice () {
    if (notice) { notice.remove (); notice = null; }
  }

  function download (url, name) {
    var a = document.createElement ('a');
    a.href = url;
    a.download = name;
    document.body.appendChild (a);
    a.click ();
    a.remove ();
  }

  function showNotice (url, name) {
    closeNotice ();
    if (!document.getElementById ('tm-print-style')) {
      var st = document.createElement ('style');
      st.id = 'tm-print-style';
      st.textContent = style;
      document.head.appendChild (st);
    }
    notice = document.createElement ('div');
    notice.id = 'tm-print';
    var text = document.createElement ('div');
    text.className = 'tm-text';
    text.textContent = 'The PDF of ' + name + ' is ready.';
    var buttons = document.createElement ('div');
    buttons.className = 'tm-buttons';
    function button (label, f) {
      var b = document.createElement ('button');
      b.textContent = label;
      b.onclick = function () { f (); closeNotice (); };
      buttons.appendChild (b);
      return b;
    }
    button ('Close', function () {});
    button ('Download', function () { download (url, name); });
    button ('Open', function () { window.open (url, '_blank'); }).autofocus = true;
    notice.appendChild (text);
    notice.appendChild (buttons);
    document.body.appendChild (notice);
  }

  return {
    // the PDF at path (in the file system of the page), name for its download
    open: function (path, name) {
      var bytes;
      try { bytes = FS.readFile (path); }
      catch (e) { console.error ('TeXmacs: no PDF at ' + path); return; }
      if (!name) name = 'document.pdf';
      var url = URL.createObjectURL (new Blob ([bytes], { type: 'application/pdf' }));
      // the tabs load their PDF from the URL: a few are kept
      urls.push (url);
      while (urls.length > 4) URL.revokeObjectURL (urls.shift ());
      var tab = null;
      try { tab = window.open (url, '_blank'); } catch (e) {}
      if (tab) closeNotice ();
      else showNotice (url, name);
    }
  };
})();
