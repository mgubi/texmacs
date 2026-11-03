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
//
// The links which TeXmacs leaves to the system (load-external in
// tm-files.scm: a web page, a mail address, a PDF or a picture) come here
// as well, through (web-open-external target file? name): a page opens in a
// tab, a mail address in the mail program, and a file of the page in the
// viewer of the browser when it has one (PDF, pictures, text), else it is
// downloaded.

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

  // text: what the notice says; name: that of the download (none for a
  // page of the web)
  function showNotice (url, name, text0) {
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
    text.textContent = text0 || 'The PDF of ' + name + ' is ready.';
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
    if (name) button ('Download', function () { download (url, name); });
    button ('Open', function () { window.open (url, '_blank'); }).autofocus = true;
    notice.appendChild (text);
    notice.appendChild (buttons);
    document.body.appendChild (notice);
  }

  // url in a tab, or the notice when the browser refuses it
  function openTab (url, name, text) {
    var tab = null;
    try { tab = window.open (url, '_blank'); } catch (e) {}
    if (tab) {
      closeNotice ();
      if (!/^blob:/.test (url)) try { tab.opener = null; } catch (e) {}
    }
    else showNotice (url, name, text);
  }

  var viewable = { pdf: 'application/pdf', png: 'image/png', jpg: 'image/jpeg',
                   jpeg: 'image/jpeg', gif: 'image/gif', webp: 'image/webp',
                   svg: 'image/svg+xml', bmp: 'image/bmp', txt: 'text/plain',
                   html: 'text/html', htm: 'text/html' };

  return {
    // a link that TeXmacs leaves to the system (see above): a page of the
    // web or a mail address (file false), or a file of the page, name being
    // the name of its download
    external: function (target, file, name) {
      if (!file) {
        if (/^mailto:/i.test (target)) {
          var a = document.createElement ('a');
          a.href = target;
          document.body.appendChild (a);
          a.click ();
          a.remove ();
        }
        else openTab (target, null, 'Open ' + target + ' in a new tab?');
        return;
      }
      var bytes;
      try { bytes = FS.readFile (target); }
      catch (e) { console.error ('TeXmacs: no file at ' + target); return; }
      if (!name) name = target.slice (target.lastIndexOf ('/') + 1);
      var ext = name.slice (name.lastIndexOf ('.') + 1).toLowerCase ();
      var type = viewable[ext];
      var url = URL.createObjectURL (new Blob ([bytes], { type: type || 'application/octet-stream' }));
      urls.push (url);
      while (urls.length > 4) URL.revokeObjectURL (urls.shift ());
      if (type) openTab (url, name, name + ' is ready.');
      else download (url, name);
    },
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
