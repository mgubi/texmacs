// The files of the page (a --pre-js of the browser build).
//
// The home directory of TeXmacs in the page, /home/web, is kept in the
// IndexedDB of the browser (see web-pre.js). This script manages it:
//
// - a panel (tmFiles.browse) to see it and to bring files in and out:
//   upload files, whole folders (a project with its images) or a zip, make
//   folders, rename, delete, download a file, or a folder as a zip;
// - in the same panel, the files of TeXmacs (/texmacs: its styles,
//   packages, Scheme code, documentation), which are not changed: they are
//   opened, downloaded, or copied into the TeXmacs folder of the user
//   ("customize": ~/.TeXmacs at the same place), where TeXmacs looks first;
// - the same panel as the Open and Save dialogs of TeXmacs (tmFiles.open,
//   tmFiles.save, called from vue_gui.cpp);
// - the files and folders dropped on the page: copied into ~/Documents,
//   keeping their structure; the documents are opened, the images dropped
//   on a document inserted where they fall, a folder shown in the panel;
// - a document given in the address of the page (texmacs.html?open=<url>),
//   opened once TeXmacs runs: the page as a viewer of documents on the web.
//
// Everything that opens a chooser of the system is a control the user
// clicks: the browsers open one only for a click being handled.

var tmFiles = (function () {
  var HOME = '/home/web';
  var DOCS = HOME + '/Documents';
  // the files of TeXmacs (its styles, packages, Scheme code...): shown, not
  // changed; "customize" copies one into the TeXmacs folder of the user,
  // where TeXmacs looks first
  var SYS = '/texmacs', USER_TM = HOME + '/.TeXmacs';
  function inSys (p) { return p === SYS || p.indexOf (SYS + '/') === 0; }
  var DOC_SUFFIXES = ['tm', 'tmml', 'ts', 'tex', 'html', 'htm', 'md', 'bib', 'scm', 'txt'];
  var IMAGE_SUFFIXES = ['png', 'jpg', 'jpeg', 'gif', 'svg', 'pdf', 'eps', 'ps', 'tif', 'tiff', 'bmp'];

  /****************************************************************************
  * The file system
  ****************************************************************************/

  function suffix (name) {
    var i = name.lastIndexOf ('.');
    return i < 0 ? '' : name.slice (i + 1).toLowerCase ();
  }
  function isDocument (name) { return DOC_SUFFIXES.indexOf (suffix (name)) >= 0; }
  function isImage (name) { return IMAGE_SUFFIXES.indexOf (suffix (name)) >= 0; }
  function join (dir, name) { return dir === '/' ? '/' + name : dir + '/' + name; }
  function parent (p) { var i = p.lastIndexOf ('/'); return i <= 0 ? '/' : p.slice (0, i); }
  function base (p) { return p.slice (p.lastIndexOf ('/') + 1); }
  function safe (n) {
    return n.split ('/').join ('_').split (String.fromCharCode (92)).join ('_');
  }
  function exists (p) { try { FS.stat (p); return true; } catch (e) { return false; } }
  function isDir (p) { try { return FS.isDir (FS.stat (p).mode); } catch (e) { return false; } }
  function mkdirs (p) { try { FS.mkdirTree (p); } catch (e) {} }
  function write (p, bytes) { mkdirs (parent (p)); FS.writeFile (p, bytes); }
  function list (dir) {
    var names = [];
    try { names = FS.readdir (dir); } catch (e) {}
    return names.filter (function (n) { return n !== '.' && n !== '..'; })
      .map (function (n) {
        var p = join (dir, n), st = FS.stat (p);
        return { name: n, path: p, dir: FS.isDir (st.mode), size: st.size, mtime: st.mtime };
      })
      .sort (function (a, b) {
        return a.dir !== b.dir ? (a.dir ? -1 : 1) : a.name.localeCompare (b.name);
      });
  }
  function remove (p) {
    if (isDir (p)) {
      list (p).forEach (function (e) { remove (e.path); });
      FS.rmdir (p);
    }
    else FS.unlink (p);
  }
  // the files under a folder: [{ rel, path }]
  function walk (dir, rel, acc) {
    list (dir).forEach (function (e) {
      var r = rel ? rel + '/' + e.name : e.name;
      if (e.dir) walk (e.path, r, acc); else acc.push ({ rel: r, path: e.path });
    });
    return acc;
  }
  // a name which is not taken in dir: "name", "name 2", ...
  function fresh (dir, name) {
    if (!exists (join (dir, name))) return name;
    var i = name.lastIndexOf ('.'), stem = i > 0 ? name.slice (0, i) : name;
    var ext = i > 0 ? name.slice (i) : '';
    for (var k = 2; ; k++) {
      var n = stem + ' ' + k + ext;
      if (!exists (join (dir, n))) return n;
    }
  }
  function save () { if (typeof tmSaveHome === 'function') tmSaveHome (); }

  /****************************************************************************
  * Zip files (the compression streams of the browser, no library)
  ****************************************************************************/

  var crcTable = null;
  function crc32 (bytes) {
    if (!crcTable) {
      crcTable = new Uint32Array (256);
      for (var n = 0; n < 256; n++) {
        var c = n;
        for (var k = 0; k < 8; k++) c = (c & 1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1);
        crcTable[n] = c >>> 0;
      }
    }
    var crc = 0xFFFFFFFF;
    for (var i = 0; i < bytes.length; i++) crc = crcTable[(crc ^ bytes[i]) & 0xFF] ^ (crc >>> 8);
    return (crc ^ 0xFFFFFFFF) >>> 0;
  }
  async function transform (bytes, stream) {
    var out = new Response (new Blob ([bytes]).stream ().pipeThrough (stream));
    return new Uint8Array (await out.arrayBuffer ());
  }
  // files: [{ rel, bytes }] -> the bytes of a zip file
  async function zip (files) {
    var canDeflate = typeof CompressionStream !== 'undefined';
    var enc = new TextEncoder (), parts = [], central = [], offset = 0;
    for (var i = 0; i < files.length; i++) {
      var name = enc.encode (files[i].rel), data = files[i].bytes;
      var crc = crc32 (data), method = 0, comp = data;
      if (canDeflate && data.length > 64) {
        var d = await transform (data, new CompressionStream ('deflate-raw'));
        if (d.length < data.length) { comp = d; method = 8; }
      }
      var h = new DataView (new ArrayBuffer (30));
      h.setUint32 (0, 0x04034b50, true); h.setUint16 (4, 20, true);
      h.setUint16 (6, 0x0800, true); h.setUint16 (8, method, true);
      h.setUint32 (14, crc, true); h.setUint32 (18, comp.length, true);
      h.setUint32 (22, data.length, true); h.setUint16 (26, name.length, true);
      parts.push (new Uint8Array (h.buffer), name, comp);
      var c = new DataView (new ArrayBuffer (46));
      c.setUint32 (0, 0x02014b50, true); c.setUint16 (4, 20, true); c.setUint16 (6, 20, true);
      c.setUint16 (8, 0x0800, true); c.setUint16 (10, method, true);
      c.setUint32 (16, crc, true); c.setUint32 (20, comp.length, true);
      c.setUint32 (24, data.length, true); c.setUint16 (28, name.length, true);
      c.setUint32 (42, offset, true);
      central.push (new Uint8Array (c.buffer), name);
      offset += 30 + name.length + comp.length;
    }
    var size = central.reduce (function (s, a) { return s + a.length; }, 0);
    var e = new DataView (new ArrayBuffer (22));
    e.setUint32 (0, 0x06054b50, true); e.setUint16 (8, files.length, true);
    e.setUint16 (10, files.length, true); e.setUint32 (12, size, true);
    e.setUint32 (16, offset, true);
    return new Blob (parts.concat (central, [new Uint8Array (e.buffer)]));
  }
  // the bytes of a zip file -> [{ rel, bytes }] (stored and deflated entries)
  async function unzip (bytes) {
    var v = new DataView (bytes.buffer, bytes.byteOffset, bytes.byteLength);
    var end = -1;
    for (var i = bytes.length - 22; i >= Math.max (0, bytes.length - 65557); i--)
      if (v.getUint32 (i, true) === 0x06054b50) { end = i; break; }
    if (end < 0) throw new Error ('not a zip file');
    var n = v.getUint16 (end + 10, true), p = v.getUint32 (end + 16, true);
    var dec = new TextDecoder (), out = [];
    for (var k = 0; k < n; k++) {
      var method = v.getUint16 (p + 10, true), csize = v.getUint32 (p + 20, true);
      var nlen = v.getUint16 (p + 28, true), xlen = v.getUint16 (p + 30, true);
      var clen = v.getUint16 (p + 32, true), local = v.getUint32 (p + 42, true);
      var name = dec.decode (bytes.subarray (p + 46, p + 46 + nlen));
      p += 46 + nlen + xlen + clen;
      if (name.endsWith ('/') || name.startsWith ('__MACOSX/')) continue;
      var start = local + 30 + v.getUint16 (local + 26, true) + v.getUint16 (local + 28, true);
      var data = bytes.subarray (start, start + csize);
      if (method === 8) data = await transform (data, new DecompressionStream ('deflate-raw'));
      else if (method !== 0) continue; // another compression: skipped
      out.push ({ rel: name, bytes: new Uint8Array (data) });
    }
    return out;
  }

  /****************************************************************************
  * Bringing files in and out
  ****************************************************************************/

  function download (name, blob) {
    var a = document.createElement ('a');
    a.href = URL.createObjectURL (blob);
    a.download = name;
    document.body.appendChild (a);
    a.click ();
    a.remove ();
    setTimeout (function () { URL.revokeObjectURL (a.href); }, 10000);
  }
  function downloadPath (p) {
    if (!isDir (p)) { download (base (p), new Blob ([FS.readFile (p)])); return; }
    var files = walk (p, base (p), []).map (function (f) {
      return { rel: f.rel, bytes: FS.readFile (f.path) };
    });
    zip (files).then (function (blob) { download (base (p) + '.zip', blob); });
  }
  // File objects with their relative paths -> written under dir
  async function importFiles (dir, items) {
    var written = [];
    for (var i = 0; i < items.length; i++) {
      var rel = items[i].rel.split ('/').map (safe).join ('/');
      var bytes = new Uint8Array (await items[i].file.arrayBuffer ());
      if (suffix (rel) === 'zip' && items[i].unpack) {
        // a zip of one folder gives that folder; the files of a zip
        // without one go into a folder named after it
        var entries = await unzip (bytes);
        var tops = {};
        entries.forEach (function (e) { tops[e.rel.split ('/')[0]] = e.rel.indexOf ('/') >= 0; });
        var names = Object.keys (tops);
        var one = names.length === 1 && tops[names[0]];
        var into = one ? '' : rel.slice (0, rel.length - 4) + '/';
        entries.forEach (function (e) { write (join (dir, into + e.rel), e.bytes); });
        written.push (join (dir, one ? names[0] : into.slice (0, -1)));
        continue;
      }
      var p = join (dir, rel);
      write (p, bytes);
      written.push (p);
    }
    save ();
    return written;
  }
  // the entries of a drop, folders included -> [{ rel, file }]
  async function dropped (dt) {
    var out = [], entries = [];
    if (dt.items)
      for (var i = 0; i < dt.items.length; i++) {
        var en = dt.items[i].webkitGetAsEntry ? dt.items[i].webkitGetAsEntry () : null;
        if (en) entries.push (en);
      }
    if (entries.length === 0) { // no entries (a synthetic drop): the plain files
      for (var j = 0; j < dt.files.length; j++)
        out.push ({ rel: dt.files[j].name, file: dt.files[j] });
      return out;
    }
    async function visit (en, rel) {
      if (en.isFile) {
        var f = await new Promise (function (ok, ko) { en.file (ok, ko); });
        out.push ({ rel: rel + en.name, file: f });
      }
      else if (en.isDirectory) {
        var reader = en.createReader (), batch;
        do {
          batch = await new Promise (function (ok, ko) { reader.readEntries (ok, ko); });
          for (var k = 0; k < batch.length; k++) await visit (batch[k], rel + en.name + '/');
        } while (batch.length > 0);
      }
    }
    for (var e = 0; e < entries.length; e++) await visit (entries[e], '');
    return out;
  }

  function toast (text) {
    var t = document.createElement ('div');
    t.textContent = text;
    t.style.cssText = 'position:fixed;left:50%;bottom:48px;transform:translateX(-50%);' +
      'background:#333;color:#fff;padding:8px 14px;border-radius:5px;z-index:20;' +
      'font:13px -apple-system,Helvetica,sans-serif;opacity:.92';
    document.body.appendChild (t);
    setTimeout (function () { t.remove (); }, 3500);
  }

  // ask TeXmacs to open a document (see vue_web_open_document)
  function openDocument (p) {
    withStackSave (function () { _vue_web_open_document (stringToUTF8OnStack (p)); });
  }
  // images dropped at (x, y) of the canvas: inserted by the editor
  function dropImages (x, y, paths) {
    withStackSave (function () {
      _vue_web_drop_files (x, y, stringToUTF8OnStack (paths.join ('\n')));
    });
  }

  /****************************************************************************
  * The panel
  ****************************************************************************/

  var current = null; // the open panel: { close () }

  function el (tag, css, text) {
    var e = document.createElement (tag);
    if (css) e.style.cssText = css;
    if (text !== undefined) e.textContent = text;
    return e;
  }
  // the buttons of the panel, those which open a chooser too: one style
  var BUTTON = 'display:inline-block;margin-right:6px;padding:2px 8px;border:1px solid #999;' +
               'border-radius:4px;background:#fff;cursor:pointer;font:inherit;color:inherit';
  function button (text, onclick) {
    var b = el ('button', BUTTON, text);
    b.onclick = onclick;
    return b;
  }
  // a button which opens a chooser: a label around a hidden input
  function chooser (id, text, attrs, onfiles) {
    var l = el ('label', BUTTON, text);
    var i = el ('input', 'display:none');
    i.type = 'file';
    i.id = id;
    for (var k in attrs) i.setAttribute (k, attrs[k]);
    i.onchange = function () { onfiles (Array.prototype.slice.call (i.files)); i.value = ''; };
    l.appendChild (i);
    return l;
  }

  // mode: 'browse', 'open' or 'save'; done (path or null) for open and save
  function panel (mode, opts, done) {
    if (current) current.close (null);
    var dir = opts.dir && isDir (opts.dir) ? opts.dir : DOCS;
    mkdirs (DOCS);
    var selected = null;
    var root = el ('div', 'position:fixed;inset:0;background:rgba(0,0,0,.25);z-index:10;' +
                          'font:13px -apple-system,Helvetica,sans-serif;color:#222');
    var box = el ('div', 'position:absolute;left:50%;top:50%;transform:translate(-50%,-50%);' +
                         'width:min(680px,94vw);height:min(520px,90vh);background:#f4f4f4;' +
                         'border:1px solid #888;border-radius:6px;box-shadow:0 6px 24px rgba(0,0,0,.3);' +
                         'display:flex;flex-direction:column');
    box.id = 'tm-files';
    root.appendChild (box);
    var head = el ('div', 'padding:10px 14px;font-weight:bold;border-bottom:1px solid #ccc',
                   mode === 'open' ? 'Open a file' : mode === 'save' ? 'Save as' : 'Files in this browser');
    var places = el ('div', 'padding:8px 14px 0;display:flex;gap:14px');
    // the files are the user's, on this computer: nothing goes to a server
    var stored = el ('span', 'margin-left:auto;color:#777;font-size:90%',
                     'Stored in this browser only; nothing is sent anywhere.');
    var crumbs = el ('div', 'padding:6px 14px;color:#444');
    var tools = el ('div', 'padding:4px 14px 8px');
    var listing = el ('div', 'flex:1;overflow:auto;background:#fff;margin:0 14px;border:1px solid #ccc');
    var foot = el ('div', 'padding:10px 14px;display:flex;gap:8px;align-items:center');
    box.appendChild (head); box.appendChild (places); box.appendChild (crumbs);
    box.appendChild (tools);
    box.appendChild (listing); box.appendChild (foot);

    var nameInput = null;
    if (mode === 'save') {
      nameInput = el ('input', 'flex:1');
      nameInput.id = 'tm-files-name';
      nameInput.value = opts.name || 'untitled.tm';
      foot.appendChild (el ('span', '', 'Name:'));
      foot.appendChild (nameInput);
    }
    var hint = null;
    if (mode !== 'save') {
      hint = el ('span', 'flex:1;color:#666');
      foot.appendChild (hint);
    }

    function close (result) {
      if (!current) return;
      current = null;
      root.remove ();
      document.removeEventListener ('keydown', onkey, true);
      if (done) done (result);
    }
    function onkey (e) {
      if (e.key === 'Escape') { e.stopPropagation (); e.preventDefault (); close (null); }
      else if (e.key === 'Enter' && mode !== 'browse') { e.stopPropagation (); e.preventDefault (); accept (); }
      else e.stopPropagation (); // the keys typed in the panel are not for TeXmacs
    }
    function accept () {
      if (mode === 'open') {
        if (selected && !isDir (selected)) close (selected);
      }
      else if (mode === 'save') {
        if (inSys (dir)) {
          window.alert ('The files of TeXmacs cannot be changed: save in one of your folders.');
          return;
        }
        var n = safe (nameInput.value.trim ());
        if (!n) return;
        close (join (dir, n));
      }
    }
    if (mode === 'browse') foot.appendChild (button ('Close', function () { close (null); }));
    else {
      foot.appendChild (button ('Cancel', function () { close (null); }));
      var ok = button (mode === 'open' ? 'Open' : 'Save', accept);
      ok.id = 'tm-files-ok';
      foot.appendChild (ok);
    }

    // a file or folder of TeXmacs into the TeXmacs folder of the user, at the
    // same place (styles/x.ts: .TeXmacs/styles/x.ts), where TeXmacs looks
    // before its own files
    function customize (e) {
      var rel = e.path.slice (SYS.length + 1), dest = join (USER_TM, rel);
      if (exists (dest) &&
          !window.confirm ('.TeXmacs/' + rel + ' exists already: replace it by the one of TeXmacs?'))
        return;
      if (exists (dest)) remove (dest);
      if (e.dir) walk (e.path, '', []).forEach (function (f) {
        write (join (dest, f.rel), FS.readFile (f.path));
      });
      else write (dest, FS.readFile (e.path));
      save ();
      toast ('Copied to .TeXmacs/' + rel + ': TeXmacs uses your copy, which you can edit ' +
             '(a style or a package in the next document, Scheme code at the next start)');
    }

    function render () {
      var sys = inSys (dir), top = sys ? SYS : HOME;
      places.textContent = '';
      [['Your files', DOCS, false], ['Files of TeXmacs', SYS, true]].forEach (function (pl) {
        var a = el ('a', 'cursor:pointer;color:#036' + (pl[2] === sys ? ';font-weight:bold' : ''), pl[0]);
        a.onclick = function () { dir = pl[1]; selected = null; mkdirs (DOCS); render (); };
        places.appendChild (a);
      });
      places.appendChild (stored);
      tools.style.display = sys ? 'none' : '';
      if (hint) hint.textContent = sys
        ? 'The files of TeXmacs cannot be changed here: "customize" copies one into your ' +
          'TeXmacs folder (.TeXmacs), where you can edit it.'
        : 'Drop files or folders here to add them to this folder.';
      crumbs.textContent = '';
      var parts = dir.slice (top.length).split ('/').filter (Boolean), acc = top;
      var home = el ('a', 'cursor:pointer;color:#036', sys ? 'TeXmacs' : 'Home');
      home.onclick = function () { dir = top; render (); };
      crumbs.appendChild (home);
      parts.forEach (function (p) {
        acc = acc + '/' + p;
        var target = acc;
        crumbs.appendChild (document.createTextNode (' / '));
        var a = el ('a', 'cursor:pointer;color:#036', p);
        a.onclick = function () { dir = target; render (); };
        crumbs.appendChild (a);
      });
      listing.textContent = '';
      var entries = list (dir);
      if (entries.length === 0)
        listing.appendChild (el ('div', 'padding:14px;color:#888', 'This folder is empty.'));
      entries.forEach (function (e) {
        var row = el ('div', 'display:flex;align-items:center;padding:4px 8px;border-bottom:1px solid #eee;' +
                             'cursor:default' + (selected === e.path ? ';background:#cde' : ''));
        row.dataset.name = e.name;
        var name = el ('span', 'flex:1', (e.dir ? '📁 ' : '📄 ') + e.name);
        row.appendChild (name);
        if (!e.dir) row.appendChild (el ('span', 'color:#888;margin-right:10px',
          e.size < 1024 ? e.size + ' B' : Math.round (e.size / 1024) + ' KB'));
        function act (text, f, tip) {
          // a small button: a light rounded border around the link
          var a = el ('a', 'cursor:pointer;color:#036;margin-left:6px;padding:1px 7px;' +
                           'border:1px solid #c8d0da;border-radius:10px;background:#fff;' +
                           'font-size:92%;white-space:nowrap', text);
          if (tip) a.title = tip;
          a.onclick = function (ev) { ev.stopPropagation (); f (); };
          row.appendChild (a);
        }
        if (mode === 'browse' && !e.dir) act ('open', function () { close (null); openDocument (e.path); });
        act ('save copy', function () { downloadPath (e.path); },
             e.dir ? 'Save a copy on your computer, as a zip' : 'Save a copy on your computer');
        if (sys) {
          act ('customize', function () { customize (e); });
          listing.appendChild (row);
          row.onclick = function () {
            if (e.dir) { dir = e.path; selected = null; render (); return; }
            selected = e.path; render ();
          };
          row.ondblclick = function () {
            if (e.dir) return;
            if (mode === 'open') close (e.path);
            else if (mode === 'browse') { close (null); openDocument (e.path); }
          };
          return;
        }
        act ('rename', function () {
          var n = window.prompt ('Rename', e.name);
          if (!n || n === e.name) return;
          n = safe (n);
          if (exists (join (dir, n))) { window.alert (n + ' exists already'); return; }
          FS.rename (e.path, join (dir, n)); save (); render ();
        });
        act ('delete', function () {
          if (!window.confirm ('Delete ' + e.name + (e.dir ? ' and everything in it' : '') + '?')) return;
          remove (e.path); save (); render ();
        });
        row.onclick = function () {
          if (e.dir) { dir = e.path; selected = null; render (); return; }
          selected = e.path;
          if (mode === 'save') nameInput.value = e.name;
          render ();
        };
        row.ondblclick = function () {
          if (e.dir) return;
          if (mode === 'open') close (e.path);
          else if (mode === 'save') accept ();
          else { close (null); openDocument (e.path); }
        };
        listing.appendChild (row);
      });
    }

    tools.appendChild (chooser ('tm-files-upload', 'Add files…', { multiple: '' }, function (fs) {
      importFiles (dir, fs.map (function (f) { return { rel: f.name, file: f }; }))
        .then (render);
    }));
    tools.appendChild (chooser ('tm-files-folder', 'Add a folder…', { webkitdirectory: '', multiple: '' }, function (fs) {
      importFiles (dir, fs.map (function (f) {
        return { rel: f.webkitRelativePath || f.name, file: f };
      })).then (render);
    }));
    tools.appendChild (chooser ('tm-files-zip', 'Add a zip…', { accept: '.zip' }, function (fs) {
      importFiles (dir, fs.map (function (f) { return { rel: f.name, file: f, unpack: true }; }))
        .then (render).catch (function (e) { window.alert ('Cannot read the zip: ' + e.message); });
    }));
    tools.appendChild (button ('New folder', function () {
      var n = window.prompt ('New folder', fresh (dir, 'folder'));
      if (!n) return;
      mkdirs (join (dir, safe (n))); save (); render ();
    }));


    root.addEventListener ('dragover', function (e) { e.preventDefault (); e.stopPropagation (); });
    root.addEventListener ('drop', function (e) {
      e.preventDefault (); e.stopPropagation ();
      if (inSys (dir)) { toast ('The files of TeXmacs cannot be changed: drop in one of your folders'); return; }
      dropped (e.dataTransfer).then (function (items) {
        return importFiles (dir, items);
      }).then (render);
    });
    root.addEventListener ('mousedown', function (e) { if (e.target === root) close (null); });
    document.addEventListener ('keydown', onkey, true);
    document.body.appendChild (root);
    current = { close: close };
    render ();
    if (nameInput) { nameInput.focus (); nameInput.select (); }
  }

  /****************************************************************************
  * Drops on the page
  ****************************************************************************/

  function installDrop () {
    // ahead of SDL (which keeps the files of a drop in /tmp, and no folder):
    // the drops of files are handled here, those of texts left to SDL
    function hasFiles (e) {
      return e.dataTransfer && Array.prototype.indexOf.call (e.dataTransfer.types, 'Files') >= 0;
    }
    window.addEventListener ('dragover', function (e) {
      if (!hasFiles (e)) return;
      e.preventDefault ();
      e.dataTransfer.dropEffect = 'copy';
    }, true);
    window.addEventListener ('drop', function (e) {
      if (!hasFiles (e) || current) return;
      e.preventDefault ();
      e.stopImmediatePropagation ();
      var canvas = document.getElementById ('canvas');
      var r = canvas.getBoundingClientRect ();
      var x = e.clientX - r.left, y = e.clientY - r.top;
      dropped (e.dataTransfer).then (async function (items) {
        mkdirs (DOCS);
        // the items at the top: the files and the folders dropped
        var tops = {};
        items.forEach (function (it) { tops[it.rel.split ('/')[0]] = true; });
        var names = Object.keys (tops);
        // a name which is taken gets another one, rather than overwriting
        var renamed = {};
        names.forEach (function (n) { renamed[n] = fresh (DOCS, safe (n)); });
        items.forEach (function (it) {
          var parts = it.rel.split ('/');
          parts[0] = renamed[parts[0]];
          it.rel = parts.join ('/');
        });
        await importFiles (DOCS, items);
        var docs = [], images = [], folders = [];
        names.forEach (function (n) {
          var p = join (DOCS, renamed[n]);
          if (isDir (p)) folders.push (p);
          else if (isImage (p)) images.push (p);
          else if (isDocument (p)) docs.push (p);
        });
        docs.forEach (openDocument);
        if (images.length > 0 && docs.length === 0) dropImages (x, y, images);
        folders.forEach (function (f) {
          var mains = list (f).filter (function (e) { return !e.dir && suffix (e.name) === 'tm'; });
          if (mains.length === 1) openDocument (mains[0].path);
        });
        if (folders.length > 0) panel ('browse', { dir: folders[0] }, null);
        toast ('Copied to Documents: ' + names.map (function (n) { return renamed[n]; }).join (', '));
      }).catch (function (err) { console.error ('TeXmacs: the drop failed', err); });
    }, true);
  }
  if (typeof window !== 'undefined') installDrop ();

  /****************************************************************************
  * A document from the web: texmacs.html?open=<url>
  ****************************************************************************/

  // The document is fetched while TeXmacs loads, and opened once it runs.
  // The url is relative to the page, or absolute: another site must allow
  // the page to read it (CORS: Access-Control-Allow-Origin). It is kept in
  // /tmp, not in the home directory: a document viewed is not a document
  // of the user (Save as puts it among them). What it refers to (images,
  // included files) is not fetched with it.
  // the promise: the document has been given to TeXmacs, or could not be
  function openFromWeb (src) {
    var u;
    try { u = new URL (src, location.href); }
    catch (e) {
      return new Promise (function (ok) {
        tmProgress.running (function () { toast ('Not an address: ' + src); ok (); });
      });
    }
    var name = safe (decodeURIComponent (base (u.pathname))) || 'document.tm';
    if (!isDocument (name)) name += '.tm';
    var fetched = fetch (u.href).then (function (r) {
      if (!r.ok) throw new Error (r.status + ' ' + r.statusText);
      return r.arrayBuffer ();
    });
    fetched.catch (function () {}); // handled once TeXmacs runs, below
    return new Promise (function (ok) {
      tmProgress.running (function () {
        fetched.then (function (buf) {
          var p = '/tmp/web/' + name;
          write (p, new Uint8Array (buf));
          openDocument (p);
        }).catch (function (err) {
          console.error ('TeXmacs: cannot open ' + u.href, err);
          toast ('Cannot open ' + u.href + ': ' + (err.message || err) +
                 (u.origin !== location.origin ? ' (does the site allow it? CORS)' : ''));
        }).then (ok);
      });
    });
  }

  // ?x=<command>: Scheme commands, as TeXmacs -x <command>, run once
  // TeXmacs runs, after the document of ?open (as -x after the files of the
  // command line), in their order. A link is anyone's, and a command could
  // change or delete the files kept in the browser: the page asks first,
  // showing them.
  function runCommands (cmds) {
    tmFrame.ask ('Run Scheme commands?',
                 'The address of this page asks TeXmacs to run ' +
                 (cmds.length === 1 ? 'this Scheme command' : 'these Scheme commands') +
                 '. Run only what you trust: a command can change or delete your files ' +
                 'kept in this browser.', cmds, 'Run')
      .then (function (yes) {
        if (!yes) return;
        cmds.forEach (function (c) {
          withStackSave (function () { _vue_web_scheme (stringToUTF8OnStack (c)); });
        });
      });
  }

  if (typeof location !== 'undefined') {
    var address = new URLSearchParams (location.search);
    var opened = address.get ('open') ? openFromWeb (address.get ('open')) : Promise.resolve ();
    var cmds = address.getAll ('x').filter (function (c) { return c.trim () !== ''; });
    if (cmds.length > 0)
      tmProgress.running (function () { opened.then (function () { runCommands (cmds); }); });
  }

  return {
    browse: function (dir) { panel ('browse', { dir: dir }, null); },
    open: function (accept, done) { panel ('open', { accept: accept }, done); },
    save: function (name, done) { panel ('save', { name: name }, done); },
    zip: zip, unzip: unzip
  };
})();
