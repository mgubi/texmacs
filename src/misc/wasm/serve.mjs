// Serve the browser build: node misc/wasm/serve.mjs [dir] [port] [KB/s]
// (default build-wasm/out/web on port 8080, at full speed), then open
// http://localhost:<port>/texmacs.html
//
// A file with a brotli copy (<file>.br, written by the build) is sent
// compressed to the browsers which accept it; a range request (a file of
// TeXmacs needed before its package, see packages.js) gets its bytes as
// they are.
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

// rate: bytes per second sent (0: as fast as possible), to see the page
// load as over a slow network
function send (file, res, opts, rate) {
  const stream = fs.createReadStream (file, opts);
  if (!rate) { stream.pipe (res); return; }
  let start = Date.now (), sent = 0;
  stream.on ('data', chunk => {
    sent += chunk.length;
    const wait = start + 1000 * sent / rate - Date.now ();
    res.write (chunk);
    if (wait > 0) { stream.pause (); setTimeout (() => stream.resume (), wait); }
  });
  stream.on ('end', () => res.end ());
}

export function serve (dir, port, host = '127.0.0.1', onServed = null, rate = 0) {
  const types = { '.html': 'text/html', '.js': 'text/javascript', '.json': 'application/json',
                  '.wasm': 'application/wasm', '.data': 'application/octet-stream',
                  '.pack': 'application/octet-stream' };
  const server = http.createServer ((req, res) => {
    let p = decodeURIComponent (req.url.split ('?')[0]);
    if (p === '/') p = '/texmacs.html';
    const file = path.join (dir, p);
    if (!file.startsWith (dir) || !fs.existsSync (file)) { res.writeHead (404); res.end (); return; }
    const type = types[path.extname (file)] || 'application/octet-stream';
    const size = fs.statSync (file).size;
    const range = /^bytes=(\d+)-(\d+)$/.exec (req.headers.range || '');
    if (range) {
      const start = Number (range[1]), end = Math.min (Number (range[2]), size - 1);
      res.writeHead (206, { 'Content-Type': type, 'Content-Length': end - start + 1,
                            'Content-Range': `bytes ${start}-${end}/${size}`,
                            'Accept-Ranges': 'bytes' });
      send (file, res, { start, end }, rate);
      if (onServed) onServed (p, end - start + 1, 'range');
      return;
    }
    // an unchanged file is not sent again (the program, above all)
    const mtime = fs.statSync (file).mtime;
    const since = Date.parse (req.headers['if-modified-since'] || '');
    if (!isNaN (since) && Math.floor (mtime.getTime () / 1000) <= Math.floor (since / 1000)) {
      res.writeHead (304, { 'Last-Modified': mtime.toUTCString () });
      res.end ();
      if (onServed) onServed (p, 0, 'unchanged');
      return;
    }
    const br = (req.headers['accept-encoding'] || '').includes ('br') && fs.existsSync (file + '.br');
    const src = br ? file + '.br' : file;
    res.writeHead (200, { 'Content-Type': type, 'Content-Length': fs.statSync (src).size,
                          'Accept-Ranges': 'bytes', 'Vary': 'Accept-Encoding',
                          'Last-Modified': mtime.toUTCString (), 'Cache-Control': 'no-cache',
                          ...(br ? { 'Content-Encoding': 'br' } : {}) });
    send (src, res, {}, rate);
    if (onServed) onServed (p, fs.statSync (src).size, br ? 'br' : 'identity');
  });
  return new Promise (ok => server.listen (port, host, () => ok (server)));
}

if (process.argv[1] && process.argv[1].endsWith ('serve.mjs')) {
  const dir = path.resolve (process.argv[2] || 'build-wasm/out/web');
  const port = Number (process.argv[3] || 8080);
  const rate = 1000 * Number (process.argv[4] || 0); // [KB/s]
  await serve (dir, port, '127.0.0.1', null, rate);
  console.log (`TeXmacs: http://localhost:${port}/texmacs.html`);
}
