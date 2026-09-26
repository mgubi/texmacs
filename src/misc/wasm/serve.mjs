// Serve the browser build: node misc/wasm/serve.mjs [dir] [port]
// (default build-wasm/out/web on port 8080), then open
// http://localhost:<port>/texmacs.html
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
const dir = path.resolve (process.argv[2] || 'build-wasm/out/web');
const port = Number (process.argv[3] || 8080);
const types = { '.html': 'text/html', '.js': 'text/javascript',
                '.wasm': 'application/wasm', '.data': 'application/octet-stream' };
http.createServer ((req, res) => {
  let p = decodeURIComponent (req.url.split ('?')[0]);
  if (p === '/') p = '/texmacs.html';
  const file = path.join (dir, p);
  if (!file.startsWith (dir)) { res.writeHead (403); res.end (); return; }
  fs.readFile (file, (err, data) => {
    if (err) { res.writeHead (404); res.end (); return; }
    res.writeHead (200, { 'Content-Type': types[path.extname (file)] || 'application/octet-stream' });
    res.end (data);
  });
}).listen (port, '127.0.0.1', () =>
  console.log (`TeXmacs: http://localhost:${port}/texmacs.html`));
