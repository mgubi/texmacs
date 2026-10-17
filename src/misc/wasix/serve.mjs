// Serve the page of the WASIX build: node misc/wasix/serve.mjs [dir] [port]
// (default build-wasix/out/web on port 8094), then open http://localhost:<port>/
//
// The Wasmer SDK runs WASIX in Web Workers which share the memory of the
// program (SharedArrayBuffer): the page must be cross-origin isolated, hence
// the headers COOP and COEP on every answer.

import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

const TYPES = { '.html': 'text/html', '.js': 'text/javascript', '.mjs': 'text/javascript',
                '.json': 'application/json', '.wasm': 'application/wasm',
                '.bin': 'application/octet-stream', '.tm': 'text/plain',
                '.pdf': 'application/pdf', '.png': 'image/png', '.svg': 'image/svg+xml' };

export function serve (dir, port, host = '127.0.0.1') {
  const server = http.createServer ((req, res) => {
    let p = decodeURIComponent (req.url.split ('?')[0]);
    if (p.endsWith ('/')) p += 'index.html';
    const file = path.join (dir, p);
    const headers = { 'Cross-Origin-Opener-Policy': 'same-origin',
                      'Cross-Origin-Embedder-Policy': 'require-corp',
                      'Cross-Origin-Resource-Policy': 'same-origin',
                      'Cache-Control': 'no-cache' };
    if (!file.startsWith (dir) || !fs.existsSync (file) || !fs.statSync (file).isFile ()) {
      res.writeHead (404, headers); res.end (); return;
    }
    res.writeHead (200, { ...headers,
                          'Content-Type': TYPES[path.extname (file)] || 'application/octet-stream',
                          'Content-Length': fs.statSync (file).size });
    fs.createReadStream (file).pipe (res);
  });
  return new Promise (ok => server.listen (port, host, () => ok (server)));
}

if (process.argv[1] && process.argv[1].endsWith ('serve.mjs')) {
  const dir = path.resolve (process.argv[2] || 'build-wasix/out/web');
  const port = Number (process.argv[3] || 8094);
  await serve (dir, port);
  console.log (`TeXmacs (WASIX): http://localhost:${port}/`);
}
