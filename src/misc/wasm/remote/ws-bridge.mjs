// A bridge for the page of a browser to a TeXmacs server which does not
// serve WebSocket clients (the servers before this branch, such as
// cloud.texmacs.org): it listens for WebSocket connections on this machine
// and passes what they say to the server over TLS, and back.
//
//   node misc/wasm/remote/ws-bridge.mjs [host [port [local port]]]
//   (defaults: cloud.texmacs.org 6561 6563)
//
// then, in the page, Remote > Login with the server "localhost" and the
// local port. The page may open ws://localhost even when it is served
// over https. The bridge sees what passes in clear (the page leaves the
// encryption to the WebSocket, and the bridge does the TLS with the
// server): run it yourself, on the machine of the browser, and it listens
// on 127.0.0.1 only. The certificate of the server is checked.
import http from 'node:http';
import tls from 'node:tls';
import crypto from 'node:crypto';

const host = process.argv[2] || 'cloud.texmacs.org';
const port = Number (process.argv[3] || 6561);
const local = Number (process.argv[4] || 6563);

function frame (data) { // a binary frame, not masked (server to client)
  const n = data.length;
  const head = n < 126 ? Buffer.from ([0x82, n])
             : n < 65536 ? Buffer.from ([0x82, 126, n >> 8, n & 255])
             : Buffer.concat ([Buffer.from ([0x82, 127]), (() => { const b = Buffer.alloc (8); b.writeBigUInt64BE (BigInt (n)); return b; }) ()]);
  return Buffer.concat ([head, data]);
}

const server = http.createServer ((req, res) => { res.writeHead (426); res.end ('WebSocket only\n'); });
server.on ('upgrade', (req, sock) => {
  const key = req.headers['sec-websocket-key'];
  if (!key) { sock.destroy (); return; }
  const accept = crypto.createHash ('sha1').update (key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11').digest ('base64');
  const proto = (req.headers['sec-websocket-protocol'] || '').split (',').map (s => s.trim ()).includes ('binary')
    ? 'Sec-WebSocket-Protocol: binary\r\n' : '';
  sock.write ('HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n' +
              'Sec-WebSocket-Accept: ' + accept + '\r\n' + proto + '\r\n');
  const id = sock.remotePort;
  console.log (`[${id}] a page connected, opening ${host}:${port}`);
  let pending = [], up = null, buf = Buffer.alloc (0), closed = false;
  const close = why => {
    if (closed) return; closed = true;
    console.log (`[${id}] closed (${why})`);
    try { sock.write (Buffer.from ([0x88, 0])); sock.end (); } catch (e) {}
    try { if (up) up.destroy (); } catch (e) {}
  };
  up = tls.connect ({ host, port, servername: host }, () => {
    console.log (`[${id}] connected to the server (${up.getProtocol ()})`);
    for (const p of pending) up.write (p);
    pending = null;
  });
  up.on ('data', d => { if (!closed) sock.write (frame (d)); });
  up.on ('error', e => close ('server: ' + (e.code || e.message)));
  up.on ('close', () => close ('the server closed'));
  sock.on ('error', () => close ('page error'));
  sock.on ('close', () => close ('the page closed'));
  sock.on ('data', d => {
    buf = Buffer.concat ([buf, d]);
    for (;;) {
      if (buf.length < 2) return;
      const op = buf[0] & 15, masked = (buf[1] & 128) !== 0;
      let n = buf[1] & 127, p = 2;
      if (n === 126) { if (buf.length < 4) return; n = buf.readUInt16BE (2); p = 4; }
      else if (n === 127) { if (buf.length < 10) return; n = Number (buf.readBigUInt64BE (2)); p = 10; }
      if (buf.length < p + (masked ? 4 : 0) + n) return;
      let payload = buf.subarray (p + (masked ? 4 : 0), p + (masked ? 4 : 0) + n);
      if (masked) { const m = buf.subarray (p, p + 4); payload = Buffer.from (payload.map ((b, i) => b ^ m[i & 3])); }
      buf = buf.subarray (p + (masked ? 4 : 0) + n);
      if (op === 8) { close ('the page said goodbye'); return; }
      if (op === 9) { sock.write (Buffer.concat ([Buffer.from ([0x8a, payload.length]), payload])); continue; }
      if (op === 0 || op === 1 || op === 2) { if (pending) pending.push (payload); else up.write (payload); }
    }
  });
});
server.listen (local, '127.0.0.1', () =>
  console.log (`bridge: ws://localhost:${local}/ -> ${host}:${port} (TLS). In the page: server "localhost", port ${local}.`));
