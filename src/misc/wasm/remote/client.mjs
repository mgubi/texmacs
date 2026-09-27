// a WebSocket client of the TeXmacs server: node client.mjs [url]
const url = process.argv[2] || 'ws://127.0.0.1:6561/';
const ws = new WebSocket (url, ['binary']);
ws.binaryType = 'arraybuffer';
const packet = s => { const b = Buffer.from (s, 'utf8'); return Buffer.concat ([Buffer.from (b.length + '\n'), b]); };
let buf = '';
ws.onopen = () => { console.log ('open, protocol', JSON.stringify (ws.protocol)); ws.send (packet ('(0 (remote-login "admin" "secret123"))')); };
ws.onmessage = e => {
  buf += Buffer.from (e.data).toString ();
  console.log ('got', JSON.stringify (buf));
  if (/ready|invalid|not/.test (buf)) { ws.close (); setTimeout (() => process.exit (0), 100); }
};
ws.onerror = e => { console.log ('error', e.message || e.type); process.exit (1); };
ws.onclose = e => console.log ('closed', e.code);
setTimeout (() => { console.log ('timeout'); process.exit (2); }, 10000);
