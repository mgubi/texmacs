// The wallet of TeXmacs in the browser (a --pre-js of the browser build; the
// Scheme side is TeXmacs/progs/security/wallet/web-wallet.scm, which keeps
// the table of the wallet while it is on).
//
// On the desktop the wallet is a table encrypted by GnuPG, which a page
// cannot run. Here the cryptography of the browser (WebCrypto) does it:
//
//   a data key (AES-GCM, 256 bits, random) encrypts the table;
//   the data key is kept wrapped (encrypted) by a key derived from the
//   passphrase (PBKDF2, SHA-256, 600000 rounds), and maybe by a key derived
//   from a passkey (the PRF extension of WebAuthn: the authenticator, Touch
//   ID or a security key, gives a secret which only it can give again).
//
// The file of the wallet (in the home directory, kept in the IndexedDB of
// the page with the other files of the user) holds only what is encrypted,
// the salts and the id of the passkey: the passphrase, the secret of the
// passkey and the data key are never written. While the wallet is on, the
// data key is held here (to encrypt the table again when it changes) and
// the table by Scheme; turning it off forgets both.
//
// The work of WebCrypto and WebAuthn is asynchronous: each call gets the
// number of a request of Scheme, and answers with
// (web-wallet-answer number ok? "text") through TeXmacs.later.

var tmWallet = (function () {
  if (typeof window === 'undefined') return null;
  var ROUNDS = 600000;
  var path = null;          // the file of the wallet
  var dataKey = null;       // CryptoKey while the wallet is on
  var dataRaw = null;       // its bytes (to wrap it for a new passkey)
  var enc = new TextEncoder (), dec = new TextDecoder ();

  function b64 (bytes) {
    var s = '', a = new Uint8Array (bytes);
    for (var i = 0; i < a.length; i++) s += String.fromCharCode (a[i]);
    return btoa (s);
  }
  function unb64 (s) {
    var t = atob (s), a = new Uint8Array (t.length);
    for (var i = 0; i < t.length; i++) a[i] = t.charCodeAt (i);
    return a;
  }
  function random (n) { return crypto.getRandomValues (new Uint8Array (n)); }
  function schemeString (s) {
    return '"' + String (s).replace (/\\/g, '\\\\').replace (/"/g, '\\"') + '"';
  }
  function answer (id, ok, text) {
    TeXmacs.later ('(web-wallet-answer ' + id + ' ' + (ok ? '#t' : '#f') + ' ' +
                   schemeString (text || '') + ')');
  }
  function fail (id) {
    return function (e) {
      var m = e && e.name === 'OperationError' ? 'wrong passphrase' :
              e && e.name === 'NotAllowedError' ? 'cancelled' :
              (e && e.message) || String (e);
      answer (id, false, m);
    };
  }

  function read () {
    try { return JSON.parse (FS.readFile (path, { encoding: 'utf8' })); }
    catch (e) { return null; }
  }
  function write (w) {
    var dir = path.slice (0, path.lastIndexOf ('/'));
    try { FS.mkdirTree (dir); } catch (e) {}
    FS.writeFile (path, JSON.stringify (w, null, 1));
    if (typeof tmSaveHome === 'function') tmSaveHome ();
  }

  async function aesKey (raw, usages) {
    return crypto.subtle.importKey ('raw', raw, 'AES-GCM', false, usages);
  }
  async function encrypt (key, bytes) {
    var iv = random (12);
    var ct = await crypto.subtle.encrypt ({ name: 'AES-GCM', iv: iv }, key, bytes);
    return { iv: b64 (iv), ct: b64 (ct) };
  }
  async function decrypt (key, box) {
    return new Uint8Array (await crypto.subtle.decrypt (
      { name: 'AES-GCM', iv: unb64 (box.iv) }, key, unb64 (box.ct)));
  }
  // the key which wraps the data key, from a passphrase
  async function passphraseKey (passphrase, salt, rounds) {
    var base = await crypto.subtle.importKey ('raw', enc.encode (passphrase), 'PBKDF2', false, ['deriveKey']);
    return crypto.subtle.deriveKey (
      { name: 'PBKDF2', salt: salt, iterations: rounds, hash: 'SHA-256' },
      base, { name: 'AES-GCM', length: 256 }, false, ['encrypt', 'decrypt']);
  }
  // ... from the secret of a passkey
  async function secretKey (secret, salt) {
    var base = await crypto.subtle.importKey ('raw', secret, 'HKDF', false, ['deriveKey']);
    return crypto.subtle.deriveKey (
      { name: 'HKDF', hash: 'SHA-256', salt: salt, info: enc.encode ('TeXmacs wallet') },
      base, { name: 'AES-GCM', length: 256 }, false, ['encrypt', 'decrypt']);
  }
  async function open (raw) {
    dataRaw = raw;
    dataKey = await aesKey (raw, ['encrypt', 'decrypt']);
  }
  async function wrapForPassphrase (passphrase) {
    var salt = random (16);
    var k = await passphraseKey (passphrase, salt, ROUNDS);
    return { salt: b64 (salt), rounds: ROUNDS, key: await encrypt (k, dataRaw) };
  }

  // the secret of a passkey (WebAuthn PRF), asking the authenticator
  async function passkeySecret (credId, prfSalt) {
    var cred = await navigator.credentials.get ({ publicKey: {
      challenge: random (32),
      rpId: location.hostname,
      allowCredentials: [{ type: 'public-key', id: unb64 (credId) }],
      userVerification: 'required',
      extensions: { prf: { eval: { first: unb64 (prfSalt) } } }
    } });
    var r = cred.getClientExtensionResults ();
    if (!r.prf || !r.prf.results || !r.prf.results.first)
      throw new Error ('this passkey cannot give a secret (no PRF) in this browser');
    return new Uint8Array (r.prf.results.first);
  }

  var api = {
    // the file of the wallet, given by Scheme (it knows the home directory)
    setPath: function (p) { path = p; return ''; },
    // what the wallet is: "none", or "passphrase", or "passphrase passkey"
    status: function () {
      var w = read ();
      if (!w) return 'none';
      return 'passphrase' + (w.passkey ? ' passkey' : '');
    },
    supported: function () {
      return !!(window.crypto && crypto.subtle && window.isSecureContext);
    },
    passkeySupported: function () {
      return !!(window.PublicKeyCredential && navigator.credentials && window.isSecureContext);
    },
    on: function () { return !!dataKey; },

    // a new wallet with an empty table (text: the table, as Scheme writes it)
    create: function (id, passphrase, table) {
      (async function () {
        await open (random (32));
        var w = { version: 1, passphrase: await wrapForPassphrase (passphrase),
                  table: await encrypt (dataKey, enc.encode (table)) };
        write (w);
        answer (id, true, table);
      }) ().catch (fail (id));
    },
    // the table, with the passphrase
    unlock: function (id, passphrase) {
      (async function () {
        var w = read ();
        if (!w) throw new Error ('no wallet');
        var k = await passphraseKey (passphrase, unb64 (w.passphrase.salt), w.passphrase.rounds);
        await open (await decrypt (k, w.passphrase.key));
        answer (id, true, dec.decode (await decrypt (dataKey, w.table)));
      }) ().catch (function (e) { dataKey = dataRaw = null; fail (id) (e); });
    },
    // the table, with the passkey
    unlockPasskey: function (id) {
      (async function () {
        var w = read ();
        if (!w || !w.passkey) throw new Error ('no passkey for the wallet');
        var secret = await passkeySecret (w.passkey.id, w.passkey.prfSalt);
        var k = await secretKey (secret, unb64 (w.passkey.salt));
        await open (await decrypt (k, w.passkey.key));
        answer (id, true, dec.decode (await decrypt (dataKey, w.table)));
      }) ().catch (function (e) { dataKey = dataRaw = null; fail (id) (e); });
    },
    // the table changed (the wallet is on)
    save: function (table) {
      if (!dataKey) return 'off';
      var key = dataKey;
      (async function () {
        var w = read ();
        if (!w || key !== dataKey) return;
        w.table = await encrypt (key, enc.encode (table));
        write (w);
      }) ().catch (function (e) { console.error ('TeXmacs: cannot save the wallet', e); });
      return '';
    },
    lock: function () {
      if (dataRaw) dataRaw.fill (0);
      dataKey = dataRaw = null;
      return '';
    },
    // a new passphrase (the wallet is on)
    changePassphrase: function (id, passphrase) {
      (async function () {
        if (!dataKey) throw new Error ('the wallet is off');
        var w = read ();
        w.passphrase = await wrapForPassphrase (passphrase);
        write (w);
        answer (id, true, '');
      }) ().catch (fail (id));
    },
    // a passkey which opens the wallet too (the wallet is on)
    addPasskey: function (id) {
      (async function () {
        if (!dataKey) throw new Error ('the wallet is off');
        var prfSalt = random (32);
        var cred = await navigator.credentials.create ({ publicKey: {
          challenge: random (32),
          rp: { id: location.hostname, name: 'TeXmacs' },
          user: { id: random (16), name: 'TeXmacs wallet', displayName: 'TeXmacs wallet' },
          pubKeyCredParams: [{ type: 'public-key', alg: -7 }, { type: 'public-key', alg: -257 }],
          authenticatorSelection: { residentKey: 'preferred', userVerification: 'required' },
          extensions: { prf: { eval: { first: prfSalt } } }
        } });
        var ext = cred.getClientExtensionResults ();
        if (!ext.prf || ext.prf.enabled === false)
          throw new Error ('this browser or this authenticator cannot give a secret (no PRF)');
        var credId = b64 (cred.rawId);
        // some give the secret at once, the others when asked
        var secret = ext.prf.results && ext.prf.results.first ?
          new Uint8Array (ext.prf.results.first) : await passkeySecret (credId, b64 (prfSalt));
        var salt = random (16);
        var k = await secretKey (secret, salt);
        var w = read ();
        w.passkey = { id: credId, prfSalt: b64 (prfSalt), salt: b64 (salt),
                      key: await encrypt (k, dataRaw) };
        write (w);
        answer (id, true, '');
      }) ().catch (fail (id));
    },
    removePasskey: function () {
      var w = read ();
      if (w && w.passkey) { delete w.passkey; write (w); }
      return '';
    },
    destroy: function () {
      api.lock ();
      try { FS.unlink (path); } catch (e) {}
      if (typeof tmSaveHome === 'function') tmSaveHome ();
      return '';
    }
  };
  return api;
}) ();
