/******************************************************************************
* MODULE     : websocket_contact.cpp
* DESCRIPTION: Contacts of the TeXmacs server for WebSocket clients
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "websocket_contact.hpp"
#include "base64.hpp"
#include "analyze.hpp"
#include "gnutls.hpp"
#include <stdint.h>
#include <string.h>
#include <errno.h>
#ifndef __MINGW32__
#include <sys/socket.h>
#define WS_SEND(a, b, c) ::send(a, b, c, 0)
#define WS_RECV(a, b, c, f) ::recv(a, b, c, f)
#else
namespace wsoc {
#include <sys/types.h>
#include <ws2tcpip.h>
}
#define WS_SEND(a, b, c) wsoc::send(a, b, c, 0)
#define WS_RECV(a, b, c, f) wsoc::recv(a, b, c, f)
#endif

/******************************************************************************
* SHA-1 (RFC 3174), for the key of the handshake
******************************************************************************/

static inline uint32_t
rol (uint32_t x, int n) {
  return (x << n) | (x >> (32 - n));
}

static string
sha1 (string msg) {
  uint32_t h[5]= { 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476,
                   0xC3D2E1F0 };
  uint64_t bits= ((uint64_t) N(msg)) * 8;
  string m= copy (msg);
  m << '\x80';
  while (N(m) % 64 != 56) m << '\0';
  for (int i= 7; i >= 0; i--) m << (char) ((bits >> (8*i)) & 0xff);
  for (int off= 0; off < N(m); off += 64) {
    uint32_t w[80];
    for (int i= 0; i < 16; i++)
      w[i]= ((uint32_t) (unsigned char) m[off+4*i] << 24) |
            ((uint32_t) (unsigned char) m[off+4*i+1] << 16) |
            ((uint32_t) (unsigned char) m[off+4*i+2] << 8) |
            ((uint32_t) (unsigned char) m[off+4*i+3]);
    for (int i= 16; i < 80; i++)
      w[i]= rol (w[i-3] ^ w[i-8] ^ w[i-14] ^ w[i-16], 1);
    uint32_t a= h[0], b= h[1], c= h[2], d= h[3], e= h[4];
    for (int i= 0; i < 80; i++) {
      uint32_t f, k;
      if (i < 20)      { f= (b & c) | ((~b) & d);          k= 0x5A827999; }
      else if (i < 40) { f= b ^ c ^ d;                     k= 0x6ED9EBA1; }
      else if (i < 60) { f= (b & c) | (b & d) | (c & d);   k= 0x8F1BBCDC; }
      else             { f= b ^ c ^ d;                     k= 0xCA62C1D6; }
      uint32_t t= rol (a, 5) + f + e + k + w[i];
      e= d; d= c; c= rol (b, 30); b= a; a= t;
    }
    h[0] += a; h[1] += b; h[2] += c; h[3] += d; h[4] += e;
  }
  string r (20);
  for (int i= 0; i < 5; i++)
    for (int j= 0; j < 4; j++)
      r[4*i+j]= (char) ((h[i] >> (24 - 8*j)) & 0xff);
  return r;
}

/******************************************************************************
* The contact
*
* The first bytes of a client say what it is: "GET " opens the handshake of
* a WebSocket (TeXmacs in a page served over http, or on this machine); a
* TLS record (0x16, a ClientHello) is a client over TLS, whose first bytes
* within TLS say it again: "GET " is a WebSocket over TLS (wss, TeXmacs in
* a page served over https, which may open no other), anything else a
* TeXmacs client over TLS, handed to the inner contact with what was read
* of it. A TeXmacs client speaks first (its login), the server never does,
* so that waiting for those bytes cannot block either. Any other client
* goes to the inner contact, as before.
*
* The TLS session of a wss client uses the certificate of the server
* ($TEXMACS_SERVER_CERT_DIR/cert.pem and key.pem, see gnutls.cpp), which
* the browser must trust: a certificate of an authority for the name of
* the host (Let's Encrypt...), not the self-signed one of the tests.
******************************************************************************/

#define WS_SNIFF 0  // the first bytes decide
#define WS_HTTP  1  // the request of the handshake
#define WS_OPEN  2  // frames
#define WS_INNER 3  // another client: the inner contact
#define WS_DEAD  4
#define WS_TLS   5  // the handshake of TLS
#define WS_TSNIFF 6 // TLS is up: its first bytes decide

static bool
would_block (int err) {
#ifdef __MINGW32__
  return err == WSAEWOULDBLOCK || err == WSAEINTR;
#else
  return err == EAGAIN || err == EWOULDBLOCK || err == EINTR;
#endif
}

static int
last_socket_error () {
#ifdef __MINGW32__
  return wsoc::WSAGetLastError ();
#else
  return errno;
#endif
}

struct websocket_server_contact_rep: tm_contact_rep {
  tm_contact inner;
  bool inner_tls;  // the inner contact is a TLS one (preference tls-server)
  array<array<string> > auths;
  tm_contact tls;  // the TLS session of a client over TLS
  bool secure;     // the bytes go through tls
  string mode;
  bool local;
  int io, state;
  bool peer_closed;
  string error;
  string raw;   // read, not yet decoded
  string data;  // decoded, not yet received
  string early; // what was read of a TeXmacs client over TLS

  websocket_server_contact_rep (tm_contact inner2, bool inner_tls2,
                                array<array<string> > auths2,
                                string mode2, bool local2):
    inner (inner2), inner_tls (inner_tls2), auths (auths2), secure (false),
    mode (mode2), local (local2), io (-1), state (WS_SNIFF),
    peer_closed (false) {
    type= SOCKET_SERVER; }

  void fail (string msg) { error= msg; state= WS_DEAD; }

  // the bytes of the client, from the socket or from TLS: their number, 0
  // at the end, -1 when there are none yet (wait) or on an error (fail)
  int pull (char* buf, int n, bool& wait) {
    wait= false;
    if (secure) {
      int r= ::receive (tls, buf, n);
      if (r >= 0) return r;
      wait= is_alive (tls); // else the session stopped itself
      return -1;
    }
    int r= WS_RECV (io, buf, n, 0);
    if (r < 0) wait= would_block (last_socket_error ());
    return r;
  }

  // write all of s, waiting (a little) while the socket is full
  bool write_all (string s) {
    int done= 0, n= N(s);
    c_string buf (s);
    while (done < n) {
      int r;
      bool wait;
      if (secure) {
        // GnuTLS wants the same arguments again after an EAGAIN
        r= ::send (tls, ((const char*) buf) + done, n - done);
        wait= r < 0 && is_alive (tls);
      }
      else {
        r= WS_SEND (io, ((const char*) buf) + done, n - done);
        wait= r < 0 && would_block (last_socket_error ());
      }
      if (r > 0) { done += r; continue; }
      if (wait) {
        struct tm_pollfd p;
        p.fd= io; p.events= TM_POLL_WRITE; p.revents= 0;
        if (tm_poll (&p, 1, 5000) > 0) continue;
      }
      return false;
    }
    return true;
  }

  // a frame of the server (not masked)
  bool send_frame (int opcode, string payload) {
    string f;
    int n= N(payload);
    f << (char) (0x80 | opcode);
    if (n < 126) f << (char) n;
    else if (n < 65536) {
      f << (char) 126 << (char) ((n >> 8) & 0xff) << (char) (n & 0xff);
    }
    else {
      f << (char) 127;
      for (int i= 7; i >= 0; i--)
        f << (char) ((((uint64_t) n) >> (8*i)) & 0xff);
    }
    f << payload;
    return write_all (f);
  }

  // the complete frames of raw into data (and the control frames answered)
  void decode () {
    while (state == WS_OPEN) {
      int n= N(raw);
      if (n < 2) return;
      unsigned char b0= (unsigned char) raw[0], b1= (unsigned char) raw[1];
      int opcode= b0 & 0x0f;
      bool masked= (b1 & 0x80) != 0;
      uint64_t len= b1 & 0x7f;
      int pos= 2;
      if (len == 126) {
        if (n < 4) return;
        len= ((uint64_t) (unsigned char) raw[2] << 8) |
              (uint64_t) (unsigned char) raw[3];
        pos= 4;
      }
      else if (len == 127) {
        if (n < 10) return;
        len= 0;
        for (int i= 2; i < 10; i++) len= (len << 8) | (unsigned char) raw[i];
        pos= 10;
      }
      if (len > (((uint64_t) 1) << 28)) { fail ("WebSocket frame too large"); return; }
      char mask[4]= { 0, 0, 0, 0 };
      if (masked) {
        if (n < pos + 4) return;
        for (int i= 0; i < 4; i++) mask[i]= raw[pos+i];
        pos += 4;
      }
      if ((uint64_t) (n - pos) < len) return;
      string payload= raw (pos, pos + (int) len);
      raw= raw (pos + (int) len, n);
      if (masked)
        for (int i= 0; i < N(payload); i++) payload[i] ^= mask[i & 3];
      switch (opcode) {
      case 0x0: case 0x1: case 0x2: data << payload; break; // data
      case 0x8: // close: answered, then the end of the stream
        if (!peer_closed) send_frame (0x8, "");
        peer_closed= true;
        return;
      case 0x9: send_frame (0xA, payload); break; // ping
      default: break; // pong, reserved
      }
    }
  }

  // read what the client has sent into raw (at most limit bytes in all);
  // false when it closed or failed (then the contact failed with msg)
  bool gather (int limit, string msg) {
    char buf[4096];
    while (N(raw) < limit) {
      bool wait;
      int r= pull (buf, sizeof (buf), wait);
      if (r > 0) { raw << string (buf, r); continue; }
      if (r == 0) { fail ("closed " * msg); return false; }
      if (wait) return true;
      fail (secure && !is_nil (tls) && ::last_error (tls) != "" ?
            msg * ": " * ::last_error (tls) : "cannot read the client " * msg);
      return false;
    }
    return true;
  }

  bool websocket_allowed () {
    if (mode == "off" || (mode != "on" && !local)) {
      fail ("WebSocket clients are not served "
            "(preference \"server websocket\")");
      return false;
    }
    return true;
  }

  // the request of the handshake, and the answer
  void handshake () {
    if (!gather (16385, "during the WebSocket handshake")) return;
    if (N(raw) > 16384) { fail ("WebSocket request too large"); return; }
    int end= search_forwards ("\r\n\r\n", raw);
    if (end < 0) return; // not all of it yet
    string request= raw (0, end);
    raw= raw (end + 4, N(raw));
    array<string> lines= tokenize (request, "\r\n");
    string key, version, upgrade;
    bool binary= false;
    for (int i= 1; i < N(lines); i++) {
      int c= search_forwards (":", lines[i]);
      if (c < 0) continue;
      string name= locase_all (lines[i] (0, c));
      string value= trim_spaces (lines[i] (c+1, N(lines[i])));
      if (name == "sec-websocket-key") key= value;
      else if (name == "sec-websocket-version") version= value;
      else if (name == "upgrade") upgrade= locase_all (value);
      else if (name == "sec-websocket-protocol") {
        array<string> ps= tokenize (value, ",");
        for (int j= 0; j < N(ps); j++)
          if (trim_spaces (ps[j]) == "binary") binary= true;
      }
    }
    if (key == "" || upgrade != "websocket" || version != "13") {
      write_all ("HTTP/1.1 400 Bad Request\r\nConnection: close\r\n\r\n");
      fail ("not a WebSocket request");
      return;
    }
    string accept= encode_base64 (sha1 (key *
                     "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"));
    string answer= "HTTP/1.1 101 Switching Protocols\r\n"
                   "Upgrade: websocket\r\nConnection: Upgrade\r\n"
                   "Sec-WebSocket-Accept: " * accept * "\r\n";
    // the subprotocol of the sockets of Emscripten, which asks for it
    if (binary) answer << "Sec-WebSocket-Protocol: binary\r\n";
    answer << "\r\n";
    if (!write_all (answer)) { fail ("WebSocket handshake failed"); return; }
    state= WS_OPEN;
    decode ();
  }

  void start (int io2) {
    io= io2;
    if (state == WS_SNIFF) {
      char buf[4];
      int r= WS_RECV (io, buf, 4, MSG_PEEK);
      if (r < 0) {
        if (!would_block (last_socket_error ())) fail ("cannot read the client");
        return;
      }
      if (r == 0) { fail ("closed by the client"); return; }
      string head (buf, r);
      if (head == string ("GET ") (0, r)) {
        if (r < 4) return; // "GET " may come in pieces
        if (!websocket_allowed ()) return;
        state= WS_HTTP;
      }
      else if (buf[0] == '\x16' && gnutls_present ()) {
        // a client over TLS: the session of the inner contact when it
        // does TLS, else one of its own, for the WebSocket clients only
        if (inner_tls && !is_nil (inner)) tls= inner;
        else {
          array<array<string> > a= auths;
          if (N(a) == 0) {
            array<string> anonymous;
            anonymous << string ("anonymous");
            a << anonymous;
          }
          tls= make_tls_server_contact (a);
        }
        if (is_nil (tls)) { fail ("no TLS for this client"); return; }
        state= WS_TLS;
      }
      else {
        if (is_nil (inner)) { fail ("no contact for this client"); return; }
        state= WS_INNER;
      }
    }
    if (state == WS_TLS) {
      ::start (tls, io); // again at each call, until the handshake is over
      if (!is_alive (tls)) {
        fail ("TLS handshake failed" *
              (::last_error (tls) != "" ? ": " * ::last_error (tls) : string ("")));
        return;
      }
      if (!is_active (tls)) return;
      secure= true;
      state= WS_TSNIFF;
    }
    if (state == WS_TSNIFF) {
      if (!gather (4, "before its first request")) return;
      if (raw == string ("GET ") (0, min (4, N(raw))) && N(raw) < 4)
        return; // "GET " may come in pieces
      if (starts (raw, "GET ")) {
        if (!websocket_allowed ()) return;
        state= WS_HTTP;
      }
      else if (inner_tls && tls.rep == inner.rep) {
        early= raw;
        raw= "";
        state= WS_INNER;
        return; // the inner contact is started: the session is its own
      }
      else {
        fail ("TeXmacs clients over TLS are not served "
              "(preference \"tls-server\")");
        return;
      }
    }
    if (state == WS_HTTP) handshake ();
    else if (state == WS_INNER) ::start (inner, io);
  }

  void stop () {
    if (state == WS_INNER) ::stop (inner);
    else {
      if (state == WS_OPEN && !peer_closed) {
        // a close frame, if the connection takes it at once
        if (secure) (void) ::send (tls, "\x88\0", 2);
        else {
          char f[2]= { (char) 0x88, 0 };
          (void) WS_SEND (io, f, 2);
        }
      }
      if (!is_nil (tls) && is_alive (tls)) ::stop (tls);
    }
    state= WS_DEAD;
    io= -1;
  }

  int send (const void* buffer, size_t length) {
    if (state == WS_INNER) return ::send (inner, buffer, length);
    if (state != WS_OPEN) return -1;
    if (!send_frame (0x2, string ((const char*) buffer, (int) length))) {
      fail ("cannot write to the WebSocket client");
      return -1;
    }
    return (int) length;
  }

  // the decoded data; -1 while there is none yet (EAGAIN), 0 at the end
  int receive (void* buffer, size_t length) {
    if (state == WS_INNER) {
      if (N(early) == 0) return ::receive (inner, buffer, length);
      int n= min ((int) length, N(early));
      memcpy (buffer, (const char*) c_string (early (0, n)), n);
      early= early (n, N(early));
      return n;
    }
    if (state != WS_OPEN) return -1;
    if (N(data) == 0 && !peer_closed) {
      char buf[16384];
      bool wait;
      int r= pull (buf, sizeof (buf), wait);
      if (r == 0) { peer_closed= true; return 0; }
      if (r < 0) {
        if (!wait) fail ("cannot read the WebSocket client");
        return -1;
      }
      raw << string (buf, r);
      decode ();
    }
    if (N(data) == 0) return peer_closed ? 0 : -1;
    int n= min ((int) length, N(data));
    memcpy (buffer, (const char*) c_string (data (0, n)), n);
    data= data (n, N(data));
    return n;
  }

  bool alive () {
    if (state == WS_DEAD) return false;
    if (state == WS_INNER) return is_alive (inner);
    if (secure || state == WS_TLS) return is_alive (tls);
    return true;
  }

  bool active () {
    if (state == WS_OPEN) return true;
    if (state == WS_INNER) return is_active (inner);
    return false;
  }

  string last_error () {
    if (state == WS_INNER) return ::last_error (inner);
    return error;
  }
};

tm_contact
make_websocket_server_contact (tm_contact inner, bool inner_tls,
                               array<array<string> > auths,
                               string mode, bool local) {
  return tm_contact ((tm_contact_rep*)
    tm_new<websocket_server_contact_rep> (inner, inner_tls, auths,
                                          mode, local));
}
