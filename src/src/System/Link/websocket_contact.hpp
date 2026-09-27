/******************************************************************************
* MODULE     : websocket_contact.hpp
* DESCRIPTION: Contacts of the TeXmacs server for WebSocket clients
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WEBSOCKET_CONTACT_H
#define WEBSOCKET_CONTACT_H
#include "tm_contact.hpp"

// The contact of a client of the server, on its port: a WebSocket client
// (TeXmacs in a browser, whose sockets are WebSockets) or the contact of
// the other clients (inner: TLS or plain, as the preference tls-server
// says; it may be null, then only WebSocket clients are served). Which one
// is known from the first bytes the client sends: "GET " opens the
// handshake of a WebSocket. A WebSocket client has no TLS of its own
// within the WebSocket: the connection is encrypted by wss, or is local.
// mode is the preference "server websocket": "local" (the default, the
// clients of this machine only), "on" (any client, e.g. behind a proxy
// which does the TLS of wss) or "off".
tm_contact make_websocket_server_contact (tm_contact inner, string mode,
                                          bool local);

#endif // WEBSOCKET_CONTACT_H
