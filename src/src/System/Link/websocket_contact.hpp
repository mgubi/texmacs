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

// The contact of a client of the port of the WebSockets of the server, for
// the versions of TeXmacs without Qt: the handshake of a WebSocket and its
// frames (see the .cpp). tls is the contact of the TLS session inside
// which the WebSocket is opened (wss), or null for a WebSocket in clear
// (ws).
tm_contact make_websocket_server_contact (tm_contact tls);

#endif // WEBSOCKET_CONTACT_H
