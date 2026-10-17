/******************************************************************************
* MODULE     : lsp_link.cpp
* DESCRIPTION: Links to language servers (Language Server Protocol)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// A language server reads and writes JSON-RPC messages, each preceded by a
// header: lines "Name: value" ended by CR LF, then an empty line, of which
// only Content-Length (the length of the message in bytes) matters. An LSP
// link frames the messages over another link, which carries the bytes (the
// pipes of a program, later a socket or a Web Worker): the protocol itself
// (requests, answers, notifications, the documents) is done in Scheme
// (progs/lsp), which gets the messages one by one through a call back.

#include "lsp_link.hpp"
#include "analyze.hpp"
#include "convert.hpp"
#include "scheme.hpp"
#include "tm_timer.hpp"

/******************************************************************************
* The framing of the messages
******************************************************************************/

struct lsp_link_rep: tm_link_rep {
  tm_link       ln;      // the link which carries the bytes
  string        buf;     // the bytes received, not yet a whole message
  int           pos;     // the start of what is left in buf
  array<string> msgs;    // the whole messages, not yet read
  string        head;    // the first of them (watch)
  string        errbuf;  // what the server wrote on stderr
  bool          news;    // something has come since the last dispatch

public:
  lsp_link_rep (tm_link ln);
  ~lsp_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();

  void    drain ();
  bool    parse ();
};

static void
lsp_link_callback (void* obj, void* info) {
  (void) info;
  lsp_link_rep* con= (lsp_link_rep*) obj;
  con->drain ();
  if (N(con->msgs) > 0 || con->errbuf != "" || !con->alive)
    con->apply_command ();
}

lsp_link_rep::lsp_link_rep (tm_link ln2): ln (ln2), pos (0), news (false) {
  alive= false;
}

lsp_link_rep::~lsp_link_rep () {
  stop ();
}

tm_link
make_lsp_link (tm_link transport) {
  return tm_new<lsp_link_rep> (transport);
}

string
lsp_frame (string msg) {
  return "Content-Length: " * as_string (N(msg)) * "\r\n\r\n" * msg;
}

string
lsp_link_rep::start () {
  if (alive) return "busy";
  buf= ""; pos= 0; msgs= array<string> (); head= ""; errbuf= "";
  string r= ln->start ();
  if (r != "ok") return r;
  alive= true;
  ln->set_command (command (lsp_link_callback, this));
  return r;
}

void
lsp_link_rep::write (string s, int channel) {
  if (!alive || channel != LINK_IN) return;
  ln->write (lsp_frame (s), LINK_IN);
}

bool
lsp_link_rep::parse () {
  // the next whole message of buf into msgs (false: none yet). The lines
  // of a header are ended by CR LF, but a line which a server prints on
  // stdout by mistake may end by LF alone and come just before a header:
  // the lines are split at each LF, and those which are not "Name: value"
  // are passed on as errors
  int end= search_forwards ("\r\n\r\n", pos, buf);
  if (end < 0) return false;
  int len= -1, i= pos;
  while (i < end) {
    int e= search_forwards ("\n", i, buf);
    if (e < 0 || e > end) e= end;
    string line= buf (i, e);
    if (N(line) > 0 && line[N(line)-1] == '\r') line= line (0, N(line)-1);
    int colon= search_forwards (":", line);
    if (colon > 0 &&
        locase_all (trim_spaces (line (0, colon))) == "content-length") {
      string val= trim_spaces (line (colon + 1, N(line)));
      if (N(val) > 0 && N(val) < 10 && is_int (val)) len= as_int (val);
    }
    else if (colon <= 0 && line != "")
      errbuf << "lsp: skipped '" << line << "'\n";
    i= e + 1;
  }
  if (len < 0) {
    // not a header: it is passed on as an error and skipped
    errbuf << "lsp: no Content-Length in '" << buf (pos, end) << "'\n";
    pos= end + 4;
    return true;
  }
  if (N(buf) - (end + 4) < len) return false;
  msgs << buf (end + 4, end + 4 + len);
  pos= end + 4 + len;
  return true;
}

void
lsp_link_rep::drain () {
  // what the link has received, into whole messages
  if (!alive) return;
  string out= ln->read (LINK_OUT);
  string err= ln->read (LINK_ERR);
  if (out != "") {
    buf << out;
    while (parse ()) {}
    // (what is used of buf is dropped once it is large enough to pay)
    if (pos > 0 && (pos == N(buf) || pos >= 65536)) {
      buf= buf (pos, N(buf));
      pos= 0;
    }
  }
  if (err != "") errbuf << err;
  if (!ln->alive) alive= false;
  if (out != "" || err != "" || !alive) news= true;
}

string&
lsp_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) {
    head= (N(msgs) > 0? msgs[0]: string (""));
    return head;
  }
  else if (channel == LINK_ERR) return errbuf;
  else return empty_string;
}

string
lsp_link_rep::read (int channel) {
  if (N(msgs) == 0 && errbuf == "") drain ();
  if (channel == LINK_OUT) {
    if (N(msgs) == 0) return "";
    string r= msgs[0];
    msgs= range (msgs, 1, N(msgs));
    return r;
  }
  else if (channel == LINK_ERR) {
    string r= errbuf;
    errbuf= "";
    return r;
  }
  else return "";
}

void
lsp_link_rep::listen (int msecs) {
  // waits until a whole message (or an error, or the end) has come, for at
  // most msecs: the bytes of a message may come in several parts
  time_t wait_until= texmacs_time () + msecs;
  drain ();
  while (alive && N(msgs) == 0 && errbuf == "") {
    time_t left= wait_until - texmacs_time ();
    if (left <= 0) break;
    ln->listen ((int) left);
    drain ();
  }
}

void
lsp_link_rep::interrupt () {
  if (alive) ln->interrupt ();
}

void
lsp_link_rep::stop () {
  ln->set_command (command ());
  if (!alive) return;
  alive= false;
  ln->stop ();
}

/******************************************************************************
* The clients of the Scheme programs
******************************************************************************/

struct lsp_client {
  int     id;
  tm_link ln;
  lsp_link_rep* rep;  // (the link, which ln keeps)
  object  call_back;
  bool    exited;     // "exit" was sent
  bool    removed;    // stopped by lsp_stop
};

static array<lsp_client*> lsp_clients;
static int lsp_client_counter= 0;
static int lsp_dispatching= 0;  // the depth of the dispatches

static lsp_client*
lsp_find (int id) {
  for (int i=0; i<N(lsp_clients); i++)
    if (lsp_clients[i]->id == id && !lsp_clients[i]->removed)
      return lsp_clients[i];
  return NULL;
}

static void
lsp_dispatch () {
  // what the servers have sent, to the call backs, for the clients which
  // have news (the pipes tell them, lsp_link_callback). A call back may
  // send, listen or stop again: a call back which waits for an answer
  // (lsp-request-wait) dispatches within this dispatch. No client is
  // deleted meanwhile (process_all_lsp), and the clients which are added
  // come at the end of lsp_clients
  lsp_dispatching++;
  bool busy= true;
  while (busy) {
    busy= false;
    for (int i=0; i<N(lsp_clients); i++) {
      lsp_client* c= lsp_clients[i];
      if (c->removed || c->exited) continue;
      if (!c->rep->news && c->ln->alive) continue;
      c->rep->news= false;
      string err= c->ln->read (LINK_ERR);
      if (err != "") {
        call (c->call_back, object ("stderr"), object (err));
        busy= true;
      }
      if (c->removed) continue;
      string msg= c->ln->read (LINK_OUT);
      if (msg != "") {
        // (the other messages of the client are read at the next round)
        c->rep->news= true;
        call (c->call_back, object ("message"), object (msg));
        busy= true;
      }
      else if (!c->ln->alive && c->ln->watch (LINK_ERR) == "") {
        c->exited= true;
        call (c->call_back, object ("exit"), object (""));
        busy= true;
      }
    }
  }
  lsp_dispatching--;
}

void
process_all_lsp () {
  // NOTE: called by the interpose handler of the server, apart from the
  // call backs of the pipes, as the clients which have ended are deleted
  // here, with their links
  if (N(lsp_clients) == 0) return;
  lsp_dispatch ();
  // (not within a dispatch: a call back which runs an event loop)
  if (lsp_dispatching > 0) return;
  array<lsp_client*> left;
  for (int i=0; i<N(lsp_clients); i++)
    if (lsp_clients[i]->removed || lsp_clients[i]->exited)
      tm_delete<lsp_client> (lsp_clients[i]);
    else left << lsp_clients[i];
  lsp_clients= left;
}

object
lsp_start (string cmd, object call_back) {
  tm_link ln= make_lsp_link (make_pipe_link (cmd));
  string r= ln->start ();
  if (r != "ok") return object (r == ""? string ("Error: no server"): r);
  lsp_client* c= tm_new<lsp_client> ();
  c->id= ++lsp_client_counter;
  c->ln= ln;
  c->rep= (lsp_link_rep*) ln.rep;
  c->call_back= call_back;
  c->exited= false;
  c->removed= false;
  lsp_clients << c;
  return object (c->id);
}

bool
lsp_send (int id, string msg) {
  lsp_client* c= lsp_find (id);
  if (c == NULL || !c->ln->alive) return false;
  c->ln->write (msg, LINK_IN);
  return true;
}

void
lsp_listen (int id, int msecs) {
  // waits for at most msecs until the server with this id has sent
  // something, and sends what has come to the call backs (for the
  // requests which are waited for, and the tests)
  lsp_client* c= lsp_find (id);
  if (c != NULL) c->ln->listen (msecs);
  lsp_dispatch ();
}

bool
lsp_alive (int id) {
  lsp_client* c= lsp_find (id);
  return c != NULL && c->ln->alive;
}

void
lsp_stop (int id) {
  // the server is ended at once (after the shutdown and exit of the
  // protocol, which are sent in Scheme); its call back gets nothing more
  lsp_client* c= lsp_find (id);
  if (c == NULL) return;
  c->ln->stop ();
  c->removed= true;
}

/******************************************************************************
* The text of a document
******************************************************************************/

static string
lsp_line_text (tree t) {
  // a line which is not a string, on one line
  if (is_atomic (t)) return cork_to_utf8 (t->label);
  string s= utf8_to_cork (tree_to_verbatim (t, false, "SourceCode"));
  return cork_to_utf8 (replace (s, "\n", " "));
}

string
lsp_document_text (tree t) {
  // the same text as lsp-lines->text of lsp-lines in Scheme, without the
  // lists of the lines there (each line converted apart: a newline is a
  // character in Cork)
  if (is_atomic (t)) {
    array<string> a= tokenize (t->label, "\n");
    string r;
    for (int i=0; i<N(a); i++) {
      if (i > 0) r << "\n";
      r << cork_to_utf8 (a[i]);
    }
    return r;
  }
  if (!is_func (t, DOCUMENT)) return lsp_line_text (t);
  string r;
  for (int i=0; i<N(t); i++) {
    if (i > 0) r << "\n";
    r << lsp_line_text (t[i]);
  }
  return r;
}
