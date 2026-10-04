
/******************************************************************************
* MODULE     : worker_link.cpp
* DESCRIPTION: TeXmacs links to a Web Worker (the plugins of the browser)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// In a browser a plugin cannot be a program (no processes), but it can be a
// Web Worker: a script which runs apart from the page and exchanges messages
// with it, as a program exchanges bytes through its pipes. A worker link is
// the pipe link of such a plugin (plugin-configure ... (:worker "url")): the
// input of a session goes to the worker, and what the worker sends back is
// its output, in the usual protocol of the plugins (DATA_BEGIN, the format,
// DATA_END...). The page (misc/wasm/workers.js) makes the worker and keeps
// what it sends; the server takes it at each pass of its interpose handler
// (process_all_workers), as it takes the output of the pipes.

#include "tm_link.hpp"
#include "hashset.hpp"
#include "iterator.hpp"

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#include <stdlib.h>

EM_JS (int, web_worker_start, (const char* url), {
  return typeof tmWorkers !== 'undefined' ? tmWorkers.start (UTF8ToString (url)) : -1;
});

EM_JS (void, web_worker_write, (int id, const char* data, int n), {
  if (typeof tmWorkers !== 'undefined')
    tmWorkers.write (id, HEAPU8.slice (data, data + n));
});

// what the worker sent on a channel since the last call (0: output, 1:
// errors), in memory of the program (freed by the caller); NULL if nothing
EM_JS (char*, web_worker_take, (int id, int channel, int* n), {
  var d = typeof tmWorkers !== 'undefined' ? tmWorkers.take (id, channel) : null;
  if (!d || d.length == 0) { HEAP32[n >> 2] = 0; return 0; }
  var p = _malloc (d.length);
  HEAPU8.set (d, p);
  HEAP32[n >> 2] = d.length;
  return p;
});

EM_JS (int, web_worker_alive, (int id), {
  return typeof tmWorkers !== 'undefined' && tmWorkers.alive (id) ? 1 : 0;
});

EM_JS (void, web_worker_interrupt, (int id), {
  if (typeof tmWorkers !== 'undefined') tmWorkers.interrupt (id);
});

EM_JS (void, web_worker_stop, (int id), {
  if (typeof tmWorkers !== 'undefined') tmWorkers.stop (id);
});
#endif

/******************************************************************************
* The worker_link class
******************************************************************************/

static hashset<pointer> worker_link_set;

struct worker_link_rep: tm_link_rep {
  string url;     // the script of the worker, relative to the page
  int    id;      // the worker of the page, -1 if none
  string outbuf;  // pending output from the plugin
  string errbuf;  // pending errors from the plugin

public:
  worker_link_rep (string url2):
    url (url2), id (-1), outbuf (""), errbuf ("") {
      alive= false;
      worker_link_set->insert ((pointer) this); }
  ~worker_link_rep () {
    stop ();
    worker_link_set->remove ((pointer) this); }

  string start () {
    if (alive) return "busy";
#ifdef __EMSCRIPTEN__
    c_string s (url);
    id= web_worker_start (s);
    if (id < 0) return "Error: cannot start the worker '" * url * "'";
    alive= true;
    return "ok";
#else
    return "Error: the workers of the plugins run in a browser only";
#endif
  }

  void write (string s, int channel) {
    if (!alive || channel != LINK_IN) return;
#ifdef __EMSCRIPTEN__
    c_string cs (s);
    web_worker_write (id, (char*) cs, N(s));
#else
    (void) s;
#endif
  }

  // the data the worker sent: true if there was some; the worker which
  // ended (its script stopped, or failed) is no longer alive
  bool feed () {
    bool news= false;
#ifdef __EMSCRIPTEN__
    for (int ch= LINK_OUT; ch <= LINK_ERR; ch++) {
      int n= 0;
      char* p= web_worker_take (id, ch, &n);
      if (p == NULL) continue;
      if (ch == LINK_OUT) outbuf << string (p, n);
      else errbuf << string (p, n);
      free (p);
      news= true;
    }
    if (!web_worker_alive (id)) alive= false;
#endif
    return news;
  }

  string& watch (int channel) {
    static string empty_string= "";
    if (channel == LINK_OUT) return outbuf;
    else if (channel == LINK_ERR) return errbuf;
    else return empty_string;
  }

  string read (int channel) {
    string r;
    if (channel == LINK_OUT) { r= outbuf; outbuf= ""; }
    else if (channel == LINK_ERR) { r= errbuf; errbuf= ""; }
    return r;
  }

  // a worker cannot be waited for (the page has to have the hand for its
  // messages to come): what came is taken, and nothing more
  void listen (int msecs) { (void) msecs; if (alive) feed (); }

  void interrupt () {
#ifdef __EMSCRIPTEN__
    if (alive) web_worker_interrupt (id);
#endif
  }

  void stop () {
    if (!alive) return;
#ifdef __EMSCRIPTEN__
    web_worker_stop (id);
#endif
    alive= false;
    id= -1;
  }
};

tm_link
make_worker_link (string url) {
  return tm_new<worker_link_rep> (url);
}

// at each pass of the interpose handler of the server: the links whose
// worker sent something have it processed (the session reads it)
void
process_all_workers () {
  // the links first: processing the data of one may stop or make others
  array<pointer> links;
  iterator<pointer> it= iterate (worker_link_set);
  while (it->busy ()) links << it->next ();
  for (int i= 0; i < N(links); i++) {
    if (!worker_link_set->contains (links[i])) continue;
    worker_link_rep* con= (worker_link_rep*) links[i];
    if (con->alive && (con->feed () || !con->alive)) con->apply_command ();
  }
}
