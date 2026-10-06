
/******************************************************************************
* MODULE     : request_link.cpp
* DESCRIPTION: TeXmacs links by http post
* COPYRIGHT  : (C) 2026  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "basic.hpp"
#include "tm_link.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "tm_timer.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include "convert.hpp"
#include "web_files.hpp"

hashset<pointer> request_link_set;
void request_callback (void *obj, void *info);

/******************************************************************************
* The request_link class
******************************************************************************/

struct request_link_rep: tm_link_rep {
  string name;          // name of the plugin
  int    status;        // negative= error, null= EOF, positive means data
  string outbuf;        // pending output from plugin
  string errbuf;        // pending errors from plugin
  bool   kill;          // the request can be cancelled
  
public:
  request_link_rep (string name);
  ~request_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();
  tree    partial ();
  string  partial_text;  // the text of the answer so far
  tree    partial_tree;  // and what is shown of it
  tree    request;       // the request, asked again after a failure for a
  int     retries;       // while (too many requests): the times it was,
  double  retry_at;      // when it will be (0 when it will not be),
  string  retry_why;     // and why

  bool    retry_wait ();

  void    feed (int channel);
};

request_link_rep::request_link_rep (string name2): name (name2) {
  request_link_set->insert ((pointer) this);
  status = 1;
  outbuf = "";
  errbuf = "";
  kill   = false;
  alive  = false;
  partial_text= "";
  partial_tree= "";
  request= "";
  retries= 0;
  retry_at= 0;
  retry_why= "";
}

request_link_rep::~request_link_rep () {
  stop ();
#ifndef __EMSCRIPTEN__
  // (no transfer may write into this link any more: libcurl)
  http_async_cancel (&outbuf);
#endif
  request_link_set->remove ((pointer) this);
}

tm_link
make_request_link (string name) {
  return tm_new<request_link_rep> (name);
}

void
close_all_requests () {
  iterator<pointer> it= iterate (request_link_set);
  while (it->busy()) {
    request_link_rep* con= (request_link_rep*) it->next();
    if (con->alive) {
      // kill actual request (or wait that it dies)
      con->alive= false;
      con->kill= true;
    }
  }
}

void
process_all_requests () {
  iterator<pointer> it= iterate (request_link_set);
  while (it->busy()) {
    request_link_rep* con= (request_link_rep*) it->next();
    //cout << con->name << " ~> " << (con->alive? "true": "false") << "\n";
    if (con->alive)
      con->apply_command ();
  }
}

/******************************************************************************
* Routines for request_links
******************************************************************************/

string
request_link_rep::start () {
  status= 1;
  outbuf= "";
  errbuf= "";
  kill= false;
  return "request";
}

static bool
eval_request (tree t, int& status,
	      string& outbuf, string& errbuf, bool& kill) {
  // cout << "eval_request, " << t << LF;
  if (is_compound (t, "http_post", 3) && is_atomic (t[0])
      && is_tuple (t[1])) {
    string url= t[0]->label;
    tree data= t[2];
    array<string> headers;
    for (int i= 0; i < N(t[1]); i++)
      if (is_atomic (t[1][i])) headers << t[1][i]->label;
    return async_http_post_json (url, headers, data,
				 status, outbuf, errbuf, kill);
  }
  if (is_compound (t, "error", 1) && is_atomic (t[0])) {
    // a request which cannot be made (an AI engine without its key): why,
    // read as an answer which failed at once (the next feed ends it)
    status= 0; outbuf= ""; errbuf= t[0]->label; kill= false;
    return false;
  }
  io_error << "request_link, unexpected request: "
           << http_mask_request (t) << LF;
  return true;
}

void
request_link_rep::write (string s, int channel) {
  // cout << "Write[" << name << "] " << s << "\n";
  if (alive || (channel != LINK_IN)) return;
  string cmd= as_string (call ("connection-request", name, "default", s));
  // cout << "Command[" << name << "," << s << "] = " << cmd << "\n";
  if (cmd == "") {
    status= 0; outbuf= ""; errbuf= ""; kill= false;
    alive= false;
    return;
  }
  tree t= scheme_to_tree (cmd);
  if (DEBUG_IO) debug_io << "Requesting '" << http_mask_request (t) << "'\n";
#ifdef __EMSCRIPTEN__
  web_async_cancel (&outbuf); // an answer still awaited is no longer wanted
#else
  http_async_cancel (&outbuf);
#endif
  status= 1; outbuf= ""; errbuf= ""; kill= false;
  partial_text= ""; partial_tree= "";
  request= t; retries= 0; retry_at= 0; retry_why= "";
  alive= !eval_request (t, status, outbuf, errbuf, kill);
}

// a request which the engine refused for a while (too many requests, an
// engine overloaded: ai_retry_delay) is asked again after a wait, three
// times at most; true while the link waits, or asks again
bool
request_link_rep::retry_wait () {
  if (retry_at > 0) {
    if (texmacs_time () < retry_at) return true;
    retry_at= 0;
    status= 1; outbuf= ""; errbuf= ""; kill= false;
    partial_text= ""; partial_tree= "";
    if (eval_request (request, status, outbuf, errbuf, kill)) alive= false;
    return true;
  }
  // (an error of HTTP: status 0 with libcurl and in a browser, negative
  // with Qt, the answer of the engine being there in both cases)
  if (status <= 0 && retries < 3 && is_compound (request, "http_post")) {
    string why;
    int d= ai_retry_delay (outbuf, name, retries, why);
    if (d > 0) {
      retries++;
      retry_at= texmacs_time () + 1000.0 * d;
      retry_why= why;
      errbuf= ""; // (the error of HTTP of Qt: not shown)
      if (DEBUG_IO)
        debug_io << "request_link, " << why << ": asked again in "
                 << d << " s" << LF;
      return true;
    }
  }
  return false;
}

void
request_link_rep::feed (int channel) {
  // cout << "Feed " << channel << "\n";
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR))) return;
  if (retry_wait ()) return;
  if (status < 0)
    io_error << "Read failed for '" << name << "'\n";
  if (status <= 0) 
    alive= false;
}

string&
request_link_rep::watch (int channel) {
  // cout << "Watch " << channel << "\n";
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else if (channel == LINK_ERR) return errbuf;
  else return empty_string;
}

string
request_link_rep::read (int channel) {
  // cout << "Read " << channel << "\n";
  if (channel == LINK_OUT) {
    if (alive) {
      // cout << "\n--- partial output ---\n" << outbuf << "\n";
      return "";
    }
    string r= outbuf;
    outbuf= "";
    return r;
  }
  else if (channel == LINK_ERR) {
    // (an error which may be followed by a question asked again: not yet)
    if (alive && retry_wait ()) return "";
    string r= errbuf;
    errbuf= "";
    return r;
  }
  else return string ("");
}

void
request_link_rep::listen (int msecs) {
  (void) msecs;
  if (!alive) return;
  feed (LINK_OUT);
  feed (LINK_IN);
}

// what is shown of a streamed answer which has not ended (ai_stream_text,
// ai_latex_partial: the LaTeX set as far as all is closed in it)
tree
request_link_rep::partial () {
  if (!alive) return "";
  if (retry_at > 0) {
    int left= (int) ((retry_at - texmacs_time ()) / 1000.0 + 0.999);
    if (left < 0) left= 0;
    return compound ("with", "color", "dark grey", "font-shape", "italic",
                     "The engine says: " * retry_why * ". Asked again in " *
                     as_string (left) * " s (" * as_string (retries) *
                     " of 3)");
  }
  if (outbuf == "") return "";
  string err, reasoning;
  string r= ai_stream_text (outbuf, name, err, reasoning);
  // (while the model thinks: the end of its reasoning)
  string key= (r == "" && reasoning != "")? "\1" * reasoning: r;
  if (key != partial_text) {
    partial_text= key;
    partial_tree= (r == "" && reasoning != "")?
      ai_reasoning_partial (reasoning): ai_latex_partial (r);
  }
  return partial_tree;
}

void
request_link_rep::interrupt () {
  if (!alive) return;
  alive= false;
  kill= true;
  retry_at= 0;
  // the answer which comes is stopped (the engine stops writing it)
#ifdef __EMSCRIPTEN__
  web_async_cancel (&outbuf);
#else
  http_async_cancel (&outbuf);
#endif
}

void
request_link_rep::stop () {
  if (!alive) return;
  alive= false;    
  kill= true;
  retry_at= 0;
#ifdef __EMSCRIPTEN__
  web_async_cancel (&outbuf);
#else
  http_async_cancel (&outbuf);
#endif
}

/******************************************************************************
* Call back for new information on pipe
******************************************************************************/

void request_callback (void *obj, void *info) {
  (void) info;
  request_link_rep* con= (request_link_rep*) obj;  
  if (!is_nil (con->feed_cmd)) {
    // cout << "request_callback applies" << LF;
    con->feed_cmd->apply (); // call the data processor
  }
}
